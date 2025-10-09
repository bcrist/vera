// The following fields are only guaranteed to be accurate during microcode processing;
// They aren't considered when deduplicating cycles, so after cycle deduplication they will only reflect one of the usages of the cycle.
func_name: []const u8,
instruction_signature: ?isa.Instruction.Signature,
initial_dr: arch.reg.DR,
initial_krio: arch.bus.K.Read_Index_Offset,
initial_wio: arch.reg.gpr.Write_Index_Offset,
encoding_len: ?isa.Instruction.Encoded.Length_Bytes,
flags: std.EnumSet(Cycle_Flags),
assigned_signals: std.EnumSet(arch.Control_Signal),

// These fields *are* considered when deduplicating:
signals: arch.Control_Signals,
next_func: ?*const anyopaque,
next_slot: ?Microcode_Builder.Slot_Data.Handle, // only considered when next_func is null

pub const Cycle_Flags = enum {
    next_insn_loaded,
    initial_dr1_valid,
    initial_dr2_valid,
    initial_krio_valid,
    initial_wio_valid,
    dr_valid,
    ir_valid,
    disallow_read_from_other_rsn,
    disallow_write_to_other_rsn,
};

pub fn init(
    name: []const u8,
    instruction_signature: ?isa.Instruction.Signature,
    initial_dr: arch.reg.DR,
    initial_krio: arch.bus.K.Read_Index_Offset,
    initial_wio: arch.reg.gpr.Write_Index_Offset,
    encoding_len: ?isa.Instruction.Encoded.Length_Bytes,
    flags: std.EnumSet(Cycle_Flags)
) Cycle {
    var final_flags = flags;
    final_flags.remove(.disallow_read_from_other_rsn);
    final_flags.remove(.disallow_write_to_other_rsn);
    return .{
        .func_name = name,
        .instruction_signature = instruction_signature,
        .initial_dr = initial_dr,
        .initial_krio = initial_krio,
        .initial_wio = initial_wio,
        .encoding_len = encoding_len,
        .flags = final_flags,
        .signals = .defaults,
        .assigned_signals = .{},
        .next_func = null,
        .next_slot = null,
    };
}

fn warn(cycle: *Cycle, comptime format: []const u8, args: anytype) void {
    var buf: [64]u8 = undefined;
    var writer = std.fs.File.stderr().writer(&buf);
    writer.interface.print("cycle: {s}:\n", .{ cycle.func_name }) catch @panic("IO Error");
    writer.interface.writeAll("    ") catch @panic("IO Error");
    writer.interface.print(format, args) catch @panic("IO Error");
    writer.interface.writeAll("\n") catch @panic("IO Error");

    inline for (@typeInfo(arch.Control_Signals).@"struct".fields) |field| {
        if (std.mem.eql(u8, field.name, "mode")) {
            _ = switch (cycle.signals.unit) {
                .none  => writer.interface.print("        mode = {f}\n", .{ cycle.signals.mode }),
                .alu   => writer.interface.print("        mode = {any}\n", .{ cycle.signals.mode.alu }),
                .shift => writer.interface.print("        mode = {any}\n", .{ cycle.signals.mode.shift }),
                .mult  => writer.interface.print("        mode = {any}\n", .{ cycle.signals.mode.mult }),
                .count, .extend => writer.interface.print("        mode = {any}\n", .{ cycle.signals.mode.count_extend }),
            } catch @panic("IO Error");
        } else {
            writer.interface.print("        {s} = {f}\n", .{
                field.name,
                @field(cycle.signals, field.name),
            }) catch @panic("IO Error");
        }
    }

    writer.interface.flush() catch @panic("IO Error");
    @panic("cycle error");
}

pub fn finish(cycle: *Cycle) void {
    switch (cycle.signals.at_op) {
        .none => {},
        .translate => {
            cycle.validate_address();
            cycle.ensure_set(.width);
        },
        .update, .invalidate => {
            cycle.validate_address();
        },
    }

    if (cycle.signals.dsrc == .dr_ir) {
        cycle.ensure_set(.drw);
        if (cycle.signals.at_op == .translate or cycle.signals.lsrc == .compute_or_d and cycle.signals.unit == .none) {
            // writing to system bus or reading to L, this is the expected case.
        } else {
            cycle.warn("DR value is written to D, but there is no bus transaction happening, and D is not being read to L", .{});
        }
    }

    if (cycle.signals.drw == .write) {
        switch (cycle.signals.dsrc) {
            .l, .dr_ir, .vabor => {},
            .system => {
                cycle.validate_address();
                if (cycle.signals.at_op != .translate) cycle.warn("Expected `at_op` to be .translate", .{});
            },
        }
    }

    switch (cycle.signals.jsrc) {
        .zero, .jr => {},
        .sr1 => cycle.ensure_set(.sr1ri),
        .sr2 => cycle.ensure_set(.sr2ri),
    }

    switch (cycle.signals.ksrc) {
        .zero => {},
        .constant => cycle.ensure_set(.constant),
        .krio_sx, .krio_zx, .krio_bit, .krio_bit_inv => cycle.validate_krio(),
        .kr => cycle.validate_kr(),
        .sr1 => cycle.ensure_set(.sr1ri),
        .sr2 => cycle.ensure_set(.sr2ri),
        .dr_byte_1_sx, .dr_byte_2_sx, .dr_byte_21_sx => {
            cycle.validate_dr();
        },
    }

    switch (cycle.signals.lsrc) {
        .compute_or_d => switch (cycle.signals.unit) {
            .none =>  switch (cycle.signals.dsrc) {
                .l => cycle.warn("`dsrc` cannot be .l when the L bus is being read from D", .{}),
                .system => switch (cycle.signals.at_op) {
                    .none => {},
                    .translate => {
                        cycle.validate_address();
                        cycle.ensure_set(.width);
                    },
                    else => cycle.warn("Expected `at_op` to be .translate", .{}),
                },
                .dr_ir => {},
                .vabor => {},
            },
            else => cycle.validate_compute_mode(),
        },
        .last_d => {},
        .status => {},
        .flags => {},
    }

    if (cycle.signals.gprw == .write or cycle.signals.tiw == .write) {
        cycle.validate_wi();
        if (cycle.signals.tiw == .write and !cycle.flags.contains(.initial_wio_valid)) {
            cycle.warn("WIO has not been specified, so GPR writes are not allowed!", .{});
        }
    }

    if (cycle.signals.sr1wsrc == .self and !cycle.is_set(.sr1ri)) {
        cycle.warn("Expected `sr1ri` to be set when `sr1wsrc` is .self!", .{});
    }
    if (cycle.signals.sr1wsrc != .no_write and !cycle.is_set(.sr1wi)) {
        cycle.warn("Expected `sr1wi` to be set when SR1 is being written!", .{});
    }

    if (cycle.signals.sr2wsrc == .self and !cycle.is_set(.sr2ri)) {
        cycle.warn("Expected `sr2ri` to be set when `sr2wsrc` is .self!", .{});
    }
    if (cycle.signals.sr2wsrc != .no_write and !cycle.is_set(.sr2wi)) {
        cycle.warn("Expected `sr2wi` to be set when SR2 is being written!", .{});
    }

    switch (cycle.signals.flag_op) {
        .hold, .zn_from_l => {},
        .compute, .compute_no_set_z => cycle.validate_compute_mode(),
        .clear_bits, .set_bits => cycle.ensure_set(.constant),
        .load_zncv, .load => cycle.ensure_set(.lsrc),
    }

    switch (cycle.signals.seq_op) {
        .next_instruction, .fault_return => {
            cycle.flags.remove(.next_insn_loaded);
            if (cycle.next_func != null) {
                cycle.warn("next() is not allowed when decoding/executing a new instruction", .{});
            }
        },
        .next_uop, .next_uop_force_normal => if (cycle.next_func == null and cycle.signals.special != .trigger_fault) {
            cycle.warn("Expected a next()", .{});
        },
    }

    if (cycle.signals.seq_op == .next_instruction and cycle.signals.allow_int != .allow) {
        cycle.warn("Expected `allow_int` when `seq_op` is .next_instruction", .{});
    }

    if (cycle.signals.drw == .write) {
        cycle.flags.insert(.dr_valid);
        cycle.flags.remove(.initial_dr1_valid);
        cycle.flags.remove(.initial_dr2_valid);
    }

    if (cycle.signals.irw == .write) {
        cycle.flags.insert(.ir_valid);
        cycle.flags.remove(.initial_krio_valid);
    }

    switch (cycle.signals.special) {
        .none => {},
        .set_guard => {},
        .check_guard => {},
        .load_fucs_from_l => {},
        .load_rsn_from_l => {},
        .load_rsn_from_l__read_from_other_rsn, .read_from_other_rsn => {
            if (cycle.flags.contains(.disallow_read_from_other_rsn)) {
                cycle.warn("Cycle contains a read from both the default and alternate RSNs", .{});
            }
        },
        .load_rsn_from_l__write_to_other_rsn, .write_to_other_rsn => {
            if (cycle.flags.contains(.disallow_write_to_other_rsn)) {
                cycle.warn("Cycle contains a write to both the default and alternate RSNs", .{});
            }
        },
        .load_rsn_from_l__read_and_write_other_rsn, .read_and_write_other_rsn => {
            if (cycle.flags.contains(.disallow_read_from_other_rsn)) {
                cycle.warn("Cycle contains a read from both the default and alternate RSNs", .{});
            }
            if (cycle.flags.contains(.disallow_write_to_other_rsn)) {
                cycle.warn("Cycle contains a write to both the default and alternate RSNs", .{});
            }
        },
        .fault_on_overflow => {},
        .trigger_fault => {},
        .load_vabor_from_d => switch (cycle.signals.dsrc) {
            .l, .dr_ir => {},
            .system, .vabor => cycle.warn("Expected `dsrc` to be .l or .dr_ir", .{}),
        },
        _ => {
            cycle.warn("Unrecognized SPECIAL command: {d}", .{ @intFromEnum(cycle.signals.special) });
        },
    }
    
    cycle.flags.remove(.disallow_read_from_other_rsn);
    cycle.flags.remove(.disallow_write_to_other_rsn);
}

fn is_set(cycle: *Cycle, signal: arch.Control_Signal) bool {
    return cycle.assigned_signals.contains(signal);
}

fn ensure_set(cycle: *Cycle, signal: arch.Control_Signal) void {
    if (!cycle.is_set(signal)) {
        cycle.warn("Expected {s} to be assigned", .{ @tagName(signal) });
    }
}

fn validate_wi(cycle: *Cycle) void {
    if (!cycle.flags.contains(.ir_valid)) {
        cycle.warn("IR is undefined, so GPR writes are not allowed!", .{});
    }
}
fn validate_kr(cycle: *Cycle) void {
    if (!cycle.flags.contains(.ir_valid)) {
        cycle.warn("IR is undefined, so KR usage is not allowed!", .{});
    }
    if (!cycle.flags.contains(.initial_krio_valid)) {
        cycle.warn("KRIO has not been specified, so KR usage is not allowed!", .{});
    }
}
fn validate_krio(cycle: *Cycle) void {
    if (!cycle.flags.contains(.ir_valid)) {
        cycle.warn("IR is undefined, so KRIO usage is not allowed!", .{});
    }
    if (!cycle.flags.contains(.initial_krio_valid)) {
        cycle.warn("KRIO has not been specified, so KRIO usage is not allowed!", .{});
    }
}
fn validate_dr(cycle: *Cycle) void {
    if (!cycle.flags.contains(.dr_valid)) {
        cycle.warn("DR is undefined, so it cannot be used for K!", .{});
    }
}

fn validate_compute_mode(cycle: *Cycle) void {
    cycle.ensure_set(.unit);
    if (cycle.signals.unit == .none) {
        cycle.warn("Expected compute unit other than .none", .{});
    } else {
        cycle.ensure_set(.mode);
        cycle.ensure_set(.jsrc);
        cycle.ensure_set(.ksrc);
    }
}

fn validate_address(cycle: *Cycle) void {
    cycle.ensure_set(.vari);
    cycle.ensure_set(.vao_src);
    if (cycle.signals.vao_src == .constant) {
        cycle.ensure_set(.constant);
    }
    cycle.ensure_set(.at_op);
    cycle.ensure_set(.space);
    cycle.ensure_set(.dsrc);
}

fn set_control_signal(c: *Cycle, comptime signal: arch.Control_Signal, raw_value: anytype) void {
    const current_value = @field(c.signals, @tagName(signal));
    const T = @TypeOf(current_value);
    const value = @as(T, raw_value);
    switch (@typeInfo(T)) {
        .@"union" => if (c.is_set(signal)) {
            const Unsigned = std.meta.Int(.unsigned, @bitSizeOf(T));
            const current: Unsigned = @bitCast(current_value);
            const new: Unsigned = @bitCast(value);
            if (current != new) {
                if (@hasDecl(T, "format")) {
                    c.warn("Can't assign {f} to {s}; already has value {f}", .{ value, @tagName(signal), current_value });
                } else {
                    c.warn("Can't assign {} to {s}; already has value {}", .{ value, @tagName(signal), current_value });
                }
                return;
            }
        },
        else => if (c.is_set(signal) and current_value != value) {
            if (@hasDecl(T, "format")) {
                c.warn("Can't assign {f} to {s}; already has value {f}", .{ value, @tagName(signal), current_value });
            } else {
                c.warn("Can't assign {} to {s}; already has value {}", .{ value, @tagName(signal), current_value });
            }
            return;
        },
    }
    switch (signal) {
        .drw => switch (value) {
            .hold => {},
            .write => {
                // if this was for a load_next_insn() then this will be overwritten after set_control_signal returns.
                c.flags.remove(.next_insn_loaded);
            },
        },
        else => {},
    }
    @field(c.signals, @tagName(signal)) = value;
    c.assigned_signals.insert(signal);
}

fn require_read_from_default_rsn(c: *Cycle) void {
    c.flags.insert(.disallow_read_from_other_rsn);
    // var buf: [64]u8 = undefined;
    // var writer = std.fs.File.stderr().writer(&buf);
    // writer.interface.print("cycle: {s}:\n", .{ c.func_name }) catch @panic("IO Error");
    // writer.interface.print("    .disallow_read_from_other_rsn\n", .{}) catch @panic("IO Error");
    // std.debug.dumpCurrentStackTraceToWriter(null, &writer.interface) catch @panic("IO Error");
}

fn require_write_to_default_rsn(c: *Cycle) void {
    c.flags.insert(.disallow_write_to_other_rsn);
    // var buf: [64]u8 = undefined;
    // var writer = std.fs.File.stderr().writer(&buf);
    // writer.interface.print("cycle: {s}:\n", .{ c.func_name }) catch @panic("IO Error");
    // writer.interface.print("    .disallow_write_to_other_rsn\n", .{}) catch @panic("IO Error");
    // std.debug.dumpCurrentStackTraceToWriter(null, &writer.interface) catch @panic("IO Error");
}

fn require_read_from_other_rsn(c: *Cycle) void {
    switch (c.signals.special) {
        .read_from_other_rsn => {},
        .write_to_other_rsn => c.signals.special = .read_and_write_other_rsn,
        .read_and_write_other_rsn => {},
        .load_rsn_from_l => c.signals.special = .load_rsn_from_l__read_from_other_rsn,
        .load_rsn_from_l__read_from_other_rsn => {},
        .load_rsn_from_l__write_to_other_rsn => c.signals.special = .load_rsn_from_l__read_and_write_other_rsn,
        .load_rsn_from_l__read_and_write_other_rsn => {},
        else => c.set_control_signal(.special, .read_from_other_rsn),
    }
}

fn require_write_to_other_rsn(c: *Cycle) void {
    switch (c.signals.special) {
        .read_from_other_rsn => c.signals.special = .read_and_write_other_rsn,
        .write_to_other_rsn => {},
        .read_and_write_other_rsn => {},
        .load_rsn_from_l => c.signals.special = .load_rsn_from_l__write_to_other_rsn,
        .load_rsn_from_l__read_from_other_rsn => c.signals.special = .load_rsn_from_l__read_and_write_other_rsn,
        .load_rsn_from_l__write_to_other_rsn => {},
        .load_rsn_from_l__read_and_write_other_rsn => {},
        else => c.set_control_signal(.special, .write_to_other_rsn),
    }
}

fn require_load_rsn_from_l(c: *Cycle) void {
    switch (c.signals.special) {
        .read_from_other_rsn => c.signals.special = .load_rsn_from_l__read_from_other_rsn,
        .write_to_other_rsn => c.signals.special = .load_rsn_from_l__write_to_other_rsn,
        .read_and_write_other_rsn => c.signals.special = .load_rsn_from_l__read_and_write_other_rsn,
        .load_rsn_from_l => {},
        .load_rsn_from_l__read_from_other_rsn => {},
        .load_rsn_from_l__write_to_other_rsn => {},
        .load_rsn_from_l__read_and_write_other_rsn => {},
        else => c.set_control_signal(.special, .load_rsn_from_l),
    }
}

///////////////////
// compute units //
///////////////////

fn set_compute(c: *Cycle, comptime unit: arch.compute.Unit, mode: anytype) void {
    c.set_control_signal(.unit, unit);
    if (@TypeOf(mode) == @TypeOf(.enum_literal)) {
        switch (unit) {
            .none => @compileError("Don't use c.set_compute(.none, ...)"),
            .alu => c.set_control_signal(.mode, arch.compute.Mode { .alu = @field(arch.compute.Mode.ALU, @tagName(mode)) }),
            .shift => c.set_control_signal(.mode, arch.compute.Mode { .shift = @field(arch.compute.Mode.Shift, @tagName(mode)) }),
            .mult => c.set_control_signal(.mode, arch.compute.Mode { .mult = @field(arch.compute.Mode.Multiply, @tagName(mode)) }),
            .count, .extend => c.set_control_signal(.mode, arch.compute.Mode { .count_extend = @field(arch.compute.Mode.Count_Extend, @tagName(mode)) }),
        }
    } else switch (unit) {
        .count, .extend => c.set_control_signal(.mode, arch.compute.Mode { .count_extend = mode }),
        inline else => |u| c.set_control_signal(.mode, @unionInit(arch.compute.Mode, @tagName(u), mode)),
    }
}

fn compute_flags(c: *Cycle, freshness: Freshness, flags: Flags_Mode) void {
    switch (flags) {
        .no_flags => return,
        .no_flags__fault_on_overflow => {
            c.set_control_signal(.special, .fault_on_overflow);
            return;
        },
        .flags__fault_on_overflow => {
            c.set_control_signal(.special, .fault_on_overflow);
        },
        .flags => {},
    }
    switch (freshness) {
        .fresh => c.set_control_signal(.flag_op, .compute),
        .cont => c.set_control_signal(.flag_op, .compute_no_set_z),
    }
}

pub fn j_plus_k(c: *Cycle, freshness: Freshness, flags: Flags_Mode) void {
    const mode: arch.compute.Mode.ALU = .{
        .op = .j_plus_k,
        .use_carry_flag = freshness == .cont,
        .invert_cin = false,
    };
    c.set_compute(.alu, mode);
    c.compute_flags(freshness, flags);
}
pub fn j_minus_k(c: *Cycle, freshness: Freshness, flags: Flags_Mode) void {
    const mode: arch.compute.Mode.ALU = .{
        .op = .j_plus_not_k,
        .use_carry_flag = freshness == .cont,
        .invert_cin = freshness == .fresh,
    };
    c.set_compute(.alu, mode);
    c.compute_flags(freshness, flags);
}
pub fn k_minus_j(c: *Cycle, freshness: Freshness, flags: Flags_Mode) void {
    const mode: arch.compute.Mode.ALU = .{
        .op = .not_j_plus_k,
        .use_carry_flag = freshness == .cont,
        .invert_cin = freshness == .fresh,
    };
    c.set_compute(.alu, mode);
    c.compute_flags(freshness, flags);
}

pub fn j_logic_k(c: *Cycle, op: Logic_Op, freshness: Freshness, flags: Flags_Mode) void {
    c.set_compute(.alu, @as(arch.compute.Mode.ALU, switch (op) {
        .xor => .logic_xor,
        ._or => .logic_or,
        ._and => .logic_and,
    }));
    c.compute_flags(freshness, flags);
}

pub fn j_shift_k(c: *Cycle, op: Shift_Op, freshness: Freshness, flags: Flags_Mode) void {
    c.set_compute(.shift, @as(arch.compute.Mode.Shift, switch (op) {
        .shl => .shl,
        .shr => .shrl,
        .shlc => .shlc,
        .shrc => .shrc,
        .shrs => .shra,
    }));
    c.compute_flags(freshness, flags);
}

pub fn swap_j(c: *Cycle, op: Swap_Op, flags: Flags_Mode) void {
    c.set_compute(.shift, @as(arch.compute.Mode.Shift, switch (op) {
        .swap_bytes => .swap_bytes,
        .swap_halves => .swap_halves,
        .reverse_bytes => .reverse_bytes,
        .reverse_bits => .reverse_bits,
        .reverse_bits_in_halves => .reverse_bits_in_halves,
        .reverse_bits_in_bytes => .reverse_bits_in_bytes,
    }));
    c.compute_flags(.fresh, flags);
}

pub fn j_times_k(c: *Cycle,
    jt: arch.compute.Mode.Multiply.Signedness, kt: arch.compute.Mode.Multiply.Signedness,
    jh: arch.compute.Mode.Multiply.Half, kh: arch.compute.Mode.Multiply.Half,
    freshness: Freshness, flags: Flags_Mode
) void {
    const mode: arch.compute.Mode.Multiply = .{
        .j_type = jt,
        .k_type = kt,
        .j = jh,
        .k = kh,
        .shift_result = false,
    };
    c.set_compute(.mult, mode);
    c.compute_flags(freshness, flags);
}
pub fn j_times_k__shl16(c: *Cycle,
    jt: arch.compute.Mode.Multiply.Signedness, kt: arch.compute.Mode.Multiply.Signedness,
    jh: arch.compute.Mode.Multiply.Half, kh: arch.compute.Mode.Multiply.Half,
    freshness: Freshness, flags: Flags_Mode
) void {
    const mode: arch.compute.Mode.Multiply = .{
        .j_type = jt,
        .k_type = kt,
        .j = jh,
        .k = kh,
        .shift_result = true,
    };
    c.set_compute(.mult, mode);
    c.compute_flags(freshness, flags);
}

pub fn count_j(c: *Cycle, count_what: Bit_Count_Polarity, dir: Bit_Count_Direction, freshness: Freshness, flags: Flags_Mode) void {
    const mode: arch.compute.Mode.Count_Extend = switch (dir) {
        .all => switch (count_what) {
            .zeroes => .count_zeroes,
            .ones => .count_ones,
        },
        .leading => switch (count_what) {
            .zeroes => .count_leading_zeroes,
            .ones => .count_leading_ones,
        },
        .trailing => switch (count_what) {
            .zeroes => .count_trailing_zeroes,
            .ones => .count_trailing_ones,
        },
    };
    c.set_compute(.count, mode);
    c.compute_flags(freshness, flags);
}

pub fn j_ext_k(c: *Cycle, ext: Zero_Or_Sign_Extension, flags: Flags_Mode) void {
    const mode: arch.compute.Mode.Count_Extend = switch (ext) {
        .zx => .zero_extend_or_truncate,
        .sx => .sign_extend_or_truncate,
    };
    c.zero_to_j();
    c.set_compute(.extend, mode);
    c.compute_flags(.fresh, flags);
}

pub fn saturate_k(c: *Cycle, what: Bit_Count_Polarity, dir: Bit_Count_Direction, flags: Flags_Mode) void {
    const mode: arch.compute.Mode.Count_Extend = switch (dir) {
        .leading => switch (what) {
            .zeroes => .saturate_zeroes_left,
            .ones => .saturate_ones_left,
        },
        .trailing => switch (what) {
            .zeroes => .saturate_zeroes_right,
            .ones => .saturate_ones_right,
        },
        .all => {
            c.warn("Expected direction to be .left or .right for `saturate_k`", .{});
            return;
        },
    };
    c.zero_to_j();
    c.set_compute(.extend, mode);
    c.compute_flags(.fresh, flags);
}

pub fn enable_flags(c: *Cycle, flags: arch.reg.Flags.Writable) void {
    c.set_control_signal(.constant, arch.microcode.Constant.init_unsigned(@intCast(flags.raw())));
    c.set_control_signal(.flag_op, .set_bits);
}
pub fn disable_flags(c: *Cycle, flags: arch.reg.Flags.Writable) void {
    c.set_control_signal(.constant, arch.microcode.Constant.init_unsigned(@intCast(flags.raw())));
    c.set_control_signal(.flag_op, .clear_bits);
}

//////////////
// to J bus //
//////////////

pub fn zero_to_j(c: *Cycle) void {
    c.set_control_signal(.jsrc, .zero);
}

pub fn reg_to_j(c: *Cycle) void {
    c.set_control_signal(.jsrc, .jr);
    c.require_read_from_default_rsn();
}
pub fn reg_alt_to_j(c: *Cycle) void {
    c.set_control_signal(.jsrc, .jr);
    c.require_read_from_other_rsn();
}

pub fn sr_to_j(c: *Cycle, which: arch.reg.sr.Any_Index) void {
    if (which.to_sr1()) |sr1| {
        c.set_control_signal(.sr1ri, sr1);
        c.set_control_signal(.jsrc, .sr1);
    } else if (which.to_sr2()) |sr2| {
        c.set_control_signal(.sr2ri, sr2);
        c.set_control_signal(.jsrc, .sr2);
    } else unreachable;
    c.require_read_from_default_rsn();
}
pub fn sr_alt_to_j(c: *Cycle, which: arch.reg.sr.Any_Index) void {
    if (which.to_sr1()) |sr1| {
        c.set_control_signal(.sr1ri, sr1);
        c.set_control_signal(.jsrc, .sr1);
    } else if (which.to_sr2()) |sr2| {
        c.set_control_signal(.sr2ri, sr2);
        c.set_control_signal(.jsrc, .sr2);
    } else unreachable;
    c.require_read_from_other_rsn();
}

//////////////
// to K bus //
//////////////

pub fn zero_to_k(c: *Cycle) void {
    c.set_control_signal(.ksrc, .zero);
}

pub fn reg_to_k(c: *Cycle) void {
    c.set_control_signal(.ksrc, .kr);
    c.require_read_from_default_rsn();
}
pub fn reg_alt_to_k(c: *Cycle) void {
    c.set_control_signal(.ksrc, .kr);
    c.require_read_from_other_rsn();
}

pub fn sr_to_k(c: *Cycle, which: arch.reg.sr.Any_Index) void {
    if (which.to_sr1()) |sr1| {
        c.set_control_signal(.sr1ri, sr1);
        c.set_control_signal(.ksrc, .sr1);
    } else if (which.to_sr2()) |sr2| {
        c.set_control_signal(.sr2ri, sr2);
        c.set_control_signal(.ksrc, .sr2);
    } else unreachable;
    c.require_read_from_default_rsn();
}
pub fn sr_alt_to_k(c: *Cycle, which: arch.reg.sr.Any_Index) void {
    if (which.to_sr1()) |sr1| {
        c.set_control_signal(.sr1ri, sr1);
        c.set_control_signal(.ksrc, .sr1);
    } else if (which.to_sr2()) |sr2| {
        c.set_control_signal(.sr2ri, sr2);
        c.set_control_signal(.ksrc, .sr2);
    } else unreachable;
    c.require_read_from_other_rsn();
}

pub fn dr_i8_to_k(c: *Cycle, is_8b_opcode: bool) void {
    if (is_8b_opcode) {
        c.set_control_signal(.ksrc, .dr_byte_1_sx);
    } else {
        c.set_control_signal(.ksrc, .dr_byte_2_sx);
    }
}

pub fn dr_i16_to_k(c: *Cycle) void {
    c.set_control_signal(.ksrc, .dr_byte_21_sx);
}

pub fn literal_to_k(c: *Cycle, literal: i64) void {
    if (literal == 0) {
        c.zero_to_k();
        return;
    }

    if (c.flags.contains(.initial_krio_valid)) {
        if (c.initial_krio.raw() == literal) {
            c.krio_to_k(.zx);
            return;
        } else if (c.initial_krio.raw_signed() == literal) {
            c.krio_to_k(.sx);
            return;
        } else if (literal == (@as(u32, 1) << c.initial_krio.raw())) {
            c.krio_bit_to_k();
            return;
        } else if (literal == ~(@as(u32, 1) << c.initial_krio.raw())) {
            c.not_krio_bit_to_k();
            return;
        }
    }

    if (c.initial_dr.@"signed[15:8]"() == literal and c.flags.contains(.initial_dr1_valid)) {
        c.dr_i8_to_k(true);
        return;
    } else if (c.initial_dr.@"signed[23:16]"() == literal and c.flags.contains(.initial_dr2_valid)) {
        c.dr_i8_to_k(false);
        return;
    } else if (c.initial_dr.@"signed[23:8]"() == literal and c.flags.contains(.initial_dr1_valid) and c.flags.contains(.initial_dr2_valid)) {
        c.dr_i16_to_k();
        return;
    }

    if (literal >= arch.microcode.Constant.min and literal <= arch.microcode.Constant.max) {
        const constant = arch.microcode.Constant.init(@intCast(literal));
        c.set_control_signal(.constant, constant);
    } else {
        c.warn("Could not encode literal {} for K", .{ literal });
        c.set_control_signal(.constant, arch.microcode.Constant.init(0));
    }
    c.set_control_signal(.ksrc, .constant);
}

pub fn krio_to_k(c: *Cycle, ext: Zero_Or_Sign_Extension) void {
    c.set_control_signal(.ksrc, @as(arch.bus.K.Source, switch (ext) {
        .zx => .krio_zx,
        .sx => .krio_sx,
    }));
}

pub fn krio_bit_to_k(c: *Cycle) void {
    c.set_control_signal(.ksrc, .krio_bit);
}

pub fn not_krio_bit_to_k(c: *Cycle) void {
    c.set_control_signal(.ksrc, .krio_bit_inv);
}

//////////////
// to L bus //
//////////////

pub fn zero_to_l(c: *Cycle) void {
    if (!c.is_set(.jsrc)) c.zero_to_j();
    if (!c.is_set(.ksrc)) c.zero_to_k();
    c.set_compute(.alu, .all_zeroes);
    c.set_control_signal(.lsrc, .compute_or_d);
}

pub fn literal_to_l(c: *Cycle, literal: i16) void {
    if (literal == 0) {
        c.zero_to_l();
    } else if (literal == -1) {
        if (!c.is_set(.jsrc)) c.zero_to_j();
        if (!c.is_set(.ksrc)) c.zero_to_k();
        c.set_compute(.alu, .all_ones);
        c.set_control_signal(.lsrc, .compute_or_d);
    } else {
        c.literal_to_k(literal);
        c.k_to_l();
    }
}

pub fn last_d_to_l(c: *Cycle) void {
    c.set_control_signal(.lsrc, .last_d);
}

pub fn status_to_l(c: *Cycle) void {
    c.set_control_signal(.lsrc, .status);
}

pub fn flags_to_l(c: *Cycle) void {
    c.set_control_signal(.lsrc, .flags);
}

pub fn vabor_to_l(c: *Cycle) void {
    c.set_control_signal(.lsrc, .compute_or_d);
    c.set_control_signal(.unit, .none);
    c.set_control_signal(.dsrc, .vabor);
}

pub fn reg_to_j_to_l(c: *Cycle) void {
    c.reg_to_j();
    c.j_to_l();
}
pub fn reg_alt_to_j_to_l(c: *Cycle) void {
    c.reg_alt_to_j();
    c.j_to_l();
}

pub fn reg_to_k_to_l(c: *Cycle) void {
    c.reg_to_k();
    c.k_to_l();
}
pub fn reg_alt_to_k_to_l(c: *Cycle) void {
    c.reg_alt_to_k();
    c.k_to_l();
}

pub fn sr_to_l(c: *Cycle, which: arch.reg.sr.Any_Index) void {
    c.sr_to_k(which);   
    c.k_to_l();
}
pub fn sr_alt_to_l(c: *Cycle, which: arch.reg.sr.Any_Index) void {
    c.sr_alt_to_k(which);   
    c.k_to_l();
}

pub fn j_to_l(c: *Cycle) void {
    c.zero_to_k();
    c.j_logic_k_to_l(._or, .fresh, .no_flags);
}

pub fn k_to_l(c: *Cycle) void {
    c.zero_to_j();
    c.j_logic_k_to_l(._or, .fresh, .no_flags);
}

pub fn dr_i8_to_l(c: *Cycle, is_8b_opcode: bool) void {
    c.dr_i8_to_k(is_8b_opcode);
    c.k_to_l();
}

pub fn dr_i16_to_l(c: *Cycle) void {
    c.dr_i16_to_k();
    c.k_to_l();
}

pub fn krio_to_l(c: *Cycle) void {
    c.krio_to_k();
    c.k_to_l();
}

pub fn krio_bit_to_l(c: *Cycle) void {
    c.krio_bit_to_k();
    c.k_to_l();
}

pub fn not_krio_bit_to_l(c: *Cycle) void {
    c.not_krio_bit_to_k();
    c.k_to_l();
}

pub fn j_plus_k_to_l(c: *Cycle, freshness: Freshness, flags: Flags_Mode) void {
    c.j_plus_k(freshness, flags);
    c.set_control_signal(.lsrc, .compute_or_d);
}
pub fn j_minus_k_to_l(c: *Cycle, freshness: Freshness, flags: Flags_Mode) void {
    c.j_minus_k(freshness, flags);
    c.set_control_signal(.lsrc, .compute_or_d);
}
pub fn k_minus_j_to_l(c: *Cycle, freshness: Freshness, flags: Flags_Mode) void {
    c.k_minus_j(freshness, flags);
    c.set_control_signal(.lsrc, .compute_or_d);
}

pub fn j_logic_k_to_l(c: *Cycle, op: Logic_Op, freshness: Freshness, flags: Flags_Mode) void {
    c.j_logic_k(op, freshness, flags);
    c.set_control_signal(.lsrc, .compute_or_d);
}

pub fn j_shift_k_to_l(c: *Cycle, op: Shift_Op, freshness: Freshness, flags: Flags_Mode) void {
    c.j_shift_k(op, freshness, flags);
    c.set_control_signal(.lsrc, .compute_or_d);
}

pub fn swap_j_to_l(c: *Cycle, op: Swap_Op, flags: Flags_Mode) void {
    c.swap_j(op, flags);
    c.set_control_signal(.lsrc, .compute_or_d);
}

pub fn jl_times_kl_to_l(c: *Cycle, js: arch.Multiply_Mode.Signedness, ks: arch.Multiply_Mode.Signedness, freshness: Freshness, flags: Flags_Mode) void {
    c.j_times_k(js, ks, .lsb, .lsb, freshness, flags);
    c.set_control_signal(.lsrc, .compute_or_d);
}
pub fn jl_times_kh_to_l(c: *Cycle, js: arch.Multiply_Mode.Signedness, ks: arch.Multiply_Mode.Signedness, freshness: Freshness, flags: Flags_Mode) void {
    c.j_times_k(js, ks, .lsb, .msb, freshness, flags);
    c.set_control_signal(.lsrc, .compute_or_d);
}
pub fn jh_times_kl_to_l(c: *Cycle, js: arch.Multiply_Mode.Signedness, ks: arch.Multiply_Mode.Signedness, freshness: Freshness, flags: Flags_Mode) void {
    c.j_times_k(js, ks, .msb, .lsb, freshness, flags);
    c.set_control_signal(.lsrc, .compute_or_d);
}
pub fn jh_times_kh_to_l(c: *Cycle, js: arch.Multiply_Mode.Signedness, ks: arch.Multiply_Mode.Signedness, freshness: Freshness, flags: Flags_Mode) void {
    c.j_times_k(js, ks, .msb, .msb, freshness, flags);
    c.set_control_signal(.lsrc, .compute_or_d);
}
pub fn jl_times_kh_shl16_to_l(c: *Cycle, js: arch.Multiply_Mode.Signedness, ks: arch.Multiply_Mode.Signedness, freshness: Freshness, flags: Flags_Mode) void {
    c.j_times_k__shl16(js, ks, .lsb, .msb, freshness, flags);
    c.set_control_signal(.lsrc, .compute_or_d);
}
pub fn jh_times_kl_shl16_to_l(c: *Cycle, js: arch.Multiply_Mode.Signedness, ks: arch.Multiply_Mode.Signedness, freshness: Freshness, flags: Flags_Mode) void {
    c.j_times_k__shl16(js, ks, .msb, .lsb, freshness, flags);
    c.set_control_signal(.lsrc, .compute_or_d);
}

pub fn count_j_to_l(c: *Cycle, count_what: Bit_Count_Polarity, dir: Bit_Count_Direction, freshness: Freshness, flags: Flags_Mode) void {
    c.count_j(count_what, dir, freshness, flags);
    c.set_control_signal(.lsrc, .compute_or_d);
}

pub fn j_ext_k_to_l(c: *Cycle, ext: Zero_Or_Sign_Extension, flags: Flags_Mode) void {
    c.j_ext_k(ext, flags);
    c.set_control_signal(.lsrc, .compute_or_d);
}

pub fn saturate_k_to_l(c: *Cycle, what: Bit_Count_Polarity, dir: Bit_Count_Direction, flags: Flags_Mode) void {
    c.saturate_k(what, dir, flags);
    c.set_control_signal(.lsrc, .compute_or_d);
}

pub fn dr_to_l(c: *Cycle) void {
    c.set_control_signal(.at_op, .none);
    c.set_control_signal(.dsrc, .dr_ir);
    c.set_control_signal(.drw, .write);
    c.d_to_l();
}

pub fn dr_to_ir(c: *Cycle) void {
    c.set_control_signal(.irw, .write);
}

pub fn ir_to_l(c: *Cycle) void {
    c.set_control_signal(.at_op, .none);
    c.set_control_signal(.dsrc, .dr_ir);
    c.set_control_signal(.drw, .hold);
    c.d_to_l();
}

pub fn d_to_l(c: *Cycle) void {
    c.set_control_signal(.unit, .none);
    c.set_control_signal(.lsrc, .compute_or_d);
}

/////////////////////
// Register Writes //
/////////////////////

pub fn l_to_reg(c: *Cycle, set_ti: bool) void {
    c.set_control_signal(.gprw, .write);
    if (set_ti) c.wi_to_ti();
    c.require_write_to_default_rsn();
}
pub fn l_to_reg_alt(c: *Cycle) void {
    c.set_control_signal(.gprw, .write);
    c.require_write_to_other_rsn();
}

pub fn l_to_sr(c: *Cycle, which: arch.reg.sr.Any_Index) void {
    if (which.to_sr1()) |sr1| {
        c.set_control_signal(.sr1wi, sr1);
        c.set_control_signal(.sr1wsrc, .l);
    } else if (which.to_sr2()) |sr2| {
        c.set_control_signal(.sr2wi, sr2);
        c.set_control_signal(.sr2wsrc, .l);
    } else {
        unreachable;
    }
    c.require_write_to_default_rsn();
}
pub fn l_to_sr_alt(c: *Cycle, which: arch.reg.sr.Any_Index) void {
    if (which.to_sr1()) |sr1| {
        c.set_control_signal(.sr1wi, sr1);
        c.set_control_signal(.sr1wsrc, .l);
    } else if (which.to_sr2()) |sr2| {
        c.set_control_signal(.sr2wi, sr2);
        c.set_control_signal(.sr2wsrc, .l);
    } else {
        unreachable;
    }
    c.require_write_to_other_rsn();
}

pub fn virtual_address_to_sr(c: *Cycle, which: arch.reg.sr.Any_Index) void {
    if (which.to_sr1()) |sr1| {
        c.set_control_signal(.sr1wi, sr1);
        c.set_control_signal(.sr1wsrc, .virtual_addr);
    } else if (which.to_sr2()) |sr2| {
        c.set_control_signal(.sr2wi, sr2);
        c.set_control_signal(.sr2wsrc, .virtual_addr);
    }
    c.require_write_to_default_rsn();
}
pub fn virtual_address_to_sr_alt(c: *Cycle, which: arch.reg.sr.Any_Index) void {
    if (which.to_sr1()) |sr1| {
        c.set_control_signal(.sr1wi, sr1);
        c.set_control_signal(.sr1wsrc, .virtual_addr);
    } else if (which.to_sr2()) |sr2| {
        c.set_control_signal(.sr2wi, sr2);
        c.set_control_signal(.sr2wsrc, .virtual_addr);
    }
    c.require_write_to_other_rsn();
}

pub fn sr1_to_sr1(c: *Cycle, src_index: arch.reg.sr1.Index, dest_index: arch.reg.sr1.Index) void {
    c.set_control_signal(.sr1ri, src_index);
    c.set_control_signal(.sr1wi, dest_index);
    c.set_control_signal(.sr1wsrc, .self);
    c.require_read_from_default_rsn();
    c.require_write_to_default_rsn();
}
pub fn sr1_to_sr1_alt(c: *Cycle, src_index: arch.reg.sr1.Index, dest_index: arch.reg.sr1.Index) void {
    c.set_control_signal(.sr1ri, src_index);
    c.set_control_signal(.sr1wi, dest_index);
    c.set_control_signal(.sr1wsrc, .self);
    c.require_read_from_default_rsn();
    c.require_write_to_other_rsn();
}
pub fn sr1_alt_to_sr1(c: *Cycle, src_index: arch.reg.sr1.Index, dest_index: arch.reg.sr1.Index) void {
    c.set_control_signal(.sr1ri, src_index);
    c.set_control_signal(.sr1wi, dest_index);
    c.set_control_signal(.sr1wsrc, .self);
    c.require_read_from_other_rsn();
    c.require_write_to_default_rsn();
}
pub fn sr1_alt_to_sr1_alt(c: *Cycle, src_index: arch.reg.sr1.Index, dest_index: arch.reg.sr1.Index) void {
    c.set_control_signal(.sr1ri, src_index);
    c.set_control_signal(.sr1wi, dest_index);
    c.set_control_signal(.sr1wsrc, .self);
    c.require_read_from_other_rsn();
    c.require_write_to_other_rsn();
}

pub fn sr2_to_sr2(c: *Cycle, src_index: arch.reg.sr2.Index, dest_index: arch.reg.sr2.Index) void {
    c.set_control_signal(.sr2ri, src_index);
    c.set_control_signal(.sr2wi, dest_index);
    c.set_control_signal(.sr2wsrc, .self);
    c.require_read_from_default_rsn();
    c.require_write_to_default_rsn();
}
pub fn sr2_to_sr2_alt(c: *Cycle, src_index: arch.reg.sr2.Index, dest_index: arch.reg.sr2.Index) void {
    c.set_control_signal(.sr2ri, src_index);
    c.set_control_signal(.sr2wi, dest_index);
    c.set_control_signal(.sr2wsrc, .self);
    c.require_read_from_default_rsn();
    c.require_write_to_other_rsn();
}
pub fn sr2_alt_to_sr2(c: *Cycle, src_index: arch.reg.sr2.Index, dest_index: arch.reg.sr2.Index) void {
    c.set_control_signal(.sr2ri, src_index);
    c.set_control_signal(.sr2wi, dest_index);
    c.set_control_signal(.sr2wsrc, .self);
    c.require_read_from_other_rsn();
    c.require_write_to_default_rsn();
}
pub fn sr2_alt_to_sr2_alt(c: *Cycle, src_index: arch.reg.sr2.Index, dest_index: arch.reg.sr2.Index) void {
    c.set_control_signal(.sr2ri, src_index);
    c.set_control_signal(.sr2wi, dest_index);
    c.set_control_signal(.sr2wsrc, .self);
    c.require_read_from_other_rsn();
    c.require_write_to_other_rsn();
}

pub fn l_to_zncv(c: *Cycle) void {
    c.set_control_signal(.flag_op, .load_zncv);
}

/// N.B. this also updates TI!
pub fn l_to_flags(c: *Cycle) void {
    c.set_control_signal(.flag_op, .load);
}

pub fn zn_flags_from_l(c: *Cycle) void {
    c.set_control_signal(.flag_op, .zn_from_l);
}

pub fn l_to_dr(c: *Cycle) void {
    c.set_control_signal(.at_op, .none);
    c.set_control_signal(.dsrc, .l);
    c.set_control_signal(.drw, .write);
}

pub fn l_to_rsn(c: *Cycle) void {
    c.require_load_rsn_from_l();
}

pub fn l_to_fucs(c: *Cycle) void {
    c.set_control_signal(.special, .load_fucs_from_l);
}

pub fn reload_asn(c: *Cycle) void {
    c.sr2_to_sr2(.asn, .asn);
}
pub fn reload_asn_alt(c: *Cycle) void {
    c.sr2_alt_to_sr2_alt(.asn, .asn);
}

pub fn l_to_vabor(c: *Cycle) void {
    c.set_control_signal(.dsrc, .l);
    c.set_control_signal(.special, .load_vabor_from_d);
}

/////////////////////
// Addresses & Bus //
/////////////////////

pub fn update_address_translation_from_l(c: *Cycle, base: arch.addr.Virtual.Base, group: arch.addr.translation.Entry.Group) void {
    c.address(base, 0);
    c.set_control_signal(.at_op, .update);
    c.set_control_signal(.space, @as(arch.addr.Space, switch (group) {
        .data_read => .physical,
        .data_write => .data,
        .stack => .stack,
        .insn => .insn,
    }));
    c.set_control_signal(.dsrc, .l);
    c.set_control_signal(.width, .@"32b");
}

pub fn invalidate_address_translation_from_l(c: *Cycle, base: arch.addr.Virtual.Base, group: arch.addr.translation.Entry.Group) void {
    c.address(base, 0);
    c.set_control_signal(.at_op, .invalidate);
    c.set_control_signal(.space, @as(arch.addr.Space, switch (group) {
        .data_read => .physical,
        .data_write => .data,
        .stack => .stack,
        .insn => .insn,
    }));
    c.set_control_signal(.dsrc, .l);
    c.set_control_signal(.width, .@"32b");
}

pub fn address(c: *Cycle, base: arch.addr.Virtual.Base, offset: anytype) void {
    c.set_control_signal(.vari, base);
    switch (@typeInfo(@TypeOf(offset))) {
        .int, .comptime_int => {
            if (offset == 0) {
                c.set_control_signal(.vao_src, .zero);
            } else if (c.initial_dr.@"signed[23:16]"() == offset and c.flags.contains(.initial_dr2_valid)) {
                c.set_control_signal(.vao_src, .i8_from_dr);
            } else if (c.initial_dr.@"signed[23:8]"() == offset and c.flags.contains(.initial_dr1_valid) and c.flags.contains(.initial_dr2_valid)) {
                c.set_control_signal(.vao_src, .i16_from_dr);
            } else if (offset >= arch.microcode.Constant.min and offset <= arch.microcode.Constant.max) {
                const constant = arch.microcode.Constant.init(@intCast(offset));
                c.set_control_signal(.vao_src, .constant);
                c.set_control_signal(.constant, constant);
            } else {
                c.warn("Could not encode address offset {}", .{ offset });
                c.set_control_signal(.vao_src, .constant);
                c.set_control_signal(.constant, arch.microcode.Constant.init(0));
            }
        },
        .enum_literal => {
            c.set_control_signal(.vao_src, offset);
        },
        else => unreachable,
    }
}

pub fn read_to_d(c: *Cycle, base: arch.addr.Virtual.Base, offset: anytype, width: arch.bus.D.Width, space: arch.addr.Space) void {
    c.address(base, offset);
    c.set_control_signal(.at_op, .translate);
    c.set_control_signal(.space, space);
    c.set_control_signal(.width, width);
    c.set_control_signal(.dsrc, .system);
    if (base == .ip and space == .insn and c.signals.sr2wi != .ip and c.signals.sr2wi != .next_ip and @typeInfo(@TypeOf(offset)) != .enum_literal) {
        if (c.encoding_len) |len| {
            var end_offset: i64 = offset;
            end_offset += switch (width) {
                .@"8b" => 1,
                .@"16b" => 2,
                .@"24b" => 3,
                .@"32b" => 4,
            };
            if (end_offset > len) {
                c.warn("IP-relative {s} read at offset {} is not contained within the expected encoding length of {} ", .{ @tagName(width), offset, len });
            }
        } else {
            c.warn("Cycle performs an IP-relative read, but there is no instruction encoding corresponding to this microcode sequence", .{});
        }
    }
}

pub fn read_to_dr(c: *Cycle, base: arch.addr.Virtual.Base, offset: anytype, width: arch.bus.D.Width, space: arch.addr.Space) void {
    c.read_to_d(base, offset, width, space);
    c.set_control_signal(.drw, .write);
}

pub fn ip_read_to_d(c: *Cycle, offset: anytype, width: arch.bus.D.Width) void {
    c.read_to_d(.ip, offset, width, .insn);
}

pub fn ip_read_to_dr(c: *Cycle, offset: anytype, width: arch.bus.D.Width) void {
    c.read_to_dr(.ip, offset, width, .insn);
}

pub fn ip_read_24b_to_dr_ir(c: *Cycle, offset: anytype) void {
    c.ip_read_to_dr(offset, .@"24b");
    c.dr_to_ir();
}

pub fn write_from_l(c: *Cycle, base: arch.addr.Virtual.Base, offset: anytype, width: arch.bus.D.Width, space: arch.addr.Space) void {
    c.address(base, offset);
    c.set_control_signal(.at_op, .translate);
    c.set_control_signal(.space, space);
    c.set_control_signal(.width, width);
    c.set_control_signal(.dsrc, .l);
}

pub fn write_from_dr(c: *Cycle, base: arch.addr.Virtual.Base, offset: anytype, width: arch.bus.D.Width, space: arch.addr.Space) void {
    c.address(base, offset);
    c.set_control_signal(.at_op, .translate);
    c.set_control_signal(.space, space);
    c.set_control_signal(.width, width);
    c.set_control_signal(.dsrc, .dr_ir);
    c.set_control_signal(.drw, .write);
}

pub fn load_next_insn(c: *Cycle) void {
    if (c.encoding_len) |offset| {
        c.set_control_signal(.sr2wi, .next_ip);
        c.set_control_signal(.sr2wsrc, .virtual_addr);
        c.ip_read_to_dr(offset, .@"24b");
        c.assume_next_insn_loaded();
    } else {
        c.warn("Cycle loads next instruction, but there is no instruction encoding corresponding to this microcode sequence", .{});
    }
}

pub fn assume_next_insn_loaded(c: *Cycle) void {
    c.flags.insert(.next_insn_loaded);
}

pub fn exec_next_insn(c: *Cycle) void {
    if (c.instruction_signature) |signature| {
        if (signature.mnemonic.is_unconditional_branch()) {
            c.warn("Cycle executes next instruction, but this mnemonic/suffix indicates it should be an unconditional branch", .{});
        } else if (signature.mnemonic.is_unconditional_call()) {
            c.warn("Cycle executes next instruction, but this mnemonic/suffix indicates it should be an unconditional call", .{});
        }
    } else {
        c.warn("Cycle executes next instruction, but no instruction signature is available", .{});
    }
    if (!c.flags.contains(.next_insn_loaded)) {
        c.warn("Cycle executes next instruction, but it has not been loaded yet, or has been clobbered", .{});
    }

    if (c.is_set(.sr2ri) and c.signals.sr2ri != .next_ip) {
        c.address(.next_ip, 0);
        c.set_control_signal(.sr2wsrc, .virtual_addr);
    } else {
        c.set_control_signal(.sr2ri, .next_ip);
        c.set_control_signal(.sr2wsrc, .self);
    }
    c.set_control_signal(.sr2wi, .ip);
    c.set_control_signal(.irw, .write);
    c.exec_ir_insn();
}

pub fn load_and_exec_next_insn(c: *Cycle) void {
    if (c.encoding_len) |offset| {
        c.branch(.ip, offset);
    } else {
        c.warn("Cycle loads next instruction, but there is no instruction encoding corresponding to this microcode sequence", .{});
    }
}

pub fn call(c: *Cycle, base: arch.addr.Virtual.Base, offset: anytype) void {
    c.sr_to_j(.ip);
    c.literal_to_k(c.encoding_len.?);
    c.j_plus_k_to_l(.fresh, .no_flags);
    c.l_to_sr(.rp);
    c.branch(base, offset);
}

pub fn branch(c: *Cycle, base: arch.addr.Virtual.Base, offset: anytype) void {
    if (base != .ip or @typeInfo(@TypeOf(offset)) != .enum_literal and offset != 0 or !c.is_set(.sr2wi)) {
        c.set_control_signal(.sr2wi, .ip);
        c.set_control_signal(.sr2wsrc, .virtual_addr);
    }
    c.read_to_dr(base, offset, .@"24b", .insn);
    c.set_control_signal(.irw, .write);
    c.exec_ir_insn();
}

pub fn exec_ir_insn(c: *Cycle) void {
    c.allow_interrupt();
    c.set_control_signal(.seq_op, .next_instruction);
}

pub fn wi_to_ti(c: *Cycle) void {
    c.set_control_signal(.tiw, .write);
}

pub fn next(c: *Cycle, func: *const anyopaque) void {
    std.debug.assert(c.next_func == null);
    c.next_func = func;
}

pub fn force_normal_execution(c: *Cycle, func: *const anyopaque) void {
    c.set_control_signal(.seq_op, .next_uop_force_normal);
    c.next(func);
}

pub fn allow_interrupt(c: *Cycle) void {
    c.set_control_signal(.allow_int, .allow);
}

pub fn fault_return(c: *Cycle) void {
    c.set_control_signal(.seq_op, .fault_return);
}

fn trigger_fault(c: *Cycle, slot: arch.microcode.Slot) void {
    c.set_control_signal(.special, .trigger_fault);
    c.set_control_signal(.seq_op, .next_uop);
    c.set_control_signal(.next, slot);
}

pub fn illegal_instruction(c: *Cycle) void {
    c.trigger_fault(.instruction_protection_fault);
}

pub fn invalid_instruction(c: *Cycle) void {
    c.trigger_fault(.invalid_instruction_fault);
}

pub fn overflow(c: *Cycle) void {
    c.trigger_fault(.overflow_fault);
}

pub fn set_guard(c: *Cycle) void {
    c.set_control_signal(.special, .set_guard);
}

pub fn check_guard(c: *Cycle) void {
    c.set_control_signal(.special, .check_guard);
}

pub fn power_mode(c: *Cycle, pm: arch.Power_Mode) void {
    c.set_control_signal(.power, pm);
}

pub const Flags_Mode = enum {
    no_flags,
    flags,
    no_flags__fault_on_overflow,
    flags__fault_on_overflow,
};

pub const Freshness = enum {
    fresh,
    cont,
};

pub const Zero_Or_Sign_Extension = enum {
    zx,
    sx,
};

pub const Logic_Op = enum {
    xor,
    _or,
    _and,
};

pub const Shift_Op = enum {
    shl,
    shr,
    shrc,
    shlc,
    shrs,
};

pub const Swap_Op = enum {
    swap_bytes,
    swap_halves,
    reverse_bytes,
    reverse_bits,
    reverse_bits_in_halves,
    reverse_bits_in_bytes,
};

pub const Bit_Count_Polarity = enum {
    zeroes,
    ones,
};

pub const Bit_Count_Direction = enum {
    all,
    leading,
    trailing,
};

const log = std.log.scoped(.cycle);

const Cycle = @This();
const Microcode_Builder = @import("Microcode_Builder.zig");
const isa = @import("isa");
const arch = @import("arch");
const bits = @import("bits");
const std = @import("std");
