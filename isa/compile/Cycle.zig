// The following fields are only guaranteed to be accurate during microcode processing;
// They aren't considered when deduplicating cycles, so after cycle deduplication they will only reflect one of the usages of the cycle.
func_name: []const u8,
instruction_signature: ?isa.Instruction.Signature,
initial_dr: arch.DR,
initial_krio: arch.K.Read_Index_Offset,
initial_wio: arch.Write_Index_Offset,
encoding_len: ?Encoded_Instruction.Length_Type,
flags: std.EnumSet(Cycle_Flags),
assigned_signals: std.EnumSet(Control_Signal),

// These fields *are* considered when deduplicating:
signals: Control_Signals,
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
};

pub fn init(
    name: []const u8,
    instruction_signature: ?isa.Instruction.Signature,
    initial_dr: arch.DR,
    initial_krio: arch.K.Read_Index_Offset,
    initial_wio: arch.Write_Index_Offset,
    encoding_len: ?Encoded_Instruction.Length_Type,
    flags: std.EnumSet(Cycle_Flags)
) Cycle {
    return .{
        .func_name = name,
        .instruction_signature = instruction_signature,
        .initial_dr = initial_dr,
        .initial_krio = initial_krio,
        .initial_wio = initial_wio,
        .encoding_len = encoding_len,
        .flags = flags,
        .signals = std.mem.zeroInit(Control_Signals, .{
            .unit = .alu,
            .mode = .{ .alu = arch.ALU_Mode.all_zeroes },
            .special = .none,
            .atop = .none,
            .tiw = false,
            .gprw = false,
            .drw = false,
            .irw = false,
            .sr1wsrc = .no_write,
            .sr2wsrc = .no_write,
            .statop = .hold,
            .dir = .none,
            .width = .@"32b",
            .allowint = false,
            .seqop = .next_uop,
            .lsrc = .status,
        }),
        .assigned_signals = .{},
        .next_func = null,
        .next_slot = null,
    };
}

fn warn(cycle: *Cycle, comptime format: []const u8, args: anytype) void {
    var stderr = std.io.getStdErr().writer();
    log.warn("{s}: ", .{ cycle.func_name });
    stderr.writeAll("    ") catch @panic("IO Error");
    stderr.print(format, args) catch @panic("IO Error");
    stderr.writeAll("\n") catch @panic("IO Error");
    @panic("cycle error");
}

pub fn finish(cycle: *Cycle) void {
    switch (cycle.signals.atop) {
        .none => {},
        .translate => {
            cycle.validate_address();
            cycle.ensure_set(.width);
        },
        .update, .invalidate => {
            cycle.validate_address();
        },
    }

    if (cycle.signals.dir == .write_from_dr_ir) {
        cycle.ensure_set(.drw);
        if (cycle.signals.atop != .translate and cycle.signals.lsrc != .d) {
            cycle.warn("DR value is written to D, but there is no bus transaction happening, and D is not being read to L", .{});
        }
    }

    if (cycle.signals.drw) {
        switch (cycle.signals.dir) {
            .write_from_l, .write_from_dr_ir => {},
            .none => cycle.warn("Expected `dir` to be set", .{}),
            .read => {
                cycle.validate_address();
                if (cycle.signals.atop != .translate) cycle.warn("Expected `atop` to be .translate", .{});
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
        .vao => cycle.ensure_set(.vao),
        .krio, .krio_bit, .krio_bit_inv => cycle.validate_krio(),
        .kr => cycle.validate_kr(),
        .sr1 => cycle.ensure_set(.sr1ri),
        .sr2 => cycle.ensure_set(.sr2ri),
        .dr_byte_1_sx, .dr_byte_2_sx, .dr_byte_21_sx => {
            cycle.validate_dr();
        },
    }

    switch (cycle.signals.lsrc) {
        .alu => cycle.validate_compute_mode(.alu),
        .shift => cycle.validate_compute_mode(.shift),
        .mult => cycle.validate_compute_mode(.mult),
        .count => cycle.validate_compute_mode(.count_extend),
        .ext => cycle.validate_compute_mode(.count_extend),
        .at_info, .status => {},
        .d => cycle.validate_bus_read(null),
    }

    if (cycle.signals.gprw or cycle.signals.tiw) {
        cycle.validate_wi();
        if (cycle.signals.tiw and !cycle.flags.contains(.initial_wio_valid)) {
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

    switch (cycle.signals.statop) {
        .hold, .load_zncv, .load_zncva_ti, .clear_a, .set_a, .zn_from_l => {},
        .compute, .compute_no_set_z => cycle.validate_compute_mode(null),
    }

    switch (cycle.signals.seqop) {
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

    if (cycle.signals.seqop == .next_instruction and !cycle.signals.allowint) {
        cycle.warn("Expected `allowint` when `seqop` is .next_instruction", .{});
    }

    if (cycle.signals.drw) {
        cycle.flags.insert(.dr_valid);
        cycle.flags.remove(.initial_dr1_valid);
        cycle.flags.remove(.initial_dr2_valid);
    }

    if (cycle.signals.irw) {
        cycle.flags.insert(.ir_valid);
        cycle.flags.remove(.initial_krio_valid);
    }
}

fn is_set(cycle: *Cycle, signal: Control_Signal) bool {
    return cycle.assigned_signals.contains(signal);
}

fn ensure_set(cycle: *Cycle, signal: Control_Signal) void {
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

fn validate_compute_mode(cycle: *Cycle, maybe_unit: ?arch.Compute_Unit) void {
    cycle.ensure_set(.unit);
    cycle.ensure_set(.mode);
    cycle.ensure_set(.jsrc);
    cycle.ensure_set(.ksrc);

    if (maybe_unit) |unit| {
        if (cycle.signals.unit != unit) {
            cycle.warn("Expected compute unit .{s} but found .{s}", .{ @tagName(unit), @tagName(cycle.signals.unit) });
        }
    }
}

fn validate_address(cycle: *Cycle) void {
    cycle.ensure_set(.vari);
    cycle.ensure_set(.vao);
    cycle.ensure_set(.atop);
    cycle.ensure_set(.vaspace);
    cycle.ensure_set(.dir);
}

fn validate_bus_read(cycle: *Cycle, width: ?arch.D.Width) void {
    switch (cycle.signals.dir) {
        .write_from_dr_ir => {},
        .none, .write_from_l => cycle.warn("Expected `dir` to be .read", .{}),
        .read => {
            cycle.validate_address();
            if (cycle.signals.atop != .translate) cycle.warn("Expected `atop` to be .translate", .{});
            if (width) |w| {
                if (w != cycle.signals.width) cycle.warn("Expected `width` to be {}", .{ w });
            }
        },
    }
}

fn set_control_signal(c: *Cycle, comptime signal: Control_Signal, raw_value: anytype) void {
    const current_value = @field(c.signals, @tagName(signal));
    const T = @TypeOf(current_value);
    const value = @as(T, raw_value);
    switch (@typeInfo(T)) {
        .Union => if (c.is_set(signal)) {
            const Unsigned = std.meta.Int(.unsigned, @bitSizeOf(T));
            const current: Unsigned = @bitCast(current_value);
            const new: Unsigned = @bitCast(value);
            if (current != new) {
                c.warn("Can't assign {} to {s}; already has value {}", .{ value, @tagName(signal), current_value });
                return;
            }
        },
        else => if (c.is_set(signal) and current_value != value) {
            c.warn("Can't assign {} to {s}; already has value {}", .{ value, @tagName(signal), current_value });
            return;
        },
    }
    switch (signal) {
        .drw => if (value) {
            // if this was for a load_next_insn() then this will be overwritten after set_control_signal returns.
            c.flags.remove(.next_insn_loaded);
        },
        else => {},
    }
    @field(c.signals, @tagName(signal)) = value;
    c.assigned_signals.insert(signal);
}

///////////////////
// compute units //
///////////////////

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
        .fresh => c.set_control_signal(.statop, .compute),
        .cont => c.set_control_signal(.statop, .compute_no_set_z),
    }
}

pub fn j_plus_k(c: *Cycle, freshness: Freshness, flags: Flags_Mode) void {
    const mode: arch.ALU_Mode = .{
        .op = .j_plus_k,
        .use_stat_c = freshness == .cont,
        .invert_cin = false,
    };
    c.set_control_signal(.unit, .alu);
    c.set_control_signal(.mode, .{ .alu = mode });
    c.compute_flags(freshness, flags);
}
pub fn j_minus_k(c: *Cycle, freshness: Freshness, flags: Flags_Mode) void {
    const mode: arch.ALU_Mode = .{
        .op = .j_plus_not_k,
        .use_stat_c = freshness == .cont,
        .invert_cin = freshness == .fresh,
    };
    c.set_control_signal(.unit, .alu);
    c.set_control_signal(.mode, .{ .alu = mode });
    c.compute_flags(freshness, flags);
}
pub fn k_minus_j(c: *Cycle, freshness: Freshness, flags: Flags_Mode) void {
    const mode: arch.ALU_Mode = .{
        .op = .not_j_plus_k,
        .use_stat_c = freshness == .cont,
        .invert_cin = freshness == .fresh,
    };
    c.set_control_signal(.unit, .alu);
    c.set_control_signal(.mode, .{ .alu = mode });
    c.compute_flags(freshness, flags);
}

pub fn j_logic_k(c: *Cycle, op: Logic_Op, freshness: Freshness, flags: Flags_Mode) void {
    c.set_control_signal(.unit, .alu);
    c.set_control_signal(.mode, .{ .alu = switch (op) {
        .xor => arch.ALU_Mode.logic_xor,
        ._or => arch.ALU_Mode.logic_or,
        ._and => arch.ALU_Mode.logic_and,
    }});
    c.compute_flags(freshness, flags);
}

pub fn j_shift_k(c: *Cycle, op: Shift_Op, freshness: Freshness, flags: Flags_Mode) void {
    c.set_control_signal(.unit, .shift);
    c.set_control_signal(.mode, .{ .shift = switch (op) {
        .shl => arch.Shift_Mode.shl,
        .shr => arch.Shift_Mode.shrl,
        .shlc => arch.Shift_Mode.shlc,
        .shrc => arch.Shift_Mode.shrc,
        .shrs => arch.Shift_Mode.shra,
    }});
    c.compute_flags(freshness, flags);
}

pub fn swap_j(c: *Cycle, op: Swap_Op, flags: Flags_Mode) void {
    c.set_control_signal(.unit, .shift);
    c.set_control_signal(.mode, .{ .shift = switch (op) {
        .swap_bytes => arch.Shift_Mode.swap_bytes,
        .swap_halves => arch.Shift_Mode.swap_halves,
        .reverse_bytes => arch.Shift_Mode.reverse_bytes,
        .reverse_bits => arch.Shift_Mode.reverse_bits,
        .reverse_bits_in_halves => arch.Shift_Mode.reverse_bits_in_halves,
        .reverse_bits_in_bytes => arch.Shift_Mode.reverse_bits_in_bytes,
    }});
    c.compute_flags(.fresh, flags);
}

pub fn j_times_k(c: *Cycle,
    jt: arch.Multiply_Mode.Signedness, kt: arch.Multiply_Mode.Signedness,
    jh: arch.Multiply_Mode.Half, kh: arch.Multiply_Mode.Half,
    freshness: Freshness, flags: Flags_Mode
) void {
    const mode: arch.Multiply_Mode = .{
        .j_type = jt,
        .k_type = kt,
        .j = jh,
        .k = kh,
        .shift_result = false,
    };
    c.set_control_signal(.unit, .mult);
    c.set_control_signal(.mode, .{ .mult = mode });
    c.compute_flags(freshness, flags);
}
pub fn j_times_k__shl16(c: *Cycle,
    jt: arch.Multiply_Mode.Signedness, kt: arch.Multiply_Mode.Signedness,
    jh: arch.Multiply_Mode.Half, kh: arch.Multiply_Mode.Half,
    freshness: Freshness, flags: Flags_Mode
) void {
    const mode: arch.Multiply_Mode = .{
        .j_type = jt,
        .k_type = kt,
        .j = jh,
        .k = kh,
        .shift_result = true,
    };
    c.set_control_signal(.unit, .mult);
    c.set_control_signal(.mode, .{ .mult = mode });
    c.compute_flags(freshness, flags);
}

pub fn count_j(c: *Cycle, count_what: Bit_Count_Polarity, dir: Bit_Count_Direction, freshness: Freshness, flags: Flags_Mode) void {
    const mode: arch.Count_Extend_Mode = switch (dir) {
        .all => switch (count_what) {
            .zeroes => arch.Count_Extend_Mode.count_zeroes,
            .ones => arch.Count_Extend_Mode.count_ones,
        },
        .leading => switch (count_what) {
            .zeroes => arch.Count_Extend_Mode.count_leading_zeroes,
            .ones => arch.Count_Extend_Mode.count_leading_ones,
        },
        .trailing => switch (count_what) {
            .zeroes => arch.Count_Extend_Mode.count_trailing_zeroes,
            .ones => arch.Count_Extend_Mode.count_trailing_ones,
        },
    };
    c.set_control_signal(.unit, .count_extend);
    c.set_control_signal(.mode, .{ .count_extend = mode });
    c.compute_flags(freshness, flags);
}

pub fn j_ext_k(c: *Cycle, ext: Zero_Or_Sign_Extension, flags: Flags_Mode) void {
    const mode: arch.Count_Extend_Mode = switch (ext) {
        .zx => arch.Count_Extend_Mode.zero_extend_or_truncate,
        .sx => arch.Count_Extend_Mode.sign_extend_or_truncate,
    };
    c.zero_to_j();
    c.set_control_signal(.unit, .count_extend);
    c.set_control_signal(.mode, .{ .count_extend = mode });
    c.compute_flags(.fresh, flags);
}

pub fn saturate_k(c: *Cycle, what: Bit_Count_Polarity, dir: Bit_Count_Direction, flags: Flags_Mode) void {
    const mode: arch.Count_Extend_Mode = switch (dir) {
        .leading => switch (what) {
            .zeroes => arch.Count_Extend_Mode.saturate_zeroes_left,
            .ones => arch.Count_Extend_Mode.saturate_ones_left,
        },
        .trailing => switch (what) {
            .zeroes => arch.Count_Extend_Mode.saturate_zeroes_right,
            .ones => arch.Count_Extend_Mode.saturate_ones_right,
        },
        .all => {
            c.warn("Expected direction to be .left or .right for `saturate_k`", .{});
            return;
        },
    };
    c.zero_to_j();
    c.set_control_signal(.unit, .count_extend);
    c.set_control_signal(.mode, .{ .count_extend = mode });
    c.compute_flags(.fresh, flags);
}

//////////////
// to J bus //
//////////////

pub fn zero_to_j(c: *Cycle) void {
    c.set_control_signal(.jsrc, .zero);
}

pub fn reg_to_j(c: *Cycle) void {
    c.set_control_signal(.jsrc, .jr);
}

pub fn sr_to_j(c: *Cycle, which: arch.Any_SR_Index) void {
    if (which.to_sr1()) |sr1| {
        c.set_control_signal(.sr1ri, sr1);
        c.set_control_signal(.jsrc, .sr1);
    } else if (which.to_sr2()) |sr2| {
        c.set_control_signal(.sr2ri, sr2);
        c.set_control_signal(.jsrc, .sr2);
    } else unreachable;
}

//////////////
// to K bus //
//////////////

pub fn zero_to_k(c: *Cycle) void {
    c.set_control_signal(.ksrc, .zero);
}

pub fn reg_to_k(c: *Cycle) void {
    c.set_control_signal(.ksrc, .kr);
}

pub fn sr_to_k(c: *Cycle, which: arch.Any_SR_Index) void {
    if (which.to_sr1()) |sr1| {
        c.set_control_signal(.sr1ri, sr1);
        c.set_control_signal(.ksrc, .sr1);
    } else if (which.to_sr2()) |sr2| {
        c.set_control_signal(.sr2ri, sr2);
        c.set_control_signal(.ksrc, .sr2);
    } else unreachable;
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

pub fn literal_to_k(c: *Cycle, literal: i16) void {
    if (literal == 0) {
        c.zero_to_k();
        return;
    }

    if (c.flags.contains(.initial_krio_valid)) {
        if (c.initial_krio.raw() == literal) {
            c.krio_to_k();
            return;
        } else if (literal == (@as(u32, 1) << c.initial_krio.raw_unsigned())) {
            c.krio_bit_to_k();
            return;
        } else if (literal == ~(@as(u32, 1) << c.initial_krio.raw_unsigned())) {
            c.not_krio_bit_to_k();
            return;
        }
    }

    if (c.initial_dr.byte1_i8() == literal and c.flags.contains(.initial_dr1_valid)) {
        c.dr_i8_to_k(true);
        return;
    } else if (c.initial_dr.byte2_i8() == literal and c.flags.contains(.initial_dr2_valid)) {
        c.dr_i8_to_k(false);
        return;
    } else if (c.initial_dr.byte21_i16() == literal and c.flags.contains(.initial_dr1_valid) and c.flags.contains(.initial_dr2_valid)) {
        c.dr_i16_to_k();
        return;
    }

    if (literal >= arch.addr.Virtual.Microcode_Offset.min and literal <= arch.addr.Virtual.Microcode_Offset.max) {
        const vao = arch.addr.Virtual.Microcode_Offset.init(@intCast(literal));
        c.set_control_signal(.vao, vao);
    } else {
        c.warn("Could not encode literal {} for K", .{ literal });
        c.set_control_signal(.vao, .zero);
    }
    c.set_control_signal(.ksrc, .vao);
}

pub fn krio_to_k(c: *Cycle) void {
    c.set_control_signal(.ksrc, .krio);
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
    c.set_control_signal(.unit, .alu);
    c.set_control_signal(.mode, .{ .alu = arch.ALU_Mode.all_zeroes });
    c.set_control_signal(.lsrc, .alu);
}

pub fn literal_to_l(c: *Cycle, literal: i16) void {
    if (literal == 0) {
        c.zero_to_l();
    } else if (literal == -1) {
        if (!c.is_set(.jsrc)) c.zero_to_j();
        if (!c.is_set(.ksrc)) c.zero_to_k();
        c.set_control_signal(.unit, .alu);
        c.set_control_signal(.mode, .{ .alu = arch.ALU_Mode.all_ones });
        c.set_control_signal(.lsrc, .alu);
    } else {
        c.literal_to_k(literal);
        c.k_to_l();
    }
}

pub fn status_to_l(c: *Cycle) void {
    c.set_control_signal(.lsrc, .status);
}

pub fn last_translation_info_to_l(c: *Cycle) void {
    c.set_control_signal(.lsrc, .at_info);
}

pub fn reg_to_l(c: *Cycle) void {
    c.reg_to_k();
    c.k_to_l();
}

pub fn sr_to_l(c: *Cycle, which: arch.Any_SR_Index) void {
    c.sr_to_k(which);   
    c.k_to_l();
}

pub fn k_to_l(c: *Cycle) void {
    c.zero_to_j();
    c.j_plus_k_to_l(.fresh, .no_flags);
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
}

pub fn krio_bit_to_l(c: *Cycle) void {
    c.krio_bit_to_k();
}

pub fn not_krio_bit_to_l(c: *Cycle) void {
    c.not_krio_bit_to_k();
}

pub fn j_plus_k_to_l(c: *Cycle, freshness: Freshness, flags: Flags_Mode) void {
    c.j_plus_k(freshness, flags);
    c.set_control_signal(.lsrc, .alu);
}
pub fn j_minus_k_to_l(c: *Cycle, freshness: Freshness, flags: Flags_Mode) void {
    c.j_minus_k(freshness, flags);
    c.set_control_signal(.lsrc, .alu);
}
pub fn k_minus_j_to_l(c: *Cycle, freshness: Freshness, flags: Flags_Mode) void {
    c.k_minus_j(freshness, flags);
    c.set_control_signal(.lsrc, .alu);
}

pub fn j_logic_k_to_l(c: *Cycle, op: Logic_Op, freshness: Freshness, flags: Flags_Mode) void {
    c.j_logic_k(op, freshness, flags);
    c.set_control_signal(.lsrc, .alu);
}

pub fn j_shift_k_to_l(c: *Cycle, op: Shift_Op, freshness: Freshness, flags: Flags_Mode) void {
    c.j_shift_k(op, freshness, flags);
    c.set_control_signal(.lsrc, .shift);
}

pub fn swap_j_to_l(c: *Cycle, op: Swap_Op, flags: Flags_Mode) void {
    c.swap_j(op, flags);
    c.set_control_signal(.lsrc, .shift);
}

pub fn jl_times_kl_to_l(c: *Cycle, js: arch.Multiply_Mode.Signedness, ks: arch.Multiply_Mode.Signedness, freshness: Freshness, flags: Flags_Mode) void {
    c.j_times_k(js, ks, .lsb, .lsb, freshness, flags);
    c.set_control_signal(.lsrc, .mult);
}
pub fn jl_times_kh_to_l(c: *Cycle, js: arch.Multiply_Mode.Signedness, ks: arch.Multiply_Mode.Signedness, freshness: Freshness, flags: Flags_Mode) void {
    c.j_times_k(js, ks, .lsb, .msb, freshness, flags);
    c.set_control_signal(.lsrc, .mult);
}
pub fn jh_times_kl_to_l(c: *Cycle, js: arch.Multiply_Mode.Signedness, ks: arch.Multiply_Mode.Signedness, freshness: Freshness, flags: Flags_Mode) void {
    c.j_times_k(js, ks, .msb, .lsb, freshness, flags);
    c.set_control_signal(.lsrc, .mult);
}
pub fn jh_times_kh_to_l(c: *Cycle, js: arch.Multiply_Mode.Signedness, ks: arch.Multiply_Mode.Signedness, freshness: Freshness, flags: Flags_Mode) void {
    c.j_times_k(js, ks, .msb, .msb, freshness, flags);
    c.set_control_signal(.lsrc, .mult);
}
pub fn jl_times_kh_shl16_to_l(c: *Cycle, js: arch.Multiply_Mode.Signedness, ks: arch.Multiply_Mode.Signedness, freshness: Freshness, flags: Flags_Mode) void {
    c.j_times_k__shl16(js, ks, .lsb, .msb, freshness, flags);
    c.set_control_signal(.lsrc, .mult);
}
pub fn jh_times_kl_shl16_to_l(c: *Cycle, js: arch.Multiply_Mode.Signedness, ks: arch.Multiply_Mode.Signedness, freshness: Freshness, flags: Flags_Mode) void {
    c.j_times_k__shl16(js, ks, .msb, .lsb, freshness, flags);
    c.set_control_signal(.lsrc, .mult);
}

pub fn count_j_to_l(c: *Cycle, count_what: Bit_Count_Polarity, dir: Bit_Count_Direction, freshness: Freshness, flags: Flags_Mode) void {
    c.count_j(count_what, dir, freshness, flags);
    c.set_control_signal(.lsrc, .count);
}

pub fn j_ext_k_to_l(c: *Cycle, ext: Zero_Or_Sign_Extension, flags: Flags_Mode) void {
    c.j_ext_k(ext, flags);
    c.set_control_signal(.lsrc, .ext);
}

pub fn saturate_k_to_l(c: *Cycle, what: Bit_Count_Polarity, dir: Bit_Count_Direction, flags: Flags_Mode) void {
    c.saturate_k(what, dir, flags);
    c.set_control_signal(.lsrc, .ext);
}

pub fn dr_to_l(c: *Cycle) void {
    c.set_control_signal(.atop, .none);
    c.set_control_signal(.dir, .write_from_dr_ir);
    c.set_control_signal(.drw, true);
    c.set_control_signal(.lsrc, .d);
}

pub fn dr_to_ir(c: *Cycle) void {
    c.set_control_signal(.irw, true);
}

pub fn ir_to_l(c: *Cycle) void {
    c.set_control_signal(.atop, .none);
    c.set_control_signal(.dir, .write_from_dr_ir);
    c.set_control_signal(.drw, false);
    c.set_control_signal(.lsrc, .d);
}

pub fn d_to_l(c: *Cycle) void {
    c.set_control_signal(.lsrc, .d);
}

/////////////////////
// Register Writes //
/////////////////////

pub fn l_to_reg(c: *Cycle) void {
    c.set_control_signal(.gprw, true);
}

pub fn l_to_sr(c: *Cycle, which: arch.Any_SR_Index) void {
    if (which.to_sr1()) |sr1| {
        c.set_control_signal(.sr1wi, sr1);
        c.set_control_signal(.sr1wsrc, .l);
    } else if (which.to_sr2()) |sr2| {
        c.set_control_signal(.sr2wi, sr2);
        c.set_control_signal(.sr2wsrc, .l);
    } else {
        unreachable;
    }
}

pub fn virtual_address_to_sr(c: *Cycle, which: arch.Any_SR_Index) void {
    if (which.to_sr1()) |sr1| {
        c.set_control_signal(.sr1wi, sr1);
        c.set_control_signal(.sr1wsrc, .virtual_addr);
    } else if (which.to_sr2()) |sr2| {
        c.set_control_signal(.sr2wi, sr2);
        c.set_control_signal(.sr2wsrc, .virtual_addr);
    }
}

pub fn sr1_to_sr1(c: *Cycle, src_index: arch.SR1_Index, dest_index: arch.SR1_Index) void {
    c.set_control_signal(.sr1ri, src_index);
    c.set_control_signal(.sr1wi, dest_index);
    c.set_control_signal(.sr1wsrc, .self);
}

pub fn sr2_to_sr2(c: *Cycle, src_index: arch.SR2_Index, dest_index: arch.SR2_Index) void {
    c.set_control_signal(.sr2ri, src_index);
    c.set_control_signal(.sr2wi, dest_index);
    c.set_control_signal(.sr2wsrc, .self);
}

pub fn l_to_stat_zncv(c: *Cycle) void {
    c.set_control_signal(.statop, .load_zncv);
}
pub fn l_to_ti_and_stat_zncva(c: *Cycle) void {
    c.set_control_signal(.statop, .load_zncva_ti);
}

pub fn zn_flags_from_l(c: *Cycle) void {
    c.set_control_signal(.statop, .zn_from_l);
}

pub fn l_to_dr(c: *Cycle) void {
    c.set_control_signal(.atop, .none);
    c.set_control_signal(.dir, .write_from_l);
    c.set_control_signal(.drw, true);
}

pub fn l_to_rsn(c: *Cycle) void {
    c.set_control_signal(.special, .load_rsn_from_l);
}

pub fn toggle_rsn(c: *Cycle) void {
    c.set_control_signal(.special, .toggle_rsn);
}

pub fn reload_asn(c: *Cycle) void {
    c.sr2_to_sr2(.asn, .asn);
}

/////////////////////
// Addresses & Bus //
/////////////////////

pub fn enable_address_translation(c: *Cycle) void {
    c.set_control_signal(.statop, .set_a);
}
pub fn disable_address_translation(c: *Cycle) void {
    c.set_control_signal(.statop, .clear_a);
}

pub fn update_address_translation_from_l(c: *Cycle, base: arch.addr.Virtual.Base_SR_Index, group: at.Entry.Group) void {
    c.address(base, 0);
    c.set_control_signal(.atop, .update);
    c.set_control_signal(.vaspace, @as(arch.addr.Space, switch (group) {
        .data_read, .data_write => .data,
        .stack => .stack,
        .insn => .insn,
    }));
    c.set_control_signal(.dir, @as(arch.D.Direction, switch (group) {
        .data_write => .write_from_l,
        .data_read, .stack, .insn => .read,
    }));
    c.set_control_signal(.width, .@"32b");
}

pub fn invalidate_address_translation_from_l(c: *Cycle, base: arch.addr.Virtual.Base_SR_Index, group: at.Entry.Group) void {
    c.address(base, 0);
    c.set_control_signal(.atop, .invalidate);
    c.set_control_signal(.vaspace, @as(arch.addr.Space, switch (group) {
        .data_read, .data_write => .data,
        .stack => .stack,
        .insn => .insn,
    }));
    c.set_control_signal(.dir, @as(arch.D.Direction, switch (group) {
        .data_write => .write_from_l,
        .data_read, .stack, .insn => .read,
    }));
    c.set_control_signal(.width, .@"32b");
}

pub fn address(c: *Cycle, base: arch.addr.Virtual.Base_SR_Index, offset: anytype) void {
    const vao: arch.addr.Virtual.Microcode_Offset = switch (@typeInfo(@TypeOf(offset))) {
        .Int, .ComptimeInt => vao: {
            if (offset >= arch.addr.Virtual.Microcode_Offset.min and offset <= arch.addr.Virtual.Microcode_Offset.max) {
                break :vao arch.addr.Virtual.Microcode_Offset.init(@intCast(offset));
            } else if (c.initial_encoding_word) |dr| {
                if (dr.byte2_i8() == offset and c.flags.contains(.initial_dr2_valid)) {
                    break :vao .i8_from_dr;
                } else if (dr.byte2_i8() * 4 == offset and c.flags.contains(.initial_dr2_valid)) {
                    break :vao .i8_x4_from_dr;
                } else if (dr.byte21_i16() == offset and c.flags.contains(.initial_dr1_valid) and c.flags.contains(.initial_dr2_valid)) {
                    break :vao .i16_from_dr;
                }
            }
            c.warn("Could not encode address offset {}", .{ offset });
            break :vao .zero;
        },
        .EnumLiteral => offset,
        else => unreachable,
    };
    c.set_control_signal(.vari, base);
    c.set_control_signal(.vao, vao);
}

pub fn read_to_d(c: *Cycle, base: arch.addr.Virtual.Base_SR_Index, offset: anytype, width: arch.D.Width, space: arch.addr.Space) void {
    c.address(base, offset);
    c.set_control_signal(.atop, .translate);
    c.set_control_signal(.vaspace, space);
    c.set_control_signal(.width, width);
    c.set_control_signal(.dir, .read);
    if (base == .ip and space == .insn and c.signals.sr2wi != .ip and c.signals.sr2wi != .next_ip and @typeInfo(@TypeOf(offset)) != .EnumLiteral) {
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

pub fn read_to_dr(c: *Cycle, base: arch.addr.Virtual.Base_SR_Index, offset: anytype, width: arch.D.Width, space: arch.addr.Space) void {
    c.read_to_d(base, offset, width, space);
    c.set_control_signal(.drw, true);
}

pub fn ip_read_to_d(c: *Cycle, offset: anytype, width: arch.D.Width) void {
    c.read_to_d(.ip, offset, width, .insn);
}

pub fn ip_read_to_dr(c: *Cycle, offset: anytype, width: arch.D.Width) void {
    c.read_to_dr(.ip, offset, width, .insn);
}

pub fn ip_read_24b_to_dr_ir(c: *Cycle, offset: anytype) void {
    c.ip_read_to_dr(offset, .@"24b");
    c.dr_to_ir();
}

pub fn write_from_l(c: *Cycle, base: arch.addr.Virtual.Base_SR_Index, offset: anytype, width: arch.D.Width, space: arch.addr.Space) void {
    c.address(base, offset);
    c.set_control_signal(.atop, .translate);
    c.set_control_signal(.vaspace, space);
    c.set_control_signal(.width, width);
    c.set_control_signal(.dir, .write_from_l);
}

pub fn write_from_dr(c: *Cycle, base: arch.addr.Virtual.Base_SR_Index, offset: anytype, width: arch.D.Width, space: arch.addr.Space) void {
    c.address(base, offset);
    c.set_control_signal(.atop, .translate);
    c.set_control_signal(.vaspace, space);
    c.set_control_signal(.width, width);
    c.set_control_signal(.dir, .write_from_dr_ir);
    c.set_control_signal(.drw, true);
}

pub fn block_transfer_pull(c: *Cycle, base: arch.addr.Virtual.Base_SR_Index, preincrement: anytype, space: arch.addr.Space) void {
    c.address(base, preincrement);
    c.set_control_signal(.atop, .translate);
    c.set_control_signal(.vaspace, space);
    c.set_control_signal(.width, .@"32b");
    c.set_control_signal(.dir, .write_from_l);
    c.set_control_signal(.special, .block_transfer);
    c.virtual_address_to_sr(base.to_any());
}

pub fn block_transfer_push(c: *Cycle, base: arch.addr.Virtual.Base_SR_Index, preincrement: anytype, space: arch.addr.Space) void {
    c.address(base, preincrement);
    c.set_control_signal(.atop, .translate);
    c.set_control_signal(.vaspace, space);
    c.set_control_signal(.width, .@"32b");
    c.set_control_signal(.dir, .read);
    c.set_control_signal(.special, .block_transfer);
    c.virtual_address_to_sr(base.to_any());
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
        switch (isa.branch_kind(signature.mnemonic, signature.suffix)) {
            .nonbranching, .conditional => {},
            .unconditional, .call => {
                c.warn("Cycle executes next instruction, but this mnemonic/suffix indicates it should be an unconditional branch or call", .{});
            },
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
    c.set_control_signal(.irw, true);
    c.exec_ir_insn();
}

pub fn load_and_exec_next_insn(c: *Cycle) void {
    if (c.encoding_len) |offset| {
        c.branch(.ip, offset);
    } else {
        c.warn("Cycle loads next instruction, but there is no instruction encoding corresponding to this microcode sequence", .{});
    }
}

pub fn call_or_branch(c: *Cycle, base: arch.addr.Virtual.Base_SR_Index, offset: anytype, mnemonic: isa.Mnemonic) void {
    if (isa.branch_kind(mnemonic, .none) == .call) {
        c.sr_to_j(.ip);
        c.literal_to_k(c.encoding_len.?);
        c.j_plus_k_to_l(.zx, .fresh, .no_flags);
        c.l_to_sr(.rp);
    }
    c.branch(base, offset);
}

pub fn branch(c: *Cycle, base: arch.addr.Virtual.Base_SR_Index, offset: anytype) void {
    if (base != .ip or @typeInfo(@TypeOf(offset)) != .EnumLiteral and offset != 0 or !c.is_set(.sr2wi)) {
        c.set_control_signal(.sr2wi, .ip);
        c.set_control_signal(.sr2wsrc, .virtual_addr);
    }
    c.read_to_dr(base, offset, .@"24b", .insn);
    c.set_control_signal(.irw, true);
    c.exec_ir_insn();
}

pub fn exec_ir_insn(c: *Cycle) void {
    c.allow_interrupt();
    c.set_control_signal(.seqop, .next_instruction);
}

pub fn wi_to_ti(c: *Cycle) void {
    c.set_control_signal(.tiw, true);
}

pub fn next(c: *Cycle, func: *const anyopaque) void {
    std.debug.assert(c.next_func == null);
    c.next_func = func;
}

pub fn force_normal_execution(c: *Cycle, func: *const anyopaque) void {
    c.set_control_signal(.seqop, .next_uop_force_normal);
    c.next(func);
}

pub fn allow_interrupt(c: *Cycle) void {
    c.set_control_signal(.allowint, true);
}

pub fn fault_return(c: *Cycle) void {
    c.set_control_signal(.seqop, .fault_return);
    c.sr_to_l(.fault_stat); // .fault_return implies loading UCA from upper bits of L
}

fn trigger_fault(c: *Cycle, slot: arch.microcode.Slot) void {
    c.set_control_signal(.special, .trigger_fault);
    c.set_control_signal(.seqop, .next_uop);
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
const Control_Signals = arch.Control_Signals;
const Control_Signal = arch.Control_Signal;
const Register_Index = arch.Register_Index;
const Encoded_Instruction = isa.Encoded_Instruction;
const at = arch.addr.translation;
const isa = @import("isa");
const arch = @import("arch");
const bits = @import("bits");
const std = @import("std");
