func_name: []const u8,
signals: Control_Signals,
assigned_signals: std.EnumSet(Control_Signal),
recursion_ptr: ?*const anyopaque, // alternative to `next` for breaking microcode loop dependencies
next_slot: ?Microcode_Builder.Slot_Data.Handle,
encoding_len: ?Encoded_Instruction.Length_Type,
flags: std.EnumSet(Cycle_Flags),

pub const Cycle_Flags = enum {
    next_insn_loaded,
    ij_valid,
    ik_valid,
    iw_valid,
};

pub fn init(name: []const u8, encoding_len: ?Encoded_Instruction.Length_Type, flags: std.EnumSet(Cycle_Flags)) Cycle {
    return .{
        .func_name = name,
        .signals = std.mem.zeroInit(Control_Signals, .{ .mode = Control_Signals.Compute_Mode.init(0) }),
        .assigned_signals = .{},
        .recursion_ptr = null,
        .next_slot = null,
        .encoding_len = encoding_len,
        .flags = flags,
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
    switch (cycle.signals.at_op) {
        .none => {},
        .translate => {
            cycle.validate_address();
            cycle.ensure_set(.bus_width);
        },
        .update, .invalidate => {
            cycle.validate_address();
        },
    }

    if (cycle.signals.bus_dir == .write_from_dr and cycle.signals.at_op != .translate and cycle.signals.ll_src != .d and cycle.signals.ll_src != .d8_sx) {
        cycle.warn("DR value is written to D, but there is no bus transaction happening, and D is not being read to LL", .{});
    }

    switch (cycle.signals.ll_src) {
        .zero, .translation_info_l, .stat, .pipeline => {},
        .iw_ik_ij_zx => {
            cycle.validate_ij();
            cycle.validate_ik();
            cycle.validate_iw();
        },
        .compute_l => cycle.validate_compute_mode(null),
        .d        => cycle.validate_bus_read(null),
        .d8_sx    => cycle.validate_bus_read(.byte),
    }

    switch (cycle.signals.lh_src) {
        .zero, .translation_info_h, .jh, .prev_uc_slot => {},
        .compute_h => cycle.validate_compute_mode(null),
        .d_sx => if (cycle.signals.ll_src != .d) cycle.warn("Expected LL source to be d when LH source is d_sx", .{}),
        .d8_sx => if (cycle.signals.ll_src != .d8_sx) cycle.warn("Expected LL source to be d8_sx when LH source is d8_sx", .{}),
        _ => cycle.warn("Unrecognized LH source", .{}),
    }

    switch (cycle.signals.jl_src) {
        .zero, .sr1l, .sr2l => {},
        .jrl => cycle.validate_ij(),
    }

    switch (cycle.signals.jh_src) {
        .zero, .sr1h, .sr2h, .neg_one => {},
        .jrh, .jrl_sx => cycle.validate_ij(),
        _ => {},
    }

    switch (cycle.signals.k_src) {
        .zero, .literal_sx, .sr1l, .sr2l => {},
        .kr, .ik_bit, .ik_zx => cycle.validate_ik(),
        .ij_ik_zx => {
            cycle.validate_ij();
            cycle.validate_ik();
        },
    }

    if (cycle.signals.reg_write != .no_write) {
        cycle.validate_iw();
    }

    if (cycle.signals.sr1_wsrc == .rsn_sr1 and !cycle.is_set(.sr1_ri)) {
        cycle.warn("Expected sr1_ri to be set when sr1_wsrc is sr1_rsn!", .{});
    }
    if (cycle.signals.sr1_wsrc != .no_write and !cycle.is_set(.sr1_wi)) {
        cycle.warn("Expected sr1_wi to be set when SR1 is being written!", .{});
    }

    if (cycle.signals.sr2_wsrc == .sr2 and !cycle.is_set(.sr2_ri)) {
        cycle.warn("Expected sr2_ri to be set when sr2_wsrc is sr2!", .{});
    }
    if (cycle.signals.sr2_wsrc != .no_write and !cycle.is_set(.sr2_wi)) {
        cycle.warn("Expected sr2_wi to be set when SR2 is being written!", .{});
    }

    switch (cycle.signals.stat_op) {
        .hold,
        .load_zncv,
        .load_zncvka,
        .clear_a, .set_a,
        .zn_16, .zn_16_no_set_z,
        .zn_32, .zn_32_no_set_z,
        => {},

        .zn_16__c_from_shift, .zn_16_no_set_z__c_from_shift,
        .zn_32__c_from_shift, .zn_32_no_set_z__c_from_shift => cycle.validate_compute_mode(.shift),

        .zncv_from_arith, .zncv_from_arith_no_set_z => cycle.validate_compute_mode(.alu),

        _ => {},
    }

    switch (cycle.signals.seq_op) {
        .next_instruction, .fault_return => {
            cycle.flags.remove(.next_insn_loaded);
            if (cycle.recursion_ptr != null) {
                cycle.warn("next() is not allowed when decoding/executing a new instruction", .{});
            }
        },
        .next_uop, .next_uop_force_normal => if (cycle.recursion_ptr == null and cycle.signals.special != .trigger_fault) {
            cycle.warn("Expected a next()", .{});
        },
    }

    if (cycle.signals.seq_op == .next_instruction and !cycle.signals.allow_int) {
        cycle.warn("Expected allow_int when seq_op is .next_instruction", .{});
    }

    if (cycle.signals.ij_op == .from_continuation and !cycle.is_set(.c_ij)) {
        cycle.warn(".c_ij must be set when .ij_op is .from_continuation", .{});
    }
    if (cycle.signals.ik_op == .from_continuation and !cycle.is_set(.c_ik)) {
        cycle.warn(".c_ik must be set when .ik_op is .from_continuation", .{});
    }
    if (cycle.signals.iw_op == .from_continuation and !cycle.is_set(.c_iw)) {
        cycle.warn(".c_iw must be set when .iw_op is .from_continuation", .{});
    }

    switch (cycle.signals.ij_op) {
        .from_continuation, .from_decode => cycle.flags.insert(.ij_valid),
        .hold, .xor1 => {},
    }
    switch (cycle.signals.ik_op) {
        .from_continuation, .from_decode => cycle.flags.insert(.ik_valid),
        .hold, .xor1 => {},
    }
    switch (cycle.signals.iw_op) {
        .from_continuation, .from_decode => cycle.flags.insert(.iw_valid),
        .hold, .xor1 => {},
    }

    if (!cycle.is_set(.unit)) cycle.set_control_signal(.unit, .count);
    if (!cycle.is_set(.special)) cycle.set_control_signal(.special, .none);
    if (!cycle.is_set(.at_op)) cycle.set_control_signal(.at_op, .none);
    if (!cycle.is_set(.ij_op)) cycle.set_control_signal(.ij_op, .hold);
    if (!cycle.is_set(.ik_op)) cycle.set_control_signal(.ik_op, .hold);
    if (!cycle.is_set(.iw_op)) cycle.set_control_signal(.iw_op, .hold);
    if (!cycle.is_set(.sr1_wsrc)) cycle.set_control_signal(.sr1_wsrc, .no_write);
    if (!cycle.is_set(.sr2_wsrc)) cycle.set_control_signal(.sr2_wsrc, .no_write);
    if (!cycle.is_set(.stat_op)) cycle.set_control_signal(.stat_op, .hold);
    if (!cycle.is_set(.bus_dir)) cycle.set_control_signal(.bus_dir, .read);
    if (!cycle.is_set(.allow_int)) cycle.set_control_signal(.allow_int, false);
    if (!cycle.is_set(.seq_op)) cycle.set_control_signal(.seq_op, .next_uop);
}

fn is_set(cycle: *Cycle, signal: Control_Signal) bool {
    return cycle.assigned_signals.contains(signal);
}

fn ensure_set(cycle: *Cycle, signal: Control_Signal) void {
    if (!cycle.is_set(signal)) {
        cycle.warn("Expected {s} to be assigned", .{ @tagName(signal) });
    }
}

fn validate_ij(cycle: *Cycle) void {
    if (!cycle.flags.contains(.ij_valid)) {
        cycle.warn("IJ is undefined for this cycle!", .{});
    }
}
fn validate_ik(cycle: *Cycle) void {
    if (!cycle.flags.contains(.ik_valid)) {
        cycle.warn("IK is undefined for this cycle!", .{});
    }
}
fn validate_iw(cycle: *Cycle) void {
    if (!cycle.flags.contains(.iw_valid)) {
        cycle.warn("IW is undefined for this cycle!", .{});
    }
}

fn validate_compute_mode(cycle: *Cycle, expected: ?Control_Signals.Compute_Unit) void {
    if (expected) |unit| {
        if (unit != cycle.signals.unit) {
            cycle.warn("Expected compute unit to be {s}, but found {s}", .{ @tagName(unit), @tagName(cycle.signals.unit) });
        }
    }

    cycle.ensure_set(.unit);
    cycle.ensure_set(.mode);

    switch (cycle.signals.unit) {
        .alu => {
            const low2: u2 = @truncate(cycle.signals.mode.raw());
            if (low2 == 3) {
                const mode = cycle.signals.mode.arith;
                if (mode.k_ext != .none) {
                    cycle.validate_jh();
                }
            } else {
                //const mode = cycle.signals.mode.logic;
                cycle.validate_jh();
            }
            cycle.validate_jl();
            cycle.validate_k();
        },
        .shift => {
            const mode = cycle.signals.mode.shift;
            if (cycle.signals.k_src != .zero or mode.wide or mode.early_swap16) {
                cycle.validate_jh();
            }
            cycle.validate_jl();
            cycle.validate_k();
        },
        .mult => {
            //const mode = cycle.signals.mode.mult;
            cycle.validate_jl();
            cycle.validate_k();
        },
        .count => {
            //const mode = cycle.signals.mode.count;
            cycle.validate_jh();
            cycle.validate_jl();
        },
    }
}

fn validate_jh(cycle: *Cycle) void {
    if (!cycle.is_set(.jh_src)) {
        cycle.warn("Expected jh_src to be set", .{});
    } else switch (cycle.signals.jh_src) {
        .zero, .neg_one, .jrh, .jrl_sx => {},
        .sr1h => cycle.ensure_set(.sr1_ri),
        .sr2h => cycle.ensure_set(.sr2_ri),
        _ => unreachable,
    }
}

fn validate_jl(cycle: *Cycle) void {
    if (!cycle.is_set(.jl_src)) {
        cycle.warn("Expected jl_src to be set", .{});
    } else switch (cycle.signals.jl_src) {
        .zero, .jrl => {},
        .sr1l => cycle.ensure_set(.sr1_ri),
        .sr2l => cycle.ensure_set(.sr2_ri),
    }
}

fn validate_k(cycle: *Cycle) void {
    if (!cycle.is_set(.k_src)) {
        cycle.warn("Expected k_src to be set", .{});
    } else switch (cycle.signals.k_src) {
        .zero, .kr, .ik_bit, .ik_zx, .ij_ik_zx => {},
        .literal_sx => cycle.ensure_set(.literal),
        .sr1l => cycle.ensure_set(.sr1_ri),
        .sr2l => cycle.ensure_set(.sr2_ri),
    }
}

fn validate_address(cycle: *Cycle) void {
    cycle.ensure_set(.base_ri);
    cycle.ensure_set(.offset_src);
    cycle.ensure_set(.at_op);
    cycle.ensure_set(.addr_space);
    cycle.ensure_set(.bus_dir);
}

fn validate_bus_read(cycle: *Cycle, width: ?Control_Signals.Bus_Width) void {
    switch (cycle.signals.bus_dir) {
        .read, .read_to_dr => {
            cycle.validate_address();
            if (cycle.signals.at_op != .translate) cycle.warn("Expected at_op to be .translate", .{});
            if (width) |w| {
                if (w != cycle.signals.bus_width) cycle.warn("Expected bus_width to be {}", .{ w });
            }
        },
        .write_from_dr => {},
        .write_from_ll => cycle.warn("Expected bus_rw to be .read", .{}),
    }
}

fn set_control_signal(c: *Cycle, comptime signal: Control_Signal, raw_value: anytype) void {
    const current_value = @field(c.signals, @tagName(signal));
    const T = @TypeOf(current_value);
    var value = @as(T, raw_value);
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
        .bus_dir => if (value == .read_to_dr) {
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

pub fn jl_plus_k(c: *Cycle, freshness: Freshness, flags: Flags_Mode) void {
    var mode = Control_Signals.Arithmetic_Mode.jl_plus_k;
    mode.carry = freshness == .cont;
    c.set_control_signal(.unit, .alu);
    c.set_control_signal(.mode, .{ .arith = mode });
    switch (flags) {
        .no_flags => {},
        .flags => switch (freshness) {
            .fresh => c.set_control_signal(.stat_op, .zncv_from_arith),
            .cont => c.set_control_signal(.stat_op, .zncv_from_arith_no_set_z),
        },
    }
}
pub fn jl_minus_k(c: *Cycle, freshness: Freshness, flags: Flags_Mode) void {
    var mode = Control_Signals.Arithmetic_Mode.jl_minus_k;
    mode.carry = freshness == .cont;
    c.set_control_signal(.unit, .alu);
    c.set_control_signal(.mode, .{ .arith = mode });
    switch (flags) {
        .no_flags => {},
        .flags => switch (freshness) {
            .fresh => c.set_control_signal(.stat_op, .zncv_from_arith),
            .cont => c.set_control_signal(.stat_op, .zncv_from_arith_no_set_z),
        },
    }
}
pub fn j_plus_k(c: *Cycle, k_ext: Zero_Sign_Or_One_Extension, freshness: Freshness, flags: Flags_Mode) void {
    const mode: Control_Signals.Arithmetic_Mode = .{
        .carry = freshness == .cont,
        .subtract = false,
        .k_ext = switch (k_ext) {
            .zx => .zx,
            ._1x => .@"1x",
            .sx => .sx,
        },
    };
    c.set_control_signal(.unit, .alu);
    c.set_control_signal(.mode, .{ .arith = mode });
    switch (flags) {
        .no_flags => {},
        .flags => switch (freshness) {
            .fresh => c.set_control_signal(.stat_op, .zncv_from_arith),
            .cont => c.set_control_signal(.stat_op, .zncv_from_arith_no_set_z),
        },
    }
}
pub fn j_minus_k(c: *Cycle, k_ext: Zero_Sign_Or_One_Extension, freshness: Freshness, flags: Flags_Mode) void {
    const mode: Control_Signals.Arithmetic_Mode = .{
        .carry = freshness == .cont,
        .subtract = true,
        .k_ext = switch (k_ext) {
            .zx => .zx,
            ._1x => .@"1x",
            .sx => .sx,
        },
    };
    c.set_control_signal(.unit, .alu);
    c.set_control_signal(.mode, .{ .arith = mode });
    switch (flags) {
        .no_flags => {},
        .flags => switch (freshness) {
            .fresh => c.set_control_signal(.stat_op, .zncv_from_arith),
            .cont => c.set_control_signal(.stat_op, .zncv_from_arith_no_set_z),
        },
    }
}

pub fn jlm_logic_km(c: *Cycle, op: Logic_Op) void {
    c.set_control_signal(.unit, .alu);
    c.set_control_signal(.mode, .{ .logic = switch (op) {
        .xor => Control_Signals.Logic_Mode.jlm_xor_km,
        .xnor => Control_Signals.Logic_Mode.jlm_xnor_km,
        ._or => Control_Signals.Logic_Mode.jlm_or_km,
        ._and => Control_Signals.Logic_Mode.jlm_and_km,
        .not_or => Control_Signals.Logic_Mode.not_jlm_or_km,
        .not_and => Control_Signals.Logic_Mode.not_jlm_and_km,
        .or_not => Control_Signals.Logic_Mode.jlm_or_not_km,
        .and_not => Control_Signals.Logic_Mode.jlm_and_not_km,
        .nand => Control_Signals.Logic_Mode.jlm_nand_km,
        .nor => Control_Signals.Logic_Mode.jlm_nor_km,
    }});
}

pub fn jl_shift_k4(c: *Cycle, dir: Shift_Direction) void {
    c.set_control_signal(.unit, .shift);
    c.set_control_signal(.mode, .{ .shift = switch (dir) {
        .left => Control_Signals.Shift_Mode.jl_shl_k4,
        .right => Control_Signals.Shift_Mode.jl_shr_k4,
    }});
}
pub fn jh_shift_k4(c: *Cycle, dir: Shift_Direction) void {
    c.set_control_signal(.unit, .shift);
    c.set_control_signal(.mode, .{ .shift = switch (dir) {
        .left => Control_Signals.Shift_Mode.jh_shl_k4,
        .right => Control_Signals.Shift_Mode.jh_shr_k4,
    }});
}
pub fn j_shift_k5(c: *Cycle, dir: Shift_Direction) void {
    c.set_control_signal(.unit, .shift);
    c.set_control_signal(.mode, .{ .shift = switch (dir) {
        .left => Control_Signals.Shift_Mode.j_shl_k5,
        .right => Control_Signals.Shift_Mode.j_shr_k5,
    }});
}

pub fn jl_times_k(c: *Cycle, jl_ext: Zero_Or_Sign_Extension, k_ext: Zero_Or_Sign_Extension) void {
    const mode: Control_Signals.Multiply_Mode = .{
        .jl = switch (jl_ext) {
            .sx => .signed,
            .zx => .unsigned,
        },
        .k = switch (k_ext) {
            .sx => .signed,
            .zx => .unsigned,
        },
        .swap_halves = false,
    };
    c.set_control_signal(.unit, .mult);
    c.set_control_signal(.mode, .{ .mult = mode });
}
pub fn jl_times_k__swap_result_halves(c: *Cycle, jl_ext: Zero_Or_Sign_Extension, k_ext: Zero_Or_Sign_Extension) void {
    const mode: Control_Signals.Multiply_Mode = .{
        .jl = switch (jl_ext) {
            .sx => .signed,
            .zx => .unsigned,
        },
        .k = switch (k_ext) {
            .sx => .signed,
            .zx => .unsigned,
        },
        .swap_halves = true,
    };
    c.set_control_signal(.unit, .mult);
    c.set_control_signal(.mode, .{ .mult = mode });
}

pub fn count_jlm(c: *Cycle, count_what: Bit_Count_Polarity, dir: Bit_Count_Direction) void {
    const mode: Control_Signals.Bit_Count_Mode = .{
        .invert_jl = switch (count_what) {
            .zeroes => true,
            .ones => false,
        },
        .leftmost_only = dir == .leading,
        .rightmost_only = dir == .trailing,
    };
    c.set_control_signal(.unit, .count);
    c.set_control_signal(.mode, .{ .count = mode });
}

//////////////
// to J bus //
//////////////

pub fn zero_to_j(c: *Cycle) void {
    c.set_control_signal(.jl_src, .zero);
    c.set_control_signal(.jh_src, .zero);
}

pub fn zero_to_jl(c: *Cycle) void {
    c.set_control_signal(.jl_src, .zero);
}

pub fn zero_to_jh(c: *Cycle) void {
    c.set_control_signal(.jh_src, .zero);
}

pub fn neg_one_to_jh(c: *Cycle) void {
    c.set_control_signal(.jh_src, .neg_one);
}

pub fn reg32_to_j(c: *Cycle) void {
    c.set_control_signal(.jl_src, .jrl);
    c.set_control_signal(.jh_src, .jrh);
}

pub fn reg_to_jl(c: *Cycle) void {
    c.set_control_signal(.jl_src, .jrl);
}

pub fn srh_to_jh(c: *Cycle, which: Control_Signals.Any_SR_Index) void {
    if (which.to_sr1_index()) |sr1| {
        c.set_control_signal(.sr1_ri, sr1);
        c.set_control_signal(.jh_src, .sr1h);
    } else if (which.to_sr2_index()) |sr2| {
        c.set_control_signal(.sr2_ri, sr2);
        c.set_control_signal(.jh_src, .sr2h);
    } else unreachable;
}

pub fn srl_to_jl(c: *Cycle, which: Control_Signals.Any_SR_Index) void {
    if (which.to_sr1_index()) |sr1| {
        c.set_control_signal(.sr1_ri, sr1);
        c.set_control_signal(.jl_src, .sr1l);
    } else if (which.to_sr2_index()) |sr2| {
        c.set_control_signal(.sr2_ri, sr2);
        c.set_control_signal(.jl_src, .sr2l);
    } else unreachable;
}

pub fn sr_to_j(c: *Cycle, which: Control_Signals.Any_SR_Index) void {
    if (which.to_sr1_index()) |sr1| {
        c.set_control_signal(.sr1_ri, sr1);
        c.set_control_signal(.jh_src, .sr1h);
        c.set_control_signal(.jl_src, .sr1l);
    } else if (which.to_sr2_index()) |sr2| {
        c.set_control_signal(.sr2_ri, sr2);
        c.set_control_signal(.jh_src, .sr2h);
        c.set_control_signal(.jl_src, .sr2l);
    } else unreachable;
}

//////////////
// to K bus //
//////////////

pub fn zero_to_k(c: *Cycle) void {
    c.set_control_signal(.k_src, .zero);
}

pub fn ij_ik_zx_to_k(c: *Cycle) void {
    c.set_control_signal(.k_src, .ij_ik_zx);
}

pub fn ik_zx_to_k(c: *Cycle) void {
    c.set_control_signal(.k_src, .ik_zx);
}

pub fn ik_bit_to_k(c: *Cycle) void {
    c.set_control_signal(.k_src, .ik_bit);
}

pub fn literal_to_k(c: *Cycle, literal: K_Literal) void {
    c.set_control_signal(.k_src, .literal_sx);
    c.set_control_signal(.literal, Control_Signals.Literal.init(@bitCast(literal)));
}

pub fn reg_to_k(c: *Cycle) void {
    c.set_control_signal(.k_src, .kr);
}

pub fn srl_to_k(c: *Cycle, which: Control_Signals.Any_SR_Index) void {
    if (which.to_sr1_index()) |sr1| {
        c.set_control_signal(.sr1_ri, sr1);
        c.set_control_signal(.k_src, .sr1l);
    } else if (which.to_sr2_index()) |sr2| {
        c.set_control_signal(.sr2_ri, sr2);
        c.set_control_signal(.k_src, .sr2l);
    } else unreachable;
}


//////////////
// to L bus //
//////////////

pub fn zero_to_lh(c: *Cycle) void {
    c.set_control_signal(.lh_src, .zero);
}

pub fn jh_to_lh(c: *Cycle) void {
    c.set_control_signal(.lh_src, .jh);
}

pub fn prev_uc_slot_to_lh(c: *Cycle) void {
    c.set_control_signal(.lh_src, .prev_uc_slot);
}

pub fn zero_to_ll(c: *Cycle) void {
    c.set_control_signal(.ll_src, .zero);
}

pub fn stat_to_ll(c: *Cycle) void {
    c.set_control_signal(.ll_src, .stat);
}

pub fn pipeline_id_to_ll(c: *Cycle) void {
    c.set_control_signal(.ll_src, .pipeline);
}

pub fn iw_ik_ij_zx_to_ll(c: *Cycle) void {
    c.set_control_signal(.ll_src, .iw_ik_ij_zx);
}

pub fn zero_to_l(c: *Cycle) void {
    c.set_control_signal(.ll_src, .zero);
    c.set_control_signal(.lh_src, .zero);
}

pub fn last_translation_info_to_l(c: *Cycle) void {
    c.set_control_signal(.ll_src, .translation_info_l);
    c.set_control_signal(.lh_src, .translation_info_h);
}


pub fn reg32_to_l(c: *Cycle) void {
    c.reg32_to_j();
    c.j_to_l();
}

pub fn srh_to_lh(c: *Cycle, which: Control_Signals.Any_SR_Index) void {
    c.srh_to_jh(which);   
    c.jh_to_lh();
}

pub fn srh_to_ll(c: *Cycle, which: Control_Signals.Any_SR_Index) void {
    c.srh_to_jh(which);   
    c.jh_to_ll();
}

pub fn srl_to_lh(c: *Cycle, which: Control_Signals.Any_SR_Index) void {
    c.srl_to_jl(which);
    c.jl_to_lh();
}

pub fn srl_to_ll(c: *Cycle, which: Control_Signals.Any_SR_Index) void {
    c.srl_to_jl(which);
    c.jl_to_ll();
}

pub fn sr_to_l(c: *Cycle, which: Control_Signals.Any_SR_Index) void {
    c.sr_to_j(which);
    c.j_to_l();
}

pub fn j_to_l(c: *Cycle) void {
    c.zero_to_k();
    c.j_shift_k5(.right);
    c.set_control_signal(.ll_src, .compute_l);
    c.set_control_signal(.lh_src, .compute_h);
}

pub fn jh_to_ll(c: *Cycle) void {
    if (!c.is_set(.jl_src)) {
        c.zero_to_jl();
    }
    c.zero_to_k();
    c.jh_shift_k4(.right);
    c.set_control_signal(.ll_src, .compute_l);
}

pub fn jl_to_ll(c: *Cycle) void {
    c.zero_to_k();
    c.jl_shift_k4(.right);
    c.set_control_signal(.ll_src, .compute_l);
}

pub fn jl_to_lh(c: *Cycle) void {
    c.zero_to_k();
    c.set_control_signal(.unit, .shift);
    c.set_control_signal(.mode, .{ .shift = .{
        .left = false,
        .early_swap16 = false,
        .late_swap16 = true,
        .wide = false
    }});
    c.set_control_signal(.lh_src, .compute_h);
}

pub fn k_to_ll(c: *Cycle) void {
    c.zero_to_jl();
    c.jl_plus_k_to_ll(.fresh, .no_flags);
}

pub fn k_to_lh(c: *Cycle) void {
    c.sr_to_j(.one);
    c.jl_times_k__swap_result_halves_to_l(.zx, .zx);
    c.compute_to_lh();
}

pub fn ij_ik_zx_to_ll(c: *Cycle) void {
    c.ij_ik_zx_to_k();
    c.k_to_ll();
}

pub fn ik_zx_to_ll(c: *Cycle) void {
    c.ik_zx_to_k();
    c.k_to_ll();
}

pub fn ik_bit_to_ll(c: *Cycle) void {
    c.ik_bit_to_k();
    c.k_to_ll();
}

pub fn jl_plus_k_to_ll(c: *Cycle, freshness: Freshness, flags: Flags_Mode) void {
    c.jl_plus_k(freshness, flags);
    c.set_control_signal(.ll_src, .compute_l);
}
pub fn jl_minus_k_to_ll(c: *Cycle, freshness: Freshness, flags: Flags_Mode) void {
    c.jl_minus_k(freshness, flags);
    c.set_control_signal(.ll_src, .compute_l);
}
pub fn j_plus_k_to_l(c: *Cycle, k_ext: Zero_Sign_Or_One_Extension, freshness: Freshness, flags: Flags_Mode) void {
    c.j_plus_k(k_ext, freshness, flags);
    c.set_control_signal(.ll_src, .compute_l);
    c.set_control_signal(.lh_src, .compute_h);
}
pub fn j_minus_k_to_l(c: *Cycle, k_ext: Zero_Sign_Or_One_Extension, freshness: Freshness, flags: Flags_Mode) void {
    c.j_plus_k(k_ext, freshness, flags);
    c.set_control_signal(.ll_src, .compute_l);
    c.set_control_signal(.lh_src, .compute_h);
}

pub fn jlm_logic_km_to_ll(c: *Cycle, op: Logic_Op, freshness: Freshness, flags: Flags_Mode) void {
    c.jlm_logic_km(op);
    c.compute_to_ll(freshness, flags);
}

pub fn jl_shift_k4_to_ll(c: *Cycle, dir: Shift_Direction, freshness: Freshness, flags: Flags_Mode) void {
    c.jl_shift_k4(dir);
    c.set_control_signal(.ll_src, .compute_l);
    switch (flags) {
        .no_flags => {},
        .flags => switch (freshness) {
            .fresh => c.set_control_signal(.stat_op, .zn_16__c_from_shift),
            .cont => c.set_control_signal(.stat_op, .zn_16_no_set_z__c_from_shift),
        },
    }
}
pub fn jh_shift_k4_to_ll(c: *Cycle, dir: Shift_Direction, freshness: Freshness, flags: Flags_Mode) void {
    c.jh_shift_k4(dir);
    c.set_control_signal(.ll_src, .compute_l);
    switch (flags) {
        .no_flags => {},
        .flags => switch (freshness) {
            .fresh => c.set_control_signal(.stat_op, .zn_16__c_from_shift),
            .cont => c.set_control_signal(.stat_op, .zn_16_no_set_z__c_from_shift),
        },
    }
}

pub fn jl_times_k_to_ll(c: *Cycle, jl_ext: Zero_Or_Sign_Extension, k_ext: Zero_Or_Sign_Extension, freshness: Freshness, flags: Flags_Mode) void {
    c.jl_times_k(jl_ext, k_ext);
    c.compute_to_ll(freshness, flags);
}
pub fn jl_times_k_to_l(c: *Cycle, jl_ext: Zero_Or_Sign_Extension, k_ext: Zero_Or_Sign_Extension, freshness: Freshness, flags: Flags_Mode) void {
    c.jl_times_k(jl_ext, k_ext);
    c.compute_to_l(freshness, flags);
}
pub fn jl_times_k__shr_16_to_ll(c: *Cycle, jl_ext: Zero_Or_Sign_Extension, k_ext: Zero_Or_Sign_Extension, freshness: Freshness, flags: Flags_Mode) void {
    c.jl_times_k__swap_result_halves(jl_ext, k_ext);
    c.compute_to_ll(freshness, flags);
}
pub fn jl_times_k__swap_result_halves_to_l(c: *Cycle, jl_ext: Zero_Or_Sign_Extension, k_ext: Zero_Or_Sign_Extension, freshness: Freshness, flags: Flags_Mode) void {
    c.jl_times_k__swap_result_halves(jl_ext, k_ext);
    c.compute_to_l(freshness, flags);
}

pub fn count_jlm_to_ll(c: *Cycle, count_what: Bit_Count_Polarity, dir: Bit_Count_Direction, freshness: Freshness, flags: Flags_Mode) void {
    c.count_jlm(count_what, dir);
    c.compute_to_l(freshness, flags);
}

pub fn compute_to_l(c: *Cycle, freshness: Freshness, flags: Flags_Mode) void {
    c.set_control_signal(.ll_src, .compute_l);
    c.set_control_signal(.lh_src, .compute_h);
    switch (flags) {
        .no_flags => {},
        .flags => c.zn_flags_from_l(freshness),
    }
}

pub fn compute_to_ll(c: *Cycle, freshness: Freshness, flags: Flags_Mode) void {
    c.set_control_signal(.ll_src, .compute_l);
    switch (flags) {
        .no_flags => {},
        .flags => c.zn_flags_from_ll(freshness),
    }
}

pub fn compute_to_lh(c: *Cycle) void {
    c.set_control_signal(.lh_src, .compute_h);
}

pub fn literal_to_ll(c: *Cycle, literal: K_Literal) void {
    if (literal == 0) {
        c.zero_to_ll();
    } else {
        c.zero_to_j();
        c.literal_to_k(literal);
        c.jlm_logic_km_to_ll(.xor, .fresh, .no_flags);
    }
}

pub fn literal_to_lh(c: *Cycle, literal: K_Literal) void {
    if (literal == 0) {
        c.zero_to_lh();
    } else {
        c.literal_to_k(literal);
        c.k_to_lh();
    }
}
pub fn literal_to_l(c: *Cycle, literal: K_Literal) void {
    if (literal == 0) {
        c.zero_to_l();
    } else {
        c.zero_to_j();
        c.literal_to_k(literal);
        c.j_plus_k_to_l(.sx, .fresh, .no_flags);
    }
}

pub fn dr_to_ll(c: *Cycle) void {
    c.set_control_signal(.at_op, .none);
    c.set_control_signal(.bus_dir, .write_from_dr);
    c.set_control_signal(.ll_src, .d);
}

pub fn d_to_ll(c: *Cycle) void {
    c.set_control_signal(.ll_src, .d);
}

pub fn d_to_l(c: *Cycle, ext: Zero_Sign_Or_One_Extension) void {
    switch (ext) {
        .zx => {
            c.set_control_signal(.ll_src, .d);
            c.set_control_signal(.lh_src, .zero);
        },
        .sx => {
            if (!c.is_set(.bus_width)) {
                c.warn(".bus_width not set!", .{});
                return;
            }
            switch (c.signals.bus_width) {
                .byte => {
                    c.set_control_signal(.ll_src, .d8_sx);
                    c.set_control_signal(.lh_src, .d8_sx);
                },
                .word => {
                    c.set_control_signal(.ll_src, .d);
                    c.set_control_signal(.lh_src, .d_sx);
                },
            }
        },
        ._1x => {
            if (c.signals.bus_width == .byte) {
                c.warn("._1x cannot be used when reading a byte value!", .{});
                return;
            }
            c.set_control_signal(.ll_src, .d);
            c.zero_to_j();
            c.zero_to_k();
            c.jlm_logic_km(.xnor);
            c.compute_to_lh();
        },
    }
}

// pub fn D_to_LH() void {
//     if (!is_set(.bus_byte)) {
//         warn("bus_byte not set!", .{});
//         return;
//     }
//     if (c.bus_byte == .byte) {
//         warn("D_to_LH() not supported for byte loads!", .{});
//         return;
//     }
//     set_control_signal(.lh_src, .d16);
// }

// pub fn D_to_LL() void {
//     if (!is_set(.bus_byte)) {
//         warn("bus_byte not set!", .{});
//         return;
//     }
//     if (c.bus_byte == .byte) {
//         warn("Use D8_to_LL() instead!", .{});
//         return;
//     }
//     set_control_signal(.ll_src, .d16);
// }

// pub fn D8_to_LL(ext: ZeroOrSignExtension) void {
//     if (!is_set(.bus_byte)) {
//         warn("bus_byte not set!", .{});
//         return;
//     }
//     if (c.bus_byte != .byte) {
//         warn("Use D_to_LL() instead!", .{});
//         return;
//     }
//     switch (ext) {
//         .zx => set_control_signal(.ll_src, .d16),
//         .sx => set_control_signal(.ll_src, .d8_sx),
//     }
// }


/////////////////////
// Register Writes //
/////////////////////

pub fn ll_to_reg(c: *Cycle) void {
    c.set_control_signal(.reg_write, .write_16);
}

pub fn l_to_reg32(c: *Cycle) void {
    c.set_control_signal(.reg_write, .write_32);
}

pub fn l_to_sr(c: *Cycle, which: Control_Signals.Any_SR_Index) void {
    if (which.to_sr1_index()) |sr1| {
        c.set_control_signal(.sr1_wi, sr1);
        c.set_control_signal(.sr1_wsrc, .l);
    } else if (which.to_sr2_index()) |sr2| {
        c.set_control_signal(.sr2_wi, sr2);
        c.set_control_signal(.sr2_wsrc, .l);
    } else {
        unreachable;
    }
}

pub fn virtual_address_to_sr(c: *Cycle, which: Control_Signals.Any_SR_Index) void {
    if (which.to_sr1_index()) |sr1| {
        c.set_control_signal(.sr1_wi, sr1);
        c.set_control_signal(.sr1_wsrc, .virtual_addr);
    } else if (which.to_sr2_index()) |sr2| {
        c.set_control_signal(.sr2_wi, sr2);
        c.set_control_signal(.sr2_wsrc, .virtual_addr);
    }
}

pub fn sr2_to_sr2(c: *Cycle, src_index: Control_Signals.SR2_Index, dest_index: Control_Signals.SR2_Index) void {
    c.set_control_signal(.sr2_ri, src_index);
    c.set_control_signal(.sr2_wi, dest_index);
    c.set_control_signal(.sr2_wsrc, .sr2);
}

pub fn ll_to_stat_zncv(c: *Cycle) void {
    c.set_control_signal(.stat_op, .load_zncv);
}
pub fn ll_to_stat_zncvka(c: *Cycle) void {
    c.set_control_signal(.stat_op, .load_zncvka);
}

pub fn zn_flags_from_l(c: *Cycle, freshness: Freshness) void {
    switch (freshness) {
        .fresh => c.set_control_signal(.stat_op, .zn_32),
        .cont => c.set_control_signal(.stat_op, .zn_32_no_set_z),
    }
}

pub fn zn_flags_from_ll(c: *Cycle, freshness: Freshness) void {
    switch (freshness) {
        .fresh => c.set_control_signal(.stat_op, .zn_16),
        .cont => c.set_control_signal(.stat_op, .zn_16_no_set_z),
    }
}

pub fn ll_to_dr(c: *Cycle) void {
    c.set_control_signal(.at_op, .none);
    c.set_control_signal(.bus_dir, .read_to_dr);
}

pub fn ll_to_rsn(c: *Cycle) void {
    c.set_control_signal(.special, .load_rsn_from_ll);
}

pub fn toggle_rsn(c: *Cycle) void {
    c.set_control_signal(.special, .toggle_rsn);
}

pub fn rsn_to_sr1h(c: *Cycle, index: Control_Signals.SR1_Index) void {
    c.set_control_signal(.sr1_ri, index);
    c.set_control_signal(.sr1_wi, index);
    c.set_control_signal(.sr1_wsrc, .rsn_sr1);
}

pub fn reload_asn(c: *Cycle) void {
    c.sr2_to_sr2(.asn, .asn);
}

pub fn next_ij(c: *Cycle, ij: hw.IJ.Raw) void {
    c.set_control_signal(.ij_op, .from_continuation);
    c.set_control_signal(.c_ij, hw.IJ.init(ij));
}

pub fn next_ik(c: *Cycle, ik: hw.IK.Raw) void {
    c.set_control_signal(.ik_op, .from_continuation);
    c.set_control_signal(.c_ik, hw.IK.init(ik));
}

pub fn next_iw(c: *Cycle, iw: hw.IW.Raw) void {
    c.set_control_signal(.iw_op, .from_continuation);
    c.set_control_signal(.c_iw, hw.IW.init(iw));
}

pub fn next_ik_bit(c: *Cycle, constant: hw.K.Raw) void {
    std.debug.assert(@popCount(constant) == 1);
    c.next_ik(@intCast(@ctz(constant)));
}

pub fn next_ij_ik_zx(c: *Cycle, constant: std.meta.Int(.unsigned, @bitSizeOf(hw.IJ) + @bitSizeOf(hw.IK))) void {
    c.next_ij(@intCast(constant >> @bitSizeOf(hw.IK)));
    c.next_ik(@truncate(constant));
}

pub fn next_ij_xor1(c: *Cycle) void {
    c.set_control_signal(.ij_op, .xor1);
}

pub fn next_ik_xor1(c: *Cycle) void {
    c.set_control_signal(.ik_op, .xor1);
}

pub fn next_iw_xor1(c: *Cycle) void {
    c.set_control_signal(.iw_op, .xor1);
}

/////////////////////
// Addresses & Bus //
/////////////////////

pub fn enable_address_translation(c: *Cycle) void {
    c.set_control_signal(.stat_op, .set_a);
}
pub fn disable_address_translation(c: *Cycle) void {
    c.set_control_signal(.stat_op, .clear_a);
}

pub fn update_address_translation_from_l(c: *Cycle, base: Control_Signals.Any_SR_Index, group: hw.addr.translation.Entry_Group) void {
    c.address(base, 0);
    c.set_control_signal(.at_op, .update);
    c.set_control_signal(.addr_space, @as(Control_Signals.Address_Space, switch (group) {
        .data_read, .data_write => .data,
        .stack => .stack,
        .insn => .insn,
    }));
    c.set_control_signal(.bus_dir, @as(Control_Signals.Bus_Direction, switch (group) {
        .data_write => .write_from_ll,
        .data_read, .stack, .insn => .read,
    }));
    c.set_control_signal(.bus_width, .word);
}

pub fn invalidate_address_translation_from_l(c: *Cycle, base: Control_Signals.Any_SR_Index, group: hw.addr.translation.Entry_Group) void {
    c.address(base, 0);
    c.set_control_signal(.at_op, .invalidate);
    c.set_control_signal(.addr_space, @as(Control_Signals.Address_Space, switch (group) {
        .data_read, .data_write => .data,
        .stack => .stack,
        .insn => .insn,
    }));
    c.set_control_signal(.bus_dir, @as(Control_Signals.Bus_Direction, switch (group) {
        .data_write => .write_from_ll,
        .data_read, .stack, .insn => .read,
    }));
    c.set_control_signal(.bus_width, .word);
}

pub fn address(c: *Cycle, base: Control_Signals.Any_SR_Index, offset: Address_Offset) void {
    c.set_control_signal(.base_ri, base);

    if (offset == 0) {
        c.set_control_signal(.offset_src, .zero);
    } else if (offset == 2) {
        c.set_control_signal(.offset_src, .two);
    } else {
        var raw_offset: Control_Signals.Literal = undefined;
        if (offset < 0) {
            raw_offset = Control_Signals.Literal.init(@intCast(@as(i8, offset) + 64));
            c.set_control_signal(.offset_src, .literal_minus_64);
        } else {
            raw_offset = Control_Signals.Literal.init(@intCast(offset));
            c.set_control_signal(.offset_src, .literal);
        }
        c.set_control_signal(.literal, raw_offset);
    }
}

pub fn read_to_d(c: *Cycle, base: Control_Signals.Any_SR_Index, offset: Address_Offset, width: Control_Signals.Bus_Width, space: Control_Signals.Address_Space) void {
    c.address(base, offset);
    c.set_control_signal(.at_op, .translate);
    c.set_control_signal(.addr_space, space);
    c.set_control_signal(.bus_width, width);
    c.set_control_signal(.bus_dir, .read);
    if (base == .ip and space == .insn) {
        if (c.encoding_len) |len| {
            var end_offset: i64 = offset;
            end_offset += switch (width) {
                .byte => 1,
                .word => 2,
            };
            if (end_offset > len) {
                c.warn("IP-relative {s} read at offset {} is not contained within the expected encoding length of {} ", .{ @tagName(width), offset, len });
            }
        } else {
            c.warn("Cycle performs an IP-relative read, but there is no instruction encoding corresponding to this microcode sequence", .{});
        }
    }
}

pub fn read_to_dr(c: *Cycle, base: Control_Signals.Any_SR_Index, offset: Address_Offset, width: Control_Signals.Bus_Width, space: Control_Signals.Address_Space) void {
    c.address(base, offset);
    c.set_control_signal(.at_op, .translate);
    c.set_control_signal(.addr_space, space);
    c.set_control_signal(.bus_width, width);
    c.set_control_signal(.bus_dir, .read_to_dr);
    if (base == .ip and space == .insn) {
        if (c.encoding_len) |len| {
            var end_offset: i64 = offset;
            end_offset += switch (width) {
                .byte => 1,
                .word => 2,
            };
            if (end_offset > len) {
                c.warn("IP-relative {s} read at offset {} is not contained within the expected encoding length of {} ", .{ @tagName(width), offset, len });
            }
        } else {
            c.warn("Cycle performs an IP-relative read, but there is no instruction encoding corresponding to this microcode sequence", .{});
        }
    }
}

pub fn ip_read_to_d(c: *Cycle, offset: Address_Offset, width: Control_Signals.Bus_Width) void {
    c.read_to_d(.ip, offset, width, .insn);
}

pub fn ip_read_to_dr(c: *Cycle, offset: Address_Offset, width: Control_Signals.Bus_Width) void {
    c.read_to_dr(.ip, offset, width, .insn);
}

pub fn write_from_ll(c: *Cycle, base: Control_Signals.Any_SR_Index, offset: Address_Offset, width: Control_Signals.Bus_Width, space: Control_Signals.Address_Space) void {
    c.address(base, offset);
    c.set_control_signal(.at_op, .translate);
    c.set_control_signal(.addr_space, space);
    c.set_control_signal(.bus_width, width);
    c.set_control_signal(.bus_dir, .write_from_ll);
}

pub fn write_from_dr(c: *Cycle, base: Control_Signals.Any_SR_Index, offset: Address_Offset, width: Control_Signals.Bus_Width, space: Control_Signals.Address_Space) void {
    c.address(base, offset);
    c.set_control_signal(.at_op, .translate);
    c.set_control_signal(.addr_space, space);
    c.set_control_signal(.bus_width, width);
    c.set_control_signal(.bus_dir, .write_from_dr);
}

pub fn block_transfer_to_ram(c: *Cycle, base: Control_Signals.Any_SR_Index, preincrement: Address_Offset, space: Control_Signals.Address_Space) void {
    c.address(base, preincrement);
    c.set_control_signal(.at_op, .translate);
    c.set_control_signal(.addr_space, space);
    c.set_control_signal(.bus_width, .word);
    c.set_control_signal(.bus_dir, .write_from_ll);
    c.set_control_signal(.special, .block_transfer);
    c.virtual_address_to_sr(base);
}

pub fn block_transfer_from_ram(c: *Cycle, base: Control_Signals.Any_SR_Index, preincrement: Address_Offset, space: Control_Signals.Address_Space) void {
    c.address(base, preincrement);
    c.set_control_signal(.at_op, .translate);
    c.set_control_signal(.addr_space, space);
    c.set_control_signal(.bus_width, .word);
    c.set_control_signal(.bus_dir, .read);
    c.set_control_signal(.special, .block_transfer);
    c.virtual_address_to_sr(base);
}

// pub fn ik_reg_to_k(c: *Cycle) void {
//     set_control_signal(.k_src, .kr);
// }

// pub fn ik_reg_to_ll(c: *Cycle) void {
//     set_control_signal(.jl_src, .zero);
//     set_control_signal(.k_src, .kr);
//     set_control_signal(.compute_mode, .{ .logic = .jl_xor_k });
//     set_control_signal(.ll_src, .logic);
// }

// pub fn ik_to_k(c: *Cycle) void {
//     set_control_signal(.k_src, .ik_zx);
// }

// pub fn ik_to_ll(c: *Cycle) void {
//     set_control_signal(.jl_src, .zero);
//     set_control_signal(.k_src, .ob_oa_zx);
//     set_control_signal(.compute_mode, .{ .logic = .jl_xor_k });
//     set_control_signal(.ll_src, .logic);
// }

// pub fn ik_to_lh(c: *Cycle) void {
//     set_control_signal(.jl_src, .zero);
//     set_control_signal(.k_src, .ik_zx);
//     set_control_signal(.compute_mode, .{ .logic = .jl_xor_k });
//     set_control_signal(.lh_src, .logic);
// }

// pub fn JL_to_L_zx() void {
//     zero_to_K();
//     set_control_signal(.compute_mode, .{ .logic = .jl_xor_k });
//     set_control_signal(.ll_src, .logic);
//     set_control_signal(.lh_src, .zero);
// }

// pub fn JL_to_LL_and_LH() void {
//     zero_to_K();
//     set_control_signal(.compute_mode, .{ .logic = .jl_xor_k });
//     set_control_signal(.ll_src, .logic);
//     set_control_signal(.lh_src, .logic);
// }

// pub fn JL_to_LH() void {
//     zero_to_JH();
//     zero_to_K();
//     set_control_signal(.compute_mode, .{ .shift = .jh_shr_k4 });
//     set_control_signal(.lh_src, .shift_h);
// }

// pub fn K_to_L(ext: ZeroSignOrOneExtension) void {
//     zero_to_J();
//     set_control_signal(.compute_mode, .{ .arith = switch (ext) {
//         .zx => .add_J_K_zx,
//         .sx => .add_J_K_sx,
//         ._1x => .add_J_K_1x,
//     } });
//     set_control_signal(.ll_src, .arith_l);
//     set_control_signal(.lh_src, .arith_h);
// }
// pub fn K_to_LL() void {
//     zero_to_JL();
//     set_control_signal(.compute_mode, .{ .logic = .jl_xor_k });
//     set_control_signal(.ll_src, .logic);
// }

// pub fn STAT_to_L() void {
//     set_control_signal(.ll_src, .stat);
//     set_control_signal(.lh_src, .zero);
// }

// pub fn LL_to_D() void {
//     set_control_signal(.at_op, .none);
//     set_control_signal(.bus_mode, .data);
//     set_control_signal(.bus_byte, .word);
//     set_control_signal(.bus_rw, .write);
// }

// pub fn ZN_from_LL(freshness: Freshness) void {
//     switch (freshness) {
//         .fresh => set_control_signal(.stat_op, .zn_from_ll),
//         .cont => set_control_signal(.stat_op, .zn_from_ll_no_set_z),
//     }
// }
// pub fn ZN_from_L(freshness: Freshness) void {
//     switch (freshness) {
//         .fresh => set_control_signal(.stat_op, .zn_from_l),
//         .cont => set_control_signal(.stat_op, .zn_from_l_no_set_z),
//     }
// }

// pub fn LL_to_ZNVC() void {
//     set_control_signal(.stat_op, .load_znvc_from_ll);
// }



pub fn load_next_insn(c: *Cycle) void {
    if (c.encoding_len) |offset| {
        c.address(.ip, offset);
        c.set_control_signal(.at_op, .translate);
        c.set_control_signal(.addr_space, .insn);
        c.set_control_signal(.bus_width, .word);
        c.set_control_signal(.bus_dir, .read_to_dr);
        c.set_control_signal(.sr2_wi, .next_ip);
        c.set_control_signal(.sr2_wsrc, .virtual_addr);
        c.assume_next_insn_loaded();
    } else {
        c.warn("Cycle loads next instruction, but there is no instruction encoding corresponding to this microcode sequence", .{});
    }
}

pub fn assume_next_insn_loaded(c: *Cycle) void {
    c.flags.insert(.next_insn_loaded);
}

pub fn exec_next_insn(c: *Cycle) void {
    if (!c.flags.contains(.next_insn_loaded)) {
        c.warn("Cycle executes next instruction, but it has not been loaded yet, or has been clobbered", .{});
    }

    if (c.is_set(.sr2_ri) and c.signals.sr2_ri != .next_ip) {
        c.address(.next_ip, 0);
        c.set_control_signal(.sr2_wsrc, .virtual_addr);
    } else {
        c.set_control_signal(.sr2_ri, .next_ip);
        c.set_control_signal(.sr2_wsrc, .sr2);
    }
    c.set_control_signal(.sr2_wi, .ip);
    c.decode_and_exec_dr(.normal);
}

pub fn load_and_exec_next_insn(c: *Cycle) void {
    if (c.encoding_len) |offset| {
        c.branch(.ip, offset);
    } else {
        c.warn("Cycle loads next instruction, but there is no instruction encoding corresponding to this microcode sequence", .{});
    }
}

pub fn branch(c: *Cycle, base: Control_Signals.Any_SR_Index, offset: Address_Offset) void {
    c.address(base, offset);
    c.set_control_signal(.at_op, .translate);
    c.set_control_signal(.addr_space, .insn);
    c.set_control_signal(.bus_width, .word);
    c.set_control_signal(.bus_dir, .read_to_dr);
    if (base != .ip or offset != 0) {
        c.set_control_signal(.sr2_wi, .ip);
        c.set_control_signal(.sr2_wsrc, .virtual_addr);
    }
    c.decode_and_exec_dr(.normal);
}

pub fn decode_and_exec_dr(c: *Cycle, id_mode: Control_Signals.ID_Mode) void {
    c.decode_dr_to_ij_ik_iw(id_mode);
    c.allow_interrupt();
    c.set_control_signal(.seq_op, .next_instruction);
    if (c.is_set(.special)) {
        switch (c.signals.special) {
            .none => c.set_control_signal(.special, .atomic_end),
            .atomic_end, .atomic_next, .atomic_this => {},
            .load_rsn_from_ll, .toggle_rsn, .trigger_fault, .block_transfer => {
                c.warn("Can't decode the next instruction in the same cycle as {}", .{ c.signals.special });
            },
        }
    } else {
        c.set_control_signal(.special, .atomic_end);
    }
}

pub fn decode_dr_to_ij_ik_iw(c: *Cycle, id_mode: Control_Signals.ID_Mode) void {
    c.set_control_signal(.id_mode, id_mode);
    c.set_control_signal(.ij_op, .from_decode);
    c.set_control_signal(.ik_op, .from_decode);
    c.set_control_signal(.iw_op, .from_decode);
}

pub fn decode_dr_to_ij(c: *Cycle, id_mode: Control_Signals.ID_Mode) void {
    c.set_control_signal(.id_mode, id_mode);
    c.set_control_signal(.ij_op, .from_decode);
}

pub fn decode_dr_to_ik(c: *Cycle, id_mode: Control_Signals.ID_Mode) void {
    c.set_control_signal(.id_mode, id_mode);
    c.set_control_signal(.ik_op, .from_decode);
}

pub fn decode_dr_to_iw(c: *Cycle, id_mode: Control_Signals.ID_Mode) void {
    c.set_control_signal(.id_mode, id_mode);
    c.set_control_signal(.iw_op, .from_decode);
}

pub fn assume_ij_valid(c: *Cycle) void {
    c.flags.insert(.ij_valid);
}
pub fn assume_ik_valid(c: *Cycle) void {
    c.flags.insert(.ik_valid);
}
pub fn assume_iw_valid(c: *Cycle) void {
    c.flags.insert(.iw_valid);
}

pub fn next(c: *Cycle, func: *const anyopaque) void {
    std.debug.assert(c.recursion_ptr == null);
    c.recursion_ptr = func;
}

pub fn force_normal_execution(c: *Cycle, func: *const anyopaque) void {
    c.set_control_signal(.seq_op, .next_uop_force_normal);
    c.next(func);
}

pub fn allow_interrupt(c: *Cycle) void {
    c.set_control_signal(.allow_int, true);
}

pub fn fault_return(c: *Cycle) void {
    c.set_control_signal(.seq_op, .fault_return);
    c.sr_to_l(.fault_uc_slot_dr); // .fault_return implies LH -> microcode slot
}

fn trigger_fault(c: *Cycle, slot: hw.microcode.Slot) void {
    c.set_control_signal(.special, .trigger_fault);
    c.set_control_signal(.seq_op, .next_uop);
    c.set_control_signal(.c_ij, slot.ij());
    c.set_control_signal(.c_ik, slot.ik());
    c.set_control_signal(.c_iw, slot.iw());
}
pub fn illegal_instruction(c: *Cycle) void {
    c.trigger_fault(.instruction_protection_fault);
}
pub fn invalid_instruction(c: *Cycle) void {
    c.trigger_fault(.invalid_instruction);
}

pub fn atomic_this_cycle(c: *Cycle) void {
    c.set_control_signal(.special, .atomic_this);
}

pub fn atomic_next_cycle_until_end(c: *Cycle) void {
    c.set_control_signal(.special, .atomic_next);
}

// pub fn load_and_exec_next_insn_no_atomic_end(ip_offset: Address_Offset) void {
//     set_control_signal(.special, .none); // don't clear atomic state
//     load_and_exec_next_insn(ip_offset);
// }

// pub fn exec_next_insn_no_atomic_end() void {
//     if (c.special != .block_transfer) {
//         set_control_signal(.special, .none); // don't clear atomic state
//     }
//     exec_next_insn();
// }

// pub fn prev_UA_to_LH() void {
//     set_control_signal(.lh_src, .prev_ua);
// }

pub const Flags_Mode = enum {
    no_flags,
    flags,
};

pub const Freshness = enum {
    fresh,
    cont,
};

pub const Zero_Or_Sign_Extension = enum {
    zx,
    sx,
};

pub const Zero_Sign_Or_One_Extension = enum {
    zx,
    sx,
    _1x,
};

pub const Logic_Op = enum {
    xor,
    xnor,
    _or,
    _and,
    not_or,
    not_and,
    or_not,
    and_not,
    nand,
    nor,
};

pub const Shift_Direction = enum {
    left,
    right,
};

pub const Swap_Halves = enum {
    normal,
    swap,
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

pub const Address_Offset = i7;
pub const K_Literal = i6;

const log = std.log.scoped(.cycle);

const Cycle = @This();
const Microcode_Builder = @import("Microcode_Builder.zig");
const Control_Signals = hw.Control_Signals;
const Control_Signal = hw.Control_Signal;
const Register_Index = hw.Register_Index;
const Encoded_Instruction = isa.Encoded_Instruction;
const isa = arch.isa;
const hw = arch.hw;
const arch = @import("lib_arch");
const bits = @import("bits");
const std = @import("std");
