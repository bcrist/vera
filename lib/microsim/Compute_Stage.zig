pipeline: hw.Pipeline,
cs: *const Control_Signals,

exec_mode: hw.Execution_Mode,
rsn: hw.RSN,
uc_slot: hw.microcode.Slot,
dr: hw.D,
ij: hw.IJ,
ik: hw.IK,
iw: hw.IW,
asn: at.ASN,
last_translation: at.Info,
stat_c: bool,
stat_v: bool,
stat_n: bool,
stat_z: bool,
stat_k: bool,
stat_a: bool,
next_k: bool,
want_atomic: bool,
stall_atomic: bool,

sr1: hw.SR,
sr2: hw.SR,
j: hw.J,
k: hw.K,
virtual_addr: hw.addr.Virtual,

pub const Debug = struct {};

pub fn is_atomic(self: Compute_Stage) bool {
    return self.want_atomic and !self.stall_atomic;
}

pub fn simulate(
    in: Compute_Stage,
    out: *Transact_Stage,
    reset: bool,
    translations: []const at.Entry_Pair
) Debug {
    switch (in.cs.unit) {
        .alu => in.simulate_alu(out),
        .shift => in.simulate_shift(out),
        .mult => in.simulate_mult(out),
        .count => in.simulate_count(out),
    }

    simulate_address_translator(in, out, translations);

    out.sr1 = in.sr1;
    out.sr2 = in.sr2;
    out.jh = in.j.hi;

    out.special_fault = in.cs.special == .trigger_fault;
    out.any_fault = out.special_fault or out.page_fault or out.page_align_fault or out.access_fault;

    out.inhibit_writes = in.stall_atomic or out.any_fault or reset;

    out.pipeline = in.pipeline;
    out.cs = in.cs;
    out.exec_mode = in.exec_mode;
    out.rsn = in.rsn;
    out.uc_slot = in.uc_slot;
    out.dr = in.dr;
    out.ij = in.ij;
    out.ik = in.ik;
    out.iw = in.iw;
    out.asn = in.asn;
    out.last_translation = in.last_translation;
    out.stat_c = in.stat_c;
    out.stat_v = in.stat_v;
    out.stat_n = in.stat_n;
    out.stat_z = in.stat_z;
    out.stat_k = in.stat_k;
    out.stat_a = in.stat_a;
    out.next_k = in.next_k;
    out.want_atomic = in.want_atomic;
    out.stall_atomic = in.stall_atomic;

    return .{};
}

fn simulate_alu(in: Compute_Stage, out: *Transact_Stage) void {
    const op: u2 = @truncate(in.cs.mode.raw());
    if (op == 3) {
        const mode = in.cs.mode.arith;

        const j: u32 = in.j.raw();
        var k: u32 = switch (mode.k_ext) {
            .zx, .none => in.k.raw(),
            .sx => bits.sx(u32, in.k.raw()),
            .@"1x" => bits._1x(u32, in.k.raw()),
        };
        var c_in: u1 = if (mode.carry) @intFromBool(in.stat_c) else if (mode.subtract) 1 else 0;

        if (mode.subtract) k = ~k;

        const raw = @as(u33, j) +% k +% c_in;
        out.alu_data = .{
            .lo = hw.LL.init(@truncate(raw)),
            .hi = hw.LH.init(@truncate(raw >> 16)),
        };

        if (mode.k_ext == .none) {
            const j15: u1 = @truncate(j >> 15);
            const k15: u1 = @truncate(k >> 15);
            const s15: u1 = @truncate(raw >> 15);

            out.alu_z = @as(u16, @truncate(raw)) == 0;
            out.alu_n = s15 != 0;
            out.alu_v = j15 == k15 and j15 != s15;
            out.alu_c = @as(u1, @truncate(raw >> 16)) != 0;
        } else {
            const j31: u1 = @truncate(j >> 31);
            const k31: u1 = @truncate(k >> 31);
            const s31: u1 = @truncate(raw >> 31);

            out.alu_z = @as(u32, @truncate(raw)) == 0;
            out.alu_n = s31 != 0;
            out.alu_v = j31 == k31 and j31 != s31;
            out.alu_c = @as(u1, @truncate(raw >> 32)) != 0;
        }

    } else {
        const mode = in.cs.mode.logic;

        var jl = in.j.lo.raw();
        var k = in.k.raw();

        if (mode.invert_jl) jl = ~jl;
        if (mode.invert_k) k = ~k;

        const result = switch (mode.op) {
            .xor => jl ^ k,
            .@"or" => jl | k,
            .@"and" => jl & k,
        };

        out.alu_data.lo = hw.LL.init(result);
    }
}
fn simulate_shift(in: Compute_Stage, out: *Transact_Stage) void {
    const mode = in.cs.mode.shift;

    const k = in.k.raw();

    const k0 = (k & 1) == 1;
    const k1 = (k & 2) == 2;
    const k2 = (k & 4) == 4;
    const k3 = (k & 8) == 8;
    const k4 = (k & 16) == 16;

    var j = in.j.raw();

    if (mode.left) {
        j = bits.swapHalves(u32, @bitReverse(j));
    }

    if (mode.wide and k4) {
        if (!mode.early_swap16) {
            j = bits.swapHalves(u32, j);
        }
    } else if (mode.early_swap16) {
        j = bits.swapHalves(u32, j);
    }

    var c: u1 = 0;
    var overflow = false;

    if (mode.wide and k4) {
        c = @truncate(j >> 31);
        if ((j & 0xFFFF_0000) != 0) {
            overflow = true;
        }
        j = j & 0x0000_FFFF;
    }

    if (k2) {
        if (@as(u4, @truncate(j)) != 0) {
            overflow = true;
        }
        j >>= 3;
        c = @truncate(j);
        j >>= 1;
    }
    if (k1) {
        if (@as(u2, @truncate(j)) != 0) {
            overflow = true;
        }
        j >>= 1;
        c = @truncate(j);
        j >>= 1;
    }
    if (k0) {
        if (@as(u1, @truncate(j)) != 0) {
            overflow = true;
        }
        c = @truncate(j);
        j >>= 1;
    }

    if (k3) {
        if (@as(u8, @truncate(j)) != 0) {
            overflow = true;
        }
        j >>= 7;
        c = @truncate(j);
        j >>= 1;
    }

    if (mode.late_swap16) {
        j = bits.swapHalves(u32, j);
    }

    if (mode.left) {
        // reverse bits in top and lower halves of J:
        j = bits.swapHalves(u32, @bitReverse(j));
    }

    out.shift_data = hw.L.init(j);
    out.shift_c = c != 0;
    out.shift_v = overflow;

    if (mode.wide) {
        out.shift_z = j == 0;
        out.shift_n = (j >> (@bitSizeOf(hw.J) - 1)) != 0;
    } else {
        out.shift_z = out.shift_data.lo.raw() == 0;
        out.shift_n = (out.shift_data.lo.raw() >> (@bitSizeOf(hw.JL) - 1)) != 0;
    }
}
fn simulate_mult(in: Compute_Stage, out: *Transact_Stage) void {
    const mode = in.cs.mode.mult;
    const signed_result = mode.jl == .signed or mode.k == .signed;

    const j = switch (mode.jl) {
        .unsigned => bits.zx(i64, in.j.lo.raw()),
        .signed => bits.sx(i64, in.j.lo.raw()),
    };

    const k = switch (mode.k) {
        .unsigned => bits.zx(i64, in.k.raw()),
        .signed => bits.sx(i64, in.k.raw()),
    };

    const result: u64 = @bitCast(j * k);
    const result_l = hw.L.init(@truncate(result));

    out.mult_z = result_l.raw() == 0;
    out.mult_n = signed_result and 0 != (result_l.raw() >> (@bitSizeOf(hw.L.Raw) - 1));
    out.mult_v = false;
    if (mode.wide) {
        const expected = if (signed_result) hw.L.init(bits.sx(hw.L.Raw, result_l.lo.raw())).hi else hw.LH.init(0);
        out.mult_v = result_l.hi != expected;
    }

    out.mult_data = if (mode.swap_halves) .{
        .lo = hw.LL.init(result_l.hi.raw()),
        .hi = hw.LH.init(result_l.lo.raw()),
    } else result_l;
}
fn simulate_count(in: Compute_Stage, out: *Transact_Stage) void {
    const mode = in.cs.mode.count;

    var jl = in.j.lo.raw();
    var k = in.k.raw();

    if (mode.invert_jl) jl = ~jl;

    const jlm = jl & k;

    var mask: u16 = undefined;
    if (!mode.leftmost_only and !mode.rightmost_only) {
        mask = 0xFFFF;
    } else {
        mask = 0;
        if (mode.leftmost_only) {
            const left_bits = @clz(~jlm);
            if (left_bits > 0) {
                mask |= @as(u16, 0xFFFF) << @intCast(16 - left_bits);
            }
        }
        if (mode.rightmost_only) {
            const right_bits = @ctz(~jlm);
            if (right_bits > 0) {
                mask |= @as(u16, 0xFFFF) >> @intCast(16 - right_bits);
            }
        }
    }

    const jlm2 = jlm & mask;

    out.count_data = hw.L.init(@popCount(jlm2));
    out.count_z = jlm2 == 0;
}

fn simulate_address_translator(in: Compute_Stage, out: *Transact_Stage, translations: []const at.Entry_Pair) void {
    const group: at.Entry_Group = switch (in.cs.addr_space) {
        .raw, .data => switch (in.cs.bus_dir) {
            .write_from_ll, .write_from_dr => .data_write,
            .none, .read => .data_read,
        },
        .stack => .stack,
        .insn => .insn,
    };

    const virtual = in.virtual_addr;

    const entry_address: at.Entry_Address = .{
        .slot = virtual.page.slot,
        .group = group,
        .asn = in.asn,
    };

    const entries = translations[entry_address.raw()];

    const primary_match = entries.primary.present and entries.primary.tag == virtual.page.tag;
    const secondary_match = entries.secondary.present and entries.secondary.tag == virtual.page.tag;
    const any_match = primary_match or secondary_match;

    var matching = entries.primary;
    var other = entries.secondary;
    if (in.cs.at_op != .none and !primary_match and (secondary_match or in.cs.at_op == .update)) {
        matching = entries.secondary;
        other = entries.primary;
    }

    const translate = in.cs.at_op == .translate;

    const insn_load = translate and in.cs.sr2_wsrc == .virtual_addr and (in.cs.sr2_wi == .ip or in.cs.sr2_wi == .next_ip);

    const enabled = in.stat_a and in.cs.addr_space != .raw;

    const enabled_translate = enabled and translate;
    const disabled_translate = !enabled and translate;

    out.at_entry_addr = entry_address;
    out.at_info = .{
        .bus_dir = in.cs.bus_dir,
        .bus_width = in.cs.bus_width,
        .addr_space = in.cs.addr_space,
        .at_op = in.cs.at_op,
        .slot = virtual.page.slot,
        .tag = virtual.page.tag,
    };
    out.at_matching_entry = matching;
    out.at_other_entry = other;

    out.physical_addr = .{
        .offset = virtual.offset,
        .frame = hw.addr.Frame.init(@truncate(virtual.page.raw())),
    };
    out.at_k = in.stat_k;
    out.page_fault = enabled_translate and !any_match;
    out.page_align_fault = false;
    out.access_fault = false;

    if (enabled_translate and any_match) {
        out.physical_addr.frame = matching.frame;

        switch (matching.access) {
            .unprivileged => {
                if (insn_load) {
                    out.at_k = false;
                }
            },
            .kernel_entry_256 => {
                if (in.stat_k or (virtual.offset.raw() & 0xFF) == 0) {
                    if (insn_load) {
                        out.at_k = true;
                    }
                } else {
                    out.access_fault = true;
                }
            },
            .kernel_entry_4096 => {
                if (in.stat_k or virtual.offset.raw() == 0) {
                    if (insn_load) {
                        out.at_k = true;
                    }
                } else {
                    out.access_fault = true;
                }
            },
            .kernel_private => {
                if (!in.stat_k) {
                    out.access_fault = true;
                }
            },
        }
    } else if (disabled_translate) {
        if (in.cs.addr_space == .insn) {
            out.at_k = true;
        }
    }

    if (virtual.offset == hw.addr.Offset.max and in.cs.bus_width == .word) {
        out.page_align_fault = true;
    }

    // if ((virtual.offset & 1) == 1) {
    //     out.base.bus_ctrl.swap_bytes = true;
    //     if (in.cs_bus_byte == .word) {
    //         out.base.bus_ctrl.even_offset +%= 1;
    //     }
    // }

    // if (translate) {
    //     if (in.cs_bus_rw == .read) {
    //         out.base.bus_ctrl.read = true;
    //     } else if (in.cs_bus_rw == .write) {
    //         out.base.bus_ctrl.write = true;
    //         if (in.cs_bus_byte == .word) {
    //             out.base.bus_ctrl.write_even = true;
    //             out.base.bus_ctrl.write_odd = true;
    //         } else if ((virtual.offset & 1) == 1) {
    //             out.base.bus_ctrl.write_odd = true;
    //         } else {
    //             out.base.bus_ctrl.write_even = true;
    //         }
    //     }
    // }
}

// pipe: misc.PipeID,
// cs: ControlSignals,
// reg: LoopRegisters,
// want_atomic: bool,
// stall_atomic: bool,
// sr1: bus.JParts,
// sr2: bus.JParts,
// jh: bus.JHigh,
// arith: arithmetic_unit.Outputs,
// shift: shifter_unit.Outputs,
// mult: multiplier_unit.Outputs,
// logic: bus.JLow,
// virtual_address: bus.VirtualAddressParts,
// at: AddressTranslator.ComputeOutputsExceptFaults,
// fault: faults.ComputeOutputs,
// inhibit_writes: bool,


const Compute_Stage = @This();
const Transact_Stage = @import("Transact_Stage.zig");
const Control_Signals = hw.Control_Signals;
const at = hw.addr.translation;
const hw = arch.hw;
const arch = @import("lib_arch");
const bits = @import("bits");
const std = @import("std");
