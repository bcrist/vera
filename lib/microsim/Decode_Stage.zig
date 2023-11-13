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

seq_literal: hw.microcode.Slot,
uc_slot_src: hw.microcode.Slot_Source,

pub const Debug = struct {};

pub fn simulate(
    in: Decode_Stage,
    out: *Setup_Stage,
    decode_rom: []const hw.decode.Result,
    microcode_rom: []const Control_Signals,
    atomic_busy: bool,
) Debug {
    const id_result = decode_rom[in.dr.raw()];

    out.ij = switch (in.cs.ij_op) {
        .hold => in.ij,
        .xor1 => hw.IJ.init(in.ij.raw() ^ 1),
        .from_continuation => in.cs.c.ij,
        .from_decode => id_result.ij,
    };
    out.ik = switch (in.cs.ik_op) {
        .hold => in.ik,
        .xor1 => hw.IK.init(in.ik.raw() ^ 1),
        .from_continuation => in.cs.c.ik,
        .from_decode => id_result.ik,
    };
    out.iw = switch (in.cs.iw_op) {
        .hold => in.iw,
        .xor1 => hw.IW.init(in.iw.raw() ^ 1),
        .from_continuation => in.cs.c.iw,
        .from_decode => id_result.iw,
    };

    const uc_slot = switch (in.uc_slot_src) {
        .hold => in.uc_slot,
        .insn_decoder => id_result.slot,
        .continuation => blk: {
            const ij_op = in.cs.ij_op;
            const ik_op = in.cs.ik_op;
            const iw_op = in.cs.iw_op;
            const mask = Control_Signals.Continuation.mask(ij_op, ik_op, iw_op);
            break :blk hw.microcode.Slot.init(in.cs.c.raw() | mask.raw());
        },
        .seq_literal => in.seq_literal,
    };

    const uc_addr: hw.microcode.Address = .{
        .slot = uc_slot,
        .flags = .{
            .z = in.stat_z,
            .n = in.stat_n,
            .c = in.stat_c,
            .v = in.stat_v,
            .k = in.stat_k,
        },
    };
    out.cs = &microcode_rom[uc_addr.raw()];

    out.want_atomic = in.want_atomic or out.cs.special == .atomic_this or out.cs.special == .atomic_next;
    out.stall_atomic = out.want_atomic and atomic_busy;

    return .{};
}

const Decode_Stage = @This();
const Setup_Stage = @import("Setup_Stage.zig");
const Control_Signals = hw.Control_Signals;
const at = hw.addr.translation;
const hw = arch.hw;
const arch = @import("arch");
//const bits = @import("bits");
const std = @import("std");
