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
inhibit_writes: bool,

pub const Debug = struct {};

pub fn simulate(
    in: Decode_Stage,
    out: *Setup_Stage,
    decode_rom: []const hw.decode.Result,
    microcode_rom: []const Control_Signals,
    atomic_busy: bool,
) Debug {
    const id_result = decode_rom[in.dr.raw()];

    out.ij = if (in.inhibit_writes) in.ij else switch (in.cs.ij_op) {
        .hold => in.ij,
        .xor1 => hw.IJ.init(in.ij.raw() ^ 1),
        .from_continuation => in.cs.c_ij,
        .from_decode => id_result.ij,
    };
    out.ik = if (in.inhibit_writes) in.ik else switch (in.cs.ik_op) {
        .hold => in.ik,
        .xor1 => hw.IK.init(in.ik.raw() ^ 1),
        .from_continuation => in.cs.c_ik,
        .from_decode => id_result.ik,
    };
    out.iw = if (in.inhibit_writes) in.iw else switch (in.cs.iw_op) {
        .hold => in.iw,
        .xor1 => hw.IW.init(in.iw.raw() ^ 1),
        .from_continuation => in.cs.c_iw,
        .from_decode => id_result.iw,
    };

    out.uc_slot = switch (in.uc_slot_src) {
        .hold => in.uc_slot,
        .insn_decoder => id_result.slot,
        .continuation => hw.microcode.continuation(in.cs.*),
        .seq_literal => in.seq_literal,
    };

    const uc_addr: hw.microcode.Address = .{
        .slot = out.uc_slot,
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

    out.pipeline = in.pipeline;
    // out.cs = in.cs;
    out.exec_mode = in.exec_mode;
    out.rsn = in.rsn;
    // out.uc_slot = in.uc_slot;
    out.dr = in.dr;
    // out.ij = in.ij;
    // out.ik = in.ik;
    // out.iw = in.iw;
    out.asn = in.asn;
    out.last_translation = in.last_translation;
    out.stat_c = in.stat_c;
    out.stat_v = in.stat_v;
    out.stat_n = in.stat_n;
    out.stat_z = in.stat_z;
    out.stat_k = in.stat_k;
    out.stat_a = in.stat_a;
    out.next_k = in.next_k;
    //out.want_atomic = in.want_atomic;
    //out.stall_atomic = in.stall_atomic;

    return .{};
}

const Decode_Stage = @This();
const Setup_Stage = @import("Setup_Stage.zig");
const Control_Signals = hw.Control_Signals;
const at = hw.addr.translation;
const hw = arch.hw;
const arch = @import("lib_arch");
//const bits = @import("bits");
const std = @import("std");
