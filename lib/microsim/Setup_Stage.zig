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

pub const Debug = struct {};

pub fn is_atomic(self: Setup_Stage) bool {
    return self.want_atomic and !self.stall_atomic;
}

pub fn simulate(
    in: Setup_Stage,
    out: *Compute_Stage,
    registers: []const hw.Register_Set,
) Debug {
    const rs = registers[in.rsn.raw()];

    const ij = in.ij.raw();
    const ik = in.ik.raw();
    const iw = in.iw.raw();
    _ = iw;

    const sr1 = rs.sr1[in.cs.sr1_ri.raw()];
    const sr2 = rs.sr2[in.cs.sr2_ri.raw()];

    out.sr1 = sr1;
    out.sr2 = sr2;

    out.j.lo = hw.JL.init(switch (in.cs.jl_src) {
        .zero => 0,
        .jrl => rs.reg[ij].raw(),
        .sr1l => sr1.lo.raw(),
        .sr2l => sr2.lo.raw(),
    });

    out.j.hi = hw.JH.init(switch (in.cs.jh_src) {
        .zero => 0,
        .jrh => rs.reg[ij^1].raw(),
        .sr1h => sr1.hi.raw(),
        .sr2h => sr2.hi.raw(),
        .jrl_sx => bits.sx(u16, rs.reg[ij].raw() >> 15),
        .neg_one => 0xFFFF,
        _ => 0xBADC,
    });

    out.k = hw.K.init(switch (in.cs.k_src) {
        .zero => 0,
        .literal_sx => bits.sx(u16, in.cs.literal.raw()),
        .sr1l => sr1.lo.raw(),
        .sr2l => sr2.lo.raw(),
        .kr => rs.reg[ik].raw(),
        .ik_bit => @as(u16, 1) << ik,
        .ik_zx => ik,
        .ij_ik_zx => bits.concat(.{ ik, ij }),
    });

    const addr_base: u32 =
        if (in.cs.base_ri.to_sr1_index()) |sr1b| rs.sr1[sr1b.raw()].raw()
        else if (in.cs.base_ri.to_sr2_index()) |sr2b| rs.sr2[sr2b.raw()].raw()
        else 0;

    const addr_offset: u32 = switch (in.cs.offset_src) {
        .zero => 0,
        .two => 2,
        .literal => in.cs.literal.raw(),
        .literal_minus_64 => bits._1x(u32, in.cs.literal.raw()),
    };

    out.virtual_addr = hw.addr.Virtual.init(addr_base +% addr_offset);

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


const Setup_Stage = @This();
const Compute_Stage = @import("Compute_Stage.zig");
const Control_Signals = hw.Control_Signals;
const at = hw.addr.translation;
const hw = arch.hw;
const arch = @import("lib_arch");
const bits = @import("bits");
const std = @import("std");
