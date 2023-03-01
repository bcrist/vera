const sim = @import("Simulator");
const ControlSignals = @import("ControlSignals");
const misc = @import("misc");
const bus = @import("bus");
const uc = @import("microcode");
const arith = @import("arith.zig");

pub const LoopState = struct {
    c: bool,
    v: bool,
    n: bool,
    z: bool,
    k: bool,
    next_k: bool,
    a: bool,

    pub fn toUCFlags(self: LoopState) uc.FlagSet {
        var uc_flags = uc.FlagSet{};
        if (self.n) uc_flags.insert(.N);
        if (self.k) uc_flags.insert(.K);
        if (self.z) uc_flags.insert(.Z);
        if (self.v) uc_flags.insert(.V);
        if (self.c) uc_flags.insert(.C);
        return uc_flags;
    }

    pub fn print(self: LoopState, writer: anytype) !void {
        const c = if (self.c) "C" else " ";
        const v = if (self.v) "V" else " ";
        const n = if (self.n) "N" else " ";
        const z = if (self.z) "Z" else " ";
        const nk = if (self.next_k) "n" else " ";
        const k = if (self.k) "K" else " ";
        const a = if (self.a) "A" else " ";
        try writer.print("STAT: {s} {s} {s} {s} {s}{s} {s}", .{ c, v, n, z, nk, k, a });
    }
};

pub const Inputs = struct {
    state: LoopState,
    inhibit_writes: bool,
    l: bus.LParts,
    shift_c: bool,
    arith_z: bool,
    arith_n: bool,
    arith_c: bool,
    arith_v: bool,
    mmu_k: bool,

    STAT_OP: ControlSignals.STAT_Op,
    SEQ_OP: ControlSignals.Sequencer_Op,
    LITERAL: ControlSignals.Literal,
};

pub fn transact(in: Inputs, power: *misc.PowerMode) LoopState {
    if (in.inhibit_writes) {
        return in.state;
    }

    const llz = in.l.low == 0;
    const lln = (in.l.low >> 15) == 1;

    const lz = @bitCast(u32, in.l) == 0;
    const ln = (in.l.high >> 15) == 1;

    var state = in.state;

    state.next_k = in.mmu_k;

    switch (in.SEQ_OP) {
        // next_k is a latch, so it has already been updated from in.mmu_k if necessary
        .next_instruction => state.k = state.next_k,
        .next_uop, .next_uop_force_normal, .fault_return => {},
    }

    switch (in.STAT_OP) {
        .hold => {},
        .ZN_from_L => {
            state.z = lz;
            state.n = ln;
        },
        .ZN_from_L_C_from_shift => {
            state.z = lz;
            state.n = ln;
            state.c = in.shift_c;
        },
        .ZN_from_L_no_set_Z => {
            state.z = lz and in.state.z;
            state.n = ln;
        },
        .ZN_from_LL => {
            state.z = llz;
            state.n = lln;
        },
        .ZN_from_LL_C_from_shift => {
            state.z = llz;
            state.n = lln;
            state.c = in.shift_c;
        },
        .ZN_from_LL_no_set_Z => {
            state.z = llz and in.state.z;
            state.n = lln;
        },
        .ZNVC_from_arith => {
            state.z = in.arith_z;
            state.n = in.arith_n;
            state.v = in.arith_v;
            state.c = in.arith_c;
        },
        .ZNVC_from_arith_no_set_Z => {
            state.z = in.arith_z and in.state.z;
            state.n = in.arith_n;
            state.v = in.arith_v;
            state.c = in.arith_c;
        },
        .load_ZNVC_from_LL, .load_ZNVCKA_from_LL => {
            const ll_bits = @bitCast(misc.StatusBits, in.l.low);
            state.z = ll_bits.z;
            state.n = ll_bits.n;
            state.v = ll_bits.v;
            state.c = ll_bits.c;
            if (in.STAT_OP == .load_ZNVCKA_from_LL) {
                state.k = ll_bits.k;
                state.a = ll_bits.a;
            }
        },
        .clear_A => state.a = false,
        .set_A => state.a = true,
        .clear_S => power.* = .run,
        .set_S => power.* = .sleep,
    }

    return state;
}
