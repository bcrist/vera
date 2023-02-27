const sim = @import("simulator");
const ctrl = @import("control_signals");
const misc = @import("misc");
const uc_layout = @import("microcode_layout");
const arith = @import("arith.zig");

const Split_L_Bus = sim.Split_L_Bus;

pub const LoopState = struct {
    c: bool,
    v: bool,
    n: bool,
    z: bool,
    k: bool,
    next_k: bool,
    a: bool,

    pub fn toUCFlags(self: LoopState) uc_layout.UC_Flag_Set {
        var uc_flags = uc_layout.UC_Flag_Set{};
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
    l: Split_L_Bus,
    shift_c: bool,
    arith_z: bool,
    arith_n: bool,
    arith_c: bool,
    arith_v: bool,
    mmu_z: bool,
    mmu_n: bool,
    mmu_k: bool,

    STAT_OP: ctrl.STAT_Op,
    SEQ_OP: ctrl.Sequencer_Op,
    LITERAL: ctrl.Literal,
};

pub fn transact(in: Inputs, power: *misc.Power_Mode) LoopState {
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
        .ZN_from_address_translator => {
            state.z = in.mmu_z;
            state.n = in.mmu_n;
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
            const ll_bits = @bitCast(misc.STAT_Bits, in.l.low);
            state.z = ll_bits.Z;
            state.n = ll_bits.N;
            state.v = ll_bits.V;
            state.c = ll_bits.C;
            if (in.STAT_OP == .load_ZNVCKA_from_LL) {
                state.k = ll_bits.K;
                state.a = ll_bits.A;
            }
        },
        .clear_A => state.a = false,
        .set_A => state.a = true,
        .clear_S => power.* = .run,
        .set_S => power.* = .sleep,
    }

    return state;
}
