const ControlSignals = @import("ControlSignals");
const misc = @import("misc");
const bus = @import("bus_types");
const uc = @import("microcode");

const StatusRegister = @This();

c: bool,
v: bool,
n: bool,
z: bool,
k: bool,
next_k: bool,
a: bool,

pub fn toUCFlags(self: StatusRegister) uc.FlagSet {
    var uc_flags = uc.FlagSet{};
    if (self.n) uc_flags.insert(.N);
    if (self.k) uc_flags.insert(.K);
    if (self.z) uc_flags.insert(.Z);
    if (self.v) uc_flags.insert(.V);
    if (self.c) uc_flags.insert(.C);
    return uc_flags;
}

pub fn getForLL(self: StatusRegister, pipe: misc.PipeID, exec_mode: misc.ExecutionMode, pwr: misc.PowerMode) bus.LLow {
    return @bitCast(bus.LLow, misc.StatusBits{
        .z = self.z,
        .n = self.n,
        .c = self.c,
        .v = self.v,
        .k = self.k,
        .a = self.a,
        .pipe = pipe,
        .mode = exec_mode,
        .pwr = pwr,
    });
}

pub fn print(self: StatusRegister, writer: anytype) !void {
    const c = if (self.c) "C" else " ";
    const v = if (self.v) "V" else " ";
    const n = if (self.n) "N" else " ";
    const z = if (self.z) "Z" else " ";
    const nk = if (self.next_k) "n" else " ";
    const k = if (self.k) "K" else " ";
    const a = if (self.a) "A" else " ";
    try writer.print("STAT: {s} {s} {s} {s} {s}{s} {s}", .{ c, v, n, z, nk, k, a });
}

pub const TransactInputs = struct {
    state: StatusRegister,
    inhibit_writes: bool,
    l: bus.LParts,
    shift_c: bool,
    arith_z: bool,
    arith_n: bool,
    arith_c: bool,
    arith_v: bool,
    at_k: bool,

    cs_stat_op: ControlSignals.StatusRegOp,
    cs_seq_op: ControlSignals.SequencerOp,
    cs_literal: ControlSignals.Literal,
};

pub const TransactOutputs = struct {
    state: StatusRegister,
    power: ?misc.PowerMode,
};

pub fn getForTransact(in: TransactInputs) TransactOutputs {
    if (in.inhibit_writes) {
        return .{
            .state = in.state,
            .power = null,
        };
    }

    const llz = in.l.low == 0;
    const lln = (in.l.low >> 15) == 1;

    const lz = @bitCast(u32, in.l) == 0;
    const ln = (in.l.high >> 15) == 1;

    var state = in.state;
    var power: ?misc.PowerMode = null;

    state.next_k = in.at_k;

    switch (in.cs_seq_op) {
        // next_k is a latch, so it has already been updated from in.at_k if necessary
        .next_instruction => state.k = state.next_k,
        .next_uop, .next_uop_force_normal, .fault_return => {},
    }

    switch (in.cs_stat_op) {
        .hold => {},
        .zn_from_l => {
            state.z = lz;
            state.n = ln;
        },
        .zn_from_l_c_from_shift => {
            state.z = lz;
            state.n = ln;
            state.c = in.shift_c;
        },
        .zn_from_l_no_set_z => {
            state.z = lz and in.state.z;
            state.n = ln;
        },
        .zn_from_ll => {
            state.z = llz;
            state.n = lln;
        },
        .zn_from_ll_c_from_shift => {
            state.z = llz;
            state.n = lln;
            state.c = in.shift_c;
        },
        .zn_from_ll_no_set_z => {
            state.z = llz and in.state.z;
            state.n = lln;
        },
        .znvc_from_arith => {
            state.z = in.arith_z;
            state.n = in.arith_n;
            state.v = in.arith_v;
            state.c = in.arith_c;
        },
        .znvc_from_arith_no_set_z => {
            state.z = in.arith_z and in.state.z;
            state.n = in.arith_n;
            state.v = in.arith_v;
            state.c = in.arith_c;
        },
        .load_znvc_from_ll, .load_znvcka_from_ll => {
            const ll_bits = @bitCast(misc.StatusBits, in.l.low);
            state.z = ll_bits.z;
            state.n = ll_bits.n;
            state.v = ll_bits.v;
            state.c = ll_bits.c;
            if (in.cs_stat_op == .load_znvcka_from_ll) {
                state.k = ll_bits.k;
                state.a = ll_bits.a;
            }
        },
        .clear_a => state.a = false,
        .set_a => state.a = true,
        .clear_s => power = .run,
        .set_s => power = .sleep,
    }

    return .{
        .state = state,
        .power = power,
    };
}
