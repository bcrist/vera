const ControlSignals = @import("ControlSignals");
const misc = @import("misc");
const bus = @import("bus_types");

pub fn compute(in: Inputs) bus.LLow {
    const mode_bits = @bitCast(ControlSignals.LogicModeBits, in.cs_compute_mode.raw());
    const j = in.jl;
    const k = if (mode_bits.invert_k) ~in.k else in.k;

    return switch (mode_bits.op) {
        .jl_or_k => j | k,
        .jl_nor_k => ~(j | k),
        .jl_and_k => j & k,
        .jl_nand_k => ~(j & k),
        .jl_xor_k => j ^ k,
        .count_k => @popCount(k),
        .count_trailing_k => @ctz(~k),
        .count_leading_k => @clz(~k),
    };
}

pub const Inputs = struct {
    jl: bus.JLow,
    k: bus.K,
    cs_compute_mode: ControlSignals.ComputeMode,
};
