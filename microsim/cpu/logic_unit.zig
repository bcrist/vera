const ControlSignals = @import("ControlSignals");
const misc = @import("misc");
const bus = @import("bus_types");

pub fn compute(in: Inputs) bus.LLow {
    const mode_bits = @bitCast(ControlSignals.LogicModeBits, in.cs_compute_mode.raw());
    const j = if (mode_bits.invert_jl) ~in.jl else in.jl;
    const k = in.k;

    return switch (mode_bits.op) {
        .jl_and_k => j & k,
        .jl_nand_k => ~(j & k),
        .jl_xor_k => j ^ k,
        .jl_nor_k => ~(j | k),
        .jl_or_k => j | k,
        .count_jl_or_k => @popCount(j | k),
        .count_trailing_jl_or_k => @ctz(~(j | k)),
        .count_leading_jl_or_k => @clz(~(j | k)),
    };
}

pub const Inputs = struct {
    jl: bus.JLow,
    k: bus.K,
    cs_compute_mode: ControlSignals.ComputeMode,
};
