const sim = @import("Simulator");
const ControlSignals = @import("ControlSignals");
const misc = @import("misc");
const bus = @import("bus");

pub fn compute(in: Inputs) Outputs {
    const mode_bits = @bitCast(ControlSignals.LogicModeBits, in.cs_compute_mode.raw());
    const j = if (mode_bits.invert_jl) ~in.j.low else in.j.low;
    const k = if (mode_bits.invert_k) ~in.k else in.k;
    const y = if (mode_bits.xor) (j ^ k) else (j & k);
    return .{
        .data = .{
            .low = (if (mode_bits.invert_y) ~y else y),
            .high = in.j.high,
        },
    };
}

pub const Inputs = struct {
    j: bus.JParts,
    k: bus.K,
    cs_compute_mode: ControlSignals.ComputeMode,
};

pub const Outputs = struct {
    data: bus.LParts,
};
