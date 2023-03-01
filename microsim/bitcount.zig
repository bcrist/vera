const sim = @import("Simulator");
const ControlSignals = @import("ControlSignals");
const bus = @import("bus");

const SplitBus = sim.SplitBus;

pub fn compute(in: Inputs) Outputs {
    const mode_bits = @bitCast(ControlSignals.BitcountModeBits, in.ALU_MODE.raw());
    var j = in.j;
    if (mode_bits.invert_jl) j = ~j;
    if (mode_bits.reverse) j = @bitReverse(j);

    return .{
        .data = (if (mode_bits.priority) @ctz(~j) else @popCount(j)),
    };
}

pub const Inputs = struct {
    j: bus.JLow,
    ALU_MODE: ControlSignals.ComputeMode,
};

pub const Outputs = struct {
    data: bus.LLow,
};
