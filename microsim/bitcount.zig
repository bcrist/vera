const sim = @import("Simulator");
const ctrl = @import("control_signals");

const SplitBus = sim.SplitBus;

pub fn compute(in: Inputs) Outputs {
    const mode_bits = @bitCast(ctrl.Bitcount_Mode_Bits, in.ALU_MODE.raw());
    var j = in.j;
    if (mode_bits.invert_JL) j = ~j;
    if (mode_bits.reverse) j = @bitReverse(j);

    return .{
        .data = (if (mode_bits.priority) @ctz(~j) else @popCount(j)),
    };
}

pub const Inputs = struct {
    j: u16,
    ALU_MODE: ctrl.ALU_Mode,
};

pub const Outputs = struct {
    data: u16,
};
