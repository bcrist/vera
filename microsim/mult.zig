const bits = @import("bits");
const sim = @import("simulator");
const ctrl = @import("control_signals");
const misc = @import("misc");

const Split_L_Bus = sim.Split_L_Bus;

pub fn compute(in: Inputs) Outputs {
    const mode_bits = @bitCast(ctrl.Multiplier_Mode_Bits, in.ALU_MODE.raw());

    const j = switch (mode_bits.JL) {
        .unsigned => bits.zx(i64, in.j),
        .signed => bits.sx(i64, in.j),
    };

    const k = switch (mode_bits.K) {
        .unsigned => bits.zx(i64, in.k),
        .signed => bits.sx(i64, in.k),
    };

    var result = @bitCast(Split_L_Bus, @truncate(u32, @bitCast(u64, j * k)));

    if (mode_bits.swap_output) {
        result = .{
            .low = result.high,
            .high = result.low,
        };
    }

    return .{ .data = result };
}

pub const Inputs = struct {
    j: misc.JL_Bus,
    k: misc.K_Bus,
    ALU_MODE: ctrl.ALU_Mode,
};

pub const Outputs = struct {
    data: Split_L_Bus,
};
