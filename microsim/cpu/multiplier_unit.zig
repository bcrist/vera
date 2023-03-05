const bits = @import("bits");
const ControlSignals = @import("ControlSignals");
const bus = @import("bus_types");

pub fn compute(in: Inputs) Outputs {
    const mode_bits = @bitCast(ControlSignals.MultModeBits, in.cs_compute_mode.raw());

    const j = switch (mode_bits.jl) {
        .unsigned => bits.zx(i64, in.j),
        .signed => bits.sx(i64, in.j),
    };

    const k = switch (mode_bits.k) {
        .unsigned => bits.zx(i64, in.k),
        .signed => bits.sx(i64, in.k),
    };

    var result = @bitCast(bus.LParts, @truncate(u32, @bitCast(u64, j * k)));

    if (mode_bits.swap_output) {
        result = .{
            .low = result.high,
            .high = result.low,
        };
    }

    return .{ .data = result };
}

pub const Inputs = struct {
    j: bus.JLow,
    k: bus.K,
    cs_compute_mode: ControlSignals.ComputeMode,
};

pub const Outputs = struct {
    data: bus.LParts,
};
