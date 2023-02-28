const bits = @import("bits");
const sim = @import("Simulator");
const ctrl = @import("control_signals");
const bus = @import("bus");

pub fn compute(in: Inputs) Outputs {
    const mode_bits = @bitCast(ctrl.Arith_Mode_Bits, in.ALU_MODE.raw());
    const narrow = mode_bits.width == .JL_K;

    const j: u32 = if (narrow) in.j.low else @bitCast(u32, in.j);
    var k: u32 = switch (mode_bits.width) {
        .J_K_zx, .JL_K => in.k,
        .J_K_sx => bits.sx(u32, in.k),
        .J_K_1x => bits._1x(u32, in.k),
    };
    var c_in: u1 = if (mode_bits.carry_borrow) @boolToInt(in.c) else 0;

    if (mode_bits.subtract) {
        k = ~k;
        c_in = ~c_in;
    }

    const raw = @as(u33, j) +% k +% c_in;

    const data = bus.LParts{
        .low = @truncate(u16, raw),
        .high = if (narrow) 0 else @truncate(u16, raw >> 16),
    };

    const z = (if (narrow) @truncate(u16, raw) else @truncate(u32, raw)) == 0;
    const n = @truncate(u1, raw >> (if (narrow) @as(u6, 15) else 31)) != 0;
    var c = @truncate(u1, raw >> (if (narrow) @as(u6, 16) else 32)) != 0;

    const v = if (narrow) blk: {
        const raw15 = @as(u16, @truncate(u15, j)) + @truncate(u15, k) + c_in;
        const c15 = @intCast(u1, raw15 >> 15) != 0;
        break :blk c15 != c;
    } else blk: {
        const raw31 = @as(u32, @truncate(u31, j)) + @truncate(u31, k) + c_in;
        const c31 = @intCast(u1, raw31 >> 31) != 0;
        break :blk c31 != c;
    };

    if (mode_bits.subtract) {
        c = !c;
    }

    return .{
        .data = data,
        .z = z,
        .n = n,
        .c = c,
        .v = v,
    };
}

pub const Inputs = struct {
    j: bus.JParts,
    k: bus.K,
    c: bool,
    ALU_MODE: ctrl.ALU_Mode,
};

pub const Outputs = struct {
    data: bus.LParts,
    z: bool,
    n: bool,
    c: bool,
    v: bool,
};
