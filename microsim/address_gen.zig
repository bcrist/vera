const sim = @import("simulator");
const ctrl = @import("control_signals");
const misc = @import("misc");

pub fn setup(in: SetupInputs) sim.VirtualAddress {
    const address_offset: i32 = switch (in.OFFSET) {
        .zero => 0,
        .two => 2,
        .LITERAL => in.LITERAL,
        .LITERAL_minus_64 => @intCast(i32, in.LITERAL) - 64,
    };

    const address = in.base +% @bitCast(u32, address_offset);

    return .{
        .offset = @truncate(misc.N_Bus, address),
        .page = @intCast(misc.P_Bus, address >> 12),
    };
}

pub const SetupInputs = struct {
    base: misc.Virtual_Address,
    OFFSET: ctrl.Address_Offset,
    LITERAL: ctrl.Literal,
};
