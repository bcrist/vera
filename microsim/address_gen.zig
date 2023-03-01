const sim = @import("Simulator");
const ControlSignals = @import("ControlSignals");
const bus = @import("bus");

pub fn setup(in: SetupInputs) bus.VirtualAddressParts {
    const address_offset: i32 = switch (in.OFFSET) {
        .zero => 0,
        .two => 2,
        .LITERAL => in.LITERAL,
        .LITERAL_minus_64 => @intCast(i32, in.LITERAL) - 64,
    };

    const address = in.base +% @bitCast(u32, address_offset);

    return .{
        .offset = @truncate(bus.PageOffset, address),
        .page = @intCast(bus.Page, address >> 12),
    };
}

pub const SetupInputs = struct {
    base: bus.VirtualAddress,
    OFFSET: ControlSignals.Address_Offset,
    LITERAL: ControlSignals.Literal,
};
