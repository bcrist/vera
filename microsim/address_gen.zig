const sim = @import("Simulator");
const ControlSignals = @import("ControlSignals");
const bus = @import("bus");

pub fn setup(in: SetupInputs) bus.VirtualAddressParts {
    const address_offset: i32 = switch (in.cs_offset) {
        .zero => 0,
        .two => 2,
        .literal => in.cs_literal,
        .literal_minus_64 => @intCast(i32, in.cs_literal) - 64,
    };

    const address = in.base +% @bitCast(u32, address_offset);

    return .{
        .offset = @truncate(bus.PageOffset, address),
        .page = @intCast(bus.Page, address >> 12),
    };
}

pub const SetupInputs = struct {
    base: bus.VirtualAddress,
    cs_offset: ControlSignals.AddressOffset,
    cs_literal: ControlSignals.Literal,
};
