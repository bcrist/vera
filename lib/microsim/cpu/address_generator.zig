const ControlSignals = @import("ControlSignals");
const bus = @import("bus_types");

pub fn setup(in: SetupInputs) SetupOutputs {
    const address_offset: i7 = switch (in.cs_offset) {
        .zero => 0,
        .two => 2,
        .literal => in.cs_literal,
        .literal_minus_64 => @intCast(i7, @as(i32, in.cs_literal) - 64),
    };

    const address = in.base +% @bitCast(u32, @as(i32, address_offset));

    return .{
        .virtual_address = .{
            .offset = @truncate(bus.PageOffset, address),
            .page = @intCast(bus.Page, address >> 12),
        },
        .debug_offset = address_offset,
    };
}

pub const SetupInputs = struct {
    base: bus.VirtualAddress,
    cs_offset: ControlSignals.AddressOffset,
    cs_literal: ControlSignals.Literal,
};

pub const SetupOutputs = struct {
    virtual_address: bus.VirtualAddressParts,
    debug_offset: i7,
};