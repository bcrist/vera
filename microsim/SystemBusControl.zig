const std = @import("std");
const bus = @import("bus");

const SystemBusControl = @This();

address: bus.PhysicalAddressParts,
read: bool,
write: bool,
write_even: bool,
write_odd: bool,
swap_bytes: bool,
wait_states: u2,
even_offset: OffsetType,
odd_offset: OffsetType,

pub const OffsetType = std.meta.Int(.unsigned, @bitSizeOf(bus.PageOffset) - 1);

pub fn init() SystemBusControl {
    return .{
        .address = bus.PhysicalAddressParts.zero,
        .read = false,
        .write = false,
        .write_even = false,
        .write_odd = false,
        .swap_bytes = false,
        .wait_states = 0,
        .even_offset = 0,
        .odd_offset = 0,
    };
}

pub fn random(rnd: std.rand.Random) SystemBusControl {
    var self = SystemBusControl{
        .address = .{
            .offset = rnd.int(bus.PageOffset),
            .frame = rnd.int(bus.Frame),
        },
        .read = rnd.boolean(),
        .write = rnd.boolean(),
        .write_even = rnd.boolean(),
        .write_odd = rnd.boolean(),
        .swap_bytes = rnd.boolean(),
        .wait_states = rnd.int(u2),
        .even_offset = undefined,
        .odd_offset = undefined,
    };

    self.odd_offset = @intCast(OffsetType, self.address.offset >> 1);
    self.even_offset = self.odd_offset + @truncate(u1, self.address.offset);

    return self;
}
