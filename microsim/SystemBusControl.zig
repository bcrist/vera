const std = @import("std");
const bus = @import("bus_types");

const SystemBusControl = @This();

address: bus.PhysicalAddressParts = bus.PhysicalAddressParts.zero,
even_offset: OffsetType = 0,
odd_offset: OffsetType = 0,
swap_bytes: bool = false,
read: bool = false,
write: bool = false,
write_even: bool = false,
write_odd: bool = false,
wait_states: u2 = 0,

pub const OffsetType = std.meta.Int(.unsigned, @bitSizeOf(bus.PageOffset) - 1);

pub fn getForTransact(raw: SystemBusControl, inhibit_writes: bool) SystemBusControl {
    return .{
        .address = raw.address,
        .even_offset = raw.even_offset,
        .odd_offset = raw.odd_offset,
        .swap_bytes = raw.swap_bytes,
        .read = !inhibit_writes and raw.read,
        .write = !inhibit_writes and raw.write,
        .write_even = !inhibit_writes and raw.write_even,
        .write_odd = !inhibit_writes and raw.write_odd,
        .wait_states = if (inhibit_writes) 0 else raw.wait_states,
    };
}

pub fn random(rnd: std.rand.Random) SystemBusControl {
    var self = SystemBusControl{
        .address = .{
            .offset = rnd.int(bus.PageOffset),
            .frame = rnd.int(bus.Frame),
        },
        .even_offset = undefined,
        .odd_offset = undefined,
        .swap_bytes = rnd.boolean(),
        .read = rnd.boolean(),
        .write = rnd.boolean(),
        .write_even = rnd.boolean(),
        .write_odd = rnd.boolean(),
        .wait_states = rnd.int(u2),
    };

    self.odd_offset = @intCast(OffsetType, self.address.offset >> 1);
    self.even_offset = self.odd_offset + @truncate(u1, self.address.offset);

    return self;
}
