// const std = @import("std");
// const bus = @import("bus_types");

// const SystemBusControl = @This();

// address: bus.PhysicalAddressParts = bus.PhysicalAddressParts.zero,
// even_offset: OffsetType = 0,
// odd_offset: OffsetType = 0,
// swap_bytes: bool = false,
// read: bool = false,
// write: bool = false,
// write_even: bool = false,
// write_odd: bool = false,

// pub const OffsetType = std.meta.Int(.unsigned, @bitSizeOf(bus.PageOffset) - 1);

// pub fn getForTransact(raw: SystemBusControl, inhibit_writes: bool) SystemBusControl {
//     return .{
//         .address = raw.address,
//         .even_offset = raw.even_offset,
//         .odd_offset = raw.odd_offset,
//         .swap_bytes = raw.swap_bytes,
//         .read = !inhibit_writes and raw.read,
//         .write = !inhibit_writes and raw.write,
//         .write_even = !inhibit_writes and raw.write_even,
//         .write_odd = !inhibit_writes and raw.write_odd,
//         .wait_states = if (inhibit_writes) 0 else raw.wait_states,
//     };
// }
