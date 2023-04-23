const std = @import("std");
const bus = @import("bus_types");
const Section = @import("Section.zig");

const page_size = std.math.maxInt(bus.PageOffset) + 1;

pub const Handle = u31;

section: Section.Handle,
usage: std.StaticBitSet(page_size),
data: [page_size]u8,
