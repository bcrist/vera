const std = @import("std");
const bus = @import("bus_types");
const Section = @import("Section.zig");

pub const page_size = std.math.maxInt(bus.PageOffset) + 1;
pub const num_pages = std.math.maxInt(bus.Page) + 1;

pub const Handle = u31;

pub const UsageBitmap = std.StaticBitSet(page_size);

page: bus.Page,
section: Section.Handle,
usage: UsageBitmap,
data: []u8,

const PageData = @This();

pub fn init(allocator: std.mem.Allocator, page: bus.Page, section: Section.Handle) PageData {
    const data = allocator.alloc(u8, page_size) catch @panic("OOM");
    std.mem.set(u8, data, 0);
    return .{
        .page = page,
        .section = section,
        .usage = UsageBitmap.initEmpty(),
        .data = data,
    };
}
