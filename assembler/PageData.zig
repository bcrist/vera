const std = @import("std");
const bus = @import("bus_types");
const Section = @import("Section.zig");
const SourceFile = @import("SourceFile.zig");
const Assembler = @import("Assembler.zig");

pub const page_size = std.math.maxInt(bus.PageOffset) + 1;
pub const num_pages = std.math.maxInt(bus.Page) + 1;

pub const Handle = u31;

pub const UsageBitmap = std.StaticBitSet(page_size);

page: bus.Page,
// TODO add kind: Section.Kind,
section: Section.Handle,
usage: UsageBitmap,
data: [page_size]u8,
chunks: std.ArrayListUnmanaged(SourceFile.Chunk),

const PageData = @This();

pub fn init(page: bus.Page, section: Section.Handle) PageData {
    return .{
        .page = page,
        .section = section,
        .usage = UsageBitmap.initEmpty(),
        .data = [_]u8{0}**page_size,
        .chunks = .{},
    };
}

pub const DataIterator = struct {
    assembler: *const Assembler,
    page: bus.Page,
    data: []const u8,

    const dummy_data = [_]u8{0} ** PageData.page_size;

    pub fn init(assembler: *const Assembler, addr: u32) DataIterator {
        const offset = @truncate(bus.PageOffset, addr);
        const page = @truncate(bus.Page, addr >> @bitSizeOf(bus.PageOffset));
        const page_datas = assembler.pages.items(.data);
        const whole_page = if (assembler.page_lookup.get(page)) |handle| &page_datas[handle] else &dummy_data;
        return .{
            .assembler = assembler,
            .page = page,
            .data = whole_page[offset..],
        };
    }

    pub fn next(self: *DataIterator) u8 {
        if (self.data.len == 0) {
            const new_page = self.page + 1;
            self.page = new_page;
            const page_datas = self.assembler.pages.items(.data);
            self.data = if (self.assembler.page_lookup.get(new_page)) |handle| &page_datas[handle] else &dummy_data;
        }
        const data = self.data;
        const b = data[0];
        self.data = data[1..];
        return b;
    }
};