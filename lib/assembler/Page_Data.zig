pub const page_size = hw.addr.Offset.count;
pub const num_pages = hw.addr.Page.count;

page: hw.addr.Page,
access: Section.Access_Policies,
section: Section.Handle,
usage: Usage_Bitmap,
data: [page_size]u8,
chunks: std.ArrayListUnmanaged(Source_File.Chunk),

pub const Handle = u31;

pub const Usage_Bitmap = std.StaticBitSet(page_size);

pub fn init(page: hw.addr.Page, access: Section.Access_Policies, section: Section.Handle) Page_Data {
    return .{
        .page = page,
        .access = access,
        .section = section,
        .usage = Usage_Bitmap.initEmpty(),
        .data = [_]u8{0}**page_size,
        .chunks = .{},
    };
}

pub const Data_Iterator = struct {
    assembler: *const Assembler,
    page: hw.addr.Page,
    data: []const u8,

    const dummy_data = [_]u8{0} ** Page_Data.page_size;

    pub fn init(assembler: *const Assembler, addr: u32) Data_Iterator {
        const offset = hw.addr.Offset.init(@truncate(addr));
        const page = hw.addr.Page.init(@truncate(addr >> @bitSizeOf(hw.addr.Offset)));
        const page_datas = assembler.pages.items(.data);
        const whole_page = if (assembler.page_lookup.get(page)) |handle| &page_datas[handle] else &dummy_data;
        return .{
            .assembler = assembler,
            .page = page,
            .data = whole_page[offset.raw()..],
        };
    }

    pub fn next(self: *Data_Iterator) u8 {
        if (self.data.len == 0) {
            const new_page = hw.addr.Page.init(self.page.raw() + 1);
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

const Page_Data = @This();
const Section = @import("Section.zig");
const Source_File = @import("Source_File.zig");
const Assembler = @import("Assembler.zig");
const hw = @import("lib_arch").hw;
const std = @import("std");
