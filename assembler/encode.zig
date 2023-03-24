const std = @import("std");
const parse = @import("parse.zig");
const ie = @import("instruction_encoding");

const ParseResult = parse.ParseResult;

pub const SectionData = struct {
    name: []const u8,
    base_address: u32,
    section_type: enum {
        program,
        zwdata,
        wdata,
        rdata,
    },

    data: []const u8,
};

pub fn encode(alloc: std.mem.Allocator, ast_data: ParseResult) !std.StringHashMapUnmanaged(SectionData) {
    var sections = std.StringHashMap(SectionData).init(alloc);
    errdefer sections.deinit();
    errdefer {
        var section_iter = sections.valueIterator();
        while (section_iter.next()) |s| {
            if (s.data.len > 0) {
                alloc.free(s.data);
            }
        }
    }

    var data = std.StringHashMap(std.ArrayListUnmanaged(u8)).init(alloc);
    defer {
        var data_iter = data.valueIterator();
        while (data_iter.next()) |list| {
            list.deinit();
        }
    }

    var section_data = data.getOrPutValue("text", .{}).value_ptr;
    var section = sections.getOrPutValue("text", .{
        .name = "text",
        .base_address = 0,
        .section_type = .program,
        .data = "",
    }).value_ptr;


    var temp: [16]u8 = undefined;
    var encoder = ie.Encoder.init(&temp);

    var temp_parameters = std.ArrayList(ie.Parameter).init(alloc);

    for(ast_data.instructions.items) |insn| {
        if (insn.encoding) |encoding| {
            // TODO build temp_parameters

            encoder.encode(.{
                .mnemonic = encoding.mnemonic,
                .suffix = encoding.suffix,
                .params = temp_parameters.items,
            }, encoding);
            temp_parameters.clearRetainingCapacity();
            try section_data.appendSlice(encoder.getData());
            encoder.reset();
        }
    }

    _ = section;

    var data_iter = data.iterator();
    while (data_iter.next()) |entry| {
        if (sections.get(entry.key_ptr.*)) |s| {
            const data_items = entry.value_ptr.items;
            if (data_items.len > 0) {
                s.data = try alloc.dupe(data_items);
            }
        }
    }

    return sections.unmanaged;
}
