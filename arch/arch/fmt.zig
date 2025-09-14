pub fn format_enum_hex(value_or_ptr: anytype, writer: *std.io.Writer) !void {
    const value = if (@typeInfo(@TypeOf(value_or_ptr)) == .pointer) value_or_ptr.* else value_or_ptr;
    const T = @TypeOf(value);

    if (std.enums.tagName(T, value)) |tag| {
        try writer.writeByte('.');
        try writer.writeAll(tag);
    } else {
        const hex_fmt = std.fmt.comptimePrint("0x{{X:0>{}}}", .{ (@bitSizeOf(T) + 3) / 4 });
        try writer.print(hex_fmt, .{ @intFromEnum(value) });
    }
}

pub fn format_enum_dec(value_or_ptr: anytype, writer: *std.io.Writer) !void {
    const value = if (@typeInfo(@TypeOf(value_or_ptr)) == .pointer) value_or_ptr.* else value_or_ptr;
    const T = @TypeOf(value);

    if (std.enums.tagName(T, value)) |tag| {
        try writer.writeByte('.');
        try writer.writeAll(tag);
    } else {
        try writer.print("{d}", .{ @intFromEnum(value) });
    }
}

pub fn format_raw_dec(value_or_ptr: anytype, writer: *std.io.Writer) !void {
    try writer.print("{d}", .{ value_or_ptr.raw() });
}

pub fn format_raw_hex(value_or_ptr: anytype, writer: *std.io.Writer) !void {
    const raw = value_or_ptr.raw();
    const T = @TypeOf(raw);

    const hex_fmt = std.fmt.comptimePrint("0x{{X:0>{}}}", .{ (@bitSizeOf(T) + 3) / 4 });
    try writer.print(hex_fmt, .{ raw });
}

const std = @import("std");
