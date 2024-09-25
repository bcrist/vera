pub fn format_enum_hex(value_or_ptr: anytype, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
    _ = fmt;
    _ = options;

    const value = if (@typeInfo(@TypeOf(value_or_ptr)) == .Pointer) value_or_ptr.* else value_or_ptr;
    const T = @TypeOf(value);

    if (std.enums.tagName(T, value)) |tag| {
        try writer.writeByte('.');
        try writer.writeAll(tag);
    } else {
        const hex_fmt = std.fmt.comptimePrint("0x{{X:0>{}}}", .{ (@bitSizeOf(T) + 3) / 4 });
        try writer.print(hex_fmt, .{ @intFromEnum(value) });
    }
}

pub fn format_enum(value_or_ptr: anytype, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
    _ = options;

    const value = if (@typeInfo(@TypeOf(value_or_ptr)) == .Pointer) value_or_ptr.* else value_or_ptr;
    const T = @TypeOf(value);

    if (std.enums.tagName(T, value)) |tag| {
        try writer.writeByte('.');
        try writer.writeAll(tag);
    } else {
        try writer.print("{" ++ fmt ++ "}", .{ @intFromEnum(value) });
    }
}

pub fn format_raw(value_or_ptr: anytype, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
    _ = options;
    try writer.print("{" ++ fmt ++ "}", .{ value_or_ptr.raw() });
}

const std = @import("std");
