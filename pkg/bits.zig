const std = @import("std");
const builtin = std.builtin;
const expectEqual = std.testing.expectEqual;

pub fn zx(comptime T: type, n: anytype) T {
    const N = @TypeOf(n);
    expectInt(T);
    expectInt(N);

    if (@bitSizeOf(T) == @bitSizeOf(N)) return n;
    if (@bitSizeOf(T) < @bitSizeOf(N)) @compileError("Cannot reduce width; use @truncate() instead");

    const NU = std.meta.Int(.unsigned, @bitSizeOf(N));
    const TU = std.meta.Int(.unsigned, @bitSizeOf(T));

    return @bitCast(T, @as(TU, @bitCast(NU, n)));
}
test "zx" {
    try expectEqual(zx(u32, @as(u8, 0xFF)), 0xFF);
    try expectEqual(zx(i32, @as(u8, 0xFF)), 0xFF);
    try expectEqual(zx(u32, @as(i8, -1)),   0xFF);
    try expectEqual(zx(u32, @as(i7, -1)),   0x7F);
    try expectEqual(zx(i32, @as(i7, -1)),   0x7F);
}

pub fn sx(comptime T: type, n: anytype) T {
    const N = @TypeOf(n);
    expectInt(T);
    expectInt(N);

    if (@bitSizeOf(T) == @bitSizeOf(N)) return @bitCast(T, n);
    if (@bitSizeOf(T) < @bitSizeOf(N)) @compileError("Cannot reduce width; use @truncate() instead");

    const NS = std.meta.Int(.signed, @bitSizeOf(N));
    const TS = std.meta.Int(.signed, @bitSizeOf(T));

    return @bitCast(T, @as(TS, @bitCast(NS, n)));
}
test "sx" {
    try expectEqual(sx(i32, @as(i8, -1)),   -1);
    try expectEqual(sx(i32, @as(u8, 0xFF)), -1);
    try expectEqual(sx(i32, @as(i7, -1)),   -1);
    try expectEqual(sx(i32, @as(u7, 0x7F)), -1);
    try expectEqual(sx(u32, @as(i8, -1)),   0xFFFF_FFFF);
    try expectEqual(sx(u32, @as(u8, 0xFF)), 0xFFFF_FFFF);
    try expectEqual(sx(u32, @as(i7, -1)),   0xFFFF_FFFF);
    try expectEqual(sx(u32, @as(u7, 0x7F)), 0xFFFF_FFFF);

    try expectEqual(sx(i32, @as(i8, 0x7F)), 0x7F);
    try expectEqual(sx(i32, @as(u8, 0x7F)), 0x7F);
    try expectEqual(sx(i32, @as(i7, 0x3F)), 0x3F);
    try expectEqual(sx(i32, @as(u7, 0x3F)), 0x3F);
    try expectEqual(sx(u32, @as(i8, 0x7F)), 0x7F);
    try expectEqual(sx(u32, @as(u8, 0x7F)), 0x7F);
    try expectEqual(sx(u32, @as(i7, 0x3F)), 0x3F);
    try expectEqual(sx(u32, @as(u7, 0x3F)), 0x3F);
}

pub fn _1x(comptime T: type, n: anytype) T {
    const N = @TypeOf(n);
    expectInt(T);
    expectInt(N);

    if (@bitSizeOf(T) == @bitSizeOf(N)) return n;
    if (@bitSizeOf(T) < @bitSizeOf(N)) @compileError("Cannot reduce width; use @truncate() instead");

    const NU = std.meta.Int(.unsigned, @bitSizeOf(N));
    const TU = std.meta.Int(.unsigned, @bitSizeOf(T));

    const upper = ~@as(TU, 0) ^ ~@as(NU, 0);

    return @bitCast(T, upper | @as(TU, @bitCast(NU, n)));
}
test "1x" {
    try expectEqual(_1x(i32, @as(i8, -1)),   -1);
    try expectEqual(_1x(i32, @as(u8, 0xFF)), -1);
    try expectEqual(_1x(i32, @as(i7, -1)),   -1);
    try expectEqual(_1x(i32, @as(u7, 0x7F)), -1);
    try expectEqual(_1x(u32, @as(i8, -1)),   0xFFFF_FFFF);
    try expectEqual(_1x(u32, @as(u8, 0xFF)), 0xFFFF_FFFF);
    try expectEqual(_1x(u32, @as(i7, -1)),   0xFFFF_FFFF);
    try expectEqual(_1x(u32, @as(u7, 0x7F)), 0xFFFF_FFFF);

    try expectEqual(_1x(i32, @as(i8, 0x7F)), @bitCast(i32, @as(u32, 0xFFFF_FF7F)));
    try expectEqual(_1x(i32, @as(u8, 0x7F)), @bitCast(i32, @as(u32, 0xFFFF_FF7F)));
    try expectEqual(_1x(i32, @as(i7, 0x3F)), @bitCast(i32, @as(u32, 0xFFFF_FFBF)));
    try expectEqual(_1x(i32, @as(u7, 0x3F)), @bitCast(i32, @as(u32, 0xFFFF_FFBF)));
    try expectEqual(_1x(u32, @as(i8, 0x7F)), 0xFFFF_FF7F);
    try expectEqual(_1x(u32, @as(u8, 0x7F)), 0xFFFF_FF7F);
    try expectEqual(_1x(u32, @as(i7, 0x3F)), 0xFFFF_FFBF);
    try expectEqual(_1x(u32, @as(u7, 0x3F)), 0xFFFF_FFBF);
}

pub fn concat2(low: anytype, high: anytype) std.meta.Int(.unsigned, @bitSizeOf(@TypeOf(low)) + @bitSizeOf(@TypeOf(high))) {
    const R = std.meta.Int(.unsigned, @bitSizeOf(@TypeOf(low)) + @bitSizeOf(@TypeOf(high)));
    return @shlExact(@as(R, high), @bitSizeOf(@TypeOf(low))) | low;
}
test "concat2" {
    try expectEqual(concat2(@as(u8, 0x01), @as(u8, 0x09)), @as(u16, 0x0901));
    try expectEqual(concat2(@as(u3, 0), @as(u4, 0xF)), @as(u7, 0x78));
}

pub fn concat(s: anytype) ConcatResultType(@TypeOf(s)) {
    const R = ConcatResultType(@TypeOf(s));
    const fields = @typeInfo(@TypeOf(s)).Struct.fields;
    var result: R = 0;
    comptime var i: comptime_int = fields.len - 1;
    inline while (i >= 0) : (i -= 1) {
        const field = fields[i];
        result = @shlExact(result, @bitSizeOf(field.type)) | @field(s, field.name);
    }
    return result;
}
fn ConcatResultType(comptime S: type) type {
    comptime var bits = 0;
    if (@typeInfo(S) == .Struct) {
        const info = @typeInfo(S).Struct;
        if (!info.is_tuple) {
            @compileError("Expected tuple");
        }
        inline for (info.fields) |field| {
            expectSignedness(field.type, .unsigned);
            bits += @bitSizeOf(field.type);
        }
    } else {
        @compileError("Expected tuple");
    }
    return std.meta.Int(.unsigned, bits);
}
test "concat" {
    try expectEqual(concat(.{
        @as(u8, 0x01),
        @as(u8, 0x99),
    }), 0x9901);

    try expectEqual(concat(.{
        @as(u1, 1),
        @as(u32, 0x1099),
        @as(u1, 1),
    }), 0x2_0000_2133);
}

pub fn swapHalves(comptime T: type, n: T) T {
    expectSignedness(T, .unsigned);
    if ((@bitSizeOf(T) & 1) == 1) @compileError("Expected even bit width");

    const h_bits = @bitSizeOf(T) / 2;
    const H = std.meta.Int(.unsigned, h_bits);

    const low = @truncate(H, n);
    const high = @truncate(H, n >> h_bits);

    return @shlExact(@as(T, low), h_bits) | high;
}
test "swapHalves" {
    try expectEqual(swapHalves(u32, 0xFFFF0000), 0xFFFF);
    try expectEqual(swapHalves(u32, 0x12345678), 0x56781234);
    try expectEqual(swapHalves(u6, 0x1), 0x8);
}

//////////////////////////////////////////////////////////////////////////////

fn expectSignedness(comptime T: type, comptime signedness: builtin.Signedness) void {
    switch (@typeInfo(T)) {
        .Int => |info| if (info.signedness == signedness) return,
        else => {},
    }

    if (signedness == .unsigned) {
        @compileError("Expected unsigned integer");
    } else {
        @compileError("Expected signed integer");
    }
}

fn expectInt(comptime T: type) void {
    if (@typeInfo(T) != .Int) @compileError("Expected integer");
}
