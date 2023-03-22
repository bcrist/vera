const std = @import("std");
const bus = @import("bus_types");
const misc = @import("misc");
const ControlSignals = @import("ControlSignals");

const decodeSpecialKLiteral = misc.decodeSpecialKLiteral;
const encodeSpecialKLiteral = misc.encodeSpecialKLiteral;

test "decodeSpecialKLiteral" {
    try std.testing.expectEqual(@as(bus.K, 0x0040), decodeSpecialKLiteral(6));
    try std.testing.expectEqual(@as(bus.K, 0x0080), decodeSpecialKLiteral(7));
    try std.testing.expectEqual(@as(bus.K, 0x0100), decodeSpecialKLiteral(8));
    try std.testing.expectEqual(@as(bus.K, 0x0200), decodeSpecialKLiteral(9));
    try std.testing.expectEqual(@as(bus.K, 0x0400), decodeSpecialKLiteral(10));
    try std.testing.expectEqual(@as(bus.K, 0x0800), decodeSpecialKLiteral(11));

    try std.testing.expectEqual(@as(bus.K, 0x0011), decodeSpecialKLiteral(0x11));
    try std.testing.expectEqual(@as(bus.K, 0x0022), decodeSpecialKLiteral(0x12));
    try std.testing.expectEqual(@as(bus.K, 0x0033), decodeSpecialKLiteral(0x13));
    try std.testing.expectEqual(@as(bus.K, 0x0044), decodeSpecialKLiteral(0x14));
    try std.testing.expectEqual(@as(bus.K, 0x0055), decodeSpecialKLiteral(0x15));
    try std.testing.expectEqual(@as(bus.K, 0x0066), decodeSpecialKLiteral(0x16));
    try std.testing.expectEqual(@as(bus.K, 0x0077), decodeSpecialKLiteral(0x17));
    try std.testing.expectEqual(@as(bus.K, 0x0088), decodeSpecialKLiteral(0x18));
    try std.testing.expectEqual(@as(bus.K, 0x0099), decodeSpecialKLiteral(0x19));
    try std.testing.expectEqual(@as(bus.K, 0x00AA), decodeSpecialKLiteral(0x1A));
    try std.testing.expectEqual(@as(bus.K, 0x00BB), decodeSpecialKLiteral(0x1B));
    try std.testing.expectEqual(@as(bus.K, 0x00CC), decodeSpecialKLiteral(0x1C));
    try std.testing.expectEqual(@as(bus.K, 0x00DD), decodeSpecialKLiteral(0x1D));
    try std.testing.expectEqual(@as(bus.K, 0x00EE), decodeSpecialKLiteral(0x1E));
    try std.testing.expectEqual(@as(bus.K, 0x00FF), decodeSpecialKLiteral(0x1F));

    try std.testing.expectEqual(@as(bus.K, 0x1100), decodeSpecialKLiteral(0x21));
    try std.testing.expectEqual(@as(bus.K, 0x2200), decodeSpecialKLiteral(0x22));
    try std.testing.expectEqual(@as(bus.K, 0x3300), decodeSpecialKLiteral(0x23));
    try std.testing.expectEqual(@as(bus.K, 0x4400), decodeSpecialKLiteral(0x24));
    try std.testing.expectEqual(@as(bus.K, 0x5500), decodeSpecialKLiteral(0x25));
    try std.testing.expectEqual(@as(bus.K, 0x6600), decodeSpecialKLiteral(0x26));
    try std.testing.expectEqual(@as(bus.K, 0x7700), decodeSpecialKLiteral(0x27));
    try std.testing.expectEqual(@as(bus.K, 0x8800), decodeSpecialKLiteral(0x28));
    try std.testing.expectEqual(@as(bus.K, 0x9900), decodeSpecialKLiteral(0x29));
    try std.testing.expectEqual(@as(bus.K, 0xAA00), decodeSpecialKLiteral(0x2A));
    try std.testing.expectEqual(@as(bus.K, 0xBB00), decodeSpecialKLiteral(0x2B));
    try std.testing.expectEqual(@as(bus.K, 0xCC00), decodeSpecialKLiteral(0x2C));
    try std.testing.expectEqual(@as(bus.K, 0xDD00), decodeSpecialKLiteral(0x2D));
    try std.testing.expectEqual(@as(bus.K, 0xEE00), decodeSpecialKLiteral(0x2E));
    try std.testing.expectEqual(@as(bus.K, 0xFF00), decodeSpecialKLiteral(0x2F));

    try std.testing.expectEqual(@as(bus.K, 0x1000), decodeSpecialKLiteral(0x31));
    try std.testing.expectEqual(@as(bus.K, 0x2000), decodeSpecialKLiteral(0x32));
    try std.testing.expectEqual(@as(bus.K, 0x3000), decodeSpecialKLiteral(0x33));
    try std.testing.expectEqual(@as(bus.K, 0x4000), decodeSpecialKLiteral(0x34));
    try std.testing.expectEqual(@as(bus.K, 0x5000), decodeSpecialKLiteral(0x35));
    try std.testing.expectEqual(@as(bus.K, 0x6000), decodeSpecialKLiteral(0x36));
    try std.testing.expectEqual(@as(bus.K, 0x7000), decodeSpecialKLiteral(0x37));
    try std.testing.expectEqual(@as(bus.K, 0x8000), decodeSpecialKLiteral(0x38));
    try std.testing.expectEqual(@as(bus.K, 0x9000), decodeSpecialKLiteral(0x39));
    try std.testing.expectEqual(@as(bus.K, 0xA000), decodeSpecialKLiteral(0x3A));
    try std.testing.expectEqual(@as(bus.K, 0xB000), decodeSpecialKLiteral(0x3B));
    try std.testing.expectEqual(@as(bus.K, 0xC000), decodeSpecialKLiteral(0x3C));
    try std.testing.expectEqual(@as(bus.K, 0xD000), decodeSpecialKLiteral(0x3D));
    try std.testing.expectEqual(@as(bus.K, 0xE000), decodeSpecialKLiteral(0x3E));
    try std.testing.expectEqual(@as(bus.K, 0xF000), decodeSpecialKLiteral(0x3F));
}

test "special k literal roundtripping" {
    for (6..12) |n| {
        const encoded = @intCast(ControlSignals.Literal, n);
        try std.testing.expectEqual(encoded, encodeSpecialKLiteral(decodeSpecialKLiteral(encoded)).?);
    }
    for (0x11..0x20) |n| {
        const encoded = @intCast(ControlSignals.Literal, n);
        try std.testing.expectEqual(encoded, encodeSpecialKLiteral(decodeSpecialKLiteral(encoded)).?);
    }
    for (0x21..0x30) |n| {
        const encoded = @intCast(ControlSignals.Literal, n);
        try std.testing.expectEqual(encoded, encodeSpecialKLiteral(decodeSpecialKLiteral(encoded)).?);
    }
    for (0x31..0x40) |n| {
        const encoded = @intCast(ControlSignals.Literal, n);
        try std.testing.expectEqual(encoded, encodeSpecialKLiteral(decodeSpecialKLiteral(encoded)).?);
    }
}
