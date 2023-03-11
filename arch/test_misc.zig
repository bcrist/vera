const std = @import("std");
const bus = @import("bus_types");
const misc = @import("misc");
const ControlSignals = @import("ControlSignals");

const decodeSpecialKLiteral = misc.decodeSpecialKLiteral;
const encodeSpecialKLiteral = misc.encodeSpecialKLiteral;

test "decodeSpecialKLiteral" {
    std.testing.expectEqual(@as(bus.K, 0x0040), decodeSpecialKLiteral(6));
    std.testing.expectEqual(@as(bus.K, 0x0080), decodeSpecialKLiteral(7));
    std.testing.expectEqual(@as(bus.K, 0x0100), decodeSpecialKLiteral(8));
    std.testing.expectEqual(@as(bus.K, 0x0200), decodeSpecialKLiteral(9));
    std.testing.expectEqual(@as(bus.K, 0x0400), decodeSpecialKLiteral(10));
    std.testing.expectEqual(@as(bus.K, 0x0800), decodeSpecialKLiteral(11));

    std.testing.expectEqual(@as(bus.K, 0x0011), decodeSpecialKLiteral(0x11));
    std.testing.expectEqual(@as(bus.K, 0x0022), decodeSpecialKLiteral(0x12));
    std.testing.expectEqual(@as(bus.K, 0x0033), decodeSpecialKLiteral(0x13));
    std.testing.expectEqual(@as(bus.K, 0x0044), decodeSpecialKLiteral(0x14));
    std.testing.expectEqual(@as(bus.K, 0x0055), decodeSpecialKLiteral(0x15));
    std.testing.expectEqual(@as(bus.K, 0x0066), decodeSpecialKLiteral(0x16));
    std.testing.expectEqual(@as(bus.K, 0x0077), decodeSpecialKLiteral(0x17));
    std.testing.expectEqual(@as(bus.K, 0x0088), decodeSpecialKLiteral(0x18));
    std.testing.expectEqual(@as(bus.K, 0x0099), decodeSpecialKLiteral(0x19));
    std.testing.expectEqual(@as(bus.K, 0x00AA), decodeSpecialKLiteral(0x1A));
    std.testing.expectEqual(@as(bus.K, 0x00BB), decodeSpecialKLiteral(0x1B));
    std.testing.expectEqual(@as(bus.K, 0x00CC), decodeSpecialKLiteral(0x1C));
    std.testing.expectEqual(@as(bus.K, 0x00DD), decodeSpecialKLiteral(0x1D));
    std.testing.expectEqual(@as(bus.K, 0x00EE), decodeSpecialKLiteral(0x1E));
    std.testing.expectEqual(@as(bus.K, 0x00FF), decodeSpecialKLiteral(0x1F));

    std.testing.expectEqual(@as(bus.K, 0x1100), decodeSpecialKLiteral(0x21));
    std.testing.expectEqual(@as(bus.K, 0x2200), decodeSpecialKLiteral(0x22));
    std.testing.expectEqual(@as(bus.K, 0x3300), decodeSpecialKLiteral(0x23));
    std.testing.expectEqual(@as(bus.K, 0x4400), decodeSpecialKLiteral(0x24));
    std.testing.expectEqual(@as(bus.K, 0x5500), decodeSpecialKLiteral(0x25));
    std.testing.expectEqual(@as(bus.K, 0x6600), decodeSpecialKLiteral(0x26));
    std.testing.expectEqual(@as(bus.K, 0x7700), decodeSpecialKLiteral(0x27));
    std.testing.expectEqual(@as(bus.K, 0x8800), decodeSpecialKLiteral(0x28));
    std.testing.expectEqual(@as(bus.K, 0x9900), decodeSpecialKLiteral(0x29));
    std.testing.expectEqual(@as(bus.K, 0xAA00), decodeSpecialKLiteral(0x2A));
    std.testing.expectEqual(@as(bus.K, 0xBB00), decodeSpecialKLiteral(0x2B));
    std.testing.expectEqual(@as(bus.K, 0xCC00), decodeSpecialKLiteral(0x2C));
    std.testing.expectEqual(@as(bus.K, 0xDD00), decodeSpecialKLiteral(0x2D));
    std.testing.expectEqual(@as(bus.K, 0xEE00), decodeSpecialKLiteral(0x2E));
    std.testing.expectEqual(@as(bus.K, 0xFF00), decodeSpecialKLiteral(0x2F));

    std.testing.expectEqual(@as(bus.K, 0x1000), decodeSpecialKLiteral(0x31));
    std.testing.expectEqual(@as(bus.K, 0x2000), decodeSpecialKLiteral(0x32));
    std.testing.expectEqual(@as(bus.K, 0x3000), decodeSpecialKLiteral(0x33));
    std.testing.expectEqual(@as(bus.K, 0x4000), decodeSpecialKLiteral(0x34));
    std.testing.expectEqual(@as(bus.K, 0x5000), decodeSpecialKLiteral(0x35));
    std.testing.expectEqual(@as(bus.K, 0x6000), decodeSpecialKLiteral(0x36));
    std.testing.expectEqual(@as(bus.K, 0x7000), decodeSpecialKLiteral(0x37));
    std.testing.expectEqual(@as(bus.K, 0x8000), decodeSpecialKLiteral(0x38));
    std.testing.expectEqual(@as(bus.K, 0x9000), decodeSpecialKLiteral(0x39));
    std.testing.expectEqual(@as(bus.K, 0xA000), decodeSpecialKLiteral(0x3A));
    std.testing.expectEqual(@as(bus.K, 0xB000), decodeSpecialKLiteral(0x3B));
    std.testing.expectEqual(@as(bus.K, 0xC000), decodeSpecialKLiteral(0x3C));
    std.testing.expectEqual(@as(bus.K, 0xD000), decodeSpecialKLiteral(0x3D));
    std.testing.expectEqual(@as(bus.K, 0xE000), decodeSpecialKLiteral(0x3E));
    std.testing.expectEqual(@as(bus.K, 0xF000), decodeSpecialKLiteral(0x3F));
}

test "special k literal roundtripping" {
    for (6..12) |n| {
        const encoded = @intCast(ControlSignals.Literal, n);
        std.testing.expectEqual(encoded, encodeSpecialKLiteral(decodeSpecialKLiteral(encoded)).?);
    }
    for (0x11..0x20) |n| {
        const encoded = @intCast(ControlSignals.Literal, n);
        std.testing.expectEqual(encoded, encodeSpecialKLiteral(decodeSpecialKLiteral(encoded)).?);
    }
    for (0x21..0x30) |n| {
        const encoded = @intCast(ControlSignals.Literal, n);
        std.testing.expectEqual(encoded, encodeSpecialKLiteral(decodeSpecialKLiteral(encoded)).?);
    }
    for (0x31..0x40) |n| {
        const encoded = @intCast(ControlSignals.Literal, n);
        std.testing.expectEqual(encoded, encodeSpecialKLiteral(decodeSpecialKLiteral(encoded)).?);
    }
}
