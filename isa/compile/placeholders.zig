pub fn Int(comptime name_or_enum_literal: anytype, comptime T: type) type {
    return struct {
        value: i64,

        pub const placeholder = parse_name(name_or_enum_literal);
        pub const domain: isa.Encoder.Domain = .{ .int = .{
            .signedness = @typeInfo(T).int.signedness,
            .bits = @bitSizeOf(T),
            .multiple = 1,
        }};
    };
}

pub fn Int_Mult(comptime name_or_enum_literal: anytype, comptime T: type, comptime multiple: u8) type {
    return struct {
        value: i64,

        pub const placeholder = parse_name(name_or_enum_literal);
        pub const domain: isa.Encoder.Domain = .{ .int = .{
            .signedness = @typeInfo(T).int.signedness,
            .bits = @bitSizeOf(T),
            .multiple = multiple,
        }};
    };
}

pub fn Range(comptime name_or_enum_literal: anytype, comptime first: i64, comptime last: i64) type {
    return struct {
        value: i64,

        pub const placeholder = parse_name(name_or_enum_literal);
        pub const domain: isa.Encoder.Domain = .{ .range = .{
            .first = first,
            .last = last,
        }};
    };
}

pub fn Options(comptime name_or_enum_literal: anytype, comptime options: anytype) type {
    return struct {
        value: i64,

        pub const placeholder = parse_name(name_or_enum_literal);
        pub const domain: isa.Encoder.Domain = .{ .enumerated = &options };
    };
}

/// Can be used as a parameter in microcode functions, but not in Encoders, since it doesn't have a domain decl.
pub fn Any(comptime name_or_enum_literal: anytype) type {
    return struct {
        value: i64,

        pub const placeholder = parse_name(name_or_enum_literal);
    };
}

pub fn Reg(comptime name_or_enum_literal: anytype) type {
    return Int(name_or_enum_literal, arch.bus.K.Read_Index_Offset.Raw);
}

pub fn Bit(comptime name_or_enum_literal: anytype) type {
    return Options(name_or_enum_literal, .{
        0x1, 0x2, 0x4, 0x8,
        0x10, 0x20, 0x40, 0x80,
        0x100, 0x200, 0x400, 0x800,
        0x1000, 0x2000, 0x4000, 0x8000,
        0x1_0000, 0x2_0000, 0x4_0000, 0x8_0000,
        0x10_0000, 0x20_0000, 0x40_0000, 0x80_0000,
        0x100_0000, 0x200_0000, 0x400_0000, 0x800_0000,
        0x1000_0000, 0x2000_0000, 0x4000_0000, 0x8000_0000,
    });
}

pub fn Negate(comptime I: type) type {
    return struct {
        value: i64,

        pub const placeholder = I.placeholder;
        pub const Inner = I;
        pub const op = .negate;
    };
}

pub fn Invert(comptime I: type, bit_width: ?u6) type {
    return struct {
        value: i64,

        pub const placeholder = I.placeholder;
        pub const Inner = I;
        pub const op = .invert;
        pub const bits = bit_width;
    };
}

pub fn Offset(comptime offset_amount: i64, comptime I: type) type {
    return struct {
        value: i64,

        pub const placeholder = I.placeholder;
        pub const Inner = I;
        pub const op = .offset;
        pub const offset = offset_amount;
    };
}

fn parse_name(comptime name_or_enum_literal: anytype) []const u8 {
    return switch (@typeInfo(@TypeOf(name_or_enum_literal))) {
        .enum_literal => @tagName(name_or_enum_literal),
        .pointer => name_or_enum_literal,
        else => @compileError("Expected enum literal or []const u8"),
    };
}

pub fn Param(comptime index_name_or_enum_literal: anytype) type {
    return switch (@typeInfo(@TypeOf(index_name_or_enum_literal))) {
        .int, .comptime_int => struct {
            signature: isa.Parameter.Signature,
            pub const index = isa.Parameter.Index.init(index_name_or_enum_literal);
        },
        else => return struct {
            signature: isa.Parameter.Signature,
            pub const placeholder = parse_name(index_name_or_enum_literal);
        },
    };
    
}

const isa = @import("isa");
const arch = @import("arch");
const std = @import("std");
