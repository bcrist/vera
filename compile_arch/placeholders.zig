pub fn Int(comptime name_or_enum_literal: anytype, comptime T: type) type {
    return struct {
        value: i64,

        pub const placeholder = parse_name(name_or_enum_literal);
        pub const domain: Domain = .{ .int = .{
            .signedness = @typeInfo(T).Int.signedness,
            .bits = @bitSizeOf(T),
            .multiple = 1,
        }};
    };
}

pub fn Int_Mult(comptime name_or_enum_literal: anytype, comptime T: type, comptime multiple: u8) type {
    return struct {
        value: i64,

        pub const placeholder = parse_name(name_or_enum_literal);
        pub const domain: Domain = .{ .int = .{
            .signedness = @typeInfo(T).Int.signedness,
            .bits = @bitSizeOf(T),
            .multiple = multiple,
        }};
    };
}

pub fn Reg(comptime name_or_enum_literal: anytype) type {
    return Int(name_or_enum_literal, Register_Index);
}

pub fn Even_Reg(comptime name_or_enum_literal: anytype) type {
    return Int_Mult(name_or_enum_literal, std.meta.Int(.unsigned, @bitSizeOf(Register_Index) - 1), 2);
}

pub fn Odd_Reg(comptime name_or_enum_literal: anytype) type {
    return Options(name_or_enum_literal, .{ 1, 3, 5, 7, 9, 11, 13, 15 });
}

pub fn Reg_Bit(comptime name_or_enum_literal:anytype) type {
    return Options(name_or_enum_literal, .{ 1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192, 16384, 32768 });
}

pub fn Range(comptime name_or_enum_literal: anytype, comptime first: i64, comptime last: i64) type {
    return struct {
        value: i64,

        pub const placeholder = parse_name(name_or_enum_literal);
        pub const domain: Domain = .{ .range = .{
            .first = first,
            .last = last,
        }};
    };
}

pub fn Options(comptime name_or_enum_literal: anytype, comptime options: anytype) type {
    return struct {
        value: i64,

        pub const placeholder = parse_name(name_or_enum_literal);
        pub const domain: Domain = .{ .enumerated = &options };
    };
}

/// Can be used as a parameter in microcode functions, but not in Encoders.
pub fn Any(comptime name_or_enum_literal: anytype) type {
    return struct {
        value: i64,

        pub const placeholder = parse_name(name_or_enum_literal);
    };
}

fn parse_name(comptime name_or_enum_literal: anytype) []const u8 {
    return switch (@typeInfo(@TypeOf(name_or_enum_literal))) {
        .EnumLiteral => @tagName(name_or_enum_literal),
        .Pointer => name_or_enum_literal,
        else => @compileError("Expected enum literal or []const u8"),
    };
}

pub fn Param(comptime name_or_enum_literal: anytype) type {
    return struct {
        value: ?i64,
        signature: Parameter.Signature,

        pub const placeholder = parse_name(name_or_enum_literal);
    };
}

const Parameter = isa.Parameter;
const Domain = isa.Instruction_Encoding.Domain;
const isa = arch.isa;
const Register_Index = hw.Register_Index;
const hw = arch.hw;
const arch = @import("lib_arch");
const std = @import("std");
