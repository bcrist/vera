pub fn Int(comptime name_or_enum_literal: anytype, comptime T: type) type {
    return struct {
        value: T,

        pub const placeholder = parse_name(name_or_enum_literal);
        pub const domain: Domain = .{ .int = .{
            .signedness = @typeInfo(T).Int.signedness,
            .bits = @bitSizeOf(T),
            .multiple = 1,
        }};
    };
}

pub fn Reg(comptime name_or_enum_literal: anytype) type {
    return Int(name_or_enum_literal, Register_Index);
}

pub fn Range(comptime name_or_enum_literal: anytype, comptime min: i64, comptime max: i64) type {
    return struct {
        value: i64,

        pub const placeholder = parse_name(name_or_enum_literal);
        pub const domain: Domain = .{ .range = .{
            .min = min,
            .max = max,
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

fn parse_name(comptime name_or_enum_literal: anytype) []const u8 {
    return switch (@typeInfo(@TypeOf(name_or_enum_literal))) {
        .EnumLiteral => @tagName(name_or_enum_literal),
        .Pointer => name_or_enum_literal,
        else => @compileError("Expected enum literal or []const u8"),
    };
}

const Domain = isa.Instruction_Encoding.Domain;
const isa = arch.isa;
const Register_Index = hw.Register_Index;
const hw = arch.hw;
const arch = @import("lib_arch");
const std = @import("std");
