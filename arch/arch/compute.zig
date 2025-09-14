pub const Unit = enum (u3) {
    none = 0,
    alu = 1,
    shift = 2,
    mult = 3,
    count = 4,
    extend = 5,
    // 6 unused
    // 7 unused

    pub inline fn init(raw_value: Raw) Unit {
        return @enumFromInt(raw_value);
    }

    pub inline fn raw(self: Unit) Raw {
        return @intFromEnum(self);
    }

    pub const format = fmt.format_enum_dec;

    pub const Raw = meta.Backing(Unit);
};

pub const Mode = packed union {
    alu: ALU,
    shift: Shift,
    mult: Multiply,
    count_extend: Count_Extend,

    pub inline fn init(raw_value: Raw) Mode {
        return @bitCast(raw_value);
    }

    pub inline fn raw(self: Mode) Raw {
        return @bitCast(self);
    }

    pub const format = fmt.format_raw_hex;

    pub const Raw = std.meta.Int(.unsigned, @bitSizeOf(Mode));

    pub const ALU = packed struct (u5) {
        op: Opcode,
        use_stat_c: bool,
        invert_cin: bool,

        /// As specified in L4C381 datasheet
        pub const Opcode = enum(u3) {
            all_zeroes = 0,
            not_j_plus_k = 1,
            j_plus_not_k = 2,
            j_plus_k = 3,
            j_xor_k = 4,
            j_or_k = 5,
            j_and_k = 6,
            all_ones = 7,

            pub inline fn init(raw_value: Opcode.Raw) Opcode {
                return @bitCast(raw_value);
            }

            pub inline fn raw(self: Opcode) Opcode.Raw {
                return @bitCast(self);
            }

            pub const format = fmt.format_enum_dec;

            pub const Raw = meta.Backing(Opcode);
        };

        pub const all_zeroes: ALU = .{ .op = .all_zeroes, .use_stat_c = false, .invert_cin = false };
        pub const nadd: ALU = .{ .op = .not_j_plus_k, .use_stat_c = false, .invert_cin = true };
        pub const naddc: ALU = .{ .op = .not_j_plus_k, .use_stat_c = true, .invert_cin = false };
        pub const sub: ALU = .{ .op = .j_plus_not_k, .use_stat_c = false, .invert_cin = true };
        pub const subc: ALU = .{ .op = .j_plus_not_k, .use_stat_c = true, .invert_cin = false };
        pub const add: ALU = .{ .op = .j_plus_k, .use_stat_c = false, .invert_cin = false };
        pub const addc: ALU = .{ .op = .j_plus_k, .use_stat_c = true, .invert_cin = false };
        pub const addi: ALU = .{ .op = .j_plus_k, .use_stat_c = false, .invert_cin = true };
        pub const logic_xor: ALU = .{ .op = .j_xor_k, .use_stat_c = false, .invert_cin = false };
        pub const logic_or: ALU = .{ .op = .j_or_k, .use_stat_c = false, .invert_cin = false };
        pub const logic_and: ALU = .{ .op = .j_and_k, .use_stat_c = false, .invert_cin = false };
        pub const all_ones: ALU = .{ .op = .all_ones, .use_stat_c = false, .invert_cin = false };
    };

    pub const Shift = packed struct (u5) {
        left: bool,
        left_xor_swap_bytes: bool,
        left_xor_swap_halves: bool,
        cin: enum (u2) {
            zero = 0,
            zero_bitreverse = 1,
            j31 = 2,
            stat_c = 3,
        },
        
        pub const shrl: Shift = .{ .left = false, .left_xor_swap_bytes = false, .left_xor_swap_halves = false, .cin = .zero };
        pub const shra: Shift = .{ .left = false, .left_xor_swap_bytes = false, .left_xor_swap_halves = false, .cin = .j31 };
        pub const shrc: Shift = .{ .left = false, .left_xor_swap_bytes = false, .left_xor_swap_halves = false, .cin = .stat_c };
        pub const shl: Shift = .{ .left = true, .left_xor_swap_bytes = true, .left_xor_swap_halves = true, .cin = .zero };
        pub const shlc: Shift = .{ .left = true, .left_xor_swap_bytes = true, .left_xor_swap_halves = true, .cin = .stat_c };

        // K should be zero for these:
        pub const swap_bytes: Shift = .{ .left = false, .left_xor_swap_bytes = true, .left_xor_swap_halves = false, .cin = .zero };
        pub const swap_halves: Shift = .{ .left = false, .left_xor_swap_bytes = false, .left_xor_swap_halves = true, .cin = .zero };
        pub const reverse_bytes: Shift = .{ .left = false, .left_xor_swap_bytes = true, .left_xor_swap_halves = true, .cin = .zero };
        pub const reverse_bits: Shift = .{ .left = false, .left_xor_swap_bytes = false, .left_xor_swap_halves = false, .cin = .zero_bitreverse };
        pub const reverse_bits_in_halves: Shift = .{ .left = false, .left_xor_swap_bytes = false, .left_xor_swap_halves = true, .cin = .zero_bitreverse };
        pub const reverse_bits_in_bytes: Shift = .{ .left = false, .left_xor_swap_bytes = true, .left_xor_swap_halves = true, .cin = .zero_bitreverse };
    };

    pub const Multiply = packed struct (u5) {
        j_type: Signedness,
        k_type: Signedness,
        j: Half,
        k: Half,
        shift_result: bool, // 16 LSBs will be 0, 16 MSBs will come from LSBs of product; useful when doing 32b * 32b -> 32b multiply

        pub const Signedness = enum (u1) {
            unsigned = 0,
            signed = 1,
        };

        pub const Half = enum (u1) {
            lsb = 0, 
            msb = 1,
        };
    };

    pub const Count_Extend = packed struct (u5) {
        invert_j: bool,
        invert_k: bool,
        saturate: enum (u1) { left, right },
        invert_saturated: bool,
        sign_extend: bool,

        // When doing popcount (including leading/trailing variants), J and K should be identical:
        pub const count_ones: Count_Extend = .{ .invert_j = false, .invert_k = false, .saturate = .right, .invert_saturated = false, .sign_extend = false };
        pub const count_zeroes: Count_Extend = .{ .invert_j = true, .invert_k = true, .saturate = .right, .invert_saturated = false, .sign_extend = false };
        pub const count_leading_ones: Count_Extend = .{ .invert_j = false, .invert_k = true, .saturate = .right, .invert_saturated = true, .sign_extend = false };
        pub const count_leading_zeroes: Count_Extend = .{ .invert_j = true, .invert_k = false, .saturate = .right, .invert_saturated = true, .sign_extend = false };
        pub const count_trailing_ones: Count_Extend = .{ .invert_j = false, .invert_k = true, .saturate = .left, .invert_saturated = true, .sign_extend = false };
        pub const count_trailing_zeroes: Count_Extend = .{ .invert_j = true, .invert_k = false, .saturate = .left, .invert_saturated = true, .sign_extend = false };

        // To sign/zero extend J to N bits, the low N-1 bits of K should be 0, and the Nth bit of K should be a 1:
        pub const zero_extend_or_truncate: Count_Extend = .{ .invert_j = false, .invert_k = false, .saturate = .right, .invert_saturated = false, .sign_extend = false };
        pub const sign_extend_or_truncate: Count_Extend = .{ .invert_j = false, .invert_k = false, .saturate = .right, .invert_saturated = false, .sign_extend = true };

        // When doing saturation, J should be 0 and K should be the value to saturate:
        pub const saturate_ones_left: Count_Extend = .{ .invert_j = false, .invert_k = false, .saturate = .left, .invert_saturated = true, .sign_extend = false };
        pub const saturate_ones_right: Count_Extend = .{ .invert_j = false, .invert_k = false, .saturate = .right, .invert_saturated = true, .sign_extend = false };
        pub const saturate_zeroes_left: Count_Extend = .{ .invert_j = false, .invert_k = true, .saturate = .left, .invert_saturated = false, .sign_extend = false };
        pub const saturate_zeroes_right: Count_Extend = .{ .invert_j = false, .invert_k = true, .saturate = .right, .invert_saturated = false, .sign_extend = false };
    };
};

const fmt = @import("fmt.zig");
const meta = @import("meta");
const std = @import("std");
