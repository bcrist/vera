value: Value,
domain: Domain,
arithmetic_offset: u64,
bit_offset: Encoded_Instruction.Bit_Length_Type,
bit_count: Encoded_Instruction.Bit_Length_Type,

pub fn required_bits(self: Encoder) Encoded_Instruction.Bit_Length_Type {
    return self.bit_offset + self.bit_count;
}

pub fn bit_mask(self: Encoder) Encoded_Instruction.Data {
    const W = std.meta.Int(.unsigned, @bitSizeOf(Encoded_Instruction.Data) + 1);
    const high_ones = (@as(W, 1) << self.bit_count) - 1;
    return @intCast(high_ones << self.bit_offset);
}

pub fn encode(self: Encoder, insn: isa.Instruction, out: *Encoded_Instruction.Data) bool {
    return self.encode_value(self.value.evaluate(insn.params), out);
}

pub fn encode_value(self: Encoder, value: i64, out: *Encoded_Instruction.Data) bool {
    const raw = self.domain.encode(value) orelse return false;
    var n: Encoded_Instruction.Data = raw + self.arithmetic_offset;
    n = @shlExact(n, self.bit_offset);
    n &= self.bit_mask();
    out.* |= n;
    return true;
}

pub fn decode(self: Encoder, data: Encoded_Instruction.Data, out: []isa.Parameter) bool {
    const shifted_data = data >> self.bit_offset;
    const bits = self.bit_count;
    const mask = (@as(u64, 1) << @intCast(bits)) - 1;
    if (shifted_data < self.arithmetic_offset) return false;
    const raw: u64 = @intCast((shifted_data - self.arithmetic_offset) & mask);
    if (self.domain.decode(raw)) |value| {
        return self.value.assign(value, out);
    }
    return false;
}

pub fn identity(what: anytype) Encoder {
    return shifted_offset(0, 0, what);
}

pub fn shifted(bit_offset: Encoded_Instruction.Bit_Length_Type, what: anytype) Encoder {
    return shifted_offset(bit_offset, 0, what);
}

pub fn offset(arith_offset: anytype, what: anytype) Encoder {
    return shifted_offset(0, arith_offset, what);
}

pub fn shifted_offset(bit_offset: Encoded_Instruction.Bit_Length_Type, arith_offset: anytype, what: anytype) Encoder {
    var value: Value = undefined;
    var domain: Domain = undefined;
    const T = @TypeOf(what);
    const info = @typeInfo(T);
    if (info == .Type) {
        value = .{ .placeholder = .{
            .kind = .param_constant,
            .index = .invalid,
            .name = what.placeholder,
        }};
        domain = what.domain;
    } else {
        if ((info == .Struct or info == .Enum) and @hasDecl(T, "value")) {
            const constant_value = T.value(what);
            const constant_value_info = @typeInfo(@TypeOf(constant_value));
            if (constant_value_info == .Int) {
                value = .{ .constant = constant_value };
                domain = .{ .int = .{
                    .signedness = constant_value_info.Int.signedness,
                    .bits = constant_value_info.Int.bits,
                    .multiple = 1,
                }};
            } else {
                const bit_size = @bitSizeOf(@TypeOf(constant_value));
                const Signed = std.meta.Int(.signed, bit_size);
                value = .{ .constant = @as(Signed, @bitCast(constant_value)) };
                domain = .{ .int = .{
                    .signedness = .signed,
                    .bits = bit_size,
                    .multiple = 1,
                }};
            }
        } else if (info == .Enum) {
            const Tag = std.meta.Tag(T);
            value = .{ .constant = @intFromEnum(what) };
            domain = .{ .int = .{
                .signedness = @typeInfo(Tag).Int.signedness,
                .bits = @bitSizeOf(Tag),
                .multiple = 1,
            }};
        } else if (info == .Int) {
            value = .{ .constant = what };
            domain = .{ .int = .{
                .signedness = info.Int.signedness,
                .bits = info.Int.bits,
                .multiple = 1,
            }};
        } else {
            const bit_size = @bitSizeOf(T);
            const Signed = std.meta.Int(.signed, bit_size);
            value = .{ .constant = @as(Signed, @bitCast(what)) };
            domain = .{ .int = .{
                .signedness = .signed,
                .bits = bit_size,
                .multiple = 1,
            }};
        }
    }
    return .{
        .value = value,
        .domain = domain,
        .arithmetic_offset = arith_offset,
        .bit_offset = bit_offset,
        .bit_count = @max(@bitSizeOf(@TypeOf(arith_offset)), domain.min_bits()),
    };
}

pub fn value_iterator(self: *const Encoder) Value_Iterator {
    return .{ 
        .encoder = self,
        .last_value = 0,
        .next_raw = 0,
        .max_raw = if (self.value == .constant) 0 else self.domain.max_encoded(),
    };
}

pub const Value_Iterator = struct {
    encoder: *const Encoder,
    last_value: i64,
    next_raw: u64,
    max_raw: u64,

    pub fn next(self: *Value_Iterator) ?i64 {
        const raw = self.next_raw;
        if (raw <= self.max_raw) {
            self.next_raw += 1;
            self.last_value = switch (self.encoder.value) {
                .constant => |constant| constant,
                .placeholder => self.encoder.domain.decode(raw).?,
            };
            return self.last_value;
        }
        return null;
    }

    pub fn reset(self: *Value_Iterator) void {
        self.next_raw = 0;
    }
};

const Encoder = @This();
const Domain = Instruction_Encoding.Domain;
const Value = Instruction_Encoding.Value;
const Instruction_Encoding = isa.Instruction_Encoding;
const Encoded_Instruction = isa.Encoded_Instruction;
const isa = arch.isa;
const hw = arch.hw;
const arch = @import("lib_arch");
const std = @import("std");
