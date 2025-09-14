value: Value,
domain: Domain,
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
    switch (self.value) {
        .placeholder => |info| {
            if (std.mem.eql(u8, info.name, "__")) {
                // This is a "don't care" encoder; we don't encode it at all.
                return true;
            }
        },
        else => {},
    }
    return self.encode_raw(self.value.evaluate(insn.params), out);
}

pub fn encode_value(self: Encoder, value: i64, out: *Encoded_Instruction.Data) bool {
    return self.encode_raw(self.value.raw_from_value(value), out);
}

fn encode_raw(self: Encoder, value: i64, out: *Encoded_Instruction.Data) bool {
    const raw: Encoded_Instruction.Data = self.domain.encode(value) orelse return false;
    var data = @shlExact(raw, self.bit_offset);
    data &= self.bit_mask();
    out.* |= data;
    return true;
}

pub fn decode(self: Encoder, data: Encoded_Instruction.Data, out: []isa.Parameter) bool {
    return self.value.assign(self.decode_raw(data) orelse return false, out);
}

pub fn decode_value(self: Encoder, data: Encoded_Instruction.Data) ?i64 {
    return self.value.value_from_raw(self.decode_raw(data) orelse return null);
}

fn decode_raw(self: Encoder, data: Encoded_Instruction.Data) ?i64 {
    const shifted_data = data >> self.bit_offset;
    const bits = self.bit_count;
    const mask = (@as(u64, 1) << @intCast(bits)) - 1;
    const raw: u64 = @intCast(shifted_data & mask);
    return self.domain.decode(raw);
}

pub fn init(bit_offset: Encoded_Instruction.Bit_Length_Type, what: anytype) Encoder {
    var value: Value = undefined;
    var domain: Domain = undefined;
    const T = @TypeOf(what);
    switch (@typeInfo(T)) {
        .@"type" => {
            if (@hasDecl(what, "Inner")) {
                const inner_encoder = comptime Encoder.init(0, what.Inner);
                if (comptime std.mem.eql(u8, @tagName(what.op), "negate")) {
                    value = .{ .negate = &inner_encoder.value };
                    domain = inner_encoder.domain;
                } else if (comptime std.mem.eql(u8, @tagName(what.op), "invert")) {
                    const bits = what.bits orelse inner_encoder.domain.min_bits();
                    value = .{ .xor = .{
                        .inner = &inner_encoder.value,
                        .mask = @bitCast((@as(u64, 1) << bits) - 1),
                    } };
                    domain = inner_encoder.domain;
                } else if (comptime std.mem.eql(u8, @tagName(what.op), "offset")) {
                    value = .{ .offset = .{
                        .inner = &inner_encoder.value,
                        .offset = what.offset,
                    } };
                    domain = inner_encoder.domain;
                } else @compileError("Unsupported placeholder transformation");
            } else {
                value = .{ .placeholder = .{
                    .kind = .param_constant,
                    .index = .invalid,
                    .name = what.placeholder,
                }};
                domain = what.domain;
            }
        },
        .@"enum" => {
            const Tag = std.meta.Tag(T);
            value = .{ .constant = @intFromEnum(what) };
            domain = .{ .int = .{
                .signedness = @typeInfo(Tag).int.signedness,
                .bits = @bitSizeOf(Tag),
                .multiple = 1,
            }};
        },
        .int => |info| {
            value = .{ .constant = what };
            domain = .{ .int = .{
                .signedness = info.signedness,
                .bits = info.bits,
                .multiple = 1,
            }};
        },
        else => {
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
        .bit_offset = bit_offset,
        .bit_count = domain.min_bits(),
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
        if (self.next_raw > self.max_raw) return null;
        const raw = self.encoder.domain.decode(self.next_raw).?;
        self.last_value = self.encoder.value.value_from_raw(raw);
        self.next_raw += 1;
        return self.last_value;
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
const isa = @import("../isa.zig");
const std = @import("std");
