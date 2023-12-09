src_signature: Instruction_Signature,
src_constraints: []const Constraint,
dest_signature: Instruction_Signature,
dest_constant_values: []const Constant_Value,
transforms: []const Transform,

pub const Constant_Value = struct {
    placeholder: Placeholder_Info,
    constant: i64,
};

pub const Transform = struct {
    src: Placeholder_Info,
    dest: Placeholder_Info,
    ops: []const Op,
};

pub const Op = union (enum) {
    negate,
    offset: i64,
    multiply: i64,
    divide_trunc: i64,
};

pub fn try_transform(self: Instruction_Transform, insn: Instruction, dest_insn: *Instruction, dest_params: *[Parameter.Index.count]Parameter) bool {
    if (self.src_signature.mnemonic != insn.mnemonic) return false;
    if (self.src_signature.suffix != insn.suffix) return false;
    if (self.src_signature.params.len != insn.params.len) return false;
    for (self.src_signature.params, insn.params) |ps, param| {
        if (!std.meta.eql(ps, param.signature)) return false;
    }
    for (self.src_constraints) |constraint| {
        if (!constraint.matches(insn.params)) return false;
    }

    const params = dest_params[0..self.dest_signature.params.len];

    for (self.dest_signature.params, params) |sig, *param| {
        param.* = .{
            .signature = sig,
            .base_register_index = 0,
            .offset_register_index = 0,
            .constant = 0,
        };
    }
    for (self.dest_constant_values) |value| {
        const ok = value.placeholder.assign(value.constant, params);
        std.debug.assert(ok);
    }
    for (self.transforms) |xform| {
        var value = xform.src.evaluate(insn.params);
        for (xform.ops) |op| {
            value = switch (op) {
                .negate => -value,
                .offset => |offset| value + offset,
                .multiply => |multiplier| value * multiplier,
                .divide_trunc => |divisor| @divTrunc(value, divisor),
            };
        }
        const ok = xform.dest.assign(value, params);
        std.debug.assert(ok);
    }

    dest_insn.mnemonic = self.dest_signature.mnemonic;
    dest_insn.suffix = self.dest_signature.suffix;
    dest_insn.params = params;
    return true;
}

const Instruction_Transform = @This();
const Constraint = Instruction_Encoding.Constraint;
const Placeholder_Info = Instruction_Encoding.Placeholder_Info;
const Instruction_Encoding = isa.Instruction_Encoding;
const Instruction = isa.Instruction;
const Parameter = isa.Parameter;
const Instruction_Signature = isa.Instruction_Signature;
const isa = @import("../isa.zig");
const std = @import("std");
