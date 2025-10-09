param: Parameter.Index,
kind: Kind,
name: []const u8,

pub const Kind = enum {
    param_base_gpr_offset,
    param_offset_gpr_offset,
    param_constant,
};

pub fn evaluate(self: Placeholder, params: []const Parameter) i64 {
    const index = self.param.raw();
    if (params.len <= index) return 0; // this is part of a "don't care" encoder
    return switch (self.kind) {
        .param_constant => params[index].constant,
        .param_base_gpr_offset => params[index].base_gpr_offset.raw(),
        .param_offset_gpr_offset => params[index].offset_gpr_offset.raw(),
    };
}

pub fn assign(self: Placeholder, value: i64, out: []Parameter) bool {
    const index = self.param.raw();
    if (out.len <= index) return true; // this is part of a "don't care" encoder
    switch (self.kind) {
        .param_constant => out[index].constant = value,
        .param_base_gpr_offset => {
            if (value < 0 or value > arch.bus.K.Read_Index_Offset.max) return false;
            out[index].base_gpr_offset = .init(@intCast(value));
        },
        .param_offset_gpr_offset => {
            if (value < 0 or value > arch.bus.K.Read_Index_Offset.max) return false;
            out[index].offset_gpr_offset = .init(@intCast(value));
        },
    }
    return true;
}

pub fn eql(a: Placeholder, b: Placeholder) bool {
    return a.param == b.param and a.kind == b.kind and std.mem.eql(u8, a.name, b.name);
}

pub fn restrictions(self: Placeholder, form: Instruction.Form) Restrictions_Iterator {
    return .init(form, self);
}

pub const Restrictions_Iterator = @import("Placeholder/Restrictions_Iterator.zig");

const Placeholder = @This();

const Instruction = @import("Instruction.zig");
const Parameter = @import("Parameter.zig");
const arch = @import("arch");
const std = @import("std");
