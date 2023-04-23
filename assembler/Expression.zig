const std = @import("std");
const lex = @import("lex.zig");
const types = @import("types.zig");
const Constant = @import("Constant.zig");
const SourceFile = @import("SourceFile.zig");
const Instruction = @import("Instruction.zig");

const Expression = @This();

token: lex.Token.Handle,
info: Info,
resolved_type: ?*const types.Type,
resolved_constant: ?*const Constant,
flags: std.EnumSet(Flags),

pub const Handle = u31;

pub const Kind = std.meta.Tag(Info);
pub const Info = union(enum) {
    list: Binary,
    arrow_list: Binary,
    literal_int,
    literal_str,
    literal_reg,
    literal_symbol_def,
    directive_symbol_def: Expression.Handle,

    literal_symbol_ref,
    directive_symbol_ref: Expression.Handle,
};

const Binary = struct {
    left: Expression.Handle,
    right: Expression.Handle,
};

pub const Flags = enum {
    constant_depends_on_address,
};