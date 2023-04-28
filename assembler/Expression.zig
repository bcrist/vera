const std = @import("std");
const lex = @import("lex.zig");
const ie = @import("instruction_encoding");
const Constant = @import("Constant.zig");
const SourceFile = @import("SourceFile.zig");
const Instruction = @import("Instruction.zig");

const Expression = @This();

const ExpressionType = ie.ExpressionType;

token: lex.Token.Handle,
info: Info,
resolved_type: ExpressionType,
resolved_constant: ?*const Constant,
flags: FlagSet,

pub const Handle = u31;

pub const FlagSet = std.EnumSet(Flags);
pub const Flags = enum {
    constant_depends_on_layout,
};

pub const Kind = std.meta.Tag(Info);
pub const Info = union(enum) {
    list: Binary,
    arrow_list: Binary,
    literal_int,
    literal_str,
    literal_reg,
    literal_symbol_def,
    directive_symbol_def: Unary,

    literal_symbol_ref,
    directive_symbol_ref: Unary,

    plus: Binary,
    minus: Binary,
    negate: Unary,
};

pub const Unary = Expression.Handle;
pub const Binary = struct {
    left: Expression.Handle,
    right: Expression.Handle,
};
