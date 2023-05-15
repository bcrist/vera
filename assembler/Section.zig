const std = @import("std");
const bus = @import("bus_types");
const SourceFile = @import("SourceFile.zig");
const Instruction = @import("Instruction.zig");
const Expression = @import("Expression.zig");
const Assembler = @import("Assembler.zig");

const PageInfo = Assembler.PageInfo;

const Section = @This();

pub const Kind = enum {
    info,
    code_user,
    code_kernel,
    entry_user,
    entry_kernel,
    data_user,
    data_kernel,
    constant_user,
    constant_kernel,
    stack,
};

pub const Handle = u31;

name: []const u8,
kind: Kind,
has_chunks: bool,
