const std = @import("std");
const lex = @import("lex.zig");
const ie = @import("instruction_encoding");
const Expression = @import("Expression.zig");
const Constant = @import("Constant.zig");

label: ?Expression.Handle,
token: lex.Token.Handle,
operation: Operation,
params: ?Expression.Handle,
address: u32,

pub const OperationType = std.meta.Tag(Operation);
pub const Operation = union(enum) {
    none,
    insn: MnemonicAndSuffix,
    bound_insn: *const ie.InstructionEncoding,
    // TODO bound_push, bound_pop?

    // Directives:
    org,
    @"align",
    keep,
    def,
    undef,
    db,
    dw,
    dd,
    section,
    code,
    kcode,
    entry,
    kentry,
    data,
    kdata,
    @"const",
    kconst,
    stack,
    push,
    pop,
};

pub const MnemonicAndSuffix = struct {
    mnemonic: ie.Mnemonic,
    suffix: ie.MnemonicSuffix,
};

pub const Handle = u31;

pub const Iterator = struct {
    begin: Handle,
    end: Handle,

    pub fn next(self: *Iterator) ?Handle {
        const handle = self.begin;
        if (handle < self.end) {
            self.begin = handle + 1;
            return handle;
        } else {
            return null;
        }
    }
};

pub fn isSectionDirective(op: OperationType) bool {
    return switch (op) {
        .section,
        .code,
        .kcode,
        .entry,
        .kentry,
        .data,
        .kdata,
        .@"const",
        .kconst,
        .stack,
        => true,

        .none,
        .org,
        .@"align",
        .keep,
        .def,
        .undef,
        .insn,
        .bound_insn,
        .db,
        .dw,
        .dd,
        .push,
        .pop,
        => false,
    };
}

pub fn isOrgHeader(op: OperationType) bool {
    // When these operations appear before a .org directive, they are included in the fixed-org chunk that follows, not the previous chunk (if any)
    // See SourceFile.collectChunks and SourceFile.backtrackOrgHeaders
    return switch (op) {
        .section,
        .code,
        .kcode,
        .entry,
        .kentry,
        .data,
        .kdata,
        .@"const",
        .kconst,
        .stack,
        .none,
        .@"align",
        .keep,
        .def,
        .undef,
        => true,

        .org,
        .insn,
        .bound_insn,
        .db,
        .dw,
        .dd,
        .push,
        .pop,
        => false,
    };
}