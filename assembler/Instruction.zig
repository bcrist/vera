const std = @import("std");
const isa = @import("isa_types");
const ie = @import("isa_encoding");
const lex = @import("lex.zig");
const Expression = @import("Expression.zig");
const Constant = @import("Constant.zig");

label: ?Expression.Handle,
token: lex.Token.Handle,
operation: Operation,
params: ?Expression.Handle,
flags: FlagSet = .{},
line_number: u32,
address: u32 = 0,
length: u32 = 0,

pub const FlagSet = std.EnumSet(Flags);

pub const OperationType = std.meta.Tag(Operation);
pub const Operation = union(enum) {
    none, // label only
    nil, // same as none, but blocks wont backtrack over this

    // .insn are transformed to .bound_insn during the layout process:
    insn: MnemonicAndSuffix,
    bound_insn: *const ie.InstructionEncoding,

    // Data directives:
    db,
    dw,
    dd,

    // Stack control directives:
    // payload is the size of the stack frame (align 2)
    push: u32,
    pop: u32,

    // Section/block directives:
    section,
    boot,
    code,
    kcode,
    entry,
    kentry,
    data,
    kdata,
    @"const",
    kconst,
    stack,

    // Misc. directives:
    org,
    @"align",
    keep,
    def,
    undef,
    local,
    range,
};

pub const MnemonicAndSuffix = struct {
    mnemonic: isa.Mnemonic,
    suffix: isa.MnemonicSuffix,
};

pub const Flags = enum {
    depends_on_layout,

    // indicates that layout has been done for db/dw/dd/push/pop
    // and it doesn't need to be done again (implies !depends_on_layout)
    length_computed, 
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

pub fn isOrgHeader(op: OperationType) bool {
    // When these operations appear before a .org directive, they are included in the fixed-org chunk that follows, not the previous chunk (if any)
    // See SourceFile.collectChunks and SourceFile.backtrackOrgHeaders
    return switch (op) {
        .section,
        .boot,
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
        .nil,
        .@"align",
        .keep,
        .def,
        .undef,
        .local,
        .range,
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

pub fn getTokenRange(insn_handle: Handle, insn_tokens: []const lex.Token.Handle, token_kinds: []const lex.TokenKind) lex.Token.Range {
    var start_of_line = insn_tokens[insn_handle];
    var end_of_line = start_of_line;

    while (start_of_line > 0 and token_kinds[start_of_line - 1] != .newline) {
        start_of_line -= 1;
    }

    while (token_kinds[end_of_line] != .newline and token_kinds[end_of_line] != .eof) {
        end_of_line += 1;
    }

    return .{
        .first = start_of_line,
        .last = end_of_line - 1,
    };
}
