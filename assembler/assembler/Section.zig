pub const Kind = enum {
    info,
    boot,
    code_user,
    code_kernel,
    entry_user,
    entry_kernel,
    data_user,
    data_kernel,
    constant_user,
    constant_kernel,

    pub fn from_directive(op: Instruction.Operation_Type) Kind {
        return switch (op) {
            .section => .info,
            .boot => .boot,
            .code => .code_user,
            .kcode => .code_kernel,
            .entry => .entry_user,
            .kentry => .entry_kernel,
            .data => .data_user,
            .kdata => .data_kernel,
            .@"const" => .constant_user,
            .kconst => .constant_kernel,

            .stack, .none, .nil, .insn, .bound_insn, .org,
            .keep, .def, .undef, .local, .@"align",
            .db, .dw, .dd, .zb, .zw, .zd, .push, .pop, .range,
            => unreachable,
        };
    }

    pub fn default_name(self: Kind) []const u8 {
        return switch (self) {
            .info => "default_info",
            .boot => "default_boot",
            .code_user => "default_code",
            .code_kernel => "kernel_code",
            .entry_user => "entry_code",
            .entry_kernel => "kernel_entry_code",
            .data_user => "rwdata",
            .data_kernel => "kernel_rwdata",
            .constant_user => "rdata",
            .constant_kernel => "kernel_rdata",
        };
    }

    pub fn access_policies(self: Kind, chunk_length: usize) Access_Policies {
        return .{
            .read = switch (self) {
                .info => null,
                .boot, .code_kernel, .entry_kernel, .data_kernel, .constant_kernel => .kernel_private,
                .code_user, .entry_user, .data_user, .constant_user => .unprivileged,
            },
            .write = switch (self) {
                .boot, .data_kernel => .kernel_private,
                .data_user => .unprivileged,
                .info, .code_user, .code_kernel, .entry_user, .entry_kernel, .constant_user, .constant_kernel => null,
            },
            .execute = switch (self) {
                .boot, .code_kernel => .kernel_private,
                .code_user, .entry_user => .unprivileged,
                .entry_kernel => if (chunk_length < 256) .kernel_entry_256 else .kernel_entry_4096,
                .info, .data_user, .data_kernel, .constant_user, .constant_kernel => null,
            },
        };
    }
};

pub const Access_Policies = struct {
    read: ?at.Access_Policy,
    write: ?at.Access_Policy,
    execute: ?at.Access_Policy,
};

name: []const u8,
kind: Kind,
has_chunks: bool,
range: ?Assembler.Address_Range,

const Section = @This();
pub const Handle = u31;

pub fn address_range(self: Section) Assembler.Address_Range {
    return self.range orelse .{
        .first = 0x1000,
        .len = 0x1_0000_0000 - 0x1000,
    };
}

const Instruction = @import("Instruction.zig");
const Expression = @import("Expression.zig");
const Assembler = @import("Assembler.zig");
const Page_Data = @import("Page_Data.zig");
const at = arch.addr.translation;
const arch = @import("arch");
const std = @import("std");