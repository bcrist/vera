pub const Instruction = @import("isa/Instruction.zig");
pub const Parameter = @import("isa/Parameter.zig");
pub const Instruction_Encoding = @import("isa/Instruction_Encoding.zig");
pub const Encoding_Database = @import("isa/Encoding_Database.zig");
pub const Decoding_Database = @import("isa/Decoding_Database.zig");
pub const read_database = @import("isa/read_database.zig");
pub const write_database = @import("isa/write_database.zig");
pub const print = @import("isa/print.zig");

pub const database_source = @embedFile("isa/database.sx");

pub const Mnemonic = enum {
    _reserved,
    // Arithmetic:
    add, addc, cmp, cmpc, sub, subc, inc, dec, neg, negc,
    // Logical:
    xor, xnor, @"or", nor, @"and", nand,
    // Single bit:
    tb, cb, sb,
    // Shifts:
    shr, shl, shrc, shlc,
    // Multiply:
    mul, mulh,
    // Bit counting:
    csb, czb, csbl, czbl, csbt, czbt,
    // Branches & Calls:
    b, eab, dab, call, callx, ret, bb, bbn, bp, bpn,
    // Basic data movement:
    c, swap, ld, ldi, ild, st, sti, ist,
    // MMU:
    sat, rat,
    // Stack:
    frame, unframe, pop, push,
    // Atomics:
    sync, ald, ast, astz, aadd, adecnz, ax, axe,
    // Memcopy & streaming:
    mcba, mcb, mcfa, mcf, sia, si, soa, so, bld, bst,
    // Faults, interrupts, and context switching:
    fret, iret, ifex, ldrs, strs, srs, park,
    // Misc:
    nop,
};

pub const Mnemonic_Suffix = enum {
    none,

    z, // zero
    nz, // not zero
    lu, // less (unsigned)
    nlu, // not less (unsigned)
    gu, // greater (unsigned)
    ngu, // not greater (unsigned)
    n, // negative
    nn, // not negative
    c, // carry
    nc, // not carry
    v, // overflow
    nv, // not overflow
    ls, // less (signed)
    nls, // not less (signed)
    gs, // greater (signed)
    ngs, // not greater (signed)
    p, // positive
    np, // not positive
    lu_gu, // less (unsigned), greater (unsigned)
    lu_z, // less (unsigned), zero
    gu_z, // greater (unsigned), zero
    ls_gs, // less (signed), greater (signed)
    ls_z, // less (signed), zero
    gs_z, // greater (signed), zero
    n_z, // negative, zero
    p_z, // positive, zero
    n_p, // negative, positive

    i, // insn
    s, // stack
    d, // data
    w, // data-write
    r, // data-read
};

pub const Instruction_Signature = struct {
    mnemonic: Mnemonic,
    suffix: Mnemonic_Suffix,
    params: []const Parameter.Signature,

    pub fn eql(self: Instruction_Signature, other: Instruction_Signature) bool {
        if (self.mnemonic != other.mnemonic or self.suffix != other.suffix or self.params.len != other.params.len) return false;
        for (self.params, other.params) |sp, op| {
            if (!std.meta.eql(sp, op)) return false;
        }
        return true;
    }
};

pub const Special_Register = enum {
    ip,
    sp,
    rp,
    bp,
    uxp,
    kxp,
    asn,
    stat,
};

pub const Address_Space = enum {
    data,
    insn,
    stack,

    pub fn directive_name(self: ?Address_Space) []const u8 {
        return if (self) |s| switch (s) {
            .data => ".d",
            .insn => ".i",
            .stack => ".s",
        } else ".raw";
    }
};

pub const Branch_Kind = enum {
    nonbranching,
    conditional,
    unconditional,
    call,
};
pub fn branch_kind(mnemonic: Mnemonic, suffix: Mnemonic_Suffix) Branch_Kind {
    return switch (mnemonic) {
        ._reserved => unreachable,

        .add, .addc, .cmp, .cmpc, .sub, .subc, .inc, .dec, .neg, .negc,
        .xor, .xnor, .@"or", .nor, .@"and", .nand,
        .tb, .cb, .sb,
        .shr, .shl, .shrc, .shlc,
        .mul, .mulh,
        .csb, .czb, .csbl, .czbl, .csbt, .czbt,
        .c, .swap, .ld, .ldi, .ild, .st, .sti, .ist,
        .sat, .rat,
        .frame, .unframe, .pop, .push,
        .sync, .ald, .ast, .astz, .aadd, .adecnz, .ax, .axe,
        .mcba, .mcb, .mcfa, .mcf, .sia, .si, .soa, .so, .bld, .bst,
        .ifex, .ldrs, .strs, .srs,
        .nop,
        => .nonbranching,

        .b, .bb, .bbn, .bp, .bpn,
        .eab, .dab, .ret, .fret, .iret, .park,
        => switch (suffix) {
            .none, .i, .s, .d, .w, .r => .unconditional,
            else => .conditional,
        },

        .call, .callx => .call,
    };
}

pub const Encoded_Instruction = struct {
    data: Data,
    len: Length_Type,

    pub const Data = u128;
    pub const Bit_Length_Type = std.math.Log2Int(Data);
    pub const Length_Type = std.meta.Int(.unsigned, @bitSizeOf(Bit_Length_Type) - 3);

    pub fn as_bytes(self: *const Encoded_Instruction) []const u8 {
        return std.mem.asBytes(&self.data)[0..self.len];
    }
};

pub const Signedness = std.builtin.Signedness;
pub const std = @import("std");
