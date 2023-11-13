pub const hw = @import("arch/hardware.zig");

pub const Mnemonic = enum {
    _reserved,
    // Arithmetic:
    add, addc, cmp, cmpb, sub, subb, inc, incc, dec, decb, neg, negb,
    // Logical:
    not, xor, xnor, @"or", nor, @"and", nand, andnot, @"test", testz,
    // Single bit:
    testb, testbz, clrb, setb, tglb,
    // Shifts:
    shr, shl, shrc, shlc,
    // Multiply:
    mul, mulh,
    // Bit counting:
    cb, cz, clb, clz, ctb, ctz,
    // Branches & Calls:
    b, eab, dab, call, ret,
    bb, bbn,
    bp, bpn,
    // Basic data movement:
    c, dup, ld, ldi, ild, st, sti, ist,
    // MMU:
    sat, rat,
    // Stack:
    frame, unframe, pop, push,
    // Atomics:
    sync, ald, ast, astz, aadd, ainc, adecnz, ax, axe,
    // Memcopy & streaming:
    mcr, mcrb, mcf, mcfb, si, sib, so, sob,
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

pub fn print_mnemonic_and_suffix(writer: anytype, mnemonic: Mnemonic, suffix: Mnemonic_Suffix) !usize {
    const mnemonic_str = @tagName(mnemonic);
    var len = mnemonic_str.len;
    try writer.writeAll(mnemonic_str);
    if (suffix != .none) {
        const suffix_str = @tagName(suffix);
        try writer.writeByte('.');
        for (suffix_str) |b| {
            try writer.writeByte(if (b == '_') '.' else b);
        }
        len += suffix_str.len + 1;
    }
    return len;
}

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
            .data => ".D",
            .insn => ".I",
            .stack => ".S",
        } else ".RAW";
    }
};

pub const Branch_Kind = enum {
    nonbranching,
    conditional,
    unconditional,
    call,
};
pub fn branch_kind(mnemonic: Mnemonic, suffix: Mnemonic_Suffix) Branch_Kind {
    // TODO ensure exec_next_insn() is only used in microcode of instructions where this returns nonbranching or conditional
    return switch (mnemonic) {
        ._reserved => unreachable,

        .add, .addc, .cmp, .cmpb, .sub, .subb, .inc, .incc, .dec, .decb, .neg, .negb,
        .not, .xor, .xnor, .@"or", .nor, .@"and", .nand, .andnot, .@"test", .testz,
        .testb, .testbz, .clrb, .setb, .tglb,
        .shr, .shl, .shrc, .shlc,
        .mul, .mulh,
        .cb, .cz, .clb, .clz, .ctb, .ctz,
        .c, .dup, .ld, .ldi, .ild, .st, .sti, .ist,
        .sat, .rat,
        .frame, .unframe, .pop, .push,
        .sync, .ald, .ast, .astz, .aadd, .ainc, .adecnz, .ax, .axe,
        .mcr, .mcrb, .mcf, .mcfb, .si, .sib, .so, .sob,
        .ifex, .ldrs, .strs, .srs,
        .nop,
        => .nonbranching,

        .b, .bb, .bbn, .bp, .bpn,
        .eab, .dab, .ret, .fret, .iret, .park,
        => switch (suffix) {
            .none, .i, .s, .d, .w, .r => .unconditional,
            else => .conditional,
        },

        .call => .call,
    };
}

pub const Opcode = u16;

pub const Opcode_Range = struct {
    min: Opcode,
    max: Opcode,

    pub fn iterator(self: Opcode_Range) Iterator {
        return .{
            .next_opcode = self.min,
            .final_opcode = self.max
        };
    }

    pub const Iterator = struct {
        next_opcode: u17,
        final_opcode: u17,

        pub fn next(self: *Iterator) ?Opcode {
            if (self.next_opcode <= self.final_opcode) {
                const opcode: Opcode = @intCast(self.next_opcode);
                self.next_opcode += 1;
                return opcode;
            } else {
                return null;
            }
        }
    };
};

pub const Constant_Range = struct {
    min: i64,
    max: i64,
};

pub const Register_Index = u4;

pub const register_count = std.math.maxInt(Register_Index) + 1;

pub const Indexed_Register = struct {
    index: Register_Index,
    signedness: ?std.builtin.Signedness,
};

pub const Indexed_Register_Range = struct {
    min: Register_Index = 0,
    max: Register_Index = std.math.maxInt(Register_Index),
    signedness: ?std.builtin.Signedness = null,

    pub fn contains(self: Indexed_Register_Range, reg: Indexed_Register) bool {
        if (self.min > reg.index or reg.index > self.max) return false;
        if (self.signedness) |s| {
            return if (reg.signedness) |rs| s == rs else false;
        } else return true;
    }
};

pub const Context_State = extern struct {
    registers: [register_count]u16,
    rp: u32,
    sp: u32,
    bp: u32,
    fault_ua_dr: u32,
    fault_rsn_stat: u32,
    int_rsn_fault_iw_ik_ij: u32,
    temp_1: u32,
    ip: u32,
    next_ip: u32,
    asn: u32,
    kxp: u32,
    uxp: u32,
    temp_2: u32,
};

pub const Vector_Table = extern struct {
    reset: [hw.Pipeline.count]u16,
    double_fault: u16,
    page_fault: u16,
    access_fault: u16,
    page_align_fault: u16,
    invalid_instruction_fault: u16,
    instruction_protection_fault: u16,
};

const std = @import("std");
