const std = @import("std");
const misc = @import("misc");

pub const Mnemonic = enum {
    _reserved,
    // Arithmetic:
    ADD, ADDC, CMP, CMPB, SUB, SUBB, INC, INCC, DEC, DECB, NEG, NEGB,
    // Logical:
    NOT, XOR, XNOR, OR, NOR, AND, NAND, ANDNOT, TEST, TESTZ,
    // Single bit:
    TESTB, TESTBZ, CLRB, SETB, TGLB,
    // Shifts:
    SHR, SHL, SHRC, SHLC,
    // Multiply:
    MUL, MULH,
    // Bit counting:
    CB, CZ, CLB, CLZ, CTB, CTZ,
    // Branches & Calls:
    B, BP, BPN, EAB, DAB, CALL, RET,
    BB, BBN,
    // Basic data movement:
    C, DUP, LD, LDI, ILD, ST, STI, IST,
    // MMU:
    SAT, RAT,
    // Stack:
    FRAME, UNFRAME, POP, PUSH,
    // Atomics:
    SYNC, ALD, AST, ASTZ, AADD, AINC, ADECNZ, AX, AXE,
    // Memcpy & streaming:
    MCR, MCRB, MCF, MCFB, SI, SIB, SO, SOB, BLD, BST,
    // Faults, interrupts, and context switching:
    FRET, IRET, IFEX, LDRS, STRS, SRS, PARK,
    // Misc:
    NOP, NOPE, SLEEP, UNSLEEP,
};

pub const MnemonicSuffix = enum {
    none,

    Z, // zero
    NZ, // not zero
    LU, // less (unsigned)
    NLU, // not less (unsigned)
    GU, // greater (unsigned)
    NGU, // not greater (unsigned)
    N, // negative
    NN, // not negative
    C, // carry
    NC, // not carry
    V, // overflow
    NV, // not overflow
    LS, // less (signed)
    NLS, // not less (signed)
    GS, // greater (signed)
    NGS, // not greater (signed)
    P, // positive
    NP, // not positive
    LU_GU, // less (unsigned), greater (unsigned)
    LU_Z, // less (unsigned), zero
    GU_Z, // greater (unsigned), zero
    LS_GS, // less (signed), greater (signed)
    LS_Z, // less (signed), zero
    GS_Z, // greater (signed), zero
    N_Z, // negative, zero
    P_Z, // positive, zero
    N_P, // negative, positive

    I, // insn
    S, // stack
    D, // data
    W, // data-write
    R, // data-read
};

pub fn printMnemonicAndSuffix(writer: anytype, mnemonic: Mnemonic, suffix: MnemonicSuffix) !usize {
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

pub const Opcode = u16;

pub const SpecialRegister = enum {
    IP,
    SP,
    RP,
    BP,
    UXP,
    KXP,
    ASN,
    STAT,
};

pub const AddressSpace = enum {
    data,
    insn,
    stack,

    pub fn getDirectiveName(self: ?AddressSpace) []const u8 {
        return if (self) |s| switch (s) {
            .data => ".D",
            .insn => ".I",
            .stack => ".S",
        } else ".RAW";
    }
};

pub const BranchKind = enum {
    nonbranching,
    conditional,
    unconditional,
    call,
};
pub fn getBranchKind(mnemonic: Mnemonic, suffix: MnemonicSuffix) BranchKind {
    // TODO ensure exec_next_insn() is only used in microcode of instructions where this returns nonbranching or conditional
    return switch (mnemonic) {
        ._reserved => unreachable,

        .ADD, .ADDC, .CMP, .CMPB, .SUB, .SUBB, .INC, .INCC, .DEC, .DECB, .NEG, .NEGB,
        .NOT, .XOR, .XNOR, .OR, .NOR, .AND, .NAND, .ANDNOT, .TEST, .TESTZ,
        .TESTB, .TESTBZ, .CLRB, .SETB, .TGLB,
        .SHR, .SHL, .SHRC, .SHLC,
        .MUL, .MULH,
        .CB, .CZ, .CLB, .CLZ, .CTB, .CTZ,
        .C, .DUP, .LD, .LDI, .ILD, .ST, .STI, .IST,
        .SAT, .RAT,
        .FRAME, .UNFRAME, .POP, .PUSH,
        .SYNC, .ALD, .AST, .ASTZ, .AADD, .AINC, .ADECNZ, .AX, .AXE,
        .MCR, .MCRB, .MCF, .MCFB, .SI, .SIB, .SO, .SOB, .BLD, .BST,
        .NOP, .NOPE, .SLEEP, .UNSLEEP,
        .IFEX, .LDRS, .STRS, .SRS,
        => .nonbranching,

        .B, .BP, .BPN, .BB, .BBN, .EAB, .DAB,
        .RET, .FRET, .IRET, .PARK,
        => switch (suffix) {
            .none, .I, .S, .D, .W, .R => .unconditional,
            else => .conditional,
        },

        .CALL => .call,
    };
}

pub const OpcodeRange = struct {
    min: Opcode,
    max: Opcode,
};

pub const ConstantRange = struct {
    min: i64,
    max: i64,
};

pub const IndexedRegister = struct {
    index: misc.RegisterIndex,
    signedness: ?std.builtin.Signedness,
};

pub const IndexedRegisterRange = struct {
    min: misc.RegisterIndex = 0,
    max: misc.RegisterIndex = 15,
    signedness: ?std.builtin.Signedness = null,

    pub fn contains(self: IndexedRegisterRange, reg: IndexedRegister) bool {
        if (self.min > reg.index or reg.index > self.max) return false;
        if (self.signedness) |s| {
            return if (reg.signedness) |rs| s == rs else false;
        } else return true;
    }
};
