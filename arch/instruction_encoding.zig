const std = @import("std");
const bits = @import("bits");
const deep_hash_map = @import("deep_hash_map");
const sx = @import("sx");
const uc = @import("microcode");
const misc = @import("misc");
const ControlSignals = @import("ControlSignals");

const assert = std.debug.assert;

// Conceptually, Opcode should probably publicly live here,
// but a lot of stuff only needs Opcode and nothing else in this file,
// so for compilation speed it's in misc.zig
const Opcode = misc.Opcode;

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
    // TODO BB, BBN,
    // Basic data movement:
    C, DUP, LD, LDI, ILD, ST, STI, IST,
    // MMU:
    CAT, CATM, CATO, SAT, CSAT, RAT,
    // Stack:
    FRAME, UNFRAME, THINK, UNTHINK, POP, PUSH,
    // Atomics:
    SYNC, ALD, AST, ASTZ, AADD, AINC, ADECNZ, AX, AXE,
    // Memcpy & streaming:
    MCR, MCRB, MCF, MCFB, SI, SIB, SO, SOB,
    // Faults, interrupts, and context switching:
    FRET, IRET, IFEX, LDRS, STRS, SRS, WFI,
    // Misc:
    NOP, NOPE, SLEEP, UNSLEEP,
};

pub fn mnemonicName(mnemonic: Mnemonic) []const u8 {
    return switch (mnemonic) {
        .ADD => "Add",
        .ADDC => "Add with Carry",
        .CMP => "Compare",
        .CMPB => "Compare with Borrow",
        .SUB => "Subtract",
        .SUBB => "Subtract with Borrow",
        .INC => "Increment",
        .INCC => "Increment with Carry",
        .DEC => "Decrement",
        .DECB => "Decrement with Borrow",
        .NEG => "Negate",
        .NEGB => "Negate with Borrow",
        .NOT => "Bitwise Complement",
        .XOR => "Bitwise Exclusive OR",
        .XNOR => "Bitwise Equivalence",
        .OR => "Bitwise OR",
        .NOR => "Bitwise NOR",
        .AND => "Bitwise AND",
        .NAND => "Bitwise NAND",
        .ANDNOT => "Bitwise Half-complemented AND",
        .TEST => "Test Bits",
        .TESTZ => "Test Bits with Z",
        .TESTB => "Test Indexed Bit",
        .TESTBZ => "Test Indexed Bit with Z",
        .CLRB => "Clear Indexed Bit",
        .SETB => "Set Indexed Bit",
        .TGLB => "Toggle Indexed Bit",
        .SHR => "Shift Right",
        .SHL => "Shift Left",
        .SHR => "Shift Right with Carry",
        .SHL => "Shift Left with Carry",
        .MUL => "Multiply",
        .MULH => "Multiply, High Half",
        .CB => "Count Set Bits",
        .CZ => "Count Zero Bits",
        .CLB => "Count Leading Set Bits",
        .CLZ => "Count Leading Zero Bits",
        .CTB => "Count Trailing Set Bits",
        .CTZ => "Count Trailing Zero Bits",
        .NOP => "No Operation",
        .NOPE => "No Operation",
        .B => "Branch",
        .BP => "Branch to Start of Page",
        .BPN => "Branch to Next Page",
        .BB => "Branch to Start of Block",
        .BBN => "Branch to Next Block",
        .EAB => "Enable Address Translation and Branch",
        .DAB => "Disable Address Translation and Branch",
        .CALL => "Call Subroutine",
        .RET => "Return from Subroutine",
        .IRET => "Return from Interrupt",
        .FRET => "Return from Fault Handler",
        .WFI => "Wait for Interrupt",
        .LDRS => "Load Registerset",
        .STRS => "Store Registerset",
        .SRS => "Switch to Registerset",
        .C => "Copy",
        .DUP => "Duplicate",
        .LD => "Load",
        .LDI => "Load and Increment",
        .ILD => "Increment and Load",
        .ST => "Store",
        .STI => "Store and Increment",
        .IST => "Increment and Store",
        .SAT => "Set Address Translation Entry",
        .RAT => "Remove Address Translation(s)",
        .FRAME => "Set Up Stack Frame",
        .UNFRAME => "Tear Down Stack Frame",
        .THINK => "Set Up Stack Frame with RP",
        .UNTHINK => "Tear Down Stack Frame with RP",
        .POP => "Pop from Stack",
        .PUSH => "Push to Stack",
        .SYNC => "Synchronize Next Instruction",
        .ALD => "Atomic Load",
        .AST => "Atomic Store",
        .ASTZ => "Atomic Store if Zero",
        .AADD => "Atomic Add",
        .AINC => "Atomic Increment",
        .ADECNZ => "Atomic Decrement if Not Zero",
        .AX => "Atomic Exchange",
        .AXE => "Atomic Exchange if Equal",
        .MCR => "Memory Copy Reverse",
        .MCRB => "Memory Copy Reverse, Bytewise",
        .MCF => "Memory Copy Forward",
        .MCFB => "Memory Copy Forward, Bytewise",
        .SI => "Stream In",
        .SIB => "Stream In Bytes",
        .SO => "Stream Out",
        .SOB => "Stream Out Bytes",
    };
}

// Assembler should infer this if it is unambiguous
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

pub const BaseExpressionType = enum {
    none,
    constant, // a literal or implicit number
    reg8, // a reference to the low 8 bits of a GPR, unknown signedness
    reg8u, // a reference to the low 8 bits of a GPR, unsigned
    reg8s, // a reference to the low 8 bits of a GPR, signed
    reg16, // a reference to a GPR, unknown signedness
    reg16u, // a reference to a GPR, unsigned
    reg16s, // a reference to a GPR, signed
    reg32, // a reference to a pair of GPRs, unknown signedness
    reg32u, // a reference to a pair of GPRs, unsigned
    reg32s, // a reference to a pair of GPRs, signed
    ptr32i, // a reference to a pair of GPRs, pointer to code memory
    ptr32s, // a reference to a pair of GPRs, pointer to stack memory
    ptr32d, // a reference to a pair of GPRs, pointer to data memory
    IP, // the instruction pointer register
    SP, // the stack pointer register
    STAT, // the status register
    RP, // the return pointer register
    BP, // the stack base pointer register
    ASN, // the address space number register
    KXP, // the kernel context pointer register
    UXP, // the user context pointer register

    pub fn isGPR(self: BaseExpressionType) bool {
        return switch (self) {
            .none, .constant, .IP, .SP, .STAT, .RP, .BP, .ASN, .KXP, .UXP => false,

            .reg8, .reg8u, .reg8s,
            .reg16, .reg16u, .reg16s,
            .reg32, .reg32u, .reg32s,
            .ptr32i, .ptr32s, .ptr32d => true,
        };
    }

    pub fn isImplicitlyConvertibleTo(self: BaseExpressionType, to: BaseExpressionType) bool {
        if (self == to) return true;

        switch (to) {
            .none, .reg8u, .reg8s, .reg16u, .reg16s, .reg32s, .IP, .SP, .STAT, .RP, .BP, .ASN, .KXP, .UXP => return false,

            .constant => return self == .none,
            .reg8 => return self == .reg8u or self == .reg8s,
            .reg16 => return self == .reg16u or self == .reg16s,
            .reg32 => return self == .reg32u or self == .reg32s or self == .ptr32i or self == .ptr32s or self == .ptr32d,
            .reg32u => return self == .ptr32i or self == .ptr32s or self == .ptr32d,
            .ptr32i, .ptr32s, .ptr32d => return self == .reg32 or self == .reg32u,
        }
    }
};

pub const ExpressionType = struct {
    base: BaseExpressionType,
    offset: BaseExpressionType = .none,

    pub fn isImplicitlyConvertibleTo(self: ExpressionType, to: ExpressionType) bool {
        return self.base.isImplicitlyConvertibleTo(to.base) and self.offset.isImplicitlyConvertibleTo(to.offset);
    }

    pub fn hasConstant(self: ExpressionType) bool {
        return self.base == .constant or self.offset == .constant;
    }
};

pub const ParameterSource = enum {
    implicit, // Parameter is not directly represented in the instruction
    OA, // The initial operand A
    OB, // The initial operand B
    OB_OA, // The combination of initial operands A and B (OB is MSB, OA is LSB)
    IP_plus_2_OA, // operand A, after it has been loaded from IP+2
    IP_plus_2_OB, // operand B, after it has been loaded from IP+2
    IP_plus_2_8, // the byte at IP+2
    IP_plus_2_16, // the word at IP+2
    IP_plus_2_32, // the 2 words at IP+2 and IP+4
    IP_plus_4_16, // the word at IP+4
    opcode, // Like implicit, but used when there are multiple opcodes in the encoding, and the parameter value is linearly related to the opcode
};

fn getMinLengthForParamSource(src: ParameterSource) usize {
    return switch (src) {
        .implicit, .OA, .OB, .OB_OA, .opcode => 2,
        .IP_plus_2_OA, .IP_plus_2_OB, .IP_plus_2_8 => 3,
        .IP_plus_2_16 => 4,
        .IP_plus_2_32, .IP_plus_4_16 => 6,
    };
}

pub const ConstantRange = struct {
    min: i64,
    max: i64,
};

pub const OpcodeRange = struct {
    min: Opcode,
    max: Opcode,
};

pub const ParameterEncoding = struct {
    type: ExpressionType,
    base_src: ParameterSource = .implicit,
    offset_src: ParameterSource = .implicit,
    arrow: bool = false,
    default_to_param: i8 = -1, // TODO!!!
    min_reg: misc.RegisterIndex = 0,
    max_reg: misc.RegisterIndex = 15,
    constant_reverse: bool = false, // store constant as `N - value` instead of `value`
    constant_align: u3 = 1,
    constant_ranges: []const ConstantRange = &.{},
    alt_constant_ranges: []const ConstantRange = &.{},

    pub fn print(self: ParameterEncoding, writer: anytype) !void {
        if (self.arrow) {
            try writer.writeAll("-> ");
        }
        try writer.print("{s} ({s})", .{ @tagName(self.type.base), @tagName(self.base_src) });
        if (self.type.offset != .none or self.offset_src != .implicit) {
            try writer.print(" + {s} ({s})", .{ @tagName(self.type.offset), @tagName(self.offset_src) });
        }

        if (self.min_reg == self.max_reg) {
            try writer.print(" reg {}", .{ self.min_reg });
        } else if (self.min_reg != 0 or self.max_reg != 15) {
            try writer.print(" reg {}-{}", .{ self.min_reg, self.max_reg });
        }

        if (self.constant_reverse) {
            try writer.writeAll(" rev");
        }

        if (self.constant_align != 1) {
            try writer.print(" align {}", .{ self.constant_align });
        }

        if (self.constant_ranges.len > 0) {
            try writer.writeAll(" ranges:");
            for (self.constant_ranges) |range| {
                try writer.print(" [{}, {}]", .{ range.min, range.max });
            }
        }

        if (self.alt_constant_ranges.len > 0) {
            try writer.writeAll(" alt_ranges:");
            for (self.alt_constant_ranges) |range| {
                try writer.print(" [{}, {}]", .{ range.min, range.max });
            }
        }
    }
};

pub const InstructionEncoding = struct {
    mnemonic: Mnemonic,
    suffix: MnemonicSuffix,
    params: []const ParameterEncoding,
    opcode_base: Opcode,
    opcodes: OpcodeRange,

    pub fn print(self: InstructionEncoding, writer: anytype) !void {
        if (self.opcodes.min == self.opcodes.max) {
            try writer.print("{X:0>4} ", .{ self.opcodes.min });
        } else {
            try writer.print("{X:0>4}-{X:0>4} ", .{ self.opcodes.min, self.opcodes.max });
        }

        if (self.opcode_base != self.opcodes.min) {
            try writer.print("base {X:0>4} ", .{ self.opcode_base });
        }

        try writer.writeAll(@tagName(self.mnemonic));
        if (self.suffix != .none) {
            try writer.print(".{s}", .{ @tagName(self.suffix) });
        }
        try writer.writeAll(" {\n");
        for (self.params) |param| {
            try writer.writeAll("   ");
            try param.print(writer);
            try writer.writeAll("\n");
        }
        try writer.writeAll("}");
    }
};
pub const InstructionEncodingAndDescription = struct {
    encoding: InstructionEncoding,
    desc: []const u8,
};

pub fn eql(a: InstructionEncoding, b: InstructionEncoding) bool {
    return a.mnemonic == b.mnemonic
        and a.suffix == b.suffix
        and a.opcode_base == b.opcode_base
        and std.meta.eql(a.params, b.params)
        ;
}

fn tryComptimeRegisterParameterEncoding(comptime name: []const u8) ?ParameterEncoding {
    comptime {
        if (name.len < 2) return null;

        var encoding = ParameterEncoding{ .type = .{ .base = undefined } };

        switch (name[0]) {
            'B' => {
                if (std.mem.eql(u8, name, "BP")) {
                    encoding.type.base = .BP;
                    return encoding;
                } else {
                    encoding.type.base = .reg8;
                }
            },
            'R' => {
                if (std.mem.eql(u8, name, "RP")) {
                    encoding.type.base = .RP;
                    return encoding;
                } else {
                    encoding.type.base = .reg16;
                }
            },
            'X' => {
                encoding.type.base = .reg32;
            },
            'I' => {
                if (std.mem.eql(u8, name, "IP")) {
                    encoding.type.base = .IP;
                    return encoding;
                } else return null;
            },
            'S' => {
                if (std.mem.eql(u8, name, "SP")) {
                    encoding.type.base = .SP;
                    return encoding;
                } else if (std.mem.eql(u8, name, "STAT")) {
                    encoding.type.base = .STAT;
                    return encoding;
                } else return null;
            },
            else => {
                if (std.mem.eql(u8, name, "ASN")) {
                    encoding.type.base = .ASN;
                    return encoding;
                } else if (std.mem.eql(u8, name, "KXP")) {
                    encoding.type.base = .KXP;
                    return encoding;
                } else if (std.mem.eql(u8, name, "UXP")) {
                    encoding.type.base = .UXP;
                    return encoding;
                } else return null;
            },
        }

        var next: comptime_int = 2;

        switch (name[1]) {
            'a' => {
                encoding.base_src = .OA;
                if (name.len >= 3 and name[2] == '1') {
                    encoding.base_src = .IP_plus_2_OA;
                    next = 3;
                }
            },
            'b' => {
                encoding.base_src = .OB;
                if (name.len >= 3 and name[2] == '1') {
                    encoding.base_src = .IP_plus_2_OB;
                    next = 3;
                }
            },
            '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' => {
                var reg = name[1] - '0';
                if (name.len >= 3 and name[2] >= '0' and name[2] <= '9') {
                    reg = reg * 10 + (name[2] - '0');
                    next = 3;
                }
                encoding.base_src = .implicit;
                encoding.min_reg = reg;
                encoding.max_reg = reg;
            },
            else => return null,
        }

        if (name.len == next) {
            return encoding;
        } else if (name.len > next + 1) {
            return null;
        }

        switch (name[next]) {
            'U' => encoding.type.base = switch (encoding.type.base) {
                .reg8 => .reg8u,
                .reg16 => .reg16u,
                .reg32 => .reg32u,
                else => unreachable,
            },
            'S' => encoding.type.base = switch (encoding.type.base) {
                .reg8 => .reg8s,
                .reg16 => .reg16s,
                .reg32 => .reg32s,
                else => unreachable,
            },
            else => return,
        }

        return encoding;
    }
}

pub fn constantRange(comptime min: i64, comptime max: i64) []const ConstantRange {
    comptime {
        return &[_]ConstantRange{.{ .min = min, .max = max }};
    }
}

pub fn signedConstantRange(comptime min: i64, comptime max: i64) []const ConstantRange {
    comptime {
        return &[_]ConstantRange{
            .{ .min = 0, .max = max },
            .{ .min = min, .max = -1 },
        };
    }
}

fn parseImmediateLiteral(str: []const u8) !i64 {
    var num_str = str;
    var mult: i64 = 1;
    if (str[0] == 'n') {
        mult = -1;
        num_str = str[1..];
    }

    return mult * try std.fmt.parseInt(i64, num_str, 0);
}

fn tryComptimeImmediateParameterEncoding(comptime name: []const u8) ?ParameterEncoding {
    comptime {
        if (name.len < 4 or !std.mem.startsWith(u8, name, "imm")) {
            return null;
        }

        var encoding = ParameterEncoding{ .type = .{ .base = .constant } };

        var parts_iter = std.mem.split(u8, name[3..], "_");
        var part = parts_iter.next() orelse return null;

        // check for alignment suffix, e.g. /2 or /4
        if (part.len > 2 and part[part.len - 2] == '/') {
            encoding.constant_align = std.fmt.parseInt(u3, part[part.len - 1 ..], 10) catch return null;
            if (encoding.constant_align < 1) {
                return null;
            }
            part = part[0 .. part.len - 2];
        }

        var ip_offset: usize = 2;
        if (part.len > 2 and part[part.len - 2] == '@') {
            ip_offset = std.fmt.parseInt(u3, part[part.len - 1 ..], 10) catch return null;
            part = part[0 .. part.len - 2];
        }

        if (part.len == 0) {
            // imm_\d+
            // imm_\d+_\d+
            const min_str = parts_iter.next() orelse return null;
            const min = parseImmediateLiteral(min_str) catch return null;
            if (parts_iter.next()) |max_str| {
                const max = parseImmediateLiteral(max_str) catch return null;
                if (parts_iter.next()) |_| return null;

                encoding.base_src = .opcode;
                encoding.constant_ranges = constantRange(min, max);
                return encoding;
            } else {
                if (min != 0) {
                    encoding.constant_ranges = constantRange(min, min);
                }
                return encoding;
            }
        }

        if (std.mem.eql(u8, part, "ba8u")) {
            encoding.base_src = .OB_OA;
            encoding.constant_ranges = constantRange(0, 255);
            return encoding;
        }

        var style: u8 = switch (part[part.len - 1]) {
            'u', 's', 'n' => |c| c,
            else => 0,
        };
        if (style != 0) {
            part = part[0 .. part.len - 1];
        }

        var n_bits: ControlSignals.Literal = undefined;
        switch (part[0]) {
            'a', 'b', 'A', 'B' => |c| {
                if (part.len != 2 or part[1] != '4') return null;
                n_bits = 4;
                encoding.base_src = switch (c) {
                    'a' => .OA,
                    'b' => .OB,
                    'A' => .IP_plus_2_OA,
                    'B' => .IP_plus_2_OB,
                    else => unreachable,
                };
            },
            else => {
                n_bits = std.fmt.parseUnsigned(ControlSignals.Literal, part, 10) catch return null;
                if (n_bits <= 8) {
                    encoding.base_src = switch (ip_offset) {
                        2 => .IP_plus_2_8,
                        else => return null,
                    };
                } else if (n_bits <= 16) {
                    encoding.base_src = switch (ip_offset) {
                        2 => .IP_plus_2_16,
                        4 => .IP_plus_4_16,
                        else => return null,
                    };
                } else if (n_bits <= 32) {
                    encoding.base_src = switch (ip_offset) {
                        2 => .IP_plus_2_32,
                        else => return null,
                    };
                } else {
                    return null;
                }
            },
        }

        const a: i64 = encoding.constant_align;

        if (style == 0) {
            if (parts_iter.next()) |min_str| {
                const max_str = parts_iter.next() orelse return null;
                const min = parseImmediateLiteral(min_str) catch return null;
                const max = parseImmediateLiteral(max_str) catch return null;
                if (parts_iter.next()) |final| {
                    if (std.mem.eql(u8, final, "rev")) {
                        encoding.constant_reverse = true;
                    } else {
                        return null;
                    }
                }
                encoding.constant_ranges = constantRange(min, max);
                return encoding;
            }
        } else if (parts_iter.next()) |final| {
            if (std.mem.eql(u8, final, "rev")) {
                encoding.constant_reverse = true;
            } else {
                return null;
            }
        }

        switch (style) {
            'u' => {
                // unsigned; positive only
                const min: i64 = 0;
                const max: i64 = (1 << n_bits) - 1;
                encoding.constant_ranges = constantRange(min * a, max * a);
            },
            'n' => {
                // unsigned; negative only
                const min: i64 = -(1 << n_bits);
                const max: i64 = -1;
                encoding.constant_ranges = constantRange(min * a, max * a);
            },
            's' => {
                // signed
                const min: i64 = -(1 << (n_bits - 1));
                const max: i64 = (1 << (n_bits - 1)) - 1;
                encoding.constant_ranges = signedConstantRange(min * a, max * a);
            },
            0 => {
                const min: i64 = 0;
                const max: i64 = (1 << n_bits) - 1;
                encoding.constant_ranges = constantRange(min * a, max * a);
                const signed_min: i64 = -(1 << (n_bits - 1));
                const signed_max: i64 = (1 << (n_bits - 1)) - 1;
                encoding.alt_constant_ranges = signedConstantRange(signed_min * a, signed_max * a);
            },
            else => unreachable,
        }

        return encoding;
    }
}

fn getParameterValueForOpcode(comptime T: type, insn: InstructionEncoding, encoding: ParameterEncoding, opcode: Opcode, src: ParameterSource) T {
    var raw: i64 = undefined;
    switch (src) {
        .OA, .OB, .OB_OA => {
            const ua = uc.getAddressForOpcode(opcode, .{});
            raw = switch (src) {
                .OA => uc.getOAForAddress(ua) orelse unreachable,
                .OB => uc.getOBForAddress(ua) orelse unreachable,
                .OB_OA => bits.concat(.{
                    uc.getOAForAddress(ua) orelse unreachable,
                    uc.getOBForAddress(ua) orelse unreachable,
                }),
                else => unreachable,
            };
        },
        .opcode => {
            assert(opcode >= insn.opcodes.min);
            assert(opcode <= insn.opcodes.max);
            raw = @as(i64, opcode) - insn.opcode_base;
        },
        .implicit => {
            raw = 0;
        },
        .IP_plus_2_OA, .IP_plus_2_OB, .IP_plus_2_8, .IP_plus_2_16, .IP_plus_2_32, .IP_plus_4_16 => {
            std.debug.panic("Constant value varies based on immediate stored outside of opcode", .{});
        },
    }

    return @intCast(T, getParameterValue(raw, encoding) orelse
        std.debug.panic("Opcode {X:0>4} results in a constant outside the configured range(s) for this parameter", .{ opcode }));
}

fn getParameterValue(raw_value: i64, encoding: ParameterEncoding) ?i64 {
    var aligned = raw_value * encoding.constant_align;

    var total_values: i64 = 0;
    for (encoding.constant_ranges) |range| {
        total_values += range.max - range.min + 1;
    }

    if (encoding.constant_reverse) {
        aligned = total_values - aligned - 1;
    }

    for (encoding.constant_ranges) |range| {
        const values = range.max - range.min + 1;
        if (aligned < values) {
            return range.min + aligned;
        }
        aligned -= values;
    }

    if (encoding.constant_ranges.len == 0 and aligned == 0) {
        return 0;
    }

    return null;
}

pub fn getParameterConstantForOpcode(comptime T: type, insn: InstructionEncoding, encoding: ParameterEncoding, opcode: Opcode) T {
    assert(encoding.type.base == .constant);
    return getParameterValueForOpcode(T, insn, encoding, opcode, encoding.base_src);
}

pub fn getParameterOffsetForOpcode(comptime T: type, insn: InstructionEncoding, encoding: ParameterEncoding, opcode: Opcode) T {
    return getParameterValueForOpcode(T, insn, encoding, opcode, encoding.offset_src);
}

fn comptimeNamedParameterEncoding(comptime name: []const u8) ParameterEncoding {
    comptime {
        if (tryComptimeRegisterParameterEncoding(name)) |param| {
            return param;
        } else if (tryComptimeImmediateParameterEncoding(name)) |param| {
            return param;
        } else if (std.mem.eql(u8, name, "to")) {
            return ParameterEncoding{
                .type = .{ .base = .none },
                .arrow = true,
            };
        }

        @compileError("Unrecognized parameter encoding: " ++ name);
    }
}

pub fn comptimeParameterEncoding(comptime arg: anytype) ParameterEncoding {
    comptime {
        const ArgType = @TypeOf(arg);

        if (ArgType == ParameterEncoding) {
            return arg;
        } else switch (@typeInfo(ArgType)) {
            .EnumLiteral => return comptimeNamedParameterEncoding(@tagName(arg)),
            .Type => return @field(arg, "param_encoding"),
            else => @compileError("I don't know what to do with argument of type " ++ @typeName(ArgType)),
        }
    }
}

pub fn comptimeRelativeParameterEncoding(comptime param: ParameterEncoding, comptime base: BaseExpressionType, comptime base_src: ParameterSource) ParameterEncoding {
    comptime {
        var new_param = param;

        new_param.type.base = base;
        new_param.base_src = base_src;

        if (base_src == .implicit and base.isGPR()) {
            new_param.min_reg = 0;
            new_param.max_reg = 0;
        }

        if (param.type.base == .constant and param.base_src == .implicit and param.constant_ranges.len == 0) {
            new_param.type.offset = .none;
            new_param.offset_src = .implicit;
        } else {
            new_param.type.offset = param.type.base;
            new_param.offset_src = param.base_src;
        }

        return new_param;
    }
}

fn validateParameterEncodingConstants(comptime param: ParameterEncoding, src: ParameterSource) void {
    comptime {
        var min_total_values: usize = switch (src) {
            .implicit => 0,
            .opcode => 2,
            .OA, .OB, .IP_plus_2_OA, .IP_plus_2_OB => 16,
            .OB_OA, .IP_plus_2_8 => 256,
            .IP_plus_2_16, .IP_plus_4_16 => 65536,
            .IP_plus_2_32 => 1 << 32,
        };
        var max_total_values: usize = switch (src) {
            .implicit => 1,
            .OA, .OB, .IP_plus_2_OA, .IP_plus_2_OB => 16,
            .OB_OA, .IP_plus_2_8 => 256,
            .opcode, .IP_plus_2_16, .IP_plus_4_16 => 65536,
            .IP_plus_2_32 => 1 << 32,
        };

        min_total_values *= param.constant_align;
        max_total_values *= param.constant_align;

        var total_values: usize = 0;
        for (param.constant_ranges) |range| {
            if (range.max < range.min) {
                @compileError(std.fmt.comptimePrint("Range minimum bound of {} must not be larger than maximum bound of {}", .{
                    range.min,
                    range.max,
                }));
            }
            total_values += range.max - range.min + param.constant_align;
        }

        for (param.alt_constant_ranges) |range| {
            if (range.max < range.min) {
                @compileError(std.fmt.comptimePrint("Alt range minimum bound of {} must not be larger than maximum bound of {}", .{
                    range.min,
                    range.max,
                }));
            }
        }

        if (total_values < min_total_values or total_values > max_total_values) {
            if (min_total_values == max_total_values) {
                @compileError(std.fmt.comptimePrint("Expected exactly {} constant values for parameter encoding, but found {}", .{ min_total_values, total_values }));
            } else {
                @compileError(std.fmt.comptimePrint("Expected between {} and {} constant values for parameter encoding, but found {}", .{ min_total_values, max_total_values, total_values }));
            }
        }
    }
}

fn validateParameterEncoding(comptime param: ParameterEncoding) void {
    comptime {
        if (param.type.base == .constant) {
            if (param.type.offset == .constant) {
                @compileError("Base and offset type cannot both be constant!");
            }
            validateParameterEncodingConstants(param, param.base_src);
        } else if (param.type.offset == .constant) {
            validateParameterEncodingConstants(param, param.offset_src);
        } else {
            if (param.constant_ranges.len > 0 or param.constant_align > 1) {
                @compileError("This parameter does not involve a constant value");
            }
        }
    }
}

pub fn comptimeParameterEncodings(comptime args: anytype) []const ParameterEncoding {
    comptime {
        const ArgsType = @TypeOf(args);
        const args_type_info = @typeInfo(ArgsType);
        if (args_type_info != .Struct) {
            @compileError("Expected tuple or struct argument, found " ++ @typeName(ArgsType));
        }

        var params: []const ParameterEncoding = &[_]ParameterEncoding{};
        var arrow_on_next_param = false;

        inline for (args_type_info.Struct.fields) |field| {
            var param: ParameterEncoding = undefined;

            if (field.type == ParameterEncoding) {
                param = @field(args, field.name);
            } else switch (@typeInfo(field.type)) {
                .EnumLiteral => param = comptimeNamedParameterEncoding(@tagName(@field(args, field.name))),
                .Type => param = @field(@field(args, field.name), "param_encoding"),
                else => @compileError("I don't know what to do with argument of type " ++ @typeName(field.type)),
            }

            if (param.type.base == .none and param.arrow) {
                if (arrow_on_next_param) @compileError("Expected a parameter after .to");
                arrow_on_next_param = true;
            } else {
                if (arrow_on_next_param) {
                    param.arrow = true;
                    arrow_on_next_param = false;
                }
                validateParameterEncoding(param);
                params = params ++ [_]ParameterEncoding{param};
            }
        }

        if (arrow_on_next_param) {
            @compileError("Expected a parameter after .to");
        }

        return params;
    }
}

pub fn getInstructionLength(encoding: InstructionEncoding) usize {
    var len: usize = 2;

    for (encoding.params) |param| {
        const len_for_param = std.math.max(getMinLengthForParamSource(param.base_src), getMinLengthForParamSource(param.offset_src));

        if (len_for_param > len) {
            len = len_for_param;
        }
    }

    return len;
}

pub const Parameter = struct {
    arrow: bool = false,
    type: ExpressionType,
    base: i64,
    offset: i64 = 0,

    pub fn print(self: Parameter, writer: anytype) !void {
        if (self.arrow) {
            try writer.writeAll("-> ");
        }
        try writer.print("{s}({})", .{ @tagName(self.type.base), self.base });
        if (self.type.offset != .none or self.offset != 0) {
            try writer.print(" + {s}({})", .{ @tagName(self.type.offset), self.offset });
        }
    }

    pub fn eql(self: Parameter, other: Parameter) bool {
        return self.arrow == other.arrow
            and self.type.base == other.type.base
            and self.type.offset == other.type.offset
            and self.base == other.base
            and self.offset == other.offset
            ;
    }
};

pub const Instruction = struct {
    mnemonic: Mnemonic,
    suffix: MnemonicSuffix,
    params: []const Parameter,

    pub fn print(self: Instruction, writer: anytype) !void {
        try writer.writeAll(@tagName(self.mnemonic));
        if (self.suffix != .none) {
            try writer.print(".{s}", .{ @tagName(self.suffix) });
        }
        try writer.writeAll(" {\n");
        for (self.params) |param| {
            try writer.writeAll("   ");
            try param.print(writer);
            try writer.writeAll("\n");
        }
        try writer.writeAll("}");
    }

    pub fn eql(self: Instruction, other: Instruction) bool {
        if (self.mnemonic != other.mnemonic
            or self.suffix != other.suffix
            or self.params.len != other.params.len
        ) return false;

        var i: usize = 0;
        while (i < self.params.len) : (i += 1) {
            if (!self.params[i].eql(other.params[i])) return false;
        }

        return true;
    }
};

fn writeOB(buf: []u8, val: misc.OperandB) void {
    assert(buf.len >= 1);
    buf[0] = bits.concat(.{
        @truncate(u4, buf[0]),
        val,
    });
}

fn writeOA(buf: []u8, val: misc.OperandA) void {
    assert(buf.len >= 1);
    buf[0] = bits.concat(.{
        val,
        @intCast(u4, buf[0] >> 4),
    });
}

fn writeOBOA(buf: []u8, val: misc.CombinedOperands) void {
    assert(buf.len >= 1);
    buf[0] = val;
}

fn writeImmOB(buf: []u8, val: misc.OperandB) void {
    assert(buf.len >= 3);
    buf[2] = bits.concat(.{
        @truncate(u4, buf[2]),
        val,
    });
}

fn writeImmOA(buf: []u8, val: misc.OperandA) void {
    assert(buf.len >= 3);
    buf[2] = bits.concat(.{
        val,
        @intCast(u4, buf[2] >> 4),
    });
}

fn writeOpcode(buf: []u8, opcode: Opcode) void {
    assert(buf.len >= 2);
    buf[0] = @truncate(u8, opcode);
    buf[1] = @intCast(u8, opcode >> 8);
}

fn writeImm8(buf: []u8, val: u8) void {
    assert(buf.len >= 3);
    buf[2] = val;
}

fn writeImm16(buf: []u8, val: u16, offset: usize) void {
    assert(buf.len >= 4);
    buf[offset] = @truncate(u8, val);
    buf[offset + 1] = @intCast(u8, val >> 8);
}

fn writeImm32(buf: []u8, val: u32) void {
    assert(buf.len >= 6);
    buf[2] = @truncate(u8, val);
    buf[3] = @truncate(u8, val >> 8);
    buf[4] = @truncate(u8, val >> 16);
    buf[5] = @intCast(u8, val >> 24);
}

fn writeParamConstant(buf: []u8, val: i64, src: ParameterSource, opcode_base: Opcode) void {
    const uval = @bitCast(u64, val);
    switch (src) {
        .implicit => {}, // nothing to store
        .OA => writeOA(buf, @truncate(misc.OperandA, uval)),
        .OB => writeOB(buf, @truncate(misc.OperandB, uval)),
        .OB_OA => writeOBOA(buf, @truncate(misc.CombinedOperands, uval)),
        .IP_plus_2_OA => writeImmOA(buf, @truncate(misc.OperandA, uval)),
        .IP_plus_2_OB => writeImmOB(buf, @truncate(misc.OperandB, uval)),
        .IP_plus_2_8 => writeImm8(buf, @truncate(u8, uval)),
        .IP_plus_2_16 => writeImm16(buf, @truncate(u16, uval), 2),
        .IP_plus_4_16 => writeImm16(buf, @truncate(u16, uval), 4),
        .IP_plus_2_32 => writeImm32(buf, @truncate(u32, uval)),
        .opcode => writeOpcode(buf, @truncate(u16, opcode_base + uval)),
    }
}

fn encodeConstant(constant: i64, encoding: ParameterEncoding) i64 {
    if (encoding.constant_ranges.len == 0 and constant == 0) {
        return 0;
    }

    var total_values: i64 = 0;
    var raw: i64 = 0;

    var i: usize = 0;
    while (i < encoding.alt_constant_ranges.len) : (i += 1) {
        const alt = encoding.alt_constant_ranges[i];
        if (constant >= alt.min and constant <= alt.max) {
            raw = total_values + @divTrunc((constant - alt.min), encoding.constant_align);
        }
    }

    i = 0;
    while (i < encoding.constant_ranges.len) : (i += 1) {
        const range = encoding.constant_ranges[i];
        if (constant >= range.min and constant <= range.max) {
            raw = total_values + @divTrunc((constant - range.min), encoding.constant_align);
        }
        total_values += @divTrunc((range.max - range.min + encoding.constant_align), encoding.constant_align);
    }

    if (encoding.constant_reverse) {
        raw = total_values - raw - 1;
    }

    return raw;
}

pub fn encodeInstruction(insn: Instruction, encoding: InstructionEncoding, buf: []u8) ![]u8 {
    assert(insn.params.len == encoding.params.len);

    writeOpcode(buf, encoding.opcodes.min);

    var i: usize = 0;
    while (i < encoding.params.len) : (i += 1) {
        const param = insn.params[i];
        const param_encoding = encoding.params[i];
        var base = param.base;
        var offset = param.offset;
        assert(param.type.base.isImplicitlyConvertibleTo(param_encoding.type.base));
        assert(param.type.offset.isImplicitlyConvertibleTo(param_encoding.type.offset));
        if (param.type.base == .constant) {
            base = encodeConstant(base, param_encoding);
        } else if (param.type.offset == .constant) {
            offset = encodeConstant(offset, param_encoding);
        }
        writeParamConstant(buf, base, param_encoding.base_src, encoding.opcode_base);
        writeParamConstant(buf, offset, param_encoding.offset_src, encoding.opcode_base);
    }

    return buf[0..getInstructionLength(encoding)];
}

pub const Encoder = struct {
    mem: []const u8,
    remaining: []u8,
    last_instruction_encoded: []const u8,

    pub fn init(mem: []u8) Encoder {
        return .{
            .mem = mem,
            .remaining = mem,
            .last_instruction_encoded = &[_]u8{},
        };
    }

    pub fn encode(self: *Encoder, insn: Instruction, encoding: InstructionEncoding) !void {
        self.last_instruction_encoded = try encodeInstruction(insn, encoding, self.remaining);
        self.remaining = self.remaining[self.last_instruction_encoded.len..];
    }

    pub fn encodeU8(self: *Encoder, d: u8) void {
        self.remaining[0] = d;
        self.remaining = self.remaining[1..];
    }
    pub fn encodeU16(self: *Encoder, d: u16) void {
        self.remaining[0] = @truncate(u8, d);
        self.remaining[1] = @intCast(u8, d >> 8);
        self.remaining = self.remaining[2..];
    }
    pub fn encodeU32(self: *Encoder, d: u32) void {
        self.remaining[0] = @truncate(u8, d);
        self.remaining[1] = @truncate(u8, d >> 8);
        self.remaining[2] = @truncate(u8, d >> 16);
        self.remaining[3] = @intCast(u8, d >> 24);
        self.remaining = self.remaining[4..];
    }

    pub fn getData(self: *Encoder) []const u8 {
        return self.mem[0 .. self.mem.len - self.remaining.len];
    }
};

fn checkConstantInRange(constant: i64, ranges: []const ConstantRange, alignment: i64) bool {
    for (ranges) |range| {
        if (constant >= range.min and constant <= range.max) {
            var dc = constant - range.min;
            if (@rem(dc, alignment) == 0) {
                return true;
            }
        }
    }
    return false;
}

fn checkBaseExpressionTypeMatchesEncoding(constant: i64, @"type": BaseExpressionType, encoding: ParameterEncoding) bool {
    if (@"type" == .constant) {
        if (!checkConstantInRange(constant, encoding.constant_ranges, encoding.constant_align) and
            !checkConstantInRange(constant, encoding.alt_constant_ranges, encoding.constant_align))
        {
            return false;
        }
    } else if (@"type".isGPR() and (constant < encoding.min_reg or constant > encoding.max_reg)) {
        return false;
    }

    return true;
}

fn checkParamMatchesEncoding(param: Parameter, encoding: ParameterEncoding) bool {
    return param.arrow == encoding.arrow
        and param.type.base.isImplicitlyConvertibleTo(encoding.type.base)
        and param.type.offset.isImplicitlyConvertibleTo(encoding.type.offset)
        and checkBaseExpressionTypeMatchesEncoding(param.base, param.type.base, encoding)
        and checkBaseExpressionTypeMatchesEncoding(param.offset, param.type.offset, encoding);
}

pub fn checkInstructionMatchesEncoding(insn: Instruction, encoding: InstructionEncoding) bool {
    if (insn.mnemonic != encoding.mnemonic) {
        return false;
    }

    if (insn.suffix != encoding.suffix and insn.suffix != .none) {
        return false;
    }

    if (insn.params.len != encoding.params.len) {
        return false;
    }

    var encoded: [6]u8 = [_]u8{0} ** 6;
    var encoded_sources = std.EnumSet(ParameterSource){};

    var i: usize = 0;
    while (i < encoding.params.len) : (i += 1) {
        const param = insn.params[i];
        const param_encoding = encoding.params[i];
        if (!checkParamMatchesEncoding(param, param_encoding)) return false;

        // The rest of this makes sure that if the encoding has multiple parameters stored in the same
        // place, it only matches if the actual parameters are compatible with each other.
        // e.g. The instruction
        //          ADD X0, R4S -> X2
        //      should not match the encoding
        //          ADD Xa, RbS -> Xa
        //      because OA can't encode X0 and X2 at the same time.
        var base = param.base;
        var offset = param.offset;
        if (param.type.base == .constant) {
            base = encodeConstant(base, param_encoding);
        } else if (param.type.offset == .constant) {
            offset = encodeConstant(offset, param_encoding);
        }

        switch (param_encoding.base_src) {
            .implicit, .opcode => {},
            else => |src| {
                if (encoded_sources.contains(src)) {
                    var required = readParamRaw(&encoded, src, encoding.opcode_base);
                    if (required != base) {
                        return false;
                    }
                } else {
                    writeParamConstant(&encoded, base, src, encoding.opcode_base);
                    encoded_sources.insert(src);
                }
            },
        }

        switch (param_encoding.offset_src) {
            .implicit, .opcode => {},
            else => |src| {
                if (encoded_sources.contains(src)) {
                    var required = readParamRaw(&encoded, src, encoding.opcode_base);
                    if (required != offset) {
                        return false;
                    }
                } else {
                    writeParamConstant(&encoded, offset, src, encoding.opcode_base);
                    encoded_sources.insert(src);
                }
            },
        }
    }

    return true;
}

fn readOB(buf: []const u8) misc.OperandB {
    assert(buf.len >= 1);
    return @intCast(misc.OperandB, buf[0] >> 4);
}

fn readOA(buf: []const u8) misc.OperandA {
    assert(buf.len >= 1);
    return @truncate(misc.OperandA, buf[0]);
}

fn readOBOA(buf: []const u8) misc.CombinedOperands {
    assert(buf.len >= 1);
    return buf[0];
}

fn readImmOB(buf: []const u8) misc.OperandB {
    assert(buf.len >= 3);
    return @intCast(misc.OperandB, buf[2] >> 4);
}

fn readImmOA(buf: []const u8) misc.OperandA {
    assert(buf.len >= 3);
    return @truncate(misc.OperandA, buf[2]);
}

fn readOpcode(buf: []const u8) Opcode {
    assert(buf.len >= 2);
    return bits.concat(.{
        buf[0],
        buf[1],
    });
}

fn readImm8(buf: []const u8) u8 {
    assert(buf.len >= 3);
    return buf[2];
}

fn readImm16(buf: []const u8, offset: usize) u16 {
    assert(buf.len >= 4);
    return bits.concat(.{
        buf[offset],
        buf[offset + 1],
    });
}

fn readImm32(buf: []const u8) u32 {
    assert(buf.len >= 6);
    return bits.concat(.{
        buf[2],
        buf[3],
        buf[4],
        buf[5],
    });
}

fn readParamRaw(buf: []const u8, src: ParameterSource, base_opcode: Opcode) i64 {
    return switch (src) {
        .implicit => @as(i64, 0),
        .OA => readOA(buf),
        .OB => readOB(buf),
        .OB_OA => readOBOA(buf),
        .IP_plus_2_OA => readImmOA(buf),
        .IP_plus_2_OB => readImmOB(buf),
        .IP_plus_2_8 => readImm8(buf),
        .IP_plus_2_16 => readImm16(buf, 2),
        .IP_plus_4_16 => readImm16(buf, 4),
        .IP_plus_2_32 => readImm32(buf),
        .opcode => @as(i64, readOpcode(buf)) - base_opcode,
    };
}

fn readParamValue(buf: []const u8, expr_type: BaseExpressionType, src: ParameterSource, param: ParameterEncoding, base_opcode: Opcode) i64 {
    if (expr_type == .none and src == .implicit) {
        return 0;
    }
    var raw = readParamRaw(buf, src, base_opcode);
    if (expr_type == .constant) {
        raw = getParameterValue(raw, param) orelse raw;
    }
    return raw;
}

fn readParam(buf: []const u8, param: ParameterEncoding, base_opcode: Opcode) Parameter {
    return Parameter{
        .arrow = param.arrow,
        .type = param.type,
        .base = readParamValue(buf, param.type.base, param.base_src, param, base_opcode),
        .offset = readParamValue(buf, param.type.offset, param.offset_src, param, base_opcode),
    };
}

pub fn parameter(base_type: BaseExpressionType, base: i64) Parameter {
    return .{
        .type = .{ .base = base_type, .offset = .none },
        .base = base,
    };
}

pub fn parameterWithOffset(base_type: BaseExpressionType, base: i64, offset_type: BaseExpressionType, offset: i64) Parameter {
    return .{
        .type = .{ .base = base_type, .offset = offset_type },
        .base = base,
        .offset = offset,
    };
}

pub fn toParameter(base_type: BaseExpressionType, base: i64) Parameter {
    return .{
        .arrow = true,
        .type = .{ .base = base_type, .offset = .none },
        .base = base,
    };
}

pub fn toParameterWithOffset(base_type: BaseExpressionType, base: i64, offset_type: BaseExpressionType, offset: i64) Parameter {
    return .{
        .arrow = true,
        .type = .{ .base = base_type, .offset = offset_type },
        .base = base,
        .offset = offset,
    };
}

pub const InstructionEncodingParser = struct {
    reader: sx.Reader(std.io.FixedBufferStream([]const u8).Reader),
    temp_allocator: std.mem.Allocator,
    data_allocator: std.mem.Allocator,
    parse_descriptions: bool,
    first: bool = true,
    descriptions: std.StringHashMapUnmanaged(void) = .{},
    constant_ranges: deep_hash_map.DeepAutoHashMapUnmanaged([]const ConstantRange, void) = .{},
    parameter_encodings: deep_hash_map.DeepAutoHashMapUnmanaged([]const ParameterEncoding, void) = .{},
    temp_constant_ranges: std.ArrayListUnmanaged(ConstantRange) = .{},
    temp_alt_constant_ranges: std.ArrayListUnmanaged(ConstantRange) = .{},
    temp_parameter_encodings: std.ArrayListUnmanaged(ParameterEncoding) = .{},

    pub fn deinitData(self: *InstructionEncodingParser) void {
        var descriptions_iter = self.descriptions.keyIterator();
        while (descriptions_iter.next()) |k| {
            self.data_allocator.free(k.*);
        }

        var constant_ranges_iter = self.constant_ranges.keyIterator();
        while (constant_ranges_iter.next()) |k| {
            self.data_allocator.free(k.*);
        }

        var parameter_encodings_iter = self.parameter_encodings.keyIterator();
        while (parameter_encodings_iter.next()) |k| {
            self.data_allocator.free(k.*);
        }
    }

    pub fn deinit(self: *InstructionEncodingParser) void {
        self.descriptions.deinit(self.temp_allocator);
        self.constant_ranges.deinit(self.temp_allocator);
        self.parameter_encodings.deinit(self.temp_allocator);
        self.temp_constant_ranges.deinit(self.temp_allocator);
        self.temp_alt_constant_ranges.deinit(self.temp_allocator);
        self.temp_parameter_encodings.deinit(self.temp_allocator);
    }

    pub fn next(self: *InstructionEncodingParser) !?InstructionEncodingAndDescription {
        if (self.first) {
            try self.reader.requireOpen();
            self.first = false;
        }
        if (!try self.reader.open()) {
            try self.reader.requireClose();
            try self.reader.requireDone();
            return null;
        }

        const min_opcode = try self.reader.requireAnyUnsigned(Opcode, 16);
        const max_opcode = try self.reader.requireAnyUnsigned(Opcode, 16);
        const mnemonic = try self.reader.requireAnyEnum(Mnemonic);
        const suffix = try self.reader.anyEnum(MnemonicSuffix) orelse .none;
        var opcode_base = min_opcode;
        var description: []const u8 = "";

        while (true) {
            if (try self.reader.expression("desc")) {
                const temp_desc = try self.reader.requireAnyString();
                try self.reader.requireClose();
                if (self.parse_descriptions) {
                    description = try self.dedupDescription(temp_desc);
                }
            } else if (try self.reader.expression("param")) {
                const arrow = try self.reader.string("->");
                const base_type = try self.reader.requireAnyEnum(BaseExpressionType);
                const offset_type = try self.reader.anyEnum(BaseExpressionType) orelse .none;

                var param = ParameterEncoding{
                    .type = .{
                        .base = base_type,
                        .offset = offset_type,
                    },
                    .arrow = arrow,
                };

                while (true) {
                    if (try self.reader.expression("base-src")) {
                        param.base_src = try self.reader.requireAnyEnum(ParameterSource);
                    } else if (try self.reader.expression("offset-src")) {
                        param.offset_src = try self.reader.requireAnyEnum(ParameterSource);
                    } else if (try self.reader.expression("rev")) {
                        param.constant_reverse = true;
                    } else if (try self.reader.expression("reg")) {
                        param.min_reg = try self.reader.requireAnyInt(misc.RegisterIndex, 10);
                        param.max_reg = try self.reader.requireAnyInt(misc.RegisterIndex, 10);
                    } else if (try self.reader.expression("range")) {
                        try self.temp_constant_ranges.append(self.temp_allocator, .{
                            .min = try self.reader.requireAnyInt(i64, 10),
                            .max = try self.reader.requireAnyInt(i64, 10),
                        });
                    } else if (try self.reader.expression("alt-range")) {
                        try self.temp_alt_constant_ranges.append(self.temp_allocator, .{
                            .min = try self.reader.requireAnyInt(i64, 10),
                            .max = try self.reader.requireAnyInt(i64, 10),
                        });
                    } else if (try self.reader.expression("align")) {
                        param.constant_align = try self.reader.requireAnyInt(u3, 10);
                    } else break;

                    try self.reader.requireClose();
                }

                try self.reader.requireClose();

                if (self.temp_constant_ranges.items.len > 0) {
                    param.constant_ranges = try self.dedupConstantRanges(self.temp_constant_ranges.items);
                }

                if (self.temp_alt_constant_ranges.items.len > 0) {
                    param.alt_constant_ranges = try self.dedupConstantRanges(self.temp_alt_constant_ranges.items);
                }

                self.temp_constant_ranges.clearRetainingCapacity();
                self.temp_alt_constant_ranges.clearRetainingCapacity();

                try self.temp_parameter_encodings.append(self.temp_allocator, param);
            } else if (try self.reader.expression("opcode-base")) {
                opcode_base = try self.reader.requireAnyUnsigned(Opcode, 16);
                try self.reader.requireClose();
            } else break;
        }
        try self.reader.requireClose();

        const params = try self.dedupParameterEncodings(self.temp_parameter_encodings.items);
        self.temp_parameter_encodings.clearRetainingCapacity();

        return InstructionEncodingAndDescription{
            .encoding = .{
                .mnemonic = mnemonic,
                .suffix = suffix,
                .params = params,
                .opcode_base = opcode_base,
                .opcodes = .{
                    .min = min_opcode,
                    .max = max_opcode,
                },
            },
            .desc = description,
        };
    }

    fn dedupDescription(self: *InstructionEncodingParser, desc: []const u8) ![]const u8 {
        if (self.descriptions.getKey(desc)) |deduped| return deduped;
        try self.descriptions.ensureUnusedCapacity(self.temp_allocator, 1);
        const to_insert = try self.data_allocator.dupe(u8, desc);
        self.descriptions.putAssumeCapacity(to_insert, {});
        return to_insert;
    }

    fn dedupConstantRanges(self: *InstructionEncodingParser, ranges: []const ConstantRange) ![]const ConstantRange {
        if (self.constant_ranges.getKey(ranges)) |deduped| return deduped;
        try self.constant_ranges.ensureUnusedCapacity(self.temp_allocator, 1);
        const to_insert = try self.data_allocator.dupe(ConstantRange, ranges);
        self.constant_ranges.putAssumeCapacity(to_insert, {});
        return to_insert;
    }

    fn dedupParameterEncodings(self: *InstructionEncodingParser, params: []const ParameterEncoding) ![]const ParameterEncoding {
        if (self.parameter_encodings.getKey(params)) |deduped| return deduped;
        try self.parameter_encodings.ensureUnusedCapacity(self.temp_allocator, 1);
        const to_insert = try self.data_allocator.dupe(ParameterEncoding, params);
        self.parameter_encodings.putAssumeCapacity(to_insert, {});
        return to_insert;
    }
};

pub const EncoderDatabase = struct {
    mnemonic_to_encoding: std.EnumMap(Mnemonic, []InstructionEncoding) = .{},

    pub fn init(arena: std.mem.Allocator, data: []const u8, temp: std.mem.Allocator) !EncoderDatabase {
        var stream = std.io.fixedBufferStream(data);
        var parser = InstructionEncodingParser{
            .reader = sx.reader(temp, stream.reader()),
            .temp_allocator = temp,
            .data_allocator = arena,
            .parse_descriptions = false,
        };
        defer parser.deinit();
        defer parser.reader.deinit();
        errdefer parser.deinitData();

        var temp_map = std.EnumMap(Mnemonic, std.ArrayListUnmanaged(InstructionEncoding)) {};
        defer {
            var iter = temp_map.iterator();
            while (iter.next()) |entry| {
                entry.value.deinit(temp);
            }
        }

        while (parser.next() catch |err| {
            if (err == error.SExpressionSyntaxError) {
                var stderr = std.io.getStdErr().writer();
                const context = parser.reader.getNextTokenContext() catch return err;
                stderr.writeAll("Syntax error in instruction encoding data:\n") catch return err;
                context.printForString(data, stderr, 150) catch return err;
            }
            return err;
        }) |encoding_and_desc| {
            const ie = encoding_and_desc.encoding;

            var list = temp_map.getPtr(ie.mnemonic) orelse ptr: {
                temp_map.put(ie.mnemonic, .{});
                break :ptr temp_map.getPtrAssertContains(ie.mnemonic);
            };

            try list.append(temp, ie);
        }

        var self = EncoderDatabase{};
        var iter = temp_map.iterator();
        while (iter.next()) |entry| {
            const encodings = try arena.dupe(InstructionEncoding, entry.value.items);
            self.mnemonic_to_encoding.put(entry.key, encodings);
        }

        return self;
    }

    pub fn getMatchingEncodings(self: EncoderDatabase, insn: Instruction) InstructionEncodingIterator {
        const encodings = self.mnemonic_to_encoding.get(insn.mnemonic) orelse &[_]InstructionEncoding{};
        return InstructionEncodingIterator.init(insn, encodings);
    }
};

pub const InstructionEncodingIterator = struct {
    insn: Instruction,
    remaining: []const InstructionEncoding,

    pub fn init(insn: Instruction, possible_encodings: []const InstructionEncoding) InstructionEncodingIterator {
        return .{
            .insn = insn,
            .remaining = possible_encodings,
        };
    }

    pub fn next(self: *InstructionEncodingIterator) ?InstructionEncoding {
        while (self.remaining.len > 0) {
            const n = self.remaining[0];
            self.remaining = self.remaining[1..];
            if (checkInstructionMatchesEncoding(self.insn, n)) {
                return n;
            }
        }
        return null;
    }
};

pub const DecoderDatabase = struct {
    opcode_to_desc_8b: std.AutoHashMapUnmanaged(u8, []const u8),
    opcode_to_desc_12b: std.AutoHashMapUnmanaged(u12, []const u8),
    opcode_to_desc_16b: std.AutoHashMapUnmanaged(u16, []const u8),

    opcode_to_encoding_8b: std.AutoHashMapUnmanaged(u8, InstructionEncoding),
    opcode_to_encoding_12b: std.AutoHashMapUnmanaged(u12, InstructionEncoding),
    opcode_to_encoding_16b: std.AutoHashMapUnmanaged(u16, InstructionEncoding),

    pub fn init(arena: std.mem.Allocator, data: []const u8, temp: std.mem.Allocator) !DecoderDatabase {
        var stream = std.io.fixedBufferStream(data);
        var parser = InstructionEncodingParser{
            .reader = sx.reader(temp, stream.reader()),
            .temp_allocator = temp,
            .data_allocator = arena,
            .parse_descriptions = true,
        };
        defer parser.deinit();
        defer parser.reader.deinit();
        errdefer parser.deinitData();

        var temp_desc_8b = std.AutoHashMap(u8, []const u8).init(temp);
        var temp_desc_12b = std.AutoHashMap(u12, []const u8).init(temp);
        var temp_desc_16b = std.AutoHashMap(u16, []const u8).init(temp);
        defer temp_desc_8b.deinit();
        defer temp_desc_12b.deinit();
        defer temp_desc_16b.deinit();

        var temp_enc_8b = std.AutoHashMap(u8, InstructionEncoding).init(temp);
        var temp_enc_12b = std.AutoHashMap(u12, InstructionEncoding).init(temp);
        var temp_enc_16b = std.AutoHashMap(u16, InstructionEncoding).init(temp);
        defer temp_enc_8b.deinit();
        defer temp_enc_12b.deinit();
        defer temp_enc_16b.deinit();

        while (parser.next() catch |err| {
            if (err == error.SExpressionSyntaxError) {
                var stderr = std.io.getStdErr().writer();
                const context = parser.reader.getNextTokenContext() catch return err;
                stderr.writeAll("Syntax error in instruction encoding data:\n") catch return err;
                context.printForString(data, stderr, 150) catch return err;
            }
            return err;
        }) |encoding_and_desc| {
            const min_opcode = encoding_and_desc.encoding.opcodes.min;
            const max_opcode = encoding_and_desc.encoding.opcodes.max;

            if (encoding_and_desc.desc.len > 0) {
                const desc = encoding_and_desc.desc;
                var opcode: u32 = min_opcode;
                while (opcode <= max_opcode and (opcode & 0xF) != 0) : (opcode += 1) {
                    try temp_desc_16b.putNoClobber(@intCast(u16, opcode), desc);
                }
                while (opcode + 0xF <= max_opcode and (opcode & 0xFF) != 0) : (opcode += 0x10) {
                    try temp_desc_12b.putNoClobber(@intCast(u12, opcode >> 4), desc);
                }
                while (opcode + 0xFF <= max_opcode) : (opcode += 0x100) {
                    try temp_desc_8b.putNoClobber(@intCast(u8, opcode >> 8), desc);
                }
                while (opcode + 0xF <= max_opcode) : (opcode += 0x10) {
                    try temp_desc_12b.putNoClobber(@intCast(u12, opcode >> 4), desc);
                }
                while (opcode <= max_opcode) : (opcode += 1) {
                    try temp_desc_16b.putNoClobber(@intCast(u16, opcode), desc);
                }
            }

            var opcode: u32 = min_opcode;
            while (opcode <= max_opcode and (opcode & 0xF) != 0) : (opcode += 1) {
                try temp_enc_16b.putNoClobber(@intCast(u16, opcode), encoding_and_desc.encoding);
            }
            while (opcode + 0xF <= max_opcode and (opcode & 0xFF) != 0) : (opcode += 0x10) {
                try temp_enc_12b.putNoClobber(@intCast(u12, opcode >> 4), encoding_and_desc.encoding);
            }
            while (opcode + 0xFF <= max_opcode) : (opcode += 0x100) {
                try temp_enc_8b.putNoClobber(@intCast(u8, opcode >> 8), encoding_and_desc.encoding);
            }
            while (opcode + 0xF <= max_opcode) : (opcode += 0x10) {
                try temp_enc_12b.putNoClobber(@intCast(u12, opcode >> 4), encoding_and_desc.encoding);
            }
            while (opcode <= max_opcode) : (opcode += 1) {
                try temp_enc_16b.putNoClobber(@intCast(u16, opcode), encoding_and_desc.encoding);
            }
        }

        return DecoderDatabase{
            .opcode_to_desc_8b = try temp_desc_8b.unmanaged.clone(arena),
            .opcode_to_desc_12b = try temp_desc_12b.unmanaged.clone(arena),
            .opcode_to_desc_16b = try temp_desc_16b.unmanaged.clone(arena),
            .opcode_to_encoding_8b = try temp_enc_8b.unmanaged.clone(arena),
            .opcode_to_encoding_12b = try temp_enc_12b.unmanaged.clone(arena),
            .opcode_to_encoding_16b = try temp_enc_16b.unmanaged.clone(arena),
        };
    }

    pub fn getDescriptionForOpcode(self: DecoderDatabase, opcode: Opcode) ?[]const u8 {
        const op8 = @intCast(u8, opcode >> 8);
        if (self.opcode_to_desc_8b.get(op8)) |encoding| return encoding;

        const op12 = @intCast(u12, opcode >> 4);
        if (self.opcode_to_desc_12b.get(op12)) |encoding| return encoding;

        return self.opcode_to_desc_16b.get(opcode);
    }

    pub fn getEncodingForOpcode(self: DecoderDatabase, opcode: Opcode) ?InstructionEncoding {
        const op8 = @intCast(u8, opcode >> 8);
        if (self.opcode_to_encoding_8b.get(op8)) |encoding| return encoding;

        const op12 = @intCast(u12, opcode >> 4);
        if (self.opcode_to_encoding_12b.get(op12)) |encoding| return encoding;

        return self.opcode_to_encoding_16b.get(opcode);
    }

    pub fn extractEncoding(self: DecoderDatabase, encoded_instruction: []const u8) ?InstructionEncoding {
        return self.getEncodingForOpcode(readOpcode(encoded_instruction));
    }

};

pub const Decoder = struct {
    db: *const DecoderDatabase,
    alloc: std.mem.Allocator,
    remaining: []const u8,
    last_instruction: []const u8,

    pub fn init(db: *const DecoderDatabase, alloc: std.mem.Allocator, program_memory: []const u8) Decoder {
        return .{
            .db = db,
            .alloc = alloc,
            .remaining = program_memory,
            .last_instruction = &.{},
        };
    }

    pub fn decode(self: *Decoder) !Instruction {
        const encoding = self.db.extractEncoding(self.remaining) orelse return error.InvalidInstruction;
        const params = try self.alloc.alloc(Parameter, encoding.params.len);
        errdefer self.alloc.free(params);

        var i: usize = 0;
        while (i < encoding.params.len) : (i += 1) {
            params[i] = readParam(self.remaining, encoding.params[i], encoding.opcode_base);
        }

        const insn = Instruction{
            .mnemonic = encoding.mnemonic,
            .suffix = encoding.suffix,
            .params = params,
        };

        const length = getInstructionLength(encoding);

        self.last_instruction = self.remaining[0..length];
        self.remaining = self.remaining[length..];

        return insn;
    }
};

// Decodes a single instruction; use Decoder directly to decode multiple instructions
pub fn decodeInstruction(db: *const DecoderDatabase, alloc: std.mem.Allocator, program_memory: []const u8) !Instruction {
    var decoder = Decoder.init(db, alloc, program_memory);
    return try decoder.decode();
}

pub fn testIdempotence(ddb: *const DecoderDatabase, edb: *const EncoderDatabase, expected_encodings: usize, insn: Instruction) !void {
    var encoding_count: usize = 0;
    var iter = edb.getMatchingEncodings(insn);
    while (iter.next()) |encoding| {
        if (encoding.suffix != insn.suffix) {
            continue;
        }

        var buf: [6]u8 = [_]u8{0} ** 6;
        var encoded = try encodeInstruction(insn, encoding, &buf);

        var temp: [10 * @sizeOf(Parameter)]u8 = undefined;
        var alloc = std.heap.FixedBufferAllocator.init(&temp);
        var decoder = Decoder.init(ddb, alloc.allocator(), encoded);
        const decoded = try decoder.decode();

        var decoded_encoding = ddb.extractEncoding(decoder.last_instruction) orelse return error.InvalidInstruction;

        if (!insn.eql(decoded)) {
            const stderr = std.io.getStdErr().writer();
            try stderr.writeAll("Roundtrip testing of instruction: ");
            try insn.print(stderr);
            try stderr.writeAll("\nWith encoding: ");
            try encoding.print(stderr);
            try stderr.writeAll("\nDecoded instruction: ");
            try decoded.print(stderr);
            try stderr.writeAll("\nFrom encoding: ");
            try decoded_encoding.print(stderr);
            try stderr.writeAll("\n");
            return error.TestExpectedError;
        }
        encoding_count += 1;
    }

    std.testing.expectEqual(expected_encodings, encoding_count) catch {
        const stderr = std.io.getStdErr().writer();
        try stderr.writeAll("Encodings for instruction: ");
        try insn.print(stderr);
        try stderr.writeAll("\n");
        iter = edb.getMatchingEncodings(insn);
        while (iter.next()) |encoding| {
            if (encoding.suffix != insn.suffix) {
                continue;
            }

            try encoding.print(stderr);
            try stderr.writeAll("\n");
        }
        return error.TestExpectedError;
    };
}