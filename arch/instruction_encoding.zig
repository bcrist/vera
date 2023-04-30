const std = @import("std");
const bits = @import("bits");
const uc = @import("microcode");
const misc = @import("misc");
const ControlSignals = @import("ControlSignals");

const assert = std.debug.assert;

// TODO maybe some of this could be moved to a separate file?  isa_types.zig?

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

pub const ExpressionType = union(enum) {
    unknown,    // Used internally by the assembler; never part of any instruction encodings
    poison,     // Used internally by the assembler; never part of any instruction encodings
    symbol_def, // Used internally by the assembler; never part of any instruction encodings

    constant,
    reg8: IndexedRegister,
    reg16: IndexedRegister,
    reg32: IndexedRegister,
    sr: SpecialRegister,
    raw_base_offset: BaseOffsetType,
    data_address: BaseOffsetType,
    insn_address: BaseOffsetType,
    stack_address: BaseOffsetType,

    pub fn reg(comptime width: u6, index: u4, signedness: ?std.builtin.Signedness) ExpressionType {
        const field = comptime switch (width) {
            8 => "reg8",
            16 => "reg16",
            32 => "reg32",
            else => @compileError("Width must be 8, 16, or 32"),
        };
        return @unionInit(ExpressionType, field, .{
            .index = index,
            .signedness = signedness,
        });
    }

    pub fn rawBaseOffset(base: ExpressionType, offset: ExpressionType) !ExpressionType {
        return .{ .raw_base_offset = .{
            .base = try BaseOffsetType.InnerType.fromExpressionType(base),
            .offset = try BaseOffsetType.InnerType.fromExpressionType(offset),
        }};
    }

    pub fn address(comptime space: AddressSpace, base: ExpressionType, offset: ExpressionType) !ExpressionType {
        return @unionInit(ExpressionType, @tagName(space) ++ "_address", .{
            .base = try BaseOffsetType.InnerType.fromExpressionType(base),
            .offset = try BaseOffsetType.InnerType.fromExpressionType(offset),
        });
    }

    pub fn absoluteAddress(comptime space: AddressSpace) ExpressionType {
        return @unionInit(ExpressionType, @tagName(space) ++ "_address", .{
            .base = .{ .constant = {} },
            .offset = .{ .constant = {} }, // offset is ignored when both base and offset are constant
        });
    }

    pub fn relativeAddress(comptime space: AddressSpace, base: ExpressionType) !ExpressionType {
        return @unionInit(ExpressionType, @tagName(space) ++ "_address", .{
            .base = try BaseOffsetType.InnerType.fromExpressionType(base),
            .offset = .{ .constant = {} },
        });
    }
};

pub const ExpressionTypeBuilder = struct {
    invalid: bool = false,
    unknown: bool = false,
    poison: bool = false,
    address_space: ?AddressSpace = null,
    a: InnerType = .{ .none = {} },
    b: InnerType = .{ .none = {} },

    const InnerType = BaseOffsetType.InnerType;

    pub fn add(self: *ExpressionTypeBuilder, t: ExpressionType) void {
        switch (t) {
            .unknown => self.unknown = true,
            .poison => self.poison = true,
            .symbol_def => self.invalid = true,
            .raw_base_offset, .data_address, .insn_address, .stack_address => |bo| {
                self.tryAddInnerType(bo.base);
                self.tryAddInnerType(bo.offset);
            },
            else => self.tryAddType(t),
        }

        if (getAddressSpace(t)) |as| {
            if (self.address_space) |eas| {
                if (eas != as) self.invalid = true;
            }
            self.address_space = as;
        }
    }

    pub fn subtract(self: *ExpressionTypeBuilder, t: ExpressionType) void {
        switch (t) {
            .unknown => self.unknown = true,
            .poison => self.poison = true,
            .symbol_def => self.invalid = true,
            .raw_base_offset, .data_address, .insn_address, .stack_address => |bo| {
                self.trySubtractInnerType(bo.base);
                self.trySubtractInnerType(bo.offset);
            },
            else => self.trySubtractType(t),
        }

        if (getAddressSpace(t)) |as| {
            if (self.address_space) |eas| {
                if (eas != as) {
                    self.invalid = true;
                } else {
                    self.address_space = null;
                }
            } else {
                self.invalid = true;
            }
        }
    }

    fn tryAddType(self: *ExpressionTypeBuilder, t: ExpressionType) void {
        self.tryAddInnerType(InnerType.fromExpressionType(t) catch return);
    }
    fn tryAddInnerType(self: *ExpressionTypeBuilder, inner: InnerType) void {
        if (getInnerTypeSum(self.a, inner)) |sum| {
            self.a = sum;
        } else if (getInnerTypeSum(self.b, inner)) |sum| {
            self.b = sum;
        } else {
            self.invalid = true;
        }
    }

    fn trySubtractType(self: *ExpressionTypeBuilder, t: ExpressionType) void {
        self.trySubtractInnerType(InnerType.fromExpressionType(t) catch return);
    }
    fn trySubtractInnerType(self: *ExpressionTypeBuilder, inner: InnerType) void {
        if (getInnerTypeDifference(self.a, inner)) |diff| {
            self.a = diff;
        } else if (getInnerTypeDifference(self.b, inner)) |diff| {
            self.b = diff;
        } else {
            self.invalid = true;
        }
    }

    fn getInnerTypeSum(a: InnerType, b: InnerType) ?InnerType {
        if (a == .none) return b;
        if (b == .none) return a;
        if (a == .constant and b == .constant) return a;
        return null;
    }

    fn getInnerTypeDifference(a: InnerType, b: InnerType) ?InnerType {
        // constants can always be subtracted, which is still a constant (we don't know if they're the same value)
        if (a == .constant and b == .constant) return a;

        // registers can "cancel out"
        if (std.meta.eql(a, b)) return .{ .none = {} };

        // anything else is unrepresentable (e.g. negative registers)
        return null;
    }

    fn getAddressSpace(tag: std.meta.Tag(ExpressionType)) ?AddressSpace {
        return switch (tag) {
            .data_address => .data,
            .insn_address => .insn,
            .stack_address => .stack,
            else => null,
        };
    }

    fn getPriority(t: InnerType) u8 {
        // higher priority will be placed in the base slot, lower in offset
        return switch (t) {
            .none => 0,
            .constant => 1,
            .reg8 => |r| 0x10 + @as(u8, r.index),
            .reg16 => |r| 0x20 + @as(u8, r.index),
            .reg32 => |r| 0x30 + @as(u8, r.index),
            .sr => |r| @intCast(u8, 0x40) + @enumToInt(r),
        };
    }

    pub fn build(self: *ExpressionTypeBuilder) !ExpressionType {
        if (self.invalid) return error.InvalidType;
        if (self.poison) return .{ .poison = {} };
        if (self.unknown) return .{ .unknown = {} };

        if (getPriority(self.a) < getPriority(self.b)) {
            const temp = self.a;
            self.a = self.b;
            self.b = temp;
        }

        const bo = BaseOffsetType{
            .base = self.a,
            .offset = self.b,
        };

        if (self.address_space) |as| {
            return switch (as) {
                .data => .{ .data_address = bo },
                .insn => .{ .insn_address = bo },
                .stack => .{ .stack_address = bo },
            };
        }

        if (bo.offset != .none or bo.base == .none) {
            return .{ .raw_base_offset = bo };
        }

        return InnerType.toExpressionType(bo.base);
    }
};

pub const BaseOffsetType = struct {
    base: InnerType,
    offset: InnerType,

    pub fn init(left: ?ExpressionType, right: ?ExpressionType) !BaseOffsetType {
        return BaseOffsetType{
            .base = try InnerType.fromExpressionType(left),
            .offset = try InnerType.fromExpressionType(right),
        };
    }

    pub const InnerType = union(enum) {
        none,
        constant,
        reg8: IndexedRegister,
        reg16: IndexedRegister,
        reg32: IndexedRegister,
        sr: SpecialRegister,

        pub fn fromExpressionType(maybe_expr_type: ?ExpressionType) !InnerType {
            return if (maybe_expr_type) |t| switch (t) {
                .constant => .{ .constant = {} },
                .reg8 => |r| .{ .reg8 = r },
                .reg16 => |r| .{ .reg16 = r },
                .reg32 => |r| .{ .reg32 = r },
                .sr => |r| .{ .sr = r },
                else => return error.InvalidType,
            } else .{ .none = {} };
        }

        pub fn toExpressionType(self: InnerType) ExpressionType {
            return switch (self) {
                .none, .constant => .{ .constant = {} },
                .reg8 => |r| .{ .reg8 = r },
                .reg16 => |r| .{ .reg16 = r },
                .reg32 => |r| .{ .reg32 = r },
                .sr => |r| .{ .sr = r },
            };
        }
    };
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

pub const SpecialRegister = enum {
    ip,
    sp,
    rp,
    bp,
    uxp,
    kxp,
    asn,
    stat,
};

pub const AddressSpace = enum {
    data,
    insn,
    stack,
};

pub const ParameterSource = enum {
    implicit, // Parameter is not directly represented in the instruction
    opcode, // Like implicit, but used when there are multiple opcodes in the encoding, and the parameter value is linearly related to the opcode
    OA, // The initial operand A
    OB, // The initial operand B
    OB_OA, // The combination of initial operands A and B (OB is MSB, OA is LSB)
    IP_plus_2_OA, // operand A, after it has been loaded from IP+2
    IP_plus_2_OB, // operand B, after it has been loaded from IP+2
    IP_plus_2_8, // the byte at IP+2
    IP_plus_2_16, // the word at IP+2
    IP_plus_2_32, // the 2 words at IP+2 and IP+4
    IP_plus_4_16, // the word at IP+4
};

fn getMinLengthForParamSource(src: ParameterSource) u3 {
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

pub const ParameterEncodingBaseType = union(enum) {
    none,
    constant,
    reg8: IndexedRegisterRange,
    reg16: IndexedRegisterRange,
    reg32: IndexedRegisterRange,
    sr: SpecialRegister,

    pub fn print(self: ParameterEncodingBaseType, writer: anytype) !void {
        switch (self) {
            .none, .constant => {
                try writer.writeAll(@tagName(self));
            },
            .reg8, .reg16, .reg32 => |irr| {
                try writer.writeAll(switch (self) {
                    .reg8 => "B",
                    .reg16 => "R",
                    .reg32 => "X",
                    else => unreachable,
                });
                if (irr.min == irr.max) {
                    try writer.print("{}", .{ irr.min });
                } else {
                    try writer.print("{}-{}", .{ irr.min, irr.max });
                }
                if (irr.signedness) |s| {
                    try writer.writeAll(switch (s) {
                        .unsigned => "U",
                        .signed => "S",
                    });
                }
            },
            .sr => |sr| {
                try writer.writeAll(@tagName(sr));
            },
        }
    }
};

pub const ParameterEncoding = struct {
    address_space: ?AddressSpace = null,
    base: ParameterEncodingBaseType = .{ .none = {} },
    offset: ParameterEncodingBaseType = .{ .none = {} },
    base_src: ParameterSource = .implicit,
    offset_src: ParameterSource = .implicit,
    arrow: bool = false,
    constant_reverse: bool = false, // store constant as `N - value` instead of `value`
    constant_align: u3 = 1,
    constant_ranges: []const ConstantRange = &.{},
    alt_constant_ranges: []const ConstantRange = &.{},
    // TODO allow restricting encoding to matching a subset of the full ConstantRanges that can be encoded.
    // that way e.g. in addition to C .imm16u -> RaU and C .imm16s -> RaS, we can also have C .imm15u -> Ra.
    // So the .signed/.unsigned suffix can be omitted for numbers that are the same regardless of
    // signed/unsigned interpretation

    pub fn print(self: ParameterEncoding, writer: anytype) !void {
        if (self.arrow) {
            try writer.writeAll("-> ");
        }

        if (self.address_space) |as| {
            try writer.print("[{s}] ", .{ @tagName(as) });
        }

        try self.base.print(writer);
        if (self.base_src != .implicit) {
            try writer.print(" ({s})", .{ @tagName(self.base_src) });
        }

        if (self.offset != .none or self.offset_src != .implicit) {
            try writer.writeAll(" + ");
            try self.offset.print(writer);
            if (self.offset_src != .implicit) {
                try writer.print(" ({s})", .{ @tagName(self.offset_src) });
            }
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

    pub fn read(self: ParameterEncoding, buf: []const u8, base_opcode: Opcode) Parameter {
        var param = Parameter{
            .arrow = self.arrow,
            .expr_type = undefined,
            .constant = 0,
        };
        const base = readParamRaw(buf, self.base_src, base_opcode);
        const offset = readParamRaw(buf, self.offset_src, base_opcode);

        if (self.address_space != null or self.offset != .none) {
            var bo: BaseOffsetType = .{
                .base = switch (self.base) {
                    .none => .{ .constant = {} },
                    .sr => |sr| .{ .sr = sr },
                    .constant => blk: {
                        param.constant = self.decodeConstant(base) orelse base;
                        break :blk .{ .constant = {} };
                    },
                    .reg8 => |re| .{ .reg8 = .{
                        .index = if (self.base_src == .implicit) re.min else base,
                        .signedness = re.signedness,
                    }},
                    .reg16 => |re| .{ .reg16 = .{
                        .index = if (self.base_src == .implicit) re.min else base,
                        .signedness = re.signedness,
                    }},
                    .reg32 => |re| .{ .reg32 = .{
                        .index = if (self.base_src == .implicit) re.min else base,
                        .signedness = re.signedness,
                    }},
                },
                .offset = switch (self.offset) {
                    .none => .{ .constant = {} },
                    .sr => |sr| .{ .sr = sr },
                    .constant => blk: {
                        param.constant = self.decodeConstant(offset) orelse offset;
                        break :blk .{ .constant = {} };
                    },
                    .reg8 => |re| .{ .reg8 = .{
                        .index = if (self.offset_src == .implicit) re.min else offset,
                        .signedness = re.signedness,
                    }},
                    .reg16 => |re| .{ .reg16 = .{
                        .index = if (self.offset_src == .implicit) re.min else offset,
                        .signedness = re.signedness,
                    }},
                    .reg32 => |re| .{ .reg32 = .{
                        .index = if (self.offset_src == .implicit) re.min else offset,
                        .signedness = re.signedness,
                    }},
                },
            };
            param.expr_type = if (self.address_space) |as| switch (as) {
                .data => .{ .data_address = bo },
                .insn => .{ .insn_address = bo },
                .stack => .{ .stack_address = bo },
            } else .{ .raw_base_offset = bo };
        } else switch (self.base) {
            .none => {
                param.expr_type = .{ .constant = {} };
            },
            .constant => {
                param.constant = self.decodeConstant(base) orelse base;
                param.expr_type = .{ .constant = {} };
            },
            .reg8 => |re| {
                param.expr_type = .{ .reg8 = .{
                    .index = if (self.base_src == .implicit) re.min else base,
                    .signedness = re.signedness,
                }};
            },
            .reg16 => |re| {
                param.expr_type = .{ .reg16 = .{
                    .index = if (self.base_src == .implicit) re.min else base,
                    .signedness = re.signedness,
                }};
            },
            .reg32 => |re| {
                param.expr_type = .{ .reg32 = .{
                    .index = if (self.base_src == .implicit) re.min else base,
                    .signedness = re.signedness,
                }};
            },
            .sr => |sr| {
                param.expr_type = .{ .sr = sr };
            },
        }

        return param;
    }

    pub fn getBaseValue(self: ParameterEncoding, param: Parameter) i64 {
        return switch (self.base) {
            .none, .sr => 0,
            .constant => self.encodeConstant(param.constant),
            .reg8, .reg16, .reg32 => switch (param.expr_type) {
                .unknown, .poison, .symbol_def, .constant, .sr => unreachable,
                .reg8, .reg16, .reg32 => |reg| reg.index,
                .raw_base_offset, .data_address, .insn_address, .stack_address => |info| switch (info.base) {
                    .none, .constant, .sr => unreachable,
                    .reg8, .reg16, .reg32 => |reg| reg.index,
                },
            },
        };
    }

    pub fn getOffsetValue(self: ParameterEncoding, param: Parameter) i64 {
        return switch (self.offset) {
            .none, .sr => 0,
            .constant => self.encodeConstant(param.constant),
            .reg8, .reg16, .reg32 => switch (param.expr_type) {
                .unknown, .poison, .symbol_def, .constant, .sr, .reg8, .reg16, .reg32 => unreachable,
                .raw_base_offset, .data_address, .insn_address, .stack_address => |info| switch (info.offset) {
                    .none, .constant, .sr => unreachable,
                    .reg8, .reg16, .reg32 => |reg| reg.index,
                },
            },
        };
    }

    // Transforms a constant value to get the raw number that's embedded in the instruction bytes
    // to represent it (but the exact location depends on base_src/offset_src)
    fn encodeConstant(self: ParameterEncoding, constant: i64) i64 {
        if (self.constant_ranges.len == 0 and constant == 0) {
            return 0;
        }

        var total_values: i64 = 0;
        var raw: i64 = 0;

        var i: usize = 0;
        while (i < self.alt_constant_ranges.len) : (i += 1) {
            const alt = self.alt_constant_ranges[i];
            if (constant >= alt.min and constant <= alt.max) {
                raw = total_values + @divTrunc((constant - alt.min), self.constant_align);
            }
            total_values += @divTrunc((alt.max - alt.min + self.constant_align), self.constant_align);
        }

        total_values = 0;

        i = 0;
        while (i < self.constant_ranges.len) : (i += 1) {
            const range = self.constant_ranges[i];
            if (constant >= range.min and constant <= range.max) {
                raw = total_values + @divTrunc((constant - range.min), self.constant_align);
            }
            total_values += @divTrunc((range.max - range.min + self.constant_align), self.constant_align);
        }

        if (self.constant_reverse) {
            raw = total_values - raw - 1;
        }

        return raw;
    }

    // The inverse of encodeConstant
    fn decodeConstant(self: ParameterEncoding, raw_value: i64) ?i64 {
        var aligned = raw_value * self.constant_align;

        var total_values: i64 = 0;
        for (self.constant_ranges) |range| {
            total_values += range.max - range.min + 1;
        }

        if (self.constant_reverse) {
            aligned = total_values - aligned - 1;
        }

        for (self.constant_ranges) |range| {
            const values = range.max - range.min + 1;
            if (aligned < values) {
                return range.min + aligned;
            }
            aligned -= values;
        }

        if (self.constant_ranges.len == 0 and aligned == 0) {
            return 0;
        }

        return null;
    }

};

pub const InstructionEncoding = struct {
    mnemonic: Mnemonic,
    suffix: MnemonicSuffix,
    params: []const ParameterEncoding,
    opcode_base: Opcode,
    opcodes: OpcodeRange,

    pub fn print(self: InstructionEncoding, writer: anytype, indent: []const u8) !void {
        if (self.opcodes.min == self.opcodes.max) {
            try writer.print("{s}{X:0>4} ", .{ indent, self.opcodes.min });
        } else {
            try writer.print("{s}{X:0>4}-{X:0>4} ", .{ indent, self.opcodes.min, self.opcodes.max });
        }

        if (self.opcode_base != self.opcodes.min) {
            try writer.print("base {X:0>4} ", .{ self.opcode_base });
        }

        try writer.writeAll(@tagName(self.mnemonic));
        if (self.suffix != .none) {
            try writer.print(".{s}", .{ @tagName(self.suffix) });
        }
        try writer.writeAll("\n");
        for (self.params) |param| {
            try writer.print("{s}   ", .{ indent });
            try param.print(writer);
            try writer.writeAll("\n");
        }
    }
};
pub const InstructionEncodingAndDescription = struct {
    encoding: InstructionEncoding,
    desc: []const u8,
};

pub fn eql(a: InstructionEncoding, b: InstructionEncoding) bool {
    // Note intentionally not checking opcodes.min or opcodes.max
    return a.mnemonic == b.mnemonic
        and a.suffix == b.suffix
        and a.opcode_base == b.opcode_base
        and std.meta.eql(a.params, b.params)
        ;
}

fn tryComptimeRegisterParameterEncoding(comptime name: []const u8) ?ParameterEncoding {
    comptime {
        if (name.len < 2) return null;

        var encoding = ParameterEncoding{};

        switch (name[0]) {
            'B' => {
                if (std.mem.eql(u8, name, "BP")) {
                    encoding.base = .{ .sr = .bp };
                    return encoding;
                } else {
                    encoding.base = .{ .reg8 = .{} };
                }
            },
            'R' => {
                if (std.mem.eql(u8, name, "RP")) {
                    encoding.base = .{ .sr = .rp };
                    return encoding;
                } else {
                    encoding.base = .{ .reg16 = .{} };
                }
            },
            'X' => {
                encoding.base = .{ .reg32 = .{} };
            },
            'I' => {
                if (std.mem.eql(u8, name, "IP")) {
                    encoding.base = .{ .sr = .ip };
                    return encoding;
                } else return null;
            },
            'S' => {
                if (std.mem.eql(u8, name, "SP")) {
                    encoding.base = .{ .sr = .sp };
                    return encoding;
                } else if (std.mem.eql(u8, name, "STAT")) {
                    encoding.base = .{ .sr = .stat };
                    return encoding;
                } else return null;
            },
            else => {
                if (std.mem.eql(u8, name, "ASN")) {
                    encoding.base = .{ .sr = .asn };
                    return encoding;
                } else if (std.mem.eql(u8, name, "KXP")) {
                    encoding.base = .{ .sr = .kxp };
                    return encoding;
                } else if (std.mem.eql(u8, name, "UXP")) {
                    encoding.base = .{ .sr = .uxp };
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
                switch (encoding.base) {
                    // These can be combined when this is fixed:
                    // https://github.com/ziglang/zig/issues/15504
                    .reg8 => |*irr| {
                        irr.min = reg;
                        irr.max = reg;
                    },
                    .reg16 => |*irr| {
                        irr.min = reg;
                        irr.max = reg;
                    },
                    .reg32 => |*irr| {
                        irr.min = reg;
                        irr.max = reg;
                    },
                    else => unreachable,
                }
            },
            else => return null,
        }

        if (name.len == next) {
            return encoding;
        } else if (name.len > next + 1) {
            return null;
        }

        switch (name[next]) {
            'U' => switch (encoding.base) {
                // These can be combined when this is fixed:
                // https://github.com/ziglang/zig/issues/15504
                .reg8  => |*irr| irr.signedness = .unsigned,
                .reg16 => |*irr| irr.signedness = .unsigned,
                .reg32 => |*irr| irr.signedness = .unsigned,
                else => unreachable,
            },
            'S' => switch (encoding.base) {
                // These can be combined when this is fixed:
                // https://github.com/ziglang/zig/issues/15504
                .reg8  => |*irr| irr.signedness = .signed,
                .reg16 => |*irr| irr.signedness = .signed,
                .reg32 => |*irr| irr.signedness = .signed,
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

        var encoding = ParameterEncoding{
            .base = .{ .constant = {} },
        };

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
            @panic("Constant value varies based on immediate stored outside of opcode");
        },
    }

    return @intCast(T, encoding.decodeConstant(raw) orelse
        std.debug.panic("Opcode {X:0>4} results in a constant outside the configured range(s) for this parameter", .{ opcode }));
}

pub fn getParameterConstantForOpcode(comptime T: type, insn: InstructionEncoding, encoding: ParameterEncoding, opcode: Opcode) T {
    assert(encoding.base == .constant);
    return getParameterValueForOpcode(T, insn, encoding, opcode, encoding.base_src);
}

pub fn getParameterOffsetForOpcode(comptime T: type, insn: InstructionEncoding, encoding: ParameterEncoding, opcode: Opcode) T {
    assert(encoding.offset == .constant);
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

pub fn comptimeAddressSpaceParameterEncoding(comptime param: ParameterEncoding, comptime address_space: ?AddressSpace) ParameterEncoding {
    comptime {
        var new_param = param;
        new_param.address_space = address_space;
        return new_param;
    }
}

pub fn comptimeRelativeParameterEncoding(comptime param: ParameterEncoding, comptime base: ParameterEncodingBaseType, comptime base_src: ParameterSource) ParameterEncoding {
    comptime {
        var new_param = param;

        new_param.base = base;
        new_param.base_src = base_src;

        if (param.base == .constant and param.base_src == .implicit and param.constant_ranges.len == 0) {
            // `base + 0` is always the same as just `base`
            new_param.offset = .none;
            new_param.offset_src = .implicit;
        } else {
            new_param.offset = param.base;
            new_param.offset_src = param.base_src;
        }

        return new_param;
    }
}

fn validateParameterEncodingConstants(comptime param: ParameterEncoding, src: ParameterSource) void {
    comptime {
        var min_total_values: usize = switch (src) {
            .implicit => 0,
            .opcode => 1,
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
        if (param.base == .none and param.offset == .none) {
            @compileError("Base and offset type cannot both be none!");
        } else if (param.base == .constant) {
            if (param.offset == .constant) {
                @compileError("Base and offset type cannot both be constant!");
            }
            validateParameterEncodingConstants(param, param.base_src);
        } else if (param.offset == .constant) {
            validateParameterEncodingConstants(param, param.offset_src);
        } else if (param.constant_ranges.len > 0 or param.constant_align > 1) {
            @compileError("This parameter does not involve a constant value");
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

            if (param.base == .none and param.arrow) {
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

pub fn getInstructionLength(encoding: InstructionEncoding) u3 {
    var len: u3 = 2;

    if (encoding.mnemonic == .NOPE) {
        // special case; NOPE is really a branch, that skips a byte,
        // but from the programmer's perspective it's an instruction with length 3.
        return 3;
    }

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
    expr_type: ExpressionType,
    constant: i64,

    pub fn print(self: Parameter, writer: anytype) !void {
        if (self.arrow) {
            try writer.writeAll("-> ");
        }
        switch (self.expr_type) {
            .unknown, .poison, .symbol_def => {
                try writer.writeAll(@tagName(self.expr_type));
            },
            .constant => {
                try writer.print("{}", .{ self.constant });
            },
            .reg8, .reg16, .reg32 => |reg| {
                try writer.writeAll(switch (self.expr_type) {
                    .reg8 => "B",
                    .reg16 => "R",
                    .reg32 => "X",
                });
                try writer.print("{}", .{ reg.index });
                if (reg.signedness) |s| {
                    try writer.writeAll(switch (s) {
                        .unsigned => " .unsigned",
                        .signed => " .signed",
                    });
                }
            },
            .sr => |reg| {
                var buf: [4]u8 = .{ 0 } ** 4;
                try writer.writeAll(std.ascii.upperString(&buf, @tagName(reg)));
            },
            .data_address, .insn_address, .stack_address => |info| {
                try writer.writeAll(switch (self.expr_type) {
                    .data_address => ".d ",
                    .insn_address => ".i ",
                    .stack_address => ".s ",
                });
                switch (info.base) {
                    .constant => {
                        try writer.print("{s}", .{ self.constant });
                    },
                    .reg8, .reg16, .reg32 => {
                        try writer.writeAll(switch (info.base) {
                            .reg8 => "B",
                            .reg16 => "R",
                            .reg32 => "X",
                        });
                        try writer.print("{}", .{ info.base.index });
                        if (info.base.signedness) |s| {
                            try writer.writeAll(switch (s) {
                                .unsigned => " .unsigned",
                                .signed => " .signed",
                            });
                        }
                    },
                    .sr => |reg| {
                        var buf: [4]u8 = .{ 0 } ** 4;
                        try writer.writeAll(std.ascii.upperString(&buf, @tagName(reg)));
                    },
                }
                switch (info.offset) {
                    .constant => if (info.base != .constant and self.constant != 0) {
                        try writer.print(" + {s}", .{ self.constant });
                    },
                    .reg8, .reg16, .reg32 => {
                        try writer.writeAll(switch (info.offset) {
                            .reg8 => " + B",
                            .reg16 => " + R",
                            .reg32 => " + X",
                        });
                        try writer.print("{}", .{ info.offset.index });
                        if (info.offset.signedness) |s| {
                            try writer.writeAll(switch (s) {
                                .unsigned => " .unsigned",
                                .signed => " .signed",
                            });
                        }
                    },
                    .sr => |reg| {
                        try writer.writeAll(" + ");
                        var buf: [4]u8 = .{ 0 } ** 4;
                        try writer.writeAll(std.ascii.upperString(&buf, @tagName(reg)));
                    },
                }
            },
        }
    }

    pub fn eql(self: Parameter, other: Parameter) bool {
        return self.arrow == other.arrow
            and std.meta.eql(self.expr_type.base, other.expr_type.base)
            and self.constant == other.constant
            ;
    }

    pub fn matches(self: Parameter, encoding: ParameterEncoding) bool {
        if (self.arrow != encoding.arrow) return false;

        switch (self.expr_type) {
            .unknown, .poison, .symbol_def => return false,
            .constant => {
                if (encoding.address_space) |_| return false;
                if (encoding.base != .constant or encoding.offset != .none) return false;
            },
            .reg8 => |reg| {
                if (encoding.address_space) |_| return false;
                if (encoding.offset != .none) return false;
                return switch (encoding.base) {
                    .reg8 => |re| re.contains(reg),
                    else => false,
                };
            },
            .reg16 => |reg| {
                if (encoding.address_space) |_| return false;
                if (encoding.offset != .none) return false;
                return switch (encoding.base) {
                    .reg16 => |re| re.contains(reg),
                    else => false,
                };
            },
            .reg32 => |reg| {
                if (encoding.address_space) |_| return false;
                if (encoding.offset != .none) return false;
                return switch (encoding.base) {
                    .reg32 => |re| re.contains(reg),
                    else => false,
                };
            },
            .sr => |reg| {
                if (encoding.address_space) |_| return false;
                if (encoding.offset != .none) return false;
                return switch (encoding.base) {
                    .sr => |re| reg == re,
                    else => false,
                };
            },
            .raw_base_offset, .data_address, .insn_address, .stack_address => |info| {
                switch (self.expr_type) {
                    .raw_base_offset => if (encoding.address_space) |_| return false,
                    .data_address => if (.data != encoding.address_space orelse return false) return false,
                    .insn_address => if (.insn != encoding.address_space orelse return false) return false,
                    .stack_address => if (.stack != encoding.address_space orelse return false) return false,
                    else => unreachable,
                }
                if (!innerTypeMatches(info.base, encoding.base)) return false;
                if (info.offset == .constant and self.constant == 0 and encoding.offset == .none) return true;
                if (!innerTypeMatches(info.offset, encoding.offset)) {
                    if (info.base == .constant or info.offset != .none or encoding.offset != .constant) return false;
                }
            }
        }

        if (encoding.base != .constant and encoding.offset != .constant) return true;

        return checkConstantInRange(self.constant, encoding.constant_ranges, encoding.constant_align)
            or checkConstantInRange(self.constant, encoding.alt_constant_ranges, encoding.constant_align);
    }

    fn innerTypeMatches(inner: BaseOffsetType.InnerType, encoding: ParameterEncodingBaseType) bool {
        switch (inner) {
            .none => return encoding == .none,
            .constant => return encoding == .constant,
            .reg8 => |reg| return switch (encoding) {
                .reg8 => |re| re.contains(reg),
                else => false,
            },
            .reg16 => |reg| return switch (encoding) {
                .reg16 => |re| re.contains(reg),
                else => false,
            },
            .reg32 => |reg| return switch (encoding) {
                .reg32 => |re| re.contains(reg),
                else => false,
            },
            .sr => |reg| return switch (encoding) {
                .sr => |re| reg == re,
                else => false,
            },
        }
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
        var first = true;
        for (self.params) |param| {
            if (first) {
                first = false;
            } else if (!param.arrow) {
                try writer.writeByte(',');
            }
            try writer.writeByte(' ');
            try param.print(writer);
        }
        try writer.writeByte('\n');
    }

    pub fn eql(self: Instruction, other: Instruction) bool {
        if (self.mnemonic != other.mnemonic
            or self.suffix != other.suffix
            or self.params.len != other.params.len
        ) return false;

        for (self.params, other.params) |s, o| {
            if (!s.eql(o)) return false;
        }

        return true;
    }

    pub fn matches(self: Instruction, encoding: InstructionEncoding) bool {
        if (self.mnemonic != encoding.mnemonic) return false;
        if (self.suffix != encoding.suffix) return false;
        if (self.params.len != encoding.params.len) return false;

        var encoded: [6]u8 = [_]u8{0} ** 6;
        var encoded_sources = std.EnumSet(ParameterSource){};

        var i: usize = 0;
        while (i < encoding.params.len) : (i += 1) {
            const param = self.params[i];
            const param_encoding = encoding.params[i];
            if (!param.matches(param_encoding)) return false;

            // The rest of this makes sure that if the encoding has multiple parameters stored in the same
            // place, it only matches if the actual parameters are compatible with each other.
            // e.g. The instruction
            //          ADD X0, R4S -> X2
            //      should not match the encoding
            //          ADD Xa, RbS -> Xa
            //      because OA can't encode X0 and X2 at the same time.
            const base = param_encoding.getBaseValue(param);
            const offset = param_encoding.getOffsetValue(param);

            switch (param_encoding.base_src) {
                .implicit, .opcode => {},
                else => |src| {
                    if (encoded_sources.contains(src)) {
                        var required = readParamRaw(&encoded, src, encoding.opcode_base);
                        if (required != base) {
                            return false;
                        }
                    } else {
                        writeParamRaw(&encoded, base, src, encoding.opcode_base);
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
                        writeParamRaw(&encoded, offset, src, encoding.opcode_base);
                        encoded_sources.insert(src);
                    }
                },
            }
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

fn writeParamRaw(buf: []u8, val: i64, src: ParameterSource, opcode_base: Opcode) void {
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

pub fn encodeInstruction(insn: Instruction, encoding: InstructionEncoding, buf: []u8) []u8 {
    assert(insn.mnemonic == encoding.mnemonic);
    assert(insn.suffix == encoding.suffix);
    assert(insn.params.len == encoding.params.len);

    writeOpcode(buf, encoding.opcodes.min);

    if (insn.mnemonic == .NOPE) {
        writeImm8(buf, 0);
    }

    for (insn.params, encoding.params) |param, param_encoding| {
        assert(param.matches(param_encoding));
        const base = param_encoding.getBaseValue(param);
        const offset = param_encoding.getOffsetValue(param);
        writeParamRaw(buf, base, param_encoding.base_src, encoding.opcode_base);
        writeParamRaw(buf, offset, param_encoding.offset_src, encoding.opcode_base);
    }

    return buf[0..getInstructionLength(encoding)];
}

pub const Encoder = struct {
    mem: []u8,
    remaining: []u8,
    last_instruction_encoded: []const u8,

    pub fn init(mem: []u8) Encoder {
        return .{
            .mem = mem,
            .remaining = mem,
            .last_instruction_encoded = "",
        };
    }

    pub fn reset(self: *Encoder) void {
        self.remaining = self.mem;
        self.last_instruction_encoded = "";
    }

    pub fn encode(self: *Encoder, insn: Instruction, encoding: InstructionEncoding) void {
        self.last_instruction_encoded = encodeInstruction(insn, encoding, self.remaining);
        self.remaining = self.remaining[self.last_instruction_encoded.len..];
    }

    pub fn encodeU8(self: *Encoder, d: u8) void {
        std.debug.assert(self.remaining.len >= 1);
        self.remaining[0] = d;
        self.remaining = self.remaining[1..];
    }
    pub fn encodeU16(self: *Encoder, d: u16) void {
        std.debug.assert(self.remaining.len >= 2);
        self.remaining[0] = @truncate(u8, d);
        self.remaining[1] = @intCast(u8, d >> 8);
        self.remaining = self.remaining[2..];
    }
    pub fn encodeU32(self: *Encoder, d: u32) void {
        std.debug.assert(self.remaining.len >= 4);
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

pub fn readOpcode(buf: []const u8) Opcode {
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

pub fn parameter(expr_type: ExpressionType, constant: i64) Parameter {
    return .{
        .expr_type = expr_type,
        .constant = constant,
    };
}

pub fn toParameter(expr_type: ExpressionType, constant: i64) Parameter {
    return .{
        .arrow = true,
        .expr_type = expr_type,
        .constant = constant,
    };
}
