const std = @import("std");
const bits = @import("bits");
const misc = @import("misc");
const isa = @import("isa_types");
const ParameterEncoding = @import("ParameterEncoding.zig");
const Opcode = isa.Opcode;
const IndexedRegister = isa.IndexedRegister;
const SpecialRegister = isa.SpecialRegister;
const ConstantRange = isa.ConstantRange;
const AddressSpace = isa.AddressSpace;

const Parameter = @This();

arrow: bool = false,
expr_type: ExpressionType,
constant: i64,


pub const ExpressionType = union(enum) {
    unknown,    // Used internally by the assembler; never part of any instruction encodings
    poison,     // Used internally by the assembler; never part of any instruction encodings
    symbol_def, // Used internally by the assembler; never part of any instruction encodings

    constant,
    reg8: IndexedRegister,
    reg16: IndexedRegister,
    reg32: IndexedRegister,
    sr: SpecialRegister,
    raw_base_offset: BaseOffset,
    data_address: BaseOffset,
    insn_address: BaseOffset,
    stack_address: BaseOffset,

    pub const BaseOffset = struct {
        base: Type,
        offset: Type,

        pub fn init(left: ?ExpressionType, right: ?ExpressionType) !BaseOffset {
            return BaseOffset{
                .base = try Type.fromExpressionType(left),
                .offset = try Type.fromExpressionType(right),
            };
        }

        pub const Type = union(enum) {
            none,
            constant,
            reg8: IndexedRegister,
            reg16: IndexedRegister,
            reg32: IndexedRegister,
            sr: SpecialRegister,

            pub fn fromExpressionType(maybe_expr_type: ?ExpressionType) !Type {
                return if (maybe_expr_type) |t| switch (t) {
                    .constant => .{ .constant = {} },
                    .reg8 => |r| .{ .reg8 = r },
                    .reg16 => |r| .{ .reg16 = r },
                    .reg32 => |r| .{ .reg32 = r },
                    .sr => |r| .{ .sr = r },
                    else => return error.InvalidType,
                } else .{ .none = {} };
            }

            pub fn toExpressionType(self: Type) ExpressionType {
                return switch (self) {
                    .none, .constant => .{ .constant = {} },
                    .reg8 => |r| .{ .reg8 = r },
                    .reg16 => |r| .{ .reg16 = r },
                    .reg32 => |r| .{ .reg32 = r },
                    .sr => |r| .{ .sr = r },
                };
            }

            pub fn matches(self: Type, encoding: ParameterEncoding.BaseOffsetEncoding) bool {
                switch (self) {
                    .none => return encoding == .none,
                    .constant => return encoding == .constant,
                    .reg8 => |r| return switch (encoding) {
                        .reg8 => |re| re.contains(r),
                        else => false,
                    },
                    .reg16 => |r| return switch (encoding) {
                        .reg16 => |re| re.contains(r),
                        else => false,
                    },
                    .reg32 => |r| return switch (encoding) {
                        .reg32 => |re| re.contains(r),
                        else => false,
                    },
                    .sr => |r| return switch (encoding) {
                        .sr => |re| r == re,
                        else => false,
                    },
                }
            }
        };
    };

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
            .base = try BaseOffset.Type.fromExpressionType(base),
            .offset = try BaseOffset.Type.fromExpressionType(offset),
        }};
    }

    pub fn address(comptime space: AddressSpace, base: ExpressionType, offset: ExpressionType) !ExpressionType {
        return @unionInit(ExpressionType, @tagName(space) ++ "_address", .{
            .base = try BaseOffset.Type.fromExpressionType(base),
            .offset = try BaseOffset.Type.fromExpressionType(offset),
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
            .base = try BaseOffset.Type.fromExpressionType(base),
            .offset = .{ .constant = {} },
        });
    }

    pub const Builder = struct {
        invalid: bool = false,
        unknown: bool = false,
        poison: bool = false,
        address_space: ?AddressSpace = null,
        a: Type = .{ .none = {} },
        b: Type = .{ .none = {} },

        const Type = BaseOffset.Type;

        pub fn add(self: *Builder, t: ExpressionType) void {
            switch (t) {
                .unknown => self.unknown = true,
                .poison => self.poison = true,
                .symbol_def => self.invalid = true,
                .raw_base_offset, .data_address, .insn_address, .stack_address => |bo| {
                    self.tryAddType(bo.base);
                    self.tryAddType(bo.offset);
                },
                else => self.tryAddExpressionType(t),
            }

            if (getAddressSpace(t)) |as| {
                if (self.address_space) |eas| {
                    if (eas != as) self.invalid = true;
                }
                self.address_space = as;
            }
        }

        pub fn subtract(self: *Builder, t: ExpressionType) void {
            switch (t) {
                .unknown => self.unknown = true,
                .poison => self.poison = true,
                .symbol_def => self.invalid = true,
                .raw_base_offset, .data_address, .insn_address, .stack_address => |bo| {
                    self.trySubtractType(bo.base);
                    self.trySubtractType(bo.offset);
                },
                else => self.trySubtractExpressionType(t),
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

        fn tryAddExpressionType(self: *Builder, t: ExpressionType) void {
            self.tryAddType(Type.fromExpressionType(t) catch return);
        }
        fn tryAddType(self: *Builder, t: Type) void {
            if (sum(self.a, t)) |s| {
                self.a = s;
            } else if (sum(self.b, t)) |s| {
                self.b = s;
            } else {
                self.invalid = true;
            }
        }

        fn trySubtractExpressionType(self: *Builder, t: ExpressionType) void {
            self.trySubtractType(Type.fromExpressionType(t) catch return);
        }
        fn trySubtractType(self: *Builder, t: Type) void {
            if (difference(self.a, t)) |d| {
                self.a = d;
            } else if (difference(self.b, t)) |d| {
                self.b = d;
            } else {
                self.invalid = true;
            }
        }

        fn sum(a: Type, b: Type) ?Type {
            if (a == .none) return b;
            if (b == .none) return a;
            if (a == .constant and b == .constant) return a;
            return null;
        }

        fn difference(a: Type, b: Type) ?Type {
            if (b == .none) return a;
            if (a == .none and b == .constant) return b;
            if (a == .constant and b == .constant) return b;

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

        fn getPriority(t: Type) u8 {
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

        pub fn build(self: *Builder) !ExpressionType {
            if (self.invalid) return error.InvalidType;
            if (self.poison) return .{ .poison = {} };
            if (self.unknown) return .{ .unknown = {} };

            if (getPriority(self.a) < getPriority(self.b)) {
                const temp = self.a;
                self.a = self.b;
                self.b = temp;
            }

            const bo = BaseOffset{
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

            return Type.toExpressionType(bo.base);
        }
    };
};


pub fn print(self: Parameter, writer: anytype, address: u32) !void {
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
                else => unreachable,
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
        .raw_base_offset, .data_address, .insn_address, .stack_address => |info| {
            try writer.writeAll(switch (self.expr_type) {
                .raw_base_offset => "",
                .data_address => ".d ",
                .insn_address => ".i ",
                .stack_address => ".s ",
                else => unreachable,
            });
            switch (info.base) {
                .none => {},
                .constant => if (self.constant < 0) {
                    try writer.print("-0x{X}", .{ -self.constant });
                } else {
                    try writer.print("0x{X}", .{ self.constant });
                },
                .reg8, .reg16, .reg32 => |reg| {
                    try writer.writeAll(switch (info.base) {
                        .reg8 => "B",
                        .reg16 => "R",
                        .reg32 => "X",
                        else => unreachable,
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
            }
            switch (info.offset) {
                .none => {},
                .constant => if (info.base != .constant and self.constant != 0) {
                    if (info.base == .sr and info.base.sr == .IP) {
                        const target = address + self.constant;
                        try writer.print(" + 0x{X} - 0x{X}", .{ target, address });
                    } else if (self.constant < 0) {
                        try writer.print(" - 0x{X}", .{ -self.constant });
                    } else {
                        try writer.print(" + 0x{X}", .{ -self.constant });
                    }
                },
                .reg8, .reg16, .reg32 => |reg| {
                    try writer.writeAll(switch (info.offset) {
                        .reg8 => " + B",
                        .reg16 => " + R",
                        .reg32 => " + X",
                        else => unreachable,
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
                    try writer.writeAll(" + ");
                    var buf: [4]u8 = .{ 0 } ** 4;
                    try writer.writeAll(std.ascii.upperString(&buf, @tagName(reg)));
                },
            }
        },
    }
}

pub fn matches(self: Parameter, encoding: ParameterEncoding) bool {
    if (self.arrow != encoding.arrow) return false;

    switch (self.expr_type) {
        .unknown, .poison, .symbol_def => return false,
        .constant => {
            if (encoding.address_space) |_| return false;
            if (encoding.base != .constant or encoding.offset != .none) return false;
            return checkConstantInRange(self.constant, encoding.base.constant);
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
            if (!info.base.matches(encoding.base)) return false;
            if (info.offset == .constant and self.constant == 0 and encoding.offset == .none) return true;
            if (!info.offset.matches(encoding.offset)) {
                if (info.base == .constant or info.offset != .none or encoding.offset != .constant) return false;
            }
            if (encoding.base == .constant) {
                return checkConstantInRange(self.constant, encoding.base.constant);
            } else if (encoding.offset == .constant) {
                return checkConstantInRange(self.constant, encoding.offset.constant);
            } else return true;
        }
    }
}

fn checkConstantInRange(constant: i64, constant_encoding: ParameterEncoding.ConstantEncoding) bool {
    for (constant_encoding.ranges) |range| {
        if (constant >= range.min and constant <= range.max) {
            var dc = constant - range.min;
            if (@rem(dc, constant_encoding.granularity) == 0) {
                return true;
            }
        }
    }
    for (constant_encoding.alt_ranges) |range| {
        if (constant >= range.min and constant <= range.max) {
            var dc = constant - range.min;
            if (@rem(dc, constant_encoding.granularity) == 0) {
                return true;
            }
        }
    }
    return false;
}


pub fn read(buf: []const u8, base_opcode: Opcode, encoding: ParameterEncoding) Parameter {
    var param = Parameter{
        .arrow = encoding.arrow,
        .expr_type = undefined,
        .constant = 0,
    };
    const base = readRaw(buf, encoding.base_src, base_opcode);
    const offset = readRaw(buf, encoding.offset_src, base_opcode);

    if (encoding.address_space != null or encoding.offset != .none) {
        var bo: ExpressionType.BaseOffset = .{
            .base = switch (encoding.base) {
                .none => .{ .constant = {} },
                .sr => |sr| .{ .sr = sr },
                .constant => blk: {
                    param.constant = encoding.decodeConstant(base) orelse base;
                    break :blk .{ .constant = {} };
                },
                .reg8 => |re| .{ .reg8 = .{
                    .index = if (encoding.base_src == .implicit) re.min else base,
                    .signedness = re.signedness,
                }},
                .reg16 => |re| .{ .reg16 = .{
                    .index = if (encoding.base_src == .implicit) re.min else base,
                    .signedness = re.signedness,
                }},
                .reg32 => |re| .{ .reg32 = .{
                    .index = if (encoding.base_src == .implicit) re.min else base,
                    .signedness = re.signedness,
                }},
            },
            .offset = switch (encoding.offset) {
                .none => .{ .constant = {} },
                .sr => |sr| .{ .sr = sr },
                .constant => blk: {
                    param.constant = encoding.decodeConstant(offset) orelse offset;
                    break :blk .{ .constant = {} };
                },
                .reg8 => |re| .{ .reg8 = .{
                    .index = if (encoding.offset_src == .implicit) re.min else offset,
                    .signedness = re.signedness,
                }},
                .reg16 => |re| .{ .reg16 = .{
                    .index = if (encoding.offset_src == .implicit) re.min else offset,
                    .signedness = re.signedness,
                }},
                .reg32 => |re| .{ .reg32 = .{
                    .index = if (encoding.offset_src == .implicit) re.min else offset,
                    .signedness = re.signedness,
                }},
            },
        };
        param.expr_type = if (encoding.address_space) |as| switch (as) {
            .data => .{ .data_address = bo },
            .insn => .{ .insn_address = bo },
            .stack => .{ .stack_address = bo },
        } else .{ .raw_base_offset = bo };
    } else switch (encoding.base) {
        .none => {
            param.expr_type = .{ .constant = {} };
        },
        .constant => {
            param.constant = encoding.decodeConstant(base) orelse base;
            param.expr_type = .{ .constant = {} };
        },
        .reg8 => |re| {
            param.expr_type = .{ .reg8 = .{
                .index = if (encoding.base_src == .implicit) re.min else base,
                .signedness = re.signedness,
            }};
        },
        .reg16 => |re| {
            param.expr_type = .{ .reg16 = .{
                .index = if (encoding.base_src == .implicit) re.min else base,
                .signedness = re.signedness,
            }};
        },
        .reg32 => |re| {
            param.expr_type = .{ .reg32 = .{
                .index = if (encoding.base_src == .implicit) re.min else base,
                .signedness = re.signedness,
            }};
        },
        .sr => |sr| {
            param.expr_type = .{ .sr = sr };
        },
    }

    return param;
}

pub fn write(self: Parameter, encoding: ParameterEncoding, opcode_base: Opcode, buf: []u8) void {
    std.debug.assert(self.matches(encoding));

    const base = getRawBase(self, encoding);
    const offset = getRawOffset(self, encoding);

    writeRaw(buf, base, encoding.base_src, opcode_base);
    writeRaw(buf, offset, encoding.offset_src, opcode_base);
}

pub fn getRawBase(self: Parameter, encoding: ParameterEncoding) i64 {
    return switch (encoding.base) {
        .none, .sr => 0,
        .constant => |ce| ce.encodeConstant(self.constant),
        .reg8, .reg16, .reg32 => switch (self.expr_type) {
            .unknown, .poison, .symbol_def, .constant, .sr => unreachable,
            .reg8, .reg16, .reg32 => |reg| reg.index,
            .raw_base_offset, .data_address, .insn_address, .stack_address => |info| switch (info.base) {
                .none, .constant, .sr => unreachable,
                .reg8, .reg16, .reg32 => |reg| reg.index,
            },
        },
    };
}

pub fn getRawOffset(self: Parameter, encoding: ParameterEncoding) i64 {
    return switch (encoding.offset) {
        .none, .sr => 0,
        .constant => |ce| ce.encodeConstant(self.constant),
        .reg8, .reg16, .reg32 => switch (self.expr_type) {
            .unknown, .poison, .symbol_def, .constant, .sr, .reg8, .reg16, .reg32 => unreachable,
            .raw_base_offset, .data_address, .insn_address, .stack_address => |info| switch (info.offset) {
                .none, .constant, .sr => unreachable,
                .reg8, .reg16, .reg32 => |reg| reg.index,
            },
        },
    };
}

pub fn readRaw(buf: []const u8, src: ParameterEncoding.Source, base_opcode: Opcode) i64 {
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

fn readOB(buf: []const u8) misc.OperandB {
    std.debug.assert(buf.len >= 1);
    return @intCast(misc.OperandB, buf[0] >> 4);
}

fn readOA(buf: []const u8) misc.OperandA {
    std.debug.assert(buf.len >= 1);
    return @truncate(misc.OperandA, buf[0]);
}

fn readOBOA(buf: []const u8) misc.CombinedOperands {
    std.debug.assert(buf.len >= 1);
    return buf[0];
}

fn readImmOB(buf: []const u8) misc.OperandB {
    std.debug.assert(buf.len >= 3);
    return @intCast(misc.OperandB, buf[2] >> 4);
}

fn readImmOA(buf: []const u8) misc.OperandA {
    std.debug.assert(buf.len >= 3);
    return @truncate(misc.OperandA, buf[2]);
}

fn readImm8(buf: []const u8) u8 {
    std.debug.assert(buf.len >= 3);
    return buf[2];
}

fn readImm16(buf: []const u8, offset: usize) u16 {
    std.debug.assert(buf.len >= 4);
    return bits.concat(.{
        buf[offset],
        buf[offset + 1],
    });
}

fn readImm32(buf: []const u8) u32 {
    std.debug.assert(buf.len >= 6);
    return bits.concat(.{
        buf[2],
        buf[3],
        buf[4],
        buf[5],
    });
}

pub fn readOpcode(buf: []const u8) Opcode {
    std.debug.assert(buf.len >= 2);
    return bits.concat(.{
        buf[0],
        buf[1],
    });
}





pub fn writeRaw(buf: []u8, val: i64, src: ParameterEncoding.Source, opcode_base: Opcode) void {
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

fn writeOB(buf: []u8, val: misc.OperandB) void {
    std.debug.assert(buf.len >= 1);
    buf[0] = bits.concat(.{
        @truncate(u4, buf[0]),
        val,
    });
}

fn writeOA(buf: []u8, val: misc.OperandA) void {
    std.debug.assert(buf.len >= 1);
    buf[0] = bits.concat(.{
        val,
        @intCast(u4, buf[0] >> 4),
    });
}

fn writeOBOA(buf: []u8, val: misc.CombinedOperands) void {
    std.debug.assert(buf.len >= 1);
    buf[0] = val;
}

fn writeImmOB(buf: []u8, val: misc.OperandB) void {
    std.debug.assert(buf.len >= 3);
    buf[2] = bits.concat(.{
        @truncate(u4, buf[2]),
        val,
    });
}

fn writeImmOA(buf: []u8, val: misc.OperandA) void {
    std.debug.assert(buf.len >= 3);
    buf[2] = bits.concat(.{
        val,
        @intCast(u4, buf[2] >> 4),
    });
}

fn writeImm8(buf: []u8, val: u8) void {
    std.debug.assert(buf.len >= 3);
    buf[2] = val;
}

fn writeImm16(buf: []u8, val: u16, offset: usize) void {
    std.debug.assert(buf.len >= 4);
    buf[offset] = @truncate(u8, val);
    buf[offset + 1] = @intCast(u8, val >> 8);
}

fn writeImm32(buf: []u8, val: u32) void {
    std.debug.assert(buf.len >= 6);
    buf[2] = @truncate(u8, val);
    buf[3] = @truncate(u8, val >> 8);
    buf[4] = @truncate(u8, val >> 16);
    buf[5] = @intCast(u8, val >> 24);
}

pub fn writeOpcode(buf: []u8, opcode: Opcode) void {
    std.debug.assert(buf.len >= 2);
    buf[0] = @truncate(u8, opcode);
    buf[1] = @intCast(u8, opcode >> 8);
}
