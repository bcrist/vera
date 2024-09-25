token: lex.Token.Handle,
info: Info,
resolved_type: Type,
resolved_constant: ?*const Constant,
flags: Flag_Set,

pub const Handle = u31;

pub const Flag_Set = std.EnumSet(Flags);
pub const Flags = enum {
    constant_depends_on_layout,
};

pub const Kind = std.meta.Tag(Info);
pub const Info = union (enum) {
    list: Binary,
    arrow,
    literal_int,
    literal_str,
    literal_reg,
    literal_current_address,
    literal_symbol_def,
    directive_symbol_def: Unary,
    literal_symbol_ref,
    directive_symbol_ref: Unary,
    local_label_def: Unary,
    negate: Unary,
    complement: Unary,
    signed_cast: Unary,
    unsigned_cast: Unary,
    remove_signedness_cast: Unary,
    absolute_address_cast: Unary,
    data_address_cast: Unary,
    insn_address_cast: Unary,
    stack_address_cast: Unary,
    remove_address_cast: Unary,
    index_to_reg: Unary,
    reg_to_index: Unary,
    plus: Binary,
    minus: Binary,
    multiply: Binary,
    shl: Binary,
    shr: Binary,
    concat: Binary,
    concat_repeat: Binary,
    bitwise_or: Binary,
    bitwise_xor: Binary,
    bitwise_and: Binary,
    length_cast: Binary,
    truncate: Binary,
    sign_extend: Binary,
    zero_extend: Binary,
    crlf_cast: Unary,
    lf_cast: Unary,
};

pub const Unary = Expression.Handle;
pub const Binary = struct {
    left: Expression.Handle,
    right: Expression.Handle,
};

pub const Type = union (enum) {
    unknown,
    poison,
    symbol_def,
    arrow,
    raw: Base_Offset_Type,
    data: Base_Offset_Type,
    insn: Base_Offset_Type,
    stack: Base_Offset_Type,

    pub fn base_offset_type(self: Type) ?Base_Offset_Type {
        return switch (self) {
            .unknown, .poison, .symbol_def, .arrow => null,
            .raw, .data, .insn, .stack => |bot| bot,
        };
    }

    pub fn address_space(self: Type) ?Address_Space {
        return switch (self) {
            .unknown, .poison, .symbol_def, .arrow, .raw => null,
            .data => .data,
            .insn => .insn,
            .stack => .stack,
        };
    }

    pub fn param_signature(self: Type) Parameter.Signature {
        return switch (self) {
            .arrow => .{
                .address_space = null,
                .base = .arrow,
                .offset = .none,
            },
            .raw => |bot| .{
                .address_space = null,
                .base = bot.base.param_kind(),
                .offset = bot.offset.param_kind(),
            },
            .data => |bot| .{
                .address_space = .data,
                .base = bot.base.param_kind(),
                .offset = bot.offset.param_kind(),
            },
            .insn => |bot| .{
                .address_space = .insn,
                .base = bot.base.param_kind(),
                .offset = bot.offset.param_kind(),
            },
            .stack => |bot| .{
                .address_space = .stack,
                .base = bot.base.param_kind(),
                .offset = bot.offset.param_kind(),
            },
            .unknown, .poison, .symbol_def => unreachable,
        };
    }

    pub fn param_base_register_index(self: Type) Register_Index {
        const bot = self.base_offset_type() orelse return 0;
        return bot.base.register_index() orelse 0;
    }

    pub fn param_offset_register_index(self: Type) Register_Index {
        const bot = self.base_offset_type() orelse return 0;
        return bot.offset.register_index() orelse 0;
    }

    pub fn simple_base(self: Type) ?Term_Type {
        const bot = self.base_offset_type() orelse return null;
        if (bot.offset != .none) return null;
        return bot.base;
    }

    pub fn is_constant(self: Type) bool {
        const base = self.simple_base() orelse return false;
        return base == .constant;
    }

    // Note this is NOT just !is_constant(); it returns false when not a BOT
    pub fn is_non_constant(self: Type) bool {
        const bot = self.base_offset_type() orelse return false;
        return bot.base != .constant or bot.offset != .none;
    }

    pub fn constant() Type {
        return .{ .raw = .{
            .base = .constant,
            .offset = .none,
        }};
    }

    pub fn sr(which: Special_Register) Type {
        return .{ .raw = .{
            .base = .{ .sr = which },
            .offset = .none,
        }};
    }

    pub fn reg(index: Register_Index, signedness: ?Signedness) Type {
        return .{ .raw = .{
            .base = .{ .reg = .{
                .index = index,
                .signedness = signedness,
            }},
            .offset = .none,
        }};
    }

    pub fn raw_base_offset(base: Type, offset: Type) !Type {
        const bbot = base.base_offset_type() orelse return error.InvalidType;
        const obot = offset.base_offset_type() orelse return error.InvalidType;
        if (bbot.offset != .none) return error.InvalidType;
        if (obot.offset != .none) return error.InvalidType;
        return .{ .raw = .{
            .base = bbot.base,
            .offset = obot.base,
        }};
    }

    pub fn address(comptime space: Address_Space, base: Type, offset: Type) !Type {
        const bbot = base.base_offset_type() orelse return error.InvalidType;
        const obot = offset.base_offset_type() orelse return error.InvalidType;
        if (bbot.offset != .none) return error.InvalidType;
        if (obot.offset != .none) return error.InvalidType;
        return @unionInit(Type, @tagName(space), .{
            .base = bbot.base,
            .offset = obot.base,
        });
    }

    pub fn address_space_cast(comptime space: Address_Space, bot: Base_Offset_Type) Type {
        return @unionInit(Type, @tagName(space), bot);
    }

    pub fn absolute_address(comptime space: Address_Space) Type {
        return @unionInit(Type, @tagName(space), .{
            .base = .constant,
            .offset = .none,
        });
    }

    pub fn relative_address(comptime space: Address_Space, base: Type) !Type {
        const bbot = base.base_offset_type() orelse return error.InvalidType;
        if (bbot.offset != .none) return error.InvalidType;
        return @unionInit(Type, @tagName(space), .{
            .base = bbot.base,
            .offset = .constant,
        });
    }
};

pub const Base_Offset_Type = struct {
    base: Term_Type,
    offset: Term_Type,
};

pub const Term_Type = union (enum) {
    none,
    constant,
    reg: Indexed_Register_Type,
    sr: Special_Register,

    pub fn param_kind(self: Term_Type) Parameter.Kind {
        return switch (self) {
            .none => .none,
            .constant => .constant,
            .reg => |reg| .{ .reg = reg.signedness },
            .sr => |sr| .{ .sr = sr },
        };
    }

    pub fn register_index(self: Term_Type) ?Register_Index {
        return switch (self) {
            .none, .constant, .sr => null,
            .reg => |reg| reg.index,
        };
    }
};

pub const Indexed_Register_Type = struct {
    signedness: ?Signedness,
    index: Register_Index,
};

pub const Type_Builder = struct {
    invalid: bool = false,
    unknown: bool = false,
    poison: bool = false,
    address_space: ?Address_Space = null,
    a: Term_Type = .none,
    b: Term_Type = .none,

    pub fn add(self: *Type_Builder, t: Type) void {
        switch (t) {
            .unknown => self.unknown = true,
            .poison => self.poison = true,
            .symbol_def, .arrow => self.invalid = true,
            .raw, .data, .insn, .stack => |bot| {
                self.try_add_term(bot.base);
                self.try_add_term(bot.offset);
            },
        }

        if (t.address_space()) |as| {
            if (self.address_space) |eas| {
                if (eas != as) self.invalid = true;
            }
            self.address_space = as;
        }
    }

    pub fn subtract(self: *Type_Builder, t: Type) void {
        switch (t) {
            .unknown => self.unknown = true,
            .poison => self.poison = true,
            .symbol_def, .arrow => self.invalid = true,
            .raw, .data, .insn, .stack => |bot| {
                self.try_subtract_term(bot.base);
                self.try_subtract_term(bot.offset);
            },
        }

        if (t.address_space()) |as| {
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

    pub fn build(self: *Type_Builder) !Type {
        if (self.invalid) return error.InvalidType;
        if (self.poison) return .poison;
        if (self.unknown) return .unknown;

        if (get_priority(self.a) < get_priority(self.b)) {
            const temp = self.a;
            self.a = self.b;
            self.b = temp;
        }

        const bot: Base_Offset_Type = .{
            .base = self.a,
            .offset = self.b,
        };

        if (self.address_space) |as| {
            return switch (as) {
                .data => .{ .data = bot },
                .insn => .{ .insn = bot },
                .stack => .{ .stack = bot },
            };
        } else return .{ .raw = bot };
    }

    fn try_add_term(self: *Type_Builder, t: Term_Type) void {
        if (sum(self.a, t)) |s| {
            self.a = s;
        } else if (sum(self.b, t)) |s| {
            self.b = s;
        } else {
            self.invalid = true;
        }
    }

    fn try_subtract_term(self: *Type_Builder, t: Term_Type) void {
        if (difference(self.a, t)) |d| {
            self.a = d;
        } else if (difference(self.b, t)) |d| {
            self.b = d;
        } else {
            self.invalid = true;
        }
    }

    fn sum(a: Term_Type, b: Term_Type) ?Term_Type {
        if (a == .none) return b;
        if (b == .none) return a;
        if (a == .constant and b == .constant) return a;
        return null;
    }

    fn difference(a: Term_Type, b: Term_Type) ?Term_Type {
        if (b == .none) return a;
        if (a == .none and b == .constant) return b;
        if (a == .constant and b == .constant) return b;

        // registers can "cancel out"
        if (std.meta.eql(a, b)) return .none;

        // anything else is unrepresentable (e.g. negative registers)
        return null;
    }

    fn get_priority(t: Term_Type) u8 {
        // higher priority will be placed in the base slot, lower in offset
        return switch (t) {
            .none => 0,
            .constant => 1,
            .reg => |r| @as(u8, 0x10) + r.index,
            .sr => |r| @as(u8, 0x40) + @intFromEnum(r),
        };
    }
};

const Expression = @This();
const lex = @import("lex.zig");
const Constant = @import("Constant.zig");
const Instruction = @import("Instruction.zig");
const Special_Register = isa.Special_Register;
const Address_Space = isa.Address_Space;
const Parameter = isa.Parameter;
const isa = @import("isa");
const Register_Index = arch.Register_Index;
const arch = @import("arch");
const Signedness = std.builtin.Signedness;
const std = @import("std");
