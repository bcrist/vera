allocator: std.mem.Allocator,
next_token: Token.Handle,
token_kinds: []Token_Kind,
token_offsets: []u32,
tokens: lex.Token_List,
source: []const u8,
temp_param_signatures: std.ArrayListUnmanaged(Parameter.Signature),
temp_constraints: std.ArrayListUnmanaged(Constraint),
temp_encoders: std.ArrayListUnmanaged(Encoder),
start_of_line: Token.Handle,

pub fn init(allocator: std.mem.Allocator, source: []const u8) Spec_Parser {
    const tokens = lex.lex(allocator, source);
    return .{
        .allocator = allocator,
        .next_token = 0,
        .start_of_line = 0,
        .token_kinds = tokens.items(.kind),
        .token_offsets = tokens.items(.offset),
        .tokens = tokens,
        .source = source,
        .temp_param_signatures = .{},
        .temp_constraints = .{},
        .temp_encoders = .{},
    };
}

pub fn prev_line(self: *Spec_Parser, source: []const u8) []const u8 {
    const start = self.token_offsets[self.start_of_line];
    const end = self.token_offsets[self.next_token];
    return source[start..end];
}

/// N.B. The parsed encoding's constraints, encodings, and param signatures are only
/// valid until the next call to next().  It is the caller's responsibility to copy them
/// to permanent storage if necessary.
pub fn next(self: *Spec_Parser) ?Instruction_Encoding {
    self.temp_param_signatures.clearRetainingCapacity();
    self.temp_constraints.clearRetainingCapacity();
    self.temp_encoders.clearRetainingCapacity();
    while (true) {
        if (self.token_kinds[self.next_token] == .eof) return null;

        self.skip_linespace();

        if (parse_helpers.parse_mnemonic(self)) |mnemonic| {
            var swap_params = false;
            const suffix = parse_helpers.parse_suffix(self, &swap_params);
            if (swap_params) {
                self.record_error_rel("Please swap the suffix ordering!", -1);
            }

            var first = true;
            while (true) {
                self.skip_linespace();

                if (self.try_token(.arrow)) {
                    self.add_signature(null, .arrow, .none);
                    continue;
                }

                const addr_space = self.parse_addr_space();
                self.skip_linespace();

                if (self.parse_int_literal()) |literal| {
                    const param_index = Parameter.Index.init(@intCast(self.temp_param_signatures.items.len));
                    self.add_constant(param_index, literal);
                    self.add_signature(addr_space, .constant, .none);

                } else if (self.parse_first_placeholder()) |placeholder| {
                    const param_index = Parameter.Index.init(@intCast(self.temp_param_signatures.items.len));
                    self.add_placeholder_constant(param_index, placeholder);
                    while (self.parse_additional_placeholder()) |p| {
                        self.add_placeholder_constant(param_index, p);
                    }
                    self.add_signature(addr_space, .constant, .none);

                } else if (self.parse_base_register(addr_space)) {
                    self.parse_offset();
                } else if (first) {
                    break;
                } else {
                    self.record_error("Expected parameter");
                }

                first = false;

                self.skip_linespace();
                if (self.try_token(.arrow)) {
                    self.add_signature(null, .arrow, .none);
                    continue;
                } else if (!self.try_token(.comma)) {
                    break;
                }
            }

            self.skip_linespace();
            if (!self.try_token(.newline) and self.token_kinds[self.next_token] != .eof) {
                self.record_error("Expected end of line");
            }

            return .{
                .signature = .{
                    .mnemonic = mnemonic,
                    .suffix = suffix,
                    .params = self.temp_param_signatures.items,
                },
                .constraints = self.temp_constraints.items,
                .encoders = self.temp_encoders.items,
            };

        } else if (!self.try_token(.newline) and self.token_kinds[self.next_token] != .eof) {
            self.record_error("Expected end of line");
        }
    }
}

fn add_signature(self: *Spec_Parser, addr_space: ?isa.Address_Space, base: Parameter.Kind, offset: Parameter.Kind) void {
    self.temp_param_signatures.append(self.allocator, .{
        .address_space = addr_space,
        .base = base,
        .offset = offset,
    }) catch @panic("OOM");
}

fn add_constant(self: *Spec_Parser, index: Parameter.Index, constant: i64) void {
    self.temp_constraints.append(self.allocator, .{
        .kind = .equal,
        .left = .{ .placeholder = .{
            .index = index,
            .kind = .param_constant,
            .name = "",
        }},
        .right = .{ .constant = constant },
    }) catch @panic("OOM");
}

fn add_base_register(self: *Spec_Parser, index: Parameter.Index, reg: Register_Index) void {
    self.temp_constraints.append(self.allocator, .{
        .kind = .equal,
        .left = .{ .placeholder = .{
            .index = index,
            .kind = .param_base_register,
            .name = "",
        }},
        .right = .{ .constant = reg },
    }) catch @panic("OOM");
}

fn add_offset_register(self: *Spec_Parser, index: Parameter.Index, reg: Register_Index) void {
    self.temp_constraints.append(self.allocator, .{
        .kind = .equal,
        .left = .{ .placeholder = .{
            .index = index,
            .kind = .param_offset_register,
            .name = "",
        }},
        .right = .{ .constant = reg },
    }) catch @panic("OOM");
}

fn add_placeholder(self: *Spec_Parser, index: Parameter.Index, placeholder: []const u8, kind: Instruction_Encoding.Placeholder_Kind) void {
    for (self.temp_encoders.items) |encoder| {
        if (std.mem.eql(u8, encoder.value.placeholder.name, placeholder)) {
            self.temp_constraints.append(self.allocator, .{
                .kind = .equal,
                .left = .{ .placeholder = .{
                    .index = index,
                    .kind = kind,
                    .name = placeholder,
                }},
                .right = encoder.value,
            }) catch @panic("OOM");
            return;
        }
    }
    self.temp_encoders.append(self.allocator, .{
        .value = .{ .placeholder = .{
            .index = index,
            .kind = kind,
            .name = placeholder,
        }},
        .domain = .{ .enumerated = &.{} },
        .bit_offset = 0,
        .bit_count = 0,
    }) catch @panic("OOM");
}
fn add_placeholder_constant(self: *Spec_Parser, index: Parameter.Index, placeholder: []const u8) void {
    self.add_placeholder(index, placeholder, .param_constant);
}
fn add_placeholder_base_register(self: *Spec_Parser, index: Parameter.Index, placeholder: []const u8) void {
    self.add_placeholder(index, placeholder, .param_base_register);
}
fn add_placeholder_offset_register(self: *Spec_Parser, index: Parameter.Index, placeholder: []const u8) void {
    self.add_placeholder(index, placeholder, .param_offset_register);
}

fn parse_base_register(self: *Spec_Parser, addr_space: ?isa.Address_Space) bool {
    const param_index = Parameter.Index.init(@intCast(self.temp_param_signatures.items.len));
    if (self.parse_sr()) |sr| {
        self.add_signature(addr_space, .{ .sr = sr }, .none);

    } else if (self.parse_literal_reg8()) |reg_index| {
        self.add_base_register(param_index, reg_index);
        self.add_signature(addr_space, .{ .reg8 = self.parse_signedness() }, .none);

    } else if (self.parse_literal_reg16()) |reg_index| {
        self.add_base_register(param_index, reg_index);
        self.add_signature(addr_space, .{ .reg16 = self.parse_signedness() }, .none);

    } else if (self.parse_literal_reg32()) |reg_index| {
        self.add_base_register(param_index, reg_index);
        self.add_signature(addr_space, .{ .reg32 = self.parse_signedness() }, .none);

    } else if (self.parse_first_placeholder_reg8()) |placeholder| {
        self.add_placeholder_base_register(param_index, placeholder);
        while (self.parse_additional_placeholder()) |p| {
            self.add_placeholder_base_register(param_index, p);
        }
        self.add_signature(addr_space, .{ .reg8 = self.parse_signedness() }, .none);

    } else if (self.parse_first_placeholder_reg16()) |placeholder| {
        self.add_placeholder_base_register(param_index, placeholder);
        while (self.parse_additional_placeholder()) |p| {
            self.add_placeholder_base_register(param_index, p);
        }
        self.add_signature(addr_space, .{ .reg16 = self.parse_signedness() }, .none);

    } else if (self.parse_first_placeholder_reg32()) |placeholder| {
        self.add_placeholder_base_register(param_index, placeholder);
        while (self.parse_additional_placeholder()) |p| {
            self.add_placeholder_base_register(param_index, p);
        }
        self.add_signature(addr_space, .{ .reg32 = self.parse_signedness() }, .none);

    } else {
        return false;
    }

    return true;
}

fn parse_offset(self: *Spec_Parser) void {
    const param_index = Parameter.Index.init(@intCast(self.temp_param_signatures.items.len - 1));

    self.skip_linespace();
    if (self.try_token(.plus)) {
        self.skip_linespace();

        if (self.parse_int_literal()) |offset| {
            self.temp_param_signatures.items[param_index.raw()].offset = .constant;
            self.add_constant(param_index, offset);

        } else if (self.parse_first_placeholder()) |placeholder| {
            self.add_placeholder_constant(param_index, placeholder);
            while (self.parse_additional_placeholder()) |p| {
                self.add_placeholder_constant(param_index, p);
            }
            self.temp_param_signatures.items[param_index.raw()].offset = .constant;

        } else if (self.parse_sr()) |sr| {
            self.temp_param_signatures.items[param_index.raw()].offset = .{ .sr = sr };

        } else if (self.parse_literal_reg8()) |reg_index| {
            self.add_offset_register(param_index, reg_index);
            self.temp_param_signatures.items[param_index.raw()].offset = .{ .reg8 = self.parse_signedness() };

        } else if (self.parse_literal_reg16()) |reg_index| {
            self.add_offset_register(param_index, reg_index);
            self.temp_param_signatures.items[param_index.raw()].offset = .{ .reg16 = self.parse_signedness() };

        } else if (self.parse_literal_reg32()) |reg_index| {
            self.add_offset_register(param_index, reg_index);
            self.temp_param_signatures.items[param_index.raw()].offset = .{ .reg32 = self.parse_signedness() };

        } else if (self.parse_first_placeholder_reg8()) |placeholder| {
            self.add_placeholder_offset_register(param_index, placeholder);
            while (self.parse_additional_placeholder()) |p| {
                self.add_placeholder_offset_register(param_index, p);
            }
            self.temp_param_signatures.items[param_index.raw()].offset = .{ .reg8 = self.parse_signedness() };

        } else if (self.parse_first_placeholder_reg16()) |placeholder| {
            self.add_placeholder_offset_register(param_index, placeholder);
            while (self.parse_additional_placeholder()) |p| {
                self.add_placeholder_offset_register(param_index, p);
            }
            self.temp_param_signatures.items[param_index.raw()].offset = .{ .reg16 = self.parse_signedness() };

        } else if (self.parse_first_placeholder_reg32()) |placeholder| {
            self.add_placeholder_offset_register(param_index, placeholder);
            while (self.parse_additional_placeholder()) |p| {
                self.add_placeholder_offset_register(param_index, p);
            }
            self.temp_param_signatures.items[param_index.raw()].offset = .{ .reg32 = self.parse_signedness() };

        } else {
            self.record_error("Expected offset constant or register");
        }

    } else if (self.token_kinds[self.next_token] == .minus) {
        if (self.parse_int_literal()) |offset| {
            self.temp_param_signatures.items[param_index.raw()].offset = .constant;
            self.add_constant(param_index, offset);
        } else {
            self.record_error("Expected offset constant");
        }
    }
}

fn parse_int_literal(self: *Spec_Parser) ?i64 {
    const begin = self.next_token;

    var negative = false;
    while (self.try_token(.minus)) {
        negative = !negative;
        self.skip_linespace();
    }
    const token = self.next_token;
    if (self.try_token(.int_literal)) {
        var remaining = self.token_location(token);
        var radix: u8 = 10;
        if (remaining.len > 2 and remaining[0] == '0') {
            switch (remaining[1]) {
                'x', 'X' => {
                    radix = 16;
                    remaining = remaining[2..];
                },
                'b', 'B' => {
                    radix = 2;
                    remaining = remaining[2..];
                },
                'q', 'Q' => {
                    radix = 4;
                    remaining = remaining[2..];
                },
                'o', 'O' => {
                    radix = 8;
                    remaining = remaining[2..];
                },
                'd', 'D' => {
                    radix = 10;
                    remaining = remaining[2..];
                },
                else => {},
            }
        }

        var raw = std.fmt.parseInt(i64, remaining, radix) catch self.record_error_abs("Invalid integer literal", token);

        if (negative) raw = -raw;

        return raw;
    }

    self.next_token = begin;
    return null;
}

fn parse_addr_space(self: *Spec_Parser) ?isa.Address_Space {
   const map = std.ComptimeStringMapWithEql(isa.Address_Space, .{
        .{ "d", .data },
        .{ "i", .insn },
        .{ "s", .stack },
    }, std.ascii.eqlIgnoreCase);

    const begin = self.next_token;
    self.skip_linespace();
    if (self.try_token(.dot) and self.try_token(.id)) {
        const str = self.token_location(self.next_token - 1);
        if (map.get(str)) |addr_space| return addr_space;
    }
    self.next_token = begin;
    return null;
}

fn parse_signedness(self: *Spec_Parser) ?Signedness {
   const map = std.ComptimeStringMapWithEql(Signedness, .{
        .{ "unsigned", .unsigned },
        .{ "signed", .signed },
    }, std.ascii.eqlIgnoreCase);

    const begin = self.next_token;
    self.skip_linespace();
    if (self.try_token(.dot) and self.try_token(.id)) {
        const str = self.token_location(self.next_token - 1);
        if (map.get(str)) |signedness| return signedness;
    }
    self.next_token = begin;
    return null;
}

fn parse_first_placeholder(self: *Spec_Parser) ?[]const u8 {
    const begin = self.next_token;

    if (self.try_token(.paren_open)) {
        self.skip_linespace();
        const placeholder_token = self.next_token;
        if (self.try_token(.id)) {
            return self.token_location(placeholder_token);
        } else {
            self.record_error("Expected placeholder name");
        }
    }

    self.next_token = begin;
    return null;
}

fn parse_additional_placeholder(self: *Spec_Parser) ?[]const u8 {
    self.skip_linespace();
    if (self.try_token(.comma)) {
        self.skip_linespace();
        const placeholder_token = self.next_token;
        if (self.try_token(.id)) {
            return self.token_location(placeholder_token);
        } else {
            self.record_error("Expected placeholder name");
        }
    } else if (self.try_token(.paren_close)) {
        return null;
    } else {
        self.record_error("Expected ')' or ','");
    }
}

fn parse_sr(self: *Spec_Parser) ?isa.Special_Register {
    const map = parse_helpers.Case_Insensitive_Enum_Map(isa.Special_Register, .{}, .{});
    const begin = self.next_token;
    self.skip_linespace();
    if (self.try_token(.id)) {
        const id = self.token_location(self.next_token - 1);
        if (map.get(id)) |sr| return sr;
    }
    self.next_token = begin;
    return null;
}

fn parse_literal_reg8(self: *Spec_Parser) ?Register_Index {
    if (self.token_kinds[self.next_token] == .id) {
        const id = self.token_location(self.next_token);
        if (id.len >= 1 and std.ascii.toLower(id[0]) == 'b') {
            const index = std.fmt.parseUnsigned(Register_Index, id[1..], 10) catch return null;
            self.next_token += 1;
            return index;
        }
    }
    return null;
}

fn parse_literal_reg16(self: *Spec_Parser) ?Register_Index {
    if (self.token_kinds[self.next_token] == .id) {
        const id = self.token_location(self.next_token);
        if (id.len >= 1 and std.ascii.toLower(id[0]) == 'r') {
            const index = std.fmt.parseUnsigned(Register_Index, id[1..], 10) catch return null;
            self.next_token += 1;
            return index;
        }
    }
    return null;
}

fn parse_literal_reg32(self: *Spec_Parser) ?Register_Index {
    if (self.token_kinds[self.next_token] == .id) {
        const id = self.token_location(self.next_token);
        if (id.len >= 1 and std.ascii.toLower(id[0]) == 'x') {
            const index = std.fmt.parseUnsigned(Register_Index, id[1..], 10) catch return null;
            self.next_token += 1;
            return index;
        }
    }
    return null;
}

fn parse_first_placeholder_reg8(self: *Spec_Parser) ?[]const u8 {
    const begin = self.next_token;
    if (self.try_token(.id)) {
        const id = self.token_location(self.next_token - 1);
        if (id.len == 1 and std.ascii.toLower(id[0]) == 'b') {
            if (self.parse_first_placeholder()) |placeholder| return placeholder;
        }
    }
    self.next_token = begin;
    return null;
}

fn parse_first_placeholder_reg16(self: *Spec_Parser) ?[]const u8 {
    const begin = self.next_token;
    if (self.try_token(.id)) {
        const id = self.token_location(self.next_token - 1);
        if (id.len == 1 and std.ascii.toLower(id[0]) == 'r') {
            if (self.parse_first_placeholder()) |placeholder| return placeholder;
        }
    }
    self.next_token = begin;
    return null;
}

fn parse_first_placeholder_reg32(self: *Spec_Parser) ?[] const u8 {
    const begin = self.next_token;
    if (self.try_token(.id)) {
        const id = self.token_location(self.next_token - 1);
        if (id.len == 1 and std.ascii.toLower(id[0]) == 'x') {
            if (self.parse_first_placeholder()) |placeholder| return placeholder;
        }
    }
    self.next_token = begin;
    return null;
}

pub fn skip_linespace(self: *Spec_Parser) void {
    _ = self.try_token(.linespace);
}

pub fn try_token(self: *Spec_Parser, kind: Token_Kind) bool {
    if (self.token_kinds[self.next_token] == kind) {
        self.next_token += 1;
        return true;
    } else {
        return false;
    }
}

pub fn record_error(self: *Spec_Parser, desc: []const u8) noreturn {
    self.record_error_abs(desc, self.next_token);
}
pub fn record_error_rel(self: *Spec_Parser, desc: []const u8, token_offset: i8) noreturn {
    self.record_error_abs(desc, @intCast(@as(i32, self.next_token) + token_offset));
}
pub fn record_error_abs(self: *Spec_Parser, desc: []const u8, token_handle: Token.Handle) noreturn {
    const token = self.tokens.get(token_handle);
    const location = token.location(self.source);
    const writer = std.io.getStdErr().writer();
    writer.writeByte('\n') catch @panic("IO Error");
    console.print_context(self.source, &.{
        .{
            .offset = token.offset,
            .len = location.len,
            .note = desc,
        },
    }, writer, 160, .{}) catch @panic("IO Error");

    @panic("Error parsing spec");
}

pub fn token_location(self: *Spec_Parser, handle: Token.Handle) []const u8 {
    return self.tokens.get(handle).location(self.source);
}

const Spec_Parser = @This();
const Token = lex.Token;
const Token_Kind = lex.Token_Kind;
const lex = assembler.lex;
const parse_helpers = assembler.parse_helpers;
const assembler = @import("lib_assembler");
const Value = Instruction_Encoding.Value;
const Constraint = Instruction_Encoding.Constraint;
const Encoder = Instruction_Encoding.Encoder;
const Instruction_Encoding = isa.Instruction_Encoding;
const Parameter = isa.Parameter;
const Mnemonic = isa.Mnemonic;
const Mnemonic_Suffix = isa.Mnemonic_Suffix;
const isa = arch.isa;
const Register_Index = arch.hw.Register_Index;
const arch = @import("lib_arch");
const Signedness = std.builtin.Signedness;
const console = @import("console");
const std = @import("std");
