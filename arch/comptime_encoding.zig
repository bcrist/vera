const std = @import("std");
const isa = @import("isa_types");
const ie = @import("isa_encoding");
const ControlSignals = @import("ControlSignals");
const ParameterEncoding = ie.ParameterEncoding;

const SpecialRegister = isa.SpecialRegister;
const AddressSpace = isa.AddressSpace;
const ConstantRange = isa.ConstantRange;

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

pub fn parameterEncodings(comptime args: anytype) []const ParameterEncoding {
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
                .EnumLiteral => param = namedParameterEncoding(@tagName(@field(args, field.name))),
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

pub fn parameterEncoding(comptime arg: anytype) ParameterEncoding {
    comptime {
        const ArgType = @TypeOf(arg);

        if (ArgType == ParameterEncoding) {
            return arg;
        } else switch (@typeInfo(ArgType)) {
            .EnumLiteral => return namedParameterEncoding(@tagName(arg)),
            .Type => return @field(arg, "param_encoding"),
            else => @compileError("I don't know what to do with argument of type " ++ @typeName(ArgType)),
        }
    }
}

pub fn addressSpaceParameterEncoding(comptime param: ParameterEncoding, comptime address_space: ?AddressSpace) ParameterEncoding {
    comptime {
        var new_param = param;
        new_param.address_space = address_space;
        return new_param;
    }
}

pub fn relativeParameterEncoding(comptime param: ParameterEncoding, comptime base: ParameterEncoding.BaseOffsetEncoding, comptime base_src: ParameterEncoding.Source) ParameterEncoding {
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

fn namedParameterEncoding(comptime name: []const u8) ParameterEncoding {
    comptime {
        if (tryRegisterParameterEncoding(name)) |param| {
            return param;
        } else if (tryImmediateParameterEncoding(name)) |param| {
            return param;
        } else if (std.mem.eql(u8, name, "to")) {
            return ParameterEncoding{
                .arrow = true,
            };
        }

        @compileError("Unrecognized parameter encoding: " ++ name);
    }
}

fn tryRegisterParameterEncoding(comptime name: []const u8) ?ParameterEncoding {
    comptime {
        if (name.len < 2) return null;

        var encoding = ParameterEncoding{};

        for (std.enums.values(SpecialRegister)) |reg| {
            if (std.mem.eql(u8, name, @tagName(reg))) {
                encoding.base = .{ .sr = reg };
                return encoding;
            }
        }

        switch (name[0]) {
            'B' => encoding.base = .{ .reg8 = .{} },
            'R' => encoding.base = .{ .reg16 = .{} },
            'X' => encoding.base = .{ .reg32 = .{} },
            else => return null,
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
        }

        if (name.len != next + 1) {
            return null;
        }

        const signedness: ?std.builtin.Signedness = switch (name[next]) {
            'U' => .unsigned,
            'S' => .signed,
            else => null,
        };

        switch (encoding.base) {
            // These can be combined when this is fixed:
            // https://github.com/ziglang/zig/issues/15504
            .reg8  => |*irr| irr.signedness = signedness,
            .reg16 => |*irr| irr.signedness = signedness,
            .reg32 => |*irr| irr.signedness = signedness,
            else => unreachable,
        }

        return encoding;
    }
}

fn tryImmediateParameterEncoding(comptime name: []const u8) ?ParameterEncoding {
    comptime {
        if (name.len < 4 or !std.mem.startsWith(u8, name, "imm")) {
            return null;
        }

        var encoding = ParameterEncoding{
            .base = .{ .constant = .{} },
        };

        var parts_iter = std.mem.split(u8, name[3..], "_");
        var part = parts_iter.next() orelse return null;

        // check for alignment suffix, e.g. /2 or /4
        if (part.len > 2 and part[part.len - 2] == '/') {
            encoding.base.constant.granularity = std.fmt.parseInt(u3, part[part.len - 1 ..], 10) catch return null;
            if (encoding.base.constant.granularity < 1) {
                return null;
            }
            part = part[0 .. part.len - 2];
        }

        // If the immediate is stored outside the opcode, is it at IP+2 or IP+4?
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
                encoding.base.constant.ranges = constantRange(min, max);
                return encoding;
            } else {
                if (min != 0) {
                    encoding.base.constant.ranges = constantRange(min, min);
                }
                return encoding;
            }
        }

        if (std.mem.eql(u8, part, "ba8u")) {
            encoding.base_src = .OB_OA;
            encoding.base.constant.ranges = constantRange(0, 255);
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

        const a: i64 = encoding.base.constant.granularity;

        if (style == 0) {
            if (parts_iter.next()) |min_str| {
                const max_str = parts_iter.next() orelse return null;
                const min = parseImmediateLiteral(min_str) catch return null;
                const max = parseImmediateLiteral(max_str) catch return null;
                if (parts_iter.next()) |final| {
                    if (std.mem.eql(u8, final, "rev")) {
                        encoding.base.constant.reverse = true;
                    } else {
                        return null;
                    }
                }
                encoding.base.constant.ranges = constantRange(min, max);
                return encoding;
            }
        } else if (parts_iter.next()) |final| {
            if (std.mem.eql(u8, final, "rev")) {
                encoding.base.constant.reverse = true;
            } else {
                return null;
            }
        }

        switch (style) {
            'u' => {
                // unsigned; positive only
                const min: i64 = 0;
                const max: i64 = (1 << n_bits) - 1;
                encoding.base.constant.ranges = constantRange(min * a, max * a);
            },
            'n' => {
                // unsigned; negative only
                const min: i64 = -(1 << n_bits);
                const max: i64 = -1;
                encoding.base.constant.ranges = constantRange(min * a, max * a);
            },
            's' => {
                // signed
                const min: i64 = -(1 << (n_bits - 1));
                const max: i64 = (1 << (n_bits - 1)) - 1;
                encoding.base.constant.ranges = signedConstantRange(min * a, max * a);
            },
            0 => {
                const min: i64 = 0;
                const max: i64 = (1 << n_bits) - 1;
                encoding.base.constant.ranges = constantRange(min * a, max * a);
                const signed_min: i64 = -(1 << (n_bits - 1));
                const signed_max: i64 = (1 << (n_bits - 1)) - 1;
                encoding.base.constant.alt_ranges = signedConstantRange(signed_min * a, signed_max * a);
            },
            else => unreachable,
        }

        return encoding;
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

fn validateParameterEncoding(comptime param: ParameterEncoding) void {
    comptime {
        if (param.base == .none and param.offset == .none) {
            @compileError("Base and offset type cannot both be none!");
        } else if (param.base == .constant) {
            validateConstantEncoding(param.base.constant, param.base_src);
        } else if (param.offset == .constant) {
            validateConstantEncoding(param.offset.constant, param.offset_src);
        }
    }
}

fn validateConstantEncoding(comptime encoding: ParameterEncoding.ConstantEncoding, src: ParameterEncoding.Source) void {
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

        min_total_values *= encoding.granularity;
        max_total_values *= encoding.granularity;

        var total_values: usize = 0;
        for (encoding.ranges) |range| {
            if (range.max < range.min) {
                @compileError(std.fmt.comptimePrint("Range minimum bound of {} must not be larger than maximum bound of {}", .{
                    range.min,
                    range.max,
                }));
            }
            total_values += range.max - range.min + encoding.granularity;
        }

        for (encoding.alt_ranges) |range| {
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
