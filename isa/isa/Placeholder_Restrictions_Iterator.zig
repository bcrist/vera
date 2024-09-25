encoding: Instruction_Encoding,
placeholder: Placeholder_Info,
state: State,
temp: [256]u8,

pub const State = union (enum) {
    initial,
    domain: usize,
    constraint: usize,
    done,
};

pub const Restriction = struct {
    left: []const u8,
    right: union (enum) {
        in_range: struct {
            first: i64,
            last: i64,
            multiple: u8,
        },
        in_set: []const i64,
        equal: []const u8,
        not_equal: []const u8,
        greater: []const u8,
        greater_or_equal: []const u8,
    },
};

pub fn init(encoding: Instruction_Encoding, placeholder: Placeholder_Info) Placeholder_Restrictions_Iterator {
    return .{
        .encoding = encoding,
        .placeholder = placeholder,
        .state = .initial,
        .temp = undefined,
    };
}

pub fn next(self: *Placeholder_Restrictions_Iterator) !?Restriction {
    const starting_constraint_index = switch (self.state) {
        .initial => ci: {
            if (self.find_domain_index(0)) |domain_index| {
                self.state = .{ .domain = domain_index };
                return try self.domain_restriction(domain_index);
            }
            break :ci 0;
        },
        .domain => |last_domain_index| ci: {
            if (self.find_domain_index(last_domain_index + 1)) |domain_index| {
                self.state = .{ .domain = domain_index };
                return try self.domain_restriction(domain_index);
            }
            break :ci 0;
        },
        .constraint => |last_constraint_index| last_constraint_index + 1,
        .done => return null,
    };

    if (self.find_constraint_index(starting_constraint_index)) |constraint_index| {
        self.state = .{ .constraint = constraint_index };
        return try self.constraint_restriction(constraint_index);
    }

    self.state = .done;
    return null;
}

fn find_domain_index(self: Placeholder_Restrictions_Iterator, start: usize) ?usize {
    for (self.encoding.encoders[start..], 0..) |enc, n| {
        if (enc.value.get_placeholder_info()) |pi| {
            if (pi.index == self.placeholder.index and pi.kind == self.placeholder.kind and std.mem.eql(u8, pi.name, self.placeholder.name)) {
                return n + start;
            }
        }
    }
    return null;
}

fn domain_restriction(self: *Placeholder_Restrictions_Iterator, domain_index: usize) !Restriction {
    const encoder = self.encoding.encoders[domain_index];
    return .{
        .left = try render_placeholder(&self.temp, encoder.value, false),
        .right = switch (encoder.domain) {
            .int => |info| .{ .in_range = .{
                .first = int_min(info.signedness, info.bits, info.multiple),
                .last = int_max(info.signedness, info.bits, info.multiple),
                .multiple = info.multiple,
            }},
            .range => |range| .{ .in_range = .{
                .first = @min(range.first, range.last),
                .last = @max(range.first, range.last),
                .multiple = 1,
            }},
            .enumerated => |options| .{ .in_set = options },
        },
    };
}

fn find_constraint_index(self: Placeholder_Restrictions_Iterator, start: usize) ?usize {
    for (self.encoding.constraints[start..], 0..) |constraint, n| {
        if (constraint.left.get_placeholder_info()) |pi| {
            if (pi.index == self.placeholder.index and pi.kind == self.placeholder.kind and std.mem.eql(u8, pi.name, self.placeholder.name)) {
                return n + start;
            }
        }
    }
    return null;
}

fn constraint_restriction(self: *Placeholder_Restrictions_Iterator, constraint_index: usize) !Restriction {
    const constraint = self.encoding.constraints[constraint_index];
    const left = try render_placeholder(self.temp[0..128], constraint.left, false);
    const right = try render_placeholder(self.temp[128..], constraint.right, false);
    return .{
        .left = left,
        .right = switch (constraint.kind) {
            .equal => .{ .equal = right },
            .not_equal => .{ .not_equal = right },
            .greater => .{ .greater = right },
            .greater_or_equal => .{ .greater_or_equal = right },
        },
    };
}

fn render_placeholder(buf: []u8, value: Value, negate: bool) ![]const u8 {
    switch (value) {
        .constant => |v| {
            const final_v = if (negate) -v else v;
            return print.buf_print_constant(buf, final_v);
        },
        .placeholder => |info| {
            if (negate) {
                return std.fmt.bufPrint(buf, "-({s})", .{ info.name });
            } else {
                return std.fmt.bufPrint(buf, "{s}", .{ info.name });
            }
        },
        .negate => |inner| {
            return render_placeholder(buf, inner.*, !negate);
        },
        .offset => |info| {
            const prefix = try render_placeholder(buf, info.inner.*, negate);
            const remaining = buf[prefix.len..];
            const final_offset = if (negate) -info.offset else info.offset;
            const offset_str = try print.buf_print_offset(remaining, final_offset);
            return buf[0 .. prefix.len + offset_str.len];
        },
    }
}

fn int_max(signedness: std.builtin.Signedness, bit_count: u6, multiple: u8) i64 {
    if (bit_count == 0) return 0;
    const non_sign_bits = bit_count - @intFromBool(signedness == .signed);
    const raw = (@as(i64, 1) << non_sign_bits) - 1;
    return raw * multiple;
}

fn int_min(signedness: std.builtin.Signedness, bit_count: u6, multiple: u8) i64 {
    if (signedness == .unsigned) return 0;
    if (bit_count == 0) return 0;
    const non_sign_bits = bit_count - 1;
    const raw = -(@as(i64, 1) << non_sign_bits);
    return raw * multiple;
}

const Placeholder_Restrictions_Iterator = @This();
const Value = Instruction_Encoding.Value;
const Domain = Instruction_Encoding.Domain;
const Placeholder_Info = Instruction_Encoding.Placeholder_Info;
const Instruction_Encoding = @import("Instruction_Encoding.zig");
const print = @import("print.zig");
const std = @import("std");
