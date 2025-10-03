form: Instruction.Form,
placeholder: Placeholder,
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

pub fn init(form: Instruction.Form, placeholder: Placeholder) Placeholder_Restrictions_Iterator {
    return .{
        .form = form,
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
    var encoder_iter = self.form.encoders();
    var n: usize = 0;
    while (encoder_iter.next()) |enc| {
        defer n += 1;
        if (n < start) continue;
        if (enc.value.get_placeholder()) |pi| {
            if (pi.param == self.placeholder.param and pi.kind == self.placeholder.kind and std.mem.eql(u8, pi.name, self.placeholder.name)) {
                return n + start;
            }
        }
    }
    return null;
}

fn domain_restriction(self: *Placeholder_Restrictions_Iterator, domain_index: usize) !Restriction {
    var encoder_iter = self.form.encoders();
    for (0..domain_index) |_| _ = encoder_iter.next();
    const encoder = encoder_iter.next().?;
    var writer = std.io.Writer.fixed(&self.temp);
    try render_placeholder(encoder.value, false, &writer);
    return .{
        .left = writer.buffered(),
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
    for (self.form.constraints[start..], 0..) |constraint, n| {
        if (constraint.left.get_placeholder()) |pi| {
            if (pi.param == self.placeholder.param and pi.kind == self.placeholder.kind and std.mem.eql(u8, pi.name, self.placeholder.name)) {
                return n + start;
            }
        }
    }
    return null;
}

fn constraint_restriction(self: *Placeholder_Restrictions_Iterator, constraint_index: usize) !Restriction {
    const constraint = self.form.constraints[constraint_index];

    var writer = std.io.Writer.fixed(&self.temp);
    try render_placeholder(constraint.left, false, &writer);
    const left = writer.buffered();

    writer = std.io.Writer.fixed(self.temp[left.len..]);
    try render_placeholder(constraint.right, false, &writer);
    const right = writer.buffered();

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

fn render_placeholder(value: Encoder.Value, negate: bool, writer: *std.io.Writer) !void {
    switch (value) {
        .constant => |v| {
            const final_v = if (negate) -v else v;
            try fmt.print_constant(final_v, writer);
        },
        .placeholder => |info| {
            if (negate) {
                return writer.print("-({s})", .{ info.name });
            } else {
                return writer.print("{s}", .{ info.name });
            }
        },
        .negate => |inner| {
            return render_placeholder(inner.*, !negate, writer);
        },
        .offset => |info| {
            try render_placeholder(info.inner.*, negate, writer);
            try writer.writeByte(' ');
            try fmt.print_offset(if (negate) -info.offset else info.offset, writer);
        },
        .xor => |info| {
            if (negate) try writer.writeAll("-(");
            try render_placeholder(info.inner.*, false, writer);
            try writer.writeAll(" ^ ");
            try fmt.print_constant(info.mask, writer);
            if (negate) try writer.writeAll(")");
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

const Encoder = @import("../Encoder.zig");
const Placeholder = @import("../Placeholder.zig");
const Instruction = @import("../Instruction.zig");
const fmt = @import("../fmt.zig");
const std = @import("std");
