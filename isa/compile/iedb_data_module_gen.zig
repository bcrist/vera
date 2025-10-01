pub fn write_module(allocator: std.mem.Allocator, forms: []const isa.Instruction.Form, writer: *std.io.Writer) !void {
    const named_mnemonics = isa.lex.case_insensitive_enum_map(isa.Mnemonic, .{}, .{});
    
    var param_signatures: std.ArrayList(isa.Parameter.Signature) = .empty;
    defer param_signatures.deinit(allocator);

    var constraints: std.ArrayList(isa.Constraint) = .empty;
    defer constraints.deinit(allocator);

    var encoders: std.ArrayList(isa.Encoder) = .empty;
    defer encoders.deinit(allocator);

    var enumerated_domains: std.ArrayList(i64) = .empty;
    defer enumerated_domains.deinit(allocator);

    var loose_values: std.ArrayList(isa.Encoder.Value) = .empty;
    defer loose_values.deinit(allocator);

    try writer.print(
        \\pub const mnemonics: [{d}]isa.Mnemonic = .{{
    , .{
        forms.len,
    });
    for (forms) |form| {
        const mnemonic = form.signature.mnemonic.name();
        if (named_mnemonics.has(mnemonic)) {
            try writer.print("\n    .{f},", .{ std.zig.fmtId(mnemonic) });
        } else {
            try writer.print("\n    .init(\"{f}\"),", .{ std.zig.fmtString(mnemonic) });
        }
    }
    try writer.print(
        \\
        \\}};
        \\
        \\pub const param_ranges: [{d}]Compact_Range = .{{
    , .{
        forms.len,
    });
    for (forms) |form| {
        const params = form.signature.params;

        const offset = indexOf(isa.Parameter.Signature, param_signatures.items, params) orelse offset: {
            const offset = param_signatures.items.len;
            try param_signatures.appendSlice(allocator, params);
            break :offset offset;
        };

        try writer.print("\n    .{{ .offset = {}, .len = {} }},", .{
            offset,
            params.len,
        });
    }
    try writer.print(
        \\
        \\}};
        \\
        \\pub const param_signatures: [{d}]isa.Parameter.Signature = .{{
    , .{
        param_signatures.items.len,
    });
    for (param_signatures.items) |signature| {
        try writer.writeAll("\n    .{ .base = ");
        try write_param_kind(signature.base, writer);
        try writer.writeAll(", .offset = ");
        try write_param_kind(signature.base, writer);
        try writer.writeAll(", .address_space = ");

        if (signature.address_space) |space| {
            try writer.print(".{t}", .{ space });
        } else {
            try writer.writeAll("null");
        }
        try writer.writeAll(" },");
    }
    try writer.print(
        \\
        \\}};
        \\
        \\pub const constraint_ranges: [{d}]Compact_Range = .{{
    , .{
        forms.len,
    });
    for (forms) |form| {
        const offset = indexOf(isa.Constraint, constraints.items, form.constraints) orelse offset: {
            const offset = constraints.items.len;
            try constraints.appendSlice(allocator, form.constraints);
            break :offset offset;
        };

        try writer.print("\n    .{{ .offset = {}, .len = {} }},", .{
            offset,
            form.constraints.len,
        });
    }
    try writer.print(
        \\
        \\}};
        \\
        \\pub const constraints: [{d}]isa.Constraint = .{{
    , .{
        constraints.items.len,
    });
    for (constraints.items) |constraint| {
        const ctx: Loose_Values = .{ .allocator = allocator, .loose_values = &loose_values };
        try writer.writeAll(
            \\
            \\    .{
            \\        .left = 
        );
        try write_encoder_value(constraint.left, ctx, writer);
        try writer.print(
            \\,
            \\        .kind = .{t},
            \\        .right = 
            , .{ constraint.kind });
        try write_encoder_value(constraint.right, ctx, writer);
        try writer.writeAll(
            \\,
            \\    },
        );
    }
    try writer.print(
        \\
        \\}};
        \\
        \\pub const encoder_ranges: [{d}]Compact_Range = .{{
    , .{
        forms.len,
    });
    for (forms) |form| {
        const offset = indexOf(isa.Encoder, encoders.items, form.encoders) orelse offset: {
            const offset = encoders.items.len;
            try encoders.appendSlice(allocator, form.encoders);
            break :offset offset;
        };

        try writer.print("\n    .{{ .offset = {}, .len = {} }},", .{
            offset,
            form.encoders.len,
        });
    }
    try writer.print(
        \\
        \\}};
        \\
        \\pub const encoders: [{d}]isa.Encoder = .{{
    , .{
        encoders.items.len,
    });
    for (encoders.items) |encoder| {
        const ctx: Loose_Values = .{ .allocator = allocator, .loose_values = &loose_values };
        try writer.print(
            \\
            \\    .{{
            \\        .bit_offset = {d},
            \\        .bit_count = {d},
            \\        .value = 
        , .{
            encoder.bit_offset,
            encoder.bit_count,
        });
        try write_encoder_value(encoder.value, ctx, writer);
        try writer.writeAll(
            \\,
            \\        .domain = 
        );
        try write_encoder_domain(allocator, &enumerated_domains, encoder.domain, writer);
        try writer.writeAll(
            \\,
            \\    },
        );
    }
    try writer.print(
        \\
        \\}};
        \\
        \\const values: [{d}]isa.Encoder.Value = .{{
    , .{
        loose_values.items.len,
    });
    for (loose_values.items) |value| {
        try writer.writeAll("\n    ");
        try write_encoder_value(value, null, writer);
        try writer.writeByte(',');
    }
    try writer.print(
        \\
        \\}};
        \\
        \\const enumerated_domains: [{d}]i64 = .{{
    , .{
        enumerated_domains.items.len,
    });
    for (enumerated_domains.items) |value| {
        try writer.print("\n    {d},", .{ value });
    }
    try writer.writeAll(
        \\
        \\};
        \\
        \\pub const Compact_Range = struct {
        \\    offset: u16,
        \\    len: u16,
        \\};
        \\
        \\const isa = @import("isa");
        \\const std = @import("std");
        \\
    );
}

fn write_param_kind(kind: isa.Parameter.Kind, writer: *std.io.Writer) !void {
    switch (kind) {
        .none, .constant => {
            try writer.writeByte('.');
            try writer.writeAll(@tagName(kind));
        },
        .reg => |maybe_sign| {
            if (maybe_sign) |sign| {
                try writer.print(".{{ .reg = .{t} }}", .{ sign });
            } else {
                try writer.writeAll(".{ .reg = null }");
            }
        },
        .sr => |sr| {
            try writer.print(".{{ .sr = .{t} }}", .{ sr });
        },
    }
}

fn write_encoder_value(value: isa.Encoder.Value, maybe_loose_values: ?Loose_Values, writer: *std.io.Writer) !void {
    try writer.print(".{{ .{t} = ", .{ value });

    switch (value) {
        .constant => |c| {
            try writer.print("{d}", .{ c });
        },
        .placeholder => |info| {
            try write_placeholder(info, writer);
        },
        .negate => |loose_value| {
            try write_loose_value(loose_value, maybe_loose_values, writer);
        },
        .xor => |info| {
            try writer.print(".{{ .mask = 0x{X}, .inner = ", .{ info.mask });
            try write_loose_value(info.inner, maybe_loose_values, writer);
            try writer.writeAll(" }");
        },
        .offset => |info| {
            try writer.print(".{{ .offset = {d}, .inner = ", .{ info.offset });
            try write_loose_value(info.inner, maybe_loose_values, writer);
            try writer.writeAll(" }");
        },
    }

    try writer.writeAll(" }");
}

const Loose_Values = struct {
    allocator: std.mem.Allocator,
    loose_values: *std.ArrayList(isa.Encoder.Value),
};
fn write_loose_value(value: *const isa.Encoder.Value, maybe_loose_values: ?Loose_Values, writer: *std.io.Writer) error{WriteFailed,OutOfMemory}!void {
    if (maybe_loose_values) |v| {
        const offset = indexOfScalarPos(isa.Encoder.Value, v.loose_values.items, 0, value.*) orelse offset: {
            const offset = v.loose_values.items.len;
            try v.loose_values.append(v.allocator, value.*);
            break :offset offset;
        };
        try writer.print("&values[{d}]", .{ offset });
    } else {
        try writer.writeByte('&');
        try write_encoder_value(value.*, null, writer);
    }
}

fn write_placeholder(placeholder: isa.Placeholder, writer: *std.io.Writer) !void {
    try writer.print(".{{ .name = \"{f}\", .kind = .{t}, .param = .init({d}) }}", .{
        std.zig.fmtString(placeholder.name),
        placeholder.kind,
        placeholder.param.raw(),
    });
}

fn write_encoder_domain(allocator: std.mem.Allocator, enumerated_domains: *std.ArrayList(i64), domain: isa.Encoder.Domain, writer: *std.io.Writer) !void {
    try writer.print(".{{ .{t} = ", .{ domain });

    switch (domain) {
        .int => |info| {
            try writer.print(".{{ .signedness = .{t}, .bits = {d}, .multiple = {d} }}", .{
                info.signedness,
                info.bits,
                info.multiple,
            });
        },
        .range => |info| {
            try writer.print(".{{ .first = {d}, .last = {d} }}", .{ info.first, info.last });
        },
        .enumerated => |values| {
            const offset = std.mem.indexOf(i64, enumerated_domains.items, values) orelse offset: {
                const offset = enumerated_domains.items.len;
                try enumerated_domains.appendSlice(allocator, values);
                break :offset offset;
            };
            try writer.print("enumerated_domains[{d}..{d}]", .{
                offset,
                offset + values.len,
            });
        },
    }

    try writer.writeAll(" }");
}


// TODO move to `meta` module?
// copied from std.mem - but uses eql() method instead of ==
pub fn indexOf(comptime T: type, haystack: []const T, needle: []const T) ?usize {
    return indexOfPos(T, haystack, 0, needle);
}

// copied from std.mem - but uses eql() method instead of ==
pub fn indexOfPos(comptime T: type, haystack: []const T, start_index: usize, needle: []const T) ?usize {
    if (needle.len > haystack.len) return null;
    if (needle.len < 2) {
        if (needle.len == 0) return start_index;
        // indexOfScalarPos is significantly faster than indexOfPosLinear
        return indexOfScalarPos(T, haystack, start_index, needle[0]);
    }

    return indexOfPosLinear(T, haystack, start_index, needle);
}

// copied from std.mem - but uses eql() method instead of ==
pub fn indexOfScalarPos(comptime T: type, slice: []const T, start_index: usize, value: T) ?usize {
    if (start_index >= slice.len) return null;
    for (slice[start_index..], start_index..) |c, j| {
        if (c.eql(value)) return j;
    }
    return null;
}

// copied from std.mem - but uses eql() method instead of ==
pub fn indexOfPosLinear(comptime T: type, haystack: []const T, start_index: usize, needle: []const T) ?usize {
    if (needle.len > haystack.len) return null;
    var i: usize = start_index;
    const end = haystack.len - needle.len;
    while (i <= end) : (i += 1) {
        for (haystack[i..][0..needle.len], needle) |a, b| {
            if (!a.eql(b)) break;
        } else return i;
    }
    return null;
}

const isa = @import("isa");
const std = @import("std");
