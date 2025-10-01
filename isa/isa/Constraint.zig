left: Encoder.Value,
right: Encoder.Value,
kind: Kind,

pub const Kind = enum {
    equal,
    not_equal,
    greater,
    greater_or_equal,
};

pub fn matches(self: Constraint, params: []const Parameter) bool {
    const left = self.left.evaluate(params);
    const right = self.right.evaluate(params);
    return switch (self.kind) {
        .equal => left == right,
        .not_equal => left != right,
        .greater => left > right,
        .greater_or_equal => left >= right,
    };
}

pub fn eql(a: Constraint, b: Constraint) bool {
    return a.kind == b.kind and a.left.eql(b.left) and a.right.eql(b.right);
}

const Constraint = @This();

const Parameter = @import("Parameter.zig");
const Encoder = @import("Encoder.zig");