min: Value = .unknown_no_data,
max: Value = .unknown_no_data,

pub const Value = union (enum) {
    value: u15,
    unknown_cyclical,
    unknown_no_data,

    pub fn min(a: Value, b: Value) Value {
        return switch (a) {
            .value => |av| switch (b) {
                .value => |bv| .{ .value = @min(av, bv) },
                .unknown_cyclical => .unknown_cyclical,
                .unknown_no_data => a,
            },
            .unknown_cyclical => .unknown_cyclical,
            .unknown_no_data => b,
        };
    }

    pub fn max(a: Value, b: Value) Value {
        return switch (a) {
            .value => |av| switch (b) {
                .value => |bv| .{ .value = @max(av, bv) },
                .unknown_cyclical => .unknown_cyclical,
                .unknown_no_data => a,
            },
            .unknown_cyclical => .unknown_cyclical,
            .unknown_no_data => b,
        };
    }

    pub fn inc(a: Value) Value {
        return switch (a) {
            .value => |av| .{ .value = av + 1 },
            .unknown_cyclical => .unknown_cyclical,
            .unknown_no_data => .{ .value = 1 },
        };
    }
};

pub fn inc(a: Min_Max) Min_Max {
    return .{
        .min = a.min.inc(),
        .max = a.max.inc(),
    };
}

pub fn merge(self: *Min_Max, other: Min_Max) void {
    self.min = self.min.min(other.min);
    self.max = self.max.max(other.max);
}

const Min_Max = @This();
const std = @import("std");
