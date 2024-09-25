pub fn main() !void {
    var ps: microsim.Pipeline_State = undefined;
    _ = &ps;

    std.process.exit(1);
}

const microsim = @import("microsim");
const std = @import("std");
