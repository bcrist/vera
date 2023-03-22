const std = @import("std");
const parse = @import("parse.zig");

const ParseResult = parse.ParseResult;

pub fn inferTypes(data: *ParseResult) !void {
    _ = data;
}

pub fn inferInstructionEncodings(data: *ParseResult) !void {
    // also infers instruction lengths and addresses

    while (true) {
        var try_again = false;

        
        for (data.instructions.items) |*insn| {
            _ = insn;
        }

        inferAddresses(data);

        if (!try_again) break;
    }
}

pub fn inferAddresses(data: *ParseResult, origin_address: u32) std.StringHashMap(u32) {
    // TODO .org/.section directive
    _ = data;
    _ = origin_address;
}
