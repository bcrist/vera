const std = @import("std");
const parse = @import("parse.zig");
const ie = @import("instruction_encoding");

const ParseResult = parse.ParseResult;

pub fn inferTypes(data: *ParseResult) !void {
    _ = data;
}

pub fn inferEncodings(data: *ParseResult, alloc: std.mem.Allocator, edb: ie.EncoderDatabase, origin_address: u32) !void {
    // also infers instruction lengths and addresses

    while (true) {
        var try_again = false;
        var missing_encoding = false;

        var ip = origin_address;
        for (data.instructions.items) |*insn| {
            // TODO handle labels
            // TODO handle directives
            if (insn.mnemonic != ._reserved) {
                var encoding_iter = edb.getMatchingEncodings(.{
                    .mnemonic = insn.mnemonic,
                    .suffix = insn.suffix,
                    .params = &.{}, // TODO params
                });
                const old_length = insn.length;
                insn.encoding = null;
                while (encoding_iter.next()) |enc| {
                    const length = ie.getInstructionLength(enc);
                    const use_this_encoding = if (insn.length) |cur_length| length < cur_length else true;
                    if (use_this_encoding) {
                        insn.encoding = enc;
                        insn.length = length;
                        insn.address = ip;
                    }
                }
                if (insn.encoding == null) {
                    try data.errors.append(alloc, .{
                        .token = insn.token,
                        .desc = "There is no possible encoding for this instruction",
                    });
                    missing_encoding = true;
                }
                if (std.meta.eql(old_length, insn.length)) {
                    try_again = true;
                }
                if (insn.length) |length| {
                    ip += length;
                }
            }
        }

        if (!try_again or missing_encoding) break;
    }
}
