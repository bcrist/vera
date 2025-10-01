test "Instruction encoding" {
    var ddb = try iedb.Decoding_Database.init(std.testing.allocator, std.testing.allocator);
    defer ddb.deinit(std.testing.allocator);

    var edb = try iedb.Encoding_Database.init(std.testing.allocator);
    defer edb.deinit(std.testing.allocator);

    const buf = try std.testing.allocator.alloc(u8, 0x10000);
    defer std.testing.allocator.free(buf);

    var prng = std.Random.Xoshiro256.init(0x89fbd77dbe9f9135);
    prng.fill(buf);

    var decoder = ddb.decoder(null, buf);
    while (true) {
        const insn = decoder.decode() catch {
            decoder.remaining = decoder.remaining[1..];
            continue;
        } orelse break;

        // const stderr = std.io.getStdErr().writer();
        // try stderr.print("{} ", .{ std.fmt.fmtSliceHexUpper(decoder.last_instruction) });
        // try isa.print.print_instruction(insn, null, stderr);
        // try stderr.writeByte('\n');

        var iter = edb.find_matches(insn);
        while (iter.next()) |encoding| {
            const encoded = encoding.encode(insn, decoder.last_encoded_instruction().data);
            if (std.mem.eql(u8, decoder.last_instruction, encoded.as_bytes())) break;
        } else {
            return error.EncodingNotFound;
        }
    }
}

const iedb = @import("iedb");
const isa = @import("isa");
const arch = @import("arch");
const std = @import("std");
