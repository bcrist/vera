test "Instruction encoding" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    const source = isa.database_source;
    var parse_data: isa.read_database.Parser_Data = .{
        .arena = arena.allocator(),
        .temp = std.testing.allocator,
    };
    const ddb = try isa.read_database.parse_decoding_db(&parse_data, source);
    const edb = try isa.read_database.parse_encoding_db(&parse_data, source);
    parse_data.deinit();

    const buf = try std.testing.allocator.alloc(u8, 0x10000);
    defer std.testing.allocator.free(buf);

    var prng = std.rand.Xoshiro256.init(0x89fbd77dbe9f9135);
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

        var iter = edb.matching_encodings(insn);
        while (iter.next()) |encoding| {
            const encoded = encoding.encode(insn, decoder.last_instruction_data().data);
            if (std.mem.eql(u8, decoder.last_instruction, encoded.as_bytes())) break;
        } else {
            return error.EncodingNotFound;
        }
    }
}

const Encoding_Database = isa.Encoding_Database;
const Decoding_Database = isa.Decoding_Database;
const isa = arch.isa;
const arch = @import("lib_arch");
const std = @import("std");
