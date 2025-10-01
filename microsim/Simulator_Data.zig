ddb: iedb.Decoding_Database,
edb: iedb.Encoding_Database,
insn_decode: *const arch.insn_decode.Rom,
microcode: *const arch.microcode.Rom,

threadlocal var data: ?Simulator_Data = null;

pub fn get() !*const Simulator_Data {
    if (data == null) {
        // N.B. we're intentionally leaking any memory allocated by this arena;
        // Simulator_Data never gets disposed once it's been initialized for a
        // thread, so there's no reason to keep the allocator around.
        var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);

        var temp = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        defer temp.deinit();

        const insn_decode = try arena.allocator().create(arch.insn_decode.Rom);
        arch.insn_decode.read_compressed_rom(insn_decode_data, insn_decode);

        const microcode = try arena.allocator().create(arch.microcode.Rom);
        arch.microcode.read_compressed_rom(arch.microcode.Setup_Microcode_Entry, setup_uc_data, microcode);
        arch.microcode.read_compressed_rom(arch.microcode.Compute_Microcode_Entry, compute_uc_data, microcode);
        arch.microcode.read_compressed_rom(arch.microcode.Transact_Microcode_Entry, transact_uc_data, microcode);
        arch.microcode.read_compressed_rom(arch.microcode.Decode_Microcode_Entry, decode_uc_data, microcode);

        data = .{
            .edb = try .init(arena.allocator()),
            .ddb = try .init(arena.allocator(), temp.allocator()),
            .insn_decode = insn_decode,
            .microcode = microcode,
        };
    }

    return &(data.?);
}

const insn_decode_data = @embedFile("insn_decode.crom");
const setup_uc_data = @embedFile("setup_uc.crom");
const compute_uc_data = @embedFile("compute_uc.crom");
const transact_uc_data = @embedFile("transact_uc.crom");
const decode_uc_data = @embedFile("decode_uc.crom");

const Simulator_Data = @This();

const iedb = @import("iedb");
const arch = @import("arch");
const std = @import("std");