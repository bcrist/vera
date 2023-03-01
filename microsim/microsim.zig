const std = @import("std");
const Simulator = @import("Simulator");
const uc_roms = @import("microcode_rom_serialization");
const ControlSignals = @import("ControlSignals");
const ie = @import("instruction_encoding");
const misc = @import("misc");

const roms = @import("microcode_roms").compressed_data;
const instruction_encoding_data = @import("instruction_encoding_data").data;

pub fn main() !void {
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const microcode = try arena.allocator().alloc(ControlSignals, misc.microcode_length);
    uc_roms.readCompressedRoms(roms, microcode);

    var temp = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const edb = try ie.EncoderDatabase.init(arena.allocator(), instruction_encoding_data, temp.allocator());
    temp.deinit();

    var program = [_]ie.Instruction {
        .{
            .mnemonic = .B,
            .suffix = .none,
            .params = &[_]ie.Parameter{
                ie.parameter(.constant, 0xFFFF_0002),
            },
        },
    };

    var program_data = [_]u8{0xFF} ** 65536;
    var encoder = ie.Encoder.init(&program_data);
    for (program) |insn| {
        var insn_iter = edb.getMatchingEncodings(insn);
        try encoder.encode(insn, insn_iter.next().?);
    }

    const vector_table = misc.ZeropageVectorTable{
        .double_fault = 0xFFFE,
        .page_fault = 0xFFFD,
        .access_fault = 0xFFFC,
        .page_align_fault = 0xFFFB,
        .instruction_protection_fault = 0xFFFA,
        .invalid_instruction = 0xFFF9,
        .pipe_0_reset = @sizeOf(misc.ZeropageVectorTable),
    };

    var sim = try Simulator.init(arena.allocator(), microcode);
    var xo = std.rand.Xoshiro256.init(12345);
    sim.randomizeState(xo.random());

    var flash = sim.memory.flashIterator(0x7E_000 * 8);
    _ = flash.writeAll(std.mem.asBytes(&vector_table));
    _ = flash.writeAll(&program_data);

    sim.resetAndStart();
    try sim.printState(stdout, .zero);

    while (true) {
        switch (try stdin.readByte()) {
            'q' => return,
            '\n' => {
                sim.cycle(1);
                try sim.printState(stdout, .zero);
            },
            else => {},
        }
    }
}
