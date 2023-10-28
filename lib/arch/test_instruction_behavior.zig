const std = @import("std");
const ie = @import("isa_encoding");
const ControlSignals = @import("ControlSignals");
const uc_roms = @import("microcode_rom_serialization.zig");
const misc = @import("misc");
const Simulator = @import("Simulator");
const RegisterFile = Simulator.RegisterFile;

const expect = std.testing.expect;
const expectEqual = std.testing.expectEqual;

var arena: std.heap.ArenaAllocator = undefined;
var ddb: ie.DecoderDatabase = undefined;
var edb: ie.EncoderDatabase = undefined;
var microcode: *const [misc.microcode_length]ControlSignals = undefined;
var globals_loaded = false;

fn initSimulator(program: []const ie.Instruction) !Simulator {
    if (!globals_loaded) {
        arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        ddb = try ie.DecoderDatabase.init(arena.allocator(), std.testing.allocator);
        edb = try ie.EncoderDatabase.init(arena.allocator(), std.testing.allocator);
        var new_microcode = try arena.allocator().create([misc.microcode_length]ControlSignals);
        uc_roms.readCompressedRoms(@import("microcode_roms").compressed_data, new_microcode);
        microcode = new_microcode;
        globals_loaded = true;
    }

    var program_data = try std.testing.allocator.alloc(u8, 256);
    defer std.testing.allocator.free(program_data);

    var encoder = ie.Encoder.init(program_data);
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

    var s = try Simulator.init(std.testing.allocator, microcode);

    var flash = s.memory.flashIterator(0x7E_000 * 8);
    _ = flash.writeAll(std.mem.asBytes(&vector_table));
    _ = flash.writeAll(program_data);

    s.register_file.setAllZeroes();

    s.resetAndInit();
    return s;
}

fn deinitSimulator(simulator: *Simulator) void {
    simulator.deinit(std.testing.allocator);
}

test "ADD X12, -128 -> X1" {
    var s = try initSimulator(&[_]ie.Instruction{
        .{
            .mnemonic = .ADD,
            .suffix = .none,
            .params = &[_]ie.Parameter{
                ie.parameter(.reg32, 12),
                ie.parameter(.constant, -128),
                ie.toParameter(.reg32, 1),
            },
        },
    });
    defer deinitSimulator(&s);

    s.cycle(2);
    try expectEqual(@as(u32, 0xFFFFFF80), s.register_file.readGPR32(0, 1));
    try expect(s.t.reg.stat.n);
    try expect(!s.t.reg.stat.c);
    try expect(!s.t.reg.stat.v);
    try expect(!s.t.reg.stat.z);

    s.resetAndInit();
    s.register_file.writeGPR32(0, 12, 123456);
    s.cycle(2);
    try expectEqual(@as(u32, 123328), s.register_file.readGPR32(0, 1));
    try expect(!s.t.reg.stat.n);
    try expect(s.t.reg.stat.c);
    try expect(!s.t.reg.stat.v);
    try expect(!s.t.reg.stat.z);

    s.resetAndInit();
    s.register_file.writeGPR32(0, 12, 128);
    //try s.debugCycle(2, .one);
    s.cycle(2);
    try expectEqual(@as(u32, 0), s.register_file.readGPR32(0, 1));
    try expect(!s.t.reg.stat.n);
    try expect(s.t.reg.stat.c);
    try expect(!s.t.reg.stat.v);
    try expect(s.t.reg.stat.z);
}

test "ADD X0, R4U -> X0" {
    var s = try initSimulator(&[_]ie.Instruction{
        .{
            .mnemonic = .ADD,
            .suffix = .none,
            .params = &[_]ie.Parameter{
                ie.parameter(.reg32, 7),
                ie.parameter(.reg16u, 4),
                ie.toParameter(.reg32, 7),
            },
        },
    });
    defer deinitSimulator(&s);

    s.register_file.writeGPR32(0, 7, 0);
    s.register_file.writeGPR(0, 4, 33000);
    s.cycle(1);
    try expectEqual(@as(u32, 33000), s.register_file.readGPR32(0, 7));
    try expect(!s.t.reg.stat.n);
    try expect(!s.t.reg.stat.c);
    try expect(!s.t.reg.stat.v);
    try expect(!s.t.reg.stat.z);

    s.resetAndInit();
    s.cycle(1);
    try expectEqual(@as(u32, 66000), s.register_file.readGPR32(0, 7));
    try expect(!s.t.reg.stat.n);
    try expect(!s.t.reg.stat.c);
    try expect(!s.t.reg.stat.v);
    try expect(!s.t.reg.stat.z);
}

test "ADD X1, R3S, X1" {
    var s = try initSimulator(&[_]ie.Instruction{
        .{
            .mnemonic = .ADD,
            .suffix = .none,
            .params = &[_]ie.Parameter{
                ie.parameter(.reg32, 7),
                ie.parameter(.reg16s, 3),
                ie.toParameter(.reg32, 7),
            },
        },
    });
    defer deinitSimulator(&s);

    s.register_file.writeSignedGPR(0, 3, @as(i16, -32000));
    s.cycle(1);
    try expectEqual(@as(i32, -32000), s.register_file.readSignedGPR32(0, 7));
    try expect(s.t.reg.stat.n);
    try expect(!s.t.reg.stat.c);
    try expect(!s.t.reg.stat.v);
    try expect(!s.t.reg.stat.z);

    s.resetAndInit();
    s.cycle(1);
    try expectEqual(@as(i32, -64000), s.register_file.readSignedGPR32(0, 7));
    try expect(s.t.reg.stat.n);
    try expect(s.t.reg.stat.c);
    try expect(!s.t.reg.stat.v);
    try expect(!s.t.reg.stat.z);
}

test "ADDC R5, 12345, R4" {
    var s = try initSimulator(&[_]ie.Instruction{.{
        .mnemonic = .ADDC,
        .suffix = .none,
        .params = &[_]ie.Parameter{
            ie.parameter(.reg16, 5),
            ie.parameter(.constant, 12345),
            ie.toParameter(.reg16, 4),
        },
    }});
    defer deinitSimulator(&s);
    s.t.reg.stat.c = true;
    s.cycle(2);
    try expectEqual(@as(u32, 12346), s.register_file.readGPR(0, 4));
    try expect(!s.t.reg.stat.n);
    try expect(!s.t.reg.stat.c);
    try expect(!s.t.reg.stat.v);
    try expect(!s.t.reg.stat.z);
}
