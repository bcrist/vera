const std = @import("std");
const ie = @import("instruction_encoding");
const ctrl = @import("control_signals");
const uc_roms = @import("microcode_rom_serialization.zig");
const sim = @import("simulator");
const register_file = @import("register_file");
const misc = @import("misc");

const rom_data = @import("microcode_roms/roms.zig");
const ie_data = @import("instruction_encoding_data").data;

const expect = std.testing.expect;
const expectEqual = std.testing.expectEqual;

var arena: std.heap.ArenaAllocator = undefined;
var ddb: ie.DecoderDatabase = undefined;
var edb: ie.EncoderDatabase = undefined;
var microcode: []ctrl.Control_Signals = undefined;
var globals_loaded = false;

fn initSimulator(program: []const ie.Instruction) !sim.Simulator {
    if (!globals_loaded) {
        arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        ddb = try ie.DecoderDatabase.init(arena.allocator(), ie_data, std.testing.allocator);
        edb = try ie.EncoderDatabase.init(arena.allocator(), ie_data, std.testing.allocator);
        microcode = try arena.allocator().alloc(ctrl.Control_Signals, misc.microcode_length);
        uc_roms.readCompressedRoms(rom_data.compressed_data, microcode);
        globals_loaded = true;
    }

    var program_data = try std.testing.allocator.alloc(u8, 256);
    defer std.testing.allocator.free(program_data);

    var encoder = ie.Encoder.init(program_data);
    for (program) |insn| {
        var insn_iter = edb.getMatchingEncodings(insn);
        try encoder.encode(insn, insn_iter.next().?);
    }

    const vector_table = misc.Zeropage_Vector_Table{
        .double_fault = 0xFFFE,
        .page_fault = 0xFFFD,
        .access_fault = 0xFFFC,
        .page_align_fault = 0xFFFB,
        .instruction_protection_fault = 0xFFFA,
        .invalid_instruction = 0xFFF9,
        .pipe_0_reset = @sizeOf(misc.Zeropage_Vector_Table),
    };

    var s = try sim.Simulator.init(std.testing.allocator, microcode);

    var flash = s.memory.flashIterator(0x7E_000 * 8);
    _ = flash.writeAll(std.mem.asBytes(&vector_table));
    _ = flash.writeAll(program_data);

    s.resetAndStart();
    return s;
}

fn deinitSimulator(simulator: *sim.Simulator) void {
    simulator.deinit();
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
    var rv = register_file.RegisterView.init(s.reg_file, s.s.reg.rsn);

    s.cycle(2);
    try expectEqual(@as(u32, 0xFFFFFF80), rv.readGPR32(1));
    try expect(s.s.reg.stat.n);
    try expect(!s.s.reg.stat.c);
    try expect(!s.s.reg.stat.v);
    try expect(!s.s.reg.stat.z);

    s.resetAndStart();
    rv.writeGPR32(12, 123456);
    s.cycle(2);
    try expectEqual(@as(u32, 123328), rv.readGPR32(1));
    try expect(!s.s.reg.stat.n);
    try expect(s.s.reg.stat.c);
    try expect(!s.s.reg.stat.v);
    try expect(!s.s.reg.stat.z);

    s.resetAndStart();
    rv.writeGPR32(12, 128);
    //try s.debugCycle(2, .one);
    s.cycle(2);
    try expectEqual(@as(u32, 0), rv.readGPR32(1));
    try expect(!s.s.reg.stat.n);
    try expect(s.s.reg.stat.c);
    try expect(!s.s.reg.stat.v);
    try expect(s.s.reg.stat.z);
}

test "ADD X0, R4U -> X0" {
    var s = try initSimulator(&[_]ie.Instruction{
        .{
            .mnemonic = .ADD,
            .suffix = .none,
            .params = &[_]ie.Parameter{
                ie.parameter(.reg32, 0),
                ie.parameter(.reg16u, 4),
                ie.toParameter(.reg32, 0),
            },
        },
    });
    defer deinitSimulator(&s);
    var rv = register_file.RegisterView.init(s.reg_file, s.s.reg.rsn);

    s.cycle(1);
    rv.writeGPR32(0, 33000);
    try expectEqual(@as(u32, 33000), rv.readGPR32(0));
    try expect(!s.s.reg.stat.n);
    try expect(!s.s.reg.stat.c);
    try expect(!s.s.reg.stat.v);
    try expect(!s.s.reg.stat.z);

    s.resetAndStart();
    s.cycle(1);
    try expectEqual(@as(u32, 66000), rv.readGPR32(0));
    try expect(!s.s.reg.stat.n);
    try expect(!s.s.reg.stat.c);
    try expect(!s.s.reg.stat.v);
    try expect(!s.s.reg.stat.z);
}

test "ADD X1, R3S, X1" {
    var s = try initSimulator(&[_]ie.Instruction{
        .{
            .mnemonic = .ADD,
            .suffix = .none,
            .params = &[_]ie.Parameter{
                ie.parameter(.reg32, 1),
                ie.parameter(.reg16s, 3),
                ie.toParameter(.reg32, 1),
            },
        },
    });
    defer deinitSimulator(&s);
    var rv = register_file.RegisterView.init(s.reg_file, s.s.reg.rsn);

    rv.writeSignedGPR(3, @as(i16, -32000));
    s.cycle(1);
    try expectEqual(@as(i32, -32000), rv.readSignedGPR32(1));
    try expect(s.s.reg.stat.n);
    try expect(!s.s.reg.stat.c);
    try expect(!s.s.reg.stat.v);
    try expect(!s.s.reg.stat.z);

    s.resetAndStart();
    s.cycle(1);
    try expectEqual(@as(i32, -64000), rv.readSignedGPR32(1));
    try expect(s.s.reg.stat.n);
    try expect(s.s.reg.stat.c);
    try expect(!s.s.reg.stat.v);
    try expect(!s.s.reg.stat.z);
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
    var rv = register_file.RegisterView.init(s.reg_file, s.s.reg.rsn);
    s.s.reg.stat.c = true;
    s.cycle(2);
    try expectEqual(@as(u32, 12346), rv.readGPR(4));
    try expect(!s.s.reg.stat.n);
    try expect(!s.s.reg.stat.c);
    try expect(!s.s.reg.stat.v);
    try expect(!s.s.reg.stat.z);
}
