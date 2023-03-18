const std = @import("std");
const TempAllocator = @import("temp_allocator");
const allocators = @import("allocators.zig");
const ib = @import("instruction_builder.zig");
const cb = @import("cycle_builder.zig");
const uc = @import("microcode");
const arch = @import("arch_builder.zig");
const uc_serialization = @import("microcode_rom_serialization.zig");
const ControlSignals = @import("ControlSignals");

pub fn main() !void {
    allocators.temp_arena = try TempAllocator.init(0x1000_0000);

    try ib.processScope(@import("_reset.zig"));
    try ib.processScope(@import("_fault.zig"));
    try ib.processScope(@import("_interrupt.zig"));
    try ib.processScope(@import("_alu.zig"));
    try ib.processScope(@import("_copy.zig"));
    try ib.processScope(@import("_branch.zig"));
    try ib.processScope(@import("_call.zig"));
    try ib.processScope(@import("_load_store.zig"));
    try ib.processScope(@import("_stack.zig"));
    try ib.processScope(@import("_address_translator.zig"));
    try ib.processScope(@import("_sync.zig"));

    try arch.validateAliases();

    var stdout = std.io.getStdOut().writer();
    try arch.writeOpcodeTableSmall(stdout);

    try assignReservedOpcodes();

    // try arch.analyzeCustom(&allocators.temp_arena, stdout);
    
    // inline for (comptime std.enums.values(ControlSignals.SignalName)) |signal| {
    //     try arch.analyzeControlSignalUsage(&allocators.temp_arena, &.{ signal }, stdout);
    // }

    // try arch.analyzeControlSignalUsage(&allocators.temp_arena, &.{ .jl_src, .jh_src }, stdout);
    // try arch.analyzeControlSignalUsage(&allocators.temp_arena, &.{ .jr_rsel, .kr_rsel }, stdout);
    // try arch.analyzeControlSignalUsage(&allocators.temp_arena, &.{ .sr1_ri, .sr2_ri }, stdout);
    // try arch.analyzeControlSignalUsage(&allocators.temp_arena, &.{ .sr1_wi, .sr2_wi }, stdout);
    // try arch.analyzeControlSignalUsage(&allocators.temp_arena, &.{ .sr1_wsrc, .sr2_wsrc }, stdout);
    // try arch.analyzeControlSignalUsage(&allocators.temp_arena, &.{ .sr1_wi, .sr1_wsrc }, stdout);
    // try arch.analyzeControlSignalUsage(&allocators.temp_arena, &.{ .sr2_wi, .sr2_wsrc }, stdout);
    // try arch.analyzeControlSignalUsage(&allocators.temp_arena, &.{ .bus_mode, .bus_byte, .bus_rw }, stdout);
    // try arch.analyzeControlSignalUsage(&allocators.temp_arena, &.{ .dl_op, .ob_oa_op }, stdout);
    // try arch.analyzeControlSignalUsage(&allocators.temp_arena, &.{ .allow_int, .seq_op }, stdout);
    // try arch.analyzeControlSignalUsage(&allocators.temp_arena, &.{ .ll_src, .lh_src }, stdout);

    std.debug.print("{} continuations left\n", .{ arch.getContinuationsLeft() });

    {
        var f = try std.fs.cwd().createFile("arch/instruction_encoding.sx", .{});
        defer f.close();
        try arch.writeInstructionData(f.writer());
    }

    var rom_data = try uc_serialization.writeCompressedRoms(allocators.temp_arena.allocator(), allocators.temp_arena.allocator(), &arch.microcode);

    for (rom_data, 0..) |data, n| {
        std.debug.print("ROM {}: {} bytes compressed\n", .{ n, data.len });

        var path_buf: [32]u8 = undefined;
        var path = try std.fmt.bufPrint(&path_buf, "arch/microcode_roms/rom_{}", .{ n });
        var af = try std.fs.cwd().atomicFile(path, .{});
        defer af.deinit();
        try af.file.writeAll(data);
        try af.finish();
    }

    rom_data = try uc_serialization.writeSRecRoms(allocators.temp_arena.allocator(), allocators.temp_arena.allocator(), &arch.microcode);
    for (rom_data, 0..) |data, n| {
        var path_buf: [32]u8 = undefined;
        var path = try std.fmt.bufPrint(&path_buf, "arch/microcode_roms/rom_{}.srec", .{ n });
        var af = try std.fs.cwd().atomicFile(path, .{});
        defer af.deinit();
        try af.file.writeAll(data);
        try af.finish();
    }
}

fn reserved() void {
    ib.encoding(._reserved, .{});
    ib.desc("Reserved for future use");
    cb.invalid_instruction();
}

fn assignReservedOpcodes() !void {
    var opIter = uc.opcodeIterator(0x0000, 0xFFFF);
    while (opIter.next()) |cur_opcode| {
        if (arch.getMicrocodeCycle(uc.getAddressForOpcode(cur_opcode, .{})) == null) {
            const granularity = uc.getOpcodeGranularity(cur_opcode);
            const last_opcode = cur_opcode +% granularity -% 1;
            try ib.processOpcodes(cur_opcode, last_opcode, reserved, false);
        }
    }
}
