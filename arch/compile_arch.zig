const std = @import("std");
const TempAllocator = @import("temp_allocator");
const allocators = @import("allocators.zig");
const ib = @import("instruction_builder.zig");
const cb = @import("cycle_builder.zig");
const uc = @import("microcode");
const microcode_builder = @import("microcode_builder.zig");
const instructions = @import("instructions.zig");
const uc_serialization = @import("microcode_rom_serialization.zig");

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
    try ib.processScope(@import("_mmu.zig"));
    try ib.processScope(@import("_sync.zig"));

    assignReservedOpcodes();

    std.debug.print("{} continuations left\n", .{ microcode_builder.getContinuationsLeft() });

    {
        var f = try std.fs.cwd().createFile("arch/instruction_encoding.sx", .{});
        defer f.close();
        try instructions.writeInstructionData(f.writer());
    }

    var rom_data = try uc_serialization.writeCompressedRoms(allocators.temp_arena.allocator(), allocators.temp_arena.allocator(), &microcode_builder.microcode);

    for (rom_data, 0..) |data, n| {
        std.debug.print("ROM {}: {} bytes compressed\n", .{ n, data.len });

        var path_buf: [32]u8 = undefined;
        var path = try std.fmt.bufPrint(&path_buf, "arch/microcode_roms/rom_{}", .{ n });
        var af = try std.fs.cwd().atomicFile(path, .{});
        defer af.deinit();
        try af.file.writeAll(data);
        try af.finish();
    }

    rom_data = try uc_serialization.writeSRecRoms(allocators.temp_arena.allocator(), allocators.temp_arena.allocator(), &microcode_builder.microcode);
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

fn assignReservedOpcodes() void {
    var opIter = uc.opcodeIterator(0x0000, 0xFFFF);
    while (opIter.next()) |cur_opcode| {
        if (microcode_builder.get(uc.getAddressForOpcode(cur_opcode, .{})) == null) {
            const granularity = uc.getOpcodeGranularity(cur_opcode);
            const last_opcode = cur_opcode +% granularity -% 1;
            ib.processOpcodes(cur_opcode, last_opcode, reserved);
        }
    }
}
