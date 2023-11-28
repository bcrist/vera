// This program generates several types of data about the CPU architecture:
//  - Instruction decode and microcode ROM contents
//  - Mappings between assembly syntax and machine code and vice-versa; used by the assembler and disassembler
//  - HTML instruction set documentation

// TODO compute flags affected by each instruction, include in encoding data/documentation

pub const Cycle = @import("compile_arch/Cycle.zig");
pub const Processor = @import("compile_arch/Processor.zig");
pub const Microcode_Builder = @import("compile_arch/Microcode_Builder.zig");
pub const Decode_ROM_Builder = @import("compile_arch/Decode_ROM_Builder.zig");
pub const opcodes = @import("compile_arch/opcodes.zig");
pub const placeholders = @import("compile_arch/placeholders.zig");

pub fn main() !void {
    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .{};
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    var temp = try TempAllocator.init(0x1000_0000);
    var processor = Processor.init(gpa.allocator(), arena.allocator(), &temp);

    processor.process(@import("compile_arch/_reset.zig").instructions);
    processor.process(@import("compile_arch/_fault.zig").instructions);
    processor.process(@import("compile_arch/_interrupt.zig").instructions);
    processor.process(@import("compile_arch/_alu.zig").instructions);
    // processor.process(@import("compile_arch/_copy.zig").instructions);
    // processor.process(@import("compile_arch/_branch.zig").instructions);
    processor.process(@import("compile_arch/_call.zig").instructions);
    // processor.process(@import("compile_arch/_load_store.zig").instructions);
    // processor.process(@import("compile_arch/_stack.zig").instructions);
    processor.process(@import("compile_arch/_address_translator.zig").instructions);
    // processor.process(@import("compile_arch/_sync.zig").instructions);

    processor.microcode.assign_slots();

    var microcode_address_usage: usize = 0;

    const uc = processor.microcode.generate_microcode(gpa.allocator());
    for (uc, 0..) |maybe_cs, addr| {
        _ = addr;
        if (maybe_cs) |cs| {
            _ = cs;
            //std.log.warn("{}", .{ addr });
            microcode_address_usage += 1;
        }
    }

    var normal_decode_usage: usize = 0;
    var alt_decode_usage: usize = 0;

    const decode = processor.decode_rom.generate_rom_data(gpa.allocator(), &processor.microcode);
    for (0.., decode) |addr, result| {
        if (result.slot != .invalid_instruction) {
            // std.log.info("{X:0>5}: {any}", .{ addr, result });
            switch (hw.decode.Address.init(@intCast(addr)).mode) {
                .normal => normal_decode_usage += 1,
                .alt => alt_decode_usage += 1,
            }
        }
    }

    std.log.info("Microcode Slot Usage: {} ({d:.1}%)", .{ microcode_address_usage / 32, @as(f32, @floatFromInt(microcode_address_usage)) / 1310.72 });
    std.log.info("Normal Decode Space Usage: {} ({d:.1}%)", .{ normal_decode_usage, @as(f32, @floatFromInt(normal_decode_usage)) / 655.36 });
    std.log.info("Alt Decode Space Usage: {} ({d:.1}%)", .{ alt_decode_usage, @as(f32, @floatFromInt(alt_decode_usage)) / 655.36 });

    // var stdout = std.io.getStdOut().writer();
    // _ = stdout;
    // try arch.writeOpcodeTableSmall(stdout);

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

    {
        var f = try std.fs.cwd().createFile("lib/arch/isa/instructions.sx", .{});
        defer f.close();

        var writer = sx.writer(gpa.allocator(), f.writer());
        defer writer.deinit();

        for (processor.encoding_list.items) |encoding| {
            try encoding.write(@TypeOf(writer.inner), &writer, false);
        }
    }

    // var rom_data = try uc_roms.writeCompressedRoms(allocators.temp_arena.allocator(), allocators.temp_arena.allocator(), &arch.microcode);

    // for (rom_data, 0..) |data, n| {
    //     std.debug.print("ROM {}: {} bytes compressed\n", .{ n, data.len });

    //     var path_buf: [32]u8 = undefined;
    //     var path = try std.fmt.bufPrint(&path_buf, "arch/microcode_roms/rom_{}", .{ n });
    //     var af = try std.fs.cwd().atomicFile(path, .{});
    //     defer af.deinit();
    //     try af.file.writeAll(data);
    //     try af.finish();
    // }

    // rom_data = try uc_roms.writeSRecRoms(allocators.temp_arena.allocator(), allocators.temp_arena.allocator(), &arch.microcode);
    // for (rom_data, 0..) |data, n| {
    //     var path_buf: [32]u8 = undefined;
    //     var path = try std.fmt.bufPrint(&path_buf, "arch/microcode_roms/rom_{}.srec", .{ n });
    //     var af = try std.fs.cwd().atomicFile(path, .{});
    //     defer af.deinit();
    //     try af.file.writeAll(data);
    //     try af.finish();
    // }
}

const hw = arch.hw;
const arch = @import("lib_arch");
const TempAllocator = @import("TempAllocator");
const sx = @import("sx");
const std = @import("std");
