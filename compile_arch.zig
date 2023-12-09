// This program generates several types of data about the CPU architecture:
//  - Instruction decode and microcode ROM contents
//  - Mappings between assembly syntax and machine code and vice-versa; used by the assembler and disassembler
//  - HTML instruction set documentation

pub const Cycle = @import("compile_arch/Cycle.zig");
pub const Processor = @import("compile_arch/Processor.zig");
pub const Microcode_Builder = @import("compile_arch/Microcode_Builder.zig");
pub const Decode_ROM_Builder = @import("compile_arch/Decode_ROM_Builder.zig");
pub const opcodes = @import("compile_arch/opcodes.zig");
pub const placeholders = @import("compile_arch/placeholders.zig");

pub const std_options = struct {
    pub const log_scope_levels: []const std.log.ScopeLevel = &.{
        .{ .scope = .compile_arch, .level = .info },
        .{ .scope = .microcode_builder, .level = .info },
    };
};

pub fn main() !void {
    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .{};
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    var temp = try TempAllocator.init(0x1000_0000);
    var processor = Processor.init(gpa.allocator(), arena.allocator(), &temp);

    processor.process(@import("compile_arch/_reset.zig").instructions);
    processor.process(@import("compile_arch/_fault.zig").instructions);
    processor.process(@import("compile_arch/_interrupt.zig").instructions);
    processor.process(@import("compile_arch/_alu.zig").instructions);
    processor.process(@import("compile_arch/_copy.zig").instructions);  
    processor.process(@import("compile_arch/_branch.zig").instructions);
    processor.process(@import("compile_arch/_call.zig").instructions);
    processor.process(@import("compile_arch/_load.zig").instructions);
    processor.process(@import("compile_arch/_store.zig").instructions);
    processor.process(@import("compile_arch/_stack.zig").instructions);
    processor.process(@import("compile_arch/_address_translator.zig").instructions);
    processor.process(@import("compile_arch/_sync.zig").instructions);
    processor.process(@import("compile_arch/_memcpy.zig").instructions);

    processor.process(@import("compile_arch/_alt.zig").instructions);

    processor.microcode.assign_slots();

    var microcode_address_usage: usize = 0;

    const uc = processor.microcode.generate_microcode(gpa.allocator());
    for (uc, 0..) |maybe_cs, addr| {
        _ = addr;
        if (maybe_cs) |cs| {
            _ = cs;
            //log.warn("{}", .{ addr });
            microcode_address_usage += 1;
        }
    }

    var normal_decode_usage: usize = 0;
    var alt_decode_usage: usize = 0;

    const decode = processor.decode_rom.generate_rom_data(gpa.allocator(), &processor.microcode);
    for (0.., decode) |addr, result| {
        if (result.slot != .invalid_instruction) {
            // log.info("{X:0>5}: {any}", .{ addr, result });
            switch (hw.decode.Address.init(@intCast(addr)).mode) {
                .normal => normal_decode_usage += 1,
                .alt => alt_decode_usage += 1,
            }
        }
    }

    log.info("Microcode Slot Usage: {} ({d:.1}%)", .{ microcode_address_usage / 32, @as(f32, @floatFromInt(microcode_address_usage)) / 1310.72 });
    log.info("Normal Decode Space Usage: {} ({d:.1}%)", .{ normal_decode_usage, @as(f32, @floatFromInt(normal_decode_usage)) / 655.36 });
    log.info("Alt Decode Space Usage: {} ({d:.1}%)", .{ alt_decode_usage, @as(f32, @floatFromInt(alt_decode_usage)) / 655.36 });

    {
        var f = try std.fs.cwd().createFile("lib/arch/isa/database.sx", .{});
        defer f.close();

        var writer = sx.writer(gpa.allocator(), f.writer());
        defer writer.deinit();

        try isa.write_database.write(@TypeOf(writer.inner), &writer, false,
            processor.transform_list.items,
            processor.encoding_list.items
        );
    }

    var uc_rom_data = try hw.microcode_roms.write_compressed_roms(gpa.allocator(), temp.allocator(), uc);
    for (uc_rom_data, 0..) |data, n| {
        log.info("Microcode ROM {}: {} bytes compressed", .{ n, data.len });

        var path_buf: [32]u8 = undefined;
        var path = try std.fmt.bufPrint(&path_buf, "roms/compressed/microcode_{}", .{ n });
        if (std.fs.path.dirname(path)) |dir| {
            try std.fs.cwd().makePath(dir);
        }
        var af = try std.fs.cwd().atomicFile(path, .{});
        defer af.deinit();
        try af.file.writeAll(data);
        try af.finish();
    }

    uc_rom_data = try hw.microcode_roms.write_srec_roms(gpa.allocator(), temp.allocator(), uc);
    for (uc_rom_data, 0..) |data, n| {
        var path_buf: [32]u8 = undefined;
        var path = try std.fmt.bufPrint(&path_buf, "roms/srec/microcode_{}.srec", .{ n });
        if (std.fs.path.dirname(path)) |dir| {
            try std.fs.cwd().makePath(dir);
        }
        var af = try std.fs.cwd().atomicFile(path, .{});
        defer af.deinit();
        try af.file.writeAll(data);
        try af.finish();
    }

    var decode_rom_data = try hw.decode_roms.write_compressed_roms(gpa.allocator(), temp.allocator(), decode);
    for (decode_rom_data, 0..) |data, n| {
        log.info("Decode ROM {}: {} bytes compressed", .{ n, data.len });

        var path_buf: [32]u8 = undefined;
        var path = try std.fmt.bufPrint(&path_buf, "roms/compressed/decode_{}", .{ n });
        if (std.fs.path.dirname(path)) |dir| {
            try std.fs.cwd().makePath(dir);
        }
        var af = try std.fs.cwd().atomicFile(path, .{});
        defer af.deinit();
        try af.file.writeAll(data);
        try af.finish();
    }

    decode_rom_data = try hw.decode_roms.write_srec_roms(gpa.allocator(), temp.allocator(), decode);
    for (decode_rom_data, 0..) |data, n| {
        var path_buf: [32]u8 = undefined;
        var path = try std.fmt.bufPrint(&path_buf, "roms/srec/decode_{}.srec", .{ n });
        if (std.fs.path.dirname(path)) |dir| {
            try std.fs.cwd().makePath(dir);
        }
        var af = try std.fs.cwd().atomicFile(path, .{});
        defer af.deinit();
        try af.file.writeAll(data);
        try af.finish();
    }

    try documentation.generate(gpa.allocator(), &processor, uc, decode);
}

const log = std.log.scoped(.compile_arch);

const documentation = @import("compile_arch/documentation.zig");
const hw = arch.hw;
const isa = arch.isa;
const arch = @import("lib_arch");
const TempAllocator = @import("TempAllocator");
const sx = @import("sx");
const std = @import("std");
