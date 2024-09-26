// This program generates several types of data about the CPU architecture:
//  - Instruction decode and microcode ROM contents
//  - Mappings between assembly syntax and machine code and vice-versa; used by the assembler and disassembler
//  - HTML instruction set documentation

const CLI_Option = enum {
    db_path,
    compact_db,
    rom_format,
    setup_uc_path,
    compute_uc_path,
    transact_uc_path,
    uc_csv_path,
    id_rom_path,
    id_csv_path,
};

const cli_options = std.StaticStringMap(CLI_Option).initComptime(.{
    .{ "--db", .db_path },
    .{ "--compact", .compact_db },
    .{ "--rom-format", .rom_format },
    .{ "--setup-uc", .setup_uc_path },
    .{ "--compute-uc", .compute_uc_path },
    .{ "--transact-uc", .transact_uc_path },
    .{ "--uc-csv", .uc_csv_path },
    .{ "--id-rom", .id_rom_path },
    .{ "--id-csv", .id_csv_path },
});

const ROM_Format = enum {
    compressed,
    compressed_dump,
    srec,
    ihex,
};

pub fn main() !void {
    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .{};
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    var temp = try Temp_Allocator.init(0x1000_0000);

    var db_path: ?[]const u8 = null;
    var compact_db = false;
    var rom_fmt: ROM_Format = .compressed;
    var setup_uc_path: ?[]const u8 = null;
    var compute_uc_path: ?[]const u8 = null;
    var transact_uc_path: ?[]const u8 = null;
    var uc_csv_path: ?[]const u8 = null;
    var id_csv_path: ?[]const u8 = null;
    var id_rom_path: ?[]const u8 = null;

    var arg_iter = try std.process.argsWithAllocator(temp.allocator());
    _ = arg_iter.next(); // ignore exe name
    while (arg_iter.next()) |arg| {
        if (cli_options.get(arg)) |option| {
            switch (option) {
                .db_path => db_path = try arena.allocator().dupe(u8, arg_iter.next() orelse expected_output_path("--db")),
                .compact_db => compact_db = true,
                .rom_format => rom_fmt = std.meta.stringToEnum(ROM_Format, arg_iter.next() orelse expected_rom_format()) orelse invalid_rom_format(arg),
                .setup_uc_path => setup_uc_path = try arena.allocator().dupe(u8, arg_iter.next() orelse expected_output_path("--setup-uc")),
                .compute_uc_path => compute_uc_path = try arena.allocator().dupe(u8, arg_iter.next() orelse expected_output_path("--compute-uc")),
                .transact_uc_path => transact_uc_path = try arena.allocator().dupe(u8, arg_iter.next() orelse expected_output_path("--transact-uc")),
                .uc_csv_path => uc_csv_path = try arena.allocator().dupe(u8, arg_iter.next() orelse expected_output_path("--uc-csv")),
                .id_rom_path => id_rom_path = try arena.allocator().dupe(u8, arg_iter.next() orelse expected_output_path("--id-rom")),
                .id_csv_path => id_csv_path = try arena.allocator().dupe(u8, arg_iter.next() orelse expected_output_path("--id-csv")),
            }
        } else {
            try std.io.getStdErr().writer().print("Unrecognized option: {s}\n", .{ arg });
            std.process.exit(1);
        }
    }

    temp.reset(.{});

    var processor = Processor.init(gpa.allocator(), arena.allocator(), &temp);
    processor.process(.{
        @import("handlers/reset.zig"),
        faults.Handler(.page_fault),
        faults.Handler(.access_fault),
        faults.Handler(.page_align_fault),
        faults.Handler(.align_fault),
        faults.Handler(.overflow_fault),
        faults.Handler(.instruction_protection_fault),
        faults.Handler(.invalid_instruction_fault),
        faults.Handler(.double_fault),
        // @import("handlers/interrupt.zig"),
        @import("handlers/invalid_instruction.zig"),

        @import("instructions/nop.zig"),
    });

    processor.microcode.assign_slots();

    const uc = processor.microcode.generate_microcode(gpa.allocator());
    const uc_fn_names = processor.microcode.generate_microcode_fn_names(gpa.allocator());

    var microcode_address_usage: usize = 0;
    for (uc, 0..) |maybe_cs, addr| {
        _ = addr;
        if (maybe_cs) |cs| {
            _ = cs;
            microcode_address_usage += 1;
        }
    }

    const decode = processor.decode_rom.generate_rom_data(gpa.allocator(), &processor.microcode);

    var insn_decode_usage: usize = 0;
    for (decode) |result| {
        if (result.entry != .invalid_instruction) {
            insn_decode_usage += 1;
        }
    }

    log.info("Microcode Slot Usage: {} ({d:.1}%)", .{ microcode_address_usage / 16, @as(f32, @floatFromInt(microcode_address_usage)) / 655.36 });
    log.info("Instruction Decode Space Usage: {} ({d:.1}%)", .{ insn_decode_usage, @as(f32, @floatFromInt(insn_decode_usage)) / 655.36 });

    if (db_path) |path| {
        var f = try std.fs.cwd().createFile(path, .{});
        defer f.close();

        var writer = f.writer();
        var sxw = sx.writer(gpa.allocator(), writer.any());
        defer sxw.deinit();

        try iedb.write_database.write(&sxw, compact_db, processor.encoding_list.items);
    }

    if (uc_csv_path) |path| {
        const uc_data = try arch.microcode.write_csv(gpa.allocator(), temp.allocator(), uc, uc_fn_names);

        if (std.fs.path.dirname(path)) |dir| {
            try std.fs.cwd().makePath(dir);
        }

        var af = try std.fs.cwd().atomicFile(path, .{});
        defer af.deinit();
        try af.file.writeAll(uc_data);
        try af.finish();
    }

    if (id_csv_path) |path| {
        const uc_data = try arch.insn_decode.write_csv(gpa.allocator(), temp.allocator(), decode);

        if (std.fs.path.dirname(path)) |dir| {
            try std.fs.cwd().makePath(dir);
        }

        var af = try std.fs.cwd().atomicFile(path, .{});
        defer af.deinit();
        try af.file.writeAll(uc_data);
        try af.finish();
    }

    if (setup_uc_path) |path| {
        const T = arch.microcode.Setup_Microcode_Entry;
        const rom_data = switch (rom_fmt) {
            .compressed, .compressed_dump => try arch.microcode.write_compressed_rom(T, gpa.allocator(), temp.allocator(), uc),
            .srec => try arch.microcode.write_srec_rom(T, gpa.allocator(), temp.allocator(), uc),
            .ihex => try arch.microcode.write_ihex_rom(T, gpa.allocator(), temp.allocator(), uc),
        };
        log.info("Setup Microcode ROM: {} bytes ({s})", .{ rom_data.len, @tagName(rom_fmt) });

        if (std.fs.path.dirname(path)) |dir| {
            try std.fs.cwd().makePath(dir);
        }

        var af = try std.fs.cwd().atomicFile(path, .{});
        defer af.deinit();
        if (rom_fmt == .compressed_dump) {
            var w = af.file.writer();
            try rom_decompress.dump(rom_data, w.any());
        } else {
            try af.file.writeAll(rom_data);
        }
        try af.finish();
    }

    if (compute_uc_path) |path| {
        const T = arch.microcode.Compute_Microcode_Entry;
        const rom_data = switch (rom_fmt) {
            .compressed, .compressed_dump => try arch.microcode.write_compressed_rom(T, gpa.allocator(), temp.allocator(), uc),
            .srec => try arch.microcode.write_srec_rom(T, gpa.allocator(), temp.allocator(), uc),
            .ihex => try arch.microcode.write_ihex_rom(T, gpa.allocator(), temp.allocator(), uc),
        };
        log.info("Compute Microcode ROM: {} bytes ({s})", .{ rom_data.len, @tagName(rom_fmt) });

        if (std.fs.path.dirname(path)) |dir| {
            try std.fs.cwd().makePath(dir);
        }

        var af = try std.fs.cwd().atomicFile(path, .{});
        defer af.deinit();
        if (rom_fmt == .compressed_dump) {
            var w = af.file.writer();
            try rom_decompress.dump(rom_data, w.any());
        } else {
            try af.file.writeAll(rom_data);
        }
        try af.finish();
    }

    if (transact_uc_path) |path| {
        const T = arch.microcode.Transact_Microcode_Entry;
        const rom_data = switch (rom_fmt) {
            .compressed, .compressed_dump => try arch.microcode.write_compressed_rom(T, gpa.allocator(), temp.allocator(), uc),
            .srec => try arch.microcode.write_srec_rom(T, gpa.allocator(), temp.allocator(), uc),
            .ihex => try arch.microcode.write_ihex_rom(T, gpa.allocator(), temp.allocator(), uc),
        };
        log.info("Transact Microcode ROM: {} bytes ({s})", .{ rom_data.len, @tagName(rom_fmt) });

        if (std.fs.path.dirname(path)) |dir| {
            try std.fs.cwd().makePath(dir);
        }

        var af = try std.fs.cwd().atomicFile(path, .{});
        defer af.deinit();
        if (rom_fmt == .compressed_dump) {
            var w = af.file.writer();
            try rom_decompress.dump(rom_data, w.any());
        } else {
            try af.file.writeAll(rom_data);
        }
        try af.finish();
    }

    if (id_rom_path) |path| {
        const rom_data = switch (rom_fmt) {
            .compressed, .compressed_dump => try arch.insn_decode.write_compressed_rom(gpa.allocator(), temp.allocator(), decode),
            .srec => try arch.insn_decode.write_srec_rom(gpa.allocator(), temp.allocator(), decode),
            .ihex => try arch.insn_decode.write_ihex_rom(gpa.allocator(), temp.allocator(), decode),
        };
        log.info("Instruction Decode ROM: {} bytes ({s})", .{ rom_data.len, @tagName(rom_fmt) });

        if (std.fs.path.dirname(path)) |dir| {
            try std.fs.cwd().makePath(dir);
        }
        var af = try std.fs.cwd().atomicFile(path, .{});
        defer af.deinit();
        if (rom_fmt == .compressed_dump) {
            var w = af.file.writer();
            try rom_decompress.dump(rom_data, w.any());
        } else {
            try af.file.writeAll(rom_data);
        }
        try af.finish();
    }

    // TODO
    //try documentation.generate(gpa.allocator(), &processor, uc, decode);
}

fn expected_output_path(option: []const u8) noreturn {
    var w = std.io.getStdErr().writer();
    w.print("Expected output path after {s} option.\n", .{ option }) catch {};
    std.process.exit(1);
}

fn expected_rom_format() noreturn {
    var w = std.io.getStdErr().writer();
    w.writeAll("Expected ROM format after --rom-format option.  Available formats are:\n") catch {};
    for (std.enums.values(ROM_Format)) |fmt| {
        w.print("    {s}\n", .{ @tagName(fmt) }) catch {};
    }
    std.process.exit(1);
}

fn invalid_rom_format(arg: []const u8) noreturn {
    var w = std.io.getStdErr().writer();
    w.print("'{s}' is not a valid ROM format.  Available formats are:\n", .{ arg }) catch {};
    for (std.enums.values(ROM_Format)) |fmt| {
        w.print("    {s}\n", .{ @tagName(fmt) }) catch {};
    }
    std.process.exit(1);
}

pub const Cycle = @import("compile/Cycle.zig");
pub const Processor = @import("compile/Processor.zig");
pub const Microcode_Builder = @import("compile/Microcode_Builder.zig");
pub const Decode_ROM_Builder = @import("compile/Decode_ROM_Builder.zig");
pub const placeholders = @import("compile/placeholders.zig");

pub const std_options: std.Options = .{
    .log_scope_levels = &.{
        .{ .scope = .compile, .level = .info },
        .{ .scope = .microcode_builder, .level = .info },
    },
};

const log = std.log.scoped(.compile);

const faults = @import("handlers/faults.zig");
const documentation = @import("compile/documentation.zig");
const iedb = @import("iedb");
const isa = @import("isa");
const arch = @import("arch");
const rom_decompress = @import("rom_decompress");
const Temp_Allocator = @import("Temp_Allocator");
const sx = @import("sx");
const std = @import("std");
