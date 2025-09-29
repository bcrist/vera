// This program generates several types of data about the CPU architecture:
//  - Instruction decode and microcode ROM contents
//  - Mappings between assembly syntax and machine code and vice-versa; used by the assembler and disassembler
//  - HTML instruction set documentation

pub const Cycle = @import("compile/Cycle.zig");
pub const Processor = @import("compile/Processor.zig");
pub const Microcode_Builder = @import("compile/Microcode_Builder.zig");
pub const Decode_ROM_Builder = @import("compile/Decode_ROM_Builder.zig");
pub const placeholders = @import("compile/placeholders.zig");

const CLI_Option = enum {
    db_path,
    compact_db,
    rom_format,
    setup_uc_path,
    compute_uc_path,
    transact_uc_path,
    decode_uc_path,
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
    .{ "--decode-uc", .decode_uc_path },
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
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    var temp = try Temp_Allocator.init(0x1000_0000);

    var db_path: ?[]const u8 = null;
    var compact_db = false;
    var rom_fmt: ROM_Format = .compressed;
    var setup_uc_path: ?[]const u8 = null;
    var compute_uc_path: ?[]const u8 = null;
    var transact_uc_path: ?[]const u8 = null;
    var decode_uc_path: ?[]const u8 = null;
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
                .decode_uc_path => decode_uc_path = try arena.allocator().dupe(u8, arg_iter.next() orelse expected_output_path("--decode-uc")),
                .uc_csv_path => uc_csv_path = try arena.allocator().dupe(u8, arg_iter.next() orelse expected_output_path("--uc-csv")),
                .id_rom_path => id_rom_path = try arena.allocator().dupe(u8, arg_iter.next() orelse expected_output_path("--id-rom")),
                .id_csv_path => id_csv_path = try arena.allocator().dupe(u8, arg_iter.next() orelse expected_output_path("--id-csv")),
            }
        } else {
            var buf: [64]u8 = undefined;
            var writer = std.fs.File.stderr().writer(&buf);
            try writer.interface.print("Unrecognized option: {s}\n", .{ arg });
            try writer.interface.flush();
            std.process.exit(1);
        }
    }

    temp.reset(.{});

    var processor = Processor.init(gpa, arena.allocator(), &temp);
    processor.process(.{
        @import("handlers/reset.zig"),
        faults.Handler(.page_fault),
        faults.Handler(.access_fault),
        faults.Handler(.pipe_fault),
        faults.Handler(.page_align_fault),
        faults.Handler(.align_fault),
        faults.Handler(.overflow_fault),
        faults.Handler(.rs_underflow_fault),
        faults.Handler(.rs_overflow_fault),
        faults.Handler(.instruction_protection_fault),
        faults.Handler(.invalid_instruction_fault),
        faults.Handler(.double_fault),
        @import("handlers/interrupt.zig"),
        @import("instructions/invalid_instruction.zig"),

        @import("instructions/nop.zig"),
        @import("instructions/park.zig"),
        @import("instructions/ret.zig"),
        @import("instructions/iret.zig"),
        @import("instructions/fret.zig"),
        @import("instructions/ifex.zig"),

        @import("instructions/add/reg.zig"),
        @import("instructions/add/imm.zig"),
        @import("instructions/add/ip_rel.zig"),
        @import("instructions/add/sp_rel.zig"),
        @import("instructions/add/bp_rel.zig"),

        @import("instructions/sub/reg.zig"),
        @import("instructions/sub/ip_rel.zig"),
        @import("instructions/sub/sp_rel.zig"),
        @import("instructions/sub/bp_rel.zig"),

        @import("instructions/nadd/reg.zig"),
        @import("instructions/nadd/imm.zig"),
        @import("instructions/nadd/ip_rel.zig"),
        @import("instructions/nadd/sp_rel.zig"),
        @import("instructions/nadd/bp_rel.zig"),

        @import("instructions/cmp/reg.zig"),
        @import("instructions/cmp/imm.zig"),
        @import("instructions/cmp/ip_rel.zig"),
        @import("instructions/cmp/sp_rel.zig"),
        @import("instructions/cmp/bp_rel.zig"),

        @import("instructions/count.zig"),
        @import("instructions/saturate.zig"),
        @import("instructions/ext.zig"),
        @import("instructions/swap.zig"),
        @import("instructions/clrbit.zig"),
        @import("instructions/setbit.zig"),
        @import("instructions/bit.zig"),

        @import("instructions/shift/reg.zig"),
        @import("instructions/shift/imm.zig"),
        @import("instructions/shift/carry.zig"),

        @import("instructions/logic/reg.zig"),
        @import("instructions/logic/imm.zig"),
        @import("instructions/logic/ip_rel.zig"),
        @import("instructions/logic/sp_rel.zig"),
        @import("instructions/logic/bp_rel.zig"),

        @import("instructions/val/reg.zig"),
        @import("instructions/val/imm.zig"),

    });

    processor.microcode.assign_slots();

    const uc = processor.microcode.generate_microcode(gpa);
    const uc_fn_names = processor.microcode.generate_microcode_fn_names(gpa);

    var microcode_address_usage: usize = 0;
    for (uc, 0..) |maybe_cs, addr| {
        _ = addr;
        if (maybe_cs) |cs| {
            _ = cs;
            microcode_address_usage += 1;
        }
    }

    const decode = processor.decode_rom.generate_rom_data(gpa, &processor.microcode);

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

        var buf: [4096]u8 = undefined;
        var writer = f.writer(&buf);
        var sxw = sx.writer(gpa, &writer.interface);
        defer sxw.deinit();

        try iedb.write_database.write(&sxw, compact_db, processor.encoding_list.items);
        try writer.interface.flush();
    }

    if (uc_csv_path) |path| {
        var buf: [4096]u8 = undefined;
        var af = try std.fs.cwd().atomicFile(path, .{
            .write_buffer = &buf,
            .make_path = true,
        });
        defer af.deinit();
        try arch.microcode.write_csv(temp.allocator(), &af.file_writer.interface, uc, uc_fn_names);
        try af.finish();
    }

    if (id_csv_path) |path| {
        var buf: [4096]u8 = undefined;
        var af = try std.fs.cwd().atomicFile(path, .{
            .write_buffer = &buf,
            .make_path = true,
        });
        defer af.deinit();
        try arch.insn_decode.write_csv(&af.file_writer.interface, decode);
        try af.finish();
    }

    if (setup_uc_path) |path| {
        const Entry = arch.microcode.Setup_Microcode_Entry;
        var buf: [4096]u8 = undefined;
        var af = try std.fs.cwd().atomicFile(path, .{
            .write_buffer = &buf,
            .make_path = true,
        });
        defer af.deinit();
        const w = &af.file_writer.interface;
        switch (rom_fmt) {
            .srec => try arch.microcode.write_srec_rom(Entry, gpa, w, uc),
            .ihex => try arch.microcode.write_ihex_rom(Entry, gpa, w, uc),
            .compressed => try arch.microcode.write_compressed_rom(Entry, gpa, w, uc),
            .compressed_dump => {
                var compressed = std.io.Writer.Allocating.init(gpa);
                defer compressed.deinit();
                try arch.microcode.write_compressed_rom(Entry, gpa, &compressed.writer, uc);
                try rom_decompress.dump(compressed.written(), w);
            },
        }
        try af.finish();
    }

    if (compute_uc_path) |path| {
        const Entry = arch.microcode.Compute_Microcode_Entry;
        var buf: [4096]u8 = undefined;
        var af = try std.fs.cwd().atomicFile(path, .{
            .write_buffer = &buf,
            .make_path = true,
        });
        defer af.deinit();
        const w = &af.file_writer.interface;
        switch (rom_fmt) {
            .srec => try arch.microcode.write_srec_rom(Entry, gpa, w, uc),
            .ihex => try arch.microcode.write_ihex_rom(Entry, gpa, w, uc),
            .compressed => try arch.microcode.write_compressed_rom(Entry, gpa, w, uc),
            .compressed_dump => {
                var compressed = std.io.Writer.Allocating.init(gpa);
                defer compressed.deinit();
                try arch.microcode.write_compressed_rom(Entry, gpa, &compressed.writer, uc);
                try rom_decompress.dump(compressed.written(), w);
            },
        }
        try af.finish();
    }

    if (transact_uc_path) |path| {
        const Entry = arch.microcode.Transact_Microcode_Entry;
        var buf: [4096]u8 = undefined;
        var af = try std.fs.cwd().atomicFile(path, .{
            .write_buffer = &buf,
            .make_path = true,
        });
        defer af.deinit();
        const w = &af.file_writer.interface;
        switch (rom_fmt) {
            .srec => try arch.microcode.write_srec_rom(Entry, gpa, w, uc),
            .ihex => try arch.microcode.write_ihex_rom(Entry, gpa, w, uc),
            .compressed => try arch.microcode.write_compressed_rom(Entry, gpa, w, uc),
            .compressed_dump => {
                var compressed = std.io.Writer.Allocating.init(gpa);
                defer compressed.deinit();
                try arch.microcode.write_compressed_rom(Entry, gpa, &compressed.writer, uc);
                try rom_decompress.dump(compressed.written(), w);
            },
        }
        try af.finish();
    }

    if (decode_uc_path) |path| {
        const Entry = arch.microcode.Decode_Microcode_Entry;
        var buf: [4096]u8 = undefined;
        var af = try std.fs.cwd().atomicFile(path, .{
            .write_buffer = &buf,
            .make_path = true,
        });
        defer af.deinit();
        const w = &af.file_writer.interface;
        switch (rom_fmt) {
            .srec => try arch.microcode.write_srec_rom(Entry, gpa, w, uc),
            .ihex => try arch.microcode.write_ihex_rom(Entry, gpa, w, uc),
            .compressed => try arch.microcode.write_compressed_rom(Entry, gpa, w, uc),
            .compressed_dump => {
                var compressed = std.io.Writer.Allocating.init(gpa);
                defer compressed.deinit();
                try arch.microcode.write_compressed_rom(Entry, gpa, &compressed.writer, uc);
                try rom_decompress.dump(compressed.written(), w);
            },
        }
        try af.finish();
    }

    if (id_rom_path) |path| {
        var buf: [4096]u8 = undefined;
        var af = try std.fs.cwd().atomicFile(path, .{
            .write_buffer = &buf,
            .make_path = true,
        });
        defer af.deinit();
        const w = &af.file_writer.interface;
        switch (rom_fmt) {
            .srec => try arch.insn_decode.write_srec_rom(gpa, w, decode),
            .ihex => try arch.insn_decode.write_ihex_rom(gpa, w, decode),
            .compressed => try arch.insn_decode.write_compressed_rom(gpa, w, decode),
            .compressed_dump => {
                var compressed = std.io.Writer.Allocating.init(gpa);
                defer compressed.deinit();
                try arch.insn_decode.write_compressed_rom(gpa, &compressed.writer, decode);
                try rom_decompress.dump(compressed.written(), w);
            },
        }
        try af.finish();
    }

    // TODO
    //try documentation.generate(gpa.allocator(), &processor, uc, decode);
}

fn expected_output_path(option: []const u8) noreturn {
    var buf: [64]u8 = undefined;
    var writer = std.fs.File.stderr().writer(&buf);
    writer.interface.print("Expected output path after {s} option.\n", .{ option }) catch {};
    writer.interface.flush() catch {};
    std.process.exit(1);
}

fn expected_rom_format() noreturn {
    var buf: [64]u8 = undefined;
    var writer = std.fs.File.stderr().writer(&buf);
    writer.interface.writeAll("Expected ROM format after --rom-format option.  Available formats are:\n") catch {};
    for (std.enums.values(ROM_Format)) |fmt| {
        writer.interface.print("    {s}\n", .{ @tagName(fmt) }) catch {};
    }
    writer.interface.flush() catch {};
    std.process.exit(1);
}

fn invalid_rom_format(arg: []const u8) noreturn {
    var buf: [64]u8 = undefined;
    var writer = std.fs.File.stderr().writer(&buf);
    writer.interface.print("'{s}' is not a valid ROM format.  Available formats are:\n", .{ arg }) catch {};
    for (std.enums.values(ROM_Format)) |fmt| {
        writer.interface.print("    {s}\n", .{ @tagName(fmt) }) catch {};
    }
    writer.interface.flush() catch {};
    std.process.exit(1);
}

const gpa = std.heap.smp_allocator;

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
