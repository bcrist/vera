const std = @import("std");
const zgui = @import("pkg/zig-gamedev/libs/zgui/build.zig");
const zglfw = @import("pkg/zig-gamedev/libs/zglfw/build.zig");
const zgpu = @import("pkg/zig-gamedev/libs/zgpu/build.zig");
const zpool = @import("pkg/zig-gamedev/libs/zpool/build.zig");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const mode = b.standardOptimizeOption(.{});

    const zgui_pkg = zgui.package(b, target, mode, .{
        .options = .{ .backend = .glfw_wgpu },
    });
    const zglfw_pkg = zglfw.package(b, target, mode, .{});
    const zpool_pkg = zpool.package(b, target, mode, .{});
    const zgpu_pkg = zgpu.package(b, target, mode, .{
        .deps = .{
            .zpool = zpool_pkg.zpool,
            .zglfw = zglfw_pkg.zglfw
        },
    });

    //[[!! include 'build' !! 269 ]]
    //[[ ################# !! GENERATED CODE -- DO NOT MODIFY !! ################# ]]

    const bits = b.createModule(.{
        .source_file = .{ .path = "pkg/bits.zig" },
    });

    const ControlSignals = b.createModule(.{
        .source_file = .{ .path = "arch/ControlSignals.zig" },
    });

    const bus_types = b.createModule(.{
        .source_file = .{ .path = "arch/bus_types.zig" },
    });

    const misc = b.createModule(.{
        .source_file = .{ .path = "arch/misc.zig" },
        .dependencies = &.{
            .{ .name = "ControlSignals", .module = ControlSignals },
            .{ .name = "bits", .module = bits },
            .{ .name = "bus_types", .module = bus_types },
        },
    });

    const isa_types = b.createModule(.{
        .source_file = .{ .path = "arch/isa_types.zig" },
        .dependencies = &.{
            .{ .name = "misc", .module = misc },
        },
    });

    const microcode = b.createModule(.{
        .source_file = .{ .path = "arch/microcode.zig" },
        .dependencies = &.{
            .{ .name = "bits", .module = bits },
            .{ .name = "isa_types", .module = isa_types },
            .{ .name = "misc", .module = misc },
        },
    });

    ControlSignals.dependencies.put("microcode", microcode) catch unreachable;
    ControlSignals.dependencies.put("misc", misc) catch unreachable;

    const Simulator = b.createModule(.{
        .source_file = .{ .path = "microsim/Simulator.zig" },
    });

    const address_translator_types = b.createModule(.{
        .source_file = .{ .path = "arch/address_translator_types.zig" },
        .dependencies = &.{
            .{ .name = "ControlSignals", .module = ControlSignals },
            .{ .name = "bus_types", .module = bus_types },
        },
    });

    const physical_address = b.createModule(.{
        .source_file = .{ .path = "arch/physical_address.zig" },
        .dependencies = &.{
            .{ .name = "bus_types", .module = bus_types },
        },
    });

    Simulator.dependencies.put("ControlSignals", ControlSignals) catch unreachable;
    Simulator.dependencies.put("Simulator", Simulator) catch unreachable;
    Simulator.dependencies.put("address_translator_types", address_translator_types) catch unreachable;
    Simulator.dependencies.put("bits", bits) catch unreachable;
    Simulator.dependencies.put("bus_types", bus_types) catch unreachable;
    Simulator.dependencies.put("microcode", microcode) catch unreachable;
    Simulator.dependencies.put("misc", misc) catch unreachable;
    Simulator.dependencies.put("physical_address", physical_address) catch unreachable;

    const dep__Zig_TempAllocator = b.dependency("Zig-TempAllocator", .{});

    const TempAllocator = dep__Zig_TempAllocator.module("TempAllocator");

    const dep__Zig_ConsoleHelper = b.dependency("Zig-ConsoleHelper", .{});

    const console = dep__Zig_ConsoleHelper.module("console");

    const deep_hash_map = b.createModule(.{
        .source_file = .{ .path = "pkg/deep_hash_map.zig" },
    });

    const dep__Zig_SX = b.dependency("Zig-SX", .{});

    const sx = dep__Zig_SX.module("sx");

    const isa_encoding = b.createModule(.{
        .source_file = .{ .path = "arch/isa_encoding.zig" },
        .dependencies = &.{
            .{ .name = "bits", .module = bits },
            .{ .name = "deep_hash_map", .module = deep_hash_map },
            .{ .name = "isa_types", .module = isa_types },
            .{ .name = "misc", .module = misc },
            .{ .name = "sx", .module = sx },
        },
    });

    const assembler_lib = b.createModule(.{
        .source_file = .{ .path = "assembler/assembler_lib.zig" },
        .dependencies = &.{
            .{ .name = "address_translator_types", .module = address_translator_types },
            .{ .name = "bits", .module = bits },
            .{ .name = "bus_types", .module = bus_types },
            .{ .name = "console", .module = console },
            .{ .name = "isa_encoding", .module = isa_encoding },
            .{ .name = "isa_types", .module = isa_types },
        },
    });

    const comptime_encoding = b.createModule(.{
        .source_file = .{ .path = "arch/comptime_encoding.zig" },
        .dependencies = &.{
            .{ .name = "ControlSignals", .module = ControlSignals },
            .{ .name = "isa_encoding", .module = isa_encoding },
            .{ .name = "isa_types", .module = isa_types },
        },
    });

    const ihex = b.createModule(.{
        .source_file = .{ .path = "pkg/ihex.zig" },
    });

    const rom_compress = b.createModule(.{
        .source_file = .{ .path = "pkg/rom_compress.zig" },
        .dependencies = &.{
            .{ .name = "bits", .module = bits },
        },
    });

    const rom_decompress = b.createModule(.{
        .source_file = .{ .path = "pkg/rom_decompress.zig" },
        .dependencies = &.{
            .{ .name = "bits", .module = bits },
            .{ .name = "rom_compress", .module = rom_compress },
        },
    });

    const srec = b.createModule(.{
        .source_file = .{ .path = "pkg/srec.zig" },
    });

    const microcode_roms = b.createModule(.{
        .source_file = .{ .path = "arch/microcode_roms/microcode_roms.zig" },
        .dependencies = &.{
            .{ .name = "ControlSignals", .module = ControlSignals },
            .{ .name = "microcode", .module = microcode },
            .{ .name = "misc", .module = misc },
            .{ .name = "rom_compress", .module = rom_compress },
            .{ .name = "rom_decompress", .module = rom_decompress },
            .{ .name = "srec", .module = srec },
        },
    });

    const assemble_exe = b.addExecutable(.{
        .name = "assemble",
        .root_source_file = .{ .path = "assembler/assemble.zig" },
        .target = target,
        .optimize = mode,
    });
    assemble_exe.addModule("address_translator_types", address_translator_types);
    assemble_exe.addModule("bits", bits);
    assemble_exe.addModule("bus_types", bus_types);
    assemble_exe.addModule("console", console);
    assemble_exe.addModule("ihex", ihex);
    assemble_exe.addModule("isa_encoding", isa_encoding);
    assemble_exe.addModule("isa_types", isa_types);
    assemble_exe.addModule("srec", srec);
    assemble_exe.addModule("sx", sx);
    b.installArtifact(assemble_exe);
    _ = makeRunStep(b, assemble_exe, "assemble", "Run assemble");

    const compile_arch = b.addExecutable(.{
        .name = "compile_arch",
        .root_source_file = .{ .path = "arch/compile_arch/compile_arch.zig" },
        .target = target,
        .optimize = mode,
    });
    compile_arch.addModule("ControlSignals", ControlSignals);
    compile_arch.addModule("TempAllocator", TempAllocator);
    compile_arch.addModule("bits", bits);
    compile_arch.addModule("comptime_encoding", comptime_encoding);
    compile_arch.addModule("deep_hash_map", deep_hash_map);
    compile_arch.addModule("isa_encoding", isa_encoding);
    compile_arch.addModule("isa_types", isa_types);
    compile_arch.addModule("microcode", microcode);
    compile_arch.addModule("microcode_roms", microcode_roms);
    compile_arch.addModule("misc", misc);
    compile_arch.addModule("physical_address", physical_address);
    compile_arch.addModule("sx", sx);
    b.installArtifact(compile_arch);
    _ = makeRunStep(b, compile_arch, "uc", "Run compile_arch");

    const microsim = b.addExecutable(.{
        .name = "microsim",
        .root_source_file = .{ .path = "microsim/microsim.zig" },
        .target = target,
        .optimize = mode,
    });
    microsim.addModule("ControlSignals", ControlSignals);
    microsim.addModule("Simulator", Simulator);
    microsim.addModule("bus_types", bus_types);
    microsim.addModule("isa_encoding", isa_encoding);
    microsim.addModule("microcode", microcode);
    microsim.addModule("microcode_roms", microcode_roms);
    microsim.addModule("misc", misc);
    microsim.addModule("sx", sx);
    microsim.addModule("zglfw", zglfw_pkg.zglfw);
    zglfw_pkg.link(microsim);
    microsim.addModule("zgpu", zgpu_pkg.zgpu);
    zgpu_pkg.link(microsim);
    microsim.addModule("zgui", zgui_pkg.zgui);
    zgui_pkg.link(microsim);
    microsim.want_lto = false;
    b.installArtifact(microsim);
    _ = makeRunStep(b, microsim, "usim", "Run microsim");

    const tests1 = b.addTest(.{
        .root_source_file = .{ .path = "assembler/Constant.zig"},
        .target = target,
        .optimize = mode,
    });
    const run_tests1 = b.addRunArtifact(tests1);

    const tests2 = b.addTest(.{
        .root_source_file = .{ .path = "assembler/SourceFile.zig"},
        .target = target,
        .optimize = mode,
    });
    tests2.addModule("address_translator_types", address_translator_types);
    tests2.addModule("bits", bits);
    tests2.addModule("bus_types", bus_types);
    tests2.addModule("console", console);
    tests2.addModule("isa_encoding", isa_encoding);
    tests2.addModule("isa_types", isa_types);
    const run_tests2 = b.addRunArtifact(tests2);

    const tests3 = b.addTest(.{
        .root_source_file = .{ .path = "assembler/lex.zig"},
        .target = target,
        .optimize = mode,
    });
    const run_tests3 = b.addRunArtifact(tests3);

    const tests4 = b.addTest(.{
        .root_source_file = .{ .path = "pkg/bits.zig"},
        .target = target,
        .optimize = mode,
    });
    const run_tests4 = b.addRunArtifact(tests4);

    const tests5 = b.addTest(.{
        .root_source_file = .{ .path = "pkg/rom_decompress.zig"},
        .target = target,
        .optimize = mode,
    });
    tests5.addModule("bits", bits);
    tests5.addModule("rom_compress", rom_compress);
    const run_tests5 = b.addRunArtifact(tests5);

    const test_step = b.step("test", "Run all tests");
    test_step.dependOn(&run_tests1.step);
    test_step.dependOn(&run_tests2.step);
    test_step.dependOn(&run_tests3.step);
    test_step.dependOn(&run_tests4.step);
    test_step.dependOn(&run_tests5.step);

    _ = assembler_lib;
    //[[ ######################### END OF GENERATED CODE ######################### ]]
}

fn makeRunStep(b: *std.build.Builder, exe: *std.build.CompileStep, name: []const u8, desc: []const u8) *std.build.RunStep {
    var run = b.addRunArtifact(exe);
    run.step.dependOn(b.getInstallStep());
    b.step(name, desc).dependOn(&run.step);
    return run;
}
