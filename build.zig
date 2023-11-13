const std = @import("std");
const zgui = @import("pkg/zig-gamedev/libs/zgui/build.zig");
const zglfw = @import("pkg/zig-gamedev/libs/zglfw/build.zig");
const zgpu = @import("pkg/zig-gamedev/libs/zgpu/build.zig");
const zpool = @import("pkg/zig-gamedev/libs/zpool/build.zig");

var builder: *std.Build = undefined;
var target: std.zig.CrossTarget = undefined;
var optimize: std.builtin.OptimizeMode = undefined;
var all_tests_step: *std.build.Step = undefined;

pub fn build(b: *std.Build) void {
    builder = b;
    target = b.standardTargetOptions(.{});
    optimize = b.standardOptimizeOption(.{});
    all_tests_step = b.step("test", "Run all tests");

    const ext = .{
        .TempAllocator = b.dependency("Zig-TempAllocator", .{}).module("TempAllocator"),
        .bits = b.dependency("Zig-BitHelper", .{}).module("bits"),
        .console = b.dependency("Zig-ConsoleHelper", .{}).module("console"),
        .deep_hash_map = b.dependency("Zig-DeepHashMap", .{}).module("deep_hash_map"),
        .sx = b.dependency("Zig-SX", .{}).module("sx"),
        .srec = b.dependency("Zig-S-Records", .{}).module("srec"),
        .ihex = b.dependency("Zig-Intel-Hex", .{}).module("ihex"),
        .rom_compress = b.dependency("Zig-ROM-Compress", .{}).module("rom-compress"),
        .rom_decompress = b.dependency("Zig-ROM-Compress", .{}).module("rom-decompress"),
        .zbox = b.dependency("zbox", .{}).module("zbox"),
    };

    const lib_arch = makeModule("lib_arch", .{
        .source_file = .{ .path = "lib/arch.zig" },
        .dependencies = &.{
            .{ .name = "bits", .module = ext.bits },
        },
    });
    lib_arch.dependencies.put("arch", lib_arch) catch @panic("OOM");

    // const lib_assembler = makeModule("assembler", .{
    //     .source_file = .{ .path = "lib/assembler.zig" },
    //     .dependencies = &.{
    //         .{ .name = "bits", .module = ext.bits },
    //         .{ .name = "arch", .module = lib_arch },
    //     },
    // });
    // _ = lib_assembler;

    const lib_microsim = makeModule("lib_microsim", .{
        .source_file = .{ .path = "lib/microsim.zig" },
        .dependencies = &.{
            .{ .name = "bits", .module = ext.bits },
            .{ .name = "arch", .module = lib_arch },
        },
    });

    _ = makeExe("block_diagram", .{
        .source_file = .{ .path = "block_diagram.zig" },
        .dependencies = &.{
            .{ .name = "zbox", .module = ext.zbox },
        },
    });

    // _ = makeExe(b, test_step, "assemble", .{
    //     .source_file = .{ .path = "assemble.zig" },
    //     .dependencies = &.{
    //         .{ .name = "assembler", .module = lib_assembler },
    //     },
    // });

    // _ = makeExe("compile_arch", .{
    //     .source_file = .{ .path = "compile_arch.zig" },
    //     .dependencies = &.{
    //         .{ .name = "TempAllocator", .module = ext.TempAllocator },
    //         .{ .name = "arch", .module = lib_arch },
    //     },
    // });

    const zgui_pkg = zgui.package(b, target, optimize, .{
        .options = .{ .backend = .glfw_wgpu },
    });
    const zglfw_pkg = zglfw.package(b, target, optimize, .{});
    const zpool_pkg = zpool.package(b, target, optimize, .{});
    const zgpu_pkg = zgpu.package(b, target, optimize, .{
        .deps = .{
            .zpool = zpool_pkg.zpool,
            .zglfw = zglfw_pkg.zglfw
        },
    });
    const microsim = makeExe("microsim", .{
        .source_file = .{ .path = "microsim.zig" },
        .dependencies = &.{
            .{ .name = "arch", .module = lib_arch },
            .{ .name = "lib_microsim", .module = lib_microsim },
            .{ .name = "sx", .module = ext.sx },
            .{ .name = "zgpu", .module = zgpu_pkg.zgpu },
            .{ .name = "zgui", .module = zgui_pkg.zgui },
        },
    });
    zglfw_pkg.link(microsim);
    zgpu_pkg.link(microsim);
    zgui_pkg.link(microsim);
    microsim.want_lto = false;
}

fn makeModule(comptime name: []const u8, options: std.build.CreateModuleOptions) *std.build.Module {
    const mod = builder.createModule(options);
    _ = makeTest(name, options);
    return mod;
}

fn makeExe(comptime name: []const u8, options: std.build.CreateModuleOptions) *std.build.CompileStep {
    const exe = builder.addExecutable(.{
        .name = name,
        .root_source_file = options.source_file,
        .target = target,
        .optimize = optimize,
    });

    for (options.dependencies) |dep| {
        exe.addModule(dep.name, dep.module);
    }

    builder.installArtifact(exe);
    var run = builder.addRunArtifact(exe);
    run.step.dependOn(builder.getInstallStep());
    builder.step(name, "run " ++ name).dependOn(&run.step);

    _ = makeTest(name, options);

    return exe;
}

fn makeTest(comptime name: []const u8, options: std.build.CreateModuleOptions) *std.build.CompileStep {
    const t = builder.addTest(.{
        .root_source_file = options.source_file,
        .target = target,
        .optimize = optimize,
    });

    for (options.dependencies) |dep| {
        t.addModule(dep.name, dep.module);
    }

    const run = builder.addRunArtifact(t);
    builder.step("test_" ++ name, "test " ++ name).dependOn(&run.step);
    all_tests_step.dependOn(&run.step);

    return t;
}
