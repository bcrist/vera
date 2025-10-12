var all_tests: *std.Build.Step = undefined;

pub fn build(b: *std.Build) void {
    const isa = b.dependency("isa", .{});
    const arch = b.dependency("arch", .{}).module("arch");
    const assembler = b.dependency("assembler", .{}).module("assembler");
    const microsim = b.dependency("microsim", .{});
    const boards = b.dependency("boards", .{});
    const lc4k = b.dependency("lc4k", .{}).module("lc4k");
    const zoink = b.dependency("zoink", .{}).module("zoink");

    all_tests = b.step("test", "run all tests");

    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    add_test(b, "arch", b.createModule(.{
        .root_source_file = b.path("src/arch.zig"),
        .target = target,
        .optimize = optimize,
        .imports = &.{
            .{ .name = "arch", .module = arch },
        },
    }));

    add_test(b, "isa", b.createModule(.{
        .root_source_file = b.path("src/isa.zig"),
        .target = target,
        .optimize = optimize,
        .imports = &.{
            .{ .name = "isa", .module = isa.module("isa") },
        },
    }));

    add_test(b, "microsim", b.createModule(.{
        .root_source_file = b.path("src/microsim.zig"),
        .target = target,
        .optimize = optimize,
        .imports = &.{
            .{ .name = "arch", .module = arch },
            .{ .name = "isa", .module = isa.module("isa") },
            .{ .name = "microsim", .module = microsim.module("microsim") },
            .{ .name = "Simulator_Data", .module = microsim.module("Simulator_Data") },
        },
    }));

    add_test(b, "assembler", b.createModule(.{
        .root_source_file = b.path("src/assembler.zig"),
        .target = target,
        .optimize = optimize,
        .imports = &.{
            .{ .name = "isa", .module = isa.module("isa") },
            .{ .name = "arch", .module = arch },
            .{ .name = "assembler", .module = assembler },
        },
    }));

    add_test(b, "instruction_encoding", b.createModule(.{
        .root_source_file = b.path("src/instruction_encoding.zig"),
        .target = target,
        .optimize = optimize,
        .imports = &.{
            .{ .name = "isa", .module = isa.module("isa") },
            .{ .name = "iedb", .module = isa.module("iedb") },
            .{ .name = "arch", .module = arch },
        },
    }));

    add_test(b, "instruction_behavior", b.createModule(.{
        .root_source_file = b.path("src/instructions.zig"),
        .target = target,
        .optimize = optimize,
        .imports = &.{
            .{ .name = "arch", .module = arch },
            .{ .name = "isa", .module = isa.module("isa") },
            .{ .name = "iedb", .module = isa.module("iedb") },
            .{ .name = "assembler", .module = assembler },
            .{ .name = "microsim", .module = microsim.module("microsim") },
            .{ .name = "Simulator_Data", .module = microsim.module("Simulator_Data") },
        },
    }));

    add_test(b, "boards", b.createModule(.{
        .root_source_file = b.path("src/boards.zig"),
        .target = target,
        .optimize = optimize,
        .imports = &.{
            .{ .name = "arch", .module = arch },
            .{ .name = "boards", .module = boards.module("address_translator") },
            .{ .name = "zoink", .module = zoink },
        },
    }));

    add_test(b, "cpld", b.createModule(.{
        .root_source_file = b.path("src/cpld.zig"),
        .target = target,
        .optimize = optimize,
        .imports = &.{
            .{ .name = "arch", .module = arch },
            .{ .name = "cpld_popcount", .module = boards.module("cpld_popcount") },
            .{ .name = "lc4k", .module = lc4k },
        },
    }));
}

fn add_test(b: *std.Build, comptime name: []const u8, root_module: *std.Build.Module) void {
    root_module.addImport("meta", b.dependency("meta", .{}).module("meta"));
    const tests = b.addTest(.{
        .name = name,
        .root_module = root_module,
    });
    b.installArtifact(tests);
    const run = b.addRunArtifact(tests);
    all_tests.dependOn(&run.step);
    b.step("test_" ++ name, "test " ++ name).dependOn(&run.step);
}

const std = @import("std");
