var all_tests: *std.Build.Step = undefined;

pub fn build(b: *std.Build) void {
    const arch = b.dependency("arch", .{}).module("arch");
    const isa = b.dependency("isa", .{}).module("isa");
    const microsim = b.dependency("microsim", .{}).module("microsim");
    const zbox = b.dependency("zbox", .{}).module("zbox");

    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const exe = b.addExecutable(.{
        .name = "generate_block_diagrams",
        .root_module = b.createModule(.{
            .root_source_file = b.path("generate_block_diagrams.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{
                .{ .name = "arch", .module = arch },
                .{ .name = "isa", .module = isa },
                .{ .name = "microsim", .module = microsim },
                .{ .name = "zbox", .module = zbox },
            },
        }),
    });
    b.installArtifact(exe);
    b.getInstallStep().dependOn(&b.addRunArtifact(exe).step);
}

const std = @import("std");
