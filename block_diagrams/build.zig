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
    // b.installArtifact(exe);
    const generate = b.addRunArtifact(exe);
    const pipeline = generate.addOutputFileArg("pipeline.svg");
    const decode = generate.addOutputFileArg("decode.svg");
    const setup = generate.addOutputFileArg("setup.svg");
    const compute = generate.addOutputFileArg("compute.svg");
    const transact = generate.addOutputFileArg("transact.svg");

    b.getInstallStep().dependOn(&b.addInstallFile(pipeline, "pipeline.svg").step);
    b.getInstallStep().dependOn(&b.addInstallFile(decode, "decode.svg").step);
    b.getInstallStep().dependOn(&b.addInstallFile(setup, "setup.svg").step);
    b.getInstallStep().dependOn(&b.addInstallFile(compute, "compute.svg").step);
    b.getInstallStep().dependOn(&b.addInstallFile(transact, "transact.svg").step);
}

const std = @import("std");
