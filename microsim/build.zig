pub fn build(b: *std.Build) void {
    const arch = b.dependency("arch", .{}).module("arch");
    const isa = b.dependency("isa", .{});
    const bits = b.dependency("bit_helper", .{}).module("bits");

    const microsim = b.addModule("microsim", .{
        .root_source_file = b.path("microsim.zig"),
    });
    microsim.addImport("arch", arch);
    microsim.addImport("isa", isa.module("isa"));
    microsim.addImport("bits", bits);

    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const exe = b.addExecutable(.{
        .name = "sim",
        .target = target,
        .root_source_file = b.path("main.zig"),
        .optimize = optimize,
    });
    exe.root_module.addImport("microsim", microsim);
    exe.root_module.addImport("arch", arch);
    exe.root_module.addImport("isa", isa.module("isa"));
    exe.root_module.addImport("bits", bits);

    b.installArtifact(exe);

    const run = b.addRunArtifact(exe);
    b.step("run", "Run microsim GUI").dependOn(&run.step);
    if (b.args) |args| run.addArgs(args);
}

const std = @import("std");
