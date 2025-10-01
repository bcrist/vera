pub fn build(b: *std.Build) void {
    const isa = b.dependency("isa", .{});
    const arch = b.dependency("arch", .{}).module("arch");
    const console = b.dependency("console_helper", .{}).module("console");
    const bits = b.dependency("bit_helper", .{}).module("bits");
    const sx = b.dependency("sx", .{}).module("sx");
    const ihex = b.dependency("ihex", .{}).module("ihex");
    const srec = b.dependency("srec", .{}).module("srec");

    const module = b.addModule("assembler", .{
        .root_source_file = b.path("assembler.zig"),
        .imports = &.{
            .{ .name = "arch", .module = arch },
            .{ .name = "isa", .module = isa.module("isa") },
            .{ .name = "iedb", .module = isa.module("iedb") },
            .{ .name = "console", .module = console },
            .{ .name = "bits", .module = bits },
            .{ .name = "sx", .module = sx },
            .{ .name = "ihex", .module = ihex },
            .{ .name = "srec", .module = srec },
        },
    });

    const exe = b.addExecutable(.{
        .name = "assemble",
        .root_module = b.createModule(.{
            .root_source_file = b.path("main.zig"),
            .target = b.standardTargetOptions(.{}),
            .optimize = b.standardOptimizeOption(.{}),
            .imports = &.{
                .{ .name = "isa", .module = isa.module("isa") },
                .{ .name = "iedb", .module = isa.module("iedb") },
                .{ .name = "assembler", .module = module },
                .{ .name = "arch", .module = arch },
                .{ .name = "console", .module = console },
            },
        }),
    });

    b.installArtifact(exe);
    
    const run = b.addRunArtifact(exe);
    b.step("run", "run assemble").dependOn(&run.step);
    if (b.args) |args| run.addArgs(args);
}

const std = @import("std");
