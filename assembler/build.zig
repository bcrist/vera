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
    });
    module.addImport("arch", arch);
    module.addImport("isa", isa.module("isa"));
    module.addImport("iedb", isa.module("iedb"));
    module.addImport("console", console);
    module.addImport("bits", bits);
    module.addImport("sx", sx);
    module.addImport("ihex", ihex);
    module.addImport("srec", srec);

    const exe = b.addExecutable(.{
        .name = "assemble",
        .root_source_file = b.path("main.zig"),
        .target = b.standardTargetOptions(.{}),
        .optimize = b.standardOptimizeOption(.{}),
    });
    exe.root_module.addImport("isa", isa.module("isa"));
    exe.root_module.addImport("iedb", isa.module("iedb"));
    exe.root_module.addImport("iedb.sx", isa.module("iedb.sx"));
    exe.root_module.addImport("assembler", module);
    exe.root_module.addImport("arch", arch);
    exe.root_module.addImport("console", console);

    b.installArtifact(exe);
    
    const run = b.addRunArtifact(exe);
    b.step("run", "run assemble").dependOn(&run.step);
    if (b.args) |args| run.addArgs(args);
}

const std = @import("std");
