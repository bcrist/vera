pub fn build(b: *std.Build) void {
    const arch = b.dependency("arch", .{}).module("arch");
    const bits = b.dependency("bit_helper", .{}).module("bits");
    const sx = b.dependency("sx", .{}).module("sx");
    const deep_hash_map = b.dependency("deep_hash_map", .{}).module("deep_hash_map");
    const temp_alloc = b.dependency("Temp_Allocator", .{}).module("Temp_Allocator");

    const isa = b.addModule("isa", .{
        .root_source_file = b.path("isa.zig"),
    });
    isa.addImport("arch", arch);
    isa.addImport("bits", bits);
    isa.addImport("deep_hash_map", deep_hash_map);

    const iedb = b.addModule("iedb", .{
        .root_source_file = b.path("iedb.zig"),
    });
    iedb.addImport("arch", arch);
    iedb.addImport("isa", isa);
    iedb.addImport("bits", bits);
    iedb.addImport("sx",  sx);
    iedb.addImport("deep_hash_map", deep_hash_map);

    const compile = b.addExecutable(.{
        .name = "compile",
        .root_source_file = b.path("compile.zig"),
        .target = b.host,
        .optimize = .Debug,
    });
    compile.root_module.addImport("arch", arch);
    compile.root_module.addImport("isa", isa);
    compile.root_module.addImport("iedb", iedb);
    compile.root_module.addImport("bits", bits);
    compile.root_module.addImport("sx",  sx);
    compile.root_module.addImport("Temp_Allocator",  temp_alloc);
    compile.root_module.addImport("deep_hash_map", deep_hash_map);

    b.installArtifact(compile);

    const run = b.addRunArtifact(compile);
    if (b.args) |args| run.addArgs(args);
    b.step("compile", "run ISA/arch compile step").dependOn(&run.step);
}

const std = @import("std");
