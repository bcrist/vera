pub fn build(b: *std.Build) void {
    const isa = b.dependency("isa", .{});
    const arch = b.dependency("arch", .{}).module("arch");
    const assembler = b.dependency("assembler", .{}).module("assembler");
    const microsim = b.dependency("microsim", .{});
    // const console = b.dependency("console_helper", .{}).module("console");
    // const bits = b.dependency("bit_helper", .{}).module("bits");
    // const sx = b.dependency("sx", .{}).module("sx");
    // const ihex = b.dependency("ihex", .{}).module("ihex");
    // const srec = b.dependency("srec", .{}).module("srec");

    const all_tests = b.step("test", "run all tests");

    {
        const tests = b.addTest(.{
            .name = "lexer",
            .root_source_file = b.path("lex.zig"),
        });
        tests.root_module.addImport("isa", isa.module("isa"));
        const run = b.addRunArtifact(tests);
        all_tests.dependOn(&run.step);
    }
    {
        const tests = b.addTest(.{
            .name = "assembler constants",
            .root_source_file = b.path("assembler/constant.zig"),
        });
        tests.root_module.addImport("isa", isa.module("isa"));
        tests.root_module.addImport("arch", arch);
        tests.root_module.addImport("assembler", assembler);
        const run = b.addRunArtifact(tests);
        all_tests.dependOn(&run.step);
    }
    {
        const tests = b.addTest(.{
            .name = "instruction encoding",
            .root_source_file = b.path("instruction_encoding.zig"),
        });
        tests.root_module.addImport("isa", isa.module("isa"));
        tests.root_module.addImport("iedb", isa.module("iedb"));
        tests.root_module.addImport("iedb.sx", isa.module("iedb.sx"));
        tests.root_module.addImport("arch", arch);
        tests.root_module.addImport("assembler", assembler);
        const run = b.addRunArtifact(tests);
        all_tests.dependOn(&run.step);
    }
    {
        const tests = b.addTest(.{
            .name = "instruction behavior",
            .root_source_file = b.path("instructions.zig"),
            
        });
        tests.root_module.addImport("arch", arch);
        tests.root_module.addImport("isa", isa.module("isa"));
        tests.root_module.addImport("iedb", isa.module("iedb"));
        tests.root_module.addImport("assembler", assembler);
        tests.root_module.addImport("microsim", microsim.module("microsim"));
        tests.root_module.addImport("Simulator_Data", microsim.module("Simulator_Data"));
        const run = b.addRunArtifact(tests);
        all_tests.dependOn(&run.step);
    }
}

const std = @import("std");
