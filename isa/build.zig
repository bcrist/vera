pub fn build(b: *std.Build) void {
    const arch = b.dependency("arch", .{}).module("arch");
    const bits = b.dependency("bit_helper", .{}).module("bits");
    const sx = b.dependency("sx", .{}).module("sx");
    const rom_compress = b.dependency("rom_compress", .{});
    const deep_hash_map = b.dependency("deep_hash_map", .{}).module("deep_hash_map");
    const temp_alloc = b.dependency("Temp_Allocator", .{}).module("Temp_Allocator");
    const console = b.dependency("console_helper", .{}).module("console");

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

    const compile_exe = b.addExecutable(.{
        .name = "compile",
        .root_source_file = b.path("compile.zig"),
        .target = b.host,
        .optimize = .Debug,
    });
    compile_exe.root_module.addImport("arch", arch);
    compile_exe.root_module.addImport("isa", isa);
    compile_exe.root_module.addImport("iedb", iedb);
    compile_exe.root_module.addImport("bits", bits);
    compile_exe.root_module.addImport("sx", sx);
    compile_exe.root_module.addImport("rom_decompress", rom_compress.module("rom_decompress"));
    compile_exe.root_module.addImport("Temp_Allocator", temp_alloc);
    compile_exe.root_module.addImport("deep_hash_map", deep_hash_map);
    compile_exe.root_module.addImport("console", console);

    b.installArtifact(compile_exe);

    const run = b.addRunArtifact(compile_exe);
    if (b.args) |args| run.addArgs(args);
    b.step("compile", "run ISA/arch compile step").dependOn(&run.step);

    const compile = b.addRunArtifact(compile_exe);
    compile.addArg("--compact");
    compile.addArg("--db");
    const iedb_sx = compile.addOutputFileArg("iedb.sx");
    compile.addArg("--rom-format");
    compile.addArg("compressed_dump");
    compile.addArg("--setup-uc");
    const setup_uc = compile.addOutputFileArg("setup_uc.crom");
    compile.addArg("--compute-uc");
    const compute_uc = compile.addOutputFileArg("compute_uc.crom");
    compile.addArg("--transact-uc");
    const transact_uc = compile.addOutputFileArg("transact_uc.crom");
    compile.addArg("--id-rom");
    const id_rom = compile.addOutputFileArg("insn_decode.crom");
    compile.addArg("--uc-csv");
    const uc_csv = compile.addOutputFileArg("microcode.csv");
    compile.addArg("--id-csv");
    const id_csv = compile.addOutputFileArg("insn_decode.csv");

    b.getInstallStep().dependOn(&b.addInstallFile(iedb_sx, "iedb.sx").step);
    b.getInstallStep().dependOn(&b.addInstallFile(uc_csv, "microcode.csv").step);
    b.getInstallStep().dependOn(&b.addInstallFile(id_csv, "insn_decode.csv").step);
    b.getInstallStep().dependOn(&b.addInstallFile(id_rom, "insn_decode.crom").step);
    b.getInstallStep().dependOn(&b.addInstallFile(setup_uc, "setup_uc.crom").step);
    b.getInstallStep().dependOn(&b.addInstallFile(compute_uc, "compute_uc.crom").step);
    b.getInstallStep().dependOn(&b.addInstallFile(transact_uc, "transact_uc.crom").step);

    _ = b.addModule("iedb.sx", .{ .root_source_file = iedb_sx });
    _ = b.addModule("insn_decode.crom", .{ .root_source_file = id_rom });
    _ = b.addModule("setup_uc.crom", .{ .root_source_file = setup_uc });
    _ = b.addModule("compute_uc.crom", .{ .root_source_file = compute_uc });
    _ = b.addModule("transact_uc.crom", .{ .root_source_file = transact_uc });
}

const std = @import("std");
