pub fn build(b: *std.Build) void {
    const rom_compress = b.dependency("rom_compress", .{});
    const srec = b.dependency("srec", .{}).module("srec");
    const ihex = b.dependency("ihex", .{}).module("ihex");

    const arch = b.addModule("arch", .{
        .root_source_file = b.path("arch.zig"),
    });
    arch.addImport("rom_compress", rom_compress.module("rom_compress"));
    arch.addImport("rom_decompress", rom_compress.module("rom_decompress"));
    arch.addImport("srec", srec);
    arch.addImport("ihex", ihex);
}

const std = @import("std");
