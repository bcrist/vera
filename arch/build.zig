pub fn build(b: *std.Build) void {
    const rom_compress = b.dependency("rom_compress", .{});
    const srec = b.dependency("srec", .{}).module("srec");
    const ihex = b.dependency("ihex", .{}).module("ihex");
    const meta = b.dependency("meta", .{}).module("meta");

    _ = b.addModule("arch", .{
        .root_source_file = b.path("arch.zig"),
        .imports = &.{
            .{ .name = "rom_compress", .module = rom_compress.module("rom_compress") },
            .{ .name = "rom_decompress", .module = rom_compress.module("rom_decompress") },
            .{ .name = "srec", .module = srec },
            .{ .name = "ihex", .module = ihex },
            .{ .name = "meta", .module = meta },
        },
    });
}

const std = @import("std");
