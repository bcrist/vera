pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const deps: Deps = .init(b, target, optimize);

    _ = addBoard(b, "address_translator", deps);
}

fn addBoard(b: *std.Build, comptime name: []const u8, deps: Deps) *std.Build.Module {
    const mod = b.addModule(name, .{
        .root_source_file = b.path(name ++ ".zig"),
    });
    inline for (std.meta.fields(Deps)) |field| {
        mod.addImport(field.name, @field(deps, field.name));
    }
    return mod;
}

const Deps = struct {
    arch: *std.Build.Module,
    isa: *std.Build.Module,
    bits: *std.Build.Module,
    sx: *std.Build.Module,
    lc4k: *std.Build.Module,
    zoink: *std.Build.Module,

    pub fn init(b: *std.Build, target: std.Build.ResolvedTarget, optimize: std.builtin.OptimizeMode) Deps {
        _ = target;
        _ = optimize;
        return .{
            .arch = b.dependency("arch", .{}).module("arch"),
            .isa = b.dependency("isa", .{}).module("isa"),
            .bits = b.dependency("bit_helper", .{}).module("bits"),
            .sx = b.dependency("sx", .{}).module("sx"),
            .lc4k = b.dependency("lc4k", .{}).module("lc4k"),
            .zoink = b.dependency("zoink", .{}).module("zoink"),
        };
    }
};

const std = @import("std");
