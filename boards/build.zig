pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const deps: Deps = .init(b, target, optimize);

    _ = addCpld(b, "popcount", deps);

    _ = addBoard(b, "address_translator", deps);

}

fn addBoard(b: *std.Build, comptime name: []const u8, deps: Deps) *std.Build.Module {
    const mod = b.addModule(name, .{
        .root_source_file = b.path(name ++ ".zig"),
    });
    inline for (@typeInfo(Deps).@"struct".fields) |field| {
        if (field.type == *std.Build.Module) {
            mod.addImport(field.name, @field(deps, field.name));
        }
    }
    return mod;
}

fn addCpld(b: *std.Build, comptime name: []const u8, deps: Deps) *std.Build.Module {
    const mod = b.addModule("cpld_" ++ name, .{
        .root_source_file = b.path("cpld/" ++ name ++ ".zig"),
        .target = deps.target,
        .optimize = deps.optimize,
    });
    inline for (@typeInfo(Deps).@"struct".fields) |field| {
        if (field.type == *std.Build.Module) {
            mod.addImport(field.name, @field(deps, field.name));
        }
    }
    const firmware = b.addExecutable(.{
        .name = "cpld_firmware_" ++ name,
        .root_module = mod,
    });
    const generate = b.addRunArtifact(firmware);
    const jed = generate.addOutputFileArg(name ++ ".jed");
    const svf = generate.addOutputFileArg(name ++ ".svf");
    const html = generate.addOutputFileArg(name ++ ".html");

    // b.installArtifact(firmware);
    b.getInstallStep().dependOn(&b.addInstallFile(jed, "firmware/" ++ name ++ ".jed").step);
    b.getInstallStep().dependOn(&b.addInstallFile(svf, "firmware/" ++ name ++ ".svf").step);
    b.getInstallStep().dependOn(&b.addInstallFile(html, "firmware/" ++ name ++ ".html").step);

    return mod;
}

const Deps = struct {
    target: std.Build.ResolvedTarget,
    optimize: std.builtin.OptimizeMode,
    arch: *std.Build.Module,
    isa: *std.Build.Module,
    bits: *std.Build.Module,
    lc4k: *std.Build.Module,
    zoink: *std.Build.Module,

    pub fn init(b: *std.Build, target: std.Build.ResolvedTarget, optimize: std.builtin.OptimizeMode) Deps {
        return .{
            .target = target,
            .optimize = optimize,
            .arch = b.dependency("arch", .{}).module("arch"),
            .isa = b.dependency("isa", .{}).module("isa"),
            .bits = b.dependency("bit_helper", .{}).module("bits"),
            .lc4k = b.dependency("lc4k", .{}).module("lc4k"),
            .zoink = b.dependency("zoink", .{}).module("zoink"),
        };
    }
};

const std = @import("std");
