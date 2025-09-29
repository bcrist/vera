pub fn build(b: *std.Build) void {
    // const target = b.standardTargetOptions(.{});
    // const optimize = b.standardOptimizeOption(.{});

    const arch = b.dependency("arch", .{}).module("arch");
    const isa = b.dependency("isa", .{});
    const bits = b.dependency("bit_helper", .{}).module("bits");
    const sx = b.dependency("sx", .{}).module("sx");

    // const sokol = b.dependency("sokol", .{
    //     .target = target,
    //     .optimize = optimize,
    // });
    // const dear_zig_bindings_sokol = b.dependency("dear_zig_bindings_sokol", .{});
    // const dear_zig_bindings = b.dependency("dear_zig_bindings", .{
    //     .target = target,
    //     .optimize = optimize,
    //     .naming = .snake,
    //     .validate_packed_structs = true,
    // });

    const microsim = b.addModule("microsim", .{
        .root_source_file = b.path("microsim.zig"),
        .imports = &.{
            .{ .name = "arch", .module = arch },
            .{ .name = "isa", .module = isa.module("isa") },
            .{ .name = "bits", .module = bits },
            .{ .name = "sx", .module = sx },
        },
    });

    const sim_data = b.addModule("Simulator_Data", .{
        .root_source_file = b.path("Simulator_Data.zig"),
        .imports = &.{
            .{ .name = "arch", .module = arch },
            .{ .name = "iedb", .module = isa.module("iedb") },
            .{ .name = "iedb.sx", .module = isa.module("iedb.sx") },
            .{ .name = "insn_decode.crom", .module = isa.module("insn_decode.crom") },
            .{ .name = "setup_uc.crom", .module = isa.module("setup_uc.crom") },
            .{ .name = "compute_uc.crom", .module = isa.module("compute_uc.crom") },
            .{ .name = "transact_uc.crom", .module = isa.module("transact_uc.crom") },
            .{ .name = "decode_uc.crom", .module = isa.module("decode_uc.crom") },
        },
    });

    _ = microsim;
    _ = sim_data;

    // const exe = b.addExecutable(.{
    //     .name = "sim",
    //     .root_module = b.createModule(.{
    //         .root_source_file = b.path("gui.zig"),
    //         .target = target,
    //         .optimize = optimize,
    //         .imports = &.{
    //             .{ .name = "microsim", .module = microsim },
    //             .{ .name = "arch", .module = arch },
    //             .{ .name = "isa", .module = isa.module("isa") },
    //             .{ .name = "bits", .module = bits },
    //             .{ .name = "sokol", .module = sokol.module("sokol") },
    //             .{ .name = "ig", .module = dear_zig_bindings.module("ig") },
    //             .{ .name = "sokol_imgui", .module = dear_zig_bindings_sokol.module("sokol_imgui") },
    //         },
    //     }),
    // });
    // b.installArtifact(exe);
    // b.step("run", "Run microsim").dependOn(&b.addRunArtifact(exe).step);
}

const std = @import("std");
