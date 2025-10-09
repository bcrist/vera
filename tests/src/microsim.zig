test "p0" {
    var debug_log = microsim.Debug_Log.init(std.testing.allocator);
    defer debug_log.deinit();
    errdefer debug_log.dump(null, .{ .insn_stuff = true }) catch {};

    const sim_data = try Simulator_Data.get();

    const num_frames = 2;
    const mem_dev = try std.testing.allocator.create(microsim.devices.Simple_Memory);
    defer std.testing.allocator.destroy(mem_dev);
    mem_dev.* = try .init(std.testing.allocator, .zero, num_frames);
    defer mem_dev.deinit();
    @memset(mem_dev.data, 0xFF);

    var sim = try microsim.Simulator(.p0).init(std.testing.allocator, sim_data.insn_decode, sim_data.microcode, &.{
        mem_dev.device(),
    }, &debug_log);
    defer sim.deinit();

    sim.simulate_reset_and_init();
    sim.simulate_cycles(1);

    var w = std.io.Writer.Allocating.init(std.testing.allocator);
    defer w.deinit();

    try write_expected_log(.zero, &w.writer);

    try debug_log.expect(.{ .insn_stuff = true }, w.written());
}

test "p1" {
    var debug_log = microsim.Debug_Log.init(std.testing.allocator);
    defer debug_log.deinit();
    errdefer debug_log.dump(null, .{ .insn_stuff = true }) catch {};

    const sim_data = try Simulator_Data.get();

    const num_frames = 2;
    const mem_dev = try std.testing.allocator.create(microsim.devices.Simple_Memory);
    defer std.testing.allocator.destroy(mem_dev);
    mem_dev.* = try .init(std.testing.allocator, .zero, num_frames);
    defer mem_dev.deinit();
    @memset(mem_dev.data, 0xFF);

    var sim = try microsim.Simulator(.p1).init(std.testing.allocator, sim_data.insn_decode, sim_data.microcode, &.{
        mem_dev.device(),
    }, &debug_log);
    defer sim.deinit();

    sim.simulate_reset_and_init();
    sim.simulate_cycles(1);

    var w = std.io.Writer.Allocating.init(std.testing.allocator);
    defer w.deinit();

    try write_expected_log(.one, &w.writer);

    try debug_log.expect(.{ .insn_stuff = true }, w.written());
}

test "p2" {
    var debug_log = microsim.Debug_Log.init(std.testing.allocator);
    defer debug_log.deinit();
    errdefer debug_log.dump(null, .{ .insn_stuff = true }) catch {};

    const sim_data = try Simulator_Data.get();

    const num_frames = 2;
    const mem_dev = try std.testing.allocator.create(microsim.devices.Simple_Memory);
    defer std.testing.allocator.destroy(mem_dev);
    mem_dev.* = try .init(std.testing.allocator, .zero, num_frames);
    defer mem_dev.deinit();
    @memset(mem_dev.data, 0xFF);

    var sim = try microsim.Simulator(.p2).init(std.testing.allocator, sim_data.insn_decode, sim_data.microcode, &.{
        mem_dev.device(),
    }, &debug_log);
    defer sim.deinit();

    sim.simulate_reset_and_init();
    sim.simulate_cycles(1);

    var w = std.io.Writer.Allocating.init(std.testing.allocator);
    defer w.deinit();

    try write_expected_log(.two, &w.writer);

    try debug_log.expect(.{ .insn_stuff = true }, w.written());
}

test "p3" {
    var debug_log = microsim.Debug_Log.init(std.testing.allocator);
    defer debug_log.deinit();
    errdefer debug_log.dump(null, .{ .insn_stuff = true }) catch {};

    const sim_data = try Simulator_Data.get();

    const num_frames = 2;
    const mem_dev = try std.testing.allocator.create(microsim.devices.Simple_Memory);
    defer std.testing.allocator.destroy(mem_dev);
    mem_dev.* = try .init(std.testing.allocator, .zero, num_frames);
    defer mem_dev.deinit();
    @memset(mem_dev.data, 0xFF);

    var sim = try microsim.Simulator(.p3).init(std.testing.allocator, sim_data.insn_decode, sim_data.microcode, &.{
        mem_dev.device(),
    }, &debug_log);
    defer sim.deinit();

    sim.simulate_reset_and_init();
    sim.simulate_cycles(1);

    var w = std.io.Writer.Allocating.init(std.testing.allocator);
    defer w.deinit();

    try write_expected_log(.three, &w.writer);

    try debug_log.expect(.{ .insn_stuff = true }, w.written());
}

pub fn write_expected_log(comptime pipe: arch.Pipeline, w: *std.io.Writer) !void {
    var c: u64 = 3;
    try w.print("{d: >6} .{t} .reset: sr1 .temp_1 = 0x{X:0>8}\n", .{ c, pipe, @as(u32, pipe.raw()) << 6 });
    c += 4;
    try w.print("{d: >6} .{t} .reset: sr1 .temp_1 = 0x{X:0>8}\n", .{ c, pipe, @as(u32, pipe.raw()) << 6 });
    c += 4;
    try w.print("{d: >6} .{t} 0x018: sr1 .temp_1 = 0x{X:0>8}\n", .{ c, pipe, pipe.raw() });
    c += 4;
    try w.print("{d: >6} .{t} 0x017: rsn = .{t}\n", .{ c, pipe, arch.reg.RSN.init(arch.reg.RSN.interrupt_pipe_0.raw() + pipe.raw()) });
    try w.print("{d: >6} .{t} 0x017: sr1 .temp_1 = 0x{X:0>8}\n", .{ c, pipe, pipe.raw() });
    try w.print("{d: >6} .{t} 0x017: sr2 .temp_2 = 0x{X:0>8}\n", .{ c, pipe, pipe.raw() });
    c += 4;
    try w.print("{d: >6} .{t} 0x016: sr2 .zero = 0x00000000\n", .{ c, pipe });
    c += 4;
    try w.print("{d: >6} .{t} 0x015: sr1 .one = 0x00000001\n", .{ c, pipe });
    c += 4;
    try w.print("{d: >6} .{t} 0x014: sr1 .two = 0x00000002\n", .{ c, pipe });
    c += 4;
    try w.print("{d: >6} .{t} 0x013: sr2 .temp_2 = 0x{X:0>8}\n", .{ c, pipe, @as(u32, pipe.raw()) + 30 });
    c += 4;
    try w.print("{d: >6} .{t} 0x012: rsn = {d}\n", .{ c, pipe, @as(arch.reg.RSN.Raw, 60) + pipe.raw() });
    try w.print("{d: >6} .{t} 0x012: sr2 .temp_2 = 0x{X:0>8}\n", .{ c, pipe, @as(u32, pipe.raw()) + 60 });
    
    const initial_rsn: arch.reg.RSN.Raw = @as(arch.reg.RSN.Raw, 60) + pipe.raw();
    var rsn_raw: u32 = initial_rsn;
    while (rsn_raw <= initial_rsn) {
        rsn_raw -%= 4;
        const rsn = arch.reg.RSN.init(@truncate(rsn_raw));
        c += 4;
        try w.print("{d: >6} .{t} 0x011: sr1 .one = 0x00000001\n", .{ c, pipe });
        try w.print("{d: >6} .{t} 0x011: sr2 .zero = 0x00000000\n", .{ c, pipe });
        c += 4;
        try w.print("{d: >6} .{t} 0x00E: sr1 .two = 0x00000002\n", .{ c, pipe });
        c += 4;
        if (rsn_raw <= arch.reg.RSN.fault_pipe_3.raw()) {
            try w.print("{d: >6} .{t} 0x00D: rsn = .{t}\n", .{ c, pipe, rsn });
        } else {
            try w.print("{d: >6} .{t} 0x00D: rsn = {d}\n", .{ c, pipe, rsn.raw() });
        }
        try w.print("{d: >6} .{t} 0x00D: sr2 .temp_2 = 0x{X:0>8}\n", .{ c, pipe, rsn_raw });
    }

    c += 4;
    try w.print("{d: >6} .{t} 0x011: sr2 .kxp = 0x0000000{X:0>1}\n", .{ c, pipe, pipe });
    c += 4;
    try w.print("{d: >6} .{t} 0x010: read  .physical DB:0xFFFF DA:0xFFFF F:.zero AB:0x000 AA:0x000 ..Aa  \n", .{ c, pipe });
    try w.print("{d: >6} .{t} 0x010: sr2 .next_ip = 0x0000FFFF\n", .{ c, pipe });
    c += 4;
    try w.print("{d: >6} .{t} 0x00F: fault: .page_align_fault\n", .{ c, pipe });
}

const Simulator_Data = @import("Simulator_Data");
const microsim = @import("microsim");
const isa = @import("isa");
const arch = @import("arch");
const std = @import("std");