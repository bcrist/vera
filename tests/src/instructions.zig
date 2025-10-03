comptime {
    _ = @import("instructions/add.zig");
}

const assembly_prefix = str: {
    var result: []const u8 = 
        \\    .boot
        \\    .org 0
        \\_reset: .dh (.raw @reset)'16
        ;
    for (@typeInfo(arch.data_structures.Vector_Table).@"struct".fields) |field| {
        if (!std.mem.eql(u8, field.name, "reset")) {
            result = result ++ "\n_" ++ field.name ++ ": .dh (.raw @halt)'16";
        }
    }

    break :str result ++
        \\
        \\halt:
        \\    park
        \\
        \\reset:
        \\
        ;
};

pub fn init_simulator(assembly: []const u8, comptime pipe: microsim.Pipeline, debug_log: ?*Debug_Log) !Simulator(pipe) {
    const data = try Simulator_Data.get();

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    var a = assembler.Assembler.init(std.testing.allocator, arena.allocator(), data.edb);
    defer a.deinit(false);

    const final_assembly = try std.mem.concat(std.testing.allocator, u8, &.{ assembly_prefix, assembly });
    defer std.testing.allocator.free(final_assembly);

    _ = a.add_source("main", final_assembly);
    a.assemble();

    if (a.errors.items.len > 0) {
        var buf: [64]u8 = undefined;
        var w = std.fs.File.stderr().writer(&buf);
        a.print_errors(&w.interface) catch {};
        w.interface.flush() catch {};
        return error.BadAssembly;
    }

    const mem = try assembler.output.copy_memory(&a, null, std.testing.allocator);
    defer std.testing.allocator.free(mem);

    const num_frames = (mem.len + arch.addr.Frame.Offset.count - 1) / arch.addr.Frame.Offset.count;
    const mem_dev = try std.testing.allocator.create(microsim.devices.Simple_Memory);
    mem_dev.* = try microsim.devices.Simple_Memory.init(std.testing.allocator, .zero, num_frames);
    @memset(mem_dev.data, 0xFF);
    @memcpy(mem_dev.data.ptr, mem);

    return try Simulator(pipe).init(std.testing.allocator, data.insn_decode, data.microcode, &.{
        mem_dev.device(),
    }, debug_log);
}

pub fn deinit_simulator(comptime pipe: microsim.Pipeline, simulator: *Simulator(pipe)) void {
    var mem_dev: *microsim.devices.Simple_Memory = @alignCast(@ptrCast(simulator.global_devices[0].ctx));
    mem_dev.deinit();
    std.testing.allocator.destroy(mem_dev);

    simulator.deinit();
}

pub fn dump_log(debug_log: *microsim.Debug_Log, insn_stuff: bool) void {
    var buf: [64]u8 = undefined;
    var w = std.fs.File.stderr().writer(&buf);
    w.interface.writeAll("Debug Log:\n") catch {};
    debug_log.dump(&w.interface, .{ .insn_stuff = insn_stuff }) catch {};
    w.interface.flush() catch {};
}

const Simulator_Data = @import("Simulator_Data");
const Debug_Log = microsim.Debug_Log;
const Simulator = microsim.Simulator;
const microsim = @import("microsim");
const assembler = @import("assembler");
const arch = @import("arch");
const std = @import("std");
