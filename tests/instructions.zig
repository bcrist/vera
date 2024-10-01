test {
    _ = @import("instructions/reset.zig");
    _ = @import("instructions/add.zig");
}

const assembly_prefix =
    \\    .boot
    \\    .org 0
    \\_reset:                         .dw (.raw @reset)'16
    \\_double_fault:                  .dw (.raw @halt)'16
    \\_page_fault:                    .dw (.raw @halt)'16
    \\_access_fault:                  .dw (.raw @halt)'16
    \\_page_align_fault:              .dw (.raw @halt)'16
    \\_align_fault:                   .dw (.raw @halt)'16
    \\_overflow_fault:                .dw (.raw @halt)'16
    \\_invalid_instruction_fault:     .dw (.raw @halt)'16
    \\_instruction_protection_fault:  .dw (.raw @halt)'16
    \\_interrupt:                     .dw (.raw @halt)'16
    \\
    \\halt:
    \\    park
    \\
    \\reset:
    \\
    ;

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

    const mem = try assembler.output.copy_memory(&a, null, std.testing.allocator);
    defer std.testing.allocator.free(mem);

    const num_frames = (mem.len + arch.addr.Frame.count - 1) / arch.addr.Frame.count;
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

// test "ADD X12, -128 -> X1" {
//     var s = try initSimulator(&[_]ie.Instruction{
//         .{
//             .mnemonic = .ADD,
//             .suffix = .none,
//             .params = &[_]ie.Parameter{
//                 ie.parameter(.reg32, 12),
//                 ie.parameter(.constant, -128),
//                 ie.toParameter(.reg32, 1),
//             },
//         },
//     });
//     defer deinitSimulator(&s);


// }

// test "ADD X0, R4U -> X0" {
//     var s = try initSimulator(&[_]ie.Instruction{
//         .{
//             .mnemonic = .ADD,
//             .suffix = .none,
//             .params = &[_]ie.Parameter{
//                 ie.parameter(.reg32, 7),
//                 ie.parameter(.reg16u, 4),
//                 ie.toParameter(.reg32, 7),
//             },
//         },
//     });
//     defer deinitSimulator(&s);

//     s.register_file.writeGPR32(0, 7, 0);
//     s.register_file.writeGPR(0, 4, 33000);
//     s.cycle(1);
//     try expectEqual(@as(u32, 33000), s.register_file.readGPR32(0, 7));
//     try expect(!s.t.reg.stat.n);
//     try expect(!s.t.reg.stat.c);
//     try expect(!s.t.reg.stat.v);
//     try expect(!s.t.reg.stat.z);

//     s.resetAndInit();
//     s.cycle(1);
//     try expectEqual(@as(u32, 66000), s.register_file.readGPR32(0, 7));
//     try expect(!s.t.reg.stat.n);
//     try expect(!s.t.reg.stat.c);
//     try expect(!s.t.reg.stat.v);
//     try expect(!s.t.reg.stat.z);
// }

// test "ADD X1, R3S, X1" {
//     var s = try initSimulator(&[_]ie.Instruction{
//         .{
//             .mnemonic = .ADD,
//             .suffix = .none,
//             .params = &[_]ie.Parameter{
//                 ie.parameter(.reg32, 7),
//                 ie.parameter(.reg16s, 3),
//                 ie.toParameter(.reg32, 7),
//             },
//         },
//     });
//     defer deinitSimulator(&s);

//     s.register_file.writeSignedGPR(0, 3, @as(i16, -32000));
//     s.cycle(1);
//     try expectEqual(@as(i32, -32000), s.register_file.readSignedGPR32(0, 7));
//     try expect(s.t.reg.stat.n);
//     try expect(!s.t.reg.stat.c);
//     try expect(!s.t.reg.stat.v);
//     try expect(!s.t.reg.stat.z);

//     s.resetAndInit();
//     s.cycle(1);
//     try expectEqual(@as(i32, -64000), s.register_file.readSignedGPR32(0, 7));
//     try expect(s.t.reg.stat.n);
//     try expect(s.t.reg.stat.c);
//     try expect(!s.t.reg.stat.v);
//     try expect(!s.t.reg.stat.z);
// }

// test "ADDC R5, 12345, R4" {
//     var s = try initSimulator(&[_]ie.Instruction{.{
//         .mnemonic = .ADDC,
//         .suffix = .none,
//         .params = &[_]ie.Parameter{
//             ie.parameter(.reg16, 5),
//             ie.parameter(.constant, 12345),
//             ie.toParameter(.reg16, 4),
//         },
//     }});
//     defer deinitSimulator(&s);
//     s.t.reg.stat.c = true;
//     s.cycle(2);
//     try expectEqual(@as(u32, 12346), s.register_file.readGPR(0, 4));
//     try expect(!s.t.reg.stat.n);
//     try expect(!s.t.reg.stat.c);
//     try expect(!s.t.reg.stat.v);
//     try expect(!s.t.reg.stat.z);
// }

const Simulator_Data = @import("Simulator_Data");
const Debug_Log = microsim.Debug_Log;
const Simulator = microsim.Simulator;
const microsim = @import("microsim");
const assembler = @import("assembler");
const arch = @import("arch");
const std = @import("std");
