test "add 123" {
    var debug_log = microsim.Debug_Log.init(std.testing.allocator);
    defer debug_log.deinit();
    errdefer debug_log.dump(std.io.getStdErr().writer(), .{ .insn_stuff = true }) catch {};

    var sim = try instructions.init_simulator(
        \\ add 123
        \\ add 1
        \\ val 0x222
        \\ add
        \\
        , .p0, &debug_log);
    defer instructions.deinit_simulator(.p0, &sim);

    const p = &sim.state.pipelines[0];

    sim.simulate_reset_and_init();
    sim.simulate_cycles(1);
    try expectEqual(arch.Write_Index.init(0), p.ti);
    try expectEqual(arch.Register_Set_Number.init(0), p.rsn);
    try expectEqual(arch.Reg.init(0), sim.state.registers[0].reg[0]);
    debug_log.clear();

    sim.simulate_cycles(1); // add 123
    try expectEqual(arch.Write_Index.init(0), p.ti);
    try expectEqual(arch.Reg.init(123), sim.state.registers[0].reg[0]);
    try expect(!p.flags.contains(.stat_n));
    try expect(!p.flags.contains(.carry_flag));
    try expect(!p.flags.contains(.stat_v));
    try expect(!p.flags.contains(.stat_z));

    sim.simulate_cycles(1); // add 1
    try expectEqual(arch.Write_Index.init(0), p.ti);
    try expectEqual(arch.Reg.init(124), sim.state.registers[0].reg[0]);
    try expect(!p.flags.contains(.stat_n));
    try expect(!p.flags.contains(.carry_flag));
    try expect(!p.flags.contains(.stat_v));
    try expect(!p.flags.contains(.stat_z));

    sim.simulate_cycles(1); // val 0x222
    try expectEqual(arch.Write_Index.init(1), p.ti);
    try expectEqual(arch.Reg.init(0x222), sim.state.registers[0].reg[1]);
    try expectEqual(arch.Reg.init(124), sim.state.registers[0].reg[0]);
    try expect(!p.flags.contains(.stat_n));
    try expect(!p.flags.contains(.carry_flag));
    try expect(!p.flags.contains(.stat_v));
    try expect(!p.flags.contains(.stat_z));

    sim.simulate_cycles(1); // add
    try expectEqual(arch.Write_Index.init(0), p.ti);
    try expectEqual(arch.Reg.init(670), sim.state.registers[0].reg[0]);
    try expect(!p.flags.contains(.stat_n));
    try expect(!p.flags.contains(.carry_flag));
    try expect(!p.flags.contains(.stat_v));
    try expect(!p.flags.contains(.stat_z));

    // TODO test carry/overflow flags more

    try debug_log.expect(.{},
        \\    75 .zero: reg 0 = 0x0000007B
        \\    79 .zero: reg 0 = 0x0000007C
        \\    83 .zero: reg 1 = 0x00000222
        \\    87 .zero: reg 0 = 0x0000029E
        \\
    );

}

const expect = std.testing.expect;
const expectEqual = std.testing.expectEqual;

const instructions = @import("../instructions.zig");
const microsim = @import("microsim");
const arch = @import("arch");
const std = @import("std");
