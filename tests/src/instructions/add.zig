test "add 123" {
    var debug_log = microsim.Debug_Log.init(std.testing.allocator);
    defer debug_log.deinit();
    errdefer instructions.dump_log(&debug_log, true);

    var sim = try instructions.init_simulator(
        \\ add 123
        \\ add 1
        \\ val 0x222
        \\ add
        \\ park
        \\
        , .p0, &debug_log);
    defer instructions.deinit_simulator(.p0, &sim);

    const p = &sim.state.pipelines[0];

    sim.simulate_reset_and_init();
    sim.simulate_cycles(1);
    try expectEqual(arch.reg.gpr.Write_Index.init(0), p.ti);
    try expectEqual(arch.reg.RSN.init(60), p.status.rsn);
    try expectEqual(arch.reg.gpr.Value.init(0), sim.state.registers[0].reg[0]);
    debug_log.clear();

    sim.simulate_cycles(1); // add 123
    try expectEqual(arch.reg.gpr.Write_Index.init(0), p.ti);
    try expectEqual(arch.reg.gpr.Value.init(123), sim.state.registers[0].reg[0]);
    try expect(!p.flags.contains(.negative));
    try expect(!p.flags.contains(.carry));
    try expect(!p.flags.contains(.overflow));
    try expect(!p.flags.contains(.zero));

    sim.simulate_cycles(1); // add 1
    try expectEqual(arch.reg.gpr.Write_Index.init(0), p.ti);
    try expectEqual(arch.reg.gpr.Value.init(124), sim.state.registers[0].reg[0]);
    try expect(!p.flags.contains(.negative));
    try expect(!p.flags.contains(.carry));
    try expect(!p.flags.contains(.overflow));
    try expect(!p.flags.contains(.zero));

    sim.simulate_cycles(1); // val 0x222
    try expectEqual(arch.reg.gpr.Write_Index.init(1), p.ti);
    try expectEqual(arch.reg.gpr.Value.init(0x222), sim.state.registers[0].reg[1]);
    try expectEqual(arch.reg.gpr.Value.init(124), sim.state.registers[0].reg[0]);
    try expect(!p.flags.contains(.negative));
    try expect(!p.flags.contains(.carry));
    try expect(!p.flags.contains(.overflow));
    try expect(!p.flags.contains(.zero));

    sim.simulate_cycles(1); // add
    try expectEqual(arch.reg.gpr.Write_Index.init(0), p.ti);
    try expectEqual(arch.reg.gpr.Value.init(670), sim.state.registers[0].reg[0]);
    try expect(!p.flags.contains(.negative));
    try expect(!p.flags.contains(.carry));
    try expect(!p.flags.contains(.overflow));
    try expect(!p.flags.contains(.zero));

    // TODO test carry/overflow flags more

    try debug_log.expect(.{},
        \\   243 .zero: reg 0 = 0x0000007B
        \\   247 .zero: reg 0 = 0x0000007C
        \\   251 .zero: reg 1 = 0x00000222
        \\   255 .zero: reg 0 = 0x0000029E
        \\
    );

}

const expect = std.testing.expect;
const expectEqual = std.testing.expectEqual;

const instructions = @import("../instructions.zig");
const microsim = @import("microsim");
const arch = @import("arch");
const std = @import("std");
