test "add 123" {
    var sim = try instructions.init_simulator("add 123", .p1, null);
    defer instructions.deinit_simulator(.p1, &sim);

    // sim.simulate_reset_and_init();

    // try expectEqual(arch.Write_Index.init(0), sim.state.pipelines[0].wi);
    // try expectEqual(arch.Register_Set_Number.init(1), sim.state.pipelines[0].rsn);

    sim.simulate_cycles(2);
    //try expectEqual(arch.Reg.init(123), sim.state.registers[1].reg[0]);
    // try expect(s.t.reg.stat.n);
    // try expect(!s.t.reg.stat.c);
    // try expect(!s.t.reg.stat.v);
    // try expect(!s.t.reg.stat.z);

    // s.resetAndInit();
    // s.register_file.writeGPR32(0, 12, 123456);
    // s.cycle(2);
    // try expectEqual(@as(u32, 123328), s.register_file.readGPR32(0, 1));
    // try expect(!s.t.reg.stat.n);
    // try expect(s.t.reg.stat.c);
    // try expect(!s.t.reg.stat.v);
    // try expect(!s.t.reg.stat.z);

    // s.resetAndInit();
    // s.register_file.writeGPR32(0, 12, 128);
    // //try s.debugCycle(2, .one);
    // s.cycle(2);
    // try expectEqual(@as(u32, 0), s.register_file.readGPR32(0, 1));
    // try expect(!s.t.reg.stat.n);
    // try expect(s.t.reg.stat.c);
    // try expect(!s.t.reg.stat.v);
    // try expect(s.t.reg.stat.z);
}

const expect = std.testing.expect;
const expectEqual = std.testing.expectEqual;

const instructions = @import("../instructions.zig");
const arch = @import("arch");
const std = @import("std");
