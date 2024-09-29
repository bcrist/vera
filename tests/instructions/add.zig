test "add 123" {
    var sim = try instructions.init_simulator("add 123", .p1);
    defer instructions.deinit_simulator(.p1, &sim);

    sim.simulate_reset_and_init();
}

const instructions = @import("../instructions.zig");