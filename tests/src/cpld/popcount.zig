test {
    var names: cpld.Chip.Names = .init(std.testing.allocator);
    defer names.deinit();

    var lp: cpld.Chip.Logic_Parser = .{
        .gpa = std.testing.allocator,
        .arena = .init(std.testing.allocator),
        .names = &names,
    };
    defer lp.arena.deinit();

    const chip = try cpld.configure(&names, &lp);

    var rng = std.Random.Xoshiro256.init(std.testing.random_seed);
    var sim = chip.simulator(null);

    sim.set_input_low(cpld.clk);
    try sim.simulate(.{});

    for (std.enums.values(arch.compute.Unit)) |unit| {
        sim.set_inputs(&cpld.unit, unit.raw());
        sim.set_input_high(cpld.clk);
        try sim.simulate(.{});
        sim.set_input_low(cpld.clk);
        try sim.simulate(.{});
        try sim.expect_oe_state(&cpld.outputs, if (unit == .count) 0b111111 else 0, &names);
        try sim.expect_signal_state(&.{ cpld.oe_out }, if (unit == .count) 0 else 1, &names);
    }

    sim.set_inputs(&cpld.unit, arch.compute.Unit.count.raw());

    for (0..100_000) |_| {
        const input: u32 = @truncate(rng.next());
        errdefer std.debug.print(
            \\For input: {b:0>32} ({d})
            \\   L1.v1 = {b:0>8}   L1.v2 = {b:0>8}   L1.v4 = {b:0>3}    L1.v8 = {b:0>3}
            \\   L2.v1 = {b:0>2}         L2.v2 = {b:0>4}       L2.v4 = {b:0>4}   L2.v8 = {b:0>2}   L2.v16 = {b:0>2}
            \\                      L3.v2 = {b}          L3.v4 = {b:0>2}     L3.v8 = {b:0>2}   L3.v16 = {b:0>2}
            \\                                                        L4.v8 = {b:0>2}   L4.v16 = {b:0>2}
            \\
        , .{
            input,
            @popCount(input),

            sim.read_signal_state(&cpld.L1.v1),
            sim.read_signal_state(&cpld.L1.v2),
            sim.read_signal_state(&cpld.L1.v4),
            sim.read_signal_state(&cpld.L1.v8),

            sim.read_signal_state(&cpld.L2.v1),
            sim.read_signal_state(&cpld.L2.v2),
            sim.read_signal_state(&cpld.L2.v4),
            sim.read_signal_state(&cpld.L2.v8),
            sim.read_signal_state(&cpld.L2.v16),

            sim.read_signal_state(&cpld.L3.v2),
            sim.read_signal_state(&cpld.L3.v4),
            sim.read_signal_state(&cpld.L3.v8),
            sim.read_signal_state(&cpld.L3.v16),

            sim.read_signal_state(&cpld.L4.v8),
            sim.read_signal_state(&cpld.L4.v16),
        });

        sim.set_inputs(&cpld.inputs, input);
        sim.set_input_high(cpld.clk);
        try sim.simulate(.{});
        sim.set_input_low(cpld.clk);
        try sim.simulate(.{});
        try sim.expect_signal_state(&cpld.outputs, @popCount(input), &names);
    }

    const results = try chip.assemble(lp.arena.allocator(), .{});
    defer results.error_arena.deinit();
    try std.testing.expect(results.errors.items.len == 0);
}

const cpld = @import("cpld_popcount");
const arch = @import("arch");
const lc4k = @import("lc4k");
const std = @import("std");
