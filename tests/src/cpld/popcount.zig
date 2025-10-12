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

    for (0..100_000) |_| {
        const input: u32 = @truncate(rng.next());
        errdefer std.debug.print(
            \\For input: {b:0>32} ({d})
            \\   L1.v1 = {b:0>8}   L1.v2 = {b:0>8}   L1.v4 = {b:0>2}     L1.v8 = {b:0>2}
            \\   L2.v1 = {b:0>2}         L2.v2 = {b:0>4}       L2.v4 = {b:0>4}   L2.v8 = {b:0>3}   L2.v16 = {b}
            \\                      L3.v2 = {b}          L3.v4 = {b:0>2}     L3.v8 = {b:0>2}    L3.v16 = {b}
            \\                                                        L4.v8 = {b:0>2}    L4.v16 = {b:0>2}
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
        try sim.simulate(.{});
        try sim.expect_signal_state(&cpld.outputs, @popCount(input), &names);
    }
}


const cpld = @import("cpld_popcount");
const lc4k = @import("lc4k");
const std = @import("std");
