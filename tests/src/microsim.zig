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

    try debug_log.expect(.{ .insn_stuff = true },
        \\     3 .zero .reset: sr1 .temp_1 = 0x00000000
        \\     7 .zero .reset: sr1 .temp_1 = 0x00000000
        \\    11 .zero 0x016: sr1 .temp_1 = 0x00000000
        \\    15 .zero 0x015: rsn = .interrupt_pipe_0
        \\    15 .zero 0x015: sr1 .temp_1 = 0x00000000
        \\    15 .zero 0x015: sr2 .temp_2 = 0x00000000
        \\    19 .zero 0x014: sr2 .zero = 0x00000000
        \\    23 .zero 0x013: sr1 .one = 0x00000001
        \\    27 .zero 0x012: sr2 .temp_2 = 0x0000001E
        \\    31 .zero 0x011: rsn = 60
        \\    31 .zero 0x011: sr2 .temp_2 = 0x0000003C
        \\    35 .zero 0x010: sr1 .one = 0x00000001
        \\    35 .zero 0x010: sr2 .zero = 0x00000000
        \\    39 .zero 0x00D: rsn = 56
        \\    39 .zero 0x00D: sr2 .temp_2 = 0x00000038
        \\    43 .zero 0x010: sr1 .one = 0x00000001
        \\    43 .zero 0x010: sr2 .zero = 0x00000000
        \\    47 .zero 0x00D: rsn = 52
        \\    47 .zero 0x00D: sr2 .temp_2 = 0x00000034
        \\    51 .zero 0x010: sr1 .one = 0x00000001
        \\    51 .zero 0x010: sr2 .zero = 0x00000000
        \\    55 .zero 0x00D: rsn = 48
        \\    55 .zero 0x00D: sr2 .temp_2 = 0x00000030
        \\    59 .zero 0x010: sr1 .one = 0x00000001
        \\    59 .zero 0x010: sr2 .zero = 0x00000000
        \\    63 .zero 0x00D: rsn = 44
        \\    63 .zero 0x00D: sr2 .temp_2 = 0x0000002C
        \\    67 .zero 0x010: sr1 .one = 0x00000001
        \\    67 .zero 0x010: sr2 .zero = 0x00000000
        \\    71 .zero 0x00D: rsn = 40
        \\    71 .zero 0x00D: sr2 .temp_2 = 0x00000028
        \\    75 .zero 0x010: sr1 .one = 0x00000001
        \\    75 .zero 0x010: sr2 .zero = 0x00000000
        \\    79 .zero 0x00D: rsn = 36
        \\    79 .zero 0x00D: sr2 .temp_2 = 0x00000024
        \\    83 .zero 0x010: sr1 .one = 0x00000001
        \\    83 .zero 0x010: sr2 .zero = 0x00000000
        \\    87 .zero 0x00D: rsn = 32
        \\    87 .zero 0x00D: sr2 .temp_2 = 0x00000020
        \\    91 .zero 0x010: sr1 .one = 0x00000001
        \\    91 .zero 0x010: sr2 .zero = 0x00000000
        \\    95 .zero 0x00D: rsn = 28
        \\    95 .zero 0x00D: sr2 .temp_2 = 0x0000001C
        \\    99 .zero 0x010: sr1 .one = 0x00000001
        \\    99 .zero 0x010: sr2 .zero = 0x00000000
        \\   103 .zero 0x00D: rsn = 24
        \\   103 .zero 0x00D: sr2 .temp_2 = 0x00000018
        \\   107 .zero 0x010: sr1 .one = 0x00000001
        \\   107 .zero 0x010: sr2 .zero = 0x00000000
        \\   111 .zero 0x00D: rsn = 20
        \\   111 .zero 0x00D: sr2 .temp_2 = 0x00000014
        \\   115 .zero 0x010: sr1 .one = 0x00000001
        \\   115 .zero 0x010: sr2 .zero = 0x00000000
        \\   119 .zero 0x00D: rsn = 16
        \\   119 .zero 0x00D: sr2 .temp_2 = 0x00000010
        \\   123 .zero 0x010: sr1 .one = 0x00000001
        \\   123 .zero 0x010: sr2 .zero = 0x00000000
        \\   127 .zero 0x00D: rsn = 12
        \\   127 .zero 0x00D: sr2 .temp_2 = 0x0000000C
        \\   131 .zero 0x010: sr1 .one = 0x00000001
        \\   131 .zero 0x010: sr2 .zero = 0x00000000
        \\   135 .zero 0x00D: rsn = .fault_pipe_0
        \\   135 .zero 0x00D: sr2 .temp_2 = 0x00000008
        \\   139 .zero 0x010: sr1 .one = 0x00000001
        \\   139 .zero 0x010: sr2 .zero = 0x00000000
        \\   143 .zero 0x00D: rsn = .interrupt_fault_pipe_0
        \\   143 .zero 0x00D: sr2 .temp_2 = 0x00000004
        \\   147 .zero 0x010: sr1 .one = 0x00000001
        \\   147 .zero 0x010: sr2 .zero = 0x00000000
        \\   151 .zero 0x00D: rsn = .interrupt_pipe_0
        \\   151 .zero 0x00D: sr2 .temp_2 = 0x00000000
        \\   155 .zero 0x010: sr1 .one = 0x00000001
        \\   155 .zero 0x010: sr2 .zero = 0x00000000
        \\   159 .zero 0x00D: rsn = 60
        \\   159 .zero 0x00D: sr2 .temp_2 = 0xFFFFFFFC
        \\   163 .zero 0x010: sr2 .kxp = 0x00000000
        \\   167 .zero 0x00F: read  .physical DB:0xFFFF DA:0xFFFF F:.zero AB:0x000 AA:0x000 ..Aa  
        \\   167 .zero 0x00F: sr2 .next_ip = 0x0000FFFF
        \\   171 .zero 0x00E: fault: .page_align_fault
        \\
    );
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

    try debug_log.expect(.{ .insn_stuff = true },
        \\     3 .one .reset: sr1 .temp_1 = 0x00000040
        \\     7 .one .reset: sr1 .temp_1 = 0x00000040
        \\    11 .one 0x016: sr1 .temp_1 = 0x00000001
        \\    15 .one 0x015: rsn = .interrupt_pipe_1
        \\    15 .one 0x015: sr1 .temp_1 = 0x00000001
        \\    15 .one 0x015: sr2 .temp_2 = 0x00000001
        \\    19 .one 0x014: sr2 .zero = 0x00000000
        \\    23 .one 0x013: sr1 .one = 0x00000001
        \\    27 .one 0x012: sr2 .temp_2 = 0x0000001F
        \\    31 .one 0x011: rsn = 61
        \\    31 .one 0x011: sr2 .temp_2 = 0x0000003D
        \\    35 .one 0x010: sr1 .one = 0x00000001
        \\    35 .one 0x010: sr2 .zero = 0x00000000
        \\    39 .one 0x00D: rsn = 57
        \\    39 .one 0x00D: sr2 .temp_2 = 0x00000039
        \\    43 .one 0x010: sr1 .one = 0x00000001
        \\    43 .one 0x010: sr2 .zero = 0x00000000
        \\    47 .one 0x00D: rsn = 53
        \\    47 .one 0x00D: sr2 .temp_2 = 0x00000035
        \\    51 .one 0x010: sr1 .one = 0x00000001
        \\    51 .one 0x010: sr2 .zero = 0x00000000
        \\    55 .one 0x00D: rsn = 49
        \\    55 .one 0x00D: sr2 .temp_2 = 0x00000031
        \\    59 .one 0x010: sr1 .one = 0x00000001
        \\    59 .one 0x010: sr2 .zero = 0x00000000
        \\    63 .one 0x00D: rsn = 45
        \\    63 .one 0x00D: sr2 .temp_2 = 0x0000002D
        \\    67 .one 0x010: sr1 .one = 0x00000001
        \\    67 .one 0x010: sr2 .zero = 0x00000000
        \\    71 .one 0x00D: rsn = 41
        \\    71 .one 0x00D: sr2 .temp_2 = 0x00000029
        \\    75 .one 0x010: sr1 .one = 0x00000001
        \\    75 .one 0x010: sr2 .zero = 0x00000000
        \\    79 .one 0x00D: rsn = 37
        \\    79 .one 0x00D: sr2 .temp_2 = 0x00000025
        \\    83 .one 0x010: sr1 .one = 0x00000001
        \\    83 .one 0x010: sr2 .zero = 0x00000000
        \\    87 .one 0x00D: rsn = 33
        \\    87 .one 0x00D: sr2 .temp_2 = 0x00000021
        \\    91 .one 0x010: sr1 .one = 0x00000001
        \\    91 .one 0x010: sr2 .zero = 0x00000000
        \\    95 .one 0x00D: rsn = 29
        \\    95 .one 0x00D: sr2 .temp_2 = 0x0000001D
        \\    99 .one 0x010: sr1 .one = 0x00000001
        \\    99 .one 0x010: sr2 .zero = 0x00000000
        \\   103 .one 0x00D: rsn = 25
        \\   103 .one 0x00D: sr2 .temp_2 = 0x00000019
        \\   107 .one 0x010: sr1 .one = 0x00000001
        \\   107 .one 0x010: sr2 .zero = 0x00000000
        \\   111 .one 0x00D: rsn = 21
        \\   111 .one 0x00D: sr2 .temp_2 = 0x00000015
        \\   115 .one 0x010: sr1 .one = 0x00000001
        \\   115 .one 0x010: sr2 .zero = 0x00000000
        \\   119 .one 0x00D: rsn = 17
        \\   119 .one 0x00D: sr2 .temp_2 = 0x00000011
        \\   123 .one 0x010: sr1 .one = 0x00000001
        \\   123 .one 0x010: sr2 .zero = 0x00000000
        \\   127 .one 0x00D: rsn = 13
        \\   127 .one 0x00D: sr2 .temp_2 = 0x0000000D
        \\   131 .one 0x010: sr1 .one = 0x00000001
        \\   131 .one 0x010: sr2 .zero = 0x00000000
        \\   135 .one 0x00D: rsn = .fault_pipe_1
        \\   135 .one 0x00D: sr2 .temp_2 = 0x00000009
        \\   139 .one 0x010: sr1 .one = 0x00000001
        \\   139 .one 0x010: sr2 .zero = 0x00000000
        \\   143 .one 0x00D: rsn = .interrupt_fault_pipe_1
        \\   143 .one 0x00D: sr2 .temp_2 = 0x00000005
        \\   147 .one 0x010: sr1 .one = 0x00000001
        \\   147 .one 0x010: sr2 .zero = 0x00000000
        \\   151 .one 0x00D: rsn = .interrupt_pipe_1
        \\   151 .one 0x00D: sr2 .temp_2 = 0x00000001
        \\   155 .one 0x010: sr1 .one = 0x00000001
        \\   155 .one 0x010: sr2 .zero = 0x00000000
        \\   159 .one 0x00D: rsn = 61
        \\   159 .one 0x00D: sr2 .temp_2 = 0xFFFFFFFD
        \\   163 .one 0x010: sr2 .kxp = 0x00000001
        \\   167 .one 0x00F: read  .physical DB:0xFFFF DA:0xFFFF F:.zero AB:0x000 AA:0x000 ..Aa  
        \\   167 .one 0x00F: sr2 .next_ip = 0x0000FFFF
        \\   171 .one 0x00E: fault: .page_align_fault
        \\
    );
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

    try debug_log.expect(.{ .insn_stuff = true },
        \\     3 .two .reset: sr1 .temp_1 = 0x00000080
        \\     7 .two .reset: sr1 .temp_1 = 0x00000080
        \\    11 .two 0x016: sr1 .temp_1 = 0x00000002
        \\    15 .two 0x015: rsn = .interrupt_pipe_2
        \\    15 .two 0x015: sr1 .temp_1 = 0x00000002
        \\    15 .two 0x015: sr2 .temp_2 = 0x00000002
        \\    19 .two 0x014: sr2 .zero = 0x00000000
        \\    23 .two 0x013: sr1 .one = 0x00000001
        \\    27 .two 0x012: sr2 .temp_2 = 0x00000020
        \\    31 .two 0x011: rsn = 62
        \\    31 .two 0x011: sr2 .temp_2 = 0x0000003E
        \\    35 .two 0x010: sr1 .one = 0x00000001
        \\    35 .two 0x010: sr2 .zero = 0x00000000
        \\    39 .two 0x00D: rsn = 58
        \\    39 .two 0x00D: sr2 .temp_2 = 0x0000003A
        \\    43 .two 0x010: sr1 .one = 0x00000001
        \\    43 .two 0x010: sr2 .zero = 0x00000000
        \\    47 .two 0x00D: rsn = 54
        \\    47 .two 0x00D: sr2 .temp_2 = 0x00000036
        \\    51 .two 0x010: sr1 .one = 0x00000001
        \\    51 .two 0x010: sr2 .zero = 0x00000000
        \\    55 .two 0x00D: rsn = 50
        \\    55 .two 0x00D: sr2 .temp_2 = 0x00000032
        \\    59 .two 0x010: sr1 .one = 0x00000001
        \\    59 .two 0x010: sr2 .zero = 0x00000000
        \\    63 .two 0x00D: rsn = 46
        \\    63 .two 0x00D: sr2 .temp_2 = 0x0000002E
        \\    67 .two 0x010: sr1 .one = 0x00000001
        \\    67 .two 0x010: sr2 .zero = 0x00000000
        \\    71 .two 0x00D: rsn = 42
        \\    71 .two 0x00D: sr2 .temp_2 = 0x0000002A
        \\    75 .two 0x010: sr1 .one = 0x00000001
        \\    75 .two 0x010: sr2 .zero = 0x00000000
        \\    79 .two 0x00D: rsn = 38
        \\    79 .two 0x00D: sr2 .temp_2 = 0x00000026
        \\    83 .two 0x010: sr1 .one = 0x00000001
        \\    83 .two 0x010: sr2 .zero = 0x00000000
        \\    87 .two 0x00D: rsn = 34
        \\    87 .two 0x00D: sr2 .temp_2 = 0x00000022
        \\    91 .two 0x010: sr1 .one = 0x00000001
        \\    91 .two 0x010: sr2 .zero = 0x00000000
        \\    95 .two 0x00D: rsn = 30
        \\    95 .two 0x00D: sr2 .temp_2 = 0x0000001E
        \\    99 .two 0x010: sr1 .one = 0x00000001
        \\    99 .two 0x010: sr2 .zero = 0x00000000
        \\   103 .two 0x00D: rsn = 26
        \\   103 .two 0x00D: sr2 .temp_2 = 0x0000001A
        \\   107 .two 0x010: sr1 .one = 0x00000001
        \\   107 .two 0x010: sr2 .zero = 0x00000000
        \\   111 .two 0x00D: rsn = 22
        \\   111 .two 0x00D: sr2 .temp_2 = 0x00000016
        \\   115 .two 0x010: sr1 .one = 0x00000001
        \\   115 .two 0x010: sr2 .zero = 0x00000000
        \\   119 .two 0x00D: rsn = 18
        \\   119 .two 0x00D: sr2 .temp_2 = 0x00000012
        \\   123 .two 0x010: sr1 .one = 0x00000001
        \\   123 .two 0x010: sr2 .zero = 0x00000000
        \\   127 .two 0x00D: rsn = 14
        \\   127 .two 0x00D: sr2 .temp_2 = 0x0000000E
        \\   131 .two 0x010: sr1 .one = 0x00000001
        \\   131 .two 0x010: sr2 .zero = 0x00000000
        \\   135 .two 0x00D: rsn = .fault_pipe_2
        \\   135 .two 0x00D: sr2 .temp_2 = 0x0000000A
        \\   139 .two 0x010: sr1 .one = 0x00000001
        \\   139 .two 0x010: sr2 .zero = 0x00000000
        \\   143 .two 0x00D: rsn = .interrupt_fault_pipe_2
        \\   143 .two 0x00D: sr2 .temp_2 = 0x00000006
        \\   147 .two 0x010: sr1 .one = 0x00000001
        \\   147 .two 0x010: sr2 .zero = 0x00000000
        \\   151 .two 0x00D: rsn = .interrupt_pipe_2
        \\   151 .two 0x00D: sr2 .temp_2 = 0x00000002
        \\   155 .two 0x010: sr1 .one = 0x00000001
        \\   155 .two 0x010: sr2 .zero = 0x00000000
        \\   159 .two 0x00D: rsn = 62
        \\   159 .two 0x00D: sr2 .temp_2 = 0xFFFFFFFE
        \\   163 .two 0x010: sr2 .kxp = 0x00000002
        \\   167 .two 0x00F: read  .physical DB:0xFFFF DA:0xFFFF F:.zero AB:0x000 AA:0x000 ..Aa  
        \\   167 .two 0x00F: sr2 .next_ip = 0x0000FFFF
        \\   171 .two 0x00E: fault: .page_align_fault
        \\
    );
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

    try debug_log.expect(.{ .insn_stuff = true },
        \\     3 .three .reset: sr1 .temp_1 = 0x000000C0
        \\     7 .three .reset: sr1 .temp_1 = 0x000000C0
        \\    11 .three 0x016: sr1 .temp_1 = 0x00000003
        \\    15 .three 0x015: rsn = .interrupt_pipe_3
        \\    15 .three 0x015: sr1 .temp_1 = 0x00000003
        \\    15 .three 0x015: sr2 .temp_2 = 0x00000003
        \\    19 .three 0x014: sr2 .zero = 0x00000000
        \\    23 .three 0x013: sr1 .one = 0x00000001
        \\    27 .three 0x012: sr2 .temp_2 = 0x00000021
        \\    31 .three 0x011: rsn = 63
        \\    31 .three 0x011: sr2 .temp_2 = 0x0000003F
        \\    35 .three 0x010: sr1 .one = 0x00000001
        \\    35 .three 0x010: sr2 .zero = 0x00000000
        \\    39 .three 0x00D: rsn = 59
        \\    39 .three 0x00D: sr2 .temp_2 = 0x0000003B
        \\    43 .three 0x010: sr1 .one = 0x00000001
        \\    43 .three 0x010: sr2 .zero = 0x00000000
        \\    47 .three 0x00D: rsn = 55
        \\    47 .three 0x00D: sr2 .temp_2 = 0x00000037
        \\    51 .three 0x010: sr1 .one = 0x00000001
        \\    51 .three 0x010: sr2 .zero = 0x00000000
        \\    55 .three 0x00D: rsn = 51
        \\    55 .three 0x00D: sr2 .temp_2 = 0x00000033
        \\    59 .three 0x010: sr1 .one = 0x00000001
        \\    59 .three 0x010: sr2 .zero = 0x00000000
        \\    63 .three 0x00D: rsn = 47
        \\    63 .three 0x00D: sr2 .temp_2 = 0x0000002F
        \\    67 .three 0x010: sr1 .one = 0x00000001
        \\    67 .three 0x010: sr2 .zero = 0x00000000
        \\    71 .three 0x00D: rsn = 43
        \\    71 .three 0x00D: sr2 .temp_2 = 0x0000002B
        \\    75 .three 0x010: sr1 .one = 0x00000001
        \\    75 .three 0x010: sr2 .zero = 0x00000000
        \\    79 .three 0x00D: rsn = 39
        \\    79 .three 0x00D: sr2 .temp_2 = 0x00000027
        \\    83 .three 0x010: sr1 .one = 0x00000001
        \\    83 .three 0x010: sr2 .zero = 0x00000000
        \\    87 .three 0x00D: rsn = 35
        \\    87 .three 0x00D: sr2 .temp_2 = 0x00000023
        \\    91 .three 0x010: sr1 .one = 0x00000001
        \\    91 .three 0x010: sr2 .zero = 0x00000000
        \\    95 .three 0x00D: rsn = 31
        \\    95 .three 0x00D: sr2 .temp_2 = 0x0000001F
        \\    99 .three 0x010: sr1 .one = 0x00000001
        \\    99 .three 0x010: sr2 .zero = 0x00000000
        \\   103 .three 0x00D: rsn = 27
        \\   103 .three 0x00D: sr2 .temp_2 = 0x0000001B
        \\   107 .three 0x010: sr1 .one = 0x00000001
        \\   107 .three 0x010: sr2 .zero = 0x00000000
        \\   111 .three 0x00D: rsn = 23
        \\   111 .three 0x00D: sr2 .temp_2 = 0x00000017
        \\   115 .three 0x010: sr1 .one = 0x00000001
        \\   115 .three 0x010: sr2 .zero = 0x00000000
        \\   119 .three 0x00D: rsn = 19
        \\   119 .three 0x00D: sr2 .temp_2 = 0x00000013
        \\   123 .three 0x010: sr1 .one = 0x00000001
        \\   123 .three 0x010: sr2 .zero = 0x00000000
        \\   127 .three 0x00D: rsn = 15
        \\   127 .three 0x00D: sr2 .temp_2 = 0x0000000F
        \\   131 .three 0x010: sr1 .one = 0x00000001
        \\   131 .three 0x010: sr2 .zero = 0x00000000
        \\   135 .three 0x00D: rsn = .fault_pipe_3
        \\   135 .three 0x00D: sr2 .temp_2 = 0x0000000B
        \\   139 .three 0x010: sr1 .one = 0x00000001
        \\   139 .three 0x010: sr2 .zero = 0x00000000
        \\   143 .three 0x00D: rsn = .interrupt_fault_pipe_3
        \\   143 .three 0x00D: sr2 .temp_2 = 0x00000007
        \\   147 .three 0x010: sr1 .one = 0x00000001
        \\   147 .three 0x010: sr2 .zero = 0x00000000
        \\   151 .three 0x00D: rsn = .interrupt_pipe_3
        \\   151 .three 0x00D: sr2 .temp_2 = 0x00000003
        \\   155 .three 0x010: sr1 .one = 0x00000001
        \\   155 .three 0x010: sr2 .zero = 0x00000000
        \\   159 .three 0x00D: rsn = 63
        \\   159 .three 0x00D: sr2 .temp_2 = 0xFFFFFFFF
        \\   163 .three 0x010: sr2 .kxp = 0x00000003
        \\   167 .three 0x00F: read  .physical DB:0xFFFF DA:0xFFFF F:.zero AB:0x000 AA:0x000 ..Aa  
        \\   167 .three 0x00F: sr2 .next_ip = 0x0000FFFF
        \\   171 .three 0x00E: fault: .page_align_fault
        \\
    );
}

const Simulator_Data = @import("Simulator_Data");
const microsim = @import("microsim");
const isa = @import("isa");
const arch = @import("arch");
const std = @import("std");