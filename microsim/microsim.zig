const std = @import("std");
const uc_roms = @import("microcode_rom_serialization");
const misc = @import("misc");
const ie = @import("instruction_encoding");
const zglfw = @import("zglfw");
const ControlSignals = @import("ControlSignals");
const Simulator = @import("Simulator");
const Gui = @import("gui/Gui.zig");

const glfw_time_reset_interval: f64 = 10_000;
const cpu_clock_frequency_hz: u64 = 20_000_000;
const min_realtime_fps = 56;
const max_microcycles_per_frame: u64 = cpu_clock_frequency_hz / min_realtime_fps;
const simulation_rate_integration_cycles: u64 = 2 * cpu_clock_frequency_hz;

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    var gpa = std.heap.GeneralPurposeAllocator(.{}) {};

    const microcode = try arena.allocator().create([misc.microcode_length]ControlSignals);
    uc_roms.readCompressedRoms(@import("microcode_roms").compressed_data, microcode);
    var sim = try Simulator.init(arena.allocator(), microcode);
    var xo = std.rand.Xoshiro256.init(12345);
    sim.randomizeState(xo.random());
    for (&sim.memory.flash) |*chip| {
        std.mem.set(u16, chip, 0xFFFF);
    }

    const ie_data = @import("instruction_encoding_data").data;
    const edb = try ie.EncoderDatabase.init(arena.allocator(), ie_data, arena.allocator());

    var program_data: [256]u8 = undefined;
    var encoder = ie.Encoder.init(&program_data);
    const program = [_]ie.Instruction {
        .{
            .mnemonic = .C,
            .suffix = .none,
            .params = &[_]ie.Parameter{
                ie.parameter(.constant, 0),
                ie.toParameter(.reg32, 12),
            },
        },
        .{
            .mnemonic = .ADD,
            .suffix = .none,
            .params = &[_]ie.Parameter{
                ie.parameter(.reg32, 12),
                ie.parameter(.constant, -128),
                ie.toParameter(.reg32, 1),
            },
        },
    };
    for (program) |insn| {
        var insn_iter = edb.getMatchingEncodings(insn);
        try encoder.encode(insn, insn_iter.next().?);
    }
    const vector_table = misc.ZeropageVectorTable{
        .double_fault = 0xFFFE,
        .page_fault = 0xFFFD,
        .access_fault = 0xFFFC,
        .page_align_fault = 0xFFFB,
        .instruction_protection_fault = 0xFFFA,
        .invalid_instruction = 0xFFF9,
        .pipe_0_reset = @sizeOf(misc.ZeropageVectorTable),
    };
    
    var flash = sim.memory.flashIterator(0x7E_000 * 8);
    _ = flash.writeAll(std.mem.asBytes(&vector_table));
    _ = flash.writeAll(&program_data);


    var ram_iter = sim.memory.flashIterator(0);
    var i: usize = 0;
    while (ram_iter.readByte()) |b| {
        if (b != 0xFF) {
            std.debug.print("{X:0>4}: {X:0>2}\n", .{ i, b });
        }
        i += 1;
    }
    std.debug.print("\n", .{});

    sim.resetAndInit();

    ram_iter = sim.memory.sramIterator(0);
    for (0..256) |_| {
        std.debug.print(" {X:0>2}", .{ ram_iter.readByte().? });
    }
    std.debug.print("\n", .{});

    var gui = try Gui.init(gpa.allocator(), &sim);
    defer gui.deinit(gpa.allocator());

    var sim_stats: ?struct {
        microcycles_elapsed: u64 = 0,
        microcycles_dropped: u64 = 0,
    } = null;

    while (true) {
        switch (try gui.update()) {
            .pause => if (sim_stats) |_| {
                sim_stats = null;
                gui.setSimulationRate(null, 0);
            },
            .run => if (sim_stats) |*stats| {
                const now = zglfw.getTime();
                if (now > glfw_time_reset_interval) {
                    zglfw.setTime(0);
                }

                const expected_microcycles = @floatToInt(u64, @trunc(now * @as(f64, cpu_clock_frequency_hz)));
                if (expected_microcycles > sim.microcycles_simulated) {
                    var microcycles_to_simulate = expected_microcycles - sim.microcycles_simulated;
                    stats.microcycles_elapsed += microcycles_to_simulate;
                    if (microcycles_to_simulate > max_microcycles_per_frame) {
                        const microcycles_to_drop = microcycles_to_simulate - max_microcycles_per_frame;
                        stats.microcycles_dropped += microcycles_to_drop;
                        sim.microcycles_simulated += microcycles_to_drop;
                        microcycles_to_simulate = max_microcycles_per_frame;
                    }
                    sim.microcycle(microcycles_to_simulate);
                    if (stats.microcycles_elapsed > simulation_rate_integration_cycles) {
                        const microcycles_executed = @intToFloat(f64, stats.microcycles_elapsed - stats.microcycles_dropped);
                        const clock_freq = @as(f64, cpu_clock_frequency_hz);
                        gui.setSimulationRate(microcycles_executed * clock_freq / @intToFloat(f64, stats.microcycles_elapsed), clock_freq);
                        stats.* = .{};
                    }
                }

                if (now > glfw_time_reset_interval) {
                    sim.microcycles_simulated = 0;
                }
            } else {
                sim_stats = .{};
                sim.microcycles_simulated = 0;
            },
            .exit => break,
        }
    }
}
