const glfw_time_reset_interval: f64 = 10_000;
const cpu_clock_frequency_hz: u64 = 20_000_000;
const min_realtime_fps = 56;
const max_microcycles_per_frame: u64 = cpu_clock_frequency_hz / min_realtime_fps;
const simulation_rate_integration_cycles: u64 = 2 * cpu_clock_frequency_hz;

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    var gpa = std.heap.GeneralPurposeAllocator(.{}) {};
    defer std.debug.assert(gpa.deinit() == .ok);

    var memory = try sim.Simple_Memory.init(arena.allocator(), hw.addr.Frame.init(0), 1024);


    const decode_rom = try arena.allocator().alloc(hw.decode.Result, hw.decode.Address.count);
    const microcode_rom = try arena.allocator().alloc(hw.Control_Signals, hw.microcode.Address.count);
    //uc_roms.readCompressedRoms(@import("microcode_roms").all_compressed_data, microcode);
    var simulator = try sim.Simulator.init(arena.allocator(), decode_rom, microcode_rom, &.{
        memory.device()
    });

    // for (&sim.memory.flash) |*chip| {
    //     std.mem.set(u16, chip, 0xFFFF);
    // }

    // const edb = try ie.data.EncoderDatabase.init(arena.allocator(), arena.allocator());
    // _ = edb;

    // const vector_table = misc.ZeropageVectorTable{
    //     .double_fault = 0xFFFE,
    //     .page_fault = 0xFFFD,
    //     .access_fault = 0xFFFC,
    //     .page_align_fault = 0xFFFB,
    //     .instruction_protection_fault = 0xFFFA,
    //     .invalid_instruction = 0xFFF9,
    //     .pipe_0_reset = @sizeOf(misc.ZeropageVectorTable),
    // };
    
    // var flash = sim.memory.flashIterator(0x7E_000 * 8);
    // _ = flash.writeAll(std.mem.asBytes(&vector_table));
    // _ = flash.writeAll(&program_data);


    // var ram_iter = sim.memory.flashIterator(0);
    // var i: usize = 0;
    // while (ram_iter.read_byte()) |b| {
    //     if (b != 0xFF) {
    //         std.debug.print("{X:0>4}: {X:0>2}\n", .{ i, b });
    //     }
    //     i += 1;
    // }
    // std.debug.print("\n", .{});

    //simulator.simulate_reset_and_init();

    // ram_iter = sim.memory.sramIterator(0);
    // for (0..256) |_| {
    //     std.debug.print(" {X:0>2}", .{ ram_iter.read_byte().? });
    // }
    // std.debug.print("\n", .{});

    var loaded_config = try Config.load(arena.allocator(), gpa.allocator());
    var gui = try Gui.init(gpa.allocator(), &simulator, loaded_config.window, loaded_config.frames);
    defer gui.deinit(gpa.allocator());
    loaded_config.deinit();

    defer saveConfig(arena.allocator(), &gui);

    var sim_stats: ?struct {
        microcycles_elapsed: u64 = 0,
        microcycles_dropped: u64 = 0,
    } = null;

    while (true) {
        switch (try gui.update()) {
            .pause => if (sim_stats) |_| {
                sim_stats = null;
                gui.set_simulation_rate(null, 0);
            },
            .run => if (sim_stats) |*stats| {
                const now = zglfw.getTime();
                if (now > glfw_time_reset_interval) {
                    zglfw.setTime(0);
                }

                const expected_microcycles: u64 = @intFromFloat(@trunc(now * @as(f64, cpu_clock_frequency_hz)));
                if (expected_microcycles > simulator.microcycles_simulated) {
                    var microcycles_to_simulate = expected_microcycles - simulator.microcycles_simulated;
                    stats.microcycles_elapsed += microcycles_to_simulate;
                    if (microcycles_to_simulate > max_microcycles_per_frame) {
                        const microcycles_to_drop = microcycles_to_simulate - max_microcycles_per_frame;
                        stats.microcycles_dropped += microcycles_to_drop;
                        simulator.microcycles_simulated += microcycles_to_drop;
                        microcycles_to_simulate = max_microcycles_per_frame;
                    }
                    simulator.simulate_microcycles(microcycles_to_simulate);
                    if (stats.microcycles_elapsed > simulation_rate_integration_cycles) {
                        const microcycles_elapsed: f64 = @floatFromInt(stats.microcycles_elapsed);
                        const microcycles_executed: f64 = @floatFromInt(stats.microcycles_elapsed - stats.microcycles_dropped);
                        const clock_freq: f64 = cpu_clock_frequency_hz;
                        gui.set_simulation_rate(microcycles_executed * clock_freq / microcycles_elapsed, clock_freq);
                        stats.* = .{};
                    }
                }

                if (now > glfw_time_reset_interval) {
                    simulator.microcycles_simulated = 0;
                }
            } else {
                sim_stats = .{};
                simulator.microcycles_simulated = 0;
            },
            .exit => break,
        }
    }
}

fn saveConfig(alloc: std.mem.Allocator, gui: *Gui) void {
    var config = Config.init(alloc, gui) catch |err| {
        std.log.warn("Failed to collect config data: {}", .{ err });
        return;
    };
    defer config.deinit();

    config.save(alloc) catch |err| {
        std.log.warn("Failed to save config data: {}", .{ err });
    };
}


const Gui = @import("microsim/gui/Gui.zig");
const Config = @import("microsim/Config.zig");
const sim = @import("lib_microsim");
// const uc_roms = @import("microcode_roms");
const hw = arch.hw;
const arch = @import("lib_arch");
const zglfw = @import("zglfw");
const std = @import("std");
