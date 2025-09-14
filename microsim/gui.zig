// const glfw_time_reset_interval: f64 = 10_000;
// const cpu_clock_frequency_hz: u64 = 20_000_000;
// const min_realtime_fps = 56;
// const max_microcycles_per_frame: u64 = cpu_clock_frequency_hz / min_realtime_fps;
// const simulation_rate_integration_cycles: u64 = 2 * cpu_clock_frequency_hz;

// pub fn main() !void {
//     var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
//     defer arena.deinit();
//     var gpa = std.heap.GeneralPurposeAllocator(.{}) {};
//     defer std.debug.assert(gpa.deinit() == .ok);

//     var memory = try sim.Simple_Memory.init(arena.allocator(), hw.addr.Frame.init(0), 1024);


//     const decode_rom = try arena.allocator().alloc(hw.decode.Result, hw.decode.Address.count);
//     const microcode_rom = try arena.allocator().alloc(hw.Control_Signals, hw.microcode.Address.count);
//     //uc_roms.readCompressedRoms(@import("microcode_roms").all_compressed_data, microcode);
//     var simulator = try sim.Simulator.init(arena.allocator(), decode_rom, microcode_rom, &.{
//         memory.device()
//     });

//     // for (&sim.memory.flash) |*chip| {
//     //     std.mem.set(u16, chip, 0xFFFF);
//     // }

//     // const edb = try ie.data.EncoderDatabase.init(arena.allocator(), arena.allocator());
//     // _ = edb;

//     // const vector_table = misc.ZeropageVectorTable{
//     //     .double_fault = 0xFFFE,
//     //     .page_fault = 0xFFFD,
//     //     .access_fault = 0xFFFC,
//     //     .page_align_fault = 0xFFFB,
//     //     .instruction_protection_fault = 0xFFFA,
//     //     .invalid_instruction = 0xFFF9,
//     //     .pipe_0_reset = @sizeOf(misc.ZeropageVectorTable),
//     // };
    
//     // var flash = sim.memory.flashIterator(0x7E_000 * 8);
//     // _ = flash.writeAll(std.mem.asBytes(&vector_table));
//     // _ = flash.writeAll(&program_data);


//     // var ram_iter = sim.memory.flashIterator(0);
//     // var i: usize = 0;
//     // while (ram_iter.read_byte()) |b| {
//     //     if (b != 0xFF) {
//     //         std.debug.print("{X:0>4}: {X:0>2}\n", .{ i, b });
//     //     }
//     //     i += 1;
//     // }
//     // std.debug.print("\n", .{});

//     //simulator.simulate_reset_and_init();

//     // ram_iter = sim.memory.sramIterator(0);
//     // for (0..256) |_| {
//     //     std.debug.print(" {X:0>2}", .{ ram_iter.read_byte().? });
//     // }
//     // std.debug.print("\n", .{});

//     var loaded_config = try Config.load(arena.allocator(), gpa.allocator());
//     var gui = try Gui.init(gpa.allocator(), &simulator, loaded_config.window, loaded_config.frames);
//     defer gui.deinit(gpa.allocator());
//     loaded_config.deinit();

//     defer saveConfig(arena.allocator(), &gui);

//     var sim_stats: ?struct {
//         microcycles_elapsed: u64 = 0,
//         microcycles_dropped: u64 = 0,
//     } = null;

//     while (true) {
//         switch (try gui.update()) {
//             .pause => if (sim_stats) |_| {
//                 sim_stats = null;
//                 gui.set_simulation_rate(null, 0);
//             },
//             .run => if (sim_stats) |*stats| {
//                 const now = zglfw.getTime();
//                 if (now > glfw_time_reset_interval) {
//                     zglfw.setTime(0);
//                 }

//                 const expected_microcycles: u64 = @intFromFloat(@trunc(now * @as(f64, cpu_clock_frequency_hz)));
//                 if (expected_microcycles > simulator.microcycles_simulated) {
//                     var microcycles_to_simulate = expected_microcycles - simulator.microcycles_simulated;
//                     stats.microcycles_elapsed += microcycles_to_simulate;
//                     if (microcycles_to_simulate > max_microcycles_per_frame) {
//                         const microcycles_to_drop = microcycles_to_simulate - max_microcycles_per_frame;
//                         stats.microcycles_dropped += microcycles_to_drop;
//                         simulator.microcycles_simulated += microcycles_to_drop;
//                         microcycles_to_simulate = max_microcycles_per_frame;
//                     }
//                     simulator.simulate_microcycles(microcycles_to_simulate);
//                     if (stats.microcycles_elapsed > simulation_rate_integration_cycles) {
//                         const microcycles_elapsed: f64 = @floatFromInt(stats.microcycles_elapsed);
//                         const microcycles_executed: f64 = @floatFromInt(stats.microcycles_elapsed - stats.microcycles_dropped);
//                         const clock_freq: f64 = cpu_clock_frequency_hz;
//                         gui.set_simulation_rate(microcycles_executed * clock_freq / microcycles_elapsed, clock_freq);
//                         stats.* = .{};
//                     }
//                 }

//                 if (now > glfw_time_reset_interval) {
//                     simulator.microcycles_simulated = 0;
//                 }
//             } else {
//                 sim_stats = .{};
//                 simulator.microcycles_simulated = 0;
//             },
//             .exit => break,
//         }
//     }
// }


var sig: sokol_imgui.State = undefined;
var pass_action: sokol.gfx.PassAction = .{};
var t: f64 = 0;

export fn frame() void {
    const width = sokol.app.width();
    const height = sokol.app.height();

    //const w: f32 = @floatFromInt(width);
    //const h: f32 = @floatFromInt(height);
    //const aspect = w/h;

    sig.new_frame(.{
        .width = width,
        .height = height,
        .delta_time = sokol.app.frameDuration(),
        .dpi_scale = sokol.app.dpiScale(),
    });
    
    ig.set_next_window_pos(.{ .x = 10, .y = 10 }, .{ .cond = .once });
    ig.set_next_window_size(.{ .x = 400, .y = 100 }, .{ .cond = .once });
    _ = ig.begin("Package", .{});
    
    //_ = ig.color_edit3("Background", @ptrCast(&state.pass_action.colors[0].clear_value.r), .{});
    ig.new_line();
    _ = ig.text("Hello World", .{});

    ig.show_metrics_window(.{});
    //ig.show_id_stack_tool_window(null);
    ig.show_demo_window(.{});
    ig.show_debug_log_window(.{});

    ig.end();

    sokol.gfx.beginPass(.{ .action = pass_action, .swapchain = sokol.glue.swapchain() });
    sig.render();
    sokol.gfx.endPass();
    sokol.gfx.commit();
}



export fn init() void {
    ig.check_version();

    sokol.gfx.setup(.{
        .environment = sokol.glue.environment(),
        .logger = .{ .func = sokol.log.func },
    });
    
    sig = sokol_imgui.State.init(.{
        .no_default_font = true,
        .allocator = std.heap.page_allocator,
    }) catch |err| {
        std.debug.panic("Failed to set up Sokol ImGui: {}", .{ err });
    };

    _ = ig.get_io().fonts.?.add_font_from_memory_ttf_static(font_data, 18.5, .{});

    pass_action.colors[0] = .{
        .load_action = .CLEAR,
        .clear_value = .{ .r = 0.05, .g = 0.25, .b = 0.15, .a = 1.0 },
    };
}

export fn cleanup() void {
    sig.deinit();
    sokol.gfx.shutdown();
}

export fn event(ev: [*c]const sokol.app.Event) void {
    // forward input events to sokol-imgui
    _ = sig.handle_event(ev.*);
}

pub fn main() void {
    sokol.app.run(.{
        .init_cb = init,
        .frame_cb = frame,
        .cleanup_cb = cleanup,
        .event_cb = event,
        .window_title = "Vera Microsim",
        .width = 1024,
        .height = 768,
        .sample_count = 4,
        .icon = .{ .sokol_default = true },
        .logger = .{ .func = sokol.log.func },
    });
}

const log = std.log.scoped(.microsim);

const font_data = @embedFile("font.ttf");

const microsim = @import("microsim");
const arch = @import("arch");
const ig = @import("ig");
const sokol_imgui = @import("sokol_imgui");
const sokol = @import("sokol");
const std = @import("std");
