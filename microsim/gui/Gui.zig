simulator: *sim.Simulator,
window: *zglfw.Window,
gctx: *zgpu.GraphicsContext,
base_style: zgui.Style,
run: bool = false,

pub fn init(allocator: std.mem.Allocator, simulator: *sim.Simulator, window_settings: ?Window_Settings, frame_settings: []zgui.WindowSettings) !Gui {
    try zglfw.init();

    const window = try zglfw.Window.create(1600, 1000, "Microsim", null);
    window.setSizeLimits(600, 400, -1, -1);

    if (window_settings) |settings| {
        window.setSize(settings.size[0], settings.size[1]);
        window.setPos(settings.pos[0], settings.pos[1]);
        window.setAttribute(.maximized, settings.maximized);
    }

    const gctx = try zgpu.GraphicsContext.create(allocator, window, .{});

    zgui.init(allocator);
    zgui.plot.init();

    const scale_factor = compute_scale_factor(window);

    const embedded_font_data = @embedFile("iosevka-custom-regular.ttf");

    const font_size = 16.0 * scale_factor;
    const font = zgui.io.addFontFromMemory(embedded_font_data, font_size);
    std.debug.assert(zgui.io.getFont(0) == font);

    zgui.io.setIniFilename(null);

    zgui.backend.initWithConfig(
        window,
        gctx.device,
        @intFromEnum(zgpu.GraphicsContext.swapchain_format),
        .{ .texture_filter_mode = .linear, .pipeline_multisample_count = 1 },
    );

    zgui.updateWindowSettings(frame_settings);

    const current_style = zgui.getStyle();
    current_style.window_min_size = .{ 320.0, 240.0 };
    const base_style = current_style.*;
    current_style.scaleAllSizes(scale_factor);

    return Gui{
        .simulator = simulator,
        .window = window,
        .gctx = gctx,
        .base_style = base_style,
    };
}

pub fn deinit(self: *Gui, allocator: std.mem.Allocator) void {
    zgui.backend.deinit();
    zgui.plot.deinit();
    zgui.deinit();
    self.gctx.destroy(allocator);
    self.window.destroy();
    zglfw.terminate();
}

pub const Update_Result = enum {
    pause,
    run,
    exit,
};

pub fn update(self: *Gui) !Update_Result {
    zglfw.pollEvents();
    
    zgui.backend.newFrame(
        self.gctx.swapchain_descriptor.width,
        self.gctx.swapchain_descriptor.height,
    );

    if (zgui.begin("Clock Control", .{})) {
        if (zgui.button("Microstep", .{})) {
            self.simulator.simulate_microcycles(1);
        }
        zgui.sameLine(.{});
        if (zgui.button("Step", .{})) {
            self.simulator.simulate_cycles(1);
        }
        zgui.sameLine(.{});
        if (zgui.button("Run", .{})) {
            self.run = true;
        }
        zgui.sameLine(.{});
        if (zgui.button("Stop", .{})) {
            self.run = false;
        }
        zgui.sameLine(.{});
        _ = zgui.checkbox("Reset", .{ .v = &self.simulator.reset });

        zgui.sameLine(.{});
        if (zgui.button("Reset & Init", .{})) {
            self.simulator.simulate_reset_and_init();
        }
    }
    zgui.end();


    //zgui.setNextWindowPos(.{ .x = 20.0, .y = 20.0, .cond = .first_use_ever });
    //zgui.setNextWindowSize(.{ .w = -1.0, .h = -1.0, .cond = .first_use_ever });

    for (std.enums.values(hw.Pipeline)) |pipe| {
        Pipeline_State.init(self.simulator, pipe).doWindow();
    }

    self.draw();
    if (self.window.shouldClose()) return .exit;
    return if (self.run) .run else .pause;
}

fn draw(self: *Gui) void {
    const gctx = self.gctx;
    //const fb_width = gctx.swapchain_descriptor.width;
    //const fb_height = gctx.swapchain_descriptor.height;

    const swapchain_texv = gctx.swapchain.getCurrentTextureView();
    defer swapchain_texv.release();

    const commands = commands: {
        const encoder = gctx.device.createCommandEncoder(null);
        defer encoder.release();

        {
            const pass = zgpu.beginRenderPassSimple(encoder, .load, swapchain_texv, null, null, null);
            defer zgpu.endReleasePass(pass);
            zgui.backend.draw(pass);
        }

        break :commands encoder.finish(null);
    };
    defer commands.release();

    gctx.submit(&.{commands});
    _ = gctx.present();
}

pub fn set_simulation_rate(self: *Gui, sim_freq: ?f64, target_sim_freq: f64) void {
    if (sim_freq) |freq_hz| {
        const freq_mhz = freq_hz / 1_000_000;
        const percent = 100 * freq_hz / target_sim_freq;
        var buf: [256]u8 = undefined;
        const title = std.fmt.bufPrintZ(&buf, "Microsim - {d:.1} fps - {d:.2} MHz ({d:.1}% realtime)", .{
            self.gctx.stats.fps,
            freq_mhz,
            percent,
        }) catch "Microsim - ? fps - ?% realtime";
        self.window.setTitle(title);
    } else {
        self.window.setTitle("Microsim");
    }
}

fn compute_scale_factor(window: *zglfw.Window) f32 {
    const scale = window.getContentScale();
    return @max(scale[0], scale[1]);
}

pub const Window_Settings = struct {
    pos: [2]i32,
    size: [2]i32,
    maximized: bool,
};

const Gui = @This();
const Pipeline_State = @import("Pipeline_State.zig");
const sim = @import("lib_microsim");
const hw = arch.hw;
const arch = @import("lib_arch");
const colors = @import("colors.zig");
const zgui = @import("zgui");
const zglfw = @import("zglfw");
const zgpu = @import("zgpu");
const wgpu = zgpu.wgpu;
const std = @import("std");
