const std = @import("std");
const zgui = @import("zgui");
const zglfw = @import("zglfw");
const zgpu = @import("zgpu");
const wgpu = zgpu.wgpu;
const misc = @import("misc");
const uc = @import("microcode");
const ControlSignals = @import("ControlSignals");
const Simulator = @import("Simulator");

const colors = @import("colors.zig");
const pipes = @import("pipes.zig");


// pub fn printState(self: *Simulator, writer: anytype, pipe: misc.PipeID) !void {
//     if (self.exec_state.reset) try writer.writeAll(" RESET");
//     if (self.exec_state.sleep) try writer.writeAll(" SLEEP");
//     for (self.exec_state.interrupt_pending, 0..) |int_pending, pipe_index| {
//         if (int_pending) {
//             try writer.print(" INT{}", .{ pipe_index + 1 });
//         }
//     }

// fn printRegs(self: *Simulator, reg: LoopRegisters, writer: anytype) !void {
//     try writer.print("      UA: {X:0>4}  DL: {X:0>4}   OA: {X:0>1}  OB: {X:0>1}  ", .{
//         reg.ua, reg.dl, reg.oa, reg.ob,
//     });
//     try reg.stat.print(writer);
// }

const Gui = @This();

sim: *Simulator,
window: *zglfw.Window,
gctx: *zgpu.GraphicsContext,
base_style: zgui.Style,
run: bool = false,

pub fn init(allocator: std.mem.Allocator, sim: *Simulator) !Gui {
    try zglfw.init();

    const window = try zglfw.Window.create(1600, 1000, "Microsim", null);
    window.setSizeLimits(600, 400, -1, -1);

    const gctx = try zgpu.GraphicsContext.create(allocator, window);

    zgui.init(allocator);
    zgui.plot.init();

    const scale_factor = computeScaleFactor(window);

    // const font_size = 16.0 * scale_factor;
    // const font_large = zgui.io.addFontFromMemory(embedded_font_data, math.floor(font_size * 1.1));
    // const font_normal = zgui.io.addFontFromFile(content_dir ++ "Roboto-Medium.ttf", math.floor(font_size));
    // assert(zgui.io.getFont(0) == font_large);
    // assert(zgui.io.getFont(1) == font_normal);

    zgui.backend.initWithConfig(
        window,
        gctx.device,
        @enumToInt(zgpu.GraphicsContext.swapchain_format),
        .{ .texture_filter_mode = .linear, .pipeline_multisample_count = 1 },
    );

    // This call is optional. Initially, zgui.io.getFont(0) is a default font.
    // zgui.io.setDefaultFont(font_normal);

    const current_style = zgui.getStyle();
    current_style.window_min_size = .{ 320.0, 240.0 };
    const base_style = current_style.*;
    current_style.scaleAllSizes(scale_factor);

    return Gui{
        .sim = sim,
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

pub const UpdateResult = enum {
    pause,
    run,
    exit,
};

pub fn update(self: *Gui) !UpdateResult {
    zglfw.pollEvents();
    
    zgui.backend.newFrame(
        self.gctx.swapchain_descriptor.width,
        self.gctx.swapchain_descriptor.height,
    );

    if (zgui.begin("Clock Control", .{})) {
        if (zgui.button("Microstep", .{})) {
            self.sim.microcycle(1);
        }
        zgui.sameLine(.{});
        if (zgui.button("Step", .{})) {
            self.sim.cycle(1);
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
        _ = zgui.checkbox("Reset", .{ .v = &self.sim.exec_state.reset });

        zgui.sameLine(.{});
        if (zgui.button("Reset & Init", .{})) {
            self.sim.resetAndInit();
        }
    }
    zgui.end();


    //zgui.setNextWindowPos(.{ .x = 20.0, .y = 20.0, .cond = .first_use_ever });
    //zgui.setNextWindowSize(.{ .w = -1.0, .h = -1.0, .cond = .first_use_ever });

    for (std.enums.values(misc.PipeID)) |pipe| {
        pipes.doPipeWindow(pipe, self.sim);
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

pub fn setSimulationRate(self: *Gui, sim_freq: ?f64, target_sim_freq: f64) void {
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

fn computeScaleFactor(window: *zglfw.Window) f32 {
    const scale = window.getContentScale();
    return std.math.max(scale[0], scale[1]);
}
