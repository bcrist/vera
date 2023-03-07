const std = @import("std");
const zgui = @import("zgui");
const zglfw = @import("zglfw");
const zgpu = @import("zgpu");
const wgpu = zgpu.wgpu;
const misc = @import("misc");
const uc = @import("microcode");
const ControlSignals = @import("ControlSignals");
const Simulator = @import("Simulator");

const registers = @import("registers.zig");

    // pub fn printRegs(self: *const RegisterView, writer: anytype) !void {
    //     try writer.print(" R1: {X:0>4}   R0: {X:0>4}      Z: {X:0>8}     Z: {X:0>8}\n", .{
    //         self.readGPR(1), self.readGPR(0), self.readSR1(.zero), self.readSR2(.zero),
    //     });
    //     try writer.print(" R3: {X:0>4}   R2: {X:0>4}     RP: {X:0>8}    IP: {X:0>8}\n", .{
    //         self.readGPR(3), self.readGPR(2), self.readSR1(.rp), self.readSR2(.ip),
    //     });
    //     try writer.print(" R5: {X:0>4}   R4: {X:0>4}     SP: {X:0>8}   NIP: {X:0>8}\n", .{
    //         self.readGPR(5), self.readGPR(4), self.readSR1(.sp), self.readSR2(.next_ip),
    //     });
    //     try writer.print(" R7: {X:0>4}   R6: {X:0>4}     BP: {X:0>8}   ASN: {X:0>8}\n", .{
    //         self.readGPR(7), self.readGPR(6), self.readSR1(.bp), self.readSR2(.asn),
    //     });
    //     try writer.print(" R9: {X:0>4}   R8: {X:0>4}   UADL: {X:0>8}   KXP: {X:0>8}\n", .{
    //         self.readGPR(9), self.readGPR(8), self.readSR1(.fault_ua_dl), self.readSR2(.kxp),
    //     });
    //     try writer.print("R11: {X:0>4}  R10: {X:0>4}  RSTAT: {X:0>8}   UXP: {X:0>8}\n", .{
    //         self.readGPR(11), self.readGPR(10), self.readSR1(.fault_rsn_stat), self.readSR2(.uxp),
    //     });
    //     try writer.print("R13: {X:0>4}  R12: {X:0>4}  ROBOA: {X:0>8}   RSR: {X:0>8}\n", .{
    //         self.readGPR(13), self.readGPR(12), self.readSR1(.int_rsn_fault_ob_oa), self.readSR2(.rs_reserved),
    //     });
    //     try writer.print("R15: {X:0>4}  R14: {X:0>4}   TMP1: {X:0>8}  TMP2: {X:0>8}\n", .{
    //         self.readGPR(15), self.readGPR(14), self.readSR1(.temp_1), self.readSR2(.temp_2),
    //     });
    // }

    // pub fn printState(self: *Simulator, writer: anytype, pipe: misc.PipeID) !void {
//     try writer.print("c{}", .{ self.microcycle_count });
//     if (self.exec_state.reset) try writer.writeAll(" RESET");
//     if (self.exec_state.sleep) try writer.writeAll(" SLEEP");
//     for (self.exec_state.interrupt_pending, 0..) |int_pending, pipe_index| {
//         if (int_pending) {
//             try writer.print(" INT{}", .{ pipe_index + 1 });
//         }
//     }

//     try writer.writeAll("\n");

//     switch (self.exec_state.pipe) {
//         .zero => switch (pipe) {
//             .zero => try self.printT(writer),
//             .one => try self.printC(writer),
//             .two => try self.printS(writer),
//         },
//         .one => switch (pipe) {
//             .zero => try self.printS(writer),
//             .one => try self.printT(writer),
//             .two => try self.printC(writer),
//         },
//         .two => switch (pipe) {
//             .zero => try self.printC(writer),
//             .one => try self.printS(writer),
//             .two => try self.printT(writer),
//         },
//     }
// }

// fn printS(self: *Simulator, writer: anytype) !void {
//     try writer.print("Pipe:{s}  RSN:{}  T -> Setup\n", .{ @tagName(self.s.pipe), self.s.reg.rsn });
//     try self.s.cs.print(writer);
//     try self.printRegs(self.s.reg, writer);
// }
// fn printC(self: *Simulator, writer: anytype) !void {
//     try writer.print("Pipe:{s}  RSN:{}  S -> Compute\n", .{ @tagName(self.c.pipe), self.c.reg.rsn });
//     try self.c.cs.print(writer);
//     try self.printRegs(self.c.reg, writer);
// }
// fn printT(self: *Simulator, writer: anytype) !void {
//     try writer.print("Pipe:{s}  RSN:{}  C -> Transact\n", .{ @tagName(self.t.pipe), self.t.reg.rsn });
//     try self.t.cs.print(writer);
//     try self.printRegs(self.t.reg, writer);
// }
// fn printRegs(self: *Simulator, reg: LoopRegisters, writer: anytype) !void {
//     try writer.print("      UA: {X:0>4}  DL: {X:0>4}   OA: {X:0>1}  OB: {X:0>1}  ", .{
//         reg.ua, reg.dl, reg.oa, reg.ob,
//     });
//     try reg.stat.print(writer);
//     try writer.writeAll("\n");
//     try register_file.RegisterView.init(self.reg_file, reg.rsn).printRegs(writer);
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
    }
    zgui.end();


    //zgui.setNextWindowPos(.{ .x = 20.0, .y = 20.0, .cond = .first_use_ever });
    //zgui.setNextWindowSize(.{ .w = -1.0, .h = -1.0, .cond = .first_use_ever });




    for (std.enums.values(misc.PipeID)) |pipe| {
        if (zgui.begin(switch (pipe) {
            .zero => "Pipe 0",
            .one => "Pipe 1",
            .two => "Pipe 2",
        }, .{})) {
            const rf = self.sim.register_file;

            var stage: []const u8 = "?";
            var reg = Simulator.LoopRegisters.init();
            var cs = ControlSignals.init();
            if (self.sim.s.pipe == pipe) {
                stage = "Compute";
                reg = self.sim.s.reg;
                cs = self.sim.s.cs;
            } else if (self.sim.c.pipe == pipe) {
                stage = "Transact";
                reg = self.sim.c.reg;
                cs = self.sim.c.cs;
            } else if (self.sim.t.pipe == pipe) {
                stage = "Setup";
                reg = self.sim.t.reg;
                cs = self.sim.t.cs;
            }
            const rsn = reg.rsn;

            zgui.text("Stage: {s}", .{ stage });
            zgui.text("RSN: {X:0>2}", .{ rsn });
            if (uc.getOpcodeForAddress(reg.ua)) |opcode| {
                zgui.text("UA: {X:0>4} - Opcode {X:0>4}", .{ reg.ua, opcode });
            } else if (uc.getContinuationNumberForAddress(reg.ua)) |cont| {
                zgui.text("UA: {X:0>4} - uop {X:0>3}", .{ reg.ua, cont });
            } else {
                zgui.text("UA: {X:0>4}", .{ reg.ua });
            }

            zgui.text("STAT: {X:0>4}", .{ reg.stat.getForLL(pipe, reg.exec_mode, self.sim.exec_state.power) });
            zgui.sameLine(.{});
            zgui.text("DL: {X:0>4}", .{ reg.dl });

            registers.showRegisterFile(rf, rsn, cs, reg.oa, reg.ob, false); // TODO inhibit_writes

            if (self.sim.s.pipe == pipe) {
                if (self.sim.s.stall_atomic) {
                    zgui.text("Stalled", .{});
                }
                if (self.sim.s.want_atomic) {
                    zgui.text("Atomic", .{});
                }
                const c = Simulator.ComputeStage.init(self.sim.s, self.sim.address_translator, self.sim.exec_state);
                zgui.text("arith: {X:0>4}_{X:0>4}", .{ c.arith.data.high, c.arith.data.low });
                zgui.text("logic: {X:0>4}_{X:0>4}", .{ c.logic.data.high, c.logic.data.low });

            } else if (self.sim.c.pipe == pipe) {
                if (self.sim.c.stall_atomic) {
                    zgui.text("Stalled", .{});
                }
                if (self.sim.c.want_atomic) {
                    zgui.text("Atomic", .{});
                }
                const t = Simulator.TransactStage.init(self.sim.c, self.sim.microcode, self.sim.exec_state, self.sim.memory, self.sim.frame_tracker);
                zgui.text("L: {X:0>4}_{X:0>4}", .{ t.l.high, t.l.low });
                zgui.text("D: {X:0>4}", .{ t.data_to_write });
                zgui.text("DL: {X:0>4}", .{ t.reg.dl });
                zgui.text("next UA: {X:0>4}", .{ t.reg.ua });

            } else if (self.sim.t.pipe == pipe) {
                if (self.sim.t.want_atomic) {
                    zgui.text("Atomic", .{});
                }
            }



            var buf: [4096]u8 = undefined;
            var stream = std.io.fixedBufferStream(&buf);
            cs.print(stream.writer()) catch {};

            zgui.textWrapped("{s}", .{ stream.getWritten() });

        }
        zgui.end();
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
