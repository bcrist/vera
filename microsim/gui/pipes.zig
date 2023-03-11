const std = @import("std");
const zgui = @import("zgui");
const misc = @import("misc");
const bus = @import("bus_types");
const uc = @import("microcode");
const Simulator = @import("Simulator");
const ControlSignals = @import("ControlSignals");
const colors = @import("colors.zig");

// TODO show literal directly?

pub fn doPipeWindow(pipe: misc.PipeID, sim: *const Simulator) void {
    defer zgui.end();
    if (!zgui.begin(switch (pipe) {
        .zero => "Pipe 0",
        .one => "Pipe 1",
        .two => "Pipe 2",
    }, .{})) return;

    const glyph_width = zgui.calcTextSize("0", .{})[0];

    const rf = sim.register_file;
    var reg = Simulator.LoopRegisters.init();
    var cs = ControlSignals.init();
    var w_color = colors.write;
    var inhibit_writes = false;
    var atomic = false;
    var stall = false;
    if (sim.s.pipe == pipe) {
        reg = sim.s.reg;
        cs = sim.s.cs;
        atomic = sim.s.want_atomic;
        stall = sim.s.stall_atomic;
        zgui.textUnformattedColored(colors.stage_compute, "Compute");
        zgui.sameLine(.{ .offset_from_start_x = 15 * glyph_width });
        zgui.textUnformattedColored(colors.label, "UA");
        zgui.sameLine(.{});
        zgui.textUnformattedColored(colors.label, "?");
        zgui.sameLine(.{});
        zgui.text("{X:0>4}", .{ reg.ua });
    } else if (sim.c.pipe == pipe) {
        reg = sim.c.reg;
        cs = sim.c.cs;
        inhibit_writes = sim.c.inhibit_writes;
        atomic = sim.c.want_atomic;
        stall = sim.c.stall_atomic;
        const t = Simulator.TransactStage.init(sim.c, sim.microcode, sim.exec_state, sim.memory, sim.frame_tracker);
        if (sim.c.inhibit_writes) w_color = colors.transparent;
        zgui.textUnformattedColored(colors.stage_transact, "Transact");
        zgui.sameLine(.{ .offset_from_start_x = 15 * glyph_width });
        zgui.textUnformattedColored(colors.label, "UA");
        zgui.sameLine(.{});
        zgui.textUnformattedColored(colors.write, switch (t.debug_ua_src) {
            .reset => "R",
            .stall => " ",
            .fault => "F",
            .interrupt => "I",
            .continuation => "U",
            .opcode => "D",
            .lh => "L",
        });
        zgui.sameLine(.{});
        zgui.text("{X:0>4}", .{ reg.ua });
    } else if (sim.t.pipe == pipe) {
        reg = sim.t.reg;
        cs = sim.t.cs;
        const s = Simulator.SetupStage.init(sim.t, sim.register_file, sim.s.isAtomic(), sim.c.isAtomic());
        atomic = s.want_atomic;
        stall = s.stall_atomic;
        zgui.textUnformattedColored(colors.stage_setup, "Setup");
        zgui.sameLine(.{ .offset_from_start_x = 15 * glyph_width });
        zgui.textUnformattedColored(colors.label, "UA");
        zgui.sameLine(.{});
        zgui.textUnformattedColored(colors.label, "?");
        zgui.sameLine(.{});
        zgui.text("{X:0>4}", .{ reg.ua });
    }
    const rsn = reg.rsn;

    zgui.sameLine(.{ .offset_from_start_x = 26 * glyph_width });
    if (uc.getOpcodeForAddress(reg.ua)) |opcode| {
        zgui.textUnformattedColored(colors.label, "Opcode");
        zgui.sameLine(.{});
        if (uc.getOpcodeGranularity(opcode) == 16) {
            zgui.text("{X:0>3}x", .{ opcode >> 4 });
        } else {
            zgui.text("{X:0>4}", .{ opcode });
        }
    } else if (uc.getContinuationNumberForAddress(reg.ua)) |uop| {
        zgui.textUnformattedColored(colors.label, "    uop");
        zgui.sameLine(.{});
        zgui.text("{X:0>3}", .{ uop });
    }

    zgui.sameLine(.{ .offset_from_start_x = 40 * glyph_width });
    zgui.textUnformattedColored(colors.label, "ASN4");
    zgui.sameLine(.{});
    zgui.textUnformattedColored(w_color, switch (cs.sr2_wi) {
        .asn => switch (cs.sr2_wsrc) {
            .no_write => " ",
            .sr2 => "S",
            .l_bus => "L",
            .virtual_address => "V",
        },
        else => " ",
    });
    zgui.sameLine(.{});
    zgui.text("{X:0>1}", .{ reg.asn });

    zgui.sameLine(.{ .offset_from_start_x = 50 * glyph_width });
    switch (reg.exec_mode) {
        .normal          => zgui.textUnformattedColored(colors.exec_normal, ""),
        .interrupt       => zgui.textUnformattedColored(colors.exec_interrupt, "Interrupt"),
        .fault           => zgui.textUnformattedColored(colors.exec_fault, "          Fault"),
        .interrupt_fault => zgui.textUnformattedColored(colors.exec_fault, "Interrupt Fault"),
    }
    zgui.sameLine(.{ .offset_from_start_x = 66 * glyph_width });
    if (stall)       zgui.textUnformattedColored(colors.exec_fault, "Stall")
    else if (atomic) zgui.textUnformattedColored(colors.exec_interrupt, "Atomic")
    else             zgui.textUnformatted(" ");

    zgui.setCursorPosX(2 * glyph_width);
    zgui.textUnformattedColored(colors.label, "RSN");
    zgui.sameLine(.{});
    zgui.textUnformattedColored(w_color, switch (cs.special) {
        .load_rsn_from_ll => "L",
        .toggle_rsn => "T",
        else => " ",
    });
    zgui.sameLine(.{});
    zgui.text("{X:0>2}", .{ rsn });

    zgui.sameLine(.{ .offset_from_start_x = 15 * glyph_width });
    zgui.textUnformattedColored(colors.label, "DL");
    zgui.sameLine(.{});
    zgui.textUnformattedColored(w_color, switch (cs.dl_op) {
        .from_d => "D",
        .to_d, .hold => " ",
    });
    zgui.sameLine(.{});
    zgui.text("{X:0>4}", .{ reg.dl });
    zgui.sameLine(.{});
    zgui.textUnformattedColored(colors.read, switch (cs.dl_op) {
        .to_d => "D",
        .from_d, .hold => " ",
    });

    zgui.sameLine(.{ .offset_from_start_x = 30 * glyph_width });
    zgui.textUnformattedColored(colors.label, "OA");
    zgui.sameLine(.{});
    zgui.textUnformattedColored(w_color, switch (cs.ob_oa_op) {
        .from_dl => "D",
        else => " "
    });
    zgui.sameLine(.{});
    zgui.text("{X:0>1}", .{ reg.oa });
    zgui.sameLine(.{});
    var oa_reads = [_]u8 {' '} ** 3;
    if (cs.jr_rsel == .oa) oa_reads[0] = 'J';
    if (cs.kr_rsel == .oa) oa_reads[1] = 'K';
    if (cs.jkr_wsel == .oa) oa_reads[2] = 'W';
    zgui.textUnformattedColored(colors.read, &oa_reads);


    zgui.sameLine(.{ .offset_from_start_x = 42 * glyph_width });
    zgui.textUnformattedColored(colors.label, "OB");
    zgui.sameLine(.{});
    zgui.textUnformattedColored(w_color, switch (cs.ob_oa_op) {
        .hold => " ",
        .from_dl => "D",
        .increment_ob => "+",
        .clear_ob => "Z",
    });
    zgui.sameLine(.{});
    zgui.text("{X:0>1}", .{ reg.ob });
    zgui.sameLine(.{});
    var ob_reads = [_]u8 {' '} ** 3;
    if (cs.jr_rsel == .ob) ob_reads[0] = 'J';
    if (cs.kr_rsel == .ob) ob_reads[1] = 'K';
    if (cs.jkr_wsel == .ob) ob_reads[2] = 'W';
    zgui.textUnformattedColored(colors.read, &ob_reads);

    {
        zgui.sameLine(.{ .offset_from_start_x = 54 * glyph_width });
        const c = if (reg.stat.c) "C" else " ";
        const v = if (reg.stat.v) "V" else " ";
        const n = if (reg.stat.n) "N" else " ";
        const z = if (reg.stat.z) "Z" else " ";
        const nk = if (reg.stat.next_k) "n" else " ";
        const k = if (reg.stat.k) "K" else " ";
        const a = if (reg.stat.a) "A" else " ";
        const i = if (sim.exec_state.interrupt_pending[@enumToInt(pipe)]) "I" else " ";
        zgui.textUnformattedColored(colors.label, "STAT");
        zgui.sameLine(.{});
        zgui.text("{s}{s}{s}{s}{s}{s}{s}{s}", .{ c, v, n, z, nk, k, a, i });
    }

    doRegisterLine(rf, rsn, 0,  .zero,                .zero,          glyph_width, cs, reg.oa, reg.ob, inhibit_writes);
    doRegisterLine(rf, rsn, 2,  .rp,                  .ip,            glyph_width, cs, reg.oa, reg.ob, inhibit_writes);
    doRegisterLine(rf, rsn, 4,  .sp,                  .next_ip,       glyph_width, cs, reg.oa, reg.ob, inhibit_writes);
    doRegisterLine(rf, rsn, 6,  .bp,                  .asn,           glyph_width, cs, reg.oa, reg.ob, inhibit_writes);
    doRegisterLine(rf, rsn, 8,  .fault_ua_dl,         .kxp,           glyph_width, cs, reg.oa, reg.ob, inhibit_writes);
    doRegisterLine(rf, rsn, 10, .fault_rsn_stat,      .uxp,           glyph_width, cs, reg.oa, reg.ob, inhibit_writes);
    doRegisterLine(rf, rsn, 12, .int_rsn_fault_ob_oa, .rs_reserved,   glyph_width, cs, reg.oa, reg.ob, inhibit_writes);
    doRegisterLine(rf, rsn, 14, .temp_1,              .temp_2,        glyph_width, cs, reg.oa, reg.ob, inhibit_writes);

    if (sim.s.pipe == pipe) {
        const rf_out = sim.register_file.setup(.{
            .rsn = reg.rsn,
            .oa = reg.oa,
            .ob = reg.ob,
            .cs_jr_rsel = cs.jr_rsel,
            .cs_kr_rsel = cs.kr_rsel,
            .cs_jr_rx = cs.jr_rx,
            .cs_kr_rx = cs.kr_rx,
            .cs_sr1_ri = cs.sr1_ri,
            .cs_sr2_ri = cs.sr2_ri,
            .cs_base = cs.base,
            .cs_literal = cs.literal,
            .cs_jl_src = cs.jl_src,
            .cs_jh_src = cs.jh_src,
            .cs_k_src = cs.k_src,
        });

        const address_out = Simulator.address_generator.setup(.{
            .base = rf_out.address_base,
            .cs_offset = cs.offset,
            .cs_literal = cs.literal,
        });

        doSetupLines(sim.s.cs, sim.s.j, sim.s.k, rf_out.address_base, address_out.debug_offset, false, glyph_width);

    } else if (sim.c.pipe == pipe) {
        const rf_out = sim.register_file.setup(.{
            .rsn = reg.rsn,
            .oa = reg.oa,
            .ob = reg.ob,
            .cs_jr_rsel = cs.jr_rsel,
            .cs_kr_rsel = cs.kr_rsel,
            .cs_jr_rx = cs.jr_rx,
            .cs_kr_rx = cs.kr_rx,
            .cs_sr1_ri = cs.sr1_ri,
            .cs_sr2_ri = cs.sr2_ri,
            .cs_base = cs.base,
            .cs_literal = cs.literal,
            .cs_jl_src = cs.jl_src,
            .cs_jh_src = cs.jh_src,
            .cs_k_src = cs.k_src,
        });

        const address_out = Simulator.address_generator.setup(.{
            .base = rf_out.address_base,
            .cs_offset = cs.offset,
            .cs_literal = cs.literal,
        });

        doSetupLines(sim.c.cs, rf_out.j, rf_out.k, rf_out.address_base, address_out.debug_offset, false, glyph_width);
    } else {
        const s = Simulator.SetupStage.init(sim.t, sim.register_file, sim.s.isAtomic(), sim.c.isAtomic());
        doSetupLines(sim.t.cs, s.j, s.k, s.debug_base, s.debug_offset, true, glyph_width);
    }
    

    if (sim.s.pipe == pipe) {
        const c = Simulator.ComputeStage.init(sim.s, sim.address_translator, sim.exec_state);
        zgui.text("arith: {X:0>4}_{X:0>4}", .{ c.arith.data.high, c.arith.data.low });
        zgui.text("logic: {X:0>4}", .{ c.logic });

    } else if (sim.c.pipe == pipe) {
        const t = Simulator.TransactStage.init(sim.c, sim.microcode, sim.exec_state, sim.memory, sim.frame_tracker);
        zgui.text("L: {X:0>4}_{X:0>4}", .{ t.l.high, t.l.low });
        zgui.text("D: {X:0>4}", .{ t.data_to_write });
        zgui.text("DL: {X:0>4}", .{ t.reg.dl });
        zgui.text("next UA: {X:0>4}", .{ t.reg.ua });

    }

    var buf: [4096]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buf);
    cs.print(stream.writer()) catch {};

    zgui.textWrapped("{s}", .{ stream.getWritten() });
}

fn doRegisterLine(
    rf: *Simulator.RegisterFile,
    rsn: misc.RegistersetNumber,
    comptime first_gpr: misc.RegisterIndex,
    comptime sr1: ControlSignals.SR1Index,
    comptime sr2: ControlSignals.SR2Index,
    glyph_width: f32,
    cs: ControlSignals,
    oa: misc.OperandA,
    ob: misc.OperandB,
    inhibit_writes: bool,
) void {
    const fmt = comptime if (first_gpr < 10) " R{}" else "R{}";

    const label0 = std.fmt.comptimePrint(fmt, .{ first_gpr + 1 });
    const label1 = std.fmt.comptimePrint(fmt, .{ first_gpr });
    const label2 = comptime switch (sr1) {
        .zero => "    Z",
        .rp => "   RP",
        .sp => "   SP",
        .bp => "   BP",
        .fault_ua_dl => " UADL",
        .fault_rsn_stat => "RSTAT",
        .int_rsn_fault_ob_oa => "ROBOA",
        .temp_1 => "   T1",
    };

    const label3 = comptime switch (sr2) {
        .zero => "  Z",
        .ip => " IP",
        .asn => "ASN",
        .next_ip => "NIP",
        .kxp => "KXP",
        .uxp => "UXP",
        .rs_reserved => "RSR",
        .temp_2 => " T2",
    };

    const jr_index: misc.RegisterIndex = switch (cs.jr_rsel) {
        .zero => 0,
        .literal => @truncate(misc.RegisterIndex, cs.literal),
        .oa => oa,
        .ob => ob,
    };
    const kr_index: misc.RegisterIndex = switch (cs.kr_rsel) {
        .zero => 0,
        .literal => @truncate(misc.RegisterIndex, cs.literal),
        .oa => oa,
        .ob => ob,
    } ^ @boolToInt(cs.kr_rx);

    var jkr_index: misc.RegisterIndex = switch (cs.jkr_wsel) {
        .zero => 0,
        .literal => @truncate(misc.RegisterIndex, cs.literal),
        .oa => oa,
        .ob => ob,
    };

    const w_color = if (inhibit_writes) colors.transparent else colors.write;

    {
        const second_gpr = first_gpr + 1;
        zgui.setCursorPosX(glyph_width * 2);
        zgui.textUnformattedColored(colors.label, label0);

        const write = switch (cs.jkr_wmode) {
            .no_write => "  ",
            .write_16 => if (second_gpr == jkr_index) "LL" else "  ",
            .write_16_xor1 => if (second_gpr == (jkr_index ^ 1)) "LL" else "  ",
            .write_32 => if (second_gpr == jkr_index) "LL" else if (second_gpr == (jkr_index ^ 1)) "LH" else "  ",
        };
        zgui.sameLine(.{});
        zgui.textUnformattedColored(w_color, write);

        zgui.sameLine(.{});
        zgui.text("{X:0>4}", .{ rf.readGPR(rsn, second_gpr) });

        var reads = [_]u8 {' '} ** 2;
        if ((cs.jl_src == .jrl) and second_gpr == jr_index) {
            reads[0] = 'J';
        } else if (cs.jh_src == .jrh and second_gpr == (jr_index ^ 1)) {
            reads[0] = 'J';
        }
        if (cs.k_src == .kr and second_gpr == kr_index) {
            reads[1] = 'K';
        }
        zgui.sameLine(.{});
        zgui.textUnformattedColored(colors.read, &reads);
    }

    {
        zgui.sameLine(.{ .offset_from_start_x = glyph_width * 17 });
        zgui.textUnformattedColored(colors.label, label1);

        const write = switch (cs.jkr_wmode) {
            .no_write => "  ",
            .write_16 => if (first_gpr == jkr_index) "LL" else "  ",
            .write_16_xor1 => if (first_gpr == (jkr_index ^ 1)) "LL" else "  ",
            .write_32 => if (first_gpr == jkr_index) "LL" else if (first_gpr == (jkr_index ^ 1)) "LH" else "  ",
        };
        zgui.sameLine(.{});
        zgui.textUnformattedColored(w_color, write);

        zgui.sameLine(.{});
        zgui.text("{X:0>4}", .{ rf.readGPR(rsn, first_gpr) });

        var reads = [_]u8 {' '} ** 2;
        if ((cs.jl_src == .jrl) and first_gpr == jr_index) {
            reads[0] = 'J';
        } else if (cs.jh_src == .jrh and first_gpr == (jr_index ^ 1)) {
            reads[0] = 'J';
        }
        if (cs.k_src == .kr and first_gpr == kr_index) {
            reads[1] = 'K';
        }
        zgui.sameLine(.{});
        zgui.textUnformattedColored(colors.read, &reads);
    }

    {
        zgui.sameLine(.{ .offset_from_start_x = glyph_width * 32 });
        zgui.textUnformattedColored(colors.label, label2);

        const write = if (cs.sr1_wi == sr1) switch (cs.sr1_wsrc) {
            .no_write => " ",
            .rsn_sr1 => "S",
            .l_bus => "L",
            .virtual_address => "V",
        } else " ";
        zgui.sameLine(.{});
        zgui.textUnformattedColored(w_color, write);

        zgui.sameLine(.{});
        zgui.text("{X:0>8}", .{ rf.readSR1(rsn, sr1) });

        var reads = [_]u8 {' '} ** 3;
        if (cs.sr1_ri == sr1) {
            if (cs.jl_src == .sr1l or cs.jh_src == .sr1h) {
                reads[0] = 'J';
            }
            if (cs.k_src == .sr1l) {
                reads[1] = 'K';
            }
            if (ControlSignals.addressBaseToSR1(cs.base)) |_| {
                reads[2] = 'B';
            }
        }
        zgui.sameLine(.{});
        zgui.textUnformattedColored(colors.read, &reads);
    }
    {
        zgui.sameLine(.{ .offset_from_start_x = glyph_width * 54 });
        zgui.textUnformattedColored(colors.label, label3);

        const write = if (cs.sr2_wi == sr2) switch (cs.sr2_wsrc) {
            .no_write => " ",
            .sr2 => "S",
            .l_bus => "L",
            .virtual_address => "V",
        } else " ";
        zgui.sameLine(.{});
        zgui.textUnformattedColored(w_color, write);

        zgui.sameLine(.{});
        zgui.text("{X:0>8}", .{ rf.readSR2(rsn, sr2) });

        var reads = [_]u8 {' '} ** 3;
        if (cs.sr2_ri == sr2) {
            if (cs.jl_src == .sr2l or cs.jh_src == .sr2h) {
                reads[0] = 'J';
            }
            if (cs.k_src == .sr2l) {
                reads[1] = 'K';
            }
            if (ControlSignals.addressBaseToSR2(cs.base)) |_| {
                reads[2] = 'B';
            }
        }
        zgui.sameLine(.{});
        zgui.textUnformattedColored(colors.read, &reads);
    }
}

fn doSetupLines(cs: ControlSignals, j: bus.JParts, k: bus.K, base: u32, offset: i7, is_setup: bool, glyph_width: f32) void {
    zgui.setCursorPosX(glyph_width * 5);
    zgui.textUnformattedColored(colors.label, switch (cs.jh_src) {
        .zero        => "  0000  ",
        .neg_one     => "  FFFF  ",
        .sx_jl       => "   SX   ",
        .jrh => if (cs.jr_rx) switch (cs.jr_rsel) {
            .zero    => "  R(0)  ",
            .literal => " R(lit) ",
            .oa      => " R(OA)  ",
            .ob      => " R(OB)  ",
        } else switch (cs.jr_rsel) {
            .zero    => " R(0^1) ",
            .literal => "R(lit^1)",
            .oa      => "R(OA^1) ",
            .ob      => "R(OB^1) ",
        },
        .sr1h        => "  SR1H  ",
        .sr2h        => "  SR2H  ",
    });

    // zgui.sameLine(.{ .offset_from_start_x = 15 * glyph_width });
    // zgui.textUnformattedColored(colors.label, "K");
    // zgui.sameLine(.{});
    // zgui.textColored(colors.alpha_gray, "{X:0>4}", .{ sim.s.k });

    // zgui.sameLine(.{ .offset_from_start_x = 25 * glyph_width });
    // zgui.textUnformattedColored(colors.label, "B+O");
    // zgui.sameLine(.{});
    // if (address_out.debug_offset < 0) {
    //     zgui.textColored(colors.alpha_gray, "{X:0>8} - {X:0>2}", .{ rf_out.address_base, @intCast(u6, -address_out.debug_offset) });
    // } else {
    //     zgui.textColored(colors.alpha_gray, "{X:0>8} + {X:0>2}", .{ rf_out.address_base, @intCast(u6, address_out.debug_offset) });
    // }

    const color = if (is_setup) colors.white else colors.alpha_gray;

    zgui.textUnformattedColored(colors.label, "J");
    zgui.sameLine(.{});
    zgui.textColored(color, "{X:0>4}_{X:0>4}", .{ j.high, j.low });

    zgui.sameLine(.{ .offset_from_start_x = 15 * glyph_width });
    zgui.textUnformattedColored(colors.label, "K");
    zgui.sameLine(.{});
    zgui.textColored(color, "{X:0>4}", .{ k });

    zgui.sameLine(.{ .offset_from_start_x = 25 * glyph_width });
    zgui.textUnformattedColored(colors.label, "B+O");
    zgui.sameLine(.{});
    if (offset < 0) {
        zgui.textColored(color, "{X:0>8} - {X:0>2}", .{ base, @intCast(u6, -offset) });
    } else {
        zgui.textColored(color, "{X:0>8} + {X:0>2}", .{ base, @intCast(u6, offset) });
    }
}