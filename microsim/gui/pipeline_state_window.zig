
pub fn doWindow(self: Pipeline_State) void {
    defer zgui.end();
    if (!zgui.begin(switch (self.pipeline) {
        .zero => "Pipe 0",
        .one => "Pipe 1",
        .two => "Pipe 2",
        .three => "Pipe 3",
    }, .{})) return;

    const glyph_width = zgui.calcTextSize("0", .{})[0];
    const w_color = if (self.inhibit_writes) colors.transparent else colors.write;

    switch (self.stage) {
        .reset => zgui.textUnformattedColored(colors.stage_reset, "<reset>"),
        .decode => zgui.textUnformattedColored(colors.stage_decode, "Decode"),
        .setup => zgui.textUnformattedColored(colors.stage_setup, "Setup"),
        .compute => zgui.textUnformattedColored(colors.stage_compute, "Compute"),
        .transact => zgui.textUnformattedColored(colors.stage_transact, "Transact"),
    }

    zgui.sameLine(.{ .offset_from_start_x = 15 * glyph_width });
    zgui.textUnformattedColored(colors.label, "UA");
    zgui.sameLine(.{});

    if (self.uc_slot_src) |src| {
        zgui.textUnformattedColored(colors.write, switch (src) {
            .hold => " ",
            .insn_decoder => "D",
            .continuation => "C",
            .seq_literal => "Q",
        });
    } else {
        zgui.textUnformattedColored(colors.label, " ");
    }
    zgui.sameLine(.{});
    zgui.text("{X:0>3}", .{ self.uc_slot.raw() });

    zgui.sameLine(.{ .offset_from_start_x = 26 * glyph_width });
    // if (uc.getOpcodeForAddress(reg.ua)) |opcode| {
    //     zgui.textUnformattedColored(colors.label, "Opcode");
    //     zgui.sameLine(.{});
    //     if (uc.getOpcodeGranularity(opcode) == 16) {
    //         zgui.text("{X:0>3}x", .{ opcode >> 4 });
    //     } else {
    //         zgui.text("{X:0>4}", .{ opcode });
    //     }
    // } else if (uc.getContinuationNumberForAddress(reg.ua)) |uop| {
    //     zgui.textUnformattedColored(colors.label, "    uop");
    //     zgui.sameLine(.{});
    //     zgui.text("{X:0>3}", .{ uop });
    // }

    zgui.sameLine(.{ .offset_from_start_x = 40 * glyph_width });
    zgui.textUnformattedColored(colors.label, "ASN4");
    zgui.sameLine(.{});
    zgui.textUnformattedColored(w_color, switch (self.cs.sr2_wi) {
        .asn => switch (self.cs.sr2_wsrc) {
            .l => "L",
            .sr2, .virtual_addr, .no_write => " ",
        },
        else => " ",
    });
    zgui.sameLine(.{});
    zgui.text("{X:0>1}", .{ self.asn.raw() });

    zgui.sameLine(.{ .offset_from_start_x = 50 * glyph_width });
    switch (self.exec_mode) {
        .normal          => zgui.textUnformattedColored(colors.exec_normal,    ""),
        .interrupt       => zgui.textUnformattedColored(colors.exec_interrupt, "Interrupt"),
        .fault           => zgui.textUnformattedColored(colors.exec_fault,     "          Fault"),
        .interrupt_fault => zgui.textUnformattedColored(colors.exec_fault,     "Interrupt Fault"),
    }
    zgui.sameLine(.{ .offset_from_start_x = 66 * glyph_width });
    if (self.stall_atomic)     zgui.textUnformattedColored(colors.exec_fault,     "Stall")
    else if (self.want_atomic) zgui.textUnformattedColored(colors.exec_interrupt, "Atomic")
    else                       zgui.textUnformatted(" ");

    zgui.setCursorPosX(2 * glyph_width);
    zgui.textUnformattedColored(colors.label, "RSN");
    zgui.sameLine(.{});
    zgui.textUnformattedColored(w_color, switch (self.cs.special) {
        .load_rsn_from_l_maybe_load_asn6 => "L",
        .toggle_rsn => "T",
        else => " ",
    });
    zgui.sameLine(.{});
    zgui.text("{X:0>2}", .{ self.rsn.raw() });

    zgui.sameLine(.{ .offset_from_start_x = 15 * glyph_width });
    zgui.textUnformattedColored(colors.label, "DR");
    zgui.sameLine(.{});
    zgui.textUnformattedColored(w_color, switch (self.cs.dr_op) {
        .hold => " ",
        .low_from_d_hold_high => "L",
        .high_from_d_hold_low => "H",
        .from_d => "D",
    });
    zgui.sameLine(.{});
    zgui.text("{X:0>4}", .{ self.dr.raw() });
    zgui.sameLine(.{});
    zgui.textUnformattedColored(colors.read, switch (self.cs.bus_dir) {
        .none, .read, .write_from_ll => " ",
        .write_from_dr => "D",
    });

    zgui.sameLine(.{ .offset_from_start_x = 30 * glyph_width });
    zgui.textUnformattedColored(colors.label, "IJ");
    zgui.sameLine(.{});
    zgui.textUnformattedColored(w_color, switch (self.cs.ij_op) {
        .zero => "0",
        .from_ij => " ",
        .from_ik => "K",
        .from_iw => "W",
        .xor1 => "^",
        .increment => "+",
        .from_continuation => "C",
        .from_decode => "D",
    });
    zgui.sameLine(.{});
    zgui.text("{X:0>1}", .{ self.ij.raw() });
    zgui.sameLine(.{});

    zgui.sameLine(.{ .offset_from_start_x = 38 * glyph_width });
    zgui.textUnformattedColored(colors.label, "IK");
    zgui.sameLine(.{});
    zgui.textUnformattedColored(w_color, switch (self.cs.ik_op) {
        .zero => "0",
        .from_ij => "J",
        .from_ik => " ",
        .from_iw => "W",
        .xor1 => "^",
        .increment => "+",
        .from_continuation => "C",
        .from_decode => "D",
    });
    zgui.sameLine(.{});
    zgui.text("{X:0>1}", .{ self.ik.raw() });
    zgui.sameLine(.{});

    zgui.sameLine(.{ .offset_from_start_x = 46 * glyph_width });
    zgui.textUnformattedColored(colors.label, "IW");
    zgui.sameLine(.{});
    zgui.textUnformattedColored(w_color, switch (self.cs.iw_op) {
        .zero => "0",
        .from_ij => "J",
        .from_ik => "K",
        .from_iw => " ",
        .xor1 => "^",
        .increment => "+",
        .from_continuation => "C",
        .from_decode => "D",
    });
    zgui.sameLine(.{});
    zgui.text("{X:0>1}", .{ self.iw.raw() });
    zgui.sameLine(.{});


    {
        zgui.sameLine(.{ .offset_from_start_x = 54 * glyph_width });
        const c = if (self.stat_c) "C" else " ";
        const v = if (self.stat_v) "V" else " ";
        const n = if (self.stat_n) "N" else " ";
        const z = if (self.stat_z) "Z" else " ";
        const nk = if (self.next_k) "k" else " ";
        const k = if (self.stat_k) "K" else " ";
        const a = if (self.stat_a) "A" else " ";
        zgui.textUnformattedColored(colors.label, "STAT");
        zgui.sameLine(.{});
        zgui.text("{s}{s}{s}{s}{s}{s}{s}", .{ c, v, n, z, nk, k, a, });
    }

    self.doRegisterLine(0,  .one,                    .zero,          glyph_width);
    self.doRegisterLine(2,  .rp,                     .ip,            glyph_width);
    self.doRegisterLine(4,  .sp,                     .next_ip,       glyph_width);
    self.doRegisterLine(6,  .bp,                     .asn,           glyph_width);
    self.doRegisterLine(8,  .fault_uc_slot_dr,       .kxp,           glyph_width);
    self.doRegisterLine(10, .fault_rsn_stat,         .uxp,           glyph_width);
    self.doRegisterLine(12, .int_rsn_fault_iw_ik_ij, .rs_reserved,   glyph_width);
    self.doRegisterLine(14, .temp_1,                 .temp_2,        glyph_width);

    // if (sim.s.pipe == pipeline) {
    //     const rf_out = sim.register_file.setup(.{
    //         .rsn = reg.rsn,
    //         .oa = reg.oa,
    //         .ob = reg.ob,
    //         .cs_jr_rsel = cs.jr_rsel,
    //         .cs_kr_rsel = cs.kr_rsel,
    //         .cs_jr_rx = cs.jr_rx,
    //         .cs_kr_rx = cs.kr_rx,
    //         .cs_sr1_ri = cs.sr1_ri,
    //         .cs_sr2_ri = cs.sr2_ri,
    //         .cs_base = cs.base,
    //         .cs_literal = cs.literal,
    //         .cs_jl_src = cs.jl_src,
    //         .cs_jh_src = cs.jh_src,
    //         .cs_k_src = cs.k_src,
    //     });

    //     const address_out = Simulator.address_generator.setup(.{
    //         .base = rf_out.address_base,
    //         .cs_offset = cs.offset,
    //         .cs_literal = cs.literal,
    //     });

    //     doSetupLines(sim.s.cs, sim.s.j, sim.s.k, rf_out.address_base, address_out.debug_offset, false, glyph_width);

    // } else if (sim.c.pipe == pipe) {
    //     const rf_out = sim.register_file.setup(.{
    //         .rsn = reg.rsn,
    //         .oa = reg.oa,
    //         .ob = reg.ob,
    //         .cs_jr_rsel = cs.jr_rsel,
    //         .cs_kr_rsel = cs.kr_rsel,
    //         .cs_jr_rx = cs.jr_rx,
    //         .cs_kr_rx = cs.kr_rx,
    //         .cs_sr1_ri = cs.sr1_ri,
    //         .cs_sr2_ri = cs.sr2_ri,
    //         .cs_base = cs.base,
    //         .cs_literal = cs.literal,
    //         .cs_jl_src = cs.jl_src,
    //         .cs_jh_src = cs.jh_src,
    //         .cs_k_src = cs.k_src,
    //     });

    //     const address_out = Simulator.address_generator.setup(.{
    //         .base = rf_out.address_base,
    //         .cs_offset = cs.offset,
    //         .cs_literal = cs.literal,
    //     });

    //     doSetupLines(sim.c.cs, rf_out.j, rf_out.k, rf_out.address_base, address_out.debug_offset, false, glyph_width);
    // } else {
    //     const s = Simulator.SetupStage.init(sim.t, sim.register_file, sim.s.isAtomic(), sim.c.isAtomic());
    //     doSetupLines(sim.t.cs, s.j, s.k, s.debug_base, s.debug_offset, true, glyph_width);
    // }
    

    // if (sim.s.pipe == pipe) {
    //     const c = Simulator.ComputeStage.init(sim.s, sim.address_translator, sim.exec_state);
    //     zgui.text("arith: {X:0>4}_{X:0>4}", .{ c.arith.data.high, c.arith.data.low });
    //     zgui.text("logic: {X:0>4}", .{ c.logic });

    // } else if (sim.c.pipe == pipe) {
    //     const t = Simulator.TransactStage.init(sim.c, sim.microcode, sim.exec_state, sim.memory, sim.frame_tracker);
    //     zgui.text("L: {X:0>4}_{X:0>4}", .{ t.l.high, t.l.low });
    //     zgui.text("D: {X:0>4}", .{ t.data_to_write });
    //     zgui.text("DL: {X:0>4}", .{ t.reg.dl });
    //     zgui.text("next UA: {X:0>4}", .{ t.reg.ua });

    // }

    // var buf: [4096]u8 = undefined;
    // var stream = std.io.fixedBufferStream(&buf);
    // cs.print(stream.writer()) catch {};

    // zgui.textWrapped("{s}", .{ stream.getWritten() });
}

fn doRegisterLine(
    self: Pipeline_State,
    comptime first_gpr: hw.Register_Index,
    comptime sr1: Control_Signals.SR1_Index,
    comptime sr2: Control_Signals.SR2_Index,
    glyph_width: f32,
) void {
    const fmt = comptime if (first_gpr < 10) " R{}" else "R{}";

    const cs = self.cs;
    const ij = self.ij.raw();
    const ik = self.ik.raw();
    const iw = self.iw.raw();

    const label0 = std.fmt.comptimePrint(fmt, .{ first_gpr + 1 });
    const label1 = std.fmt.comptimePrint(fmt, .{ first_gpr });
    const label2 = comptime switch (sr1) {
        .one => "  ONE",
        .rp => "   RP",
        .sp => "   SP",
        .bp => "   BP",
        .fault_uc_slot_dr => " FSDR",
        .fault_rsn_stat => "  FRS",
        .int_rsn_fault_iw_ik_ij => " IRFI",
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

    const w_color = if (self.inhibit_writes) colors.transparent else colors.write;

    {
        const second_gpr = first_gpr + 1;
        zgui.setCursorPosX(glyph_width * 2);
        zgui.textUnformattedColored(colors.label, label0);

        var write_label = "  ";
        if (second_gpr != 0) switch (cs.reg_write) {
            .no_write => {},
            .write_16 => {
                if (second_gpr == iw) write_label = "LL";
            },
            .write_32 => {
                if (second_gpr == iw) write_label = "LL";
                if (second_gpr == (iw ^ 1)) write_label = "LH";
            },
            _ => {},
        };
        zgui.sameLine(.{});
        zgui.textUnformattedColored(w_color, write_label);

        zgui.sameLine(.{});
        zgui.text("{X:0>4}", .{ self.rs.reg[second_gpr].raw() });

        var reads = [_]u8 {' '} ** 2;
        if ((cs.jl_src == .jrl or cs.jh_src == .jrl_sx) and second_gpr == ij) {
            reads[0] = 'J';
        } else if (cs.jh_src == .jrh and second_gpr == (ij ^ 1)) {
            reads[0] = 'J';
        }
        if (cs.k_src == .kr and second_gpr == ik) {
            reads[1] = 'K';
        }
        zgui.sameLine(.{});
        zgui.textUnformattedColored(colors.read, &reads);
    }

    {
        zgui.sameLine(.{ .offset_from_start_x = glyph_width * 17 });
        zgui.textUnformattedColored(colors.label, label1);

        var write_label = "  ";
        if (first_gpr != 0) switch (cs.reg_write) {
            .no_write => {},
            .write_16 => {
                if (first_gpr == iw) write_label = "LL";
            },
            .write_32 => {
                if (first_gpr == iw) write_label = "LL";
                if (first_gpr == (iw ^ 1)) write_label = "LH";
            },
            _ => {},
        };
        zgui.sameLine(.{});
        zgui.textUnformattedColored(w_color, write_label);

        zgui.sameLine(.{});
        zgui.text("{X:0>4}", .{ self.rs.reg[first_gpr].raw() });

        var reads = [_]u8 {' '} ** 2;
        if ((cs.jl_src == .jrl or cs.jh_src == .jrl_sx) and first_gpr == ij) {
            reads[0] = 'J';
        } else if (cs.jh_src == .jrh and first_gpr == (ij ^ 1)) {
            reads[0] = 'J';
        }
        if (cs.k_src == .kr and first_gpr == ik) {
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
            .l => "L",
            .virtual_addr => "V",
        } else " ";
        zgui.sameLine(.{});
        zgui.textUnformattedColored(w_color, write);

        zgui.sameLine(.{});
        zgui.text("{X:0>8}", .{ self.rs.sr1[sr1.raw()].raw() });

        var reads = [_]u8 {' '} ** 3;
        if (cs.sr1_ri == sr1) {
            if (cs.jl_src == .sr1l or cs.jh_src == .sr1h) {
                reads[0] = 'J';
            }
            if (cs.k_src == .sr1l) {
                reads[1] = 'K';
            }
        }
        if (cs.base_ri.to_sr1_index()) |sr1b| {
            if (sr1b == sr1) {
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
            .l => "L",
            .virtual_addr => "V",
        } else " ";
        zgui.sameLine(.{});
        zgui.textUnformattedColored(w_color, write);

        zgui.sameLine(.{});
        zgui.text("{X:0>8}", .{ self.rs.sr2[sr2.raw()].raw() });

        var reads = [_]u8 {' '} ** 3;
        if (cs.sr2_ri == sr2) {
            if (cs.jl_src == .sr2l or cs.jh_src == .sr2h) {
                reads[0] = 'J';
            }
            if (cs.k_src == .sr2l) {
                reads[1] = 'K';
            }
        }
        if (cs.base_ri.to_sr2_index()) |sr2b| {
            if (sr2b == sr2) {
                reads[2] = 'B';
            }
        }
        zgui.sameLine(.{});
        zgui.textUnformattedColored(colors.read, &reads);
    }
}

// fn doSetupLines(cs: Control_Signals, j: hw.J, k: hw.K, base: u32, offset: i7, is_setup: bool, glyph_width: f32) void {
//     zgui.setCursorPosX(glyph_width * 5);
//     zgui.textUnformattedColored(colors.label, switch (cs.jh_src) {
//         .zero        => "  0000  ",
//         .neg_one     => "  FFFF  ",
//         .sx_jl       => "   SX   ",
//         .jrh => if (cs.jr_rx) switch (cs.jr_rsel) {
//             .zero    => "  R(0)  ",
//             .literal => " R(lit) ",
//             .oa      => " R(OA)  ",
//             .ob      => " R(OB)  ",
//         } else switch (cs.jr_rsel) {
//             .zero    => " R(0^1) ",
//             .literal => "R(lit^1)",
//             .oa      => "R(OA^1) ",
//             .ob      => "R(OB^1) ",
//         },
//         .sr1h        => "  SR1H  ",
//         .sr2h        => "  SR2H  ",
//     });

//     // zgui.sameLine(.{ .offset_from_start_x = 15 * glyph_width });
//     // zgui.textUnformattedColored(colors.label, "K");
//     // zgui.sameLine(.{});
//     // zgui.textColored(colors.alpha_gray, "{X:0>4}", .{ sim.s.k });

//     // zgui.sameLine(.{ .offset_from_start_x = 25 * glyph_width });
//     // zgui.textUnformattedColored(colors.label, "B+O");
//     // zgui.sameLine(.{});
//     // if (address_out.debug_offset < 0) {
//     //     zgui.textColored(colors.alpha_gray, "{X:0>8} - {X:0>2}", .{ rf_out.address_base, @intCast(u6, -address_out.debug_offset) });
//     // } else {
//     //     zgui.textColored(colors.alpha_gray, "{X:0>8} + {X:0>2}", .{ rf_out.address_base, @intCast(u6, address_out.debug_offset) });
//     // }

//     const color = if (is_setup) colors.white else colors.alpha_gray;

//     zgui.textUnformattedColored(colors.label, "J");
//     zgui.sameLine(.{});
//     zgui.textColored(color, "{X:0>4}_{X:0>4}", .{ j.high, j.low });

//     zgui.sameLine(.{ .offset_from_start_x = 15 * glyph_width });
//     zgui.textUnformattedColored(colors.label, "K");
//     zgui.sameLine(.{});
//     zgui.textColored(color, "{X:0>4}", .{ k });

//     zgui.sameLine(.{ .offset_from_start_x = 25 * glyph_width });
//     zgui.textUnformattedColored(colors.label, "B+O");
//     zgui.sameLine(.{});
//     if (offset < 0) {
//         zgui.textColored(color, "{X:0>8} - {X:0>2}", .{ base, @intCast(u6, -offset) });
//     } else {
//         zgui.textColored(color, "{X:0>8} + {X:0>2}", .{ base, @intCast(u6, offset) });
//     }
// }

const Pipeline_State = @This();
const sim = @import("lib_microsim");
const Control_Signals = hw.Control_Signals;
const at = hw.addr.translation;
const hw = arch.hw;
const arch = @import("lib_arch");
const colors = @import("colors.zig");
const zgui = @import("zgui");
const std = @import("std");
