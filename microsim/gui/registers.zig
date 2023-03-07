const std = @import("std");
const zgui = @import("zgui");
const misc = @import("misc");
const Simulator = @import("Simulator");
const ControlSignals = @import("ControlSignals");

const no_color = [_]f32 { 0, 0, 0, 0 };
const label_color = [_]f32 { 0.4, 0.4, 0.4, 1.0 };
const read_color = [_]f32 { 0.2, 0.8, 0.2, 1.0 };
const write_color = [_]f32 { 0.7, 0.4, 0.2, 1.0 };
const inhibited_write_color = [_]f32 { 0.7, 0.4, 0.2, 0.5 };

fn showLine(
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

    const w_color = if (inhibit_writes) inhibited_write_color else write_color;

    {
        const second_gpr = first_gpr + 1;
        zgui.setCursorPosX(glyph_width * 2);
        zgui.textUnformattedColored(label_color, label0);

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
        if ((cs.jl_src == .jrl or cs.jh_src == .jrl or cs.jh_src == .sx_jl) and second_gpr == jr_index) {
            reads[0] = 'J';
        } else if (cs.jh_src == .jrh and second_gpr == (jr_index ^ 1)) {
            reads[0] = 'J';
        }
        if (cs.k_src == .kr and second_gpr == kr_index) {
            reads[1] = 'K';
        }
        zgui.sameLine(.{});
        zgui.textUnformattedColored(read_color, &reads);
    }

    {
        zgui.sameLine(.{ .offset_from_start_x = glyph_width * 17 });
        zgui.textUnformattedColored(label_color, label1);

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
        if ((cs.jl_src == .jrl or cs.jh_src == .jrl or cs.jh_src == .sx_jl) and first_gpr == jr_index) {
            reads[0] = 'J';
        } else if (cs.jh_src == .jrh and first_gpr == (jr_index ^ 1)) {
            reads[0] = 'J';
        }
        if (cs.k_src == .kr and first_gpr == kr_index) {
            reads[1] = 'K';
        }
        zgui.sameLine(.{});
        zgui.textUnformattedColored(read_color, &reads);
    }

    {
        zgui.sameLine(.{ .offset_from_start_x = glyph_width * 32 });
        zgui.textUnformattedColored(label_color, label2);

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
            if (0 == (@enumToInt(cs.base) & 8)) {
                reads[2] = 'B';
            }
        }
        zgui.sameLine(.{});
        zgui.textUnformattedColored(read_color, &reads);
    }
    {
        zgui.sameLine(.{ .offset_from_start_x = glyph_width * 52 });
        zgui.textUnformattedColored(label_color, label3);

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
            if (1 == (@enumToInt(cs.base) & 8)) {
                reads[2] = 'B';
            }
        }
        zgui.sameLine(.{});
        zgui.textUnformattedColored(read_color, &reads);
    }
}

pub fn showRegisterFile(
    rf: *Simulator.RegisterFile,
    rsn: misc.RegistersetNumber,
    cs: ControlSignals,
    oa: misc.OperandA,
    ob: misc.OperandB,
    inhibit_writes: bool,
) void {
    const glyph_width = zgui.calcTextSize("0", .{})[0] + 1;
    showLine(rf, rsn, 0, .zero, .zero, glyph_width, cs, oa, ob, inhibit_writes);
    showLine(rf, rsn, 2, .rp, .ip, glyph_width, cs, oa, ob, inhibit_writes);
    showLine(rf, rsn, 4, .sp, .next_ip, glyph_width, cs, oa, ob, inhibit_writes);
    showLine(rf, rsn, 6, .bp, .asn, glyph_width, cs, oa, ob, inhibit_writes);
    showLine(rf, rsn, 8, .fault_ua_dl, .kxp, glyph_width, cs, oa, ob, inhibit_writes);
    showLine(rf, rsn, 10, .fault_rsn_stat, .uxp, glyph_width, cs, oa, ob, inhibit_writes);
    showLine(rf, rsn, 12, .int_rsn_fault_ob_oa, .rs_reserved, glyph_width, cs, oa, ob, inhibit_writes);
    showLine(rf, rsn, 14, .temp_1, .temp_2, glyph_width, cs, oa, ob, inhibit_writes);
}
