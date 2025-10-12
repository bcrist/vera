// Implements a 32 bit population counter.
// i.e. instead of bits having a value corresponding to ascending powers of 2, all 32 input bits have value 1.

pub const Chip = lc4k.LC4064ZE_TQFP100;
const speed_grade = 7;

pub const clk = Chip.Signal.clk0;

pub const unit = [@bitSizeOf(arch.compute.Unit)]Chip.Signal {
    .io_D1,
    .io_D2,
    .io_D3,
};

pub const inputs = [_]Chip.Signal {
    .io_A1,
    .io_A2,
    .io_A3,
    .io_A4,
    .io_A5,
    .io_A6,
    .io_A7,
    .io_A8,
    .io_A9,
    .io_A10,
    .io_A11,
    .io_A12,
    .io_A13,
    .io_A14,
    .io_A15,
    .io_B15,
    .io_B14,
    .io_B13,
    .io_B12,
    .io_B11,
    .io_B10,
    .io_B9,
    .io_B8,
    .io_B7,
    .io_B6,
    .io_B5,
    .io_B4,
    .io_B3,
    .io_B2,
    .io_B1,
    .io_B0,
    .io_C0,
};

pub const outputs = [_]Chip.Signal {
    .io_D10,
    .io_D11,
    .io_D12,
    .io_D13,
    .io_D14,
    .io_C14,
};

pub const oe_out = Chip.Signal.io_D15;

pub const L0 = struct {
    pub const v1 = inputs;
};

pub const L1 = struct {
    pub const v1 = [_]Chip.Signal {
        .mc_A1,
        .mc_A2,
        .mc_A3,
        .mc_A4,
        .mc_A5,
        .mc_A6,
        .mc_A7,
        .mc_A8,
    };
    pub const v2 = [_]Chip.Signal {
        .mc_A9,
        .mc_A10,
        .mc_A11,
        .mc_A12,
        .mc_A13,
        .mc_A14,
        .mc_A15,
        .mc_B1,
    };
    pub const v4 = [_]Chip.Signal {
        .mc_B3,
        .mc_B4,
        .mc_B6,
    };
    pub const v8 = [_]Chip.Signal {
        .mc_B7,
        .mc_B8,
        .mc_B9,
    };
    pub const v32 = [_]Chip.Signal {
        .mc_B10,
    };
};

pub const L2 = struct {
    pub const v1 = [_]Chip.Signal {
        .mc_C0,
        .mc_C1,
    };
    pub const v2 = [_]Chip.Signal {
        .mc_C2,
        .mc_C3,
        .mc_C4,
        .mc_C5,
    };
    pub const v4 = [_]Chip.Signal {
        .mc_C6,
        .mc_C7,
        .mc_C8,
        .mc_C9,
    };
    pub const v8 = [_]Chip.Signal {
        .mc_C10,
        .mc_C11,
    };
    pub const v16 = [_]Chip.Signal {
        .mc_C12,
        .mc_C13,
    };
    pub const v32 = [_]Chip.Signal {
        .mc_B11,
    };
};

pub const L3 = struct {
    pub const v2 = [_]Chip.Signal {
        .mc_C15,
    };
    pub const v4 = [_]Chip.Signal {
        .mc_D0,
        .mc_D1,
    };
    pub const v8 = [_]Chip.Signal {
        .mc_D2,
        .mc_D3,
    };
    pub const v16 = [_]Chip.Signal {
        .mc_D4,
        .mc_D5,
    };
};

pub const L4 = struct {
    pub const v8 = [_]Chip.Signal {
        .mc_D6,
        .mc_D7,
    };
    pub const v16 = [_]Chip.Signal {
        .mc_D8,
        .mc_D9,
    };
};

pub fn configure(names: *Chip.Names, lp: *Chip.Logic_Parser) !Chip {
    var chip: Chip = .{};
    
    @setEvalBranchQuota(10000);
    try names.add_signal_name(clk, "CLK");
    try names.add_signal_name(oe_out, "oe_out");
    try names.add_names(unit, .{ .name = "UNIT" });
    try names.add_names(inputs, .{ .name = "in" });
    try names.add_names(outputs, .{ .name = "out" });
    try names.add_names(L1, .{ .name = "L1" });
    try names.add_names(L2, .{ .name = "L2" });
    try names.add_names(L3, .{ .name = "L3" });
    try names.add_names(L4, .{ .name = "L4" });

    for (&chip.glb) |*glb| {
        glb.bclock0 = .clk0_pos;
    }

    const oe_eqn = std.fmt.comptimePrint("(UNIT == {d}'{d}) & !CLK", .{
        @bitSizeOf(arch.compute.Unit),
        arch.compute.Unit.count.raw(),
    });

    chip.goe2 = .{ .source = .{ .glb_shared_pt_enable = 3 } };
    chip.glb[3].shared_pt_enable = try lp.pt(oe_eqn, .{});

    chip.default_slew_rate = .fast;

    chip.mc(oe_out.mc()).logic = try lp.logic("!(" ++ oe_eqn ++ ")", .{});
    chip.mc(oe_out.mc()).output.oe = .output_only;

    // Remaining outputs:
    // output[0..6]

    // Remaining v1:
    // L0.v1[0] 
    // L0.v1[1]
    // L0.v1[2]
    // L0.v1[3]
    // L0.v1[4] 
    // L0.v1[5]
    // L0.v1[6]
    // L0.v1[7]
    // L0.v1[8] 
    // L0.v1[9]
    // L0.v1[10]
    // L0.v1[11]
    // L0.v1[12] 
    // L0.v1[13]
    // L0.v1[14]
    // L0.v1[15]
    // L0.v1[16] 
    // L0.v1[17]
    // L0.v1[18]
    // L0.v1[19]
    // L0.v1[20] 
    // L0.v1[21]
    // L0.v1[22]
    // L0.v1[23]
    // L0.v1[24] 
    // L0.v1[25]
    // L0.v1[26]
    // L0.v1[27]
    // L0.v1[28]
    // L0.v1[29]
    // L0.v1[30]
    // L0.v1[31]



    // layer 1:
    try compressor_4_2(&chip, lp, extract4(&L0.v1, 0),                             .{ L1.v1[0], L1.v2[0] });
    try compressor_4_2(&chip, lp, extract4(&L0.v1, 4),                             .{ L1.v1[1], L1.v2[1] });
    try compressor_4_2(&chip, lp, extract4(&L0.v1, 8),                             .{ L1.v1[2], L1.v2[2] });
    try compressor_4_2(&chip, lp, extract4(&L0.v1, 12),                            .{ L1.v1[3], L1.v2[3] });
    try compressor_4_2(&chip, lp, extract4(&L0.v1, 16),                            .{ L1.v1[4], L1.v2[4] });
    try compressor_4_2(&chip, lp, extract4(&L0.v1, 20),                            .{ L1.v1[5], L1.v2[5] });
    try compressor_4_2(&chip, lp, extract4(&L0.v1, 24),                            .{ L1.v1[6], L1.v2[6] });
    try compressor_4_2(&chip, lp, extract4(&L0.v1, 28),                            .{ L1.v1[7], L1.v2[7] });
    try compressor_3_2_v4(&chip, lp, extract4(&L0.v1, 0), extract4(&L0.v1, 4), extract4(&L0.v1, 8),    .{ L1.v4[0], L1.v8[0] });
    try compressor_3_2_v4(&chip, lp, extract4(&L0.v1, 12), extract4(&L0.v1, 16), extract4(&L0.v1, 20), .{ L1.v4[1], L1.v8[1] });
    try compressor_2_2_v4(&chip, lp, extract4(&L0.v1, 24), extract4(&L0.v1, 28),                       .{ L1.v4[2], L1.v8[2] });

    chip.mc(L1.v32[0].mc()).logic = try lp.logic("&in", .{});

    for (L1.v1) |signal| {
        chip.mc(signal.mc()).func = .{ .d_ff = .{ .clock = .bclock0 } };
    }
    for (L1.v2) |signal| {
        chip.mc(signal.mc()).func = .{ .d_ff = .{ .clock = .bclock0 } };
    }
    for (L1.v4) |signal| {
        chip.mc(signal.mc()).func = .{ .d_ff = .{ .clock = .bclock0 } };
    }
    for (L1.v8) |signal| {
        chip.mc(signal.mc()).func = .{ .d_ff = .{ .clock = .bclock0 } };
    }
    for (L1.v32) |signal| {
        chip.mc(signal.mc()).func = .{ .d_ff = .{ .clock = .bclock0 } };
    }

    // Remaining v1:
    // L1.v1[0] 
    // L1.v1[1]
    // L1.v1[2]
    // L1.v1[3]
    // L1.v1[4]
    // L1.v1[5]
    // L1.v1[6]
    // L1.v1[7]

    // Remaining v2:
    // L1.v2[0]
    // L1.v2[1]
    // L1.v2[2]
    // L1.v2[3]
    // L1.v2[4]
    // L1.v2[5]
    // L1.v2[6]
    // L1.v2[7]

    // Remaining v4:
    // L1.v4[0]
    // L1.v4[1]
    // L1.v4[2]

    // Remaining v8:
    // L1.v8[0]
    // L1.v8[1]
    // L1.v8[2]



    // Layer 2:
    try compressor_4_2(&chip, lp, extract4(&L1.v1, 0), .{ L2.v1[0], L2.v2[0] });
    try compressor_4_2(&chip, lp, extract4(&L1.v1, 4), .{ L2.v1[1], L2.v2[1] });
    try compressor_4_2(&chip, lp, extract4(&L1.v2, 0),           .{ L2.v2[2], L2.v4[0] });
    try compressor_4_2(&chip, lp, extract4(&L1.v2, 4),           .{ L2.v2[3], L2.v4[1] });

    chip.mc(L2.v4[2].mc()).logic = try lp.logic("&a ^ &b", .{
        .a = extract4(&L1.v1, 0),
        .b = extract4(&L1.v1, 4),
    });
    chip.mc(L2.v4[3].mc()).logic = try lp.logic("^L1.v4", .{});

    chip.mc(L2.v8[0].mc()).logic = try lp.logic(
        \\&L1.v1 ^ |{
        \\  L1.v4[0] & L1.v4[1]
        \\  L1.v4[0] & L1.v4[2]
        \\  L1.v4[1] & L1.v4[2]
        \\}
    , .{});

    chip.mc(L2.v16[0].mc()).logic = try lp.logic(
        \\&L1.v1 & |{
        \\  L1.v4[0] & L1.v4[1]
        \\  L1.v4[0] & L1.v4[2]
        \\  L1.v4[1] & L1.v4[2]
        \\}
    , .{});

    try compressor_4_2_d4(&chip, lp, L1.v8, extract4(&L1.v2, 4), .{ L2.v8[1], L2.v16[1] });

    chip.mc(L2.v32[0].mc()).logic = try lp.logic("L1.v32", .{});

    // Remaining v2:
    // L2.v2[0]
    // L2.v2[1]
    // L2.v2[2]
    // L2.v2[3]
    // L2.v1[0..2] == 0b11

    // Remaining v4:
    // L2.v4[0]
    // L2.v4[1]
    // L2.v4[2]
    // L2.v4[3]

    // Remaining v8:
    // L2.v8[0]
    // L2.v8[1]
    // L1.v2[0..4] == 0b1111

    // Remaining v16:
    // L2.v16[0]
    // L2.v16[1]



    // Layer 3:
    try compressor_4_2(&chip, lp, L2.v2,     .{ L3.v2[0], L3.v4[0] });
    try compressor_4_2(&chip, lp, L2.v4,               .{ L3.v4[1], L3.v8[0] });
    try compressor_3_2_c4(&chip, lp, L2.v8, extract4(&L1.v2, 0), &.{ L3.v8[1], L3.v16[0] });
    try compressor_3_2_c4(&chip, lp, L2.v16, extract4(&L2.v4, 0),          &.{ L3.v16[1] });

    // Remaining v4:
    // L3.v4[0]
    // L3.v4[1]
    // { L3.v2  L2.v1 } == 0b111

    // Remaining v8:
    // L3.v8[0]
    // L3.v8[1]
    // L2.v2[0..4] == 0b1111

    // Remaining v16:
    // L3.v16[0]
    // L3.v16[1]



    // Layer 4:
    chip.mc(L4.v8[0].mc()).logic = try lp.logic(
        \\|{
        \\  (L3.v4[0] & L3.v4[1])
        \\  (L3.v4[0] & &{ L3.v2 L2.v1 })
        \\  (L3.v4[1] & &{ L3.v2 L2.v1 })
        \\}
    , .{});
    try compressor_3_2_c4(&chip, lp, L3.v8, extract4(&L2.v2, 0), &.{ L4.v8[1], L4.v16[0] });

    chip.mc(L4.v16[1].mc()).logic = try lp.logic("L3.v16[0] ^ L3.v16[1]", .{});

    // Remaining v8:
    // L4.v8[0]
    // L4.v8[1]

    // Remaining v16:
    // L4.v16[0]
    // L4.v16[0]



    chip.mc(outputs[0].mc()).logic = try lp.logic("L2.v1[0] ^ L2.v1[1]", .{});
    chip.mc(outputs[1].mc()).logic = try lp.logic("L3.v2[0] ^ (L2.v1 == 0b11)", .{});
    chip.mc(outputs[2].mc()).logic = try lp.logic("({L3.v2 L2.v1} == 0b111) ^ (L3.v4[0] ^ L3.v4[1])", .{});
    chip.mc(outputs[3].mc()).logic = try lp.logic("L4.v8[0] ^ L4.v8[1]", .{});
    chip.mc(outputs[4].mc()).logic = try lp.logic("L4.v16[0] ^ L4.v16[1] ^ (L4.v8 == 0b11)", .{});
    chip.mc(outputs[5].mc()).logic = try lp.logic("L2.v32", .{});

    inline for (outputs) |out| {
        chip.mc(out.mc()).output.oe = .goe2;
    }

    return chip;
}

fn compressor_4_2(chip: *Chip, lp: *Chip.Logic_Parser, in: [4]Chip.Signal, out: [2]Chip.Signal) !void {
    chip.mc(out[0].mc()).logic = try lp.logic(
        \\|{
        \\  abc == 3'1
        \\  abc == 3'2
        \\  abc == 3'4
        \\  abc == 3'7
        \\} ^ d
    , .{ .abc = in[0..3], .d = in[3] });

    chip.mc(out[1].mc()).logic = try lp.logic(
        \\!|{
        \\  !b & !c & !d
        \\  !a & !c & !d
        \\  !a & !b & !d
        \\  !a & !b & !c
        \\  a & b & c & d
        \\}
    , .{ .a = in[0], .b = in[1], .c = in[2], .d = in[3] });
}

fn compressor_4_2_d4(chip: *Chip, lp: *Chip.Logic_Parser, abc: [3]Chip.Signal, d: [4]Chip.Signal, out: [2]Chip.Signal) !void {
    chip.mc(out[0].mc()).logic = try lp.logic(
        \\|{
        \\  abc == 3'1
        \\  abc == 3'2
        \\  abc == 3'4
        \\  abc == 3'7
        \\} ^ &d
    , .{ .abc = abc, .d = d });

    chip.mc(out[1].mc()).logic = try lp.logic(
        \\|{
        \\  a & b & !c
        \\  !a & b & c
        \\  !b & c & &d
        \\  b & !c & &d
        \\  a & !c & &d
        \\  a & c & !d[0]
        \\  a & c & !d[1]
        \\  a & c & !d[2]
        \\  a & c & !d[3]
        \\}
    , .{ .a = abc[0], .b = abc[1], .c = abc[2], .d = d });
}

fn compressor_3_2_c4(chip: *Chip, lp: *Chip.Logic_Parser, ab: [2]Chip.Signal, c: [4]Chip.Signal, out: []const Chip.Signal) !void {
    if (out.len >= 1) {
        chip.mc(out[0].mc()).logic = try lp.logic("(a ^ b) ^ &c", .{
            .a = ab[0],
            .b = ab[1],
            .c = c,
        });
    }

    if (out.len >= 2) {
        chip.mc(out[1].mc()).logic = try lp.logic(
            \\|{
            \\  (a & b)
            \\  (a & &c)
            \\  (b & &c)
            \\}
        , .{ .a = ab[0], .b = ab[1], .c = c });
    }
}

fn compressor_3_2_v4(chip: *Chip, lp: *Chip.Logic_Parser, comptime a: [4]Chip.Signal, comptime b: [4]Chip.Signal, comptime c: [4]Chip.Signal, comptime out: [2]Chip.Signal) !void {
    chip.mc(out[0].mc()).logic = try lp.logic(
        \\|{
        \\  (&a & b != 0xF)
        \\  (&b & a != 0xF)
        \\} ^ &c
    , .{ .a = a, .b = b, .c = c });
    
    chip.mc(out[1].mc()).logic = try lp.logic(
        \\|{
        \\  (&a & &b)
        \\  (&a & &c)
        \\  (&b & &c)
        \\}
    , .{ .a = a, .b = b, .c = c });
}

fn compressor_2_2_v4(chip: *Chip, lp: *Chip.Logic_Parser, comptime a: [4]Chip.Signal, comptime b: [4]Chip.Signal, comptime out: [2]Chip.Signal) !void {
    chip.mc(out[0].mc()).logic = try lp.logic("&a ^ &b", .{ .a = a, .b = b });
    chip.mc(out[1].mc()).logic = try lp.logic("&a & &b", .{ .a = a, .b = b });
}

fn extract4(in: []const Chip.Signal, offset: usize) [4]Chip.Signal {
    return in[offset..][0..4].*;
}

pub fn main() !void {
    var arg_iter = try std.process.argsWithAllocator(std.heap.smp_allocator);
    _ = arg_iter.next(); // executable
    const jed_path = arg_iter.next().?;
    const svf_path = arg_iter.next().?;
    const html_path = arg_iter.next().?;
    defer arg_iter.deinit();

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    var names: Chip.Names = .init(std.heap.smp_allocator);
    defer names.deinit();

    var lp: Chip.Logic_Parser = .{
        .gpa = std.heap.smp_allocator,
        .arena = .init(std.heap.page_allocator),
        .names = &names,
    };

    const chip = try configure(&names, &lp);

    try names.propagate_names(arena.allocator(), &chip);

    const results = try chip.assemble(arena.allocator(), .{});

    try Chip.write_jed_file(results.jedec, jed_path, .{});
    try Chip.write_svf_file(results.jedec, svf_path, .{});
    try Chip.write_report_file(speed_grade, results.jedec, html_path, .{
        .design_name = "popcount",
        .errors = results.errors.items,
        .names = &names,
    });
}

const arch = @import("arch");
const lc4k = @import("lc4k");
const std = @import("std");
