const Chip = lc4k.LC4032ZE_TQFP48;

const clock_pin = Chip.clock_pins[2];
const oe_pin = Chip.pins._41;

const output_pins = [_]Chip.Pin {
    Chip.pins._23,
    Chip.pins._24,
    Chip.pins._26,
    Chip.pins._27,
    Chip.pins._28,
    Chip.pins._31,
    Chip.pins._32,
    Chip.pins._33,
};

fn configure_chip(chip: *Chip) !void {
    chip.* = .{};

    chip.glb[0].shared_pt_enable = comptime oe_pin.when_high().pt();
    chip.goe0.source = .{ .glb_shared_pt_enable = 0 };
    chip.goe0.polarity = .positive;

    inline for (output_pins, 0..) |out, bit| {
        var mc = chip.mc(out.mc());
        mc.func = .{ .t_ff = .{ .clock = .bclock2 }};
        mc.output.oe = .goe0;

        mc.logic = comptime .{ .sum = .{
            .sum = &.{ blk: {
                // Each bit of the counter should toggle when every lower bit is a 1
                var pt = Chip.PT.always();
                var n = 0;
                while (n < bit) : (n += 1) {
                    pt = pt.and_factor(Chip.Signal.mc_fb(output_pins[n].mc()).when_high());
                }
                break :blk pt;
            }},
            .polarity = .positive,
        }};
    }
}


pub fn configure(b: *Board, config: *const Chip) !void {
    const U1 = b.part(LC4032ZE);
    U1.config = config;
    U1.clk[2] = b.net("CLK");
    U1.set_net_by_pin(oe_pin, b.net("OE"));
    U1.set_bus_by_pins(&output_pins, &b.bus("OUT", output_pins.len));
}

test {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    var b: zoink.Board = .{ .arena = arena.allocator(), .gpa = std.testing.allocator };
    defer b.deinit();

    var chip: Chip = undefined;
    try configure_chip(&chip);
    try configure(&b, &chip);
    try b.finish_configuration(std.testing.allocator);

    var v = try zoink.Validator.init(&b, .{});
    defer v.deinit();

    const CLK = b.get_net("CLK");
    const OE = b.get_net("OE");
    const OUT = b.get_bus("OUT");

    try v.reset();

    try v.set(OE, .gnd);
    try v.update();

    try v.expect_weak(OUT);

    try v.set(OE, .p3v3);
    try v.update();

    try v.expect_bus(OUT, 0, LVCMOS);

    try v.set(CLK, .p3v3);
    try v.update();

    try v.expect_bus(OUT, 1, LVCMOS);

    try v.set(CLK, .gnd);
    try v.update();

    try v.expect_bus(OUT, 1, LVCMOS);

    try v.set(CLK, .p3v3);
    try v.update();
    try v.set(CLK, .gnd);
    try v.update();

    try v.expect_bus(OUT, 2, LVCMOS);

    try v.set(OE, .gnd);
    try v.update();

    try v.expect_weak(OUT);

    for (0..17) |_| {
        try v.set(CLK, .p3v3);
        try v.update();
        try v.set(CLK, .gnd);
        try v.update();
    }

    try v.expect_weak(OUT);

    try v.set(OE, .p3v3);
    try v.update();

    try v.expect_bus(OUT, 19, LVCMOS);
}

const LC4032ZE = zoink.parts.LC4032ZE_TQFP48_3v3;

const LVCMOS = zoink.Voltage.LVCMOS;
const Board = zoink.Board;

const lc4k = @import("lc4k");
const zoink = @import("zoink");
const std = @import("std");
