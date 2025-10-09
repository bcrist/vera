d: *Drawing,
input_box: *Box,
output_box: *Box,
v_extents: V_Extents,

guard_logic: *Box,
system_bus_interface: *Box,
data_byte_swapper: *Box,
offset_01_joiner: *Box,

int_pending_mux: *Box,
pipe_counter: *Box,

dr_mux: *Box,
dr_ir_to_d: *Box,
d_ir_joiner: *Box,
ir_mux: *Box,
dr_ir_write_control: *Box,

vabor_mux: *Box,
vabor_to_d: *Box,
vabor_write_control: *Box,

d_l_bridge: *Box,

flag_write_control: *Box,
flags_to_l: *Box,

rsn_mux: *Box,
rsn_l_joiner: *Box,
rsn_write_control: *Box,

fwrite_mux: *Box,
fwidth_mux: *Box,
fspace_mux: *Box,

fucs_mux: *Box,
fucs_uca_joiner: *Box,
fucs_l_joiner: *Box,
fucs_write_control: *Box,

status_to_l: *Box,
compute_to_l: *Box,

write_rsn_decoder: *Box,

gpr: *Box,
sr1: *Box,
sr2: *Box,

gpr_addr_joiner: *Box,
sr1_addr_joiner: *Box,
sr2_addr_joiner: *Box,

sr1_mux: *Box,
sr2_mux: *Box,

gpr_write_control: *Box,
sr1_write_control: *Box,
sr2_write_control: *Box,

asn6_joiner: *Box,
asn6_mux: *Box,
asn6_write_control: *Box,

microcode: *Box,

at_primary: *Box,
at_secondary: *Box,

at_primary_updater: *Box,
at_secondary_updater: *Box,

pub fn init(d: *Drawing, input_box: *Box, output_box: *Box) @This() {
    return .{
        .d = d,
        .input_box = input_box,
        .output_box = output_box,
        .v_extents = .{
            .input_min = d.some_y(),
            .input_max = d.some_y(),
            .output_min = d.some_y(),
            .output_max = d.some_y(),
        },

        .guard_logic = d.box(.{ .label = "Guard\nRegisters", .class = "reg" }),
        .system_bus_interface = d.box(.{ .label = "System Bus\nRAM & Devices", .class = "reg" }),
        .data_byte_swapper = d.box(.{}),
        .offset_01_joiner = d.box(.{ .shape = .joiner_h }),

        .int_pending_mux = d.box(.{ .shape = .mux }),
        .pipe_counter = d.box(.{ .label = "Pipeline\nCounter", .class = "reg" }),

        .dr_mux = d.box(.{ .shape = .mux }),
        .dr_ir_to_d = d.box(.{ .label = "DR/IR to D" }),
        .d_ir_joiner = d.box(.{ .shape = .joiner_h }),
        .ir_mux = d.box(.{ .shape = .mux }),
        .dr_ir_write_control = d.box(.{ .label = "DR/IR Write\nControl" }),

        .vabor_mux = d.box(.{ .shape = .mux }),
        .vabor_to_d = d.box(.{ .label = "VABOR to D" }),
        .vabor_write_control = d.box(.{ .label = "VABOR Write\nControl" }),

        .d_l_bridge = d.box(.{ .label = "D/L Bridge" }),

        .flag_write_control = d.box(.{ .label = "Flag/TI\nUpdater" }),
        .flags_to_l = d.box(.{ .label = "Flags to L" }),

        .rsn_mux = d.box(.{ .shape = .mux }),
        .rsn_l_joiner = d.box(.{ .shape = .joiner_h }),
        .rsn_write_control = d.box(.{ .label = "RSN Write\nControl" }),

        .fwrite_mux = d.box(.{ .shape = .mux }),
        .fwidth_mux = d.box(.{ .shape = .mux }),
        .fspace_mux = d.box(.{ .shape = .mux }),

        .fucs_mux = d.box(.{ .shape = .mux }),
        .fucs_uca_joiner = d.box(.{ .shape = .joiner_h }),
        .fucs_l_joiner = d.box(.{ .shape = .joiner_h }),
        .fucs_write_control = d.box(.{ .label = "Fault UC Slot\nWrite Control" }),

        .status_to_l = d.box(.{ .label = "Status to L" }),
        .compute_to_l = d.box(.{ .label = "Compute to L" }),

        .write_rsn_decoder = d.box(.{ .label = "RSN\nDecoder" }),

        .gpr = d.box(.{ .label = "JR/KR", .class = "reg" }),
        .sr1 = d.box(.{ .label = "SR1/SR1B", .class = "reg" }),
        .sr2 = d.box(.{ .label = "SR2/SR2B", .class = "reg" }),

        .gpr_addr_joiner = d.box(.{ .shape = .joiner_h }),
        .sr1_addr_joiner = d.box(.{ .shape = .joiner_h }),
        .sr2_addr_joiner = d.box(.{ .shape = .joiner_h }),

        .sr1_mux = d.box(.{ .shape = .mux }),
        .sr2_mux = d.box(.{ .shape = .mux }),

        .gpr_write_control = d.box(.{ .label = "GPR Write\nControl" }),
        .sr1_write_control = d.box(.{ .label = "SR1 Write\nControl" }),
        .sr2_write_control = d.box(.{ .label = "SR2 Write\nControl" }),

        .asn6_joiner = d.box(.{ .shape = .joiner_h }),
        .asn6_mux = d.box(.{ .shape = .mux }),
        .asn6_write_control = d.box(.{ .label = "ASN6 Write\nControl" }),

        .microcode = d.box(.{ .label = "\u{B5}code" }),

        .at_primary = d.box(.{ .label = "Primary\nAT File", .class = "reg" }),
        .at_secondary = d.box(.{ .label = "Secondary\nAT File", .class = "reg" }),

        .at_primary_updater = d.box(.{ .label = "Primary\nAT Updater" }),
        .at_secondary_updater = d.box(.{ .label = "Secondary\nAT Updater" }),
    };
}

pub fn config(self: *@This()) void {
    const col: [5]zbox.X_Ref = .{
        self.d.some_x(),
        self.d.some_x(),
        self.d.some_x(),
        self.d.some_x(),
        self.d.some_x(),
    };
    _ = col[0].attach_to_offset(self.input_box.right(), 180);
    _ = col[1].attach_to_offset(col[0], 100);
    _ = col[2].attach_to_offset(col[1], 160);
    _ = col[3].attach_to_offset(col[2], 160);
    _ = col[4].attach_to_offset(col[3], 220);
    _ = self.output_box.left().attach_to_offset(col[4], 150);


    _ = self.guard_logic.width(160).x().attach_to(col[0]);
    _ = self.system_bus_interface.width(160).x().attach_to(col[2]);
    _ = self.offset_01_joiner.x().attach_to(col[0]);
    _ = self.data_byte_swapper.width(150).x().attach_to(col[2]);
    _ = self.int_pending_mux.x().attach_to(col[4]);
    _ = self.pipe_counter.width(140).x().attach_to_offset(col[3], 100);
    _ = self.dr_mux.x().attach_to(col[3]);
    _ = self.ir_mux.x().attach_to(col[4]);
    _ = self.d_ir_joiner.x().attach_between(col[3], col[4], 0.5);
    _ = self.dr_ir_to_d.width(150).x().attach_to(col[1]);
    _ = self.dr_ir_write_control.width(150).x().attach_to(col[1]);

    _ = self.vabor_mux.x().attach_to(col[3]);
    _ = self.vabor_to_d.width(150).x().attach_to(col[1]);
    _ = self.vabor_write_control.width(150).x().attach_to(col[1]);

    _ = self.d_l_bridge.width(150).x().attach_to(col[2]);

    _ = self.flag_write_control.width(150).x().attach_to_offset(col[3], 100);
    _ = self.flags_to_l.width(150).x().attach_to(col[1]);

    _ = self.rsn_mux.x().attach_to_offset(col[3], 100);
    _ = self.rsn_l_joiner.x().attach_to_offset(col[3], 0);
    _ = self.rsn_write_control.width(150).x().attach_to(col[1]);

    _ = self.fwrite_mux.x().attach_to_offset(col[3], 60);
    _ = self.fwidth_mux.x().attach_to_offset(col[3], 120);
    _ = self.fspace_mux.x().attach_to_offset(col[3], 180);

    _ = self.fucs_mux.x().attach_to_offset(col[3], 60);
    _ = self.fucs_uca_joiner.x().attach_to_offset(col[2], 50);
    _ = self.fucs_l_joiner.x().attach_to_offset(col[2], 50);
    _ = self.fucs_write_control.width(150).x().attach_to(col[1]);

    _ = self.status_to_l.width(150).x().attach_to(col[1]);
    _ = self.compute_to_l.width(150).x().attach_to(col[1]);

    const sram_width = 150;
    _ = self.write_rsn_decoder.width(100).x().attach_to(col[1]);
    _ = self.gpr.width(sram_width).x().attach_to(col[4]);
    _ = self.sr1.width(sram_width).x().attach_to(col[4]);
    _ = self.sr2.width(sram_width).x().attach_to(col[4]);

    _ = self.gpr_addr_joiner.x().attach_to(col[3]);
    _ = self.sr1_addr_joiner.x().attach_to(col[3]);
    _ = self.sr2_addr_joiner.x().attach_to(col[3]);

    _ = self.sr1_mux.x().attach_to(col[3]);
    _ = self.sr2_mux.x().attach_to(col[3]);

    _ = self.gpr_write_control.width(150).x().attach_to(col[1]);
    _ = self.sr1_write_control.width(150).x().attach_to(col[1]);
    _ = self.sr2_write_control.width(150).x().attach_to(col[1]);

    _ = self.asn6_joiner.x().attach_between(col[3], col[4], 0.5);
    _ = self.asn6_mux.x().attach_to(col[4]);
    _ = self.asn6_write_control.width(150).x().attach_to(col[1]);

    _ = self.microcode.width(200).x().attach_to(col[3]);

    _ = self.at_primary.width(150).x().attach_to(col[4]);
    _ = self.at_secondary.width(150).x().attach_to(col[4]);

    _ = self.at_primary_updater.width(150).x().attach_to(col[3]);
    _ = self.at_secondary_updater.width(150).x().attach_to(col[3]);


    _ = self.guard_logic.height(120).top().anchor_at(-25);
    _ = self.system_bus_interface.height(230).top().attach_to_offset(self.guard_logic.bottom(), 0);
    _ = self.offset_01_joiner.y().attach_to(self.data_byte_swapper.y());
    _ = self.data_byte_swapper.height(80).top().attach_to_offset(self.system_bus_interface.bottom(), 60);
    _ = self.int_pending_mux.y().attach_to(self.system_bus_interface.y());
    _ = self.pipe_counter.height(80).top().attach_to_offset(self.int_pending_mux.bottom(), 50);
    _ = self.dr_mux.top().attach_to_offset(self.data_byte_swapper.bottom(), 25);
    _ = self.ir_mux.top().attach_to_offset(self.dr_mux.bottom(), 50);
    _ = self.dr_ir_to_d.height(100).y().attach_between(self.dr_mux.bottom(), self.ir_mux.top(), 0.5);
    _ = self.dr_ir_write_control.height(60).top().attach_to_offset(self.dr_ir_to_d.bottom(), 60);

    _ = self.vabor_mux.top().attach_to_offset(self.dr_ir_write_control.bottom(), 25);
    _ = self.vabor_to_d.height(60).top().attach_to_offset(self.vabor_mux.bottom(), -10);
    _ = self.vabor_write_control.height(60).top().attach_to_offset(self.vabor_to_d.bottom(), 25);

    _ = self.d_l_bridge.height(80).top().attach_to_offset(self.vabor_write_control.bottom(), 50);

    _ = self.flag_write_control.height(560).top().attach_to_offset(self.d_l_bridge.bottom(), 50);
    _ = self.flags_to_l.height(220).top().attach_to_offset(self.flag_write_control.bottom(), 0);


    _ = self.rsn_mux.y().attach_to_offset(self.flags_to_l.bottom(), 50);
    _ = self.rsn_write_control.height(60).top().attach_to_offset(self.rsn_mux.bottom(), 0);

    _ = self.fwrite_mux.top().attach_to_offset(self.rsn_write_control.bottom(), 25);
    _ = self.fwidth_mux.y().attach_to_offset(self.fwrite_mux.y(), 75);
    _ = self.fspace_mux.y().attach_to_offset(self.fwidth_mux.y(), 75);

    _ = self.fucs_mux.top().attach_to_offset(self.fspace_mux.bottom(), 75);
    _ = self.fucs_write_control.height(60).top().attach_to_offset(self.fucs_mux.bottom(), 0);

    _ = self.status_to_l.height(160).top().attach_to_offset(self.fucs_write_control.bottom(), 25);
    _ = self.compute_to_l.height(180).top().attach_to_offset(self.status_to_l.bottom(), 75);
    
    _ = self.write_rsn_decoder.height(100).top().attach_to_offset(self.compute_to_l.bottom(), 50);

    _ = self.gpr.height(160).top().attach_to_offset(self.write_rsn_decoder.bottom(), 50);
    _ = self.sr1.height(200).top().attach_to_offset(self.gpr.bottom(), 50);
    _ = self.sr2.height(200).top().attach_to_offset(self.sr1.bottom(), 50);

    _ = self.gpr_addr_joiner.y().attach_to_offset(self.gpr.top(), 20);
    _ = self.sr1_addr_joiner.y().attach_to_offset(self.sr1.top(), 20);
    _ = self.sr2_addr_joiner.y().attach_to_offset(self.sr2.top(), 20);

    _ = self.sr1_mux.y().attach_to_offset(self.sr1.bottom(), -20);
    _ = self.sr2_mux.y().attach_to_offset(self.sr2.bottom(), -20);

    _ = self.gpr_write_control.height(60).y().attach_to(self.gpr.y());
    _ = self.sr1_write_control.height(60).y().attach_to(self.sr1.y());
    _ = self.sr2_write_control.height(60).y().attach_to(self.sr2.y());

    _ = self.asn6_mux.top().attach_to_offset(self.sr2.bottom(), 50);
    _ = self.asn6_write_control.height(80).top().attach_to_offset(self.asn6_mux.bottom(), 5);

    _ = self.microcode.height(120).y().attach_between(self.status_to_l.bottom(), self.compute_to_l.top(), 0.5);

    _ = self.at_primary.height(180).top().attach_to_offset(self.asn6_write_control.bottom(), 50);
    _ = self.at_secondary.height(180).top().attach_to_offset(self.at_primary.bottom(), 50);

    _ = self.at_primary_updater.height(180).y().attach_to(self.at_primary.y());
    _ = self.at_secondary_updater.height(180).y().attach_to(self.at_secondary.y());

    // wires
    ///////////////////////////////////////////////////////////////////
    
    // guard registers
    const guard_frame_wire = self.guard_logic.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.addr.Frame) })
        .bit_mark_at(0.2)
        .label("FRAME", .{})
        .end_at_mutable_point(self.input_box.right_side(""));
    _ = self.guard_logic.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.addr.Frame.Offset) })
        .bit_mark_at(0.2)
        .label("OFFSET", .{})
        .end_at_mutable_point(self.input_box.right_side(""));
    _ = self.guard_logic.left_side("")
        .wire_h(.{})
        .label("WRITE", .{})
        .end_at_mutable_point(self.input_box.right_side(""));
    _ = self.guard_logic.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.Special_Op), .class = "control" })
        .bit_mark_at(0.2)
        .label("SPECIAL", .{})
        .end_at_mutable_point(self.input_box.right_side(""));
    _ = self.guard_logic.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.Pipeline) })
        .bit_mark_at(0.2)
        .label("PIPE", .{})
        .end_at_mutable_point(self.input_box.right_side(""));

    _ = self.guard_logic.right_side("");
    const guard_fail_wire = self.guard_logic.right_side("")
        .wire_h(.{})
        .label("GUARD_FAIL", .{});
    _ = guard_fail_wire
        .turn_at_offset(self.system_bus_interface.right(), 25)
        .turn_and_end_at(self.flag_write_control.left_side(""))
        .label("GUARD_FAIL", .{ .alignment = .right })
        ;
    _ = self.system_bus_interface.left_side("")
        .wire_h(.{ .dir = .junction_end })
        .length(-25)
        .turn()
        .end_at(guard_fail_wire.y());


    // system interface
    _ = self.system_bus_interface.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.addr.Frame) })
        .bit_mark()
        .label("FRAME", .{})
        .end_at_mutable_point(self.input_box.right_side(""));

    _ = self.system_bus_interface.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.addr.Frame.Word_Offset) })
        .bit_mark()
        .label("AA", .{})
        .end_at_mutable_point(self.input_box.right_side(""));

    _ = self.system_bus_interface.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.addr.Frame.Word_Offset) })
        .bit_mark()
        .label("AB", .{})
        .end_at_mutable_point(self.input_box.right_side(""));

    _ = self.system_bus_interface.left_side("")
        .wire_h(.{})
        .label("LBA", .{})
        .end_at_mutable_point(self.input_box.right_side(""));
    _ = self.system_bus_interface.left_side("")
        .wire_h(.{})
        .label("UBA", .{})
        .end_at_mutable_point(self.input_box.right_side(""));

    _ = self.system_bus_interface.left_side("")
        .wire_h(.{})
        .label("LBB", .{})
        .end_at_mutable_point(self.input_box.right_side(""));
    _ = self.system_bus_interface.left_side("")
        .wire_h(.{})
        .label("UBB", .{})
        .end_at_mutable_point(self.input_box.right_side(""));

    _ = self.system_bus_interface.left_side("")
        .wire_h(.{})
        .label("READ", .{})
        .end_at_mutable_point(self.input_box.right_side(""));
    _ = self.system_bus_interface.left_side("")
        .wire_h(.{})
        .label("WRITE", .{})
        .end_at_mutable_point(self.input_box.right_side(""));

    const reset_wire = self.system_bus_interface.right_side_upper("")
        .wire_h(.{})
        .label("RESET", .{ .alignment = .right })
        .end_at_mutable_point(self.output_box.left_side(""));

    inline for (0..arch.Pipeline.count) |p| {
        _ = self.int_pending_mux.left_side(std.fmt.comptimePrint("{b:0>2}", .{ p }))
            .wire_h(.{})
            .fmt_label("INT_PENDING_{d}", .{ p }, .{ .alignment = .right })
            .end_at_mutable_point(self.system_bus_interface.right_side(""));
    }
    _ = self.int_pending_mux.right_side("")
        .wire_h(.{})
        .label("INT_PENDING", .{ .alignment = .right })
        .end_at_mutable_point(self.output_box.left_side(""));

    _ = self.int_pending_mux.bottom_side("")
        .wire_v(.{ .bits = @bitSizeOf(arch.Pipeline), .dir = .junction_end })
        .bit_mark()
        .end_at(self.pipe_counter.y());

    _ = self.pipe_counter.right_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.Pipeline) })
        .label("PIPE", .{ .alignment = .right })
        .bit_mark_at(0.5)
        .end_at_mutable_point(self.output_box.left_side(""));


    // data bus byte swapper
    _ = self.system_bus_interface.bottom_side("")
        .wire_v(.{ .bits = @bitSizeOf(arch.bus.DA) })
        .label("DA", .{})
        .bit_mark()
        .end_at_mutable_point(self.data_byte_swapper.top_side(""));
    _ = self.system_bus_interface.bottom_side("");
    _ = self.system_bus_interface.bottom_side("");
    _ = self.system_bus_interface.bottom_side("")
        .wire_v(.{ .bits = @bitSizeOf(arch.bus.DB) })
        .label("DB", .{})
        .bit_mark()
        .end_at_mutable_point(self.data_byte_swapper.top_side(""));

    _ = self.offset_01_joiner.right_side("")
        .wire_h(.{})
        .label("OFFSET[1]", .{})
        .end_at(self.data_byte_swapper.left_side("Swap DA/DB").x());
    _ = self.offset_01_joiner.right_side("")
        .wire_h(.{})
        .label("OFFSET[0]", .{})
        .end_at(self.data_byte_swapper.left_side("Shift 1 byte").x());
    _ = self.offset_01_joiner.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.addr.Frame.Offset) })
        .label("OFFSET", .{})
        .bit_mark()
        .end_at_mutable_point(self.input_box.right_side(""));

    // D bus
    const d_bus = self.data_byte_swapper.bottom_side("")
        .wire_v(.{ .bits = @bitSizeOf(arch.bus.D) })
        .label("D", .{ .alignment = .right })
        .continue_at(self.dr_ir_to_d.top())
        .label("D", .{})
        .end_at(self.d_l_bridge.top());

    // dr_mux
    const dr_in_wire = self.dr_mux.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.reg.DR), .class = "reg" })
        .label("DR", .{})
        .bit_mark()
        .end_at_mutable_point(self.input_box.right_side(""));
    _ = self.dr_mux.left_side("");
    _ = self.dr_mux.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.reg.DR), .dir = .junction_end })
        .bit_mark()
        .end_at(d_bus.x());
    const dr_out_wire = self.dr_mux.right_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.reg.DR), .class = "reg" })
        .label("DR", .{ .alignment = .right })
        .bit_mark()
        .end_at_mutable_point(self.output_box.left_side(""));


    // ir_mux
    const ir_mux_wire = self.ir_mux.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.reg.IR), .class = "reg" })
        .bit_mark()
        .label("DR[15:0]", .{})
        .label("", .{})
        .end_at(self.d_ir_joiner.right());

    _ = self.ir_mux.left_side("");
    const ir_in_wire = self.ir_mux.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.reg.IR), .class = "reg" })
        .bit_mark_at(0.6)
        .label("IR", .{})
        .end_at_mutable_point(self.input_box.right_side(""));

    _ = self.d_ir_joiner.y().attach_to(ir_mux_wire.y());
    _ = self.d_ir_joiner.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.reg.DR), .class = "reg", .dir = .junction_end })
        .length(-50)
        .turn()
        .end_at(dr_out_wire.y());


    _ = self.ir_mux.right_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.reg.IR), .class = "reg" })
        .label("IR", .{ .alignment = .right })
        .bit_mark()
        .end_at_mutable_point(self.output_box.left_side(""));

    // dr/ir to d
    _ = self.dr_ir_to_d.right_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.bus.D), .dir = .junction_end })
        .bit_mark()
        .end_at(d_bus.x());

    _ = self.dr_ir_to_d.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.reg.DR), .class = "reg", .dir = .junction_end })
        .bit_mark()
        .length(-50)
        .turn()
        .end_at(dr_in_wire.y());

    _ = self.dr_ir_to_d.left_side("")
        .wire_h(.{ .class = "control" })
        .label("DRW", .{})
        .end_at_mutable_point(self.input_box.right_side(""));

    _ = self.dr_ir_to_d.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.bus.D.Source), .class = "control" })
        .bit_mark()
        .label("DSRC", .{})
        .end_at_mutable_point(self.input_box.right_side(""));

    _ = self.dr_ir_to_d.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.reg.IR), .class = "reg", .dir = .junction_end })
        .bit_mark()
        .length(-50)
        .turn()
        .end_at(ir_in_wire.y());

    // DR/IR write control
    _ = self.dr_ir_write_control.left_side("")
        .wire_h(.{ .class = "control" })
        .label("DRW", .{})
        .end_at_mutable_point(self.input_box.right_side(""));
    _ = self.dr_ir_write_control.left_side("")
        .wire_h(.{ .class = "control" })
        .label("IRW", .{})
        .end_at_mutable_point(self.input_box.right_side(""));
    _ = self.dr_ir_write_control.left_side("")
        .wire_h(.{})
        .label("ANY_FAULT", .{})
        .end_at_mutable_point(self.input_box.right_side(""));
    _ = self.dr_ir_write_control.right_side("")
        .wire_h(.{})
        .turn_and_end_at(self.dr_mux.bottom_side(""));
    _ = self.dr_ir_write_control.right_side("")
        .wire_h(.{})
        .turn_and_end_at(self.ir_mux.bottom_side(""));

    // vabor mux
    _ = self.vabor_mux.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.addr.Virtual) })
        .label("VA", .{})
        .bit_mark()
        .end_at_mutable_point(self.input_box.right_side(""));
    const vabor_in_wire = self.vabor_mux.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.addr.Virtual), .class = "reg" })
        .label("VABOR", .{})
        .bit_mark()
        .end_at_mutable_point(self.input_box.right_side(""));
    _ = self.vabor_mux.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.addr.Virtual), .dir = .junction_end })
        .bit_mark()
        .end_at(d_bus.x());
    _ = self.vabor_mux.right_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.addr.Virtual), .class = "reg" })
        .label("VABOR", .{ .alignment = .right })
        .bit_mark()
        .end_at_mutable_point(self.output_box.left_side(""));

    // vabor to d
    _ = self.vabor_to_d.right_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.bus.D), .dir = .junction_end })
        .bit_mark()
        .end_at(d_bus.x());
    _ = self.vabor_to_d.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.addr.Virtual), .class = "reg", .dir = .junction_end })
        .bit_mark()
        .length(-50)
        .turn()
        .end_at(vabor_in_wire.y());
    _ = self.vabor_to_d.left_side("")
        .wire_h(.{ .class = "reg" })
        .label("FLAG_BUS_OVERRIDE", .{})
        .end_at_mutable_point(self.input_box.right_side(""));
    _ = self.vabor_to_d.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.bus.D.Source), .class = "control" })
        .label("DSRC", .{})
        .bit_mark()
        .end_at_mutable_point(self.input_box.right_side(""));

    // vabor write control
    _ = self.vabor_write_control.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.Special_Op), .class = "control" })
        .label("SPECIAL", .{})
        .end_at_mutable_point(self.input_box.right_side(""));
    _ = self.vabor_write_control.left_side("")
        .wire_h(.{})
        .label("ANY_FAULT", .{})
        .end_at_mutable_point(self.input_box.right_side(""));
    _ = self.vabor_write_control.right_side("")
        .wire_h(.{ .bits = 2 })
        .turn_and_end_at(self.vabor_mux.bottom_side(""))
        .bit_mark();


    _ = self.d_l_bridge.top().offset(-40).intersection_with(d_bus.x())
        .wire_h(.{ .bits = @bitSizeOf(arch.bus.D), .dir = .junction_begin })
        .label("LAST_D", .{ .alignment = .right })
        .bit_mark()
        .end_at_mutable_point(self.output_box.left_side(""));

    // D/L Bridge
    _ = self.d_l_bridge.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.bus.D.Source), .class = "control" })
        .label("DSRC", .{})
        .bit_mark()
        .end_at_mutable_point(self.input_box.right_side(""));
    _ = self.d_l_bridge.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.bus.L.Source), .class = "control" })
        .label("LSRC", .{})
        .bit_mark()
        .end_at_mutable_point(self.input_box.right_side(""));
    _ = self.d_l_bridge.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.compute.Unit), .class = "control" })
        .label("UNIT", .{})
        .bit_mark()
        .end_at_mutable_point(self.input_box.right_side(""));

    // L bus
    const l_bus = self.d_l_bridge.bottom_side("")
        .wire_v(.{ .bits = @bitSizeOf(arch.bus.L) })
        .label("L", .{ .alignment = .right })
        .continue_at(self.compute_to_l.top())
        .label("L", .{});


    // flags write control
    _ = self.flag_write_control.right_side("").wire_h(.{ .class = "reg" }).label("FLAG_Z", .{ .alignment = .right }).end_at_mutable_point(self.output_box.left_side(""));
    _ = self.flag_write_control.right_side("").wire_h(.{ .class = "reg" }).label("FLAG_N", .{ .alignment = .right }).end_at_mutable_point(self.output_box.left_side(""));
    _ = self.flag_write_control.right_side("").wire_h(.{ .class = "reg" }).label("FLAG_C", .{ .alignment = .right }).end_at_mutable_point(self.output_box.left_side(""));
    _ = self.flag_write_control.right_side("").wire_h(.{ .class = "reg" }).label("FLAG_V", .{ .alignment = .right }).end_at_mutable_point(self.output_box.left_side(""));
    _ = self.flag_write_control.right_side("").wire_h(.{ .class = "reg" }).label("FLAG_AT_ENABLE", .{ .alignment = .right }).end_at_mutable_point(self.output_box.left_side(""));
    _ = self.flag_write_control.right_side("").wire_h(.{ .class = "reg" }).label("FLAG_BUS_OVERRIDE", .{ .alignment = .right }).end_at_mutable_point(self.output_box.left_side(""));
    _ = self.flag_write_control.right_side("").wire_h(.{ .class = "reg" }).label("FLAG_SUPER", .{ .alignment = .right }).end_at_mutable_point(self.output_box.left_side(""));
    _ = self.flag_write_control.right_side("").wire_h(.{ .class = "reg" }).label("FLAG_AT_SUPER", .{ .alignment = .right }).end_at_mutable_point(self.output_box.left_side(""));

    _ = self.flag_write_control.right_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.reg.gpr.Write_Index), .class = "reg" })
        .bit_mark()
        .label("TI", .{ .alignment = .right })
        .end_at_mutable_point(self.output_box.left_side(""));


    _ = self.flag_write_control.left_side("").wire_h(.{ .class = "reg" }).label("FLAG_Z", .{}).end_at_mutable_point(self.input_box.right_side(""));
    _ = self.flag_write_control.left_side("").wire_h(.{ .class = "reg" }).label("FLAG_N", .{}).end_at_mutable_point(self.input_box.right_side(""));
    _ = self.flag_write_control.left_side("").wire_h(.{ .class = "reg" }).label("FLAG_C", .{}).end_at_mutable_point(self.input_box.right_side(""));
    _ = self.flag_write_control.left_side("").wire_h(.{ .class = "reg" }).label("FLAG_V", .{}).end_at_mutable_point(self.input_box.right_side(""));
    _ = self.flag_write_control.left_side("").wire_h(.{ .class = "reg" }).label("FLAG_AT_ENABLE", .{}).end_at_mutable_point(self.input_box.right_side(""));
    _ = self.flag_write_control.left_side("").wire_h(.{ .class = "reg" }).label("FLAG_BUS_OVERRIDE", .{}).end_at_mutable_point(self.input_box.right_side(""));
    _ = self.flag_write_control.left_side("").wire_h(.{ .class = "reg" }).label("FLAG_SUPER", .{}).end_at_mutable_point(self.input_box.right_side(""));
    _ = self.flag_write_control.left_side("").wire_h(.{ .class = "reg" }).label("FLAG_AT_SUPER", .{}).end_at_mutable_point(self.input_box.right_side(""));

    _ = self.flag_write_control.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.reg.gpr.Write_Index), .class = "reg" })
        .bit_mark()
        .label("TI", .{})
        .end_at_mutable_point(self.input_box.right_side(""));

    _ = self.flag_write_control.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.reg.Flags.Op), .class = "control" })
        .bit_mark()
        .label("FLAG_OP", .{})
        .end_at_mutable_point(self.input_box.right_side(""));

    _ = self.flag_write_control.left_side("")
        .wire_h(.{ .class = "control" })
        .label("TIW", .{})
        .end_at_mutable_point(self.input_box.right_side(""));

    _ = self.flag_write_control.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.compute.Unit), .class = "control" })
        .bit_mark()
        .label("UNIT", .{})
        .end_at_mutable_point(self.input_box.right_side(""));
    
    _ = self.flag_write_control.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.bus.D.Source), .class = "control" })
        .bit_mark()
        .label("DSRC", .{})
        .end_at_mutable_point(self.input_box.right_side(""));

    _ = self.flag_write_control.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.microcode.Constant), .class = "control" })
        .bit_mark()
        .label("CONSTANT", .{})
        .end_at_mutable_point(self.input_box.right_side(""));

    _ = self.flag_write_control.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.Sequencer_Op), .class = "control" })
        .bit_mark()
        .label("SEQ_OP", .{})
        .end_at_mutable_point(self.input_box.right_side(""));
    _ = self.flag_write_control.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.addr.translation.Op), .class = "control" })
        .bit_mark()
        .label("AT_OP", .{})
        .end_at_mutable_point(self.input_box.right_side(""));

    _ = self.flag_write_control.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.reg.gpr.Write_Index) })
        .bit_mark()
        .label("WI", .{})
        .end_at_mutable_point(self.input_box.right_side(""));

    _ = self.flag_write_control.left_side("")
        .wire_h(.{})
        .label("ANY_FAULT", .{})
        .end_at_mutable_point(self.input_box.right_side(""));

    _ = self.flag_write_control.left_side("")
        .wire_h(.{})
        .label("ALU_C", .{})
        .end_at_mutable_point(self.input_box.right_side(""));
    _ = self.flag_write_control.left_side("")
        .wire_h(.{})
        .label("ALU_V", .{})
        .end_at_mutable_point(self.input_box.right_side(""));
    _ = self.flag_write_control.left_side("")
        .wire_h(.{})
        .label("MULT_V", .{})
        .end_at_mutable_point(self.input_box.right_side(""));
    _ = self.flag_write_control.left_side("")
        .wire_h(.{})
        .label("SHIFT_C", .{})
        .end_at_mutable_point(self.input_box.right_side(""));
    _ = self.flag_write_control.left_side("")
        .wire_h(.{})
        .label("SHIFT_V", .{})
        .end_at_mutable_point(self.input_box.right_side(""));
    _ = self.flag_write_control.left_side("")
        .wire_h(.{})
        .label("EXT_V", .{})
        .end_at_mutable_point(self.input_box.right_side(""));
    _ = self.flag_write_control.left_side("")
        .wire_h(.{})
        .label("AT_ADD_SUPER", .{})
        .end_at_mutable_point(self.input_box.right_side(""));
    _ = self.flag_write_control.left_side("")
        .wire_h(.{})
        .label("AT_REMOVE_SUPER", .{})
        .end_at_mutable_point(self.input_box.right_side(""));
    _ = self.flag_write_control.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.bus.L), .dir = .junction_end })
        .bit_mark()
        .end_at(l_bus.x());

    // flags to l
    _ = self.flags_to_l.right_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.reg.Status), .dir = .junction_end })
        .bit_mark()
        .end_at(l_bus.x());

    _ = self.flags_to_l.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.bus.L.Source), .class = "control" })
        .label("LSRC", .{})
        .bit_mark()
        .end_at_mutable_point(self.input_box.right_side(""));

    _ = self.flags_to_l.left_side("")
        .wire_h(.{ .class = "reg" })
        .label("FLAG_Z", .{})
        .end_at_mutable_point(self.input_box.right_side(""));

    _ = self.flags_to_l.left_side("")
        .wire_h(.{ .class = "reg" })
        .label("FLAG_N", .{})
        .end_at_mutable_point(self.input_box.right_side(""));

    _ = self.flags_to_l.left_side("")
        .wire_h(.{ .class = "reg" })
        .label("FLAG_C", .{})
        .end_at_mutable_point(self.input_box.right_side(""));

    _ = self.flags_to_l.left_side("")
        .wire_h(.{ .class = "reg" })
        .label("FLAG_V", .{})
        .end_at_mutable_point(self.input_box.right_side(""));

    _ = self.flags_to_l.left_side("")
        .wire_h(.{ .class = "reg" })
        .label("FLAG_AT_ENABLE", .{})
        .end_at_mutable_point(self.input_box.right_side(""));

    _ = self.flags_to_l.left_side("")
        .wire_h(.{ .class = "reg" })
        .label("FLAG_BUS_OVERRIDE", .{})
        .end_at_mutable_point(self.input_box.right_side(""));

    _ = self.flags_to_l.left_side("")
        .wire_h(.{ .class = "reg" })
        .label("FLAG_SUPER", .{})
        .end_at_mutable_point(self.input_box.right_side(""));

    _ = self.flags_to_l.left_side("")
        .wire_h(.{ .class = "reg" })
        .label("FLAG_AT_SUPER", .{})
        .end_at_mutable_point(self.input_box.right_side(""));

    _ = self.flags_to_l.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.reg.gpr.Write_Index), .class = "reg" })
        .label("TI", .{})
        .bit_mark()
        .end_at_mutable_point(self.input_box.right_side(""));

    _ = self.flags_to_l.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.Execution_Mode), .class = "reg" })
        .label("EXEC_MODE", .{})
        .bit_mark()
        .end_at_mutable_point(self.input_box.right_side(""));







    // compute to l
    _ = self.compute_to_l.right_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.bus.D), .dir = .junction_end })
        .bit_mark()
        .end_at(l_bus.x());

    _ = self.compute_to_l.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.compute.Unit), .class = "control" })
        .label("UNIT", .{})
        .bit_mark()
        .end_at_mutable_point(self.input_box.right_side(""));

    _ = self.compute_to_l.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.bus.L.Source), .class = "control" })
        .label("LSRC", .{})
        .bit_mark()
        .end_at_mutable_point(self.input_box.right_side(""));

    _ = self.compute_to_l.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.bus.D) })
        .label("LAST_D", .{})
        .bit_mark()
        .end_at_mutable_point(self.input_box.right_side(""));

    _ = self.compute_to_l.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.bus.L) })
        .label("ALU_RESULT", .{})
        .bit_mark()
        .end_at_mutable_point(self.input_box.right_side(""));
    _ = self.compute_to_l.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.bus.L) })
        .label("MULT_RESULT", .{})
        .bit_mark()
        .end_at_mutable_point(self.input_box.right_side(""));
    _ = self.compute_to_l.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.bus.L) })
        .label("SHIFT_RESULT", .{})
        .bit_mark()
        .end_at_mutable_point(self.input_box.right_side(""));
    _ = self.compute_to_l.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.bus.L) })
        .bit_mark()
        .length(-50)
        .small_box("zx", .left)
        .change_bits(6)
        .bit_mark_at(0.2)
        .label("COUNT_RESULT", .{})
        .end_at_mutable_point(self.input_box.right_side(""));
    _ = self.compute_to_l.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.bus.L) })
        .label("EXT_RESULT", .{})
        .bit_mark()
        .end_at_mutable_point(self.input_box.right_side(""));

    // rsn mux
    _ = self.rsn_mux.right_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.reg.RSN), .class = "reg" })
        .bit_mark()
        .label("RSN", .{ .alignment = .right })
        .end_at_mutable_point(self.output_box.left_side(""));
    _ = self.rsn_mux.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.reg.RSN), .class = "reg" })
        .bit_mark()
        .label("RSN", .{})
        .end_at_mutable_point(self.input_box.right_side(""));
    const rsn_from_l_wire = self.rsn_mux.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.reg.RSN) })
        .bit_mark()
        .fmt_label("L[{d}:{d}]", .{
            @bitOffsetOf(arch.reg.Status, "rsn") + @bitSizeOf(arch.reg.RSN) - 1,
            @bitOffsetOf(arch.reg.Status, "rsn"),
        }, .{})
        .label("", .{})
        .end_at(self.rsn_l_joiner.x());
    _ = self.rsn_l_joiner.y().attach_to(rsn_from_l_wire.y());
    _ = self.rsn_l_joiner.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.bus.L), .dir = .junction_end })
        .bit_mark()
        .end_at(l_bus.x());

    // RSN write control
    _ = self.rsn_write_control.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.Special_Op), .class = "control" })
        .bit_mark()
        .label("SPECIAL", .{})
        .end_at_mutable_point(self.input_box.right_side(""));
    _ = self.rsn_write_control.left_side("")
        .wire_h(.{ })
        .label("ANY_FAULT", .{})
        .end_at_mutable_point(self.input_box.right_side(""));
    _ = self.rsn_write_control.right_side("")
        .wire_h(.{ })
        .turn_and_end_at(self.rsn_mux.bottom_side(""));

    // fwrite/fwidth/fspace muxes
    _ = self.fwrite_mux.right_side("")
        .wire_h(.{ .class = "reg" })
        .label("FAULT_WRITE", .{ .alignment = .right })
        .end_at_mutable_point(self.output_box.left_side(""));
    _ = self.fwrite_mux.left_side("")
        .wire_h(.{ .class = "reg" })
        .label("FAULT_WRITE", .{})
        .end_at_mutable_point(self.input_box.right_side(""));
    _ = self.fwrite_mux.left_side("")
        .wire_h(.{})
        .length(-50)
        .small_box("!=" ++ @tagName(arch.bus.D.Source.system), .left)
        .change_bits(@bitSizeOf(arch.bus.D.Source))
        .change_class("control")
        .label("DSRC", .{})
        .bit_mark()
        .end_at_mutable_point(self.input_box.right_side(""));

    _ = self.fwidth_mux.right_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.bus.D.Width), .class = "reg" })
        .label("FAULT_WIDTH", .{ .alignment = .right })
        .bit_mark()
        .end_at_mutable_point(self.output_box.left_side(""));
    _ = self.fwidth_mux.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.bus.D.Width), .class = "reg" })
        .label("FAULT_WIDTH", .{})
        .bit_mark()
        .end_at_mutable_point(self.input_box.right_side(""));
    _ = self.fwidth_mux.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.bus.D.Width), .class = "control" })
        .label("WIDTH", .{})
        .bit_mark()
        .end_at_mutable_point(self.input_box.right_side(""));

    _ = self.fspace_mux.right_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.bus.D.Width), .class = "reg" })
        .label("FAULT_SPACE", .{ .alignment = .right })
        .bit_mark()
        .end_at_mutable_point(self.output_box.left_side(""));
    _ = self.fspace_mux.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.bus.D.Width), .class = "reg" })
        .label("FAULT_SPACE", .{})
        .bit_mark()
        .end_at_mutable_point(self.input_box.right_side(""));
    _ = self.fspace_mux.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.bus.D.Width), .class = "control" })
        .label("SPACE", .{})
        .bit_mark()
        .end_at_mutable_point(self.input_box.right_side(""));

    const fspace_any_fault_wire = self.fspace_mux.bottom_side("")
        .wire_v(.{})
        .length(35)
        .turn()
        .label("ANY_FAULT", .{})
        .end_at_mutable_point(self.input_box.right_side(""));
    _ = self.fwidth_mux.bottom_side("")
        .wire_v(.{ .dir = .junction_end })
        .end_at(fspace_any_fault_wire.y());
    _ = self.fwrite_mux.bottom_side("")
        .wire_v(.{ .dir = .junction_end })
        .end_at(fspace_any_fault_wire.y());


    // FUCS mux
    _ = self.fucs_mux.right_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.microcode.Slot), .class = "reg" })
        .label("FAULT_UC_SLOT", .{ .alignment = .right })
        .bit_mark()
        .end_at_mutable_point(self.output_box.left_side(""));
    const ucs_uca_wire = self.fucs_mux.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.microcode.Slot) })
        .fmt_label("UCA[{d}:{d}]", .{
            @bitOffsetOf(arch.microcode.Address, "slot") + @bitSizeOf(arch.microcode.Slot) - 1,
            @bitOffsetOf(arch.microcode.Address, "slot"),
        }, .{})
        .label("", .{})
        .bit_mark()
        .end_at(self.fucs_uca_joiner.right());
    _ = self.fucs_uca_joiner.y().attach_to(ucs_uca_wire.y());
    _ = self.fucs_uca_joiner.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.microcode.Address) })
        .label("UCA", .{})
        .bit_mark()
        .end_at_mutable_point(self.input_box.right_side(""));

    _ = self.fucs_mux.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.microcode.Slot), .class = "reg" })
        .label("FAULT_UC_SLOT", .{})
        .bit_mark()
        .end_at_mutable_point(self.input_box.right_side(""));

    const ucs_l_wire = self.fucs_mux.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.microcode.Slot) })
        .fmt_label("L[{d}:{d}]", .{
            @bitOffsetOf(arch.reg.Status, "fucs") + @bitSizeOf(arch.microcode.Slot) - 1,
            @bitOffsetOf(arch.reg.Status, "fucs"),
        }, .{})
        .label("", .{})
        .bit_mark()
        .end_at(self.fucs_l_joiner.right());
    _ = self.fucs_l_joiner.y().attach_to(ucs_l_wire.y());
    _ = self.fucs_l_joiner.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.bus.L), .dir = .junction_end })
        .bit_mark()
        .end_at(l_bus.x());

    // FUCS write control
    _ = self.fucs_write_control.right_side("")
        .wire_h(.{ .bits = 2 })
        .bit_mark()
        .turn_and_end_at(self.fucs_mux.bottom_side(""));

    _ = self.fucs_write_control.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.Special_Op), .class = "control" })
        .label("SPECIAL", .{})
        .bit_mark()
        .end_at_mutable_point(self.input_box.right_side(""));
    _ = self.fucs_write_control.left_side("")
        .wire_h(.{})
        .label("ANY_FAULT", .{})
        .end_at_mutable_point(self.input_box.right_side(""));


    // status to l
    _ = self.status_to_l.right_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.reg.Status), .dir = .junction_end })
        .bit_mark()
        .end_at(l_bus.x());

    _ = self.status_to_l.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.bus.L.Source), .class = "control" })
        .label("LSRC", .{})
        .bit_mark()
        .end_at_mutable_point(self.input_box.right_side(""));

    _ = self.status_to_l.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.reg.RSN), .class = "reg" })
        .label("RSN", .{})
        .bit_mark()
        .end_at_mutable_point(self.input_box.right_side(""));

    _ = self.status_to_l.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.Pipeline) })
        .label("PIPE", .{})
        .bit_mark()
        .end_at_mutable_point(self.input_box.right_side(""));

    _ = self.status_to_l.left_side("")
        .wire_h(.{ .class = "reg" })
        .label("FAULT_WRITE", .{})
        .end_at_mutable_point(self.input_box.right_side(""));

    _ = self.status_to_l.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.bus.D.Width), .class = "reg" })
        .label("FAULT_WIDTH", .{})
        .bit_mark()
        .end_at_mutable_point(self.input_box.right_side(""));

    _ = self.status_to_l.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.addr.Space), .class = "reg" })
        .label("FAULT_SPACE", .{})
        .bit_mark()
        .end_at_mutable_point(self.input_box.right_side(""));

    _ = self.status_to_l.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.bus.D.Width), .class = "reg" })
        .label("FAULT_UC_SLOT", .{})
        .bit_mark()
        .end_at_mutable_point(self.input_box.right_side(""));



    // write_rsn_decoder
    _ = self.write_rsn_decoder.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.reg.RSN), .class = "reg" })
        .label("RSN", .{})
        .bit_mark_at(0.2)
        .end_at_mutable_point(self.input_box.right_side(""));
    _ = self.write_rsn_decoder.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.Pipeline) })
        .label("PIPE", .{})
        .bit_mark_at(0.2)
        .end_at_mutable_point(self.input_box.right_side(""));
    _ = self.write_rsn_decoder.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.Execution_Mode), .class = "reg" })
        .label("EXEC_MODE", .{})
        .bit_mark_at(0.2)
        .end_at_mutable_point(self.input_box.right_side(""));
    _ = self.write_rsn_decoder.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.Special_Op), .class = "control" })
        .label("SPECIAL", .{})
        .bit_mark_at(0.2)
        .end_at_mutable_point(self.input_box.right_side(""));


    _ = self.gpr_addr_joiner.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.reg.gpr.Write_Index) })
        .bit_mark()
        .label("WI", .{})
        .end_at_mutable_point(self.input_box.right_side(""));

    _ = self.sr1_addr_joiner.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.reg.sr1.Index), .class = "control" })
        .bit_mark()
        .label("SR1WI", .{})
        .end_at_mutable_point(self.input_box.right_side(""));

    _ = self.sr2_addr_joiner.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.reg.sr2.Index), .class = "control" })
        .bit_mark()
        .label("SR2WI", .{})
        .end_at_mutable_point(self.input_box.right_side(""));

    const write_rsn_wire = self.write_rsn_decoder.right_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.reg.RSN) })
        .bit_mark()
        .turn_between(col[2], col[3], 0.5);

    _ = write_rsn_wire.turn_and_end_at(self.sr2_addr_joiner.left_side("")).bit_mark();
    _ = self.sr1_addr_joiner.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.reg.RSN), .dir = .junction_end })
        .bit_mark()
        .end_at(write_rsn_wire.x());
    _ = self.gpr_addr_joiner.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.reg.RSN), .dir = .junction_end })
        .bit_mark()
        .end_at(write_rsn_wire.x());

    _ = self.gpr_addr_joiner.right_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.reg.gpr.Write_Index) + @bitSizeOf(arch.reg.RSN) })
        .bit_mark()
        .end_at_mutable_point(self.gpr.left_side("A"));

    _ = self.sr1_addr_joiner.right_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.reg.sr1.Index) + @bitSizeOf(arch.reg.RSN) })
        .bit_mark()
        .end_at_mutable_point(self.sr1.left_side("A"));

    _ = self.sr2_addr_joiner.right_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.reg.sr2.Index) + @bitSizeOf(arch.reg.RSN) })
        .bit_mark()
        .end_at_mutable_point(self.sr2.left_side("A"));


    _ = self.gpr_write_control.left_side("")
        .wire_h(.{ .class = "control" })
        .label("GPRW", .{})
        .end_at_mutable_point(self.input_box.right_side(""));
    _ = self.gpr_write_control.left_side("")
        .wire_h(.{})
        .label("ANY_FAULT", .{})
        .end_at_mutable_point(self.input_box.right_side(""));
    _ = self.gpr_write_control.right_side("")
        .wire_h(.{})
        .end_at_mutable_point(self.gpr.left_side("WE"));

    _ = self.sr1_write_control.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.reg.sr.Write_Source), .class = "control" })
        .label("SR1WSRC", .{})
        .bit_mark()
        .end_at_mutable_point(self.input_box.right_side(""));
    _ = self.sr1_write_control.left_side("")
        .wire_h(.{})
        .label("ANY_FAULT", .{})
        .end_at_mutable_point(self.input_box.right_side(""));
    _ = self.sr1_write_control.right_side("")
        .wire_h(.{})
        .end_at_mutable_point(self.sr1.left_side("WE"));

    _ = self.sr2_write_control.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.reg.sr.Write_Source), .class = "control" })
        .label("SR2WSRC", .{})
        .bit_mark()
        .end_at_mutable_point(self.input_box.right_side(""));
    _ = self.sr2_write_control.left_side("")
        .wire_h(.{})
        .label("ANY_FAULT", .{})
        .end_at_mutable_point(self.input_box.right_side(""));
    _ = self.sr2_write_control.right_side("")
        .wire_h(.{})
        .end_at_mutable_point(self.sr2.left_side("WE"));
    

    _ = self.gpr.bottom().offset(-25).intersection_with(l_bus.x())
        .wire_h(.{ .bits = @bitSizeOf(arch.bus.L), .dir = .junction_begin })
        .bit_mark()
        .end_at_mutable_point(self.gpr.left_side("D"));


    _ = self.sr1_mux.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.bus.L), .dir = .junction_end })
        .bit_mark_at(0.75)
        .end_at(l_bus.x());
    _ = self.sr1_mux.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.reg.sr1.Value) })
        .label("SR1D", .{})
        .bit_mark()
        .end_at_mutable_point(self.input_box.right_side(""));
    _ = self.sr1_mux.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.addr.Virtual) })
        .label("VA", .{})
        .bit_mark()
        .end_at_mutable_point(self.input_box.right_side(""));
    _ = self.sr1_mux.right_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.reg.sr1.Value) })
        .bit_mark()
        .end_at_mutable_point(self.sr1.left_side("D"));
    _ = self.sr1_mux.top_side("")
        .wire_v(.{ .bits = 2 })
        .bit_mark()
        .turn_and_end_at(self.sr1_write_control.right_side(""));


    _ = self.sr2_mux.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.bus.L), .dir = .junction_end })
        .bit_mark_at(0.75)
        .end_at(l_bus.x());
    _ = self.sr2_mux.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.reg.sr2.Value) })
        .label("SR2D", .{})
        .bit_mark()
        .end_at_mutable_point(self.input_box.right_side(""));
    _ = self.sr2_mux.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.addr.Virtual) })
        .label("VA", .{})
        .bit_mark()
        .end_at_mutable_point(self.input_box.right_side(""));
    const sr2_data_wire = self.sr2_mux.right_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.reg.sr2.Value) })
        .bit_mark()
        .end_at_mutable_point(self.sr2.left_side("D"));
    _ = self.sr2_mux.top_side("")
        .wire_v(.{ .bits = 2 })
        .bit_mark()
        .turn_and_end_at(self.sr2_write_control.right_side(""));


    const asn6_output_wire = self.asn6_mux.right_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.addr.translation.Entry.ASN6), .class = "reg" })
        .bit_mark()
        .label("ASN6", .{ .alignment = .right })
        .end_at_mutable_point(self.output_box.left_side(""));
    const asn6_write_data_wire = self.asn6_mux.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.addr.translation.Entry.ASN6) })
        .bit_mark()
        .fmt_label("[{d}:0]", .{
            @bitSizeOf(arch.addr.translation.Entry.ASN6) - 1,
        }, .{})
        .label("", .{})
        .end_at(self.asn6_joiner.right());
    _ = self.asn6_mux.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.addr.translation.Entry.ASN6), .class = "reg" })
        .bit_mark()
        .label("ASN6", .{})
        .end_at_mutable_point(self.input_box.right_side(""));

    _ = self.asn6_joiner.y().attach_to(asn6_write_data_wire.y());
    _ = self.asn6_joiner.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.reg.sr2.Value), .dir = .junction_end })
        .bit_mark()
        .turn_between(self.sr2_mux.right(), self.sr2.left(), 0.25)
        .end_at(sr2_data_wire.y());

    _ = self.asn6_write_control.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.reg.sr2.Index), .class = "control" })
        .bit_mark()
        .label("SR2WI", .{})
        .end_at_mutable_point(self.input_box.right_side(""));
    _ = self.asn6_write_control.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.Special_Op), .class = "control" })
        .bit_mark()
        .label("SPECIAL", .{})
        .end_at_mutable_point(self.input_box.right_side(""));
    _ = self.asn6_write_control.left_side("")
        .wire_h(.{})
        .label("ANY_FAULT", .{})
        .end_at_mutable_point(self.input_box.right_side(""));
    _ = self.asn6_write_control.right_side("")
        .wire_h(.{})
        .turn_and_end_at(self.asn6_mux.bottom_side(""));



    // at primary updater
    _ = self.at_primary_updater.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.addr.translation.Op), .class = "control" })
        .bit_mark()
        .label("AT_OP", .{})
        .end_at_mutable_point(self.input_box.right_side(""));
    _ = self.at_primary_updater.left_side("")
        .wire_h(.{})
        .label("ANY_FAULT", .{})
        .end_at_mutable_point(self.input_box.right_side(""));
    _ = self.at_primary_updater.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.addr.Page) })
        .bit_mark()
        .label("PAGE", .{})
        .end_at_mutable_point(self.input_box.right_side(""));
    _ = self.at_primary_updater.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.addr.translation.Entry.Group) })
        .bit_mark()
        .label("AT_GROUP", .{})
        .end_at_mutable_point(self.input_box.right_side(""));
    _ = self.at_primary_updater.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.addr.translation.Entry.ASN6), .class = "reg" })
        .bit_mark()
        .label("ASN6", .{})
        .end_at_mutable_point(self.input_box.right_side(""));
    _ = self.at_primary_updater.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.addr.translation.Entry) })
        .bit_mark()
        .label("AT_MATCHING_ENTRY", .{})
        .end_at_mutable_point(self.input_box.right_side(""));
    _ = self.at_primary_updater.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.bus.L), .dir = .junction_end })
        .bit_mark()
        .end_at(l_bus.x());

    // at secondary updater
    _ = self.at_secondary_updater.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.addr.translation.Op), .class = "control" })
        .bit_mark()
        .label("AT_OP", .{})
        .end_at_mutable_point(self.input_box.right_side(""));
    _ = self.at_secondary_updater.left_side("")
        .wire_h(.{})
        .label("ANY_FAULT", .{})
        .end_at_mutable_point(self.input_box.right_side(""));
    _ = self.at_secondary_updater.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.addr.Page) })
        .bit_mark()
        .label("PAGE", .{})
        .end_at_mutable_point(self.input_box.right_side(""));
    _ = self.at_secondary_updater.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.addr.translation.Entry.Group) })
        .bit_mark()
        .label("AT_GROUP", .{})
        .end_at_mutable_point(self.input_box.right_side(""));
    _ = self.at_secondary_updater.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.addr.translation.Entry.ASN6), .class = "reg" })
        .bit_mark()
        .label("ASN6", .{})
        .end_at_mutable_point(self.input_box.right_side(""));
    const at_other_entry_wire = self.at_secondary_updater.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.addr.translation.Entry) })
        .bit_mark()
        .label("AT_OTHER_ENTRY", .{})
        .end_at_mutable_point(self.input_box.right_side(""));
    _ = l_bus.turn_and_end_at(self.at_secondary_updater.left_side("")).bit_mark();


    // at primary
    _ = self.at_primary.left_side("A")
        .wire_h(.{ .bits = @bitSizeOf(arch.addr.translation.Entry.Address) })
        .bit_mark()
        .end_at_mutable_point(self.at_primary_updater.right_side(""));
    _ = self.at_primary.left_side("D")
        .wire_h(.{ .bits = @bitSizeOf(arch.addr.translation.Entry) })
        .bit_mark()
        .end_at_mutable_point(self.at_primary_updater.right_side(""));
    _ = self.at_primary.left_side("WE")
        .wire_h(.{})
        .end_at_mutable_point(self.at_primary_updater.right_side(""));

    // at secondary
    _ = self.at_secondary.left_side("A")
        .wire_h(.{ .bits = @bitSizeOf(arch.addr.translation.Entry.Address) })
        .bit_mark()
        .end_at_mutable_point(self.at_secondary_updater.right_side(""));
    _ = self.at_secondary.left_side("D")
        .wire_h(.{ .bits = @bitSizeOf(arch.addr.translation.Entry) })
        .bit_mark()
        .end_at_mutable_point(self.at_secondary_updater.right_side(""));
    _ = self.at_secondary.left_side("WE")
        .wire_h(.{})
        .end_at_mutable_point(self.at_secondary_updater.right_side(""));

    // microcode
    inline for (@typeInfo(arch.microcode.Decode_Microcode_Entry).@"struct".fields) |field| {
        if (!std.mem.startsWith(u8, field.name, "_")) {
            const label = std.ascii.allocUpperString(self.d.state.arena.allocator(), field.name) catch @panic("OOM");

            const offset = @bitOffsetOf(arch.microcode.Decode_Microcode_Entry, field.name);
            const bits = @bitSizeOf(field.type);

            const data_bits = if (bits > 1) std.fmt.comptimePrint("D[{d}:{d}]", .{
                offset + bits - 1,
                offset,
            }) else std.fmt.comptimePrint("D{d}", .{ offset });

            const wire = self.microcode.right_side(data_bits)
                .wire_h(.{ .bits = @bitSizeOf(field.type), .class = "control" })
                .label(label, .{ .alignment = .right })
                .end_at_mutable_point(self.output_box.left_side(""));
            
            if (@bitSizeOf(field.type) > 1) _ = wire.bit_mark();
        }
    }
    _ = self.microcode.left_side(std.fmt.comptimePrint("A[{d}:0]", .{
        @bitSizeOf(arch.microcode.Address) - 1
    })).wire_h(.{ .bits = @bitSizeOf(arch.microcode.Address) })
        .bit_mark()
        .label("UCA", .{})
        .end_at_mutable_point(self.input_box.right_side(""));

    // v_extents
    ///////////////////////////////////////////////////////////////////

    _ = self.v_extents.input_min.attach_to(guard_frame_wire.y());
    _ = self.v_extents.input_max.attach_to(at_other_entry_wire.y());

    _ = self.v_extents.output_min.attach_to(reset_wire.y());
    _ = self.v_extents.output_max.attach_to(asn6_output_wire.y());
}
    
const X_Ref = zbox.X_Ref;
const Box = zbox.Box;
const Drawing = zbox.Drawing;

const V_Extents = @import("V_Extents.zig");
const zbox = @import("zbox");
const arch = @import("arch");
const std = @import("std");
