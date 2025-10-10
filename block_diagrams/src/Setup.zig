d: *Drawing,
input_box: *Box,
output_box: *Box,
v_extents: V_Extents,

read_rsn_decoder: *Box,
vari_rsn_decoder: *Box,

jr: *Box,
sr1: *Box,
sr2: *Box,
kr: *Box,
sr1b: *Box,
sr2b: *Box,

j_mux: *Box,
k_mux: *Box,
kri_underflow_fault_detector: *Box,
va_base_mux: *Box,
va_offset_mux: *Box,
va_adder: *Box,

at_group_decode: *Box,
microcode: *Box,

k_dr_joiner: *Box,
vari_joiner: *Box,
va_dr_joiner: *Box,
va_joiner: *Box,

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

        .read_rsn_decoder = d.box(.{ .label = "RSN\nDecoder" }),
        .vari_rsn_decoder = d.box(.{ .label = "RSN\nDecoder" }),

        .jr = d.box(.{ .label = "JR", .class = "reg" }),
        .sr1 = d.box(.{ .label = "SR1", .class = "reg" }),
        .sr2 = d.box(.{ .label = "SR2", .class = "reg" }),
        .kr = d.box(.{ .label = "KR", .class = "reg" }),
        .sr1b = d.box(.{ .label = "SR1B", .class = "reg" }),
        .sr2b = d.box(.{ .label = "SR2B", .class = "reg" }),

        .j_mux = d.box(.{ .shape = .mux }),
        .k_mux = d.box(.{ .shape = .mux }),
        .kri_underflow_fault_detector = d.box(.{ .label = "KRI Underflow\nFault Detector" }),
        .va_base_mux = d.box(.{ .shape = .mux }),
        .va_offset_mux = d.box(.{ .shape = .mux }),
        .va_adder = d.box(.{ .shape = .alu, .label = "+" }),

        .at_group_decode = d.box(.{ .label = "AT Group\nDecoder" }),
        .microcode = d.box(.{ .label = "\u{B5}code" }),

        .k_dr_joiner = d.box(.{ .shape = .joiner_h }),
        .vari_joiner = d.box(.{ .shape = .joiner_h }),
        .va_dr_joiner = d.box(.{ .shape = .joiner_h }),
        .va_joiner = d.box(.{ .shape = .joiner_h }),
    };
}

pub fn config(self: *@This()) void {
    const device_column_0 = self.d.some_x().attach_to_offset(self.input_box.right(), 150);
    const device_column_1 = self.d.some_x().attach_to_offset(device_column_0, 220);
    const device_column_2 = self.d.some_x().attach_to_offset(device_column_1, 260);
    const device_column_3 = self.d.some_x().attach_to_offset(device_column_2, 200);
    _ = self.output_box.left().attach_to_offset(device_column_3, 180);

    const sram_width = 120;

    _ = self.read_rsn_decoder.width(100).x().attach_to(device_column_0);
    _ = self.vari_rsn_decoder.width(100).x().attach_to(device_column_0);
    _ = self.jr.width(sram_width).x().attach_to(device_column_1);
    _ = self.sr1.width(sram_width).x().attach_to(device_column_1);
    _ = self.sr2.width(sram_width).x().attach_to(device_column_1);
    _ = self.kr.width(sram_width).x().attach_to(device_column_1);
    _ = self.sr1b.width(sram_width).x().attach_to(device_column_1);
    _ = self.sr2b.width(sram_width).x().attach_to(device_column_1);
    _ = self.j_mux.x().attach_to(device_column_2);
    _ = self.k_mux.x().attach_to(device_column_2);
    _ = self.kri_underflow_fault_detector.width(150).x().attach_to_offset(device_column_3, -50);
    _ = self.va_base_mux.x().attach_to(device_column_2);
    _ = self.va_offset_mux.width(60).x().attach_to(device_column_2);
    _ = self.va_adder.x().attach_to(device_column_3);
    _ = self.at_group_decode.width(150).x().attach_to(device_column_3);
    _ = self.microcode.width(200).x().attach_to_offset(device_column_2, -100);

    _ = self.k_dr_joiner.x().attach_to(device_column_1);
    _ = self.vari_joiner.x().attach_to(device_column_0);
    _ = self.va_dr_joiner.x().attach_between(device_column_0, device_column_1, 0.5);
    _ = self.va_joiner.x().attach_to_offset(self.va_adder.right(), 40);

    const sram_height = 80;

    _ = self.j_mux.top().anchor_at(35);
    _ = self.jr.height(sram_height);
    _ = self.sr1.height(sram_height).top().attach_to_offset(self.jr.bottom(), 50);
    _ = self.read_rsn_decoder.height(100).y().attach_between(self.sr1.bottom(), self.sr2.top(), 0.5);
    _ = self.sr2.height(sram_height).bottom().attach_to_offset(self.kr.top(), -50);
    _ = self.kr.height(sram_height);
    _ = self.k_mux.top().attach_to_offset(self.j_mux.bottom(), 250);
    _ = self.kri_underflow_fault_detector.height(100).top().attach_to_offset(self.k_mux.bottom(), -5);
    _ = self.sr1b.height(sram_height).top().attach_to_offset(self.kr.bottom(), 350);
    _ = self.vari_rsn_decoder.height(75).y().attach_between(self.sr1b.top(), self.sr2b.bottom(), 0.6);
    _ = self.sr2b.height(sram_height).top().attach_to_offset(self.sr1b.bottom(), 25);
    _ = self.va_base_mux.y().attach_between(self.sr1b.bottom(), self.sr2b.top(), 0.5);
    _ = self.va_offset_mux.top().attach_to_offset(self.va_base_mux.bottom(), 80);
    _ = self.va_adder.y().attach_between(self.va_base_mux.y(), self.va_offset_mux.y(), 0.5);
    _ = self.at_group_decode.height(100).top().attach_to_offset(self.va_adder.bottom(), 110);
    _ = self.microcode.height(200).top().attach_to_offset(self.at_group_decode.bottom(), 15);

    _ = self.va_joiner.bottom().attach_to_offset(self.va_adder.y(), -25);


    // wires
    ///////////////////////////////////////////////////////////////////
    
    // j_mux
    const jsrc_wire = self.j_mux.top_side("")
        .wire_v(.{ .bits = @bitSizeOf(arch.bus.J.Source), .class = "control" })
        .turn_at(self.d.y(0))
        .label("JSRC", .{})
        .bit_mark()
        .end_at_mutable_point(self.input_box.right_side(""));

    _ = self.j_mux.left_side(@tagName(arch.bus.J.Source.zero))
        .wire_h(.{ .bits = @bitSizeOf(arch.bus.J) })
        .label("0", .{})
        .length(-50);

    const jr_wire = self.j_mux.left_side(@tagName(arch.bus.J.Source.jr))
        .wire_h(.{ .bits = @bitSizeOf(arch.bus.J) })
        .end_at(self.jr.right());
    _ = self.jr.y().attach_to(jr_wire.y());

    const jk_mux_sr_wires = self.d.columns();
    _ = jk_mux_sr_wires.center().attach_between(self.jr.right(), self.j_mux.left(), 0.5);
    _ = self.j_mux.left_side(@tagName(arch.bus.J.Source.sr1))
        .wire_h(.{ .bits = @bitSizeOf(arch.bus.J), .dir = .junction_end })
        .turn_at(jk_mux_sr_wires.push())
        .end_at(self.sr1.y());
    _ = self.j_mux.left_side(@tagName(arch.bus.J.Source.sr2))
        .wire_h(.{ .bits = @bitSizeOf(arch.bus.J), .dir = .junction_end })
        .turn_at(jk_mux_sr_wires.push())
        .end_at(self.sr2.y());

    const j_wire = self.j_mux.right_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.bus.J) })
        .bit_mark()
        .label("J", .{ .alignment = .right })
        .end_at_mutable_point(self.output_box.left_side(""));

    // k_mux
    {
        const km_sr2 = self.k_mux.left_side(@tagName(arch.bus.K.Source.sr2));
        const km_sr1 = self.k_mux.left_side(@tagName(arch.bus.K.Source.sr1));
        const km_kr = self.k_mux.left_side(@tagName(arch.bus.K.Source.kr));
        const km_zero = self.k_mux.left_side(@tagName(arch.bus.K.Source.zero));
        const km_krio_bit_inv = self.k_mux.left_side(@tagName(arch.bus.K.Source.krio_bit_inv));
        const km_krio_bit = self.k_mux.left_side(@tagName(arch.bus.K.Source.krio_bit));
        const km_krio_sx = self.k_mux.left_side(@tagName(arch.bus.K.Source.krio_sx));
        const km_krio_zx = self.k_mux.left_side(@tagName(arch.bus.K.Source.krio_zx));
        const km_constant = self.k_mux.left_side(@tagName(arch.bus.K.Source.constant));
        const km_dr_byte_21 = self.k_mux.left_side(@tagName(arch.bus.K.Source.dr_byte_21_sx));
        const km_dr_byte_2 = self.k_mux.left_side(@tagName(arch.bus.K.Source.dr_byte_2_sx));
        const km_dr_byte_1 = self.k_mux.left_side(@tagName(arch.bus.K.Source.dr_byte_1_sx));

        _ = km_sr2.wire_h(.{ .bits = @bitSizeOf(arch.bus.K) })
            .turn_at(jk_mux_sr_wires.get(1))
            .turn()
            .end_at_point(self.sr2.right_side(""));
        _ = km_sr1.wire_h(.{ .bits = @bitSizeOf(arch.bus.K) })
            .turn_at(jk_mux_sr_wires.get(0))
            .turn()
            .end_at_point(self.sr1.right_side(""));

        _ = km_kr.wire_h(.{ .bits = @bitSizeOf(arch.bus.K) })
            .end_at(self.kr.right());
        _ = self.kr.y().attach_to(km_kr.y());

        _ = km_zero.wire_h(.{ .bits = @bitSizeOf(arch.bus.K) })
            .label("0", .{})
            .length(-50);

        const krio_wire = km_krio_zx.wire_h(.{ .bits = @bitSizeOf(arch.bus.K) })
            .length(-25)
            .small_box("zx", .left)
            .change_bits(@bitSizeOf(arch.bus.K.Read_Index_Offset))
            .label("KRIO", .{})
            .bit_mark()
            .end_at_mutable_point(self.input_box.right_side(""));

        const krio_bit_wire = km_krio_bit.wire_h(.{ .bits = @bitSizeOf(arch.bus.K), .dir = .junction_end })
            .length(-95);
        _ = krio_bit_wire.small_box("1<<", .left)
            .length(-25)
            .turn().end_at(krio_wire.y());

        _ = km_krio_sx.wire_h(.{ .bits = @bitSizeOf(arch.bus.K), .dir = .junction_end })
            .length(-60)
            .small_box("sx", .left)
            .length(-25)
            .turn().end_at(krio_wire.y());

        _ = km_krio_bit_inv.wire_h(.{ .bits = @bitSizeOf(arch.bus.K), .dir = .junction_end })
            .length(-25)
            .small_box("~", .left)
            .length(-25)
            .turn().end_at(krio_bit_wire.y());

        _ = km_constant.wire_h(.{ .bits = @bitSizeOf(arch.bus.K) })
            .length(-60)
            .small_box("sx", .left)
            .change_bits(@bitSizeOf(arch.microcode.Constant))
            .change_class("control")
            .label("CONSTANT", .{})
            .bit_mark()
            .end_at_mutable_point(self.input_box.right_side(""));

        _ = km_dr_byte_21.wire_h(.{ .bits = @bitSizeOf(arch.bus.K) })
            .length(-25)
            .small_box("sx", .left)
            .change_bits(@bitSizeOf(@TypeOf(arch.reg.DR.init(0).@"signed[23:8]"())))
            .change_class("reg")
            .label("DR[23:8]", .{})
            .label("", .{})
            .bit_mark_at(0.4)
            .end_at(self.k_dr_joiner.right_side("").x());

        _ = km_dr_byte_2.wire_h(.{ .bits = @bitSizeOf(arch.bus.K) })
            .length(-60)
            .small_box("sx", .left)
            .change_bits(@bitSizeOf(@TypeOf(arch.reg.DR.init(0).@"signed[23:16]"())))
            .change_class("reg")
            .label("DR[23:16]", .{})
            .label("", .{})
            .bit_mark_at(0.25)
            .end_at(self.k_dr_joiner.right_side("").x());

        _ = km_dr_byte_1.wire_h(.{ .bits = @bitSizeOf(arch.bus.K) })
            .length(-25)
            .small_box("sx", .left)
            .change_bits(@bitSizeOf(@TypeOf(arch.reg.DR.init(0).@"signed[15:8]"())))
            .change_class("reg")
            .label("DR[15:8]", .{})
            .label("", .{})
            .bit_mark_at(0.4)
            .end_at(self.k_dr_joiner.right_side("").x());
        
        _ = self.k_dr_joiner.left_side("")
            .wire_h(.{ .bits = @bitSizeOf(arch.reg.DR), .class = "reg" })
            .label("DR", .{})
            .bit_mark()
            .end_at_mutable_point(self.input_box.right_side(""));

        _ = self.k_dr_joiner.y().attach_to(km_dr_byte_2.y());
    }

    const sr_data_wires = self.d.rows();
    _ = sr_data_wires.middle().attach_between(self.sr1.bottom(), self.sr2.top(), 0.5);
    _ = jk_mux_sr_wires.get(0).intersection_with(sr_data_wires.push())
        .wire_h(.{ .bits = @bitSizeOf(arch.reg.sr1.Value), .dir = .junction_begin })
        .bit_mark()
        .label("SR1D", .{ .alignment = .right })
        .end_at_mutable_point(self.output_box.left_side(""));
    _ = jk_mux_sr_wires.get(1).intersection_with(sr_data_wires.push())
        .wire_h(.{ .bits = @bitSizeOf(arch.reg.sr2.Value), .dir = .junction_begin })
        .bit_mark()
        .label("SR2D", .{ .alignment = .right })
        .end_at_mutable_point(self.output_box.left_side(""));

    _ = self.k_mux.right_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.bus.K) })
        .bit_mark()
        .label("K", .{ .alignment = .right })
        .end_at_mutable_point(self.output_box.left_side(""));

    // kri_underflow_fault_detector
    const ksrc_wire = self.kri_underflow_fault_detector.left_side("")

        .wire_h(.{ .bits = @bitSizeOf(arch.bus.K.Source), .class = "control" })
        .bit_mark()
        .label("KSRC", .{})
        .end_at_mutable_point(self.input_box.right_side(""));
    _ = self.kri_underflow_fault_detector.left_side("")
        .wire_h(.{})
        .label("KRI_UNDERFLOW", .{})
        .end_at_mutable_point(self.input_box.right_side(""));
    _ = self.kri_underflow_fault_detector.right_side("")
        .wire_h(.{})
        .label("KRI_UNDERFLOW_FAULT", .{ .alignment = .right })
        .end_at_mutable_point(self.output_box.left_side(""));

    _ = self.k_mux.bottom_side("")
        .wire_v(.{ .bits = @bitSizeOf(arch.bus.K.Source), .class = "control", .dir = .junction_end })
        .length(35)
        .end_at(ksrc_wire.y());


    // read_rsn_decoder
    _ = self.read_rsn_decoder.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.reg.RSN), .class = "reg" })
        .label("RSN", .{})
        .bit_mark_at(0.2)
        .end_at_mutable_point(self.input_box.right_side(""));
    _ = self.read_rsn_decoder.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.Pipeline) })
        .label("PIPE", .{})
        .bit_mark_at(0.2)
        .end_at_mutable_point(self.input_box.right_side(""));
    _ = self.read_rsn_decoder.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.Execution_Mode), .class = "reg" })
        .label("EXEC_MODE", .{})
        .bit_mark_at(0.2)
        .end_at_mutable_point(self.input_box.right_side(""));
    _ = self.read_rsn_decoder.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.Special_Op), .class = "control" })
        .label("SPECIAL", .{})
        .bit_mark_at(0.2)
        .end_at_mutable_point(self.input_box.right_side(""));

    // vari_rsn_decoder
    _ = self.vari_rsn_decoder.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.reg.RSN), .class = "reg" })
        .label("RSN", .{})
        .bit_mark_at(0.2)
        .end_at_mutable_point(self.input_box.right_side(""));
    _ = self.vari_rsn_decoder.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.Pipeline) })
        .label("PIPE", .{})
        .bit_mark_at(0.2)
        .end_at_mutable_point(self.input_box.right_side(""));
    _ = self.vari_rsn_decoder.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.Execution_Mode), .class = "reg" })
        .label("EXEC_MODE", .{})
        .bit_mark_at(0.2)
        .end_at_mutable_point(self.input_box.right_side(""));


    // jr
    _ = self.jr.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.reg.gpr.Index), .class = "reg" })
        .label("TI", .{})
        .bit_mark()
        .end_at_mutable_point(self.input_box.right_side(""));
    const jr_kr_rsn_wire = self.jr.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.reg.RSN) })
        .turn_between(self.read_rsn_decoder.right(), self.jr.left(), 0.8);
    _ = jr_kr_rsn_wire.turn_and_end_at(self.kr.left_side(""));
    _ = self.read_rsn_decoder.right_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.reg.RSN), .dir = .junction_end })
        .bit_mark()
        .label("READ_RSN", .{})
        .end_at(jr_kr_rsn_wire.x());


    // sr1
    _ = self.sr1.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.reg.sr1.Index), .class = "control" })
        .label("SR1RI", .{})
        .bit_mark()
        .end_at_mutable_point(self.input_box.right_side(""));
    _ = self.sr1.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.reg.RSN), .dir = .junction_end })
        .end_at(jr_kr_rsn_wire.x());

    // sr2
    _ = self.sr2.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.reg.RSN), .dir = .junction_end })
        .end_at(jr_kr_rsn_wire.x());

    _ = self.sr2.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.reg.sr2.Index), .class = "control" })
        .label("SR2RI", .{})
        .bit_mark()
        .end_at_mutable_point(self.input_box.right_side(""));

    // kr
    _ = self.kr.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.reg.gpr.Index) })
        .label("KRI", .{})
        .bit_mark()
        .end_at_mutable_point(self.input_box.right_side(""));

    // sr1b
    const vari_wire = self.sr1b.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.addr.Virtual.Base), .class = "control" })
        .label("VARI[2:0]", .{})
        .label("", .{})
        .end_at_mutable_point(self.vari_joiner.right_side(""));

    const sr1b_sr2b_rsn_wire = self.sr1b.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.reg.RSN) })
        .turn_between(self.vari_rsn_decoder.right(), self.sr1b.left(), 0.8);
    _ = sr1b_sr2b_rsn_wire.turn_and_end_at(self.sr2b.left_side(""));
    _ = self.vari_rsn_decoder.right_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.reg.RSN), .dir = .junction_end })
        .bit_mark()
        .label("VARI_RSN", .{})
        .end_at(sr1b_sr2b_rsn_wire.x());

    // sr2b
    _ = self.sr2b.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.addr.Virtual.Base), .class = "control", .dir = .junction_end })
        .turn_between(self.vari_rsn_decoder.right(), self.sr2b.left(), 0.6)
        .end_at(vari_wire.y());

    // vari_joiner
    _ = self.vari_joiner.height(80).y().attach_to(self.sr1b.top());
    _ = self.vari_joiner.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.addr.Virtual.Base), .class = "control" })
        .label("VARI", .{})
        .bit_mark()
        .end_at_mutable_point(self.input_box.right_side(""));

    // va_base_mux
    _ = self.va_base_mux.top_side("")
        .wire_v(.{ .class = "control" })
        .turn_at_offset(self.sr1b.top(), -25)
        .label("VARI[3]", .{})
        .label("", .{})
        .end_at_mutable_point(self.vari_joiner.right_side(""));
    _ = self.va_base_mux.left_side("0")
        .wire_h(.{ .bits = @bitSizeOf(arch.reg.sr1.Value) })
        .turn()
        .turn()
        .bit_mark()
        .end_at_point(self.sr1b.right_side(""));
    _ = self.va_base_mux.left_side("1")
        .wire_h(.{ .bits = @bitSizeOf(arch.reg.sr2.Value) })
        .turn()
        .turn()
        .bit_mark()
        .end_at_point(self.sr2b.right_side(""));

    // va_adder
    _ = self.va_adder.left_side_upper("")
        .wire_h(.{ .bits = @bitSizeOf(arch.addr.Virtual) })
        .bit_mark()
        .turn()
        .turn()
        .end_at_point(self.va_base_mux.right_side(""));
    _ = self.va_adder.left_side_lower("")
        .wire_h(.{ .bits = @bitSizeOf(arch.addr.Virtual) })
        .bit_mark()
        .turn()
        .turn()
        .end_at_point(self.va_offset_mux.right_side(""));
    _ = self.va_adder.right_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.addr.Virtual) })
        .bit_mark()
        .label("VA", .{ .alignment = .right })
        .end_at_mutable_point(self.output_box.left_side(""));

    _ = self.va_joiner.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.addr.Virtual), .dir = .junction_end })
        .length(-20)
        .turn()
        .bit_mark()
        .end_at(self.va_adder.y());
    _ = self.va_joiner.right_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.addr.Page) })
        .bit_mark_at(0.2)
        .label("PAGE", .{ .alignment = .right })
        .end_at_mutable_point(self.output_box.left_side(""));
    _ = self.va_joiner.right_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.addr.Page.Offset) })
        .bit_mark_at(0.2)
        .label("OFFSET", .{ .alignment = .right })
        .end_at_mutable_point(self.output_box.left_side(""));

    // va_offset_mux
    {
        const vm_zero = self.va_offset_mux.left_side(@tagName(arch.addr.Virtual.Offset_Source.zero));
        const vm_i8_from_dr = self.va_offset_mux.left_side(@tagName(arch.addr.Virtual.Offset_Source.i8_from_dr));
        const vm_i16_from_dr = self.va_offset_mux.left_side(@tagName(arch.addr.Virtual.Offset_Source.i16_from_dr));
        const vm_constant = self.va_offset_mux.left_side(@tagName(arch.addr.Virtual.Offset_Source.constant));

        _ = vm_zero.wire_h(.{ .bits = 32 })
            .length(-40)
            .label("0", .{});

        _ = vm_i8_from_dr.wire_h(.{ .bits = 32 })
            .length(-50)
            .bit_mark()
            .small_box("sx", .left)
            .change_bits(8)
            .change_class("reg")
            .bit_mark()
            .label("DR[23:16]", .{})
            .label("", .{})
            .end_at_mutable_point(self.va_dr_joiner.right_side(""));

        _ = vm_i16_from_dr.wire_h(.{ .bits = 32 })
            .length(-85)
            .bit_mark()
            .small_box("sx", .left)
            .change_bits(16)
            .change_class("reg")
            .bit_mark()
            .label("DR[23:8]", .{})
            .label("", .{})
            .end_at_mutable_point(self.va_dr_joiner.right_side(""));

        _ = vm_constant.wire_h(.{ .bits = 32 })
            .length(-50)
            .bit_mark()
            .small_box("sx", .left)
            .change_bits(@bitSizeOf(arch.microcode.Constant))
            .change_class("control")
            .bit_mark()
            .label("CONSTANT", .{})
            .end_at_mutable_point(self.input_box.right_side(""));
        
        _ = self.va_offset_mux.bottom_side("")
            .wire_v(.{ .bits = @bitSizeOf(arch.addr.Virtual.Offset_Source), .class = "control" })
            .length(35)
            .turn()
            .bit_mark()
            .label("VAO_SRC", .{})
            .end_at_mutable_point(self.input_box.right_side(""));

        _ = self.va_dr_joiner.height(40).y().attach_between(vm_i16_from_dr.y(), vm_i8_from_dr.y(), 0.5);
    }

    _ = self.va_dr_joiner.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.reg.DR), .class = "reg" })
        .label("DR", .{})
        .bit_mark()
        .end_at_mutable_point(self.input_box.right_side(""));

    // microcode
    var dsrc_wire: ?*zbox.Wire_H = null;
    var space_wire: ?*zbox.Wire_H = null;
    var last_uc_wire: ?*zbox.Wire_H = null;
    inline for (@typeInfo(arch.microcode.Compute_Microcode_Entry).@"struct".fields) |field| {
        if (!std.mem.startsWith(u8, field.name, "_")) {
            const label = std.ascii.allocUpperString(self.d.state.arena.allocator(), field.name) catch @panic("OOM");

            const offset = @bitOffsetOf(arch.microcode.Compute_Microcode_Entry, field.name);
            const bits = @bitSizeOf(field.type);

            const data_bits = if (bits > 1) std.fmt.comptimePrint("D[{d}:{d}]", .{
                offset + bits - 1,
                offset,
            }) else std.fmt.comptimePrint("D{d}", .{ offset });

            last_uc_wire = self.microcode.right_side(data_bits)
                .wire_h(.{ .bits = @bitSizeOf(field.type), .class = "control" })
                .label(label, .{ .alignment = .right })
                .end_at_mutable_point(self.output_box.left_side(""));

            if (@bitSizeOf(field.type) > 1) _ = last_uc_wire.?.bit_mark();

            if (comptime std.mem.eql(u8, field.name, "space")) {
                space_wire = last_uc_wire;
            } else if (comptime std.mem.eql(u8, field.name, "dsrc")) {
                dsrc_wire = last_uc_wire;
            }
        }
    }
    const uca_wire = self.microcode.left_side(std.fmt.comptimePrint("A[{d}:0]", .{
        @bitSizeOf(arch.microcode.Address) - 1
    })).wire_h(.{ .bits = @bitSizeOf(arch.microcode.Address) })
        .bit_mark()
        .label("UCA", .{})
        .end_at_mutable_point(self.input_box.right_side(""));


    // at_group_decode
    const at_group_inputs = self.d.columns();
    _ = at_group_inputs.center().attach_between(self.microcode.right(), self.at_group_decode.left(), 0.5);

    _ = self.at_group_decode.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.addr.Space), .class = "control", .dir = .junction_end })
        .bit_mark()
        .turn_at(at_group_inputs.push())
        .end_at(space_wire.?.y());
    _ = self.at_group_decode.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.bus.D.Source), .class = "control", .dir = .junction_end })
        .bit_mark()
        .turn_at(at_group_inputs.push())
        .end_at(dsrc_wire.?.y());

    _ = self.at_group_decode.right_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.addr.translation.Entry.Group) })
        .bit_mark()
        .label("AT_GROUP", .{ .alignment = .right })
        .end_at_mutable_point(self.output_box.left_side(""));

    // v_extents
    ///////////////////////////////////////////////////////////////////

    _ = self.v_extents.input_min.attach_to(jsrc_wire.y());
    _ = self.v_extents.input_max.attach_to(uca_wire.y());

    _ = self.v_extents.output_min.attach_to(j_wire.y());
    _ = self.v_extents.output_max.attach_to(last_uc_wire.?.y());
}

const X_Ref = zbox.X_Ref;
const Box = zbox.Box;
const Drawing = zbox.Drawing;

const V_Extents = @import("V_Extents.zig");
const zbox = @import("zbox");
const arch = @import("arch");
const std = @import("std");
