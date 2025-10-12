d: *Drawing,
input_box: *Box,
output_box: *Box,
v_extents: V_Extents,

alu_mode_joiner: *Box,
alu_cin_and: *Box,
alu_cin_xor: *Box,
alu: *Box,

mult_mode_joiner: *Box,
mult_j_joiner: *Box,
mult_k_joiner: *Box,
mult_j_mux: *Box,
mult_k_mux: *Box,
mult: *Box,
mult_output_shifter: *Box,
mult_overflow_detector: *Box,

shift_mode_joiner: *Box,
shift_cin: *Box,
shift_j_joiner: *Box,
shift_j_conditioning: *Box,
shift_k_decoder: *Box,
shift: *Box,
shift_output_select: *Box,
shift_flag_logic: *Box,

cx_mode_joiner: *Box,
cx_j_xor: *Box,
cx_k_xor: *Box,
cx_k_sat_left: *Box,
cx_k_sat_right: *Box,
cx_k_sat_mux: *Box,
cx_k_sat_xor: *Box,
cx_j_and_sat: *Box,
cx_j_count: *Box,
cx_sx_mux: *Box,
cx_sx_or: *Box,
cx_ext_xor: *Box,
cx_sx_decoder: *Box,

fault_logic: *Box,

at_frame_mux: *Box,
at_page_joiner: *Box,
at_primary: *Box,
at_secondary: *Box,
at_entry_swap: *Box,
at_pri_joiner: *Box,
at_sec_joiner: *Box,
at_matcher: *Box,
at_me_joiner: *Box,
at_control_logic: *Box,

offset_joiner: *Box,
offset_adder: *Box,

microcode: *Box,

pub fn init(d: *Drawing, input_box: *Box, output_box: *Box) @This() {
    return .{
        .d = d,
        .input_box = input_box,
        .output_box = output_box,
        .v_extents = .init(d),

        .alu_mode_joiner = d.box(.{ .shape = .joiner_h }),
        .alu_cin_and = d.box(.{ .shape = .@"and" }),
        .alu_cin_xor = d.box(.{ .shape = .xor }),
        .alu = d.box(.{ .shape = .alu, .label = "ALU" }),

        .mult_mode_joiner = d.box(.{ .shape = .joiner_h }),
        .mult_j_joiner = d.box(.{ .shape = .joiner_h }),
        .mult_k_joiner = d.box(.{ .shape = .joiner_h }),
        .mult_j_mux = d.box(.{ .shape = .mux }),
        .mult_k_mux = d.box(.{ .shape = .mux }),
        .mult = d.box(.{ .shape = .alu, .label = "\u{D7}" }),
        .mult_output_shifter = d.box(.{ .label = "Output Select" }),
        .mult_overflow_detector = d.box(.{ .label = "16b Overflow\nDetector" }),

        .shift_mode_joiner = d.box(.{ .shape = .joiner_h }),
        .shift_cin = d.box(.{ .label = "Carry / Mode\nDecoder" }),
        .shift_j_joiner = d.box(.{ .shape = .joiner_h }),
        .shift_j_conditioning = d.box(.{ .label = "" }),
        .shift_k_decoder = d.box(.{ .label = "K Decoder" }),
        .shift = d.box(.{ .shape = .alu, .label = ">>" }),
        .shift_output_select = d.box(.{ .label = "Output Select" }),
        .shift_flag_logic = d.box(.{ .label = "Flag Logic" }),

        .cx_mode_joiner = d.box(.{ .shape = .joiner_h }),
        .cx_j_xor = d.box(.{ .shape = .xor }),
        .cx_k_xor = d.box(.{ .shape = .xor }),
        .cx_k_sat_left = d.box(.{ .label = "Saturate Left" }),
        .cx_k_sat_right = d.box(.{ .label = "Saturate Right" }),
        .cx_k_sat_mux = d.box(.{ .shape = .mux }),
        .cx_k_sat_xor = d.box(.{ .shape = .xor }),
        .cx_j_and_sat = d.box(.{ .shape = .@"and" }),
        .cx_j_count = d.box(.{ .label = "Pop Count" }),
        .cx_sx_mux = d.box(.{ .shape = .mux }),
        .cx_sx_or = d.box(.{ .shape = .@"or" }),
        .cx_ext_xor = d.box(.{ .shape = .xor }),
        .cx_sx_decoder = d.box(.{ .label = "SX Decoder" }),

        .fault_logic = d.box(.{ .label = "Fault\nDetection\n&\nBus\nControl" }),

        .at_frame_mux = d.box(.{ .shape = .mux }),
        .at_page_joiner = d.box(.{ .shape = .joiner_h }),
        .at_primary = d.box(.{ .label = "Primary\nAT File", .class = "reg" }),
        .at_secondary = d.box(.{ .label = "Secondary\nAT File", .class = "reg" }),
        .at_entry_swap = d.box(.{ .shape = .bowtie }),
        .at_pri_joiner = d.box(.{ .shape = .joiner_h }),
        .at_sec_joiner = d.box(.{ .shape = .joiner_h }),
        .at_matcher = d.box(.{}),
        .at_me_joiner = d.box(.{ .shape = .joiner_h }),
        .at_control_logic = d.box(.{ .label = "Control\nLogic" }),

        .offset_joiner = d.box(.{ .shape = .joiner_h }),
        .offset_adder = d.box(.{ .shape = .alu, .label = "+" }),

        .microcode = d.box(.{ .label = "\u{B5}code" }),
    };
}

pub fn config(self: *@This()) void {
    const device_column_0 = self.d.some_x().attach_to_offset(self.input_box.right(), 100);
    const device_column_1 = self.d.some_x().attach_to_offset(device_column_0, 300);
    const device_column_2 = self.d.some_x().attach_to_offset(device_column_1, 220);
    const device_column_3 = self.d.some_x().attach_to_offset(device_column_2, 220);
    const device_column_4 = self.d.some_x().attach_to_offset(device_column_3, 320);
    _ = self.output_box.left().attach_to_offset(device_column_4, 250);

    _ = self.alu_mode_joiner.x().attach_to(device_column_0);
    _ = self.alu_cin_and.x().attach_between(device_column_0, device_column_1, 0.35);
    _ = self.alu_cin_xor.x().attach_between(device_column_0, device_column_1, 0.65);
    _ = self.alu.x().attach_to(device_column_1);

    _ = self.mult_mode_joiner.x().attach_to(device_column_0);
    _ = self.mult_j_joiner.x().attach_to(device_column_0);
    _ = self.mult_k_joiner.x().attach_to(device_column_0);
    _ = self.mult_j_mux.x().attach_between(device_column_0, device_column_1, 0.5);
    _ = self.mult_k_mux.x().attach_between(device_column_0, device_column_1, 0.35);
    _ = self.mult.x().attach_to(device_column_1);
    _ = self.mult_output_shifter.width(150).x().attach_to(device_column_2);
    _ = self.mult_overflow_detector.width(150).x().attach_to(device_column_2);

    _ = self.shift_mode_joiner.x().attach_to(device_column_0);
    _ = self.shift_cin.width(150).x().attach_to(device_column_1);
    _ = self.shift_j_joiner.x().attach_to(device_column_0);
    _ = self.shift_j_conditioning.width(150).x().attach_to(device_column_1);
    _ = self.shift_k_decoder.width(150).x().attach_to(device_column_1);
    _ = self.shift.width(90).x().attach_to(device_column_2);
    _ = self.shift_output_select.width(150).x().attach_to(device_column_3);
    _ = self.shift_flag_logic.width(150).x().attach_to(device_column_3);

    _ = self.cx_mode_joiner.x().attach_to_offset(device_column_0, -40);
    _ = self.cx_j_xor.right().attach_to_offset(self.cx_k_sat_left.left(), -80);
    _ = self.cx_k_xor.right().attach_to_offset(self.cx_k_sat_left.left(), -80);
    _ = self.cx_k_sat_left.width(150).x().attach_to(device_column_1);
    _ = self.cx_k_sat_right.width(150).x().attach_to(device_column_1);
    _ = self.cx_k_sat_mux.left().attach_to_offset(self.cx_k_sat_left.right(), 40);
    _ = self.cx_k_sat_xor.left().attach_to_offset(self.cx_k_sat_mux.right(), 60);
    _ = self.cx_j_and_sat.left().attach_to_offset(self.cx_k_sat_xor.right(), 60);
    _ = self.cx_j_count.width(125).left().attach_to_offset(self.cx_j_and_sat.right(), 40);
    _ = self.cx_sx_mux.left().attach_to_offset(self.cx_k_sat_xor.right(), 60);
    _ = self.cx_sx_or.left().attach_to_offset(self.cx_j_and_sat.right(), 40);
    _ = self.cx_ext_xor.left().attach_to_offset(self.cx_sx_or.right(), 60);
    _ = self.cx_sx_decoder.width(150).x().attach_to(device_column_1);

    _ = self.fault_logic.width(100).x().attach_to(device_column_4);

    _ = self.at_frame_mux.left().attach_to_offset(self.at_control_logic.right(), 20);
    _ = self.at_page_joiner.x().attach_to_offset(device_column_0, -40);
    _ = self.at_primary.width(100).x().attach_to_offset(device_column_1, -140);
    _ = self.at_secondary.width(100).x().attach_to_offset(device_column_1, -140);
    _ = self.at_entry_swap.width(60).x().attach_to_offset(device_column_2, 40);
    _ = self.at_pri_joiner.x().attach_to_offset(self.at_primary.right(), 40);
    _ = self.at_sec_joiner.x().attach_to_offset(self.at_secondary.right(), 40);
    _ = self.at_matcher.width(120).left().attach_to_offset(self.at_pri_joiner.right(), 120);
    _ = self.at_me_joiner.x().attach_to_offset(self.at_entry_swap.right(), 60);
    _ = self.at_control_logic.width(100).x().attach_to_offset(device_column_3, 40);

    _ = self.offset_joiner.x().attach_to_offset(device_column_0, 100);
    _ = self.offset_adder.x().attach_to(device_column_1);

    _ = self.microcode.width(200).x().attach_to_offset(device_column_3, -100);


    _ = self.alu_mode_joiner.top().anchor_at(-25);
    _ = self.alu_cin_xor.top().attach_to_offset(self.alu_mode_joiner.bottom(), 15);
    _ = self.alu.top().attach_to_offset(self.alu_cin_xor.bottom(), 25);

    _ = self.mult_mode_joiner.top().attach_to_offset(self.alu.bottom(), 50);
    _ = self.mult.top().attach_to_offset(self.mult_mode_joiner.bottom(), 25);
    _ = self.mult_output_shifter.height(80).y().attach_to(self.mult.y());
    _ = self.mult_overflow_detector.height(80).bottom().attach_to_offset(self.mult_output_shifter.top(), -25);

    _ = self.shift_cin.height(120).top().attach_to_offset(self.mult.bottom(), 75);
    _ = self.shift_j_conditioning.height(100).top().attach_to_offset(self.shift_cin.bottom(), 45);
    _ = self.shift_k_decoder.height(140).top().attach_to_offset(self.shift_j_conditioning.bottom(), 45);
    _ = self.shift.height(150).top().attach_to_offset(self.shift_cin.bottom(), 70);
    _ = self.shift_output_select.height(80).y().attach_to_offset(self.shift.y(), -20);
    _ = self.shift_flag_logic.height(160).top().attach_to_offset(self.shift_output_select.bottom(), 45);

    _ = self.cx_mode_joiner.top().attach_to_offset(self.shift_k_decoder.bottom(), 100);
    _ = self.cx_j_xor.top().attach_to_offset(self.cx_mode_joiner.bottom(), 25);
    _ = self.cx_k_sat_left.height(60).top().attach_to_offset(self.cx_j_xor.bottom(), 25);
    _ = self.cx_k_sat_right.height(60).top().attach_to_offset(self.cx_k_sat_left.bottom(), 10);
    _ = self.cx_k_xor.y().attach_to(self.cx_k_sat_left.y());
    _ = self.cx_k_sat_mux.height(100).y().attach_between(self.cx_k_sat_left.y(), self.cx_k_sat_right.y(), 0.5);
    _ = self.cx_k_sat_xor.y().attach_to_offset(self.cx_k_sat_mux.y(), -10);
    _ = self.cx_j_and_sat.y().attach_to_offset(self.cx_j_xor.y(), 10);
    _ = self.cx_j_count.height(60).y().attach_to_offset(self.cx_j_and_sat.y(), -120);
    _ = self.cx_sx_mux.y().attach_to_offset(self.cx_k_sat_xor.y(), 20);
    _ = self.cx_sx_or.y().attach_to_offset(self.cx_j_and_sat.y(), 10);
    _ = self.cx_ext_xor.bottom().attach_to_offset(self.cx_sx_or.y(), -25);
    _ = self.cx_sx_decoder.height(80).top().attach_to_offset(self.cx_k_sat_right.bottom(), 25);

    _ = self.fault_logic.height(400).top().attach_to_offset(self.cx_sx_decoder.bottom(), -25);

    _ = self.at_frame_mux.top().attach_to_offset(self.fault_logic.bottom(), 25);
    _ = self.at_primary.height(80).top().attach_to_offset(self.at_page_joiner.bottom(), 20);
    _ = self.at_secondary.height(80).top().attach_to_offset(self.at_primary.bottom(), 60);
    _ = self.at_entry_swap.height(50).y().attach_to_offset(self.at_primary.y(), -10);
    _ = self.at_pri_joiner.y().attach_to(self.at_primary.y());
    _ = self.at_sec_joiner.y().attach_to(self.at_secondary.y());
    _ = self.at_matcher.height(120).y().attach_between(self.at_primary.y(), self.at_secondary.y(), 0.5);
    _ = self.at_control_logic.height(380).top().attach_to_offset(self.at_entry_swap.y(), 50);

    _ = self.offset_joiner.top().attach_to_offset(self.at_control_logic.bottom(), 20);
    _ = self.offset_adder.height(80).top().attach_to_offset(self.offset_joiner.top(), 40);

    _ = self.microcode.height(280).top().attach_to_offset(self.offset_adder.bottom(), 50);

    // wires
    ///////////////////////////////////////////////////////////////////
    
    // alu
    var alu_mode_wire: *zbox.Wire_H = undefined;
    var alu_v_wire: *zbox.Wire_H = undefined;
    var alu_c_wire: *zbox.Wire_H = undefined;
    {
        alu_mode_wire = self.alu_mode_joiner.left_side("")
            .wire_h(.{ .bits = @bitSizeOf(arch.compute.Mode), .class = "control" })
            .label("MODE", .{})
            .bit_mark()
            .end_at_mutable_point(self.input_box.right_side(""));

        _ = self.alu_mode_joiner.right_side("")
            .wire_h(.{ .bits = @bitSizeOf(arch.compute.Mode.ALU.Opcode), .class = "control" })
            .fmt_label("MODE[{d}:{d}]", .{
                @bitOffsetOf(arch.compute.Mode.ALU, "op") + @bitSizeOf(arch.compute.Mode.ALU.Opcode) - 1,
                @bitOffsetOf(arch.compute.Mode.ALU, "op"),
            }, .{})
            .label("", .{})
            .turn_and_end_at(self.alu.top_side("S[2:0]"));

        _ = self.alu_mode_joiner.right_side("")
            .wire_h(.{ .class = "control" })
            .fmt_label("MODE[{d}]", .{
                @bitOffsetOf(arch.compute.Mode.ALU, "invert_cin")
            }, .{})
            .label("", .{})
            .length(150)
            .turn()
            .turn_and_end_at(self.alu_cin_xor.left_side(""));

        _ = self.alu_mode_joiner.right_side("")
            .wire_h(.{ .class = "control" })
            .fmt_label("MODE[{d}]", .{
                @bitOffsetOf(arch.compute.Mode.ALU, "use_carry_flag")
            }, .{})
            .label("", .{})
            .length(65)
            .turn()
            .turn_and_end_at(self.alu_cin_and.left_side(""));

        const alu_non_inverted_cin = self.alu_cin_xor.left_side("")
            .wire_h(.{})
            .end_at(self.alu_cin_and.right());

        _ = self.alu_cin_and.y().attach_to(alu_non_inverted_cin.y());

        _ = self.alu_cin_and.left_side("")
            .wire_h(.{ .class = "reg" })
            .label("FLAG_C", .{})
            .end_at_mutable_point(self.input_box.right_side(""));

        _ = self.alu_cin_xor.right_side("")
            .wire_h(.{})
            .turn_between(self.alu_cin_xor.right(), self.alu.left(), 0.5)
            .turn_and_end_at(self.alu.left_side_upper("Cin"));

        _ = self.alu.left_side_upper("")
            .wire_h(.{ .bits = @bitSizeOf(arch.bus.J) })
            .label("J", .{})
            .bit_mark()
            .end_at_mutable_point(self.input_box.right_side(""));

        _ = self.alu.left_side_lower("")
            .wire_h(.{ .bits = @bitSizeOf(arch.bus.K) })
            .label("K", .{})
            .bit_mark()
            .end_at_mutable_point(self.input_box.right_side(""));

        alu_c_wire = self.alu.right_side("Cout")
            .wire_h(.{})
            .label("ALU_C", .{ .alignment = .right })
            .end_at_mutable_point(self.output_box.left_side(""));
        _ = self.alu.right_side("")
            .wire_h(.{ .bits = @bitSizeOf(arch.bus.L) })
            .bit_mark()
            .label("ALU_RESULT", .{ .alignment = .right })
            .end_at_mutable_point(self.output_box.left_side(""));
        alu_v_wire = self.alu.right_side("overflow")
            .wire_h(.{})
            .label("ALU_V", .{ .alignment = .right })
            .end_at_mutable_point(self.output_box.left_side(""));
    }

    // mult
    var mult_v_wire: *zbox.Wire_H = undefined;
    {
        const between_mult_and_ovf = self.d.columns();
        _ = between_mult_and_ovf.center().attach_between(self.mult.right(), self.mult_overflow_detector.left(), 0.5);

        _ = self.mult_mode_joiner.left_side("")
            .wire_h(.{ .bits = @bitSizeOf(arch.compute.Mode), .class = "control" })
            .label("MODE", .{})
            .bit_mark()
            .end_at_mutable_point(self.input_box.right_side(""));

        _ = self.mult_mode_joiner.right_side("")
            .wire_h(.{ .class = "control" })
            .fmt_label("MODE[{d}]", .{
                @bitOffsetOf(arch.compute.Mode.Multiply, "shift_result")
            }, .{})
            .label("", .{})
            .turn_at(between_mult_and_ovf.push())
            .turn_and_end_at(self.mult_output_shifter.left_side_upper("<<16"));

        const j_type_wire = self.mult_mode_joiner.right_side("")
            .wire_h(.{ .class = "control" })
            .fmt_label("MODE[{d}]", .{
                @bitOffsetOf(arch.compute.Mode.Multiply, "j_type")
            }, .{})
            .label("", .{})
            .length(230)
            .turn();
        _ = j_type_wire.turn_and_end_at(self.mult.left_side_upper("S"));

        const k_type_wire = self.mult_mode_joiner.right_side("")
            .wire_h(.{ .class = "control" })
            .fmt_label("MODE[{d}]", .{
                @bitOffsetOf(arch.compute.Mode.Multiply, "k_type")
            }, .{})
            .label("", .{})
            .length(210)
            .turn();
        _ = k_type_wire.turn_and_end_at(self.mult.left_side_lower("S"));

        _ = self.mult_mode_joiner.right_side("")
            .wire_h(.{ .class = "control" })
            .fmt_label("MODE[{d}]", .{
                @bitOffsetOf(arch.compute.Mode.Multiply, "j"),
            }, .{})
            .label("", .{})
            .turn_and_end_at(self.mult_j_mux.top_side(""));

        _ = self.mult_mode_joiner.right_side("")
            .wire_h(.{ .class = "control" })
            .fmt_label("MODE[{d}]", .{
                @bitOffsetOf(arch.compute.Mode.Multiply, "k"),
            }, .{})
            .label("", .{})
            .turn_and_end_at(self.mult_k_mux.top_side(""));

        const j_wire = self.mult.left_side_upper("")
            .wire_h(.{ .bits = 16 })
            .bit_mark_at(0.75)
            .end_at(self.mult_j_mux.right());
        _ = self.mult_j_mux.y().attach_to(j_wire.y());
        _ = self.mult_j_joiner.y().attach_to(j_wire.y());

        const k_wire = self.mult.left_side_lower("")
            .wire_h(.{ .bits = 16 })
            .bit_mark()
            .end_at(self.mult_k_mux.right());
        _ = self.mult_k_mux.y().attach_to(k_wire.y());
        _ = self.mult_k_joiner.y().attach_to(k_wire.y());


        _ = self.mult_j_joiner.right_side("")
            .wire_h(.{ .bits = 16 })
            .bit_mark_at(0.65)
            .label("J[15:0]", .{})
            .label("", .{})
            .end_at(self.mult_j_mux.left_side("0").x());

        _ = self.mult_j_joiner.right_side("")
            .wire_h(.{ .bits = 16 })
            .bit_mark_at(0.65)
            .label("J[31:16]", .{})
            .label("", .{})
            .end_at(self.mult_j_mux.left_side("1").x());


        _ = self.mult_k_joiner.right_side("")
            .wire_h(.{ .bits = 16 })
            .bit_mark_at(0.8)
            .label("K[15:0]", .{})
            .label("", .{})
            .end_at(self.mult_k_mux.left_side("0").x());

        _ = self.mult_k_joiner.right_side("")
            .wire_h(.{ .bits = 16 })
            .bit_mark_at(0.8)
            .label("K[31:16]", .{})
            .label("", .{})
            .end_at(self.mult_k_mux.left_side("1").x());


        _ = self.mult_j_joiner.left_side("")
            .wire_h(.{ .bits = @bitSizeOf(arch.bus.J) })
            .bit_mark()
            .label("J", .{})
            .end_at_mutable_point(self.input_box.right_side(""));

        _ = self.mult_k_joiner.left_side("")
            .wire_h(.{ .bits = @bitSizeOf(arch.bus.K) })
            .bit_mark()
            .label("K", .{})
            .end_at_mutable_point(self.input_box.right_side(""));

        const mult_output_wire = self.mult.right_side("")
            .wire_h(.{ .bits = 32 })
            .bit_mark_at(0.3)
            .end_at_mutable_point(self.mult_output_shifter.left_side(""));

        _ = self.mult_overflow_detector.left_side("")
            .wire_h(.{ .class = "control", .dir = .junction_end })
            .end_at(j_type_wire.x());
        _ = self.mult_overflow_detector.left_side("")
            .wire_h(.{ .class = "control", .dir = .junction_end })
            .end_at(k_type_wire.x());
        _ = self.mult_overflow_detector.left_side("")
            .wire_h(.{ .bits = 32, .dir = .junction_end })
            .turn_at(between_mult_and_ovf.push())
            .end_at(mult_output_wire.y());


        _ = self.mult_output_shifter.right_side("")
            .wire_h(.{ .bits = @bitSizeOf(arch.bus.L) })
            .bit_mark_at(0.75)
            .label("MULT_RESULT", .{ .alignment = .right })
            .end_at_mutable_point(self.output_box.left_side(""));
        mult_v_wire = self.mult_overflow_detector.right_side("")
            .wire_h(.{})
            .label("MULT_V", .{ .alignment = .right })
            .end_at_mutable_point(self.output_box.left_side(""));
    }

    // shift
    var shift_v_wire: *zbox.Wire_H = undefined;
    {
        _ = self.shift_mode_joiner.left_side("")
            .wire_h(.{ .bits = @bitSizeOf(arch.compute.Mode), .class = "control" })
            .label("MODE", .{})
            .bit_mark()
            .end_at_mutable_point(self.input_box.right_side(""));

        const mode_cols = self.d.columns();
        _ = mode_cols.center().attach_between(self.shift_mode_joiner.right(), self.shift_cin.left(), 0.5);

        _ = self.shift_cin.left_side("")
            .wire_h(.{ .class = "reg" })
            .label("FLAG_C", .{})
            .end_at_mutable_point(self.input_box.right_side(""));

        inline for (0..@bitSizeOf(arch.compute.Mode.Shift.Carry)) |i| {
            const bit = @bitOffsetOf(arch.compute.Mode.Shift, "cin") + @bitSizeOf(arch.compute.Mode.Shift.Carry) - i - 1;
            _ = self.shift_mode_joiner.right_side("")
                .wire_h(.{ .class = "control" })
                .fmt_label("MODE[{d}]", .{ bit }, .{})
                .label("", .{})
                .end_at(self.shift_cin.left_side("").x());
        }
        const shl_wire_end = self.shift_cin.left_side("");

        _ = self.shift_j_joiner.left_side("")
            .wire_h(.{ .bits = @bitSizeOf(arch.bus.J) })
            .label("J", .{})
            .bit_mark()
            .end_at_mutable_point(self.input_box.right_side(""));

        const j_wire = self.shift_j_joiner.right_side("")
            .wire_h(.{ .bits = @bitSizeOf(arch.bus.J) })
            .bit_mark()
            .fmt_label("J[{d}:0]", .{
                @bitSizeOf(arch.bus.J) - 1
            }, .{})
            .label("", .{});

        _ = self.shift_cin.left_side("")
            .wire_h(.{})
            .turn_at(mode_cols.push())
            .turn_and_end_at(self.shift_j_joiner.right_side(""))
            .fmt_label("J[{d}]", .{
                @bitSizeOf(arch.bus.J) - 1
            }, .{})
            .label("", .{});

        const shl_wire = self.shift_mode_joiner.right_side("")
            .wire_h(.{ .class = "control" })
            .fmt_label("MODE[{d}]", .{
                @bitOffsetOf(arch.compute.Mode.Shift, "left"),
            }, .{})
            .label("", .{})
            .end_at(shl_wire_end.x());
        _ = self.shift_mode_joiner.y().attach_to(shl_wire_end.y());
        _ = self.shift_j_conditioning.left_side("reverse bits in bytes")
            .wire_h(.{ .class = "control", .dir = .junction_end })
            .length(-25)
            .turn_at(mode_cols.push())
            .end_at(shl_wire.y());
        _ = self.shift_mode_joiner.right_side("")
            .wire_h(.{ .class = "control" })
            .fmt_label("MODE[{d}]", .{
                @bitOffsetOf(arch.compute.Mode.Shift, "left_xor_swap_bytes"),
            }, .{})
            .label("", .{})
            .turn_at(mode_cols.push())
            .turn_and_end_at(self.shift_j_conditioning.left_side("swap bytes in halves"));
        _ = self.shift_mode_joiner.right_side("")
            .wire_h(.{ .class = "control" })
            .fmt_label("MODE[{d}]", .{
                @bitOffsetOf(arch.compute.Mode.Shift, "left_xor_swap_halves"),
            }, .{})
            .label("", .{})
            .turn_at(mode_cols.push())
            .turn_and_end_at(self.shift_j_conditioning.left_side("swap halves"));

        const j_conditioning_j_end = self.shift_j_conditioning.left_side("");
        _ = j_wire.end_at(j_conditioning_j_end.x());
        _ = self.shift_j_joiner.top().attach_to_offset(j_conditioning_j_end.y(), -10);

        mode_cols.interface.flip();

        const before_shift_cols = self.d.columns();
        _ = before_shift_cols.center().attach_between(self.shift_cin.right(), self.shift.left(), 0.5);

        const after_shift_cols = self.d.columns();
        _ = after_shift_cols.center().attach_between(self.shift.right(), self.shift_output_select.left(), 0.5);

        const cin_wire_v = self.shift_cin.right_side("")
            .wire_h(.{ })
            .turn_at(before_shift_cols.push());
        const cin_wire = cin_wire_v.turn_and_end_at(self.shift.left_side_upper("Cin"));

        _ = self.shift_cin.right_side("")
            .wire_h(.{})
            .turn_at(after_shift_cols.push())
            .turn_and_end_at(self.shift_output_select.left_side_upper("reverse bits"));

        _ = self.shift_output_select.left_side("")
            .wire_h(.{ .dir = .junction_end })
            .turn_at(after_shift_cols.push())
            .turn_at_offset(self.shift.top(), -50)
            .end_at(cin_wire_v.x());
        
        _ = self.shift_output_select.left_side("")
            .wire_h(.{ .bits = @bitSizeOf(arch.bus.J) })
            .bit_mark_at(0.75)
            .end_at(self.shift.right_side("").x());

        _ = self.shift.left_side_upper("")
            .wire_h(.{ .bits = @bitSizeOf(arch.bus.J) })
            .bit_mark_at(0.2)
            .end_at_mutable_point(self.shift_j_conditioning.right_side(""));
        _ = self.shift.left_side_lower("");
        _ = self.shift.left_side_lower("")
            .wire_h(.{ .bits = std.math.log2_int_ceil(usize, @bitSizeOf(arch.bus.J)) })
            .bit_mark_at(0.2)
            .end_at(self.shift_k_decoder.right_side("").x());

        _ = self.shift_k_decoder.left_side("")
            .wire_h(.{ .bits = @bitSizeOf(arch.bus.K) })
            .bit_mark()
            .label("K", .{})
            .end_at_mutable_point(self.input_box.right_side(""));

        _ = self.shift.right_side("");
        _ = self.shift.right_side("");

        _ = self.shift.right_side_lower("Cout")
            .wire_h(.{})
            .turn_at(after_shift_cols.get(1))
            .turn_and_end_at(self.shift_flag_logic.left_side(""));
        _ = self.shift.right_side_lower("overflow")
            .wire_h(.{})
            .turn_at(after_shift_cols.push())
            .turn_and_end_at(self.shift_flag_logic.left_side(""));
        _ = self.shift.right_side_lower("");

        _ = self.shift_k_decoder.right_side("");
        const k_over_31_wire = self.shift_flag_logic.left_side("")
            .wire_h(.{})
            .end_at_mutable_point(self.shift_k_decoder.right_side(">=32"));

        _ = self.shift_flag_logic.left_side("")
            .wire_h(.{})
            .end_at_mutable_point(self.shift_k_decoder.right_side("==32"));

        _ = self.shift_flag_logic.left_side("")
            .wire_h(.{})
            .end_at_mutable_point(self.shift_k_decoder.right_side("==0"));

        _ = self.shift_flag_logic.left_side("")
            .wire_h(.{ .dir = .junction_end })
            .turn_at(before_shift_cols.get(0))
            .end_at(cin_wire.y());

        _ = self.shift_j_conditioning.right_side_lower("==0")
            .wire_h(.{})
            .turn_at(before_shift_cols.push())
            .turn_and_end_at(self.shift_flag_logic.left_side(""));

        _ = self.shift_output_select.left_side_lower("force Cin")
            .wire_h(.{ .dir = .junction_end })
            .turn_at(after_shift_cols.get(0))
            .end_at(k_over_31_wire.y());


        _ = self.shift_output_select.right_side("")
            .wire_h(.{ .bits = @bitSizeOf(arch.bus.L) })
            .label("SHIFT_RESULT", .{ .alignment = .right })
            .bit_mark()
            .end_at_mutable_point(self.output_box.left_side(""));
        _ = self.shift_flag_logic.right_side("")
            .wire_h(.{ .bits = @bitSizeOf(arch.bus.L) })
            .label("SHIFT_C", .{ .alignment = .right })
            .end_at_mutable_point(self.output_box.left_side(""));
        shift_v_wire = self.shift_flag_logic.right_side("")
            .wire_h(.{ .bits = @bitSizeOf(arch.bus.L) })
            .label("SHIFT_V", .{ .alignment = .right })
            .end_at_mutable_point(self.output_box.left_side(""));

        before_shift_cols.interface.flip();
        after_shift_cols.interface.flip();
    }

    // count/ext
    var ext_v_wire: *zbox.Wire_H = undefined;
    {
        _ = self.cx_mode_joiner.left_side("")
            .wire_h(.{ .bits = @bitSizeOf(arch.compute.Mode), .class = "control" })
            .bit_mark()
            .label("MODE", .{})
            .end_at_mutable_point(self.input_box.right_side(""));

        const mode_invert_saturate = self.cx_mode_joiner.right_side("");
        const mode_saturate_dir = self.cx_mode_joiner.right_side("");

        const mode_cols = self.d.columns();
        _ = mode_cols.center().attach_between(self.cx_mode_joiner.right(), self.cx_j_xor.left(), 0.65);

        const jk_xor_cols = self.d.columns();
        _ = jk_xor_cols.center().attach_between(self.cx_j_xor.right(), self.cx_k_sat_left.left(), 0.5);

        _ = self.cx_j_xor.left_side("")
            .wire_h(.{ .class = "control" })
            .turn_at(mode_cols.push())
            .turn_and_end_at(self.cx_mode_joiner.right_side(""))
            .fmt_label("MODE[{d}]", .{
                @bitOffsetOf(arch.compute.Mode.Count_Extend, "invert_j")
            }, .{})
            .label("", .{});
        _ = self.cx_j_xor.left_side("")
            .wire_h(.{ .bits = @bitSizeOf(arch.bus.J) })
            .bit_mark()
            .label("J", .{})
            .end_at_mutable_point(self.input_box.right_side(""));

        _ = self.cx_k_xor.left_side("")
            .wire_h(.{ .class = "control" })
            .turn_at(mode_cols.push())
            .turn_and_end_at(self.cx_mode_joiner.right_side(""))
            .fmt_label("MODE[{d}]", .{
                @bitOffsetOf(arch.compute.Mode.Count_Extend, "invert_k")
            }, .{})
            .label("", .{});
        _ = self.cx_k_xor.left_side("")
            .wire_h(.{ .bits = @bitSizeOf(arch.bus.K) })
            .bit_mark()
            .label("K", .{})
            .end_at_mutable_point(self.input_box.right_side(""));

        const k_xor_wire = self.cx_k_xor.right_side("")
            .wire_h(.{ .bits = @bitSizeOf(arch.bus.K) })
            .bit_mark_at(0.25)
            .end_at_mutable_point(self.cx_k_sat_left.left_side(""));

        const k_xor_wire_2 = self.cx_k_sat_right.left_side("")
            .wire_h(.{ .bits = @bitSizeOf(arch.bus.K), .dir = .junction_end })
            .turn_at(jk_xor_cols.push())
            .end_at(k_xor_wire.y());


        _ = self.cx_k_sat_left.right_side("")
            .wire_h(.{ .bits = @bitSizeOf(arch.bus.K) })
            .bit_mark()
            .end_at_mutable_point(self.cx_k_sat_mux.left_side(""));

        _ = self.cx_k_sat_right.right_side("")
            .wire_h(.{ .bits = @bitSizeOf(arch.bus.K) })
            .bit_mark()
            .end_at_mutable_point(self.cx_k_sat_mux.left_side(""));


        _ = self.cx_k_sat_mux.top_side("")
            .wire_v(.{ .class = "control" })
            .turn_and_end_at(mode_saturate_dir)
            .fmt_label("MODE[{d}]", .{
                @bitOffsetOf(arch.compute.Mode.Count_Extend, "saturate")
            }, .{})
            .label("", .{});

        _ = self.cx_k_sat_xor.left_side("")
            .wire_h(.{ .class = "control" })
            .turn_between(self.cx_k_sat_mux.right(), self.cx_k_sat_xor.left(), 0.5)
            .turn_and_end_at(mode_invert_saturate)
            .fmt_label("MODE[{d}]", .{
                @bitOffsetOf(arch.compute.Mode.Count_Extend, "invert_saturated")
            }, .{})
            .label("", .{});

        _ = self.cx_k_sat_xor.left_side("")
            .wire_h(.{ .bits = @bitSizeOf(arch.bus.K) })
            .bit_mark()
            .end_at(self.cx_k_sat_mux.right());

        const j_wire = self.cx_j_xor.right_side("")
            .wire_h(.{ .bits = @bitSizeOf(arch.bus.J) })
            .bit_mark()
            .end_at(self.cx_j_and_sat.left_side("").x());
        const k_mask_wire = self.cx_k_sat_xor.right_side("")
            .wire_h(.{ .bits = @bitSizeOf(arch.bus.K), .dir = .invert_end })
            .end_at(self.cx_sx_mux.left_side("").x());
        _ = self.cx_sx_mux.left_side("");
        _ = self.cx_sx_mux.left_side("")
            .wire_h(.{ .bits = @bitSizeOf(arch.bus.K) })
            .label("0", .{})
            .length(-40);

        _ = self.cx_j_and_sat.left_side("")
            .wire_h(.{ .bits = @bitSizeOf(arch.bus.K), .dir = .junction_end })
            .length(-40)
            .turn()
            .bit_mark()
            .end_at(k_mask_wire.y());

        const masked_j_wire = self.cx_j_and_sat.right_side("")
            .wire_h(.{ .bits = @bitSizeOf(arch.bus.J) })
            .end_at(self.cx_sx_or.left_side("").x());
        _ = self.cx_sx_or.left_side("")
            .wire_h(.{ .bits = @bitSizeOf(arch.bus.K) })
            .length(-20)
            .turn()
            .bit_mark()
            .turn_and_end_at(self.cx_sx_mux.right_side(""));

        const ext_wire = self.cx_sx_or.right_side("")
            .wire_h(.{ .bits = @bitSizeOf(arch.bus.L) })
            .bit_mark()
            .label("EXT_RESULT", .{ .alignment = .right })
            .end_at_mutable_point(self.output_box.left_side(""));

        


        _ = self.cx_ext_xor.left_side("")
            .wire_h(.{ .bits = @bitSizeOf(arch.bus.J), .dir = .junction_end })
            .bit_mark_at(0.25)
            .turn_at_offset(self.cx_k_sat_xor.right(), 20)
            .end_at(j_wire.y());
        _ = self.cx_ext_xor.left_side("")
            .wire_h(.{ .bits = @bitSizeOf(arch.bus.J), .dir = .junction_end })
            .length(-40)
            .turn()
            .bit_mark()
            .end_at(ext_wire.y());
        ext_v_wire = self.cx_ext_xor.right_side("")
            .wire_h(.{ .bits = @bitSizeOf(arch.bus.J) })
            .length(20)
            .small_box("!=0", .right)
            .label("EXT_V", .{ .alignment = .right })
            .end_at_mutable_point(self.output_box.left_side(""));

        _ = self.cx_sx_decoder.left_side("")
            .wire_h(.{ .bits = @bitSizeOf(arch.bus.K), .dir = .junction_end })
            .turn_and_end_at(k_xor_wire_2.origin());

        _ = self.cx_sx_decoder.left_side("")
            .wire_h(.{ .bits = @bitSizeOf(arch.bus.J), .dir = .junction_end })
            .turn_at(jk_xor_cols.push())
            .end_at(j_wire.y());

        _ = self.cx_sx_decoder.left_side("")
            .wire_h(.{ .class = "control" })
            .turn_at(mode_cols.push())
            .turn_and_end_at(self.cx_mode_joiner.right_side(""))
            .fmt_label("MODE[{d}]", .{
                @bitOffsetOf(arch.compute.Mode.Count_Extend, "sign_extend")
            }, .{})
            .label("", .{});

        _ = self.cx_sx_decoder.right_side("")
            .wire_h(.{})
            .turn_and_end_at(self.cx_sx_mux.bottom_side(""));

        _ = self.cx_j_count.left_side("")
            .wire_h(.{ .bits = @bitSizeOf(arch.bus.J), .dir = .junction_end })
            .length(-20)
            .turn()
            .bit_mark_at(0.33)
            .end_at(masked_j_wire.y());
        _ = self.cx_j_count.right_side("")
            .wire_h(.{ .bits = std.math.log2_int_ceil(usize, @bitSizeOf(arch.bus.L) + 1) })
            .bit_mark()
            .label("COUNT_RESULT", .{ .alignment = .right })
            .end_at_mutable_point(self.output_box.left_side(""));

        mode_cols.interface.flip();
        jk_xor_cols.interface.flip();
    }

    // AT
    var page_fault: *zbox.Wire_H = undefined;
    var page_align_fault: *zbox.Wire_H = undefined;
    var align_fault: *zbox.Wire_H = undefined;
    var pipe_fault: *zbox.Wire_H = undefined;
    var access_fault: *zbox.Wire_H = undefined;
    {
        _ = self.at_page_joiner.left_side("")
            .wire_h(.{ .bits = @bitSizeOf(arch.addr.Page) })
            .bit_mark()
            .label("PAGE", .{})
            .end_at_mutable_point(self.input_box.right_side(""));


        const page_wire = self.at_frame_mux.left_side("")
            .wire_h(.{ .bits = @bitSizeOf(arch.addr.Frame) })
            .bit_mark()
            .fmt_label("PAGE[{d}:0]", .{
                @bitSizeOf(arch.addr.Frame) - 1,
            }, .{})
            .label("", .{})
            .end_at(self.at_page_joiner.right_side("").x());

        _ = self.at_page_joiner.y().attach_to_offset(page_wire.y(), 20);

        const page_joiner_tag = self.at_page_joiner.right_side("");

        _ = self.at_frame_mux.right_side("")
            .wire_h(.{ .bits = @bitSizeOf(arch.addr.Frame) })
            .bit_mark()
            .label("FRAME", .{ .alignment = .right })
            .end_at_mutable_point(self.output_box.left_side(""));

        _ = self.at_primary.left_side("")
            .wire_h(.{ .bits = @bitSizeOf(arch.addr.translation.Entry.ASN6), .class = "reg" })
            .bit_mark()
            .label("ASN6", .{})
            .end_at_mutable_point(self.input_box.right_side(""));
        _ = self.at_primary.left_side("")
            .wire_h(.{ .bits = @bitSizeOf(arch.addr.translation.Entry.Group) })
            .bit_mark()
            .label("AT_GROUP", .{})
            .end_at_mutable_point(self.input_box.right_side(""));
        const slot_wire = self.at_primary.left_side("")
            .wire_h(.{ .bits = @bitSizeOf(arch.addr.Page.Slot) })
            .bit_mark()
            .turn_between(self.at_page_joiner.right(), self.at_primary.left(), 0.5);

        _ = slot_wire.turn()
            .fmt_label("PAGE[{d}:0]", .{
                @bitSizeOf(arch.addr.Page.Slot) - 1
            }, .{})
            .label("", .{})
            .end_at_point(self.at_page_joiner.right_side(""));

        _ = self.at_secondary.left_side("")
            .wire_h(.{ .bits = @bitSizeOf(arch.addr.translation.Entry.ASN6), .class = "reg" })
            .bit_mark()
            .label("ASN6", .{})
            .end_at_mutable_point(self.input_box.right_side(""));
        _ = self.at_secondary.left_side("")
            .wire_h(.{ .bits = @bitSizeOf(arch.addr.translation.Entry.Group) })
            .bit_mark()
            .label("AT_GROUP", .{})
            .end_at_mutable_point(self.input_box.right_side(""));
        _ = self.at_secondary.left_side("")
            .wire_h(.{ .bits = @bitSizeOf(arch.addr.Page.Slot), .dir = .junction_end })
            .bit_mark()
            .turn_and_end_at(slot_wire.origin());

        _ = self.at_primary.right_side("")
            .wire_h(.{ .bits = @bitSizeOf(arch.addr.translation.Entry) })
            .bit_mark()
            .label("PE", .{ .alignment = .right })
            .end_at(self.at_pri_joiner.left());
        _ = self.at_secondary.right_side("")
            .wire_h(.{ .bits = @bitSizeOf(arch.addr.translation.Entry) })
            .bit_mark()
            .label("SE", .{ .alignment = .right })
            .end_at(self.at_sec_joiner.left());

        _ = self.at_pri_joiner.right_side("")
            .wire_h(.{ .bits = @bitSizeOf(arch.addr.translation.Entry) })
            .fmt_label("SE[{d}:0]", .{
                @bitSizeOf(arch.addr.translation.Entry) - 1
            }, .{})
            .bit_mark()
            .label("", .{})
            .end_at(self.at_entry_swap.left_side("").x());

        const matcher_cols = self.d.columns();
        _ = matcher_cols.center().attach_between(self.at_pri_joiner.right(), self.at_matcher.left(), 0.8);

        _ = self.at_matcher.left_side("PE tag")
            .wire_h(.{ .bits = @bitSizeOf(arch.addr.Page.Tag) })
            .turn_at(matcher_cols.push())
            .turn_and_end_at(self.at_pri_joiner.right_side(""))
            .fmt_label("PE[{d}:{d}]", .{
                @bitOffsetOf(arch.addr.translation.Entry, "tag") + @bitSizeOf(arch.addr.Page.Tag) - 1,
                @bitOffsetOf(arch.addr.translation.Entry, "tag"),
            }, .{})
            .bit_mark_at(0.25)
            .label("", .{});
        _ = self.at_matcher.left_side("PE present")
            .wire_h(.{})
            .turn_at(matcher_cols.push())
            .turn_and_end_at(self.at_pri_joiner.right_side(""))
            .fmt_label("PE[{d}]", .{
                @bitOffsetOf(arch.addr.translation.Entry, "present"),
            }, .{})
            .label("", .{});

        _ = self.at_matcher.left_side("VA tag")
            .wire_h(.{ .bits = @bitSizeOf(arch.addr.Page.Tag) })
            .bit_mark()
            .turn_at_offset(slot_wire.x(), 20)
            .turn_and_end_at(page_joiner_tag)
            .fmt_label("PAGE[{d}:{d}]", .{
                @bitOffsetOf(arch.addr.Page, "tag") + @bitSizeOf(arch.addr.Page.Tag) - 1,
                @bitOffsetOf(arch.addr.Page, "tag"),
            }, .{})
            .label("", .{});

        _ = self.at_matcher.left_side("")
            .wire_h(.{ .bits = @bitSizeOf(arch.addr.translation.Op), .class = "control" })
            .label("AT_OP", .{})
            .bit_mark()
            .end_at_mutable_point(self.input_box.right_side(""));

        _ = self.at_matcher.left_side("SE tag")
            .wire_h(.{ .bits = @bitSizeOf(arch.addr.Page.Tag) })
            .turn_at(matcher_cols.get(1))
            .turn_and_end_at(self.at_sec_joiner.right_side(""))
            .fmt_label("SE[{d}:{d}]", .{
                @bitOffsetOf(arch.addr.translation.Entry, "tag") + @bitSizeOf(arch.addr.Page.Tag) - 1,
                @bitOffsetOf(arch.addr.translation.Entry, "tag"),
            }, .{})
            .bit_mark_at(0.25)
            .label("", .{});
        _ = self.at_matcher.left_side("SE present")
            .wire_h(.{})
            .turn_at(matcher_cols.get(0))
            .turn_and_end_at(self.at_sec_joiner.right_side(""))
            .fmt_label("SE[{d}]", .{
                @bitOffsetOf(arch.addr.translation.Entry, "present"),
            }, .{})
            .label("", .{});

        _ = self.at_sec_joiner.right_side("")
            .wire_h(.{ .bits = @bitSizeOf(arch.addr.translation.Entry) })
            .fmt_label("SE[{d}:0]", .{
                @bitSizeOf(arch.addr.translation.Entry) - 1
            }, .{})
            .bit_mark()
            .label("", .{})
            .turn_at_offset(self.at_entry_swap.left(), -20)
            .turn_and_end_at(self.at_entry_swap.left_side(""));

        _ = self.at_matcher.right_side("swap")
            .wire_h(.{})
            .turn_and_end_at(self.at_entry_swap.bottom_side(""));

        const me_wire = self.at_entry_swap.right_side("")
            .wire_h(.{ .bits = @bitSizeOf(arch.addr.translation.Entry) })
            .label("AT_MATCHING_ENTRY", .{ .alignment = .right })
            .bit_mark_at(0.75)
            .end_at_mutable_point(self.output_box.left_side(""));
        _ = self.at_entry_swap.right_side("")
            .wire_h(.{ .bits = @bitSizeOf(arch.addr.translation.Entry) })
            .label("AT_OTHER_ENTRY", .{ .alignment = .right })
            .bit_mark_at(0.75)
            .end_at_mutable_point(self.output_box.left_side(""));

        _ = self.at_me_joiner.left_side("")
            .wire_h(.{ .bits = @bitSizeOf(arch.addr.translation.Entry), .dir = .junction_end })
            .label("ME", .{ .alignment = .right })
            .turn_between(self.at_entry_swap.right(), self.at_me_joiner.left(), 0.5)
            .bit_mark()
            .end_at(me_wire.y());

        _ = self.at_me_joiner.right_side("")
            .wire_h(.{ .bits = @bitSizeOf(arch.addr.Frame) })
            .fmt_label("ME[{d}:{d}]", .{
                @bitOffsetOf(arch.addr.translation.Entry, "frame") + @bitSizeOf(arch.addr.Frame) - 1,
                @bitOffsetOf(arch.addr.translation.Entry, "frame"),
            }, .{})
            .label("", .{})
            .length(60)
            .turn()
            .turn_and_end_at(self.at_frame_mux.left_side(""));

        const access_wire = self.at_control_logic.left_side("access")
            .wire_h(.{ .bits = @bitSizeOf(arch.addr.translation.Access_Policy) })
            .fmt_label("ME[{d}:{d}]", .{
                @bitOffsetOf(arch.addr.translation.Entry, "access") + @bitSizeOf(arch.addr.translation.Access_Policy) - 1,
                @bitOffsetOf(arch.addr.translation.Entry, "access"),
            }, .{})
            .label("", .{})
            // .bit_mark()
            .end_at(self.at_me_joiner.right_side("").x());

        _ = self.at_me_joiner.y().attach_to(access_wire.y());

        _ = self.at_control_logic.left_side("pipe")
            .wire_h(.{ .bits = @bitSizeOf(arch.addr.translation.Pipeline_Policy) })
            .fmt_label("ME[{d}:{d}]", .{
                @bitOffsetOf(arch.addr.translation.Entry, "pipe") + @bitSizeOf(arch.addr.translation.Pipeline_Policy) - 1,
                @bitOffsetOf(arch.addr.translation.Entry, "pipe"),
            }, .{})
            .label("", .{})
            // .bit_mark()
            .end_at(self.at_me_joiner.right_side("").x());

        _ = self.at_control_logic.left_side("")
            .wire_h(.{})
            .turn_at(self.at_entry_swap.x())
            .turn_and_end_at(self.at_matcher.right_side("match"));

        _ = self.at_control_logic.left_side("");
        _ = self.at_control_logic.left_side("");
        _ = self.at_control_logic.left_side("");
        _ = self.at_control_logic.left_side("");
        _ = self.at_control_logic.left_side("");

        _ = self.at_control_logic.left_side("")
            .wire_h(.{ .class = "reg" })
            .label("FLAG_AT_ENABLE", .{})
            .end_at_mutable_point(self.input_box.right_side(""));
        _ = self.at_control_logic.left_side("")
            .wire_h(.{ .class = "reg" })
            .label("FLAG_BUS_OVERRIDE", .{})
            .end_at_mutable_point(self.input_box.right_side(""));
        _ = self.at_control_logic.left_side("")
            .wire_h(.{ .class = "reg" })
            .label("FLAG_SUPER", .{})
            .end_at_mutable_point(self.input_box.right_side(""));

        _ = self.at_control_logic.left_side("")
            .wire_h(.{ .bits = @bitSizeOf(arch.addr.translation.Op), .class = "control" })
            .bit_mark()
            .label("AT_OP", .{})
            .end_at_mutable_point(self.input_box.right_side(""));
        _ = self.at_control_logic.left_side("")
            .wire_h(.{ .bits = @bitSizeOf(arch.addr.Space), .class = "control" })
            .bit_mark()
            .label("SPACE", .{})
            .end_at_mutable_point(self.input_box.right_side(""));
        _ = self.at_control_logic.left_side("")
            .wire_h(.{ .bits = @bitSizeOf(arch.bus.D.Width), .class = "control" })
            .bit_mark()
            .label("WIDTH", .{})
            .end_at_mutable_point(self.input_box.right_side(""));
        _ = self.at_control_logic.left_side("")
            .wire_h(.{ .bits = @bitSizeOf(arch.reg.sr2.Index), .class = "control" })
            .bit_mark()
            .label("SR2WI", .{})
            .end_at_mutable_point(self.input_box.right_side(""));
        _ = self.at_control_logic.left_side("")
            .wire_h(.{ .bits = @bitSizeOf(arch.reg.sr.Write_Source), .class = "control" })
            .bit_mark()
            .label("SR2WSRC", .{})
            .end_at_mutable_point(self.input_box.right_side(""));

        _ = self.at_control_logic.left_side("")
            .wire_h(.{ .bits = @bitSizeOf(arch.Pipeline) })
            .bit_mark()
            .label("PIPE", .{})
            .end_at_mutable_point(self.input_box.right_side(""));

        const offset_wire = self.at_control_logic.left_side("")
            .wire_h(.{ .bits = @bitSizeOf(arch.addr.Page.Offset) })
            .bit_mark()
            .label("OFFSET", .{})
            .end_at_mutable_point(self.input_box.right_side(""));

        _ = self.at_control_logic.right_side("")
            .wire_h(.{})
            .turn_and_end_at(self.at_frame_mux.bottom_side(""));

        page_fault = self.at_control_logic.right_side("")
            .wire_h(.{})
            .label("PAGE_FAULT", .{ .alignment = .right })
            .end_at_mutable_point(self.output_box.left_side(""));
        page_align_fault = self.at_control_logic.right_side("")
            .wire_h(.{})
            .label("PAGE_ALIGN_FAULT", .{ .alignment = .right })
            .end_at_mutable_point(self.output_box.left_side(""));
        align_fault = self.at_control_logic.right_side("")
            .wire_h(.{})
            .label("ALIGN_FAULT", .{ .alignment = .right })
            .end_at_mutable_point(self.output_box.left_side(""));
        access_fault = self.at_control_logic.right_side("")
            .wire_h(.{})
            .label("ACCESS_FAULT", .{ .alignment = .right })
            .end_at_mutable_point(self.output_box.left_side(""));
        pipe_fault = self.at_control_logic.right_side("")
            .wire_h(.{})
            .label("PIPE_FAULT", .{ .alignment = .right })
            .end_at_mutable_point(self.output_box.left_side(""));

        _ = self.at_control_logic.right_side("")
            .wire_h(.{})
            .label("AT_ADD_SUPER", .{ .alignment = .right })
            .end_at_mutable_point(self.output_box.left_side(""));
        _ = self.at_control_logic.right_side("")
            .wire_h(.{})
            .label("AT_REMOVE_SUPER", .{ .alignment = .right })
            .end_at_mutable_point(self.output_box.left_side(""));

        _ = self.at_control_logic.right_side("")
            .wire_h(.{})
            .label("UBA", .{ .alignment = .right })
            .end_at_mutable_point(self.output_box.left_side(""));
        _ = self.at_control_logic.right_side("")
            .wire_h(.{})
            .label("LBA", .{ .alignment = .right })
            .end_at_mutable_point(self.output_box.left_side(""));
        _ = self.at_control_logic.right_side("")
            .wire_h(.{})
            .label("UBB", .{ .alignment = .right })
            .end_at_mutable_point(self.output_box.left_side(""));
        _ = self.at_control_logic.right_side("")
            .wire_h(.{})
            .label("LBB", .{ .alignment = .right })
            .end_at_mutable_point(self.output_box.left_side(""));

        _ = self.offset_joiner.left_side("")
            .wire_h(.{ .bits = @bitSizeOf(arch.addr.Page.Offset), .dir = .junction_end })
            .label("OFFSET", .{ .alignment = .right })
            .bit_mark()
            .turn_at(device_column_0)
            .end_at(offset_wire.y());

        const ab_wire = self.offset_joiner.right_side("")
            .wire_h(.{ .bits = @bitSizeOf(arch.addr.Frame.Word_Offset) })
            .fmt_label("OFFSET[{d}:2]", .{
                1 + @bitSizeOf(arch.addr.Frame.Word_Offset),
            }, .{})
            .label("AB", .{ .alignment = .right })
            .bit_mark()
            .end_at_mutable_point(self.output_box.left_side(""));

        _ = self.offset_adder.left_side_upper("")
            .wire_h(.{ .bits = @bitSizeOf(arch.addr.Frame.Word_Offset), .dir = .junction_end })
            .length(-40)
            .turn()
            .bit_mark()
            .end_at(ab_wire.y());
        _ = self.offset_adder.left_side_lower("")
            .wire_h(.{ .bits = @bitSizeOf(arch.addr.Frame.Word_Offset) })
            .bit_mark()
            .length(-60)
            .turn()
            .length(-20)
            .small_box("zx", .up)
            .change_bits(1)
            .turn_and_end_at(self.offset_joiner.right_side(""))
            .label("OFFSET[1]", .{})
            .label("", .{});

        _ = self.offset_adder.right_side("")
            .wire_h(.{ .bits = @bitSizeOf(arch.addr.Frame.Word_Offset) })
            .label("AA", .{ .alignment = .right })
            .bit_mark()
            .end_at_mutable_point(self.output_box.left_side(""));

        matcher_cols.interface.flip();
    }

    // fault logic
    {
        _ = self.fault_logic.left_side("")
            .wire_h(.{ .dir = .junction_end })
            .length(-20)
            .turn()
            .end_at(ext_v_wire.y());
        _ = self.fault_logic.left_side("")
            .wire_h(.{ .dir = .junction_end })
            .length(-40)
            .turn()
            .end_at(shift_v_wire.y());
        _ = self.fault_logic.left_side("")
            .wire_h(.{ .dir = .junction_end })
            .length(-60)
            .turn()
            .end_at(mult_v_wire.y());
        _ = self.fault_logic.left_side("")
            .wire_h(.{ .dir = .junction_end })
            .length(-80)
            .turn()
            .end_at(alu_v_wire.y());
        _ = self.fault_logic.left_side("")
            .wire_h(.{ .bits = @bitSizeOf(arch.compute.Unit), .class = "control" })
            .bit_mark()
            .label("UNIT", .{})
            .end_at_mutable_point(self.input_box.right_side(""));
        _ = self.fault_logic.left_side("")
            .wire_h(.{ .bits = @bitSizeOf(arch.Special_Op), .class = "control" })
            .bit_mark()
            .label("SPECIAL", .{})
            .end_at_mutable_point(self.input_box.right_side(""));
        _ = self.fault_logic.right_side("")
            .wire_h(.{})
            .label("OVERFLOW_FAULT", .{ .alignment = .right })
            .end_at_mutable_point(self.output_box.left_side(""));
        _ = self.fault_logic.right_side("")
            .wire_h(.{})
            .label("INSN_FAULT", .{ .alignment = .right })
            .end_at_mutable_point(self.output_box.left_side(""));
        _ = self.fault_logic.left_side("")
            .wire_h(.{})
            .label("KRI_UNDERFLOW_FAULT", .{})
            .end_at_mutable_point(self.input_box.right_side(""));
        _ = self.fault_logic.left_side("")
            .wire_h(.{ .class = "control" })
            .label("GPRW", .{})
            .end_at_mutable_point(self.input_box.right_side(""));
        _ = self.fault_logic.left_side("")
            .wire_h(.{ .class = "control" })
            .label("TIW", .{})
            .end_at_mutable_point(self.input_box.right_side(""));
        _ = self.fault_logic.left_side("")
            .wire_h(.{})
            .label("WI_UNDERFLOW", .{})
            .end_at_mutable_point(self.input_box.right_side(""));
        _ = self.fault_logic.left_side("")
            .wire_h(.{})
            .label("WI_OVERFLOW", .{})
            .end_at_mutable_point(self.input_box.right_side(""));

        _ = self.fault_logic.right_side("")
            .wire_h(.{})
            .label("RS_UNDERFLOW_FAULT", .{ .alignment = .right })
            .end_at_mutable_point(self.output_box.left_side(""));
        _ = self.fault_logic.right_side("")
            .wire_h(.{})
            .label("RS_OVERFLOW_FAULT", .{ .alignment = .right })
            .end_at_mutable_point(self.output_box.left_side(""));
        _ = self.fault_logic.right_side("");
        _ = self.fault_logic.right_side("")
            .wire_h(.{})
            .label("ANY_FAULT", .{ .alignment = .right })
            .end_at_mutable_point(self.output_box.left_side(""));



        _ = self.fault_logic.left_side("")
            .wire_h(.{ .bits = @bitSizeOf(arch.addr.translation.Op), .class = "control" })
            .bit_mark()
            .label("AT_OP", .{})
            .end_at_mutable_point(self.input_box.right_side(""));
        _ = self.fault_logic.left_side("")
            .wire_h(.{ .bits = @bitSizeOf(arch.bus.D.Source), .class = "control" })
            .bit_mark()
            .label("DSRC", .{})
            .end_at_mutable_point(self.input_box.right_side(""));
        _ = self.fault_logic.left_side("")
            .wire_h(.{ .class = "reg" })
            .label("FLAG_BUS_OVERRIDE", .{})
            .end_at_mutable_point(self.input_box.right_side(""));

        _ = self.fault_logic.right_side("");
        _ = self.fault_logic.right_side("")
            .wire_h(.{})
            .label("READ", .{ .alignment = .right })
            .end_at_mutable_point(self.output_box.left_side(""));
        _ = self.fault_logic.right_side("")
            .wire_h(.{})
            .label("WRITE", .{ .alignment = .right })
            .end_at_mutable_point(self.output_box.left_side(""));


        _ = self.fault_logic.left_side("")
            .wire_h(.{ .dir = .junction_end })
            .length(-100)
            .turn()
            .end_at(page_fault.y());
        _ = self.fault_logic.left_side("")
            .wire_h(.{ .dir = .junction_end })
            .length(-80)
            .turn()
            .end_at(page_align_fault.y());
        _ = self.fault_logic.left_side("")
            .wire_h(.{ .dir = .junction_end })
            .length(-60)
            .turn()
            .end_at(align_fault.y());
        _ = self.fault_logic.left_side("")
            .wire_h(.{ .dir = .junction_end })
            .length(-40)
            .turn()
            .end_at(access_fault.y());
        _ = self.fault_logic.left_side("")
            .wire_h(.{ .dir = .junction_end })
            .length(-20)
            .turn()
            .end_at(pipe_fault.y());
    }

    // microcode
    var last_uc_wire: ?*zbox.Wire_H = null;
    inline for (@typeInfo(arch.microcode.Transact_Microcode_Entry).@"struct".fields) |field| {
        if (!std.mem.startsWith(u8, field.name, "_")) {
            const label = std.ascii.allocUpperString(self.d.state.arena.allocator(), field.name) catch @panic("OOM");

            const offset = @bitOffsetOf(arch.microcode.Transact_Microcode_Entry, field.name);
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
        }
    }
    const uca_wire = self.microcode.left_side(std.fmt.comptimePrint("A[{d}:0]", .{
        @bitSizeOf(arch.microcode.Address) - 1
    })).wire_h(.{ .bits = @bitSizeOf(arch.microcode.Address) })
        .bit_mark()
        .label("UCA", .{})
        .end_at_mutable_point(self.input_box.right_side(""));

    // v_extents
    ///////////////////////////////////////////////////////////////////

    _ = self.v_extents.input.min.attach_to(alu_mode_wire.y());
    _ = self.v_extents.input.max.attach_to(uca_wire.y());

    _ = self.v_extents.output.min.attach_to(alu_c_wire.y());
    _ = self.v_extents.output.max.attach_to(last_uc_wire.?.y());
}

const X_Ref = zbox.X_Ref;
const Y_Ref = zbox.Y_Ref;
const Box = zbox.Box;
const Drawing = zbox.Drawing;

const V_Extents = @import("V_Extents.zig");
const zbox = @import("zbox");
const arch = @import("arch");
const std = @import("std");
