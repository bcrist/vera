d: *Drawing,
input_box: *Box,
output_box: *Box,
v_extents: V_Extents,

microcode: *Box,
sequencer: *Box,
insn_decode: *Box,

wi_adder: *Box,
kri_subtractor: *Box,

uc_flag_mux: *Box,
ucs_mux: *Box,
uca_joiner: *Box,

pub fn init(d: *Drawing, input_box: *Box, output_box: *Box) @This() {
    return .{
        .d = d,
        .input_box = input_box,
        .output_box = output_box,
        .v_extents = .init(d),

        .microcode = d.box(.{ .label = "\u{B5}code" }),
        .sequencer = d.box(.{ .label = "Sequencer" }),
        .insn_decode = d.box(.{ .label = "Instruction\nDecoder\n\n" }),

        .wi_adder = d.box(.{ .label = "+", .shape = .alu }),
        .kri_subtractor = d.box(.{ .label = "-", .shape = .alu }),

        .uc_flag_mux = d.box(.{ .shape = .mux }),
        .ucs_mux = d.box(.{ .shape = .mux }),
        .uca_joiner = d.box(.{ .shape = .joiner_h }),
    };
}

pub fn config(self: *@This()) void {
    const device_column_1 = self.d.some_x().attach_to_offset(self.input_box.right(), 220);
    const device_column_2 = self.d.some_x().attach_to_offset(device_column_1, 350);
    const device_column_3 = self.d.some_x().attach_to_offset(device_column_2, 290);
    _ = self.output_box.left().attach_to_offset(device_column_3, 200);

    const between_column_1_and_2 = self.d.some_x().attach_between(device_column_1, device_column_2, 0.6);
    const between_column_2_and_3 = self.d.some_x().attach_between(device_column_2, device_column_3, 0.35);

    _ = self.microcode.width(200).x().attach_to(device_column_3);
    _ = self.sequencer.width(150).x().attach_to(device_column_1);
    _ = self.insn_decode.width(200).x().attach_to(device_column_1);
    _ = self.wi_adder.x().attach_to(device_column_2);
    _ = self.kri_subtractor.x().attach_to(device_column_2);
    _ = self.uc_flag_mux.x().attach_to(device_column_2);
    _ = self.ucs_mux.width(60).x().attach_to(device_column_2);
    _ = self.uca_joiner.x().attach_to(between_column_2_and_3);

    _ = self.wi_adder.height(100).top().anchor_at(0);
    _ = self.insn_decode.height(365).top().attach_to(self.wi_adder.y());
    _ = self.kri_subtractor.height(100).top().attach_to_offset(self.wi_adder.bottom(), 60);
    _ = self.uc_flag_mux.top().attach_to_offset(self.kri_subtractor.bottom(), 120);
    _ = self.microcode.height(180).top().attach_to_offset(self.kri_subtractor.bottom(), 350);
    _ = self.ucs_mux.top().attach_to_offset(self.uc_flag_mux.bottom(), 100);
    _ = self.uca_joiner.height(220).y().attach_between(self.ucs_mux.y(), self.uc_flag_mux.y(), 0.5);
    _ = self.sequencer.height(320).top().attach_to(self.ucs_mux.bottom());

    // wires
    ///////////////////////////////////////////////////////////////////

    // wi_adder
    const ti_wire = self.wi_adder.left_side_upper("")
        .wire_h(.{ .bits = @bitSizeOf(arch.reg.gpr.Write_Index), .class = "reg" })
        .bit_mark()
        .continue_at(between_column_1_and_2)
        .label("TI", .{})
        .end_at_mutable_point(self.input_box.right_side(""));

    _ = self.wi_adder.left_side_lower("")
        .wire_h(.{ .bits = @bitSizeOf(arch.reg.gpr.Write_Index) })
        .bit_mark()
        .length(-50)
        .small_box("sx", .left)
        .change_bits(@bitSizeOf(arch.reg.gpr.Write_Index_Offset))
        .bit_mark()
        .label("WIO", .{})
        .end_at_mutable_point(self.insn_decode.right_side(std.fmt.comptimePrint("D[{d}:{d}]", .{
            @bitOffsetOf(arch.insn_decode.Result, "wio") + @bitSizeOf(arch.reg.gpr.Write_Index_Offset) - 1,
            @bitOffsetOf(arch.insn_decode.Result, "wio")
        })));

    const wi_overflow_wire = self.wi_adder.top_side("")
        .wire_h(.{})
        .label("WI_OVERFLOW", .{ .alignment = .right })
        .end_at_mutable_point(self.output_box.left_side(""));

    _ = self.wi_adder.right_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.reg.gpr.Write_Index) })
        .bit_mark()
        .label("WI", .{ .alignment = .right })
        .end_at_mutable_point(self.output_box.left_side(""));

    _ = self.wi_adder.bottom_side("")
        .wire_h(.{})
        .label("WI_UNDERFLOW", .{ .alignment = .right })
        .end_at_mutable_point(self.output_box.left_side(""));

    // kri_subtractor
    _ = self.kri_subtractor.left_side_upper("")
        .wire_h(.{ .bits = @bitSizeOf(arch.bus.K.Read_Index), .class = "reg", .dir = .junction_end })
        .bit_mark()
        .turn_at(between_column_1_and_2)
        .end_at(ti_wire.y());

    _ = self.kri_subtractor.left_side_lower("")
        .wire_h(.{ .bits = @bitSizeOf(arch.bus.K.Read_Index) })
        .bit_mark()
        .length(-50)
        .small_box("zx", .left)
        .change_bits(@bitSizeOf(arch.bus.K.Read_Index_Offset))
        .bit_mark()
        .label("KRIO", .{})
        .end_at_mutable_point(self.insn_decode.right_side(std.fmt.comptimePrint("D[{d}:{d}]", .{
            @bitOffsetOf(arch.insn_decode.Result, "krio") + @bitSizeOf(arch.bus.K.Read_Index_Offset) - 1,
            @bitOffsetOf(arch.insn_decode.Result, "krio")
        })))
        .y().intersection_with(between_column_1_and_2)
        .wire_v(.{ .bits = @bitSizeOf(arch.bus.K.Read_Index_Offset), .dir = .junction_begin })
        .turn_between(self.kri_subtractor.bottom(), self.uc_flag_mux.top(), 0.3)
        .bit_mark()
        .label("KRIO", .{ .alignment = .right })
        .end_at_mutable_point(self.output_box.left_side(""));

    _ = self.kri_subtractor.right_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.bus.K.Read_Index) })
        .bit_mark()
        .label("KRI", .{ .alignment = .right })
        .end_at_mutable_point(self.output_box.left_side(""));

    _ = self.kri_subtractor.bottom_side("")
        .wire_h(.{})
        .label("KRI_UNDERFLOW", .{ .alignment = .right })
        .end_at_mutable_point(self.output_box.left_side(""));

    // uc_flag_mux
    _ = self.uc_flag_mux.top_side("")
        .wire_v(.{ .bits = @bitSizeOf(arch.insn_decode.CV_Mode) })
        .turn_between(self.uc_flag_mux.top(), self.kri_subtractor.bottom(), 0.3)
        .bit_mark()
        .label("CV_MODE", .{})
        .end_at_mutable_point(self.insn_decode.right_side(std.fmt.comptimePrint("D[{d}:{d}]", .{
            @bitOffsetOf(arch.insn_decode.Result, "cv") + @bitSizeOf(arch.insn_decode.CV_Mode) - 1,
            @bitOffsetOf(arch.insn_decode.Result, "cv")
        })));

    _ = self.uc_flag_mux.left_side(@tagName(arch.insn_decode.CV_Mode.zero))
        .wire_h(.{})
        .length(-50)
        .label("0", .{});
    _ = self.uc_flag_mux.left_side(@tagName(arch.insn_decode.CV_Mode.one))
        .wire_h(.{})
        .length(-50)
        .label("1", .{});
    _ = self.uc_flag_mux.left_side(@tagName(arch.insn_decode.CV_Mode.c))
        .wire_h(.{ .class = "reg" })
        .label("FLAG_C", .{})
        .end_at_mutable_point(self.input_box.right_side(""));
    _ = self.uc_flag_mux.left_side(@tagName(arch.insn_decode.CV_Mode.v))
        .wire_h(.{ .class = "reg" })
        .label("FLAG_V", .{})
        .end_at_mutable_point(self.input_box.right_side(""));

    // microcode
    inline for (@typeInfo(arch.microcode.Setup_Microcode_Entry).@"struct".fields) |field| {
        if (!std.mem.startsWith(u8, field.name, "_")) {
            const label = std.ascii.allocUpperString(self.d.state.arena.allocator(), field.name) catch @panic("OOM");

            const offset = @bitOffsetOf(arch.microcode.Setup_Microcode_Entry, field.name);
            const bits = @bitSizeOf(field.type);

            const data_bits = if (bits > 1) std.fmt.comptimePrint("D[{d}:{d}]", .{
                offset + bits - 1,
                offset,
            }) else std.fmt.comptimePrint("D{d}", .{ offset });

            const uc_wire = self.microcode.right_side(data_bits)
                .wire_h(.{ .bits = @bitSizeOf(field.type), .class = "control" })
                .label(label, .{ .alignment = .right })
                .end_at_mutable_point(self.output_box.left_side(""));

            if (@bitSizeOf(field.type) > 1) _ = uc_wire.bit_mark_at(0.25);
        }
    }

    // insn decoder
    _ = self.insn_decode.left_side(std.fmt.comptimePrint("A[{d}:0]", .{
        @bitSizeOf(arch.insn_decode.Address) - 1,
    })).wire_h(.{ .bits = @bitSizeOf(arch.insn_decode.Address), .class = "reg" })
        .label("IR", .{})
        .bit_mark()
        .end_at_mutable_point(self.input_box.right_side(""));

    // ucs_mux
    _ = self.ucs_mux.left_side(@tagName(arch.microcode.Slot.Source.insn_decoder))
        .wire_h(.{ .bits = @bitSizeOf(arch.microcode.Slot) })
        .bit_mark()
        .turn_at(between_column_1_and_2)
        .turn()
        .label("ENTRY_POINT", .{})
        .end_at_point(self.insn_decode.right_side_lower(std.fmt.comptimePrint("D[{d}:{d}]", .{
            @bitOffsetOf(arch.insn_decode.Result, "entry") + @bitSizeOf(arch.microcode.Slot) - 1,
            @bitOffsetOf(arch.insn_decode.Result, "entry")
        })));
    self.d.state.constrain_offset(&self.insn_decode.get_right_lower_interface().span.end,
        self.insn_decode.bottom()._y, -25, "entry point");

    _ = self.ucs_mux.left_side(@tagName(arch.microcode.Slot.Source.continuation))
        .wire_h(.{ .bits = @bitSizeOf(arch.microcode.Slot), .class = "control" })
        .label("NEXT", .{})
        .bit_mark()
        .end_at_mutable_point(self.input_box.right_side(""));

    _ = self.ucs_mux.left_side(@tagName(arch.microcode.Slot.Source.fucs))
        .wire_h(.{ .bits = @bitSizeOf(arch.microcode.Slot), .class = "reg" })
        .label("FAULT_UC_SLOT", .{})
        .bit_mark()
        .end_at_mutable_point(self.input_box.right_side(""));

    // uca_joiner
    _ = self.uc_flag_mux.right_side("")
        .wire_h(.{})
        .fmt_label("UCA[{d}]", .{
            @bitOffsetOf(arch.microcode.Address, "flags") + @bitOffsetOf(arch.microcode.Flags, "cv")
        }, .{ .alignment = .right })
        .label("", .{})
        .end_at_mutable_point(self.uca_joiner.left_side(""));
    _ = self.uca_joiner.left_side("")
        .wire_h(.{ .class = "reg" })
        .fmt_label("UCA[{d}]", .{
            @bitOffsetOf(arch.microcode.Address, "flags") + @bitOffsetOf(arch.microcode.Flags, "n")
        }, .{ .alignment = .right })
        // .continue_at(between_column_1_and_2)
        .label("FLAG_N", .{})
        .end_at_mutable_point(self.input_box.right_side(""));
    _ = self.uca_joiner.left_side("")
        .wire_h(.{ .class = "reg" })
        .fmt_label("UCA[{d}]", .{
            @bitOffsetOf(arch.microcode.Address, "flags") + @bitOffsetOf(arch.microcode.Flags, "z")
        }, .{ .alignment = .right })
        // .continue_at(between_column_1_and_2)
        .label("FLAG_Z", .{})
        .end_at_mutable_point(self.input_box.right_side(""));
    _ = self.uca_joiner.left_side("")
        .wire_h(.{ .class = "reg" })
        .fmt_label("UCA[{d}]", .{
            @bitOffsetOf(arch.microcode.Address, "flags") + @bitOffsetOf(arch.microcode.Flags, "k")
        }, .{ .alignment = .right })
        // .continue_at(between_column_1_and_2)
        .label("FLAG_K", .{})
        .end_at_mutable_point(self.input_box.right_side(""));
    _ = self.ucs_mux.right_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.microcode.Slot) })
        .fmt_label("UCA[{d}:{d}]", .{
            @bitOffsetOf(arch.microcode.Address, "slot") + @bitSizeOf(arch.microcode.Slot) - 1,
            @bitOffsetOf(arch.microcode.Address, "slot"),
        }, .{ .alignment = .right })
        .label("", .{})
        .end_at_mutable_point(self.uca_joiner.left_side(""));

    const uca_wire = self.uca_joiner.right_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.microcode.Address) })
        .bit_mark()
        .label("UCA", .{ .alignment = .right })
        .end_at_mutable_point(self.output_box.left_side(""));

    _ = self.microcode.left_side(std.fmt.comptimePrint("A[{d}:0]", .{
        @bitSizeOf(arch.microcode.Address) - 1
    })).wire_h(.{ .bits = @bitSizeOf(arch.microcode.Address), .dir = .junction_end })
        .length(-50)
        .bit_mark()
        .turn()
        .end_at(uca_wire.y());

    // sequencer
    _ = self.sequencer.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.Sequencer_Op), .class = "control" })
        .label("SEQ_OP", .{})
        .bit_mark_at(0.25)
        .end_at_mutable_point(self.input_box.right_side(""));

    _ = self.sequencer.left_side("")
        .wire_h(.{ .class = "control" })
        .label("ALLOW_INT", .{})
        .end_at_mutable_point(self.input_box.right_side(""));

    _ = self.sequencer.left_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.Execution_Mode), .class = "reg" })
        .label("EXEC_MODE", .{})
        .bit_mark_at(0.25)
        .end_at_mutable_point(self.input_box.right_side(""));

    _ = self.sequencer.left_side("")
        .wire_h(.{})
        .label("INT_PENDING", .{})
        .end_at_mutable_point(self.input_box.right_side(""));

    _ = self.sequencer.left_side("")
        .wire_h(.{})
        .label("RESET", .{})
        .end_at_mutable_point(self.input_box.right_side(""));

    var last_fault_wire = self.sequencer.left_side("")
        .wire_h(.{})
        .label("ANY_FAULT", .{})
        .end_at_mutable_point(self.input_box.right_side(""));
    
    var fault_flag_iter = microsim.Pipeline_State.fault_flags.iterator();
    while (fault_flag_iter.next()) |fault| {
        const label = std.ascii.allocUpperString(self.d.state.arena.allocator(), @tagName(fault)) catch @panic("OOM");
        last_fault_wire = self.sequencer.left_side("")
            .wire_h(.{})
            .label(label, .{})
            .end_at_mutable_point(self.input_box.right_side(""));
    }

    _ = self.sequencer.right_side_upper("")
        .wire_h(.{ .bits = @bitSizeOf(arch.microcode.Slot.Sequencer_Literal) })
        .label("UC_LITERAL", .{})
        .bit_mark()
        .turn_at(between_column_1_and_2)
        .length(-30)
        .small_box("zx", .up)
        .change_bits(@bitSizeOf(arch.microcode.Slot))
        .turn()
        .bit_mark()
        .end_at_point(self.ucs_mux.left_side(@tagName(arch.microcode.Slot.Source.seq_literal)));

    _ = self.sequencer.right_side("")
        .wire_h(.{ .bits = @bitSizeOf(arch.microcode.Slot.Source) })
        .label("UC_SLOT_SRC", .{})
        .bit_mark()
        .turn()
        .end_at_point(self.ucs_mux.bottom_center());

    const seq_exec_mode_wire = self.sequencer.right_side_lower("")
        .wire_h(.{ .bits = @bitSizeOf(arch.Execution_Mode), .class = "reg" })
        .label("EXEC_MODE", .{})
        .continue_at(device_column_2)
        .label("EXEC_MODE", .{ .alignment = .right })
        .bit_mark()
        .end_at_mutable_point(self.output_box.left_side(""));


    // v_extents
    ///////////////////////////////////////////////////////////////////

    _ = self.v_extents.input.min.attach_to(ti_wire.y());
    _ = self.v_extents.input.max.attach_to(last_fault_wire.y());

    _ = self.v_extents.output.min.attach_to(wi_overflow_wire.y());
    _ = self.v_extents.output.max.attach_to(seq_exec_mode_wire.y());
}

const X_Ref = zbox.X_Ref;
const Box = zbox.Box;
const Drawing = zbox.Drawing;

const V_Extents = @import("V_Extents.zig");
const zbox = @import("zbox");
const microsim = @import("microsim");
const isa = @import("isa");
const arch = @import("arch");
const std = @import("std");
