
pub fn main() !void {
    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .{};
    defer std.debug.assert(gpa.deinit() == .ok);

    var d = zbox.Drawing.init(gpa.allocator());
    defer d.deinit();
    d.title = "Architecture Block Diagram";
    d.style.bus_style.junction_radius = 4;
    d.style.bus_style.bit_mark_label_offset_y = 5;
    d.style.bus_style.bit_mark_length = 7;
    d.style.box_padding_x = 4;
    d.style.extra_css =
        \\ .wire.wire.wire.wire { stroke-width: 2; }
        \\ .wire.wire.wire.bitmark { stroke-width: 1; }
        \\ .wire.wire.wire.junction { stroke: none; }
        \\ .wire.control { stroke: #6d6; }
        \\ .wire-label.control { fill: #6d6; }
        \\ .wire.junction.control { fill: #6d6; }
        \\ .box.pipeline { fill: #4F466A; }
        \\ .box-label.interface.interface { font-size: 12px; }
        ;

    const phase_label_y = d.y(-200);

    const start = d.separator_v()
        .label(phase_label_y, "Decode", .{ .alignment = .right, .baseline = .hanging })
        .x();

    const end_of_decode = d.separator_v()
        .label(phase_label_y, "Decode", .{ .alignment = .right })
        .label(phase_label_y, "Setup", .{ .alignment = .right, .baseline = .hanging })
        .x();

    const end_of_setup = d.separator_v()
        .label(phase_label_y, "Setup", .{ .alignment = .right })
        .label(phase_label_y, "Compute", .{ .alignment = .right, .baseline = .hanging })
        .x().attach_to_offset(end_of_decode, 100);

    const end_of_compute = d.separator_v()
        .label(phase_label_y, "Compute", .{ .alignment = .right })
        .label(phase_label_y, "Transact", .{ .alignment = .right, .baseline = .hanging })
        .x().attach_to_offset(end_of_setup, 100);

    const end_of_transact = d.separator_v()
        .label(phase_label_y, "Transact", .{ .alignment = .right })
        .x().attach_to_offset(end_of_compute, 100);

    const decode = Decode.init(d, start, end_of_decode);
    const setup = Setup.init(d, decode, end_of_setup);
    const compute = Compute.init(d, setup, end_of_compute);
    const transact = Transact.init(d, compute, end_of_transact);
    _ = transact;


    var f = try std.fs.cwd().createFile("doc/block_diagram.svg", .{});
    defer f.close();
    try d.render_svg(f.writer());
}

const Decode = struct {
    out: *Box,

    pub fn init(d: *Drawing, begin: X_Ref, end: X_Ref) Decode {
        const in = d.box(.{ .class="pipeline" }).width(50);
        const out = d.box(.{ .class="pipeline" }).width(50);

        _ = in.x().attach_to(begin);
        _ = in.top().anchor_at(0);

        


        const seq = d.box(.{ .label = "SEQ" }).size(120, 240);
        _ = seq.top_left().attach_to_offset(in.top_right(), 150, 100);


        const c_mask = d.box(.{ .label = "C MASK"}).size(120, 100);
        _ = c_mask.top_center().attach_to_offset(seq.bottom_center(), 0, 50);


        const id_rom = d.box(.{ .label = "ID ROM"}).size(120, 100);
        _ = id_rom.top_center().attach_to_offset(c_mask.bottom_center(), 0, 40);


        const uca_lit_zx = d.box(.{ .shape = .small, .label = "zx" });

        const uca_mux = d.box(.{ .shape = .mux }).height(80);
        _ = uca_mux.left().attach_to_offset(id_rom.right(), 160);
        _ = uca_mux.y().attach_between(c_mask.top(), id_rom.bottom(), 0.45);

        const ij_mux = index_mux(d, in, out, id_rom, "IJ", "C[3:0]");
        _ = ij_mux.top_center().attach_to_offset(uca_mux.bottom_center(), 0, 100);

        const ik_mux = index_mux(d, in, out, id_rom, "IK", "C[7:4]");
        _ = ik_mux.top_center().attach_to_offset(ij_mux.bottom_center(), 0, 60);

        const iw_mux = index_mux(d, in, out, id_rom, "IW", "C[11:8]");
        _ = iw_mux.top_center().attach_to_offset(ik_mux.bottom_center(), 0, 60);



        const uc_rom = d.box(.{ .label = "&#181;C ROM" }).width(120);


        _ = seq.left_side("")
            .wire_h(.{ .bits = 2 })
            .label("EXEC_MODE", .{})
            .bit_mark_at(0.3)
            .end_at(in.right());
        _ = seq.right_side("")
            .wire_h(.{ .bits = 2 })
            .label("EXEC_MODE", .{})
            .label("EXEC_MODE", .{ .alignment = .right })
            .bit_mark()
            .end_at(out.left());

        _ = seq.left_side("")
            .wire_h(.{ .bits = 2, .class="control" })
            .label("SEQ_OP", .{})
            .bit_mark_at(0.3)
            .end_at(in.right());

        _ = seq.left_side("").wire_h(.{ .class="control" }).label("ALLOW_INT", .{}).end_at(in.right());

        _ = seq.left_side("").wire_h(.{}).label("RESET", .{}).end_at(in.right());
        _ = seq.left_side("").wire_h(.{}).label("WANT_ATOMIC", .{}).end_at(in.right());
        _ = seq.left_side("").wire_h(.{}).label("STALL_ATOMIC", .{}).end_at(in.right());
        _ = seq.left_side("").wire_h(.{}).label("PAGE_FAULT", .{}).end_at(in.right());
        _ = seq.left_side("").wire_h(.{}).label("PAGE_ALIGN_FAULT", .{}).end_at(in.right());
        _ = seq.left_side("").wire_h(.{}).label("ACCESS_FAULT", .{}).end_at(in.right());
        _ = seq.left_side("").wire_h(.{}).label("INSN_FAULT", .{}).end_at(in.right());
        _ = seq.left_side("").wire_h(.{}).label("INT_PEND", .{}).end_at(in.right());


        _ = seq.right_side("")
            .wire_h(.{ .bits = 2 })
            .label("&#181;CA_SEL", .{})
            .bit_mark()
            .turn_and_end_at(uca_mux.top_side(""));

        _ = seq.right_side("")
            .wire_h(.{ .bits = 3 })
            .label("&#181;CA_LIT", .{})
            .turn()
            .bit_mark()
            .end_at_point(uca_lit_zx.top_side(""));

        _ = uca_lit_zx.bottom_side("")
            .wire_v(.{ .bits = 12 })
            .bit_mark()
            .turn_and_end_at(uca_mux.left_side(""));






        _ = id_rom.left_side("")
            .wire_h(.{ .bits = 16 })
            .label("ID", .{})
            .bit_mark()
            .end_at(in.right());

        _ = id_rom.left_side("")
            .wire_h(.{ .class = "control" })
            .label("ID_MODE", .{})
            .end_at(in.right());

        const opcode = id_rom.right_side("")
            .wire_h(.{ .bits = 12 })
            .label("OPCODE", .{})
            .bit_mark();




        const ii_columns = d.columns();
        _ = ii_columns.center().attach_between(id_rom.right(), uca_mux.left(), 0.5);

        _ = id_rom.right_side("")
            .wire_h(.{ .bits = 4 })
            .label("IIJ", .{})
            .bit_mark()
            .turn_at(ii_columns.push())
            .turn_and_end_at(ij_mux.get_left_side(0));

        _ = id_rom.right_side("")
            .wire_h(.{ .bits = 4 })
            .label("IIK", .{})
            .bit_mark()
            .turn_at(ii_columns.push())
            .turn_and_end_at(ik_mux.get_left_side(0));

        _ = id_rom.right_side("")
            .wire_h(.{ .bits = 4 })
            .label("IIW", .{})
            .bit_mark()
            .turn_at(ii_columns.push())
            .turn_and_end_at(iw_mux.get_left_side(0));

        ii_columns.interface.flip();

        _ = uca_lit_zx.y().attach_between(seq.bottom(), c_mask.top(), 0.9);
        _ = uca_lit_zx.x().attach_to(ii_columns.get(2));


        _ = c_mask.left_side("").wire_h(.{ .bits = 12, .class="control" }).label("C", .{}).bit_mark_at(0.3).end_at(in.right());
        _ = c_mask.left_side("").wire_h(.{ .bits = 2, .class="control" }).label("IJ_SEL", .{}).bit_mark_at(0.3).end_at(in.right());
        _ = c_mask.left_side("").wire_h(.{ .bits = 2, .class="control" }).label("IK_SEL", .{}).bit_mark_at(0.3).end_at(in.right());
        _ = c_mask.left_side("").wire_h(.{ .bits = 2, .class="control" }).label("IW_SEL", .{}).bit_mark_at(0.3).end_at(in.right());
        _ = c_mask.right_side("").wire_h(.{ .bits = 12 }).label("C", .{}).bit_mark().turn_at(ii_columns.get(1)).turn_and_end_at(uca_mux.left_side(""));
        _ = uca_mux.left_side("").wire_h(.{ .bits = 12 }).label("&#181;CA", .{}).bit_mark().end_at(in.right());
        _ = opcode.turn_at(ii_columns.get(2)).turn_and_end_at(uca_mux.left_side(""));


        // TODO make a cleaner API for this in Interface
//        d.state.constrain_offset(&uca_mux.get_left_interface().span.end, uca_mux.bottom()._y, -20, "asdf");



        _ = uc_rom.left_side("")
            .wire_h(.{ .bits = 5 })
            .length(-50)
            .bit_mark()
            .turn()
            .turn_at_offset(seq.top(), -50)
            .label("KVCNZ", .{})
            .end_at_mutable_point(in.right_side(""));

        const uca_to_uc_rom = uca_mux.right_side("")
            .wire_h(.{ .bits = 12 })
            .length(50)
            .bit_mark();

        _ = uca_to_uc_rom
            .turn()
            .turn()
            .bit_mark()
            .end_at_point(uc_rom.left_side(""));

        _ = uca_to_uc_rom.endpoint()
            .wire_v(.{ .bits = 12, .dir = .junction_begin })
            .turn_at_offset(uc_rom.bottom(), 25)
            .label("&#181;CA", .{ .alignment = .right })
            .bit_mark()
            .end_at(out.left());


        _ = uc_rom.height(620);
        _ = uc_rom.right_side("").wire_h(.{ .bits = 6, .class="control" }).bit_mark_at(0.3).label("LITERAL", .{ .alignment = .right }).end_at(out.left());
        _ = uc_rom.right_side("").wire_h(.{ .bits = 12, .class="control" }).bit_mark_at(0.3).label("C", .{ .alignment = .right }).end_at(out.left());
        _ = uc_rom.right_side("").wire_h(.{ .bits = 1, .class="control" }).label("ID_MODE", .{ .alignment = .right }).end_at(out.left());
        _ = uc_rom.right_side("").wire_h(.{ .bits = 2, .class="control" }).bit_mark_at(0.3).label("IJ_SEL", .{ .alignment = .right }).end_at(out.left());
        _ = uc_rom.right_side("").wire_h(.{ .bits = 2, .class="control" }).bit_mark_at(0.3).label("IK_SEL", .{ .alignment = .right }).end_at(out.left());
        _ = uc_rom.right_side("").wire_h(.{ .bits = 2, .class="control" }).bit_mark_at(0.3).label("IW_SEL", .{ .alignment = .right }).end_at(out.left());
        _ = uc_rom.right_side("").wire_h(.{ .bits = 1, .class="control" }).label("JKR_WMODE", .{ .alignment = .right }).end_at(out.left());
        _ = uc_rom.right_side("").wire_h(.{ .bits = 2, .class="control" }).bit_mark_at(0.3).label("JL_SRC", .{ .alignment = .right }).end_at(out.left());
        _ = uc_rom.right_side("").wire_h(.{ .bits = 3, .class="control" }).bit_mark_at(0.3).label("JH_SRC", .{ .alignment = .right }).end_at(out.left());
        _ = uc_rom.right_side("").wire_h(.{ .bits = 3, .class="control" }).bit_mark_at(0.3).label("K_SRC", .{ .alignment = .right }).end_at(out.left());
        _ = uc_rom.right_side("").wire_h(.{ .bits = 3, .class="control" }).bit_mark_at(0.3).label("SR1_RI", .{ .alignment = .right }).end_at(out.left());
        _ = uc_rom.right_side("").wire_h(.{ .bits = 3, .class="control" }).bit_mark_at(0.3).label("SR2_RI", .{ .alignment = .right }).end_at(out.left());
        _ = uc_rom.right_side("").wire_h(.{ .bits = 3, .class="control" }).bit_mark_at(0.3).label("SR1_WI", .{ .alignment = .right }).end_at(out.left());
        _ = uc_rom.right_side("").wire_h(.{ .bits = 3, .class="control" }).bit_mark_at(0.3).label("SR2_WI", .{ .alignment = .right }).end_at(out.left());
        _ = uc_rom.right_side("").wire_h(.{ .bits = 2, .class="control" }).bit_mark_at(0.3).label("SR1_WSRC", .{ .alignment = .right }).end_at(out.left());
        _ = uc_rom.right_side("").wire_h(.{ .bits = 2, .class="control" }).bit_mark_at(0.3).label("SR2_WSRC", .{ .alignment = .right }).end_at(out.left());
        _ = uc_rom.right_side("").wire_h(.{ .bits = 4, .class="control" }).bit_mark_at(0.3).label("BASE", .{ .alignment = .right }).end_at(out.left());
        _ = uc_rom.right_side("").wire_h(.{ .bits = 2, .class="control" }).bit_mark_at(0.3).label("OFFSET", .{ .alignment = .right }).end_at(out.left());
        _ = uc_rom.right_side("").wire_h(.{ .bits = 4, .class="control" }).bit_mark_at(0.3).label("MODE", .{ .alignment = .right }).end_at(out.left());
        _ = uc_rom.right_side("").wire_h(.{ .bits = 2, .class="control" }).bit_mark_at(0.3).label("BUS_MODE", .{ .alignment = .right }).end_at(out.left());
        _ = uc_rom.right_side("").wire_h(.{ .bits = 1, .class="control" }).label("BUS_BYTE", .{ .alignment = .right }).end_at(out.left());
        _ = uc_rom.right_side("").wire_h(.{ .bits = 1, .class="control" }).label("BUS_RW", .{ .alignment = .right }).end_at(out.left());
        _ = uc_rom.right_side("").wire_h(.{ .bits = 2, .class="control" }).bit_mark_at(0.3).label("AT_OP", .{ .alignment = .right }).end_at(out.left());
        _ = uc_rom.right_side("").wire_h(.{ .bits = 3, .class="control" }).bit_mark_at(0.3).label("SPECIAL", .{ .alignment = .right }).end_at(out.left());
        _ = uc_rom.right_side("").wire_h(.{ .bits = 4, .class="control" }).bit_mark_at(0.3).label("LL_SRC", .{ .alignment = .right }).end_at(out.left());
        _ = uc_rom.right_side("").wire_h(.{ .bits = 4, .class="control" }).bit_mark_at(0.3).label("LH_SRC", .{ .alignment = .right }).end_at(out.left());
        _ = uc_rom.right_side("").wire_h(.{ .bits = 4, .class="control" }).bit_mark_at(0.3).label("STAT_OP", .{ .alignment = .right }).end_at(out.left());
        _ = uc_rom.right_side("").wire_h(.{ .bits = 2, .class="control" }).bit_mark_at(0.3).label("IDR_OP", .{ .alignment = .right }).end_at(out.left());
        _ = uc_rom.right_side("").wire_h(.{ .bits = 2, .class="control" }).bit_mark_at(0.3).label("SEQ_OP", .{ .alignment = .right }).end_at(out.left());
        _ = uc_rom.right_side("").wire_h(.{ .bits = 1, .class="control" }).label("ALLOW_INT", .{ .alignment = .right }).end_at(out.left());

        seq.get_right_interface().spacing = 100;
        uc_rom.get_left_interface().spacing = 200;
        _ = uc_rom.top().anchor_at(0);
        _ = uc_rom.left().attach_to_offset(uca_mux.right(), 100);


        _ = out.left().attach_to_offset(uc_rom.right(), 150);
        _ = out.top().attach_to(in.top());
        _ = out.bottom().attach_to_offset(iw_mux.bottom(), 0);

        _ = in.bottom().attach_to_offset(iw_mux.bottom(), 60);
        _ = end.attach_to(out.x());

        return .{
            .out = out,
        };
    }

    fn index_mux(d: *Drawing, in: *Box, out: *Box, id_rom: *Box, comptime name: []const u8, continuation_bits: []const u8) *Box {
        const mux = d.box(.{ .shape = .mux }).height(80);

        _ = mux.left_side("");
        _ = mux.left_side("")
            .wire_h(.{ .bits = 4, .class = "control" })
            .continue_at(id_rom.x())
            .label(continuation_bits, .{})
            .bit_mark()
            .end_at_mutable_point(in.right_side(""));
        const wire = mux.left_side("")
            .wire_h(.{ .bits = 4 })
            .continue_at(id_rom.x())
            .label(name, .{})
            .bit_mark()
            .end_at_mutable_point(in.right_side(""));

        const xor_wire = mux.left_side("")
            .wire_h(.{ .bits = 4 });

        const xor = d.box(.{ .shape = .small, .label = "^1" });
        _ = xor.x().attach_to(id_rom.right());
        _ = xor.y().attach_to(xor_wire.y());

        _ = xor_wire.end_at(xor.right());

        _ = xor.left_side("")
            .wire_h(.{ .bits = 4, .dir = .junction_end })
            .turn_and_end_at(wire.origin());

        _ = mux.bottom_side("")
            .wire_v(.{ .bits = 2, .class = "control" })
            .length(30)
            .turn()
            .label(name ++ "_SEL", .{})
            .bit_mark()
            .end_at_mutable_point(in.right_side(""));
        _ = mux.right_side("")
            .wire_h(.{ .bits = 4 })
            .label(name, .{ .alignment = .right })
            .bit_mark()
            .end_at(out.left());

        return mux;
    }
};

const Setup = struct {
    out: *Box,

    pub fn init(d: *Drawing, decode: Decode, end: X_Ref) Setup {
        const in = decode.out;
        const out = d.box(.{ .class="pipeline" }).width(50);

        const jr = d.box(.{ .label = "JR" }).size(100, 80);
        const kr = d.box(.{ .label = "KR" }).size(100, 80);
        const sr1 = d.box(.{ .label = "SR1" }).size(100, 80);
        const sr2 = d.box(.{ .label = "SR2" }).size(100, 80);
        const sr1b = d.box(.{ .label = "SR1B" }).size(100, 80);
        const sr2b = d.box(.{ .label = "SR2B" }).size(100, 80);

        const jhmux = d.box(.{ .shape = .mux }).size(40, 120);
        const jlmux = d.box(.{ .shape = .mux }).size(40, 80);
        const kmux = d.box(.{ .shape = .mux }).size(40, 160);

        const jh_sx_jr = d.box(.{ .shape = .small, .label = "sx" });

        const k_zx_ij_ik = d.box(.{ .shape = .small, .label = "zx" });
        const k_zx_ik = d.box(.{ .shape = .small, .label = "zx" });
        const k_bit_ik = d.box(.{ .shape = .small, .label = "1&lt;&lt;" });
        const k_zx_literal = d.box(.{ .shape = .small, .label = "zx" });
        const k_1x_literal = d.box(.{ .shape = .small, .label = "1x" });

        const offset_zx_literal = d.box(.{ .shape = .small, .label = "zx" });
        const offset_1x_literal = d.box(.{ .shape = .small, .label = "1x" });

        const offset_mux = d.box(.{ .shape = .mux }).size(40, 80);
        const base_mux = d.box(.{ .shape = .mux }).size(40, 50);
        const agu = d.box(.{ .label = "+" }).size(80, 140); // TODO ALU shape?

        _ = jr.left().attach_to_offset(in.right(), 200);
        _ = jr.top().anchor_at(50);
        _ = sr1.top_center().attach_to_offset(jr.bottom_center(), 0, 30);
        _ = sr2.top_center().attach_to_offset(sr1.bottom_center(), 0, 30);
        _ = kr.top_center().attach_to_offset(sr2.bottom_center(), 0, 75);
        _ = sr1b.top_center().attach_to_offset(kr.bottom_center(), 0, 300);
        _ = sr2b.top_center().attach_to_offset(sr1b.bottom_center(), 0, 25);

        _ = jhmux.left().attach_to_offset(jr.right(), 360);
        _ = jhmux.top().anchor_at(80);
        _ = jlmux.top_left().attach_to_offset(jhmux.bottom_left(), 0, 50);
        _ = kmux.top_left().attach_to_offset(jlmux.bottom_left(), 0, 125);

        _ = jh_sx_jr.x().attach_to_offset(jhmux.left(), -100);
        _ = k_zx_ij_ik.x().attach_to(jh_sx_jr.x());
        _ = k_zx_ik.right().attach_to_offset(jhmux.left(), -50);
        _ = k_bit_ik.x().attach_to(jh_sx_jr.x());
        _ = k_zx_literal.x().attach_to_offset(kr.left(), 0);
        _ = k_1x_literal.x().attach_to_offset(kr.left(), 50);
        _ = offset_zx_literal.x().attach_to(k_zx_literal.x());
        _ = offset_1x_literal.x().attach_to(k_1x_literal.x());

        _ = offset_mux.left().attach_to_offset(sr1b.right(), 100);
        _ = base_mux.left().attach_to(offset_mux.left());
        _ = base_mux.top().attach_to_offset(sr1b.top(), 25);
        _ = agu.y().attach_between(offset_mux.y(), base_mux.y(), 0.5);
        _ = agu.left().attach_to_offset(offset_mux.right(), 100);

        _ = jhmux.top_side("").wire_v(.{ .bits = 3, .class="control" })
            .turn_at_offset(jr.top(), -25)
            .bit_mark()
            .label("JH_SRC", .{})
            .end_at(in.right());

        _ = jlmux.bottom_side("").wire_v(.{ .bits = 2, .class="control" })
            .turn_at_offset(sr2.bottom(), 25)
            .bit_mark()
            .label("JL_SRC", .{})
            .end_at(in.right());

        _ = kmux.top_side("").wire_v(.{ .bits = 3, .class="control" })
            .turn_at_offset(kr.top(), -25)
            .bit_mark()
            .label("K_SRC", .{})
            .end_at(in.right());


        const left_cols = d.columns();
        _ = left_cols.right().attach_to_offset(jr.left(), -60);

        const cols = d.columns();
        _ = cols.center().attach_to_offset(jr.right(), 150);
        _ = cols.push();
        _ = cols.push();
        _ = cols.push();
        _ = cols.push();
        _ = cols.push();
        _ = cols.push();

        // JR inputs
        _ = jr.left_side("")
            .wire_h(.{ .bits = 6 })
            .bit_mark()
            .continue_at(left_cols.push())
            .label("RSN", .{})
            .end_at(in.right());
        _ = jr.left_side("")
            .wire_h(.{ .bits = 4 })
            .bit_mark()
            .continue_at(left_cols.get(0))
            .label("IJ", .{})
            .end_at(in.right());

        // SR2B inputs
        const rsn_wire = sr2b.left_side("")
            .wire_h(.{ .bits = 6, .dir = .junction_end })
            .label("RSN", .{ .alignment = .right })
            .bit_mark()
            .turn_at(left_cols.get(0))
            .end_at(jr.get_left_side(0).y());

        // KR inputs
        _ = kr.left_side("")
            .wire_h(.{ .bits = 6, .dir = .junction_end })
            .label("RSN", .{ .alignment = .right })
            .bit_mark()
            .end_at(rsn_wire.x());
        _ = kr.left_side("")
            .wire_h(.{ .bits = 4 })
            .bit_mark()
            .continue_at(rsn_wire.x())
            .label("IK", .{})
            .end_at(in.right());

        // K mux inputs using IJ/IK/LITERAL
        k_zx_ij_ik.get_left_interface().spacing = 12;
        _ = k_zx_ij_ik.left_side("")
            .wire_h(.{ .bits = 4, .dir = .junction_end })
            .turn_at(cols.get(4))
            .turn_at_offset(kr.bottom(), 25)
            .label("IJ", .{ .alignment = .right })
            .bit_mark()
            .turn_at(left_cols.push())
            .end_at(jr.get_left_side(1).y());
        _ = k_bit_ik.left_side("")
            .wire_h(.{ .bits = 4, .dir = .junction_end })
            .continue_at(cols.get(5))
            .label("IK", .{ .alignment = .right })
            .bit_mark()
            .turn_at(left_cols.push())
            .end_at(kr.get_left_side(1).y());
        _ = k_zx_ij_ik.left_side("")
            .wire_h(.{ .bits = 4, .dir = .junction_end })
            .turn_at(cols.get(5))
            .end_at(k_bit_ik.get_left_side(0).y());
        _ = k_zx_ik.left_side("")
            .wire_h(.{ .bits = 4, .dir = .junction_end })
            .end_at(cols.get(5));
        _ = k_zx_literal.left_side("")
            .wire_h(.{ .bits = 6, .class="control" })
            .label("LITERAL", .{})
            .bit_mark()
            .end_at(in.right());
        _ = k_1x_literal.left_side("")
            .wire_h(.{ .bits = 6, .class="control", .dir = .junction_end })
            .length(-75)
            .turn()
            .end_at(k_zx_literal.get_left_side(0).y());

        // OFFSET mux inputs using LITERAL
        _ = offset_zx_literal.left_side("")
            .wire_h(.{ .bits = 6, .class="control", .dir = .junction_end })
            .length(-25)
            .turn()
            .end_at(k_1x_literal.get_left_side(0).y());
        _ = offset_1x_literal.left_side("")
            .wire_h(.{ .bits = 6, .class="control", .dir = .junction_end })
            .length(-75)
            .turn()
            .end_at(offset_zx_literal.get_left_side(0).y());

        // SR1 inputs
        _ = sr1.left_side("")
            .wire_h(.{ .bits = 6, .dir = .junction_end })
            .label("RSN", .{ .alignment = .right })
            .bit_mark()
            .end_at(rsn_wire.x());
        _ = sr1.left_side("")
            .wire_h(.{ .bits = 3, .class="control" })
            .bit_mark()
            .continue_at(rsn_wire.x())
            .label("SR1_RI", .{})
            .end_at(in.right());

        // SR2 inputs
        _ = sr2.left_side("")
            .wire_h(.{ .bits = 6, .dir = .junction_end })
            .label("RSN", .{ .alignment = .right })
            .bit_mark()
            .end_at(rsn_wire.x());
        _ = sr2.left_side("")
            .wire_h(.{ .bits = 3, .class="control" })
            .bit_mark()
            .continue_at(rsn_wire.x())
            .label("SR2_RI", .{})
            .end_at(in.right());


        // SR1B inputs
        _ = sr1b.left_side("")
            .wire_h(.{ .bits = 6, .dir = .junction_end })
            .label("RSN", .{ .alignment = .right })
            .bit_mark()
            .end_at(rsn_wire.x());
        _ = sr1b.left_side("")
            .wire_h(.{ .bits = 3, .class="control" })
            .bit_mark()
            .continue_at(rsn_wire.x())
            .label("BASE[2:0]", .{})
            .end_at(in.right());

        // SR2B inputs (again)
        _ = sr2b.left_side("")
            .wire_h(.{ .bits = 3, .class="control", .dir = .junction_end })
            .bit_mark()
            .continue_at(rsn_wire.x())
            .turn_at(left_cols.get(1))
            .end_at(sr1b.get_left_side(1).y());


        const mux_left_50 = jhmux.left().offset(-50);

        // JH mux
        _ = jhmux.left_side("")
            .wire_h(.{ .bits = 16 })
            .label("0x0000", .{ .alignment = .right, .baseline = .middle })
            .bit_mark()
            .length(-50);
        _ = jhmux.left_side("")
            .wire_h(.{ .bits = 16 })
            .label("0xFFFF", .{ .alignment = .right, .baseline = .middle })
            .bit_mark()
            .length(-50);
        _ = jr.right_side("")
            .wire_h(.{ .bits = 16 })
            .label("JR[31:16]", .{})
            .turn_at(cols.get(5))
            .turn_at(jhmux.left_side("").y())
            .continue_at(mux_left_50)
            .bit_mark()
            .end_at(jhmux.left());
        _ = sr1.right_side("")
            .wire_h(.{ .bits = 16 })
            .label("SR1[31:16]", .{})
            .turn_at(cols.get(0))
            .turn_at(jhmux.left_side("").y())
            .continue_at(mux_left_50)
            .bit_mark()
            .end_at(jhmux.left());
        _ = sr2.right_side("")
            .wire_h(.{ .bits = 16 })
            .label("SR1[31:16]", .{})
            .turn_at(cols.get(1))
            .turn_at(jhmux.left_side("").y())
            .continue_at(mux_left_50)
            .bit_mark()
            .end_at(jhmux.left());
        _ = jhmux.left_side("")
            .wire_h(.{ .bits = 16 })
            .bit_mark()
            .continue_at(mux_left_50)
            .end_at(jh_sx_jr.right())
            .y().attach(jh_sx_jr.y());

        _ = jh_sx_jr.left_side("")
            .wire_h(.{ .dir = .junction_end })
            .label("JR[15]", .{ .alignment = .right })
            .end_at(cols.get(4));


        // JL mux
        _ = jlmux.left_side("")
            .wire_h(.{ .bits = 16 })
            .label("0x0000", .{ .alignment = .right, .baseline = .middle })
            .bit_mark()
            .length(-50);
        _ = jr.right_side("")
            .wire_h(.{ .bits = 16 })
            .label("JR[15:0]", .{})
            .turn_at(cols.get(4))
            .turn_at(jlmux.left_side("").y())
            .continue_at(mux_left_50)
            .bit_mark()
            .end_at(jlmux.left());
        _ = sr1.right_side("")
            .wire_h(.{ .bits = 16 })
            .label("SR1[15:0]", .{})
            .turn_at(cols.get(3))
            .turn_at(jlmux.left_side("").y())
            .continue_at(mux_left_50)
            .bit_mark()
            .end_at(jlmux.left());
        _ = sr2.right_side("")
            .wire_h(.{ .bits = 16 })
            .label("SR2[15:0]", .{})
            .turn_at(cols.get(2))
            .turn_at(jlmux.left_side("").y())
            .continue_at(mux_left_50)
            .bit_mark()
            .end_at(jlmux.left());

        // K mux
        _ = kr.right_side("")
            .wire_h(.{ .bits = 16 })
            .label("KR", .{})
            .turn_at(cols.get(0))
            .turn_at(kmux.left_side("").y())
            .continue_at(mux_left_50)
            .bit_mark()
            .end_at(kmux.left());
        _ = kmux.left_side("")
            .wire_h(.{ .bits = 16, .dir = .junction_end })
            .bit_mark()
            .continue_at(mux_left_50)
            .turn_at(cols.get(3))
            .end_at(jlmux.get_left_side(2).y());
        _ = kmux.left_side("")
            .wire_h(.{ .bits = 16, .dir = .junction_end })
            .bit_mark()
            .continue_at(mux_left_50)
            .turn_at(cols.get(2))
            .end_at(jlmux.get_left_side(3).y());
        _ = kmux.left_side("")
            .wire_h(.{ .bits = 16 })
            .bit_mark()
            .continue_at(mux_left_50)
            .end_at(k_zx_ij_ik.right())
            .y().attach(k_zx_ij_ik.y());
        _ = kmux.left_side("")
            .wire_h(.{ .bits = 16 })
            .bit_mark()
            .continue_at(mux_left_50)
            .end_at(k_zx_ik.right())
            .y().attach(k_zx_ik.y());
        _ = kmux.left_side("")
            .wire_h(.{ .bits = 16 })
            .bit_mark()
            .continue_at(mux_left_50)
            .end_at(k_bit_ik.right())
            .y().attach(k_bit_ik.y());
        _ = kmux.left_side("")
            .wire_h(.{ .bits = 16 })
            .bit_mark()
            .continue_at(mux_left_50)
            .end_at(k_zx_literal.right())
            .y().attach(k_zx_literal.y());
        _ = kmux.left_side("")
            .wire_h(.{ .bits = 16 })
            .bit_mark()
            .continue_at(mux_left_50)
            .end_at(k_1x_literal.right())
            .y().attach(k_1x_literal.y());


        // OFFSET mux
        const offset_mux_left_50 = offset_mux.left().offset(-50);
        _ = offset_mux.top().attach_to_offset(kmux.bottom(), 20);
        _ = offset_mux.left_side("")
            .wire_h(.{ .bits = 32 })
            .bit_mark()
            .continue_at(offset_mux_left_50)
            .end_at(offset_zx_literal.right())
            .y().attach(offset_zx_literal.y());
        _ = offset_mux.left_side("")
            .wire_h(.{ .bits = 32 })
            .bit_mark()
            .continue_at(offset_mux_left_50)
            .end_at(offset_1x_literal.right())
            .y().attach(offset_1x_literal.y());
        _ = offset_mux.left_side("")
            .wire_h(.{ .bits = 32 })
            .label("0x00000000", .{ .alignment = .right, .baseline = .middle })
            .bit_mark()
            .length(-50);
        _ = offset_mux.left_side("")
            .wire_h(.{ .bits = 32 })
            .label("0x00000002", .{ .alignment = .right, .baseline = .middle })
            .bit_mark()
            .length(-50);

        _ = offset_mux.bottom_side("")
            .wire_v(.{ .bits = 2, .class="control" })
            .length(35)
            .turn()
            .label("OFFSET", .{})
            .end_at(in.right());



        // BASE mux
        _ = base_mux.left_side("")
            .wire_h(.{ .bits = 32 })
            .bit_mark()
            .turn_at(d.between_x(sr1b.right(), base_mux.left(), 0.5))
            .turn_and_end_at(sr1b.right_side(""));
        _ = base_mux.left_side("")
            .wire_h(.{ .bits = 32 })
            .bit_mark()
            .turn_at(d.between_x(sr1b.right(), base_mux.left(), 0.5))
            .turn_and_end_at(sr2b.right_side(""));
        _ = base_mux.top_side("")
            .wire_v(.{ .bits = 1, .class="control" })
            .turn_at_offset(sr1b.top(), -25)
            .label("BASE[3]", .{})
            .end_at(in.right());

        // AGU
        const between_agu_and_muxes = d.between_x(offset_mux.right(), agu.left(), 0.5);
        agu.get_left_interface().spacing = 75;
        _ = offset_mux.right_side("")
            .wire_h(.{ .bits = 32 })
            .bit_mark()
            .turn_at(between_agu_and_muxes)
            .turn_and_end_at(agu.left_side(""));
        _ = base_mux.right_side("")
            .wire_h(.{ .bits = 32 })
            .bit_mark()
            .turn_at(between_agu_and_muxes)
            .turn_and_end_at(agu.left_side(""));

        // Outputs
        _ = jhmux.right_side("")
            .wire_h(.{ .bits = 16 })
            .bit_mark()
            .label("JH", .{ .alignment = .right })
            .end_at(out.left());

        _ = jlmux.right_side("")
            .wire_h(.{ .bits = 16 })
            .bit_mark()
            .label("JL", .{ .alignment = .right })
            .end_at(out.left());

        _ = kmux.right_side("")
            .wire_h(.{ .bits = 16 })
            .bit_mark()
            .label("K", .{ .alignment = .right })
            .end_at(out.left());

        agu.get_right_interface().spacing = 75;
        _ = agu.right_side("")
            .wire_h(.{ .bits = 20 })
            .bit_mark()
            .label("P", .{ .alignment = .right })
            .end_at(out.left());
        _ = agu.right_side("")
            .wire_h(.{ .bits = 12 })
            .bit_mark()
            .label("N", .{ .alignment = .right })
            .end_at(out.left());


        left_cols.interface.flip();

        _ = out.left().attach_to_offset(jhmux.right(), 150);
        _ = out.top().attach_to(in.top());
        _ = out.bottom().attach_to_offset(agu.bottom(), 0);

        _ = end.attach_to(out.x());

        return .{
            .out = out,
        };
    }
};

const Compute = struct {
    out: *Box,

    pub fn init(d: *Drawing, setup: Setup, end: X_Ref) Compute {
        const in = setup.out;
        const out = d.box(.{ .class="pipeline" }).width(50);

        const arith_bottom = arith(d, in, out);
        const shifter_bottom = shifter(d, in, out, arith_bottom);
        _ = shifter_bottom;

        _ = out.top().attach_to(in.top());
        _ = end.attach_to(out.x());

        return .{
            .out = out,
        };
    }

    fn arith(d: *Drawing, in: *Box, out: *Box) Y_Ref {
        const adder = d.box(.{ .label = "+" }).size(80, 140); // TODO ALU shape?
        const k_inv = d.box(.{ .shape = .small, .label = "^" });
        const k_mux = d.box(.{ .shape = .mux }).size(40, 80);
        const k_zx = d.box(.{ .shape = .small, .label = "zx" });
        const k_sx = d.box(.{ .shape = .small, .label = "sx" });
        const k_1x = d.box(.{ .shape = .small, .label = "1x" });

        const c_and = d.box(.{ .shape = .small, .label = "&amp;" });
        const c_inv = d.box(.{ .shape = .small, .label = "^" });

        const cout_mux = d.box(.{ .shape = .mux }).size(40, 50);
        const cout_dec = d.box(.{ .shape = .small, .label = "=0" });
        const cout_inv = d.box(.{ .shape = .small, .label = "^" });

        const znv_mux = d.box(.{ .shape = .mux }).size(40, 50);

        // Arith
        const cin_wire = adder.top_side("C0")
            .wire_v(.{})
            .length(-25)
            .turn();
        _ = cin_wire.endpoint()
            .attach(c_inv.middle_right())
            .x().attach_to(k_inv.right());
        _ = adder.left_side("")
            .wire_h(.{ .bits = 16 })
            .bit_mark()
            .label("JH", .{})
            .end_at(in.right());
        _ = adder.left_side("")
            .wire_h(.{ .bits = 16 })
            .bit_mark()
            .label("JL", .{})
            .end_at(in.right());
        _ = adder.left_side("");
        _ = adder.left_side("");
        _ = adder.left_side("");
        _ = adder.left_side("")
            .wire_h(.{ .bits = 32 })
            .bit_mark()
            .length(-40)
            .endpoint().attach(k_inv.middle_right());
        _ = k_inv.left_side("")
            .wire_h(.{ .bits = 32 })
            .bit_mark()
            .length(-40)
            .endpoint().attach(k_mux.middle_right());

        // Cin processing
        _ = c_inv.left_side("")
            .wire_h(.{})
            .length(-40)
            .endpoint().attach(c_and.middle_right());
        _ = c_and.left_side("")
            .wire_h(.{})
            .label("STAT_C", .{})
            .end_at(in.right());
        _ = c_and.top_side("")
            .wire_v(.{ .class="control" })
            .length(-25)
            .turn()
            .label("MODE[1]", .{})
            .end_at(in.right());
        _ = c_inv.bottom_side("")
            .wire_v(.{ .class="control" })
            .end_at(k_inv.top());


        // K Mux
        _ = k_mux.left_side("")
            .wire_h(.{ .bits = 32 })
            .bit_mark()
            .length(-40)
            .endpoint().attach(k_zx.middle_right());
        _ = k_mux.left_side("")
            .wire_h(.{ .bits = 32 })
            .bit_mark()
            .continue_at(k_mux.left().offset(-40))
            .length(-40)
            .endpoint().attach(k_sx.middle_right());
        _ = k_mux.left_side("")
            .wire_h(.{ .bits = 32 })
            .bit_mark()
            .length(-40)
            .endpoint().attach(k_1x.middle_right());
        const mode32 = k_mux.bottom_side("")
            .wire_v(.{ .bits = 2, .class="control" })
            .length(75)
            .turn()
            .label("MODE[3:2]", .{})
            .bit_mark()
            .end_at(in.right());

        const k_wire = k_sx.left_side("")
            .wire_h(.{ .bits = 16 })
            .length(-25);
        _ = k_wire.turn().turn_at(k_wire.y())
            .label("K", .{})
            .bit_mark()
            .end_at(in.right());

        _ = k_zx.left_side("")
            .wire_h(.{ .bits = 16, .dir = .junction_end })
            .turn_and_end_at(k_wire.endpoint());
        _ = k_1x.left_side("")
            .wire_h(.{ .bits = 16, .dir = .junction_end })
            .turn_and_end_at(k_wire.endpoint());


        _ = adder.right_side("")
            .wire_h(.{ .bits = 16 })
            .label("ARITH_H", .{ .alignment = .right })
            .bit_mark()
            .end_at(out.left());
        _ = adder.right_side("")
            .wire_h(.{ .bits = 16 })
            .label("ARITH_L", .{ .alignment = .right })
            .bit_mark()
            .end_at(out.left());

        _ = adder.bottom_side("C16")
            .wire_v(.{})
            .turn_and_end_at(cout_mux.left_side(""));
        _ = adder.bottom_side("C32")
            .wire_v(.{})
            .turn_and_end_at(cout_mux.left_side(""));

        _ = cout_mux.bottom_side("")
            .wire_v(.{})
            .turn_at(mode32.y())
            .length(-100)
            .endpoint().attach(cout_dec.middle_right());
        _ = cout_dec.left_side("")
            .wire_h(.{ .bits = 2, .class="control", .dir = .junction_end })
            .end_at(mode32.origin().x());

        _ = cout_mux.top_left().attach_to_offset(adder.bottom_right(), 50, 10);
        cout_mux.get_left_interface().flip();

        _ = cout_mux.right_side("")
            .wire_h(.{})
            .length(25)
            .endpoint().attach(cout_inv.middle_left());
        _ = cout_inv.right_side("")
            .wire_h(.{})
            .label("ARITH_C", .{ .alignment = .right })
            .end_at(out.left());

        const mode0 = cout_inv.bottom_side("")
            .wire_v(.{ .class="control" })
            .turn_at_offset(mode32.y(), 20)
            .label("MODE[0]", .{})
            .end_at(in.right());
        _ = k_inv.bottom_side("")
            .wire_v(.{ .class="control", .dir = .junction_end })
            .end_at(mode0.y());


        _ = znv_mux.bottom_center().attach_to_offset(cout_mux.top_center(), 0, -30);
        _ = znv_mux.left_side("")
            .wire_h(.{ .bits = 2 })
            .bit_mark()
            .end_at_mutable_point(adder.right_side("ZNV32"));
        _ = znv_mux.left_side("")
            .wire_h(.{ .bits = 2 })
            .bit_mark()
            .end_at_mutable_point(adder.right_side("ZNV16"));
        _ = znv_mux.right_side("")
            .wire_h(.{ .bits = 2 })
            .label("ARITH_ZNV", .{ .alignment = .right })
            .bit_mark()
            .end_at(out.left());
        _ = znv_mux.bottom_side("")
            .wire_v(.{})
            .end_at(cout_mux.top());

        _ = adder.right_side("");
        _ = adder.right_side("");
        _ = adder.right_side("");
        _ = adder.right_side(""); // TODO box interface alignment


        _ = adder.top_left().attach_to_offset(in.top_right(), 350, 90);

        _ = out.left().attach_to_offset(cout_inv.right(), 75);
        _ = out.bottom().attach_to_offset(cout_inv.bottom(), 0);

        return mode0.y();
    }

    fn shifter(d: *Drawing, in: *Box, out: *Box, top: Y_Ref) Y_Ref {
        _ = out;
        const early_reverse_h = d.box(.{ .label = "rev" }).width(50);
        const early_reverse_l = d.box(.{ .label = "rev" }).width(50);

        const early_swap16 = d.box(.{ .label = "&#10536;" }).width(50);
        const early_mask_high = d.box(.{ .label = "&amp;" }).width(50);

        const shift = d.box(.{ .label = ">>" }).width(50);

        const late_swap16 = d.box(.{ .label = "&#10536;" }).width(50);

        const late_reverse_h = d.box(.{ .label = "rev" }).width(50);
        const late_reverse_l = d.box(.{ .label = "rev" }).width(50);




        const top_y = top.offset(100);

        _ = early_reverse_h.top().attach_to(top_y);
        _ = early_reverse_l.top().attach_to(early_reverse_h.bottom());
        _ = early_swap16.top().attach_to(top_y);
        _ = early_swap16.bottom().attach_to(early_reverse_l.bottom());
        _ = early_mask_high.top().attach_to(top_y);
        _ = shift.top().attach_to(top_y);
        _ = shift.bottom().attach_to(early_swap16.bottom());
        _ = late_swap16.top().attach_to(top_y);
        _ = late_swap16.bottom().attach_to(early_reverse_l.bottom());
        _ = late_reverse_h.top().attach_to(top_y);
        _ = late_reverse_l.top().attach_to(early_reverse_h.bottom());

        _ = early_reverse_h.left().attach_to_offset(in.right(), 100);
        _ = early_reverse_l.left().attach_to_offset(in.right(), 100);
        _ = early_swap16.left().attach_to_offset(early_reverse_h.right(), 40);
        _ = early_mask_high.left().attach_to_offset(early_swap16.right(), 40);
        _ = shift.left().attach_to_offset(early_mask_high.right(), 40);

        _ = late_swap16.left().attach_to_offset(shift.right(), 40);
        _ = late_reverse_h.left().attach_to_offset(late_swap16.right(), 40);
        _ = late_reverse_l.left().attach_to_offset(late_swap16.right(), 40);

        

        return early_reverse_l.bottom();
    }

};

const Transact = struct {
    pub fn init(d: *Drawing, compute: Compute, end: X_Ref) Transact {
        _ = compute;
        _ = end;
        _ = d;
        return .{};
    }
};



const Drawing = zbox.Drawing;
const Box = zbox.Box;
const X_Ref = zbox.X_Ref;
const Y_Ref = zbox.Y_Ref;

const zbox = @import("zbox");
const std = @import("std");
