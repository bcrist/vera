
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

    const start = d.separatorV()
        .label(phase_label_y, "Decode", .{ .alignment = .right, .baseline = .hanging })
        .x();

    const end_of_decode = d.separatorV()
        .label(phase_label_y, "Decode", .{ .alignment = .right })
        .label(phase_label_y, "Setup", .{ .alignment = .right, .baseline = .hanging })
        .x();

    const end_of_setup = d.separatorV()
        .label(phase_label_y, "Setup", .{ .alignment = .right })
        .label(phase_label_y, "Compute", .{ .alignment = .right, .baseline = .hanging })
        .x().attachToOffset(end_of_decode, 100);

    const end_of_compute = d.separatorV()
        .label(phase_label_y, "Compute", .{ .alignment = .right })
        .label(phase_label_y, "Transact", .{ .alignment = .right, .baseline = .hanging })
        .x().attachToOffset(end_of_setup, 100);

    const end_of_transact = d.separatorV()
        .label(phase_label_y, "Transact", .{ .alignment = .right })
        .x().attachToOffset(end_of_compute, 100);

    const decode = Decode.init(d, start, end_of_decode);
    const setup = Setup.init(d, decode, end_of_setup);
    const compute = Compute.init(d, setup, end_of_compute);
    const transact = Transact.init(d, compute, end_of_transact);
    _ = transact;


    var f = try std.fs.cwd().createFile("doc/block_diagram.svg", .{});
    defer f.close();
    try d.renderSvg(f.writer());
}

const Decode = struct {
    out: *Box,

    pub fn init(d: *Drawing, begin: XRef, end: XRef) Decode {
        const in = d.box(.{ .class="pipeline" }).width(50);
        const out = d.box(.{ .class="pipeline" }).width(50);

        _ = in.x().attachTo(begin);
        _ = in.top().anchorAt(0);

        


        const seq = d.box(.{ .label = "SEQ" }).size(120, 240);
        _ = seq.topLeft().attachToOffset(in.topRight(), 150, 100);


        const c_mask = d.box(.{ .label = "C MASK"}).size(120, 100);
        _ = c_mask.topCenter().attachToOffset(seq.bottomCenter(), 0, 50);


        const id_rom = d.box(.{ .label = "ID ROM"}).size(120, 100);
        _ = id_rom.topCenter().attachToOffset(c_mask.bottomCenter(), 0, 40);


        const uca_lit_zx = d.box(.{ .shape = .small, .label = "zx" });

        const uca_mux = d.box(.{ .shape = .mux }).height(80);
        _ = uca_mux.left().attachToOffset(id_rom.right(), 160);
        _ = uca_mux.y().attachBetween(c_mask.top(), id_rom.bottom(), 0.45);

        const ij_mux = index_mux(d, in, out, id_rom, "IJ", "C[3:0]");
        _ = ij_mux.topCenter().attachToOffset(uca_mux.bottomCenter(), 0, 100);

        const ik_mux = index_mux(d, in, out, id_rom, "IK", "C[7:4]");
        _ = ik_mux.topCenter().attachToOffset(ij_mux.bottomCenter(), 0, 60);

        const iw_mux = index_mux(d, in, out, id_rom, "IW", "C[11:8]");
        _ = iw_mux.topCenter().attachToOffset(ik_mux.bottomCenter(), 0, 60);



        const uc_rom = d.box(.{ .label = "&#181;C ROM" }).width(120);


        _ = seq.leftSide("")
            .wireH(.{ .bits = 2 })
            .label("EXEC_MODE", .{})
            .bitMarkAt(0.3)
            .endAt(in.right());
        _ = seq.rightSide("")
            .wireH(.{ .bits = 2 })
            .label("EXEC_MODE", .{})
            .label("EXEC_MODE", .{ .alignment = .right })
            .bitMark()
            .endAt(out.left());

        _ = seq.leftSide("")
            .wireH(.{ .bits = 2, .class="control" })
            .label("SEQ_OP", .{})
            .bitMarkAt(0.3)
            .endAt(in.right());

        _ = seq.leftSide("").wireH(.{ .class="control" }).label("ALLOW_INT", .{}).endAt(in.right());

        _ = seq.leftSide("").wireH(.{}).label("RESET", .{}).endAt(in.right());
        _ = seq.leftSide("").wireH(.{}).label("WANT_ATOMIC", .{}).endAt(in.right());
        _ = seq.leftSide("").wireH(.{}).label("STALL_ATOMIC", .{}).endAt(in.right());
        _ = seq.leftSide("").wireH(.{}).label("PAGE_FAULT", .{}).endAt(in.right());
        _ = seq.leftSide("").wireH(.{}).label("PAGE_ALIGN_FAULT", .{}).endAt(in.right());
        _ = seq.leftSide("").wireH(.{}).label("ACCESS_FAULT", .{}).endAt(in.right());
        _ = seq.leftSide("").wireH(.{}).label("INSN_FAULT", .{}).endAt(in.right());
        _ = seq.leftSide("").wireH(.{}).label("INT_PEND", .{}).endAt(in.right());


        _ = seq.rightSide("")
            .wireH(.{ .bits = 2 })
            .label("&#181;CA_SEL", .{})
            .bitMark()
            .turnAndEndAt(uca_mux.topSide(""));

        _ = seq.rightSide("")
            .wireH(.{ .bits = 3 })
            .label("&#181;CA_LIT", .{})
            .turn()
            .bitMark()
            .endAtPoint(uca_lit_zx.topSide(""));

        _ = uca_lit_zx.bottomSide("")
            .wireV(.{ .bits = 12 })
            .bitMark()
            .turnAndEndAt(uca_mux.leftSide(""));






        _ = id_rom.leftSide("")
            .wireH(.{ .bits = 16 })
            .label("ID", .{})
            .bitMark()
            .endAt(in.right());

        _ = id_rom.leftSide("")
            .wireH(.{ .class = "control" })
            .label("ID_MODE", .{})
            .endAt(in.right());

        const opcode = id_rom.rightSide("")
            .wireH(.{ .bits = 12 })
            .label("OPCODE", .{})
            .bitMark();




        const ii_columns = d.columns();
        _ = ii_columns.center().attachBetween(id_rom.right(), uca_mux.left(), 0.5);

        _ = id_rom.rightSide("")
            .wireH(.{ .bits = 4 })
            .label("IIJ", .{})
            .bitMark()
            .turnAt(ii_columns.push())
            .turnAndEndAt(ij_mux.getLeftSide(0));

        _ = id_rom.rightSide("")
            .wireH(.{ .bits = 4 })
            .label("IIK", .{})
            .bitMark()
            .turnAt(ii_columns.push())
            .turnAndEndAt(ik_mux.getLeftSide(0));

        _ = id_rom.rightSide("")
            .wireH(.{ .bits = 4 })
            .label("IIW", .{})
            .bitMark()
            .turnAt(ii_columns.push())
            .turnAndEndAt(iw_mux.getLeftSide(0));

        ii_columns.interface.flip();

        _ = uca_lit_zx.y().attachBetween(seq.bottom(), c_mask.top(), 0.9);
        _ = uca_lit_zx.x().attachTo(ii_columns.get(2));


        _ = c_mask.leftSide("").wireH(.{ .bits = 12, .class="control" }).label("C", .{}).bitMarkAt(0.3).endAt(in.right());
        _ = c_mask.leftSide("").wireH(.{ .bits = 2, .class="control" }).label("IJ_SEL", .{}).bitMarkAt(0.3).endAt(in.right());
        _ = c_mask.leftSide("").wireH(.{ .bits = 2, .class="control" }).label("IK_SEL", .{}).bitMarkAt(0.3).endAt(in.right());
        _ = c_mask.leftSide("").wireH(.{ .bits = 2, .class="control" }).label("IW_SEL", .{}).bitMarkAt(0.3).endAt(in.right());
        _ = c_mask.rightSide("").wireH(.{ .bits = 12 }).label("C", .{}).bitMark().turnAt(ii_columns.get(1)).turnAndEndAt(uca_mux.leftSide(""));
        _ = uca_mux.leftSide("").wireH(.{ .bits = 12 }).label("&#181;CA", .{}).bitMark().endAt(in.right());
        _ = opcode.turnAt(ii_columns.get(2)).turnAndEndAt(uca_mux.leftSide(""));


        // TODO make a cleaner API for this in Interface
//        d.state.constrainOffset(&uca_mux.getLeftInterface().span.end, uca_mux.bottom()._y, -20, "asdf");



        _ = uc_rom.leftSide("")
            .wireH(.{ .bits = 5 })
            .length(-50)
            .bitMark()
            .turn()
            .turnAtOffset(seq.top(), -50)
            .label("KVCNZ", .{})
            .endAtMutablePoint(in.rightSide(""));

        const uca_to_uc_rom = uca_mux.rightSide("")
            .wireH(.{ .bits = 12 })
            .length(50)
            .bitMark();

        _ = uca_to_uc_rom
            .turn()
            .turn()
            .bitMark()
            .endAtPoint(uc_rom.leftSide(""));

        _ = uca_to_uc_rom.endpoint()
            .wireV(.{ .bits = 12, .dir = .junction_begin })
            .turnAtOffset(uc_rom.bottom(), 25)
            .label("&#181;CA", .{ .alignment = .right })
            .bitMark()
            .endAt(out.left());


        _ = uc_rom.height(620);
        _ = uc_rom.rightSide("").wireH(.{ .bits = 6, .class="control" }).bitMarkAt(0.3).label("LITERAL", .{ .alignment = .right }).endAt(out.left());
        _ = uc_rom.rightSide("").wireH(.{ .bits = 12, .class="control" }).bitMarkAt(0.3).label("C", .{ .alignment = .right }).endAt(out.left());
        _ = uc_rom.rightSide("").wireH(.{ .bits = 1, .class="control" }).label("ID_MODE", .{ .alignment = .right }).endAt(out.left());
        _ = uc_rom.rightSide("").wireH(.{ .bits = 2, .class="control" }).bitMarkAt(0.3).label("IJ_SEL", .{ .alignment = .right }).endAt(out.left());
        _ = uc_rom.rightSide("").wireH(.{ .bits = 2, .class="control" }).bitMarkAt(0.3).label("IK_SEL", .{ .alignment = .right }).endAt(out.left());
        _ = uc_rom.rightSide("").wireH(.{ .bits = 2, .class="control" }).bitMarkAt(0.3).label("IW_SEL", .{ .alignment = .right }).endAt(out.left());
        _ = uc_rom.rightSide("").wireH(.{ .bits = 1, .class="control" }).label("JKR_WMODE", .{ .alignment = .right }).endAt(out.left());
        _ = uc_rom.rightSide("").wireH(.{ .bits = 2, .class="control" }).bitMarkAt(0.3).label("JL_SRC", .{ .alignment = .right }).endAt(out.left());
        _ = uc_rom.rightSide("").wireH(.{ .bits = 3, .class="control" }).bitMarkAt(0.3).label("JH_SRC", .{ .alignment = .right }).endAt(out.left());
        _ = uc_rom.rightSide("").wireH(.{ .bits = 3, .class="control" }).bitMarkAt(0.3).label("K_SRC", .{ .alignment = .right }).endAt(out.left());
        _ = uc_rom.rightSide("").wireH(.{ .bits = 3, .class="control" }).bitMarkAt(0.3).label("SR1_RI", .{ .alignment = .right }).endAt(out.left());
        _ = uc_rom.rightSide("").wireH(.{ .bits = 3, .class="control" }).bitMarkAt(0.3).label("SR2_RI", .{ .alignment = .right }).endAt(out.left());
        _ = uc_rom.rightSide("").wireH(.{ .bits = 3, .class="control" }).bitMarkAt(0.3).label("SR1_WI", .{ .alignment = .right }).endAt(out.left());
        _ = uc_rom.rightSide("").wireH(.{ .bits = 3, .class="control" }).bitMarkAt(0.3).label("SR2_WI", .{ .alignment = .right }).endAt(out.left());
        _ = uc_rom.rightSide("").wireH(.{ .bits = 2, .class="control" }).bitMarkAt(0.3).label("SR1_WSRC", .{ .alignment = .right }).endAt(out.left());
        _ = uc_rom.rightSide("").wireH(.{ .bits = 2, .class="control" }).bitMarkAt(0.3).label("SR2_WSRC", .{ .alignment = .right }).endAt(out.left());
        _ = uc_rom.rightSide("").wireH(.{ .bits = 4, .class="control" }).bitMarkAt(0.3).label("BASE", .{ .alignment = .right }).endAt(out.left());
        _ = uc_rom.rightSide("").wireH(.{ .bits = 2, .class="control" }).bitMarkAt(0.3).label("OFFSET", .{ .alignment = .right }).endAt(out.left());
        _ = uc_rom.rightSide("").wireH(.{ .bits = 4, .class="control" }).bitMarkAt(0.3).label("MODE", .{ .alignment = .right }).endAt(out.left());
        _ = uc_rom.rightSide("").wireH(.{ .bits = 2, .class="control" }).bitMarkAt(0.3).label("BUS_MODE", .{ .alignment = .right }).endAt(out.left());
        _ = uc_rom.rightSide("").wireH(.{ .bits = 1, .class="control" }).label("BUS_BYTE", .{ .alignment = .right }).endAt(out.left());
        _ = uc_rom.rightSide("").wireH(.{ .bits = 1, .class="control" }).label("BUS_RW", .{ .alignment = .right }).endAt(out.left());
        _ = uc_rom.rightSide("").wireH(.{ .bits = 2, .class="control" }).bitMarkAt(0.3).label("AT_OP", .{ .alignment = .right }).endAt(out.left());
        _ = uc_rom.rightSide("").wireH(.{ .bits = 3, .class="control" }).bitMarkAt(0.3).label("SPECIAL", .{ .alignment = .right }).endAt(out.left());
        _ = uc_rom.rightSide("").wireH(.{ .bits = 4, .class="control" }).bitMarkAt(0.3).label("LL_SRC", .{ .alignment = .right }).endAt(out.left());
        _ = uc_rom.rightSide("").wireH(.{ .bits = 4, .class="control" }).bitMarkAt(0.3).label("LH_SRC", .{ .alignment = .right }).endAt(out.left());
        _ = uc_rom.rightSide("").wireH(.{ .bits = 4, .class="control" }).bitMarkAt(0.3).label("STAT_OP", .{ .alignment = .right }).endAt(out.left());
        _ = uc_rom.rightSide("").wireH(.{ .bits = 2, .class="control" }).bitMarkAt(0.3).label("IDR_OP", .{ .alignment = .right }).endAt(out.left());
        _ = uc_rom.rightSide("").wireH(.{ .bits = 2, .class="control" }).bitMarkAt(0.3).label("SEQ_OP", .{ .alignment = .right }).endAt(out.left());
        _ = uc_rom.rightSide("").wireH(.{ .bits = 1, .class="control" }).label("ALLOW_INT", .{ .alignment = .right }).endAt(out.left());

        seq.getRightInterface().spacing = 100;
        uc_rom.getLeftInterface().spacing = 200;
        _ = uc_rom.top().anchorAt(0);
        _ = uc_rom.left().attachToOffset(uca_mux.right(), 100);


        _ = out.left().attachToOffset(uc_rom.right(), 150);
        _ = out.top().attachTo(in.top());
        _ = out.bottom().attachToOffset(iw_mux.bottom(), 0);

        _ = in.bottom().attachToOffset(iw_mux.bottom(), 60);
        _ = end.attachTo(out.x());

        return .{
            .out = out,
        };
    }

    fn index_mux(d: *Drawing, in: *Box, out: *Box, id_rom: *Box, comptime name: []const u8, continuation_bits: []const u8) *Box {
        const mux = d.box(.{ .shape = .mux }).height(80);

        _ = mux.leftSide("");
        _ = mux.leftSide("")
            .wireH(.{ .bits = 4, .class = "control" })
            .continueAt(id_rom.x())
            .label(continuation_bits, .{})
            .bitMark()
            .endAtMutablePoint(in.rightSide(""));
        const wire = mux.leftSide("")
            .wireH(.{ .bits = 4 })
            .continueAt(id_rom.x())
            .label(name, .{})
            .bitMark()
            .endAtMutablePoint(in.rightSide(""));

        const xor_wire = mux.leftSide("")
            .wireH(.{ .bits = 4 });

        const xor = d.box(.{ .shape = .small, .label = "^1" });
        _ = xor.x().attachTo(id_rom.right());
        _ = xor.y().attachTo(xor_wire.y());

        _ = xor_wire.endAt(xor.right());

        _ = xor.leftSide("")
            .wireH(.{ .bits = 4, .dir = .junction_end })
            .turnAndEndAt(wire.origin());

        _ = mux.bottomSide("")
            .wireV(.{ .bits = 2, .class = "control" })
            .length(30)
            .turn()
            .label(name ++ "_SEL", .{})
            .bitMark()
            .endAtMutablePoint(in.rightSide(""));
        _ = mux.rightSide("")
            .wireH(.{ .bits = 4 })
            .label(name, .{ .alignment = .right })
            .bitMark()
            .endAt(out.left());

        return mux;
    }
};

const Setup = struct {
    out: *Box,

    pub fn init(d: *Drawing, decode: Decode, end: XRef) Setup {
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

        _ = jr.left().attachToOffset(in.right(), 200);
        _ = jr.top().anchorAt(50);
        _ = sr1.topCenter().attachToOffset(jr.bottomCenter(), 0, 30);
        _ = sr2.topCenter().attachToOffset(sr1.bottomCenter(), 0, 30);
        _ = kr.topCenter().attachToOffset(sr2.bottomCenter(), 0, 75);
        _ = sr1b.topCenter().attachToOffset(kr.bottomCenter(), 0, 300);
        _ = sr2b.topCenter().attachToOffset(sr1b.bottomCenter(), 0, 25);

        _ = jhmux.left().attachToOffset(jr.right(), 360);
        _ = jhmux.top().anchorAt(80);
        _ = jlmux.topLeft().attachToOffset(jhmux.bottomLeft(), 0, 50);
        _ = kmux.topLeft().attachToOffset(jlmux.bottomLeft(), 0, 125);

        _ = jh_sx_jr.x().attachToOffset(jhmux.left(), -100);
        _ = k_zx_ij_ik.x().attachTo(jh_sx_jr.x());
        _ = k_zx_ik.right().attachToOffset(jhmux.left(), -50);
        _ = k_bit_ik.x().attachTo(jh_sx_jr.x());
        _ = k_zx_literal.x().attachToOffset(kr.left(), 0);
        _ = k_1x_literal.x().attachToOffset(kr.left(), 50);
        _ = offset_zx_literal.x().attachTo(k_zx_literal.x());
        _ = offset_1x_literal.x().attachTo(k_1x_literal.x());

        _ = offset_mux.left().attachToOffset(sr1b.right(), 100);
        _ = base_mux.left().attachTo(offset_mux.left());
        _ = base_mux.top().attachToOffset(sr1b.top(), 25);
        _ = agu.y().attachBetween(offset_mux.y(), base_mux.y(), 0.5);
        _ = agu.left().attachToOffset(offset_mux.right(), 100);

        _ = jhmux.topSide("").wireV(.{ .bits = 3, .class="control" })
            .turnAtOffset(jr.top(), -25)
            .bitMark()
            .label("JH_SRC", .{})
            .endAt(in.right());

        _ = jlmux.bottomSide("").wireV(.{ .bits = 2, .class="control" })
            .turnAtOffset(sr2.bottom(), 25)
            .bitMark()
            .label("JL_SRC", .{})
            .endAt(in.right());

        _ = kmux.topSide("").wireV(.{ .bits = 3, .class="control" })
            .turnAtOffset(kr.top(), -25)
            .bitMark()
            .label("K_SRC", .{})
            .endAt(in.right());


        const left_cols = d.columns();
        _ = left_cols.right().attachToOffset(jr.left(), -60);

        const cols = d.columns();
        _ = cols.center().attachToOffset(jr.right(), 150);
        _ = cols.push();
        _ = cols.push();
        _ = cols.push();
        _ = cols.push();
        _ = cols.push();
        _ = cols.push();

        // JR inputs
        _ = jr.leftSide("")
            .wireH(.{ .bits = 6 })
            .bitMark()
            .continueAt(left_cols.push())
            .label("RSN", .{})
            .endAt(in.right());
        _ = jr.leftSide("")
            .wireH(.{ .bits = 4 })
            .bitMark()
            .continueAt(left_cols.get(0))
            .label("IJ", .{})
            .endAt(in.right());

        // SR2B inputs
        const rsn_wire = sr2b.leftSide("")
            .wireH(.{ .bits = 6, .dir = .junction_end })
            .label("RSN", .{ .alignment = .right })
            .bitMark()
            .turnAt(left_cols.get(0))
            .endAt(jr.getLeftSide(0).y());

        // KR inputs
        _ = kr.leftSide("")
            .wireH(.{ .bits = 6, .dir = .junction_end })
            .label("RSN", .{ .alignment = .right })
            .bitMark()
            .endAt(rsn_wire.x());
        _ = kr.leftSide("")
            .wireH(.{ .bits = 4 })
            .bitMark()
            .continueAt(rsn_wire.x())
            .label("IK", .{})
            .endAt(in.right());

        // K mux inputs using IJ/IK/LITERAL
        k_zx_ij_ik.getLeftInterface().spacing = 12;
        _ = k_zx_ij_ik.leftSide("")
            .wireH(.{ .bits = 4, .dir = .junction_end })
            .turnAt(cols.get(4))
            .turnAtOffset(kr.bottom(), 25)
            .label("IJ", .{ .alignment = .right })
            .bitMark()
            .turnAt(left_cols.push())
            .endAt(jr.getLeftSide(1).y());
        _ = k_bit_ik.leftSide("")
            .wireH(.{ .bits = 4, .dir = .junction_end })
            .continueAt(cols.get(5))
            .label("IK", .{ .alignment = .right })
            .bitMark()
            .turnAt(left_cols.push())
            .endAt(kr.getLeftSide(1).y());
        _ = k_zx_ij_ik.leftSide("")
            .wireH(.{ .bits = 4, .dir = .junction_end })
            .turnAt(cols.get(5))
            .endAt(k_bit_ik.getLeftSide(0).y());
        _ = k_zx_ik.leftSide("")
            .wireH(.{ .bits = 4, .dir = .junction_end })
            .endAt(cols.get(5));
        _ = k_zx_literal.leftSide("")
            .wireH(.{ .bits = 6, .class="control" })
            .label("LITERAL", .{})
            .bitMark()
            .endAt(in.right());
        _ = k_1x_literal.leftSide("")
            .wireH(.{ .bits = 6, .class="control", .dir = .junction_end })
            .length(-75)
            .turn()
            .endAt(k_zx_literal.getLeftSide(0).y());

        // OFFSET mux inputs using LITERAL
        _ = offset_zx_literal.leftSide("")
            .wireH(.{ .bits = 6, .class="control", .dir = .junction_end })
            .length(-25)
            .turn()
            .endAt(k_1x_literal.getLeftSide(0).y());
        _ = offset_1x_literal.leftSide("")
            .wireH(.{ .bits = 6, .class="control", .dir = .junction_end })
            .length(-75)
            .turn()
            .endAt(offset_zx_literal.getLeftSide(0).y());

        // SR1 inputs
        _ = sr1.leftSide("")
            .wireH(.{ .bits = 6, .dir = .junction_end })
            .label("RSN", .{ .alignment = .right })
            .bitMark()
            .endAt(rsn_wire.x());
        _ = sr1.leftSide("")
            .wireH(.{ .bits = 3, .class="control" })
            .bitMark()
            .continueAt(rsn_wire.x())
            .label("SR1_RI", .{})
            .endAt(in.right());

        // SR2 inputs
        _ = sr2.leftSide("")
            .wireH(.{ .bits = 6, .dir = .junction_end })
            .label("RSN", .{ .alignment = .right })
            .bitMark()
            .endAt(rsn_wire.x());
        _ = sr2.leftSide("")
            .wireH(.{ .bits = 3, .class="control" })
            .bitMark()
            .continueAt(rsn_wire.x())
            .label("SR2_RI", .{})
            .endAt(in.right());


        // SR1B inputs
        _ = sr1b.leftSide("")
            .wireH(.{ .bits = 6, .dir = .junction_end })
            .label("RSN", .{ .alignment = .right })
            .bitMark()
            .endAt(rsn_wire.x());
        _ = sr1b.leftSide("")
            .wireH(.{ .bits = 3, .class="control" })
            .bitMark()
            .continueAt(rsn_wire.x())
            .label("BASE[2:0]", .{})
            .endAt(in.right());

        // SR2B inputs (again)
        _ = sr2b.leftSide("")
            .wireH(.{ .bits = 3, .class="control", .dir = .junction_end })
            .bitMark()
            .continueAt(rsn_wire.x())
            .turnAt(left_cols.get(1))
            .endAt(sr1b.getLeftSide(1).y());


        const mux_left_50 = jhmux.left().offset(-50);

        // JH mux
        _ = jhmux.leftSide("")
            .wireH(.{ .bits = 16 })
            .label("0x0000", .{ .alignment = .right, .baseline = .middle })
            .bitMark()
            .length(-50);
        _ = jhmux.leftSide("")
            .wireH(.{ .bits = 16 })
            .label("0xFFFF", .{ .alignment = .right, .baseline = .middle })
            .bitMark()
            .length(-50);
        _ = jr.rightSide("")
            .wireH(.{ .bits = 16 })
            .label("JR[31:16]", .{})
            .turnAt(cols.get(5))
            .turnAt(jhmux.leftSide("").y())
            .continueAt(mux_left_50)
            .bitMark()
            .endAt(jhmux.left());
        _ = sr1.rightSide("")
            .wireH(.{ .bits = 16 })
            .label("SR1[31:16]", .{})
            .turnAt(cols.get(0))
            .turnAt(jhmux.leftSide("").y())
            .continueAt(mux_left_50)
            .bitMark()
            .endAt(jhmux.left());
        _ = sr2.rightSide("")
            .wireH(.{ .bits = 16 })
            .label("SR1[31:16]", .{})
            .turnAt(cols.get(1))
            .turnAt(jhmux.leftSide("").y())
            .continueAt(mux_left_50)
            .bitMark()
            .endAt(jhmux.left());
        _ = jhmux.leftSide("")
            .wireH(.{ .bits = 16 })
            .bitMark()
            .continueAt(mux_left_50)
            .endAt(jh_sx_jr.right())
            .y().attach(jh_sx_jr.y());

        _ = jh_sx_jr.leftSide("")
            .wireH(.{ .dir = .junction_end })
            .label("JR[15]", .{ .alignment = .right })
            .endAt(cols.get(4));


        // JL mux
        _ = jlmux.leftSide("")
            .wireH(.{ .bits = 16 })
            .label("0x0000", .{ .alignment = .right, .baseline = .middle })
            .bitMark()
            .length(-50);
        _ = jr.rightSide("")
            .wireH(.{ .bits = 16 })
            .label("JR[15:0]", .{})
            .turnAt(cols.get(4))
            .turnAt(jlmux.leftSide("").y())
            .continueAt(mux_left_50)
            .bitMark()
            .endAt(jlmux.left());
        _ = sr1.rightSide("")
            .wireH(.{ .bits = 16 })
            .label("SR1[15:0]", .{})
            .turnAt(cols.get(3))
            .turnAt(jlmux.leftSide("").y())
            .continueAt(mux_left_50)
            .bitMark()
            .endAt(jlmux.left());
        _ = sr2.rightSide("")
            .wireH(.{ .bits = 16 })
            .label("SR2[15:0]", .{})
            .turnAt(cols.get(2))
            .turnAt(jlmux.leftSide("").y())
            .continueAt(mux_left_50)
            .bitMark()
            .endAt(jlmux.left());

        // K mux
        _ = kr.rightSide("")
            .wireH(.{ .bits = 16 })
            .label("KR", .{})
            .turnAt(cols.get(0))
            .turnAt(kmux.leftSide("").y())
            .continueAt(mux_left_50)
            .bitMark()
            .endAt(kmux.left());
        _ = kmux.leftSide("")
            .wireH(.{ .bits = 16, .dir = .junction_end })
            .bitMark()
            .continueAt(mux_left_50)
            .turnAt(cols.get(3))
            .endAt(jlmux.getLeftSide(2).y());
        _ = kmux.leftSide("")
            .wireH(.{ .bits = 16, .dir = .junction_end })
            .bitMark()
            .continueAt(mux_left_50)
            .turnAt(cols.get(2))
            .endAt(jlmux.getLeftSide(3).y());
        _ = kmux.leftSide("")
            .wireH(.{ .bits = 16 })
            .bitMark()
            .continueAt(mux_left_50)
            .endAt(k_zx_ij_ik.right())
            .y().attach(k_zx_ij_ik.y());
        _ = kmux.leftSide("")
            .wireH(.{ .bits = 16 })
            .bitMark()
            .continueAt(mux_left_50)
            .endAt(k_zx_ik.right())
            .y().attach(k_zx_ik.y());
        _ = kmux.leftSide("")
            .wireH(.{ .bits = 16 })
            .bitMark()
            .continueAt(mux_left_50)
            .endAt(k_bit_ik.right())
            .y().attach(k_bit_ik.y());
        _ = kmux.leftSide("")
            .wireH(.{ .bits = 16 })
            .bitMark()
            .continueAt(mux_left_50)
            .endAt(k_zx_literal.right())
            .y().attach(k_zx_literal.y());
        _ = kmux.leftSide("")
            .wireH(.{ .bits = 16 })
            .bitMark()
            .continueAt(mux_left_50)
            .endAt(k_1x_literal.right())
            .y().attach(k_1x_literal.y());


        // OFFSET mux
        const offset_mux_left_50 = offset_mux.left().offset(-50);
        _ = offset_mux.top().attachToOffset(kmux.bottom(), 20);
        _ = offset_mux.leftSide("")
            .wireH(.{ .bits = 32 })
            .bitMark()
            .continueAt(offset_mux_left_50)
            .endAt(offset_zx_literal.right())
            .y().attach(offset_zx_literal.y());
        _ = offset_mux.leftSide("")
            .wireH(.{ .bits = 32 })
            .bitMark()
            .continueAt(offset_mux_left_50)
            .endAt(offset_1x_literal.right())
            .y().attach(offset_1x_literal.y());
        _ = offset_mux.leftSide("")
            .wireH(.{ .bits = 32 })
            .label("0x00000000", .{ .alignment = .right, .baseline = .middle })
            .bitMark()
            .length(-50);
        _ = offset_mux.leftSide("")
            .wireH(.{ .bits = 32 })
            .label("0x00000002", .{ .alignment = .right, .baseline = .middle })
            .bitMark()
            .length(-50);

        _ = offset_mux.bottomSide("")
            .wireV(.{ .bits = 2, .class="control" })
            .length(35)
            .turn()
            .label("OFFSET", .{})
            .endAt(in.right());



        // BASE mux
        _ = base_mux.leftSide("")
            .wireH(.{ .bits = 32 })
            .bitMark()
            .turnAt(d.betweenX(sr1b.right(), base_mux.left(), 0.5))
            .turnAndEndAt(sr1b.rightSide(""));
        _ = base_mux.leftSide("")
            .wireH(.{ .bits = 32 })
            .bitMark()
            .turnAt(d.betweenX(sr1b.right(), base_mux.left(), 0.5))
            .turnAndEndAt(sr2b.rightSide(""));
        _ = base_mux.topSide("")
            .wireV(.{ .bits = 1, .class="control" })
            .turnAtOffset(sr1b.top(), -25)
            .label("BASE[3]", .{})
            .endAt(in.right());

        // AGU
        const between_agu_and_muxes = d.betweenX(offset_mux.right(), agu.left(), 0.5);
        agu.getLeftInterface().spacing = 75;
        _ = offset_mux.rightSide("")
            .wireH(.{ .bits = 32 })
            .bitMark()
            .turnAt(between_agu_and_muxes)
            .turnAndEndAt(agu.leftSide(""));
        _ = base_mux.rightSide("")
            .wireH(.{ .bits = 32 })
            .bitMark()
            .turnAt(between_agu_and_muxes)
            .turnAndEndAt(agu.leftSide(""));

        // Outputs
        _ = jhmux.rightSide("")
            .wireH(.{ .bits = 16 })
            .bitMark()
            .label("JH", .{ .alignment = .right })
            .endAt(out.left());

        _ = jlmux.rightSide("")
            .wireH(.{ .bits = 16 })
            .bitMark()
            .label("JL", .{ .alignment = .right })
            .endAt(out.left());

        _ = kmux.rightSide("")
            .wireH(.{ .bits = 16 })
            .bitMark()
            .label("K", .{ .alignment = .right })
            .endAt(out.left());

        agu.getRightInterface().spacing = 75;
        _ = agu.rightSide("")
            .wireH(.{ .bits = 20 })
            .bitMark()
            .label("P", .{ .alignment = .right })
            .endAt(out.left());
        _ = agu.rightSide("")
            .wireH(.{ .bits = 12 })
            .bitMark()
            .label("N", .{ .alignment = .right })
            .endAt(out.left());


        left_cols.interface.flip();

        _ = out.left().attachToOffset(jhmux.right(), 150);
        _ = out.top().attachTo(in.top());
        _ = out.bottom().attachToOffset(agu.bottom(), 0);

        _ = end.attachTo(out.x());

        return .{
            .out = out,
        };
    }
};

const Compute = struct {
    out: *Box,

    pub fn init(d: *Drawing, setup: Setup, end: XRef) Compute {
        const in = setup.out;
        const out = d.box(.{ .class="pipeline" }).width(50);

        const arith_bottom = arith(d, in, out);
        const shifter_bottom = shifter(d, in, out, arith_bottom);
        _ = shifter_bottom;

        _ = out.top().attachTo(in.top());
        _ = end.attachTo(out.x());

        return .{
            .out = out,
        };
    }

    fn arith(d: *Drawing, in: *Box, out: *Box) YRef {
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
        const cin_wire = adder.topSide("C0")
            .wireV(.{})
            .length(-25)
            .turn();
        _ = cin_wire.endpoint()
            .attach(c_inv.middleRight())
            .x().attachTo(k_inv.right());
        _ = adder.leftSide("")
            .wireH(.{ .bits = 16 })
            .bitMark()
            .label("JH", .{})
            .endAt(in.right());
        _ = adder.leftSide("")
            .wireH(.{ .bits = 16 })
            .bitMark()
            .label("JL", .{})
            .endAt(in.right());
        _ = adder.leftSide("");
        _ = adder.leftSide("");
        _ = adder.leftSide("");
        _ = adder.leftSide("")
            .wireH(.{ .bits = 32 })
            .bitMark()
            .length(-40)
            .endpoint().attach(k_inv.middleRight());
        _ = k_inv.leftSide("")
            .wireH(.{ .bits = 32 })
            .bitMark()
            .length(-40)
            .endpoint().attach(k_mux.middleRight());

        // Cin processing
        _ = c_inv.leftSide("")
            .wireH(.{})
            .length(-40)
            .endpoint().attach(c_and.middleRight());
        _ = c_and.leftSide("")
            .wireH(.{})
            .label("STAT_C", .{})
            .endAt(in.right());
        _ = c_and.topSide("")
            .wireV(.{ .class="control" })
            .length(-25)
            .turn()
            .label("MODE[1]", .{})
            .endAt(in.right());
        _ = c_inv.bottomSide("")
            .wireV(.{ .class="control" })
            .endAt(k_inv.top());


        // K Mux
        _ = k_mux.leftSide("")
            .wireH(.{ .bits = 32 })
            .bitMark()
            .length(-40)
            .endpoint().attach(k_zx.middleRight());
        _ = k_mux.leftSide("")
            .wireH(.{ .bits = 32 })
            .bitMark()
            .continueAt(k_mux.left().offset(-40))
            .length(-40)
            .endpoint().attach(k_sx.middleRight());
        _ = k_mux.leftSide("")
            .wireH(.{ .bits = 32 })
            .bitMark()
            .length(-40)
            .endpoint().attach(k_1x.middleRight());
        const mode32 = k_mux.bottomSide("")
            .wireV(.{ .bits = 2, .class="control" })
            .length(75)
            .turn()
            .label("MODE[3:2]", .{})
            .bitMark()
            .endAt(in.right());

        const k_wire = k_sx.leftSide("")
            .wireH(.{ .bits = 16 })
            .length(-25);
        _ = k_wire.turn().turnAt(k_wire.y())
            .label("K", .{})
            .bitMark()
            .endAt(in.right());

        _ = k_zx.leftSide("")
            .wireH(.{ .bits = 16, .dir = .junction_end })
            .turnAndEndAt(k_wire.endpoint());
        _ = k_1x.leftSide("")
            .wireH(.{ .bits = 16, .dir = .junction_end })
            .turnAndEndAt(k_wire.endpoint());


        _ = adder.rightSide("")
            .wireH(.{ .bits = 16 })
            .label("ARITH_H", .{ .alignment = .right })
            .bitMark()
            .endAt(out.left());
        _ = adder.rightSide("")
            .wireH(.{ .bits = 16 })
            .label("ARITH_L", .{ .alignment = .right })
            .bitMark()
            .endAt(out.left());

        _ = adder.bottomSide("C16")
            .wireV(.{})
            .turnAndEndAt(cout_mux.leftSide(""));
        _ = adder.bottomSide("C32")
            .wireV(.{})
            .turnAndEndAt(cout_mux.leftSide(""));

        _ = cout_mux.bottomSide("")
            .wireV(.{})
            .turnAt(mode32.y())
            .length(-100)
            .endpoint().attach(cout_dec.middleRight());
        _ = cout_dec.leftSide("")
            .wireH(.{ .bits = 2, .class="control", .dir = .junction_end })
            .endAt(mode32.origin().x());

        _ = cout_mux.topLeft().attachToOffset(adder.bottomRight(), 50, 10);
        cout_mux.getLeftInterface().flip();

        _ = cout_mux.rightSide("")
            .wireH(.{})
            .length(25)
            .endpoint().attach(cout_inv.middleLeft());
        _ = cout_inv.rightSide("")
            .wireH(.{})
            .label("ARITH_C", .{ .alignment = .right })
            .endAt(out.left());

        const mode0 = cout_inv.bottomSide("")
            .wireV(.{ .class="control" })
            .turnAtOffset(mode32.y(), 20)
            .label("MODE[0]", .{})
            .endAt(in.right());
        _ = k_inv.bottomSide("")
            .wireV(.{ .class="control", .dir = .junction_end })
            .endAt(mode0.y());


        _ = znv_mux.bottomCenter().attachToOffset(cout_mux.topCenter(), 0, -30);
        _ = znv_mux.leftSide("")
            .wireH(.{ .bits = 2 })
            .bitMark()
            .endAtMutablePoint(adder.rightSide("ZNV32"));
        _ = znv_mux.leftSide("")
            .wireH(.{ .bits = 2 })
            .bitMark()
            .endAtMutablePoint(adder.rightSide("ZNV16"));
        _ = znv_mux.rightSide("")
            .wireH(.{ .bits = 2 })
            .label("ARITH_ZNV", .{ .alignment = .right })
            .bitMark()
            .endAt(out.left());
        _ = znv_mux.bottomSide("")
            .wireV(.{})
            .endAt(cout_mux.top());

        _ = adder.rightSide("");
        _ = adder.rightSide("");
        _ = adder.rightSide("");
        _ = adder.rightSide(""); // TODO box interface alignment


        _ = adder.topLeft().attachToOffset(in.topRight(), 350, 90);

        _ = out.left().attachToOffset(cout_inv.right(), 75);
        _ = out.bottom().attachToOffset(cout_inv.bottom(), 0);

        return mode0.y();
    }

    fn shifter(d: *Drawing, in: *Box, out: *Box, top: YRef) YRef {
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

        _ = early_reverse_h.top().attachTo(top_y);
        _ = early_reverse_l.top().attachTo(early_reverse_h.bottom());
        _ = early_swap16.top().attachTo(top_y);
        _ = early_swap16.bottom().attachTo(early_reverse_l.bottom());
        _ = early_mask_high.top().attachTo(top_y);
        _ = shift.top().attachTo(top_y);
        _ = shift.bottom().attachTo(early_swap16.bottom());
        _ = late_swap16.top().attachTo(top_y);
        _ = late_swap16.bottom().attachTo(early_reverse_l.bottom());
        _ = late_reverse_h.top().attachTo(top_y);
        _ = late_reverse_l.top().attachTo(early_reverse_h.bottom());

        _ = early_reverse_h.left().attachToOffset(in.right(), 100);
        _ = early_reverse_l.left().attachToOffset(in.right(), 100);
        _ = early_swap16.left().attachToOffset(early_reverse_h.right(), 40);
        _ = early_mask_high.left().attachToOffset(early_swap16.right(), 40);
        _ = shift.left().attachToOffset(early_mask_high.right(), 40);

        _ = late_swap16.left().attachToOffset(shift.right(), 40);
        _ = late_reverse_h.left().attachToOffset(late_swap16.right(), 40);
        _ = late_reverse_l.left().attachToOffset(late_swap16.right(), 40);

        

        return early_reverse_l.bottom();
    }

};

const Transact = struct {
    pub fn init(d: *Drawing, compute: Compute, end: XRef) Transact {
        _ = compute;
        _ = end;
        _ = d;
        return .{};
    }
};



const Drawing = zbox.Drawing;
const Box = zbox.Box;
const XRef = zbox.XRef;
const YRef = zbox.YRef;

const zbox = @import("zbox");
const std = @import("std");
