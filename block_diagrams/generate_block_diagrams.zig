pub fn main() !void {
    var d = zbox.Drawing.init(gpa);
    defer d.deinit();
    d.title = "Architecture Block Diagram";
    d.style.bus_style.junction_radius = 4;
    d.style.bus_style.bit_mark_label_offset_y = 5;
    d.style.bus_style.bit_mark_length = 7;
    d.style.box_padding_x = 4;

    const phase_label_y = d.y(-200);

    const decode_in = d.box(.{ .class = "reg" }).width(50);
    _ = decode_in.x().attach(d.separator_v()
        .label(phase_label_y, "Decode", .{ .alignment = .right, .baseline = .hanging })
        .x()
    );

    const decode_out_setup_in = d.box(.{ .class = "reg" }).width(50);
    _ = decode_out_setup_in.x().attach(d.separator_v()
        .label(phase_label_y, "Decode", .{ .alignment = .right })
        .label(phase_label_y, "Setup", .{ .alignment = .right, .baseline = .hanging })
        .x()
    );
    const setup_out_compute_in = d.box(.{ .class = "reg" }).width(50);
    _ = setup_out_compute_in.x().attach(d.separator_v()
        .label(phase_label_y, "Setup", .{ .alignment = .right })
        .label(phase_label_y, "Compute", .{ .alignment = .right, .baseline = .hanging })
        .x()
    );
    const compute_out_transact_in = d.box(.{ .class = "reg" }).width(50);
    _ = compute_out_transact_in.x().attach(d.separator_v()
        .label(phase_label_y, "Compute", .{ .alignment = .right })
        .label(phase_label_y, "Transact", .{ .alignment = .right, .baseline = .hanging })
        .x()
    );
    const transact_out = d.box(.{ .class = "reg" }).width(50);
    _ = transact_out.x().attach(d.separator_v()
        .label(phase_label_y, "Transact", .{ .alignment = .right })
        .x()
    );

    var decode: Decode = .init(d, decode_in, decode_out_setup_in);
    var setup: Setup = .init(d, decode_out_setup_in, setup_out_compute_in);
    var compute: Compute = .init(d, setup_out_compute_in, compute_out_transact_in);
    var transact: Transact = .init(d, compute_out_transact_in, transact_out);

    decode.config();
    setup.config();
    compute.config();
    transact.config();

    _ = decode_in.top().attach_to_offset(decode.v_extents.input_min, -25);
    _ = decode_in.bottom().attach_to_offset(decode.v_extents.input_max, 25);

    _ = decode_out_setup_in.top().attach_to_min_offset(&.{ decode.v_extents.output_min, setup.v_extents.input_min }, -25);
    _ = decode_out_setup_in.bottom().attach_to_max_offset(&.{ decode.v_extents.output_max, setup.v_extents.input_max }, 25);

    _ = setup_out_compute_in.top().attach_to_min_offset(&.{ setup.v_extents.output_min, compute.v_extents.input_min }, -25);
    _ = setup_out_compute_in.bottom().attach_to_max_offset(&.{ setup.v_extents.output_max, compute.v_extents.input_max }, 25);

    _ = compute_out_transact_in.top().attach_to_min_offset(&.{ compute.v_extents.output_min, transact.v_extents.input_min }, -25);
    _ = compute_out_transact_in.bottom().attach_to_max_offset(&.{ compute.v_extents.output_max, transact.v_extents.input_max }, 25);

    _ = transact_out.top().attach_to_offset(transact.v_extents.output_min, -25);
    _ = transact_out.bottom().attach_to_offset(transact.v_extents.output_max, 25);

    try d.render_svg_to_file(std.fs.cwd(), "block_diagram.svg");
}

const Decode = @import("src/Decode.zig");
const Setup = @import("src/Setup.zig");
const Compute = @import("src/Compute.zig");
const Transact = @import("src/Transact.zig");

const gpa = std.heap.smp_allocator;

const Drawing = zbox.Drawing;
const Box = zbox.Box;
const X_Ref = zbox.X_Ref;
const Y_Ref = zbox.Y_Ref;

const zbox = @import("zbox");
const std = @import("std");
