pub fn main() !void {
    var arg_iter = try std.process.argsWithAllocator(std.heap.smp_allocator);
    defer arg_iter.deinit();
    _ = arg_iter.next(); // executable
    try generate_pipeline_diagram(std.heap.smp_allocator, arg_iter.next().?, .initFull(), "Full Pipeline Block Diagram");
    try generate_pipeline_diagram(std.heap.smp_allocator, arg_iter.next().?, .initOne(.decode), "Decode Stage Block Diagram");
    try generate_pipeline_diagram(std.heap.smp_allocator, arg_iter.next().?, .initOne(.setup), "Setup Stage Block Diagram");
    try generate_pipeline_diagram(std.heap.smp_allocator, arg_iter.next().?, .initOne(.compute), "Compute Stage Block Diagram");
    try generate_pipeline_diagram(std.heap.smp_allocator, arg_iter.next().?, .initOne(.transact), "Transact Stage Block Diagram");
}

const Stage_Transition = enum (u3) {
    transact_decode_left = 0,
    decode_setup = 1,
    setup_compute = 2,
    compute_transact = 3,
    transact_decode_right = 4,

    pub fn prev(self: Stage_Transition) arch.Pipeline.Stage {
        return arch.Pipeline.Stage.init(@truncate(@intFromEnum(self) + 3));
    }

    pub fn next(self: Stage_Transition) arch.Pipeline.Stage {
        return arch.Pipeline.Stage.init(@truncate(@intFromEnum(self)));
    }

    pub fn left(self: Stage_Transition) ?arch.Pipeline.Stage {
        return if (self == .transact_decode_left) null else self.prev();
    }

    pub fn right(self: Stage_Transition) ?arch.Pipeline.Stage {
        return if (self == .transact_decode_right) null else self.next();
    }
};

fn capitalized_stage_name(comptime stage: arch.Pipeline.Stage) []const u8 {
    const raw = @tagName(stage);
    comptime var buf: [raw.len]u8 = undefined;
    @memcpy(&buf, raw);
    buf[0] = comptime std.ascii.toUpper(buf[0]);
    const final_name = buf;
    return &final_name;
}

fn generate_pipeline_diagram(gpa: std.mem.Allocator, filename: []const u8, stages: std.EnumSet(arch.Pipeline.Stage), title: []const u8) !void {
    var d = zbox.Drawing.init(gpa);
    defer d.deinit();
    d.title = title;
    d.style.bus_style.junction_radius = 4;
    d.style.bus_style.bit_mark_label_offset_y = 5;
    d.style.bus_style.bit_mark_length = 7;
    d.style.box_padding_x = 4;

    const phase_label_y = d.y(-200);

    var io_boxes: std.EnumMap(Stage_Transition, *zbox.Box) = .{};

    inline for (comptime std.enums.values(Stage_Transition)) |transition| {
        const want_l = if (transition.left()) |l| stages.contains(l) else false;
        const want_r = if (transition.right()) |r| stages.contains(r) else false;
        if (want_l or want_r) {
            const box = d.box(.{ .class = "reg" }).width(50);
            _ = box.x().attach(d.separator_v()
                .label(phase_label_y, capitalized_stage_name(transition.prev()), .{ .alignment = .right })
                .label(phase_label_y, capitalized_stage_name(transition.next()), .{ .alignment = .right, .baseline = .hanging })
                .x()
            );
            io_boxes.put(transition, box);
        }
    }

    var extents: std.EnumArray(Stage_Transition, std.MultiArrayList(V_Extents.Min_Max)) = .initFill(.empty);
    defer for (&extents.values) |*list| {
        list.deinit(gpa);
    };

    if (stages.contains(.decode)) {
        var decode: Decode = .init(d,
            io_boxes.getAssertContains(.transact_decode_left),
            io_boxes.getAssertContains(.decode_setup));
        decode.config();
        try extents.getPtr(.transact_decode_left).append(gpa, decode.v_extents.input);
        try extents.getPtr(.decode_setup).append(gpa, decode.v_extents.output);
    }

    if (stages.contains(.setup)) {
        var setup: Setup = .init(d,
            io_boxes.getAssertContains(.decode_setup),
            io_boxes.getAssertContains(.setup_compute));
        setup.config();
        try extents.getPtr(.decode_setup).append(gpa, setup.v_extents.input);
        try extents.getPtr(.setup_compute).append(gpa, setup.v_extents.output);
    }

    if (stages.contains(.compute)) {
        var compute: Compute = .init(d,
            io_boxes.getAssertContains(.setup_compute),
            io_boxes.getAssertContains(.compute_transact));
        
        compute.config();
        try extents.getPtr(.setup_compute).append(gpa, compute.v_extents.input);
        try extents.getPtr(.compute_transact).append(gpa, compute.v_extents.output);
    }

    if (stages.contains(.transact)) {
        var transact: Transact = .init(d,
            io_boxes.getAssertContains(.compute_transact),
            io_boxes.getAssertContains(.transact_decode_right));
        transact.config();
        try extents.getPtr(.compute_transact).append(gpa, transact.v_extents.input);
        try extents.getPtr(.transact_decode_right).append(gpa, transact.v_extents.output);
    }

    var iter = extents.iterator();
    while (iter.next()) |entry| {
        switch (entry.value.len) {
            0 => continue,
            1 => {
                const box = io_boxes.getAssertContains(entry.key);
                const item = entry.value.get(0);
                _ = box.top().attach_to_offset(item.min, -25);
                _ = box.bottom().attach_to_offset(item.max, 25);
            },
            else => {
                const box = io_boxes.getAssertContains(entry.key);
                _ = box.top().attach_to_min_offset(entry.value.items(.min), -25);
                _ = box.bottom().attach_to_max_offset(entry.value.items(.max), 25);
            },
        }
    }

    try d.render_svg_to_file(std.fs.cwd(), filename);
}

const Decode = @import("src/Decode.zig");
const Setup = @import("src/Setup.zig");
const Compute = @import("src/Compute.zig");
const Transact = @import("src/Transact.zig");
const V_Extents = @import("src/V_Extents.zig");

const Drawing = zbox.Drawing;
const Box = zbox.Box;
const X_Ref = zbox.X_Ref;
const Y_Ref = zbox.Y_Ref;

const arch = @import("arch");
const zbox = @import("zbox");
const std = @import("std");
