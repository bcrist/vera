const config_filename = "simconfig.sx";

window: ?struct {
    pos: ?ig.internal.Vec2ih = null,
    maximized: ?bool = null,
} = null,
gui: ?struct {
    frame: []const ig.Window_Settings_Info = &.{},
    table: []const Table_Settings_Info = &.{},
    settings: []const Generic_Settings_Info = &.{},
} = null,

const Table_Settings_Info = struct {
    id: ig.ID = 0,
    ref_scale: ?f32 = null,
    col: []const ig.Table_Column_Settings_Info,
};

const Generic_Settings_Info = struct {
    handler_name: []const u8 = "",
    settings_name: []const u8 = "",
    prop: []const Property = &.{},
    data: []const []const u8 = &.{},

    const Property = struct {
        key: []const u8 = "",
        value: []const u8 = "",
    };
};

const context = struct {
    pub const window = struct {
        pub const pos = struct {
            pub const inline_fields = &.{ "x", "y" };
        };
    };
    pub const gui = struct {
        pub const frame = struct {
            pub const inline_fields = &.{ "window_name" };
            pub const viewport = struct {
                pub const inline_fields = &.{ "id" };
                pub const pos = struct {
                    pub const inline_fields = &.{ "x", "y" };
                };
            };
            pub const pos = struct {
                pub const inline_fields = &.{ "x", "y" };
            };
            pub const size = struct {
                pub const inline_fields = &.{ "x", "y" };
            };
        };
        pub const table = struct {
            pub const inline_fields = &.{ "id" };
            pub const id = "0x{X:0>8}";
            pub const col = struct {
                pub const inline_fields = &.{ "index" };
                pub const sort = struct {
                    pub const inline_fields = &.{ "column", "direction" };
                };
            };
        };
        pub const settings = struct {
            pub const inline_fields = &.{ "handler_name", "settings_name" };
            pub const prop = struct {
                pub const inline_fields = &.{ "key", "value" };
            };
        };
    };
};

pub fn load(arena: std.mem.Allocator) !void {
    const path = try std.fs.selfExeDirPathAlloc(temp);
    defer temp.free(path);

    var dir = try std.fs.openDirAbsolute(path, .{});
    defer dir.close();

    var file = dir.openFile(config_filename, .{}) catch |err| switch (err) {
        error.FileNotFound => return,
        else => return err,
    };
    defer file.close();
    const reader = file.reader();
    var r = sx.reader(temp, reader.any());
    defer r.deinit();

    const config = r.require_object(temp, Config, context) catch |err| switch (err) {
        error.SExpressionSyntaxError => {
            log.err("Failed to parse microsim-config.sx!", .{});
            const ctx = try r.token_context();
            try ctx.print_for_file(&file, std.io.getStdErr().writer(), 160);
            return err;
        },
        else => return err,
    };
}

pub fn save(self: Config, temp: std.mem.Allocator) !void {
    const path = try std.fs.selfExeDirPathAlloc(temp);
    defer temp.free(path);
    var dir = try std.fs.openDirAbsolute(path, .{});
    defer dir.close();
    var file = try dir.createFile(config_filename, .{});
    defer file.close();
    var writer = sx.writer(temp, file.writer());
    defer writer.deinit();

    try writer.expression_expanded(expr_root);

    if (self.window) |window| {
        try writer.expression_expanded(expr_window);

        try writer.expression(expr_position);
        try writer.int(window.pos[0], 10);
        try writer.int(window.pos[1], 10);
        try writer.close(); // expr_position

        try writer.expression(expr_size);
        try writer.int(window.size[0], 10);
        try writer.int(window.size[1], 10);
        try writer.close(); // expr_size

        if (window.maximized) {
            try writer.expression(expr_maximized);
            try writer.close();
        }

        try writer.close(); // expr_window
    }

    try writer.expression_expanded(expr_frames);
    for (self.frames) |frame| {
        try writer.expression_expanded(frame.name);

        try writer.expression(expr_position);
        try writer.int(frame.pos[0], 10);
        try writer.int(frame.pos[1], 10);
        try writer.close(); // expr_position

        try writer.expression(expr_size);
        try writer.int(frame.size[0], 10);
        try writer.int(frame.size[1], 10);
        try writer.close(); // expr_size

        if (frame.collapsed) {
            try writer.expression(expr_collapsed);
            try writer.close();
        }

        try writer.close(); // frame.name
    }
    try writer.close(); // expr_frames

    try writer.done(); // expr_root
}

const log = std.log.scoped(.microsim);

const Config = @This();

const ig = @import("ig");
const sx = @import("sx");
const std = @import("std");
