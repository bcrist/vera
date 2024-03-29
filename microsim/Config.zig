const config_filename = "microsim-config.sx";

const expr_root = "microsim-config";

const expr_window = "window";
const expr_frames = "frames";

const expr_position = "position";
const expr_size = "size";
const expr_maximized = "maximized";
const expr_collapsed = "collapsed";

allocator: std.mem.Allocator,
window: ?Gui.Window_Settings,
frames: []zgui.WindowSettings,

pub fn init(alloc: std.mem.Allocator, gui: *const Gui) !Config {
    const frame_settings = try zgui.getWindowSettings(alloc);
    return .{
        .allocator = alloc,
        .window = .{
            .pos = gui.window.getPos(),
            .size = gui.window.getSize(),
            .maximized = gui.window.getAttribute(.maximized),
        },
        .frames = frame_settings,
    };
}

pub fn deinit(self: *Config) void {
    self.allocator.free(self.frames);
}

pub fn load(alloc: std.mem.Allocator, temp: std.mem.Allocator) !Config {
    const path = try std.fs.selfExeDirPathAlloc(temp);
    defer temp.free(path);
    var dir = try std.fs.openDirAbsolute(path, .{});
    defer dir.close();
    var file = dir.openFile(config_filename, .{}) catch |err| switch (err) {
        error.FileNotFound => return .{
            .allocator = alloc,
            .window = null,
            .frames = try alloc.alloc(zgui.WindowSettings, 0),
        },
        else => return err,
    };
    defer file.close();
    var reader = sx.reader(temp, file.reader());
    defer reader.deinit();

    return parse(alloc, temp, &reader) catch |err| switch (err) {
        error.SExpressionSyntaxError => {
            log.err("Failed to parse microsim-config.sx!", .{});
            const context = try reader.token_context();
            try context.print_for_file(&file, std.io.getStdErr().writer(), 160);
            return err;
        },
        else => return err,
    };
}

fn parse(alloc: std.mem.Allocator, temp: std.mem.Allocator, reader: *sx.Reader(std.fs.File.Reader)) !Config {
    var window_settings: ?Gui.Window_Settings = null;
    var frames = std.ArrayList(zgui.WindowSettings).init(temp);
    defer frames.deinit();

    try reader.require_expression(expr_root);

    while (true) {
        if (try reader.expression(expr_window)) {
            var settings = Gui.Window_Settings{
                .pos = .{ 100, 100 },
                .size = .{ 600, 400 },
                .maximized = false,
            };
            while (true) {
                if (try reader.expression(expr_position)) {
                    settings.pos[0] = try reader.require_any_int(i32, 10);
                    settings.pos[1] = try reader.require_any_int(i32, 10);
                    try reader.require_close();
                } else if (try reader.expression(expr_size)) {
                    settings.size[0] = try reader.require_any_int(i32, 10);
                    settings.size[1] = try reader.require_any_int(i32, 10);
                    try reader.require_close();
                } else if (try reader.expression(expr_maximized)) {
                    settings.maximized = true;
                    try reader.require_close();
                } else break;
            }
            try reader.require_close(); // expr_window
            window_settings = settings;
        } else if (try reader.expression(expr_frames)) {
            while (try reader.any_expression()) |name| {
                var frame_settings = try frames.addOne();
                frame_settings.name = try alloc.dupeZ(u8, name);
                while (true) {
                    if (try reader.expression(expr_position)) {
                        frame_settings.pos[0] = try reader.require_any_int(i16, 10);
                        frame_settings.pos[1] = try reader.require_any_int(i16, 10);
                        try reader.require_close();
                    } else if (try reader.expression(expr_size)) {
                        frame_settings.size[0] = try reader.require_any_int(i16, 10);
                        frame_settings.size[1] = try reader.require_any_int(i16, 10);
                        try reader.require_close();
                    } else if (try reader.expression(expr_collapsed)) {
                        frame_settings.collapsed = true;
                        try reader.require_close();
                    } else break;
                }
                try reader.require_close(); // expr_frames
            }
            try reader.require_close(); // expr_frames
        } else break;
    }

    try reader.require_close(); // expr_root
    try reader.require_done();

    return .{
        .allocator = alloc,
        .window = window_settings,
        .frames = try alloc.dupe(zgui.WindowSettings, frames.items),
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
const Gui = @import("gui/Gui.zig");
const zglfw = @import("zglfw");
const zgui = @import("zgui");
const sx = @import("sx");
const std = @import("std");
