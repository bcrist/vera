log: std.ArrayList(Report),
current_microcycle: usize,

pub const Report = struct {
    microcycle: usize,
    pipeline: arch.Pipeline,
    action: Action,
};

pub const Action = union (enum) {
    rsn: arch.Register_Set_Number,
    reg: Reg_Info,
    sr: SR_Info,
    read: Bus_Info,
    write: Bus_Info,
    fault: arch.microcode.Slot,
};

pub const Reg_Info = struct {
    wi: arch.Write_Index,
    old_data: arch.Reg,
    new_data: arch.Reg,
};

pub const SR_Info = struct {
    index: arch.Any_SR_Index,
    old_data: arch.Reg,
    new_data: arch.Reg,
};

pub const Bus_Info = struct {
    ctrl: Bus_Control,
    data: Bus_Data,
};

pub fn init(gpa: std.mem.Allocator) Debug_Log {
    return .{
        .log = std.ArrayList(Report).init(gpa),
        .current_microcycle = 0,
    };
}

pub fn deinit(self: *Debug_Log) void {
    self.log.deinit();
}

pub fn clear(self: *Debug_Log) void {
    self.log.clearRetainingCapacity();
}

pub fn report(self: *Debug_Log, pipeline: arch.Pipeline, action: Action) void {
    self.log.append(.{
        .microcycle = self.current_microcycle,
        .pipeline = pipeline,
        .action = action,
    }) catch @panic("OOM");
}

pub fn dump(self: *Debug_Log, writer: anytype) !void {
    for (self.log.items) |r| {
        try writer.print("{:>6} {}: {s}", .{ r.microcycle, r.pipeline, @tagName(r.action) });
        switch (r.action) {
            .fault => |slot| try writer.print(": {}\n", .{ slot }),
            .rsn => |rsn| try writer.print(" = {}\n", .{ rsn }),
            .reg => |info| try writer.print(" {} = {}\n", .{ info.wi, info.new_data }),
            .sr => |info| try writer.print(" {} = {}\n", .{ info.index, info.new_data }),
            .read, .write => |info| try writer.print("{s}DB:{} DA:{} F:{} AB:{} AA:{} {s}{s}{s}{s} {s}{s}{s}\n", .{
                if (r.action == .read) "  " else " ",
                info.data.db,
                info.data.da,
                info.ctrl.frame,
                info.ctrl.ab,
                info.ctrl.aa,
                if (info.ctrl.ubb) "B" else ".",
                if (info.ctrl.lbb) "b" else ".",
                if (info.ctrl.uba) "A" else ".",
                if (info.ctrl.lba) "a" else ".",
                if (info.ctrl.update_frame_state) "U" else " ",
                if (info.ctrl.guard_mismatch) "!" else " ",
                if (info.ctrl.block_transfer) " block_transfer" else "",
            }),
        }
    }
}

pub fn expect(self: *Debug_Log, expected: []const u8) !void {
    var temp = std.ArrayList(u8).init(std.testing.allocator);
    defer temp.deinit();
    try self.dump(temp.writer());
    try std.testing.expectEqualStrings(expected, temp.items);
}

const Debug_Log = @This();

const Bus_Control = @import("Bus_Control.zig");
const Bus_Data = @import("Bus_Data.zig");
const arch = @import("arch");
const std = @import("std");
