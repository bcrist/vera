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
    rsn: arch.Register_Set_Number,
    wi: arch.Write_Index,
    old_data: arch.Reg,
    new_data: arch.Reg,
};

pub const SR_Info = struct {
    rsn: arch.Register_Set_Number,
    index: arch.reg.sr.Any_Index,
    old_data: arch.Reg,
    new_data: arch.Reg,
};

pub const Bus_Info = struct {
    space: arch.addr.Space,
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

const Dump_Options = struct {
    insn_stuff: bool = false,
};

pub fn dump(self: *Debug_Log, writer: anytype, options: Dump_Options) !void {
    for (self.log.items) |r| {
        switch (r.action) {
            .fault => |slot| {
                try writer.print("{:>6} {}: {s}: {}\n", .{ r.microcycle, r.pipeline, @tagName(r.action), slot });
            },
            .rsn => |rsn| {
                try writer.print("{:>6} {}: {s} = {}\n", .{ r.microcycle, r.pipeline, @tagName(r.action), rsn });
            },
            .reg => |info| {
                try writer.print("{:>6} {}: {s} {} = {}\n", .{ r.microcycle, r.pipeline, @tagName(r.action), info.wi, info.new_data });
            },
            .sr => |info| {
                if (options.insn_stuff or switch (info.index) {
                    .temp_1, .int_stat, .fault_stat, .fault_dr, .fault_ir, .ip, .next_ip, .temp_2 => false,
                    else => true,
                }) {
                    try writer.print("{:>6} {}: {s} {} = {}\n", .{ r.microcycle, r.pipeline, @tagName(r.action), info.index, info.new_data });
                }
            },
            .read, .write => |info| {
                if (options.insn_stuff or info.space != .insn) {
                    try writer.print("{:>6} {}: {s}{s}{} DB:{} DA:{} F:{} AB:{} AA:{} {s}{s}{s}{s} {s}{s}{s}\n", .{
                        r.microcycle, r.pipeline, @tagName(r.action),
                        if (r.action == .read) "  " else " ",
                        info.space,
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
                    });
                }
            },
        }
    }
}

pub fn expect(self: *Debug_Log, options: Dump_Options, expected: []const u8) !void {
    var temp = std.ArrayList(u8).init(std.testing.allocator);
    defer temp.deinit();
    try self.dump(temp.writer(), options);
    try std.testing.expectEqualStrings(expected, temp.items);
}

const Debug_Log = @This();

const Bus_Control = @import("Bus_Control.zig");
const Bus_Data = @import("Bus_Data.zig");
const arch = @import("arch");
const std = @import("std");
