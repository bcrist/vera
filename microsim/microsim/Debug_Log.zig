gpa: std.mem.Allocator,
log: std.ArrayList(Report),
current_microcycle: usize,

pub const Report = struct {
    microcycle: usize,
    pipeline: arch.Pipeline,
    ucs: arch.microcode.Slot,
    action: Action,
};

pub const Action = union (enum) {
    rsn: arch.reg.RSN,
    reg: Reg_Info,
    sr1: SR1_Info,
    sr2: SR2_Info,
    read: Bus_Info,
    write: Bus_Info,
    fault: arch.microcode.Slot,
    corrupted_microcode: []const u8,
};

pub const Reg_Info = struct {
    rsn: arch.reg.RSN,
    wi: arch.reg.gpr.Write_Index,
    old_data: arch.reg.gpr.Value,
    new_data: arch.reg.gpr.Value,
};

pub const SR1_Info = struct {
    rsn: arch.reg.RSN,
    index: arch.reg.sr1.Index,
    old_data: arch.reg.sr1.Value,
    new_data: arch.reg.sr1.Value,
};

pub const SR2_Info = struct {
    rsn: arch.reg.RSN,
    index: arch.reg.sr2.Index,
    old_data: arch.reg.sr2.Value,
    new_data: arch.reg.sr2.Value,
};

pub const Bus_Info = struct {
    space: arch.addr.Space,
    ctrl: Bus_Control,
    data: Bus_Data,
};

pub fn init(gpa: std.mem.Allocator) Debug_Log {
    return .{
        .gpa = gpa,
        .log = .empty,
        .current_microcycle = 0,
    };
}

pub fn deinit(self: *Debug_Log) void {
    self.log.deinit(self.gpa);
}

pub fn clear(self: *Debug_Log) void {
    self.log.clearRetainingCapacity();
}

pub fn report(self: *Debug_Log, pipeline: arch.Pipeline, ucs: arch.microcode.Slot, action: Action) void {
    self.log.append(self.gpa, .{
        .microcycle = self.current_microcycle,
        .pipeline = pipeline,
        .ucs = ucs,
        .action = action,
    }) catch @panic("OOM");
}

const Dump_Options = struct {
    insn_stuff: bool = false,
};

pub fn dump(self: *Debug_Log, writer: ?*std.io.Writer, options: Dump_Options) !void {
    var buf: [64]u8 = undefined;
    var stderr = std.fs.File.stderr().writer(&buf);
    var w = writer orelse &stderr.interface;

    for (self.log.items) |r| {
        switch (r.action) {
            .fault => |slot| {
                try dump_prefix(r, w, options);
                try w.print(": {f}\n", .{ slot });
            },
            .rsn => |rsn| {
                try dump_prefix(r, w, options);
                try w.print(" = {f}\n", .{ rsn });
            },
            .reg => |info| {
                try dump_prefix(r, w, options);
                try w.print(" {f} = {f}\n", .{ info.wi, info.new_data });
            },
            .sr1 => |info| {
                if (options.insn_stuff or switch (info.index) {
                    .temp_1, .int_flags, .fault_d, .fault_dr, .fault_ir => false,
                    else => true,
                }) {
                    try dump_prefix(r, w, options);
                    try w.print(" {f} = {f}\n", .{ info.index, info.new_data });
                }
            },
            .sr2 => |info| {
                if (options.insn_stuff or switch (info.index) {
                    .ip, .next_ip, .temp_2 => false,
                    else => true,
                }) {
                    try dump_prefix(r, w, options);
                    try w.print(" {f} = {f}\n", .{ info.index, info.new_data });
                }
            },
            .read, .write => |info| {
                if (options.insn_stuff or info.space != .insn) {
                    try dump_prefix(r, w, options);
                    try w.print("{s}{f} DB:{f} DA:{f} F:{f} AB:{f} AA:{f} {s}{s}{s}{s} {s}\n", .{
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
                        if (info.ctrl.guard_mismatch) "!" else " ",
                    });
                }
            },
            .corrupted_microcode => |what| {
                try dump_prefix(r, w, options);
                try w.print(" {s}", .{ what });
            },
        }
    }

    try w.flush();
}

fn dump_prefix(r: Report, w: *std.io.Writer, options: Dump_Options) !void {
    if (options.insn_stuff) {
        try w.print("{:>6} {f} {f}: {t}", .{
            r.microcycle,
            r.pipeline,
            r.ucs,
            r.action,
        });
    } else {
        try w.print("{:>6} {f}: {t}", .{
            r.microcycle,
            r.pipeline,
            r.action,
        });
    }
}

pub fn expect(self: *Debug_Log, options: Dump_Options, expected: []const u8) !void {
    var temp = std.io.Writer.Allocating.init(std.testing.allocator);
    defer temp.deinit();
    try self.dump(&temp.writer, options);
    try std.testing.expectEqualStrings(expected, temp.written());
}

const Debug_Log = @This();

const Bus_Control = @import("Bus_Control.zig");
const Bus_Data = @import("Bus_Data.zig");
const arch = @import("arch");
const std = @import("std");
