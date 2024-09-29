// This device doesn't include block transfer logic and stores all bytes in logical order,
// so it's useful for running CPU/instruction validation tests when you want to inspect
// memory externally.

allocator: std.mem.Allocator,
frame_begin: arch.addr.Frame,
frame_end: arch.addr.Frame,
data: []u8,

pub fn init(allocator: std.mem.Allocator, first_frame: arch.addr.Frame, num_frames: usize) !Simple_Memory {
    const data = try allocator.alloc(u8, arch.addr.Frame.num_bytes_per_frame * num_frames);
    return .{
        .allocator = allocator,
        .frame_begin = first_frame,
        .frame_end = arch.addr.Frame.init(@intCast(first_frame.raw() + num_frames)),
        .data = data,
    };
}

pub fn deinit(self: *Simple_Memory) void {
    self.allocator.free(self.data);
}

pub fn device(self: *Simple_Memory) Device {
    return Device.init(Simple_Memory, self, null);
}

pub fn read(self: *const Simple_Memory, ctrl: Bus_Control) ?Bus_Data {
    if (ctrl.frame.raw() < self.frame_begin.raw()) return null;
    if (ctrl.frame.raw() >= self.frame_end.raw()) return null;

    const base: usize = (ctrl.frame.raw() - self.frame_begin.raw()) * arch.addr.Frame.num_bytes_per_frame;
    const base_a = base + ctrl.aa.raw() * 4;
    const base_b = base + ctrl.ab.raw() * 4;

    return .{
        .da = .{
            .lo = self.data[base_a],
            .hi = self.data[base_a + 1],
        },
        .db = .{
            .lo = self.data[base_b],
            .hi = self.data[base_b + 1],
        },
    };
}

pub fn write(self: *Simple_Memory, ctrl: Bus_Control, data: Bus_Data) void {
    if (ctrl.frame.raw() < self.frame_begin.raw()) return;
    if (ctrl.frame.raw() >= self.frame_end.raw()) return;
    if (ctrl.guard_mismatch) return;
    if (ctrl.block_transfer) return;

    const base: usize = (ctrl.frame.raw() - self.frame_begin.raw()) * arch.addr.Frame.num_bytes_per_frame;
    const base_a = base + ctrl.aa.raw() * 4;
    const base_b = base + ctrl.ab.raw() * 4;

    if (ctrl.lba) self.data[base_a] = data.da.lo;
    if (ctrl.uba) self.data[base_a + 1] = data.da.hi;
    if (ctrl.lbb) self.data[base_b] = data.db.lo;
    if (ctrl.ubb) self.data[base_b + 1] = data.db.hi;
}

const Simple_Memory = @This();
const Device = @import("../Device.zig");
const Bus_Control = @import("../Bus_Control.zig");
const Bus_Data = @import("../Bus_Data.zig");
const arch = @import("arch");
const bits = @import("bits");
const std = @import("std");
