// This device doesn't simulate the nuances of the real memory system (even/odd address calculation, byte swapping, etc.)
// and it doesn't include block transfer logic at all, but it's especially useful for running CPU/instruction validation tests.

allocator: std.mem.Allocator,
frame_begin: hw.addr.Frame,
frame_end: hw.addr.Frame,
data: []u8,

pub fn init(allocator: std.mem.Allocator, first_frame: hw.addr.Frame, num_frames: usize) !Simple_Memory {
    const data = try allocator.alloc(u8, hw.addr.Frame.num_addresses_per_frame * num_frames);
    return .{
        .allocator = allocator,
        .frame_begin = first_frame,
        .frame_end = hw.addr.Frame.init(@intCast(first_frame.raw() + num_frames)),
        .data = data,
    };
}

pub fn deinit(self: *Simple_Memory) void {
    self.allocator.free(self.data);
}

pub fn device(self: *Simple_Memory) Device {
    return Device.init(Simple_Memory, self, null);
}

pub fn read(self: *const Simple_Memory, addr: hw.addr.Physical, width: hw.Control_Signals.Bus_Width) ?hw.D {
    _ = width;

    if (addr.frame.raw() < self.frame_begin.raw()) return null;
    if (addr.frame.raw() >= self.frame_end.raw()) return null;

    var offset = addr;
    offset.frame = hw.addr.Frame.init(addr.frame.raw() - self.frame_begin.raw());
    const i = offset.raw();

    return hw.D.init(bits.concat(.{ self.data[i], self.data[i+1] }));
}

pub fn write(self: *Simple_Memory, addr: hw.addr.Physical, width: hw.Control_Signals.Bus_Width, data: hw.D) void {
    if (addr.frame.raw() < self.frame_begin.raw()) return;
    if (addr.frame.raw() >= self.frame_end.raw()) return;

    var offset = addr;
    offset.frame = hw.addr.Frame.init(addr.frame.raw() - self.frame_begin.raw());
    const i = offset.raw();

    self.data[i] = @truncate(data.raw());
    if (width == .word) {
        self.data[i+1] = @truncate(data.raw() >> 8);
    }
}

const Simple_Memory = @This();
const Device = @import("../Device.zig");
const hw = arch.hw;
const arch = @import("lib_arch");
const bits = @import("bits");
const std = @import("std");
