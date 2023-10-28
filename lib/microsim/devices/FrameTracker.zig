const std = @import("std");
const bus = @import("bus_types");
const physical_address = @import("physical_address");
const SystemBusControl = @import("Simulator").SystemBusControl;

const base_frame = physical_address.DeviceFrame.sys_interrupt_controller;
const ram_end_frame = physical_address.toFrame(physical_address.ram_end);
const num_ram_frame_chunks = (ram_end_frame / 8) + 1;

const FrameTracker = @This();

accessed_frames: [num_ram_frame_chunks]u8 = [_]u8{0} ** num_ram_frame_chunks,
dirty_frames: [num_ram_frame_chunks]u8 = [_]u8{0} ** num_ram_frame_chunks,

pub fn reset(self: *FrameTracker) void {
    self.accessed_frames = .{0} ** num_ram_frame_chunks;
    self.dirty_frames = .{0} ** num_ram_frame_chunks;
}

pub fn randomize(self: *FrameTracker, rnd: std.rand.Random) void {
    rnd.bytes(std.mem.sliceAsBytes(self.accessed_frames[0..]));
    rnd.bytes(std.mem.sliceAsBytes(self.dirty_frames[0..]));
}

pub fn read(self: *const FrameTracker, bus_ctrl: SystemBusControl) ?bus.D {
    if (!bus_ctrl.read) return null;

    switch (@intToEnum(physical_address.DeviceFrame, bus_ctrl.address.frame)) {
        .sys_accessed_frames => {
            if (bus_ctrl.address.offset < num_ram_frame_chunks) {
                return self.accessed_frames[bus_ctrl.address.offset];
            }
        },
        .sys_dirty_frames => {
            if (bus_ctrl.address.offset < num_ram_frame_chunks) {
                return self.dirty_frames[bus_ctrl.address.offset];
            }
        },
        else => {},
    }

    return null;
}

pub fn transact(self: *FrameTracker, bus_ctrl: SystemBusControl, data: bus.D, update_frame_state: bool) void {
    switch (@intToEnum(physical_address.DeviceFrame, bus_ctrl.address.frame)) {
        .sys_accessed_frames => {
            if (bus_ctrl.address.offset < num_ram_frame_chunks) {
                if (bus_ctrl.read) {
                    self.accessed_frames[bus_ctrl.address.offset] = 0;
                }
                if (bus_ctrl.write) {
                    self.accessed_frames[bus_ctrl.address.offset] = @truncate(u8, data);
                }
            }
        },
        .sys_dirty_frames => {
            if (bus_ctrl.address.offset < num_ram_frame_chunks) {
                if (bus_ctrl.read) {
                    self.dirty_frames[bus_ctrl.address.offset] = 0;
                }
                if (bus_ctrl.write) {
                    self.dirty_frames[bus_ctrl.address.offset] = @truncate(u8, data);
                }
            }
        },
        else => {
            if (update_frame_state and bus_ctrl.address.frame <= ram_end_frame) {
                const frame_chunk = bus_ctrl.address.frame / 8;
                const frame_chunk_mask: u8 = @as(u8, 1) << @truncate(u3, bus_ctrl.address.frame);

                if (bus_ctrl.read) {
                    self.accessed_frames[frame_chunk] |= frame_chunk_mask;
                }
                if (bus_ctrl.write) {
                    self.accessed_frames[frame_chunk] |= frame_chunk_mask;
                    self.dirty_frames[frame_chunk] |= frame_chunk_mask;
                }
            }
        },
    }
}
