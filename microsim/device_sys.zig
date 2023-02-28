const std = @import("std");
const sim = @import("Simulator");
const bus = @import("bus");
const physical_address = @import("physical_address");

const SystemBusControl = @import("SystemBusControl.zig");

const base_frame = physical_address.DeviceFrame.sys_interrupt_controller;
const ram_end_frame = physical_address.toFrame(physical_address.ram_end);
const num_ram_frame_chunks = (ram_end_frame / 8) + 1;

pub const State = struct {
    accessed_frames: [num_ram_frame_chunks]u8 = [_]u8{0} ** num_ram_frame_chunks,
    dirty_frames: [num_ram_frame_chunks]u8 = [_]u8{0} ** num_ram_frame_chunks,

    pub fn reset(self: *State) void {
        self.accessed_frames = .{0} ** num_ram_frame_chunks;
        self.dirty_frames = .{0} ** num_ram_frame_chunks;
    }

    pub fn randomize(self: *State, rnd: std.rand.Random) void {
        rnd.bytes(std.mem.sliceAsBytes(self.accessed_frames[0..]));
        rnd.bytes(std.mem.sliceAsBytes(self.dirty_frames[0..]));
    }
};

pub fn read(state: *State, bus_ctrl: SystemBusControl) ?bus.D {
    if (!bus_ctrl.read) return null;

    switch (@intToEnum(physical_address.DeviceFrame, bus_ctrl.address.frame)) {
        .sys_accessed_frames => {
            var data: ?bus.D = null;
            if (bus_ctrl.address.offset < num_ram_frame_chunks) {
                data = state.accessed_frames[bus_ctrl.address.offset];
                state.accessed_frames[bus_ctrl.address.offset] = 0;
            }
            return data;
        },
        .sys_dirty_frames => {
            var data: ?bus.D = null;
            if (bus_ctrl.address.offset < num_ram_frame_chunks) {
                data = state.dirty_frames[bus_ctrl.address.offset];
                state.dirty_frames[bus_ctrl.address.offset] = 0;
            }
            return data;
        },
        else => return null,
    }
}

pub fn write(state: *State, bus_ctrl: SystemBusControl, data: bus.D, update_frame_state: bool) void {
    switch (@intToEnum(physical_address.DeviceFrame, bus_ctrl.address.frame)) {
        .sys_accessed_frames => {
            if (bus_ctrl.write and bus_ctrl.address.offset < num_ram_frame_chunks) {
                state.accessed_frames[bus_ctrl.address.offset] = @truncate(u8, data);
            }
        },
        .sys_dirty_frames => {
            if (bus_ctrl.write and bus_ctrl.address.offset < num_ram_frame_chunks) {
                state.dirty_frames[bus_ctrl.address.offset] = @truncate(u8, data);
            }
        },
        else => {
            if (update_frame_state and bus_ctrl.address.frame <= ram_end_frame) {
                const frame_chunk = bus_ctrl.address.frame / 8;
                const frame_chunk_mask: u8 = @as(u8, 1) << @truncate(u3, bus_ctrl.address.frame);

                if (bus_ctrl.read) {
                    state.accessed_frames[frame_chunk] |= frame_chunk_mask;
                }
                if (bus_ctrl.write) {
                    state.accessed_frames[frame_chunk] |= frame_chunk_mask;
                    state.dirty_frames[frame_chunk] |= frame_chunk_mask;
                }
            }
        },
    }
}
