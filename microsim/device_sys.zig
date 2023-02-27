const std = @import("std");
const sim = @import("simulator");
const misc = @import("misc");

const base_frame = misc.Device_Frames.sys_interrupt_controller;
const ram_end_frame = misc.physicalAddressToFrame(misc.Special_Physical_Address.ram_end);
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

pub fn read(state: *State, bus_ctrl: sim.BusControl, address: sim.PhysicalAddress) ?misc.D_Bus {
    if (!bus_ctrl.read) return null;

    switch (@intToEnum(misc.Device_Frames, address.frame)) {
        .sys_accessed_frames => {
            var data: ?misc.D_Bus = null;
            if (address.offset < num_ram_frame_chunks) {
                data = state.accessed_frames[address.offset];
                state.accessed_frames[address.offset] = 0;
            }
            return data;
        },
        .sys_dirty_frames => {
            var data: ?misc.D_Bus = null;
            if (address.offset < num_ram_frame_chunks) {
                data = state.dirty_frames[address.offset];
                state.dirty_frames[address.offset] = 0;
            }
            return data;
        },
        else => return null,
    }
}

pub fn write(state: *State, bus_ctrl: sim.BusControl, address: sim.PhysicalAddress, data: misc.D_Bus, update_frame_state: bool) void {
    switch (@intToEnum(misc.Device_Frames, address.frame)) {
        .sys_accessed_frames => {
            if (bus_ctrl.write and address.offset < num_ram_frame_chunks) {
                state.accessed_frames[address.offset] = @truncate(u8, data);
            }
        },
        .sys_dirty_frames => {
            if (bus_ctrl.write and address.offset < num_ram_frame_chunks) {
                state.dirty_frames[address.offset] = @truncate(u8, data);
            }
        },
        else => {
            if (update_frame_state and address.frame <= ram_end_frame) {
                const frame_chunk = address.frame / 8;
                const frame_chunk_mask: u8 = @as(u8, 1) << @truncate(u3, address.frame);

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
