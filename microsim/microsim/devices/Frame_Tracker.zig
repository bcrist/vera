const frame_tracker_frame = arch.addr.Frame.init(arch.addr.Frame.device_0.raw() + 2);
const num_frame_chunks = arch.addr.Frame.count / 16;

accessed_frames: [num_frame_chunks]arch.DA = .{0} ** num_frame_chunks,
dirty_frames: [num_frame_chunks]arch.DB = .{0} ** num_frame_chunks,

pub fn device(self: *Frame_Tracker) Device {
    return Device.init(Frame_Tracker, self, null);
}

pub fn read(self: *const Frame_Tracker, ctrl: Bus_Control) ?Bus_Data {
    if (ctrl.frame == frame_tracker_frame) {
        var da: arch.DA = .{ .lo = 0, .hi = 0 };
        var db: arch.DB = .{ .lo = 0, .hi = 0 };

        const a = ctrl.aa.raw();
        if (a < num_frame_chunks) {
            if (ctrl.lba) da.lo = self.accessed_frames[a].lo;
            if (ctrl.uba) da.hi = self.accessed_frames[a].hi;
        }

        const b = ctrl.ab.raw();
        if (b < num_frame_chunks) {
            if (ctrl.lbb) db.lo = self.dirty_frames[b].lo;
            if (ctrl.ubb) db.hi = self.dirty_frames[b].hi;
        }

        return .{ .da = da, .db = db };
    } else if (ctrl.update_frame_state) {
        const addr = ctrl.frame / 16;
        const bit: u4 = @intCast(ctrl.frame & 0xF);
        const mask = @as(arch.DA.Raw, 1) << bit;
        self.accessed_frames[addr] = self.accessed_frames[addr].raw() | mask;
    }
    return null;
}

pub fn write(self: *Frame_Tracker, ctrl: Bus_Control, data: Bus_Data) void {
    if (ctrl.guard_mismatch) return;

    if (ctrl.frame == frame_tracker_frame) {
        const a = ctrl.aa.raw();
        if (a < num_frame_chunks) {
            if (ctrl.lba) self.accessed_frames[a].lo = data.da.lo;
            if (ctrl.uba) self.accessed_frames[a].hi = data.da.hi;
        }

        const b = ctrl.ab.raw();
        if (b < num_frame_chunks) {
            if (ctrl.lbb) self.dirty_frames[b].lo = data.db.lo;
            if (ctrl.ubb) self.dirty_frames[b].hi = data.db.hi;
        }

    } else if (ctrl.update_frame_state) {
        const addr = ctrl.frame / 16;
        const bit: u4 = @intCast(ctrl.frame & 0xF);
        const mask = @as(arch.DA.Raw, 1) << bit;
        self.accessed_frames[addr] = self.accessed_frames[addr].raw() | mask;
        self.dirty_frames[addr] = self.dirty_frames[addr].raw() | mask;
    }
}

const Frame_Tracker = @This();
const Device = @import("../Device.zig");
const Bus_Control = @import("../Bus_Control.zig");
const Bus_Data = @import("../Bus_Data.zig");
const arch = @import("arch");
const bits = @import("bits");
const std = @import("std");
