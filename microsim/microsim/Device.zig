ctx: *anyopaque,
slot: ?u3,
debug_name: []const u8,
read_fn: *const fn(ctx: *anyopaque, ctrl: Bus_Control) ?Bus_Data,
write_fn: *const fn(ctx: *anyopaque, ctrl: Bus_Control, data: Bus_Data) void,
update_fn: ?*const fn(ctx: *anyopaque) void,

pub fn init(comptime T: type, device: *T, slot: ?u3) Device {
    const wrap = struct {
        fn read(ctx: *anyopaque, ctrl: Bus_Control) ?Bus_Data {
            const dev: *T = @ptrCast(@alignCast(ctx));
            return @call(.always_inline, T.read, .{ dev, ctrl });
        }
        fn write(ctx: *anyopaque, ctrl: Bus_Control, data: Bus_Data) void {
            const dev: *T = @ptrCast(@alignCast(ctx));
            @call(.always_inline, T.write, .{ dev, ctrl, data });
        }
        fn update(ctx: *anyopaque) void {
            const dev: *T = @ptrCast(@alignCast(ctx));
            @call(.always_inline, T.update, .{ dev });
        }
    };

    return .{
        .ctx = device,
        .slot = slot,
        .debug_name = if (@hasField(T, "name")) device.name else @typeName(T),
        .read_fn = wrap.read,
        .write_fn = wrap.write,
        .update_fn = if (@hasDecl(T, "update")) wrap.update else null,
    };
}

pub fn updatable(self: Device) ?Updatable_Device {
    if (self.update_fn) |func| {
        return .{
            .ctx = self.ctx,
            .update_fn = func,
        };
    }
    return null;
}

pub const Updatable_Device = struct {
    ctx: *anyopaque,
    update_fn: *const fn(ctx: *anyopaque) void,
};

pub fn log_contention(self: Device, ucycle: u64) void {
    if (self.slot) |slot| {
        log.warn("ucycle {}: Read data contention detected!  Device: {s} (slot {})", .{
            ucycle,
            self.debug_name,
            slot,
        });
    } else {
        log.warn("ucycle {}: Read data contention detected!  Device: {s}", .{
            ucycle,
            self.debug_name,
        });
    }
}

const log = std.log.scoped(.device);
const Device = @This();
const Bus_Control = @import("Bus_Control.zig");
const Bus_Data = @import("Bus_Data.zig");
const arch = @import("arch");
const std = @import("std");
