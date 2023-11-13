ctx: *anyopaque,
slot: ?u3,
debug_name: []const u8,
read_fn: *const fn(ctx: *anyopaque, addr: hw.addr.Physical, width: hw.Control_Signals.Bus_Width) ?hw.D,
write_fn: *const fn(ctx: *anyopaque, addr: hw.addr.Physical, width: hw.Control_Signals.Bus_Width, data: hw.D) void,
update_fn: ?*const fn(ctx: *anyopaque, simulator: *Simulator) void,

pub fn init(comptime T: type, device: *T, slot: ?u3) Device {
    const wrap = struct {
        fn read(ctx: *anyopaque, addr: hw.addr.Physical, width: hw.Control_Signals.Bus_Width) ?hw.D {
            const dev: *T = @ptrCast(@alignCast(ctx));
            return @call(.always_inline, T.read, .{ dev, addr, width });
        }
        fn write(ctx: *anyopaque, addr: hw.addr.Physical, width: hw.Control_Signals.Bus_Width, data: hw.D) void {
            const dev: *T = @ptrCast(@alignCast(ctx));
            @call(.always_inline, T.write, .{ dev, addr, width, data });
        }
        fn update(ctx: *anyopaque, simulator: *Simulator) void {
            const dev: *T = @ptrCast(@alignCast(ctx));
            @call(.always_inline, T.update, .{ dev, simulator });
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
    update_fn: *const fn(ctx: *anyopaque, simulator: *Simulator) void,
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
const Simulator = @import("Simulator.zig");
const hw = arch.hw;
const arch = @import("lib_arch");
const std = @import("std");
