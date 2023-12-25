allocator: std.mem.Allocator,
decode_rom: []const hw.decode.Result,
microcode_rom: []const Control_Signals,
device_slots: [8]?*Device,
slotted_devices: []Device,
global_devices: []Device,
updatable_devices: []Updatable_Device,

reset: bool,
interrupts_pending: [hw.Pipeline.count]bool,
power: hw.Power_Mode,

registers: []hw.Register_Set,
translations: []at.Entry_Pair,

d: Decode_Stage,
dd: Decode_Stage.Debug,
s: Setup_Stage,
sd: Setup_Stage.Debug,
c: Compute_Stage,
cd: Compute_Stage.Debug,
t: Transact_Stage,
td: Transact_Stage.Debug,

microcycles_simulated: u64,

pub fn init(allocator: std.mem.Allocator, decode_rom: []const hw.decode.Result, microcode_rom: []const Control_Signals, devices: []const Device) !Simulator {
    const registers = try allocator.alloc(hw.Register_Set, hw.RSN.count);
    errdefer allocator.free(registers);
    @memset(registers, std.mem.zeroes(hw.Register_Set));

    const translations = try allocator.alloc(at.Entry_Pair, at.Entry_Address.count);
    errdefer allocator.free(translations);
    @memset(translations, std.mem.zeroes(at.Entry_Pair));

    var updatable_device_count: usize = 0;
    var slotted_device_count: usize = 0;
    for (devices) |d| {
        if (d.update_fn != null) updatable_device_count += 1;
        if (d.slot != null) slotted_device_count += 1;
    }

    var device_slots: [8]?*Device = .{ null } ** 8;
    const slotted_devices = try allocator.alloc(Device, slotted_device_count);
    errdefer allocator.free(slotted_devices);
    if (slotted_devices.len > 0) {
        var next: usize = 0;
        for (devices) |d| {
            if (d.slot) |slot| {
                if (device_slots[slot] != null) return error.DeviceSlotConflict;
                slotted_devices[next] = d;
                device_slots[slot] = &slotted_devices[next];
                next += 1;
            }
        }
    }

    const global_devices = try allocator.alloc(Device, devices.len - slotted_device_count);
    errdefer allocator.free(global_devices);
    if (global_devices.len > 0) {
        var next: usize = 0;
        for (devices) |d| {
            if (d.slot == null) {
                global_devices[next] = d;
                next += 1;
            }
        }
    }

    const updatable_devices = try allocator.alloc(Updatable_Device, updatable_device_count);
    errdefer allocator.free(updatable_devices);
    if (updatable_devices.len > 0) {
        var next: usize = 0;
        for (devices) |d| {
            if (d.updatable()) |ud| {
                updatable_devices[next] = ud;
                next += 1;
            }
        }
    }

    return .{
        .allocator = allocator,
        .decode_rom = decode_rom,
        .microcode_rom = microcode_rom,
        .device_slots = device_slots,
        .slotted_devices = slotted_devices,
        .global_devices = global_devices,
        .updatable_devices = updatable_devices,
        .reset = false,
        .interrupts_pending = .{ false } ** hw.Pipeline.count,
        .power = .run,
        .registers = registers,
        .translations = translations,
        .d = std.mem.zeroInit(Decode_Stage, .{ .cs = &microcode_rom[0] }),
        .dd = std.mem.zeroes(Decode_Stage.Debug),
        .s = std.mem.zeroInit(Setup_Stage, .{ .cs = &microcode_rom[0] }),
        .sd = std.mem.zeroes(Setup_Stage.Debug),
        .c = std.mem.zeroInit(Compute_Stage, .{ .cs = &microcode_rom[0] }),
        .cd = std.mem.zeroes(Compute_Stage.Debug),
        .t = std.mem.zeroInit(Transact_Stage, .{ .cs = &microcode_rom[0] }),
        .td = std.mem.zeroes(Transact_Stage.Debug),
        .microcycles_simulated = 0,
    };
}

pub fn deinit(self: *Simulator) void {
    self.allocator.free(self.translations);
    self.allocator.free(self.registers);
    self.allocator.free(self.updatable_devices);
    self.allocator.free(self.slotted_devices);
    self.allocator.free(self.global_devices);
}

pub fn simulate_microcycles(self: *Simulator, n: u64) void {
    var i: u64 = 0;
    while (i < n) : (i += 1) {
        const atomic_busy = self.s.is_atomic() or self.c.is_atomic() or self.t.is_atomic();

        for (self.updatable_devices) |d| {
            d.update_fn(d.ctx, self);
        }

        var read_data: hw.D = hw.D.init(0);
        if (self.t.read()) |addr| {
            var read_collision = false;
            var read_device: ?*Device = null;
            for (self.global_devices) |*d| {
                if (d.read_fn(d.ctx, addr, self.t.cs.bus_width)) |result| {
                    if (read_device) |previous| {
                        if (!read_collision) {
                            previous.log_contention(self.microcycles_simulated + i);
                            read_collision = true;
                        }
                        d.log_contention(self.microcycles_simulated + i);
                    }
                    read_data = result;
                    read_device = d;
                }
            }
            if (addr.device_slot()) |slot| {
                if (self.device_slots[slot]) |d| {
                    if (d.read_fn(d.ctx, addr, self.t.cs.bus_width)) |result| {
                        if (read_device) |previous| {
                            if (!read_collision) {
                                previous.log_contention(self.microcycles_simulated + i);
                                read_collision = true;
                            }
                            d.log_contention(self.microcycles_simulated + i);
                        }
                        read_data = result;
                    }
                }
            }
        }

        var t_out: Decode_Stage = self.d;
        self.td = self.t.simulate(&t_out, read_data, self.power, self.reset, self.registers, self.translations, &self.interrupts_pending);
        t_out.pipeline = hw.Pipeline.init(self.d.pipeline.raw() +% 1);

        if (self.td.write_addr) |addr| {
            const data = self.td.d;
            for (self.global_devices) |*d| {
                d.write_fn(d.ctx, addr, self.t.cs.bus_width, data);
            }
            if (addr.device_slot()) |slot| {
                if (self.device_slots[slot]) |d| {
                    d.write_fn(d.ctx, addr, self.t.cs.bus_width, data);
                }
            }
        }

        self.cd = self.c.simulate(&self.t, self.reset, self.translations);
        self.sd = self.s.simulate(&self.c, self.registers);
        self.dd = self.d.simulate(&self.s, self.decode_rom, self.microcode_rom, atomic_busy);
        self.d = t_out;
    }
    self.microcycles_simulated += n;
}

pub fn simulate_cycles(self: *Simulator, n: u64) void {
    self.simulate_microcycles(n * 4);
}

pub fn simulate_reset(self: *Simulator) void {
    self.reset = true;
    self.simulate_cycles(1);
    while (self.t.pipeline != .zero) {
        self.simulate_microcycles(1);
    }
    self.reset = false;
}

pub fn simulate_reset_and_init(self: *Simulator) void {
    self.simulate_reset();
    while (self.t.cs.seq_op != .next_instruction) {
        self.simulate_cycles(1);
    }
}

const log = std.log.scoped(.sim);
const Simulator = @This();
const Decode_Stage = @import("Decode_Stage.zig");
const Setup_Stage = @import("Setup_Stage.zig");
const Compute_Stage = @import("Compute_Stage.zig");
const Transact_Stage = @import("Transact_Stage.zig");
const Updatable_Device = Device.Updatable_Device;
const Device = @import("Device.zig");
const Control_Signals = hw.Control_Signals;
const at = hw.addr.translation;
const hw = arch.hw;
const arch = @import("lib_arch");
const std = @import("std");
