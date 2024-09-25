allocator: std.mem.Allocator,
insn_decode_rom: *const arch.insn_decode.Rom,
microcode_rom: *const arch.microcode.Rom,
device_slots: [8]?*Device,
slotted_devices: []Device,
global_devices: []Device,
updatable_devices: []Updatable_Device,

state: *State,

microcycles_simulated: u64,

pub const State = struct {
    reset: bool,
    interrupts_pending: [arch.Pipeline.count]bool,
    pipelines: [arch.Pipeline.count]Pipeline_State,
    registers: arch.Register_File,
    guards: arch.Guarded_Memory_File,
    translations: at.Translation_File,
};

pub fn init(allocator: std.mem.Allocator, insn_decode_rom: *const arch.insn_decode.Result, microcode_rom: *const arch.microcode.Rom, devices: []const Device) !Simulator {
    const state = try allocator.create(State);
    errdefer allocator.destroy(state);
    state.* = .{
        .reset = true,
        .interrupts_pending = .{ false } ** arch.Pipeline.count,
        .pipelines = .{
            .{ .pipe = .zero, .next_stage = .decode },
            .{ .pipe = .one, .next_stage = .transact },
            .{ .pipe = .two, .next_stage = .compute },
            .{ .pipe = .three, .next_stage = .setup },
        },
        .registers = .{ .{
            .reg = .{ arch.Reg.init(0) } ** arch.register_count,
            .sr1 = .{ arch.Reg.init(0) } ** arch.SR1_Index.count,
            .sr2 = .{ arch.Reg.init(0) } ** arch.SR2_Index.count,
        }} ** arch.Register_Set_Number.count,
        .guards = .{ arch.Guarded_Memory_Register.init(0) } ** arch.Pipeline.count,
        .translations = .{ .{ 
            .primary = at.Entry.init(0),
            .secondary = at.Entry.init(0),
        }} ** at.Entry.Address.count,
    };

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
        .decode_rom = insn_decode_rom,
        .microcode_rom = microcode_rom,
        .device_slots = device_slots,
        .slotted_devices = slotted_devices,
        .global_devices = global_devices,
        .updatable_devices = updatable_devices,
        .state = state,
        .microcycles_simulated = 0,
    };
}

pub fn deinit(self: *Simulator) void {
    self.allocator.free(self.updatable_devices);
    self.allocator.free(self.slotted_devices);
    self.allocator.free(self.global_devices);
    self.allocator.destroy(self.state);
}

pub fn simulate_microcycles(self: *Simulator, n: u64) void {
    const state = self.state;

    var i: u64 = 0;
    while (i < n) : (i += 1) {

        for (self.updatable_devices) |d| {
            d.update_fn(d.ctx);
        }

        var transact_stage: arch.Pipeline.Raw = undefined;
        for (&state.pipelines) |pipe| {
            switch (pipe.next_stage) {
                .decode => pipe.simulate_decode(self.insn_decode_rom, self.microcode_rom, &state.interrupts_pending, state.reset),
                .setup => pipe.simulate_setup(&state.registers),
                .compute => pipe.simulate_compute(&state.translations),
                .transact => transact_stage = pipe.pipe.raw(),
            }
        }

        var flags = state.pipelines[transact_stage].flags;
        if (flags.contains(.read)) {
            var read_collision = false;
            var read_device: ?*Device = null;

            const ctrl: Bus_Control = .{
                .frame = state.pipelines[transact_stage].frame,
                .aa = state.pipelines[transact_stage].aa,
                .ab = state.pipelines[transact_stage].ab,
                .lba = flags.contains(.lba),
                .uba = flags.contains(.uba),
                .lbb = flags.contains(.lbb),
                .ubb = flags.contains(.ubb),
                .guard_mismatch = flags.contains(.guard_mismatch),
                .block_transfer = flags.contains(.block_transfer),
                .update_frame_state = flags.contains(.update_frame_state),
            };

            for (self.global_devices) |*d| {
                if (d.read_fn(d.ctx, ctrl)) |result| {
                    if (read_device) |previous| {
                        if (!read_collision) {
                            previous.log_contention(self.microcycles_simulated + i);
                            read_collision = true;
                        }
                        d.log_contention(self.microcycles_simulated + i);
                    }
                    state.pipelines[transact_stage].da = result.da;
                    state.pipelines[transact_stage].db = result.db;
                    read_device = d;
                }
            }

            const pa: arch.addr.Physical = .{
                .frame = ctrl.frame,
                .offset = state.pipelines[transact_stage].va.offset,
            };

            if (pa.device_slot()) |slot| {
                if (self.device_slots[slot]) |d| {
                    if (d.read_fn(d.ctx, ctrl)) |result| {
                        if (read_device) |previous| {
                            if (!read_collision) {
                                previous.log_contention(self.microcycles_simulated + i);
                                read_collision = true;
                            }
                            d.log_contention(self.microcycles_simulated + i);
                        }
                        state.pipelines[transact_stage].da = result.da;
                        state.pipelines[transact_stage].db = result.db;
                    }
                }
            }
        }

        state.pipelines[transact_stage].simulate_transact(&state.registers, &state.translations, &state.guards);

        if (flags.contains(.write)) {
            const ctrl: Bus_Control = .{
                .frame = state.pipelines[transact_stage].frame,
                .aa = state.pipelines[transact_stage].aa,
                .ab = state.pipelines[transact_stage].ab,
                .lba = flags.contains(.lba),
                .uba = flags.contains(.uba),
                .lbb = flags.contains(.lbb),
                .ubb = flags.contains(.ubb),
                .guard_mismatch = flags.contains(.guard_mismatch),
                .block_transfer = flags.contains(.block_transfer),
                .update_frame_state = flags.contains(.update_frame_state),
            };

            const data: Bus_Data = .{
                .da = state.pipelines[transact_stage].da,
                .db = state.pipelines[transact_stage].db,
            };

            for (self.global_devices) |*d| {
                d.write_fn(d.ctx, ctrl, data);
            }

            const pa: arch.addr.Physical = .{
                .frame = ctrl.frame,
                .offset = state.pipelines[transact_stage].va.offset,
            };

            if (pa.device_slot()) |slot| {
                if (self.device_slots[slot]) |d| {
                    d.write_fn(d.ctx, ctrl, data);
                }
            }
        }
    }
    self.microcycles_simulated += n;
}

pub fn simulate_cycles(self: *Simulator, n: u64) void {
    self.simulate_microcycles(n * arch.Pipeline.count);
}

pub fn simulate_reset(self: *Simulator) void {
    self.state.reset = true;
    self.simulate_cycles(1);
    while (self.state.pipelines[0].next_stage != .transact) {
        self.simulate_microcycles(1);
    }
    self.state.reset = false;
}

pub fn simulate_reset_and_init(self: *Simulator) void {
    self.simulate_reset();
    while (self.state.pipelines[0].cs.seqop != .next_instruction) {
        self.simulate_cycles(1);
    }
}

const log = std.log.scoped(.sim);
const Simulator = @This();
const Pipeline_State = @import("Pipeline_State.zig");
const Bus_Data = @import("Bus_Data.zig");
const Bus_Control = @import("Bus_Control.zig");
const Updatable_Device = Device.Updatable_Device;
const Device = @import("Device.zig");
const at = arch.addr.translation;
const arch = @import("arch");
const std = @import("std");
