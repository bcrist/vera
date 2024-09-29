pub const Pipeline = enum {
    all,
    p0,
    p1,
    p2,
    p3,
};
pub fn Simulator(comptime pipeline: Pipeline) type {
    const num_pipelines = switch (pipeline) {
        .all => arch.Pipeline.count,
        .p0, .p1, .p2, .p3 => 1,
    };

    return struct {
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
            interrupts_pending: [num_pipelines]bool,
            pipelines: [num_pipelines]Pipeline_State,
            registers: arch.Register_File,
            guards: arch.Guarded_Memory_File,
            translations: at.Translation_File,
        };

        const Self = @This();

        pub fn init(allocator: std.mem.Allocator, insn_decode_rom: *const arch.insn_decode.Rom, microcode_rom: *const arch.microcode.Rom, devices: []const Device) !Self {
            const state = try allocator.create(State);
            errdefer allocator.destroy(state);
            state.* = .{
                .reset = true,
                .interrupts_pending = .{ false } ** num_pipelines,
                .pipelines = switch (pipeline) {
                    .p0 => .{ .{ .pipe = .zero, .next_stage = .decode } },
                    .p1 => .{ .{ .pipe = .one, .next_stage = .decode } },
                    .p2 => .{ .{ .pipe = .two, .next_stage = .decode } },
                    .p3 => .{ .{ .pipe = .three, .next_stage = .decode } },
                    .all => .{
                        .{ .pipe = .zero, .next_stage = .decode },
                        .{ .pipe = .one, .next_stage = .transact },
                        .{ .pipe = .two, .next_stage = .compute },
                        .{ .pipe = .three, .next_stage = .setup },
                    },
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
                .insn_decode_rom = insn_decode_rom,
                .microcode_rom = microcode_rom,
                .device_slots = device_slots,
                .slotted_devices = slotted_devices,
                .global_devices = global_devices,
                .updatable_devices = updatable_devices,
                .state = state,
                .microcycles_simulated = 0,
            };
        }

        pub fn deinit(self: *Self) void {
            self.allocator.free(self.updatable_devices);
            self.allocator.free(self.slotted_devices);
            self.allocator.free(self.global_devices);
            self.allocator.destroy(self.state);
        }

        pub fn simulate_cycles(self: *Self, n: u64) void {
            self.simulate_microcycles(n * arch.Pipeline.count);
        }

        pub fn simulate_reset(self: *Self) void {
            self.state.reset = true;
            self.simulate_cycles(1);
            while (self.state.pipelines[0].next_stage != .transact) {
                self.simulate_microcycles(1);
            }
            self.state.reset = false;
        }

        pub fn simulate_reset_and_init(self: *Self) void {
            self.simulate_reset();
            while (self.state.pipelines[0].cs.seqop != .next_instruction) {
                self.simulate_cycles(1);
            }
        }

        pub fn simulate_microcycles(self: *Self, n: u64) void {
            const state = self.state;

            var i: u64 = 0;
            while (i < n) : (i += 1) {
                for (self.updatable_devices) |d| {
                    d.update_fn(d.ctx);
                }

                var maybe_transact_stage: ?usize = null;
                for (0.., &state.pipelines) |pipe_index, *pipe| {
                    switch (pipe.next_stage) {
                        .decode => pipe.simulate_decode(self.insn_decode_rom, self.microcode_rom, state.interrupts_pending[if (num_pipelines == 1) 0 else pipe.pipe.raw()], state.reset),
                        .setup => pipe.simulate_setup(&state.registers),
                        .compute => pipe.simulate_compute(&state.translations),
                        .transact => maybe_transact_stage = pipe_index,
                    }
                }

                if (maybe_transact_stage) |transact_stage| {
                    var flags = state.pipelines[transact_stage].flags;
                    if (flags.contains(.read)) {
                        const ctrl = state.pipelines[transact_stage].get_bus_control();
                        var data: Bus_Data = .{ .da = arch.DA.init(0), .db = arch.DB.init(0) };
                        var contention: Read_Contention_State = .no_read;

                        for (self.global_devices) |*d| {
                            maybe_read_device(d, ctrl, &data, &contention, self.microcycles_simulated + i);
                        }

                        const pa: arch.addr.Physical = .{
                            .frame = ctrl.frame,
                            .offset = state.pipelines[transact_stage].va.offset,
                        };

                        if (pa.device_slot()) |slot| {
                            if (self.device_slots[slot]) |d| {
                                maybe_read_device(d, ctrl, &data, &contention, self.microcycles_simulated + i);
                            }
                        }
                        state.pipelines[transact_stage].da = data.da;
                        state.pipelines[transact_stage].db = data.db;
                    }

                    state.pipelines[transact_stage].simulate_transact(&state.registers, &state.translations, &state.guards);

                    if (flags.contains(.write)) {
                        const ctrl = state.pipelines[transact_stage].get_bus_control();

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
            }
            self.microcycles_simulated += n;
        }

        const Read_Contention_State = union (enum) {
            no_read,
            uncontended: *Device,
            contended,
        };

        fn maybe_read_device(device: *Device, ctrl: Bus_Control, data: *Bus_Data, contention: *Read_Contention_State, microcycles: usize) void {
            if (device.read_fn(device.ctx, ctrl)) |result| {
                switch (contention.*) {
                    .no_read => contention.* = .{ .uncontended = device },
                    .uncontended => |previous| {
                        previous.log_contention(microcycles);
                        device.log_contention(microcycles);
                        contention.* = .contended;
                    },
                    .contended => device.log_contention(microcycles),
                }
                data.* = result;
            }
        }
    };
}

const log = std.log.scoped(.sim);
const Pipeline_State = @import("Pipeline_State.zig");
const Bus_Data = @import("Bus_Data.zig");
const Bus_Control = @import("Bus_Control.zig");
const Updatable_Device = Device.Updatable_Device;
const Device = @import("Device.zig");
const at = arch.addr.translation;
const arch = @import("arch");
const std = @import("std");
