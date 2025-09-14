pub const Pipeline = enum {
    all,
    p0,
    p1,
    p2,
    p3,
};

pub const Dump_State_Options = struct {
    uca: bool = false,
    cs: bool = false,
    sr: bool = false,
    reg: usize = 0,

    pub const all: Dump_State_Options = .{
        .uca = true,
        .cs = true,
        .sr = true,
        .reg = 32,
    };
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

        pub fn init(allocator: std.mem.Allocator, insn_decode_rom: *const arch.insn_decode.Rom, microcode_rom: *const arch.microcode.Rom, devices: []const Device, debug_log: ?*Debug_Log) !Self {
            const state = try allocator.create(State);
            errdefer allocator.destroy(state);
            state.* = .{
                .reset = true,
                .interrupts_pending = .{ false } ** num_pipelines,
                .pipelines = switch (pipeline) {
                    .p0 => .{ .{ .pipe = .zero, .next_stage = .decode, .debug_log = debug_log } },
                    .p1 => .{ .{ .pipe = .one, .next_stage = .decode, .debug_log = debug_log } },
                    .p2 => .{ .{ .pipe = .two, .next_stage = .decode, .debug_log = debug_log } },
                    .p3 => .{ .{ .pipe = .three, .next_stage = .decode, .debug_log = debug_log } },
                    .all => .{
                        .{ .pipe = .zero, .next_stage = .decode, .debug_log = debug_log },
                        .{ .pipe = .one, .next_stage = .transact, .debug_log = debug_log },
                        .{ .pipe = .two, .next_stage = .compute, .debug_log = debug_log },
                        .{ .pipe = .three, .next_stage = .setup, .debug_log = debug_log },
                    },
                },
                .registers = .{ .{
                    .reg = .{ arch.Reg.init(0) } ** arch.register_count,
                    .sr1 = .{ arch.Reg.init(0) } ** arch.reg.sr1.Index.count,
                    .sr2 = .{ arch.Reg.init(0) } ** arch.reg.sr2.Index.count,
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
            var i: u64 = self.microcycles_simulated;
            const final_microcycle = i +% n;
            while (i != final_microcycle) : (i +%= 1) {
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
                    if (state.pipelines[transact_stage].debug_log) |dl| {
                        dl.current_microcycle = i;
                    }

                    var flags = state.pipelines[transact_stage].flags;
                    if (flags.contains(.read)) {
                        const ctrl = state.pipelines[transact_stage].get_bus_control();
                        var data: Bus_Data = .{ .da = arch.DA.init(0), .db = arch.DB.init(0) };
                        var contention: Read_Contention_State = .no_read;

                        for (self.global_devices) |*d| {
                            maybe_read_device(d, ctrl, &data, &contention, i);
                        }

                        const pa: arch.addr.Physical = .{
                            .frame = ctrl.frame,
                            .offset = state.pipelines[transact_stage].va.offset,
                        };

                        if (pa.device_slot()) |slot| {
                            if (self.device_slots[slot]) |d| {
                                maybe_read_device(d, ctrl, &data, &contention, i);
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
            self.microcycles_simulated = final_microcycle;
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

        pub fn dump_state(self: *Self, writer: anytype, options: Dump_State_Options) !void {
            try writer.print("Microcycle: {}\n", .{ self.microcycles_simulated });
            try writer.print("Reset: {}\n", .{ self.state.reset });

            try writer.writeAll("Interrupts:");
            for (self.state.interrupts_pending) |v| {
                try writer.print(" {}", .{ v });
            }
            try writer.writeAll("\n");

            try writer.writeAll("Guards:");
            for (self.state.guards) |g| {
                try writer.print(" {}", .{ g });
            }
            try writer.writeAll("\n");

            for (self.state.pipelines) |pipe| {
                try writer.print("Pipeline {}:\n", .{ pipe.pipe });
                try writer.print("   Next Stage:    {}\n", .{ pipe.next_stage });
                try writer.print("   Exec Mode:     {}\n", .{ pipe.emode });
                if (options.uca) {
                    try writer.print("   UCA PUCS:      {} {}\n", .{ pipe.uca, pipe.pucs });
                }
                try writer.print("   DR IR:         {} {}\n", .{ pipe.dr, pipe.ir });
                try writer.print("   RSN:           {}\n", .{ pipe.rsn });
                try writer.print("   SR Data:       {} {}\n", .{ pipe.sr1d, pipe.sr2d });
                try writer.print("   TI WI JRI KRI: {} {} {} {} (KRIO={})\n", .{ pipe.ti, pipe.wi, pipe.jri, pipe.kri, pipe.krio });
                try writer.print("   J K:           {} {}\n", .{ pipe.j, pipe.k });
                try writer.print("   ASN4 VA:       {} {}\n", .{ pipe.asn4, pipe.va });
                try writer.print("   Last AT Info:  {}\n", .{ pipe.last_at_info });
                try writer.print("   AT Entry Addr: {}\n", .{ pipe.at_entry_addr });
                try writer.print("   AT Entries:    {} {}\n", .{ pipe.matching_entry, pipe.other_entry });
                try writer.print("   FRAME AA AB:   {} {} {}\n", .{ pipe.frame, pipe.aa, pipe.ab });
                try writer.print("   Compute Res:   {} c={} v={} count={}\n", .{ pipe.compute_result.value, pipe.compute_result.cout, pipe.compute_result.vout, pipe.count_result });
                try writer.print("   DA DB:         {} {}\n", .{ pipe.da, pipe.db });

                if (pipe.next_stage == .transact) {
                    try writer.print("   L:             {}\n", .{ pipe.get_l() });
                }

                try writer.writeAll("   Flags:        ");
                var iter = pipe.flags.iterator();
                while (iter.next()) |flag| {
                    try writer.print(" {s}", .{ @tagName(flag) });
                }
                try writer.writeAll("\n");

                if (options.cs) {
                    try writer.writeAll("   Control Signals:\n");

                    const want_compute = switch (pipe.cs.statop) {
                        .compute, .compute_no_set_z => true,
                        .hold, .zn_from_l, .clear_a, .set_a, .load_zncv, .load_zncva_ti => false,
                    } or switch (pipe.cs.lsrc) {
                        .alu, .shift, .mult, .count, .ext => true,
                        .at_info, .status, .d => false,
                    };

                    if (want_compute) {
                        switch (pipe.cs.jsrc) {
                            .zero => try writer.print("      JSRC={}", .{ pipe.cs.jsrc }),
                            .jr => try writer.print("      JSRC=JR[{}]", .{ pipe.jri }),
                            .sr1 => try writer.print("      JSRC=SR1[{}]", .{ pipe.cs.sr1ri }),
                            .sr2 => try writer.print("      JSRC=SR2[{}]", .{ pipe.cs.sr2ri }),
                        }
                        switch (pipe.cs.ksrc) {
                            .zero, .krio, .krio_bit, .krio_bit_inv, .dr_byte_1_sx, .dr_byte_2_sx, .dr_byte_21_sx,
                            => try writer.print("  KSRC={}\n", .{ pipe.cs.ksrc }),

                            .vao => try writer.print("  KSRC={} ({})\n", .{ pipe.cs.ksrc, pipe.cs.vao }),
                            .kr => try writer.print("  KSRC=KR[{}]\n", .{ pipe.kri }),
                            .sr1 => try writer.print("  KSRC=SR1[{}]\n", .{ pipe.cs.sr1ri }),
                            .sr2 => try writer.print("  KSRC=SR2[{}]\n", .{ pipe.cs.sr2ri }),
                        }

                        try writer.print("      UNIT={}  MODE={}\n", .{ pipe.cs.unit, pipe.cs.mode });
                    }

                    try writer.print("      LSRC={}\n", .{ pipe.cs.lsrc });

                    if (pipe.cs.dir != .none or pipe.cs.atop != .none) {
                        try writer.print("      VARI={}  VAO={}\n", .{ pipe.cs.vari, pipe.cs.vao });
                        try writer.print("      DIR={}  WIDTH={}  VASPACE={}  ATOP={}\n", .{ pipe.cs.dir, pipe.cs.width, pipe.cs.vaspace, pipe.cs.atop });
                    }

                    if (pipe.cs.sr1wsrc != .no_write) {
                        if (pipe.cs.sr1wsrc == .self) {
                            try writer.print("      write SR1[{}] -> SR1[{}]\n", .{ pipe.cs.sr1ri, pipe.cs.sr1wi });
                        } else {
                            try writer.print("      write {} -> SR1[{}]\n", .{ pipe.cs.sr1wsrc, pipe.cs.sr1wi });
                        }
                    }
                    if (pipe.cs.sr2wsrc != .no_write) {
                        if (pipe.cs.sr2wsrc == .self) {
                            try writer.print("      write SR2[{}] -> SR2[{}]\n", .{ pipe.cs.sr2ri, pipe.cs.sr2wi });
                        } else {
                            try writer.print("      write {} -> SR2[{}]\n", .{ pipe.cs.sr2wsrc, pipe.cs.sr2wi });
                        }
                    }

                    if (pipe.cs.gprw) {
                        try writer.print("      write .l -> reg[{}]", .{ pipe.wi });
                        if (pipe.cs.tiw) {
                            try writer.writeAll("  TIW=true");
                        }
                        try writer.writeAll("\n");
                    } else if (pipe.cs.tiw) {
                        try writer.writeAll("      TIW=true\n");
                    }

                    if (pipe.cs.drw) {
                        try writer.print("      write .d -> DR\n", .{});
                    }
                    if (pipe.cs.irw) {
                        try writer.print("      write DR -> IR\n", .{});
                    }

                    if (pipe.cs.statop != .hold) {
                        try writer.print("      STATOP={}\n", .{ pipe.cs.statop });
                    }

                    if (pipe.cs.special != .none) {
                        try writer.print("      SPECIAL={}\n", .{ pipe.cs.special });
                    }

                    if (options.uca) {
                        try writer.print("      SEQOP={}  NEXT={}  ALLOWINT={}  POWER={}\n", .{ pipe.cs.seqop, pipe.cs.next, pipe.cs.allowint, pipe.cs.power });
                    } else {
                        try writer.print("      SEQOP={}  ALLOWINT={}  POWER={}\n", .{ pipe.cs.seqop, pipe.cs.allowint, pipe.cs.power });
                    }
                }

                if (options.sr) {
                    try writer.writeAll("   Special Registers:\n");

                    for (0..@max(arch.reg.sr1.Index.count, arch.reg.sr2.Index.count)) |i| {
                        var buf: [16]u8 = undefined;

                        if (i < arch.reg.sr1.Index.count) {
                            const index_name = try std.fmt.bufPrint(&buf, "{}", .{ arch.reg.sr1.Index.init(@intCast(i)) });
                            try writer.writeByteNTimes(' ', 20 - index_name.len);
                            try writer.writeAll(index_name);
                            try writer.print("={}", .{ self.state.registers[pipe.rsn.raw()].sr1[i] });
                        } else {
                            try writer.writeByteNTimes(' ', 20);
                        }

                        if (i < arch.reg.sr2.Index.count) {
                            const index_name = try std.fmt.bufPrint(&buf, "{}", .{ arch.reg.sr2.Index.init(@intCast(i)) });
                            try writer.writeByteNTimes(' ', 16 - index_name.len);
                            try writer.writeAll(index_name);
                            try writer.print("={}", .{ self.state.registers[pipe.rsn.raw()].sr2[i] });
                        }
                        try writer.writeAll("\n");
                    }
                }

                if (options.reg > 0) {
                    try writer.writeAll("   Registers:\n");

                    for (0..options.reg) |i| {
                        const ri: arch.Register_Index = @truncate(pipe.ti.raw() -% i);
                        try writer.print("      r{:<2} ({X:0>2}): {}", .{ i, ri, self.state.registers[pipe.rsn.raw()].reg[ri] });
                        if (pipe.wi.raw() == ri and pipe.cs.gprw) {
                            try writer.writeAll(" WI");
                        }
                        try writer.writeAll("\n");
                    }
                }
            }
        }
    };
}

const log = std.log.scoped(.sim);

const Debug_Log = @import("Debug_Log.zig");
const Pipeline_State = @import("Pipeline_State.zig");
const Bus_Data = @import("Bus_Data.zig");
const Bus_Control = @import("Bus_Control.zig");
const Updatable_Device = Device.Updatable_Device;
const Device = @import("Device.zig");
const at = arch.addr.translation;
const arch = @import("arch");
const std = @import("std");
