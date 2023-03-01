const std = @import("std");
const ControlSignals = @import("ControlSignals");
const uc = @import("microcode");
const bits = @import("bits");
const misc = @import("misc");
const bus = @import("bus");
const register_file = @import("register_file");
const address_gen = @import("address_gen.zig");
const arith = @import("arith.zig");
const shift = @import("shift.zig");
const mult = @import("mult.zig");
const logic = @import("logic.zig");
const bitcount = @import("bitcount.zig");
const mmu = @import("mmu.zig");
const misc_registers = @import("misc_registers.zig");
const stat = @import("stat.zig");
const faults = @import("faults.zig");
const decoder = @import("decoder.zig");
const Memory = @import("memory.zig").Memory;
const device_sys = @import("device_sys.zig");
const assert = std.debug.assert;

const SystemBusControl = @import("SystemBusControl.zig");

const Simulator = @This();

allocator: std.mem.Allocator,
microcode: []const ControlSignals,
exec_state: ExecutionState,
reg_file: *register_file.State,
mmu_state: *mmu.State,
memory: *Memory, 
device_sys_state: *device_sys.State,
s: SetupInputs = SetupInputs.init(),
c: ComputeInputs = ComputeInputs.init(),
t: TransactInputs = TransactInputs.init(),
microcycle_count: u64,

pub fn init(allocator: std.mem.Allocator, microcode: []const ControlSignals) !Simulator {
    const memory = try allocator.create(Memory);
    errdefer allocator.destroy(memory);
    const reg_file_state = try allocator.create(register_file.State);
    errdefer allocator.destroy(reg_file_state);
    const mmu_state = try allocator.create(mmu.State);
    errdefer allocator.destroy(mmu_state);
    const device_sys_state = try allocator.create(device_sys.State);
    errdefer allocator.destroy(device_sys_state);

    memory.reset();
    reg_file_state.reset();
    mmu_state.reset();
    device_sys_state.reset();

    return Simulator{
        .allocator = allocator,
        .microcode = microcode,
        .exec_state = ExecutionState.init(),
        .reg_file = reg_file_state,
        .mmu_state = mmu_state,
        .memory = memory,
        .device_sys_state = device_sys_state,
        .microcycle_count = 0,
    };
}

pub fn deinit(self: *Simulator) void {
    self.allocator.destroy(self.device_sys_state);
    self.allocator.destroy(self.mmu_state);
    self.allocator.destroy(self.reg_file);
    self.allocator.destroy(self.memory);
}

pub fn randomizeState(self: *Simulator, rnd: std.rand.Random) void {
    self.memory.randomize(rnd);
    self.reg_file.randomize(rnd);
    self.mmu_state.randomize(rnd);
    self.device_sys_state.randomize(rnd);
    self.s.randomize(rnd);
    self.c.randomize(rnd);
    self.t.randomize(rnd);
}

pub fn microcycle(self: *Simulator, n: u64) void {
    var i: u64 = 0;
    while (i < n) : (i += 1) {
        self.exec_state.pipe = self.exec_state.pipe.next();

        const atomic_comp = self.c.want_atomic and !self.c.stall_atomic;
        const atomic_trans = self.t.want_atomic and !self.t.stall_atomic;

        var next_c = simulate_setup(self.reg_file, self.s, atomic_comp, atomic_trans);
        var next_t = simulate_compute(self.mmu_state, self.exec_state, self.c);
        var next_s = simulate_transact(self.microcode, self.memory, self.reg_file, self.mmu_state, self.device_sys_state, &self.exec_state, self.t);

        self.s = next_s;
        self.c = next_c;
        self.t = next_t;

        self.microcycle_count += 1;
    }
}

pub fn cycle(self: *Simulator, n: u64) void {
    self.microcycle(n * 3);
}

pub fn debugCycle(self: *Simulator, n: u64, pipe: ControlSignals.Pipe_ID) !void {
    var writer = std.io.getStdErr().writer();
    try self.printState(writer, pipe);

    var i: usize = 0;
    while (i < n) : (i += 1) {
        self.cycle(1);
        try self.printState(writer, pipe);
    }
}

pub fn reset(self: *Simulator) void {
    self.exec_state.reset = true;
    self.microcycle(switch (self.exec_state.pipe) {
        .zero => 4,
        .one => 3,
        .two => 5,
    });
    self.exec_state.reset = false;
}

pub fn resetAndStart(self: *Simulator) void {
    self.reset();
    while (self.s.pipe != .zero) {
        self.microcycle(1);
    }
    while (self.s.cs.seq_op != .next_instruction) {
        self.cycle(1);
    }
    self.cycle(1);
}

pub fn printState(self: *Simulator, writer: anytype, pipe: misc.PipeID) !void {
    try writer.print("c{}", .{ self.microcycle_count });
    if (self.exec_state.reset) try writer.writeAll(" RESET");
    if (self.exec_state.sleep) try writer.writeAll(" SLEEP");
    for (self.exec_state.interrupt_pending, 0..) |int_pending, pipe_index| {
        if (int_pending) {
            try writer.print(" INT{}", .{ pipe_index + 1 });
        }
    }

    try writer.writeAll("\n");

    switch (self.exec_state.pipe) {
        .zero => switch (pipe) {
            .zero => try self.printT(writer),
            .one => try self.printC(writer),
            .two => try self.printS(writer),
        },
        .one => switch (pipe) {
            .zero => try self.printS(writer),
            .one => try self.printT(writer),
            .two => try self.printC(writer),
        },
        .two => switch (pipe) {
            .zero => try self.printC(writer),
            .one => try self.printS(writer),
            .two => try self.printT(writer),
        },
    }
}

fn printS(self: *Simulator, writer: anytype) !void {
    try writer.print("Pipe:{s}  RSN:{}  T -> Setup\n", .{ @tagName(self.s.pipe), self.s.reg.rsn });
    try self.s.cs.print(writer);
    try self.printRegs(self.s.reg, writer);
}
fn printC(self: *Simulator, writer: anytype) !void {
    try writer.print("Pipe:{s}  RSN:{}  S -> Compute\n", .{ @tagName(self.c.pipe), self.c.reg.rsn });
    try self.c.cs.print(writer);
    try self.printRegs(self.c.reg, writer);
}
fn printT(self: *Simulator, writer: anytype) !void {
    try writer.print("Pipe:{s}  RSN:{}  C -> Transact\n", .{ @tagName(self.t.pipe), self.t.reg.rsn });
    try self.t.cs.print(writer);
    try self.printRegs(self.t.reg, writer);
}
fn printRegs(self: *Simulator, reg: LoopRegisters, writer: anytype) !void {
    try writer.print("      UA: {X:0>4}  DL: {X:0>4}   OA: {X:0>1}  OB: {X:0>1}  ", .{
        reg.ua, reg.dl, reg.oa, reg.ob,
    });
    try reg.stat.print(writer);
    try writer.writeAll("\n");
    try register_file.RegisterView.init(self.reg_file, reg.rsn).printRegs(writer);
}

const ExecutionState = struct {
    reset: bool,
    sleep: bool,
    interrupt_pending: [3]bool,
    pipe: misc.PipeID, // The pipe ID currently in the transact stage
    power: misc.PowerMode,

    pub fn init() ExecutionState {
        return .{
            .reset = false,
            .sleep = false,
            .interrupt_pending = .{ false } ** 3,
            .pipe = .zero,
            .power = .run,
        };
    }
};

const LoopRegisters = struct {
    exec_mode: misc.ExecutionMode,
    rsn: misc.RegistersetNumber,
    ua: uc.Address,
    dl: bus.D,
    oa: misc.OperandA,
    ob: misc.OperandB,
    stat: stat.LoopState,
    asn: misc.address_translator.AddressSpaceNumber,
    last_mmu_op: mmu.OperationInfo,

    pub fn init() LoopRegisters {
        return .{
            .exec_mode = .fault,
            .rsn = 0,
            .ua = 0,
            .dl = 0,
            .oa = 0,
            .ob = 0,
            .stat = .{
                .c = false,
                .v = false,
                .n = false,
                .z = false,
                .next_k = false,
                .k = false,
                .a = false,
            },
            .asn = 0,
            .last_mmu_op = mmu.OperationInfo.init(),
        };
    }

    pub fn randomize(self: *LoopRegisters, rnd: std.rand.Random) void {
        self.exec_mode = rnd.enumValue(misc.ExecutionMode);
        self.rsn = rnd.int(misc.RegistersetNumber);
        self.ua = rnd.int(uc.Address);
        self.dl = rnd.int(bus.D);
        self.oa = rnd.int(misc.OperandA);
        self.ob = rnd.int(misc.OperandB);
        self.stat = .{
            .c = rnd.boolean(),
            .v = rnd.boolean(),
            .n = rnd.boolean(),
            .z = rnd.boolean(),
            .next_k = true, // This must be guaranteed by reset logic
            .k = true, // This must be guaranteed by reset logic
            .a = rnd.boolean(),
        };
        self.asn = rnd.int(misc.address_translator.AddressSpaceNumber);
        self.last_mmu_op = mmu.OperationInfo.random(rnd);
    }
};

const SetupInputs = struct {
    pipe: misc.PipeID,
    cs: ControlSignals,
    reg: LoopRegisters,
    want_atomic: bool,

    pub fn init() SetupInputs {
        return .{
            .pipe = .zero,
            .cs = ControlSignals.init(),
            .reg = LoopRegisters.init(),
            .want_atomic = false,
        };
    }

    pub fn randomize(self: *SetupInputs, rnd: std.rand.Random) void {
        self.pipe = rnd.enumValue(misc.PipeID);
        self.cs.randomize(rnd);
        self.reg.randomize(rnd);
        self.want_atomic = rnd.boolean();
    }
};

const ComputeInputs = struct {
    pipe: misc.PipeID,
    cs: ControlSignals,
    reg: LoopRegisters,
    want_atomic: bool,
    stall_atomic: bool,
    sr1: bus.JParts,
    sr2: bus.JParts,
    j: bus.JParts,
    k: bus.K,
    virtual_address: bus.VirtualAddressParts,

    pub fn init() ComputeInputs {
        return .{
            .pipe = .zero,
            .cs = ControlSignals.init(),
            .reg = LoopRegisters.init(),
            .want_atomic = false,
            .stall_atomic = false,
            .sr1 = bus.JParts.zero,
            .sr2 = bus.JParts.zero,
            .j = bus.JParts.zero,
            .k = 0,
            .virtual_address = bus.VirtualAddressParts.zero,
        };
    }

    pub fn randomize(self: *ComputeInputs, rnd: std.rand.Random) void {
        self.pipe = rnd.enumValue(misc.PipeID);
        self.cs.randomize(rnd);
        self.reg.randomize(rnd);
        self.want_atomic = rnd.boolean();
        self.stall_atomic = rnd.boolean();
        self.sr1 = .{
            .low = rnd.int(bus.JLow),
            .high = rnd.int(bus.JHigh),
        };
        self.sr2 = .{
            .low = rnd.int(bus.JLow),
            .high = rnd.int(bus.JHigh),
        };
        self.j = .{
            .low = rnd.int(bus.JLow),
            .high = rnd.int(bus.JHigh),
        };
        self.k = rnd.int(bus.K);
        self.virtual_address = .{
            .offset = rnd.int(bus.PageOffset),
            .page = rnd.int(bus.Page),
        };
    }
};

const TransactInputs = struct {
    pipe: misc.PipeID,
    cs: ControlSignals,
    reg: LoopRegisters,
    want_atomic: bool,
    stall_atomic: bool,
    sr1: bus.JParts,
    sr2: bus.JParts,
    arith: arith.Outputs,
    shift: shift.Outputs,
    mult: mult.Outputs,
    logic: logic.Outputs,
    bitcount: bitcount.Outputs,
    virtual_address: bus.VirtualAddressParts,
    mmu_op: mmu.OperationInfo,
    mmu_slot_address: misc.address_translator.Slot,
    mmu_matching_entry: mmu.Entry,
    mmu_other_entry: mmu.Entry,
    mmu_k: bool,
    bus_ctrl: SystemBusControl,
    fault: faults.ComputeOutputs,
    inhibit_writes: bool,

    pub fn init() TransactInputs {
        return .{
            .pipe = .zero,
            .cs = ControlSignals.init(),
            .reg = LoopRegisters.init(),
            .want_atomic = false,
            .stall_atomic = false,
            .sr1 = bus.JParts.zero,
            .sr2 = bus.JParts.zero,
            .arith = .{
                .data = bus.LParts.zero,
                .z = false,
                .n = false,
                .c = false,
                .v = false,
            },
            .shift = .{
                .data = bus.LParts.zero,
                .c = false,
            },
            .mult = .{
                .data = bus.LParts.zero,
            },
            .logic = .{
                .data = bus.LParts.zero,
            },
            .bitcount = .{
                .data = 0,
            },
            .virtual_address = bus.VirtualAddressParts.zero,
            .mmu_op = mmu.OperationInfo.init(),
            .mmu_slot_address = 0,
            .mmu_matching_entry = mmu.Entry{},
            .mmu_other_entry = mmu.Entry{},
            .mmu_k = false,
            .bus_ctrl = SystemBusControl.init(),
            .fault = .{
                .any = false,
                .page = false,
                .page_align = false,
                .access = false,
                .special = false,
            },
            .inhibit_writes = false,
        };
    }

    pub fn randomize(self: *TransactInputs, rnd: std.rand.Random) void {
        self.pipe = rnd.enumValue(misc.PipeID);
        self.cs.randomize(rnd);
        self.reg.randomize(rnd);
        self.want_atomic = rnd.boolean();
        self.stall_atomic = rnd.boolean();
        self.sr1 = .{
            .low = rnd.int(bus.JLow),
            .high = rnd.int(bus.JHigh),
        };
        self.sr2 = .{
            .low = rnd.int(bus.JLow),
            .high = rnd.int(bus.JHigh),
        };
        self.arith = .{
            .data = .{
                .low = rnd.int(bus.LLow),
                .high = rnd.int(bus.LHigh),
            },
            .z = rnd.boolean(),
            .n = rnd.boolean(),
            .c = rnd.boolean(),
            .v = rnd.boolean(),
        };
        self.shift = .{
            .data = .{
                .low = rnd.int(bus.LLow),
                .high = rnd.int(bus.LHigh),
            },
            .c = rnd.boolean(),
        };
        self.mult.data = .{
            .low = rnd.int(bus.LLow),
            .high = rnd.int(bus.LHigh),
        };
        self.logic.data = .{
            .low = rnd.int(bus.LLow),
            .high = rnd.int(bus.LHigh),
        };
        self.bitcount = .{
            .data = rnd.int(u16),
        };
        self.virtual_address = .{
            .offset = rnd.int(bus.PageOffset),
            .page = rnd.int(bus.Page),
        };
        self.mmu_op = mmu.OperationInfo.random(rnd);
        self.mmu_slot_address = rnd.int(misc.address_translator.Slot);
        self.mmu_matching_entry = mmu.Entry.random(rnd);
        self.mmu_other_entry = mmu.Entry.random(rnd);
        self.bus_ctrl = SystemBusControl.random(rnd);
        self.fault = .{
            .any = false,
            .page = rnd.boolean(),
            .page_align = rnd.boolean(),
            .access = rnd.boolean(),
            .special = rnd.boolean(),
        };
        self.fault.any = self.fault.page or self.fault.page_align or self.fault.access or self.fault.special;
        self.inhibit_writes = self.fault.any;
    }
};

fn simulate_setup(reg_file: *const register_file.State, in: SetupInputs, atomic_comp: bool, atomic_trans: bool) ComputeInputs {
    const rf = register_file.setup(reg_file, .{
        .rsn = in.reg.rsn,
        .oa = in.reg.oa,
        .ob = in.reg.ob,
        .cs_jr_rsel = in.cs.jr_rsel,
        .cs_kr_rsel = in.cs.kr_rsel,
        .cs_jr_rx = in.cs.jr_rx,
        .cs_kr_rx = in.cs.kr_rx,
        .cs_sr1_ri = in.cs.sr1_ri,
        .cs_sr2_ri = in.cs.sr2_ri,
        .cs_base = in.cs.base,
        .cs_literal = in.cs.literal,
        .cs_jl_src = in.cs.jl_src,
        .cs_jh_src = in.cs.jh_src,
        .cs_k_src = in.cs.k_src,
    });

    const want_atomic = in.want_atomic or in.cs.special == .atomic_this or in.cs.special == .atomic_next;

    return .{
        .pipe = in.pipe,
        .cs = in.cs,
        .reg = in.reg,
        .want_atomic = want_atomic,
        .stall_atomic = want_atomic and (atomic_comp or atomic_trans),
        .j = rf.j,
        .k = rf.k,
        .sr1 = rf.sr1,
        .sr2 = rf.sr2,
        .virtual_address = address_gen.setup(.{
            .base = rf.address_base,
            .cs_offset = in.cs.offset,
            .cs_literal = in.cs.literal,
        }),
    };
}

fn simulate_compute(mmu_state: *const mmu.State, es: ExecutionState, in: ComputeInputs) TransactInputs {
    const arith_out = arith.compute(.{
        .j = in.j,
        .k = in.k,
        .c = in.reg.stat.c,
        .cs_compute_mode = in.cs.compute_mode,
    });
    const shift_out = shift.compute(.{
        .j = in.j,
        .k = in.k,
        .cs_compute_mode = in.cs.compute_mode,
    });
    const mult_out = mult.compute(.{
        .j = in.j.low,
        .k = in.k,
        .cs_compute_mode = in.cs.compute_mode,
    });
    const logic_out = logic.compute(.{
        .j = in.j,
        .k = in.k,
        .cs_compute_mode = in.cs.compute_mode,
    });
    const bitcount_out = bitcount.compute(.{
        .j = in.j.low,
        .cs_compute_mode = in.cs.compute_mode,
    });

    var mmu_out = mmu.compute(mmu_state, .{
        .virtual_address = in.virtual_address,
        .asn = in.reg.asn,
        .enable_flag = in.reg.stat.a,
        .kernel_flag = in.reg.stat.k,
        .cs_bus_mode = in.cs.bus_mode,
        .cs_bus_rw = in.cs.bus_rw,
        .cs_bus_byte = in.cs.bus_byte,
        .cs_at_op = in.cs.at_op,
        .cs_sr2_wi = in.cs.sr2_wi,
        .cs_sr2_wsrc = in.cs.sr2_wsrc,
    });

    const fault = faults.compute(.{
        .page_fault = mmu_out.page_fault,
        .page_align_fault = mmu_out.page_align_fault,
        .access_fault = mmu_out.access_fault,
        .cs_special = in.cs.special,
    });

    const inhibit_writes = in.stall_atomic or fault.any;

    if (inhibit_writes) {
        mmu_out.bus_ctrl.read = false;
        mmu_out.bus_ctrl.write = false;
        mmu_out.bus_ctrl.write_even = false;
        mmu_out.bus_ctrl.write_odd = false;
        mmu_out.bus_ctrl.wait_states = 0;
    }

    return .{
        .pipe = es.pipe,
        .cs = in.cs,
        .reg = in.reg,
        .want_atomic = in.want_atomic,
        .stall_atomic = in.stall_atomic,
        .sr1 = in.sr1,
        .sr2 = in.sr2,
        .arith = arith_out,
        .shift = shift_out,
        .mult = mult_out,
        .logic = logic_out,
        .bitcount = bitcount_out,
        .virtual_address = in.virtual_address,
        .mmu_op = mmu_out.op,
        .mmu_slot_address = mmu_out.slot,
        .mmu_matching_entry = mmu_out.matching_entry,
        .mmu_other_entry = mmu_out.other_entry,
        .mmu_k = mmu_out.new_kernel_flag,
        .bus_ctrl = mmu_out.bus_ctrl,
        .fault = fault,
        .inhibit_writes = inhibit_writes,
    };
}

fn simulate_transact(microcode: []const ControlSignals, memory: *Memory, reg_file: *register_file.State, mmu_state: *mmu.State, sys_state: *device_sys.State, es: *ExecutionState, in: TransactInputs) SetupInputs {
    const mem_d = memory.read(in.bus_ctrl);
    const sys_d = device_sys.read(sys_state, in.bus_ctrl);
    const dl_d = misc_registers.readDL(in.reg.dl, in.cs.dl_op, in.bus_ctrl);
    // :NewDevice: handle read here

    var d: bus.D = 0;
    var num_d_drivers: usize = 0;

    if (mem_d) |dd| {
        d = dd;
        num_d_drivers += 1;
    }
    if (sys_d) |dd| {
        d = dd;
        num_d_drivers += 1;
    }
    if (dl_d) |dd| {
        d = dd;
        num_d_drivers += 1;
    }

    assert(num_d_drivers <= 1);

    // Note this happens even if in.inhibit_writes is true
    const new_last_mmu_op = if (in.cs.at_op == .none) in.reg.last_mmu_op else in.mmu_op;

    var l: bus.LParts = undefined;
    l.low = switch (in.cs.ll_src) {
        .zero => 0,
        .logic_l => in.logic.data.low,
        .shift_l => in.shift.data.low,
        .arith_l => in.arith.data.low,
        .mult_l => in.mult.data.low,
        .bitcount => in.bitcount.data,
        .d16 => d,
        .d8_sx => bits.sx(bus.LLow, @truncate(u8, d)),
        .last_mmu_op_l => @truncate(u16, new_last_mmu_op.toU32()),
        .stat => @bitCast(u16, misc.StatusBits{
            .z = in.reg.stat.z,
            .n = in.reg.stat.n,
            .c = in.reg.stat.c,
            .v = in.reg.stat.v,
            .k = in.reg.stat.k,
            .a = in.reg.stat.a,
            .pipe = in.pipe,
            .mode = in.reg.exec_mode,
            .pwr = es.power,
        }),
        .pipe => @enumToInt(in.pipe),
    };
    l.high = switch (in.cs.lh_src) {
        .zero => 0,
        .logic_l => in.logic.data.low,
        .logic_h => in.logic.data.high,
        .shift_h => in.shift.data.high,
        .arith_h => in.arith.data.high,
        .mult_h => in.mult.data.high,
        .d16 => d,
        .sx_ll => bits.sx(bus.LHigh, @truncate(u1, l.low >> 15)),
        .last_mmu_op_h => @intCast(u16, new_last_mmu_op.toU32() >> 16),
        .prev_ua => in.reg.ua,
    };

    if (in.bus_ctrl.write and in.cs.dl_op != .to_d) {
        assert(in.cs.ll_src != .d16);
        assert(in.cs.ll_src != .d8_sx);
        d = l.low;
        num_d_drivers += 1;
    }

    assert(num_d_drivers <= 1);

    memory.write(in.bus_ctrl, d, in.cs.special == .block_transfer);
    device_sys.write(sys_state, in.bus_ctrl, d, in.mmu_matching_entry.update_frame_state);
    // :NewDevice: handle write here

    var next_asn = in.reg.asn;
    if (!in.inhibit_writes and in.cs.sr2_wi == .asn) {
        next_asn = @truncate(misc.address_translator.AddressSpaceNumber, l.low);
    }

    mmu.transact(mmu_state, .{
        .inhibit_writes = in.inhibit_writes,
        .matching_entry = in.mmu_matching_entry,
        .other_entry = in.mmu_other_entry,
        .slot = in.mmu_slot_address,
        .l = l,
        .tag = in.mmu_op.tag,
        .cs_at_op = in.mmu_op.cs_at_op,
    });

    const stat_out = stat.transact(.{
        .state = in.reg.stat,
        .inhibit_writes = in.inhibit_writes,
        .l = l,
        .shift_c = in.shift.c,
        .arith_z = in.arith.z,
        .arith_n = in.arith.n,
        .arith_c = in.arith.c,
        .arith_v = in.arith.v,
        .mmu_k = in.mmu_k,
        .cs_stat_op = in.cs.stat_op,
        .cs_seq_op = in.cs.seq_op,
        .cs_literal = in.cs.literal,
    }, &es.power);

    const misc_out = misc_registers.transact(.{
        .rsn = in.reg.rsn,
        .ll = l.low,
        .dl = in.reg.dl,
        .oa = in.reg.oa,
        .ob = in.reg.ob,
        .inhibit_writes = in.inhibit_writes,
        .data = d,
        .cs_dl_op = in.cs.dl_op,
        .cs_ob_oa_op = in.cs.ob_oa_op,
        .cs_special = in.cs.special,
    });

    register_file.transact(reg_file, .{
        .rsn = misc_out.rsn,
        .read_rsn = in.reg.rsn,
        .oa = in.reg.oa,
        .ob = in.reg.ob,
        .inhibit_writes = in.inhibit_writes,
        .l = l,
        .virtual_address = in.virtual_address,
        .sr1 = in.sr1,
        .sr2 = in.sr2,
        .cs_jkr_wsel = in.cs.jkr_wsel,
        .cs_jkr_wmode = in.cs.jkr_wmode,
        .cs_sr1_wsrc = in.cs.sr1_wsrc,
        .cs_sr2_wsrc = in.cs.sr2_wsrc,
        .cs_sr1_wi = in.cs.sr1_wi,
        .cs_sr2_wi = in.cs.sr2_wi,
        .cs_literal = in.cs.literal,
    });

    const decode_out = decoder.transact(.{
        .exec_mode = in.reg.exec_mode,
        .ua = in.reg.ua,
        .reset = es.reset,
        .interrupt_pending = es.interrupt_pending[@enumToInt(in.pipe)],
        .fault = in.fault,
        .want_atomic = in.want_atomic,
        .stall_atomic = in.stall_atomic,
        .stat = stat_out,
        .lh = l.high,
        .dl = misc_out.dl,
        .cs_special = in.cs.special,
        .cs_next_uop = in.cs.next_uop,
        .cs_seq_op = in.cs.seq_op,
        .cs_allow_int = in.cs.allow_int,
    });

    var next_atomic = in.want_atomic;
    if (!in.inhibit_writes) {
        switch (in.cs.special) {
            .atomic_next => {
                next_atomic = true;
            },
            .atomic_end, .atomic_this => {
                next_atomic = false;
            },
            else => {},
        }
    }

    return .{
        .pipe = in.pipe,
        .cs = microcode[decode_out.ua],
        .reg = .{
            .exec_mode = decode_out.exec_mode,
            .ua = decode_out.ua,
            .dl = misc_out.dl,
            .oa = misc_out.oa,
            .ob = misc_out.ob,
            .rsn = misc_out.rsn,
            .stat = stat_out,
            .asn = next_asn,
            .last_mmu_op = new_last_mmu_op,
        },
        .want_atomic = next_atomic,
    };
}
