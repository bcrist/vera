const std = @import("std");
const ctrl = @import("control_signals");
const uc_layout = @import("microcode_layout");
const bits = @import("bits");
const misc = @import("misc");
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

pub const Simulator = struct {
    allocator: std.mem.Allocator,
    microcode: []const ctrl.Control_Signals,
    exec_state: ExecutionState,
    reg_file: *register_file.State,
    mmu_state: *mmu.State,
    memory: *Memory,
    device_sys_state: *device_sys.State,

    s: SetupInputs = SetupInputs.init(),
    c: ComputeInputs = ComputeInputs.init(),
    t: TransactInputs = TransactInputs.init(),

    microcycle_count: u64,

    pub fn init(allocator: std.mem.Allocator, microcode: []const ctrl.Control_Signals) !Simulator {
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

    pub fn debugCycle(self: *Simulator, n: u64, pipe: ctrl.Pipe_ID) !void {
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
        while (self.s.cs.SEQ_OP != .next_instruction) {
            self.cycle(1);
        }
        self.cycle(1);
    }

    pub fn printState(self: *Simulator, writer: anytype, pipe: misc.Pipe_ID) !void {
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
            reg.ua, reg.dl, reg.oboa.oa, reg.oboa.ob,
        });
        try reg.stat.print(writer);
        try writer.writeAll("\n");
        try register_file.RegisterView.init(self.reg_file, reg.rsn).printRegs(writer);
    }
};

pub const Split_J_Bus = packed struct {
    low: misc.JL_Bus,
    high: misc.JH_Bus,

    const zero = Split_J_Bus{
        .low = 0,
        .high = 0,
    };
};

pub const Split_L_Bus = packed struct {
    low: misc.LL_Bus,
    high: misc.LH_Bus,

    const zero = Split_L_Bus{
        .low = 0,
        .high = 0,
    };
};

pub const OB_OA = packed struct {
    oa: misc.OA,
    ob: misc.OB,

    const zero = OB_OA{
        .oa = 0,
        .ob = 0,
    };
};

pub const VirtualAddress = packed struct {
    offset: misc.N_Bus,
    page: misc.P_Bus,

    pub const zero = VirtualAddress{
        .offset = 0,
        .page = 0,
    };
};

pub const PhysicalAddress = struct {
    offset: misc.N_Bus,
    even_offset: OffsetType,
    odd_offset: OffsetType,
    frame: misc.F_Bus,
    swap_bytes: bool,

    const OffsetType = std.meta.Int(.unsigned, @bitSizeOf(misc.N_Bus) - 1);

    pub const zero = PhysicalAddress{
        .offset = 0,
        .even_offset = 0,
        .odd_offset = 0,
        .frame = 0,
        .swap_bytes = false,
    };
};

pub const BusControl = struct {
    read: bool,
    write: bool,
    write_even: bool,
    write_odd: bool,
    wait_states: u2,

    pub fn init() BusControl {
        return .{
            .read = false,
            .write = false,
            .write_even = false,
            .write_odd = false,
            .wait_states = 0,
        };
    }
};

const ExecutionState = struct {
    reset: bool,
    sleep: bool,
    interrupt_pending: [3]bool,
    pipe: misc.Pipe_ID, // The pipe ID currently in the transact stage
    power: misc.Power_Mode,

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
    exec_mode: misc.Execution_Mode,
    rsn: misc.RSN,
    ua: uc_layout.UC_Address,
    dl: misc.DL,
    oboa: OB_OA,
    stat: stat.LoopState,
    asn: misc.AT_ASN,
    last_mmu_op: mmu.OperationInfo,
    mmu_matching_entry: mmu.Entry,
    mmu_other_entry: mmu.Entry,

    pub fn init() LoopRegisters {
        return .{
            .exec_mode = .fault,
            .rsn = 0,
            .ua = 0,
            .dl = 0,
            .oboa = OB_OA.zero,
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
            .mmu_matching_entry = mmu.Entry{},
            .mmu_other_entry = mmu.Entry{},
        };
    }

    pub fn randomize(self: *LoopRegisters, rnd: std.rand.Random) void {
        self.exec_mode = rnd.enumValue(misc.Execution_Mode);
        self.rsn = rnd.int(misc.RSN);
        self.ua = rnd.int(uc_layout.UC_Address);
        self.dl = rnd.int(misc.DL);
        self.oboa = .{
            .oa = rnd.int(misc.OA),
            .ob = rnd.int(misc.OB),
        };
        self.stat = .{
            .c = rnd.boolean(),
            .v = rnd.boolean(),
            .n = rnd.boolean(),
            .z = rnd.boolean(),
            .next_k = true, // This must be guaranteed by reset logic
            .k = true, // This must be guaranteed by reset logic
            .a = rnd.boolean(),
        };
        self.asn = rnd.int(misc.AT_ASN);
        self.last_mmu_op = mmu.OperationInfo.random(rnd);
        self.mmu_matching_entry = mmu.Entry.random(rnd);
        self.mmu_other_entry = mmu.Entry.random(rnd);
    }
};

const SetupInputs = struct {
    pipe: misc.Pipe_ID,
    cs: ctrl.Control_Signals,
    reg: LoopRegisters,
    want_atomic: bool,

    pub fn init() SetupInputs {
        return .{
            .pipe = .zero,
            .cs = ctrl.Control_Signals.init(),
            .reg = LoopRegisters.init(),
            .want_atomic = false,
        };
    }

    pub fn randomize(self: *SetupInputs, rnd: std.rand.Random) void {
        self.pipe = rnd.enumValue(misc.Pipe_ID);
        self.cs.randomize(rnd);
        self.reg.randomize(rnd);
        self.want_atomic = rnd.boolean();
    }
};

const ComputeInputs = struct {
    pipe: misc.Pipe_ID,
    cs: ctrl.Control_Signals,
    reg: LoopRegisters,
    want_atomic: bool,
    stall_atomic: bool,
    sr1: Split_J_Bus,
    sr2: Split_J_Bus,
    j: Split_J_Bus,
    k: misc.K_Bus,
    virtual_address: VirtualAddress,

    pub fn init() ComputeInputs {
        return .{
            .pipe = .zero,
            .cs = ctrl.Control_Signals.init(),
            .reg = LoopRegisters.init(),
            .want_atomic = false,
            .stall_atomic = false,
            .sr1 = Split_J_Bus.zero,
            .sr2 = Split_J_Bus.zero,
            .j = Split_J_Bus.zero,
            .k = 0,
            .virtual_address = VirtualAddress.zero,
        };
    }

    pub fn randomize(self: *ComputeInputs, rnd: std.rand.Random) void {
        self.pipe = rnd.enumValue(misc.Pipe_ID);
        self.cs.randomize(rnd);
        self.reg.randomize(rnd);
        self.want_atomic = rnd.boolean();
        self.stall_atomic = rnd.boolean();
        self.sr1 = .{
            .low = rnd.int(misc.JL_Bus),
            .high = rnd.int(misc.JH_Bus),
        };
        self.sr2 = .{
            .low = rnd.int(misc.JL_Bus),
            .high = rnd.int(misc.JH_Bus),
        };
        self.j = .{
            .low = rnd.int(misc.JL_Bus),
            .high = rnd.int(misc.JH_Bus),
        };
        self.k = rnd.int(misc.K_Bus);
        self.virtual_address = .{
            .offset = rnd.int(misc.N_Bus),
            .page = rnd.int(misc.P_Bus),
        };
    }
};

const TransactInputs = struct {
    pipe: misc.Pipe_ID,
    cs: ctrl.Control_Signals,
    reg: LoopRegisters,
    want_atomic: bool,
    stall_atomic: bool,
    sr1: Split_J_Bus,
    sr2: Split_J_Bus,
    arith: arith.Outputs,
    shift: shift.Outputs,
    mult: mult.Outputs,
    logic: logic.Outputs,
    bitcount: bitcount.Outputs,
    virtual_address: VirtualAddress,
    physical_address: PhysicalAddress,
    mmu_op: mmu.OperationInfo,
    mmu_slot_address: misc.AT_Slot,
    mmu_matching_entry: mmu.Entry,
    mmu_other_entry: mmu.Entry,
    mmu_k: bool,
    bus_ctrl: BusControl,
    fault: faults.ComputeOutputs,
    inhibit_writes: bool,

    pub fn init() TransactInputs {
        return .{
            .pipe = .zero,
            .cs = ctrl.Control_Signals.init(),
            .reg = LoopRegisters.init(),
            .want_atomic = false,
            .stall_atomic = false,
            .sr1 = Split_J_Bus.zero,
            .sr2 = Split_J_Bus.zero,
            .arith = .{
                .data = Split_L_Bus.zero,
                .z = false,
                .n = false,
                .c = false,
                .v = false,
            },
            .shift = .{
                .data = Split_L_Bus.zero,
                .c = false,
            },
            .mult = .{
                .data = Split_L_Bus.zero,
            },
            .logic = .{
                .data = Split_L_Bus.zero,
            },
            .bitcount = .{
                .data = 0,
            },
            .virtual_address = VirtualAddress.zero,
            .physical_address = PhysicalAddress.zero,
            .mmu_op = mmu.OperationInfo.init(),
            .mmu_slot_address = 0,
            .mmu_matching_entry = mmu.Entry{},
            .mmu_other_entry = mmu.Entry{},
            .mmu_k = false,
            .bus_ctrl = BusControl.init(),
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
        self.pipe = rnd.enumValue(misc.Pipe_ID);
        self.cs.randomize(rnd);
        self.reg.randomize(rnd);
        self.want_atomic = rnd.boolean();
        self.stall_atomic = rnd.boolean();
        self.sr1 = .{
            .low = rnd.int(misc.JL_Bus),
            .high = rnd.int(misc.JH_Bus),
        };
        self.sr2 = .{
            .low = rnd.int(misc.JL_Bus),
            .high = rnd.int(misc.JH_Bus),
        };
        self.arith = .{
            .data = .{
                .low = rnd.int(misc.LL_Bus),
                .high = rnd.int(misc.LH_Bus),
            },
            .z = rnd.boolean(),
            .n = rnd.boolean(),
            .c = rnd.boolean(),
            .v = rnd.boolean(),
        };
        self.shift = .{
            .data = .{
                .low = rnd.int(misc.LL_Bus),
                .high = rnd.int(misc.LH_Bus),
            },
            .c = rnd.boolean(),
        };
        self.mult.data = .{
            .low = rnd.int(misc.LL_Bus),
            .high = rnd.int(misc.LH_Bus),
        };
        self.logic.data = .{
            .low = rnd.int(misc.LL_Bus),
            .high = rnd.int(misc.LH_Bus),
        };
        self.bitcount = .{
            .data = rnd.int(u16),
        };
        self.virtual_address = .{
            .offset = rnd.int(misc.N_Bus),
            .page = rnd.int(misc.P_Bus),
        };
        self.physical_address = .{
            .offset = rnd.int(misc.N_Bus),
            .even_offset = 0,
            .odd_offset = 0,
            .frame = rnd.int(misc.F_Bus),
            .swap_bytes = rnd.boolean(),
        };
        self.physical_address.odd_offset = @intCast(u11, self.physical_address.offset >> 1);
        self.physical_address.even_offset = @intCast(u11, self.physical_address.odd_offset + (self.physical_address.offset & 1));
        self.mmu_op = mmu.OperationInfo.random(rnd);
        self.mmu_slot_address = rnd.int(misc.AT_Slot);
        self.mmu_matching_entry = mmu.Entry.random(rnd);
        self.mmu_other_entry = mmu.Entry.random(rnd);
        self.bus_ctrl = .{
            .read = rnd.boolean(),
            .write = rnd.boolean(),
            .write_even = rnd.boolean(),
            .write_odd = rnd.boolean(),
            .wait_states = rnd.int(u2),
        };
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
        .oboa = in.reg.oboa,
        .JR_RSEL = in.cs.JR_RSEL,
        .KR_RSEL = in.cs.KR_RSEL,
        .JR_RX = in.cs.JR_RX,
        .KR_RX = in.cs.KR_RX,
        .SR1_RI = in.cs.SR1_RI,
        .SR2_RI = in.cs.SR2_RI,
        .BASE = in.cs.BASE,
        .LITERAL = in.cs.LITERAL,
        .JL_SRC = in.cs.JL_SRC,
        .JH_SRC = in.cs.JH_SRC,
        .K_SRC = in.cs.K_SRC,
    });

    const want_atomic = in.want_atomic or in.cs.SPECIAL == .atomic_this or in.cs.SPECIAL == .atomic_next;

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
            .OFFSET = in.cs.OFFSET,
            .LITERAL = in.cs.LITERAL,
        }),
    };
}

fn simulate_compute(mmu_state: *const mmu.State, es: ExecutionState, in: ComputeInputs) TransactInputs {
    const arith_out = arith.compute(.{
        .j = in.j,
        .k = in.k,
        .c = in.reg.stat.c,
        .ALU_MODE = in.cs.ALU_MODE,
    });
    const shift_out = shift.compute(.{
        .j = in.j,
        .k = in.k,
        .ALU_MODE = in.cs.ALU_MODE,
    });
    const mult_out = mult.compute(.{
        .j = in.j.low,
        .k = in.k,
        .ALU_MODE = in.cs.ALU_MODE,
    });
    const logic_out = logic.compute(.{
        .j = in.j,
        .k = in.k,
        .ALU_MODE = in.cs.ALU_MODE,
    });
    const bitcount_out = bitcount.compute(.{
        .j = in.j.low,
        .ALU_MODE = in.cs.ALU_MODE,
    });

    var mmu_out = mmu.compute(mmu_state, .{
        .virtual_address = in.virtual_address,
        .asn = in.reg.asn,
        .enable_flag = in.reg.stat.a,
        .kernel_flag = in.reg.stat.k,
        .matching_entry = in.reg.mmu_matching_entry,
        .other_entry = in.reg.mmu_other_entry,
        .BUS_MODE = in.cs.BUS_MODE,
        .BUS_RW = in.cs.BUS_RW,
        .BUS_BYTE = in.cs.BUS_BYTE,
        .LL_SRC = in.cs.LL_SRC,
        .AT_OP = in.cs.AT_OP,
        .SR2_WI = in.cs.SR2_WI,
        .SR2_WSRC = in.cs.SR2_WSRC,
    });

    const fault = faults.compute(.{
        .page_fault = mmu_out.page_fault,
        .page_align_fault = mmu_out.page_align_fault,
        .access_fault = mmu_out.access_fault,
        .SPECIAL = in.cs.SPECIAL,
    });

    const inhibit_writes = in.stall_atomic or fault.any;

    if (inhibit_writes) {
        mmu_out.bus_ctrl = .{
            .read = false,
            .write = false,
            .write_even = false,
            .write_odd = false,
            .wait_states = 0,
        };
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
        .physical_address = mmu_out.physical_address,
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

fn simulate_transact(microcode: []const ctrl.Control_Signals, memory: *Memory, reg_file: *register_file.State, mmu_state: *mmu.State, sys_state: *device_sys.State, es: *ExecutionState, in: TransactInputs) SetupInputs {
    const mem_d = memory.read(in.bus_ctrl, in.physical_address);
    const sys_d = device_sys.read(sys_state, in.bus_ctrl, in.physical_address);
    const dl_d = misc_registers.readDL(in.reg.dl, in.cs.DL_OP, in.bus_ctrl);
    // :NewDevice: handle read here

    var d: misc.D_Bus = 0;
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
    const new_last_mmu_op = if (in.cs.AT_OP == .hold) in.reg.last_mmu_op else in.mmu_op;

    var l: Split_L_Bus = undefined;
    l.low = switch (in.cs.LL_SRC) {
        .zero => 0,
        .logic => in.logic.data.low,
        .shift_L => in.shift.data.low,
        .arith_L => in.arith.data.low,
        .mult_L => in.mult.data.low,
        .bitcount => in.bitcount.data,
        .D16 => d,
        .D8_sx => bits.sx(misc.LL_Bus, @truncate(u8, d)),
        .AT_ME_L => @truncate(u16, @bitCast(u32, in.mmu_matching_entry)),
        .AT_OE_L => @truncate(u16, @bitCast(u32, in.mmu_other_entry)),
        .last_mmu_op_L => @truncate(u16, new_last_mmu_op.toU32()),
        .STAT => @bitCast(u16, misc.STAT_Bits{
            .Z = in.reg.stat.z,
            .N = in.reg.stat.n,
            .C = in.reg.stat.c,
            .V = in.reg.stat.v,
            .K = in.reg.stat.k,
            .A = in.reg.stat.a,
            .PIPE = in.pipe,
            .MODE = in.reg.exec_mode,
            .PWR = es.power,
        }),
        .pipe_ID => @enumToInt(in.pipe),
    };
    l.high = switch (in.cs.LH_SRC) {
        .zero => 0,
        .logic => in.logic.data.low,
        .shift_H => in.shift.data.high,
        .arith_H => in.arith.data.high,
        .mult_H => in.mult.data.high,
        .D16 => d,
        .sx => bits.sx(misc.LH_Bus, @truncate(u1, l.low >> 15)),
        .JH => in.logic.data.high,
        .AT_ME_H => @intCast(u16, @bitCast(u32, in.mmu_matching_entry) >> 16),
        .AT_OE_H => @intCast(u16, @bitCast(u32, in.mmu_other_entry) >> 16),
        .last_mmu_op_H => @intCast(u16, new_last_mmu_op.toU32() >> 16),
        .prev_UA => in.reg.ua,
    };

    if (in.bus_ctrl.write and in.cs.DL_OP != .to_D) {
        assert(in.cs.LL_SRC != .D16);
        assert(in.cs.LL_SRC != .D8_sx);
        d = l.low;
        num_d_drivers += 1;
    }

    assert(num_d_drivers <= 1);

    memory.write(in.bus_ctrl, in.physical_address, d, in.cs.SPECIAL == .block_transfer);
    device_sys.write(sys_state, in.bus_ctrl, in.physical_address, d, in.mmu_matching_entry.update_frame_state);
    // :NewDevice: handle write here

    var next_asn = in.reg.asn;
    if (!in.inhibit_writes and in.cs.SR2_WI == .ASN) {
        next_asn = @truncate(misc.AT_ASN, l.low);
    }

    const mmu_out = mmu.transact(mmu_state, .{
        .inhibit_writes = in.inhibit_writes,
        .matching_entry = in.mmu_matching_entry,
        .other_entry = in.mmu_other_entry,
        .slot = in.mmu_slot_address,
        .l = l,
        .tag = in.mmu_op.tag,
        .AT_OP = in.mmu_op.AT_OP,
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
        .mmu_z = mmu_out.z,
        .mmu_n = mmu_out.n,
        .mmu_k = in.mmu_k,
        .STAT_OP = in.cs.STAT_OP,
        .SEQ_OP = in.cs.SEQ_OP,
        .LITERAL = in.cs.LITERAL,
    }, &es.power);

    const misc_out = misc_registers.transact(.{
        .rsn = in.reg.rsn,
        .ll = l.low,
        .dl = in.reg.dl,
        .oboa = in.reg.oboa,
        .inhibit_writes = in.inhibit_writes,
        .data = d,
        .DL_OP = in.cs.DL_OP,
        .OB_OA_OP = in.cs.OB_OA_OP,
        .SPECIAL = in.cs.SPECIAL,
    });

    register_file.transact(reg_file, .{
        .rsn = misc_out.rsn,
        .read_rsn = in.reg.rsn,
        .oboa = in.reg.oboa,
        .inhibit_writes = in.inhibit_writes,
        .l = l,
        .virtual_address = in.virtual_address,
        .sr1 = in.sr1,
        .sr2 = in.sr2,
        .JKR_WSEL = in.cs.JKR_WSEL,
        .JKR_WMODE = in.cs.JKR_WMODE,
        .SR1_WSRC = in.cs.SR1_WSRC,
        .SR2_WSRC = in.cs.SR2_WSRC,
        .SR1_WI = in.cs.SR1_WI,
        .SR2_WI = in.cs.SR2_WI,
        .LITERAL = in.cs.LITERAL,
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
        .SPECIAL = in.cs.SPECIAL,
        .NEXT_UOP = in.cs.NEXT_UOP,
        .SEQ_OP = in.cs.SEQ_OP,
        .ALLOW_INT = in.cs.ALLOW_INT,
    });

    var next_atomic = in.want_atomic;
    if (!in.inhibit_writes) {
        switch (in.cs.SPECIAL) {
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
            .oboa = misc_out.oboa,
            .rsn = misc_out.rsn,
            .stat = stat_out,
            .asn = next_asn,
            .last_mmu_op = new_last_mmu_op,
            .mmu_matching_entry = mmu_out.matching_entry,
            .mmu_other_entry = mmu_out.other_entry,
        },
        .want_atomic = next_atomic,
    };
}
