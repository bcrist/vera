const std = @import("std");
const ControlSignals = @import("ControlSignals");
const uc = @import("microcode");
const bits = @import("bits");
const misc = @import("misc");
const bus = @import("bus_types");

pub const arithmetic_unit = @import("cpu/arithmetic_unit.zig");
pub const logic_unit = @import("cpu/logic_unit.zig");
pub const shifter_unit = @import("cpu/shifter_unit.zig");
pub const multiplier_unit = @import("cpu/multiplier_unit.zig");
pub const RegisterFile = @import("cpu/RegisterFile.zig");
pub const StatusRegister = @import("cpu/StatusRegister.zig");
pub const decoder = @import("cpu/decoder.zig");
pub const faults = @import("cpu/faults.zig");
pub const address_generator = @import("cpu/address_generator.zig");
pub const AddressTranslator = @import("cpu/AddressTranslator.zig");

pub const Memory = @import("devices/Memory.zig");
pub const FrameTracker = @import("devices/FrameTracker.zig");

pub const SetupStage = @import("SetupStage.zig");
pub const ComputeStage = @import("ComputeStage.zig");
pub const TransactStage = @import("TransactStage.zig");

pub const SystemBusControl = @import("SystemBusControl.zig");
pub const LoopRegisters = @import("LoopRegisters.zig");

pub const ExecutionState = struct {
    reset: bool = false,
    interrupt_pending: [3]bool = .{ false } ** 3,
    pipe: misc.PipeID = .zero, // The pipe ID currently in the transact stage
    power: misc.PowerMode = .run,
};

const Simulator = @This();

microcode: *const [misc.microcode_length]ControlSignals,
register_file: *RegisterFile,
address_translator: *AddressTranslator,
memory: *Memory, 
frame_tracker: *FrameTracker,

exec_state: ExecutionState = .{},
s: SetupStage.PipelineRegister = .{},
c: ComputeStage.PipelineRegister = .{},
t: TransactStage.PipelineRegister = .{},
microcycles_simulated: u64 = 0,

pub fn init(allocator: std.mem.Allocator, microcode: *const [misc.microcode_length]ControlSignals) !Simulator {
    const memory = try allocator.create(Memory);
    errdefer allocator.destroy(memory);
    const register_file = try allocator.create(RegisterFile);
    errdefer allocator.destroy(register_file);
    const address_translator = try allocator.create(AddressTranslator);
    errdefer allocator.destroy(address_translator);
    const frame_tracker = try allocator.create(FrameTracker);
    errdefer allocator.destroy(frame_tracker);

    memory.reset();
    register_file.reset();
    address_translator.reset();
    frame_tracker.reset();

    return Simulator{
        .microcode = microcode,
        .exec_state = ExecutionState{},

        .register_file = register_file,
        .address_translator = address_translator,
        .memory = memory,
        .frame_tracker = frame_tracker,
    };
}

pub fn deinit(self: *Simulator, allocator: std.mem.Allocator) void {
    allocator.destroy(self.frame_tracker);
    allocator.destroy(self.address_translator);
    allocator.destroy(self.register_file);
    allocator.destroy(self.memory);
}

pub fn randomizeState(self: *Simulator, rnd: std.rand.Random) void {
    self.memory.randomize(rnd);
    self.register_file.randomize(rnd);
    self.address_translator.randomize(rnd);
    self.frame_tracker.randomize(rnd);
    self.s.randomize(rnd);
    self.c.randomize(rnd);
    self.t.randomize(rnd);
}

pub fn microcycle(self: *Simulator, n: u64) void {
    var i: u64 = 0;
    while (i < n) : (i += 1) {
        self.exec_state.pipe = self.exec_state.pipe.next();

        const s = SetupStage.init(self.t, self.register_file, self.s.isAtomic(), self.c.isAtomic());
        const c = ComputeStage.init(self.s, self.address_translator, self.exec_state);
        const t = TransactStage.init(self.c, self.microcode, self.exec_state, self.memory, self.frame_tracker);

        self.s.update(s);
        self.c.update(c);
        self.t.update(t, &self.exec_state, self.register_file, self.address_translator, self.memory, self.frame_tracker);

        self.microcycles_simulated += 1;
    }
}

pub fn cycle(self: *Simulator, n: u64) void {
    self.microcycle(n * 3);
}

pub fn resetAndCycle(self: *Simulator) void {
    self.exec_state.reset = true;
    self.cycle(1);
    while (self.t.pipe != .zero) {
        self.microcycle(1);
    }
    self.exec_state.reset = false;
}

pub fn resetAndInit(self: *Simulator) void {
    self.resetAndCycle();
    while (self.t.cs.seq_op != .next_instruction) {
        self.cycle(1);
    }
    self.cycle(1);
}
