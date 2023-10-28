const std = @import("std");
const bits = @import("bits");
const misc = @import("misc");
const bus = @import("bus_types");
const at_types = @import("address_translator_types");
const ControlSignals = @import("ControlSignals");
const Simulator = @import("Simulator");
const ExecutionState = Simulator.ExecutionState;
const LoopRegisters = Simulator.LoopRegisters;
const AddressTranslator = Simulator.AddressTranslator;
const Memory = Simulator.Memory;
const FrameTracker = Simulator.FrameTracker;
const RegisterFile = Simulator.RegisterFile;
const StatusRegister = Simulator.StatusRegister;
const decoder = Simulator.decoder;
const SystemBusControl = @import("SystemBusControl.zig");
const ComputeStage = @import("ComputeStage.zig");


pub const 



pipe: misc.PipeID,
cs: ControlSignals,
reg: LoopRegisters,


pub fn init(
    in: TransactStage.PipelineRegister,
    insn_decode: *const [misc.opcode_count]