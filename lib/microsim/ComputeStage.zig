const std = @import("std");
const misc = @import("misc");
const bus = @import("bus_types");
const at_types = @import("address_translator_types");
const ControlSignals = @import("ControlSignals");
const Simulator = @import("Simulator");
const arithmetic_unit = Simulator.arithmetic_unit;
const shifter_unit = Simulator.shifter_unit;
const logic_unit = Simulator.logic_unit;
const multiplier_unit = Simulator.multiplier_unit;
const faults = Simulator.faults;
const AddressTranslator = Simulator.AddressTranslator;
const LoopRegisters = Simulator.LoopRegisters;
const ExecutionState = Simulator.ExecutionState;
const SystemBusControl = @import("SystemBusControl.zig");
const SetupStage = @import("SetupStage.zig");

const ComputeStage = @This();

pipe: misc.PipeID,
cs: ControlSignals,
reg: LoopRegisters,
want_atomic: bool,
stall_atomic: bool,
sr1: bus.JParts,
sr2: bus.JParts,
jh: bus.JHigh,
arith: arithmetic_unit.Outputs,
shift: shifter_unit.Outputs,
mult: multiplier_unit.Outputs,
logic: bus.JLow,
virtual_address: bus.VirtualAddressParts,
at: AddressTranslator.ComputeOutputsExceptFaults,
fault: faults.ComputeOutputs,
inhibit_writes: bool,

pub fn init(in: SetupStage.PipelineRegister, address_translator: *const AddressTranslator, es: ExecutionState) ComputeStage {
    const arith_out = arithmetic_unit.compute(.{
        .j = in.j,
        .k = in.k,
        .c = in.reg.stat.c,
        .cs_compute_mode = in.cs.compute_mode,
    });
    const shift_out = shifter_unit.compute(.{
        .j = in.j,
        .k = in.k,
        .cs_compute_mode = in.cs.compute_mode,
    });
    const mult_out = multiplier_unit.compute(.{
        .jl = in.j.low,
        .k = in.k,
        .cs_compute_mode = in.cs.compute_mode,
    });
    const logic_out = logic_unit.compute(.{
        .jl = in.j.low,
        .k = in.k,
        .cs_compute_mode = in.cs.compute_mode,
    });

    var at_out = address_translator.compute(.{
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
        .page_fault = at_out.page_fault,
        .page_align_fault = at_out.page_align_fault,
        .access_fault = at_out.access_fault,
        .cs_special = in.cs.special,
    });

    const inhibit_writes = in.stall_atomic or fault.any;

    return .{
        .pipe = es.pipe,
        .cs = in.cs,
        .reg = in.reg,
        .want_atomic = in.want_atomic,
        .stall_atomic = in.stall_atomic,
        .sr1 = in.sr1,
        .sr2 = in.sr2,
        .jh = in.j.high,
        .arith = arith_out,
        .shift = shift_out,
        .mult = mult_out,
        .logic = logic_out,
        .virtual_address = in.virtual_address,
        .at = at_out.base,
        .fault = fault,
        .inhibit_writes = inhibit_writes,
    };
}

pub const PipelineRegister = struct {
    pipe: misc.PipeID = .zero,
    cs: ControlSignals = ControlSignals.init(),
    reg: LoopRegisters = LoopRegisters.init(),
    want_atomic: bool = false,
    stall_atomic: bool = false,
    sr1: bus.JParts = bus.JParts.zero,
    sr2: bus.JParts = bus.JParts.zero,
    jh: bus.JHigh = 0,
    arith: arithmetic_unit.Outputs = .{
        .data = bus.LParts.zero,
        .z = false,
        .n = false,
        .c = false,
        .v = false,
    },
    shift: shifter_unit.Outputs = .{
        .data = bus.LParts.zero,
        .c = false,
    },
    mult: multiplier_unit.Outputs = .{
        .data = bus.LParts.zero,
    },
    logic: bus.LLow = 0,
    virtual_address: bus.VirtualAddressParts = bus.VirtualAddressParts.zero,
    at: AddressTranslator.ComputeOutputsExceptFaults = .{
        .info = at_types.TranslationInfo{},
        .entry_address = 0,
        .matching_entry = at_types.Entry{},
        .other_entry = at_types.Entry{},
        .bus_ctrl = SystemBusControl{},
        .new_kernel_flag = false,
    },
    fault: faults.ComputeOutputs = .{
        .any = false,
        .page = false,
        .page_align = false,
        .access = false,
        .special = false,
    },
    inhibit_writes: bool = false,

    pub fn randomize(self: *PipelineRegister, rnd: std.rand.Random) void {
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
        self.jh = rnd.int(bus.JHigh);
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
        self.logic = rnd.int(bus.LLow);
        self.virtual_address = .{
            .offset = rnd.int(bus.PageOffset),
            .page = rnd.int(bus.Page),
        };
        self.at = .{
            .info = at_types.TranslationInfo.random(rnd),
            .entry_address = rnd.int(at_types.EntryAddress),
            .matching_entry = at_types.Entry.random(rnd),
            .other_entry = at_types.Entry.random(rnd),
            .bus_ctrl = SystemBusControl.random(rnd),
            .new_kernel_flag = rnd.boolean(),
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

    pub fn update(self: *PipelineRegister, out: ComputeStage) void {
        self.pipe = out.pipe;
        self.cs = out.cs;
        self.reg = out.reg;
        self.want_atomic = out.want_atomic;
        self.stall_atomic = out.stall_atomic;
        self.sr1 = out.sr1;
        self.sr2 = out.sr2;
        self.jh = out.jh;
        self.arith = out.arith;
        self.shift = out.shift;
        self.mult = out.mult;
        self.logic = out.logic;
        self.virtual_address = out.virtual_address;
        self.at = out.at;
        self.fault = out.fault;
        self.inhibit_writes = out.inhibit_writes;
    }

    pub fn isAtomic(self: PipelineRegister) bool {
        return self.want_atomic and !self.stall_atomic;
    }

};
