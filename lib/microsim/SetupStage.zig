const std = @import("std");
const misc = @import("misc");
const bus = @import("bus_types");
const ControlSignals = @import("ControlSignals");
const Simulator = @import("Simulator");
const RegisterFile = Simulator.RegisterFile;
const LoopRegisters = Simulator.LoopRegisters;
const address_generator = Simulator.address_generator;
const TransactStage = @import("TransactStage.zig");

const SetupStage = @This();

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

debug_base: u32,
debug_offset: i7,

pub fn init(in: TransactStage.PipelineRegister, register_file: *const RegisterFile, atomic_compute: bool, atomic_transact: bool) SetupStage {
    const rf = register_file.setup(.{
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
    const stall_atomic = want_atomic and (atomic_compute or atomic_transact);

    const address_out = address_generator.setup(.{
        .base = rf.address_base,
        .cs_offset = in.cs.offset,
        .cs_literal = in.cs.literal,
    });

    return .{
        .pipe = in.pipe,
        .cs = in.cs,
        .reg = in.reg,
        .want_atomic = want_atomic,
        .stall_atomic = stall_atomic,
        .j = rf.j,
        .k = rf.k,
        .sr1 = rf.sr1,
        .sr2 = rf.sr2,
        .virtual_address = address_out.virtual_address,
        .debug_base = rf.address_base,
        .debug_offset = address_out.debug_offset,
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
    j: bus.JParts = bus.JParts.zero,
    k: bus.K = 0,
    virtual_address: bus.VirtualAddressParts = bus.VirtualAddressParts.zero,

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

    pub fn update(self: *PipelineRegister, out: SetupStage) void {
        self.pipe = out.pipe;
        self.cs = out.cs;
        self.reg = out.reg;
        self.want_atomic = out.want_atomic;
        self.stall_atomic = out.stall_atomic;
        self.sr1 = out.sr1;
        self.sr2 = out.sr2;
        self.j = out.j;
        self.k = out.k;
        self.virtual_address = out.virtual_address;
    }

    pub fn isAtomic(self: PipelineRegister) bool {
        return self.want_atomic and !self.stall_atomic;
    }

};
