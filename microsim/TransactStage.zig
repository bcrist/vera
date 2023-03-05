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

const TransactStage = @This();

pipe: misc.PipeID,
cs: ControlSignals,
reg: LoopRegisters,
want_atomic: bool,
l: bus.LParts,
bus_ctrl: SystemBusControl,
num_data_drivers: usize,
data_to_write: bus.D,
is_block_transfer: bool,
update_frame_state: bool,
power: ?misc.PowerMode,
register_file: RegisterFile.TransactInputs,
address_translator: AddressTranslator.TransactInputs,

pub fn init(
    in: ComputeStage.PipelineRegister,
    microcode: *const [misc.microcode_length]ControlSignals,
    exec_state: ExecutionState,
    memory: *const Memory,
    frame_tracker: *const FrameTracker
) TransactStage {
    const bus_ctrl = in.at.bus_ctrl.getForTransact(in.inhibit_writes);

    var d: bus.D = 0;
    var num_d_drivers: usize = 0;

    if (memory.read(bus_ctrl)) |mem_data| {
        d = mem_data;
        num_d_drivers += 1;
    }
    if (frame_tracker.read(bus_ctrl)) |frame_tracker_data| {
        d = frame_tracker_data;
        num_d_drivers += 1;
    }
    if (in.cs.dl_op == .to_d) {
        d = in.reg.dl;
        num_d_drivers += 1;
    }

    // Note this happens even if in.inhibit_writes is true
    const at_info_latch = if (in.cs.at_op == .none) in.reg.last_translation else in.at.info;

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
        .last_translation_info_l => @truncate(u16, @bitCast(u32, at_info_latch)),
        .stat => @bitCast(u16, misc.StatusBits{
            .z = in.reg.stat.z,
            .n = in.reg.stat.n,
            .c = in.reg.stat.c,
            .v = in.reg.stat.v,
            .k = in.reg.stat.k,
            .a = in.reg.stat.a,
            .pipe = in.pipe,
            .mode = in.reg.exec_mode,
            .pwr = exec_state.power,
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
        .last_translation_info_h => @intCast(u16, @bitCast(u32, at_info_latch) >> 16),
        .prev_ua => in.reg.ua,
    };

    var output_ll_to_d = false;
    if (bus_ctrl.write and in.cs.dl_op != .to_d) {
        output_ll_to_d = true;
        d = l.low;
        num_d_drivers += 1;
    }

    const dl = if (in.inhibit_writes) in.reg.dl else switch (in.cs.dl_op) {
        .from_d => d,
        .hold, .to_d => in.reg.dl,
    };

    const oa = if (in.inhibit_writes) in.reg.oa else switch (in.cs.ob_oa_op) {
        .hold, .increment_ob, .clear_ob => in.reg.oa,
        .from_dl => @truncate(misc.OperandA, dl),
    };

    const ob = if (in.inhibit_writes) in.reg.ob else switch (in.cs.ob_oa_op) {
        .hold => in.reg.ob,
        .from_dl => @truncate(misc.OperandB, dl >> 4),
        .increment_ob => in.reg.ob +% 1,
        .clear_ob => 0,
    };

    const rsn = if (in.inhibit_writes) in.reg.rsn else switch (in.cs.special) {
        .load_rsn_from_ll => @truncate(misc.RegistersetNumber, l.low),
        .toggle_rsn => in.reg.rsn ^ 0x20,
        else => in.reg.rsn,
    };

    const asn = if (!in.inhibit_writes and in.cs.sr2_wi == .asn) @truncate(at_types.AddressSpaceNumber, l.low) else in.reg.asn;

    const stat_out = StatusRegister.getForTransact(.{
        .state = in.reg.stat,
        .inhibit_writes = in.inhibit_writes,
        .l = l,
        .shift_c = in.shift.c,
        .arith_z = in.arith.z,
        .arith_n = in.arith.n,
        .arith_c = in.arith.c,
        .arith_v = in.arith.v,
        .at_k = in.at.new_kernel_flag,
        .cs_stat_op = in.cs.stat_op,
        .cs_seq_op = in.cs.seq_op,
        .cs_literal = in.cs.literal,
    });

    const decode_out = decoder.getForTransact(.{
        .exec_mode = in.reg.exec_mode,
        .ua = in.reg.ua,
        .reset = exec_state.reset,
        .interrupt_pending = exec_state.interrupt_pending[@enumToInt(in.pipe)],
        .fault = in.fault,
        .want_atomic = in.want_atomic,
        .stall_atomic = in.stall_atomic,
        .flags = stat_out.state.toUCFlags(),
        .lh = l.high,
        .dl = dl,
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
            .dl = dl,
            .oa = oa,
            .ob = ob,
            .rsn = rsn,
            .stat = stat_out.state,
            .asn = asn,
            .last_translation = at_info_latch,
        },
        .power = stat_out.power,
        .l = l,
        .want_atomic = next_atomic,
        .bus_ctrl = bus_ctrl,
        .num_data_drivers = num_d_drivers,
        .data_to_write = d,
        .is_block_transfer = in.cs.special == .block_transfer,
        .update_frame_state = in.at.matching_entry.update_frame_state,
        .register_file = .{
            .rsn = in.reg.rsn,
            .setup_rsn = in.reg.rsn,
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
        },
        .address_translator = .{
            .inhibit_writes = in.inhibit_writes,
            .entry_address = in.at.entry_address,
            .matching_entry = in.at.matching_entry,
            .other_entry = in.at.other_entry,
            .l = l,
            .tag = in.at.info.tag,
            .cs_at_op = in.at.info.cs_at_op,
        },
    };
}

pub const PipelineRegister = struct {
    pipe: misc.PipeID = .zero,
    cs: ControlSignals = ControlSignals.init(),
    reg: LoopRegisters = LoopRegisters.init(),
    want_atomic: bool = false,

    pub fn randomize(self: *PipelineRegister, rnd: std.rand.Random) void {
        self.pipe = rnd.enumValue(misc.PipeID);
        self.cs.randomize(rnd);
        self.reg.randomize(rnd);
        self.want_atomic = rnd.boolean();
    }

    pub fn update(self: *PipelineRegister, out: TransactStage, exec_state: *ExecutionState, register_file: *RegisterFile, address_translator: *AddressTranslator, memory: *Memory, frame_tracker: *FrameTracker) void {
        self.pipe = out.pipe;
        self.cs = out.cs;
        self.reg = out.reg;
        self.want_atomic = out.want_atomic;

        //assert(out.num_data_drivers <= 1);

        // if (out.output_ll_to_d) {
        //     assert(in.cs.ll_src != .d16);
        //     assert(in.cs.ll_src != .d8_sx);
        // }

        exec_state.power = out.power orelse exec_state.power;

        register_file.transact(out.register_file);
        address_translator.transact(out.address_translator);
        memory.transact(out.bus_ctrl, out.data_to_write, out.is_block_transfer);
        frame_tracker.transact(out.bus_ctrl, out.data_to_write, out.update_frame_state);
    }
};
