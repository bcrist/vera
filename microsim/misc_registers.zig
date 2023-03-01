const sim = @import("Simulator");
const ControlSignals = @import("ControlSignals");
const misc = @import("misc");
const bus = @import("bus");

const SystemBusControl = @import("SystemBusControl.zig");

pub fn readDL(dl: u16, cs_dl_op: ControlSignals.DataLatchOp, bus_ctrl: SystemBusControl) ?u16 {
    if (cs_dl_op == .to_d and !bus_ctrl.read) {
        return dl;
    } else {
        return null;
    }
}

pub const TransactInputs = struct {
    dl: bus.D,
    oa: misc.OperandA,
    ob: misc.OperandB,
    rsn: misc.RegistersetNumber,
    inhibit_writes: bool,
    data: bus.D,
    ll: bus.LLow,
    cs_dl_op: ControlSignals.DataLatchOp,
    cs_ob_oa_op: ControlSignals.OperandRegOp,
    cs_special: ControlSignals.SpecialOp,
};

pub const TransactOutputs = struct {
    dl: bus.D,
    oa: misc.OperandA,
    ob: misc.OperandB,
    rsn: misc.RegistersetNumber,
};

pub fn transact(in: TransactInputs) TransactOutputs {
    if (in.inhibit_writes) {
        return .{
            .dl = in.dl,
            .oa = in.oa,
            .ob = in.ob,
            .rsn = in.rsn,
        };
    }
    const dl = switch (in.cs_dl_op) {
        .from_d => in.data,
        .hold, .to_d => in.dl,
    };
    const oa = switch (in.cs_ob_oa_op) {
        .hold, .increment_ob, .clear_ob => in.oa,
        .from_dl => @truncate(misc.OperandA, dl),
    };
    const ob = switch (in.cs_ob_oa_op) {
        .hold => in.ob,
        .from_dl => @truncate(misc.OperandB, dl >> 4),
        .increment_ob => in.ob +% 1,
        .clear_ob => 0,
    };
    const rsn: misc.RegistersetNumber = switch (in.cs_special) {
        .load_rsn_from_ll => @truncate(misc.RegistersetNumber, in.ll),
        .toggle_rsn => in.rsn ^ 0x20,
        else => in.rsn,
    };

    return .{
        .dl = dl,
        .oa = oa,
        .ob = ob,
        .rsn = rsn,
    };
}
