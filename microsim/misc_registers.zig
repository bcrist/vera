const sim = @import("Simulator");
const ControlSignals = @import("ControlSignals");
const misc = @import("misc");
const bus = @import("bus");

const SystemBusControl = @import("SystemBusControl.zig");

pub fn readDL(dl: u16, DL_OP: ControlSignals.DataLatchOp, bus_ctrl: SystemBusControl) ?u16 {
    if (DL_OP == .to_d and !bus_ctrl.read) {
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
    DL_OP: ControlSignals.DataLatchOp,
    OB_OA_OP: ControlSignals.OperandRegOp,
    SPECIAL: ControlSignals.SpecialOp,
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
    const dl = switch (in.DL_OP) {
        .from_d => in.data,
        .hold, .to_d => in.dl,
    };
    const oa = switch (in.OB_OA_OP) {
        .hold, .increment_ob, .clear_ob => in.oa,
        .from_dl => @truncate(misc.OperandA, dl),
    };
    const ob = switch (in.OB_OA_OP) {
        .hold => in.ob,
        .from_dl => @truncate(misc.OperandB, dl >> 4),
        .increment_ob => in.ob +% 1,
        .clear_ob => 0,
    };
    const rsn: misc.RegistersetNumber = switch (in.SPECIAL) {
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
