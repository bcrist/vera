const sim = @import("simulator");
const ctrl = @import("control_signals");
const misc = @import("misc");

const BusControl = sim.BusControl;
const OB_OA = sim.OB_OA;

pub fn readDL(dl: u16, DL_OP: ctrl.Data_Latch_Op, bus_ctrl: BusControl) ?u16 {
    if (DL_OP == .to_D and !bus_ctrl.read) {
        return dl;
    } else {
        return null;
    }
}

pub const TransactInputs = struct {
    dl: misc.DL,
    oboa: OB_OA,
    rsn: misc.RSN,
    inhibit_writes: bool,
    data: misc.D_Bus,
    ll: misc.LL_Bus,
    DL_OP: ctrl.Data_Latch_Op,
    OB_OA_OP: ctrl.Operand_Reg_Op,
    SPECIAL: ctrl.Special_Op,
};

pub const TransactOutputs = struct {
    dl: misc.DL,
    oboa: OB_OA,
    rsn: misc.RSN,
};

pub fn transact(in: TransactInputs) TransactOutputs {
    if (in.inhibit_writes) {
        return .{
            .dl = in.dl,
            .oboa = in.oboa,
            .rsn = in.rsn,
        };
    }
    const dl = switch (in.DL_OP) {
        .from_D => in.data,
        .hold, .to_D => in.dl,
    };
    const oboa = switch (in.OB_OA_OP) {
        .hold => in.oboa,
        .from_DL => @bitCast(OB_OA, @truncate(u8, dl)),
        .increment_OB => OB_OA{
            .oa = in.oboa.oa,
            .ob = in.oboa.ob +% 1,
        },
        .clear_OB => OB_OA{
            .oa = in.oboa.oa,
            .ob = 0,
        },
    };
    const rsn: misc.RSN = switch (in.SPECIAL) {
        .load_RSN_from_LL => @truncate(misc.RSN, in.ll),
        .toggle_RSN => in.rsn ^ 0x20,
        else => in.rsn,
    };

    return .{
        .dl = dl,
        .oboa = oboa,
        .rsn = rsn,
    };
}
