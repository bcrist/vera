const ib = @import("instruction_builder.zig");
const cb = @import("cycle_builder.zig");
const ControlSignals = @import("ControlSignals");
const misc = @import("misc");

const SignedOffsetForLiteral = misc.SignedOffsetForLiteral;

const encodingWithSuffix = ib.encodingWithSuffix;
const IP_relative = ib.IP_relative;
const SP_relative = ib.SP_relative;
const UXP_relative = ib.UXP_relative;
const KXP_relative = ib.KXP_relative;
const Xa_relative = ib.Xa_relative;
const desc = ib.desc;
const next_cycle = ib.next_cycle;
const conditional_next_cycle = ib.conditional_next_cycle;
const opcode_high = ib.opcode_high;
const OB = ib.OB;
const kernel = ib.kernel;
const negative = ib.negative;
const zero = ib.zero;

const op_reg_to_LL = cb.op_reg_to_LL;
const op_reg32_to_L = cb.op_reg32_to_L;
const reg_to_LL = cb.reg_to_LL;
const L_to_SR = cb.L_to_SR;
const LL_to_op_reg = cb.LL_to_op_reg;
const LL_to_reg = cb.LL_to_reg;
const write_from_LL = cb.write_from_LL;
const write_from_DL = cb.write_from_DL;
const read_to_D = cb.read_to_D;
const D_to_L = cb.D_to_L;
const D_to_DL = cb.D_to_DL;
const load_next_insn = cb.load_next_insn;
const exec_next_insn_no_atomic_end = cb.exec_next_insn_no_atomic_end;
const load_and_exec_next_insn = cb.load_and_exec_next_insn;
const op_reg32_plus_op_reg_to_L = cb.op_reg32_plus_op_reg_to_L;
const op_reg32_plus_literal_to_L = cb.op_reg32_plus_literal_to_L;
const op_reg32_minus_literal_to_L = cb.op_reg32_minus_literal_to_L;
const SR_plus_op_reg_to_L = cb.SR_plus_op_reg_to_L;
const SR_plus_literal_to_L = cb.SR_plus_literal_to_L;
const IP_read_to_D = cb.IP_read_to_D;
const SR_plus_SRL_to_L = cb.SR_plus_SRL_to_L;
const L_to_op_reg32 = cb.L_to_op_reg32;
const illegal_instruction = cb.illegal_instruction;
const PN_to_SR = cb.PN_to_SR;
const allow_interrupt = cb.allow_interrupt;

fn fromRegPointer(inc: SignedOffsetForLiteral) void {
    op_reg32_plus_literal_to_L(.OA, inc, .fresh, .no_flags);
    L_to_SR(.temp_1);
    load_next_insn(2);
    next_cycle();
}

fn incRegPointer(inc: SignedOffsetForLiteral) void {
    op_reg32_plus_literal_to_L(.OA, inc, .fresh, .no_flags);
    L_to_SR(.temp_1);
    L_to_op_reg32(.OA);
    load_next_insn(2);
    next_cycle();
}

fn fromRegPointerPlusRegOffset() void {
    op_reg32_plus_op_reg_to_L(.OA, .OB, .zx, .fresh, .no_flags);
    L_to_SR(.temp_1);
    load_next_insn(2);
    next_cycle();
}

fn fromRegPointerPlusImmOffset(offset: SignedOffsetForLiteral) void {
    op_reg32_plus_literal_to_L(.OA, offset, .fresh, .no_flags);
    L_to_SR(.temp_1);
    load_next_insn(2);
    next_cycle();
}

fn fromSPPlusRegOffset() void {
    SR_plus_op_reg_to_L(.sp, .OA, .sx, .fresh, .no_flags);
    L_to_SR(.temp_1);
    load_next_insn(2);
    next_cycle();
}

fn fromSPPlusImmOffset(offset: SignedOffsetForLiteral) void {
    SR_plus_literal_to_L(.sp, offset, .fresh, .no_flags);
    L_to_SR(.temp_1);
    load_next_insn(2);
    next_cycle();
}

fn fromSPPlusImm16Offset() void {
    IP_read_to_D(2, .word);
    D_to_L(.zx);
    L_to_SR(.temp_2);
    next_cycle();

    SR_plus_SRL_to_L(.sp, .temp_2, .sx, .fresh, .no_flags);
    L_to_SR(.temp_1);
    load_next_insn(4);
    next_cycle();
}

fn opLoad8(reg: cb.OA_or_OB, bus_mode: ControlSignals.BusMode, ext: cb.ZX_SX_or_1X, offset: SignedOffsetForLiteral) void {
    read_to_D(.temp_1, offset, .byte, bus_mode);
    D_to_L(ext);
    LL_to_op_reg(switch (reg) {
        .OA => .OA,
        .OB => .OB,
    });
    exec_next_insn_no_atomic_end();
}
fn opLoad16(reg: cb.OA_or_OB, bus_mode: ControlSignals.BusMode, offset: SignedOffsetForLiteral) void {
    read_to_D(.temp_1, offset, .word, bus_mode);
    D_to_L(.zx);
    LL_to_op_reg(switch (reg) {
        .OA => .OA,
        .OB => .OB,
    });
    exec_next_insn_no_atomic_end();
}
fn opLoad32(reg: cb.OA_or_OB, bus_mode: ControlSignals.BusMode, offset: SignedOffsetForLiteral) void {
    read_to_D(.temp_1, offset, .word, bus_mode);
    D_to_L(.zx);
    LL_to_op_reg(switch (reg) {
        .OA => .OA,
        .OB => .OB,
    });
    next_cycle();

    read_to_D(.temp_1, offset + 2, .word, bus_mode);
    D_to_L(.zx);
    LL_to_op_reg(switch (reg) {
        .OA => .OAxor1,
        .OB => .OBxor1,
    });
    exec_next_insn_no_atomic_end();
}

fn load8(reg: misc.RegisterIndex, bus_mode: ControlSignals.BusMode, ext: cb.ZX_SX_or_1X, offset: SignedOffsetForLiteral) void {
    read_to_D(.temp_1, offset, .byte, bus_mode);
    D_to_L(ext);
    LL_to_reg(reg);
    exec_next_insn_no_atomic_end();
}
fn load16(reg: misc.RegisterIndex, bus_mode: ControlSignals.BusMode, offset: SignedOffsetForLiteral) void {
    read_to_D(.temp_1, offset, .word, bus_mode);
    D_to_L(.zx);
    LL_to_reg(reg);
    exec_next_insn_no_atomic_end();
}
fn load32(reg: misc.RegisterIndex, bus_mode: ControlSignals.BusMode, offset: SignedOffsetForLiteral) void {
    read_to_D(.temp_1, offset, .word, bus_mode);
    D_to_L(.zx);
    LL_to_reg(reg);
    next_cycle();

    read_to_D(.temp_1, offset + 2, .word, bus_mode);
    D_to_L(.zx);
    LL_to_reg(reg ^ 1);
    exec_next_insn_no_atomic_end();
}

fn opStore8(reg: cb.OA_or_OB, bus_mode: ControlSignals.BusMode, offset: SignedOffsetForLiteral) void {
    op_reg_to_LL(switch (reg) {
        .OA => .OA,
        .OB => .OB,
    });
    write_from_LL(.temp_1, offset, .byte, bus_mode);
    exec_next_insn_no_atomic_end();
}
fn opStore16(reg: cb.OA_or_OB, bus_mode: ControlSignals.BusMode, offset: SignedOffsetForLiteral) void {
    op_reg_to_LL(switch (reg) {
        .OA => .OA,
        .OB => .OB,
    });
    write_from_LL(.temp_1, offset, .word, bus_mode);
    exec_next_insn_no_atomic_end();
}
fn opStore32(reg: cb.OA_or_OB, bus_mode: ControlSignals.BusMode, offset: SignedOffsetForLiteral) void {
    op_reg_to_LL(switch (reg) {
        .OA => .OA,
        .OB => .OB,
    });
    write_from_LL(.temp_1, offset, .word, bus_mode);
    next_cycle();

    op_reg_to_LL(switch (reg) {
        .OA => .OAxor1,
        .OB => .OBxor1,
    });
    write_from_LL(.temp_1, offset + 2, .word, bus_mode);
    exec_next_insn_no_atomic_end();
}

fn store8(reg: misc.RegisterIndex, bus_mode: ControlSignals.BusMode, offset: SignedOffsetForLiteral) void {
    reg_to_LL(reg);
    write_from_LL(.temp_1, offset, .byte, bus_mode);
    exec_next_insn_no_atomic_end();
}
fn store16(reg: misc.RegisterIndex, bus_mode: ControlSignals.BusMode, offset: SignedOffsetForLiteral) void {
    reg_to_LL(reg);
    write_from_LL(.temp_1, offset, .word, bus_mode);
    exec_next_insn_no_atomic_end();
}
fn store32(reg: misc.RegisterIndex, bus_mode: ControlSignals.BusMode, offset: SignedOffsetForLiteral) void {
    reg_to_LL(reg);
    write_from_LL(.temp_1, offset, .word, bus_mode);
    next_cycle();

    reg_to_LL(reg ^ 1);
    write_from_LL(.temp_1, offset + 2, .word, bus_mode);
    exec_next_insn_no_atomic_end();
}

// Instruction memory loads
pub fn _C800_C8FF() void {
    encodingWithSuffix(.LD, .I, .{ Xa_relative(.I, .imm_0), .to, .BbU });
    //syntax("LD.I Xa -> UBb");
    desc("Load unsigned byte using pointer to instruction memory");
    fromRegPointer(0);
    opLoad8(.OB, .insn, .zx, 0);
}
pub fn _C900_C9FF() void {
    encodingWithSuffix(.LD, .I, .{ Xa_relative(.I, .imm_0), .to, .BbS });
    //syntax("LD.I Xa -> SBb");
    desc("Load signed byte using pointer to instruction memory");
    fromRegPointer(0);
    opLoad8(.OB, .insn, .sx, 0);
}
pub fn _CA00_CAFF() void {
    encodingWithSuffix(.LD, .I, .{ Xa_relative(.I, .imm_0), .to, .Rb });
    //syntax("LD.I Xa -> Rb");
    desc("Load 16b register using pointer to instruction memory");
    fromRegPointer(0);
    opLoad16(.OB, .insn, 0);
}
pub fn _CB00_CBFF() void {
    encodingWithSuffix(.LD, .I, .{ Xa_relative(.I, .imm_0), .to, .Xb });
    //syntax("LD.I Xa -> Xb");
    desc("Load 32b register using pointer to instruction memory");
    fromRegPointer(0);
    opLoad32(.OB, .insn, 0);
}

// plain stack pointer load/store
pub fn _D800_D8FF() void {
    encodingWithSuffix(.LD, .S, .{ Xa_relative(.S, .imm_0), .to, .BbU });
    //syntax("LD.S Xa -> UBb");
    desc("Load unsigned byte using pointer to stack memory");
    fromRegPointer(0);
    opLoad8(.OB, .stack, .zx, 0);
}
pub fn _D900_D9FF() void {
    encodingWithSuffix(.LD, .S, .{ Xa_relative(.S, .imm_0), .to, .BbS });
    //syntax("LD.S Xa -> SBb");
    desc("Load signed byte using pointer to stack memory");
    fromRegPointer(0);
    opLoad8(.OB, .stack, .sx, 0);
}
pub fn _DA00_DAFF() void {
    encodingWithSuffix(.LD, .S, .{ Xa_relative(.S, .imm_0), .to, .Rb });
    //syntax("LD.S Xa -> Rb");
    desc("Load 16b register using pointer to stack memory");
    fromRegPointer(0);
    opLoad16(.OB, .stack, 0);
}
pub fn _DB00_DBFF() void {
    encodingWithSuffix(.LD, .S, .{ Xa_relative(.S, .imm_0), .to, .Xb });
    //syntax("LD.S Xa -> Xb");
    desc("Load 32b register using pointer to stack memory");
    fromRegPointer(0);
    opLoad32(.OB, .stack, 0);
}

pub fn _1000_10FF() void {
    encodingWithSuffix(.ST, .S, .{ .Bb, .to, Xa_relative(.S, .imm_0) });
    //syntax("ST.S Bb -> Xa");
    desc("Store byte using pointer to stack memory");
    fromRegPointer(0);
    opStore8(.OB, .stack, 0);
}
pub fn _1100_11FF() void {
    encodingWithSuffix(.ST, .S, .{ .Rb, .to, Xa_relative(.S, .imm_0) });
    //syntax("ST.S Rb -> Xa");
    desc("Store 16b register using pointer to stack memory");
    fromRegPointer(0);
    opStore16(.OB, .stack, 0);
}
pub fn _1200_12FF() void {
    encodingWithSuffix(.ST, .S, .{ .Xb, .to, Xa_relative(.S, .imm_0) });
    //syntax("ST.S Xb -> Xa");
    desc("Store 32b register using pointer to stack memory");
    fromRegPointer(0);
    opStore32(.OB, .stack, 0);
}

// plain data pointer load/store
pub fn _E800_E8FF() void {
    encodingWithSuffix(.LD, .D, .{ Xa_relative(.D, .imm_0), .to, .BbU });
    //syntax("LD.D Xa -> UBb");
    desc("Load unsigned byte using pointer to data memory");
    fromRegPointer(0);
    opLoad8(.OB, .data, .zx, 0);
}
pub fn _E900_E9FF() void {
    encodingWithSuffix(.LD, .D, .{ Xa_relative(.D, .imm_0), .to, .BbS });
    //syntax("LD.D Xa -> SBb");
    desc("Load signed byte using pointer to data memory");
    fromRegPointer(0);
    opLoad8(.OB, .data, .sx, 0);
}
pub fn _EA00_EAFF() void {
    encodingWithSuffix(.LD, .D, .{ Xa_relative(.D, .imm_0), .to, .Rb });
    //syntax("LD.D Xa -> Rb");
    desc("Load 16b register using pointer to data memory");
    fromRegPointer(0);
    opLoad16(.OB, .data, 0);
}
pub fn _EB00_EBFF() void {
    encodingWithSuffix(.LD, .D, .{ Xa_relative(.D, .imm_0), .to, .Xb });
    //syntax("LD.D Xa -> Xb");
    desc("Load 32b register using pointer to data memory");
    fromRegPointer(0);
    opLoad32(.OB, .data, 0);
}

pub fn _1300_13FF() void {
    encodingWithSuffix(.ST, .D, .{ .Bb, .to, Xa_relative(.D, .imm_0) });
    //syntax("ST.D Bb -> Xa");
    desc("Store byte using pointer to data memory");
    fromRegPointer(0);
    opStore8(.OB, .data, 0);
}
pub fn _1400_14FF() void {
    encodingWithSuffix(.ST, .D, .{ .Rb, .to, Xa_relative(.D, .imm_0) });
    //syntax("ST.D Rb -> Xa");
    desc("Store 16b register using pointer to data memory");
    fromRegPointer(0);
    opStore16(.OB, .data, 0);
}
pub fn _1500_15FF() void {
    encodingWithSuffix(.ST, .D, .{ .Xb, .to, Xa_relative(.D, .imm_0) });
    //syntax("ST.D Xb -> Xa");
    desc("Store 32b register using pointer to data memory");
    fromRegPointer(0);
    opStore32(.OB, .data, 0);
}

// data pointer+reg load/store
pub fn _EC00_ECFF() void {
    encodingWithSuffix(.LD, .D, .{ Xa_relative(.D, .RbU), .to, .B0U });
    //syntax("LD.D Xa+URb -> UB0");
    desc("Load unsigned byte using pointer to data memory with 16b register offset");
    fromRegPointerPlusRegOffset();
    load8(0, .data, .zx, 0);
}
pub fn _ED00_EDFF() void {
    encodingWithSuffix(.LD, .D, .{ Xa_relative(.D, .RbU), .to, .B0S });
    //syntax("LD.D Xa+URb -> SB0");
    desc("Load signed byte using pointer to data memory with 16b register offset");
    fromRegPointerPlusRegOffset();
    load8(0, .data, .sx, 0);
}
pub fn _EE00_EEFF() void {
    encodingWithSuffix(.LD, .D, .{ Xa_relative(.D, .RbU), .to, .R0 });
    //syntax("LD.D Xa+URb -> R0");
    desc("Load 16b register using pointer to data memory with 16b register offset");
    fromRegPointerPlusRegOffset();
    load16(0, .data, 0);
}
pub fn _EF00_EFFF() void {
    encodingWithSuffix(.LD, .D, .{ Xa_relative(.D, .RbU), .to, .X0 });
    //syntax("LD.D Xa+URb -> X0");
    desc("Load 32b register using pointer to data memory with 16b register offset");
    fromRegPointerPlusRegOffset();
    load32(0, .data, 0);
}

pub fn _1600_16FF() void {
    encodingWithSuffix(.ST, .D, .{ .B0, .to, Xa_relative(.D, .RbU) });
    //syntax("ST.D B0 -> Xa+URb");
    desc("Store byte using pointer to data memory with 16b register offset");
    fromRegPointerPlusRegOffset();
    store8(0, .data, 0);
}
pub fn _1700_17FF() void {
    encodingWithSuffix(.ST, .D, .{ .R0, .to, Xa_relative(.D, .RbU) });
    //syntax("ST.D R0 -> Xa+URb");
    desc("Store 16b register using pointer to data memory with 16b register offset");
    fromRegPointerPlusRegOffset();
    store16(0, .data, 0);
}
pub fn _1800_18FF() void {
    encodingWithSuffix(.ST, .D, .{ .X0, .to, Xa_relative(.D, .RbU) });
    //syntax("ST.D X0 -> Xa+URb");
    desc("Store 32b register using pointer to data memory with 16b register offset");
    fromRegPointerPlusRegOffset();
    store32(0, .data, 0);
}

// data pointer+imm load/store
pub fn _DC00_DCFF() void {
    encodingWithSuffix(.LD, .D, .{ Xa_relative(.D, .immb4u), .to, .B0U });
    //syntax("LD.D Xa+immb[0,15] -> UB0");
    desc("Load unsigned byte using pointer to data memory with immediate offset");
    fromRegPointerPlusImmOffset(OB());
    load8(0, .data, .zx, 0);
}
pub fn _DD00_DDFF() void {
    encodingWithSuffix(.LD, .D, .{ Xa_relative(.D, .immb4u), .to, .B0S });
    //syntax("LD.D Xa+immb[0,15] -> SB0");
    desc("Load signed byte using pointer to data memory with immediate offset");
    fromRegPointerPlusImmOffset(OB());
    load8(0, .data, .sx, 0);
}
pub fn _DE00_DEFF() void {
    encodingWithSuffix(.LD, .D, .{ Xa_relative(.D, .@"immb4u/2"), .to, .R0 });
    //syntax("LD.D Xa+2*immb[0,15] -> R0");
    desc("Load 16b register using pointer to data memory with immediate offset (align 2)");
    fromRegPointerPlusImmOffset(@as(SignedOffsetForLiteral, OB()) * 2);
    load16(0, .data, 0);
}
pub fn _DF00_DFFF() void {
    encodingWithSuffix(.LD, .D, .{ Xa_relative(.D, .@"immb4u/4"), .to, .X0 });
    //syntax("LD.D Xa+4*immb[0,15] -> X0");
    desc("Load 32b register using pointer to data memory with immediate offset (align 4)");
    fromRegPointerPlusImmOffset(@as(SignedOffsetForLiteral, OB()) * 4);
    load32(0, .data, 0);
}

pub fn _1900_19FF() void {
    encodingWithSuffix(.ST, .D, .{ .B0, .to, Xa_relative(.D, .immb4u) });
    //syntax("ST.D B0 -> Xa+immb[0,15]");
    desc("Store byte using pointer to data memory with immediate offset");
    fromRegPointerPlusImmOffset(OB());
    store8(0, .data, 0);
}
pub fn _1A00_1AFF() void {
    encodingWithSuffix(.ST, .D, .{ .R0, .to, Xa_relative(.D, .@"immb4u/2") });
    //syntax("ST.D R0 -> Xa+2*immb[0,15]");
    desc("Store 16b register using pointer to data memory with immediate offset (align 2)");
    fromRegPointerPlusImmOffset(@as(SignedOffsetForLiteral, OB()) * 2);
    store16(0, .data, 0);
}
pub fn _1B00_1BFF() void {
    encodingWithSuffix(.ST, .D, .{ .X0, .to, Xa_relative(.D, .@"immb4u/4") });
    //syntax("ST.D X0 -> Xa+4*immb[0,15]");
    desc("Store 32b register using pointer to data memory with immediate offset (align 4)");
    fromRegPointerPlusImmOffset(@as(SignedOffsetForLiteral, OB()) * 4);
    store32(0, .data, 0);
}

// SP+imm load/store
pub fn _CC00_CCFF() void {
    encodingWithSuffix(.LD, .S, .{ SP_relative(.immb4u), .to, .BaU });
    //syntax("LD.S SP+immb[0,15] -> UBa");
    desc("Load unsigned byte from stack with immediate offset");
    fromSPPlusImmOffset(OB());
    opLoad8(.OA, .stack, .zx, 0);
}
pub fn _CD00_CDFF() void {
    encodingWithSuffix(.LD, .S, .{ SP_relative(.immb4u), .to, .BaS });
    //syntax("LD.S SP+immb[0,15] -> SBa");
    desc("Load signed byte from stack with immediate offset");
    fromSPPlusImmOffset(OB());
    opLoad8(.OA, .stack, .sx, 0);
}
pub fn _CE00_CEFF() void {
    encodingWithSuffix(.LD, .S, .{ SP_relative(.@"immb4u/2"), .to, .Ra });
    //syntax("LD.S SP+2*immb[0,15] -> Ra");
    desc("Load 16b register from stack with immediate offset (align 2)");
    fromSPPlusImmOffset(@as(SignedOffsetForLiteral, OB()) * 2);
    opLoad16(.OA, .stack, 0);
}
pub fn _CF00_CFFF() void {
    encodingWithSuffix(.LD, .S, .{ SP_relative(.@"immb4u/4"), .to, .Xa });
    //syntax("LD.S SP+4*immb[0,15] -> Xa");
    desc("Load 32b register from stack with immediate offset (align 4)");
    fromSPPlusImmOffset(@as(SignedOffsetForLiteral, OB()) * 4);
    opLoad32(.OA, .stack, 0);
}

pub fn _1C00_1CFF() void {
    encodingWithSuffix(.ST, .S, .{ .Ba, .to, SP_relative(.immb4u) });
    //syntax("ST.S Ba -> SP+immb[0,15]");
    desc("Store byte to stack with immediate offset");
    fromSPPlusImmOffset(OB());
    opStore8(.OA, .stack, 0);
}
pub fn _1D00_1DFF() void {
    encodingWithSuffix(.ST, .S, .{ .Ra, .to, SP_relative(.@"immb4u/2") });
    //syntax("ST.S Ra -> SP+2*immb[0,15]");
    desc("Store 16b register to stack with immediate offset (align 2)");
    fromSPPlusImmOffset(@as(SignedOffsetForLiteral, OB()) * 2);
    opStore16(.OA, .stack, 0);
}
pub fn _1E00_1EFF() void {
    encodingWithSuffix(.ST, .S, .{ .Xa, .to, SP_relative(.@"immb4u/4") });
    //syntax("ST.S Xa -> SP+4*immb[0,15]");
    desc("Store 32b register to stack with immediate offset (align 4)");
    fromSPPlusImmOffset(@as(SignedOffsetForLiteral, OB()) * 4);
    opStore32(.OA, .stack, 0);
}

// SP+imm16 load/store
pub fn _C000_C00F() void {
    encodingWithSuffix(.LD, .S, .{ SP_relative(.imm16s), .to, .BaU });
    //syntax("LD.S SP+imm16[-32768,32767] -> UBa");
    desc("Load unsigned byte from stack with immediate offset");
    fromSPPlusImm16Offset();
    opLoad8(.OA, .stack, .zx, 0);
}
pub fn _C010_C01F() void {
    encodingWithSuffix(.LD, .S, .{ SP_relative(.imm16s), .to, .BaS });
    //syntax("LD.S SP+imm16[-32768,32767] -> SBa");
    desc("Load signed byte from stack with immediate offset");
    fromSPPlusImm16Offset();
    opLoad8(.OA, .stack, .sx, 0);
}
pub fn _C020_C02F() void {
    encodingWithSuffix(.LD, .S, .{ SP_relative(.imm16s), .to, .Ra });
    //syntax("LD.S SP+imm16[-32768,32767] -> Ra");
    desc("Load 16b register from stack with immediate offset");
    fromSPPlusImm16Offset();
    opLoad16(.OA, .stack, 0);
}
pub fn _C030_C03F() void {
    encodingWithSuffix(.LD, .S, .{ SP_relative(.imm16s), .to, .Xa });
    //syntax("LD.S SP+imm16[-32768,32767] -> Xa");
    desc("Load 32b register from stack with immediate offset");
    fromSPPlusImm16Offset();
    opLoad32(.OA, .stack, 0);
}

pub fn _C040_C04F() void {
    encodingWithSuffix(.ST, .S, .{ .Ba, .to, SP_relative(.imm16s) });
    //syntax("ST.S Ba -> SP+imm16[-32768,32767]");
    desc("Store byte to stack with immediate offset");
    fromSPPlusImm16Offset();
    opStore8(.OA, .stack, 0);
}
pub fn _C050_C05F() void {
    encodingWithSuffix(.ST, .S, .{ .Ra, .to, SP_relative(.imm16s) });
    //syntax("ST.S Ra -> SP+imm16[-32768,32767]");
    desc("Store 16b register to stack with immediate offset");
    fromSPPlusImm16Offset();
    opStore16(.OA, .stack, 0);
}
pub fn _C060_C06F() void {
    encodingWithSuffix(.ST, .S, .{ .Xa, .to, SP_relative(.imm16s) });
    //syntax("ST.S Xa -> SP+imm16[-32768,32767]");
    desc("Store 32b register to stack with immediate offset");
    fromSPPlusImm16Offset();
    opStore32(.OA, .stack, 0);
}

// SP+reg load/store
pub fn _BC00_BCFF() void {
    encodingWithSuffix(.LD, .S, .{ SP_relative(.RaS), .to, .BbU });
    //syntax("LD.S SP+SRa -> UBb");
    desc("Load unsigned byte from stack with 16b register offset");
    fromSPPlusRegOffset();
    opLoad8(.OB, .stack, .zx, 0);
}
pub fn _BD00_BDFF() void {
    encodingWithSuffix(.LD, .S, .{ SP_relative(.RaS), .to, .BbS });
    //syntax("LD.S SP+SRa -> SBb");
    desc("Load signed byte from stack with 16b register offset");
    fromSPPlusRegOffset();
    opLoad8(.OB, .stack, .sx, 0);
}
pub fn _BE00_BEFF() void {
    encodingWithSuffix(.LD, .S, .{ SP_relative(.RaS), .to, .Rb });
    //syntax("LD.S SP+SRa -> Rb");
    desc("Load 16b register from stack with 16b register offset");
    fromSPPlusRegOffset();
    opLoad16(.OB, .stack, 0);
}
pub fn _BF00_BFFF() void {
    encodingWithSuffix(.LD, .S, .{ SP_relative(.RaS), .to, .Xb });
    //syntax("LD.S SP+SRa -> Xb");
    desc("Load 32b register from stack with 16b register offset");
    fromSPPlusRegOffset();
    opLoad32(.OB, .stack, 0);
}

pub fn _2000_20FF() void {
    encodingWithSuffix(.ST, .S, .{ .Bb, .to, SP_relative(.RaS) });
    //syntax("ST.S Bb -> SP+SRa");
    desc("Store byte to stack with 16b register offset");
    fromSPPlusRegOffset();
    opStore8(.OB, .stack, 0);
}
pub fn _2100_21FF() void {
    encodingWithSuffix(.ST, .S, .{ .Rb, .to, SP_relative(.RaS) });
    //syntax("ST.S Rb -> SP+SRa");
    desc("Store 16b register to stack with 16b register offset");
    fromSPPlusRegOffset();
    opStore16(.OB, .stack, 0);
}
pub fn _2200_22FF() void {
    encodingWithSuffix(.ST, .S, .{ .Xb, .to, SP_relative(.RaS) });
    //syntax("ST.S Xb -> SP+SRa");
    desc("Store 32b register to stack with 16b register offset");
    fromSPPlusRegOffset();
    opStore32(.OB, .stack, 0);
}

// postincrement load/store
pub fn _9800_98FF() void {
    encodingWithSuffix(.LDI, .D, .{ Xa_relative(.D, .imm_0), .to, .BbU });
    //syntax("LDI.D Xa -> UBb");
    desc("Load unsigned byte using pointer to data memory; postincrement");
    incRegPointer(1);
    opLoad8(.OB, .data, .zx, -1);
}
pub fn _9900_99FF() void {
    encodingWithSuffix(.LDI, .D, .{ Xa_relative(.D, .imm_0), .to, .BbS });
    //syntax("LDI.D Xa -> SBb");
    desc("Load signed byte using pointer to data memory; postincrement");
    incRegPointer(1);
    opLoad8(.OB, .data, .sx, -1);
}
pub fn _9A00_9AFF() void {
    encodingWithSuffix(.LDI, .D, .{ Xa_relative(.D, .imm_0), .to, .Rb });
    //syntax("LDI.D Xa -> Rb");
    desc("Load 16b register using pointer to data memory; postincrement");
    incRegPointer(2);
    opLoad16(.OB, .data, -2);
}
pub fn _9B00_9BFF() void {
    encodingWithSuffix(.LDI, .D, .{ Xa_relative(.D, .imm_0), .to, .Xb });
    //syntax("LDI.D Xa -> Xb");
    desc("Load 32b register using pointer to data memory; postincrement");
    incRegPointer(4);
    opLoad32(.OB, .data, -4);
}

pub fn _2300_23FF() void {
    encodingWithSuffix(.STI, .D, .{ .Bb, .to, Xa_relative(.D, .imm_0) });
    //syntax("STI.D Bb -> Xa");
    desc("Store byte using pointer to data memory; postincrement");
    incRegPointer(1);
    opStore8(.OB, .data, -1);
}
pub fn _2400_24FF() void {
    encodingWithSuffix(.STI, .D, .{ .Rb, .to, Xa_relative(.D, .imm_0) });
    //syntax("STI.D Rb -> Xa");
    desc("Store 16b register using pointer to data memory; postincrement");
    incRegPointer(2);
    opStore16(.OB, .data, -2);
}
pub fn _2500_25FF() void {
    encodingWithSuffix(.STI, .D, .{ .Xb, .to, Xa_relative(.D, .imm_0) });
    //syntax("STI.D Xb -> Xa");
    desc("Store 32b register using pointer to data memory; postincrement");
    incRegPointer(4);
    opStore32(.OB, .data, -4);
}

// preincrement load/store
pub fn _9C00_9CFF() void {
    encodingWithSuffix(.ILD, .D, .{ Xa_relative(.D, .imm_0), .to, .BbU });
    //syntax("ILD.D Xa -> UBb");
    desc("Load unsigned byte using pointer to data memory; preincrement");
    incRegPointer(1);
    opLoad8(.OB, .data, .zx, 0);
}
pub fn _9D00_9DFF() void {
    encodingWithSuffix(.ILD, .D, .{ Xa_relative(.D, .imm_0), .to, .BbS });
    //syntax("ILD.D Xa -> SBb");
    desc("Load signed byte using pointer to data memory; preincrement");
    incRegPointer(1);
    opLoad8(.OB, .data, .sx, 0);
}
pub fn _9E00_9EFF() void {
    encodingWithSuffix(.ILD, .D, .{ Xa_relative(.D, .imm_0), .to, .Rb });
    //syntax("ILD.D Xa -> Rb");
    desc("Load 16b register using pointer to data memory; preincrement");
    incRegPointer(2);
    opLoad16(.OB, .stack, 0);
}
pub fn _9F00_9FFF() void {
    encodingWithSuffix(.ILD, .D, .{ Xa_relative(.D, .imm_0), .to, .Xb });
    //syntax("ILD.D Xa -> Xb");
    desc("Load 32b register using pointer to data memory; preincrement");
    incRegPointer(4);
    opLoad32(.OB, .stack, 0);
}

pub fn _2600_26FF() void {
    encodingWithSuffix(.IST, .D, .{ .Bb, .to, Xa_relative(.D, .imm_0) });
    //syntax("IST.D Bb -> Xa");
    desc("Load signed byte using pointer to data memory; preincrement");
    incRegPointer(1);
    opStore8(.OB, .data, 0);
}
pub fn _2700_27FF() void {
    encodingWithSuffix(.IST, .D, .{ .Rb, .to, Xa_relative(.D, .imm_0) });
    //syntax("IST.D Rb -> Xa");
    desc("Load 16b register using pointer to data memory; preincrement");
    incRegPointer(2);
    opStore16(.OB, .stack, 0);
}
pub fn _2800_28FF() void {
    encodingWithSuffix(.IST, .D, .{ .Xb, .to, Xa_relative(.D, .imm_0) });
    //syntax("IST.D Xb -> Xa");
    desc("Load 32b register using pointer to data memory; preincrement");
    incRegPointer(4);
    opStore32(.OB, .stack, 0);
}

// user context load
pub fn _B800_B8FF() void {
    encodingWithSuffix(.LD, .D, .{ UXP_relative(.@"immb4u/2"), .to, .Ra });
    //syntax("LD.D UXP+2*immb[0,15] -> Ra");
    desc("Load 16b register from user context, with immediate offset (align 2)");

    SR_plus_literal_to_L(.uxp, @as(SignedOffsetForLiteral, OB()) * 2, .fresh, .no_flags);
    L_to_SR(.temp_1);
    load_next_insn(2);
    next_cycle();

    opLoad16(.OA, .data, 0);
}
pub fn _B900_B9FF() void {
    encodingWithSuffix(.LD, .D, .{ UXP_relative(.@"immb4u/4"), .to, .Xa });
    //syntax("LD.D UXP+4*immb[0,15] -> Xa");
    desc("Load 32b register from user context, with immediate offset (align 4)");

    SR_plus_literal_to_L(.uxp, @as(SignedOffsetForLiteral, OB()) * 4, .fresh, .no_flags);
    L_to_SR(.temp_1);
    load_next_insn(2);
    next_cycle();

    opLoad32(.OA, .data, 0);
}

// kernel context load
pub fn _BA00_BAFF() void {
    encodingWithSuffix(.LD, .D, .{ KXP_relative(.@"immb4u/2"), .to, .Ra });
    //syntax("LD.D KXP+2*immb[0,15] -> Ra");
    desc("Load 16b register from user context, with immediate offset (align 2)");

    if (!kernel()) {
        illegal_instruction();
        return;
    }

    SR_plus_literal_to_L(.uxp, @as(SignedOffsetForLiteral, OB()) * 2, .fresh, .no_flags);
    L_to_SR(.temp_1);
    load_next_insn(2);
    next_cycle();

    opLoad16(.OA, .data, 0);
}
pub fn _BB00_BBFF() void {
    encodingWithSuffix(.LD, .D, .{ KXP_relative(.@"immb4u/4"), .to, .Xa });
    //syntax("LD.D KXP+4*immb[0,15] -> Xa");
    desc("Load 32b register from user context, with immediate offset (align 4)");

    if (!kernel()) {
        illegal_instruction();
        return;
    }

    SR_plus_literal_to_L(.uxp, @as(SignedOffsetForLiteral, OB()) * 4, .fresh, .no_flags);
    L_to_SR(.temp_1);
    load_next_insn(2);
    next_cycle();

    opLoad32(.OA, .data, 0);
}

pub fn _0C60_0C6F() void {
    encodingWithSuffix(.MCR, .D, .{ .XaS, .BP, .to, .RP });
    desc("Copy a block of memory.  BP points to the first byte of the source block and RP points to the first byte of the destination block.  If blocks overlap, BP must be larger than RP.  If a block crosses a page boundary, it must be word aligned.  Takes approximately block_bytes + 2 cycles to complete.");

    op_reg32_plus_literal_to_L(.OA, -2, .fresh, .flags);
    conditional_next_cycle(0x207);
}
pub fn _continuation_207() void {
    if (negative()) {
        // length == 0 or 1 bytes
        op_reg32_plus_literal_to_L(.OA, -1, .fresh, .flags);
        conditional_next_cycle(0x208);
    } else if (zero()) {
        // length == 2 bytes
        read_to_D(.bp, 0, .word, .data);
        D_to_DL();
        op_reg32_plus_literal_to_L(.OA, -2, .fresh, .flags);
        L_to_op_reg32(.OA);
        next_cycle();

        write_from_DL(.rp, 0, .word, .data);
        next_cycle();

        load_and_exec_next_insn(2);
    } else {
        // length > 2 bytes
        read_to_D(.bp, 0, .word, .data);
        D_to_DL();
        op_reg32_plus_literal_to_L(.OA, -2, .fresh, .flags);
        L_to_op_reg32(.OA);
        next_cycle();

        write_from_DL(.rp, 0, .word, .data);
        op_reg32_plus_literal_to_L(.OA, -2, .fresh, .flags);
        conditional_next_cycle(0x209);
    }
}
pub fn _continuation_208() void {
    if (negative()) {
        // length == 0
        op_reg32_plus_literal_to_L(.OA, 0, .fresh, .flags);
        load_and_exec_next_insn(2);
    } else {
        // length == 1 byte
        read_to_D(.bp, 0, .byte, .data);
        D_to_DL();
        op_reg32_plus_literal_to_L(.OA, -1, .fresh, .flags);
        L_to_op_reg32(.OA);
        next_cycle();

        write_from_DL(.rp, 0, .byte, .data);
        next_cycle();

        load_and_exec_next_insn(2);
    }
}
pub fn _continuation_209() void {
    if (negative()) {
        // exactly 1 byte left
        read_to_D(.bp, 2, .byte, .data);
        D_to_DL();
        PN_to_SR(.bp);
        op_reg32_plus_literal_to_L(.OA, -1, .fresh, .flags);
        L_to_op_reg32(.OA);
        next_cycle();

        write_from_DL(.rp, 2, .byte, .data);
        PN_to_SR(.rp);
        next_cycle();

        load_and_exec_next_insn(2);
    } else if (zero()) {
        // exactly 2 bytes left
        read_to_D(.bp, 2, .word, .data);
        D_to_DL();
        PN_to_SR(.bp);
        op_reg32_plus_literal_to_L(.OA, -2, .fresh, .flags);
        L_to_op_reg32(.OA);
        next_cycle();

        write_from_DL(.rp, 2, .word, .data);
        PN_to_SR(.rp);
        next_cycle();

        load_and_exec_next_insn(2);
    } else {
        // > 2 bytes left
        read_to_D(.bp, 2, .word, .data);
        D_to_DL();
        PN_to_SR(.bp);
        op_reg32_plus_literal_to_L(.OA, -2, .fresh, .flags);
        L_to_op_reg32(.OA);
        next_cycle();

        write_from_DL(.rp, 2, .word, .data);
        op_reg32_plus_literal_to_L(.OA, -2, .fresh, .flags);
        allow_interrupt();
        conditional_next_cycle(0x209);
    }
}

pub fn _0C70_0C7F() void {
    encodingWithSuffix(.MCRB, .D, .{ .XaS, .BP, .to, .RP });
    desc("Copy a block of memory with byte transfers only.  BP points to the first byte of the source block and RP points to the first byte of the destination block.  If blocks overlap, BP must be larger than RP.  Takes approximately block_bytes * 2 + 2 cycles to complete, but is not susceptible to any of the alignment constraints of MCR.");

    op_reg32_plus_literal_to_L(.OA, -1, .fresh, .flags);
    conditional_next_cycle(0x20A);
}
pub fn _continuation_20A() void {
    if (negative()) {
        // length == 0
        op_reg32_plus_literal_to_L(.OA, 0, .fresh, .flags);
        load_and_exec_next_insn(2);
    } else if (zero()) {
        // length == 1 byte
        read_to_D(.bp, 0, .byte, .data);
        D_to_DL();
        op_reg32_plus_literal_to_L(.OA, -1, .fresh, .flags);
        L_to_op_reg32(.OA);
        next_cycle();

        write_from_DL(.rp, 0, .byte, .data);
        next_cycle();

        load_and_exec_next_insn(2);
    } else {
        // length > 1 byte
        read_to_D(.bp, 0, .byte, .data);
        D_to_DL();
        op_reg32_plus_literal_to_L(.OA, -1, .fresh, .flags);
        L_to_op_reg32(.OA);
        next_cycle();

        write_from_DL(.rp, 0, .byte, .data);
        op_reg32_plus_literal_to_L(.OA, -1, .fresh, .flags);
        conditional_next_cycle(0x20B);
    }
}
pub fn _continuation_20B() void {
    // main memcpy loop
    if (zero()) {
        // exactly 1 byte left
        read_to_D(.bp, 1, .byte, .data);
        D_to_DL();
        PN_to_SR(.bp);
        op_reg32_minus_literal_to_L(.OA, 1, .fresh, .flags);
        L_to_op_reg32(.OA);
        next_cycle();

        write_from_DL(.rp, 1, .byte, .data);
        PN_to_SR(.rp);
        next_cycle();

        load_and_exec_next_insn(2);
    } else {
        // > 1 byte left
        read_to_D(.bp, 1, .byte, .data);
        D_to_DL();
        PN_to_SR(.bp);
        op_reg32_minus_literal_to_L(.OA, 1, .fresh, .flags);
        L_to_op_reg32(.OA);
        next_cycle();

        write_from_DL(.rp, 1, .byte, .data);
        PN_to_SR(.rp);
        op_reg32_minus_literal_to_L(.OA, 1, .fresh, .flags);
        allow_interrupt();
        conditional_next_cycle(0x20B);
    }
}

pub fn _0CA0_0CAF() void {
    encodingWithSuffix(.MCF, .D, .{ .XaS, .BP, .to, .RP });
    desc("Copy a block of memory.  BP points to the byte following the source block and RP points to the byte following the destination block.  If blocks overlap, BP must be smaller than RP.  If a block crosses a page boundary, it must be word aligned.  Takes approximately block_bytes + 2 cycles to complete.");

    op_reg32_plus_literal_to_L(.OA, -2, .fresh, .flags);
    conditional_next_cycle(0x20C);
}
pub fn _continuation_20C() void {
    if (negative()) {
        // length == 0 or 1 bytes
        op_reg32_plus_literal_to_L(.OA, -1, .fresh, .flags);
        conditional_next_cycle(0x20D);
    } else if (zero()) {
        // length == 2 bytes
        read_to_D(.bp, -2, .word, .data);
        D_to_DL();
        PN_to_SR(.bp);
        op_reg32_plus_literal_to_L(.OA, -2, .fresh, .flags);
        L_to_op_reg32(.OA);
        next_cycle();

        write_from_DL(.rp, -2, .word, .data);
        PN_to_SR(.rp);
        next_cycle();

        load_and_exec_next_insn(2);
    } else {
        // length > 2 bytes
        read_to_D(.bp, -2, .word, .data);
        D_to_DL();
        PN_to_SR(.bp);
        op_reg32_plus_literal_to_L(.OA, -2, .fresh, .flags);
        L_to_op_reg32(.OA);
        next_cycle();

        write_from_DL(.rp, -2, .word, .data);
        PN_to_SR(.rp);
        op_reg32_plus_literal_to_L(.OA, -2, .fresh, .flags);
        conditional_next_cycle(0x20E);
    }
}
pub fn _continuation_20D() void {
    if (negative()) {
        // length == 0
        op_reg32_plus_literal_to_L(.OA, 0, .fresh, .flags);
        load_and_exec_next_insn(2);
    } else {
        // length == 1 byte
        read_to_D(.bp, -1, .byte, .data);
        D_to_DL();
        PN_to_SR(.bp);
        op_reg32_plus_literal_to_L(.OA, -1, .fresh, .flags);
        L_to_op_reg32(.OA);
        next_cycle();

        write_from_DL(.rp, -1, .byte, .data);
        PN_to_SR(.rp);
        next_cycle();

        load_and_exec_next_insn(2);
    }
}
pub fn _continuation_20E() void {
    if (negative()) {
        // exactly 1 byte left
        read_to_D(.bp, -1, .byte, .data);
        D_to_DL();
        PN_to_SR(.bp);
        op_reg32_plus_literal_to_L(.OA, -1, .fresh, .flags);
        L_to_op_reg32(.OA);
        next_cycle();

        write_from_DL(.rp, -1, .byte, .data);
        PN_to_SR(.rp);
        next_cycle();

        load_and_exec_next_insn(2);
    } else if (zero()) {
        // exactly 2 bytes left
        read_to_D(.bp, -2, .word, .data);
        D_to_DL();
        PN_to_SR(.bp);
        op_reg32_plus_literal_to_L(.OA, -2, .fresh, .flags);
        L_to_op_reg32(.OA);
        next_cycle();

        write_from_DL(.rp, -2, .word, .data);
        PN_to_SR(.rp);
        next_cycle();

        load_and_exec_next_insn(2);
    } else {
        // > 2 bytes left
        read_to_D(.bp, -2, .word, .data);
        D_to_DL();
        PN_to_SR(.bp);
        op_reg32_plus_literal_to_L(.OA, -2, .fresh, .flags);
        L_to_op_reg32(.OA);
        next_cycle();

        write_from_DL(.rp, -2, .word, .data);
        PN_to_SR(.rp);
        op_reg32_plus_literal_to_L(.OA, -2, .fresh, .flags);
        allow_interrupt();
        conditional_next_cycle(0x20E);
    }
}

pub fn _0CB0_0CBF() void {
    encodingWithSuffix(.MCFB, .D, .{ .XaS, .BP, .to, .RP });
    desc("Copy a block of memory with byte transfers only.  BP points to the byte following the source block and RP points to the byte following the destination block.  If blocks overlap, BP must be smaller than RP.  Takes approximately block_bytes * 2 + 2 cycles to complete, but is not susceptible to any of the alignment constraints of MCF.");

    op_reg32_plus_literal_to_L(.OA, -1, .fresh, .flags);
    conditional_next_cycle(0x20F);
}
pub fn _continuation_20F() void {
    if (negative()) {
        // length == 0
        op_reg32_plus_literal_to_L(.OA, 0, .fresh, .flags);
        load_and_exec_next_insn(2);
    } else if (zero()) {
        // length == 1 byte
        read_to_D(.bp, -1, .byte, .data);
        D_to_DL();
        PN_to_SR(.bp);
        op_reg32_plus_literal_to_L(.OA, -1, .fresh, .flags);
        L_to_op_reg32(.OA);
        next_cycle();

        write_from_DL(.rp, -1, .byte, .data);
        PN_to_SR(.rp);
        next_cycle();

        load_and_exec_next_insn(2);
    } else {
        // length > 1 byte
        read_to_D(.bp, -1, .byte, .data);
        D_to_DL();
        PN_to_SR(.bp);
        op_reg32_plus_literal_to_L(.OA, -1, .fresh, .flags);
        L_to_op_reg32(.OA);
        next_cycle();

        write_from_DL(.rp, -1, .byte, .data);
        PN_to_SR(.rp);
        op_reg32_plus_literal_to_L(.OA, -1, .fresh, .flags);
        allow_interrupt();
        conditional_next_cycle(0x20F);
    }
}

pub fn _0CC0_0CCF() void {
    encodingWithSuffix(.SI, .D, .{ .XaS, .BP, .to, .RP });
    desc("Stream from a fixed address to a block of memory.  BP points to the fixed address and RP points to the first byte of the destination block.  If the block crosses a page boundary, it must be word aligned.  Takes approximately block_bytes + 2 cycles to complete.");

    op_reg32_plus_literal_to_L(.OA, -2, .fresh, .flags);
    conditional_next_cycle(0x207);
}
pub fn _continuation_210() void {
    if (negative()) {
        // length == 0 or 1 bytes
        op_reg32_plus_literal_to_L(.OA, -1, .fresh, .flags);
        conditional_next_cycle(0x211);
    } else if (zero()) {
        // length == 2 bytes
        read_to_D(.bp, 0, .word, .data);
        D_to_DL();
        op_reg32_plus_literal_to_L(.OA, -2, .fresh, .flags);
        L_to_op_reg32(.OA);
        next_cycle();

        write_from_DL(.rp, 0, .word, .data);
        next_cycle();

        load_and_exec_next_insn(2);
    } else {
        // length > 2 bytes
        read_to_D(.bp, 0, .word, .data);
        D_to_DL();
        op_reg32_plus_literal_to_L(.OA, -2, .fresh, .flags);
        L_to_op_reg32(.OA);
        next_cycle();

        write_from_DL(.rp, 0, .word, .data);
        op_reg32_plus_literal_to_L(.OA, -2, .fresh, .flags);
        conditional_next_cycle(0x212);
    }
}
pub fn _continuation_211() void {
    if (negative()) {
        // length == 0
        op_reg32_plus_literal_to_L(.OA, 0, .fresh, .flags);
        load_and_exec_next_insn(2);
    } else {
        // length == 1 byte
        read_to_D(.bp, 0, .byte, .data);
        D_to_DL();
        op_reg32_plus_literal_to_L(.OA, -1, .fresh, .flags);
        L_to_op_reg32(.OA);
        next_cycle();

        write_from_DL(.rp, 0, .byte, .data);
        next_cycle();

        load_and_exec_next_insn(2);
    }
}
pub fn _continuation_212() void {
    if (negative()) {
        // exactly 1 byte left
        read_to_D(.bp, 0, .byte, .data);
        D_to_DL();
        op_reg32_plus_literal_to_L(.OA, -1, .fresh, .flags);
        L_to_op_reg32(.OA);
        next_cycle();

        write_from_DL(.rp, 2, .byte, .data);
        PN_to_SR(.rp);
        next_cycle();

        load_and_exec_next_insn(2);
    } else if (zero()) {
        // exactly 2 bytes left
        read_to_D(.bp, 0, .word, .data);
        D_to_DL();
        op_reg32_plus_literal_to_L(.OA, -2, .fresh, .flags);
        L_to_op_reg32(.OA);
        next_cycle();

        write_from_DL(.rp, 2, .word, .data);
        PN_to_SR(.rp);
        next_cycle();

        load_and_exec_next_insn(2);
    } else {
        // > 2 bytes left
        read_to_D(.bp, 0, .word, .data);
        D_to_DL();
        op_reg32_plus_literal_to_L(.OA, -2, .fresh, .flags);
        L_to_op_reg32(.OA);
        next_cycle();

        write_from_DL(.rp, 2, .word, .data);
        op_reg32_plus_literal_to_L(.OA, -2, .fresh, .flags);
        allow_interrupt();
        conditional_next_cycle(0x212);
    }
}

pub fn _0CD0_0CDF() void {
    encodingWithSuffix(.SIB, .D, .{ .XaS, .BP, .to, .RP });
    desc("Stream from a fixed address to a block of memory, with byte transfers only.  BP points to the fixed address and RP points to the first byte of the destination block.  Takes approximately block_bytes * 2 + 2 cycles to complete, but is not susceptible to any of the alignment constraints of SI.");

    op_reg32_plus_literal_to_L(.OA, -1, .fresh, .flags);
    conditional_next_cycle(0x213);
}
pub fn _continuation_213() void {
    if (negative()) {
        // length == 0
        op_reg32_plus_literal_to_L(.OA, 0, .fresh, .flags);
        load_and_exec_next_insn(2);
    } else if (zero()) {
        // length == 1 byte
        read_to_D(.bp, 0, .byte, .data);
        D_to_DL();
        op_reg32_plus_literal_to_L(.OA, -1, .fresh, .flags);
        L_to_op_reg32(.OA);
        next_cycle();

        write_from_DL(.rp, 0, .byte, .data);
        next_cycle();

        load_and_exec_next_insn(2);
    } else {
        // length > 1 byte
        read_to_D(.bp, 0, .byte, .data);
        D_to_DL();
        op_reg32_plus_literal_to_L(.OA, -1, .fresh, .flags);
        L_to_op_reg32(.OA);
        next_cycle();

        write_from_DL(.rp, 0, .byte, .data);
        op_reg32_plus_literal_to_L(.OA, -1, .fresh, .flags);
        conditional_next_cycle(0x214);
    }
}
pub fn _continuation_214() void {
    // main memcpy loop
    if (zero()) {
        // exactly 1 byte left
        read_to_D(.bp, 0, .byte, .data);
        D_to_DL();
        op_reg32_minus_literal_to_L(.OA, 1, .fresh, .flags);
        L_to_op_reg32(.OA);
        next_cycle();

        write_from_DL(.rp, 1, .byte, .data);
        PN_to_SR(.rp);
        next_cycle();

        load_and_exec_next_insn(2);
    } else {
        // > 1 byte left
        read_to_D(.bp, 0, .byte, .data);
        D_to_DL();
        op_reg32_minus_literal_to_L(.OA, 1, .fresh, .flags);
        L_to_op_reg32(.OA);
        next_cycle();

        write_from_DL(.rp, 1, .byte, .data);
        PN_to_SR(.rp);
        op_reg32_minus_literal_to_L(.OA, 1, .fresh, .flags);
        allow_interrupt();
        conditional_next_cycle(0x214);
    }
}

pub fn _0CE0_0CEF() void {
    encodingWithSuffix(.SO, .D, .{ .XaS, .BP, .to, .RP });
    desc("Stream a block of memory to a fixed address.  BP points to the first byte of the source block and RP points to the fixed destination address.  If the block crosses a page boundary, it must be word aligned.  Takes approximately block_bytes + 2 cycles to complete.");

    op_reg32_plus_literal_to_L(.OA, -2, .fresh, .flags);
    conditional_next_cycle(0x215);
}
pub fn _continuation_215() void {
    if (negative()) {
        // length == 0 or 1 bytes
        op_reg32_plus_literal_to_L(.OA, -1, .fresh, .flags);
        conditional_next_cycle(0x216);
    } else if (zero()) {
        // length == 2 bytes
        read_to_D(.bp, 0, .word, .data);
        D_to_DL();
        op_reg32_plus_literal_to_L(.OA, -2, .fresh, .flags);
        L_to_op_reg32(.OA);
        next_cycle();

        write_from_DL(.rp, 0, .word, .data);
        next_cycle();

        load_and_exec_next_insn(2);
    } else {
        // length > 2 bytes
        read_to_D(.bp, 0, .word, .data);
        D_to_DL();
        op_reg32_plus_literal_to_L(.OA, -2, .fresh, .flags);
        L_to_op_reg32(.OA);
        next_cycle();

        write_from_DL(.rp, 0, .word, .data);
        op_reg32_plus_literal_to_L(.OA, -2, .fresh, .flags);
        conditional_next_cycle(0x217);
    }
}
pub fn _continuation_216() void {
    if (negative()) {
        // length == 0
        op_reg32_plus_literal_to_L(.OA, 0, .fresh, .flags);
        load_and_exec_next_insn(2);
    } else {
        // length == 1 byte
        read_to_D(.bp, 0, .byte, .data);
        D_to_DL();
        op_reg32_plus_literal_to_L(.OA, -1, .fresh, .flags);
        L_to_op_reg32(.OA);
        next_cycle();

        write_from_DL(.rp, 0, .byte, .data);
        next_cycle();

        load_and_exec_next_insn(2);
    }
}
pub fn _continuation_217() void {
    if (negative()) {
        // exactly 1 byte left
        read_to_D(.bp, 2, .byte, .data);
        D_to_DL();
        PN_to_SR(.bp);
        op_reg32_plus_literal_to_L(.OA, -1, .fresh, .flags);
        L_to_op_reg32(.OA);
        next_cycle();

        write_from_DL(.rp, 0, .byte, .data);
        next_cycle();

        load_and_exec_next_insn(2);
    } else if (zero()) {
        // exactly 2 bytes left
        read_to_D(.bp, 2, .word, .data);
        D_to_DL();
        PN_to_SR(.bp);
        op_reg32_plus_literal_to_L(.OA, -2, .fresh, .flags);
        L_to_op_reg32(.OA);
        next_cycle();

        write_from_DL(.rp, 0, .word, .data);
        next_cycle();

        load_and_exec_next_insn(2);
    } else {
        // > 2 bytes left
        read_to_D(.bp, 2, .word, .data);
        D_to_DL();
        PN_to_SR(.bp);
        op_reg32_plus_literal_to_L(.OA, -2, .fresh, .flags);
        L_to_op_reg32(.OA);
        next_cycle();

        write_from_DL(.rp, 0, .word, .data);
        op_reg32_plus_literal_to_L(.OA, -2, .fresh, .flags);
        allow_interrupt();
        conditional_next_cycle(0x217);
    }
}

pub fn _0CF0_0CFF() void {
    encodingWithSuffix(.SOB, .D, .{ .XaS, .BP, .to, .RP });
    desc("Stream a block of memory to a fixed address, with byte transfers only.  BP points to the first byte of the source block and RP points to the fixed destination address.  Takes approximately block_bytes * 2 + 2 cycles to complete, but is not susceptible to any of the alignment constraints of SO.");

    op_reg32_plus_literal_to_L(.OA, -1, .fresh, .flags);
    conditional_next_cycle(0x218);
}
pub fn _continuation_218() void {
    if (negative()) {
        // length == 0
        op_reg32_plus_literal_to_L(.OA, 0, .fresh, .flags);
        load_and_exec_next_insn(2);
    } else if (zero()) {
        // length == 1 byte
        read_to_D(.bp, 0, .byte, .data);
        D_to_DL();
        op_reg32_plus_literal_to_L(.OA, -1, .fresh, .flags);
        L_to_op_reg32(.OA);
        next_cycle();

        write_from_DL(.rp, 0, .byte, .data);
        next_cycle();

        load_and_exec_next_insn(2);
    } else {
        // length > 1 byte
        read_to_D(.bp, 0, .byte, .data);
        D_to_DL();
        op_reg32_plus_literal_to_L(.OA, -1, .fresh, .flags);
        L_to_op_reg32(.OA);
        next_cycle();

        write_from_DL(.rp, 0, .byte, .data);
        op_reg32_plus_literal_to_L(.OA, -1, .fresh, .flags);
        conditional_next_cycle(0x219);
    }
}
pub fn _continuation_219() void {
    // main memcpy loop
    if (zero()) {
        // exactly 1 byte left
        read_to_D(.bp, 1, .byte, .data);
        D_to_DL();
        PN_to_SR(.bp);
        op_reg32_minus_literal_to_L(.OA, 1, .fresh, .flags);
        L_to_op_reg32(.OA);
        next_cycle();

        write_from_DL(.rp, 0, .byte, .data);
        next_cycle();

        load_and_exec_next_insn(2);
    } else {
        // > 1 byte left
        read_to_D(.bp, 1, .byte, .data);
        D_to_DL();
        PN_to_SR(.bp);
        op_reg32_minus_literal_to_L(.OA, 1, .fresh, .flags);
        L_to_op_reg32(.OA);
        next_cycle();

        write_from_DL(.rp, 0, .byte, .data);
        op_reg32_minus_literal_to_L(.OA, 1, .fresh, .flags);
        allow_interrupt();
        conditional_next_cycle(0x219);
    }
}
