const ib = @import("instruction_builder.zig");
const cb = @import("cycle_builder.zig");
const ControlSignals = @import("ControlSignals");
const misc = @import("misc");

const SignedOffsetForLiteral = misc.SignedOffsetForLiteral;

const encoding = ib.encoding;
const addr = ib.addr;
const IP_relative = ib.IP_relative;
const SP_relative = ib.SP_relative;
const UXP_relative = ib.UXP_relative;
const KXP_relative = ib.KXP_relative;
const Xa_relative = ib.Xa_relative;
const desc = ib.desc;
const next_cycle = ib.next_cycle;
const next_cycle_conditional = ib.next_cycle_conditional;
const next_cycle_explicit = ib.next_cycle_explicit;
const opcode_high = ib.opcode_high;
const OB = ib.OB;
const kernel = ib.kernel;
const negative = ib.negative;
const positive = ib.positive;
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
const D_to_LL = cb.D_to_LL;
const D8_to_LL = cb.D8_to_LL;
const D_to_DL = cb.D_to_DL;
const load_next_insn = cb.load_next_insn;
const exec_next_insn_no_atomic_end = cb.exec_next_insn_no_atomic_end;
const load_and_exec_next_insn = cb.load_and_exec_next_insn;
const load_and_exec_next_insn_no_atomic_end = cb.load_and_exec_next_insn_no_atomic_end;
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
const SR_to_PN = cb.SR_to_PN;
const allow_interrupt = cb.allow_interrupt;
const block_transfer_to_ram = cb.block_transfer_to_ram;
const block_transfer_from_ram = cb.block_transfer_from_ram;
const ZN_from_L = cb.ZN_from_L;

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
    SR_plus_op_reg_to_L(.sp, .OB, .sx, .fresh, .no_flags);
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

fn opLoad8(reg: cb.OperandSelection, bus_mode: ControlSignals.BusMode, ext: cb.ZeroOrSignExtension, offset: SignedOffsetForLiteral) void {
    read_to_D(.temp_1, offset, .byte, bus_mode);
    D8_to_LL(ext);
    LL_to_op_reg(switch (reg) {
        .OA => .OA,
        .OB => .OB,
    });
    exec_next_insn_no_atomic_end();
}
fn opLoad16(reg: cb.OperandSelection, bus_mode: ControlSignals.BusMode, offset: SignedOffsetForLiteral) void {
    read_to_D(.temp_1, offset, .word, bus_mode);
    D_to_LL();
    LL_to_op_reg(switch (reg) {
        .OA => .OA,
        .OB => .OB,
    });
    exec_next_insn_no_atomic_end();
}
fn opLoad32(reg: cb.OperandSelection, bus_mode: ControlSignals.BusMode, offset: SignedOffsetForLiteral) void {
    read_to_D(.temp_1, offset, .word, bus_mode);
    D_to_LL();
    LL_to_op_reg(switch (reg) {
        .OA => .OA,
        .OB => .OB,
    });
    next_cycle();

    read_to_D(.temp_1, offset + 2, .word, bus_mode);
    D_to_LL();
    LL_to_op_reg(switch (reg) {
        .OA => .OAxor1,
        .OB => .OBxor1,
    });
    exec_next_insn_no_atomic_end();
}

fn load8(reg: misc.RegisterIndex, bus_mode: ControlSignals.BusMode, ext: cb.ZeroOrSignExtension, offset: SignedOffsetForLiteral) void {
    read_to_D(.temp_1, offset, .byte, bus_mode);
    D8_to_LL(ext);
    LL_to_reg(reg);
    exec_next_insn_no_atomic_end();
}
fn load16(reg: misc.RegisterIndex, bus_mode: ControlSignals.BusMode, offset: SignedOffsetForLiteral) void {
    read_to_D(.temp_1, offset, .word, bus_mode);
    D_to_LL();
    LL_to_reg(reg);
    exec_next_insn_no_atomic_end();
}
fn load32(reg: misc.RegisterIndex, bus_mode: ControlSignals.BusMode, offset: SignedOffsetForLiteral) void {
    read_to_D(.temp_1, offset, .word, bus_mode);
    D_to_LL();
    LL_to_reg(reg);
    next_cycle();

    read_to_D(.temp_1, offset + 2, .word, bus_mode);
    D_to_LL();
    LL_to_reg(reg ^ 1);
    exec_next_insn_no_atomic_end();
}

fn opStore8(reg: cb.OperandSelection, bus_mode: ControlSignals.BusMode, offset: SignedOffsetForLiteral) void {
    op_reg_to_LL(switch (reg) {
        .OA => .OA,
        .OB => .OB,
    });
    write_from_LL(.temp_1, offset, .byte, bus_mode);
    exec_next_insn_no_atomic_end();
}
fn opStore16(reg: cb.OperandSelection, bus_mode: ControlSignals.BusMode, offset: SignedOffsetForLiteral) void {
    op_reg_to_LL(switch (reg) {
        .OA => .OA,
        .OB => .OB,
    });
    write_from_LL(.temp_1, offset, .word, bus_mode);
    exec_next_insn_no_atomic_end();
}
fn opStore32(reg: cb.OperandSelection, bus_mode: ControlSignals.BusMode, offset: SignedOffsetForLiteral) void {
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
    encoding(.LD, .{ addr(.insn, .Xa), .to, .BbU });
    desc("Load unsigned byte using pointer to instruction memory");
    fromRegPointer(0);
    opLoad8(.OB, .insn, .zx, 0);
}
pub fn _C900_C9FF() void {
    encoding(.LD, .{ addr(.insn, .Xa), .to, .BbS });
    desc("Load signed byte using pointer to instruction memory");
    fromRegPointer(0);
    opLoad8(.OB, .insn, .sx, 0);
}
pub fn _CA00_CAFF() void {
    encoding(.LD, .{ addr(.insn, .Xa), .to, .Rb });
    desc("Load 16b register using pointer to instruction memory");
    fromRegPointer(0);
    opLoad16(.OB, .insn, 0);
}
pub fn _CB00_CBFF() void {
    encoding(.LD, .{ addr(.insn, .Xa), .to, .Xb });
    desc("Load 32b register using pointer to instruction memory");
    fromRegPointer(0);
    opLoad32(.OB, .insn, 0);
}

// plain stack pointer load/store
pub fn _D800_D8FF() void {
    encoding(.LD, .{ addr(.stack, .Xa), .to, .BbU });
    desc("Load unsigned byte using pointer to stack memory");
    fromRegPointer(0);
    opLoad8(.OB, .stack, .zx, 0);
}
pub fn _D900_D9FF() void {
    encoding(.LD, .{ addr(.stack, .Xa), .to, .BbS });
    desc("Load signed byte using pointer to stack memory");
    fromRegPointer(0);
    opLoad8(.OB, .stack, .sx, 0);
}
pub fn _DA00_DAFF() void {
    encoding(.LD, .{ addr(.stack, .Xa), .to, .Rb });
    desc("Load 16b register using pointer to stack memory");
    fromRegPointer(0);
    opLoad16(.OB, .stack, 0);
}
pub fn _DB00_DBFF() void {
    encoding(.LD, .{ addr(.stack, .Xa), .to, .Xb });
    desc("Load 32b register using pointer to stack memory");
    fromRegPointer(0);
    opLoad32(.OB, .stack, 0);
}

pub fn _1000_10FF() void {
    encoding(.ST, .{ .Bb, .to, addr(.stack, .Xa) });
    desc("Store byte using pointer to stack memory");
    fromRegPointer(0);
    opStore8(.OB, .stack, 0);
}
pub fn _1100_11FF() void {
    encoding(.ST, .{ .Rb, .to, addr(.stack, .Xa) });
    desc("Store 16b register using pointer to stack memory");
    fromRegPointer(0);
    opStore16(.OB, .stack, 0);
}
pub fn _1200_12FF() void {
    encoding(.ST, .{ .Xb, .to, addr(.stack, .Xa) });
    desc("Store 32b register using pointer to stack memory");
    fromRegPointer(0);
    opStore32(.OB, .stack, 0);
}

// plain data pointer load/store
pub fn _E800_E8FF() void {
    encoding(.LD, .{ addr(.data, .Xa), .to, .BbU });
    desc("Load unsigned byte using pointer to data memory");
    fromRegPointer(0);
    opLoad8(.OB, .data, .zx, 0);
}
pub fn _E900_E9FF() void {
    encoding(.LD, .{ addr(.data, .Xa), .to, .BbS });
    desc("Load signed byte using pointer to data memory");
    fromRegPointer(0);
    opLoad8(.OB, .data, .sx, 0);
}
pub fn _EA00_EAFF() void {
    encoding(.LD, .{ addr(.data, .Xa), .to, .Rb });
    desc("Load 16b register using pointer to data memory");
    fromRegPointer(0);
    opLoad16(.OB, .data, 0);
}
pub fn _EB00_EBFF() void {
    encoding(.LD, .{ addr(.data, .Xa), .to, .Xb });
    desc("Load 32b register using pointer to data memory");
    fromRegPointer(0);
    opLoad32(.OB, .data, 0);
}

pub fn _1300_13FF() void {
    encoding(.ST, .{ .Bb, .to, addr(.data, .Xa) });
    desc("Store byte using pointer to data memory");
    fromRegPointer(0);
    opStore8(.OB, .data, 0);
}
pub fn _1400_14FF() void {
    encoding(.ST, .{ .Rb, .to, addr(.data, .Xa) });
    desc("Store 16b register using pointer to data memory");
    fromRegPointer(0);
    opStore16(.OB, .data, 0);
}
pub fn _1500_15FF() void {
    encoding(.ST, .{ .Xb, .to, addr(.data, .Xa) });
    desc("Store 32b register using pointer to data memory");
    fromRegPointer(0);
    opStore32(.OB, .data, 0);
}

// data pointer+reg load/store
pub fn _EC00_ECFF() void {
    encoding(.LD, .{ Xa_relative(.data, .RbU), .to, .B0U });
    desc("Load unsigned byte using pointer to data memory with 16b register offset");
    fromRegPointerPlusRegOffset();
    load8(0, .data, .zx, 0);
}
pub fn _ED00_EDFF() void {
    encoding(.LD, .{ Xa_relative(.data, .RbU), .to, .B0S });
    desc("Load signed byte using pointer to data memory with 16b register offset");
    fromRegPointerPlusRegOffset();
    load8(0, .data, .sx, 0);
}
pub fn _EE00_EEFF() void {
    encoding(.LD, .{ Xa_relative(.data, .RbU), .to, .R0 });
    desc("Load 16b register using pointer to data memory with 16b register offset");
    fromRegPointerPlusRegOffset();
    load16(0, .data, 0);
}
pub fn _EF00_EFFF() void {
    encoding(.LD, .{ Xa_relative(.data, .RbU), .to, .X0 });
    desc("Load 32b register using pointer to data memory with 16b register offset");
    fromRegPointerPlusRegOffset();
    load32(0, .data, 0);
}

pub fn _1600_16FF() void {
    encoding(.ST, .{ .B0, .to, Xa_relative(.data, .RbU) });
    desc("Store byte using pointer to data memory with 16b register offset");
    fromRegPointerPlusRegOffset();
    store8(0, .data, 0);
}
pub fn _1700_17FF() void {
    encoding(.ST, .{ .R0, .to, Xa_relative(.data, .RbU) });
    desc("Store 16b register using pointer to data memory with 16b register offset");
    fromRegPointerPlusRegOffset();
    store16(0, .data, 0);
}
pub fn _1800_18FF() void {
    encoding(.ST, .{ .X0, .to, Xa_relative(.data, .RbU) });
    desc("Store 32b register using pointer to data memory with 16b register offset");
    fromRegPointerPlusRegOffset();
    store32(0, .data, 0);
}

// data pointer+imm load/store
pub fn _DC00_DCFF() void {
    encoding(.LD, .{ Xa_relative(.data, .immb4u), .to, .B0U });
    desc("Load unsigned byte using pointer to data memory with immediate offset");
    fromRegPointerPlusImmOffset(OB());
    load8(0, .data, .zx, 0);
}
pub fn _DD00_DDFF() void {
    encoding(.LD, .{ Xa_relative(.data, .immb4u), .to, .B0S });
    desc("Load signed byte using pointer to data memory with immediate offset");
    fromRegPointerPlusImmOffset(OB());
    load8(0, .data, .sx, 0);
}
pub fn _DE00_DEFF() void {
    encoding(.LD, .{ Xa_relative(.data, .@"immb4u/2"), .to, .R0 });
    desc("Load 16b register using pointer to data memory with immediate offset (align 2)");
    fromRegPointerPlusImmOffset(@as(SignedOffsetForLiteral, OB()) * 2);
    load16(0, .data, 0);
}
pub fn _DF00_DFFF() void {
    encoding(.LD, .{ Xa_relative(.data, .@"immb4u/4"), .to, .X0 });
    desc("Load 32b register using pointer to data memory with immediate offset (align 4)");
    fromRegPointerPlusImmOffset(@as(SignedOffsetForLiteral, OB()) * 4);
    load32(0, .data, 0);
}

pub fn _1900_19FF() void {
    encoding(.ST, .{ .B0, .to, Xa_relative(.data, .immb4u) });
    desc("Store byte using pointer to data memory with immediate offset");
    fromRegPointerPlusImmOffset(OB());
    store8(0, .data, 0);
}
pub fn _1A00_1AFF() void {
    encoding(.ST, .{ .R0, .to, Xa_relative(.data, .@"immb4u/2") });
    desc("Store 16b register using pointer to data memory with immediate offset (align 2)");
    fromRegPointerPlusImmOffset(@as(SignedOffsetForLiteral, OB()) * 2);
    store16(0, .data, 0);
}
pub fn _1B00_1BFF() void {
    encoding(.ST, .{ .X0, .to, Xa_relative(.data, .@"immb4u/4") });
    desc("Store 32b register using pointer to data memory with immediate offset (align 4)");
    fromRegPointerPlusImmOffset(@as(SignedOffsetForLiteral, OB()) * 4);
    store32(0, .data, 0);
}

// SP+imm load/store
pub fn _CC00_CCFF() void {
    encoding(.LD, .{ SP_relative(.stack, .immb4u), .to, .BaU });
    desc("Load unsigned byte from stack with immediate offset");
    fromSPPlusImmOffset(OB());
    opLoad8(.OA, .stack, .zx, 0);
}
pub fn _CD00_CDFF() void {
    encoding(.LD, .{ SP_relative(.stack, .immb4u), .to, .BaS });
    desc("Load signed byte from stack with immediate offset");
    fromSPPlusImmOffset(OB());
    opLoad8(.OA, .stack, .sx, 0);
}
pub fn _CE00_CEFF() void {
    encoding(.LD, .{ SP_relative(.stack, .@"immb4u/2"), .to, .Ra });
    desc("Load 16b register from stack with immediate offset (align 2)");
    fromSPPlusImmOffset(@as(SignedOffsetForLiteral, OB()) * 2);
    opLoad16(.OA, .stack, 0);
}
pub fn _CF00_CFFF() void {
    encoding(.LD, .{ SP_relative(.stack, .@"immb4u/4"), .to, .Xa });
    desc("Load 32b register from stack with immediate offset (align 4)");
    fromSPPlusImmOffset(@as(SignedOffsetForLiteral, OB()) * 4);
    opLoad32(.OA, .stack, 0);
}

pub fn _1C00_1CFF() void {
    encoding(.ST, .{ .Ba, .to, SP_relative(.stack, .immb4u) });
    desc("Store byte to stack with immediate offset");
    fromSPPlusImmOffset(OB());
    opStore8(.OA, .stack, 0);
}
pub fn _1D00_1DFF() void {
    encoding(.ST, .{ .Ra, .to, SP_relative(.stack, .@"immb4u/2") });
    desc("Store 16b register to stack with immediate offset (align 2)");
    fromSPPlusImmOffset(@as(SignedOffsetForLiteral, OB()) * 2);
    opStore16(.OA, .stack, 0);
}
pub fn _1E00_1EFF() void {
    encoding(.ST, .{ .Xa, .to, SP_relative(.stack, .@"immb4u/4") });
    desc("Store 32b register to stack with immediate offset (align 4)");
    fromSPPlusImmOffset(@as(SignedOffsetForLiteral, OB()) * 4);
    opStore32(.OA, .stack, 0);
}

// SP+imm16 load/store
pub fn _C000_C00F() void {
    encoding(.LD, .{ SP_relative(.stack, .imm16s), .to, .BaU });
    desc("Load unsigned byte from stack with immediate offset");
    fromSPPlusImm16Offset();
    opLoad8(.OA, .stack, .zx, 0);
}
pub fn _C010_C01F() void {
    encoding(.LD, .{ SP_relative(.stack, .imm16s), .to, .BaS });
    desc("Load signed byte from stack with immediate offset");
    fromSPPlusImm16Offset();
    opLoad8(.OA, .stack, .sx, 0);
}
pub fn _C020_C02F() void {
    encoding(.LD, .{ SP_relative(.stack, .imm16s), .to, .Ra });
    desc("Load 16b register from stack with immediate offset");
    fromSPPlusImm16Offset();
    opLoad16(.OA, .stack, 0);
}
pub fn _C030_C03F() void {
    encoding(.LD, .{ SP_relative(.stack, .imm16s), .to, .Xa });
    desc("Load 32b register from stack with immediate offset");
    fromSPPlusImm16Offset();
    opLoad32(.OA, .stack, 0);
}

pub fn _C040_C04F() void {
    encoding(.ST, .{ .Ba, .to, SP_relative(.stack, .imm16s) });
    desc("Store byte to stack with immediate offset");
    fromSPPlusImm16Offset();
    opStore8(.OA, .stack, 0);
}
pub fn _C050_C05F() void {
    encoding(.ST, .{ .Ra, .to, SP_relative(.stack, .imm16s) });
    desc("Store 16b register to stack with immediate offset");
    fromSPPlusImm16Offset();
    opStore16(.OA, .stack, 0);
}
pub fn _C060_C06F() void {
    encoding(.ST, .{ .Xa, .to, SP_relative(.stack, .imm16s) });
    desc("Store 32b register to stack with immediate offset");
    fromSPPlusImm16Offset();
    opStore32(.OA, .stack, 0);
}

// SP+reg load/store
pub fn _BC00_BCFF() void {
    encoding(.LD, .{ SP_relative(.stack, .RbS), .to, .BaU });
    desc("Load unsigned byte from stack with 16b register offset");
    fromSPPlusRegOffset();
    opLoad8(.OA, .stack, .zx, 0);
}
pub fn _BD00_BDFF() void {
    encoding(.LD, .{ SP_relative(.stack, .RbS), .to, .BaS });
    desc("Load signed byte from stack with 16b register offset");
    fromSPPlusRegOffset();
    opLoad8(.OA, .stack, .sx, 0);
}
pub fn _BE00_BEFF() void {
    encoding(.LD, .{ SP_relative(.stack, .RbS), .to, .Ra });
    desc("Load 16b register from stack with 16b register offset");
    fromSPPlusRegOffset();
    opLoad16(.OA, .stack, 0);
}
pub fn _BF00_BFFF() void {
    encoding(.LD, .{ SP_relative(.stack, .RbS), .to, .Xa });
    desc("Load 32b register from stack with 16b register offset");
    fromSPPlusRegOffset();
    opLoad32(.OA, .stack, 0);
}

pub fn _2000_20FF() void {
    encoding(.ST, .{ .Ba, .to, SP_relative(.stack, .RbS) });
    desc("Store byte to stack with 16b register offset");
    fromSPPlusRegOffset();
    opStore8(.OA, .stack, 0);
}
pub fn _2100_21FF() void {
    encoding(.ST, .{ .Ra, .to, SP_relative(.stack, .RbS) });
    desc("Store 16b register to stack with 16b register offset");
    fromSPPlusRegOffset();
    opStore16(.OA, .stack, 0);
}
pub fn _2200_22FF() void {
    encoding(.ST, .{ .Xa, .to, SP_relative(.stack, .RbS) });
    desc("Store 32b register to stack with 16b register offset");
    fromSPPlusRegOffset();
    opStore32(.OA, .stack, 0);
}

// postincrement load/store
pub fn _9800_98FF() void {
    encoding(.LDI, .{ addr(.data, .Xa), .to, .BbU });
    desc("Load unsigned byte using pointer to data memory; postincrement");
    incRegPointer(1);
    opLoad8(.OB, .data, .zx, -1);
}
pub fn _9900_99FF() void {
    encoding(.LDI, .{ addr(.data, .Xa), .to, .BbS });
    desc("Load signed byte using pointer to data memory; postincrement");
    incRegPointer(1);
    opLoad8(.OB, .data, .sx, -1);
}
pub fn _9A00_9AFF() void {
    encoding(.LDI, .{ addr(.data, .Xa), .to, .Rb });
    desc("Load 16b register using pointer to data memory; postincrement");
    incRegPointer(2);
    opLoad16(.OB, .data, -2);
}
pub fn _9B00_9BFF() void {
    encoding(.LDI, .{ addr(.data, .Xa), .to, .Xb });
    desc("Load 32b register using pointer to data memory; postincrement");
    incRegPointer(4);
    opLoad32(.OB, .data, -4);
}

pub fn _2300_23FF() void {
    encoding(.STI, .{ .Bb, .to, addr(.data, .Xa) });
    desc("Store byte using pointer to data memory; postincrement");
    incRegPointer(1);
    opStore8(.OB, .data, -1);
}
pub fn _2400_24FF() void {
    encoding(.STI, .{ .Rb, .to, addr(.data, .Xa) });
    desc("Store 16b register using pointer to data memory; postincrement");
    incRegPointer(2);
    opStore16(.OB, .data, -2);
}
pub fn _2500_25FF() void {
    encoding(.STI, .{ .Xb, .to, addr(.data, .Xa) });
    desc("Store 32b register using pointer to data memory; postincrement");
    incRegPointer(4);
    opStore32(.OB, .data, -4);
}

// preincrement load/store
pub fn _9C00_9CFF() void {
    encoding(.ILD, .{ addr(.data, .Xa), .to, .BbU });
    desc("Load unsigned byte using pointer to data memory; preincrement");
    incRegPointer(1);
    opLoad8(.OB, .data, .zx, 0);
}
pub fn _9D00_9DFF() void {
    encoding(.ILD, .{ addr(.data, .Xa), .to, .BbS });
    desc("Load signed byte using pointer to data memory; preincrement");
    incRegPointer(1);
    opLoad8(.OB, .data, .sx, 0);
}
pub fn _9E00_9EFF() void {
    encoding(.ILD, .{ addr(.data, .Xa), .to, .Rb });
    desc("Load 16b register using pointer to data memory; preincrement");
    incRegPointer(2);
    opLoad16(.OB, .stack, 0);
}
pub fn _9F00_9FFF() void {
    encoding(.ILD, .{ addr(.data, .Xa), .to, .Xb });
    desc("Load 32b register using pointer to data memory; preincrement");
    incRegPointer(4);
    opLoad32(.OB, .stack, 0);
}

pub fn _2600_26FF() void {
    encoding(.IST, .{ .Bb, .to, addr(.data, .Xa) });
    desc("Load signed byte using pointer to data memory; preincrement");
    incRegPointer(1);
    opStore8(.OB, .data, 0);
}
pub fn _2700_27FF() void {
    encoding(.IST, .{ .Rb, .to, addr(.data, .Xa) });
    desc("Load 16b register using pointer to data memory; preincrement");
    incRegPointer(2);
    opStore16(.OB, .stack, 0);
}
pub fn _2800_28FF() void {
    encoding(.IST, .{ .Xb, .to, addr(.data, .Xa) });
    desc("Load 32b register using pointer to data memory; preincrement");
    incRegPointer(4);
    opStore32(.OB, .stack, 0);
}

// user context load
pub fn _B800_B8FF() void {
    encoding(.LD, .{ UXP_relative(.data, .@"immb4u/2"), .to, .Ra });
    desc("Load 16b register from user context, with immediate offset (align 2)");

    SR_plus_literal_to_L(.uxp, @as(SignedOffsetForLiteral, OB()) * 2, .fresh, .no_flags);
    L_to_SR(.temp_1);
    load_next_insn(2);
    next_cycle();

    opLoad16(.OA, .data, 0);
}
// pub fn _B800_B8FF() void {
//     encoding(.LD, .{ UXP_relative(.insn, .@"immb4u/2"), .to, .Ra });
//     desc("Load 16b register from user context, with immediate offset (align 2)");
    
//     SR_plus_literal_to_L(.uxp, @as(SignedOffsetForLiteral, OB()) * 2, .fresh, .no_flags);
//     L_to_SR(.temp_1);
//     load_next_insn(2);
//     next_cycle();

//     opLoad16(.OA, .insn, 0);
// }

pub fn _B900_B9FF() void {
    encoding(.LD, .{ UXP_relative(.data, .@"immb4u/4"), .to, .Xa });
    desc("Load 32b register from user context, with immediate offset (align 4)");

    SR_plus_literal_to_L(.uxp, @as(SignedOffsetForLiteral, OB()) * 4, .fresh, .no_flags);
    L_to_SR(.temp_1);
    load_next_insn(2);
    next_cycle();

    opLoad32(.OA, .data, 0);
}
// pub fn _B900_B9FF() void {
//     encoding(.LD, .{ UXP_relative(.insn, .@"immb4u/4"), .to, .Xa });
//     desc("Load 32b register from user context, with immediate offset (align 4)");
    
//     SR_plus_literal_to_L(.uxp, @as(SignedOffsetForLiteral, OB()) * 4, .fresh, .no_flags);
//     L_to_SR(.temp_1);
//     load_next_insn(2);
//     next_cycle();

//     opLoad32(.OA, .insn, 0);
// }

// kernel context load
pub fn _BA00_BAFF() void {
    encoding(.LD, .{ KXP_relative(.data, .@"immb4u/2"), .to, .Ra });
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
// pub fn _BA00_BAFF() void {
//     encoding(.LD, .{ KXP_relative(.insn, .@"immb4u/2"), .to, .Ra });
//     desc("Load 16b register from user context, with immediate offset (align 2)");
    
//     if (!kernel()) {
//         illegal_instruction();
//         return;
//     }

//     SR_plus_literal_to_L(.uxp, @as(SignedOffsetForLiteral, OB()) * 2, .fresh, .no_flags);
//     L_to_SR(.temp_1);
//     load_next_insn(2);
//     next_cycle();

//     opLoad16(.OA, .insn, 0);
// }

pub fn _BB00_BBFF() void {
    encoding(.LD, .{ KXP_relative(.data, .@"immb4u/4"), .to, .Xa });
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
// pub fn _BB00_BBFF() void {
//     encoding(.LD, .{ KXP_relative(.insn, .@"immb4u/4"), .to, .Xa });
//     desc("Load 32b register from user context, with immediate offset (align 4)");
    
//     if (!kernel()) {
//         illegal_instruction();
//         return;
//     }

//     SR_plus_literal_to_L(.uxp, @as(SignedOffsetForLiteral, OB()) * 4, .fresh, .no_flags);
//     L_to_SR(.temp_1);
//     load_next_insn(2);
//     next_cycle();

//     opLoad32(.OA, .insn, 0);
// }

pub fn _0C60_0C6F() void {
    encoding(.MCR, .{ .XaS, addr(.data, .BP), .to, addr(.data, .RP) });
    desc("Copy a block of memory.  BP points to the first byte of the source block and RP points to the first byte of the destination block.  If blocks overlap, BP must be larger than RP.  If a block crosses a page boundary, it must be word aligned.  Takes approximately block_bytes + 2 cycles to complete.");

    op_reg32_plus_literal_to_L(.OA, -2, .fresh, .flags);
    next_cycle_conditional(mcr_continuation_1);
}
fn mcr_continuation_1() void {
    if (negative()) {
        // length == 0 or 1 bytes
        op_reg32_plus_literal_to_L(.OA, -1, .fresh, .flags);
        next_cycle_conditional(mcr_continuation_2);
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
        next_cycle_explicit(0x209);
    }
}
fn mcr_continuation_2() void {
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
        next_cycle_explicit(0x209);
    }
}

pub fn _0C70_0C7F() void {
    encoding(.MCRB, .{ .XaS, addr(.data, .BP), .to, addr(.data, .RP) });
    desc("Copy a block of memory with byte transfers only.  BP points to the first byte of the source block and RP points to the first byte of the destination block.  If blocks overlap, BP must be larger than RP.  Takes approximately block_bytes * 2 + 2 cycles to complete, but is not susceptible to any of the alignment constraints of MCR.");

    op_reg32_plus_literal_to_L(.OA, -1, .fresh, .flags);
    next_cycle_conditional(mcrb_continuation_1);
}
pub fn mcrb_continuation_1() void {
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
        next_cycle_explicit(0x20B);
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
        next_cycle_explicit(0x20B);
    }
}

pub fn _0CA0_0CAF() void {
    encoding(.MCF, .{ .XaS, addr(.data, .BP), .to, addr(.data, .RP) });
    desc("Copy a block of memory.  BP points to the byte following the source block and RP points to the byte following the destination block.  If blocks overlap, BP must be smaller than RP.  If a block crosses a page boundary, it must be word aligned.  Takes approximately block_bytes + 2 cycles to complete.");

    op_reg32_plus_literal_to_L(.OA, -2, .fresh, .flags);
    next_cycle_conditional(mcf_continuation_1);
}
fn mcf_continuation_1() void {
    if (negative()) {
        // length == 0 or 1 bytes
        op_reg32_plus_literal_to_L(.OA, -1, .fresh, .flags);
        next_cycle_conditional(mcf_continuation_2);
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
        next_cycle_explicit(0x20E);
    }
}
fn mcf_continuation_2() void {
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
        next_cycle_explicit(0x20E);
    }
}

pub fn _0CB0_0CBF() void {
    encoding(.MCFB, .{ .XaS, addr(.data, .BP), .to, addr(.data, .RP) });
    desc("Copy a block of memory with byte transfers only.  BP points to the byte following the source block and RP points to the byte following the destination block.  If blocks overlap, BP must be smaller than RP.  Takes approximately block_bytes * 2 + 2 cycles to complete, but is not susceptible to any of the alignment constraints of MCF.");

    op_reg32_plus_literal_to_L(.OA, -1, .fresh, .flags);
    next_cycle_explicit(0x20F);
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
        next_cycle_explicit(0x20F);
    }
}

pub fn _0CC0_0CCF() void {
    encoding(.SI, .{ .XaS, addr(.data, .BP), .to, addr(.data, .RP) });
    desc("Stream from a fixed address to a block of memory.  BP points to the fixed address and RP points to the first byte of the destination block.  If the block crosses a page boundary, it must be word aligned.  Takes approximately block_bytes + 2 cycles to complete.");

    op_reg32_plus_literal_to_L(.OA, -2, .fresh, .flags);
    next_cycle_conditional(si_continuation_1);
}
pub fn si_continuation_1() void {
    if (negative()) {
        // length == 0 or 1 bytes
        op_reg32_plus_literal_to_L(.OA, -1, .fresh, .flags);
        next_cycle_conditional(si_continuation_2);
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
        next_cycle_explicit(0x212);
    }
}
fn si_continuation_2() void {
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
        next_cycle_explicit(0x212);
    }
}

pub fn _0CD0_0CDF() void {
    encoding(.SIB, .{ .XaS, addr(.data, .BP), .to, addr(.data, .RP) });
    desc("Stream from a fixed address to a block of memory, with byte transfers only.  BP points to the fixed address and RP points to the first byte of the destination block.  Takes approximately block_bytes * 2 + 2 cycles to complete, but is not susceptible to any of the alignment constraints of SI.");

    op_reg32_plus_literal_to_L(.OA, -1, .fresh, .flags);
    next_cycle_conditional(sib_continuation_1);
}
fn sib_continuation_1() void {
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
        next_cycle_explicit(0x214);
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
        next_cycle_explicit(0x214);
    }
}

pub fn _0CE0_0CEF() void {
    encoding(.SO, .{ .XaS, addr(.data, .BP), .to, addr(.data, .RP) });
    desc("Stream a block of memory to a fixed address.  BP points to the first byte of the source block and RP points to the fixed destination address.  If the block crosses a page boundary, it must be word aligned.  Takes approximately block_bytes + 2 cycles to complete.");

    op_reg32_plus_literal_to_L(.OA, -2, .fresh, .flags);
    next_cycle_conditional(so_continuation_1);
}
fn so_continuation_1() void {
    if (negative()) {
        // length == 0 or 1 bytes
        op_reg32_plus_literal_to_L(.OA, -1, .fresh, .flags);
        next_cycle_conditional(so_continuation_2);
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
        next_cycle_explicit(0x217);
    }
}
fn so_continuation_2() void {
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
        next_cycle_explicit(0x217);
    }
}

pub fn _0CF0_0CFF() void {
    encoding(.SOB, .{ .XaS, addr(.data, .BP), .to, addr(.data, .RP) });
    desc("Stream a block of memory to a fixed address, with byte transfers only.  BP points to the first byte of the source block and RP points to the fixed destination address.  Takes approximately block_bytes * 2 + 2 cycles to complete, but is not susceptible to any of the alignment constraints of SO.");

    op_reg32_plus_literal_to_L(.OA, -1, .fresh, .flags);
    next_cycle_conditional(sob_continuation_1);
}
fn sob_continuation_1() void {
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
        next_cycle_explicit(0x219);
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
        next_cycle_explicit(0x219);
    }
}

pub fn _0D00_0D0F() void {
    encoding(.BLD, .{ .to, addr(.data, .Xa) });
    desc("Load 8 bytes from FLASH or PSRAM to RAM.  Xa points to the first chunk of RAM to write to (will be incremented by 8).  Block source must be configured beforehand by writing to the configuration port.  Only one pipe may perform a block operation at a time.");

    if (!kernel()) {
        illegal_instruction();
        return;
    }

    op_reg32_to_L(.OA);
    L_to_SR(.temp_1);
    load_next_insn(2);
    next_cycle();

    op_reg32_plus_literal_to_L(.OA, 8, .fresh, .no_flags);
    L_to_op_reg32(.OA);
    block_transfer_to_ram(.temp_1, 0, .data);
    exec_next_insn_no_atomic_end();
}

pub fn _0D10_0D1F() void {
    encoding(.BLD, .{ .XaS, .to, addr(.data, .BP) });
    desc("Load from FLASH or PSRAM to RAM.  Xa indicates the number of bytes remaining to be copied (will be decremented by 8 each cycle; operation ends when zero or negative).  BP points to just before the first byte of RAM to write to (will be incremented by 8 before each cycle).  Block source must be configured beforehand by writing to the configuration port.  Only one pipe may perform a block operation at a time.");

    _ = positive(); // Make sure that we query N/Z flags even if K is false
    
    if (!kernel()) {
        illegal_instruction();
        return;
    }

    _continuation_204();
}
pub fn _continuation_204() void {
    if (positive()) {
        block_transfer_to_ram(.bp, 8, .data);
        PN_to_SR(.bp);
        op_reg32_minus_literal_to_L(.OA, 8, .fresh, .flags);
        L_to_op_reg32(.OA);
        next_cycle_explicit(0x204);
    } else {
        load_and_exec_next_insn_no_atomic_end(2);
    }
}


pub fn _0D20_0D2F() void {
    encoding(.BST, .{ addr(.data, .Xa) });
    desc("Store 8 bytes from RAM into FLASH or PSRAM.  Xa points to the first chunk of RAM to read (will be incremented by 8).  Block destination must be configured beforehand by writing to the configuration port.  Only one pipe may perform a block operation at a time.");

    if (!kernel()) {
        illegal_instruction();
        return;
    }

    op_reg32_to_L(.OA);
    L_to_SR(.temp_1);
    load_next_insn(2);
    next_cycle();

    op_reg32_plus_literal_to_L(.OA, 8, .fresh, .no_flags);
    L_to_op_reg32(.OA);
    block_transfer_from_ram(.temp_1, 0, .data);
    exec_next_insn_no_atomic_end();
}

pub fn _0D30_0D3F() void {
    encoding(.BST, .{ .XaS, addr(.data, .BP) });
    desc("Store from RAM into FLASH or PSRAM.  Xa indicates the number of bytes remaining to be copied (will be decremented by 8 each cycle; operation ends when zero or negative).  BP points to just before the first byte of RAM to read (will be incremented by 8 before each cycle).  Block destination must be configured beforehand by writing to the configuration port.  Only one pipe may perform a block operation at a time.");

    _ = positive(); // Make sure that we query N/Z flags even if K is false

    if (!kernel()) {
        illegal_instruction();
        return;
    }

    _continuation_205();
}
pub fn _continuation_205() void {
    if (positive()) {
        block_transfer_from_ram(.bp, 8, .data);
        PN_to_SR(.bp);
        op_reg32_minus_literal_to_L(.OA, 8, .fresh, .flags);
        L_to_op_reg32(.OA);
        next_cycle_explicit(0x205);
    } else {
        load_and_exec_next_insn_no_atomic_end(2);
    }
}
