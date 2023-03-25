const ib = @import("instruction_builder.zig");
const cb = @import("cycle_builder.zig");

const Xa_relative = ib.Xa_relative;
const encoding = ib.encoding;
const encodingWithSuffix = ib.encodingWithSuffix;
const desc = ib.desc;
const next_cycle = ib.next_cycle;
const next_cycle_conditional = ib.next_cycle_conditional;
const zero = ib.zero;

const op_reg32_to_L = cb.op_reg32_to_L;
const op_reg_to_JL = cb.op_reg_to_JL;
const reg_to_JL = cb.reg_to_JL;
const L_to_SR = cb.L_to_SR;
const IP_read_to_D = cb.IP_read_to_D;
const read_to_D = cb.read_to_D;
const write_from_LL = cb.write_from_LL;
const add_to_LL = cb.add_to_LL;
const sub_to_LL = cb.sub_to_LL;
const D_to_L = cb.D_to_L;
const D_to_LL = cb.D_to_LL;
const D_to_OB_OA = cb.D_to_OB_OA;
const LL_to_op_reg = cb.LL_to_op_reg;
const LL_to_reg = cb.LL_to_reg;
const L_to_op_reg32 = cb.L_to_op_reg32;
const reg_to_J = cb.reg_to_J;
const reg_to_K = cb.reg_to_K;
const op_reg_to_J = cb.op_reg_to_J;
const op_reg_to_K = cb.op_reg_to_K;
const op_reg_to_L = cb.op_reg_to_L;
const op_reg_to_LL = cb.op_reg_to_LL;
const op_reg_plus_literal_to_LL = cb.op_reg_plus_literal_to_LL;
const op_reg_minus_literal_to_LL = cb.op_reg_minus_literal_to_LL;
const op_reg32_plus_literal_to_L = cb.op_reg32_plus_literal_to_L;
const op_reg32_minus_literal_to_L = cb.op_reg32_minus_literal_to_L;
const load_and_exec_next_insn = cb.load_and_exec_next_insn;
const load_next_insn = cb.load_next_insn;
const exec_next_insn = cb.exec_next_insn;
const assume_next_insn_loaded = cb.assume_next_insn_loaded;
const atomic_this_cycle = cb.atomic_this_cycle;
const atomic_next_cycle_until_end = cb.atomic_next_cycle_until_end;
const ZN_from_LL = cb.ZN_from_LL;

pub fn _0002() void {
    encoding(.SYNC, .{});
    //syntax("SYNC");
    desc("Forces the next instruction to be atomic");
    atomic_next_cycle_until_end();
    load_and_exec_next_insn(2);
}

pub fn _3000_30FF() void {
    encodingWithSuffix(.ALD, .D, .{ Xa_relative(.D, .imm_0), .to, .Rb });
    //syntax("ALD.D Xa -> Rb");
    desc("Atomic load 16b register using pointer to data memory");

    op_reg32_to_L(.OA);
    L_to_SR(.temp_1);
    load_next_insn(2);
    next_cycle();

    atomic_this_cycle();
    read_to_D(.temp_1, 0, .word, .data);
    D_to_LL();
    LL_to_op_reg(.OB);
    exec_next_insn();
}

pub fn _3100_31FF() void {
    encodingWithSuffix(.ALD, .D, .{ Xa_relative(.D, .imm_0), .to, .Xb });
    //syntax("ALD.D Xa -> Xb");
    desc("Atomic load 32b register using pointer to data memory");

    op_reg32_to_L(.OA);
    L_to_SR(.temp_1);
    load_next_insn(2);
    next_cycle();

    atomic_this_cycle();
    read_to_D(.temp_1, 0, .word, .data);
    D_to_LL();
    LL_to_op_reg(.OB);
    next_cycle();

    atomic_this_cycle();
    read_to_D(.temp_1, 2, .word, .data);
    D_to_LL();
    LL_to_op_reg(.OBxor1);
    exec_next_insn();
}

pub fn _3200_32FF() void {
    encodingWithSuffix(.AST, .D, .{ .Rb, .to, Xa_relative(.D, .imm_0) });
    //syntax("AST.D Rb -> Xa");
    desc("Atomic store 16b register using pointer to data memory");

    op_reg32_to_L(.OA);
    L_to_SR(.temp_1);
    load_next_insn(2);
    next_cycle();

    atomic_this_cycle();
    op_reg_to_LL(.OB);
    write_from_LL(.temp_1, 0, .word, .data);
    exec_next_insn();
}

pub fn _3300_33FF() void {
    encodingWithSuffix(.AST, .D, .{ .Xb, .to, Xa_relative(.D, .imm_0) });
    //syntax("AST.D Xb -> Xa");
    desc("Atomic store 32b register using pointer to data memory");

    op_reg32_to_L(.OA);
    L_to_SR(.temp_1);
    load_next_insn(2);
    next_cycle();

    atomic_this_cycle();
    op_reg_to_LL(.OB);
    write_from_LL(.temp_1, 0, .word, .data);
    next_cycle();

    atomic_this_cycle();
    op_reg_to_LL(.OBxor1);
    write_from_LL(.temp_1, 2, .word, .data);
    exec_next_insn();
}

pub fn _3400_34FF() void {
    encodingWithSuffix(.ASTZ, .D, .{ .Rb, .to, Xa_relative(.D, .imm_0) });
    //syntax("ASTZ.D Rb -> Xa");
    desc("Atomic store 16b register using pointer to data memory, if previous stored value is 0");

    op_reg32_to_L(.OA);
    L_to_SR(.temp_1);
    load_next_insn(2);
    next_cycle();

    atomic_this_cycle();
    read_to_D(.temp_1, 0, .word, .data);
    D_to_LL();
    ZN_from_LL(.fresh);
    next_cycle_conditional(astz16_continuation);
}
fn astz16_continuation() void {
    assume_next_insn_loaded(2);
    if (zero()) {
        atomic_this_cycle();
        op_reg_to_LL(.OB);
        write_from_LL(.temp_1, 0, .word, .data);
        exec_next_insn();
    } else {
        exec_next_insn();
    }
}

pub fn _3500_35FF() void {
    encodingWithSuffix(.ASTZ, .D, .{ .Xb, .to, Xa_relative(.D, .imm_0) });
    //syntax("ASTZ.D Xb -> Xa");
    desc("Atomic store 32b register using pointer to data memory, if previous stored value is 0");

    op_reg32_to_L(.OA);
    L_to_SR(.temp_1);
    load_next_insn(2);
    next_cycle();

    atomic_this_cycle();
    read_to_D(.temp_1, 0, .word, .data);
    D_to_LL();
    ZN_from_LL(.fresh);
    next_cycle();

    atomic_this_cycle();
    read_to_D(.temp_1, 2, .word, .data);
    D_to_LL();
    ZN_from_LL(.cont);
    next_cycle_conditional(astz32_continuation);
}
fn astz32_continuation() void {
    assume_next_insn_loaded(2);
    if (zero()) {
        atomic_this_cycle();
        op_reg_to_LL(.OB);
        write_from_LL(.temp_1, 0, .word, .data);
        next_cycle();

        atomic_this_cycle();
        op_reg_to_LL(.OBxor1);
        write_from_LL(.temp_1, 2, .word, .data);
        exec_next_insn();
    } else {
        exec_next_insn();
    }
}

pub fn _3600_36FF() void {
    encodingWithSuffix(.AADD, .D, .{ Xa_relative(.D, .imm_0), .to, .R0, .Rb, .to, Xa_relative(.D, .imm_0) });
    //syntax("AADD.D Xa -> R0, Rb -> Xa");
    desc("16b Atomic add using pointer to data memory");

    op_reg32_to_L(.OA);
    L_to_SR(.temp_1);
    load_next_insn(2);
    next_cycle();

    atomic_this_cycle();
    read_to_D(.temp_1, 0, .word, .data);
    D_to_LL();
    LL_to_reg(0);
    next_cycle();

    atomic_this_cycle();
    op_reg_to_K(.OB);
    reg_to_JL(0);
    add_to_LL(.fresh, .flags);
    LL_to_reg(0);
    write_from_LL(.temp_1, 0, .word, .data);
    exec_next_insn();
}

pub fn _3700_37FF() void {
    encodingWithSuffix(.AADD, .D, .{ Xa_relative(.D, .imm_0), .to, .X0, .Xb, .to, Xa_relative(.D, .imm_0) });
    //syntax("AADD.D Xa -> X0, Xb -> Xa");
    desc("32b Atomic add using pointer to data memory");

    op_reg32_to_L(.OA);
    L_to_SR(.temp_1);
    load_next_insn(2);
    next_cycle();

    atomic_this_cycle();
    read_to_D(.temp_1, 0, .word, .data);
    D_to_LL();
    LL_to_reg(0);
    next_cycle();

    atomic_this_cycle();
    read_to_D(.temp_1, 2, .word, .data);
    D_to_LL();
    LL_to_reg(1);
    next_cycle();

    atomic_this_cycle();
    op_reg_to_K(.OB);
    reg_to_JL(0);
    add_to_LL(.fresh, .flags);
    LL_to_reg(0);
    write_from_LL(.temp_1, 0, .word, .data);
    next_cycle();

    atomic_this_cycle();
    op_reg_to_K(.OBxor1);
    reg_to_JL(0);
    add_to_LL(.cont, .flags);
    LL_to_reg(1);
    write_from_LL(.temp_1, 2, .word, .data);
    exec_next_insn();
}

// Atomic increment & decrement
// Useful for implementing semaphores, mutexes, and latches

pub fn _3800_380F() void {
    encodingWithSuffix(.AINC, .D, .{ Xa_relative(.D, .imm_0), .to, .Ra });
    //syntax("AINC.D Xa -> Ra");
    desc("16b Atomic increment using pointer to data memory, overwriting pointer with new value");

    op_reg32_to_L(.OA);
    L_to_SR(.temp_1);
    load_next_insn(2);
    next_cycle();

    atomic_this_cycle();
    read_to_D(.temp_1, 0, .word, .data);
    D_to_LL();
    LL_to_op_reg(.OA);
    next_cycle();

    atomic_this_cycle();
    op_reg_plus_literal_to_LL(.OA, 1, .fresh, .flags);
    LL_to_op_reg(.OA);
    write_from_LL(.temp_1, 0, .word, .data);
    exec_next_insn();
}

pub fn _3900_39FF() void {
    encodingWithSuffix(.AINC, .D, .{ Xa_relative(.D, .imm_0), .to, .Xa });
    //syntax("AINC.D Xa -> Xa");
    desc("32b Atomic increment using pointer to data memory, overwriting pointer with new value");

    op_reg32_to_L(.OA);
    L_to_SR(.temp_1);
    load_next_insn(2);
    next_cycle();

    atomic_this_cycle();
    read_to_D(.temp_1, 0, .word, .data);
    D_to_LL();
    LL_to_op_reg(.OA);
    next_cycle();

    atomic_this_cycle();
    read_to_D(.temp_1, 2, .word, .data);
    D_to_LL();
    LL_to_op_reg(.OAxor1);
    next_cycle();

    atomic_this_cycle();
    op_reg32_plus_literal_to_L(.OA, 1, .fresh, .flags);
    L_to_op_reg32(.OA);
    write_from_LL(.temp_1, 0, .word, .data);
    next_cycle();

    atomic_this_cycle();
    op_reg_to_LL(.OAxor1);
    write_from_LL(.temp_1, 2, .word, .data);
    exec_next_insn();
}

pub fn _3A00_3A0F() void {
    encodingWithSuffix(.ADECNZ, .D, .{ Xa_relative(.D, .imm_0), .to, .Ra });
    //syntax("ADECNZ.D Xa -> Rb");
    desc("16b Atomic decrement using pointer to data memory, if stored value is not zero.  Overwrites address with final value.");

    op_reg32_to_L(.OA);
    L_to_SR(.temp_1);
    load_next_insn(2);
    next_cycle();

    atomic_this_cycle();
    read_to_D(.temp_1, 0, .word, .data);
    D_to_LL();
    LL_to_op_reg(.OA);
    ZN_from_LL(.fresh);
    next_cycle_conditional(adecnz16_continuation);
}
fn adecnz16_continuation() void {
    assume_next_insn_loaded(2);
    if (zero()) {
        exec_next_insn();
    } else {
        atomic_this_cycle();
        op_reg_minus_literal_to_LL(.OA, 1, .fresh, .no_flags);
        LL_to_op_reg(.OA);
        write_from_LL(.temp_1, 0, .word, .data);
        exec_next_insn();
    }
}

pub fn _3B00_3B0F() void {
    encodingWithSuffix(.ADECNZ, .D, .{ Xa_relative(.D, .imm_0), .to, .Xa });
    //syntax("ADECNZ.D Xa -> Xa");
    desc("32b Atomic decrement using pointer to data memory, if stored value is not zero.  Overwrites address with final value.");

    op_reg32_to_L(.OA);
    L_to_SR(.temp_1);
    load_next_insn(2);
    next_cycle();

    atomic_this_cycle();
    read_to_D(.temp_1, 0, .word, .data);
    D_to_LL();
    LL_to_op_reg(.OA);
    ZN_from_LL(.fresh);
    next_cycle();

    atomic_this_cycle();
    read_to_D(.temp_1, 2, .word, .data);
    D_to_LL();
    LL_to_op_reg(.OAxor1);
    ZN_from_LL(.cont);
    next_cycle_conditional(adecnz32_continuation);
}
fn adecnz32_continuation() void {
    assume_next_insn_loaded(2);
    if (zero()) {
        exec_next_insn();
    } else {
        atomic_this_cycle();
        op_reg32_minus_literal_to_L(.OA, 1, .fresh, .no_flags);
        L_to_op_reg32(.OA);
        write_from_LL(.temp_1, 0, .word, .data);
        next_cycle();

        atomic_this_cycle();
        op_reg_to_LL(.OAxor1);
        write_from_LL(.temp_1, 2, .word, .data);
        exec_next_insn();
    }
}

pub fn _3C00_3C0F() void {
    encodingWithSuffix(.AX, .D, .{ Xa_relative(.D, .imm_0), .to, .Rb1, .Ra1, .to, Xa_relative(.D, .imm_0) });
    //syntax("AX.D Xa -> Rb1, Ra1 -> Xa");
    desc("16b Atomic exchange using pointer to data memory");

    op_reg32_to_L(.OA);
    L_to_SR(.temp_1);
    IP_read_to_D(2, .byte);
    D_to_OB_OA();
    next_cycle();

    load_next_insn(3);
    next_cycle();

    atomic_this_cycle();
    read_to_D(.temp_1, 0, .word, .data);
    D_to_LL();
    LL_to_op_reg(.OB);
    next_cycle();

    atomic_this_cycle();
    op_reg_to_LL(.OA);
    write_from_LL(.temp_1, 0, .word, .data);
    exec_next_insn();
}

pub fn _3C10_3C1F() void {
    encodingWithSuffix(.AX, .D, .{ Xa_relative(.D, .imm_0), .to, .Xb1, .Xa1, .to, Xa_relative(.D, .imm_0) });
    //syntax("AX.D Xa -> Xb1, Xa1 -> Xa");
    desc("32b Atomic exchange using pointer to data memory");

    op_reg32_to_L(.OA);
    L_to_SR(.temp_1);
    IP_read_to_D(2, .byte);
    D_to_OB_OA();
    next_cycle();

    load_next_insn(3);
    next_cycle();

    atomic_this_cycle();
    read_to_D(.temp_1, 0, .word, .data);
    D_to_LL();
    LL_to_op_reg(.OB);
    next_cycle();

    atomic_this_cycle();
    read_to_D(.temp_1, 2, .word, .data);
    D_to_LL();
    LL_to_op_reg(.OBxor1);
    next_cycle();

    atomic_this_cycle();
    op_reg_to_LL(.OA);
    write_from_LL(.temp_1, 0, .word, .data);
    next_cycle();

    atomic_this_cycle();
    op_reg_to_LL(.OAxor1);
    write_from_LL(.temp_1, 2, .word, .data);
    exec_next_insn();
}

pub fn _3C20_3C2F() void {
    encodingWithSuffix(.AXE, .D, .{ Xa_relative(.D, .imm_0), .to, .R0, .Rb1, .Ra1, .to, Xa_relative(.D, .imm_0) });
    //syntax("AXE.D Xa -> R0, Rb1, Ra1 -> Xa");
    desc("16b Atomic exchange using pointer to data memory, if stored value equals probe value");

    op_reg32_to_L(.OA);
    L_to_SR(.temp_1);
    IP_read_to_D(2, .byte);
    D_to_OB_OA();
    next_cycle();

    load_next_insn(3);
    next_cycle();

    atomic_this_cycle();
    read_to_D(.temp_1, 0, .word, .data);
    D_to_LL();
    LL_to_reg(0);
    next_cycle();

    atomic_this_cycle();
    reg_to_JL(0);
    op_reg_to_K(.OB);
    sub_to_LL(.fresh, .flags);
    next_cycle_conditional(axe16_continuation);
}
fn axe16_continuation() void {
    assume_next_insn_loaded(3);
    if (zero()) {
        atomic_this_cycle();
        op_reg_to_LL(.OA);
        write_from_LL(.temp_1, 0, .word, .data);
        exec_next_insn();
    } else {
        exec_next_insn();
    }
}

pub fn _3C30_3C3F() void {
    encodingWithSuffix(.AXE, .D, .{ Xa_relative(.D, .imm_0), .to, .X0, .Xb1, .Xa1, .to, Xa_relative(.D, .imm_0) });
    //syntax("AXE.D Xa -> X0, Xb1, Xa1 -> Xa");
    desc("32b Atomic exchange using pointer to data memory, if stored value equals probe value");

    op_reg32_to_L(.OA);
    L_to_SR(.temp_1);
    IP_read_to_D(2, .byte);
    D_to_OB_OA();
    next_cycle();

    load_next_insn(3);
    next_cycle();

    atomic_this_cycle();
    read_to_D(.temp_1, 0, .word, .data);
    D_to_LL();
    LL_to_reg(0);
    next_cycle();

    atomic_this_cycle();
    reg_to_JL(0);
    op_reg_to_K(.OB);
    sub_to_LL(.fresh, .flags);
    next_cycle_conditional(axe32_continuation_1);
}
fn axe32_continuation_1() void {
    assume_next_insn_loaded(3);
    if (zero()) {
        atomic_this_cycle();
        read_to_D(.temp_1, 2, .word, .data);
        D_to_LL();
        LL_to_reg(1);
        next_cycle();

        atomic_this_cycle();
        reg_to_JL(1);
        op_reg_to_K(.OBxor1);
        sub_to_LL(.cont, .flags);
        next_cycle_conditional(axe32_continuation_2);
    } else {
        exec_next_insn();
    }
}
fn axe32_continuation_2() void {
    assume_next_insn_loaded(3);
    if (zero()) {
        atomic_this_cycle();
        op_reg_to_LL(.OA);
        write_from_LL(.temp_1, 0, .word, .data);
        next_cycle();

        atomic_this_cycle();
        op_reg_to_LL(.OAxor1);
        write_from_LL(.temp_1, 2, .word, .data);
        exec_next_insn();
    } else {
        exec_next_insn();
    }
}
