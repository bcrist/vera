const ib = @import("instruction_builder.zig");
const cb = @import("cycle_builder.zig");

const Xa_relative = ib.Xa_relative;
const encoding = ib.encoding;
const desc = ib.desc;
const next_cycle = ib.next_cycle;
const OB = ib.OB;

const write_from_LL = cb.write_from_LL;
const read_to_D = cb.read_to_D;
const IP_read_to_D = cb.IP_read_to_D;
const D_to_L = cb.D_to_L;
const D_to_LL = cb.D_to_LL;
const D_to_LH = cb.D_to_LH;
const D8_to_LL = cb.D8_to_LL;
const op_reg_to_LL = cb.op_reg_to_LL;
const L_to_SR = cb.L_to_SR;
const L_to_op_reg32 = cb.L_to_op_reg32;
const LL_to_op_reg = cb.LL_to_op_reg;
const OB_OA_to_K = cb.OB_OA_to_K;
const op_reg32_to_L = cb.op_reg32_to_L;
const SR_to_J = cb.SR_to_J;
const SR_to_L = cb.SR_to_L;
const SRL_to_K = cb.SRL_to_K;
const SRL_to_LL = cb.SRL_to_LL;
const SRH_to_LL = cb.SRH_to_LL;
const SR_plus_literal_to_L = cb.SR_plus_literal_to_L;
const SR_plus_reg_to_L = cb.SR_plus_reg_to_L;
const SR_minus_reg_to_L = cb.SR_minus_reg_to_L;
const SR_minus_literal_to_L = cb.SR_minus_literal_to_L;
const add_to_L = cb.add_to_L;
const sub_to_L = cb.sub_to_L;
const load_next_insn = cb.load_next_insn;
const exec_next_insn = cb.exec_next_insn;
const load_and_exec_next_insn = cb.load_and_exec_next_insn;

pub fn _FB80_FB8F() void {
    encoding(.C, .{ Xa_relative(.S, .imm_0), .to, .SP });
    //syntax("C Xa -> SP");
    desc("Copy 32b register to stack pointer");

    op_reg32_to_L(.OA);
    L_to_SR(.sp);
    load_and_exec_next_insn(2);
}

pub fn _0008() void {
    encoding(.UNFRAME, .{ .R0 });
    //syntax("UNFRAME SRa");
    desc("Add 16b register to stack pointer");

    SR_plus_reg_to_L(.sp, 0, .sx, .fresh, .no_flags);
    L_to_SR(.sp);
    load_and_exec_next_insn(2);
}

pub fn _0009() void {
    encoding(.FRAME, .{ .R0 });
    //syntax("FRAME SRa");
    desc("Subtract 16b register from stack pointer");

    SR_minus_reg_to_L(.sp, 0, .sx, .fresh, .no_flags);
    L_to_SR(.sp);
    load_and_exec_next_insn(2);
}

pub fn _5A00_5AFF() void {
    encoding(.UNFRAME, .{ .immba8u });
    //syntax("UNFRAME immba[0,255]");
    desc("Add immediate to stack pointer");

    SR_to_J(.sp);
    OB_OA_to_K();
    add_to_L(.zx, .fresh, .no_flags);
    L_to_SR(.sp);
    load_and_exec_next_insn(2);
}

pub fn _5B00_5BFF() void {
    encoding(.FRAME, .{ .immba8u });
    //syntax("FRAME immba[0,255]");
    desc("Subtract immediate from stack pointer");

    SR_to_J(.sp);
    OB_OA_to_K();
    sub_to_L(.zx, .fresh, .no_flags);
    L_to_SR(.sp);
    load_and_exec_next_insn(2);
}

pub fn _0004() void {
    encoding(.UNFRAME, .{ .imm16u });
    //syntax("UNFRAME imm16[0,65535]");
    desc("Add immediate to stack pointer");

    IP_read_to_D(2, .word);
    D_to_L(.zx);
    L_to_SR(.temp_2);
    next_cycle();

    SR_to_J(.sp);
    SRL_to_K(.temp_2);
    add_to_L(.zx, .fresh, .no_flags);
    L_to_SR(.sp);
    load_and_exec_next_insn(4);
}

pub fn _0005() void {
    encoding(.FRAME, .{ .imm16u });
    //syntax("FRAME imm16[0,65535]");
    desc("Subtract immediate from stack pointer");

    IP_read_to_D(2, .word);
    D_to_L(.zx);
    L_to_SR(.temp_2);
    next_cycle();

    SR_to_J(.sp);
    SRL_to_K(.temp_2);
    sub_to_L(.zx, .fresh, .no_flags);
    L_to_SR(.sp);
    load_and_exec_next_insn(4);
}

pub fn _E480_E49F() void {
    var ext: cb.ZeroOrSignExtension = undefined;
    switch (OB()) {
        0x8 => {
            encoding(.POP, .{ .BaU });
            //syntax("POP UBa");
            desc("Pop unsigned byte from stack to 16b register");
            ext = .zx;
        },
        0x9 => {
            encoding(.POP, .{ .BaS });
            //syntax("POP SBa");
            desc("Pop signed byte from stack to 16b register");
            ext = .sx;
        },
        else => unreachable,
    }

    read_to_D(.sp, 0, .byte, .stack);
    D8_to_LL(ext);
    LL_to_op_reg(.OA);
    next_cycle();

    SR_plus_literal_to_L(.sp, 1, .fresh, .no_flags);
    L_to_SR(.sp);
    load_and_exec_next_insn(2);
}

pub fn _E4A0_E4AF() void {
    encoding(.POP, .{ .Ra });
    //syntax("POP Ra");
    desc("Pop word from stack to 16b register");

    read_to_D(.sp, 0, .word, .stack);
    D_to_LL();
    LL_to_op_reg(.OA);
    next_cycle();

    SR_plus_literal_to_L(.sp, 2, .fresh, .no_flags);
    L_to_SR(.sp);
    load_and_exec_next_insn(2);
}

pub fn _E4B0_E4BF() void {
    encoding(.POP, .{ .Xa });
    //syntax("POP Xa");
    desc("Pop double word from stack to 32b register");

    read_to_D(.sp, 0, .word, .stack);
    D_to_L(.zx);
    L_to_SR(.temp_2);
    next_cycle();

    read_to_D(.sp, 2, .word, .stack);
    D_to_LH();
    SRL_to_LL(.temp_2);
    L_to_op_reg32(.OA);
    next_cycle();

    SR_plus_literal_to_L(.sp, 4, .fresh, .no_flags);
    L_to_SR(.sp);
    load_and_exec_next_insn(2);
}

pub fn _E4C0_E4CF() void {
    encoding(.PUSH, .{ .Ba });
    //syntax("PUSH Ba");
    desc("Push byte to stack from 16b register");

    op_reg_to_LL(.OA);
    write_from_LL(.sp, -1, .byte, .stack);
    next_cycle();

    SR_minus_literal_to_L(.sp, 1, .fresh, .no_flags);
    L_to_SR(.sp);
    load_and_exec_next_insn(2);
}

pub fn _E4D0_E4DF() void {
    encoding(.PUSH, .{ .Ra });
    //syntax("PUSH Ra");
    desc("Push word to stack from 16b register");

    op_reg_to_LL(.OA);
    write_from_LL(.sp, -2, .word, .stack);
    next_cycle();

    SR_minus_literal_to_L(.sp, 2, .fresh, .no_flags);
    L_to_SR(.sp);
    load_and_exec_next_insn(2);
}

pub fn _E4E0_E4EF() void {
    encoding(.PUSH, .{ .Xa });
    //syntax("PUSH Xa");
    desc("Push double word to stack from 32b register");

    op_reg_to_LL(.OA);
    write_from_LL(.sp, -4, .word, .stack);
    next_cycle();

    op_reg_to_LL(.OAxor1);
    write_from_LL(.sp, -2, .word, .stack);
    next_cycle();

    SR_minus_literal_to_L(.sp, 4, .fresh, .no_flags);
    L_to_SR(.sp);
    load_and_exec_next_insn(2);
}
