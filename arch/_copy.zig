const ib = @import("instruction_builder.zig");
const cb = @import("cycle_builder.zig");

const SignedOffsetForLiteral = @import("misc").SignedOffsetForLiteral;

const encoding = ib.encoding;
const getParameterConstant = ib.getParameterConstant;
const getParameterOffset = ib.getParameterOffset;
const IP_relative = ib.IP_relative;
const SP_relative = ib.SP_relative;
const Xa_relative = ib.Xa_relative;
const desc = ib.desc;
const next_cycle = ib.next_cycle;
const opcode = ib.opcode;
const opcode_high = ib.opcode_high;
const OB = ib.OB;
const kernel = ib.kernel;

const illegal_instruction = cb.illegal_instruction;
const literal_to_L = cb.literal_to_L;
const literal_to_LL = cb.literal_to_LL;
const IP_read_to_D = cb.IP_read_to_D;
const LL_to_op_reg = cb.LL_to_op_reg;
const L_to_op_reg32 = cb.L_to_op_reg32;
const L_to_SR = cb.L_to_SR;
const D_to_L = cb.D_to_L;
const D_to_LH = cb.D_to_LH;
const op_reg_to_LL = cb.op_reg_to_LL;
const op_reg_to_L = cb.op_reg_to_L;
const op_reg32_to_L = cb.op_reg32_to_L;
const load_and_exec_next_insn = cb.load_and_exec_next_insn;
const SR_plus_op_reg_to_L = cb.SR_plus_op_reg_to_L;
const SR_plus_literal_to_L = cb.SR_plus_literal_to_L;
const SR_plus_SRL_to_L = cb.SR_plus_SRL_to_L;
const SRL_to_LL = cb.SRL_to_LL;
const D_to_OB_OA = cb.D_to_OB_OA;
const SR_to_L = cb.SR_to_L;
const JL_to_LL_and_LH = cb.JL_to_LL_and_LH;
const op_reg_to_J = cb.op_reg_to_J;
const SR_minus_op_reg_to_L = cb.SR_minus_op_reg_to_L;
const STAT_to_L = cb.STAT_to_L;
const LL_to_ZNVC = cb.LL_to_ZNVC;

pub fn _4800_48FF() void {
    encoding(.C, .{ .Ra, .to, .Rb });
    //syntax("C Ra -> Rb");
    desc("Copy 16b register to another 16b register");

    op_reg_to_LL(.OA);
    LL_to_op_reg(.OB);
    load_and_exec_next_insn(2);
}

pub fn _4900_49FF() void {
    encoding(.C, .{ .RaU, .to, .Xb });
    //syntax("C URa -> Xb");
    desc("Copy unsigned value from 16b register to 32b register");

    op_reg_to_L(.OA, .zx);
    L_to_op_reg32(.OB);
    load_and_exec_next_insn(2);
}

pub fn _4A00_4AFF() void {
    encoding(.C, .{ .RaS, .to, .Xb });
    //syntax("C SRa -> Xb");
    desc("Copy signed value from 16b register to 32b register");

    op_reg_to_L(.OA, .sx);
    L_to_op_reg32(.OB);
    load_and_exec_next_insn(2);
}

pub fn _4B00_4BFF() void {
    encoding(.C, .{ .Xa, .to, .Xb });
    //syntax("C Xa -> Xb");
    desc("Copy 32b register to another 32b register");

    op_reg32_to_L(.OA);
    L_to_op_reg32(.OB);
    load_and_exec_next_insn(2);
}

pub fn _4C00_4CFF() void {
    encoding(.C, .{ IP_relative(.RaS), .to, .Xb });
    //syntax("C IP+SRa -> Xb");
    desc("Copy IP-relative address to 32b register");

    SR_plus_op_reg_to_L(.ip, .OA, .sx, .fresh, .no_flags);
    L_to_op_reg32(.OB);
    load_and_exec_next_insn(2);
}

pub fn _4D00_4DFF() void {
    encoding(.C, .{ SP_relative(.RaS), .to, .Xb });
    //syntax("C SP+SRa -> Xb");
    desc("Copy SP-relative address to 32b register");

    SR_plus_op_reg_to_L(.sp, .OA, .sx, .fresh, .no_flags);
    L_to_op_reg32(.OB);
    load_and_exec_next_insn(2);
}

pub fn _4E00_4EFF() void {
    encoding(.C, .{ IP_relative(.immb4s), .to, .Xa });
    //syntax("C IP+imm4[-8,7] -> Xa");
    desc("Copy IP-relative (immediate offset) address to 32b register");

    const offset = getParameterOffset(0);
    SR_plus_literal_to_L(.ip, offset, .fresh, .no_flags);
    L_to_op_reg32(.OA);
    load_and_exec_next_insn(2);
}

pub fn _4F00_4FFF() void {
    encoding(.C, .{ SP_relative(.immb4s), .to, .Xa });
    //syntax("C SP+imm4[-8,7] -> Xa");
    desc("Copy SP-relative (immediate offset) address to 32b register");

    const offset = getParameterOffset(0);
    SR_plus_literal_to_L(.sp, offset, .fresh, .no_flags);
    L_to_op_reg32(.OA);
    load_and_exec_next_insn(2);
}

pub fn _5800_580F() void {
    encoding(.C, .{ IP_relative(.imm16u), .to, .Xa });
    //syntax("C IP+imm[0,65535] -> Xa");
    desc("Copy IP-relative (immediate offset) address to 32b register");

    IP_read_to_D(2, .word);
    D_to_L(.zx);
    L_to_SR(.temp_1);
    next_cycle();

    SR_plus_SRL_to_L(.ip, .temp_1, .zx, .fresh, .no_flags);
    L_to_op_reg32(.OA);
    load_and_exec_next_insn(4);
}
pub fn _5810_581F() void {
    encoding(.C, .{ IP_relative(.imm16n), .to, .Xa });
    //syntax("C IP+imm[-65536,-1] -> Xa");
    desc("Copy IP-relative (immediate offset) address to 32b register");

    IP_read_to_D(2, .word);
    D_to_L(.zx);
    L_to_SR(.temp_1);
    next_cycle();

    SR_plus_SRL_to_L(.ip, .temp_1, ._1x, .fresh, .no_flags);
    L_to_op_reg32(.OA);
    load_and_exec_next_insn(4);
}

pub fn _5820_582F() void {
    encoding(.C, .{ SP_relative(.imm16u), .to, .Xa });
    //syntax("C SP+imm[0,65535] -> Xa");
    desc("Copy SP-relative (immediate offset) address to 32b register");

    IP_read_to_D(2, .word);
    D_to_L(.zx);
    L_to_SR(.temp_2);
    next_cycle();

    SR_plus_SRL_to_L(.sp, .temp_2, .zx, .fresh, .no_flags);
    L_to_op_reg32(.OA);
    load_and_exec_next_insn(4);
}
pub fn _5830_583F() void {
    encoding(.C, .{ SP_relative(.imm16n), .to, .Xa });
    //syntax("C SP+imm[-65536,-1] -> Xa");
    desc("Copy SP-relative (immediate offset) address to 32b register");

    IP_read_to_D(2, .word);
    D_to_L(.zx);
    L_to_SR(.temp_2);
    next_cycle();

    SR_plus_SRL_to_L(.sp, .temp_2, ._1x, .fresh, .no_flags);
    L_to_op_reg32(.OA);
    load_and_exec_next_insn(4);
}

pub fn _5C00_5CFF() void {
    encoding(.C, .{ .immb4u, .to, .Ra });
    //syntax("C imm4[0,15] -> Ra");
    desc("Copy immediate to 16b register");

    literal_to_LL(getParameterConstant(SignedOffsetForLiteral, 0));
    LL_to_op_reg(.OA);
    load_and_exec_next_insn(2);
}
pub fn _5D00_5DFF() void {
    encoding(.C, .{ .immb4n, .to, .Ra });
    //syntax("C imm4[-16,-1] -> Ra");
    desc("Copy immediate to 16b register");

    literal_to_LL(getParameterConstant(SignedOffsetForLiteral, 0));
    LL_to_op_reg(.OA);
    load_and_exec_next_insn(2);
}

pub fn _5E00_5EFF() void {
    encoding(.C, .{ .immb4u, .to, .Xa });
    //syntax("C imm4[0,15] -> Xa");
    desc("Copy immediate to 32b register");

    literal_to_L(getParameterConstant(SignedOffsetForLiteral, 0));
    L_to_op_reg32(.OA);
    load_and_exec_next_insn(2);
}
pub fn _5F00_5FFF() void {
    encoding(.C, .{ .immb4n, .to, .Xa });
    //syntax("C imm4[-16,-1] -> Xa");
    desc("Copy immediate to 32b register");

    literal_to_L(getParameterConstant(SignedOffsetForLiteral, 0));
    L_to_op_reg32(.OA);
    load_and_exec_next_insn(2);
}

pub fn _5840_58FF() void {
    switch (OB()) {
        0x4 => encoding(.C, .{ .imm_16, .to, .Ra }),
        0x5 => encoding(.C, .{ .imm_32, .to, .Ra }),
        0x6 => encoding(.C, .{ .imm_64, .to, .Ra }),
        0x7 => encoding(.C, .{ .imm_128, .to, .Ra }),
        0x8 => encoding(.C, .{ .imm_256, .to, .Ra }),
        0x9 => encoding(.C, .{ .imm_512, .to, .Ra }),
        0xA => encoding(.C, .{ .imm_1024, .to, .Ra }),
        0xB => encoding(.C, .{ .imm_2048, .to, .Ra }),
        0xC => encoding(.C, .{ .imm_4096, .to, .Ra }),
        0xD => encoding(.C, .{ .imm_8192, .to, .Ra }),
        0xE => encoding(.C, .{ .imm_16384, .to, .Ra }),
        0xF => encoding(.C, .{ .imm_32768, .to, .Ra }),
        else => unreachable,
    }
    desc("Copy immediate to 16b register");

    literal_to_L(getParameterConstant(i17, 0));
    LL_to_op_reg(.OA);
    load_and_exec_next_insn(2);
}

pub fn _FB00_FB0F() void {
    encoding(.C, .{ .imm8u, .to, .Ra });
    //syntax("C imm8[0,255] -> Ra");
    desc("Copy immediate to 16b register");

    IP_read_to_D(2, .byte);
    D_to_L(.zx);
    LL_to_op_reg(.OA);
    next_cycle();

    load_and_exec_next_insn(3);
}

pub fn _FB10_FB1F() void {
    encoding(.C, .{ .imm8u, .to, .Xa });
    //syntax("C imm8[0,255] -> Xa");
    desc("Copy immediate to 32b register");

    IP_read_to_D(2, .byte);
    D_to_L(.zx);
    L_to_op_reg32(.OA);
    next_cycle();

    load_and_exec_next_insn(3);
}

pub fn _FB20_FB2F() void {
    encoding(.C, .{ .imm16u, .to, .Ra });
    //syntax("C imm16[0,65535] -> Ra");
    desc("Copy immediate to 16b register");

    IP_read_to_D(2, .word);
    D_to_L(.zx);
    LL_to_op_reg(.OA);
    next_cycle();

    load_and_exec_next_insn(4);
}

pub fn _FB30_FB3F() void {
    encoding(.C, .{ .imm16u, .to, .Xa });
    //syntax("C imm16[0,65535] -> Xa");
    desc("Copy immediate to 32b register");

    IP_read_to_D(2, .word);
    D_to_L(.zx);
    L_to_op_reg32(.OA);
    next_cycle();

    load_and_exec_next_insn(4);
}

pub fn _FB40_FB4F() void {
    encoding(.C, .{ .imm16n, .to, .Xa });
    //syntax("C imm16[-65536,-1] -> Xa");
    desc("Copy immediate to 32b register");

    IP_read_to_D(2, .word);
    D_to_L(._1x);
    L_to_op_reg32(.OA);
    next_cycle();

    load_and_exec_next_insn(4);
}

pub fn _FB50_FB5F() void {
    encoding(.C, .{ .imm32, .to, .Xa });
    //syntax("C imm32[0,4294967295] -> Xa");
    desc("Copy immediate to 32b register");

    IP_read_to_D(2, .word);
    D_to_L(.zx);
    L_to_SR(.temp_1);
    next_cycle();

    IP_read_to_D(4, .word);
    D_to_LH();
    SRL_to_LL(.temp_1);
    L_to_op_reg32(.OA);
    next_cycle();

    load_and_exec_next_insn(6);
}

pub fn _FB60_FB6F() void {
    encoding(.C, .{ .Ra, .to, .Ra1, .to, .Rb1 });
    //syntax("C Ra -> Ra1 -> Rb1");
    desc("16b three register copy or swap");

    IP_read_to_D(2, .byte);
    D_to_OB_OA();
    op_reg_to_L(.OA, .zx);
    L_to_SR(.temp_1);
    next_cycle();

    op_reg_to_LL(.OA);
    LL_to_op_reg(.OB);
    next_cycle();

    SRL_to_LL(.temp_1);
    LL_to_op_reg(.OA);
    load_and_exec_next_insn(3);
}

pub fn _FB70_FB7F() void {
    encoding(.C, .{ .Xa, .to, .Xa1, .to, .Xb1 });
    //syntax("C Xa -> Xa1 -> Xb1");
    desc("32b three register copy or swap");

    IP_read_to_D(2, .byte);
    D_to_OB_OA();
    op_reg32_to_L(.OA);
    L_to_SR(.temp_1);
    next_cycle();

    op_reg32_to_L(.OA);
    L_to_op_reg32(.OB);
    next_cycle();

    SR_to_L(.temp_1);
    L_to_op_reg32(.OA);
    load_and_exec_next_insn(3);
}

pub fn _5900_59FF() void {
    encoding(.DUP, .{ .Ra, .to, .Xb });
    //syntax("DUP Ra -> Xb");
    desc("Concatenate 16b register with itself, storing result in 32b register");

    op_reg_to_J(.OB, .zx);
    JL_to_LL_and_LH();
    L_to_op_reg32(.OB);
    load_and_exec_next_insn(2);
}

pub fn _FBD0_FBDF() void {
    encoding(.C, .{ .STAT, .to, .Ra });
    //syntax("C STAT -> Ra");
    desc("Copy status register to 16b register");

    STAT_to_L();
    LL_to_op_reg(.OA);
    load_and_exec_next_insn(2);
}

pub fn _FBE0_FBEF() void {
    encoding(.C, .{ .Ra, .to, .STAT });
    //syntax("C Ra -> STAT");
    desc("Copy 16b register to status register (flags only)");

    op_reg_to_LL(.OA);
    LL_to_ZNVC();
    load_and_exec_next_insn(2);
}

pub fn _FAE0_FAEF() void {
    encoding(.C, .{ .RP, .to, Xa_relative(.I, .imm_0) });
    //syntax("C RP -> Xa");
    desc("Copy return pointer to 32b register");

    SR_to_L(.rp);
    L_to_op_reg32(.OA);
    load_and_exec_next_insn(2);
}

pub fn _FAF0_FAFF() void {
    encoding(.C, .{ Xa_relative(.I, .imm_0), .to, .RP });
    //syntax("C Xa -> RP");
    desc("Copy 32b register to return pointer");

    op_reg32_to_L(.OA);
    L_to_SR(.rp);
    load_and_exec_next_insn(2);
}

pub fn _FBF0_FBFF() void {
    encoding(.C, .{ .BP, .to, Xa_relative(.D, .imm_0) });
    //syntax("C BP -> Xa");
    desc("Copy base pointer to 32b register");

    SR_to_L(.bp);
    L_to_op_reg32(.OA);
    load_and_exec_next_insn(2);
}

pub fn _E4F0_E4FF() void {
    encoding(.C, .{ Xa_relative(.D, .imm_0), .to, .BP });
    //syntax("C Xa -> BP");
    desc("Copy 32b register to base pointer");

    op_reg32_to_L(.OA);
    L_to_SR(.bp);
    load_and_exec_next_insn(2);
}

pub fn _E460_E46F() void {
    encoding(.C, .{ .ASN, .to, .Xa });
    //syntax("C ASN -> Xa");
    desc("Copy address space number to 32b register");

    if (!kernel()) {
        illegal_instruction();
        return;
    }

    SR_to_L(.asn);
    L_to_op_reg32(.OA);
    load_and_exec_next_insn(2);
}

pub fn _E470_E47F() void {
    encoding(.C, .{ .Xa, .to, .ASN });
    //syntax("C Xa -> ASN");
    desc("Copy 32b register to address space number");

    if (!kernel()) {
        illegal_instruction();
        return;
    }

    op_reg32_to_L(.OA);
    L_to_SR(.asn);
    next_cycle();

    load_and_exec_next_insn(2);
}

pub fn _93B0_93BF() void {
    encoding(.C, .{ .KXP, .to, Xa_relative(.D, .imm_0) });
    //syntax("C KXP -> Xa");
    desc("Copy kernel context pointer to 32b register");

    if (!kernel()) {
        illegal_instruction();
        return;
    }

    SR_to_L(.kxp);
    L_to_op_reg32(.OA);
    load_and_exec_next_insn(2);
}

pub fn _93C0_93CF() void {
    encoding(.C, .{ Xa_relative(.D, .imm_0), .to, .KXP });
    //syntax("C Xa -> KXP");
    desc("Copy 32b register to kernel context pointer");

    if (!kernel()) {
        illegal_instruction();
        return;
    }

    op_reg32_to_L(.OA);
    L_to_SR(.kxp);
    next_cycle();

    load_and_exec_next_insn(2);
}

pub fn _93D0_93DF() void {
    encoding(.C, .{ .UXP, .to, Xa_relative(.D, .imm_0) });
    //syntax("C UXP -> Xa");
    desc("Copy user context pointer to 32b register");

    SR_to_L(.uxp);
    L_to_op_reg32(.OA);
    load_and_exec_next_insn(2);
}

pub fn _93E0_93EF() void {
    encoding(.C, .{ Xa_relative(.D, .imm_0), .to, .UXP });
    //syntax("C Xa -> UXP");
    desc("Copy 32b register to user context pointer");

    op_reg32_to_L(.OA);
    L_to_SR(.uxp);
    next_cycle();

    load_and_exec_next_insn(2);
}
