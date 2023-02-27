const ib = @import("instruction_builder.zig");
const cb = @import("cycle_builder.zig");
const ctrl = @import("control_signals");
const ie = @import("instruction_encoding");

const Mnemonic = ie.Mnemonic;

const getParameterConstant = ib.getParameterConstant;
const desc = ib.desc;
const encoding = ib.encoding;
const parameter = ib.parameter;
const next_cycle = ib.next_cycle;
const opcode = ib.opcode;
const opcode_high = ib.opcode_high;
const carry_borrow = ib.carry_borrow;
const OA = ib.OA;
const OB = ib.OB;

const op_reg_to_L = cb.op_reg_to_L;
const op_reg_plus_literal_to_LL = cb.op_reg_plus_literal_to_LL;
const op_reg32_plus_literal_to_L = cb.op_reg32_plus_literal_to_L;
const op_reg_plus_SRL_to_LL = cb.op_reg_plus_SRL_to_LL;
const op_reg_plus_op_reg_to_LL = cb.op_reg_plus_op_reg_to_LL;
const op_reg32_plus_SRL_to_L = cb.op_reg32_plus_SRL_to_L;
const op_reg32_plus_op_reg_to_L = cb.op_reg32_plus_op_reg_to_L;
const op_reg32_minus_op_reg_to_L = cb.op_reg32_minus_op_reg_to_L;
const op_reg32_minus_SRL_to_L = cb.op_reg32_minus_SRL_to_L;
const op_reg_logic_literal_to_LL = cb.op_reg_logic_literal_to_LL;
const op_reg_logic_op_reg_to_LL = cb.op_reg_logic_op_reg_to_LL;
const op_reg_logic_SRL_to_LL = cb.op_reg_logic_SRL_to_LL;
const op_reg32_shift_op_reg_to_L = cb.op_reg32_shift_op_reg_to_L;
const op_reg_shift_op_reg_to_LL = cb.op_reg_shift_op_reg_to_LL;
const op_reg_shift_literal_to_LL = cb.op_reg_shift_literal_to_LL;
const op_reg32_shift_literal_to_L = cb.op_reg32_shift_literal_to_L;
const op_reg_minus_op_reg_to_LL = cb.op_reg_minus_op_reg_to_LL;
const op_reg_minus_SRL_to_LL = cb.op_reg_minus_SRL_to_LL;
const op_reg_minus_literal_to_LL = cb.op_reg_minus_literal_to_LL;
const op_reg32_minus_literal_to_L = cb.op_reg32_minus_literal_to_L;
const SRL_minus_op_reg_to_LL = cb.SRL_minus_op_reg_to_LL;
const zero_minus_op_reg_to_LL = cb.zero_minus_op_reg_to_LL;
const load_and_exec_next_insn = cb.load_and_exec_next_insn;
const LL_to_op_reg = cb.LL_to_op_reg;
const L_to_op_reg32 = cb.L_to_op_reg32;
const L_to_SR = cb.L_to_SR;
const IP_read_to_D = cb.IP_read_to_D;
const D_to_L = cb.D_to_L;
const op_reg_mult_SRL_to_L = cb.op_reg_mult_SRL_to_L;
const op_reg_mult_SRL_to_LL = cb.op_reg_mult_SRL_to_LL;
const op_reg_mult_op_reg_to_L = cb.op_reg_mult_op_reg_to_L;
const op_reg_mult_op_reg_to_LL = cb.op_reg_mult_op_reg_to_LL;
const bitcount_op_reg_to_LL = cb.bitcount_op_reg_to_LL;
const L_to_reg32 = cb.L_to_reg32;

pub fn _7000_70FF() void {
    encoding(.ADD, .{ .Xa, .imm8s, .to, .Xb });
    //syntax("ADD Xa, imm8[-128,127] -> Xb");
    desc("Add signed 8b immediate to 32b regiser");

    IP_read_to_D(2, .byte);
    D_to_L(.sx);
    L_to_SR(.temp_1);
    next_cycle();

    op_reg32_plus_SRL_to_L(.OA, .temp_1, .sx, .fresh, .flags);
    L_to_op_reg32(.OB);
    load_and_exec_next_insn(3);
}

pub fn _7100_71FF() void {
    encoding(.ADD, .{ .Xa, .imm16u, .to, .Xb });
    //syntax("ADD Xa, imm16[0,65535] -> Xb");
    desc("Add signed 17b immediate to 32b register");

    IP_read_to_D(2, .word);
    D_to_L(.zx);
    L_to_SR(.temp_1);
    next_cycle();

    op_reg32_plus_SRL_to_L(.OA, .temp_1, .zx, .fresh, .flags);
    L_to_op_reg32(.OB);
    load_and_exec_next_insn(4);
}

pub fn _7200_72FF() void {
    encoding(.ADD, .{ .Xa, .imm16n, .to, .Xb });
    //syntax("ADD Xa, imm16[-65536,65535] -> Xb");
    desc("Add signed 17b immediate to 32b register");

    IP_read_to_D(2, .word);
    D_to_L(.zx);
    L_to_SR(.temp_1);
    next_cycle();

    op_reg32_plus_SRL_to_L(.OA, .temp_1, ._1x, .fresh, .flags);
    L_to_op_reg32(.OB);
    load_and_exec_next_insn(4);
}

pub fn _7300_74FF() void {
    var ext: cb.ZX_SX_or_1X = undefined;
    switch (opcode_high()) {
        0x73 => {
            encoding(.ADD, .{ .Xa, .RbU, .to, .Xa });
            //syntax("ADD Xa, URb -> Xa");
            desc("Add unsigned 16b register to 32b register");
            ext = .zx;
        },
        0x74 => {
            encoding(.ADD, .{ .Xa, .RbS, .to, .Xa });
            //syntax("ADD Xa, SRb -> Xa");
            desc("Add signed 16b register to 32b register");
            ext = .sx;
        },
        else => unreachable,
    }

    op_reg32_plus_op_reg_to_L(.OA, .OB, ext, .fresh, .flags);
    L_to_op_reg32(.OA);
    load_and_exec_next_insn(2);
}

pub fn _7500_76FF() void {
    var freshness: cb.ALU_Freshness = undefined;
    switch (opcode_high()) {
        0x75 => {
            encoding(.ADD, .{ .Ra, .imm8s, .to, .Rb });
            //syntax("ADD Ra, imm8[-128,127] -> Rb");
            desc("Add signed 8b immediate to 16b regiser");
            freshness = .fresh;
        },
        0x76 => {
            encoding(.ADDC, .{ .Ra, .imm8s, .to, .Rb });
            //syntax("ADDC Ra, imm8[-128,127] -> Rb");
            desc("Add signed 8b immediate to 16b regiser, with carry");
            freshness = .cont;
        },
        else => unreachable,
    }

    IP_read_to_D(2, .byte);
    D_to_L(.sx);
    L_to_SR(.temp_1);
    next_cycle();

    op_reg_plus_SRL_to_LL(.OA, .temp_1, freshness, .flags);
    LL_to_op_reg(.OB);
    load_and_exec_next_insn(3);
}

pub fn _7700_78FF() void {
    var freshness: cb.ALU_Freshness = undefined;
    switch (opcode_high()) {
        0x77 => {
            encoding(.ADD, .{ .Ra, .imm16, .to, .Rb });
            //syntax("ADD Ra, imm16[0,65535] -> Rb");
            desc("Add signed or unsigned 16b immediate to 16b regiser");
            freshness = .fresh;
        },
        0x78 => {
            encoding(.ADDC, .{ .Ra, .imm16, .to, .Rb });
            //syntax("ADDC Ra, imm16[0,65535] -> Rb");
            desc("Add signed or unsigned 16b immediate to 16b regiser, with carry");
            freshness = .cont;
        },
        else => unreachable,
    }

    IP_read_to_D(2, .word);
    D_to_L(.zx);
    L_to_SR(.temp_1);
    next_cycle();

    op_reg_plus_SRL_to_LL(.OA, .temp_1, freshness, .flags);
    LL_to_op_reg(.OB);
    load_and_exec_next_insn(4);
}

pub fn _7900_7AFF() void {
    var freshness: cb.ALU_Freshness = undefined;
    switch (opcode_high()) {
        0x79 => {
            encoding(.ADD, .{ .Ra, .Rb, .to, .Ra });
            //syntax("ADD Ra, Rb -> Ra");
            desc("Add 16b register to another 16b regiser");
            freshness = .fresh;
        },
        0x7A => {
            encoding(.ADDC, .{ .Ra, .Rb, .to, .Ra });
            //syntax("ADDC Ra, Rb -> Ra");
            desc("Add 16b register to another 16b regiser, with carry");
            freshness = .cont;
        },
        else => unreachable,
    }

    op_reg_plus_op_reg_to_LL(.OA, .OB, freshness, .flags);
    LL_to_op_reg(.OA);
    load_and_exec_next_insn(2);
}

pub fn _8000_81FF() void {
    var freshness: cb.ALU_Freshness = undefined;
    switch (opcode_high()) {
        0x80 => {
            encoding(.CMP, .{ .Ra, .Rb });
            //syntax("CMP Ra, Rb");
            desc("Compare register to another register");
            freshness = .fresh;
        },
        0x81 => {
            encoding(.CMPB, .{ .Ra, .Rb });
            //syntax("CMPB Ra, Rb");
            desc("Compare register to another register, with borrow");
            freshness = .cont;
        },
        else => unreachable,
    }

    op_reg_minus_op_reg_to_LL(.OA, .OB, freshness, .flags);
    load_and_exec_next_insn(2);
}

pub fn _8B00_8CFF() void {
    var ext: cb.ZX_SX_or_1X = undefined;
    switch (opcode_high()) {
        0x8B => {
            encoding(.CMP, .{ .Xa, .RbU });
            //syntax("CMP Xa, URb");
            desc("Compare 32b register to 16b register, unsigned");
            ext = .zx;
        },
        0x8C => {
            encoding(.CMP, .{ .Xa, .RbS });
            //syntax("CMP Xa, SRb");
            desc("Compare 32b register to 16b register, signed");
            ext = .sx;
        },
        else => unreachable,
    }

    op_reg32_minus_op_reg_to_L(.OA, .OB, ext, .fresh, .flags);
    load_and_exec_next_insn(2);
}

pub fn _FA40_FA5F() void {
    var freshness: cb.ALU_Freshness = undefined;
    switch (OB()) {
        0x4 => {
            encoding(.CMP, .{ .Ra, .imm16 });
            //syntax("CMP Ra, imm16[0,65535]");
            desc("Compare 16b register to immediate");
            freshness = .fresh;
        },
        0x5 => {
            encoding(.CMPB, .{ .Ra, .imm16 });
            //syntax("CMPB Ra, imm16[0,65535]");
            desc("Compare 16b register to immediate, with borrow");
            freshness = .cont;
        },
        else => unreachable,
    }

    IP_read_to_D(2, .word);
    D_to_L(.zx);
    L_to_SR(.temp_1);
    next_cycle();

    op_reg_minus_SRL_to_LL(.OA, .temp_1, freshness, .flags);
    load_and_exec_next_insn(4);
}

pub fn _FA60_FA6F() void {
    encoding(.CMP, .{ .Xa, .imm16u });
    //syntax("CMP Xa, imm16[0,65535]");
    desc("Compare 32b register to immediate");

    IP_read_to_D(2, .word);
    D_to_L(.zx);
    L_to_SR(.temp_1);
    next_cycle();

    op_reg32_minus_SRL_to_L(.OA, .temp_1, .zx, .fresh, .flags);
    load_and_exec_next_insn(4);
}

pub fn _FA70_FA7F() void {
    encoding(.CMP, .{ .Xa, .imm16n });
    //syntax("CMP Xa, imm16[-65536,-1]");
    desc("Compare 32b register to immediate");

    IP_read_to_D(2, .word);
    D_to_L(.zx);
    L_to_SR(.temp_1);
    next_cycle();

    op_reg32_minus_SRL_to_L(.OA, .temp_1, ._1x, .fresh, .flags);
    load_and_exec_next_insn(4);
}

pub fn _8300_84FF() void {
    var ext: cb.ZX_SX_or_1X = undefined;
    switch (opcode_high()) {
        0x83 => {
            encoding(.SUB, .{ .Xa, .RbU, .to, .Xa });
            //syntax("SUB Xa, URb -> Xa");
            desc("Subtract unsigned 16b register from 32b register");
            ext = .zx;
        },
        0x84 => {
            encoding(.SUB, .{ .Xa, .RbS, .to, .Xa });
            //syntax("SUB Xa, SRb -> Xa");
            desc("Subtract signed 16b register from 32b register");
            ext = .sx;
        },
        else => unreachable,
    }

    op_reg32_minus_op_reg_to_L(.OA, .OB, ext, .fresh, .flags);
    L_to_op_reg32(.OA);
    load_and_exec_next_insn(2);
}

pub fn _8500_86FF() void {
    var freshness: cb.ALU_Freshness = undefined;
    switch (opcode_high()) {
        0x85 => {
            encoding(.SUB, .{ .imm8s, .Ra, .to, .Rb });
            //syntax("SUB imm8[-128,127], Ra -> Rb");
            desc("Subtract register from immediate and store result in another register");
            freshness = .fresh;
        },
        0x86 => {
            encoding(.SUBB, .{ .imm8s, .Ra, .to, .Rb });
            //syntax("SUBB imm8[-128,127], Ra -> Rb");
            desc("Subtract register from immediate and store result in another register, with borrow");
            freshness = .cont;
        },
        else => unreachable,
    }

    IP_read_to_D(2, .byte);
    D_to_L(.sx);
    L_to_SR(.temp_1);
    next_cycle();

    SRL_minus_op_reg_to_LL(.temp_1, .OA, freshness, .flags);
    LL_to_op_reg(.OB);
    load_and_exec_next_insn(3);
}

pub fn _8700_88FF() void {
    var freshness: cb.ALU_Freshness = undefined;
    switch (opcode_high()) {
        0x87 => {
            encoding(.SUB, .{ .imm16, .Ra, .to, .Rb });
            //syntax("SUB imm16[0,65535], Ra -> Rb");
            desc("Subtract register from immediate and store result in another register");
            freshness = .fresh;
        },
        0x88 => {
            encoding(.SUBB, .{ .imm16, .Ra, .to, .Rb });
            //syntax("SUBB imm16[0,65535], Ra -> Rb");
            desc("Subtract register from immediate and store result in another register, with borrow");
            freshness = .cont;
        },
        else => unreachable,
    }

    IP_read_to_D(2, .word);
    D_to_L(.zx);
    L_to_SR(.temp_1);
    next_cycle();

    SRL_minus_op_reg_to_LL(.temp_1, .OA, freshness, .flags);
    LL_to_op_reg(.OB);
    load_and_exec_next_insn(4);
}

pub fn _8900_8AFF() void {
    var freshness: cb.ALU_Freshness = undefined;
    switch (opcode_high()) {
        0x89 => {
            encoding(.SUB, .{ .Ra, .Rb, .to, .Ra });
            //syntax("SUB Ra, Rb -> Ra");
            desc("Subtract register from another register");
            freshness = .fresh;
        },
        0x8A => {
            encoding(.SUBB, .{ .Ra, .Rb, .to, .Ra });
            //syntax("SUBB Ra, Rb -> Ra");
            desc("Subtract register from another register, with borrow");
            freshness = .cont;
        },
        else => unreachable,
    }

    op_reg_minus_op_reg_to_LL(.OA, .OB, freshness, .flags);
    LL_to_op_reg(.OA);
    load_and_exec_next_insn(2);
}

pub fn _9300_931F() void {
    var freshness: cb.ALU_Freshness = undefined;
    switch (OB()) {
        0x0 => {
            encoding(.INC, .{ .Ra, .to, .Ra });
            //syntax("INC Ra -> Ra");
            desc("Increment 16b register");
            freshness = .fresh;
        },
        0x1 => {
            encoding(.INCC, .{ .Ra, .to, .Ra });
            //syntax("INCC Ra -> Ra");
            desc("Increment 16b register, with carry");
            freshness = .cont;
        },
        else => unreachable,
    }

    op_reg_plus_literal_to_LL(.OA, 1, freshness, .flags);
    LL_to_op_reg(.OA);
    load_and_exec_next_insn(2);
}

pub fn _9320_933F() void {
    var freshness: cb.ALU_Freshness = undefined;
    switch (OB()) {
        0x2 => {
            encoding(.INC, .{ .Xa, .to, .Xa });
            //syntax("INC Xa -> Xa");
            desc("Increment 32b register");
            freshness = .fresh;
        },
        0x3 => {
            encoding(.INCC, .{ .Xa, .to, .Xa });
            //syntax("INCC Xa -> Xa");
            desc("Increment 32b register, with carry");
            freshness = .cont;
        },
        else => unreachable,
    }

    op_reg32_plus_literal_to_L(.OA, 1, freshness, .flags);
    L_to_op_reg32(.OA);
    load_and_exec_next_insn(2);
}

pub fn _9340_935F() void {
    var freshness: cb.ALU_Freshness = undefined;
    switch (OB()) {
        0x4 => {
            encoding(.DEC, .{ .Ra, .to, .Ra });
            //syntax("DEC Ra -> Ra");
            desc("Decrement 16b register");
            freshness = .fresh;
        },
        0x5 => {
            encoding(.DECB, .{ .Ra, .to, .Ra });
            //syntax("DECB Ra -> Ra");
            desc("Decrement 16b register, with borrow");
            freshness = .cont;
        },
        else => unreachable,
    }

    op_reg_minus_literal_to_LL(.OA, 1, freshness, .flags);
    LL_to_op_reg(.OA);
    load_and_exec_next_insn(2);
}

pub fn _9360_937F() void {
    var freshness: cb.ALU_Freshness = undefined;
    switch (OB()) {
        0x6 => {
            encoding(.DEC, .{ .Xa, .to, .Xa });
            //syntax("DEC Xa -> Xa");
            desc("Decrement 32b register");
            freshness = .fresh;
        },
        0x7 => {
            encoding(.DECB, .{ .Xa, .to, .Xa });
            //syntax("DECB Xa -> Xa");
            desc("Decrement 32b register, with borrow");
            freshness = .cont;
        },
        else => unreachable,
    }

    op_reg32_minus_literal_to_L(.OA, 1, freshness, .flags);
    L_to_op_reg32(.OA);
    load_and_exec_next_insn(2);
}

pub fn _9380_939F() void {
    var freshness: cb.ALU_Freshness = undefined;
    switch (OB()) {
        0x8 => {
            encoding(.NEG, .{ .Ra, .to, .Ra });
            //syntax("NEG Ra -> Ra");
            desc("Negate 16b register");
            freshness = .fresh;
        },
        0x9 => {
            encoding(.NEGB, .{ .Ra, .to, .Ra });
            //syntax("NEGB Ra -> Ra");
            desc("Negate 16b register, with borrow");
            freshness = .cont;
        },
        else => unreachable,
    }

    zero_minus_op_reg_to_LL(.OA, freshness, .flags);
    L_to_op_reg32(.OA);
    load_and_exec_next_insn(2);
}

pub fn _93A0_93AF() void {
    encoding(.NOT, .{ .Ra, .to, .Ra });
    //syntax("NOT Ra -> Ra");
    desc("Bitwise complement of 16b register");

    op_reg_logic_literal_to_LL(.OA, .JL_xor_K, -1, .fresh, .flags);
    LL_to_op_reg(.OA);
    load_and_exec_next_insn(2);
}

pub fn _B000_B6FF() void {
    var op: ctrl.Logic_Mode = undefined;
    var mn: Mnemonic = undefined;
    var d: []const u8 = undefined;
    switch (opcode_high()) {
        0xB0 => { mn = .XOR;    op = .JL_xor_K;     d = "Bitwise XOR of two 16b registers";},
        0xB1 => { mn = .XNOR;   op = .JL_xnor_K;    d = "Bitwise XNOR of two 16b registers";},
        0xB2 => { mn = .OR;     op = .JL_or_K;      d = "Bitwise OR of two 16b registers";},
        0xB3 => { mn = .NOR;    op = .JL_nor_K;     d = "Bitwise NOR of two 16b registers";},
        0xB4 => { mn = .AND;    op = .JL_and_K;     d = "Bitwise AND of two 16b registers";},
        0xB5 => { mn = .NAND;   op = .JL_nand_K;    d = "Bitwise NAND of two 16b registers";},
        0xB6 => { mn = .ANDNOT; op = .JL_and_not_K; d = "Bitwise AND of two 16b registers (second register is complemented)";},
        else => unreachable,
    }

    encoding(mn, .{ .Ra, .Rb, .to, .Ra });
    desc(d);

    op_reg_logic_op_reg_to_LL(.OA, op, .OB, .fresh, .flags);
    LL_to_op_reg(.OA);
    load_and_exec_next_insn(2);
}

pub fn _C100_C5FF() void {
    var op: ctrl.Logic_Mode = undefined;
    var mn: Mnemonic = undefined;
    var d: []const u8 = undefined;
    switch (opcode_high()) {
        0xC1 => { mn = .XOR;  op = .JL_xor_K;  d = "Bitwise XOR of 16b register and immediate"; },
        0xC2 => { mn = .OR;   op = .JL_or_K;   d = "Bitwise OR of 16b register and immediate"; },
        0xC3 => { mn = .NOR;  op = .JL_nor_K;  d = "Bitwise NOR of 16b register and immediate"; },
        0xC4 => { mn = .AND;  op = .JL_and_K;  d = "Bitwise AND of 16b register and immediate"; },
        0xC5 => { mn = .NAND; op = .JL_nand_K; d = "Bitwise NAND of 16b register and immediate"; },
        else => unreachable,
    }

    encoding(mn, .{ .Ra, .imm16, .to, .Rb });
    desc(d);

    IP_read_to_D(2, .word);
    D_to_L(.zx);
    L_to_SR(.temp_1);
    next_cycle();

    op_reg_logic_SRL_to_LL(.OA, op, .temp_1, .fresh, .flags);
    LL_to_op_reg(.OB);
    load_and_exec_next_insn(4);
}

pub fn _C600_C7FF() void {
    var freshness: cb.ALU_Freshness = undefined;
    switch (opcode_high()) {
        0xC6 => {
            encoding(.TEST, .{ .Ra, .Rb });
            //syntax("TEST Ra, Rb");
            desc("Set Z and N flags based on bitwise AND of two 16b registers");
            freshness = .fresh;
        },
        0xC7 => {
            encoding(.TESTZ, .{ .Ra, .Rb });
            //syntax("TESTZ Ra, Rb");
            desc("Set Z and N flags based on bitwise AND of two 16b registers (don't set Z if already clear)");
            freshness = .cont;
        },
        else => unreachable,
    }

    op_reg_logic_op_reg_to_LL(.OA, .JL_and_K, .OB, freshness, .flags);
    load_and_exec_next_insn(2);
}

pub fn _9100_91FF() void {
    encoding(.TESTB, .{ .Ra, .immb4u });
    //syntax("TESTB Ra, immb[0,15]");
    desc("Set Z and N flags according to single bit from 16b register");

    op_reg_logic_literal_to_LL(.OA, .JL_and_K, @as(u16, 1) << OB(), .fresh, .flags);
    load_and_exec_next_insn(2);
}

pub fn _9200_92FF() void {
    encoding(.TESTBZ, .{ .Ra, .immb4u });
    //syntax("TESTBZ Ra, immb[0,15]");
    desc("Set Z and N flags according to single bit from 16b register, without setting the Z flag if it is already clear");

    op_reg_logic_literal_to_LL(.OA, .JL_and_K, @as(u16, 1) << OB(), .cont, .flags);
    load_and_exec_next_insn(2);
}

pub fn _A000_A0FF() void {
    encoding(.CLRB, .{ .Ra, .immb4u, .to, .Ra });
    //syntax("CLRB Ra, immb[0,15] -> Ra");
    desc("Clear single bit in 16b register");

    op_reg_logic_literal_to_LL(.OA, .JL_and_not_K, @as(u16, 1) << OB(), .fresh, .flags);
    LL_to_op_reg(.OA);
    load_and_exec_next_insn(2);
}

pub fn _A100_A1FF() void {
    encoding(.SETB, .{ .Ra, .immb4u, .to, .Ra });
    //syntax("SETB Ra, immb[0,15] -> Ra");
    desc("Set single bit in 16b register");

    op_reg_logic_literal_to_LL(.OA, .JL_or_K, @as(u16, 1) << OB(), .fresh, .flags);
    LL_to_op_reg(.OA);
    load_and_exec_next_insn(2);
}

pub fn _A200_A2FF() void {
    encoding(.TGLB, .{ .Ra, .immb4u, .to, .Ra });
    //syntax("TGLB Ra, immb[0,15] -> Ra");
    desc("Toggle single bit in 16b register");

    op_reg_logic_literal_to_LL(.OA, .JL_xor_K, @as(u16, 1) << OB(), .fresh, .flags);
    LL_to_op_reg(.OA);
    load_and_exec_next_insn(2);
}

pub fn _D000_D1FF() void {
    var dir: cb.Shift_Dir = undefined;
    switch (opcode_high()) {
        0xD0 => {
            encoding(.SHR, .{ .XaU, .Rb, .to, .XaU });
            //syntax("SHR UXa, Rb -> UXa");
            desc("Right shift 32b register 0-31 bits, zero extending");
            dir = .right;
        },
        0xD1 => {
            encoding(.SHL, .{ .Xa, .Rb, .to, .Xa });
            //syntax("SHL Xa, Rb -> Xa");
            desc("Left shift 32b register 0-31 bits");
            dir = .left;
        },
        else => unreachable,
    }

    op_reg32_shift_op_reg_to_L(.OA, dir, .OB, .flags);
    L_to_op_reg32(.OA);
    load_and_exec_next_insn(2);
}

pub fn _D200_D4FF() void {
    var dir: cb.Shift_Dir = undefined;
    var ext: cb.ZX_SX_or_1X = undefined;
    switch (opcode_high()) {
        0xD2 => {
            encoding(.SHR, .{ .RaU, .Rb, .to, .RaU });
            //syntax("SHR URa, Rb -> Ra");
            desc("Shift right 16b register 0-15 bits, unsigned");
            dir = .right;
            ext = .zx;
        },
        0xD3 => {
            encoding(.SHR, .{ .RaS, .Rb, .to, .RaS });
            //syntax("SHR SRa, Rb -> Ra");
            desc("Shift right 16b register 0-15 bits, signed");
            dir = .right;
            ext = .sx;
        },
        0xD4 => {
            encoding(.SHL, .{ .Ra, .Rb, .to, .Ra });
            //syntax("SHL Ra, Rb -> Ra");
            desc("Shift left 16b register 0-15 bits");
            dir = .left;
            ext = .zx;
        },
        else => unreachable,
    }

    op_reg_shift_op_reg_to_LL(.OA, ext, dir, .OB, .flags);
    LL_to_op_reg(.OA);
    load_and_exec_next_insn(2);
}

pub fn _D500_D7FF() void {
    var dir: cb.Shift_Dir = undefined;
    var ext: cb.ZX_SX_or_1X = undefined;
    switch (opcode_high()) {
        0xD5 => {
            encoding(.SHR, .{ .RaU, .immb4u, .to, .RaU });
            //syntax("SHR URa, immb[0,15] -> Ra");
            desc("Shift right 16b register 0-15 bits, unsigned");
            dir = .right;
            ext = .zx;
        },
        0xD6 => {
            encoding(.SHR, .{ .RaS, .immb4u, .to, .RaS });
            //syntax("SHR SRa, immb[0,15] -> Ra");
            desc("Shift right 16b register 0-15 bits, signed");
            dir = .right;
            ext = .sx;
        },
        0xD7 => {
            encoding(.SHL, .{ .Ra, .immb4u, .to, .Ra });
            //syntax("SHL Ra, immb[0,15] -> Ra");
            desc("Shift left 16b register 0-15 bits");
            dir = .left;
            ext = .zx;
        },
        else => unreachable,
    }

    const amount = getParameterConstant(u4, 1);
    op_reg_shift_literal_to_LL(.OA, ext, dir, amount, .flags);
    LL_to_op_reg(.OA);
    load_and_exec_next_insn(2);
}

pub fn _E000_E3FF() void {
    var dir: cb.Shift_Dir = undefined;
    switch (opcode_high()) {
        0xE0 => {
            encoding(.SHR, .{ .XaU, .immb4u, .to, .XaU });
            //syntax("SHR UXa, immb[0,15] -> UXa");
            desc("Right shift 32b register 0-31 bits, zero extending");
            dir = .right;
        },
        0xE1 => {
            encoding(.SHR, .{ .XaU, .immb4_16_31, .to, .XaU });
            //syntax("SHR UXa, immb[16,31] -> UXa");
            desc("Right shift 32b register 0-31 bits, zero extending");
            dir = .right;
        },
        0xE2 => {
            encoding(.SHL, .{ .Xa, .immb4u, .to, .Xa });
            //syntax("SHL Xa, immb[0,15] -> Xa");
            desc("Left shift 32b register 0-31 bits");
            dir = .left;
        },
        0xE3 => {
            encoding(.SHL, .{ .Xa, .immb4_16_31, .to, .Xa });
            //syntax("SHL Xa, immb[16,31] -> Xa");
            desc("Left shift 32b register 0-31 bits");
            dir = .left;
        },
        else => unreachable,
    }

    const amount = getParameterConstant(u5, 1);
    op_reg32_shift_literal_to_L(.OA, dir, amount, .flags);
    L_to_op_reg32(.OA);
    load_and_exec_next_insn(2);
}

pub fn _0900_090F() void {
    encoding(.SHRC, .{ .Ra, .to, .Ra });
    desc("Shift right 16b register 1 bit, shift Carry into MSB");

    op_reg_shift_literal_to_LL(.OA, (if (carry_borrow()) ._1x else .zx), .right, 1, .flags);
    LL_to_op_reg(.OA);
    load_and_exec_next_insn(2);
}

pub fn _0910_091F() void {
    encoding(.SHLC, .{ .Ra, .to, .Ra });
    desc("Shift left 16b register 1 bit, shift Carry into LSB");

    op_reg_shift_literal_to_LL(.OA, (if (carry_borrow()) ._1x else .zx), .left, 1, .flags);
    LL_to_op_reg(.OA);
    load_and_exec_next_insn(2);
}

pub fn _F000_F0FF() void {
    encoding(.MUL, .{ .Ra, .imm16, .to, .Rb });
    //syntax("MUL Ra, imm16[-32768,32767] -> Rb");
    desc("Multiply 16b register by immediate, truncate result to 16b and store in another register");

    IP_read_to_D(2, .word);
    D_to_L(.zx);
    L_to_SR(.temp_1);
    next_cycle();

    op_reg_mult_SRL_to_LL(.OA, .zx, .temp_1, .zx, .normal, .flags);
    LL_to_op_reg(.OB);
    load_and_exec_next_insn(4);
}

pub fn _F100_F1FF() void {
    encoding(.MUL, .{ .Ra, .Rb, .to, .Ra });
    //syntax("MUL Ra, Rb -> Ra");
    desc("Multiply 16b registers, truncate result to 16b");

    op_reg_mult_op_reg_to_LL(.OA, .zx, .OB, .zx, .normal, .flags);
    LL_to_op_reg(.OA);
    load_and_exec_next_insn(2);
}

pub fn _F200_F2FF() void {
    encoding(.MULH, .{ .RaU, .RbU, .to, .RaU });
    //syntax("MULH URa, URb -> Ra");
    desc("Multiply 16b unsigned registers, store high 16b of result");

    op_reg_mult_op_reg_to_LL(.OA, .zx, .OB, .zx, .swap, .flags);
    LL_to_op_reg(.OA);
    load_and_exec_next_insn(2);
}

pub fn _F300_F3FF() void {
    encoding(.MULH, .{ .RaU, .RbS, .to, .RaS });
    //syntax("MULH URa, SRb -> SRa");
    desc("Multiply 16b unsigned registers, store high 16b of result");

    op_reg_mult_op_reg_to_LL(.OA, .sx, .OB, .zx, .swap, .flags);
    LL_to_op_reg(.OA);
    load_and_exec_next_insn(2);
}

pub fn _F400_F4FF() void {
    encoding(.MULH, .{ .RaS, .RbU, .to, .RaS });
    //syntax("MULH SRa, URb -> Ra");
    desc("Multiply 16b unsigned registers, store high 16b of result");

    op_reg_mult_op_reg_to_LL(.OA, .sx, .OB, .zx, .swap, .flags);
    LL_to_op_reg(.OA);
    load_and_exec_next_insn(2);
}

pub fn _F500_F5FF() void {
    encoding(.MULH, .{ .RaS, .RbS, .to, .RaS });
    //syntax("MULH SRa, SRb -> Ra");
    desc("Multiply 16b unsigned registers, store high 16b of result");

    op_reg_mult_op_reg_to_LL(.OA, .sx, .OB, .sx, .swap, .flags);
    LL_to_op_reg(.OA);
    load_and_exec_next_insn(2);
}

pub fn _F600_F8FF() void {
    var ext_l: cb.ZX_or_SX = undefined;
    var ext_r: cb.ZX_or_SX = undefined;
    switch (opcode_high()) {
        0xF6 => {
            encoding(.MUL, .{ .RaU, .RbU, .to, .X0U });
            //syntax("MUL URa, URb -> UX0");
            desc("Multiply 16b unsigned registers, store 32b result");
            ext_l = .zx;
            ext_r = .zx;
        },
        0xF7 => {
            encoding(.MUL, .{ .RaS, .RbS, .to, .X0S });
            //syntax("MUL SRa, SRb -> SX0");
            desc("Multiply 16b signed registers, store 32b result");
            ext_l = .sx;
            ext_r = .sx;
        },
        0xF8 => {
            encoding(.MUL, .{ .RaU, .RbS, .to, .X0S });
            //syntax("MUL URa, SRb -> SX0");
            desc("Multiply 16b unsigned and signed registers, store 32b result");
            ext_l = .zx;
            ext_r = .sx;
        },
        else => unreachable,
    }

    op_reg_mult_op_reg_to_L(.OA, ext_l, .OB, ext_r, .flags);
    L_to_reg32(0);
    load_and_exec_next_insn(2);
}

pub fn _E400_E40F() void {
    encoding(.CB, .{ .Ra, .to, .Ra });
    //syntax("CB Ra -> Ra");
    desc("Count set bits in 16b register");

    bitcount_op_reg_to_LL(.OA, .all, 1, .flags);
    LL_to_op_reg(.OA);
    load_and_exec_next_insn(2);
}

pub fn _E410_E41F() void {
    encoding(.CZ, .{ .Ra, .to, .Ra });
    //syntax("CZ Ra -> Ra");
    desc("Count zero bits in 16b register");

    bitcount_op_reg_to_LL(.OA, .all, 0, .flags);
    LL_to_op_reg(.OA);
    load_and_exec_next_insn(2);
}

pub fn _E420_E42F() void {
    encoding(.CLB, .{ .Ra, .to, .Ra });
    //syntax("CLB Ra -> Ra");
    desc("Count leading (most significant) set bits in 16b register");

    bitcount_op_reg_to_LL(.OA, .leading, 1, .flags);
    LL_to_op_reg(.OA);
    load_and_exec_next_insn(2);
}

pub fn _E430_E43F() void {
    encoding(.CLZ, .{ .Ra, .to, .Ra });
    //syntax("CLZ Ra -> Ra");
    desc("Count leading (most significant) zero bits in 16b register");

    bitcount_op_reg_to_LL(.OA, .leading, 0, .flags);
    LL_to_op_reg(.OA);
    load_and_exec_next_insn(2);
}

pub fn _E440_E44F() void {
    encoding(.CTB, .{ .Ra, .to, .Ra });
    //syntax("CTB Ra -> Ra");
    desc("Count trailing (least significant) set bits in 16b register");

    bitcount_op_reg_to_LL(.OA, .trailing, 1, .flags);
    LL_to_op_reg(.OA);
    load_and_exec_next_insn(2);
}

pub fn _E450_E45F() void {
    encoding(.CTZ, .{ .Ra, .to, .Ra });
    //syntax("CTZ Ra -> Ra");
    desc("Count trailing (least significant) zero bits in 16b register");

    bitcount_op_reg_to_LL(.OA, .trailing, 0, .flags);
    LL_to_op_reg(.OA);
    load_and_exec_next_insn(2);
}
