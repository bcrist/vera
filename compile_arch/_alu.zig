const ib = @import("instruction_builder.zig");
const cb = @import("cycle_builder.zig");
const ControlSignals = @import("ControlSignals");
const isa = @import("isa_types");
const ie = @import("isa_encoding");

const Mnemonic = isa.Mnemonic;

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
    var ext: cb.ZeroSignOrOneExtension = undefined;
    switch (opcode_high()) {
        0x73 => {
            encoding(.ADD, .{ .Xa, .RbU, .to, .Xa });
            desc("Add unsigned 16b register to 32b register");
            ext = .zx;
        },
        0x74 => {
            encoding(.ADD, .{ .Xa, .RbS, .to, .Xa });
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
    var freshness: cb.Freshness = undefined;
    switch (opcode_high()) {
        0x75 => {
            encoding(.ADD, .{ .Ra, .imm8s, .to, .Rb });
            desc("Add signed 8b immediate to 16b regiser");
            freshness = .fresh;
        },
        0x76 => {
            encoding(.ADDC, .{ .Ra, .imm8s, .to, .Rb });
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

pub fn _7700_77FF() void {
    encoding(.ADD, .{ .RaU, .imm16u, .to, .RbU });
    desc("Add unsigned 16b immediate to 16b regiser");

    IP_read_to_D(2, .word);
    D_to_L(.zx);
    L_to_SR(.temp_1);
    next_cycle();

    op_reg_plus_SRL_to_LL(.OA, .temp_1, .fresh, .flags);
    LL_to_op_reg(.OB);
    load_and_exec_next_insn(4);
}
pub fn _alias_7700_77FF_signed() void {
    encoding(.ADD, .{ .RaS, .imm16s, .to, .RbS });
    desc("Add signed 16b immediate to 16b regiser");
}

pub fn _7800_78FF() void {
    encoding(.ADDC, .{ .RaU, .imm16u, .to, .RbU });
    desc("Add unsigned 16b immediate to 16b regiser, with carry");

    IP_read_to_D(2, .word);
    D_to_L(.zx);
    L_to_SR(.temp_1);
    next_cycle();

    op_reg_plus_SRL_to_LL(.OA, .temp_1, .cont, .flags);
    LL_to_op_reg(.OB);
    load_and_exec_next_insn(4);
}
pub fn _alias_7800_78FF_signed() void {
    encoding(.ADDC, .{ .RaS, .imm16s, .to, .RbS });
    desc("Add signed 16b immediate to 16b regiser, with carry");
}

pub fn _7900_7AFF() void {
    var freshness: cb.Freshness = undefined;
    switch (opcode_high()) {
        0x79 => {
            encoding(.ADD, .{ .Ra, .Rb, .to, .Ra });
            desc("Add 16b register to another 16b regiser");
            freshness = .fresh;
        },
        0x7A => {
            encoding(.ADDC, .{ .Ra, .Rb, .to, .Ra });
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
    var freshness: cb.Freshness = undefined;
    switch (opcode_high()) {
        0x80 => {
            encoding(.CMP, .{ .Ra, .Rb });
            desc("Compare register to another register");
            freshness = .fresh;
        },
        0x81 => {
            encoding(.CMPB, .{ .Ra, .Rb });
            desc("Compare register to another register, with borrow");
            freshness = .cont;
        },
        else => unreachable,
    }

    op_reg_minus_op_reg_to_LL(.OA, .OB, freshness, .flags);
    load_and_exec_next_insn(2);
}

pub fn _8B00_8CFF() void {
    var ext: cb.ZeroSignOrOneExtension = undefined;
    switch (opcode_high()) {
        0x8B => {
            encoding(.CMP, .{ .Xa, .RbU });
            desc("Compare 32b register to 16b register, unsigned");
            ext = .zx;
        },
        0x8C => {
            encoding(.CMP, .{ .Xa, .RbS });
            desc("Compare 32b register to 16b register, signed");
            ext = .sx;
        },
        else => unreachable,
    }

    op_reg32_minus_op_reg_to_L(.OA, .OB, ext, .fresh, .flags);
    load_and_exec_next_insn(2);
}

pub fn _FA40_FA4F() void {
    encoding(.CMP, .{ .RaU, .imm16u });
    desc("Compare 16b register to immediate");

    IP_read_to_D(2, .word);
    D_to_L(.zx);
    L_to_SR(.temp_1);
    next_cycle();

    op_reg_minus_SRL_to_LL(.OA, .temp_1, .fresh, .flags);
    load_and_exec_next_insn(4);
}
pub fn _alias_FA40_FA4F_signed() void {
    encoding(.CMP, .{ .RaS, .imm16s });
    desc("Compare 16b register to immediate");
}

pub fn _FA50_FA5F() void {
    encoding(.CMPB, .{ .RaU, .imm16u });
    desc("Compare 16b register to immediate, with borrow");

    IP_read_to_D(2, .word);
    D_to_L(.zx);
    L_to_SR(.temp_1);
    next_cycle();

    op_reg_minus_SRL_to_LL(.OA, .temp_1, .cont, .flags);
    load_and_exec_next_insn(4);
}
pub fn _alias_FA50_FA5F_signed() void {
    encoding(.CMPB, .{ .RaS, .imm16s });
    desc("Compare 16b register to immediate, with borrow");
}

pub fn _FA60_FA6F() void {
    encoding(.CMP, .{ .Xa, .imm16u });
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
    desc("Compare 32b register to immediate");

    IP_read_to_D(2, .word);
    D_to_L(.zx);
    L_to_SR(.temp_1);
    next_cycle();

    op_reg32_minus_SRL_to_L(.OA, .temp_1, ._1x, .fresh, .flags);
    load_and_exec_next_insn(4);
}

pub fn _8300_84FF() void {
    var ext: cb.ZeroSignOrOneExtension = undefined;
    switch (opcode_high()) {
        0x83 => {
            encoding(.SUB, .{ .Xa, .RbU, .to, .Xa });
            desc("Subtract unsigned 16b register from 32b register");
            ext = .zx;
        },
        0x84 => {
            encoding(.SUB, .{ .Xa, .RbS, .to, .Xa });
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
    var freshness: cb.Freshness = undefined;
    switch (opcode_high()) {
        0x85 => {
            encoding(.SUB, .{ .imm8s, .Rb, .to, .Ra });
            desc("Subtract register from immediate and store result in another register");
            freshness = .fresh;
        },
        0x86 => {
            encoding(.SUBB, .{ .imm8s, .Rb, .to, .Ra });
            desc("Subtract register from immediate and store result in another register, with borrow");
            freshness = .cont;
        },
        else => unreachable,
    }

    IP_read_to_D(2, .byte);
    D_to_L(.sx);
    L_to_SR(.temp_1);
    next_cycle();

    SRL_minus_op_reg_to_LL(.temp_1, .OB, freshness, .flags);
    LL_to_op_reg(.OA);
    load_and_exec_next_insn(3);
}

pub fn _8700_87FF() void {
    encoding(.SUB, .{ .imm16u, .RbU, .to, .RaU });
    desc("Subtract register from immediate and store result in another register");

    IP_read_to_D(2, .word);
    D_to_L(.zx);
    L_to_SR(.temp_1);
    next_cycle();

    SRL_minus_op_reg_to_LL(.temp_1, .OB, .fresh, .flags);
    LL_to_op_reg(.OA);
    load_and_exec_next_insn(4);
}
pub fn _alias_8700_87FF_signed() void {
    encoding(.SUB, .{ .imm16s, .RbS, .to, .RaS });
    desc("Subtract register from immediate and store result in another register");
}

pub fn _8800_88FF() void {
    encoding(.SUBB, .{ .imm16u, .RbU, .to, .RaU });
    desc("Subtract register from immediate and store result in another register, with borrow");

    IP_read_to_D(2, .word);
    D_to_L(.zx);
    L_to_SR(.temp_1);
    next_cycle();

    SRL_minus_op_reg_to_LL(.temp_1, .OB, .cont, .flags);
    LL_to_op_reg(.OA);
    load_and_exec_next_insn(4);
}
pub fn _alias_8800_88FF_signed() void {
    encoding(.SUBB, .{ .imm16s, .RbS, .to, .RaS });
    desc("Subtract register from immediate and store result in another register, with borrow");
}

pub fn _8900_8AFF() void {
    var freshness: cb.Freshness = undefined;
    switch (opcode_high()) {
        0x89 => {
            encoding(.SUB, .{ .Ra, .Rb, .to, .Ra });
            desc("Subtract register from another register");
            freshness = .fresh;
        },
        0x8A => {
            encoding(.SUBB, .{ .Ra, .Rb, .to, .Ra });
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
    var freshness: cb.Freshness = undefined;
    switch (OB()) {
        0x0 => {
            encoding(.INC, .{ .Ra, .to, .Ra });
            desc("Increment 16b register");
            freshness = .fresh;
        },
        0x1 => {
            encoding(.INCC, .{ .Ra, .to, .Ra });
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
    var freshness: cb.Freshness = undefined;
    switch (OB()) {
        0x2 => {
            encoding(.INC, .{ .Xa, .to, .Xa });
            desc("Increment 32b register");
            freshness = .fresh;
        },
        0x3 => {
            encoding(.INCC, .{ .Xa, .to, .Xa });
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
    var freshness: cb.Freshness = undefined;
    switch (OB()) {
        0x4 => {
            encoding(.DEC, .{ .Ra, .to, .Ra });
            desc("Decrement 16b register");
            freshness = .fresh;
        },
        0x5 => {
            encoding(.DECB, .{ .Ra, .to, .Ra });
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
    var freshness: cb.Freshness = undefined;
    switch (OB()) {
        0x6 => {
            encoding(.DEC, .{ .Xa, .to, .Xa });
            desc("Decrement 32b register");
            freshness = .fresh;
        },
        0x7 => {
            encoding(.DECB, .{ .Xa, .to, .Xa });
            desc("Decrement 32b register, with borrow");
            freshness = .cont;
        },
        else => unreachable,
    }

    op_reg32_minus_literal_to_L(.OA, 1, freshness, .flags);
    L_to_op_reg32(.OA);
    load_and_exec_next_insn(2);
}

pub fn _9400_94FF() void {
    encoding(.NEG, .{ .Rb, .to, .Ra });
    desc("Negate 16b register");

    zero_minus_op_reg_to_LL(.OB, .fresh, .flags);
    L_to_op_reg32(.OA);
    load_and_exec_next_insn(2);
}

pub fn _9500_95FF() void {
    encoding(.NEGB, .{ .Rb, .to, .Ra });
    desc("Negate 16b register, with borrow");

    zero_minus_op_reg_to_LL(.OB, .cont, .flags);
    L_to_op_reg32(.OA);
    load_and_exec_next_insn(2);
}

pub fn _93A0_93AF() void {
    encoding(.NOT, .{ .Ra, .to, .Ra });
    desc("Bitwise complement of 16b register");

    op_reg_logic_literal_to_LL(.OA, .jl_xor_k, -1, .fresh, .flags);
    LL_to_op_reg(.OA);
    load_and_exec_next_insn(2);
}

pub fn _B000_B6FF() void {
    var op: ControlSignals.LogicMode = undefined;
    var mn: Mnemonic = undefined;
    var d: []const u8 = undefined;
    switch (opcode_high()) {
        0xB0 => { mn = .XOR;    op = .jl_xor_k;  d = "Bitwise XOR of two 16b registers"; },
        0xB1 => { mn = .XNOR;   op = .jl_xnor_k; d = "Bitwise XNOR of two 16b registers"; },
        0xB2 => { mn = .OR;     op = .jl_or_k;   d = "Bitwise OR of two 16b registers"; },
        0xB3 => { mn = .NOR;    op = .jl_nor_k;  d = "Bitwise NOR of two 16b registers"; },
        0xB4 => { mn = .AND;    op = .jl_and_k;  d = "Bitwise AND of two 16b registers"; },
        0xB5 => { mn = .NAND;   op = .jl_nand_k; d = "Bitwise NAND of two 16b registers"; },
        0xB6 => { mn = .ANDNOT; op = .njl_nor_k; d = "Bitwise AND of two 16b registers (second register is complemented)"; },
        else => unreachable,
    }

    encoding(mn, .{ .Ra, .Rb, .to, .Ra });
    desc(d);

    op_reg_logic_op_reg_to_LL(.OA, op, .OB, .fresh, .flags);
    LL_to_op_reg(.OA);
    load_and_exec_next_insn(2);
}

pub fn _C100_C5FF() void {
    var op: ControlSignals.LogicMode = undefined;
    var mn: Mnemonic = undefined;
    var d: []const u8 = undefined;
    switch (opcode_high()) {
        0xC1 => { mn = .XOR;  op = .jl_xor_k;  d = "Bitwise XOR of 16b register and immediate"; },
        0xC2 => { mn = .OR;   op = .jl_or_k;   d = "Bitwise OR of 16b register and immediate"; },
        0xC3 => { mn = .NOR;  op = .jl_nor_k;  d = "Bitwise NOR of 16b register and immediate"; },
        0xC4 => { mn = .AND;  op = .jl_and_k;  d = "Bitwise AND of 16b register and immediate"; },
        0xC5 => { mn = .NAND; op = .jl_nand_k; d = "Bitwise NAND of 16b register and immediate"; },
        else => unreachable,
    }

    encoding(mn, .{ .RaU, .imm16u, .to, .RbU });
    desc(d);

    IP_read_to_D(2, .word);
    D_to_L(.zx);
    L_to_SR(.temp_1);
    next_cycle();

    op_reg_logic_SRL_to_LL(.OA, op, .temp_1, .fresh, .flags);
    LL_to_op_reg(.OB);
    load_and_exec_next_insn(4);
}
pub fn _alias_C100_C5FF_signed() void {
    var mn: Mnemonic = undefined;
    var d: []const u8 = undefined;
    switch (opcode_high()) {
        0xC1 => { mn = .XOR;  d = "Bitwise XOR of 16b register and immediate"; },
        0xC2 => { mn = .OR;   d = "Bitwise OR of 16b register and immediate"; },
        0xC3 => { mn = .NOR;  d = "Bitwise NOR of 16b register and immediate"; },
        0xC4 => { mn = .AND;  d = "Bitwise AND of 16b register and immediate"; },
        0xC5 => { mn = .NAND; d = "Bitwise NAND of 16b register and immediate"; },
        else => unreachable,
    }

    encoding(mn, .{ .RaS, .imm16s, .to, .RbS });
    desc(d);
}

pub fn _C600_C7FF() void {
    var freshness: cb.Freshness = undefined;
    switch (opcode_high()) {
        0xC6 => {
            encoding(.TEST, .{ .Ra, .Rb });
            desc("Set Z and N flags based on bitwise AND of two 16b registers");
            freshness = .fresh;
        },
        0xC7 => {
            encoding(.TESTZ, .{ .Ra, .Rb });
            desc("Set Z and N flags based on bitwise AND of two 16b registers (don't set Z if already clear)");
            freshness = .cont;
        },
        else => unreachable,
    }

    op_reg_logic_op_reg_to_LL(.OA, .jl_and_k, .OB, freshness, .flags);
    load_and_exec_next_insn(2);
}

pub fn _9100_91FF() void {
    encoding(.TESTB, .{ .Ra, .immb4u });
    desc("Set Z and N flags according to single bit from 16b register");

    op_reg_logic_literal_to_LL(.OA, .jl_and_k, @as(u16, 1) << OB(), .fresh, .flags);
    load_and_exec_next_insn(2);
}

pub fn _9200_92FF() void {
    encoding(.TESTBZ, .{ .Ra, .immb4u });
    desc("Set Z and N flags according to single bit from 16b register, without setting the Z flag if it is already clear");

    op_reg_logic_literal_to_LL(.OA, .jl_and_k, @as(u16, 1) << OB(), .cont, .flags);
    load_and_exec_next_insn(2);
}

pub fn _A000_A0FF() void {
    encoding(.CLRB, .{ .Ra, .immb4u, .to, .Ra });
    desc("Clear single bit in 16b register");

    op_reg_logic_literal_to_LL(.OA, .njl_nor_k, @as(u16, 1) << OB(), .fresh, .flags);
    LL_to_op_reg(.OA);
    load_and_exec_next_insn(2);
}

pub fn _A100_A1FF() void {
    encoding(.SETB, .{ .Ra, .immb4u, .to, .Ra });
    desc("Set single bit in 16b register");

    op_reg_logic_literal_to_LL(.OA, .jl_or_k, @as(u16, 1) << OB(), .fresh, .flags);
    LL_to_op_reg(.OA);
    load_and_exec_next_insn(2);
}

pub fn _A200_A2FF() void {
    encoding(.TGLB, .{ .Ra, .immb4u, .to, .Ra });
    desc("Toggle single bit in 16b register");

    op_reg_logic_literal_to_LL(.OA, .jl_xor_k, @as(u16, 1) << OB(), .fresh, .flags);
    LL_to_op_reg(.OA);
    load_and_exec_next_insn(2);
}

pub fn _D000_D1FF() void {
    var dir: cb.ShiftDirection = undefined;
    switch (opcode_high()) {
        0xD0 => {
            encoding(.SHR, .{ .XaU, .Rb, .to, .XaU });
            desc("Right shift 32b register 0-31 bits, zero extending");
            dir = .right;
        },
        0xD1 => {
            encoding(.SHL, .{ .Xa, .Rb, .to, .Xa });
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
    var dir: cb.ShiftDirection = undefined;
    var ext: cb.ZeroSignOrOneExtension = undefined;
    switch (opcode_high()) {
        0xD2 => {
            encoding(.SHR, .{ .RaU, .Rb, .to, .RaU });
            desc("Shift right 16b register 0-15 bits, unsigned");
            dir = .right;
            ext = .zx;
        },
        0xD3 => {
            encoding(.SHR, .{ .RaS, .Rb, .to, .RaS });
            desc("Shift right 16b register 0-15 bits, signed");
            dir = .right;
            ext = .sx;
        },
        0xD4 => {
            encoding(.SHL, .{ .Ra, .Rb, .to, .Ra });
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
    var dir: cb.ShiftDirection = undefined;
    var ext: cb.ZeroSignOrOneExtension = undefined;
    switch (opcode_high()) {
        0xD5 => {
            encoding(.SHR, .{ .RaU, .immb4u, .to, .RaU });
            desc("Shift right 16b register 0-15 bits, unsigned");
            dir = .right;
            ext = .zx;
        },
        0xD6 => {
            encoding(.SHR, .{ .RaS, .immb4u, .to, .RaS });
            desc("Shift right 16b register 0-15 bits, signed");
            dir = .right;
            ext = .sx;
        },
        0xD7 => {
            encoding(.SHL, .{ .Ra, .immb4u, .to, .Ra });
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
    var dir: cb.ShiftDirection = undefined;
    switch (opcode_high()) {
        0xE0 => {
            encoding(.SHR, .{ .XaU, .immb4u, .to, .XaU });
            desc("Right shift 32b register 0-31 bits, zero extending");
            dir = .right;
        },
        0xE1 => {
            encoding(.SHR, .{ .XaU, .immb4_16_31, .to, .XaU });
            desc("Right shift 32b register 0-31 bits, zero extending");
            dir = .right;
        },
        0xE2 => {
            encoding(.SHL, .{ .Xa, .immb4u, .to, .Xa });
            desc("Left shift 32b register 0-31 bits");
            dir = .left;
        },
        0xE3 => {
            encoding(.SHL, .{ .Xa, .immb4_16_31, .to, .Xa });
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
    encoding(.MUL, .{ .RaU, .imm16u, .to, .RbU });
    desc("Multiply 16b register by immediate, truncate result to 16b and store in another register");

    IP_read_to_D(2, .word);
    D_to_L(.zx);
    L_to_SR(.temp_1);
    next_cycle();

    op_reg_mult_SRL_to_LL(.OA, .zx, .temp_1, .zx, .normal, .flags);
    LL_to_op_reg(.OB);
    load_and_exec_next_insn(4);
}
pub fn _alias_F000_F0FF_signed() void {
    encoding(.MUL, .{ .RaS, .imm16s, .to, .RbS });
    desc("Multiply 16b register by immediate, truncate result to 16b and store in another register");
}

pub fn _F100_F1FF() void {
    encoding(.MUL, .{ .Ra, .Rb, .to, .Ra });
    desc("Multiply 16b registers, truncate result to 16b");

    op_reg_mult_op_reg_to_LL(.OA, .zx, .OB, .zx, .normal, .flags);
    LL_to_op_reg(.OA);
    load_and_exec_next_insn(2);
}

pub fn _F200_F2FF() void {
    encoding(.MULH, .{ .RaU, .RbU, .to, .RaU });
    desc("Multiply 16b unsigned registers, store high 16b of result");

    op_reg_mult_op_reg_to_LL(.OA, .zx, .OB, .zx, .swap, .flags);
    LL_to_op_reg(.OA);
    load_and_exec_next_insn(2);
}

pub fn _F300_F3FF() void {
    encoding(.MULH, .{ .RaU, .RbS, .to, .RaS });
    desc("Multiply 16b unsigned registers, store high 16b of result");

    op_reg_mult_op_reg_to_LL(.OA, .sx, .OB, .zx, .swap, .flags);
    LL_to_op_reg(.OA);
    load_and_exec_next_insn(2);
}

pub fn _F400_F4FF() void {
    encoding(.MULH, .{ .RaS, .RbU, .to, .RaS });
    desc("Multiply 16b unsigned registers, store high 16b of result");

    op_reg_mult_op_reg_to_LL(.OA, .sx, .OB, .zx, .swap, .flags);
    LL_to_op_reg(.OA);
    load_and_exec_next_insn(2);
}

pub fn _F500_F5FF() void {
    encoding(.MULH, .{ .RaS, .RbS, .to, .RaS });
    desc("Multiply 16b unsigned registers, store high 16b of result");

    op_reg_mult_op_reg_to_LL(.OA, .sx, .OB, .sx, .swap, .flags);
    LL_to_op_reg(.OA);
    load_and_exec_next_insn(2);
}

pub fn _F600_F8FF() void {
    var ext_l: cb.ZeroOrSignExtension = undefined;
    var ext_r: cb.ZeroOrSignExtension = undefined;
    switch (opcode_high()) {
        0xF6 => {
            encoding(.MUL, .{ .RaU, .RbU, .to, .X0U });
            desc("Multiply 16b unsigned registers, store 32b result");
            ext_l = .zx;
            ext_r = .zx;
        },
        0xF7 => {
            encoding(.MUL, .{ .RaS, .RbS, .to, .X0S });
            desc("Multiply 16b signed registers, store 32b result");
            ext_l = .sx;
            ext_r = .sx;
        },
        0xF8 => {
            encoding(.MUL, .{ .RaU, .RbS, .to, .X0S });
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
    desc("Count set bits in 16b register");

    bitcount_op_reg_to_LL(.OA, .all, 1, .flags);
    LL_to_op_reg(.OA);
    load_and_exec_next_insn(2);
}

pub fn _E410_E41F() void {
    encoding(.CZ, .{ .Ra, .to, .Ra });
    desc("Count zero bits in 16b register");

    bitcount_op_reg_to_LL(.OA, .all, 0, .flags);
    LL_to_op_reg(.OA);
    load_and_exec_next_insn(2);
}

pub fn _E420_E42F() void {
    encoding(.CLB, .{ .Ra, .to, .Ra });
    desc("Count leading (most significant) set bits in 16b register");

    bitcount_op_reg_to_LL(.OA, .leading, 1, .flags);
    LL_to_op_reg(.OA);
    load_and_exec_next_insn(2);
}

pub fn _E430_E43F() void {
    encoding(.CLZ, .{ .Ra, .to, .Ra });
    desc("Count leading (most significant) zero bits in 16b register");

    bitcount_op_reg_to_LL(.OA, .leading, 0, .flags);
    LL_to_op_reg(.OA);
    load_and_exec_next_insn(2);
}

pub fn _E440_E44F() void {
    encoding(.CTB, .{ .Ra, .to, .Ra });
    desc("Count trailing (least significant) set bits in 16b register");

    bitcount_op_reg_to_LL(.OA, .trailing, 1, .flags);
    LL_to_op_reg(.OA);
    load_and_exec_next_insn(2);
}

pub fn _E450_E45F() void {
    encoding(.CTZ, .{ .Ra, .to, .Ra });
    desc("Count trailing (least significant) zero bits in 16b register");

    bitcount_op_reg_to_LL(.OA, .trailing, 0, .flags);
    LL_to_op_reg(.OA);
    load_and_exec_next_insn(2);
}
