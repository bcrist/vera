const assert = @import("std").debug.assert;
const ib = @import("instruction_builder.zig");
const cb = @import("cycle_builder.zig");
const ie = @import("instruction_encoding");

const ParameterEncoding = ie.ParameterEncoding;
const MnemonicSuffix = ie.MnemonicSuffix;
const ConstantRange = ie.ConstantRange;
const constantRange = ie.constantRange;

const getParameterOffset = ib.getParameterOffset;
const encoding = ib.encoding;
const encodingWithSuffix = ib.encodingWithSuffix;
const IP_relative = ib.IP_relative;
const Xa_relative = ib.Xa_relative;
const X0_relative = ib.X0_relative;
const parameter = ib.parameter;
const desc = ib.desc;
const next_cycle = ib.next_cycle;
const opcode = ib.opcode;
const OA = ib.OA;
const kernel = ib.kernel;
const zero = ib.zero;
const carry_borrow = ib.carry_borrow;
const negative = ib.negative;
const positive = ib.positive;
const unsigned_less_than = ib.unsigned_less_than;
const unsigned_greater_than = ib.unsigned_greater_than;
const signed_less_than = ib.signed_less_than;
const signed_greater_than = ib.signed_greater_than;

const IP_read_to_D = cb.IP_read_to_D;
const D_to_L = cb.D_to_L;
const D_to_LH = cb.D_to_LH;
const JH_to_LH = cb.JH_to_LH;
const L_to_SR = cb.L_to_SR;
const JL_to_LL = cb.JL_to_LL;
const SR_minus_SRL_to_L = cb.SR_minus_SRL_to_L;
const SR_plus_reg_to_L = cb.SR_plus_reg_to_L;
const SR_plus_SRL_to_L = cb.SR_plus_SRL_to_L;
const SRL_to_LL = cb.SRL_to_LL;
const reg32_to_L = cb.reg32_to_L;
const op_reg32_to_L = cb.op_reg32_to_L;
const branch = cb.branch;
const SRL_logic_literal_to_LL = cb.SRL_logic_literal_to_LL;
const disable_address_translation = cb.disable_address_translation;
const enable_address_translation = cb.enable_address_translation;
const load_and_exec_next_insn = cb.load_and_exec_next_insn;
const load_and_exec_next_insn_no_atomic_end = cb.load_and_exec_next_insn_no_atomic_end;
const illegal_instruction = cb.illegal_instruction;
const exec_latched_insn = cb.exec_latched_insn;

pub fn _0000() void {
    encoding(.NOP, .{});
    desc("No operation; equivalent to B IP+2");
    load_and_exec_next_insn(2);
}

pub fn _0001() void {
    encoding(.WFI, .{});
    desc("Does nothing on the current pipe until an interrupt is triggered, or the CPU is reset");

    if (!kernel()) {
        illegal_instruction();
        return;
    }

    exec_latched_insn();
}

pub fn _0100_013C() void {
    encoding(.B, .{ IP_relative(.imm_3_63) });
    //syntax("B IP+immba[3,63]");
    desc("IP-relative unconditional branch");

    branch(.ip, getParameterOffset(0));
}
pub fn _013D_017B() void {
    encoding(.B, .{ IP_relative(.imm_n64_n2) });
    //syntax("B IP+immba[-64,-2]");
    desc("IP-relative unconditional branch");

    branch(.ip, getParameterOffset(0));
}
pub fn alias_0100() void {
    encoding(.NOPE, .{});
    desc("No operation; equivalent to B IP+3");
}

pub fn _017C() void {
    encoding(.B, .{ IP_relative(.imm8_63_318) });
    //syntax("B IP+imm8[63,318]");
    desc("IP-relative unconditional branch");

    IP_read_to_D(2, .byte);
    D_to_L(.zx);
    L_to_SR(.temp_1);
    next_cycle();

    SR_plus_SRL_to_L(.ip, .temp_1, .zx, .fresh, .no_flags);
    L_to_SR(.next_ip);
    next_cycle();

    branch(.next_ip, 63);
}

pub fn _017D() void {
    encoding(.B, .{ IP_relative(.imm8_n319_n64_rev) });
    //syntax("B IP+imm8[-319,-64]");
    desc("IP-relative unconditional branch");

    IP_read_to_D(2, .byte);
    D_to_L(.zx);
    L_to_SR(.temp_1);
    next_cycle();

    SR_minus_SRL_to_L(.ip, .temp_1, .zx, .fresh, .no_flags);
    L_to_SR(.next_ip);
    next_cycle();

    branch(.next_ip, -64);
}

pub fn _017E() void {
    encoding(.B, .{ IP_relative(.imm16u) });
    //syntax("B IP+imm16[0,65535]");
    desc("IP-relative unconditional branch");

    IP_read_to_D(2, .word);
    D_to_L(.zx);
    L_to_SR(.temp_1);
    next_cycle();

    SR_plus_SRL_to_L(.ip, .temp_1, .zx, .fresh, .no_flags);
    L_to_SR(.next_ip);
    next_cycle();

    branch(.next_ip, 0);
}

pub fn _017F() void {
    encoding(.B, .{ IP_relative(.imm16n) });
    //syntax("B IP+imm16[-65536,-1]");
    desc("IP-relative unconditional branch");

    IP_read_to_D(2, .word);
    D_to_L(.zx);
    L_to_SR(.temp_1);
    next_cycle();

    SR_plus_SRL_to_L(.ip, .temp_1, ._1x, .fresh, .no_flags);
    L_to_SR(.next_ip);
    next_cycle();

    branch(.next_ip, 0);
}

pub fn _0006() void {
    encoding(.B, .{ IP_relative(.R0) });
    //syntax("B IP+UR0");
    desc("IP-relative unsigned register unconditional branch");

    SR_plus_reg_to_L(.ip, 0, .zx, .fresh, .no_flags);
    L_to_SR(.next_ip);
    next_cycle();

    branch(.next_ip, 0);
}

pub fn _0007() void {
    encoding(.B, .{ IP_relative(.R0) });
    //syntax("B IP+SR0");
    desc("IP-relative signed register unconditional branch");

    SR_plus_reg_to_L(.ip, 0, .sx, .fresh, .no_flags);
    L_to_SR(.next_ip);
    next_cycle();

    branch(.next_ip, 0);
}

pub fn _FAB0_FABF() void {
    encoding(.B, .{ Xa_relative(.I, .imm_0) });
    //syntax("B Xa");
    desc("Register unconditional branch");

    op_reg32_to_L(.OA);
    L_to_SR(.next_ip);
    next_cycle();

    branch(.next_ip, 0);
}

pub fn _0180() void {
    encoding(.B, .{ .imm32u });
    //syntax("B imm32[0,4294967295]");
    desc("Absolute unconditional branch");

    IP_read_to_D(2, .word);
    D_to_L(.zx);
    L_to_SR(.temp_1);
    next_cycle();

    IP_read_to_D(4, .word);
    SRL_to_LL(.temp_1);
    D_to_LH();
    L_to_SR(.next_ip);
    next_cycle();

    branch(.next_ip, 0);
}

pub fn _0200_023C() void {
    encodingWithSuffix(.B, .Z, .{ IP_relative(.imm_3_63) });
    //syntax("B.Z IP+immba[3,63]");
    desc("IP-relative branch if zero");

    if (zero()) {
        branch(.ip, getParameterOffset(0));
    } else {
        load_and_exec_next_insn_no_atomic_end(2);
    }
}
pub fn _023D_027B() void {
    encodingWithSuffix(.B, .Z, .{ IP_relative(.imm_n64_n2) });
    //syntax("B.Z IP+immba[-64,-2]");
    desc("IP-relative branch if zero");

    if (zero()) {
        branch(.ip, getParameterOffset(0));
    } else {
        load_and_exec_next_insn_no_atomic_end(2);
    }
}

pub fn _0300_033C() void {
    encodingWithSuffix(.B, .NZ, .{ IP_relative(.imm_3_63) });
    //syntax("B.NZ IP+immba[3,63]");
    desc("IP-relative branch if nonzero");

    if (!zero()) {
        branch(.ip, getParameterOffset(0));
    } else {
        load_and_exec_next_insn_no_atomic_end(2);
    }
}
pub fn _033D_037B() void {
    encodingWithSuffix(.B, .NZ, .{ IP_relative(.imm_n64_n2) });
    //syntax("B.NZ IP+immba[-64,-2]");
    desc("IP-relative branch if nonzero");

    if (!zero()) {
        branch(.ip, getParameterOffset(0));
    } else {
        load_and_exec_next_insn_no_atomic_end(2);
    }
}

fn conditionName(comptime condition: MnemonicSuffix) []const u8 {
    comptime {
        return switch (condition) {
            .Z => "zero",
            .NZ => "not zero",
            .LU => "less (unsigned)",
            .NLU => "not less (unsigned)",
            .GU => "greater (unsigned)",
            .NGU => "not greater (unsigned)",
            .N => "negative",
            .NN => "not negative",
            .C => "carry",
            .NC => "not carry",
            .LS => "less (signed)",
            .NLS => "not less (signed)",
            .GS => "greater (signed)",
            .NGS => "not greater (signed)",
            .P => "positive",
            .NP => "not positive",
            .LU_GU => "less (unsigned) or greater (unsigned)",
            .LU_Z => "less (unsigned) or zero",
            .GU_Z => "greater (unsigned) or zero",
            .LS_GS => "less (signed) or greater (signed)",
            .LS_Z => "less (signed) or zero",
            .GS_Z => "greater (signed) or zero",
            .N_Z => "negative or zero",
            .P_Z => "positive or zero",
            .N_P => "negative or positive",
            else => unreachable,
        };
    }
}

fn conditionFn(comptime condition: MnemonicSuffix) fn () bool {
    comptime {
        return switch (condition) {
            .Z, .NZ => zero,
            .LU, .NLU => unsigned_less_than,
            .GU, .NGU => unsigned_greater_than,
            .N, .NN => negative,
            .C, .NC => carry_borrow,
            .LS, .NLS => signed_less_than,
            .GS, .NGS => signed_greater_than,
            .P, .NP => positive,
            else => unreachable,
        };
    }
}

fn conditionIsInverse(comptime condition: MnemonicSuffix) bool {
    comptime {
        return switch (condition) {
            .Z, .LU, .GU, .N, .C, .LS, .GS, .P => false,
            .NZ, .NLU, .NGU, .NN, .NC, .NLS, .NGS, .NP => true,
            else => unreachable,
        };
    }
}

const n6_14 = ParameterEncoding{
    .type = .{ .base = .constant },
    .constant_ranges = &[_]ConstantRange{
        .{ .min = 4,  .max = 14 },
        .{ .min = -6, .max = -2 },
    },
    .base_src = .OA,
};

fn conditional_n6_14(comptime condition: MnemonicSuffix) void {
    encodingWithSuffix(.B, condition, .{ IP_relative(n6_14) });
    desc("IP-relative branch if " ++ comptime conditionName(condition));

    const conditionFunc = conditionFn(condition);
    var should_branch = conditionFunc();
    if (comptime conditionIsInverse(condition)) should_branch = !should_branch;
    if (should_branch) {
        branch(.ip, getParameterOffset(0));
    } else {
        load_and_exec_next_insn_no_atomic_end(2);
    }
}

pub fn _0800_080F() void { conditional_n6_14(.LU); }
pub fn _0810_081F() void { conditional_n6_14(.NLU); }
pub fn _0820_082F() void { conditional_n6_14(.GU); }
pub fn _0830_083F() void { conditional_n6_14(.NGU); }
pub fn _0840_084F() void { conditional_n6_14(.N); }
pub fn _0850_085F() void { conditional_n6_14(.NN); }
pub fn _0860_086F() void { conditional_n6_14(.C); }
pub fn _0870_087F() void { conditional_n6_14(.NC); }
pub fn _0C00_0C0F() void { conditional_n6_14(.LS); }
pub fn _0C10_0C1F() void { conditional_n6_14(.NLS); }
pub fn _0C20_0C2F() void { conditional_n6_14(.GS); }
pub fn _0C30_0C3F() void { conditional_n6_14(.NGS); }
pub fn _0C40_0C4F() void { conditional_n6_14(.P); }
pub fn _0C50_0C5F() void { conditional_n6_14(.NP); }

fn conditional_15_270(comptime condition: MnemonicSuffix) void {
    encodingWithSuffix(.B, condition, .{ IP_relative(.imm8_15_270) });
    desc("IP-relative branch if " ++ comptime conditionName(condition));

    const conditionFunc = conditionFn(condition);
    var should_branch = conditionFunc();
    if (comptime conditionIsInverse(condition)) should_branch = !should_branch;
    if (should_branch) {
        IP_read_to_D(2, .byte);
        D_to_L(.zx);
        L_to_SR(.temp_1);
        next_cycle();

        SR_plus_SRL_to_L(.ip, .temp_1, .zx, .fresh, .no_flags);
        L_to_SR(.next_ip);
        next_cycle();

        branch(.next_ip, 15);
    } else {
        load_and_exec_next_insn_no_atomic_end(3);
    }
}

fn conditional_n262_n7(comptime condition: MnemonicSuffix) void {
    encodingWithSuffix(.B, condition, .{ IP_relative(.imm8_n262_n7_rev) });
    desc("IP-relative branch if " ++ comptime conditionName(condition));

    const conditionFunc = conditionFn(condition);
    var should_branch = conditionFunc();
    if (comptime conditionIsInverse(condition)) should_branch = !should_branch;
    if (should_branch) {
        IP_read_to_D(2, .byte);
        D_to_L(.zx);
        L_to_SR(.temp_1);
        next_cycle();

        SR_minus_SRL_to_L(.ip, .temp_1, .zx, .fresh, .no_flags);
        L_to_SR(.next_ip);
        next_cycle();

        branch(.next_ip, -7);
    } else {
        load_and_exec_next_insn_no_atomic_end(3);
    }
}

pub fn _027C() void { conditional_15_270(.Z); }
pub fn _027D() void { conditional_n262_n7(.Z); }
pub fn _027E() void { conditional_15_270(.NZ); }
pub fn _027F() void { conditional_n262_n7(.NZ); }
pub fn _0880() void { conditional_15_270(.LU); }
pub fn _0881() void { conditional_n262_n7(.LU); }
pub fn _0882() void { conditional_15_270(.NLU); }
pub fn _0883() void { conditional_n262_n7(.NLU); }
pub fn _0884() void { conditional_15_270(.GU); }
pub fn _0885() void { conditional_n262_n7(.GU); }
pub fn _0886() void { conditional_15_270(.NGU); }
pub fn _0887() void { conditional_n262_n7(.NGU); }
pub fn _0888() void { conditional_15_270(.N); }
pub fn _0889() void { conditional_n262_n7(.N); }
pub fn _088A() void { conditional_15_270(.NN); }
pub fn _088B() void { conditional_n262_n7(.NN); }
pub fn _088C() void { conditional_15_270(.C); }
pub fn _088D() void { conditional_n262_n7(.C); }
pub fn _088E() void { conditional_15_270(.NC); }
pub fn _088F() void { conditional_n262_n7(.NC); }
pub fn _0C80() void { conditional_15_270(.LS); }
pub fn _0C81() void { conditional_n262_n7(.LS); }
pub fn _0C82() void { conditional_15_270(.NLS); }
pub fn _0C83() void { conditional_n262_n7(.NLS); }
pub fn _0C84() void { conditional_15_270(.GS); }
pub fn _0C85() void { conditional_n262_n7(.GS); }
pub fn _0C86() void { conditional_15_270(.NGS); }
pub fn _0C87() void { conditional_n262_n7(.NGS); }
pub fn _0C88() void { conditional_15_270(.P); }
pub fn _0C89() void { conditional_n262_n7(.P); }
pub fn _0C8A() void { conditional_15_270(.NP); }
pub fn _0C8B() void { conditional_n262_n7(.NP); }

fn conditional_s16(comptime condition: MnemonicSuffix) void {
    encodingWithSuffix(.B, condition, .{ IP_relative(.imm16s) });
    desc("IP-relative branch if " ++ comptime conditionName(condition));

    const conditionFunc = conditionFn(condition);
    var should_branch = conditionFunc();
    if (comptime conditionIsInverse(condition)) should_branch = !should_branch;
    if (should_branch) {
        IP_read_to_D(2, .word);
        D_to_L(.zx);
        L_to_SR(.temp_1);
        next_cycle();

        SR_plus_SRL_to_L(.ip, .temp_1, .sx, .fresh, .no_flags);
        L_to_SR(.next_ip);
        next_cycle();

        branch(.next_ip, 0);
    } else {
        load_and_exec_next_insn_no_atomic_end(4);
    }
}

pub fn _037C() void { conditional_s16(.Z); }
pub fn _037D() void { conditional_s16(.NZ); }
pub fn _0890() void { conditional_s16(.LU); }
pub fn _0891() void { conditional_s16(.NLU); }
pub fn _0892() void { conditional_s16(.GU); }
pub fn _0893() void { conditional_s16(.NGU); }
pub fn _0894() void { conditional_s16(.N); }
pub fn _0895() void { conditional_s16(.NN); }
pub fn _0896() void { conditional_s16(.C); }
pub fn _0897() void { conditional_s16(.NC); }
pub fn _0C90() void { conditional_s16(.LS); }
pub fn _0C91() void { conditional_s16(.NLS); }
pub fn _0C92() void { conditional_s16(.GS); }
pub fn _0C93() void { conditional_s16(.NGS); }
pub fn _0C94() void { conditional_s16(.P); }
pub fn _0C95() void { conditional_s16(.NP); }

pub fn _0184() void {
    encoding(.BP, .{});
    //syntax("BP");
    desc("Branch to start of current page");

    SRL_logic_literal_to_LL(.ip, .jl_and_k, 0xF000, .fresh, .no_flags);
    JH_to_LH();
    L_to_SR(.next_ip);
    next_cycle();

    branch(.next_ip, 0);
}

pub fn _0185() void {
    encoding(.BPN, .{});
    //syntax("BPN");
    desc("Branch to start of next page");

    SRL_logic_literal_to_LL(.ip, .njl_nand_k, 0xF000, .fresh, .no_flags);
    JH_to_LH();
    L_to_SR(.next_ip);
    next_cycle();

    branch(.next_ip, 1);
}

pub fn _0186() void {
    encoding(.BB, .{});
    //syntax("BB");
    desc("Branch to start of current 256-byte-aligned block");

    SRL_logic_literal_to_LL(.ip, .jl_and_k, 0xFF00, .fresh, .no_flags);
    JH_to_LH();
    L_to_SR(.next_ip);
    next_cycle();

    branch(.next_ip, 0);
}

pub fn _0187() void {
    encoding(.BBN, .{});
    //syntax("BBN");
    desc("Branch to start of next 256-byte-aligned block");

    SRL_logic_literal_to_LL(.ip, .jl_or_k, 0xFF, .fresh, .no_flags);
    JH_to_LH();
    L_to_SR(.next_ip);
    next_cycle();

    branch(.next_ip, 1);
}

pub fn _0188() void {
    encoding(.EAB, .{ X0_relative(.I, .imm_0) });
    //syntax("EAB X0");
    desc("Enable address translation + register unconditional branch");

    if (!kernel()) {
        illegal_instruction();
        return;
    }

    reg32_to_L(0);
    L_to_SR(.next_ip);
    enable_address_translation();
    next_cycle();

    branch(.next_ip, 0);
}

pub fn _0189() void {
    encoding(.DAB, .{ X0_relative(.I, .imm_0) });
    //syntax("DAB X0");
    desc("Disable address translation + register unconditional branch");

    if (!kernel()) {
        illegal_instruction();
        return;
    }

    reg32_to_L(0);
    L_to_SR(.next_ip);
    disable_address_translation();
    next_cycle();

    branch(.next_ip, 0);
}

fn firstCondition(comptime condition: MnemonicSuffix) MnemonicSuffix {
    comptime {
        return switch (condition) {
            .LU_GU, .LU_Z => return .LU,
            .GU_Z => return .GU,
            .LS_GS, .LS_Z => return .LS,
            .GS_Z => return .GS,
            .N_Z, .N_P => return .N,
            .P_Z => return .P,
            else => unreachable,
        };
    }
}

fn secondCondition(comptime condition: MnemonicSuffix) MnemonicSuffix {
    comptime {
        return switch (condition) {
            .LU_Z, .GU_Z, .LS_Z, .GS_Z, .N_Z, .P_Z => return .Z,
            .LU_GU => return .GU,
            .LS_GS => return .GS,
            .N_P => return .P,
            else => unreachable,
        };
    }
}

fn conditional_double_branch_s16(comptime condition: MnemonicSuffix) void {
    encodingWithSuffix(.B, condition, .{ IP_relative(.imm16s), IP_relative(.@"imm16s@4") });
    desc("IP-relative ternary branch if " ++ comptime conditionName(condition));

    const first = comptime firstCondition(condition);
    const second = comptime secondCondition(condition);

    assert(!conditionIsInverse(first));
    assert(!conditionIsInverse(second));

    if (conditionFn(first)()) {
        IP_read_to_D(2, .word);
    } else if (conditionFn(second)()) {
        IP_read_to_D(4, .word);
    } else {
        load_and_exec_next_insn_no_atomic_end(6);
        return;
    }
    D_to_L(.zx);
    L_to_SR(.temp_1);
    next_cycle();

    SR_plus_SRL_to_L(.ip, .temp_1, .sx, .fresh, .no_flags);
    L_to_SR(.next_ip);
    next_cycle();

    branch(.next_ip, 0);
}

pub fn _0898() void { conditional_double_branch_s16(.LU_GU); }
pub fn _0899() void { conditional_double_branch_s16(.LU_Z); }
pub fn _089A() void { conditional_double_branch_s16(.GU_Z); }
pub fn _0C98() void { conditional_double_branch_s16(.LS_GS); }
pub fn _0C99() void { conditional_double_branch_s16(.LS_Z); }
pub fn _0C9A() void { conditional_double_branch_s16(.GS_Z); }
pub fn _0C96() void { conditional_double_branch_s16(.N_Z); }
pub fn _0C97() void { conditional_double_branch_s16(.P_Z); }
pub fn _0C9B() void { conditional_double_branch_s16(.N_P); }
