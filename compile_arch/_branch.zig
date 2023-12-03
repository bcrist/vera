pub const instructions = .{
    struct { pub const spec = // nop 1
        \\nop 1
        \\nop
        \\b .i ip + 1
        ;
        pub const encoding = .{
            opcodes.Lo8.nop1,
        };
        pub fn entry(c: *Cycle) void {
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = // nop 2
        \\nop 2
        \\b .i ip + 2
        ;
        pub const encoding = .{
            opcodes.Lo16.nop2,
        };
        pub fn entry(c: *Cycle) void {
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = // nop 3
        \\nop 3
        \\b .i ip + 3
        ;
        pub const encoding = .{
            opcodes.Lo16.nop3,
            Encoder.shifted(16, @as(u8, 0)),
        };
        pub fn entry(c: *Cycle) void {
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = "park";
        pub const encoding = .{
            opcodes.Lo16.park,
        };
        pub fn entry(c: *Cycle, flags: Flags) void {
            if (!flags.kernel()) return c.illegal_instruction();
            c.decode_and_exec_dr(.normal);
        }
    },
    struct { pub const spec = "b .i ip + (imm)";
        pub const encoding = .{
            Encoder.offset(@intFromEnum(opcodes.Lo16.branch_4), Imm),
        };
        const Imm = Range(.imm, 4, 63);
        pub fn entry(c: *Cycle, imm: Imm) void {
            c.branch(.ip, @intCast(imm.value));
        }
    },
    struct { pub const spec = "b .i ip + (imm)";
        pub const encoding = .{
            Encoder.offset(@intFromEnum(opcodes.Lo16.branch_n64), Imm),
        };
        const Imm = Range(.imm, -64, -1);
        pub fn entry(c: *Cycle, imm: Imm) void {
            c.branch(.ip, @intCast(imm.value));
        }
    },
};



// pub fn _017C() void {
//     encoding(.B, .{ IP_relative(.insn, .imm8_63_318) });
//     desc("IP-relative unconditional branch");

//     IP_read_to_D(2, .byte);
//     D_to_L(.zx);
//     L_to_SR(.temp_1);
//     next_cycle();

//     SR_plus_SRL_to_L(.ip, .temp_1, .zx, .fresh, .no_flags);
//     L_to_SR(.next_ip);
//     next_cycle();

//     branch(.next_ip, 63);
// }

// pub fn _017D() void {
//     encoding(.B, .{ IP_relative(.insn, .imm8_n319_n64_rev) });
//     desc("IP-relative unconditional branch");

//     IP_read_to_D(2, .byte);
//     D_to_L(.zx);
//     L_to_SR(.temp_1);
//     next_cycle();

//     SR_minus_SRL_to_L(.ip, .temp_1, .zx, .fresh, .no_flags);
//     L_to_SR(.next_ip);
//     next_cycle();

//     branch(.next_ip, -64);
// }

// pub fn _017E() void {
//     encoding(.B, .{ IP_relative(.insn, .imm16u) });
//     desc("IP-relative unconditional branch");

//     IP_read_to_D(2, .word);
//     D_to_L(.zx);
//     L_to_SR(.temp_1);
//     next_cycle();

//     SR_plus_SRL_to_L(.ip, .temp_1, .zx, .fresh, .no_flags);
//     L_to_SR(.next_ip);
//     next_cycle();

//     branch(.next_ip, 0);
// }

// pub fn _017F() void {
//     encoding(.B, .{ IP_relative(.insn, .imm16n) });
//     desc("IP-relative unconditional branch");

//     IP_read_to_D(2, .word);
//     D_to_L(.zx);
//     L_to_SR(.temp_1);
//     next_cycle();

//     SR_plus_SRL_to_L(.ip, .temp_1, ._1x, .fresh, .no_flags);
//     L_to_SR(.next_ip);
//     next_cycle();

//     branch(.next_ip, 0);
// }

// pub fn _0006() void {
//     encoding(.B, .{ IP_relative(.insn, .R0U) });
//     desc("IP-relative unsigned register unconditional branch");

//     SR_plus_reg_to_L(.ip, 0, .zx, .fresh, .no_flags);
//     L_to_SR(.next_ip);
//     next_cycle();

//     branch(.next_ip, 0);
// }

// pub fn _0007() void {
//     encoding(.B, .{ IP_relative(.insn, .R0S) });
//     desc("IP-relative signed register unconditional branch");

//     SR_plus_reg_to_L(.ip, 0, .sx, .fresh, .no_flags);
//     L_to_SR(.next_ip);
//     next_cycle();

//     branch(.next_ip, 0);
// }

// pub fn _FAB0_FABF() void {
//     encoding(.B, .{ addr(.insn, .Xa) });
//     desc("Register unconditional branch");

//     op_reg32_to_L(.OA);
//     L_to_SR(.next_ip);
//     next_cycle();

//     branch(.next_ip, 0);
// }

// pub fn _0180() void {
//     encoding(.B, .{ addr(.insn, .imm32u) });
//     desc("Absolute unconditional branch");

//     IP_read_to_D(2, .word);
//     D_to_L(.zx);
//     L_to_SR(.temp_1);
//     next_cycle();

//     IP_read_to_D(4, .word);
//     SRL_to_LL(.temp_1);
//     D_to_LH();
//     L_to_SR(.next_ip);
//     next_cycle();

//     branch(.next_ip, 0);
// }

// pub fn _0200_023C() void {
//     encodingWithSuffix(.B, .Z, .{ IP_relative(.insn, .imm_3_63) });
//     desc("IP-relative branch if zero");

//     if (zero()) {
//         branch(.ip, getParameterOffset(0));
//     } else {
//         load_and_exec_next_insn_no_atomic_end(2);
//     }
// }
// pub fn _023D_027B() void {
//     encodingWithSuffix(.B, .Z, .{ IP_relative(.insn, .imm_n64_n2) });
//     desc("IP-relative branch if zero");

//     if (zero()) {
//         branch(.ip, getParameterOffset(0));
//     } else {
//         load_and_exec_next_insn_no_atomic_end(2);
//     }
// }

// pub fn _0300_033C() void {
//     encodingWithSuffix(.B, .NZ, .{ IP_relative(.insn, .imm_3_63) });
//     desc("IP-relative branch if nonzero");

//     if (!zero()) {
//         branch(.ip, getParameterOffset(0));
//     } else {
//         load_and_exec_next_insn_no_atomic_end(2);
//     }
// }
// pub fn _033D_037B() void {
//     encodingWithSuffix(.B, .NZ, .{ IP_relative(.insn, .imm_n64_n2) });
//     desc("IP-relative branch if nonzero");

//     if (!zero()) {
//         branch(.ip, getParameterOffset(0));
//     } else {
//         load_and_exec_next_insn_no_atomic_end(2);
//     }
// }

// fn conditionName(comptime condition: MnemonicSuffix) []const u8 {
//     comptime {
//         return switch (condition) {
//             .Z => "zero",
//             .NZ => "not zero",
//             .LU => "less (unsigned)",
//             .NLU => "not less (unsigned)",
//             .GU => "greater (unsigned)",
//             .NGU => "not greater (unsigned)",
//             .N => "negative",
//             .NN => "not negative",
//             .C => "carry",
//             .NC => "not carry",
//             .LS => "less (signed)",
//             .NLS => "not less (signed)",
//             .GS => "greater (signed)",
//             .NGS => "not greater (signed)",
//             .P => "positive",
//             .NP => "not positive",
//             .LU_GU => "less (unsigned) or greater (unsigned)",
//             .LU_Z => "less (unsigned) or zero",
//             .GU_Z => "greater (unsigned) or zero",
//             .LS_GS => "less (signed) or greater (signed)",
//             .LS_Z => "less (signed) or zero",
//             .GS_Z => "greater (signed) or zero",
//             .N_Z => "negative or zero",
//             .P_Z => "positive or zero",
//             .N_P => "negative or positive",
//             else => unreachable,
//         };
//     }
// }

// fn conditionFn(comptime condition: MnemonicSuffix) fn () bool {
//     return switch (condition) {
//         .Z, .NZ => zero,
//         .LU, .NLU => unsigned_less_than,
//         .GU, .NGU => unsigned_greater_than,
//         .N, .NN => negative,
//         .C, .NC => carry_borrow,
//         .LS, .NLS => signed_less_than,
//         .GS, .NGS => signed_greater_than,
//         .P, .NP => positive,
//         else => unreachable,
//     };
// }

// fn conditionIsInverse(condition: MnemonicSuffix) bool {
//     return switch (condition) {
//         .Z, .LU, .GU, .N, .C, .LS, .GS, .P => false,
//         .NZ, .NLU, .NGU, .NN, .NC, .NLS, .NGS, .NP => true,
//         else => unreachable,
//     };
// }

// const n6_14 = ParameterEncoding{
//     .base = .{ .constant = .{
//         .ranges = &[_]ConstantRange{
//             .{ .min = 4,  .max = 14 },
//             .{ .min = -6, .max = -2 },
//         },
//     }},
//     .base_src = .OA,
// };

// fn conditional_n6_14(comptime condition: MnemonicSuffix) void {
//     encodingWithSuffix(.B, condition, .{ IP_relative(.insn, n6_14) });
//     desc("IP-relative branch if " ++ comptime conditionName(condition));

//     const conditionFunc = comptime conditionFn(condition);
//     var should_branch = conditionFunc();
//     if (comptime conditionIsInverse(condition)) should_branch = !should_branch;
//     if (should_branch) {
//         branch(.ip, getParameterOffset(0));
//     } else {
//         load_and_exec_next_insn_no_atomic_end(2);
//     }
// }

// pub fn _0800_080F() void { conditional_n6_14(.LU); }
// pub fn _0810_081F() void { conditional_n6_14(.NLU); }
// pub fn _0820_082F() void { conditional_n6_14(.GU); }
// pub fn _0830_083F() void { conditional_n6_14(.NGU); }
// pub fn _0840_084F() void { conditional_n6_14(.N); }
// pub fn _0850_085F() void { conditional_n6_14(.NN); }
// pub fn _0860_086F() void { conditional_n6_14(.C); }
// pub fn _0870_087F() void { conditional_n6_14(.NC); }
// pub fn _0C00_0C0F() void { conditional_n6_14(.LS); }
// pub fn _0C10_0C1F() void { conditional_n6_14(.NLS); }
// pub fn _0C20_0C2F() void { conditional_n6_14(.GS); }
// pub fn _0C30_0C3F() void { conditional_n6_14(.NGS); }
// pub fn _0C40_0C4F() void { conditional_n6_14(.P); }
// pub fn _0C50_0C5F() void { conditional_n6_14(.NP); }

// fn conditional_15_270(comptime condition: MnemonicSuffix) void {
//     encodingWithSuffix(.B, condition, .{ IP_relative(.insn, .imm8_15_270) });
//     desc("IP-relative branch if " ++ comptime conditionName(condition));

//     const conditionFunc = conditionFn(condition);
//     var should_branch = conditionFunc();
//     if (comptime conditionIsInverse(condition)) should_branch = !should_branch;
//     if (should_branch) {
//         IP_read_to_D(2, .byte);
//         D_to_L(.zx);
//         L_to_SR(.temp_1);
//         next_cycle();

//         SR_plus_SRL_to_L(.ip, .temp_1, .zx, .fresh, .no_flags);
//         L_to_SR(.next_ip);
//         next_cycle();

//         branch(.next_ip, 15);
//     } else {
//         load_and_exec_next_insn_no_atomic_end(3);
//     }
// }

// fn conditional_n262_n7(comptime condition: MnemonicSuffix) void {
//     encodingWithSuffix(.B, condition, .{ IP_relative(.insn, .imm8_n262_n7_rev) });
//     desc("IP-relative branch if " ++ comptime conditionName(condition));

//     const conditionFunc = conditionFn(condition);
//     var should_branch = conditionFunc();
//     if (comptime conditionIsInverse(condition)) should_branch = !should_branch;
//     if (should_branch) {
//         IP_read_to_D(2, .byte);
//         D_to_L(.zx);
//         L_to_SR(.temp_1);
//         next_cycle();

//         SR_minus_SRL_to_L(.ip, .temp_1, .zx, .fresh, .no_flags);
//         L_to_SR(.next_ip);
//         next_cycle();

//         branch(.next_ip, -7);
//     } else {
//         load_and_exec_next_insn_no_atomic_end(3);
//     }
// }

// pub fn _027C() void { conditional_15_270(.Z); }
// pub fn _027D() void { conditional_n262_n7(.Z); }
// pub fn _027E() void { conditional_15_270(.NZ); }
// pub fn _027F() void { conditional_n262_n7(.NZ); }
// pub fn _0880() void { conditional_15_270(.LU); }
// pub fn _0881() void { conditional_n262_n7(.LU); }
// pub fn _0882() void { conditional_15_270(.NLU); }
// pub fn _0883() void { conditional_n262_n7(.NLU); }
// pub fn _0884() void { conditional_15_270(.GU); }
// pub fn _0885() void { conditional_n262_n7(.GU); }
// pub fn _0886() void { conditional_15_270(.NGU); }
// pub fn _0887() void { conditional_n262_n7(.NGU); }
// pub fn _0888() void { conditional_15_270(.N); }
// pub fn _0889() void { conditional_n262_n7(.N); }
// pub fn _088A() void { conditional_15_270(.NN); }
// pub fn _088B() void { conditional_n262_n7(.NN); }
// pub fn _088C() void { conditional_15_270(.C); }
// pub fn _088D() void { conditional_n262_n7(.C); }
// pub fn _088E() void { conditional_15_270(.NC); }
// pub fn _088F() void { conditional_n262_n7(.NC); }
// pub fn _0C80() void { conditional_15_270(.LS); }
// pub fn _0C81() void { conditional_n262_n7(.LS); }
// pub fn _0C82() void { conditional_15_270(.NLS); }
// pub fn _0C83() void { conditional_n262_n7(.NLS); }
// pub fn _0C84() void { conditional_15_270(.GS); }
// pub fn _0C85() void { conditional_n262_n7(.GS); }
// pub fn _0C86() void { conditional_15_270(.NGS); }
// pub fn _0C87() void { conditional_n262_n7(.NGS); }
// pub fn _0C88() void { conditional_15_270(.P); }
// pub fn _0C89() void { conditional_n262_n7(.P); }
// pub fn _0C8A() void { conditional_15_270(.NP); }
// pub fn _0C8B() void { conditional_n262_n7(.NP); }

// fn conditional_s16(comptime condition: MnemonicSuffix) void {
//     encodingWithSuffix(.B, condition, .{ IP_relative(.insn, .imm16s) });
//     desc("IP-relative branch if " ++ comptime conditionName(condition));

//     const conditionFunc = conditionFn(condition);
//     var should_branch = conditionFunc();
//     if (comptime conditionIsInverse(condition)) should_branch = !should_branch;
//     if (should_branch) {
//         IP_read_to_D(2, .word);
//         D_to_L(.zx);
//         L_to_SR(.temp_1);
//         next_cycle();

//         SR_plus_SRL_to_L(.ip, .temp_1, .sx, .fresh, .no_flags);
//         L_to_SR(.next_ip);
//         next_cycle();

//         branch(.next_ip, 0);
//     } else {
//         load_and_exec_next_insn_no_atomic_end(4);
//     }
// }

// pub fn _037C() void { conditional_s16(.Z); }
// pub fn _037D() void { conditional_s16(.NZ); }
// pub fn _0890() void { conditional_s16(.LU); }
// pub fn _0891() void { conditional_s16(.NLU); }
// pub fn _0892() void { conditional_s16(.GU); }
// pub fn _0893() void { conditional_s16(.NGU); }
// pub fn _0894() void { conditional_s16(.N); }
// pub fn _0895() void { conditional_s16(.NN); }
// pub fn _0896() void { conditional_s16(.C); }
// pub fn _0897() void { conditional_s16(.NC); }
// pub fn _0C90() void { conditional_s16(.LS); }
// pub fn _0C91() void { conditional_s16(.NLS); }
// pub fn _0C92() void { conditional_s16(.GS); }
// pub fn _0C93() void { conditional_s16(.NGS); }
// pub fn _0C94() void { conditional_s16(.P); }
// pub fn _0C95() void { conditional_s16(.NP); }

// pub fn _0184() void {
//     encoding(.BP, .{});
//     desc("Branch to start of current page");

//     SRL_logic_literal_to_LL(.ip, .jl_and_k, 0xF000, .fresh, .no_flags);
//     JH_to_LH();
//     L_to_SR(.next_ip);
//     next_cycle();

//     branch(.next_ip, 0);
// }

// pub fn _0185() void {
//     encoding(.BPN, .{});
//     desc("Branch to start of next page");

//     SRL_logic_literal_to_LL(.ip, .njl_nand_k, 0xF000, .fresh, .no_flags);
//     JH_to_LH();
//     L_to_SR(.next_ip);
//     next_cycle();

//     branch(.next_ip, 1);
// }

// pub fn _0186() void {
//     encoding(.BB, .{});
//     desc("Branch to start of current 256-byte-aligned block");

//     SRL_logic_literal_to_LL(.ip, .jl_and_k, 0xFF00, .fresh, .no_flags);
//     JH_to_LH();
//     L_to_SR(.next_ip);
//     next_cycle();

//     branch(.next_ip, 0);
// }

// pub fn _0187() void {
//     encoding(.BBN, .{});
//     desc("Branch to start of next 256-byte-aligned block");

//     SRL_logic_literal_to_LL(.ip, .jl_or_k, 0xFF, .fresh, .no_flags);
//     JH_to_LH();
//     L_to_SR(.next_ip);
//     next_cycle();

//     branch(.next_ip, 1);
// }

// pub fn _0188() void {
//     encoding(.EAB, .{ addr(.insn, .X0) });
//     desc("Enable address translation + register unconditional branch");

//     if (!kernel()) {
//         illegal_instruction();
//         return;
//     }

//     reg32_to_L(0);
//     L_to_SR(.next_ip);
//     enable_address_translation();
//     next_cycle();

//     branch(.next_ip, 0);
// }

// pub fn _0189() void {
//     encoding(.DAB, .{ addr(.insn, .X0) });
//     desc("Disable address translation + register unconditional branch");

//     if (!kernel()) {
//         illegal_instruction();
//         return;
//     }

//     reg32_to_L(0);
//     L_to_SR(.next_ip);
//     disable_address_translation();
//     next_cycle();

//     branch(.next_ip, 0);
// }

// fn firstCondition(condition: MnemonicSuffix) MnemonicSuffix {
//     return switch (condition) {
//         .LU_GU, .LU_Z => return .LU,
//         .GU_Z => return .GU,
//         .LS_GS, .LS_Z => return .LS,
//         .GS_Z => return .GS,
//         .N_Z, .N_P => return .N,
//         .P_Z => return .P,
//         else => unreachable,
//     };
// }

// fn secondCondition(condition: MnemonicSuffix) MnemonicSuffix {
//     return switch (condition) {
//         .LU_Z, .GU_Z, .LS_Z, .GS_Z, .N_Z, .P_Z => return .Z,
//         .LU_GU => return .GU,
//         .LS_GS => return .GS,
//         .N_P => return .P,
//         else => unreachable,
//     };
// }

// fn conditional_double_branch_s16(comptime condition: MnemonicSuffix) void {
//     encodingWithSuffix(.B, condition, .{ IP_relative(.insn, .imm16s), IP_relative(.insn, .@"imm16s@4") });
//     desc("IP-relative ternary branch if " ++ comptime conditionName(condition));

//     const first = comptime firstCondition(condition);
//     const second = comptime secondCondition(condition);

//     assert(!conditionIsInverse(first));
//     assert(!conditionIsInverse(second));

//     if (conditionFn(first)()) {
//         IP_read_to_D(2, .word);
//     } else if (conditionFn(second)()) {
//         IP_read_to_D(4, .word);
//     } else {
//         load_and_exec_next_insn_no_atomic_end(6);
//         return;
//     }
//     D_to_L(.zx);
//     L_to_SR(.temp_1);
//     next_cycle();

//     SR_plus_SRL_to_L(.ip, .temp_1, .sx, .fresh, .no_flags);
//     L_to_SR(.next_ip);
//     next_cycle();

//     branch(.next_ip, 0);
// }

// pub fn _0898() void { conditional_double_branch_s16(.LU_GU); }
// pub fn _0899() void { conditional_double_branch_s16(.LU_Z); }
// pub fn _089A() void { conditional_double_branch_s16(.GU_Z); }
// pub fn _0C98() void { conditional_double_branch_s16(.LS_GS); }
// pub fn _0C99() void { conditional_double_branch_s16(.LS_Z); }
// pub fn _0C9A() void { conditional_double_branch_s16(.GS_Z); }
// pub fn _0C96() void { conditional_double_branch_s16(.N_Z); }
// pub fn _0C97() void { conditional_double_branch_s16(.P_Z); }
// pub fn _0C9B() void { conditional_double_branch_s16(.N_P); }


const Cycle = @import("Cycle.zig");
const Encoder = isa.Instruction_Encoding.Encoder;
const Instruction_Signature = isa.Instruction_Signature;
const Parameter = isa.Parameter;
const Int = placeholders.Int;
const Range = placeholders.Range;
const Reg = placeholders.Reg;
const placeholders = @import("placeholders.zig");
const opcodes = @import("opcodes.zig");
const Flags = arch.hw.microcode.Flags;
const isa = arch.isa;
const arch = @import("lib_arch");
