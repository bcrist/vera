pub const instructions = .{
    struct { pub const spec =
        \\c x(src) -> sp
        \\c x(src) -> sp
        \\c x(src) -> sp
        ;
        // pub const encoding = .{
        //     opcode,
        //     Encoder.shifted(12, Reg(.src)),
        // };

        // fn opcode(params: []const Parameter.Signature) opcodes.Lo12 {
        //     return switch (params[1].base.sr) {
        //         .sp => .copy_reg32_to_sp,
        //     };
        // }

        // pub fn entry(c: *Cycle, params: []const Parameter.Signature) void {
        //     c.reg32_to_l();
        //     c.l_to_sr(switch (params[1].base.sr) {
        //         .sp => .sp,
        //     });
        //     c.load_and_exec_next_insn();
        // }
    },
};


pub fn _0008() void {
    encoding(.UNFRAME, .{ .R0U });
    desc("Add 16b register to stack pointer");

    SR_plus_reg_to_L(.sp, 0, .zx, .fresh, .no_flags);
    L_to_SR(.sp);
    load_and_exec_next_insn(2);
}

pub fn _0009() void {
    encoding(.FRAME, .{ .R0U });
    desc("Subtract 16b register from stack pointer");

    SR_minus_reg_to_L(.sp, 0, .zx, .fresh, .no_flags);
    L_to_SR(.sp);
    load_and_exec_next_insn(2);
}

pub fn _5A00_5AFF() void {
    encoding(.UNFRAME, .{ .immba8u });
    desc("Add immediate to stack pointer");

    SR_to_J(.sp);
    OB_OA_to_K();
    add_to_L(.zx, .fresh, .no_flags);
    L_to_SR(.sp);
    load_and_exec_next_insn(2);
}

pub fn _5B00_5BFF() void {
    encoding(.FRAME, .{ .immba8u });
    desc("Subtract immediate from stack pointer");

    SR_to_J(.sp);
    OB_OA_to_K();
    sub_to_L(.zx, .fresh, .no_flags);
    L_to_SR(.sp);
    load_and_exec_next_insn(2);
}

pub fn _0004() void {
    encoding(.UNFRAME, .{ .imm16u });
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
            desc("Pop unsigned byte from stack to 16b register");
            ext = .zx;
        },
        0x9 => {
            encoding(.POP, .{ .BaS });
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

const Cycle = @import("Cycle.zig");
const Encoder = isa.Instruction_Encoding.Encoder;
const Parameter = isa.Parameter;
const Int = placeholders.Int;
const Range = placeholders.Range;
const Reg = placeholders.Reg;
const placeholders = @import("placeholders.zig");
const opcodes = @import("opcodes.zig");
const isa = arch.isa;
const arch = @import("lib_arch");
