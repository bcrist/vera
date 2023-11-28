pub const instructions = .{
    struct { pub const spec = // add x(src), (imm8s) -> x(dest)
        \\add x(src), (imm) -> x(dest)
        \\add x(src) .signed, (imm) -> x(dest) .signed
        \\add x(src) .unsigned, (imm) -> x(dest) .unsigned
        ;
        pub const encoding = .{
            opcodes.Lo8.add_reg32_s8,
            Encoder.shifted(8, Reg(.src)),
            Encoder.shifted(12, Reg(.dest)),
            Encoder.shifted(16, Int(.imm, i8)),
        };
        pub const ij = Reg(.src);
        pub const iw = Reg(.dest);

        pub fn entry(c: *Cycle) void {
            c.ip_read_to_d(2, .byte);
            c.d_to_l(.sx);
            c.l_to_sr(.temp_1);
            c.next(add);
        }

        pub fn add(c: *Cycle) void {
            c.reg32_to_j();
            c.srl_to_k(.temp_1);
            c.j_plus_k_to_l(.sx, .fresh, .flags);
            c.l_to_reg32();
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = // add x(src), (imm16u) -> x(dest)
        \\add x(src), (imm) -> x(dest)
        \\add x(src) .signed, (imm) -> x(dest) .signed
        \\add x(src) .unsigned, (imm) -> x(dest) .unsigned
        ;
        pub const encoding = .{
            opcodes.Lo8.add_reg32_u16,
            Encoder.shifted(8, Reg(.src)),
            Encoder.shifted(12, Reg(.dest)),
            Encoder.shifted(16, Int(.imm, u16)),
        };
        pub const ij = Reg(.src);
        pub const iw = Reg(.dest);

        pub fn entry(c: *Cycle) void {
            c.ip_read_to_d(2, .word);
            c.d_to_l(.zx);
            c.l_to_sr(.temp_1);
            c.next(add);
        }

        pub fn add(c: *Cycle) void {
            c.reg32_to_j();
            c.srl_to_k(.temp_1);
            c.j_plus_k_to_l(.zx, .fresh, .flags);
            c.l_to_reg32();
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = // add x(src), (imm16n) -> x(dest)";
        \\add x(src), (imm) -> x(dest)
        \\add x(src) .signed, (imm) -> x(dest) .signed
        \\add x(src) .unsigned, (imm) -> x(dest) .unsigned
        ;
        pub const encoding = .{
            opcodes.Lo8.add_reg32_n16,
            Encoder.shifted(8, Reg(.src)),
            Encoder.shifted(12, Reg(.dest)),
            Encoder.shifted(16, Range(.imm, -0x10000, -1)),
        };
        pub const ij = Reg(.src);
        pub const iw = Reg(.dest);

        pub fn entry(c: *Cycle) void {
            c.ip_read_to_d(2, .word);
            c.d_to_l(.zx);
            c.l_to_sr(.temp_1);
            c.next(add);
        }

        pub fn add(c: *Cycle) void {
            c.reg32_to_j();
            c.srl_to_k(.temp_1);
            c.j_plus_k_to_l(._1x, .fresh, .flags);
            c.l_to_reg32();
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = // add x(a), r(b) .unsigned -> x(a)
        \\add x(a), r(b) .unsigned
        \\add x(a), r(b) .unsigned -> x(a)
        \\add x(a) .signed, r(b) .unsigned
        \\add x(a) .signed, r(b) .unsigned -> x(a) .signed
        \\add x(a) .unsigned, r(b) .unsigned
        \\add x(a) .unsigned, r(b) .unsigned -> x(a) .unsigned
        ;
        pub const encoding = .{
            opcodes.Lo8.add_reg32_reg16u,
            Encoder.shifted(8, Reg(.a)),
            Encoder.shifted(12, Reg(.b)),
        };
        pub const ij = Reg(.a);
        pub const ik = Reg(.b);
        pub const iw = Reg(.a);

        pub fn entry(c: *Cycle) void {
            c.reg32_to_j();
            c.reg_to_k();
            c.j_plus_k_to_l(.zx, .fresh, .flags);
            c.l_to_reg32();
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = // add x(a), r(b) .signed -> x(a)
        \\add x(a), r(b) .signed
        \\add x(a), r(b) .signed -> x(a)
        \\add x(a) .signed, r(b) .signed
        \\add x(a) .signed, r(b) .signed -> x(a) .signed
        \\add x(a) .unsigned, r(b) .signed
        \\add x(a) .unsigned, r(b) .signed -> x(a) .unsigned
        ;
        pub const encoding = .{
            opcodes.Lo8.add_reg32_reg16s,
            Encoder.shifted(8, Reg(.a)),
            Encoder.shifted(12, Reg(.b)),
        };
        pub const ij = Reg(.a);
        pub const ik = Reg(.b);
        pub const iw = Reg(.a);

        pub fn entry(c: *Cycle) void {
            c.reg32_to_j();
            c.reg_to_k();
            c.j_plus_k_to_l(.sx, .fresh, .flags);
            c.l_to_reg32();
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = // add[c] r(src), (imm8s) -> r(dest)
        \\add r(src), (imm) -> r(dest)
        \\add r(src) .signed, (imm) -> r(dest) .signed
        \\add r(src) .unsigned, (imm) -> r(dest) .unsigned
        \\addc r(src), (imm) -> r(dest)
        \\addc r(src) .signed, (imm) -> r(dest) .signed
        \\addc r(src) .unsigned, (imm) -> r(dest) .unsigned
        ;
        pub const encoding = .{
            opcode,
            Encoder.shifted(8, Reg(.src)),
            Encoder.shifted(12, Reg(.dest)),
            Encoder.shifted(16, Int(.imm, i8)),
        };
        pub const ij = Reg(.src);
        pub const iw = Reg(.dest);

        fn opcode(mnemonic: isa.Mnemonic) opcodes.Lo8 {
            return switch (mnemonic) {
                .add => .add_reg16_s8,
                .addc => .add_reg16_s8_with_carry,
                else => unreachable,
            };
        }

        pub fn entry(c: *Cycle) void {
            c.ip_read_to_d(2, .byte);
            c.d_to_l(.sx);
            c.l_to_sr(.temp_1);
            c.next(add);
        }

        pub fn add(c: *Cycle, mnemonic: isa.Mnemonic) void {
            c.reg_to_jl();
            c.srl_to_k(.temp_1);
            c.jl_plus_k_to_ll(switch (mnemonic) {
                .add => .fresh,
                .addc => .cont,
                else => unreachable,
            }, .flags);
            c.ll_to_reg();
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = // add[c] r(src), (imm16u) -> r(dest)
        \\add r(src), (imm) -> r(dest)
        \\add r(src) .signed, (imm) -> r(dest) .signed
        \\add r(src) .unsigned, (imm) -> r(dest) .unsigned
        \\addc r(src), (imm) -> r(dest)
        \\addc r(src) .signed, (imm) -> r(dest) .signed
        \\addc r(src) .unsigned, (imm) -> r(dest) .unsigned
        ;
        pub const encoding = .{
            opcode,
            Encoder.shifted(8, Reg(.src)),
            Encoder.shifted(12, Reg(.dest)),
            Encoder.shifted(16, Int(.imm, u16)),
        };
        pub const ij = Reg(.src);
        pub const iw = Reg(.dest);

        fn opcode(mnemonic: isa.Mnemonic) opcodes.Lo8 {
            return switch (mnemonic) {
                .add => .add_reg16_i16,
                .addc => .add_reg16_i16_with_carry,
                else => unreachable,
            };
        }

        pub fn entry(c: *Cycle) void {
            c.ip_read_to_d(2, .word);
            c.d_to_l(.zx);
            c.l_to_sr(.temp_1);
            c.next(add);
        }

        pub fn add(c: *Cycle, mnemonic: isa.Mnemonic) void {
            c.reg_to_jl();
            c.srl_to_k(.temp_1);
            c.jl_plus_k_to_ll(switch (mnemonic) {
                .add => .fresh,
                .addc => .cont,
                else => unreachable,
            }, .flags);
            c.ll_to_reg();
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = // add[c] r(src), (imm16s) -> r(dest)
        \\add r(src), (imm) -> r(dest)
        \\add r(src) .signed, (imm) -> r(dest) .signed
        \\add r(src) .unsigned, (imm) -> r(dest) .unsigned
        \\addc r(src), (imm) -> r(dest)
        \\addc r(src) .signed, (imm) -> r(dest) .signed
        \\addc r(src) .unsigned, (imm) -> r(dest) .unsigned
        ;
        pub const encoding = .{
            opcode,
            Encoder.shifted(8, Reg(.src)),
            Encoder.shifted(12, Reg(.dest)),
            Encoder.shifted(16, Int(.imm, i16)),
        };
        pub const ij = Reg(.src);
        pub const iw = Reg(.dest);

        fn opcode(mnemonic: isa.Mnemonic) opcodes.Lo8 {
            return switch (mnemonic) {
                .add => .add_reg16_i16,
                .addc => .add_reg16_i16_with_carry,
                else => unreachable,
            };
        }

        pub fn entry(c: *Cycle) void {
            c.ip_read_to_d(2, .word);
            c.d_to_l(.zx);
            c.l_to_sr(.temp_1);
            c.next(add);
        }

        pub fn add(c: *Cycle, mnemonic: isa.Mnemonic) void {
            c.reg_to_jl();
            c.srl_to_k(.temp_1);
            c.jl_plus_k_to_ll(switch (mnemonic) {
                .add => .fresh,
                .addc => .cont,
                else => unreachable,
            }, .flags);
            c.ll_to_reg();
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = // add[c] r(a), r(b) -> r(a)
        \\add r(a), r(b)
        \\add r(a), r(b) -> r(a)
        \\add r(a) .signed, r(b) .signed
        \\add r(a) .signed, r(b) .signed -> r(a) .signed
        \\add r(a) .unsigned, r(b) .unsigned
        \\add r(a) .unsigned, r(b) .unsigned -> r(a) .unsigned
        \\addc r(a), r(b)
        \\addc r(a), r(b) -> r(a)
        \\addc r(a) .signed, r(b) .signed
        \\addc r(a) .signed, r(b) .signed -> r(a) .signed
        \\addc r(a) .unsigned, r(b) .unsigned
        \\addc r(a) .unsigned, r(b) .unsigned -> r(a) .unsigned
        ;
        pub const encoding = .{
            opcode,
            Encoder.shifted(8, Reg(.a)),
            Encoder.shifted(12, Reg(.b)),
        };
        pub const ij = Reg(.a);
        pub const ik = Reg(.b);
        pub const iw = Reg(.a);

        fn opcode(mnemonic: isa.Mnemonic) opcodes.Lo8 {
            return switch (mnemonic) {
                .add => .add_reg16_reg16,
                .addc => .add_reg16_reg16_with_carry,
                else => unreachable,
            };
        }

        pub fn entry(c: *Cycle, mnemonic: isa.Mnemonic) void {
            c.reg_to_jl();
            c.reg_to_k();
            c.jl_plus_k_to_ll(switch (mnemonic) {
                .add => .fresh,
                .addc => .cont,
                else => unreachable,
            }, .flags);
            c.ll_to_reg();
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = // cmp[b] r(a), r(b)
        \\cmp r(a), r(b)
        \\cmp r(a) .signed, r(b) .signed
        \\cmp r(a) .unsigned, r(b) .unsigned
        \\cmpb r(a), r(b)
        \\cmpb r(a) .signed, r(b) .signed
        \\cmpb r(a) .unsigned, r(b) .unsigned
        ;
        pub const encoding = .{
            opcode,
            Encoder.shifted(8, Reg(.a)),
            Encoder.shifted(12, Reg(.b)),
        };
        pub const ij = Reg(.a);
        pub const ik = Reg(.b);

        fn opcode(mnemonic: isa.Mnemonic) opcodes.Lo8 {
            return switch (mnemonic) {
                .cmp => .compare_reg16_reg16,
                .cmpb => .compare_reg16_reg16_with_borrow,
                else => unreachable,
            };
        }

        pub fn entry(c: *Cycle, mnemonic: isa.Mnemonic) void {
            c.reg_to_jl();
            c.reg_to_k();
            c.jl_minus_k(switch (mnemonic) {
                .cmp => .fresh,
                .cmpb => .cont,
                else => unreachable,
            }, .flags);
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = // cmp x(a), r(b)
        \\cmp x(a), r(b) .signed
        \\cmp x(a), r(b) .unsigned
        \\cmp x(a) .signed, r(b) .signed
        \\cmp x(a) .signed, r(b) .unsigned
        \\cmp x(a) .unsigned, r(b) .signed
        \\cmp x(a) .unsigned, r(b) .unsigned
        ;
        pub const encoding = .{
            opcode,
            Encoder.shifted(8, Reg(.a)),
            Encoder.shifted(12, Reg(.b)),
        };
        pub const ij = Reg(.a);
        pub const ik = Reg(.b);

        fn opcode(signature: isa.Instruction_Signature) opcodes.Lo8 {
            return switch (signature.params[1].base.reg16.?) {
                .signed => .compare_reg32_reg16s,
                .unsigned => .compare_reg32_reg16u,
            };
        }

        pub fn entry(c: *Cycle, signature: isa.Instruction_Signature) void {
            c.reg32_to_j();
            c.reg_to_k();
            c.j_minus_k(switch (signature.params[1].base.reg16.?) {
                .signed => .sx,
                .unsigned => .zx,
            }, .fresh, .flags);
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = // cmp[b] r(a), (imm15u)
        \\cmp r(a), (imm)
        \\cmpb r(a), (imm)
        ;
        pub const constraints = .{
            .{ .imm, .less_than, 0x8000 },
        };
        pub const encoding = .{
            opcode,
            Encoder.shifted(12, Reg(.a)),
            Encoder.shifted(16, Int(.imm, u16)),
        };
        pub const ij = Reg(.a);

        fn opcode(mnemonic: isa.Mnemonic) opcodes.Lo12 {
            return switch (mnemonic) {
                .cmp => .compare_reg16_i16,
                .cmpb => .compare_reg16_i16_with_borrow,
                else => unreachable,
            };
        }

        pub fn entry(c: *Cycle) void {
            c.ip_read_to_d(2, .word);
            c.d_to_l(.zx);
            c.l_to_sr(.temp_1);
            c.next(compare);
        }

        pub fn compare(c: *Cycle, mnemonic: isa.Mnemonic) void {
            c.reg_to_jl();
            c.srl_to_k(.temp_1);
            c.jl_minus_k(switch (mnemonic) {
                .cmp => .fresh,
                .cmpb => .cont,
                else => unreachable,
            }, .flags);
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = // cmp[b] r(a), (imm16u)
        \\cmp r(a) .unsigned, (imm)
        \\cmpb r(a) .unsigned, (imm)
        ;
        pub const encoding = .{
            opcode,
            Encoder.shifted(12, Reg(.a)),
            Encoder.shifted(16, Int(.imm, u16)),
        };
        pub const ij = Reg(.a);

        fn opcode(mnemonic: isa.Mnemonic) opcodes.Lo12 {
            return switch (mnemonic) {
                .cmp => .compare_reg16_i16,
                .cmpb => .compare_reg16_i16_with_borrow,
                else => unreachable,
            };
        }

        pub fn entry(c: *Cycle) void {
            c.ip_read_to_d(2, .word);
            c.d_to_l(.zx);
            c.l_to_sr(.temp_1);
            c.next(compare);
        }

        pub fn compare(c: *Cycle, mnemonic: isa.Mnemonic) void {
            c.reg_to_jl();
            c.srl_to_k(.temp_1);
            c.jl_minus_k(switch (mnemonic) {
                .cmp => .fresh,
                .cmpb => .cont,
                else => unreachable,
            }, .flags);
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = // cmp[b] r(a), (imm16s)
        \\cmp r(a) .signed, (imm)
        \\cmpb r(a) .signed, (imm)
        ;
        pub const encoding = .{
            opcode,
            Encoder.shifted(12, Reg(.a)),
            Encoder.shifted(16, Int(.imm, i16)),
        };
        pub const ij = Reg(.a);

        fn opcode(mnemonic: isa.Mnemonic) opcodes.Lo12 {
            return switch (mnemonic) {
                .cmp => .compare_reg16_i16,
                .cmpb => .compare_reg16_i16_with_borrow,
                else => unreachable,
            };
        }

        pub fn entry(c: *Cycle) void {
            c.ip_read_to_d(2, .word);
            c.d_to_l(.zx);
            c.l_to_sr(.temp_1);
            c.next(compare);
        }

        pub fn compare(c: *Cycle, mnemonic: isa.Mnemonic) void {
            c.reg_to_jl();
            c.srl_to_k(.temp_1);
            c.jl_minus_k(switch (mnemonic) {
                .cmp => .fresh,
                .cmpb => .cont,
                else => unreachable,
            }, .flags);
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = // cmp x(a), (imm16u)
        \\cmp x(a), (imm)
        \\cmp x(a) .signed, (imm)
        \\cmp x(a) .unsigned, (imm)
        ;
        pub const encoding = .{
            opcodes.Lo12.compare_reg32_u16,
            Encoder.shifted(12, Reg(.a)),
            Encoder.shifted(16, Int(.imm, u16)),
        };
        pub const ij = Reg(.a);

        pub fn entry(c: *Cycle) void {
            c.ip_read_to_d(2, .word);
            c.d_to_l(.zx);
            c.l_to_sr(.temp_1);
            c.next(compare);
        }

        pub fn compare(c: *Cycle) void {
            c.reg32_to_j();
            c.srl_to_k(.temp_1);
            c.j_minus_k(.zx, .fresh, .flags);
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = // cmp x(a), (imm16n)
        \\cmp x(a) .signed, (imm)
        ;
        pub const encoding = .{
            opcodes.Lo12.compare_reg32_n16,
            Encoder.shifted(12, Reg(.a)),
            Encoder.shifted(16, Range(.imm, -0x10000, -1)),
        };
        pub const ij = Reg(.a);

        pub fn entry(c: *Cycle) void {
            c.ip_read_to_d(2, .word);
            c.d_to_l(.zx);
            c.l_to_sr(.temp_1);
            c.next(compare);
        }

        pub fn compare(c: *Cycle) void {
            c.reg32_to_j();
            c.reg_to_k();
            c.j_minus_k(._1x, .fresh, .flags);
            c.load_and_exec_next_insn();
        }
    },
};



// pub fn _8300_84FF() void {
//     var ext: cb.ZeroSignOrOneExtension = undefined;
//     switch (opcode_high()) {
//         0x83 => {
//             encoding(.SUB, .{ .Xa, .RbU, .to, .Xa });
//             desc("Subtract unsigned 16b register from 32b register");
//             ext = .zx;
//         },
//         0x84 => {
//             encoding(.SUB, .{ .Xa, .RbS, .to, .Xa });
//             desc("Subtract signed 16b register from 32b register");
//             ext = .sx;
//         },
//         else => unreachable,
//     }

//     op_reg32_minus_op_reg_to_L(.OA, .OB, ext, .fresh, .flags);
//     L_to_op_reg32(.OA);
//     load_and_exec_next_insn(2);
// }

// pub fn _8500_86FF() void {
//     var freshness: cb.Freshness = undefined;
//     switch (opcode_high()) {
//         0x85 => {
//             encoding(.SUB, .{ .imm8s, .Rb, .to, .Ra });
//             desc("Subtract register from immediate and store result in another register");
//             freshness = .fresh;
//         },
//         0x86 => {
//             encoding(.SUBB, .{ .imm8s, .Rb, .to, .Ra });
//             desc("Subtract register from immediate and store result in another register, with borrow");
//             freshness = .cont;
//         },
//         else => unreachable,
//     }

//     IP_read_to_D(2, .byte);
//     D_to_L(.sx);
//     L_to_SR(.temp_1);
//     next_cycle();

//     SRL_minus_op_reg_to_LL(.temp_1, .OB, freshness, .flags);
//     LL_to_op_reg(.OA);
//     load_and_exec_next_insn(3);
// }

// pub fn _8700_87FF() void {
//     encoding(.SUB, .{ .imm16u, .RbU, .to, .RaU });
//     desc("Subtract register from immediate and store result in another register");

//     IP_read_to_D(2, .word);
//     D_to_L(.zx);
//     L_to_SR(.temp_1);
//     next_cycle();

//     SRL_minus_op_reg_to_LL(.temp_1, .OB, .fresh, .flags);
//     LL_to_op_reg(.OA);
//     load_and_exec_next_insn(4);
// }
// pub fn _alias_8700_87FF_signed() void {
//     encoding(.SUB, .{ .imm16s, .RbS, .to, .RaS });
//     desc("Subtract register from immediate and store result in another register");
// }

// pub fn _8800_88FF() void {
//     encoding(.SUBB, .{ .imm16u, .RbU, .to, .RaU });
//     desc("Subtract register from immediate and store result in another register, with borrow");

//     IP_read_to_D(2, .word);
//     D_to_L(.zx);
//     L_to_SR(.temp_1);
//     next_cycle();

//     SRL_minus_op_reg_to_LL(.temp_1, .OB, .cont, .flags);
//     LL_to_op_reg(.OA);
//     load_and_exec_next_insn(4);
// }
// pub fn _alias_8800_88FF_signed() void {
//     encoding(.SUBB, .{ .imm16s, .RbS, .to, .RaS });
//     desc("Subtract register from immediate and store result in another register, with borrow");
// }

// pub fn _8900_8AFF() void {
//     var freshness: cb.Freshness = undefined;
//     switch (opcode_high()) {
//         0x89 => {
//             encoding(.SUB, .{ .Ra, .Rb, .to, .Ra });
//             desc("Subtract register from another register");
//             freshness = .fresh;
//         },
//         0x8A => {
//             encoding(.SUBB, .{ .Ra, .Rb, .to, .Ra });
//             desc("Subtract register from another register, with borrow");
//             freshness = .cont;
//         },
//         else => unreachable,
//     }

//     op_reg_minus_op_reg_to_LL(.OA, .OB, freshness, .flags);
//     LL_to_op_reg(.OA);
//     load_and_exec_next_insn(2);
// }

// pub fn _9300_931F() void {
//     var freshness: cb.Freshness = undefined;
//     switch (OB()) {
//         0x0 => {
//             encoding(.INC, .{ .Ra, .to, .Ra });
//             desc("Increment 16b register");
//             freshness = .fresh;
//         },
//         0x1 => {
//             encoding(.INCC, .{ .Ra, .to, .Ra });
//             desc("Increment 16b register, with carry");
//             freshness = .cont;
//         },
//         else => unreachable,
//     }

//     op_reg_plus_literal_to_LL(.OA, 1, freshness, .flags);
//     LL_to_op_reg(.OA);
//     load_and_exec_next_insn(2);
// }

// pub fn _9320_933F() void {
//     var freshness: cb.Freshness = undefined;
//     switch (OB()) {
//         0x2 => {
//             encoding(.INC, .{ .Xa, .to, .Xa });
//             desc("Increment 32b register");
//             freshness = .fresh;
//         },
//         0x3 => {
//             encoding(.INCC, .{ .Xa, .to, .Xa });
//             desc("Increment 32b register, with carry");
//             freshness = .cont;
//         },
//         else => unreachable,
//     }

//     op_reg32_plus_literal_to_L(.OA, 1, freshness, .flags);
//     L_to_op_reg32(.OA);
//     load_and_exec_next_insn(2);
// }

// pub fn _9340_935F() void {
//     var freshness: cb.Freshness = undefined;
//     switch (OB()) {
//         0x4 => {
//             encoding(.DEC, .{ .Ra, .to, .Ra });
//             desc("Decrement 16b register");
//             freshness = .fresh;
//         },
//         0x5 => {
//             encoding(.DECB, .{ .Ra, .to, .Ra });
//             desc("Decrement 16b register, with borrow");
//             freshness = .cont;
//         },
//         else => unreachable,
//     }

//     op_reg_minus_literal_to_LL(.OA, 1, freshness, .flags);
//     LL_to_op_reg(.OA);
//     load_and_exec_next_insn(2);
// }

// pub fn _9360_937F() void {
//     var freshness: cb.Freshness = undefined;
//     switch (OB()) {
//         0x6 => {
//             encoding(.DEC, .{ .Xa, .to, .Xa });
//             desc("Decrement 32b register");
//             freshness = .fresh;
//         },
//         0x7 => {
//             encoding(.DECB, .{ .Xa, .to, .Xa });
//             desc("Decrement 32b register, with borrow");
//             freshness = .cont;
//         },
//         else => unreachable,
//     }

//     op_reg32_minus_literal_to_L(.OA, 1, freshness, .flags);
//     L_to_op_reg32(.OA);
//     load_and_exec_next_insn(2);
// }

// pub fn _9400_94FF() void {
//     encoding(.NEG, .{ .Rb, .to, .Ra });
//     desc("Negate 16b register");

//     zero_minus_op_reg_to_LL(.OB, .fresh, .flags);
//     L_to_op_reg32(.OA);
//     load_and_exec_next_insn(2);
// }

// pub fn _9500_95FF() void {
//     encoding(.NEGB, .{ .Rb, .to, .Ra });
//     desc("Negate 16b register, with borrow");

//     zero_minus_op_reg_to_LL(.OB, .cont, .flags);
//     L_to_op_reg32(.OA);
//     load_and_exec_next_insn(2);
// }

// pub fn _93A0_93AF() void {
//     encoding(.NOT, .{ .Ra, .to, .Ra });
//     desc("Bitwise complement of 16b register");

//     op_reg_logic_literal_to_LL(.OA, .jl_xor_k, -1, .fresh, .flags);
//     LL_to_op_reg(.OA);
//     load_and_exec_next_insn(2);
// }

// pub fn _B000_B6FF() void {
//     var op: ControlSignals.LogicMode = undefined;
//     var mn: Mnemonic = undefined;
//     var d: []const u8 = undefined;
//     switch (opcode_high()) {
//         0xB0 => { mn = .XOR;    op = .jl_xor_k;  d = "Bitwise XOR of two 16b registers"; },
//         0xB1 => { mn = .XNOR;   op = .jl_xnor_k; d = "Bitwise XNOR of two 16b registers"; },
//         0xB2 => { mn = .OR;     op = .jl_or_k;   d = "Bitwise OR of two 16b registers"; },
//         0xB3 => { mn = .NOR;    op = .jl_nor_k;  d = "Bitwise NOR of two 16b registers"; },
//         0xB4 => { mn = .AND;    op = .jl_and_k;  d = "Bitwise AND of two 16b registers"; },
//         0xB5 => { mn = .NAND;   op = .jl_nand_k; d = "Bitwise NAND of two 16b registers"; },
//         0xB6 => { mn = .ANDNOT; op = .njl_nor_k; d = "Bitwise AND of two 16b registers (second register is complemented)"; },
//         else => unreachable,
//     }

//     encoding(mn, .{ .Ra, .Rb, .to, .Ra });
//     desc(d);

//     op_reg_logic_op_reg_to_LL(.OA, op, .OB, .fresh, .flags);
//     LL_to_op_reg(.OA);
//     load_and_exec_next_insn(2);
// }

// pub fn _C100_C5FF() void {
//     var op: ControlSignals.LogicMode = undefined;
//     var mn: Mnemonic = undefined;
//     var d: []const u8 = undefined;
//     switch (opcode_high()) {
//         0xC1 => { mn = .XOR;  op = .jl_xor_k;  d = "Bitwise XOR of 16b register and immediate"; },
//         0xC2 => { mn = .OR;   op = .jl_or_k;   d = "Bitwise OR of 16b register and immediate"; },
//         0xC3 => { mn = .NOR;  op = .jl_nor_k;  d = "Bitwise NOR of 16b register and immediate"; },
//         0xC4 => { mn = .AND;  op = .jl_and_k;  d = "Bitwise AND of 16b register and immediate"; },
//         0xC5 => { mn = .NAND; op = .jl_nand_k; d = "Bitwise NAND of 16b register and immediate"; },
//         else => unreachable,
//     }

//     encoding(mn, .{ .RaU, .imm16u, .to, .RbU });
//     desc(d);

//     IP_read_to_D(2, .word);
//     D_to_L(.zx);
//     L_to_SR(.temp_1);
//     next_cycle();

//     op_reg_logic_SRL_to_LL(.OA, op, .temp_1, .fresh, .flags);
//     LL_to_op_reg(.OB);
//     load_and_exec_next_insn(4);
// }
// pub fn _alias_C100_C5FF_signed() void {
//     var mn: Mnemonic = undefined;
//     var d: []const u8 = undefined;
//     switch (opcode_high()) {
//         0xC1 => { mn = .XOR;  d = "Bitwise XOR of 16b register and immediate"; },
//         0xC2 => { mn = .OR;   d = "Bitwise OR of 16b register and immediate"; },
//         0xC3 => { mn = .NOR;  d = "Bitwise NOR of 16b register and immediate"; },
//         0xC4 => { mn = .AND;  d = "Bitwise AND of 16b register and immediate"; },
//         0xC5 => { mn = .NAND; d = "Bitwise NAND of 16b register and immediate"; },
//         else => unreachable,
//     }

//     encoding(mn, .{ .RaS, .imm16s, .to, .RbS });
//     desc(d);
// }

// pub fn _C600_C7FF() void {
//     var freshness: cb.Freshness = undefined;
//     switch (opcode_high()) {
//         0xC6 => {
//             encoding(.TEST, .{ .Ra, .Rb });
//             desc("Set Z and N flags based on bitwise AND of two 16b registers");
//             freshness = .fresh;
//         },
//         0xC7 => {
//             encoding(.TESTZ, .{ .Ra, .Rb });
//             desc("Set Z and N flags based on bitwise AND of two 16b registers (don't set Z if already clear)");
//             freshness = .cont;
//         },
//         else => unreachable,
//     }

//     op_reg_logic_op_reg_to_LL(.OA, .jl_and_k, .OB, freshness, .flags);
//     load_and_exec_next_insn(2);
// }

// pub fn _9100_91FF() void {
//     encoding(.TESTB, .{ .Ra, .immb4u });
//     desc("Set Z and N flags according to single bit from 16b register");

//     op_reg_logic_literal_to_LL(.OA, .jl_and_k, @as(u16, 1) << OB(), .fresh, .flags);
//     load_and_exec_next_insn(2);
// }

// pub fn _9200_92FF() void {
//     encoding(.TESTBZ, .{ .Ra, .immb4u });
//     desc("Set Z and N flags according to single bit from 16b register, without setting the Z flag if it is already clear");

//     op_reg_logic_literal_to_LL(.OA, .jl_and_k, @as(u16, 1) << OB(), .cont, .flags);
//     load_and_exec_next_insn(2);
// }

// pub fn _A000_A0FF() void {
//     encoding(.CLRB, .{ .Ra, .immb4u, .to, .Ra });
//     desc("Clear single bit in 16b register");

//     op_reg_logic_literal_to_LL(.OA, .njl_nor_k, @as(u16, 1) << OB(), .fresh, .flags);
//     LL_to_op_reg(.OA);
//     load_and_exec_next_insn(2);
// }

// pub fn _A100_A1FF() void {
//     encoding(.SETB, .{ .Ra, .immb4u, .to, .Ra });
//     desc("Set single bit in 16b register");

//     op_reg_logic_literal_to_LL(.OA, .jl_or_k, @as(u16, 1) << OB(), .fresh, .flags);
//     LL_to_op_reg(.OA);
//     load_and_exec_next_insn(2);
// }

// pub fn _A200_A2FF() void {
//     encoding(.TGLB, .{ .Ra, .immb4u, .to, .Ra });
//     desc("Toggle single bit in 16b register");

//     op_reg_logic_literal_to_LL(.OA, .jl_xor_k, @as(u16, 1) << OB(), .fresh, .flags);
//     LL_to_op_reg(.OA);
//     load_and_exec_next_insn(2);
// }

// pub fn _D000_D1FF() void {
//     var dir: cb.ShiftDirection = undefined;
//     switch (opcode_high()) {
//         0xD0 => {
//             encoding(.SHR, .{ .XaU, .Rb, .to, .XaU });
//             desc("Right shift 32b register 0-31 bits, zero extending");
//             dir = .right;
//         },
//         0xD1 => {
//             encoding(.SHL, .{ .Xa, .Rb, .to, .Xa });
//             desc("Left shift 32b register 0-31 bits");
//             dir = .left;
//         },
//         else => unreachable,
//     }

//     op_reg32_shift_op_reg_to_L(.OA, dir, .OB, .flags);
//     L_to_op_reg32(.OA);
//     load_and_exec_next_insn(2);
// }

// pub fn _D200_D4FF() void {
//     var dir: cb.ShiftDirection = undefined;
//     var ext: cb.ZeroSignOrOneExtension = undefined;
//     switch (opcode_high()) {
//         0xD2 => {
//             encoding(.SHR, .{ .RaU, .Rb, .to, .RaU });
//             desc("Shift right 16b register 0-15 bits, unsigned");
//             dir = .right;
//             ext = .zx;
//         },
//         0xD3 => {
//             encoding(.SHR, .{ .RaS, .Rb, .to, .RaS });
//             desc("Shift right 16b register 0-15 bits, signed");
//             dir = .right;
//             ext = .sx;
//         },
//         0xD4 => {
//             encoding(.SHL, .{ .Ra, .Rb, .to, .Ra });
//             desc("Shift left 16b register 0-15 bits");
//             dir = .left;
//             ext = .zx;
//         },
//         else => unreachable,
//     }

//     op_reg_shift_op_reg_to_LL(.OA, ext, dir, .OB, .flags);
//     LL_to_op_reg(.OA);
//     load_and_exec_next_insn(2);
// }

// pub fn _D500_D7FF() void {
//     var dir: cb.ShiftDirection = undefined;
//     var ext: cb.ZeroSignOrOneExtension = undefined;
//     switch (opcode_high()) {
//         0xD5 => {
//             encoding(.SHR, .{ .RaU, .immb4u, .to, .RaU });
//             desc("Shift right 16b register 0-15 bits, unsigned");
//             dir = .right;
//             ext = .zx;
//         },
//         0xD6 => {
//             encoding(.SHR, .{ .RaS, .immb4u, .to, .RaS });
//             desc("Shift right 16b register 0-15 bits, signed");
//             dir = .right;
//             ext = .sx;
//         },
//         0xD7 => {
//             encoding(.SHL, .{ .Ra, .immb4u, .to, .Ra });
//             desc("Shift left 16b register 0-15 bits");
//             dir = .left;
//             ext = .zx;
//         },
//         else => unreachable,
//     }

//     const amount = getParameterConstant(u4, 1);
//     op_reg_shift_literal_to_LL(.OA, ext, dir, amount, .flags);
//     LL_to_op_reg(.OA);
//     load_and_exec_next_insn(2);
// }

// pub fn _E000_E3FF() void {
//     var dir: cb.ShiftDirection = undefined;
//     switch (opcode_high()) {
//         0xE0 => {
//             encoding(.SHR, .{ .XaU, .immb4u, .to, .XaU });
//             desc("Right shift 32b register 0-31 bits, zero extending");
//             dir = .right;
//         },
//         0xE1 => {
//             encoding(.SHR, .{ .XaU, .immb4_16_31, .to, .XaU });
//             desc("Right shift 32b register 0-31 bits, zero extending");
//             dir = .right;
//         },
//         0xE2 => {
//             encoding(.SHL, .{ .Xa, .immb4u, .to, .Xa });
//             desc("Left shift 32b register 0-31 bits");
//             dir = .left;
//         },
//         0xE3 => {
//             encoding(.SHL, .{ .Xa, .immb4_16_31, .to, .Xa });
//             desc("Left shift 32b register 0-31 bits");
//             dir = .left;
//         },
//         else => unreachable,
//     }

//     const amount = getParameterConstant(u5, 1);
//     op_reg32_shift_literal_to_L(.OA, dir, amount, .flags);
//     L_to_op_reg32(.OA);
//     load_and_exec_next_insn(2);
// }

// pub fn _0900_090F() void {
//     encoding(.SHRC, .{ .Ra, .to, .Ra });
//     desc("Shift right 16b register 1 bit, shift Carry into MSB");

//     op_reg_shift_literal_to_LL(.OA, (if (carry_borrow()) ._1x else .zx), .right, 1, .flags);
//     LL_to_op_reg(.OA);
//     load_and_exec_next_insn(2);
// }

// pub fn _0910_091F() void {
//     encoding(.SHLC, .{ .Ra, .to, .Ra });
//     desc("Shift left 16b register 1 bit, shift Carry into LSB");

//     op_reg_shift_literal_to_LL(.OA, (if (carry_borrow()) ._1x else .zx), .left, 1, .flags);
//     LL_to_op_reg(.OA);
//     load_and_exec_next_insn(2);
// }

// pub fn _F000_F0FF() void {
//     encoding(.MUL, .{ .RaU, .imm16u, .to, .RbU });
//     desc("Multiply 16b register by immediate, truncate result to 16b and store in another register");

//     IP_read_to_D(2, .word);
//     D_to_L(.zx);
//     L_to_SR(.temp_1);
//     next_cycle();

//     op_reg_mult_SRL_to_LL(.OA, .zx, .temp_1, .zx, .normal, .flags);
//     LL_to_op_reg(.OB);
//     load_and_exec_next_insn(4);
// }
// pub fn _alias_F000_F0FF_signed() void {
//     encoding(.MUL, .{ .RaS, .imm16s, .to, .RbS });
//     desc("Multiply 16b register by immediate, truncate result to 16b and store in another register");
// }

// pub fn _F100_F1FF() void {
//     encoding(.MUL, .{ .Ra, .Rb, .to, .Ra });
//     desc("Multiply 16b registers, truncate result to 16b");

//     op_reg_mult_op_reg_to_LL(.OA, .zx, .OB, .zx, .normal, .flags);
//     LL_to_op_reg(.OA);
//     load_and_exec_next_insn(2);
// }

// pub fn _F200_F2FF() void {
//     encoding(.MULH, .{ .RaU, .RbU, .to, .RaU });
//     desc("Multiply 16b unsigned registers, store high 16b of result");

//     op_reg_mult_op_reg_to_LL(.OA, .zx, .OB, .zx, .swap, .flags);
//     LL_to_op_reg(.OA);
//     load_and_exec_next_insn(2);
// }

// pub fn _F300_F3FF() void {
//     encoding(.MULH, .{ .RaU, .RbS, .to, .RaS });
//     desc("Multiply 16b unsigned registers, store high 16b of result");

//     op_reg_mult_op_reg_to_LL(.OA, .sx, .OB, .zx, .swap, .flags);
//     LL_to_op_reg(.OA);
//     load_and_exec_next_insn(2);
// }

// pub fn _F400_F4FF() void {
//     encoding(.MULH, .{ .RaS, .RbU, .to, .RaS });
//     desc("Multiply 16b unsigned registers, store high 16b of result");

//     op_reg_mult_op_reg_to_LL(.OA, .sx, .OB, .zx, .swap, .flags);
//     LL_to_op_reg(.OA);
//     load_and_exec_next_insn(2);
// }

// pub fn _F500_F5FF() void {
//     encoding(.MULH, .{ .RaS, .RbS, .to, .RaS });
//     desc("Multiply 16b unsigned registers, store high 16b of result");

//     op_reg_mult_op_reg_to_LL(.OA, .sx, .OB, .sx, .swap, .flags);
//     LL_to_op_reg(.OA);
//     load_and_exec_next_insn(2);
// }

// pub fn _F600_F8FF() void {
//     var ext_l: cb.ZeroOrSignExtension = undefined;
//     var ext_r: cb.ZeroOrSignExtension = undefined;
//     switch (opcode_high()) {
//         0xF6 => {
//             encoding(.MUL, .{ .RaU, .RbU, .to, .X0U });
//             desc("Multiply 16b unsigned registers, store 32b result");
//             ext_l = .zx;
//             ext_r = .zx;
//         },
//         0xF7 => {
//             encoding(.MUL, .{ .RaS, .RbS, .to, .X0S });
//             desc("Multiply 16b signed registers, store 32b result");
//             ext_l = .sx;
//             ext_r = .sx;
//         },
//         0xF8 => {
//             encoding(.MUL, .{ .RaU, .RbS, .to, .X0S });
//             desc("Multiply 16b unsigned and signed registers, store 32b result");
//             ext_l = .zx;
//             ext_r = .sx;
//         },
//         else => unreachable,
//     }

//     op_reg_mult_op_reg_to_L(.OA, ext_l, .OB, ext_r, .flags);
//     L_to_reg32(0);
//     load_and_exec_next_insn(2);
// }

// pub fn _E400_E40F() void {
//     encoding(.CB, .{ .Ra, .to, .Ra });
//     desc("Count set bits in 16b register");

//     bitcount_op_reg_to_LL(.OA, .all, 1, .flags);
//     LL_to_op_reg(.OA);
//     load_and_exec_next_insn(2);
// }

// pub fn _E410_E41F() void {
//     encoding(.CZ, .{ .Ra, .to, .Ra });
//     desc("Count zero bits in 16b register");

//     bitcount_op_reg_to_LL(.OA, .all, 0, .flags);
//     LL_to_op_reg(.OA);
//     load_and_exec_next_insn(2);
// }

// pub fn _E420_E42F() void {
//     encoding(.CLB, .{ .Ra, .to, .Ra });
//     desc("Count leading (most significant) set bits in 16b register");

//     bitcount_op_reg_to_LL(.OA, .leading, 1, .flags);
//     LL_to_op_reg(.OA);
//     load_and_exec_next_insn(2);
// }

// pub fn _E430_E43F() void {
//     encoding(.CLZ, .{ .Ra, .to, .Ra });
//     desc("Count leading (most significant) zero bits in 16b register");

//     bitcount_op_reg_to_LL(.OA, .leading, 0, .flags);
//     LL_to_op_reg(.OA);
//     load_and_exec_next_insn(2);
// }

// pub fn _E440_E44F() void {
//     encoding(.CTB, .{ .Ra, .to, .Ra });
//     desc("Count trailing (least significant) set bits in 16b register");

//     bitcount_op_reg_to_LL(.OA, .trailing, 1, .flags);
//     LL_to_op_reg(.OA);
//     load_and_exec_next_insn(2);
// }

// pub fn _E450_E45F() void {
//     encoding(.CTZ, .{ .Ra, .to, .Ra });
//     desc("Count trailing (least significant) zero bits in 16b register");

//     bitcount_op_reg_to_LL(.OA, .trailing, 0, .flags);
//     LL_to_op_reg(.OA);
//     load_and_exec_next_insn(2);
// }


const Cycle = @import("Cycle.zig");
const Encoder = isa.Instruction_Encoding.Encoder;
const Int = placeholders.Int;
const Range = placeholders.Range;
const Reg = placeholders.Reg;
const placeholders = @import("placeholders.zig");
const opcodes = @import("opcodes.zig");
const isa = arch.isa;
const arch = @import("lib_arch");
