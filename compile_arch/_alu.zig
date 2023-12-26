pub const instructions = .{
    struct { pub const spec = "sub r(src), (imm) -> r(dest)";
        pub const transform = "add r(src), (imm) -> r(dest)";
        pub const conversions = .{
            .{ .src, .src },
            .{ .dest, .dest },
            .{ .imm, .imm, .negate },
        };
    },
    struct { pub const spec = "subc r(src), (imm) -> r(dest)";
        pub const transform = "addc r(src), (imm) -> r(dest)";
        pub const conversions = .{
            .{ .src, .src },
            .{ .dest, .dest },
            .{ .imm, .imm, .negate, .{ .offset = -1 } },
        };
    },
    struct { pub const spec = "add (imm), r(src) -> r(dest)";
        pub const transform = "add r(src), (imm) -> r(dest)";
        pub const conversions = .{
            .{ .imm, .imm },
            .{ .src, .src },
            .{ .dest, .dest },
        };
    },
    struct { pub const spec = "addc (imm), r(src) -> r(dest)";
        pub const transform = "addc r(src), (imm) -> r(dest)";
        pub const conversions = .{
            .{ .imm, .imm },
            .{ .src, .src },
            .{ .dest, .dest },
        };
    },
    struct { pub const spec = "add (imm), x(src) -> x(dest)";
        pub const transform = "add x(src), (imm) -> x(dest)";
        pub const conversions = .{
            .{ .imm, .imm },
            .{ .src, .src },
            .{ .dest, .dest },
        };
    },
    struct { pub const spec = "add r(a), x(b) -> x(dest)";
        pub const transform = "add x(b), r(a) -> x(dest)";
        pub const conversions = .{
            .{ .a, .a },
            .{ .b, .b },
            .{ .dest, .dest },
        };
    },
    struct { pub const spec = // add x(src), (imm8s) -> x(dest)
        \\add x(src), (imm) -> x(dest)
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
    struct { pub const spec = // add x(a), r(b) -> x(a)
        \\add x(a), r(b) .unsigned
        \\add x(a), r(b) .unsigned -> x(a)
        \\add x(a), r(b) .signed
        \\add x(a), r(b) .signed -> x(a)
        ;
        pub const encoding = .{
            opcode,
            Encoder.shifted(8, Reg(.a)),
            Encoder.shifted(12, Reg(.b)),
        };
        pub const ij = Reg(.a);
        pub const ik = Reg(.b);
        pub const iw = Reg(.a);

        fn opcode(params: []const isa.Parameter.Signature) opcodes.Lo8 {
            return switch (params[1].base.reg16.?) {
                .unsigned => .add_reg32_reg16u,
                .signed => .add_reg32_reg16s,
            };
        }

        pub fn entry(c: *Cycle, params: []const isa.Parameter.Signature) void {
            c.reg32_to_j();
            c.reg_to_k();
            c.j_plus_k_to_l(switch (params[1].base.reg16.?) {
                .unsigned => .zx,
                .signed => .sx,
            }, .fresh, .flags);
            c.l_to_reg32();
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = // add[c] r(src), (imm8s) -> r(dest)
        \\add r(src), (imm) -> r(dest)
        \\addc r(src), (imm) -> r(dest)
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
        \\addc r(src), (imm) -> r(dest)
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
        \\addc r(src), (imm) -> r(dest)
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
    struct { pub const spec = // ??? r(a), r(b) -> r(a)
        \\add r(a), r(b)
        \\add r(a), r(b) -> r(a)
        \\addc r(a), r(b)
        \\addc r(a), r(b) -> r(a)
        \\sub r(a), r(b)
        \\sub r(a), r(b) -> r(a)
        \\subc r(a), r(b)
        \\subc r(a), r(b) -> r(a)
        \\xor r(a), r(b)
        \\xor r(a), r(b) -> r(a)
        \\xnor r(a), r(b)
        \\xnor r(a), r(b) -> r(a)
        \\or r(a), r(b)
        \\or r(a), r(b) -> r(a)
        \\nor r(a), r(b)
        \\nor r(a), r(b) -> r(a)
        \\and r(a), r(b)
        \\and r(a), r(b) -> r(a)
        \\nand r(a), r(b)
        \\nand r(a), r(b) -> r(a)
        \\andnot r(a), r(b)
        \\andnot r(a), r(b) -> r(a)
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
                .sub => .subtract_reg16_reg16,
                .subc => .subtract_reg16_reg16_with_carry,
                .xor => .xor_reg16_reg16,
                .xnor => .xnor_reg16_reg16,
                .@"or" => .or_reg16_reg16,
                .nor => .nor_reg16_reg16,
                .@"and" => .and_reg16_reg16,
                .nand => .nand_reg16_reg16,
                .andnot => .andnot_reg16_reg16,
                else => unreachable,
            };
        }

        pub fn entry(c: *Cycle, mnemonic: isa.Mnemonic) void {
            c.reg_to_jl();
            c.neg_one_to_jh();
            c.reg_to_k();
            switch (mnemonic) {
                .add  => c.jl_plus_k_to_ll(.fresh, .flags),
                .addc => c.jl_plus_k_to_ll(.cont, .flags),
                .sub  => c.jl_minus_k_to_ll(.fresh, .flags),
                .subc => c.jl_minus_k_to_ll(.cont, .flags),
                .xor => c.jl_logic_k_to_ll(.xor, .fresh, .flags),
                .xnor => c.jl_logic_k_to_ll(.xnor, .fresh, .flags),
                .@"or" => c.jl_logic_k_to_ll(._or, .fresh, .flags),
                .nor => c.jl_logic_k_to_ll(.nor, .fresh, .flags),
                .@"and" => c.jl_logic_k_to_ll(._and, .fresh, .flags),
                .nand => c.jl_logic_k_to_ll(.nand, .fresh, .flags),
                .andnot => c.jl_logic_k_to_ll(.and_not, .fresh, .flags),
                else => unreachable,
            }
            c.ll_to_reg();
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = // cmp[c] r(a), r(b)
        \\cmp r(a), r(b)
        \\cmpc r(a), r(b)
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
                .cmpc => .compare_reg16_reg16_with_carry,
                else => unreachable,
            };
        }

        pub fn entry(c: *Cycle, mnemonic: isa.Mnemonic) void {
            c.reg_to_jl();
            c.reg_to_k();
            c.jl_minus_k(switch (mnemonic) {
                .cmp => .fresh,
                .cmpc => .cont,
                else => unreachable,
            }, .flags);
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = // cmp x(a), r(b)
        \\cmp x(a), r(b) .signed
        \\cmp x(a), r(b) .unsigned
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
    struct { pub const spec = // cmp[c] r(a), (imm16u)
        \\cmp r(a), (imm)
        \\cmpc r(a), (imm)
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
                .cmpc => .compare_reg16_i16_with_carry,
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
                .cmpc => .cont,
                else => unreachable,
            }, .flags);
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = // cmp[c] r(a), (imm16s)
        \\cmp r(a), (imm)
        \\cmpc r(a), (imm)
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
                .cmpc => .compare_reg16_i16_with_carry,
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
                .cmpc => .cont,
                else => unreachable,
            }, .flags);
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = // cmp x(a), (imm16u)
        \\cmp x(a), (imm)
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
            c.srl_to_k(.temp_1);
            c.j_minus_k(._1x, .fresh, .flags);
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = // sub x(a), r(b) -> x(a)
        \\sub x(a), r(b) .unsigned
        \\sub x(a), r(b) .unsigned -> x(a)
        \\sub x(a), r(b) .signed
        \\sub x(a), r(b) .signed -> x(a)
        ;
        pub const encoding = .{
            opcode,
            Encoder.shifted(8, Reg(.a)),
            Encoder.shifted(12, Reg(.b)),
        };
        pub const ij = Reg(.a);
        pub const ik = Reg(.b);
        pub const iw = Reg(.a);

        fn opcode(params: []const isa.Parameter.Signature) opcodes.Lo8 {
            return switch (params[1].base.reg16.?) {
                .unsigned => .subtract_reg32_reg16u,
                .signed => .subtract_reg32_reg16s,
            };
        }

        pub fn entry(c: *Cycle, params: []const isa.Parameter.Signature) void {
            c.reg32_to_j();
            c.reg_to_k();
            c.j_minus_k_to_l(switch (params[1].base.reg16.?) {
                .unsigned => .zx,
                .signed => .sx,
            }, .fresh, .flags);
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = // sub[c] (imm8s), r(src) -> r(dest)
        \\sub (imm), r(src) -> r(dest)
        \\subc (imm), r(src) -> r(dest)
        ;
        pub const encoding = .{
            opcode,
            Encoder.shifted(8, Reg(.src)),
            Encoder.shifted(12, Reg(.dest)),
            Encoder.shifted(16, Int(.imm, i8)),
        };
        pub const ik = Reg(.src);
        pub const iw = Reg(.dest);

        fn opcode(mnemonic: isa.Mnemonic) opcodes.Lo8 {
            return switch (mnemonic) {
                .sub => .subtract_s8_reg16,
                .subc => .subtract_s8_reg16_with_carry,
                else => unreachable,
            };
        }

        pub fn entry (c: *Cycle) void {
            c.ip_read_to_d(2, .byte);
            c.d_to_l(.sx);
            c.l_to_sr(.temp_1);
            c.next(subtract);
        }

        pub fn subtract(c: *Cycle, mnemonic: isa.Mnemonic) void {
            c.srl_to_jl(.temp_1);
            c.reg_to_k();
            c.jl_minus_k_to_ll(switch (mnemonic) {
                .sub => .fresh,
                .subc => .cont,
                else => unreachable,
            }, .flags);
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = // sub[c] (imm16), r(src) -> r(dest)
        \\sub (imm), r(src) -> r(dest)
        \\subc (imm), r(src) -> r(dest)
        ;
        pub const forms = .{
            struct {
                pub const encoding = .{
                    opcode,
                    Encoder.shifted(8, Reg(.src)),
                    Encoder.shifted(12, Reg(.dest)),
                    Encoder.shifted(16, Int(.imm, u16)),
                };
            },
            struct {
                pub const encoding = .{
                    opcode,
                    Encoder.shifted(8, Reg(.src)),
                    Encoder.shifted(12, Reg(.dest)),
                    Encoder.shifted(16, Int(.imm, i16)),
                };
            },
        };
        pub const ik = Reg(.src);
        pub const iw = Reg(.dest);

        fn opcode(mnemonic: isa.Mnemonic) opcodes.Lo8 {
            return switch (mnemonic) {
                .sub => .subtract_i16_reg16,
                .subc => .subtract_i16_reg16_with_carry,
                else => unreachable,
            };
        }

        pub fn entry (c: *Cycle) void {
            c.ip_read_to_d(2, .word);
            c.d_to_l(.zx);
            c.l_to_sr(.temp_1);
            c.next(subtract);
        }

        pub fn subtract(c: *Cycle, mnemonic: isa.Mnemonic) void {
            c.srl_to_jl(.temp_1);
            c.reg_to_k();
            c.jl_minus_k_to_ll(switch (mnemonic) {
                .sub => .fresh,
                .subc => .cont,
                else => unreachable,
            }, .flags);
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = // inc[c] r(reg) -> r(reg)
        \\inc r(reg)
        \\inc r(reg) -> r(reg)
        \\add r(reg), 1
        \\add r(reg), 1 -> r(reg)
        \\sub r(reg), -1
        \\sub r(reg), -1 -> r(reg)
        \\incc r(reg)
        \\incc r(reg) -> r(reg)
        \\addc r(reg), 0
        \\addc r(reg), 0 -> r(reg)
        ;
        pub const encoding = .{
            opcode,
            Encoder.shifted(12, Reg(.reg)),
        };
        pub const ij = Reg(.reg);
        pub const iw = Reg(.reg);

        fn opcode(mnemonic: isa.Mnemonic) opcodes.Lo12 {
            return switch (mnemonic) {
                .inc, .add, .sub => .increment_reg16,
                .incc, .addc => .increment_reg16_if_carry,
                else => unreachable,
            };
        }

        pub fn entry(c: *Cycle, mnemonic: isa.Mnemonic) void {
            c.reg_to_jl();
            switch (mnemonic) {
                .inc, .add, .sub => {
                    c.srl_to_k(.one);
                    c.jl_plus_k_to_ll(.fresh, .flags);
                },
                .incc, .addc => {
                    c.zero_to_k();
                    c.jl_plus_k_to_ll(.cont, .flags);
                },
                else => unreachable,
            }
            c.ll_to_reg();
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = // dec[c] r(reg) -> r(reg)
        \\dec r(reg)
        \\dec r(reg) -> r(reg)
        \\add r(reg), -1
        \\add r(reg), -1 -> r(reg)
        \\sub r(reg), 1
        \\sub r(reg), 1 -> r(reg)
        \\decc r(reg)
        \\decc r(reg) -> r(reg)
        \\subc r(reg), 0
        \\subc r(reg), 0 -> r(reg)
        ;
        pub const encoding = .{
            opcode,
            Encoder.shifted(12, Reg(.reg)),
        };
        pub const ij = Reg(.reg);
        pub const iw = Reg(.reg);

        fn opcode(mnemonic: isa.Mnemonic) opcodes.Lo12 {
            return switch (mnemonic) {
                .dec, .add, .sub => .decrement_reg16,
                .decc, .subc => .decrement_reg16_if_not_carry,
                else => unreachable,
            };
        }

        pub fn entry(c: *Cycle, mnemonic: isa.Mnemonic) void {
            c.reg_to_jl();
            switch (mnemonic) {
                .dec, .add, .sub => {
                    c.srl_to_k(.one);
                    c.jl_minus_k_to_ll(.fresh, .flags);
                },
                .decc, .subc => {
                    c.zero_to_k();
                    c.jl_minus_k_to_ll(.cont, .flags);
                },
                else => unreachable,
            }
            c.ll_to_reg();
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = // inc[c] x(reg) -> x(reg)
        \\inc x(reg)
        \\inc x(reg) -> x(reg)
        \\add x(reg), 1
        \\add x(reg), 1 -> x(reg)
        \\sub x(reg), -1
        \\sub x(reg), -1 -> x(reg)
        \\incc x(reg)
        \\incc x(reg) -> x(reg)
        \\addc x(reg), 0
        \\addc x(reg), 0 -> x(reg)
        ;
        pub const encoding = .{
            opcode,
            Encoder.shifted(12, Reg(.reg)),
        };
        pub const ij = Reg(.reg);
        pub const iw = Reg(.reg);

        fn opcode(mnemonic: isa.Mnemonic) opcodes.Lo12 {
            return switch (mnemonic) {
                .inc, .add, .sub => .increment_reg32,
                .incc, .addc => .increment_reg32_if_carry,
                else => unreachable,
            };
        }

        pub fn entry(c: *Cycle, mnemonic: isa.Mnemonic) void {
            c.reg32_to_j();
            switch (mnemonic) {
                .inc, .add, .sub => {
                    c.srl_to_k(.one);
                    c.j_plus_k_to_l(.zx, .fresh, .flags);
                },
                .incc, .addc => {
                    c.zero_to_k();
                    c.j_plus_k_to_l(.zx, .cont, .flags);
                },
                else => unreachable,
            }
            c.l_to_reg32();
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = // dec[c] x(reg) -> x(reg)
        \\dec x(reg)
        \\dec x(reg) -> x(reg)
        \\add x(reg), -1
        \\add x(reg), -1 -> x(reg)
        \\sub x(reg), 1
        \\sub x(reg), 1 -> x(reg)
        \\decc x(reg)
        \\decc x(reg) -> x(reg)
        \\subc x(reg), 0
        \\subc x(reg), 0 -> x(reg)
        ;
        pub const encoding = .{
            opcode,
            Encoder.shifted(12, Reg(.reg)),
        };
        pub const ij = Reg(.reg);
        pub const iw = Reg(.reg);

        fn opcode(mnemonic: isa.Mnemonic) opcodes.Lo12 {
            return switch (mnemonic) {
                .dec, .add, .sub => .decrement_reg32,
                .decc, .subc => .decrement_reg32_if_not_carry,
                else => unreachable,
            };
        }

        pub fn entry(c: *Cycle, mnemonic: isa.Mnemonic) void {
            c.reg32_to_j();
            switch (mnemonic) {
                .dec, .add, .sub => {
                    c.srl_to_k(.one);
                    c.j_minus_k_to_l(.zx, .fresh, .flags);
                },
                .decc, .subc => {
                    c.zero_to_k();
                    c.j_minus_k_to_l(.zx, .cont, .flags);
                },
                else => unreachable,
            }
            c.l_to_reg32();
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = // neg[c] r(reg) -> r(reg)
        \\neg r(reg)
        \\neg r(reg) -> r(reg)
        \\sub 0, r(reg) -> r(reg)
        \\negc r(reg)
        \\negc r(reg) -> r(reg)
        \\subc 0, r(reg) -> r(reg)
        ;
        pub const encoding = .{
            opcode,
            Encoder.shifted(12, Reg(.reg)),
        };
        pub const ik = Reg(.reg);
        pub const iw = Reg(.reg);

        fn opcode(mnemonic: isa.Mnemonic) opcodes.Lo12 {
            return switch (mnemonic) {
                .neg, .sub => .negate_reg16,
                .negc, .subc => .negate_reg16_with_carry,
                else => unreachable,
            };
        }

        pub fn entry(c: *Cycle, mnemonic: isa.Mnemonic) void {
            c.zero_to_jl();
            c.reg_to_k();
            switch (mnemonic) {
                .neg, .sub => {
                    c.jl_minus_k_to_ll(.fresh, .flags);
                },
                .negc, .subc => {
                    c.jl_minus_k_to_ll(.cont, .flags);
                },
                else => unreachable,
            }
            c.ll_to_reg();
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = // not r(reg) -> r(reg)
        \\not r(reg)
        \\not r(reg) -> r(reg)
        ;
        pub const encoding = .{
            opcodes.Lo12.complement_reg16,
            Encoder.shifted(12, Reg(.reg)),
        };
        pub const ik = Reg(.reg);
        pub const iw = Reg(.reg);

        pub fn entry(c: *Cycle) void {
            c.zero_to_jl();
            c.reg_to_k();
            c.jl_logic_k_to_ll(.xnor, .fresh, .flags);
            c.ll_to_reg();
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = // test[z] r(a), r(b)
        \\test r(a), r(b)
        \\testz r(a), r(b)
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
                .@"test" => .test_reg16,
                .testz => .test_reg16_no_set_z,
                else => unreachable,
            };
        }

        pub fn entry(c: *Cycle, mnemonic: isa.Mnemonic) void {
            c.reg_to_jl();
            c.reg_to_k();
            c.jl_logic_k_to_ll(._and, switch (mnemonic) {
                .@"test" => .fresh,
                .testz => .cont,
                else => unreachable,
            }, .flags);
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = // testb[z] r(a), (imm4u)
        \\testb r(reg), (imm)
        \\testbz r(reg), (imm)
        ;
        pub const encoding = .{
            opcode,
            Encoder.shifted(8, Reg(.reg)),
            Encoder.shifted(12, Imm),
        };
        const Imm = Int(.imm, u4);
        pub const ij = Reg(.reg);
        pub const ik = Imm;

        fn opcode(mnemonic: isa.Mnemonic) opcodes.Lo8 {
            return switch (mnemonic) {
                .testb => .test_bit_reg16,
                .testbz => .test_bit_reg16_no_set_z,
                else => unreachable,
            };
        }

        pub fn entry(c: *Cycle, mnemonic: isa.Mnemonic) void {
            c.reg_to_jl();
            c.ik_bit_to_k();
            c.jl_logic_k_to_ll(._and, switch (mnemonic) {
                .testb => .fresh,
                .testbz => .cont,
                else => unreachable,
            }, .flags);
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = // <clr/set/tgl>b r(reg), (imm4u) -> r(reg)
        \\clrb r(reg), (imm)
        \\clrb r(reg), (imm) -> r(reg)
        \\setb r(reg), (imm)
        \\setb r(reg), (imm) -> r(reg)
        \\tglb r(reg), (imm)
        \\tglb r(reg), (imm) -> r(reg)
        ;
        pub const encoding = .{
            opcode,
            Encoder.shifted(8, Reg(.reg)),
            Encoder.shifted(12, Imm),
        };
        const Imm = Int(.imm, u4);
        pub const ij = Reg(.reg);
        pub const ik = Imm;
        pub const iw = Reg(.reg);

        fn opcode(mnemonic: isa.Mnemonic) opcodes.Lo8 {
            return switch (mnemonic) {
                .clrb => .clear_bit_reg16_u4,
                .setb => .set_bit_reg16_u4,
                .tglb => .toggle_bit_reg16_u4,
                else => unreachable,
            };
        }

        pub fn entry(c: *Cycle, mnemonic: isa.Mnemonic) void {
            c.reg_to_jl();
            c.ik_bit_to_k();
            c.jl_logic_k_to_ll(switch (mnemonic) {
                .clrb => .and_not,
                .setb => ._or,
                .tglb => .xor,
                else => unreachable,
            }, .fresh, .flags);
            c.ll_to_reg();
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = // <shr/shl> <reg>, b(bits) -> <reg>
        \\shr x(reg) .unsigned, b(bits)
        \\shr x(reg) .unsigned, b(bits) -> x(reg)
        \\shl x(reg), r(bits)
        \\shl x(reg), r(bits) -> x(reg)
        \\shr r(reg) .unsigned, b(bits)
        \\shr r(reg) .unsigned, b(bits) -> r(reg)
        \\shr r(reg) .signed, b(bits)
        \\shr r(reg) .signed, b(bits) -> r(reg)
        \\shl r(reg), b(bits)
        \\shl r(reg), b(bits) -> r(reg)
        ;
        pub const encoding = .{
            opcode,
            Encoder.shifted(8, Reg(.reg)),
            Encoder.shifted(12, Reg(.bits)),
        };
        pub const ij = Reg(.reg);
        pub const ik = Reg(.bits);
        pub const iw = Reg(.reg);

        fn opcode(mnemonic: isa.Mnemonic, params: []const Parameter.Signature) opcodes.Lo8 {
            return switch (mnemonic) {
                .shr => switch (params[0].base) {
                    .reg16 => |signedness| switch (signedness.?) {
                        .unsigned => .shift_right_reg16u_reg4,
                        .signed => .shift_right_reg16s_reg4,
                    },
                    .reg32 => .shift_right_reg32_reg5,
                    else => unreachable,
                },
                .shl => switch (params[0].base) {
                    .reg16 => .shift_left_reg16_reg4,
                    .reg32 => .shift_left_reg32_reg5,
                    else => unreachable,
                },
                else => unreachable,
            };
        }

        pub fn entry(c: *Cycle, mnemonic: isa.Mnemonic, params: []const Parameter.Signature) void {
            const dir: Cycle.Shift_Direction = switch (mnemonic) {
                .shr => .right,
                .shl => .left,
                else => unreachable,
            };

            c.reg_to_k();
            if (params[0].base == .reg32) {
                c.reg32_to_j();
                c.j_shift_k5_to_l(dir, .fresh, .flags);
                c.l_to_reg32();
            } else {
                c.reg_to_j(if (params[0].base.reg16) |signedness| switch (signedness) {
                    .unsigned => .zx,
                    .signed => .sx,
                } else .zx);
                c.jl_shift_k4_to_ll(dir, .fresh, .flags);
                c.ll_to_reg();
            }
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = // <shr/shl> <reg>, (imm) -> <reg>
        \\shr r(reg) .unsigned, (imm)
        \\shr r(reg) .unsigned, (imm) -> r(reg)
        \\shr r(reg) .signed, (imm)
        \\shr r(reg) .signed, (imm) -> r(reg)
        \\shl r(reg), (imm)
        \\shl r(reg), (imm) -> r(reg)
        \\shr x(reg) .unsigned, (imm)
        \\shr x(reg) .unsigned, (imm) -> x(reg)
        \\shl x(reg), (imm)
        \\shl x(reg), (imm) -> x(reg)
        ;
        pub const encoding = .{
            opcode,
            Encoder.shifted(8, Reg(.reg)),
            Encoder.shifted(12, Imm),
        };
        const Imm = Int(.imm, u4);
        pub const ij = Reg(.reg);
        pub const iw = Reg(.reg);

        fn opcode(mnemonic: isa.Mnemonic, params: []const Parameter.Signature) opcodes.Lo8 {
            return switch (mnemonic) {
                .shr => switch (params[0].base) {
                    .reg16 => |signedness| switch (signedness.?) {
                        .unsigned => .shift_right_reg16u_u4,
                        .signed => .shift_right_reg16s_u4,
                    },
                    .reg32 => .shift_right_reg32_u4,
                    else => unreachable,
                },
                .shl => switch (params[0].base) {
                    .reg16 => .shift_left_reg16_u4,
                    .reg32 => .shift_left_reg32_u4,
                    else => unreachable,
                },
                else => unreachable,
            };
        }

        pub fn entry(c: *Cycle, mnemonic: isa.Mnemonic, params: []const Parameter.Signature, imm: Imm) void {
            const dir: Cycle.Shift_Direction = switch (mnemonic) {
                .shr => .right,
                .shl => .left,
                else => unreachable,
            };

            c.literal_to_k(@intCast(imm.value));
            if (params[0].base == .reg32) {
                c.reg32_to_j();
                c.j_shift_k5_to_l(dir, .fresh, .flags);
                c.l_to_reg32();
            } else {
                c.reg_to_j(if (params[0].base.reg16) |signedness| switch (signedness) {
                    .unsigned => .zx,
                    .signed => .sx,
                } else .zx);
                c.jl_shift_k4_to_ll(dir, .fresh, .flags);
                c.ll_to_reg();
            }
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = // <shr/shl> <reg>, (imm) -> <reg>
        \\shr x(reg) .unsigned, (imm)
        \\shr x(reg) .unsigned, (imm) -> x(reg)
        \\shl x(reg), (imm)
        \\shl x(reg), (imm) -> x(reg)
        ;
        pub const encoding = .{
            opcode,
            Encoder.shifted(8, Reg(.reg)),
            Encoder.shifted(12, Imm),
        };
        const Imm = Range(.imm, 16, 31);
        pub const ij = Reg(.reg);
        pub const iw = Reg(.reg);

        fn opcode(mnemonic: isa.Mnemonic) opcodes.Lo8 {
            return switch (mnemonic) {
                .shr => .shift_right_reg32_u4_16_31,
                .shl => .shift_left_reg32_u4_16_31,
                else => unreachable,
            };
        }

        pub fn entry(c: *Cycle, mnemonic: isa.Mnemonic, imm: Imm) void {
            const dir: Cycle.Shift_Direction = switch (mnemonic) {
                .shr => .right,
                .shl => .left,
                else => unreachable,
            };

            c.literal_to_k(@intCast(imm.value));
            c.reg32_to_j();
            c.j_shift_k5_to_l(dir, .fresh, .flags);
            c.l_to_reg32();
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = // <shrc/shlc> <reg> -> <reg>
        \\shrc r(reg)
        \\shrc r(reg) -> r(reg)
        \\shlc r(reg)
        \\shlc r(reg) -> r(reg)
        ;
        pub const encoding = .{
            opcode,
            Encoder.shifted(12, Reg(.reg)),
        };
        pub const ij = Reg(.reg);
        pub const iw = Reg(.reg);

        fn opcode(mnemonic: isa.Mnemonic) opcodes.Lo12 {
            return switch (mnemonic) {
                .shrc => .shift_right_reg16_c,
                .shlc => .shift_left_reg16_c,
                else => unreachable,
            };
        }

        pub fn entry(c: *Cycle, mnemonic: isa.Mnemonic, flags: Flags) void {
            const dir: Cycle.Shift_Direction = switch (mnemonic) {
                .shrc => .right,
                .shlc => .left,
                else => unreachable,
            };

            c.reg_to_jl();
            if (flags.carry()) c.neg_one_to_jh() else c.zero_to_jh();
            c.srl_to_k(.one);
            c.jl_shift_k4_to_ll(dir, .fresh, .flags);
            c.ll_to_reg();
            c.load_and_exec_next_insn();
        }
    },

    struct { pub const spec = // <count> r(reg) -> r(reg)
        \\cb r(reg)
        \\cb r(reg) -> r(reg)
        \\cz r(reg)
        \\cz r(reg) -> r(reg)
        \\clb r(reg)
        \\clb r(reg) -> r(reg)
        \\clz r(reg)
        \\clz r(reg) -> r(reg)
        \\ctb r(reg)
        \\ctb r(reg) -> r(reg)
        \\ctz r(reg)
        \\ctz r(reg) -> r(reg)
        ;
        pub const encoding = .{
            opcode,
            Encoder.shifted(12, Reg(.reg)),
        };
        pub const ij = Reg(.reg);
        pub const iw = Reg(.reg);

        fn opcode(mnemonic: isa.Mnemonic) opcodes.Lo12 {
            return switch (mnemonic) {
                .cb => .count_bits,
                .cz => .count_zeroes,
                .clb => .count_bits_leading,
                .clz => .count_zeroes_leading,
                .ctb => .count_bits_trailing,
                .ctz => .count_zeroes_trailing,
                else => unreachable,
            };
        }

        pub fn entry(c: *Cycle, mnemonic: isa.Mnemonic) void {
            c.reg_to_jl();
            c.literal_to_k(-1);
            c.count_jl_and_k_to_ll(switch (mnemonic) {
                .cb, .clb, .ctb => .ones,
                .cz, .clz, .ctz => .zeroes,
                else => unreachable,
            }, switch (mnemonic) {
                .cb, .cz => .all,
                .ctb, .ctz => .trailing,
                .clb, .clz => .leading,
                else => unreachable,
            }, .fresh, .flags);
            c.ll_to_reg();
            c.load_and_exec_next_insn();
        }
    },

    struct { pub const spec = "mul (imm), r(src) -> r(dest)";
        pub const transform = "mul r(src), (imm) -> r(dest)";
        pub const conversions = .{
            .{ .imm, .imm },
            .{ .src, .src },
            .{ .dest, .dest },
        };
    },
    struct { pub const spec = "mul r(src), (imm) -> r(dest)";
        pub const encoding = .{
            opcodes.Lo8.multiply_low_reg16_i16,
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
            c.next(multiply);
        }

        pub fn multiply(c: *Cycle) void {
            c.reg_to_jl();
            c.srl_to_k(.temp_1);
            c.jl_times_k_to_ll(.zx, .zx, .fresh, .flags);
            c.ll_to_reg();
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = "mul r(src), (imm) -> r(dest)";
        pub const encoding = .{
            opcodes.Lo8.multiply_low_reg16_i16,
            Encoder.shifted(8, Reg(.src)),
            Encoder.shifted(12, Reg(.dest)),
            Encoder.shifted(16, Int(.imm, i16)),
        };
        pub const ij = Reg(.src);
        pub const iw = Reg(.dest);

        pub fn entry(c: *Cycle) void {
            c.ip_read_to_d(2, .word);
            c.d_to_l(.zx);
            c.l_to_sr(.temp_1);
            c.next(multiply);
        }

        pub fn multiply(c: *Cycle) void {
            c.reg_to_jl();
            c.srl_to_k(.temp_1);
            c.jl_times_k_to_ll(.zx, .zx, .fresh, .flags);
            c.ll_to_reg();
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = // mul r(src), (imm) -> x(dest)
        \\mul r(src) .unsigned, (imm) -> x(dest) .unsigned
        \\mul r(src) .signed, (imm) -> x(dest) .signed
        ;
        pub const encoding = .{
            opcode,
            Encoder.shifted(8, Reg(.src)),
            Encoder.shifted(12, Reg(.dest)),
            Encoder.shifted(16, Int(.imm, u16)),
        };
        pub const ij = Reg(.src);
        pub const iw = Reg(.dest);

        fn opcode(params: []const Parameter.Signature) opcodes.Lo8 {
            return switch (params[0].base.reg16.?) {
                .unsigned => .multiply_full_reg16u_u16,
                .signed => .multiply_full_reg16s_u16,
            };
        }

        pub fn entry(c: *Cycle) void {
            c.ip_read_to_d(2, .word);
            c.d_to_l(.zx);
            c.l_to_sr(.temp_1);
            c.next(multiply);
        }

        pub fn multiply(c: *Cycle, params: []const Parameter.Signature) void {
            c.reg_to_jl();
            c.srl_to_k(.temp_1);
            c.jl_times_k_to_l(switch (params[0].base.reg16.?) {
                .unsigned => .zx,
                .signed => .sx,
            }, .zx, .fresh, .flags);
            c.l_to_reg32();
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = // mul r(src), (imm) -> x(dest)
        \\mul r(src) .unsigned, (imm) -> x(dest) .signed
        \\mul r(src) .signed, (imm) -> x(dest) .signed
        ;
        pub const encoding = .{
            opcode,
            Encoder.shifted(8, Reg(.src)),
            Encoder.shifted(12, Reg(.dest)),
            Encoder.shifted(16, Int(.imm, i16)),
        };
        pub const ij = Reg(.src);
        pub const iw = Reg(.dest);

        fn opcode(params: []const Parameter.Signature) opcodes.Lo8 {
            return switch (params[0].base.reg16.?) {
                .unsigned => .multiply_full_reg16u_s16,
                .signed => .multiply_full_reg16s_s16,
            };
        }

        pub fn entry(c: *Cycle) void {
            c.ip_read_to_d(2, .word);
            c.d_to_l(.zx);
            c.l_to_sr(.temp_1);
            c.next(multiply);
        }

        pub fn multiply(c: *Cycle, params: []const Parameter.Signature) void {
            c.reg_to_jl();
            c.srl_to_k(.temp_1);
            c.jl_times_k_to_l(switch (params[0].base.reg16.?) {
                .unsigned => .zx,
                .signed => .sx,
            }, .sx, .fresh, .flags);
            c.l_to_reg32();
            c.load_and_exec_next_insn();
        }
    },

    struct { pub const spec = "mul r(a), r(b) -> r(a)";
        pub const encoding = .{
            opcodes.Lo8.multiply_low_reg16_reg16,
            Encoder.shifted(8, Reg(.a)),
            Encoder.shifted(12, Reg(.b)),
        };
        pub const ij = Reg(.a);
        pub const ik = Reg(.b);
        pub const iw = Reg(.a);

        pub fn entry(c: *Cycle) void {
            c.reg_to_jl();
            c.reg_to_k();
            c.jl_times_k_to_ll(.zx, .sx, .fresh, .flags);
            c.ll_to_reg();
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = // mulh r(a), r(b) -> r(a)
        \\mulh r(a) .unsigned, r(b) .unsigned -> r(a) .unsigned
        \\mulh r(a) .unsigned, r(b) .signed -> r(a) .signed
        \\mulh r(a) .signed, r(b) .unsigned -> r(a) .signed
        \\mulh r(a) .signed, r(b) .signed -> r(a) .signed
        ;
        pub const encoding = .{
            opcode,
            Encoder.shifted(8, Reg(.a)),
            Encoder.shifted(12, Reg(.b)),
        };
        pub const ij = Reg(.a);
        pub const ik = Reg(.b);
        pub const iw = Reg(.a);

        fn opcode(params: []const Parameter.Signature) opcodes.Lo8 {
            return switch (params[0].base.reg16.?) {
                .unsigned => switch (params[1].base.reg16.?) {
                    .unsigned => .multiply_high_reg16u_reg16u,
                    .signed => .multiply_high_reg16u_reg16s,
                },
                .signed => switch (params[1].base.reg16.?) {
                    .unsigned => .multiply_high_reg16s_reg16u,
                    .signed => .multiply_high_reg16s_reg16s,
                },
            };
        }

        pub fn entry(c: *Cycle, params: []const Parameter.Signature) void {
            c.reg_to_jl();
            c.reg_to_k();
            c.jl_times_k__shr_16_to_ll(switch (params[0].base.reg16.?) {
                .unsigned => .zx,
                .signed => .sx,
            }, switch (params[1].base.reg16.?) {
                .unsigned => .zx,
                .signed => .sx,
            }, .fresh, .flags);
            c.ll_to_reg();
            c.load_and_exec_next_insn();
        }
    },

    struct { pub const spec = // mul r(a), r(b) -> x0
        \\mul r(a) .unsigned, r(b) .unsigned -> x0 .unsigned
        \\mul r(a) .unsigned, r(b) .signed -> x0 .signed
        \\mul r(b) .signed, r(a) .unsigned -> x0 .signed
        \\mul r(a) .signed, r(b) .signed -> x0 .signed
        ;
        pub const encoding = .{
            opcode,
            Encoder.shifted(8, Reg(.a)),
            Encoder.shifted(12, Reg(.b)),
        };
        pub const ij = Reg(.a);
        pub const ik = Reg(.b);
        pub const iw = Reg(.a);

        fn opcode(params: []const Parameter.Signature) opcodes.Lo8 {
            return switch (params[0].base.reg16.?) {
                .unsigned => switch (params[1].base.reg16.?) {
                    .unsigned => .multiply_full_reg16u_reg16u_x0,
                    .signed => .multiply_full_reg16u_reg16s_x0,
                },
                .signed => switch (params[1].base.reg16.?) {
                    .unsigned => .multiply_full_reg16u_reg16s_x0,
                    .signed => .multiply_full_reg16s_reg16s_x0,
                },
            };
        }

        pub fn entry(c: *Cycle, params: []const Parameter.Signature) void {
            c.reg_to_jl();
            c.reg_to_k();
            if (params[0].base.reg16.? != params[1].base.reg16.?) {
                c.jl_times_k__shr_16_to_ll(.zx, .sx, .fresh, .flags);
            } else switch (params[3].base.reg32.?) {
                .unsigned => c.jl_times_k__shr_16_to_ll(.zx, .zx, .fresh, .flags),
                .signed => c.jl_times_k__shr_16_to_ll(.sx, .sx, .fresh, .flags),
            }
            c.ll_to_reg();
            c.load_and_exec_next_insn();
        }
    },
};

const Cycle = @import("Cycle.zig");
const Encoder = isa.Instruction_Encoding.Encoder;
const Parameter = isa.Parameter;
const Int = placeholders.Int;
const Range = placeholders.Range;
const Reg = placeholders.Reg;
const Flags = arch.hw.microcode.Flags;
const placeholders = @import("placeholders.zig");
const opcodes = @import("opcodes.zig");
const isa = arch.isa;
const arch = @import("lib_arch");
