const region_encoder = Encoder.init(14, @as(u2, 2));

pub const instructions = .{
    struct { // <op> r(a), r(b) -> r(a)
        pub const spec =
            \\cmp   r(a), r(b)
            \\cmpc  r(a), r(b)
            \\and   r(a), r(b)
            \\and   r(a), r(b) -> r(a)
            \\nand  r(a), r(b)
            \\nand  r(a), r(b) -> r(a)
            \\or    r(a), r(b)
            \\or    r(a), r(b) -> r(a)
            \\nor   r(a), r(b)
            \\nor   r(a), r(b) -> r(a)
            \\xor   r(a), r(b)
            \\xor   r(a), r(b) -> r(a)
            \\xnor  r(a), r(b)
            \\xnor  r(a), r(b) -> r(a)
            ;
        pub const encoding = .{
            Reg(.a),
            Encoder.init(4, Even_Reg(.b)),
            mnemonic_encoder,
            Encoder.init(10, @as(u4, 4)),
            region_encoder,
        };
        pub const ij = Reg(.a);
        pub const ik = Reg(.b);
        pub const iw = Reg(.a);

        fn mnemonic_encoder(mnemonic: isa.Mnemonic) Encoder {
            return Encoder.init(7, @as(u3, switch (mnemonic) {
                .cmp => 0,
                .cmpc => 1,
                .@"and" => 2,
                .nand => 3,
                .@"or" => 4,
                .nor => 5,
                .xor => 6,
                .xnor => 7,
                else => unreachable,
            }));
        }

        pub fn entry(c: *Cycle, mnemonic: isa.Mnemonic) void {
            c.reg_to_jl();
            c.reg_to_k();
            switch (mnemonic) {
                .cmp => c.jl_minus_k(.fresh, .flags),
                .cmpc => c.jl_minus_k(.cont, .flags),
                .@"and" => c.jl_logic_k_to_ll(._and, .fresh, .flags),
                .nand => c.jl_logic_k_to_ll(.nand, .fresh, .flags),
                .@"or" => c.jl_logic_k_to_ll(._or, .fresh, .flags),
                .nor => c.jl_logic_k_to_ll(.nor, .fresh, .flags),
                .xor => c.jl_logic_k_to_ll(.xor, .fresh, .flags),
                .xnor => c.jl_logic_k_to_ll(.xnor, .fresh, .flags),
                else => unreachable,
            }
            c.ll_to_reg();
            c.load_and_exec_next_insn();
        }
    },
    struct { // <op> r(reg), (imm16) -> r(reg)
        pub const spec =
            \\cmp   r(reg), (imm)
            \\cmpc  r(reg), (imm)
            \\and   r(reg), (imm)
            \\and   r(reg), (imm) -> r(reg)
            \\and   (imm), r(reg) -> r(reg)
            \\nand  r(reg), (imm)
            \\nand  r(reg), (imm) -> r(reg)
            \\nand  (imm), r(reg) -> r(reg)
            \\or    r(reg), (imm)
            \\or    r(reg), (imm) -> r(reg)
            \\or    (imm), r(reg) -> r(reg)
            \\nor   r(reg), (imm)
            \\nor   r(reg), (imm) -> r(reg)
            \\nor   (imm), r(reg) -> r(reg)
            \\xor   r(reg), (imm)
            \\xor   r(reg), (imm) -> r(reg)
            \\xor   (imm), r(reg) -> r(reg)
            \\xnor  r(reg), (imm)
            \\xnor  r(reg), (imm) -> r(reg)
            \\xnor  (imm), r(reg) -> r(reg)
        ;
        pub const forms = .{
            struct {
                pub const encoding = .{
                    Reg(.reg),
                    mnemonic_encoder,
                    Encoder.init(7, @as(u7, 0x1f)),
                    region_encoder,
                    Encoder.init(16, Int(.imm, u16)),
                };
            },
            struct {
                pub const encoding = .{
                    Reg(.reg),
                    mnemonic_encoder,
                    Encoder.init(7, @as(u7, 0x1f)),
                    region_encoder,
                    Encoder.init(16, Int(.imm, i16)),
                };
            },
        };
        pub const ij = Reg(.reg);
        pub const iw = Reg(.reg);

        fn mnemonic_encoder(mnemonic: isa.Mnemonic) Encoder {
            return Encoder.init(4, @as(u3, switch (mnemonic) {
                .cmp => 0,
                .cmpc => 1,
                .@"and" => 2,
                .nand => 3,
                .@"or" => 4,
                .nor => 5,
                .xor => 6,
                .xnor => 7,
                else => unreachable,
            }));
        }

        pub fn entry(c: *Cycle) void {
            c.ip_read_to_d(2, .word);
            c.d_to_l(.zx);
            c.l_to_sr(.temp_1);
            c.next(compute);
        }

        pub fn compute(c: *Cycle, mnemonic: isa.Mnemonic) void {
            c.reg_to_jl();
            c.srl_to_k(.temp_1);
            switch (mnemonic) {
                .cmp => c.jl_minus_k(.fresh, .flags),
                .cmpc => c.jl_minus_k(.cont, .flags),
                .@"and" => c.jl_logic_k_to_ll(._and, .fresh, .flags),
                .nand => c.jl_logic_k_to_ll(.nand, .fresh, .flags),
                .@"or" => c.jl_logic_k_to_ll(._or, .fresh, .flags),
                .nor => c.jl_logic_k_to_ll(.nor, .fresh, .flags),
                .xor => c.jl_logic_k_to_ll(.xor, .fresh, .flags),
                .xnor => c.jl_logic_k_to_ll(.xnor, .fresh, .flags),
                else => unreachable,
            }
            c.ll_to_reg();
            c.load_and_exec_next_insn();
        }
    },
    struct { // <tb/cb/sb> r(reg), (imm)
        pub const spec = 
            \\bit r(reg), (imm)
            \\setbit r(reg), (imm)
            \\clrbit r(reg), (imm)
            ;
        pub const encoding = .{
            Bit(.imm),
            Encoder.init(4, Even_Reg(.reg)),
            mnemonic_encoder,
            Encoder.init(9, @as(u5, 7)),
            region_encoder,
        };
        pub const ij = Reg(.reg);
        pub const ik = Bit(.imm);
        pub const iw = Reg(.reg);

        fn mnemonic_encoder(mnemonic: isa.Mnemonic) Encoder {
            return Encoder.init(7, @as(u2, switch (mnemonic) {
                .cb => 0,
                .sb => 1,
                .tb => 2,
                else => unreachable,
            }));
        }

        pub fn entry(c: *Cycle, mnemonic: isa.Mnemonic) void {
            c.reg_to_jl();
            c.ik_bit_to_k();
            switch (mnemonic) {
                .tb => c.jl_logic_k(._and, .fresh, .flags),
                .sb => {
                    c.jl_logic_k_to_ll(._or, .fresh, .flags);
                    c.ll_to_reg();
                },
                .cb => {
                    c.jl_logic_k_to_ll(.and_not, .fresh, .flags);
                    c.ll_to_reg();
                },
                else => unreachable,
            }
            c.load_and_exec_next_insn();
        }
    },
    struct { // <op> r(src) -> r(dest)
        pub const spec =
            \\neg r(src, dest)
            \\neg r(src) -> r(dest)
            \\sub 0, r(src) -> r(dest)
            \\negc r(src, dest)
            \\negc r(src) -> r(dest)
            \\subc 0, r(src) -> r(dest)
            \\csb r(src, dest)
            \\csb r(src) -> b(dest)
            \\czb r(src, dest)
            \\czb r(src) -> b(dest)
            \\csbl r(src, dest)
            \\csbl r(src) -> b(dest)
            \\czbl r(src, dest)
            \\czbl r(src) -> b(dest)
            \\csbt r(src, dest)
            \\csbt r(src) -> b(dest)
            \\czbt r(src, dest)
            \\czbt r(src) -> b(dest)
            ;
        pub const encoding = .{
            Even_Reg(.dest),
            Encoder.init(3, Even_Reg(.src)),
            mnemonic_encoder,
            Encoder.init(9, @as(u5, 5)),
            region_encoder,
        };
        pub const ij = Reg(.src);
        pub const ik = Reg(.src);
        pub const iw = Reg(.dest);

        fn mnemonic_encoder(mnemonic: isa.Mnemonic) Encoder {
            return Encoder.init(6, @as(u3, switch (mnemonic) {
                .neg, .sub => 0,
                .negc, .subc => 1,
                .czb => 2,
                .csb => 3,
                .czbl => 4,
                .csbl => 5,
                .czbt => 6,
                .csbt => 7,
                else => unreachable,
            }));
        }

        pub fn entry(c: *Cycle, mnemonic: isa.Mnemonic) void {
            switch (mnemonic) {
                .neg, .sub => {
                    c.zero_to_jl();
                    c.reg_to_k();
                    c.jl_minus_k_to_ll(.fresh, .flags);
                },
                .negc, .subc => {
                    c.zero_to_jl();
                    c.reg_to_k();
                    c.jl_minus_k_to_ll(.cont, .flags);
                },
                else => {
                    c.reg_to_jl();
                    c.literal_to_k(-1);
                    switch (mnemonic) {
                        .czb => c.count_jl_and_k_to_ll(.zeroes, .all, .fresh, .flags),
                        .csb => c.count_jl_and_k_to_ll(.ones, .all, .fresh, .flags),
                        .czbl => c.count_jl_and_k_to_ll(.zeroes, .leading, .fresh, .flags),
                        .csbl => c.count_jl_and_k_to_ll(.ones, .leading, .fresh, .flags),
                        .czbt => c.count_jl_and_k_to_ll(.zeroes, .trailing, .fresh, .flags),
                        .csbt => c.count_jl_and_k_to_ll(.ones, .trailing, .fresh, .flags),
                        else => unreachable,
                    }
                },
            }
            c.ll_to_reg();
            c.load_and_exec_next_insn();
        }
    },
    struct { // push r(a), r(b)
        pub const spec =
            \\push r(a), r(b)
            ;
        pub const encoding = .{
            Reg(.a),
            Encoder.init(4, Reg(.b)),
            Encoder.init(8, @as(u6, 0x0)),
            region_encoder,
        };
        pub const ij = Reg(.a);
        pub const ik = Reg(.b);

        pub fn entry(c: *Cycle) void {
            c.reg_to_jl();
            c.jl_to_ll();
            c.write_from_ll(.sp, -2, .word, .stack);
            c.next(write_b);
        }

        pub fn write_b(c: *Cycle) void {
            c.reg_to_k();
            c.k_to_ll();
            c.write_from_ll(.sp, -4, .word, .stack);
            c.next(modify_sp);
        }

        pub fn modify_sp(c: *Cycle) void {
            c.sr_to_j(.sp);
            c.literal_to_k(4);
            c.j_minus_k_to_l(.zx, .fresh, .no_flags);
            c.l_to_sr(.sp);
            c.load_and_exec_next_insn();
        }
    },
    struct { // pop r(b), r(a)
        pub const spec =
            \\pop r(b), r(a)
            ;
        pub const encoding = .{
            Reg(.a),
            Encoder.init(4, Reg(.b)),
            Encoder.init(8, @as(u6, 0x2)),
            region_encoder,
        };
        pub const ik = Reg(.a);
        pub const iw = Reg(.b);

        pub fn entry(c: *Cycle) void {
            c.read_to_d(.sp, 0, .word, .stack);
            c.d_to_l(.zx);
            c.ll_to_reg();
            c.next_iw_from_ik();
            c.next(read_b);
        }

        pub fn read_b(c: *Cycle) void {
            c.read_to_d(.sp, 2, .word, .stack);
            c.d_to_l(.zx);
            c.ll_to_reg();
            c.next(modify_sp);
        }

        pub fn modify_sp(c: *Cycle) void {
            c.sr_to_j(.sp);
            c.literal_to_k(4);
            c.j_plus_k_to_l(.zx, .fresh, .no_flags);
            c.l_to_sr(.sp);
            c.load_and_exec_next_insn();
        }
    },
    struct { // push x(a), x(b)
        pub const spec = 
            \\push x(a), x(b)
            ;
        pub const encoding = .{
            Even_Reg(.a),
            Encoder.init(3, Even_Reg(.b)),
            Encoder.init(6, @as(u8, 0x5)),
            region_encoder,
        };
        pub const ij = Reg(.a);
        pub const ik = Reg(.b);

        pub fn entry(c: *Cycle) void {
            c.reg_to_jl();
            c.jl_to_ll();
            c.write_from_ll(.sp, -4, .word, .stack);
            c.next_ij_xor1();
            c.next(write_a_high);
        }

        pub fn write_a_high(c: *Cycle) void {
            c.reg_to_jl();
            c.jl_to_ll();
            c.write_from_ll(.sp, -2, .word, .stack);
            c.next(write_b);
        }

        pub fn write_b(c: *Cycle) void {
            c.reg_to_k();
            c.k_to_ll();
            c.write_from_ll(.sp, -8, .word, .stack);
            c.next_ik_xor1();
            c.next(write_b_high);
        }

        pub fn write_b_high(c: *Cycle) void {
            c.reg_to_k();
            c.k_to_ll();
            c.write_from_ll(.sp, -6, .word, .stack);
            c.next(modify_sp);
        }

        pub fn modify_sp(c: *Cycle) void {
            c.sr_to_j(.sp);
            c.literal_to_k(8);
            c.j_minus_k_to_l(.zx, .fresh, .no_flags);
            c.l_to_sr(.sp);
            c.load_and_exec_next_insn();
        }
    },
    struct { // pop x(b), x(a)
        pub const spec =
            \\pop x(b), x(a)
            ;
        pub const encoding = .{
            Even_Reg(.a),
            Encoder.init(3, Even_Reg(.b)),
            Encoder.init(6, @as(u8, 0xd)),
            region_encoder,
        };
        pub const ik = Reg(.a);
        pub const iw = Reg(.b);

        pub fn entry(c: *Cycle) void {
            c.read_to_d(.sp, 0, .word, .stack);
            c.d_to_l(.zx);
            c.ll_to_reg();
            c.next_iw_xor1();
            c.next(read_a_high);
        }

        pub fn read_a_high(c: *Cycle) void {
            c.read_to_d(.sp, 2, .word, .stack);
            c.d_to_l(.zx);
            c.ll_to_reg();
            c.next_iw_from_ik();
            c.next(read_b);
        }

        pub fn read_b(c: *Cycle) void {
            c.read_to_d(.sp, 4, .word, .stack);
            c.d_to_l(.zx);
            c.ll_to_reg();
            c.next_iw_xor1();
            c.next(read_b_high);
        }

        pub fn read_b_high(c: *Cycle) void {
            c.read_to_d(.sp, 6, .word, .stack);
            c.d_to_l(.zx);
            c.ll_to_reg();
            c.next(modify_sp);
        }

        pub fn modify_sp(c: *Cycle) void {
            c.sr_to_j(.sp);
            c.literal_to_k(8);
            c.j_plus_k_to_l(.zx, .fresh, .no_flags);
            c.l_to_sr(.sp);
            c.load_and_exec_next_insn();
        }
    },
    struct { // push <reg>
        pub const spec =
            \\push r(reg)
            \\push x(reg)
            ;
        pub const encoding = .{
            Reg(.reg),
            width_encoder,
            Encoder.init(5, @as(u9, 0x8)),
            region_encoder,
        };
        pub const ij = Reg(.reg);
        pub const ik: u4 = 0;

        fn width_encoder(reg: Param(.reg)) Encoder {
            return Encoder.init(4, @as(u1, switch (reg.signature.base) {
                .reg16 => 0,
                .reg32 => 1,
                else => unreachable,
            }));
        }

        pub fn entry(c: *Cycle, reg: Param(.reg)) void {
            c.reg_to_jl();
            c.jl_to_ll();
            if (reg.signature.base == .reg32) {
                c.write_from_ll(.sp, -4, .word, .stack);
                c.next_ij_xor1();
                c.next(write_high);
            } else {
                c.write_from_ll(.sp, -2, .word, .stack);
                c.next_ij(2);
                c.next(modify_sp);
            }
        }

        pub fn write_high(c: *Cycle) void {
            c.reg_to_jl();
            c.jl_to_ll();
            c.write_from_ll(.sp, -2, .word, .stack);
            c.next_ij(4);
            c.next(modify_sp);
        }

        pub fn modify_sp(c: *Cycle) void {
            c.sr_to_j(.sp);
            c.ik_ij_sx_to_k();
            c.j_minus_k_to_l(.zx, .fresh, .no_flags);
            c.l_to_sr(.sp);
            c.load_and_exec_next_insn();
        }
    },
    struct { // pop <reg>
        pub const spec =
            \\pop r(reg)
            \\pop x(reg)
            ;
        pub const encoding = .{
            Reg(.reg),
            width_encoder,
            Encoder.init(5, @as(u9, 0x18)),
            region_encoder,
        };
        pub const ik_ij = bytes_encoder;
        pub const iw = Reg(.reg);

        fn width_encoder(reg: Param(.reg)) Encoder {
            return Encoder.init(4, @as(u1, switch (reg.signature.base) {
                .reg16 => 0,
                .reg32 => 1,
                else => unreachable,
            }));
        }
        fn bytes_encoder(reg: Param(.reg)) u3 {
            return switch (reg.signature.base) {
                .reg16 => 2,
                .reg32 => 4,
                else => unreachable,
            };
        }

        pub fn entry(c: *Cycle, reg: Param(.reg)) void {
            c.read_to_d(.sp, 0, .word, .stack);
            c.d_to_l(.zx);
            c.ll_to_reg();
            if (reg.signature.base == .reg32) {
                c.next_iw_xor1();
                c.next(read_high);
            } else {
                c.next(modify_sp);
            }
        }

        pub fn read_high(c: *Cycle) void {
            c.read_to_d(.sp, 2, .word, .stack);
            c.d_to_l(.zx);
            c.ll_to_reg();
            c.next(modify_sp);
        }

        pub fn modify_sp(c: *Cycle) void {
            c.sr_to_j(.sp);
            c.ik_ij_sx_to_k();
            c.j_plus_k_to_l(.zx, .fresh, .no_flags);
            c.l_to_sr(.sp);
            c.load_and_exec_next_insn();
        }
    },
    struct { // <mcba/sia/soa> x(bytes) .signed, .d bp -> .d rp
        pub const spec =
            \\mcba x(bytes) .signed, .d bp -> .d rp
            \\sia x(bytes) .signed, .d bp -> .d rp
            \\soa x(bytes) .signed, .d bp -> .d rp
            ;
        pub const encoding = .{
            Even_Reg(.bytes),
            Encoder.init(3, @as(u1, 0)),
            mnemonic_encoder,
            Encoder.init(6, @as(u8, 0x21)),
            region_encoder,
        };
        pub const ij = Reg(.bytes);
        pub const iw = Reg(.bytes);

        fn mnemonic_encoder(mnemonic: isa.Mnemonic) Encoder {
            return Encoder.init(4, @as(u2, switch (mnemonic) {
                .mcba => 0,
                .sia => 2,
                .soa => 3,
                else => unreachable,
            }));
        }

        pub fn entry(c: *Cycle) void {
            c.reg32_to_j();
            c.literal_to_k(-2);
            c.j_plus_k_to_l(.sx, .fresh, .flags);
            c.next(initial_check_for_2_bytes_remaining);
        }

        pub fn initial_check_for_2_bytes_remaining(c: *Cycle, flags: Flags) void {
            if (flags.negative()) {
                // length == 0 or 1 bytes
                c.reg32_to_j();
                c.literal_to_k(-1);
                c.j_plus_k_to_l(.sx, .fresh, .flags);
                c.next(initial_check_for_1_byte_remaining);
            } else {
                c.read_to_dr(.bp, 0, .full, .data);
                c.reg32_to_j();
                c.literal_to_k(-2);
                c.j_plus_k_to_l(.sx, .fresh, .flags);
                c.l_to_reg32();
                if (flags.zero()) {
                    // length == 2 bytes
                    c.next(write_single_word);
                } else {
                    // length > 2 bytes
                    c.next(write_initial_word);
                }
            }
        }

        pub fn initial_check_for_1_byte_remaining(c: *Cycle, flags: Flags) void {
            c.reg32_to_j();
            c.literal_to_k(-2);
            c.j_plus_k_to_l(.sx, .fresh, .flags);
            c.l_to_reg32();
            if (flags.negative()) {
                // length == 0
                c.load_and_exec_next_insn();
            } else {
                // length == 1 byte
                c.read_to_dr(.bp, 0, .low, .data);
                c.next(write_single_byte);
            }
        }

        pub fn write_single_word(c: *Cycle) void {
            c.write_from_dr(.rp, 0, .word, .data);
            c.next(load_and_exec_next_insn);
        }

        pub fn write_single_byte(c: *Cycle) void {
            c.write_from_dr(.rp, 0, .byte, .data);
            c.next(load_and_exec_next_insn);
        }

        pub fn write_initial_word(c: *Cycle) void {
            c.write_from_dr(.rp, 0, .word, .data);
            c.reg32_to_j();
            c.literal_to_k(-2);
            c.j_plus_k_to_l(.sx, .fresh, .flags);
            c.next(read);
        }

        pub fn read(c: *Cycle, flags: Flags, mnemonic: isa.Mnemonic) void {
            const offset: u2 = if (mnemonic == .sia) 0 else 2;
            c.read_to_dr(.bp, offset, if (flags.negative()) .low else .full, .data);
            c.virtual_address_to_sr(.bp);
            c.reg32_to_j();
            c.literal_to_k(-2);
            c.j_plus_k_to_l(.sx, .fresh, .flags);
            c.l_to_reg32();
            if (flags.zero()) {
                // exactly 2 bytes left
                c.next(write_final_word);
            } else if (flags.negative()) {
                // exactly 1 byte left
                c.next(write_final_byte);
            } else {
                // > 2 bytes left
                c.next(write_word);
            }
        }

        pub fn write_word(c: *Cycle, mnemonic: isa.Mnemonic) void {
            const offset: u2 = if (mnemonic == .soa) 0 else 2;
            c.write_from_dr(.rp, offset, .word, .data);
            c.virtual_address_to_sr(.rp);
            c.reg32_to_j();
            c.literal_to_k(-2);
            c.j_plus_k_to_l(.sx, .fresh, .flags);
            c.allow_interrupt();
            c.next(read);
        }

        pub fn write_final_word(c: *Cycle, mnemonic: isa.Mnemonic) void {
            const offset: u2 = if (mnemonic == .soa) 0 else 2;
            c.write_from_dr(.rp, offset, .word, .data);
            c.virtual_address_to_sr(.rp);
            c.next(load_and_exec_next_insn);
        }

        pub fn write_final_byte(c: *Cycle, mnemonic: isa.Mnemonic) void {
            const offset: u2 = if (mnemonic == .soa) 0 else 2;
            c.write_from_dr(.rp, offset, .byte, .data);
            c.virtual_address_to_sr(.rp);
            c.next(load_and_exec_next_insn);
        }

        pub const load_and_exec_next_insn = Cycle.load_and_exec_next_insn;
    },
    struct { // <mcb/si/so> x(bytes) .signed, .d bp -> .d rp
        pub const spec =
            \\mcb x(bytes) .signed, .d bp -> .d rp
            \\si x(bytes) .signed, .d bp -> .d rp
            \\so x(bytes) .signed, .d bp -> .d rp
            ;
        pub const encoding = .{
            Even_Reg(.bytes),
            Encoder.init(3, @as(u1, 1)),
            mnemonic_encoder,
            Encoder.init(6, @as(u8, 0x21)),
            region_encoder,
        };
        pub const ij = Reg(.bytes);
        pub const iw = Reg(.bytes);

        fn mnemonic_encoder(mnemonic: isa.Mnemonic) Encoder {
            return Encoder.init(4, @as(u2, switch (mnemonic) {
                .mcb => 0,
                .si => 2,
                .so => 3,
                else => unreachable,
            }));
        }

        pub fn entry(c: *Cycle) void {
            c.reg32_to_j();
            c.srl_to_k(.one);
            c.j_minus_k_to_l(.sx, .fresh, .flags);
            c.next(initial_check_for_1_byte_remaining);
            c.l_to_reg32();
        }

        pub fn initial_check_for_1_byte_remaining(c: *Cycle, flags: Flags) void {
            if (flags.negative()) {
                // length == 0
                c.load_and_exec_next_insn();
            } else {
                c.read_to_dr(.bp, 0, .low, .data);
                if (flags.zero()) {
                    // length == 1 byte
                    c.next(write_single);
                } else {
                    // length > 1 byte
                    c.next(write_initial);
                }
            }
        }

        pub fn write_single(c: *Cycle) void {
            c.write_from_dr(.rp, 0, .byte, .data);
            c.next(load_and_exec_next_insn);
        }

        pub fn write_initial(c: *Cycle) void {
            c.write_from_dr(.rp, 0, .byte, .data);
            c.reg32_to_j();
            c.srl_to_k(.one);
            c.j_minus_k_to_l(.sx, .fresh, .flags);
            c.next(read);
        }

        pub fn read(c: *Cycle, flags: Flags, mnemonic: isa.Mnemonic) void {
            const offset: u2 = if (mnemonic == .si) 0 else 1;
            c.read_to_dr(.bp, offset, .low, .data);
            c.virtual_address_to_sr(.bp);
            c.reg32_to_j();
            c.srl_to_k(.one);
            c.j_minus_k_to_l(.sx, .fresh, .flags);
            c.l_to_reg32();
            if (flags.zero()) {
                // exactly 1 byte left
                c.next(write_final);
            } else {
                // > 2 bytes left
                c.next(write);
            }
        }

        pub fn write(c: *Cycle, mnemonic: isa.Mnemonic) void {
            const offset: u2 = if (mnemonic == .so) 0 else 1;
            c.write_from_dr(.rp, offset, .byte, .data);
            c.virtual_address_to_sr(.rp);
            c.reg32_to_j();
            c.srl_to_k(.one);
            c.j_minus_k_to_l(.sx, .fresh, .flags);
            c.allow_interrupt();
            c.next(read);
        }

        pub fn write_final(c: *Cycle, mnemonic: isa.Mnemonic) void {
            const offset: u2 = if (mnemonic == .so) 0 else 1;
            c.write_from_dr(.rp, offset, .byte, .data);
            c.virtual_address_to_sr(.rp);
            c.next(load_and_exec_next_insn);
        }

        pub const load_and_exec_next_insn = Cycle.load_and_exec_next_insn;
    },
    struct { // mcfa x(bytes) .signed, .d bp -> .d rp
        pub const spec = "mcfa x(bytes) .signed, .d bp -> .d rp";
        pub const encoding = .{
            Even_Reg(.bytes),
            Encoder.init(3, @as(u1, 0)),
            Encoder.init(4, @as(u2, 1)),
            Encoder.init(6, @as(u8, 0x21)),
            region_encoder,
        };
        pub const ij = Reg(.bytes);
        pub const iw = Reg(.bytes);

        pub fn entry(c: *Cycle) void {
            c.reg32_to_j();
            c.literal_to_k(-2);
            c.j_plus_k_to_l(.sx, .fresh, .flags);
            c.next(initial_check_for_2_bytes_remaining);
        }

        pub fn initial_check_for_2_bytes_remaining(c: *Cycle, flags: Flags) void {
            if (flags.negative()) {
                // length == 0 or 1 bytes
                c.reg32_to_j();
                c.literal_to_k(-1);
                c.j_plus_k_to_l(.sx, .fresh, .flags);
                c.l_to_reg32();
                c.next(initial_check_for_1_byte_remaining);
            } else {
                c.read_to_dr(.bp, -2, .full, .data);
                c.virtual_address_to_sr(.bp);
                c.reg32_to_j();
                c.literal_to_k(-2);
                c.j_plus_k_to_l(.sx, .fresh, .flags);
                c.l_to_reg32();
                if (flags.zero()) {
                    // length == 2 bytes
                    c.next(write_final_word);
                } else {
                    // length > 2 bytes
                    c.next(write_word);
                }
            }
        }

        pub fn initial_check_for_1_byte_remaining(c: *Cycle, flags: Flags) void {
            c.reg32_to_j();
            c.literal_to_k(-1);
            c.j_plus_k_to_l(.sx, .fresh, .flags);
            c.l_to_reg32();
            if (flags.negative()) {
                // length == 0
                c.load_and_exec_next_insn();
            } else {
                // length == 1 byte
                c.read_to_dr(.bp, -1, .low, .data);
                c.virtual_address_to_sr(.bp);
                c.next(write_final_byte);
            }
        }

        pub fn read(c: *Cycle, flags: Flags) void {
            if (flags.negative()) {
                c.read_to_dr(.bp, -1, .low, .data);
                c.literal_to_k(-1);
            } else {
                c.read_to_dr(.bp, -2, .full, .data);
                c.literal_to_k(-2);
            }
            c.virtual_address_to_sr(.bp);
            c.reg32_to_j();
            c.j_plus_k_to_l(.sx, .fresh, .flags);
            c.l_to_reg32();
            if (flags.zero()) {
                // exactly 2 bytes left
                c.next(write_final_word);
            } else if (flags.negative()) {
                // exactly 1 byte left
                c.next(write_final_byte);
            } else {
                // > 2 bytes left
                c.next(write_word);
            }
        }

        pub fn write_word(c: *Cycle) void {
            c.write_from_dr(.rp, -2, .word, .data);
            c.virtual_address_to_sr(.rp);
            c.reg32_to_j();
            c.literal_to_k(-2);
            c.j_plus_k_to_l(.sx, .fresh, .flags);
            c.allow_interrupt();
            c.next(read);
        }

        pub fn write_final_word(c: *Cycle) void {
            c.write_from_dr(.rp, -2, .word, .data);
            c.virtual_address_to_sr(.rp);
            c.next(load_and_exec_next_insn);
        }

        pub fn write_final_byte(c: *Cycle) void {
            c.write_from_dr(.rp, -1, .byte, .data);
            c.virtual_address_to_sr(.rp);
            c.literal_to_k(-1);
            c.reg32_to_j();
            c.j_plus_k_to_l(.sx, .fresh, .flags);
            c.l_to_reg32();
            c.next(load_and_exec_next_insn);
        }

        pub const load_and_exec_next_insn = Cycle.load_and_exec_next_insn;
    },
    struct { // mcf x(bytes) .signed, .d bp -> .d rp
        pub const spec = "mcf x(bytes) .signed, .d bp -> .d rp";
        pub const encoding = .{
            Even_Reg(.bytes),
            Encoder.init(3, @as(u1, 1)),
            Encoder.init(4, @as(u2, 1)),
            Encoder.init(6, @as(u8, 0x21)),
            region_encoder,
        };
        pub const ij = Reg(.bytes);
        pub const iw = Reg(.bytes);

        pub fn entry(c: *Cycle) void {
            c.reg32_to_j();
            c.srl_to_k(.one);
            c.j_minus_k_to_l(.sx, .fresh, .flags);
            c.next(read);
        }

        pub fn read(c: *Cycle, flags: Flags) void {
            c.reg32_to_j();
            c.srl_to_k(.one);
            c.j_minus_k_to_l(.sx, .fresh, .flags);
            c.l_to_reg32();
            if (flags.negative()) {
                // length == 0
                c.load_and_exec_next_insn();
            } else {
                c.read_to_dr(.bp, -1, .low, .data);
                c.virtual_address_to_sr(.bp);
                if (flags.zero()) {
                    // length == 1 byte
                    c.next(write_final);
                } else {
                    // length > 1 byte
                    c.next(write);
                }
            }
        }

        pub fn write(c: *Cycle) void {
            c.write_from_dr(.rp, -1, .byte, .data);
            c.virtual_address_to_sr(.rp);
            c.reg32_to_j();
            c.srl_to_k(.one);
            c.j_minus_k_to_l(.sx, .fresh, .flags);
            c.allow_interrupt();
            c.next(read);
        }

        pub fn write_final(c: *Cycle) void {
            c.write_from_dr(.rp, -1, .byte, .data);
            c.virtual_address_to_sr(.rp);
            c.next(load_and_exec_next_insn);
        }

        pub const load_and_exec_next_insn = Cycle.load_and_exec_next_insn;
    },
};

const Cycle = @import("../compile/Cycle.zig");
const Encoder = isa.Instruction_Encoding.Encoder;
const Parameter = isa.Parameter;
const Options = placeholders.Options;
const Range = placeholders.Range;
const Int = placeholders.Int;
const Int_Mult = placeholders.Int_Mult;
const Reg = placeholders.Reg;
const Bit = placeholders.Bit;
const Even_Reg = placeholders.Even_Reg;
const Odd_Reg = placeholders.Odd_Reg;
const Param = placeholders.Param;
const Negate = placeholders.Negate;
const Offset = placeholders.Offset;
const placeholders = @import("../compile/placeholders.zig");
const Control_Signals = arch.Control_Signals;
const Flags = arch.microcode.Flags;
const isa = @import("isa");
const arch = @import("arch");
const std = @import("std");
