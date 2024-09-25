const region_encoder = Encoder.init(14, @as(u2, 2));

pub const instructions = .{
    struct { // <add/cmp> r(reg), (imm8s) -> r(reg)
        pub const forms = .{
            struct {
                pub const spec =
                    \\add r(reg), (imm)
                    \\add r(reg), (imm) -> r(reg)
                    \\add (imm), r(reg) -> r(reg)
                    \\cmp r(reg), (imm)
                    ;
                pub const encoding = .{
                    Int(.imm, i8),
                    Encoder.init(8, Even_Reg(.reg)),
                    mnemonic_encoder,
                    Encoder.init(12, @as(u2, 3)),
                    region_encoder,
                };
            },
            struct {
                pub const spec =
                    \\sub r(reg), (imm)
                    \\sub r(reg), (imm) -> r(reg)
                    ;
                pub const encoding = .{
                    Negate(Int(.imm, i8)),
                    Encoder.init(8, Even_Reg(.reg)),
                    mnemonic_encoder,
                    Encoder.init(12, @as(u2, 3)),
                    region_encoder,
                };
            },
        };
        pub const ij = Reg(.reg);
        pub const iw = Reg(.reg);

        fn mnemonic_encoder(mnemonic: isa.Mnemonic) Encoder {
            return Encoder.init(11, @as(u1, switch (mnemonic) {
                .cmp => 0,
                .add, .sub => 1,
                else => unreachable,
            }));
        }

        pub fn entry(c: *Cycle, mnemonic: isa.Mnemonic, imm: placeholders.Any(.imm)) void {
            c.reg_to_jl();
            switch (mnemonic) {
                .add, .sub => {
                    const value = imm.value * @as(i64, if (mnemonic == .sub) -1 else 1);
                    c.literal_to_k(@intCast(value));
                    c.jl_plus_k_to_ll(.fresh, .flags);
                    c.ll_to_reg();
                },
                .cmp => {
                    c.literal_to_k(@intCast(imm.value));
                    c.jl_minus_k(.fresh, .flags);
                },
                else => unreachable,
            }
            c.load_and_exec_next_insn();
        }
    },
    struct { // <add/addc/sub/subc> r(a), r(b) -> r(dest)
        pub const spec =
            \\add r(a), r(b) -> r(dest)
            \\sub r(a), r(b) -> r(dest)
            \\addc r(a), r(b) -> r(dest)
            \\subc r(a), r(b) -> r(dest)
            \\add r(a, dest), r(b)
            \\sub r(a, dest), r(b)
            \\addc r(a, dest), r(b)
            \\subc r(a, dest), r(b)
            ;
        pub const encoding = .{
            Even_Reg(.dest),
            Encoder.init(3, Even_Reg(.a)),
            Encoder.init(6, Even_Reg(.b)),
            mnemonic_encoder,
            Encoder.init(11, @as(u3, 5)),
            region_encoder,
        };
        pub const ij = Reg(.a);
        pub const ik = Reg(.b);
        pub const iw = Reg(.dest);

        fn mnemonic_encoder(mnemonic: isa.Mnemonic) Encoder {
            return Encoder.init(9, @as(u2, switch (mnemonic) {
                .add => 0,
                .addc => 1,
                .sub => 2,
                .subc => 3,
                else => unreachable,
            }));
        }

        pub fn entry(c: *Cycle, mnemonic: isa.Mnemonic) void {
            c.reg_to_jl();
            c.reg_to_k();
            switch (mnemonic) {
                .add => c.jl_plus_k_to_ll(.fresh, .flags),
                .addc => c.jl_plus_k_to_ll(.cont, .flags),
                .sub => c.jl_minus_k_to_ll(.fresh, .flags),
                .subc => c.jl_minus_k_to_ll(.cont, .flags),
                else => unreachable,
            }
            c.ll_to_reg();
            c.load_and_exec_next_insn();
        }
    },
    struct { // <add/sub> x(a), r(b) -> x(dest)
        pub const spec = 
            \\add x(a), r(b) .unsigned -> x(dest)
            \\add x(a), r(b) .signed -> x(dest)
            \\add r(b) .unsigned, x(a) -> x(dest)
            \\add r(b) .signed, x(a) -> x(dest)
            \\sub x(a), r(b) .unsigned -> x(dest)
            \\sub x(a), r(b) .signed -> x(dest)
            \\sub r(b) .unsigned, x(a) -> x(dest)
            \\sub r(b) .signed, x(a) -> x(dest)
            \\add x(a, dest), r(b) .unsigned
            \\add x(a, dest), r(b) .signed
            \\sub x(a, dest), r(b) .unsigned
            \\sub x(a, dest), r(b) .signed
            ;
        pub const encoding = .{
            Even_Reg(.dest),
            Encoder.init(3, Even_Reg(.a)),
            Encoder.init(6, Even_Reg(.b)),
            signedness_encoder,
            mnemonic_encoder,
            Encoder.init(11, @as(u3, 4)),
            region_encoder,
        };
        pub const ij = Reg(.a);
        pub const ik = Reg(.b);
        pub const iw = Reg(.dest);

        fn signedness_encoder(b: Param(.b)) Encoder {
            return Encoder.init(9, @as(u1, switch (b.signature.base.reg16.?) {
                .unsigned => 0,
                .signed => 1,
            }));
        }
        fn mnemonic_encoder(mnemonic: isa.Mnemonic) Encoder {
            return Encoder.init(10, @as(u1, switch (mnemonic) {
                .add => 0,
                .sub => 1,
                else => unreachable,
            }));
        }

        pub fn entry(c: *Cycle, mnemonic: isa.Mnemonic, b: Param(.b)) void {
            c.reg32_to_j();
            c.reg_to_k();
            const ext: Cycle.Zero_Sign_Or_One_Extension = switch (b.signature.base.reg16.?) {
                .unsigned => .zx,
                .signed => .sx,
            };
            switch (mnemonic) {
                .add => c.j_plus_k_to_l(ext, .fresh, .flags),
                .sub => c.j_minus_k_to_l(ext, .fresh, .flags),
                else => unreachable,
            }
            c.l_to_reg32();
            c.load_and_exec_next_insn();
        }
    },
    struct { // <add/sub> <src_reg>, (imm3_1_8) -> <dest_reg>
        pub const spec =
            \\add r(src, dest), (imm)
            \\add r(src), (imm) -> r(dest)
            \\add (imm), r(src) -> r(dest)
            \\add x(src, dest), (imm)
            \\add x(src), (imm) -> x(dest)
            \\add (imm), x(src) -> x(dest)
            \\sub r(src, dest), (imm)
            \\sub r(src), (imm) -> r(dest)
            \\sub x(src, dest), (imm)
            \\sub x(src), (imm) -> x(dest)
            ;
        pub const encoding = .{
            Even_Reg(.dest),
            Encoder.init(3, Even_Reg(.src)),
            Encoder.init(6, Range(.imm, 1, 8)),
            width_encoder,
            mnemonic_encoder,
            Encoder.init(11, @as(u3, 3)),
            region_encoder,
        };
        pub const ij = Reg(.src);
        pub const iw = Reg(.dest);

        fn width_encoder(src: Param(.src)) Encoder {
            return Encoder.init(9, @as(u1, switch (src.signature.base) {
                .reg16 => 0,
                .reg32 => 1,
                else => unreachable,
            }));
        }
        fn mnemonic_encoder(mnemonic: isa.Mnemonic) Encoder {
            return Encoder.init(10, @as(u1, switch (mnemonic) {
                .add => 0,
                .sub => 1,
                else => unreachable,
            }));
        }

        pub fn entry(c: *Cycle, imm: placeholders.Any(.imm), mnemonic: isa.Mnemonic, src: Param(.src)) void {
            switch (src.signature.base) {
                .reg16 => c.reg_to_jl(),
                .reg32 => c.reg32_to_j(),
                else => unreachable,
            }
            c.literal_to_k(@intCast(imm.value));
            switch (src.signature.base) {
                .reg16 => {
                    switch (mnemonic) {
                        .add => c.jl_plus_k_to_ll(.fresh, .flags),
                        .sub => c.jl_minus_k_to_ll(.fresh, .flags),
                        else => unreachable,
                    }
                    c.ll_to_reg();
                },
                .reg32 => {
                    switch (mnemonic) {
                        .add => c.j_plus_k_to_l(.sx, .fresh, .flags),
                        .sub => c.j_minus_k_to_l(.sx, .fresh, .flags),
                        else => unreachable,
                    }
                    c.l_to_reg32();
                },
                else => unreachable,
            }
            c.load_and_exec_next_insn();
        }
    },
    struct { // <add/sub> <src_reg>, (imm3_n1_n8) -> <dest_reg>
        pub const spec =
            \\add r(src), (imm) -> r(dest)
            \\add x(src), (imm) -> x(dest)
            \\add (imm), r(src) -> r(dest)
            \\add (imm), x(src) -> x(dest)
            \\sub r(src), (imm) -> r(dest)
            \\sub x(src), (imm) -> x(dest)
            \\add r(src, dest), (imm)
            \\add x(src, dest), (imm)
            \\sub r(src, dest), (imm)
            \\sub x(src, dest), (imm)
            ;
        pub const encoding = .{
            Even_Reg(.dest),
            Encoder.init(3, Even_Reg(.src)),
            Encoder.init(6, Range(.imm, -1, -8)),
            width_encoder,
            mnemonic_encoder,
            Encoder.init(11, @as(u3, 3)),
            region_encoder,
        };
        pub const ij = Reg(.src);
        pub const iw = Reg(.dest);

        fn width_encoder(src: Param(.src)) Encoder {
            return Encoder.init(9, @as(u1, switch (src.signature.base) {
                .reg16 => 0,
                .reg32 => 1,
                else => unreachable,
            }));
        }
        fn mnemonic_encoder(mnemonic: isa.Mnemonic) Encoder {
            return Encoder.init(10, @as(u1, switch (mnemonic) {
                .add => 1,
                .sub => 0,
                else => unreachable,
            }));
        }

        pub fn entry(c: *Cycle, imm: placeholders.Any(.imm), mnemonic: isa.Mnemonic, src: Param(.src)) void {
            switch (src.signature.base) {
                .reg16 => c.reg_to_jl(),
                .reg32 => c.reg32_to_j(),
                else => unreachable,
            }
            c.literal_to_k(@intCast(-imm.value));
            switch (src.signature.base) {
                .reg16 => {
                    switch (mnemonic) {
                        .add => c.jl_minus_k_to_ll(.fresh, .flags),
                        .sub => c.jl_plus_k_to_ll(.fresh, .flags),
                        else => unreachable,
                    }
                    c.ll_to_reg();
                },
                .reg32 => {
                    switch (mnemonic) {
                        .add => c.j_minus_k_to_l(.sx, .fresh, .flags),
                        .sub => c.j_plus_k_to_l(.sx, .fresh, .flags),
                        else => unreachable,
                    }
                    c.l_to_reg32();
                },
                else => unreachable,
            }
            c.load_and_exec_next_insn();
        }
    },
    struct { // <inc/dec> <src> -> <dest>
        pub const spec =
            \\inc r(src, dest)
            \\inc r(src) -> r(dest)
            \\inc x(src, dest)
            \\inc x(src) -> x(dest)
            \\dec r(src, dest)
            \\dec r(src) -> r(dest)
            \\dec x(src, dest)
            \\dec x(src) -> x(dest)
            ;
        pub const encoding = .{
            Even_Reg(.dest),
            Encoder.init(3, Even_Reg(.src)),
            Encoder.init(6, @as(u3, 0)),
            width_encoder,
            mnemonic_encoder,
            Encoder.init(11, @as(u3, 3)),
            region_encoder,
        };
        pub const ij = Reg(.src);
        pub const iw = Reg(.dest);

        fn width_encoder(src: Param(.src)) Encoder {
            return Encoder.init(9, @as(u1, switch (src.signature.base) {
                .reg16 => 0,
                .reg32 => 1,
                else => unreachable,
            }));
        }
        fn mnemonic_encoder(mnemonic: isa.Mnemonic) Encoder {
            return Encoder.init(10, @as(u1, switch (mnemonic) {
                .inc => 0,
                .dec => 1,
                else => unreachable,
            }));
        }

        pub fn entry(c: *Cycle, mnemonic: isa.Mnemonic, src: Param(.src)) void {
            switch (src.signature.base) {
                .reg16 => c.reg_to_jl(),
                .reg32 => c.reg32_to_j(),
                else => unreachable,
            }
            c.literal_to_k(1);
            switch (src.signature.base) {
                .reg16 => {
                    switch (mnemonic) {
                        .inc => c.jl_plus_k_to_ll(.fresh, .flags),
                        .dec => c.jl_minus_k_to_ll(.fresh, .flags),
                        else => unreachable,
                    }
                    c.ll_to_reg();
                },
                .reg32 => {
                    switch (mnemonic) {
                        .inc => c.j_plus_k_to_l(.sx, .fresh, .flags),
                        .dec => c.j_minus_k_to_l(.sx, .fresh, .flags),
                        else => unreachable,
                    }
                    c.l_to_reg32();
                },
                else => unreachable,
            }
            c.load_and_exec_next_insn();
        }
    },
    struct { // <add/addc/sub/subc> r(src), (imm16) -> r(dest)
        pub const spec =
            \\add r(src, dest), (imm)
            \\addc r(src, dest), (imm)
            \\add r(src), (imm) -> r(dest)
            \\addc r(src), (imm) -> r(dest)
            \\addc (imm), r(src) -> r(dest)
            \\sub (imm), r(src) -> r(dest)
            \\subc (imm), r(src) -> r(dest)
            ;
        pub const forms = .{
            struct {
                pub const encoding = .{
                    Reg(.dest),
                    Encoder.init(4, Reg(.src)),
                    mnemonic_encoder,
                    Encoder.init(10, @as(u4, 5)),
                    region_encoder,
                    Encoder.init(16, Int(.imm, u16)),
                };
            },
            struct {
                pub const encoding = .{
                    Reg(.dest),
                    Encoder.init(4, Reg(.src)),
                    mnemonic_encoder,
                    Encoder.init(10, @as(u4, 5)),
                    region_encoder,
                    Encoder.init(16, Int(.imm, i16)),
                };
            },
        };
        pub const ij = Reg(.src);
        pub const ik = Reg(.src);
        pub const iw = Reg(.dest);

        fn mnemonic_encoder(mnemonic: isa.Mnemonic) Encoder {
            return Encoder.init(8, @as(u2, switch (mnemonic) {
                .add => 0,
                .addc => 1,
                .sub => 2,
                .subc => 3,
                else => unreachable,
            }));
        }

        pub fn entry(c: *Cycle) void {
            c.ip_read_to_d(2, .word);
            c.d_to_l(.zx);
            c.l_to_sr(.temp_1);
            c.next(add);
        }

        pub fn add(c: *Cycle, mnemonic: isa.Mnemonic) void {
            const freshness: Cycle.Freshness = switch (mnemonic) {
                .add, .sub => .fresh,
                .addc, .subc => .cont,
                else => unreachable,
            };
            switch (mnemonic) {
                .add, .addc => {
                    c.reg_to_jl();
                    c.srl_to_k(.temp_1);
                    c.jl_plus_k_to_ll(freshness, .flags);
                },
                .sub, .subc => {
                    c.sr_to_j(.temp_1);
                    c.reg_to_k();
                    c.jl_minus_k_to_ll(freshness, .flags);
                },
                else => unreachable,
            }
            c.ll_to_reg();
            c.load_and_exec_next_insn();
        }
    },
    struct { // <sub/subc> r(src), (imm16) -> r(dest)
        pub const forms = .{
            struct {
                pub const spec =
                    \\sub r(src, dest), (imm)
                    \\sub r(src), (imm) -> r(dest)
                    ;
                pub const encoding = .{
                    Reg(.dest),
                    Encoder.init(4, Reg(.src)),
                    Encoder.init(8, @as(u2, 0)),
                    Encoder.init(10, @as(u4, 5)),
                    region_encoder,
                    Encoder.init(16, Negate(Int(.imm, u16))),
                };
            },
            struct {
                pub const spec =
                    \\sub r(src, dest), (imm)
                    \\sub r(src), (imm) -> r(dest)
                    ;
                pub const encoding = .{
                    Reg(.dest),
                    Encoder.init(4, Reg(.src)),
                    Encoder.init(8, @as(u2, 0)),
                    Encoder.init(10, @as(u4, 5)),
                    region_encoder,
                    Encoder.init(16, Negate(Int(.imm, i16))),
                };
            },
            struct {
                pub const spec =
                    \\subc r(src, dest), (imm)
                    \\subc r(src), (imm) -> r(dest)
                    ;
                pub const encoding = .{
                    Reg(.dest),
                    Encoder.init(4, Reg(.src)),
                    Encoder.init(8, @as(u2, 1)),
                    Encoder.init(10, @as(u4, 5)),
                    region_encoder,
                    Encoder.init(16, Offset(-1, Negate(Int(.imm, u16)))),
                };
            },
            struct {
                pub const spec =
                    \\subc r(src, dest), (imm)
                    \\subc r(src), (imm) -> r(dest)
                    ;
                pub const encoding = .{
                    Reg(.dest),
                    Encoder.init(4, Reg(.src)),
                    Encoder.init(8, @as(u2, 1)),
                    Encoder.init(10, @as(u4, 5)),
                    region_encoder,
                    Encoder.init(16, Offset(-1, Negate(Int(.imm, i16)))),
                };
            },
        };
        pub const ij = Reg(.src);
        pub const ik = Reg(.src);
        pub const iw = Reg(.dest);

        pub fn entry(c: *Cycle) void {
            c.ip_read_to_d(2, .word);
            c.d_to_l(.zx);
            c.l_to_sr(.temp_1);
            c.next(add);
        }

        pub fn add(c: *Cycle, mnemonic: isa.Mnemonic) void {
            const freshness: Cycle.Freshness = switch (mnemonic) {
                .sub => .fresh,
                .subc => .cont,
                else => unreachable,
            };
            c.reg_to_jl();
            c.srl_to_k(.temp_1);
            c.jl_plus_k_to_ll(freshness, .flags);
            c.ll_to_reg();
            c.load_and_exec_next_insn();
        }
    },
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
            \\tb r(reg), (imm)
            \\sb r(reg), (imm)
            \\cb r(reg), (imm)
            ;
        pub const encoding = .{
            Reg_Bit(.imm),
            Encoder.init(4, Even_Reg(.reg)),
            mnemonic_encoder,
            Encoder.init(9, @as(u5, 7)),
            region_encoder,
        };
        pub const ij = Reg(.reg);
        pub const ik = Reg_Bit(.imm);
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
    struct { // add x(src), (imm) -> x(dest)
        pub const forms = .{
            struct {
                pub const spec = 
                    \\add x(src, dest), (imm)
                    \\add x(src), (imm) -> x(dest)
                    \\add (imm), x(src) -> x(dest)
                    ;
                pub const encoding = .{
                    Even_Reg(.dest),
                    Encoder.init(3, Even_Reg(.src)),
                    Encoder.init(6, @as(u8, 0x36)),
                    region_encoder,
                    Encoder.init(16, Int(.imm, u16)),
                };
            },
            struct {
                pub const spec = 
                    \\sub x(src, dest), (imm)
                    \\sub x(src), (imm) -> x(dest)
                    ;
                pub const encoding = .{
                    Even_Reg(.dest),
                    Encoder.init(3, Even_Reg(.src)),
                    Encoder.init(6, @as(u8, 0x36)),
                    region_encoder,
                    Encoder.init(16, Negate(Int(.imm, u16))),
                };
            },
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
    struct { // add x(src), (imm) -> x(dest)
        pub const forms = .{
            struct {
                pub const spec =
                    \\add x(src, dest), (imm)
                    \\add x(src), (imm) -> x(dest)
                    \\add (imm), x(src) -> x(dest)
                    ;
                pub const encoding = .{
                    Even_Reg(.dest),
                    Encoder.init(3, Even_Reg(.src)),
                    Encoder.init(6, @as(u8, 0x37)),
                    region_encoder,
                    Encoder.init(16, Range(.imm, -0x10000, -1)),
                };
            },
            struct {
                pub const spec =
                    \\sub x(src, dest), (imm)
                    \\sub x(src), (imm) -> x(dest)
                    ;
                pub const encoding = .{
                    Even_Reg(.dest),
                    Encoder.init(3, Even_Reg(.src)),
                    Encoder.init(6, @as(u8, 0x37)),
                    region_encoder,
                    Encoder.init(16, Negate(Range(.imm, -0x10000, -1))),
                };
            },
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
    struct { // cmp x(a), r(b)
        pub const spec =
            \\cmp x(a), r(b) .unsigned
            \\cmp x(a), r(b) .signed
            ;
        pub const encoding = .{
            Even_Reg(.a),
            Encoder.init(3, Even_Reg(.b)),
            signedness_encoder,
            Encoder.init(7, @as(u7, 0x1a)),
            region_encoder,
        };
        pub const ij = Reg(.a);
        pub const ik = Reg(.b);

        fn signedness_encoder(b: Param(.b)) Encoder {
            return Encoder.init(6, @as(u1, switch (b.signature.base.reg16.?) {
                .unsigned => 0,
                .signed => 1,
            }));
        }

        pub fn entry(c: *Cycle, b: Param(.b)) void {
            c.reg32_to_j();
            c.reg_to_k();
            c.j_minus_k(switch (b.signature.base.reg16.?) {
                .unsigned => .zx,
                .signed => .sx,
            }, .fresh, .flags);
            c.load_and_exec_next_insn();
        }
    },
    struct { // <addc/subc> <src> -> <dest>
        pub const spec =
            \\addc r(src, dest)
            \\addc r(src) -> r(dest)
            \\addc r(src, dest), 0
            \\addc r(src), 0 -> r(dest)
            \\addc 0, r(src) -> r(dest)
            \\addc x(src, dest)
            \\addc x(src) -> x(dest)
            \\addc x(src, dest), 0
            \\addc x(src), 0 -> x(dest)
            \\addc 0, x(src) -> x(dest)
            \\subc r(src, dest)
            \\subc r(src) -> r(dest)
            \\subc r(src, dest), 0
            \\subc r(src), 0 -> r(dest)
            \\subc x(src, dest)
            \\subc x(src) -> x(dest)
            \\subc x(src, dest), 0
            \\subc x(src), 0 -> x(dest)
            ;
        pub const encoding = .{
            Even_Reg(.dest),
            Encoder.init(3, Even_Reg(.src)),
            width_encoder,
            mnemonic_encoder,
            Encoder.init(8, @as(u6, 0xc)),
            region_encoder,
        };
        pub const ij = Reg(.src);
        pub const iw = Reg(.dest);

        fn width_encoder(src: Param(.src)) Encoder {
            return Encoder.init(6, @as(u1, switch (src.signature.base) {
                .reg16 => 0,
                .reg32 => 1,
                else => unreachable,
            }));
        }
        fn mnemonic_encoder(mnemonic: isa.Mnemonic) Encoder {
            return Encoder.init(7, @as(u1, switch (mnemonic) {
                .subc => 0,
                .addc => 1,
                else => unreachable,
            }));
        }

        pub fn entry(c: *Cycle, mnemonic: isa.Mnemonic, src: Param(.src)) void {
            c.zero_to_k();
            switch (src.signature.base) {
                .reg16 => {
                    c.reg_to_jl();
                    switch (mnemonic) {
                        .addc => c.jl_plus_k_to_ll(.fresh, .flags),
                        .subc => c.jl_minus_k_to_ll(.fresh, .flags),
                        else => unreachable,
                    }
                    c.ll_to_reg();
                },
                .reg32 => {
                    c.reg32_to_j();
                    switch (mnemonic) {
                        .addc => c.j_plus_k_to_l(.zx, .fresh, .flags),
                        .subc => c.j_minus_k_to_l(.zx, .fresh, .flags),
                        else => unreachable,
                    }
                    c.l_to_reg32();
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
    struct { // cmp[c] r(a), (imm16)
        pub const spec = 
            \\cmp r(a), (imm)
            \\cmpc r(a), (imm)
            ;
        pub const ij = Reg(.a);
        pub const forms = .{
            struct {
                pub const encoding = .{
                    Reg(.a),
                    mnemonic_encoder,
                    Encoder.init(5, @as(u9, 0x40)),
                    region_encoder,
                    Encoder.init(16, Int(.imm, u16)),
                };
            },
            struct {
                pub const encoding = .{
                    Reg(.a),
                    mnemonic_encoder,
                    Encoder.init(5, @as(u9, 0x40)),
                    region_encoder,
                    Encoder.init(16, Int(.imm, i16)),
                };
            },
        };

        fn mnemonic_encoder(mnemonic: isa.Mnemonic) Encoder {
            return Encoder.init(4, @as(u1, switch (mnemonic) {
                .cmp => 0,
                .cmpc => 1,
                else => unreachable,
            }));
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
    struct { // cmp[c] x(a), (imm16u)
        pub const spec =
            \\cmp x(a), (imm)
            ;
        pub const encoding = .{
            Reg(.a),
            Encoder.init(4, @as(u10, 0x082)),
            region_encoder,
            Encoder.init(16, Int(.imm, u16)),
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
    struct { // cmp[c] x(a), (imm16n)
        pub const spec =
            \\cmp x(a), (imm)
            ;
        pub const encoding = .{
            Reg(.a),
            Encoder.init(4, @as(u10, 0x083)),
            region_encoder,
            Encoder.init(16, Range(.imm, -0x10000, -1)),
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
    struct { // bld x(bytes) .signed -> .d bp
        pub const spec = "bld x(bytes) .signed -> .d bp";
        //Load block(s) from FLASH or PSRAM to RAM
        // x(bytes) indicates the number of bytes remaining to be copied (should be a multiple of 8 and > 0)
        // x(bytes) will be decremented by 8 each cycle; operation ends when zero or negative
        // BP points to just before the first byte of RAM to write to (will be incremented by 8 before each cycle).
        // Block source must be configured beforehand by writing to the configuration port.
        // Only one pipes 1 and 3 may perform block transfers.
        pub const encoding = .{
            Even_Reg(.bytes),
            Encoder.init(3, @as(u11, 0x110)),
            region_encoder,
        };
        pub const ij = Reg(.bytes);
        pub const iw = Reg(.bytes);

        pub fn entry(c: *Cycle, flags: Flags) void {
            if (!flags.kernel()) return c.illegal_instruction();
            c.reg32_to_j();
            c.literal_to_k(8);
            c.j_minus_k_to_l(.zx, .fresh, .flags);
            c.l_to_reg32();
            c.load_next_insn();
            c.next(transfer);
        }

        pub fn transfer(c: *Cycle, flags: Flags) void {
            c.block_transfer_to_ram(.bp, 8, .data);
            if (flags.positive()) {
                c.reg32_to_j();
                c.literal_to_k(8);
                c.j_minus_k_to_l(.zx, .fresh, .flags);
                c.l_to_reg32();
                c.next(transfer);
            } else {
                c.exec_next_insn();
            }
        }
    },
    struct { // bst x(bytes) .signed, .d bp
        pub const spec = "bst x(bytes) .signed, .d bp";
        //Store block(s) from RAM into FLASH or PSRAM
        // x(bytes) indicates the number of bytes remaining to be copied (should be a multiple of 8 and > 0)
        // x(bytes) will be decremented by 8 each cycle; operation ends when zero or negative
        // BP points to just before the first byte of RAM to read (will be incremented by 8 before each cycle).
        // Block destination must be configured beforehand by writing to the configuration port.
        // Only one pipes 1 and 3 may perform block transfers.
        pub const encoding = .{
            Even_Reg(.bytes),
            Encoder.init(3, @as(u11, 0x111)),
            region_encoder,
        };
        pub const ij = Reg(.bytes);
        pub const iw = Reg(.bytes);

        pub fn entry(c: *Cycle, flags: Flags) void {
            if (!flags.kernel()) return c.illegal_instruction();
            c.reg32_to_j();
            c.literal_to_k(8);
            c.j_minus_k_to_l(.zx, .fresh, .flags);
            c.l_to_reg32();
            c.load_next_insn();
            c.next(transfer);
        }

        pub fn transfer(c: *Cycle, flags: Flags) void {
            c.block_transfer_from_ram(.bp, 8, .data);
            if (flags.positive()) {
                c.reg32_to_j();
                c.literal_to_k(8);
                c.j_minus_k_to_l(.zx, .fresh, .flags);
                c.l_to_reg32();
                c.next(transfer);
            } else {
                c.exec_next_insn();
            }
        }
    },
    struct { // ald .d x(src) -> <reg>
        pub const spec =
            \\ald .d x(src) -> r(dest)
            \\ald .d x(src) -> x(dest)
            ;
        pub const encoding = .{
            Even_Reg(.dest),
            Encoder.init(3, Even_Reg(.src)),
            atomic_width_encoder,
            Encoder.init(7, @as(u7, 0x7)),
            region_encoder,
        };
        pub const ij = Reg(.src);
        pub const iw = Reg(.dest);

        pub fn entry(c: *Cycle) void {
            c.reg32_to_l();
            c.l_to_sr(.temp_1);
            c.load_next_insn();
            c.next(atomic_load);
        }

        pub fn atomic_load(c: *Cycle, dest: Param(.dest)) void {
            c.atomic_this_cycle();
            c.read_to_d(.temp_1, 0, .word, .data);
            c.d_to_ll();
            c.ll_to_reg();
            if (dest.signature.base == .reg32) {
                c.next_iw_xor1();
                c.next(atomic_load_high);
            } else {
                c.exec_next_insn();
            }
        }

        pub fn atomic_load_high(c: *Cycle) void {
            c.atomic_this_cycle();
            c.read_to_d(.temp_1, 2, .word, .data);
            c.d_to_ll();
            c.ll_to_reg();
            c.exec_next_insn();
        }
    },
    struct { // ast <reg> -> .d x(dest)
        pub const spec =
            \\ast r(src) -> .d x(dest)
            \\ast x(src) -> .d x(dest)
            ;
        pub const encoding = .{
            Even_Reg(.dest),
            Encoder.init(3, Even_Reg(.src)),
            atomic_width_encoder,
            Encoder.init(7, @as(u7, 0x3)),
            region_encoder,
        };
        pub const ij = Reg(.dest);
        pub const ik = Reg(.src);

        pub fn entry(c: *Cycle) void {
            c.reg32_to_l();
            c.l_to_sr(.temp_1);
            c.load_next_insn();
            c.next(atomic_store);
        }

        pub fn atomic_store(c: *Cycle, src: Param(.src)) void {
            c.atomic_this_cycle();
            c.reg_to_k();
            c.k_to_ll();
            c.write_from_ll(.temp_1, 0, .word, .data);
            if (src.signature.base == .reg32) {
                c.next_ik_xor1();
                c.next(atomic_store_high);
            } else {
                c.exec_next_insn();
            }
        }

        pub fn atomic_store_high(c: *Cycle) void {
            c.atomic_this_cycle();
            c.reg_to_k();
            c.k_to_ll();
            c.write_from_ll(.temp_1, 2, .word, .data);
            c.exec_next_insn();
        }
    },
    struct { // aadd .d x(mem), <reg> -> <reg0>
        pub const spec =
            \\aadd r(offset), .d x(mem) -> r0
            \\aadd .d x(mem), r(offset) -> r0
            \\aadd x(offset), .d x(mem) -> x0
            \\aadd .d x(mem), x(offset) -> x0
            ;
        pub const encoding = .{
            Even_Reg(.mem),
            Encoder.init(3, Even_Reg(.offset)),
            atomic_width_encoder,
            Encoder.init(7, @as(u7, 0x8)),
            region_encoder,
        };
        pub const ij = Reg(.mem);
        pub const ik = Reg(.offset);
        pub const iw: u4 = 0;

        pub fn entry(c: *Cycle) void {
            c.reg32_to_l();
            c.l_to_sr(.temp_1);
            c.load_next_insn();
            c.next_ij_from_iw();
            c.next(atomic_load);
        }

        pub fn atomic_load(c: *Cycle, offset: Param(.offset)) void {
            c.atomic_this_cycle();
            c.read_to_d(.temp_1, 0, .word, .data);
            c.d_to_ll();
            c.ll_to_reg();
            if (offset.signature.base == .reg32) {
                c.next_iw_xor1();
                c.next(atomic_load_high);
            } else {
                c.next(atomic_store);
            }
        }

        pub fn atomic_load_high(c: *Cycle) void {
            c.atomic_this_cycle();
            c.read_to_d(.temp_1, 2, .word, .data);
            c.d_to_ll();
            c.ll_to_reg();
            c.next_iw_xor1();
            c.next(atomic_store);
        }

        pub fn atomic_store(c: *Cycle, offset: Param(.offset)) void {
            c.atomic_this_cycle();
            c.reg_to_jl();
            c.reg_to_k();
            c.jl_plus_k_to_ll(.fresh, .flags);
            c.ll_to_reg();
            c.write_from_ll(.temp_1, 0, .word, .data);
            if (offset.signature.base == .reg32) {
                c.next_ij_xor1();
                c.next_ik_xor1();
                c.next_iw_xor1();
                c.next(atomic_store_high);
            } else {
                c.exec_next_insn();
            }
        }

        pub fn atomic_store_high(c: *Cycle) void {
            c.atomic_this_cycle();
            c.reg_to_jl();
            c.reg_to_k();
            c.jl_plus_k_to_ll(.cont, .flags);
            c.ll_to_reg();
            c.write_from_ll(.temp_1, 2, .word, .data);
            c.exec_next_insn();
        }
    },
    struct { // aadd .d x(mem), (imm:1,2,4,8) -> <reg0>
        pub const spec =
            \\aadd (imm), .d x(mem) -> r0
            \\aadd .d x(mem), (imm) -> r0
            \\aadd (imm), .d x(mem) -> x0
            \\aadd .d x(mem), (imm) -> x0
            ;
        pub const encoding = .{
            Even_Reg(.mem),
            Encoder.init(3, Imm),
            Encoder.init(5, @as(u1, 0)),
            atomic_width_encoder,
            Encoder.init(7, @as(u7, 0x9)),
            region_encoder,
        };
        const Imm = Options(.imm, .{ 1, 2, 4, 8 });
        pub const ij = Reg(.mem);
        pub const ik = Imm;
        pub const iw: u4 = 0;

        pub fn entry(c: *Cycle) void {
            c.reg32_to_l();
            c.l_to_sr(.temp_1);
            c.load_next_insn();
            c.next_ij_from_iw();
            c.next(atomic_load);
        }

        pub fn atomic_load(c: *Cycle, dest: Param(3)) void {
            c.atomic_this_cycle();
            c.read_to_d(.temp_1, 0, .word, .data);
            c.d_to_ll();
            c.ll_to_reg();
            if (dest.signature.base == .reg32) {
                c.next_iw_xor1();
                c.next(atomic_load_high);
            } else {
                c.next(atomic_store);
            }
        }

        pub fn atomic_load_high(c: *Cycle) void {
            c.atomic_this_cycle();
            c.read_to_d(.temp_1, 2, .word, .data);
            c.d_to_ll();
            c.ll_to_reg();
            c.next_iw_xor1();
            c.next(atomic_store);
        }

        pub fn atomic_store(c: *Cycle, dest: Param(3)) void {
            c.atomic_this_cycle();
            c.reg_to_jl();
            c.ik_bit_to_k();
            c.jl_plus_k_to_ll(.fresh, .flags);
            c.ll_to_reg();
            c.write_from_ll(.temp_1, 0, .word, .data);
            if (dest.signature.base == .reg32) {
                c.next_ij_xor1();
                c.next_iw_xor1();
                c.next(atomic_store_high);
            } else {
                c.exec_next_insn();
            }
        }

        pub fn atomic_store_high(c: *Cycle) void {
            c.atomic_this_cycle();
            c.reg_to_jl();
            c.zero_to_k();
            c.jl_plus_k_to_ll(.cont, .flags);
            c.ll_to_reg();
            c.write_from_ll(.temp_1, 2, .word, .data);
            c.exec_next_insn();
        }
    },
    struct { // aadd .d x(mem), (imm:-1,-2,-4,-8) -> <reg0>
        pub const spec = 
            \\aadd .d x(mem), (imm) -> r0
            \\aadd (imm), .d x(mem) -> r0
            \\aadd .d x(mem), (imm) -> x0
            \\aadd (imm), .d x(mem) -> x0
            ;
        pub const encoding = .{
            Even_Reg(.mem),
            Encoder.init(3, Imm),
            Encoder.init(5, @as(u1, 1)),
            atomic_width_encoder,
            Encoder.init(7, @as(u7, 0x9)),
            region_encoder,
        };
        const Imm = Options(.imm, .{ -1, -2, -4, -8 });
        pub const ij = Reg(.mem);
        pub const ik = Imm;
        pub const iw: u4 = 0;

        pub fn entry(c: *Cycle) void {
            c.reg32_to_l();
            c.l_to_sr(.temp_1);
            c.load_next_insn();
            c.next_ij_from_iw();
            c.next(atomic_load);
        }

        pub fn atomic_load(c: *Cycle, dest: Param(3)) void {
            c.atomic_this_cycle();
            c.read_to_d(.temp_1, 0, .word, .data);
            c.d_to_ll();
            c.ll_to_reg();
            if (dest.signature.base == .reg32) {
                c.next_iw_xor1();
                c.next(atomic_load_high);
            } else {
                c.next(atomic_store);
            }
        }

        pub fn atomic_load_high(c: *Cycle) void {
            c.atomic_this_cycle();
            c.read_to_d(.temp_1, 2, .word, .data);
            c.d_to_ll();
            c.ll_to_reg();
            c.next_iw_xor1();
            c.next(atomic_store);
        }

        pub fn atomic_store(c: *Cycle, dest: Param(3)) void {
            c.atomic_this_cycle();
            c.reg_to_jl();
            c.ik_bit_to_k();
            c.jl_minus_k_to_ll(.fresh, .flags);
            c.ll_to_reg();
            c.write_from_ll(.temp_1, 0, .word, .data);
            if (dest.signature.base == .reg32) {
                c.next_ij_xor1();
                c.next_iw_xor1();
                c.next(atomic_store_high);
            } else {
                c.exec_next_insn();
            }
        }

        pub fn atomic_store_high(c: *Cycle) void {
            c.atomic_this_cycle();
            c.reg_to_jl();
            c.zero_to_k();
            c.jl_plus_k_to_ll(.cont, .flags);
            c.ll_to_reg();
            c.write_from_ll(.temp_1, 2, .word, .data);
            c.exec_next_insn();
        }
    },
    struct { // astz <reg> -> .d x(dest)
        pub const spec =
            \\astz r(src) -> .d x(dest)
            \\astz x(src) -> .d x(dest)
            ;
        pub const encoding = .{
            Even_Reg(.dest),
            Encoder.init(3, Even_Reg(.src)),
            atomic_width_encoder,
            Encoder.init(7, @as(u7, 0xb)),
            region_encoder,
        };
        pub const ij = Reg(.dest);
        pub const ik = Reg(.src);

        pub fn entry(c: *Cycle) void {
            c.reg32_to_l();
            c.l_to_sr(.temp_1);
            c.load_next_insn();
            c.next(atomic_check);
        }

        pub fn atomic_check(c: *Cycle, src: Param(.src)) void {
            c.atomic_this_cycle();
            c.read_to_d(.temp_1, 0, .word, .data);
            c.d_to_ll();
            c.zn_flags_from_ll();
            if (src.signature.base == .reg32) {
                c.next_ik_xor1();
                c.next(atomic_check_high);
            } else {
                c.next(atomic_store);
            }
        }

        pub fn atomic_check_high(c: *Cycle, flags: Flags) void {
            if (flags.zero()) {
                c.atomic_this_cycle();
                c.read_to_d(.temp_1, 2, .word, .data);
                c.d_to_ll();
                c.zn_flags_from_ll();
                c.next_ik_xor1();
                c.next(atomic_store);
            } else {
                c.exec_next_insn();
            }
        }

        pub fn atomic_store(c: *Cycle, flags: Flags, src: Param(.src)) void {
            if (flags.zero()) {
                c.atomic_this_cycle();
                c.reg_to_k();
                c.k_to_ll();
                c.write_from_ll(.temp_1, 0, .word, .data);
                if (src.signature.base == .reg32) {
                    c.next_ik_xor1();
                    c.next(atomic_store_high);
                } else {
                    c.exec_next_insn();
                }
            } else {
                c.exec_next_insn();
            }
        }

        pub fn atomic_store_high(c: *Cycle) void {
            c.atomic_this_cycle();
            c.reg_to_k();
            c.k_to_ll();
            c.write_from_ll(.temp_1, 2, .word, .data);
            c.exec_next_insn();
        }
    },
    struct { // adecnz .d x(mem) -> <reg0>
        pub const spec =
            \\adecnz .d x(mem) -> r0
            \\adecnz .d x(mem) -> x0
            ;
        pub const encoding = .{
            Even_Reg(.mem),
            Encoder.init(3, @as(u3, 2)),
            atomic_width_encoder,
            Encoder.init(7, @as(u7, 0xa)),
            region_encoder,
        };
        pub const ij = Reg(.mem);
        pub const iw: u4 = 0;

        pub fn entry(c: *Cycle) void {
            c.reg32_to_l();
            c.l_to_sr(.temp_1);
            c.load_next_insn();
            c.next_ij_from_iw();
            c.next(atomic_load);
        }

        pub fn atomic_load(c: *Cycle, dest: Param(2)) void {
            c.atomic_this_cycle();
            c.read_to_d(.temp_1, 0, .word, .data);
            c.d_to_ll();
            c.ll_to_reg();
            c.zn_flags_from_ll();
            if (dest.signature.base == .reg32) {
                c.next_iw_xor1();
                c.next(atomic_load_high);
            } else {
                c.next(atomic_store);
            }
        }

        pub fn atomic_load_high(c: *Cycle, flags: Flags) void {
            if (flags.zero()) {
                c.exec_next_insn();
            } else {
                c.atomic_this_cycle();
                c.read_to_d(.temp_1, 2, .word, .data);
                c.d_to_ll();
                c.ll_to_reg();
                c.zn_flags_from_ll();
                c.next_iw_xor1();
                c.next(atomic_store);
            }
        }

        pub fn atomic_store(c: *Cycle, flags: Flags, dest: Param(2)) void {
            if (flags.zero()) {
                c.exec_next_insn();
            } else {
                c.atomic_this_cycle();
                c.reg_to_jl();
                c.literal_to_k(1);
                c.jl_plus_k_to_ll(.fresh, .flags);
                c.ll_to_reg();
                c.write_from_ll(.temp_1, 0, .word, .data);
                if (dest.signature.base == .reg32) {
                    c.next_ij_xor1();
                    c.next_iw_xor1();
                    c.next(atomic_store_high);
                } else {
                    c.exec_next_insn();
                }
            }
        }

        pub fn atomic_store_high(c: *Cycle) void {
            c.atomic_this_cycle();
            c.reg_to_jl();
            c.zero_to_k();
            c.jl_plus_k_to_ll(.cont, .flags);
            c.ll_to_reg();
            c.write_from_ll(.temp_1, 2, .word, .data);
            c.exec_next_insn();
        }
    },
    struct { // ax <src> -> .d x(mem) -> <dest>
        pub const spec =
            \\ax r(src, dest), .d x(mem)
            \\ax .d x(mem), r(src, dest)
            \\ax r(src) -> .d x(mem) -> r(dest)
            \\ax x(src, dest), .d x(mem)
            \\ax .d x(mem), x(src, dest)
            \\ax x(src) -> .d x(mem) -> x(dest)
            ;
        pub const encoding = .{
            Even_Reg(.mem),
            Encoder.init(3, @as(u3, 0)),
            atomic_width_encoder,
            Encoder.init(7, @as(u7, 0xa)),
            region_encoder,
            Encoder.init(16, Reg(.src)),
            Encoder.init(20, Int(.__, u4)),
            Encoder.init(24, Reg(.dest)),
            Encoder.init(28, @as(u4, 0)),
        };
        pub const ij = Reg(.mem);

        pub fn entry(c: *Cycle) void {
            c.reg32_to_l();
            c.l_to_sr(.temp_1);
            c.ip_read_to_dr(2, .full);
            c.next_ij_from_decode(.alt);
            c.next_iw_from_decode(.alt);
            c.next(load_next_insn);
        }

        pub fn load_next_insn(c: *Cycle) void {
            c.load_next_insn();
            // c.next_iw_from_ik();
            c.next(atomic_load);
        }

        pub fn atomic_load(c: *Cycle, dest: Param(.dest)) void {
            c.atomic_this_cycle();
            c.read_to_d(.temp_1, 0, .word, .data);
            c.d_to_ll();
            c.ll_to_reg();
            if (dest.signature.base == .reg32) {
                c.next_iw_xor1();
                c.next(atomic_load_high);
            } else {
                c.next(atomic_store);
            }
        }

        pub fn atomic_load_high(c: *Cycle) void {
            c.atomic_this_cycle();
            c.read_to_d(.temp_1, 2, .word, .data);
            c.d_to_ll();
            c.ll_to_reg();
            c.next_iw_xor1();
            c.next(atomic_store);
        }

        pub fn atomic_store(c: *Cycle, dest: Param(.dest)) void {
            c.atomic_this_cycle();
            c.reg_to_jl();
            c.jl_to_ll();
            c.write_from_ll(.temp_1, 0, .word, .data);
            if (dest.signature.base == .reg32) {
                c.next_ij_xor1();
                c.next(atomic_store_high);
            } else {
                c.exec_next_insn();
            }
        }

        pub fn atomic_store_high(c: *Cycle) void {
            c.atomic_this_cycle();
            c.reg_to_jl();
            c.jl_to_ll();
            c.write_from_ll(.temp_1, 2, .word, .data);
            c.exec_next_insn();
        }
    },
    struct { // axe <expected>, <src> -> .d x(mem) -> <dest>
        pub const spec = 
            \\axe r(expected), r(src, dest), .d x(mem)
            \\axe r(expected), .d x(mem), r(src, dest)
            \\axe r(expected), r(src) -> .d x(mem) -> r(dest)
            \\axe x(expected), x(src, dest), .d x(mem)
            \\axe x(expected), .d x(mem), x(src, dest)
            \\axe x(expected), x(src) -> .d x(mem) -> x(dest)
            ;
        pub const encoding = .{
            Even_Reg(.mem),
            Encoder.init(3, @as(u3, 1)),
            atomic_width_encoder,
            Encoder.init(7, @as(u7, 0xa)),
            region_encoder,
            Encoder.init(16, Reg(.src)),
            Encoder.init(20, Reg(.expected)),
            Encoder.init(24, Reg(.dest)),
            Encoder.init(28, @as(u4, 0)),
        };
        pub const ij = Reg(.mem);

        pub fn entry(c: *Cycle) void {
            c.reg32_to_l();
            c.l_to_sr(.temp_1);
            c.ip_read_to_dr(2, .full);
            c.next_ij_from_decode(.alt);
            c.next_ik_from_decode(.alt);
            c.next_iw_from_decode(.alt);
            c.next(atomic_load);
        }

        pub fn atomic_load(c: *Cycle) void {
            c.atomic_this_cycle();
            c.read_to_d(.temp_1, 0, .word, .data);
            c.d_to_l(.zx);
            c.l_to_sr(.temp_2);
            c.next(compare);
        }

        pub fn compare(c: *Cycle, src: Param(.src)) void {
            c.atomic_this_cycle();
            c.srl_to_jl(.temp_2);
            c.reg_to_k();
            c.jl_minus_k(.fresh, .flags);
            if (src.signature.base == .reg32) {
                c.next(atomic_load_high);
            } else {
                c.next(atomic_store);
            }
        }

        pub fn atomic_load_high(c: *Cycle, flags: Flags) void {
            if (flags.zero()) {
                c.atomic_this_cycle();
                c.read_to_d(.temp_1, 2, .word, .data);
                c.d_to_l(.zx);
                c.l_to_sr(.temp_2);
                c.next_ik_xor1();
                c.next(compare_high);
            } else {
                c.load_and_exec_next_insn();
            }
        }

        pub fn compare_high(c: *Cycle) void {
            c.atomic_this_cycle();
            c.srl_to_jl(.temp_2);
            c.reg_to_k();
            c.jl_minus_k(.fresh, .flags);
            c.next(atomic_store);
        }

        pub fn atomic_store(c: *Cycle, flags: Flags, src: Param(.src)) void {
            if (flags.zero()) {
                c.atomic_this_cycle();
                c.reg_to_jl();
                c.jl_to_ll();
                c.write_from_ll(.temp_1, 0, .word, .data);
                if (src.signature.base == .reg32) {
                    c.next_ij_xor1();
                    c.next(atomic_store_high);
                } else {
                    c.next(load_and_exec_next_insn);
                }
            } else {
                c.load_and_exec_next_insn();
            }
        }

        pub fn atomic_store_high(c: *Cycle) void {
            c.atomic_this_cycle();
            c.reg_to_jl();
            c.jl_to_ll();
            c.write_from_ll(.temp_1, 2, .word, .data);
            c.next(load_and_exec_next_insn);
        }

        pub const load_and_exec_next_insn = Cycle.load_and_exec_next_insn;
    },
};

fn atomic_width_encoder(params: []const Parameter.Signature) Encoder {
    for (params) |param| {
        if (param.address_space != null) continue;
        return Encoder.init(6, @as(u1, switch (param.base) {
            .reg16 => 0,
            .reg32 => 1,
            else => continue,
        }));
    }
    @panic("Expected reg16 or reg32 parameter");
}


const Cycle = @import("../compile/Cycle.zig");
const Encoder = isa.Instruction_Encoding.Encoder;
const Parameter = isa.Parameter;
const Options = placeholders.Options;
const Range = placeholders.Range;
const Int = placeholders.Int;
const Int_Mult = placeholders.Int_Mult;
const Reg = placeholders.Reg;
const Reg_Bit = placeholders.Reg_Bit;
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
