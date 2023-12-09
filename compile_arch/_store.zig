pub const instructions = .{
    struct { pub const spec = // st <reg> -> <space> x(dest)
        \\st b(src) -> .s x(dest)
        \\st r(src) -> .s x(dest)
        \\st x(src) -> .s x(dest)
        \\st b(src) -> .d x(dest)
        \\st r(src) -> .d x(dest)
        \\st x(src) -> .d x(dest)
        ;
        pub const encoding = .{
            opcode,
            Encoder.shifted(8, Reg(.src)),
            Encoder.shifted(12, Reg(.dest)),
        };
        pub const ij = Reg(.dest);
        pub const ik = Reg(.src);

        fn opcode(params: []const Parameter.Signature) opcodes.Lo8 {
            return switch (params[2].address_space.?) {
                .data => switch (params[0].base) {
                    .reg8  => .store_data_reg32_reg8,
                    .reg16 => .store_data_reg32_reg16,
                    .reg32 => .store_data_reg32_reg32,
                    else => unreachable,
                },
                .stack => switch (params[0].base) {
                    .reg8  => .store_stack_reg32_reg8,
                    .reg16 => .store_stack_reg32_reg16,
                    .reg32 => .store_stack_reg32_reg32,
                    else => unreachable,
                },
                else => unreachable,
            };
        }

        pub fn entry(c: *Cycle) void {
            c.reg32_to_l();
            c.l_to_sr(.temp_1);
            c.load_next_insn();
            c.next(store_from_temp_1.store);
        }

        pub usingnamespace store_from_temp_1;
    },
    struct { pub const spec = // st <reg> -> .d x(base) + r(offset) .unsigned
        \\st b0 -> .d x(base) + r(offset) .unsigned
        \\st r0 -> .d x(base) + r(offset) .unsigned
        \\st x0 -> .d x(base) + r(offset) .unsigned
        ;
        pub const encoding = .{
            opcode,
            Encoder.shifted(8, Reg(.base)),
            Encoder.shifted(12, Reg(.offset)),
        };
        pub const ij = Reg(.base);
        pub const ik = Reg(.offset);

        fn opcode(params: []const Parameter.Signature) opcodes.Lo8 {
            return switch (params[0].base) {
                .reg8  => .store_data_reg32_plus_reg16_reg8,
                .reg16 => .store_data_reg32_plus_reg16_reg16,
                .reg32 => .store_data_reg32_plus_reg16_reg32,
                else => unreachable,
            };
        }

        pub fn entry(c: *Cycle) void {
            c.reg32_to_j();
            c.reg_to_k();
            c.j_plus_k_to_l(.zx, .fresh, .no_flags);
            c.l_to_sr(.temp_1);
            c.load_next_insn();
            c.next(store_from_temp_1.store);
        }

        pub usingnamespace store_from_temp_1;
    },
    struct { pub const spec = // st b0 -> .d x(base) + (imm4u)
        \\st b0 -> .d x(base) + (imm)
        ;
        pub const encoding = .{
            opcodes.Lo8.store_data_reg32_plus_u4_reg8,
            Encoder.shifted(8, Reg(.base)),
            Encoder.shifted(12, Imm),
        };
        const Imm = Int(.imm, u4);
        pub const ij = Reg(.base);
        pub const ik: u4 = 0;

        pub fn entry(c: *Cycle, imm: Imm) void {
            c.reg32_to_j();
            c.literal_to_k(@intCast(imm.value));
            c.j_plus_k_to_l(.zx, .fresh, .no_flags);
            c.l_to_sr(.temp_1);
            c.load_next_insn();
            c.next(store_from_temp_1.store);
        }

        pub usingnamespace store_from_temp_1;
    },
    struct { pub const spec = // st r0 -> .d x(base) + (imm4u_m2)
        \\st r0 -> .d x(base) + (imm)
        ;
        pub const encoding = .{
            opcodes.Lo8.store_data_reg32_plus_u4m2_reg16,
            Encoder.shifted(8, Reg(.base)),
            Encoder.shifted(12, Imm),
        };
        const Imm = Int_Mult(.imm, u4, 2);
        pub const ij = Reg(.base);
        pub const ik: u4 = 0;

        pub fn entry(c: *Cycle, imm: Imm) void {
            c.reg32_to_j();
            c.literal_to_k(@intCast(imm.value));
            c.j_plus_k_to_l(.zx, .fresh, .no_flags);
            c.l_to_sr(.temp_1);
            c.load_next_insn();
            c.next(store_from_temp_1.store);
        }

        pub usingnamespace store_from_temp_1;
    },
    struct { pub const spec = // st x0 -> .d x(base) + (imm4u_m4)
        \\st x0 -> .d x(base) + (imm)
        ;
        pub const encoding = .{
            opcodes.Lo8.store_data_reg32_plus_u4m4_reg32,
            Encoder.shifted(8, Reg(.base)),
            Encoder.shifted(12, Imm),
        };
        const Imm = Int_Mult(.imm, u4, 4);
        pub const ij = Reg(.base);
        pub const ik: u4 = 0;

        pub fn entry(c: *Cycle, imm: Imm) void {
            c.reg32_to_j();
            c.literal_to_k(@intCast(@divExact(imm.value, 2)));
            c.j_plus_k_to_l(.zx, .fresh, .no_flags);
            c.l_to_sr(.temp_1);
            c.load_next_insn();
            c.next(store_from_temp_1.store_with_half_offset);
        }

        pub usingnamespace store_from_temp_1;
    },
    struct { pub const spec = // st b(src) -> .s sp + (imm4u)
        \\st b(src) -> .s sp + (imm)
        ;
        pub const encoding = .{
            opcodes.Lo8.store_stack_plus_u4_reg8,
            Encoder.shifted(8, Reg(.src)),
            Encoder.shifted(12, Imm),
        };
        const Imm = Int(.imm, u4);
        pub const ik_ij = Imm;
        pub const iw = Reg(.src);

        pub fn entry(c: *Cycle) void {
            c.sr_to_j(.sp);
            c.ik_ij_zx_to_k();
            c.j_plus_k_to_l(.zx, .fresh, .no_flags);
            c.l_to_sr(.temp_1);
            c.load_next_insn();
            c.next_ik_from_iw();
            c.next(store_from_temp_1.store);
        }

        pub usingnamespace store_from_temp_1;
    },
    struct { pub const spec = // st r(src) -> .s sp + (imm4u_m2)
        \\st r(src) -> .s sp + (imm)
        ;
        pub const encoding = .{
            opcodes.Lo8.store_stack_plus_u4m2_reg16,
            Encoder.shifted(8, Reg(.src)),
            Encoder.shifted(12, Imm),
        };
        const Imm = Int_Mult(.imm, u4, 2);
        pub const ik_ij = Encoder.shifted(1, Imm);
        pub const iw = Reg(.src);

        pub fn entry(c: *Cycle) void {
            c.sr_to_j(.sp);
            c.ik_ij_zx_to_k();
            c.j_plus_k_to_l(.zx, .fresh, .no_flags);
            c.l_to_sr(.temp_1);
            c.load_next_insn();
            c.next_ik_from_iw();
            c.next(store_from_temp_1.store);
        }

        pub usingnamespace store_from_temp_1;
    },
    struct { pub const spec = // st x(src) -> .s sp + (imm4u_m4)
        \\st x(src) -> .s sp + (imm)
        ;
        pub const encoding = .{
            opcodes.Lo8.store_stack_plus_u4m4_reg32,
            Encoder.shifted(8, Reg(.src)),
            Encoder.shifted(12, Imm),
        };
        const Imm = Int_Mult(.imm, u4, 4);
        pub const ik_ij = Encoder.shifted(2, Imm);
        pub const iw = Reg(.src);

        pub fn entry(c: *Cycle) void {
            c.sr_to_j(.sp);
            c.ik_ij_zx_to_k();
            c.j_plus_k_to_l(.zx, .fresh, .no_flags);
            c.l_to_sr(.temp_1);
            c.load_next_insn();
            c.next_ik_from_iw();
            c.next(store_from_temp_1.store);
        }

        pub usingnamespace store_from_temp_1;
    },
    struct { pub const spec = // st <reg> -> .s sp + (imm16s)
        \\st b(src) -> .s sp + (imm)
        \\st r(src) -> .s sp + (imm)
        \\st x(src) -> .s sp + (imm)
        ;
        pub const encoding = .{
            opcode,
            Encoder.shifted(12, Reg(.src)),
            Encoder.shifted(16, Int(.imm, i16)),
        };
        pub const ik = Reg(.src);

        fn opcode(params: []const Parameter.Signature) opcodes.Lo12 {
            return switch (params[0].base) {
                .reg8  => .store_stack_plus_s16_reg8u,
                .reg16 => .store_stack_plus_s16_reg16,
                .reg32 => .store_stack_plus_s16_reg32,
                else => unreachable,
            };
        }

        pub fn entry(c: *Cycle) void {
            c.ip_read_to_d(2, .word);
            c.d_to_l(.sx);
            c.l_to_sr(.temp_2);
            c.next(compute_addr);
        }

        pub fn compute_addr(c: *Cycle) void {
            c.sr_to_j(.sp);
            c.srl_to_k(.temp_2);
            c.j_plus_k_to_l(.sx, .fresh, .no_flags);
            c.l_to_sr(.temp_1);
            c.load_next_insn();
            c.next(store_from_temp_1.store);
        }

        pub usingnamespace store_from_temp_1;
    },
    struct { pub const spec = // st <reg> -> .s sp + r(offset) .signed
        \\st b(src) -> .s sp + r(offset) .signed
        \\st r(src) -> .s sp + r(offset) .signed
        \\st x(src) -> .s sp + r(offset) .signed
        ;
        pub const encoding = .{
            opcode,
            Encoder.shifted(8, Reg(.src)),
            Encoder.shifted(12, Reg(.offset)),
        };
        pub const ij = Reg(.src);
        pub const ik = Reg(.offset);

        fn opcode(params: []const Parameter.Signature) opcodes.Lo8 {
            return switch (params[0].base) {
                .reg8  => .store_stack_plus_reg16s_reg8u,
                .reg16 => .store_stack_plus_reg16s_reg16,
                .reg32 => .store_stack_plus_reg16s_reg32,
                else => unreachable,
            };
        }

        pub fn entry(c: *Cycle) void {
            c.sr_to_j(.sp);
            c.reg_to_k();
            c.j_plus_k_to_l(.sx, .fresh, .no_flags);
            c.l_to_sr(.temp_1);
            c.load_next_insn();
            c.next_ik_from_ij();
            c.next(store_from_temp_1.store);
        }

        pub usingnamespace store_from_temp_1;
    },
    struct { pub const spec = // [i]st[i] <reg> -> .d x(dest)
        \\sti b(src) -> .d x(dest)
        \\sti r(src) -> .d x(dest)
        \\sti x(src) -> .d x(dest)
        \\ist b(src) -> .d x(dest)
        \\ist r(src) -> .d x(dest)
        \\ist x(src) -> .d x(dest)
        ;
        pub const encoding = .{
            opcode,
            Encoder.shifted(8, Reg(.src)),
            Encoder.shifted(12, Reg(.dest)),
        };
        pub const ij = Reg(.dest);
        pub const ik = Reg(.src);
        pub const iw = Reg(.dest);

        fn opcode(mnemonic: isa.Mnemonic, params: []const Parameter.Signature) opcodes.Lo8 {
            return switch (mnemonic) {
                .sti => switch (params[0].base) {
                    .reg8  => .store_and_increment_data_reg32_reg8,
                    .reg16 => .store_and_increment_data_reg32_reg16,
                    .reg32 => .store_and_increment_data_reg32_reg32,
                    else => unreachable,
                },
                .ist => switch (params[0].base) {
                    .reg8  => .increment_and_store_data_reg32_reg8,
                    .reg16 => .increment_and_store_data_reg32_reg16,
                    .reg32 => .increment_and_store_data_reg32_reg32,
                    else => unreachable,
                },
                else => unreachable,
            };
        }

        pub fn entry(c: *Cycle, mnemonic: isa.Mnemonic, params: []const Parameter.Signature) void {
            c.reg32_to_j();
            c.literal_to_k(switch (params[0].base) {
                .reg8 => 1,
                .reg16 => 2,
                .reg32 => 4,
                else => unreachable,
            });
            c.j_plus_k_to_l(.zx, .fresh, .no_flags);
            c.l_to_reg32();
            c.l_to_sr(.temp_1);
            c.load_next_insn();
            if (mnemonic == .sti) {
                c.next(store_from_temp_1.store_before_increment);
            } else {
                c.next(store_from_temp_1.store);
            }
        }

        pub usingnamespace store_from_temp_1;
    },
};

const store_from_temp_1 = struct {
    pub fn store(c: *Cycle, params: []const Parameter.Signature) void {
        const addr_space: Control_Signals.Address_Space = switch(params[2].address_space.?) {
            .data => .data,
            .insn => .insn,
            .stack => .stack,
        };

        c.reg_to_k();
        c.k_to_ll();
        c.write_from_ll(.temp_1, 0, switch (params[0].base) {
            .reg8 => .byte,
            .reg16, .reg32 => .word,
            else => unreachable,
        }, addr_space);
        if (params[0].base == .reg32) {
            c.next_ik_xor1();
            c.next(store_high);
        } else {
            c.exec_next_insn();
        }
    }

    pub fn store_with_half_offset(c: *Cycle, params: []const Parameter.Signature, imm: placeholders.Any(.imm)) void {
        const addr_space: Control_Signals.Address_Space = switch(params[2].address_space.?) {
            .data => .data,
            .insn => .insn,
            .stack => .stack,
        };

        c.reg_to_k();
        c.k_to_ll();
        c.write_from_ll(.temp_1, @intCast(@divExact(imm.value, 2)), switch (params[0].base) {
            .reg8 => .byte,
            .reg16, .reg32 => .word,
            else => unreachable,
        }, addr_space);
        if (params[0].base == .reg32) {
            c.virtual_address_to_sr(.temp_1);
            c.next_ik_xor1();
            c.next(store_high);
        } else {
            c.exec_next_insn();
        }
    }

    pub fn store_before_increment(c: *Cycle, params: []const Parameter.Signature) void {
        const addr_space: Control_Signals.Address_Space = switch(params[2].address_space.?) {
            .data => .data,
            .insn => .insn,
            .stack => .stack,
        };

        c.reg_to_k();
        c.k_to_ll();
        switch (params[0].base) {
            .reg8 => c.write_from_ll(.temp_1, -1, .byte, addr_space),
            .reg16 => c.write_from_ll(.temp_1, -2, .word, addr_space),
            .reg32 => c.write_from_ll(.temp_1, -4, .word, addr_space),
            else => unreachable,
        }
        if (params[0].base == .reg32) {
            c.virtual_address_to_sr(.temp_1);
            c.next_ik_xor1();
            c.next(store_high);
        } else {
            c.exec_next_insn();
        }
    }

    pub fn store_high(c: *Cycle, params: []const Parameter.Signature) void {
        const addr_space: Control_Signals.Address_Space = switch(params[2].address_space.?) {
            .data => .data,
            .insn => .insn,
            .stack => .stack,
        };

        c.reg_to_k();
        c.k_to_ll();
        c.write_from_ll(.temp_1, 2, .word, addr_space);
        c.exec_next_insn();
    }
};

const Cycle = @import("Cycle.zig");
const Encoder = isa.Instruction_Encoding.Encoder;
const Parameter = isa.Parameter;
const Int = placeholders.Int;
const Int_Mult = placeholders.Int_Mult;
const Range = placeholders.Range;
const Reg = placeholders.Reg;
const placeholders = @import("placeholders.zig");
const opcodes = @import("opcodes.zig");
const Control_Signals = arch.hw.Control_Signals;
const isa = arch.isa;
const arch = @import("lib_arch");
