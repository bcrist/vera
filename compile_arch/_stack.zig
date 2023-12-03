pub const instructions = .{
    struct { pub const spec = // [un]frame r0 .unsigned
        \\unframe r0 .unsigned
        \\frame r0 .unsigned
        ;
        pub const encoding = .{
            opcode,
        };
        pub const ik: u4 = 0;

        fn opcode(mnemonic: isa.Mnemonic) opcodes.Lo16 {
            return switch (mnemonic) {
                .frame => .subtract_sp_r0u,
                .unframe => .add_sp_r0u,
                else => unreachable,
            };
        }

        pub fn entry(c: *Cycle, mnemonic: isa.Mnemonic) void {
            c.sr_to_j(.sp);
            c.reg_to_k();
            switch (mnemonic) {
                .frame => c.j_minus_k_to_l(.zx, .fresh, .no_flags),
                .unframe => c.j_plus_k_to_l(.zx, .fresh, .no_flags),
                else => unreachable,
            }
            c.l_to_sr(.sp);
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = // [un]frame (imm)
        \\unframe (imm)
        \\frame (imm)
        ;
        pub const encoding = .{
            opcode,
            Encoder.shifted(8, Int(.imm, u8)),
        };
        pub const ik_ij = Int(.imm, u8);

        fn opcode(mnemonic: isa.Mnemonic) opcodes.Lo8 {
            return switch (mnemonic) {
                .frame => .subtract_sp_u8,
                .unframe => .add_sp_u8,
                else => unreachable,
            };
        }

        pub fn entry(c: *Cycle, mnemonic: isa.Mnemonic) void {
            c.sr_to_j(.sp);
            c.ik_ij_zx_to_k();
            switch (mnemonic) {
                .frame => c.j_minus_k_to_l(.zx, .fresh, .no_flags),
                .unframe => c.j_plus_k_to_l(.zx, .fresh, .no_flags),
                else => unreachable,
            }
            c.l_to_sr(.sp);
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = // [un]frame (imm)
        \\unframe (imm)
        \\frame (imm)
        ;
        pub const encoding = .{
            opcode,
            Encoder.shifted(16, Int(.imm, u16)),
        };

        fn opcode(mnemonic: isa.Mnemonic) opcodes.Lo16 {
            return switch (mnemonic) {
                .frame => .subtract_sp_u16,
                .unframe => .add_sp_u16,
                else => unreachable,
            };
        }

        pub fn entry(c: *Cycle) void {
            c.ip_read_to_d(2, .word);
            c.d_to_l(.zx);
            c.l_to_sr(.temp_2);
            c.next(modify_sp);
        }

        pub fn modify_sp(c: *Cycle, mnemonic: isa.Mnemonic) void {
            c.sr_to_j(.sp);
            c.srl_to_k(.temp_2);
            switch (mnemonic) {
                .frame => c.j_minus_k_to_l(.zx, .fresh, .no_flags),
                .unframe => c.j_plus_k_to_l(.zx, .fresh, .no_flags),
                else => unreachable,
            }
            c.l_to_sr(.sp);
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = // pop <reg>
        \\pop b(reg) .signed
        \\pop b(reg) .unsigned
        \\pop r(reg)
        \\pop x(reg)
        ;
        pub const encoding = .{
            opcode,
            Encoder.shifted(12, Reg(.reg)),
        };
        pub const iw = Reg(.reg);

        fn opcode(params: []const Parameter.Signature) opcodes.Lo12 {
            return switch (params[0].base) {
                .reg8 => |signedness| switch (signedness.?) {
                    .unsigned => .pop_reg8u,
                    .signed => .pop_reg8s,
                },
                .reg16 => .pop_reg16,
                .reg32 => .pop_reg32,
                else => unreachable,
            };
        }

        pub fn entry(c: *Cycle, params: []const Parameter.Signature) void {
            c.read_to_d(.sp, 0, switch (params[0].base) {
                .reg8 => .byte,
                .reg16, .reg32 => .word,
                else => unreachable,
            }, .stack);
            c.d_to_l(switch (params[0].base) {
                .reg8 => |signedness| switch (signedness.?) {
                    .unsigned => .zx,
                    .signed => .sx,
                },
                .reg16, .reg32 => .zx,
                else => unreachable,
            });
            c.ll_to_reg();
            if (params[0].base == .reg32) c.next_iw_xor1();
            c.next(if (params[0].base == .reg32) pop_high else modify_sp);
        }

        pub fn pop_high(c: *Cycle) void {
            c.read_to_d(.sp, 2, .word, .stack);
            c.d_to_ll();
            c.ll_to_reg();
            c.next(modify_sp);
        }

        pub fn modify_sp(c: *Cycle, params: []const Parameter.Signature) void {
            c.sr_to_j(.sp);
            c.literal_to_k(switch (params[0].base) {
                .reg8 => 1,
                .reg16 => 2,
                .reg32 => 4,
                else => unreachable,
            });
            c.j_plus_k_to_l(.zx, .fresh, .no_flags);
            c.l_to_sr(.sp);
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = // push <reg>
        \\push b(reg)
        \\push r(reg)
        \\push x(reg)
        ;
        pub const encoding = .{
            opcode,
            Encoder.shifted(12, Reg(.reg)),
        };
        pub const ij = Reg(.reg);

        fn opcode(params: []const Parameter.Signature) opcodes.Lo12 {
            return switch (params[0].base) {
                .reg8 => .push_reg8,
                .reg16 => .push_reg16,
                .reg32 => .push_reg32,
                else => unreachable,
            };
        }

        pub fn entry(c: *Cycle, params: []const Parameter.Signature) void {
            c.reg_to_jl();
            c.jl_to_ll();
            c.write_from_ll(.sp, switch (params[0].base) {
                .reg8 => -1,
                .reg16 => -2,
                .reg32 => -4,
                else => unreachable,
            }, switch (params[0].base) {
                .reg8 => .byte,
                .reg16, .reg32 => .word,
                else => unreachable,
            }, .stack);
            if (params[0].base == .reg32) c.next_ij_xor1();
            c.next(if (params[0].base == .reg32) push_high else modify_sp);
        }

        pub fn push_high(c: *Cycle) void {
            c.reg_to_jl();
            c.jl_to_ll();
            c.write_from_ll(.sp, -2, .word, .stack);
            c.next(modify_sp);
        }

        pub fn modify_sp(c: *Cycle, params: []const Parameter.Signature) void {
            c.sr_to_j(.sp);
            c.literal_to_k(switch (params[0].base) {
                .reg8 => 1,
                .reg16 => 2,
                .reg32 => 4,
                else => unreachable,
            });
            c.j_minus_k_to_l(.zx, .fresh, .no_flags);
            c.l_to_sr(.sp);
            c.load_and_exec_next_insn();
        }
    },
};

const Cycle = @import("Cycle.zig");
const Encoder = isa.Instruction_Encoding.Encoder;
const Instruction_Signature = isa.Instruction_Signature;
const Parameter = isa.Parameter;
const Int = placeholders.Int;
const Range = placeholders.Range;
const Reg = placeholders.Reg;
const placeholders = @import("placeholders.zig");
const opcodes = @import("opcodes.zig");
const isa = arch.isa;
const arch = @import("lib_arch");
