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
            opcodes.Lo8.branch_imm,
            Encoder.shifted_offset(8, opcodes.Hi8.branch_4.value(), Imm),
        };
        const Imm = Range(.imm, 4, 63);
        pub fn entry(c: *Cycle, imm: Imm) void {
            c.branch(.ip, @intCast(imm.value));
        }
    },
    struct { pub const spec = "b .i ip + (imm)";
        pub const encoding = .{
            opcodes.Lo8.branch_imm,
            Encoder.shifted_offset(8, opcodes.Hi8.branch_n64.value(), Imm),
        };
        const Imm = Range(.imm, -64, -1);
        pub fn entry(c: *Cycle, imm: Imm) void {
            c.branch(.ip, @intCast(imm.value));
        }
    },
    struct { pub const spec = "b .i ip + (imm)";
        pub const encoding = .{
            opcodes.Lo8.branch_imm_63_318,
            Encoder.shifted(8, Imm),
        };
        pub const constraints = .{
            .{ .imm, .greater_or_equal, 64 },
        };
        const Imm = Range(.imm, 63, 63 + 255);
        pub const ik_ij = Imm;

        pub fn entry(c: *Cycle) void {
            c.sr_to_j(.ip);
            c.ik_ij_zx_to_k();
            c.j_plus_k_to_l(.zx, .fresh, .no_flags);
            c.l_to_sr(.next_ip);
            c.next(branch);
        }

        pub fn branch(c: *Cycle) void {
            c.branch(.next_ip, 63);
        }
    },
    struct { pub const spec = "b .i ip + (imm)";
        pub const encoding = .{
            opcodes.Lo8.branch_imm_n64_n319,
            Encoder.shifted(8, Imm),
        };
        pub const constraints = .{
            .{ .imm, .less_or_equal, -65 },
        };
        const Imm = Range(.imm, -64, -64 - 255);
        pub const ik_ij = Imm;

        pub fn entry(c: *Cycle) void {
            c.sr_to_j(.ip);
            c.ik_ij_zx_to_k();
            c.j_minus_k_to_l(.zx, .fresh, .no_flags);
            c.l_to_sr(.next_ip);
            c.next(branch);
        }

        pub fn branch(c: *Cycle) void {
            c.branch(.next_ip, -64);
        }
    },
    struct { pub const spec = "b .i ip + (imm)";
        pub const encoding = .{
            opcodes.Lo16.branch_imm16u,
            Encoder.shifted(16, Int(.imm, u16)),
        };

        pub fn entry(c: *Cycle) void {
            c.ip_read_to_d(2, .word);
            c.d_to_l(.zx);
            c.l_to_sr(.temp_1);
            c.next(compute_next_ip);
        }

        pub fn compute_next_ip(c: *Cycle) void {
            c.sr_to_j(.ip);
            c.srl_to_k(.temp_1);
            c.j_plus_k_to_l(.zx, .fresh, .no_flags);
            c.l_to_sr(.next_ip);
            c.next(branch);
        }

        pub fn branch(c: *Cycle) void {
            c.branch(.next_ip, 0);
        }
    },
    struct { pub const spec = "b .i ip + (imm)";
        pub const encoding = .{
            opcodes.Lo16.branch_imm16n,
            Encoder.shifted(16, Range(.imm, -0x10000, -1)),
        };

        pub fn entry(c: *Cycle) void {
            c.ip_read_to_d(2, .word);
            c.d_to_l(.zx);
            c.l_to_sr(.temp_1);
            c.next(compute_next_ip);
        }

        pub fn compute_next_ip(c: *Cycle) void {
            c.sr_to_j(.ip);
            c.srl_to_k(.temp_1);
            c.j_plus_k_to_l(._1x, .fresh, .no_flags);
            c.l_to_sr(.next_ip);
            c.next(branch);
        }

        pub fn branch(c: *Cycle) void {
            c.branch(.next_ip, 0);
        }
    },
    struct { pub const spec = //b .i ip + r(reg)
        \\b .i ip + r(reg) .unsigned
        \\b .i ip + r(reg) .signed
        ;
        pub const encoding = .{
            opcode,
            Encoder.shifted(12, Reg(.reg)),
        };
        pub const ik = Reg(.reg);

        fn opcode(params: []const Parameter.Signature) opcodes.Lo12 {
            return switch (params[0].offset.reg16.?) {
                .unsigned => .branch_reg16u,
                .signed => .branch_reg16s,
            };
        }

        pub fn entry(c: *Cycle, params: []const Parameter.Signature) void {
            c.sr_to_j(.ip);
            c.reg_to_k();
            c.j_plus_k_to_l(switch (params[0].offset.reg16.?) {
                .unsigned => .zx,
                .signed => .sx,
            }, .fresh, .no_flags);
            c.l_to_sr(.next_ip);
            c.next(branch);
        }

        pub fn branch(c: *Cycle) void {
            c.branch(.next_ip, 0);
        }
    },
    struct { pub const spec = "b .i x(reg)";
        pub const encoding = .{
            opcodes.Lo12.branch_abs_reg32,
            Encoder.shifted(12, Reg(.reg)),
        };
        pub const ij = Reg(.reg);

        pub fn entry(c: *Cycle) void {
            c.reg32_to_l();
            c.l_to_sr(.next_ip);
            c.next(branch);
        }

        pub fn branch(c: *Cycle) void {
            c.branch(.next_ip, 0);
        }
    },
    struct { pub const spec = "b .i (imm)";
        pub const encoding = .{
            opcodes.Lo16.branch_abs_imm32,
            Encoder.shifted(16, Int(.imm, u32)),
        };

        pub fn entry(c: *Cycle) void {
            c.ip_read_to_d(4, .word);
            c.d_to_l(.zx);
            c.l_to_sr(.temp_1);
            c.next(load_imm_low);
        }

        pub fn load_imm_low(c: *Cycle) void {
            c.ip_read_to_d(2, .word);
            c.d_to_ll();
            c.srl_to_lh(.temp_1);
            c.l_to_sr(.next_ip);
            c.next(branch);
        }

        pub fn branch(c: *Cycle) void {
            c.branch(.next_ip, 0);
        }
    },
    struct { pub const spec = // b.[n]z .i ip + (imm_3_63)
        \\b.z .i ip + (imm)
        \\b.nz .i ip + (imm)
        ;
        pub const encoding = .{
            opcodes.Lo8.branch_z_nz_imm,
            opcode_hi8,
        };
        const Imm = Range(.imm, 3, 63);

        fn opcode_hi8(suffix: isa.Mnemonic_Suffix) Encoder {
            return Encoder.shifted_offset(8, switch (suffix) {
                .z => opcodes.Hi8.branch_z_3.value(),
                .nz => opcodes.Hi8.branch_nz_3.value(),
                else => unreachable,
            }, Imm);
        }

        pub fn entry(c: *Cycle, flags: Flags, suffix: isa.Mnemonic_Suffix, imm: Imm) void {
            if (check_condition(suffix, flags)) {
                c.branch(.ip, @intCast(imm.value));
            } else {
                c.load_and_exec_next_insn_no_atomic_end();
            }
        }
    },
    struct { pub const spec = // b.[n]z .i ip + (imm_n64_n1)
        \\b.z .i ip + (imm)
        \\b.nz .i ip + (imm)
        ;
        pub const encoding = .{
            opcodes.Lo8.branch_z_nz_imm,
            opcode_hi8,
        };
        const Imm = Range(.imm, -64, -1);

        fn opcode_hi8(suffix: isa.Mnemonic_Suffix) Encoder {
            return Encoder.shifted_offset(8, switch (suffix) {
                .z => opcodes.Hi8.branch_z_n64.value(),
                .nz => opcodes.Hi8.branch_nz_n64.value(),
                else => unreachable,
            }, Imm);
        }

        pub fn entry(c: *Cycle, flags: Flags, suffix: isa.Mnemonic_Suffix, imm: Imm) void {
            if (check_condition(suffix, flags)) {
                c.branch(.ip, @intCast(imm.value));
            } else {
                c.load_and_exec_next_insn_no_atomic_end();
            }
        }
    },
    struct { pub const spec = // b.<cond> .i ip + (imm_n6_12)
        \\b.lu .i ip + (imm)
        \\b.nlu .i ip + (imm)
        \\b.gu .i ip + (imm)
        \\b.ngu .i ip + (imm)
        \\b.n .i ip + (imm)
        \\b.nn .i ip + (imm)
        \\b.c .i ip + (imm)
        \\b.nc .i ip + (imm)
        \\b.ls .i ip + (imm)
        \\b.nls .i ip + (imm)
        \\b.gs .i ip + (imm)
        \\b.ngs .i ip + (imm)
        \\b.p .i ip + (imm)
        \\b.np .i ip + (imm)
        ;

        pub const encoding = .{
            opcodes.Lo8.branch_conditional_imm_n6_12,
            Encoder.shifted(8, Imm),
            condition_encoder,
        };
        const Imm = Options(.imm, .{ -6, -5, -4, -3, -2, -1, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 });

        pub fn entry(c: *Cycle, flags: Flags, suffix: isa.Mnemonic_Suffix, imm: Imm) void {
            if (check_condition(suffix, flags)) {
                c.branch(.ip, @intCast(imm.value));
            } else {
                c.load_and_exec_next_insn_no_atomic_end();
            }
        }
    },
    struct { pub const spec = // b.<cond> .i ip + (imm_13_268)
        \\b.z .i ip + (imm)
        \\b.nz .i ip + (imm)
        \\b.lu .i ip + (imm)
        \\b.nlu .i ip + (imm)
        \\b.gu .i ip + (imm)
        \\b.ngu .i ip + (imm)
        \\b.n .i ip + (imm)
        \\b.nn .i ip + (imm)
        \\b.c .i ip + (imm)
        \\b.nc .i ip + (imm)
        \\b.ls .i ip + (imm)
        \\b.nls .i ip + (imm)
        \\b.gs .i ip + (imm)
        \\b.ngs .i ip + (imm)
        \\b.p .i ip + (imm)
        \\b.np .i ip + (imm)
        ;

        pub const encoding = .{
            opcodes.Lo12.branch_conditional_imm_13_268,
            condition_encoder,
            Encoder.shifted(16, Range(.imm, 13, 13 + 255)),
        };

        pub fn entry(c: *Cycle, flags: Flags, suffix: isa.Mnemonic_Suffix) void {
            if (check_condition(suffix, flags)) {
                c.ip_read_to_d(2, .byte);
                c.d_to_l(.zx);
                c.l_to_sr(.temp_1);
                c.next(compute_next_ip);
            } else {
                c.load_and_exec_next_insn_no_atomic_end();
            }
        }

        pub fn compute_next_ip(c: *Cycle) void {
            c.sr_to_j(.ip);
            c.srl_to_k(.temp_1);
            c.j_plus_k_to_l(.zx, .fresh, .no_flags);
            c.l_to_sr(.next_ip);
            c.next(branch);
        }

        pub fn branch(c: *Cycle) void {
            c.branch(.next_ip, 15);
        }
    },
    struct { pub const spec = // b.<cond> .i ip + (imm_n15_270)
        \\b.z .i ip + (imm)
        \\b.nz .i ip + (imm)
        \\b.lu .i ip + (imm)
        \\b.nlu .i ip + (imm)
        \\b.gu .i ip + (imm)
        \\b.ngu .i ip + (imm)
        \\b.n .i ip + (imm)
        \\b.nn .i ip + (imm)
        \\b.c .i ip + (imm)
        \\b.nc .i ip + (imm)
        \\b.ls .i ip + (imm)
        \\b.nls .i ip + (imm)
        \\b.gs .i ip + (imm)
        \\b.ngs .i ip + (imm)
        \\b.p .i ip + (imm)
        \\b.np .i ip + (imm)
        ;

        pub const encoding = .{
            opcodes.Lo12.branch_conditional_imm_n262_n7,
            condition_encoder,
            Encoder.shifted(16, Range(.imm, -7, -7 - 255)),
        };

        pub fn entry(c: *Cycle, flags: Flags, suffix: isa.Mnemonic_Suffix) void {
            if (check_condition(suffix, flags)) {
                c.ip_read_to_d(2, .byte);
                c.d_to_l(.zx);
                c.l_to_sr(.temp_1);
                c.next(compute_next_ip);
            } else {
                c.load_and_exec_next_insn_no_atomic_end();
            }
        }

        pub fn compute_next_ip(c: *Cycle) void {
            c.sr_to_j(.ip);
            c.srl_to_k(.temp_1);
            c.j_minus_k_to_l(.zx, .fresh, .no_flags);
            c.l_to_sr(.next_ip);
            c.next(branch);
        }

        pub fn branch(c: *Cycle) void {
            c.branch(.next_ip, -7);
        }
    },
    struct { pub const spec = // b.<cond> .i ip + (imm_s16)
        \\b.z .i ip + (imm)
        \\b.nz .i ip + (imm)
        \\b.lu .i ip + (imm)
        \\b.nlu .i ip + (imm)
        \\b.gu .i ip + (imm)
        \\b.ngu .i ip + (imm)
        \\b.n .i ip + (imm)
        \\b.nn .i ip + (imm)
        \\b.c .i ip + (imm)
        \\b.nc .i ip + (imm)
        \\b.ls .i ip + (imm)
        \\b.nls .i ip + (imm)
        \\b.gs .i ip + (imm)
        \\b.ngs .i ip + (imm)
        \\b.p .i ip + (imm)
        \\b.np .i ip + (imm)
        ;
        pub const encoding = .{
            opcodes.Lo12.branch_conditional_s16,
            condition_encoder,
            Encoder.shifted(16, Int(.imm, i16)),
        };

        pub fn entry(c: *Cycle, flags: Flags, suffix: isa.Mnemonic_Suffix) void {
            if (check_condition(suffix, flags)) {
                c.ip_read_to_d(2, .word);
                c.d_to_l(.sx);
                c.l_to_sr(.temp_1);
                c.next(compute_next_ip);
            } else {
                c.load_and_exec_next_insn_no_atomic_end();
            }
        }

        pub fn compute_next_ip(c: *Cycle) void {
            c.sr_to_j(.ip);
            c.srl_to_k(.temp_1);
            c.j_plus_k_to_l(.sx, .fresh, .no_flags);
            c.l_to_sr(.next_ip);
            c.next(branch);
        }

        pub fn branch(c: *Cycle) void {
            c.branch(.next_ip, 0);
        }
    },
    struct { pub const spec = "bp";
        pub const encoding = .{
            opcodes.Lo16.branch_to_start_of_current_page,
        };
        pub const ij: u4 = 0xF;
        pub const ik: u4 = 0xF;
        pub const iw: u4 = 0xF;

        pub fn entry(c: *Cycle) void {
            c.sr_to_j(.ip);
            c.iw_ik_ij_zx_to_k();
            c.jl_logic_k_to_ll(.and_not, .fresh, .no_flags);
            c.jh_to_lh();
            c.l_to_sr(.next_ip);
            c.next(branch);
        }

        pub fn branch(c: *Cycle) void {
            c.branch(.next_ip, 0);
        }
    },
    struct { pub const spec = "bpn";
        pub const encoding = .{
            opcodes.Lo16.branch_to_start_of_next_page,
        };
        pub const ij: u4 = 0xF;
        pub const ik: u4 = 0xF;
        pub const iw: u4 = 0xF;

        pub fn entry(c: *Cycle) void {
            c.sr_to_j(.ip);
            c.iw_ik_ij_zx_to_k();
            c.jl_logic_k_to_ll(._or, .fresh, .no_flags);
            c.jh_to_lh();
            c.l_to_sr(.next_ip);
            c.next(branch);
        }

        pub fn branch(c: *Cycle) void {
            c.branch(.next_ip, 1);
        }
    },
    struct { pub const spec = "bb";
        pub const encoding = .{
            opcodes.Lo16.branch_to_start_of_current_block,
        };
        pub const ij: u4 = 0xF;
        pub const ik: u4 = 0xF;

        pub fn entry(c: *Cycle) void {
            c.sr_to_j(.ip);
            c.ik_ij_zx_to_k();
            c.jl_logic_k_to_ll(.and_not, .fresh, .no_flags);
            c.jh_to_lh();
            c.l_to_sr(.next_ip);
            c.next(branch);
        }

        pub fn branch(c: *Cycle) void {
            c.branch(.next_ip, 0);
        }
    },
    struct { pub const spec = "bbn";
        pub const encoding = .{
            opcodes.Lo16.branch_to_start_of_next_block,
        };
        pub const ij: u4 = 0xF;
        pub const ik: u4 = 0xF;

        pub fn entry(c: *Cycle) void {
            c.sr_to_j(.ip);
            c.ik_ij_zx_to_k();
            c.jl_logic_k_to_ll(._or, .fresh, .no_flags);
            c.jh_to_lh();
            c.l_to_sr(.next_ip);
            c.next(branch);
        }

        pub fn branch(c: *Cycle) void {
            c.branch(.next_ip, 1);
        }
    },
    struct { pub const spec = // b.<cond>.<cond> .i ip + (imm_s16), .i ip + (imm_s16)
        \\b.lu.gu .i ip + (imm1), .i ip + (imm2)
        \\b.lu.z .i ip + (imm1), .i ip + (imm2)
        \\b.gu.z .i ip + (imm1), .i ip + (imm2)
        \\b.ls.gs .i ip + (imm1), .i ip + (imm2)
        \\b.ls.z .i ip + (imm1), .i ip + (imm2)
        \\b.gs.z .i ip + (imm1), .i ip + (imm2)
        \\b.n.z .i ip + (imm1), .i ip + (imm2)
        \\b.p.z .i ip + (imm1), .i ip + (imm2)
        \\b.n.p .i ip + (imm1), .i ip + (imm2)
        ;
        pub const encoding = .{
            opcodes.Lo12.branch_double_conditional,
            condition_encoder,
            Encoder.shifted(16, Int(.imm1, i16)),
            Encoder.shifted(32, Int(.imm2, i16)),
        };

        pub fn entry(c: *Cycle, flags: Flags, suffix: isa.Mnemonic_Suffix) void {
            if (check_condition(first_condition(suffix), flags)) {
                c.ip_read_to_d(2, .word);
                c.d_to_l(.sx);
                c.l_to_sr(.temp_1);
                c.next(compute_next_ip);
            } else if (check_condition(second_condition(suffix), flags)) {
                c.ip_read_to_d(4, .word);
                c.d_to_l(.sx);
                c.l_to_sr(.temp_1);
                c.next(compute_next_ip);
            } else {
                c.load_and_exec_next_insn_no_atomic_end();
            }
        }

        pub fn compute_next_ip(c: *Cycle) void {
            c.sr_to_j(.ip);
            c.srl_to_k(.temp_1);
            c.j_plus_k_to_l(.sx, .fresh, .no_flags);
            c.l_to_sr(.next_ip);
            c.next(branch);
        }

        pub fn branch(c: *Cycle) void {
            c.branch(.next_ip, 0);
        }
    },
    struct { pub const spec = //eab/dab .i x0
        \\eab .i x0
        \\dab .i x0
        ;
        pub const encoding = .{
            opcode,
        };
        pub const ij: u4 = 0;

        fn opcode(mnemonic: isa.Mnemonic) opcodes.Lo16 {
            return switch (mnemonic) {
                .eab => .enable_translation_and_branch,
                .dab => .disable_translation_and_branch,
                else => unreachable,
            };
        }

        pub fn entry(c: *Cycle, flags: Flags, mnemonic: isa.Mnemonic) void {
            if (!flags.kernel()) return c.illegal_instruction();

            c.reg32_to_l();
            c.l_to_sr(.next_ip);
            switch (mnemonic) {
                .eab => c.enable_address_translation(),
                .dab => c.disable_address_translation(),
                else => unreachable,
            }
            c.next(branch);
        }

        pub fn branch(c: *Cycle) void {
            c.branch(.next_ip, 0);
        }
    },
};

fn condition_encoder(condition: isa.Mnemonic_Suffix) Encoder {
    const index: u4 = switch (condition) {
        .z => 0,
        .nz => 1,
        .lu => 2,
        .nlu => 3,
        .gu => 4,
        .ngu => 5,
        .n => 6,
        .nn => 7,
        .c => 8,
        .nc => 9,
        .ls => 10,
        .nls => 11,
        .gs => 12,
        .ngs => 13,
        .p => 14,
        .np => 15,

        .lu_gu => 0,
        .lu_z => 1,
        .gu_z => 2,
        .ls_gs => 4,
        .ls_z => 5,
        .gs_z => 6,
        .n_z => 8,
        .n_p => 9,
        .p_z => 10,
        else => unreachable,
    };
    return Encoder.shifted(12, index);
}

fn check_condition(condition: isa.Mnemonic_Suffix, flags: Flags) bool {
    return switch (condition) {
        .z => flags.zero(),
        .nz => !flags.zero(),
        .lu => flags.unsigned_less_than(),
        .nlu => !flags.unsigned_less_than(),
        .gu => flags.unsigned_greater_than(),
        .ngu => !flags.unsigned_greater_than(),
        .n => flags.negative(),
        .nn => !flags.negative(),
        .c => flags.carry(),
        .nc => !flags.carry(),
        .ls => flags.signed_less_than(),
        .nls => !flags.signed_less_than(),
        .gs => flags.signed_greater_than(),
        .ngs => !flags.signed_greater_than(),
        .p => flags.positive(),
        .np => !flags.positive(),
        else => unreachable,
    };
}

fn first_condition(condition: isa.Mnemonic_Suffix) isa.Mnemonic_Suffix {
    return switch (condition) {
        .lu_gu, .lu_z => return .lu,
        .gu_z => return .gu,
        .ls_gs, .ls_z => return .ls,
        .gs_z => return .gs,
        .n_z, .n_p => return .n,
        .p_z => return .p,
        else => unreachable,
    };
}

fn second_condition(condition: isa.Mnemonic_Suffix) isa.Mnemonic_Suffix {
    return switch (condition) {
        .lu_z, .gu_z, .ls_z, .gs_z, .n_z, .p_z => return .z,
        .lu_gu => return .gu,
        .ls_gs => return .gs,
        .n_p => return .p,
        else => unreachable,
    };
}

const Cycle = @import("Cycle.zig");
const Encoder = isa.Instruction_Encoding.Encoder;
const Instruction_Signature = isa.Instruction_Signature;
const Parameter = isa.Parameter;
const Options = placeholders.Options;
const Range = placeholders.Range;
const Int = placeholders.Int;
const Reg = placeholders.Reg;
const placeholders = @import("placeholders.zig");
const opcodes = @import("opcodes.zig");
const Flags = arch.hw.microcode.Flags;
const isa = arch.isa;
const arch = @import("lib_arch");
