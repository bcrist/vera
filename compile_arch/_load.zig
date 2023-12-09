pub const instructions = .{
    struct { pub const spec = // ld <space> x(src) -> <reg>
        \\ld .i x(src) -> b(dest) .unsigned
        \\ld .i x(src) -> b(dest) .signed
        \\ld .i x(src) -> r(dest)
        \\ld .i x(src) -> x(dest)
        \\ld .s x(src) -> b(dest) .unsigned
        \\ld .s x(src) -> b(dest) .signed
        \\ld .s x(src) -> r(dest)
        \\ld .s x(src) -> x(dest)
        \\ld .d x(src) -> b(dest) .unsigned
        \\ld .d x(src) -> b(dest) .signed
        \\ld .d x(src) -> r(dest)
        \\ld .d x(src) -> x(dest)
        ;
        pub const encoding = .{
            opcode,
            Encoder.shifted(8, Reg(.src)),
            Encoder.shifted(12, Reg(.dest)),
        };
        pub const ij = Reg(.src);
        pub const iw = Reg(.dest);

        fn opcode(params: []const Parameter.Signature) opcodes.Lo8 {
            return switch (params[0].address_space.?) {
                .data => switch (params[2].base) {
                    .reg8 => |signedness| switch (signedness.?) {
                        .unsigned => .load_data_reg32_reg8u,
                        .signed => .load_data_reg32_reg8s,
                    },
                    .reg16 => .load_data_reg32_reg16,
                    .reg32 => .load_data_reg32_reg32,
                    else => unreachable,
                },
                .insn => switch (params[2].base) {
                    .reg8 => |signedness| switch (signedness.?) {
                        .unsigned => .load_insn_reg32_reg8u,
                        .signed => .load_insn_reg32_reg8s,
                    },
                    .reg16 => .load_insn_reg32_reg16,
                    .reg32 => .load_insn_reg32_reg32,
                    else => unreachable,
                },
                .stack => switch (params[2].base) {
                    .reg8 => |signedness| switch (signedness.?) {
                        .unsigned => .load_stack_reg32_reg8u,
                        .signed => .load_stack_reg32_reg8s,
                    },
                    .reg16 => .load_stack_reg32_reg16,
                    .reg32 => .load_stack_reg32_reg32,
                    else => unreachable,
                },
            };
        }

        pub fn entry(c: *Cycle) void {
            c.reg32_to_l();
            c.l_to_sr(.temp_1);
            c.load_next_insn();
            c.next(load_from_temp_1.load);
        }

        pub usingnamespace load_from_temp_1;
    },
    struct { pub const spec = // ld .d x(base) + r(offset) .unsigned -> <reg>
        \\ld .d x(base) + r(offset) .unsigned -> b0 .unsigned
        \\ld .d x(base) + r(offset) .unsigned -> b0 .signed
        \\ld .d x(base) + r(offset) .unsigned -> r0
        \\ld .d x(base) + r(offset) .unsigned -> x0
        ;
        pub const encoding = .{
            opcode,
            Encoder.shifted(8, Reg(.base)),
            Encoder.shifted(12, Reg(.offset)),
        };
        pub const ij = Reg(.base);
        pub const ik = Reg(.offset);
        pub const iw: u4 = 0;

        fn opcode(params: []const Parameter.Signature) opcodes.Lo8 {
            return switch (params[2].base) {
                .reg8 => |signedness| switch (signedness.?) {
                    .unsigned => .load_data_reg32_plus_reg16_reg8u,
                    .signed => .load_data_reg32_plus_reg16_reg8s,
                },
                .reg16 => .load_data_reg32_plus_reg16_reg16,
                .reg32 => .load_data_reg32_plus_reg16_reg32,
                else => unreachable,
            };
        }

        pub fn entry(c: *Cycle) void {
            c.reg32_to_j();
            c.reg_to_k();
            c.j_plus_k_to_l(.zx, .fresh, .no_flags);
            c.l_to_sr(.temp_1);
            c.load_next_insn();
            c.next(load_from_temp_1.load);
        }

        pub usingnamespace load_from_temp_1;
    },
    struct { pub const spec = // ld .d x(base) + (imm4u) -> b0
        \\ld .d x(base) + (imm) -> b0 .unsigned
        \\ld .d x(base) + (imm) -> b0 .signed
        ;
        pub const encoding = .{
            opcode,
            Encoder.shifted(8, Reg(.base)),
            Encoder.shifted(12, Imm),
        };
        const Imm = Int(.imm, u4);
        pub const ij = Reg(.base);
        pub const iw: u4 = 0;

        fn opcode(params: []const Parameter.Signature) opcodes.Lo8 {
            return switch (params[2].base.reg8.?) {
                .unsigned => .load_data_reg32_plus_u4_reg8u,
                .signed => .load_data_reg32_plus_u4_reg8s,
            };
        }

        pub fn entry(c: *Cycle, imm: Imm) void {
            c.reg32_to_j();
            c.literal_to_k(@intCast(imm.value));
            c.j_plus_k_to_l(.zx, .fresh, .no_flags);
            c.l_to_sr(.temp_1);
            c.load_next_insn();
            c.next(load_from_temp_1.load);
        }

        pub usingnamespace load_from_temp_1;
    },
    struct { pub const spec = // ld .d x(base) + (imm4u_m2) -> r0
        \\ld .d x(base) + (imm) -> r0
        ;
        pub const encoding = .{
            opcodes.Lo8.load_data_reg32_plus_u4m2_reg16,
            Encoder.shifted(8, Reg(.base)),
            Encoder.shifted(12, Imm),
        };
        const Imm = Int_Mult(.imm, u4, 2);
        pub const ij = Reg(.base);
        pub const iw: u4 = 0;

        pub fn entry(c: *Cycle, imm: Imm) void {
            c.reg32_to_j();
            c.literal_to_k(@intCast(imm.value));
            c.j_plus_k_to_l(.zx, .fresh, .no_flags);
            c.l_to_sr(.temp_1);
            c.load_next_insn();
            c.next(load_from_temp_1.load);
        }

        pub usingnamespace load_from_temp_1;
    },
    struct { pub const spec = // ld .d x(base) + (imm4u_m4) -> x0
        \\ld .d x(base) + (imm) -> x0
        ;
        pub const encoding = .{
            opcodes.Lo8.load_data_reg32_plus_u4m4_reg32,
            Encoder.shifted(8, Reg(.base)),
            Encoder.shifted(12, Imm),
        };
        const Imm = Int_Mult(.imm, u4, 4);
        pub const ij = Reg(.base);
        pub const iw: u4 = 0;

        pub fn entry(c: *Cycle, imm: Imm) void {
            c.reg32_to_j();
            c.literal_to_k(@intCast(@divExact(imm.value, 2)));
            c.j_plus_k_to_l(.zx, .fresh, .no_flags);
            c.l_to_sr(.temp_1);
            c.load_next_insn();
            c.next(load_from_temp_1.load_with_half_offset);
        }

        pub usingnamespace load_from_temp_1;
    },
    struct { pub const spec = // ld .s sp + (imm4u) -> b(dest)
        \\ld .s sp + (imm) -> b(dest) .unsigned
        \\ld .s sp + (imm) -> b(dest) .signed
        ;
        pub const encoding = .{
            opcode,
            Encoder.shifted(8, Reg(.dest)),
            Encoder.shifted(12, Int(.imm, u4)),
        };
        pub const ij = Int(.imm, u4);
        pub const ik: u4 = 0;
        pub const iw = Reg(.dest);

        fn opcode(params: []const Parameter.Signature) opcodes.Lo8 {
            return switch (params[2].base.reg8.?) {
                .unsigned => .load_stack_plus_u4_reg8u,
                .signed => .load_stack_plus_u4_reg8s,
            };
        }

        pub fn entry(c: *Cycle) void {
            c.sr_to_j(.sp);
            c.ik_ij_zx_to_k();
            c.j_plus_k_to_l(.zx, .fresh, .no_flags);
            c.l_to_sr(.temp_1);
            c.load_next_insn();
            c.next(load_from_temp_1.load);
        }

        pub usingnamespace load_from_temp_1;
    },
    struct { pub const spec = // ld .s sp + (imm4u_m2) -> r(dest)
        \\ld .s sp + (imm) -> r(dest)
        ;
        pub const encoding = .{
            opcodes.Lo8.load_stack_plus_u4m2_reg16,
            Encoder.shifted(8, Reg(.dest)),
            Encoder.shifted(12, Imm),
        };
        const Imm = Int_Mult(.imm, u4, 2);
        pub const ik_ij = Encoder.shifted(1, Imm);
        pub const iw = Reg(.dest);

        pub fn entry(c: *Cycle) void {
            c.sr_to_j(.sp);
            c.ik_ij_zx_to_k();
            c.j_plus_k_to_l(.zx, .fresh, .no_flags);
            c.l_to_sr(.temp_1);
            c.load_next_insn();
            c.next(load_from_temp_1.load);
        }

        pub usingnamespace load_from_temp_1;
    },
    struct { pub const spec = // ld .s sp + (imm4u_m4) -> x(dest)
        \\ld .s sp + (imm) -> x(dest)
        ;
        pub const encoding = .{
            opcodes.Lo8.load_stack_plus_u4m4_reg32,
            Encoder.shifted(8, Reg(.dest)),
            Encoder.shifted(12, Imm),
        };
        const Imm = Int_Mult(.imm, u4, 4);
        pub const ik_ij = Encoder.shifted(2, Imm);
        pub const iw = Reg(.dest);

        pub fn entry(c: *Cycle) void {
            c.sr_to_j(.sp);
            c.ik_ij_zx_to_k();
            c.j_plus_k_to_l(.zx, .fresh, .no_flags);
            c.l_to_sr(.temp_1);
            c.load_next_insn();
            c.next(load_from_temp_1.load);
        }

        pub usingnamespace load_from_temp_1;
    },
    struct { pub const spec = // ld .s sp + (imm16s) -> <reg>
        \\ld .s sp + (imm) -> b(dest) .unsigned
        \\ld .s sp + (imm) -> b(dest) .signed
        \\ld .s sp + (imm) -> r(dest)
        \\ld .s sp + (imm) -> x(dest)
        ;
        pub const encoding = .{
            opcode,
            Encoder.shifted(12, Reg(.dest)),
            Encoder.shifted(16, Int(.imm, i16)),
        };
        pub const iw = Reg(.dest);

        fn opcode(params: []const Parameter.Signature) opcodes.Lo12 {
            return switch (params[2].base) {
                .reg8 => |signedness| switch (signedness.?) {
                    .unsigned => .load_stack_plus_s16_reg8u,
                    .signed => .load_stack_plus_s16_reg8s,
                },
                .reg16 => .load_stack_plus_s16_reg16,
                .reg32 => .load_stack_plus_s16_reg32,
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
            c.next(load_from_temp_1.load);
        }

        pub usingnamespace load_from_temp_1;
    },
    struct { pub const spec = // ld .s sp + r(offset) .signed -> <reg>
        \\ld .s sp + r(offset) .signed -> b(dest) .unsigned
        \\ld .s sp + r(offset) .signed -> b(dest) .signed
        \\ld .s sp + r(offset) .signed -> r(dest)
        \\ld .s sp + r(offset) .signed -> x(dest)
        ;
        pub const encoding = .{
            opcode,
            Encoder.shifted(8, Reg(.dest)),
            Encoder.shifted(12, Reg(.offset)),
        };
        pub const ik = Reg(.offset);
        pub const iw = Reg(.dest);

        fn opcode(params: []const Parameter.Signature) opcodes.Lo8 {
            return switch (params[2].base) {
                .reg8 => |signedness| switch (signedness.?) {
                    .unsigned => .load_stack_plus_reg16s_reg8u,
                    .signed => .load_stack_plus_reg16s_reg8s,
                },
                .reg16 => .load_stack_plus_reg16s_reg16,
                .reg32 => .load_stack_plus_reg16s_reg32,
                else => unreachable,
            };
        }

        pub fn entry(c: *Cycle) void {
            c.sr_to_j(.sp);
            c.reg_to_k();
            c.j_plus_k_to_l(.sx, .fresh, .no_flags);
            c.l_to_sr(.temp_1);
            c.load_next_insn();
            c.next(load_from_temp_1.load);
        }

        pub usingnamespace load_from_temp_1;
    },
    struct { pub const spec = // ld .d <uxp/kxp> + (imm4u_m2) -> r(dest)
        \\ld .d uxp + (imm) -> r(dest)
        \\ld .d kxp + (imm) -> r(dest)
        ;
        pub const encoding = .{
            opcode,
            Encoder.shifted(8, Reg(.dest)),
            Encoder.shifted(12, Imm),
        };
        const Imm = Int_Mult(.imm, u4, 2);
        pub const ik_ij = Encoder.shifted(1, Imm);
        pub const iw = Reg(.dest);

        fn opcode(params: []const Parameter.Signature) opcodes.Lo8 {
            return switch (params[0].base.sr) {
                .uxp => .load_data_uxp_plus_u4m2_reg16,
                .kxp => .load_data_kxp_plus_u4m2_reg16,
                else => unreachable,
            };
        }

        pub fn entry(c: *Cycle, params: []const Parameter.Signature) void {
            c.sr_to_j(switch (params[0].base.sr) {
                .uxp => .uxp,
                .kxp => .kxp,
                else => unreachable,
            });
            c.ik_ij_zx_to_k();
            c.j_plus_k_to_l(.zx, .fresh, .no_flags);
            c.l_to_sr(.temp_1);
            c.load_next_insn();
            c.next(load_from_temp_1.load);
        }

        pub usingnamespace load_from_temp_1;
    },
    struct { pub const spec = // ld .d <uxp/kxp> + (imm4u_m4) -> x(dest)
        \\ld .d uxp + (imm) -> x(dest)
        \\ld .d kxp + (imm) -> x(dest)
        ;
        pub const encoding = .{
            opcode,
            Encoder.shifted(8, Reg(.dest)),
            Encoder.shifted(12, Imm),
        };
        const Imm = Int_Mult(.imm, u4, 4);
        pub const ik_ij = Encoder.shifted(2, Imm);
        pub const iw = Reg(.dest);

        fn opcode(params: []const Parameter.Signature) opcodes.Lo8 {
            return switch (params[0].base.sr) {
                .uxp => .load_data_uxp_plus_u4m4_reg32,
                .kxp => .load_data_kxp_plus_u4m4_reg32,
                else => unreachable,
            };
        }

        pub fn entry(c: *Cycle, params: []const Parameter.Signature) void {
            c.sr_to_j(switch (params[0].base.sr) {
                .uxp => .uxp,
                .kxp => .kxp,
                else => unreachable,
            });
            c.ik_ij_zx_to_k();
            c.j_plus_k_to_l(.zx, .fresh, .no_flags);
            c.l_to_sr(.temp_1);
            c.load_next_insn();
            c.next(load_from_temp_1.load);
        }

        pub usingnamespace load_from_temp_1;
    },
    struct { pub const spec = // [i]ld[i] .d x(src) -> <reg>
        \\ldi .d x(src) -> b(dest) .unsigned
        \\ldi .d x(src) -> b(dest) .signed
        \\ldi .d x(src) -> r(dest)
        \\ldi .d x(src) -> x(dest)
        \\ild .d x(src) -> b(dest) .unsigned
        \\ild .d x(src) -> b(dest) .signed
        \\ild .d x(src) -> r(dest)
        \\ild .d x(src) -> x(dest)
        ;
        pub const encoding = .{
            opcode,
            Encoder.shifted(8, Reg(.src)),
            Encoder.shifted(12, Reg(.dest)),
        };
        pub const ij = Reg(.src);
        pub const ik = Reg(.dest);
        pub const iw = Reg(.src);

        fn opcode(mnemonic: isa.Mnemonic, params: []const Parameter.Signature) opcodes.Lo8 {
            return switch (mnemonic) {
                .ldi => switch (params[2].base) {
                    .reg8 => |signedness| switch (signedness.?) {
                        .unsigned => .load_and_increment_data_reg32_reg8u,
                        .signed => .load_and_increment_data_reg32_reg8s,
                    },
                    .reg16 => .load_and_increment_data_reg32_reg16,
                    .reg32 => .load_and_increment_data_reg32_reg32,
                    else => unreachable,
                },
                .ild => switch (params[2].base) {
                    .reg8 => |signedness| switch (signedness.?) {
                        .unsigned => .increment_and_load_data_reg32_reg8u,
                        .signed => .increment_and_load_data_reg32_reg8s,
                    },
                    .reg16 => .increment_and_load_data_reg32_reg16,
                    .reg32 => .increment_and_load_data_reg32_reg32,
                    else => unreachable,
                },
                else => unreachable,
            };
        }

        pub fn entry(c: *Cycle, mnemonic: isa.Mnemonic, params: []const Parameter.Signature) void {
            c.reg32_to_j();
            c.literal_to_k(switch (params[2].base) {
                .reg8 => 1,
                .reg16 => 2,
                .reg32 => 4,
                else => unreachable,
            });
            c.j_plus_k_to_l(.zx, .fresh, .no_flags);
            c.l_to_reg32();
            c.l_to_sr(.temp_1);
            c.next_iw_from_ik();
            c.load_next_insn();
            if (mnemonic == .ldi) {
                c.next(load_from_temp_1.load_before_increment);
            } else {
                c.next(load_from_temp_1.load);
            }
        }

        pub usingnamespace load_from_temp_1;
    },
};


const load_from_temp_1 = struct {
    pub fn load(c: *Cycle, params: []const Parameter.Signature) void {
        const addr_space: Control_Signals.Address_Space = switch(params[0].address_space.?) {
            .data => .data,
            .insn => .insn,
            .stack => .stack,
        };

        switch (params[2].base) {
            .reg8 => |signedness| {
                c.read_to_d(.temp_1, 0, .byte, addr_space);
                c.d_to_l(switch (signedness.?) {
                    .unsigned => .zx,
                    .signed => .sx,
                });
            },
            .reg16, .reg32 => {
                c.read_to_d(.temp_1, 0, .word, addr_space);
                c.d_to_l(.zx);
            },
            else => unreachable,
        }
        c.ll_to_reg();
        if (params[2].base == .reg32) {
            c.next_iw_xor1();
            c.next(load_high);
        } else {
            c.exec_next_insn();
        }
    }

    pub fn load_with_half_offset(c: *Cycle, params: []const Parameter.Signature, imm: placeholders.Any(.imm)) void {
        const addr_space: Control_Signals.Address_Space = switch(params[0].address_space.?) {
            .data => .data,
            .insn => .insn,
            .stack => .stack,
        };

        const offset = @divExact(imm.value, 2);
        switch (params[2].base) {
            .reg8 => |signedness| {
                c.read_to_d(.temp_1, @intCast(offset), .byte, addr_space);
                c.d_to_l(switch (signedness.?) {
                    .unsigned => .zx,
                    .signed => .sx,
                });
                c.ll_to_reg();
                c.exec_next_insn();
            },
            .reg16 => {
                c.read_to_d(.temp_1, @intCast(offset), .word, addr_space);
                c.d_to_l(.zx);
                c.ll_to_reg();
                c.exec_next_insn();
            },
            .reg32 => {
                c.read_to_d(.temp_1, @intCast(offset), .word, addr_space);
                c.virtual_address_to_sr(.temp_1);
                c.d_to_l(.zx);
                c.ll_to_reg();
                c.next_iw_xor1();
                c.next(load_high);
            },
            else => unreachable,
        }
    }

    pub fn load_before_increment(c: *Cycle, params: []const Parameter.Signature) void {
        const addr_space: Control_Signals.Address_Space = switch(params[0].address_space.?) {
            .data => .data,
            .insn => .insn,
            .stack => .stack,
        };

        switch (params[2].base) {
            .reg8 => |signedness| {
                c.read_to_d(.temp_1, -1, .byte, addr_space);
                c.d_to_l(switch (signedness.?) {
                    .unsigned => .zx,
                    .signed => .sx,
                });
                c.ll_to_reg();
                c.exec_next_insn();
            },
            .reg16 => {
                c.read_to_d(.temp_1, -2, .word, addr_space);
                c.d_to_l(.zx);
                c.ll_to_reg();
                c.exec_next_insn();
            },
            .reg32 => {
                c.read_to_d(.temp_1, -4, .word, addr_space);
                c.virtual_address_to_sr(.temp_1);
                c.d_to_l(.zx);
                c.ll_to_reg();
                c.next_iw_xor1();
                c.next(load_high);
            },
            else => unreachable,
        }
    }

    pub fn load_high(c: *Cycle, params: []const Parameter.Signature) void {
        const addr_space: Control_Signals.Address_Space = switch(params[0].address_space.?) {
            .data => .data,
            .insn => .insn,
            .stack => .stack,
        };

        c.read_to_d(.temp_1, 2, .word, addr_space);
        c.d_to_l(.zx);
        c.ll_to_reg();
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
