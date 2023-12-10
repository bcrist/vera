pub const instructions = .{
    struct { pub const spec = "sync";
        pub const encoding = .{
            opcodes.Lo16.sync,
        };

        pub fn entry(c: *Cycle) void {
            c.atomic_next_cycle_until_end();
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = // ald .d x(src) -> <reg>
        \\ald .d x(src) -> r(dest)
        \\ald .d x(src) -> x(dest)
        ;
        pub const encoding = .{
            opcode,
            Encoder.shifted(8, Reg(.src)),
            Encoder.shifted(12, Reg(.dest)),
        };
        pub const ij = Reg(.src);
        pub const iw = Reg(.dest);

        fn opcode(params: []const Parameter.Signature) opcodes.Lo8 {
            return switch (params[2].base) {
                .reg16 => .atomic_load_reg16,
                .reg32 => .atomic_load_reg32,
                else => unreachable,
            };
        }

        pub fn entry(c: *Cycle) void {
            c.reg32_to_l();
            c.l_to_sr(.temp_1);
            c.load_next_insn();
            c.next(atomic_load);
        }

        pub fn atomic_load(c: *Cycle, params: []const Parameter.Signature) void {
            c.atomic_this_cycle();
            c.read_to_d(.temp_1, 0, .word, .data);
            c.d_to_ll();
            c.ll_to_reg();
            if (params[2].base == .reg32) {
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
    struct { pub const spec = // ast <reg> -> .d x(dest)
        \\ast r(src) -> .d x(dest)
        \\ast x(src) -> .d x(dest)
        ;
        pub const encoding = .{
            opcode,
            Encoder.shifted(8, Reg(.dest)),
            Encoder.shifted(12, Reg(.src)),
        };
        pub const ij = Reg(.dest);
        pub const ik = Reg(.src);

        fn opcode(params: []const Parameter.Signature) opcodes.Lo8 {
            return switch (params[0].base) {
                .reg16 => .atomic_store_reg16,
                .reg32 => .atomic_store_reg32,
                else => unreachable,
            };
        }

        pub fn entry(c: *Cycle) void {
            c.reg32_to_l();
            c.l_to_sr(.temp_1);
            c.load_next_insn();
            c.next(atomic_store);
        }

        pub fn atomic_store(c: *Cycle, params: []const Parameter.Signature) void {
            c.atomic_this_cycle();
            c.reg_to_k();
            c.k_to_ll();
            c.write_from_ll(.temp_1, 0, .word, .data);
            if (params[0].base == .reg32) {
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
    struct { pub const spec = // astz <reg> -> .d x(dest)
        \\astz r(src) -> .d x(dest)
        \\astz x(src) -> .d x(dest)
        ;
        pub const encoding = .{
            opcode,
            Encoder.shifted(8, Reg(.dest)),
            Encoder.shifted(12, Reg(.src)),
        };
        pub const ij = Reg(.dest);
        pub const ik = Reg(.src);

        fn opcode(params: []const Parameter.Signature) opcodes.Lo8 {
            return switch (params[0].base) {
                .reg16 => .atomic_store_if_zero_reg16,
                .reg32 => .atomic_store_if_zero_reg32,
                else => unreachable,
            };
        }

        pub fn entry(c: *Cycle) void {
            c.reg32_to_l();
            c.l_to_sr(.temp_1);
            c.load_next_insn();
            c.next(atomic_check);
        }

        pub fn atomic_check(c: *Cycle, params: []const Parameter.Signature) void {
            c.atomic_this_cycle();
            c.read_to_d(.temp_1, 0, .word, .data);
            c.d_to_ll();
            c.zn_flags_from_ll();
            if (params[0].base == .reg32) {
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

        pub fn atomic_store(c: *Cycle, flags: Flags, params: []const Parameter.Signature) void {
            if (flags.zero()) {
                c.atomic_this_cycle();
                c.reg_to_k();
                c.k_to_ll();
                c.write_from_ll(.temp_1, 0, .word, .data);
                if (params[0].base == .reg32) {
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
    struct { pub const spec = // aadd .d x(mem), <reg> -> <reg0>
        \\aadd .d x(mem), r(offset) -> r0
        \\aadd .d x(mem), x(offset) -> x0
        ;
        pub const encoding = .{
            opcode,
            Encoder.shifted(8, Reg(.mem)),
            Encoder.shifted(12, Reg(.offset)),
        };
        pub const ij = Reg(.mem);
        pub const ik = Reg(.offset);
        pub const iw: u4 = 0;

        fn opcode(params: []const Parameter.Signature) opcodes.Lo8 {
            return switch (params[1].base) {
                .reg16 => .atomic_add_reg16,
                .reg32 => .atomic_add_reg32,
                else => unreachable,
            };
        }

        pub fn entry(c: *Cycle) void {
            c.reg32_to_l();
            c.l_to_sr(.temp_1);
            c.load_next_insn();
            c.next_ij_from_iw();
            c.next(atomic_load);
        }

        pub fn atomic_load(c: *Cycle, params: []const Parameter.Signature) void {
            c.atomic_this_cycle();
            c.read_to_d(.temp_1, 0, .word, .data);
            c.d_to_ll();
            c.ll_to_reg();
            if (params[1].base == .reg32) {
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

        pub fn atomic_store(c: *Cycle, params: []const Parameter.Signature) void {
            c.atomic_this_cycle();
            c.reg_to_jl();
            c.reg_to_k();
            c.jl_plus_k_to_ll(.fresh, .flags);
            c.ll_to_reg();
            c.write_from_ll(.temp_1, 0, .word, .data);
            if (params[1].base == .reg32) {
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
    struct { pub const spec = // ainc .d x(mem) -> <reg0>
        \\ainc .d x(mem) -> r0
        \\ainc .d x(mem) -> x0
        ;
        pub const encoding = .{
            opcode,
            Encoder.shifted(12, Reg(.mem)),
        };
        pub const ij = Reg(.mem);
        pub const iw: u4 = 0;

        fn opcode(params: []const Parameter.Signature) opcodes.Lo12 {
            return switch (params[2].base) {
                .reg16 => .atomic_increment_reg16,
                .reg32 => .atomic_increment_reg32,
                else => unreachable,
            };
        }

        pub fn entry(c: *Cycle) void {
            c.reg32_to_l();
            c.l_to_sr(.temp_1);
            c.load_next_insn();
            c.next_ij_from_iw();
            c.next(atomic_load);
        }

        pub fn atomic_load(c: *Cycle, params: []const Parameter.Signature) void {
            c.atomic_this_cycle();
            c.read_to_d(.temp_1, 0, .word, .data);
            c.d_to_ll();
            c.ll_to_reg();
            if (params[2].base == .reg32) {
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

        pub fn atomic_store(c: *Cycle, params: []const Parameter.Signature) void {
            c.atomic_this_cycle();
            c.reg_to_jl();
            c.literal_to_k(1);
            c.jl_plus_k_to_ll(.fresh, .flags);
            c.ll_to_reg();
            c.write_from_ll(.temp_1, 0, .word, .data);
            if (params[2].base == .reg32) {
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
    struct { pub const spec = // adecnz .d x(mem) -> <reg0>
        \\adecnz .d x(mem) -> r0
        \\adecnz .d x(mem) -> x0
        ;
        pub const encoding = .{
            opcode,
            Encoder.shifted(12, Reg(.mem)),
        };
        pub const ij = Reg(.mem);
        pub const iw: u4 = 0;

        fn opcode(params: []const Parameter.Signature) opcodes.Lo12 {
            return switch (params[2].base) {
                .reg16 => .atomic_decrement_if_not_zero_reg16,
                .reg32 => .atomic_decrement_if_not_zero_reg32,
                else => unreachable,
            };
        }

        pub fn entry(c: *Cycle) void {
            c.reg32_to_l();
            c.l_to_sr(.temp_1);
            c.load_next_insn();
            c.next_ij_from_iw();
            c.next(atomic_load);
        }

        pub fn atomic_load(c: *Cycle, params: []const Parameter.Signature) void {
            c.atomic_this_cycle();
            c.read_to_d(.temp_1, 0, .word, .data);
            c.d_to_ll();
            c.ll_to_reg();
            c.zn_flags_from_ll();
            if (params[2].base == .reg32) {
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

        pub fn atomic_store(c: *Cycle, flags: Flags, params: []const Parameter.Signature) void {
            if (flags.zero()) {
                c.exec_next_insn();
            } else {
                c.atomic_this_cycle();
                c.reg_to_jl();
                c.literal_to_k(1);
                c.jl_plus_k_to_ll(.fresh, .flags);
                c.ll_to_reg();
                c.write_from_ll(.temp_1, 0, .word, .data);
                if (params[2].base == .reg32) {
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
    struct { pub const spec = "ax r(reg), .d x(mem)";
        pub const transform = "ax r(src) -> .d x(mem) -> r(dest)";
        pub const conversions = .{
            .{ .reg, .src },
            .{ .reg, .dest },
            .{ .mem, .mem },
        };
    },
    struct { pub const spec = "ax x(reg), .d x(mem)";
        pub const transform = "ax x(src) -> .d x(mem) -> x(dest)";
        pub const conversions = .{
            .{ .reg, .src },
            .{ .reg, .dest },
            .{ .mem, .mem },
        };
    },
    struct { pub const spec = "ax .d x(mem), r(reg)";
        pub const transform = "ax r(src) -> .d x(mem) -> r(dest)";
        pub const conversions = .{
            .{ .reg, .src },
            .{ .reg, .dest },
            .{ .mem, .mem },
        };
    },
    struct { pub const spec = "ax .d x(mem), x(reg)";
        pub const transform = "ax x(src) -> .d x(mem) -> x(dest)";
        pub const conversions = .{
            .{ .reg, .src },
            .{ .reg, .dest },
            .{ .mem, .mem },
        };
    },
    struct { pub const spec = // ax <src> -> .d x(mem) -> <dest>
        \\ax r(src) -> .d x(mem) -> r(dest)
        \\ax x(src) -> .d x(mem) -> x(dest)
        ;
        pub const encoding = .{
            opcode,
            Encoder.shifted(12, Reg(.mem)),
            Encoder.shifted(16, Reg(.src)),
            Encoder.shifted(20, Reg(.dest)),
            Encoder.shifted(24, @as(u8, 0)),
        };
        pub const ij = Reg(.mem);

        fn opcode(params: []const Parameter.Signature) opcodes.Lo12 {
            return switch (params[0].base) {
                .reg16 => .atomic_exchange_reg16,
                .reg32 => .atomic_exchange_reg32,
                else => unreachable,
            };
        }

        pub fn entry(c: *Cycle) void {
            c.reg32_to_l();
            c.l_to_sr(.temp_1);
            c.ip_read_to_dr(2, .full);
            c.next_ij_from_decode(.alt);
            c.next_ik_from_decode(.alt);
            c.next(load_next_insn);
        }

        pub fn load_next_insn(c: *Cycle) void {
            c.load_next_insn();
            c.next_iw_from_ik();
            c.next(atomic_load);
        }

        pub fn atomic_load(c: *Cycle, params: []const Parameter.Signature) void {
            c.atomic_this_cycle();
            c.read_to_d(.temp_1, 0, .word, .data);
            c.d_to_ll();
            c.ll_to_reg();
            if (params[2].base == .reg32) {
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

        pub fn atomic_store(c: *Cycle, params: []const Parameter.Signature) void {
            c.atomic_this_cycle();
            c.reg_to_jl();
            c.jl_to_ll();
            c.write_from_ll(.temp_1, 0, .word, .data);
            if (params[2].base == .reg32) {
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


    struct { pub const spec = // axe <src> -> .d x(mem) -> <dest>
        \\axe r(src) -> .d x(mem) -> r(expected)
        \\axe x(src) -> .d x(mem) -> x(expected)
        ;
        pub const encoding = .{
            opcode,
            Encoder.shifted(12, Reg(.mem)),
            Encoder.shifted(16, Reg(.src)),
            Encoder.shifted(20, Reg(.expected)),
            Encoder.shifted(24, @as(u8, 0)),
        };
        pub const ij = Reg(.mem);

        fn opcode(params: []const Parameter.Signature) opcodes.Lo12 {
            return switch (params[0].base) {
                .reg16 => .atomic_exchange_if_equal_reg16,
                .reg32 => .atomic_exchange_if_equal_reg32,
                else => unreachable,
            };
        }

        pub fn entry(c: *Cycle) void {
            c.reg32_to_l();
            c.l_to_sr(.temp_1);
            c.ip_read_to_dr(2, .full);
            c.next_ij_from_decode(.alt);
            c.next_ik_from_decode(.alt);
            c.next(atomic_load);
        }

        pub fn atomic_load(c: *Cycle) void {
            c.atomic_this_cycle();
            c.read_to_d(.temp_1, 0, .word, .data);
            c.d_to_l(.zx);
            c.l_to_sr(.temp_2);
            c.next(compare);
        }

        pub fn compare(c: *Cycle, params: []const Parameter.Signature) void {
            c.atomic_this_cycle();
            c.srl_to_jl(.temp_2);
            c.reg_to_k();
            c.jl_minus_k(.fresh, .flags);
            if (params[2].base == .reg32) {
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

        pub fn atomic_store(c: *Cycle, flags: Flags, params: []const Parameter.Signature) void {
            if (flags.zero()) {
                c.atomic_this_cycle();
                c.reg_to_jl();
                c.jl_to_ll();
                c.write_from_ll(.temp_1, 0, .word, .data);
                if (params[2].base == .reg32) {
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

const Cycle = @import("Cycle.zig");
const Encoder = isa.Instruction_Encoding.Encoder;
const Int = placeholders.Int;
const Range = placeholders.Range;
const Reg = placeholders.Reg;
const placeholders = @import("placeholders.zig");
const opcodes = @import("opcodes.zig");
const Parameter = isa.Parameter;
const Flags = hw.microcode.Flags;
const hw = arch.hw;
const isa = arch.isa;
const arch = @import("lib_arch");
const std = @import("std");
