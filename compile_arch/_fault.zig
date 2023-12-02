pub const instructions = .{
    Fault_Handler(.page_fault),
    Fault_Handler(.access_fault),
    Fault_Handler(.page_align_fault),
    Fault_Handler(.instruction_protection_fault),
    Fault_Handler(.invalid_instruction_fault),
    Fault_Handler(.double_fault),
    struct { pub const slot = .invalid_instruction;
        pub fn entry(c: *Cycle) void {
            c.invalid_instruction();
        }
    },
    struct { pub const spec = "fret";
        pub const encoding = opcodes.Lo16.return_from_fault;

        pub fn entry(c: *Cycle, flags: Flags) void {
            if (!flags.kernel()) return c.illegal_instruction();
            c.toggle_rsn();
            c.next(restore_stat);
        }

        pub fn restore_stat(c: *Cycle) void {
            c.reload_asn();
            c.sr_to_l(.fault_rsn_stat);
            c.ll_to_stat_zncvka();
            c.next(restore_ij_ik_iw);
        }

        pub fn restore_ij_ik_iw(c: *Cycle) void {
            c.sr_to_l(.int_rsn_fault_iw_ik_ij);
            c.ll_to_dr();
            c.decode_dr_to_ij_ik_iw(.alt);
            c.next(restore_dr_and_retry);
        }

        pub fn restore_dr_and_retry(c: *Cycle) void {
            c.sr_to_l(.fault_uc_slot_dr);
            c.ll_to_dr();
            // If a page fault occurs during an instruction preceeded by SYNC,
            // we want to make sure it's still atomic when we retry.
            // It doesn't hurt to "upgrade" a non-atomic instruction, so we
            // just assume all faults happen during atomic instructions
            c.atomic_next_cycle_until_end();
            c.fault_return();
        }
    },
    struct { pub const spec = "ifex"; // Exit interrupt or fault handler without changing registersets or retrying the faulted operation
        pub const encoding = opcodes.Lo16.exit_handler;

        pub fn entry(c: *Cycle, flags: Flags) void {
            if (!flags.kernel()) return c.illegal_instruction();
            c.force_normal_execution(load_next_insn);
        }

        pub fn load_next_insn(c: *Cycle) void {
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = "ldrs .d x(src) -> b(registerset)";
        pub const encoding = .{
            opcodes.Lo8.load_registerset,
            Encoder.shifted(8, Reg(.src)),
            Encoder.shifted(12, Reg(.registerset)),
        };
        pub const ij = Reg(.src);
        pub const ik = Reg(.registerset);
        pub const iw: u4 = 0;

        const LDRS = @This();

        pub fn entry(c: *Cycle, flags: Flags) void {
            if (!flags.kernel()) return c.illegal_instruction();

            // Move the base address into SR2 so that we can move it to .RS_reserved without using L:
            c.reg32_to_l(); // x(src)
            c.l_to_sr(.temp_2);
            c.next(switch_rsn);
        }

        pub fn switch_rsn(c: *Cycle) void {
            c.rsn_to_sr1h(.fault_rsn_stat);
            c.reg_to_k(); // r(registerset)
            c.ll_to_rsn();
            c.sr2_to_sr2(.temp_2, .rs_reserved);
            c.next(load_r0);
        }

        pub const load_r0 = load_gpr(0);
        pub const load_r1 = load_gpr(1);
        pub const load_r2 = load_gpr(2);
        pub const load_r3 = load_gpr(3);
        pub const load_r4 = load_gpr(4);
        pub const load_r5 = load_gpr(5);
        pub const load_r6 = load_gpr(6);
        pub const load_r7 = load_gpr(7);
        pub const load_r8 = load_gpr(8);
        pub const load_r9 = load_gpr(9);
        pub const load_r10 = load_gpr(10);
        pub const load_r11 = load_gpr(11);
        pub const load_r12 = load_gpr(12);
        pub const load_r13 = load_gpr(13);
        pub const load_r14 = load_gpr(14);
        pub const load_r15 = load_gpr(15);

        fn load_gpr(comptime n: hw.Register_Index) fn(c: *Cycle)void {
            return struct {
                pub fn func(c: *Cycle) void {
                    c.read_to_d(.rs_reserved, @as(Cycle.Address_Offset, n) * 2, .word, .data);
                    c.d_to_l(.zx);
                    c.ll_to_reg();
                    if (n < 15) {
                        c.next_iw(n + 1);
                        c.next(@field(LDRS, std.fmt.comptimePrint("load_r{}", .{ n + 1 })));
                    } else {
                        c.next_ik_bit(hw.register_count * @sizeOf(hw.R));
                        c.next(offset_rs_reserved);
                    }
                }
            }.func;
        }

        pub fn offset_rs_reserved(c: *Cycle) void {
            c.sr_to_j(.rs_reserved);
            c.ik_bit_to_k();
            c.j_plus_k_to_l(.zx, .fresh, .no_flags);
            c.l_to_sr(.rs_reserved);
            c.next(load_rp);
        }

        pub const load_rp                      = load_sr_pt1(.rp);
        pub const load_sp                      = load_sr_pt1(.sp);
        pub const load_bp                      = load_sr_pt1(.bp);
        pub const load_fault_uc_slot_dr        = load_sr_pt1(.fault_uc_slot_dr);
        pub const load_fault_rsn_stat          = load_sr_pt1(.fault_rsn_stat);
        pub const load_int_rsn_fault_iw_ik_ij  = load_sr_pt1(.int_rsn_fault_iw_ik_ij);
        pub const load_ip                      = load_sr_pt1(.ip);
        pub const load_next_ip                 = load_sr_pt1(.next_ip);
        pub const load_asn                     = load_sr_pt1(.asn);
        pub const load_kxp                     = load_sr_pt1(.kxp);
        pub const load_uxp                     = load_sr_pt1(.uxp);
        pub const load_temp_2                  = load_sr_pt1(.temp_2);
        pub const load_temp_1                  = load_sr_pt1(.temp_1);

        pub const load_rp_2                      = load_sr_pt2(.rp, load_sp);
        pub const load_sp_2                      = load_sr_pt2(.sp, load_bp);
        pub const load_bp_2                      = load_sr_pt2(.bp, load_fault_uc_slot_dr);
        pub const load_fault_uc_slot_dr_2        = load_sr_pt2(.fault_uc_slot_dr, load_fault_rsn_stat);
        pub const load_fault_rsn_stat_2          = load_sr_pt2(.fault_rsn_stat, load_int_rsn_fault_iw_ik_ij);
        pub const load_int_rsn_fault_iw_ik_ij_2  = load_sr_pt2(.int_rsn_fault_iw_ik_ij, load_ip);
        pub const load_ip_2                      = load_sr_pt2(.ip, load_next_ip);
        pub const load_next_ip_2                 = load_sr_pt2(.next_ip, load_asn);
        pub const load_asn_2                     = load_sr_pt2(.asn, load_kxp);
        pub const load_kxp_2                     = load_sr_pt2(.kxp, load_uxp);
        pub const load_uxp_2                     = load_sr_pt2(.uxp, load_temp_2);
        pub const load_temp_2_2                  = load_sr_pt2(.temp_2, load_temp_1);
        pub const load_temp_1_2                  = load_sr_pt2(.temp_1, restore_rsn);

        fn load_sr_pt1(comptime sr: hw.Control_Signals.Any_SR_Index) fn(c: *Cycle)void {
            const offset = @offsetOf(arch.Context_State, @tagName(sr)) - (hw.register_count * @sizeOf(hw.R));
            return struct {
                pub fn func(c: *Cycle) void {
                    c.read_to_d(.rs_reserved, offset + 2, .word, .data);
                    c.d_to_l(.zx);
                    c.l_to_sr(.temp_1);
                    c.next(@field(LDRS, "load_" ++ @tagName(sr) ++ "_2"));
                }
            }.func;
        }
        fn load_sr_pt2(comptime sr: hw.Control_Signals.Any_SR_Index, comptime next: *const anyopaque) fn(c: *Cycle)void {
            const offset = @offsetOf(arch.Context_State, @tagName(sr)) - (hw.register_count * @sizeOf(hw.R));
            return struct {
                pub fn func(c: *Cycle) void {
                    c.read_to_d(.rs_reserved, offset, .word, .data);
                    c.srl_to_lh(.temp_1);
                    c.d_to_ll();
                    c.l_to_sr(sr);
                    c.next(next);
                }
            }.func;
        }

        pub fn restore_rsn(c: *Cycle) void {
            c.srh_to_ll(.fault_rsn_stat);
            c.ll_to_rsn();
            c.next(load_next_insn);
        }

        pub fn load_next_insn(c: *Cycle) void {
            c.load_and_exec_next_insn();
        }

    },
    struct { pub const spec = "strs b(registerset) -> .d x(dest)";
        pub const encoding = .{
            opcodes.Lo8.store_registerset,
            Encoder.shifted(8, Reg(.dest)),
            Encoder.shifted(12, Reg(.registerset)),
        };
        pub const ij = Reg(.dest);
        pub const ik = Reg(.registerset);

        const STRS = @This();

        pub fn entry(c: *Cycle, flags: Flags) void {
            if (!flags.kernel()) return c.illegal_instruction();
            // Move the base address into .temp_2 so that we can move it to .rs_reserved without using L:
            c.reg32_to_l();
            c.l_to_sr(.temp_2);
            c.next(switch_rsn);
        }

        pub fn switch_rsn(c: *Cycle) void {
            // switch RSN, copy base address to .RS_reserved:
            c.rsn_to_sr1h(.fault_rsn_stat);
            c.reg_to_k(); // r(registerset)
            c.k_to_ll();
            c.ll_to_rsn();
            c.sr2_to_sr2(.temp_2, .rs_reserved);
            c.next_ik(0);
            c.next(store_r0);
        }

        pub const store_r0 = store_gpr(0);
        pub const store_r1 = store_gpr(1);
        pub const store_r2 = store_gpr(2);
        pub const store_r3 = store_gpr(3);
        pub const store_r4 = store_gpr(4);
        pub const store_r5 = store_gpr(5);
        pub const store_r6 = store_gpr(6);
        pub const store_r7 = store_gpr(7);
        pub const store_r8 = store_gpr(8);
        pub const store_r9 = store_gpr(9);
        pub const store_r10 = store_gpr(10);
        pub const store_r11 = store_gpr(11);
        pub const store_r12 = store_gpr(12);
        pub const store_r13 = store_gpr(13);
        pub const store_r14 = store_gpr(14);
        pub const store_r15 = store_gpr(15);

        fn store_gpr(comptime n: hw.Register_Index) fn(c: *Cycle)void {
            return struct {
                pub fn func(c: *Cycle) void {
                    c.reg_to_k();
                    c.k_to_ll();
                    c.write_from_ll(.rs_reserved, @as(Cycle.Address_Offset, n) * 2, .word, .data);
                    if (n < 15) {
                        c.next_ik(n + 1);
                        c.next(@field(STRS, std.fmt.comptimePrint("store_r{}", .{ n + 1 })));
                    } else {
                        c.next_ik_bit(hw.register_count * @sizeOf(hw.R));
                        c.next(offset_rs_reserved);
                    }
                }
            }.func;
        }

        pub fn offset_rs_reserved(c: *Cycle) void {
            c.sr_to_j(.rs_reserved);
            c.ik_bit_to_k();
            c.j_plus_k_to_l(.zx, .fresh, .no_flags);
            c.l_to_sr(.rs_reserved);
            c.next(store_rp);
        }

        pub const store_rp                      = store_sr_pt1(.rp);
        pub const store_sp                      = store_sr_pt1(.sp);
        pub const store_bp                      = store_sr_pt1(.bp);
        pub const store_fault_uc_slot_dr        = store_sr_pt1(.fault_uc_slot_dr);
        pub const store_fault_rsn_stat          = store_sr_pt1(.fault_rsn_stat);
        pub const store_int_rsn_fault_iw_ik_ij  = store_sr_pt1(.int_rsn_fault_iw_ik_ij);
        pub const store_ip                      = store_sr_pt1(.ip);
        pub const store_next_ip                 = store_sr_pt1(.next_ip);
        pub const store_asn                     = store_sr_pt1(.asn);
        pub const store_kxp                     = store_sr_pt1(.kxp);
        pub const store_uxp                     = store_sr_pt1(.uxp);
        pub const store_temp_2                  = store_sr_pt1(.temp_2);
        pub const store_temp_1                  = store_sr_pt1(.temp_1);

        pub const store_rp_2                      = store_sr_pt2(.rp, store_sp);
        pub const store_sp_2                      = store_sr_pt2(.sp, store_bp);
        pub const store_bp_2                      = store_sr_pt2(.bp, store_fault_uc_slot_dr);
        pub const store_fault_uc_slot_dr_2        = store_sr_pt2(.fault_uc_slot_dr, store_fault_rsn_stat);
        pub const store_fault_rsn_stat_2          = store_sr_pt2(.fault_rsn_stat, store_int_rsn_fault_iw_ik_ij);
        pub const store_int_rsn_fault_iw_ik_ij_2  = store_sr_pt2(.int_rsn_fault_iw_ik_ij, store_ip);
        pub const store_ip_2                      = store_sr_pt2(.ip, store_next_ip);
        pub const store_next_ip_2                 = store_sr_pt2(.next_ip, store_asn);
        pub const store_asn_2                     = store_sr_pt2(.asn, store_kxp);
        pub const store_kxp_2                     = store_sr_pt2(.kxp, store_uxp);
        pub const store_uxp_2                     = store_sr_pt2(.uxp, store_temp_2);
        pub const store_temp_2_2                  = store_sr_pt2(.temp_2, store_temp_1);
        pub const store_temp_1_2                  = store_sr_pt2(.temp_1, restore_rsn);

        fn store_sr_pt1(comptime sr: hw.Control_Signals.Any_SR_Index) fn(c: *Cycle)void {
            const offset = @offsetOf(arch.Context_State, @tagName(sr)) - (hw.register_count * @sizeOf(hw.R));
            return struct {
                pub fn func(c: *Cycle) void {
                    c.srh_to_ll(sr);
                    c.write_from_ll(.rs_reserved, offset + 2, .word, .data);
                    c.next(@field(STRS, "store_" ++ @tagName(sr) ++ "_2"));
                }
            }.func;
        }
        fn store_sr_pt2(comptime sr: hw.Control_Signals.Any_SR_Index, comptime next: *const anyopaque) fn(c: *Cycle)void {
            const offset = @offsetOf(arch.Context_State, @tagName(sr)) - (hw.register_count * @sizeOf(hw.R));
            return struct {
                pub fn func(c: *Cycle) void {
                    c.srl_to_ll(sr);
                    c.write_from_ll(.rs_reserved, offset, .word, .data);
                    c.next(next);
                }
            }.func;
        }

        pub fn restore_rsn(c: *Cycle) void {
            c.srh_to_ll(.fault_rsn_stat);
            c.ll_to_rsn();
            c.next(load_next_insn);
        }

        pub fn load_next_insn(c: *Cycle) void {
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = "srs r(reg)";
        pub const encoding = .{
            opcodes.Lo12.switch_to_registerset,
            Encoder.shifted(12, Reg(.reg)),
        };
        pub const ij = Reg(.reg);

        pub fn entry(c: *Cycle, flags: Flags) void {
            if (!flags.kernel()) return c.illegal_instruction();
            // Increment IP so that if/when someone switches back to this registerset,
            // this instruction doesn't get executed a second time.
            c.sr_to_j(.ip);
            c.literal_to_k(2);
            c.j_plus_k_to_l(.zx, .fresh, .no_flags);
            c.l_to_sr(.ip);
            c.next(switch_rsn);
        }

        pub fn switch_rsn(c: *Cycle) void {
            c.reg_to_jl();
            c.jl_to_ll();
            c.ll_to_rsn();
            c.next(reload_asn);
        }

        pub fn reload_asn(c: *Cycle) void {
            c.reload_asn();
            c.next(branch);
        }

        pub fn branch(c: *Cycle) void {
            c.branch(.ip, 0);
        }
    },
};

fn Fault_Handler(comptime handler: hw.microcode.Slot) type {
    return struct {
        pub const slot = handler;

        pub const entry = store_faulted_slot_and_dr;

        pub fn store_faulted_slot_and_dr(c: *Cycle) void {
            c.prev_uc_slot_to_lh();
            c.dr_to_ll();
            c.l_to_sr(.fault_uc_slot_dr);
            c.next(store_stat);
        }

        pub fn store_stat(c: *Cycle) void {
            c.srh_to_lh(.fault_rsn_stat);
            c.stat_to_ll();
            c.l_to_sr(.fault_rsn_stat);
            c.next(store_ij_ik_iw);
        }

        pub fn store_ij_ik_iw(c: *Cycle) void {
            c.assume_ij_valid();
            c.assume_ik_valid();
            c.assume_iw_valid();
            c.srh_to_lh(.int_rsn_fault_iw_ik_ij);
            c.iw_ik_ij_zx_to_ll();
            c.l_to_sr(.int_rsn_fault_iw_ik_ij);
            c.next_iw(0);
            c.next(read_last_translation_info);
        }

        pub fn read_last_translation_info(c: *Cycle) void {
            c.toggle_rsn();
            c.last_translation_info_to_l();
            c.l_to_reg32();
            c.next(load_vector);
        }

        pub fn load_vector(c: *Cycle) void {
            c.reload_asn();
            c.read_to_d(.zero, @offsetOf(arch.Vector_Table, @tagName(handler)), .word, .raw);
            c.d_to_l(.zx);
            c.l_to_sr(.temp_1);
            c.next(branch);
        }

        pub fn branch(c: *Cycle) void {
            c.branch(.temp_1, 0);
        }

    };
}

const Cycle = @import("Cycle.zig");
const Encoder = isa.Instruction_Encoding.Encoder;
const Int = placeholders.Int;
const Range = placeholders.Range;
const Reg = placeholders.Reg;
const placeholders = @import("placeholders.zig");
const opcodes = @import("opcodes.zig");
const Flags = hw.microcode.Flags;
const hw = arch.hw;
const isa = arch.isa;
const arch = @import("lib_arch");
const std = @import("std");
