pub const instructions = .{
    struct { pub const slot = hw.microcode.Slot.interrupt;
        const vector_register = hw.addr.Physical.interrupt_controller;
        pub const entry = persist_stat;

        fn persist_stat(c: *Cycle) void {
            // N.B. although fault_rsn_stat says it's for fault handlers, we also use it for interrupt handlers.
            // This is okay because the interrupt handler switches to a different RSN, so it shouldn't be
            // possible for a fault to happen before IRET is called.
            c.srh_to_lh(.fault_rsn_stat);
            c.stat_to_ll();
            c.l_to_sr(.fault_rsn_stat);
            c.next(swap_rsn);
        }

        pub fn swap_rsn(c: *Cycle) void {
            c.rsn_to_sr1h(.int_rsn_fault_iw_ik_ij);
            c.pipeline_id_to_ll();
            c.ll_to_rsn();
            c.next_ik_ij_zx(@truncate(vector_register.raw() >> 16));
            c.next(compute_vector_address);
        }

        pub fn compute_vector_address(c: *Cycle) void {
            c.reload_asn();
            c.srl_to_jl(.one);
            c.ik_ij_zx_to_k();
            c.jl_times_k__swap_result_halves(.zx, .zx);
            c.compute_to_l(.fresh, .no_flags);
            c.l_to_sr(.temp_1);
            c.next(read_vector);
        }

        pub fn read_vector(c: *Cycle) void {
            c.read_to_d(.temp_1, @intCast(vector_register.raw() & 0xFFFF), .word, .raw);
            c.d_to_l(.zx);
            c.l_to_sr(.next_ip);
            c.next(branch);
        }

        pub fn branch(c: *Cycle) void {
            c.branch(.next_ip, 0);
        }
    },
    struct { pub const spec = "iret";
        pub const encoding = .{ opcodes.Lo16.return_from_interrupt };

        pub fn entry(c: *Cycle, flags: hw.microcode.Flags) void {
            if (!flags.kernel()) return c.illegal_instruction();

            c.srh_to_ll(.int_rsn_fault_iw_ik_ij);
            c.ll_to_rsn();
            c.next(restore_stat);
        }

        pub fn restore_stat(c: *Cycle) void {
            c.reload_asn();
            c.srl_to_ll(.fault_rsn_stat);
            c.ll_to_stat_zncvka();
            c.force_normal_execution(branch);
        }

        pub fn branch(c: *Cycle) void {
            c.branch(.ip, 0);
        }
    },
};

const Cycle = @import("Cycle.zig");
const Encoder = isa.Instruction_Encoding.Encoder;
const Int = placeholders.Int;
const Range = placeholders.Range;
const Reg = placeholders.Reg;
const placeholders = @import("placeholders.zig");
const opcodes = @import("opcodes.zig");
const hw = arch.hw;
const isa = arch.isa;
const arch = @import("lib_arch");
const std = @import("std");
