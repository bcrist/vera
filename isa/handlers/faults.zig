pub fn Fault_Handler(comptime handler: arch.microcode.Slot) type {
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
            c.next(store_iw_ik_ij);
        }

        pub fn store_iw_ik_ij(c: *Cycle) void {
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

const Cycle = @import("../compile/Cycle.zig");
const Encoder = isa.Instruction_Encoding.Encoder;
const Int = placeholders.Int;
const Range = placeholders.Range;
const Reg = placeholders.Reg;
const placeholders = @import("../compile/placeholders.zig");
const Flags = arch.microcode.Flags;
const isa = @import("isa");
const arch = @import("arch");
