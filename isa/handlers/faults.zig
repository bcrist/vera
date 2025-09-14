pub fn Handler(comptime handler: arch.microcode.Slot) type {
    return struct {
        pub const slot = handler;

        pub const entry = store_status;

        pub fn store_status(c: *Cycle) void {
            c.status_to_l();
            c.l_to_sr_alt(.fault_status);
            c.next(store_flags);
        }

        pub fn store_flags(c: *Cycle) void {
            c.flags_to_l();
            c.l_to_sr_alt(.fault_flags);
            c.next(store_dr);
        }

        pub fn store_dr(c: *Cycle) void {
            c.dr_to_l();
            c.l_to_sr_alt(.fault_dr);
            c.next(store_ir);
        }

        pub fn store_ir(c: *Cycle) void {
            c.ir_to_l();
            c.l_to_sr_alt(.fault_ir);
            c.next(read_atr);
        }

        pub fn read_atr(c: *Cycle) void {
            c.atr_to_l();
            c.disable_flags(.{ .ate = true, .read_override = true });
            c.l_to_sr(.bp);
            c.reload_asn();
            c.next(load_vector);
        }

        pub fn load_vector(c: *Cycle) void {
            c.read_to_d(.zero, @offsetOf(arch.data_structures.Vector_Table, @tagName(handler)), .@"16b", .physical);
            c.d_to_l();
            c.l_to_sr(.temp_1);
            c.next(branch);
        }

        pub fn branch(c: *Cycle) void {
            c.zero_to_l();
            c.l_to_flags(); // set TI to 0
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
