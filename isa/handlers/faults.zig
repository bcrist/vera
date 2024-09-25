pub fn Handler(comptime handler: arch.microcode.Slot) type {
    return struct {
        pub const slot = handler;

        pub const entry = store_stat;

        pub fn store_stat(c: *Cycle) void {
            c.status_to_l();
            c.l_to_sr(.fault_stat);
            c.next(store_dr);
        }

        pub fn store_dr(c: *Cycle) void {
            c.dr_to_l();
            c.l_to_sr(.fault_dr);
            c.next(read_last_translation_info);
        }

        pub fn read_last_translation_info(c: *Cycle) void {
            c.toggle_rsn();
            c.last_translation_info_to_l();
            c.disable_address_translation();
            c.l_to_sr(.bp);
            c.next(load_vector);
        }

        pub fn load_vector(c: *Cycle) void {
            c.reload_asn();
            c.read_to_d(.zero, @offsetOf(arch.Vector_Table, @tagName(handler)), .@"16b", .raw);
            c.d_to_l();
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
