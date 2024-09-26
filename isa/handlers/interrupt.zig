pub const slot = arch.microcode.Slot.interrupt;

pub const entry = save_stat;

pub fn save_stat(c: *Cycle) void {
    c.status_to_l();
    c.l_to_sr(.int_stat);
    c.next(change_rsn);
}

pub fn change_rsn(c: *Cycle) void {
    c.sr_to_j(.int_stat);
    c.literal_to_k(3);
    c.j_logic_k_to_l(._and, .fresh, .no_flags);
    c.l_to_rsn();
    c.sr1_to_sr1(.int_stat, .int_stat);
    c.disable_address_translation();
    c.next(read_vector);
}

pub fn read_vector(c: *Cycle) void {
    c.reload_asn();
    c.read_to_d(.zero, @offsetOf(arch.Vector_Table, "interrupt"), .@"16b", .raw);
    c.d_to_l();
    c.l_to_sr(.temp_1);
    c.next(branch);
}

pub fn branch(c: *Cycle) void {
    c.branch(.temp_1, 0);
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
