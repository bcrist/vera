pub const slot = arch.microcode.Slot.interrupt;

pub const entry = save_flags;

pub fn save_flags(c: *Cycle) void {
    c.flags_to_l();
    c.l_to_sr_alt(.int_flags);
    c.disable_flags(.{ .at_enable = true, .bus_override = true });
    c.next(read_vector);
}

pub fn read_vector(c: *Cycle) void {
    c.reload_asn();
    c.read_to_d(.zero, @offsetOf(arch.data_structures.Vector_Table, "interrupt"), .@"16b", .physical);
    c.d_to_l();
    c.l_to_sr(.temp_1);
    c.next(branch);
}

pub fn branch(c: *Cycle) void {
    c.zero_to_l();
    c.l_to_flags(); // set TI to 0
    c.branch(.temp_1, 0);
}

const Cycle = @import("../compile/Cycle.zig");
const Encoder = isa.Encoder;
const Int = placeholders.Int;
const Range = placeholders.Range;
const Reg = placeholders.Reg;
const placeholders = @import("../compile/placeholders.zig");
const Flags = arch.microcode.Flags;
const isa = @import("isa");
const arch = @import("arch");
