pub const forms = .{
    struct {
        pub const spec = "fret";

        pub const encoding = .{
            opcodes.LSB.misc_16,
            Encoder.init(8, opcodes.Misc_16.fret),
        };

        pub const entry = restore_ir;
    },
    struct {
        pub const spec = "freto";

        pub const encoding = .{
            opcodes.LSB.misc_16,
            Encoder.init(8, opcodes.Misc_16.freto),
        };
        pub const krio: arch.bus.K.Read_Index_Offset.Raw = @bitOffsetOf(arch.reg.Flags, "bus_override");

        pub const entry = load_vabor;
    },
};

pub fn load_vabor(c: *Cycle, flags: Flags) void {
    if (!flags.kernel()) return c.illegal_instruction();
    c.reg_to_j_to_l();
    c.l_to_vabor();
    c.next(restore_flags_set_bus_override);
}

pub fn restore_flags_set_bus_override(c: *Cycle) void {
    c.sr_alt_to_j(.fault_flags);
    c.literal_to_k((arch.reg.Flags.Writable { .bus_override = true }).raw());
    c.j_logic_k_to_l(._or, .fresh, .no_flags);
    c.l_to_flags();
    c.next(restore_ir);
}

pub fn restore_flags(c: *Cycle, flags: Flags) void {
    if (!flags.kernel()) return c.illegal_instruction();
    c.sr_alt_to_l(.fault_flags);
    c.l_to_flags();
    c.next(restore_ir);
}

pub fn restore_ir(c: *Cycle) void {
    c.sr_alt_to_l(.fault_ir);    
    c.l_to_dr();
    c.dr_to_ir();
    c.next(restore_dr);
}

pub fn restore_dr(c: *Cycle) void {
    c.sr_alt_to_l(.fault_dr);
    c.l_to_dr();
    c.next(restore_asn6);
}

pub fn restore_asn6(c: *Cycle) void {
    c.reload_asn_alt();
    c.next(restore_uca_and_retry);
}

pub fn restore_uca_and_retry(c: *Cycle) void {
    c.fault_return();
}

const opcodes = @import("opcodes.zig");
const Cycle = @import("../compile/Cycle.zig");
const Encoder = isa.Instruction_Encoding.Encoder;
const isa = @import("isa");
const Flags = arch.microcode.Flags;
const arch = @import("arch");
