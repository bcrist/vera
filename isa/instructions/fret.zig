pub const spec = "fret";

pub const encoding = .{
    opcodes.LSB.misc_16,
    Encoder.init(8, opcodes.Misc_16.fret),
};

pub const entry = toggle_rsn;

pub fn toggle_rsn(c: *Cycle, flags: Flags) void {
    if (!flags.kernel()) return c.illegal_instruction();

    c.toggle_rsn();
    c.next(restore_ir);
}

pub fn restore_ir(c: *Cycle) void {
    c.reload_asn();
    c.sr_to_l(.fault_ir);
    c.l_to_dr();
    c.dr_to_ir();
    c.next(restore_dr);
}

pub fn restore_dr(c: *Cycle) void {
    c.sr_to_l(.fault_dr);
    c.l_to_dr();
    c.next(restore_stat_uca_and_retry);
}

pub fn restore_stat_uca_and_retry(c: *Cycle) void {
    c.sr_to_l(.fault_stat);
    c.l_to_ti_and_stat_zncva();
    c.fault_return();
}

const opcodes = @import("opcodes.zig");
const Cycle = @import("../compile/Cycle.zig");
const Encoder = isa.Instruction_Encoding.Encoder;
const isa = @import("isa");
const Flags = arch.microcode.Flags;
const arch = @import("arch");
