pub const spec = "iret";

pub const encoding = .{
    opcodes.LSB.misc16,
    Encoder.init(8, opcodes.Misc16.iret),
};

pub const entry = restore_rsn;

pub fn restore_rsn(c: *Cycle, flags: Flags) void {
    if (!flags.kernel()) return c.illegal_instruction();

    c.sr_to_j(.int_stat);
    c.literal_to_k(@offsetOf(arch.Status, "rsn"));
    c.j_shift_k_to_l(.shr, .fresh, .no_flags);
    c.l_to_rsn();
    c.next(restore_stat);
}

pub fn restore_stat(c: *Cycle) void {
    c.reload_asn();
    c.sr_to_l(.int_stat);
    c.l_to_ti_and_stat_zncva();
    c.force_normal_execution(branch);
}

pub fn branch(c: *Cycle) void {
    c.branch(.ip, 0);
}

const opcodes = @import("opcodes.zig");
const Cycle = @import("../compile/Cycle.zig");
const Encoder = isa.Instruction_Encoding.Encoder;
const isa = @import("isa");
const Flags = arch.microcode.Flags;
const arch = @import("arch");
