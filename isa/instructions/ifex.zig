// Exit interrupt or fault handler without changing registersets or retrying the faulted operation

pub const spec = "ifex";

pub const encoding = .{
    opcodes.LSB.misc_16,
    Encoder.init(8, opcodes.Misc_16.ifex),
};

pub fn entry(c: *Cycle, flags: Flags) void {
    if (!flags.kernel()) return c.illegal_instruction();
    
    c.force_normal_execution(load_next_insn);
}

pub fn load_next_insn(c: *Cycle) void {
    c.load_and_exec_next_insn();
}

const opcodes = @import("opcodes.zig");
const Cycle = @import("../compile/Cycle.zig");
const Encoder = isa.Encoder;
const isa = @import("isa");
const Flags = arch.microcode.Flags;
const arch = @import("arch");
