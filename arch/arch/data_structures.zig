// Structs here define the layout of certain in memory data structures that are used directly by the CPU microcode.

// Used by the LDRS and STRS instructions:
pub const Context_State = extern struct {
    reg: [reg.gpr.count]reg.gpr.Value.Raw,
    rp: reg.sr1.Value.Raw,
    sp: reg.sr1.Value.Raw,
    bp: reg.sr1.Value.Raw,
    temp_1: reg.sr1.Value.Raw,
    int_flags: reg.sr1.Value.Raw,
    fault_status: reg.sr1.Value.Raw,
    fault_flags: reg.sr1.Value.Raw,
    fault_dr: reg.sr1.Value.Raw,
    fault_ir: reg.sr1.Value.Raw,
    ip: reg.sr2.Value.Raw,
    asn: reg.sr2.Value.Raw,
    next_ip: reg.sr2.Value.Raw,
    kxp: reg.sr2.Value.Raw,
    uxp: reg.sr2.Value.Raw,
    temp_2: reg.sr2.Value.Raw,
};

// Used by interrupt and fault handlers:
pub const Vector_Table = extern struct {
    reset: u16,
    interrupt: u16,
    double_fault: u16,
    page_fault: u16,
    access_fault: u16,
    pipe_fault: u16,
    page_align_fault: u16,
    align_fault: u16,
    overflow_fault: u16,
    rs_underflow_fault: u16,
    rs_overflow_fault: u16,
    invalid_instruction_fault: u16,
    instruction_protection_fault: u16,
};

const reg = @import("reg.zig");