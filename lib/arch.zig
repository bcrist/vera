pub const hw = @import("arch/hardware.zig");
pub const isa = @import("arch/isa.zig");

pub const Context_State = extern struct {
    registers: [hw.register_count]u16,
    rp: u32,
    sp: u32,
    bp: u32,
    fault_uc_slot_dr: u32,
    fault_rsn_stat: u32,
    int_rsn_fault_iw_ik_ij: u32,
    temp_1: u32,
    ip: u32,
    next_ip: u32,
    asn: u32,
    kxp: u32,
    uxp: u32,
    temp_2: u32,
};

pub const Vector_Table = extern struct {
    reset: u16,
    double_fault: u16,
    page_fault: u16,
    access_fault: u16,
    page_align_fault: u16,
    invalid_instruction_fault: u16,
    instruction_protection_fault: u16,
};

const std = @import("std");
