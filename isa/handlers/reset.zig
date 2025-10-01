pub const slot = arch.microcode.Slot.reset;

// The sequencer guarantees that all pipes exit reset in the interrupt execution mode.
// Therefore they will automatically be using RSN 0-3.

pub fn entry(c: *Cycle) void {
    c.status_to_l();
    c.l_to_sr(.temp_1);
    // c.disable_address_translation();
    c.next(pipe_from_status);
}

pub fn pipe_from_status(c: *Cycle) void {
    c.sr_to_j(.temp_1);
    c.literal_to_k(@bitOffsetOf(arch.reg.Status, "pipe"));
    c.j_shift_k_to_l(.shr, .fresh, .no_flags);
    c.l_to_sr(.temp_1);
    c.next(rsn_from_temp);
}

pub fn rsn_from_temp(c: *Cycle) void {
    c.sr_to_j(.temp_1);
    c.literal_to_k(~@as(arch.Pipeline.Raw, 0));
    c.j_logic_k_to_l(._and, .fresh, .no_flags);
    c.l_to_sr(.temp_1);
    c.l_to_sr(.temp_2);
    c.l_to_rsn();
    c.next(init_rsn_0123_zero);
}

pub fn init_rsn_0123_zero(c: *Cycle) void {
    c.zero_to_l();
    c.l_to_sr(.zero);
    c.next(init_rsn_0123_one);
}
pub fn init_rsn_0123_one(c: *Cycle) void {
    c.address(.zero, 1);
    c.virtual_address_to_sr(.one);
    c.next(rsn_30_33);
}

pub fn rsn_30_33(c: *Cycle) void {
    // we can't jump directly from RSN 0-4 to RSN 60-64 because VAO is always signed when used as K,
    // so we add 30 this cycle and another 30 next cycle:
    c.sr_to_j(.temp_2);
    c.literal_to_k(30);
    c.j_plus_k_to_l(.fresh, .no_flags);
    c.l_to_sr(.temp_2);
    c.next(rsn_60_63);
}

pub fn rsn_60_63(c: *Cycle) void {
    std.debug.assert(arch.reg.RSN.count == 64);
    c.sr_to_j(.temp_2);
    c.literal_to_k(30);
    c.j_plus_k_to_l(.fresh, .flags);
    c.l_to_sr(.temp_2);
    c.l_to_rsn();
    c.next(init_sr_zero_one);
}

pub fn init_sr_zero_one(c: *Cycle, flags: Flags) void {
    if (flags.negative()) {
        c.sr_to_l(.temp_1);
        c.zn_flags_from_l();
        c.l_to_sr(.kxp);
        c.next(load_reset_vector);
    } else {
        c.sr2_to_sr2_alt(.zero, .zero);
        c.address(.zero, 1);
        c.virtual_address_to_sr_alt(.one);
        c.next(rsn_minus_4);
    }
}

pub fn rsn_minus_4(c: *Cycle) void {
    c.sr_to_j(.temp_2);
    c.literal_to_k(4);
    c.j_minus_k_to_l(.fresh, .flags);
    c.l_to_sr(.temp_2);
    c.l_to_rsn();
    c.next(init_sr_zero_one);
}

pub fn load_reset_vector(c: *Cycle) void {
    c.read_to_d(.zero, @offsetOf(arch.data_structures.Vector_Table, "reset"), .@"16b", .physical);
    c.d_to_l();
    c.l_to_sr(.next_ip);
    c.next(boot);
}

pub fn boot(c: *Cycle) void {
    // Initialization of GPRs, SP, etc. is the responsibility of the startup routine/OS.
    // Reset automatically sets exec_mode to .interrupt in hardware.
    // It's expected that the startup code will eventually use IRET or IFEX
    // when it wants to start executing user code.
    c.branch(.next_ip, 0);
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
const std = @import("std");
