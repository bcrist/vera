pub const slot = arch.microcode.Slot.reset;

pub fn entry(c: *Cycle) void {
    // The very start is a bit tricky.
    // We can't rely on most registers because the other pipes might be using the same RSN.
    // So the first order of business is to get each pipe into a different registerset.
    // The low 2 bits of the status register are the pipeline number, so this should ensure
    // that they're unique:
    c.status_to_l();
    c.l_to_rsn();
    c.l_to_sr(.temp_1);
    c.next(clean_rsn);
}

pub fn clean_rsn(c: *Cycle) void {
    // The previous cycle got each pipeline into a unique RSN, but not necessarily
    // 0/1/2/3 (the ones we want) so we do it again here.  We also set flags so that we
    // can detect which is pipeline 0 a little later.
    c.sr_to_j(.temp_1);
    c.literal_to_k(3);
    c.j_logic_k_to_l(._and, .fresh, .flags);
    c.l_to_sr(.temp_1);
    c.l_to_rsn();
    c.next(init_zero_register);
}

pub fn init_zero_register(c: *Cycle) void {
    // Set up the .zero SR for RSN 0/1/2/3 and disable address translation if it's on
    c.zero_to_l();
    c.l_to_sr(.zero);
    c.disable_address_translation();
    c.next(init_one_register_and_check_for_pipe_0);
}

pub fn init_one_register_and_check_for_pipe_0(c: *Cycle, flags: Flags) void {
    // Set up the .one SR for RSN 0/1/2/3
    c.address(.zero, 1);
    c.virtual_address_to_sr(.one);
    if (flags.zero()) {
        c.next(begin_block_transfer);
    } else {
        // check for Pipe 1 next cycle
        c.sr_to_j(.temp_1);
        c.literal_to_k(1);
        c.j_minus_k_to_l(.fresh, .flags);
        c.next(check_for_pipe_1);
    }
}

pub fn begin_block_transfer(c: *Cycle) void {
    // We're going to create the transfer control frame address by shifting two 5-bit values from VAO literals,
    // so let's make sure this will cover the bits we need:
    std.debug.assert((arch.addr.Frame.block_transfer_control_frame.raw() & 0x01E0) == 0);
    c.literal_to_l(arch.addr.Frame.block_transfer_control_frame.raw() >> 9);
    c.l_to_sr(.temp_1);
    c.next(shift_block_transfer_control_frame);
}
pub fn shift_block_transfer_control_frame(c: *Cycle) void {
    c.sr_to_j(.temp_1);
    c.literal_to_k(9);
    c.j_shift_k_to_l(.shl, .fresh, .no_flags);
    c.l_to_sr(.temp_1);
    c.next(finish_block_transfer_control_frame);
}
pub fn finish_block_transfer_control_frame(c: *Cycle) void {
    c.sr_to_j(.temp_1);
    c.literal_to_k(arch.addr.Frame.block_transfer_control_frame.raw() & 0x1F);
    c.j_logic_k_to_l(._or, .fresh, .no_flags);
    c.l_to_sr(.temp_1);
    c.next(shift_block_transfer_control_address);
}
pub fn shift_block_transfer_control_address(c: *Cycle) void {
    c.sr_to_j(.temp_1);
    c.literal_to_k(@bitSizeOf(arch.addr.Offset));
    c.j_shift_k_to_l(.shl, .fresh, .no_flags);
    c.l_to_sr(.temp_1);
    c.next(compute_block_transfer_control_data);
}
pub fn compute_block_transfer_control_data(c: *Cycle) void {
    // The FLASH boot sector address is 0x7E000
    c.literal_to_l(0x7);
    c.l_to_sr(.temp_2);
    c.next(shift_block_transfer_control_data);
}
pub fn shift_block_transfer_control_data(c: *Cycle) void {
    c.sr_to_j(.temp_2);
    c.literal_to_k(4);
    c.j_shift_k_to_l(.shl, .fresh, .no_flags);
    c.l_to_sr(.temp_2);
    c.next(compute_block_transfer_control_data_2);
}
pub fn compute_block_transfer_control_data_2(c: *Cycle) void {
    c.sr_to_j(.temp_2);
    c.literal_to_k(0xE);
    c.j_logic_k_to_l(._or, .fresh, .no_flags);
    c.l_to_sr(.temp_2);
    c.next(shift_block_transfer_control_data_2);
}
pub fn shift_block_transfer_control_data_2(c: *Cycle) void {
    c.sr_to_j(.temp_2);
    c.literal_to_k(12);
    c.j_shift_k_to_l(.shl, .fresh, .no_flags);
    c.l_to_sr(.temp_2);
    c.next(write_block_transfer_control);
}
pub fn write_block_transfer_control(c: *Cycle) void {
    c.sr_to_l(.temp_2);
    c.write_from_l(.temp_1, 12, .@"32b", .raw); // block transfer from FLASH, with auto advance
    c.next(readback_block_transfer_address);
}
pub fn readback_block_transfer_address(c: *Cycle) void {
    c.read_to_d(.temp_1, 12, .@"32b", .raw);
    c.d_to_l();
    c.l_to_sr(.bp);
    c.next(check_block_transfer_address);
}
pub fn check_block_transfer_address(c: *Cycle) void {
    c.sr_to_j(.temp_2);
    c.sr_to_k(.bp);
    c.j_logic_k(.xor, .fresh, .flags);
    c.next(setup_write_pointer);
}
pub fn setup_write_pointer(c: *Cycle, flags: Flags) void {
    if (flags.zero()) {
        // .temp_1 will be incremented by 8 before each write, so we start at -8.
        c.literal_to_l(-8);
        c.l_to_sr(.temp_1);
        c.next(setup_transfer_counter);
    } else {
        // Block transfer address wasn't retained; assume we don't have block transfer functionality
        // and that RAM already contains our vector table and boot program:
        c.read_to_d(.zero, @offsetOf(arch.Vector_Table, "reset"), .@"16b", .raw);
        c.d_to_l();
        c.l_to_sr(.next_ip);
        c.next(boot);
    }
}
pub fn setup_transfer_counter(c: *Cycle) void {
    // We will be copying 0x2000 blocks, tracked in .temp_2
    c.sr_to_j(.one);
    c.literal_to_k(13);
    c.j_shift_k_to_l(.shl, .fresh, .flags);
    c.l_to_sr(.temp_2);
    c.next(block_transfer_loop);
}
pub fn block_transfer_loop(c: *Cycle, flags: Flags) void {
    if (!flags.zero()) {
        c.block_transfer_pull(.temp_1, 8, .raw);
        c.sr_to_j(.temp_2); // transfers remaining
        c.sr_to_k(.one);
        c.j_minus_k_to_l(.fresh, .flags);
        c.l_to_sr(.temp_2);
        c.next(block_transfer_loop);
    } else {
        // Done with the block transfer!
        // Time to read the reset vector:
        c.read_to_d(.zero, @offsetOf(arch.Vector_Table, "reset"), .@"16b", .raw);
        c.d_to_l();
        c.l_to_sr(.next_ip);
        c.next(boot);
    }
}
pub fn boot(c: *Cycle) void {
    // Initialization of GPRs, SP, etc. is the responsibility of the startup routine/OS.
    // Reset automatically sets exec_mode to .interrupt_fault in hardware.
    // It's expected that the startup code will eventually use FRET/IRET or IFEX
    // when it wants to start executing user code.
    // It should also ensure that the RSN gets changed since it starts as 0, which is an interrupt RSN
    c.branch(.next_ip, 0);
}

pub fn check_for_pipe_1(c: *Cycle, flags: Flags) void {
    if (flags.zero()) {
        // We are pipe 1
        c.sr_to_j(.one);
        c.literal_to_k(@bitSizeOf(arch.Register_Set_Number));
        c.j_shift_k_to_l(.shl, .fresh, .no_flags);
        c.l_to_sr(.temp_2);
        c.next(begin_init_other_register_sets);
    } else {
        c.force_normal_execution(begin_wait_for_interrupt);
    }
}

pub fn begin_init_other_register_sets(c: *Cycle) void {
    // .temp_2 contains Register_Set_Number.count; subtract 1 to get the max RSN:
    c.sr_to_j(.temp_2);
    c.literal_to_k(1);
    c.j_minus_k_to_l(.fresh, .flags);
    c.l_to_sr(.rs_reserved); // using rs_reserved to avoid interfering with the block transfer when we get down to RSN 0
    c.l_to_rsn();
    c.address(.zero, 1);
    c.virtual_address_to_sr(.one);
    c.next(init_register_set);
}

pub fn init_register_set(c: *Cycle, flags: Flags) void {
    if (flags.zero()) {
        c.literal_to_l(1); // Only Pipe 1 initializes the register sets
        c.l_to_rsn();
        c.force_normal_execution(begin_wait_for_interrupt);
    } else {
        c.zero_to_l();
        c.l_to_sr(.zero);
        c.next(next_registerset);
    }
}

pub fn next_registerset(c: *Cycle) void {
    c.sr_to_j(.rs_reserved);
    c.literal_to_k(1);
    c.j_minus_k_to_l(.fresh, .flags);
    c.l_to_rsn();
    c.l_to_sr(.rs_reserved);
    c.address(.zero, 1);
    c.virtual_address_to_sr(.one);
    c.next(init_register_set);
}

pub fn begin_wait_for_interrupt(c: *Cycle) void {
    c.literal_to_l(@intFromEnum(opcodes.Misc_16.park));
    c.l_to_sr(.temp_1);
    c.next(move_park_to_upper_bits);
}
pub fn move_park_to_upper_bits(c: *Cycle) void {
    c.sr_to_j(.temp_1);
    c.literal_to_k(8);
    c.j_shift_k_to_l(.shl, .fresh, .no_flags);
    c.l_to_sr(.temp_1);
    c.next(wait_for_interrupt);
}
pub fn wait_for_interrupt(c: *Cycle) void {
    const lsb: arch.addr.Virtual.Microcode_Offset.Raw = @intFromEnum(opcodes.LSB.misc_16);
    std.debug.assert(lsb >= 0);
    c.sr_to_j(.temp_1);
    c.literal_to_k(lsb);
    c.j_logic_k_to_l(._or, .fresh, .no_flags);
    c.l_to_dr();
    c.dr_to_ir();
    c.exec_ir_insn();
}


const opcodes = @import("../instructions/opcodes.zig");
const Cycle = @import("../compile/Cycle.zig");
const Encoder = isa.Instruction_Encoding.Encoder;
const Int = placeholders.Int;
const Range = placeholders.Range;
const Reg = placeholders.Reg;
const placeholders = @import("../compile/placeholders.zig");
const Flags = arch.microcode.Flags;
const isa = @import("isa");
const arch = @import("arch");
const std = @import("std");
