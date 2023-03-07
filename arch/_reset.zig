const std = @import("std");
const misc = @import("misc");
const uc = @import("microcode");
const ib = @import("instruction_builder.zig");
const cb = @import("cycle_builder.zig");
const physical_address = @import("physical_address");

const assert = std.debug.assert;

const encoding = ib.encoding;
const desc = ib.desc;
const next_cycle = ib.next_cycle;
const conditional_next_cycle = ib.conditional_next_cycle;
const next_cycle_force_normal_execution = ib.next_cycle_force_normal_execution;
const uc_address = ib.uc_address;
const kernel = ib.kernel;
const zero = ib.zero;

const illegal_instruction = cb.illegal_instruction;
const load_and_exec_next_insn = cb.load_and_exec_next_insn;
const block_transfer_to_ram = cb.block_transfer_to_ram;
const reg_to_J = cb.reg_to_J;
const reg32_to_J = cb.reg32_to_J;
const literal_to_K = cb.literal_to_K;
const OB_OA_to_K = cb.OB_OA_to_K;
const reg_to_K = cb.reg_to_K;
const JH_to_LH = cb.JH_to_LH;
const pipe_id_to_L = cb.pipe_id_to_L;
const literal_to_L = cb.literal_to_L;
const literal_to_LL = cb.literal_to_LL;
const zero_to_L = cb.zero_to_L;
const D_to_L = cb.D_to_L;
const D_to_DL = cb.D_to_DL;
const write_from_LL = cb.write_from_LL;
const read_to_D = cb.read_to_D;
const SR_minus_literal_to_L = cb.SR_minus_literal_to_L;
const SRL_logic_literal_to_LL = cb.SRL_logic_literal_to_LL;
const logic_to_LL = cb.logic_to_LL;
const shift_to_LL = cb.shift_to_LL;
const sub_to_LL = cb.sub_to_LL;
const LL_to_reg = cb.LL_to_reg;
const LL_to_RSN = cb.LL_to_RSN;
const LL_to_STAT = cb.LL_to_STAT;
const LL_to_D = cb.LL_to_D;
const L_to_SR = cb.L_to_SR;
const L_to_SR1 = cb.L_to_SR1;
const L_to_SR2 = cb.L_to_SR2;
const branch = cb.branch;
const clear_OB = cb.clear_OB;
const increment_OB = cb.increment_OB;
const enable_sleep = cb.enable_sleep;
const disable_sleep = cb.disable_sleep;
const ZN_from_LL = cb.ZN_from_LL;
const disable_address_translation = cb.disable_address_translation;
const neg_one_to_JH = cb.neg_one_to_JH;
const zero_to_JL = cb.zero_to_JL;
const decode_operands = cb.decode_operands;
const allow_interrupt = cb.allow_interrupt;
const exec_latched_insn = cb.exec_latched_insn;

pub fn _handler_0() void {
    //desc("Initialization/reset entry point");
    assert(uc_address() == @enumToInt(uc.Vectors.reset));

    // The very start is a bit tricky.
    // We can't rely on most registers because the other pipes might be using the same RSN.
    // So the first order of business is to get each pipe into a different registerset,
    // specifically RSN 0, 1, and 2:
    pipe_id_to_L();
    LL_to_RSN();
    // We also store the RSN in .temp_1 so we can use it easily later:
    L_to_SR(.temp_1);
    ZN_from_LL(.fresh);
    next_cycle();

    // Set up the .zero registers in RSN 0, 1, and 2:
    zero_to_L();
    L_to_SR1(.zero);
    L_to_SR2(.zero);
    disable_address_translation();
    conditional_next_cycle(0x200);
}

pub fn _continuation_200() void {
    if (zero()) {
        // We are pipe 0; proceed with copying the boot rom into ram immediately.

        // First we need to configure the block transfer register.
        // We do that by writing a value to an address near misc.Device_Frames.sys_block_transfer_config (0x801_000)
        const base_block_transfer_address = physical_address.fromFrame(@enumToInt(physical_address.DeviceFrame.sys_block_transfer_config));

        literal_to_LL(@intCast(i17, base_block_transfer_address >> 16));
        LL_to_reg(1);
        clear_OB();
        next_cycle();

        literal_to_LL(@truncate(u16, base_block_transfer_address));
        LL_to_reg(0);
        increment_OB();
        next_cycle();

        // 0x800 indicates that we want the address to auto-increment after every block transfer:
        reg32_to_J(0);
        literal_to_K(0x800);
        JH_to_LH();
        logic_to_LL(.jl_or_k, .fresh, .no_flags);
        L_to_SR(.temp_1);
        increment_OB();
        next_cycle();

        // 0x200 indicates that we want to copy from FLASH, not PSRAM
        SRL_logic_literal_to_LL(.temp_1, .jl_or_k, 0x200, .fresh, .no_flags);
        JH_to_LH();
        L_to_SR(.temp_1);
        increment_OB();
        next_cycle();

        // The FLASH address is 0x7E000, and the low 16 bits are the value we write to the register.
        // 0xE000 is generated with the equivalent of @truncate(u16, 0xFFFF0000 >> 3):
        neg_one_to_JH();
        zero_to_JL();
        OB_OA_to_K(); // 3; we can't use literal_to_K(3) because we need to use the literal for the temp_1 offset of 7
        shift_to_LL(.right, .no_flags);
        // The high 3 bits of the FLASH address get added as an offset to the address where we write it:
        write_from_LL(.temp_1, 0x7, .word, .raw);
        next_cycle();

        // Next we set up .temp_1 to be our destination pointer.
        // It will be incremented by 8 before each write, so we start at -8.
        literal_to_L(-8);
        L_to_SR(.temp_1);
        next_cycle();

        // We set R1 to be 1, so that we can decrement R0 without using LITERAL:
        literal_to_L(1);
        LL_to_reg(1);
        next_cycle();

        // We set up R0 to be a counter of how many block transfers are remaining:
        literal_to_L(0x2000);
        LL_to_reg(0);
        ZN_from_LL(.fresh);
        conditional_next_cycle(0x201);
    } else {
        // We are pipe 1 or 2.
        // Subtract 1 from .temp_1 so that next cycle we will know which next cycle.
        SR_minus_literal_to_L(.temp_1, 1, .fresh, .flags);
        L_to_SR1(.temp_1);
        conditional_next_cycle(0x202);
    }
}

pub fn _continuation_201() void {
    // do pipe 0's block transfer
    if (!zero()) {
        // We need to copy more data
        block_transfer_to_ram(.temp_1, 8, .raw);
        reg_to_J(0, .zx);
        reg_to_K(1);
        sub_to_LL(.fresh, .flags);
        LL_to_reg(0);
        conditional_next_cycle(0x201);
    } else {
        // Done with the block transfer!
        // Time to read the reset vector:
        read_to_D(.zero, @offsetOf(misc.ZeropageVectorTable, "pipe_0_reset"), .word, .raw);
        D_to_L(.zx);
        L_to_SR(.next_ip);
        disable_sleep();
        next_cycle();

        branch(.next_ip, 0);

        // Initialization of GPRs, SP, etc. is the responsibility of the startup routine/OS.
        // Reset automatically sets exec_mode to .interrupt_fault in hardware.
        // It's expected that the startup code will eventually use FRET/IRET or IFEX
        // when it wants to start executing user code.
        // It should also ensure that the RSN gets changed since it starts as 0, which is the interrupt RSN
    }
}

pub fn _continuation_202() void {
    if (zero()) {
        // We are pipe 1.
        // Our job is to initialize the .zero registers for all registersets.
        literal_to_L(std.math.maxInt(misc.RegistersetNumber));
        LL_to_RSN();
        L_to_SR(.temp_1);
        ZN_from_LL(.fresh);
        conditional_next_cycle(0x203);
    } else {
        // We are pipe 2.
        wait_for_interrupt();
    }
}

pub fn _continuation_203() void {
    if (!zero()) {
        zero_to_L();
        L_to_SR1(.zero);
        L_to_SR2(.zero);
        next_cycle();

        SR_minus_literal_to_L(.temp_1, 1, .fresh, .flags);
        LL_to_RSN();
        L_to_SR1(.temp_1);
        conditional_next_cycle(0x203);
    } else {
        pipe_id_to_L();
        LL_to_RSN();
        next_cycle();

        wait_for_interrupt();
    }
}

fn wait_for_interrupt() void {
    literal_to_LL(0x0001);
    LL_to_D();
    D_to_DL();
    next_cycle_force_normal_execution();

    exec_latched_insn();
}

pub fn _018B() void {
    encoding(.SLEEP, .{});
    desc("Enable sleep mode");

    if (!kernel()) {
        illegal_instruction();
        return;
    }

    enable_sleep();
    load_and_exec_next_insn(2);
}

pub fn _018A() void {
    encoding(.UNSLEEP, .{});
    desc("Disable sleep mode");

    if (!kernel()) {
        illegal_instruction();
        return;
    }

    disable_sleep();
    load_and_exec_next_insn(2);
}
