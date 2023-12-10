pub const instructions = .{
    struct { pub const slot = hw.microcode.Slot.reset;
        pub const entry = reset;

        pub fn reset(c: *Cycle) void {
            // The very start is a bit tricky.
            // We can't rely on most registers because the other pipes might be using the same RSN.
            // So the first order of business is to get each pipe into a different registerset,
            // specifically RSN 0, 1, and 2:
            c.zero_to_lh();
            c.pipeline_id_to_ll();
            c.ll_to_rsn();
            // We also store the RSN in .temp_1 so we can use it easily later:
            c.l_to_sr(.temp_1);
            c.zn_flags_from_ll();
            c.next_iw(0);
            c.next(init_zero_registers);
        }

        pub fn init_zero_registers(c: *Cycle) void {
            c.zero_to_l();
            c.ll_to_reg();
            c.l_to_sr(.zero);
            c.address(.zero, 1);
            c.virtual_address_to_sr(.one);
            c.disable_address_translation();
            c.next_ik_bit(@intCast(hw.addr.Physical.block_transfer_controller_0.raw() >> 16));
            c.next(check_for_pipe_0);
        }

        pub fn check_for_pipe_0(c: *Cycle, flags: Flags) void {
            if (flags.zero()) {
                // First we need to configure the block transfer register.
                c.sr_to_j(.one);
                c.ik_bit_to_k(); // high bits of block transfer register address
                c.jl_times_k__swap_result_halves_to_l(.zx, .zx, .fresh, .no_flags);
                c.l_to_sr(.temp_1);
                c.next_ik_bit(@truncate(hw.addr.Physical.block_transfer_controller_0.raw()));
                c.next(compute_block_transfer_register_low);
            } else {
                c.next_ik_bit(hw.RSN.count);
                c.next(pipe_n_begin_init_constant_registers);
            }
        }

        pub fn compute_block_transfer_register_low(c: *Cycle) void {
            c.srh_to_lh(.temp_1);
            c.ik_bit_to_ll(); // low bits of block transfer register address
            c.l_to_sr(.temp_1);
            c.next_iw_xor1();
            c.next(read_block_transfer_register);
        }

        // detect if the block transfer unit exists before trying to copy all the boot memory
        pub fn read_block_transfer_register(c: *Cycle) void {
            c.read_to_d(.temp_1, 0, .word, .raw);
            c.d_to_l(.zx);
            c.next_ij(1);
            c.ll_to_reg(); // R1
            c.next(block_transfer_signature_times_three);
        }

        pub fn block_transfer_signature_times_three(c: *Cycle) void {
            c.reg_to_jl(); // R1
            c.literal_to_k(3);
            c.jl_times_k_to_ll(.zx, .zx, .fresh, .no_flags);
            c.ll_to_reg(); // R1
            c.next(block_transfer_signature_plus_one);
        }

        pub fn block_transfer_signature_plus_one(c: *Cycle) void {
            c.reg_to_jl(); // R1
            c.literal_to_k(1);
            c.jl_plus_k_to_ll(.fresh, .flags);
            c.next(check_block_transfer_signature);
        }

        pub fn check_block_transfer_signature(c: *Cycle, flags: Flags) void {
            if (flags.zero()) {
                // signature match; continue with the block transfer
                c.literal_to_ll(0xA);
                c.ll_to_reg();
                c.next(compute_block_transfer_flags);
            } else {
                // block transfers not supported; assume memory is already initialized somehow
                c.zero_to_l();
                c.zn_flags_from_ll(); // ensure Z flag is set so we skip the transfer loop
                c.next(block_transfer_loop);
            }
        }

        pub fn compute_block_transfer_flags(c: *Cycle) void {
            // 0x800 indicates that we want the address to auto-increment after every block transfer:
            // 0x200 indicates that we want to copy from FLASH, not PSRAM
            c.reg_to_jl(); // R1
            c.zero_to_jh();
            c.literal_to_k(8);
            c.jl_shift_k4_to_ll(.left, .fresh, .no_flags);
            c.ll_to_reg(); // R1 <- 0xA00
            c.next(update_block_transfer_address);
        }

        pub fn update_block_transfer_address(c: *Cycle) void {
            c.sr_to_j(.temp_1);
            c.reg_to_k(); // R1
            c.j_plus_k_to_l(.zx, .fresh, .no_flags);
            c.l_to_sr(.temp_1);
            c.next(compute_block_transfer_register_data);
        }

        pub fn compute_block_transfer_register_data(c: *Cycle) void {
            // The FLASH boot sector address is 0x7E000
            // and the low 16 bits are the value we write to the block transfer register.
            // 0xE000 is generated with the equivalent of @as(u16, @truncate(0xFFFF0000 >> 3)):
            c.neg_one_to_jh();
            c.zero_to_jl();
            c.literal_to_k(3);
            c.jl_shift_k4_to_ll(.right, .fresh, .no_flags);
            c.ll_to_reg(); // R1
            c.next(write_block_transfer_register);
        }

        pub fn write_block_transfer_register(c: *Cycle) void {
            // The high 3 bits of the FLASH address get added as an offset to the address where we write it:
            c.reg32_to_j(); // X1
            c.write_from_ll(.temp_1, 0x7, .word, .raw);
            c.next(setup_write_pointer);
        }

        pub fn setup_write_pointer(c: *Cycle) void {
            // .temp_1 will be incremented by 8 before each write, so we start at -8.
            c.literal_to_l(-8);
            c.l_to_sr(.temp_1);
            c.next_ik_bit(0x2000);
            c.next(setup_transfer_counter);
        }

        pub fn setup_transfer_counter(c: *Cycle) void {
            c.ik_bit_to_ll(); // 0x2000
            c.ll_to_reg(); // R1
            c.zn_flags_from_ll();
            c.next(block_transfer_loop);
        }

        pub fn block_transfer_loop(c: *Cycle, flags: Flags) void {
            if (!flags.zero()) {
                c.block_transfer_to_ram(.temp_1, 8, .raw);
                c.reg_to_jl(); // R1, transfers remaining
                c.zero_to_jh();
                c.srl_to_k(.one);
                c.jl_minus_k_to_ll(.fresh, .flags);
                c.ll_to_reg(); // R1
                c.next(block_transfer_loop);
            } else {
                // Done with the block transfer!
                // Time to read the reset vector:
                c.read_to_d(.zero, @offsetOf(arch.Vector_Table, "reset"), .word, .raw);
                c.d_to_l(.zx);
                c.l_to_sr(.next_ip);
                c.next(boot);
            }
        }

        pub fn boot(c: *Cycle) void {
            c.branch(.next_ip, 0);

            // Initialization of GPRs, SP, etc. is the responsibility of the startup routine/OS.
            // Reset automatically sets exec_mode to .interrupt_fault in hardware.
            // It's expected that the startup code will eventually use FRET/IRET or IFEX
            // when it wants to start executing user code.
            // It should also ensure that the RSN gets changed since it starts as 0, which is the interrupt RSN
        }

        pub fn pipe_n_begin_init_constant_registers(c: *Cycle) void {
            c.ik_bit_to_ll();
            c.ll_to_rsn();
            c.l_to_sr(.temp_1);
            c.zn_flags_from_ll();
            c.next(pipe_n_init_constant_registers);
        }

        pub fn pipe_n_init_constant_registers(c: *Cycle, flags: Flags) void {
            if (!flags.zero()) {
                c.zero_to_l();
                c.l_to_sr(.zero);
                c.address(.zero, 1);
                c.virtual_address_to_sr(.one);
                c.next(pipe_n_next_registerset);
            } else {
                c.pipeline_id_to_ll();
                c.ll_to_rsn();
                c.force_normal_execution(wait_for_interrupt);
            }
        }

        pub fn pipe_n_next_registerset(c: *Cycle) void {
            c.sr_to_j(.temp_1);
            c.literal_to_k(1);
            c.j_minus_k_to_l(.zx, .fresh, .flags);
            c.ll_to_rsn();
            c.l_to_sr(.temp_1);
            c.next(pipe_n_init_constant_registers);
        }

        pub fn wait_for_interrupt(c: *Cycle) void {
            c.decode_and_exec_dr(.normal);
        }
    },
};

const Cycle = @import("Cycle.zig");
const Encoder = isa.Instruction_Encoding.Encoder;
const Int = placeholders.Int;
const Range = placeholders.Range;
const Reg = placeholders.Reg;
const placeholders = @import("placeholders.zig");
const opcodes = @import("opcodes.zig");
const Flags = hw.microcode.Flags;
const hw = arch.hw;
const isa = arch.isa;
const arch = @import("lib_arch");
