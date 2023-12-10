pub const instructions = .{
    struct { pub const spec =
        \\mcr x(bytes) .signed, .d bp -> .d rp
        \\si  x(bytes) .signed, .d bp -> .d rp
        \\so  x(bytes) .signed, .d bp -> .d rp
        ;
        pub const encoding = .{
            opcode,
            Encoder.shifted(12, Reg(.bytes)),
        };
        pub const ij = Reg(.bytes);
        pub const iw = Reg(.bytes);

        fn opcode(mnemonic: isa.Mnemonic) opcodes.Lo12 {
            return switch (mnemonic) {
                .mcr => .memcpy_reverse,
                .si => .stream_in,
                .so => .stream_out,
                else => unreachable,
            };
        }

        pub fn entry(c: *Cycle) void {
            c.reg32_to_j();
            c.literal_to_k(-2);
            c.j_plus_k_to_l(.sx, .fresh, .flags);
            c.next(initial_check_for_2_bytes_remaining);
        }

        pub fn initial_check_for_2_bytes_remaining(c: *Cycle, flags: Flags) void {
            if (flags.negative()) {
                // length == 0 or 1 bytes
                c.reg32_to_j();
                c.literal_to_k(-1);
                c.j_plus_k_to_l(.sx, .fresh, .flags);
                c.next(initial_check_for_1_byte_remaining);
            } else {
                c.read_to_dr(.bp, 0, .full, .data);
                c.reg32_to_j();
                c.literal_to_k(-2);
                c.j_plus_k_to_l(.sx, .fresh, .flags);
                c.l_to_reg32();
                if (flags.zero()) {
                    // length == 2 bytes
                    c.next(write_single_word);
                } else {
                    // length > 2 bytes
                    c.next(write_initial_word);
                }
            }
        }

        pub fn initial_check_for_1_byte_remaining(c: *Cycle, flags: Flags) void {
            c.reg32_to_j();
            c.literal_to_k(-2);
            c.j_plus_k_to_l(.sx, .fresh, .flags);
            c.l_to_reg32();
            if (flags.negative()) {
                // length == 0
                c.load_and_exec_next_insn();
            } else {
                // length == 1 byte
                c.read_to_dr(.bp, 0, .low, .data);
                c.next(write_single_byte);
            }
        }

        pub fn write_single_word(c: *Cycle) void {
            c.write_from_dr(.rp, 0, .word, .data);
            c.next(load_and_exec_next_insn);
        }

        pub fn write_single_byte(c: *Cycle) void {
            c.write_from_dr(.rp, 0, .byte, .data);
            c.next(load_and_exec_next_insn);
        }

        pub fn write_initial_word(c: *Cycle) void {
            c.write_from_dr(.rp, 0, .word, .data);
            c.reg32_to_j();
            c.literal_to_k(-2);
            c.j_plus_k_to_l(.sx, .fresh, .flags);
            c.next(read);
        }

        pub fn read(c: *Cycle, flags: Flags, mnemonic: isa.Mnemonic) void {
            c.read_to_dr(.bp, if (mnemonic == .si) 0 else 2, if (flags.negative()) .low else .full, .data);
            c.virtual_address_to_sr(.bp);
            c.reg32_to_j();
            c.literal_to_k(-2);
            c.j_plus_k_to_l(.sx, .fresh, .flags);
            c.l_to_reg32();
            if (flags.zero()) {
                // exactly 2 bytes left
                c.next(write_final_word);
            } else if (flags.negative()) {
                // exactly 1 byte left
                c.next(write_final_byte);
            } else {
                // > 2 bytes left
                c.next(write_word);
            }
        }

        pub fn write_word(c: *Cycle, mnemonic: isa.Mnemonic) void {
            c.write_from_dr(.rp, if (mnemonic == .so) 0 else 2, .word, .data);
            c.virtual_address_to_sr(.rp);
            c.reg32_to_j();
            c.literal_to_k(-2);
            c.j_plus_k_to_l(.sx, .fresh, .flags);
            c.allow_interrupt();
            c.next(read);
        }

        pub fn write_final_word(c: *Cycle, mnemonic: isa.Mnemonic) void {
            c.write_from_dr(.rp, if (mnemonic == .so) 0 else 2, .word, .data);
            c.virtual_address_to_sr(.rp);
            c.next(load_and_exec_next_insn);
        }

        pub fn write_final_byte(c: *Cycle, mnemonic: isa.Mnemonic) void {
            c.write_from_dr(.rp, if (mnemonic == .so) 0 else 2, .byte, .data);
            c.virtual_address_to_sr(.rp);
            c.next(load_and_exec_next_insn);
        }

        pub const load_and_exec_next_insn = Cycle.load_and_exec_next_insn;
    },
    struct { pub const spec =
        \\mcrb x(bytes) .signed, .d bp -> .d rp
        \\sib  x(bytes) .signed, .d bp -> .d rp
        \\sob  x(bytes) .signed, .d bp -> .d rp
        ;
        pub const encoding = .{
            opcode,
            Encoder.shifted(12, Reg(.bytes)),
        };
        pub const ij = Reg(.bytes);
        pub const iw = Reg(.bytes);

        fn opcode(mnemonic: isa.Mnemonic) opcodes.Lo12 {
            return switch (mnemonic) {
                .mcrb => .memcpy_reverse_bytewise,
                .sib => .stream_in_bytewise,
                .sob => .stream_out_bytewise,
                else => unreachable,
            };
        }

        pub fn entry(c: *Cycle) void {
            c.reg32_to_j();
            c.srl_to_k(.one);
            c.j_minus_k_to_l(.sx, .fresh, .flags);
            c.next(initial_check_for_1_byte_remaining);
            c.l_to_reg32();
        }

        pub fn initial_check_for_1_byte_remaining(c: *Cycle, flags: Flags) void {
            if (flags.negative()) {
                // length == 0
                c.load_and_exec_next_insn();
            } else {
                c.read_to_dr(.bp, 0, .low, .data);
                if (flags.zero()) {
                    // length == 1 byte
                    c.next(write_single);
                } else {
                    // length > 1 byte
                    c.next(write_initial);
                }
            }
        }

        pub fn write_single(c: *Cycle) void {
            c.write_from_dr(.rp, 0, .byte, .data);
            c.next(load_and_exec_next_insn);
        }

        pub fn write_initial(c: *Cycle) void {
            c.write_from_dr(.rp, 0, .byte, .data);
            c.reg32_to_j();
            c.srl_to_k(.one);
            c.j_minus_k_to_l(.sx, .fresh, .flags);
            c.next(read);
        }

        pub fn read(c: *Cycle, flags: Flags, mnemonic: isa.Mnemonic) void {
            c.read_to_dr(.bp, if (mnemonic == .si) 0 else 1, .low, .data);
            c.virtual_address_to_sr(.bp);
            c.reg32_to_j();
            c.srl_to_k(.one);
            c.j_minus_k_to_l(.sx, .fresh, .flags);
            c.l_to_reg32();
            if (flags.zero()) {
                // exactly 1 byte left
                c.next(write_final);
            } else {
                // > 2 bytes left
                c.next(write);
            }
        }

        pub fn write(c: *Cycle, mnemonic: isa.Mnemonic) void {
            c.write_from_dr(.rp, if (mnemonic == .so) 0 else 1, .byte, .data);
            c.virtual_address_to_sr(.rp);
            c.reg32_to_j();
            c.srl_to_k(.one);
            c.j_minus_k_to_l(.sx, .fresh, .flags);
            c.allow_interrupt();
            c.next(read);
        }

        pub fn write_final(c: *Cycle, mnemonic: isa.Mnemonic) void {
            c.write_from_dr(.rp, if (mnemonic == .so) 0 else 1, .byte, .data);
            c.virtual_address_to_sr(.rp);
            c.next(load_and_exec_next_insn);
        }

        pub const load_and_exec_next_insn = Cycle.load_and_exec_next_insn;
    },
    struct { pub const spec = "mcf x(bytes) .signed, .d bp -> .d rp";
    
        pub const encoding = .{
            opcodes.Lo12.memcpy_forward,
            Encoder.shifted(12, Reg(.bytes)),
        };
        pub const ij = Reg(.bytes);
        pub const iw = Reg(.bytes);

        pub fn entry(c: *Cycle) void {
            c.reg32_to_j();
            c.literal_to_k(-2);
            c.j_plus_k_to_l(.sx, .fresh, .flags);
            c.next(initial_check_for_2_bytes_remaining);
        }

        pub fn initial_check_for_2_bytes_remaining(c: *Cycle, flags: Flags) void {
            if (flags.negative()) {
                // length == 0 or 1 bytes
                c.reg32_to_j();
                c.literal_to_k(-1);
                c.j_plus_k_to_l(.sx, .fresh, .flags);
                c.l_to_reg32();
                c.next(initial_check_for_1_byte_remaining);
            } else {
                c.read_to_dr(.bp, -2, .full, .data);
                c.virtual_address_to_sr(.bp);
                c.reg32_to_j();
                c.literal_to_k(-2);
                c.j_plus_k_to_l(.sx, .fresh, .flags);
                c.l_to_reg32();
                if (flags.zero()) {
                    // length == 2 bytes
                    c.next(write_final_word);
                } else {
                    // length > 2 bytes
                    c.next(write_word);
                }
            }
        }

        pub fn initial_check_for_1_byte_remaining(c: *Cycle, flags: Flags) void {
            c.reg32_to_j();
            c.literal_to_k(-1);
            c.j_plus_k_to_l(.sx, .fresh, .flags);
            c.l_to_reg32();
            if (flags.negative()) {
                // length == 0
                c.load_and_exec_next_insn();
            } else {
                // length == 1 byte
                c.read_to_dr(.bp, -1, .low, .data);
                c.virtual_address_to_sr(.bp);
                c.next(write_final_byte);
            }
        }

        pub fn read(c: *Cycle, flags: Flags) void {
            if (flags.negative()) {
                c.read_to_dr(.bp, -1, .low, .data);
                c.literal_to_k(-1);
            } else {
                c.read_to_dr(.bp, -2, .full, .data);
                c.literal_to_k(-2);
            }
            c.virtual_address_to_sr(.bp);
            c.reg32_to_j();
            c.j_plus_k_to_l(.sx, .fresh, .flags);
            c.l_to_reg32();
            if (flags.zero()) {
                // exactly 2 bytes left
                c.next(write_final_word);
            } else if (flags.negative()) {
                // exactly 1 byte left
                c.next(write_final_byte);
            } else {
                // > 2 bytes left
                c.next(write_word);
            }
        }

        pub fn write_word(c: *Cycle) void {
            c.write_from_dr(.rp, -2, .word, .data);
            c.virtual_address_to_sr(.rp);
            c.reg32_to_j();
            c.literal_to_k(-2);
            c.j_plus_k_to_l(.sx, .fresh, .flags);
            c.allow_interrupt();
            c.next(read);
        }

        pub fn write_final_word(c: *Cycle) void {
            c.write_from_dr(.rp, -2, .word, .data);
            c.virtual_address_to_sr(.rp);
            c.next(load_and_exec_next_insn);
        }

        pub fn write_final_byte(c: *Cycle) void {
            c.write_from_dr(.rp, -1, .byte, .data);
            c.virtual_address_to_sr(.rp);
            c.literal_to_k(-1);
            c.reg32_to_j();
            c.j_plus_k_to_l(.sx, .fresh, .flags);
            c.l_to_reg32();
            c.next(load_and_exec_next_insn);
        }

        pub const load_and_exec_next_insn = Cycle.load_and_exec_next_insn;
    },
    struct { pub const spec = "mcfb x(bytes) .signed, .d bp -> .d rp";
        pub const encoding = .{
            opcodes.Lo12.memcpy_forward_bytewise,
            Encoder.shifted(12, Reg(.bytes)),
        };
        pub const ij = Reg(.bytes);
        pub const iw = Reg(.bytes);

        pub fn entry(c: *Cycle) void {
            c.reg32_to_j();
            c.srl_to_k(.one);
            c.j_minus_k_to_l(.sx, .fresh, .flags);
            c.next(read);
        }

        pub fn read(c: *Cycle, flags: Flags) void {
            c.reg32_to_j();
            c.srl_to_k(.one);
            c.j_minus_k_to_l(.sx, .fresh, .flags);
            c.l_to_reg32();
            if (flags.negative()) {
                // length == 0
                c.load_and_exec_next_insn();
            } else {
                c.read_to_dr(.bp, -1, .low, .data);
                c.virtual_address_to_sr(.bp);
                if (flags.zero()) {
                    // length == 1 byte
                    c.next(write_final);
                } else {
                    // length > 1 byte
                    c.next(write);
                }
            }
        }

        pub fn write(c: *Cycle) void {
            c.write_from_dr(.rp, -1, .byte, .data);
            c.virtual_address_to_sr(.rp);
            c.reg32_to_j();
            c.srl_to_k(.one);
            c.j_minus_k_to_l(.sx, .fresh, .flags);
            c.allow_interrupt();
            c.next(read);
        }

        pub fn write_final(c: *Cycle) void {
            c.write_from_dr(.rp, -1, .byte, .data);
            c.virtual_address_to_sr(.rp);
            c.next(load_and_exec_next_insn);
        }

        pub const load_and_exec_next_insn = Cycle.load_and_exec_next_insn;
    },

    struct { pub const spec = "bld x(bytes) .signed -> .d bp";
        //Load block(s) from FLASH or PSRAM to RAM
        // x(bytes) indicates the number of bytes remaining to be copied (should be a multiple of 8 and > 0)
        // x(bytes) will be decremented by 8 each cycle; operation ends when zero or negative
        // BP points to just before the first byte of RAM to write to (will be incremented by 8 before each cycle).
        // Block source must be configured beforehand by writing to the configuration port.
        // Only one pipes 1 and 3 may perform block transfers.
        pub const encoding = .{
            opcodes.Lo12.block_load_to_ram,
            Encoder.shifted(12, Reg(.bytes)),
        };
        pub const ij = Reg(.bytes);
        pub const iw = Reg(.bytes);

        pub fn entry(c: *Cycle, flags: Flags) void {
            if (!flags.kernel()) return c.illegal_instruction();
            c.reg32_to_j();
            c.literal_to_k(8);
            c.j_minus_k_to_l(.zx, .fresh, .flags);
            c.l_to_reg32();
            c.load_next_insn();
            c.next(transfer);
        }

        pub fn transfer(c: *Cycle, flags: Flags) void {
            c.block_transfer_to_ram(.bp, 8, .data);
            if (flags.positive()) {
                c.reg32_to_j();
                c.literal_to_k(8);
                c.j_minus_k_to_l(.zx, .fresh, .flags);
                c.l_to_reg32();
                c.next(transfer);
            } else {
                c.exec_next_insn();
            }
        }
    },

    struct { pub const spec = "bst x(bytes) .signed, .d bp";
        //Store block(s) from RAM into FLASH or PSRAM
        // x(bytes) indicates the number of bytes remaining to be copied (should be a multiple of 8 and > 0)
        // x(bytes) will be decremented by 8 each cycle; operation ends when zero or negative
        // BP points to just before the first byte of RAM to read (will be incremented by 8 before each cycle).
        // Block destination must be configured beforehand by writing to the configuration port.
        // Only one pipes 1 and 3 may perform block transfers.
        pub const encoding = .{
            opcodes.Lo12.block_store_from_ram,
            Encoder.shifted(12, Reg(.bytes)),
        };
        pub const ij = Reg(.bytes);
        pub const iw = Reg(.bytes);

        pub fn entry(c: *Cycle, flags: Flags) void {
            if (!flags.kernel()) return c.illegal_instruction();
            c.reg32_to_j();
            c.literal_to_k(8);
            c.j_minus_k_to_l(.zx, .fresh, .flags);
            c.l_to_reg32();
            c.load_next_insn();
            c.next(transfer);
        }

        pub fn transfer(c: *Cycle, flags: Flags) void {
            c.block_transfer_from_ram(.bp, 8, .data);
            if (flags.positive()) {
                c.reg32_to_j();
                c.literal_to_k(8);
                c.j_minus_k_to_l(.zx, .fresh, .flags);
                c.l_to_reg32();
                c.next(transfer);
            } else {
                c.exec_next_insn();
            }
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
const Parameter = isa.Parameter;
const Flags = hw.microcode.Flags;
const hw = arch.hw;
const isa = arch.isa;
const arch = @import("lib_arch");
const std = @import("std");
