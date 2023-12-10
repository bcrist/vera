pub const instructions = .{
    struct { pub const spec = // sat
        \\sat.w x(entry) -> .d x(addr)
        \\sat.r x(entry) -> .d x(addr)
        \\sat.s x(entry) -> .s x(addr)
        \\sat.i x(entry) -> .i x(addr)
        ;
        pub const encoding = .{
            opcode,
            Encoder.shifted(12, Reg(.addr)),
            Encoder.shifted(16, Reg(.entry)),
            Encoder.shifted(20, @as(u12, 0)),
        };
        pub const ij = Reg(.addr);

        fn opcode(suffix: Mnemonic_Suffix) opcodes.Lo12 {
            return switch (suffix) {
                .w => opcodes.Lo12.update_address_translation__data_write,
                .r => opcodes.Lo12.update_address_translation__data_read,
                .s => opcodes.Lo12.update_address_translation__stack,
                .i => opcodes.Lo12.update_address_translation__insn,
                else => unreachable,
            };
        }

        pub fn entry(c: *Cycle, flags: Flags) void {
            if (!flags.kernel()) return c.illegal_instruction();

            c.reg32_to_l();
            c.l_to_sr(.temp_1);
            c.ip_read_to_dr(2, .full);
            c.next_ij_from_decode(.alt);
            c.next(update_translation);
        }

        pub fn update_translation(c: *Cycle, suffix: Mnemonic_Suffix) void {
            const group: arch.hw.addr.translation.Entry_Group = switch (suffix) {
                .w => .data_write,
                .r => .data_read,
                .s => .stack,
                .i => .insn,
                else => unreachable,
            };

            c.reg32_to_l();
            c.update_address_translation_from_l(.temp_1, group);
            c.next(load_and_exec_next_instruction);
        }

        pub fn load_and_exec_next_instruction(c: *Cycle) void {
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = // rat
        \\rat.w .d x(addr), r(mask)
        \\rat.r .d x(addr), r(mask)
        \\rat.s .s x(addr), r(mask)
        \\rat.i .i x(addr), r(mask)
        ;
        pub const encoding = .{
            opcode,
            Encoder.shifted(12, Reg(.addr)),
            Encoder.shifted(16, Reg(.mask)),
            Encoder.shifted(20, @as(u12, 0)),
        };
        pub const ij = Reg(.addr);

        fn opcode(suffix: Mnemonic_Suffix) opcodes.Lo12 {
            return switch (suffix) {
                .w => opcodes.Lo12.invalidate_address_translation__data_write,
                .r => opcodes.Lo12.invalidate_address_translation__data_read,
                .s => opcodes.Lo12.invalidate_address_translation__stack,
                .i => opcodes.Lo12.invalidate_address_translation__insn,
                else => unreachable,
            };
        }

        pub fn entry(c: *Cycle, flags: Flags) void {
            if (!flags.kernel()) return c.illegal_instruction();

            c.reg32_to_l();
            c.l_to_sr(.temp_1);
            c.ip_read_to_dr(2, .full);
            c.next_ij_from_decode(.alt);
            c.next(invalidate_translation);
        }

        pub fn invalidate_translation(c: *Cycle, suffix: Mnemonic_Suffix) void {
            const group: arch.hw.addr.translation.Entry_Group = switch (suffix) {
                .w => .data_write,
                .r => .data_read,
                .s => .stack,
                .i => .insn,
                else => unreachable,
            };

            c.reg32_to_l();
            c.invalidate_address_translation_from_l(.temp_1, group);
            c.next(load_and_exec_next_instruction);
        }

        pub fn load_and_exec_next_instruction(c: *Cycle) void {
            c.load_and_exec_next_insn();
        }
    },
};

const Cycle = @import("Cycle.zig");
const Reg = placeholders.Reg;
const placeholders = @import("placeholders.zig");
const opcodes = @import("opcodes.zig");
const Encoder = isa.Instruction_Encoding.Encoder;
const Mnemonic_Suffix = isa.Mnemonic_Suffix;
const Mnemonic = isa.Mnemonic;
const isa = arch.isa;
const Flags = arch.hw.microcode.Flags;
const arch = @import("lib_arch");
