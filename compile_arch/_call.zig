pub const instructions = .{
    struct { pub const spec = "call .i ip + (imm)";
        pub const encoding = .{
            opcodes.Lo8.ip_relative_call__forward,
            Encoder.shifted(8, Int(.imm, u16)),
        };

        pub const entry = read_imm;

        pub fn read_imm(c: *Cycle) void {
            c.ip_read_to_d(1, .word);
            c.d_to_l(.zx);
            c.l_to_sr(.temp_1);
            c.next(compute_next_ip);
        }

        pub fn compute_next_ip(c: *Cycle) void {
            c.sr_to_j(.ip);
            c.srl_to_k(.temp_1);
            c.j_plus_k_to_l(.zx, .fresh, .no_flags);
            c.l_to_sr(.next_ip);
            c.next(save_return_address_and_branch);
        }

        pub fn save_return_address_and_branch(c: *Cycle) void {
            c.sr_to_j(.ip);
            c.literal_to_k(3);
            c.j_plus_k_to_l(.sx, .fresh, .no_flags);
            c.l_to_sr(.rp);
            c.branch(.next_ip, 0);
        }
    },
    struct { pub const spec = "call .i ip + (imm)";
        pub const encoding = .{
            opcodes.Lo8.ip_relative_call__back,
            Encoder.shifted(8, Range(.imm, -0x10000, -1)),
        };

        pub const entry = read_imm;

        pub fn read_imm(c: *Cycle) void {
            c.ip_read_to_d(1, .word);
            c.d_to_l(.zx);
            c.l_to_sr(.temp_1);
            c.next(compute_next_ip);
        }

        pub fn compute_next_ip(c: *Cycle) void {
            c.sr_to_j(.ip);
            c.srl_to_k(.temp_1);
            c.j_plus_k_to_l(._1x, .fresh, .no_flags);
            c.l_to_sr(.next_ip);
            c.next(save_return_address_and_branch);
        }

        pub fn save_return_address_and_branch(c: *Cycle) void {
            c.sr_to_j(.ip);
            c.literal_to_k(3);
            c.j_plus_k_to_l(.zx, .fresh, .no_flags);
            c.l_to_sr(.rp);
            c.branch(.next_ip, 0);
        }
    },
    struct { pub const spec = "call .i (imm)";
        pub const encoding = .{
            opcodes.Lo16.absolute_call,
            Encoder.shifted(16, Int(.imm, u32)),
        };

        pub const entry = read_imm_hi;

        pub fn read_imm_hi(c: *Cycle) void {
            c.ip_read_to_d(4, .word);
            c.d_to_l(.zx);
            c.l_to_sr(.temp_1);
            c.next(read_imm_lo);
        }

        pub fn read_imm_lo(c: *Cycle) void {
            c.ip_read_to_d(2, .word);
            c.d_to_ll();
            c.srl_to_lh(.temp_1);
            c.l_to_sr(.next_ip);
            c.next(save_return_address_and_branch);
        }

        pub fn save_return_address_and_branch(c: *Cycle) void {
            c.sr_to_j(.ip);
            c.literal_to_k(6);
            c.j_plus_k_to_l(.zx, .fresh, .no_flags);
            c.l_to_sr(.rp);
            c.branch(.next_ip, 0);
        }
    },
    struct { pub const spec = // call .i x(r)
        \\call .i x(r)
        ;
        pub const encoding = .{
            opcodes.Lo12.register_call,
            Encoder.shifted(12, Reg(.r)),
        };
        pub const ij = Reg(.r);

        pub fn entry(c: *Cycle) void {
            c.reg32_to_l();
            c.l_to_sr(.next_ip);
            c.next(save_return_address_and_branch);
        }

        pub fn save_return_address_and_branch(c: *Cycle) void {
            c.sr_to_j(.ip);
            c.literal_to_k(2);
            c.j_plus_k_to_l(.zx, .fresh, .no_flags);
            c.l_to_sr(.rp);
            c.branch(.next_ip, 0);
        }
    },
    struct { pub const spec = "ret";
        pub const encoding = opcodes.Lo8.call_return;
        pub fn entry(c: *Cycle) void {
            c.zero_to_l();
            c.l_to_sr(.rp);
            c.branch(.rp, 0);
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
const isa = arch.isa;
const arch = @import("lib_arch");
