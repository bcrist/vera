pub const insn_decode = @import("arch/insn_decode.zig");
pub const microcode = @import("arch/microcode.zig");
pub const addr = @import("arch/addr.zig");
pub const Control_Signals = @import("arch/Control_Signals.zig");
pub const Control_Signal = std.meta.FieldEnum(Control_Signals);

pub const Pipeline = enum (u2) {
    zero = 0,
    one = 1,
    two = 2,
    three = 3,

    pub inline fn init(raw_value: Raw) Pipeline {
        return @enumFromInt(raw_value);
    }

    pub inline fn raw(self: Pipeline) Raw {
        return @intFromEnum(self);
    }

    pub const format = fmt.format_enum;

    pub const Raw = std.meta.Tag(Pipeline); 
    pub const count = std.math.maxInt(Raw) + 1;

    pub const Stage = enum (u2) {
        decode = 0,
        setup = 1,
        compute = 2,
        transact = 3,

        pub inline fn init(raw_value: Stage.Raw) Stage {
            return @enumFromInt(raw_value);
        }

        pub inline fn raw(self: Stage) Stage.Raw {
            return @intFromEnum(self);
        }

        pub const format = fmt.format_enum;

        pub const Raw = std.meta.Tag(Stage); 
        pub const count = std.math.maxInt(Stage.Raw) + 1;
    };
};

pub const Execution_Mode = enum (u2) {
    normal = 0,
    interrupt = 1,
    fault = 2,
    interrupt_fault = 3,

    pub inline fn init(raw_value: Raw) Execution_Mode {
        return @enumFromInt(raw_value);
    }

    pub inline fn raw(self: Execution_Mode) Raw {
        return @intFromEnum(self);
    }

    pub inline fn is_fault(self: Execution_Mode) bool {
        return switch (self) {
            .normal, .interrupt => false,
            .fault, .interrupt_fault => true,
        };
    }

    pub inline fn is_interrupt(self: Execution_Mode) bool {
        return switch (self) {
            .normal, .fault => false,
            .interrupt, .interrupt_fault => true,
        };
    }

    pub const format = fmt.format_enum;

    pub const Raw = std.meta.Tag(Execution_Mode); 
};

pub const Power_Mode = enum (u1) {
    run = 0,
    sleep = 1,

    pub inline fn init(raw_value: Raw) Power_Mode {
        return @enumFromInt(raw_value);
    }

    pub inline fn raw(self: Power_Mode) Raw {
        return @intFromEnum(self);
    }

    pub const format = fmt.format_enum;

    pub const Raw = std.meta.Tag(Power_Mode); 
};

pub const Sequencer_Op = enum (u2) {
    next_uop = 0,
    next_instruction = 1,
    next_uop_force_normal = 2,
    fault_return = 3,

    pub inline fn init(raw_value: Raw) Sequencer_Op {
        return @enumFromInt(raw_value);
    }

    pub inline fn raw(self: Sequencer_Op) Raw {
        return @intFromEnum(self);
    }

    pub const format = fmt.format_enum;

    pub const Raw = std.meta.Tag(Sequencer_Op); 
};

pub const Special_Op = enum (u3) {
    none = 0,
    set_guard = 1,
    check_guard = 2,
    fault_on_overflow = 3,
    block_transfer = 4,
    load_rsn_from_l = 5,
    toggle_rsn = 6,
    trigger_fault = 7,

    pub inline fn init(raw_value: Raw) Special_Op {
        return @enumFromInt(raw_value);
    }

    pub inline fn raw(self: Special_Op) Raw {
        return @intFromEnum(self);
    }

    pub const format = fmt.format_enum;

    pub const Raw = std.meta.Tag(Special_Op); 
};

pub const Register_Set_Number = enum (u7) {
    interrupt_pipe_0 = 0,
    interrupt_pipe_1 = 1,
    interrupt_pipe_2 = 2,
    interrupt_pipe_3 = 3,
    _,

    pub inline fn init(raw_value: Raw) Register_Set_Number {
        return @enumFromInt(raw_value);
    }

    pub inline fn raw(self: Register_Set_Number) Raw {
        return @intFromEnum(self);
    }

    pub const format = fmt.format_enum;

    pub const Raw = std.meta.Tag(Register_Set_Number);
    pub const count = std.math.maxInt(Raw) + 1;
    pub const msb = init(0x40);
};

pub const Register_Index = u5;
pub const register_count = std.math.maxInt(Register_Index) + 1;

pub const Write_Index = enum (Register_Index) {
    _,

    pub inline fn init(raw_value: Raw) Write_Index {
        return @enumFromInt(raw_value);
    }

    pub inline fn raw(self: Write_Index) Raw {
        return @intFromEnum(self);
    }

    pub const format = fmt.format_enum;

    pub const Raw = std.meta.Tag(Write_Index);
    pub const max = std.math.maxInt(Raw);
};

pub const Write_Index_Offset = enum (Register_Index) {
    _,

    pub inline fn init(raw_value: Raw) Write_Index_Offset {
        return @enumFromInt(raw_value);
    }

    pub inline fn raw(self: Write_Index_Offset) Raw {
        return @intFromEnum(self);
    }

    pub const format = fmt.format_enum;

    pub const Raw = std.meta.Tag(Write_Index_Offset);
    pub const max = std.math.maxInt(Raw);
};

pub const SR1_Index = enum (u4) {
    one = 0,            // 0x0000_0001 (must never be overwritten)
    rp = 1,             // return pointer
    sp = 2,             // stack pointer
    bp = 3,             // stack base pointer
    // 4 unused
    // 5 unused
    // 6 unused
    temp_1 = 7,
    int_stat = 8,       // Upon entering an interrupt handler, RSN is set to the current pipe number.  The old RSN is saved here, along with the rest of the status register.
    fault_stat = 9,     // Upon entering a fault handler, before toggling the RSN, the status register is saved here, including WI and UCA.
    fault_dr = 10,      // Upon entering a fault handler, before toggling the RSN, the current DR is saved here.
    fault_ir = 11,      // Upon entering a fault handler, before toggling the RSN, the current IR (and upper half of DR) is saved here.
    _,

    pub inline fn init(raw_value: Raw) SR1_Index {
        return @enumFromInt(raw_value);
    }

    pub inline fn raw(self: SR1_Index) Raw {
        return @intFromEnum(self);
    }

    pub const format = fmt.format_enum;

    pub const Raw = std.meta.Tag(SR1_Index);
    pub const count = std.math.maxInt(Raw) + 1;
};

pub const SR2_Index = enum (u4) {
    zero = 0,           // 0x0000_0000 (must never be overwritten)
    ip = 1,             // instruction pointer
    asn = 2,            // address space number
    next_ip = 3,        // when next instruction is loaded before the last cycle of the current instruction, the next IP is kept here.
    kxp = 4,            // kernel context pointer
    uxp = 5,            // user context pointer
    // 6 unused
    temp_2 = 7,
    rs_reserved = 8,    // used only by STRS/LDRS and not stored in Context_State.  If a fault occurs during one of these instructions, the faulting registerset cannot be STRS/LDRS'd and then resumed.
    _,

    pub inline fn init(raw_value: Raw) SR2_Index {
        return @enumFromInt(raw_value);
    }

    pub inline fn raw(self: SR2_Index) Raw {
        return @intFromEnum(self);
    }

    pub const format = fmt.format_enum;

    pub const Raw = std.meta.Tag(SR2_Index);
    pub const count = std.math.maxInt(Raw) + 1;
};

pub const Any_SR_Index = enum (u5) {
    one = raw_from_sr1(.one),
    rp = raw_from_sr1(.rp),
    sp = raw_from_sr1(.sp),
    bp = raw_from_sr1(.bp),
    temp_1 = raw_from_sr1(.temp_1),
    int_stat = raw_from_sr1(.int_stat),
    fault_stat = raw_from_sr1(.fault_stat),
    fault_dr = raw_from_sr1(.fault_dr),
    fault_ir = raw_from_sr1(.fault_ir),
    zero = raw_from_sr2(.zero),
    ip = raw_from_sr2(.ip),
    asn = raw_from_sr2(.asn),
    next_ip = raw_from_sr2(.next_ip),
    kxp = raw_from_sr2(.kxp),
    uxp = raw_from_sr2(.uxp),
    temp_2 = raw_from_sr2(.temp_2),
    rs_reserved = raw_from_sr2(.rs_reserved),

    pub inline fn init(raw_value: Raw) Any_SR_Index {
        return @enumFromInt(raw_value);
    }

    pub inline fn raw(self: Any_SR_Index) Raw {
        return @intFromEnum(self);
    }

    pub fn raw_from_sr1(sr1: SR1_Index) u5 {
        return sr1.raw();
    }
    pub fn from_sr1(sr1: SR1_Index) Any_SR_Index {
        return @enumFromInt(sr1.raw());
    }

    pub fn raw_from_sr2(sr2: SR2_Index) u5 {
        return sr2.raw() + @as(u5, 16);
    }
    pub fn from_sr2(sr2: SR2_Index) Any_SR_Index {
        return @enumFromInt(sr2.raw() + @as(Raw, 16));
    }

    pub fn to_sr1(self: Any_SR_Index) ?SR1_Index {
        const ord = @intFromEnum(self);
        return if (ord < 16) @enumFromInt(ord) else null;
    }

    pub fn to_sr2(self: Any_SR_Index) ?SR2_Index {
        const ord = @intFromEnum(self);
        return if (ord >= 16) @enumFromInt(ord - 16) else null;
    }

    pub const format = fmt.format_enum;

    pub const Raw = std.meta.Tag(Any_SR_Index);
    pub const count = std.math.maxInt(Raw) + 1;
};

pub const SR_Write_Source = enum (u2) {
    no_write = 0,
    self = 1,
    l = 2,
    virtual_addr = 3,

    pub inline fn init(raw_value: Raw) SR_Write_Source {
        return @enumFromInt(raw_value);
    }

    pub inline fn raw(self: SR_Write_Source) Raw {
        return @intFromEnum(self);
    }

    pub const format = fmt.format_enum;

    pub const Raw = std.meta.Tag(SR_Write_Source);
    pub const count = std.math.maxInt(Raw) + 1;
};

pub const Reg = enum (u32) {
    _,

    pub inline fn init(raw_value: Raw) Reg {
        return @enumFromInt(raw_value);
    }

    pub inline fn raw(self: Reg) Raw {
        return @intFromEnum(self);
    }

    pub const format = fmt.format_enum;

    pub const Raw = std.meta.Tag(Reg);
};

pub const Register_Set = struct {
    reg: [register_count]Reg,
    sr1: [SR1_Index.count]Reg,
    sr2: [SR2_Index.count]Reg,
};

pub const Register_File = [Register_Set_Number.count]Register_Set;

pub const J = enum (u32) {
    _,

    pub inline fn init(raw_value: Raw) J {
        return @enumFromInt(raw_value);
    }

    pub inline fn raw(self: J) Raw {
        return @intFromEnum(self);
    }

    pub const format = fmt.format_raw_hex;

    pub const Raw = std.meta.Tag(J);

    pub const Read_Index = enum (Register_Index) {
        _,

        pub inline fn init(raw_value: Read_Index.Raw) Read_Index {
            return @enumFromInt(raw_value);
        }

        pub inline fn raw(self: Read_Index) Read_Index.Raw {
            return @intFromEnum(self);
        }

        pub const format = fmt.format_enum;

        pub const Raw = std.meta.Tag(Read_Index);
        pub const max = std.math.maxInt(Read_Index.Raw);
    };

    pub const Source = enum (u2) {
        zero = 0,
        jr = 1,
        sr1 = 2,
        sr2 = 3,

        pub inline fn init(raw_value: Source.Raw) Source {
            return @enumFromInt(raw_value);
        }

        pub inline fn raw(self: Source) Source.Raw {
            return @intFromEnum(self);
        }

        pub const format = fmt.format_enum;

        pub const Raw = std.meta.Tag(Source);
    };
};

pub const K = enum (u32) {
    _,

    pub inline fn init(raw_value: Raw) K {
        return @enumFromInt(raw_value);
    }

    pub inline fn raw(self: K) Raw {
        return @intFromEnum(self);
    }

    pub const format = fmt.format_raw_hex;

    pub const Raw = std.meta.Tag(K);

    pub const Source = enum (u4) {
        zero = 0,
        krio = 1,
        krio_bit = 2,
        krio_bit_inv = 3,
        dr_byte_1_sx = 4,
        dr_byte_2_sx = 5,
        dr_byte_21_sx = 6,
        vao = 7,
        sr1 = 8,
        sr2 = 9,
        kr = 10,

        pub inline fn init(raw_value: Source.Raw) Source {
            return @enumFromInt(raw_value);
        }

        pub inline fn raw(self: Source) Source.Raw {
            return @intFromEnum(self);
        }

        pub const format = fmt.format_enum;

        pub const Raw = std.meta.Tag(Source);
    };

    pub const Read_Index = enum (Register_Index) {
        _,

        pub inline fn init(raw_value: Read_Index.Raw) Read_Index {
            return @enumFromInt(raw_value);
        }

        pub inline fn raw(self: Read_Index) Read_Index.Raw {
            return @intFromEnum(self);
        }

        pub const format = fmt.format_enum;

        pub const Raw = std.meta.Tag(Read_Index);
        pub const max = std.math.maxInt(Read_Index.Raw);
    };

    pub const Read_Index_Offset = enum (u5) {
        _,

        pub inline fn init(raw_value: Read_Index_Offset.Raw) Read_Index_Offset {
            return @enumFromInt(raw_value);
        }

        pub inline fn raw(self: Read_Index_Offset) Read_Index_Offset.Raw {
            return @intFromEnum(self);
        }

        pub const format = fmt.format_enum;

        pub const Raw = std.meta.Tag(Read_Index_Offset);
        pub const max = std.math.maxInt(Read_Index_Offset.Raw);
    };
};

pub const L = enum (u32) {
    _,

    pub inline fn init(raw_value: Raw) L {
        return @enumFromInt(raw_value);
    }

    pub inline fn raw(self: L) Raw {
        return @intFromEnum(self);
    }

    pub const format = fmt.format_raw_hex;

    pub const Raw = std.meta.Tag(L);

    pub const Source = enum (u3) {
        alu = 0,
        shift = 1,
        mult = 2,
        count = 3,
        ext = 4,
        at_info = 5,
        status = 6,
        d = 7,

        pub inline fn init(raw_value: Source.Raw) Source {
            return @enumFromInt(raw_value);
        }

        pub inline fn raw(self: Source) Source.Raw {
            return @intFromEnum(self);
        }

        pub const format = fmt.format_enum;

        pub const Raw = std.meta.Tag(Source);
    };
};

pub const D = enum (u32) {
    _,

    pub inline fn init(raw_value: Raw) D {
        return @enumFromInt(raw_value);
    }

    pub inline fn raw(self: D) Raw {
        return @intFromEnum(self);
    }

    pub const format = fmt.format_raw_hex;

    pub const Raw = std.meta.Tag(D);

    pub const Direction = enum (u2) {
        none = 0,
        read = 1,
        write_from_l = 2,
        write_from_dr_ir = 3, // if DRW is set, full 32b of DR is output.  Otherwise upper half is DR, lower half is IR.

        pub inline fn init(raw_value: Direction.Raw) Direction {
            return @enumFromInt(raw_value);
        }

        pub inline fn raw(self: Direction) Direction.Raw {
            return @intFromEnum(self);
        }

        pub const format = fmt.format_enum;

        pub const Raw = std.meta.Tag(Direction);
    };

    pub const Width = enum (u2) {
        @"32b" = 0,
        @"8b" = 1,
        @"16b" = 2,
        @"24b" = 3,

        pub inline fn init(raw_value: Width.Raw) Width {
            return @enumFromInt(raw_value);
        }

        pub inline fn raw(self: Width) Width.Raw {
            return @intFromEnum(self);
        }

        pub const format = fmt.format_enum;

        pub const Raw = std.meta.Tag(Width);
    };
};

pub const DA = packed struct (u16) {
    lo: u8,
    hi: u8,

    pub inline fn init(raw_value: Raw) DA {
        return @bitCast(raw_value);
    }

    pub inline fn raw(self: DA) Raw {
        return @bitCast(self);
    }

    pub const format = fmt.format_raw_hex;

    pub const Raw = u16;
};

pub const DB = packed struct (u16) {
    lo: u8,
    hi: u8,

    pub inline fn init(raw_value: Raw) DB {
        return @bitCast(raw_value);
    }

    pub inline fn raw(self: DB) Raw {
        return @bitCast(self);
    }

    pub const format = fmt.format_raw_hex;

    pub const Raw = u16;
};

pub const DR = enum (u32) {
    _,

    pub inline fn init(raw_value: Raw) DR {
        return @enumFromInt(raw_value);
    }

    pub inline fn raw(self: DR) Raw {
        return @intFromEnum(self);
    }

    pub inline fn byte1_u8(self: DR) u8 {
        return @truncate(self.raw() >> 8);
    }
    pub inline fn byte1_i8(self: DR) i8 {
        return @bitCast(self.byte1_u8());
    }
    pub inline fn byte1_i32(self: DR) i32 {
        return self.byte1_i8();
    }

    pub inline fn byte2_u8(self: DR) u8 {
        return @truncate(self.raw() >> 16);
    }
    pub inline fn byte2_i8(self: DR) i8 {
        return @bitCast(self.byte2_u8());
    }
    pub inline fn byte2_i32(self: DR) i32 {
        return self.byte2_i8();
    }

    pub inline fn byte21_u16(self: DR) u16 {
        return @truncate(self.raw() >> 8);
    }
    pub inline fn byte21_i16(self: DR) i16 {
        return @bitCast(self.byte21_u16());
    }
    pub inline fn byte21_i32(self: DR) i32 {
        return self.byte21_i16();
    }

    pub const format = fmt.format_raw_hex;

    pub const Raw = std.meta.Tag(DR);
};

pub const IR = enum (u16) {
    _,

    pub inline fn init(raw_value: Raw) IR {
        return @enumFromInt(raw_value);
    }

    pub inline fn raw(self: IR) Raw {
        return @intFromEnum(self);
    }

    pub fn to_byte_swapped(self: IR) Raw {
        const r: u16 = self.raw();
        return (r >> 8) | (r << 8);
    }

    pub fn from_byte_swapped(byte_swapped: Raw) IR {
        return init((byte_swapped >> 8) | (byte_swapped << 8));
    }

    pub const format = fmt.format_raw_hex;

    pub const Raw = std.meta.Tag(IR);
    pub const count = std.math.maxInt(Raw) + 1;
};

pub const Compute_Unit = enum (u2) {
    alu = 0,
    shift = 1,
    mult = 2,
    count_extend = 3,

    pub inline fn init(raw_value: Raw) Compute_Unit {
        return @enumFromInt(raw_value);
    }

    pub inline fn raw(self: Compute_Unit) Raw {
        return @intFromEnum(self);
    }

    pub const format = fmt.format_enum;

    pub const Raw = std.meta.Tag(Compute_Unit);
};

pub const Compute_Mode = packed union {
    alu: ALU_Mode,
    shift: Shift_Mode,
    mult: Multiply_Mode,
    count_extend: Count_Extend_Mode,

    pub inline fn init(raw_value: Raw) Compute_Mode {
        return @bitCast(raw_value);
    }

    pub inline fn raw(self: Compute_Mode) Raw {
        return @bitCast(self);
    }

    pub const format = fmt.format_raw_hex;

    pub const Raw = std.meta.Int(.unsigned, @bitSizeOf(Compute_Mode));
};

/// As specified in L4C381 datasheet
pub const ALU_Opcode = enum(u3) {
    all_zeroes = 0,
    not_j_plus_k = 1,
    j_plus_not_k = 2,
    j_plus_k = 3,
    j_xor_k = 4,
    j_or_k = 5,
    j_and_k = 6,
    all_ones = 7,

    pub inline fn init(raw_value: Raw) ALU_Opcode {
        return @bitCast(raw_value);
    }

    pub inline fn raw(self: ALU_Opcode) Raw {
        return @bitCast(self);
    }

    pub const format = fmt.format_enum;

    pub const Raw = std.meta.Tag(Compute_Unit);
};

pub const ALU_Mode = packed struct (u5) {
    op: ALU_Opcode,
    use_stat_c: bool,
    invert_cin: bool,

    pub const all_zeroes: ALU_Mode = .{ .op = .all_zeroes, .use_stat_c = false, .invert_cin = false };
    pub const nadd: ALU_Mode = .{ .op = .not_j_plus_k, .use_stat_c = false, .invert_cin = true };
    pub const naddc: ALU_Mode = .{ .op = .not_j_plus_k, .use_stat_c = true, .invert_cin = false };
    pub const sub: ALU_Mode = .{ .op = .j_plus_not_k, .use_stat_c = false, .invert_cin = true };
    pub const subc: ALU_Mode = .{ .op = .j_plus_not_k, .use_stat_c = true, .invert_cin = false };
    pub const add: ALU_Mode = .{ .op = .j_plus_k, .use_stat_c = false, .invert_cin = false };
    pub const addc: ALU_Mode = .{ .op = .j_plus_k, .use_stat_c = true, .invert_cin = false };
    pub const addi: ALU_Mode = .{ .op = .j_plus_k, .use_stat_c = false, .invert_cin = true };
    pub const logic_xor: ALU_Mode = .{ .op = .j_xor_k, .use_stat_c = false, .invert_cin = false };
    pub const logic_or: ALU_Mode = .{ .op = .j_or_k, .use_stat_c = false, .invert_cin = false };
    pub const logic_and: ALU_Mode = .{ .op = .j_and_k, .use_stat_c = false, .invert_cin = false };
    pub const all_ones: ALU_Mode = .{ .op = .all_ones, .use_stat_c = false, .invert_cin = false };
};

pub const Shift_Mode = packed struct (u5) {
    left: bool,
    left_xor_swap_bytes: bool,
    left_xor_swap_halves: bool,
    cin: enum (u2) {
        zero = 0,
        zero_bitreverse = 1,
        j31 = 2,
        stat_c = 3,
    },
    
    pub const shrl: Shift_Mode = .{ .left = false, .left_xor_swap_bytes = false, .left_xor_swap_halves = false, .cin = .zero };
    pub const shra: Shift_Mode = .{ .left = false, .left_xor_swap_bytes = false, .left_xor_swap_halves = false, .cin = .j31 };
    pub const shrc: Shift_Mode = .{ .left = false, .left_xor_swap_bytes = false, .left_xor_swap_halves = false, .cin = .stat_c };
    pub const shl: Shift_Mode = .{ .left = true, .left_xor_swap_bytes = true, .left_xor_swap_halves = true, .cin = .zero };
    pub const shlc: Shift_Mode = .{ .left = true, .left_xor_swap_bytes = true, .left_xor_swap_halves = true, .cin = .stat_c };

    pub const swap_bytes: Shift_Mode = .{ .left = false, .left_xor_swap_bytes = true, .left_xor_swap_halves = false, .cin = .zero };
    pub const swap_halves: Shift_Mode = .{ .left = false, .left_xor_swap_bytes = false, .left_xor_swap_halves = true, .cin = .zero };
    pub const reverse_bytes: Shift_Mode = .{ .left = false, .left_xor_swap_bytes = true, .left_xor_swap_halves = true, .cin = .zero };
    pub const reverse_bits: Shift_Mode = .{ .left = false, .left_xor_swap_bytes = false, .left_xor_swap_halves = false, .cin = .zero_bitreverse };
    pub const reverse_bits_in_halves: Shift_Mode = .{ .left = false, .left_xor_swap_bytes = false, .left_xor_swap_halves = true, .cin = .zero_bitreverse };
    pub const reverse_bits_in_bytes: Shift_Mode = .{ .left = false, .left_xor_swap_bytes = true, .left_xor_swap_halves = true, .cin = .zero_bitreverse };
};

pub const Multiply_Mode = packed struct (u5) {
    j_type: Signedness,
    k_type: Signedness,
    j: Half,
    k: Half,
    shift_result: bool, // 16 LSBs will be 0, 16 MSBs will come from LSBs of product; useful when doing 32b * 32b -> 32b multiply

    pub const Signedness = enum (u1) {
        unsigned = 0,
        signed = 1,
    };

    pub const Half = enum (u1) {
        lsb = 0, 
        msb = 1,
    };
};

pub const Count_Extend_Mode = packed struct (u5) {
    invert_j: bool,
    invert_k: bool,
    saturate: enum (u1) { left, right },
    invert_saturated: bool,
    sign_extend: bool,

    // When doing popcount (including leading/trailing variants), J and K should be identical:
    pub const count_ones: Count_Extend_Mode = .{ .invert_j = false, .invert_k = false, .saturate = .right, .invert_saturated = false, .sign_extend = false };
    pub const count_zeroes: Count_Extend_Mode = .{ .invert_j = true, .invert_k = true, .saturate = .right, .invert_saturated = false, .sign_extend = false };

    pub const count_leading_ones: Count_Extend_Mode = .{ .invert_j = false, .invert_k = true, .saturate = .right, .invert_saturated = true, .sign_extend = false };
    pub const count_leading_zeroes: Count_Extend_Mode = .{ .invert_j = true, .invert_k = false, .saturate = .right, .invert_saturated = true, .sign_extend = false };

    pub const count_trailing_ones: Count_Extend_Mode = .{ .invert_j = false, .invert_k = true, .saturate = .left, .invert_saturated = true, .sign_extend = false };
    pub const count_trailing_zeroes: Count_Extend_Mode = .{ .invert_j = true, .invert_k = false, .saturate = .left, .invert_saturated = true, .sign_extend = false };

    // To sign/zero extend J to N bits, the low N bits of K should be 0, and the N+1th bit of K should be a 1:
    pub const zero_extend_or_truncate: Count_Extend_Mode = .{ .invert_j = false, .invert_k = false, .saturate = .left, .invert_saturated = true, .sign_extend = false };
    pub const sign_extend_or_truncate: Count_Extend_Mode = .{ .invert_j = false, .invert_k = false, .saturate = .left, .invert_saturated = true, .sign_extend = true };

    // When doing saturation, J should be 0 and K should be the value to saturate:
    pub const saturate_ones_left: Count_Extend_Mode = .{ .invert_j = false, .invert_k = false, .saturate = .left, .invert_saturated = true, .sign_extend = false };
    pub const saturate_ones_right: Count_Extend_Mode = .{ .invert_j = false, .invert_k = false, .saturate = .right, .invert_saturated = true, .sign_extend = false };

    pub const saturate_zeroes_left: Count_Extend_Mode = .{ .invert_j = false, .invert_k = true, .saturate = .left, .invert_saturated = false, .sign_extend = false };
    pub const saturate_zeroes_right: Count_Extend_Mode = .{ .invert_j = false, .invert_k = true, .saturate = .right, .invert_saturated = false, .sign_extend = false };
};

pub const Status = packed struct (u32) {
    pipeline: Pipeline,
    z: bool,
    n: bool,
    c: bool,
    v: bool,
    k: bool,
    a: bool,
    rsn: Register_Set_Number,
    top: Write_Index,
    pucs: microcode.Slot,

    pub inline fn init(raw_value: Raw) Status {
        return @bitCast(raw_value);
    }

    pub inline fn raw(self: Status) Raw {
        return @bitCast(self);
    }

    pub const format = fmt.format_raw_hex;

    pub const Raw = u16;

    pub const Op = enum (u3) {
        hold = 0,
        zn_from_l = 1,
        clear_a = 2,
        set_a = 3,
        compute = 4,
        compute_no_set_z = 5,
        load_zncv = 6,
        load_zncva_ti = 7,

        pub inline fn init(raw_value: Op.Raw) Op {
            return @enumFromInt(raw_value);
        }

        pub inline fn raw(self: Op) Op.Raw {
            return @intFromEnum(self);
        }

        pub const format = fmt.format_enum;

        pub const Raw = std.meta.Tag(Op);
    };
};

pub const Guarded_Memory_File = [Pipeline.count]Guarded_Memory_Register;

pub const Guarded_Memory_Register = packed struct (u20) {
    offset: u9,
    frame: u11,

    pub inline fn init(raw_value: Raw) Guarded_Memory_Register {
        return @bitCast(raw_value);
    }

    pub inline fn from_frame_and_offset(frame: addr.Frame, offset: addr.Offset) Guarded_Memory_Register {
        return .{
            .offset = @intCast(offset.raw() >> 3),
            .frame = @truncate(frame.raw()),
        };
    }

    pub inline fn raw(self: Guarded_Memory_Register) Raw {
        return @bitCast(self);
    }

    pub const format = fmt.format_raw_hex;

    pub const Raw = u20;
    pub const invalid = Guarded_Memory_Register.init(0);
};

pub const Context_State = extern struct {
    registers: [register_count]u16,
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
    align_fault: u16,
    overflow_fault: u16,
    invalid_instruction_fault: u16,
    instruction_protection_fault: u16,
    interrupt: u16,
};

const fmt = @import("arch/fmt.zig");
const std = @import("std");
