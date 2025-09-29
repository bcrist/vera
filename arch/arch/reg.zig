// "Register Set Number"
pub const RSN = enum (u6) {
    interrupt_pipe_0 = 0,
    interrupt_pipe_1 = 1,
    interrupt_pipe_2 = 2,
    interrupt_pipe_3 = 3,
    interrupt_fault_pipe_0 = 4,
    interrupt_fault_pipe_1 = 5,
    interrupt_fault_pipe_2 = 6,
    interrupt_fault_pipe_3 = 7,
    fault_pipe_0 = 8,
    fault_pipe_1 = 9,
    fault_pipe_2 = 10,
    fault_pipe_3 = 11,
    _,

    pub inline fn init(raw_value: Raw) RSN {
        return @enumFromInt(raw_value);
    }

    pub inline fn raw(self: RSN) Raw {
        return @intFromEnum(self);
    }

    pub const format = fmt.format_enum_dec;

    pub const Raw = meta.Backing(RSN);
    pub const count = std.math.maxInt(Raw) + 1;
};

pub const gpr = struct {
    pub const Index = u6;
    pub const count = std.math.maxInt(Index) + 1;

    pub const Write_Index = enum (Index) {
        _,

        pub inline fn init(raw_value: Raw) Write_Index {
            return @enumFromInt(raw_value);
        }

        pub inline fn raw(self: Write_Index) Raw {
            return @intFromEnum(self);
        }

        pub const format = fmt.format_enum_dec;

        pub const Raw = meta.Backing(Write_Index);
        pub const max = std.math.maxInt(Raw);
    };

    pub const Write_Index_Offset = enum (i5) {
        _,

        pub inline fn init(raw_value: Raw) Write_Index_Offset {
            return @enumFromInt(raw_value);
        }

        pub inline fn init_unsigned(raw_value: Raw_Unsigned) Write_Index_Offset {
            return .init(@bitCast(raw_value));
        }

        pub inline fn raw(self: Write_Index_Offset) Raw {
            return @intFromEnum(self);
        }

        pub inline fn raw_unsigned(self: Write_Index_Offset) Raw_Unsigned {
            return @bitCast(@intFromEnum(self));
        }

        pub const format = fmt.format_enum_dec;

        pub const Raw = meta.Backing(Write_Index_Offset);
        pub const Raw_Unsigned = std.meta.Int(.unsigned, @bitSizeOf(Raw));
        pub const max = std.math.maxInt(Raw);
    };

    pub const Value = enum (u32) {
        _,

        pub inline fn init(raw_value: Raw) Value {
            return @enumFromInt(raw_value);
        }

        pub inline fn raw(self: Value) Raw {
            return @intFromEnum(self);
        }

        pub const format = fmt.format_enum_hex;

        pub const Raw = meta.Backing(Value);
    };
};

pub const sr1 = struct {
    pub const Index = enum (u4) {
        one = 0,            // 0x0000_0001 (must never be overwritten)
        rp = 1,             // return pointer
        sp = 2,             // stack pointer
        bp = 3,             // stack base pointer
        // 4 unused
        // 5 unused
        // 6 unused
        temp_1 = 7,
        int_flags = 8,      // Upon entering an interrupt handler, the flags register is stored here
        // 9 unused
        // 10 unused
        // 11 unused
        fault_d = 12,
        fault_flags = 13,   // Upon entering a fault handler, the flags register is stored here
        fault_dr = 14,      // Upon entering a fault handler, DR is stored here.
        fault_ir = 15,      // Upon entering a fault handler, IR is stored here.
        _,

        pub inline fn init(raw_value: Raw) Index {
            return @enumFromInt(raw_value);
        }

        pub inline fn raw(self: Index) Raw {
            return @intFromEnum(self);
        }

        pub const format = fmt.format_enum_dec;

        pub const Raw = meta.Backing(Index);
        pub const count = std.math.maxInt(Raw) + 1;
    };

    pub const Value = enum (u32) {
        _,

        pub inline fn init(raw_value: Raw) Value {
            return @enumFromInt(raw_value);
        }

        pub inline fn raw(self: Value) Raw {
            return @intFromEnum(self);
        }

        pub const format = fmt.format_enum_hex;

        pub const Raw = meta.Backing(Value);
    };
};

pub const sr2 = struct {
    pub const Index = enum (u4) {
        zero = 0,           // 0x0000_0000 (must never be overwritten)
        ip = 1,             // instruction pointer
        asn = 2,            // address space number
        next_ip = 3,        // when next instruction is loaded before the last cycle of the current instruction, the next IP is kept here.
        kxp = 4,            // kernel context pointer
        uxp = 5,            // user context pointer
        // 6 unused
        temp_2 = 7,
        _,

        pub inline fn init(raw_value: Raw) Index {
            return @enumFromInt(raw_value);
        }

        pub inline fn raw(self: Index) Raw {
            return @intFromEnum(self);
        }

        pub const format = fmt.format_enum_dec;

        pub const Raw = meta.Backing(Index);
        pub const count = std.math.maxInt(Raw) + 1;
    };

    pub const Value = enum (u32) {
        _,

        pub inline fn init(raw_value: Raw) Value {
            return @enumFromInt(raw_value);
        }

        pub inline fn raw(self: Value) Raw {
            return @intFromEnum(self);
        }

        pub const format = fmt.format_enum_hex;

        pub const Raw = meta.Backing(Value);
    };
};

pub const sr = struct {
    pub const Any_Index = enum (u5) {
        one = raw_from_sr1(.one),
        rp = raw_from_sr1(.rp),
        sp = raw_from_sr1(.sp),
        bp = raw_from_sr1(.bp),
        temp_1 = raw_from_sr1(.temp_1),
        int_flags = raw_from_sr1(.int_flags),
        fault_d = raw_from_sr1(.fault_d),
        fault_flags = raw_from_sr1(.fault_flags),
        fault_dr = raw_from_sr1(.fault_dr),
        fault_ir = raw_from_sr1(.fault_ir),
        zero = raw_from_sr2(.zero),
        ip = raw_from_sr2(.ip),
        asn = raw_from_sr2(.asn),
        next_ip = raw_from_sr2(.next_ip),
        kxp = raw_from_sr2(.kxp),
        uxp = raw_from_sr2(.uxp),
        temp_2 = raw_from_sr2(.temp_2),

        pub inline fn init(raw_value: Raw) Any_Index {
            return @enumFromInt(raw_value);
        }

        pub inline fn raw(self: Any_Index) Raw {
            return @intFromEnum(self);
        }

        pub fn raw_from_sr1(i: sr1.Index) u5 {
            return i.raw();
        }
        pub fn from_sr1(i: sr1.Index) Any_Index {
            return @enumFromInt(i.raw());
        }

        pub fn raw_from_sr2(i: sr2.Index) u5 {
            return i.raw() + @as(Raw, 16);
        }
        pub fn from_sr2(i: sr2.Index) Any_Index {
            return @enumFromInt(i.raw() + @as(Raw, 16));
        }

        pub fn to_sr1(self: Any_Index) ?sr1.Index {
            const ord = @intFromEnum(self);
            return if (ord < 16) @enumFromInt(ord) else null;
        }

        pub fn to_sr2(self: Any_Index) ?sr2.Index {
            const ord = @intFromEnum(self);
            return if (ord >= 16) @enumFromInt(ord - 16) else null;
        }

        pub const format = fmt.format_enum_dec;

        pub const Raw = u5;
        pub const count = std.math.maxInt(Raw) + 1;
    };

    pub const Write_Source = enum (u2) {
        no_write = 0,
        self = 1,
        l = 2,
        virtual_addr = 3,

        pub inline fn init(raw_value: Raw) Write_Source {
            return @enumFromInt(raw_value);
        }

        pub inline fn raw(self: Write_Source) Raw {
            return @intFromEnum(self);
        }

        pub const format = fmt.format_enum_dec;

        pub const Raw = meta.Backing(Write_Source);
        pub const count = std.math.maxInt(Raw) + 1;
    };
};

pub const File = [RSN.count]Stack;
pub const Stack = struct {
    reg: [gpr.count]gpr.Value,
    sr1: [sr1.Index.count]sr1.Value,
    sr2: [sr2.Index.count]sr2.Value,
};

pub const guard = struct {
    pub const Value = packed struct (u20) {
        offset: u9,
        frame: u11,

        pub inline fn init(raw_value: Raw) Value {
            return @bitCast(raw_value);
        }

        pub inline fn from_frame_and_offset(frame: addr.Frame, offset: addr.Frame.Offset) Value {
            return .{
                .offset = @intCast(offset.raw() >> 3),
                .frame = @truncate(frame.raw()),
            };
        }

        pub inline fn raw(self: Value) Raw {
            return @bitCast(self);
        }

        pub const format = fmt.format_raw_hex;

        pub const Raw = meta.Backing(Value);
        pub const invalid = Value.init(0);
    };

    pub const File = [misc.Pipeline.count]Value;
};

pub const DR = enum (u32) {
    _,

    pub inline fn init(raw_value: Raw) DR {
        return @enumFromInt(raw_value);
    }

    pub inline fn raw(self: DR) Raw {
        return @intFromEnum(self);
    }

    pub inline fn @"unsigned[15:8]"(self: DR) u8 {
        return @truncate(self.raw() >> 8);
    }
    pub inline fn @"signed[15:8]"(self: DR) i8 {
        return @bitCast(self.@"unsigned[15:8]"());
    }

    pub inline fn @"unsigned[23:16]"(self: DR) u8 {
        return @truncate(self.raw() >> 16);
    }
    pub inline fn @"signed[23:16]"(self: DR) i8 {
        return @bitCast(self.@"unsigned[23:16]"());
    }

    pub inline fn @"unsigned[23:8]"(self: DR) u16 {
        return @truncate(self.raw() >> 8);
    }
    pub inline fn @"signed[23:8]"(self: DR) i16 {
        return @bitCast(self.@"unsigned[23:8]"());
    }

    pub const format = fmt.format_raw_hex;

    pub const Raw = meta.Backing(DR);
};

// Note that unlike a typical RISC pipeline, there is no dedicated instruction fetch stage.
// Each instruction is responsible for updating the `IP` register for the next instruction,
// as well as loading the instruction data into `DR` and `IR`.  The updates to `IR` and `IP`
// must happen in the Transact stage of the last cycle of the previous instruction, and
// indeed most instructions that don't reference memory can load the next instruction in
// parallel with the actual computational work.  Instructions that take multiple microcode
// cycles can load `DR` for the next instruction during any cycle, as long as the old data
// isn't needed anymore.  For instance, a load or store instruction with a register + offset
// addressing mode needs to compute the final address in the first cycle, then do the actual
// load/store during the second cycle.  The memory bus is free during the first cycle to load
// the next instruction, instead of having to add a third cycle to load the next instruction.
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

    pub const Raw = meta.Backing(IR);
    pub const count = std.math.maxInt(Raw) + 1;
};

pub const Status = packed struct (u32) {
    rsn: RSN,
    pipe: misc.Pipeline,
    fwrite: bool,
    fwidth: bus.D.Width,
    fspace: addr.Space,
    fucs: microcode.Slot,
    _unused: u7 = 0,

    pub inline fn init(raw_value: Raw) Status {
        return @bitCast(raw_value);
    }

    pub inline fn raw(self: Status) Raw {
        return @bitCast(self);
    }

    pub const format = fmt.format_raw_hex;

    pub const Raw = meta.Backing(Status);
};

pub const Flags = packed struct (u32) {
    z: bool, // zero
    n: bool, // negative
    c: bool, // carry
    v: bool, // overflow
    at_enable: bool, // address translation enabled
    bus_override: bool, // suppress next bus operation
    super: bool, // kernel/supervisor mode
    at_super: bool, // copied to super when IR is loaded for a new instruction
    top: gpr.Write_Index,
    _unused2: u2 = 0,
    mode: misc.Execution_Mode,
    _unused3: u14 = 0,

    pub inline fn init(raw_value: Raw) Flags {
        return @bitCast(raw_value);
    }

    pub inline fn raw(self: Flags) Raw {
        return @bitCast(self);
    }

    pub const format = fmt.format_raw_hex;

    pub const Raw = meta.Backing(Flags);

    // These bits are able to be toggled with .clear_bits and .set_bits
    pub const Writable = packed struct (u32) {
        z: bool = false,
        n: bool = false,
        c: bool = false,
        v: bool = false,
        at_enable: bool = false,
        bus_override: bool = false,
        _unused: u26 = 0,

        pub inline fn init(raw_value: Writable.Raw) Writable {
            return @bitCast(raw_value);
        }

        pub inline fn raw(self: Writable) Writable.Raw {
            return @bitCast(self);
        }

        pub const format = fmt.format_enum_hex;

        pub const Raw = meta.Backing(Writable);
    };

    pub const Op = enum (u3) {
        hold = 0,
        zn_from_l = 1,
        compute = 2,
        compute_no_set_z = 3,
        clear_bits = 4, // bit mask comes from VAO
        set_bits = 5,   // bit mask comes from VAO
        load_zncv = 6,
        load = 7,

        pub inline fn init(raw_value: Op.Raw) Op {
            return @enumFromInt(raw_value);
        }

        pub inline fn raw(self: Op) Op.Raw {
            return @intFromEnum(self);
        }

        pub const format = fmt.format_enum_dec;

        pub const Raw = meta.Backing(Op);
    };
};

const addr = @import("addr.zig");
const microcode = @import("microcode.zig");
const bus = @import("bus.zig");
const misc = @import("misc.zig");
const fmt = @import("fmt.zig");
const meta = @import("meta");
const std = @import("std");
