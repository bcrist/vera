pub const Control_Signals = @import("Control_Signals.zig");
pub const Control_Signal = std.meta.FieldEnum(Control_Signals);

// pub const Signed_Offset_For_Literal = i7; // note not all possible values are valid

pub const Pipeline = enum(u2) {
    zero = 0,
    one = 1,
    two = 2,
    three = 3,

    pub fn init(raw_value: u2) Pipeline {
        return @enumFromInt(raw_value);
    }
    pub fn raw(self: Pipeline) u2 {
        return @intFromEnum(self);
    }

    pub const count = 4;
};

pub const Execution_Mode = enum(u2) {
    normal = 0,
    interrupt = 1,
    fault = 2,
    interrupt_fault = 3,

    pub fn init(raw_value: u2) Execution_Mode {
        return @enumFromInt(raw_value);
    }
    pub fn raw(self: Execution_Mode) u2 {
        return @intFromEnum(self);
    }
    pub fn is_fault(self: Execution_Mode) bool {
        return switch (self) {
            .normal, .interrupt => false,
            .fault, .interrupt_fault => true,
        };
    }
    pub fn is_interrupt(self: Execution_Mode) bool {
        return switch (self) {
            .normal, .fault => false,
            .interrupt, .interrupt_fault => true,
        };
    }
};

pub const Power_Mode = enum(u1) {
    run = 0,
    sleep = 1,
};

pub const microcode = struct {
    pub const Slot = enum (u12) {
        reset = 0,
        page_fault = 1,
        access_fault = 2,
        page_align_fault = 3,
        instruction_protection_fault = 4,
        invalid_instruction = 5,
        double_fault = 6,
        interrupt = 7,
        _,
        pub fn init(raw_value: u12) Slot {
            return @enumFromInt(raw_value);
        }
        pub fn raw(self: Slot) u12 {
            return @intFromEnum(self);
        }

        pub const first = init(8);
        pub const last = init(std.math.maxInt(u12));
        pub const count = @as(usize, last) + 1;
    };

    pub const Slot_Source = enum (u2) {
        hold = 0,
        insn_decoder = 1,
        continuation = 2,
        seq_literal = 3,
    };

    pub const Flags = packed struct (u5) {
        z: bool,
        n: bool,
        c: bool,
        v: bool,
        k: bool,

        pub fn init(raw_value: u5) Flags {
            return @bitCast(raw_value);
        }
        pub fn raw(self: Flags) u5 {
            return @bitCast(self);
        }

        pub fn zero(self: Flags) bool { return self.z; }
        pub fn negative(self: Flags) bool { return self.n; }
        pub fn positive(self: Flags) bool { return !self.z and !self.n; }
        pub fn carry_or_borrow(self: Flags) bool { return self.c; }
        pub fn overflow(self: Flags) bool { return self.v; }
        pub fn kernel(self: Flags) bool { return self.k; }
        pub fn unsigned_less_than(self: Flags) bool { return self.c and !self.z; }
        pub fn unsigned_greater_than(self: Flags) bool { return !self.c and !self.z; }
        pub fn signed_less_than(self: Flags) bool { return !self.z and self.n != self.v; }
        pub fn signed_greater_than(self: Flags) bool { return !self.z and self.n == self.v; }

        pub fn inverted(self: Flags) Flags {
            return @bitCast(~self.raw());
        }

        pub fn combined_with(self: Flags, other: Flags) Flags {
            const self_raw: u5 = @bitCast(self);
            const other_raw: u5 = @bitCast(other);
            return @bitCast(self_raw | other_raw);
        }

        pub fn permutations(permutable_bits: Flags) Permutation_Iterator {
            return .{
                .permutable_bits = permutable_bits,
                .next_permutation = init(0),
            };
        }
        pub const Permutation_Iterator = struct {
            permutable_bits: Flags,
            next_permutation: ?Flags,

            pub fn next(self: *Permutation_Iterator) ?Flags {
                if (self.next_permutation) |p| {
                    self.next_permutation = increment(p, self.permutable_bits);
                    return p;
                }
                return null;
            }

            fn increment(p: Flags, permutable_bits: Flags) ?Flags {
                var result = p;
                if (permutable_bits.z) {
                    result.z = !result.z;
                    if (!p.z) return result;
                }
                if (permutable_bits.n) {
                    result.n = !result.n;
                    if (!p.n) return result;
                }
                if (permutable_bits.c) {
                    result.c = !result.c;
                    if (!p.c) return result;
                }
                if (permutable_bits.v) {
                    result.v = !result.v;
                    if (!p.v) return result;
                }
                if (permutable_bits.k) {
                    result.k = !result.k;
                    if (!p.k) return result;
                }
                return null;
            }
        };
    };

    pub const Address = packed struct (u17) {
        flags: Flags,
        slot: Slot,

        pub fn init(raw_value: u17) Address {
            return @bitCast(raw_value);
        }
        pub fn raw(self: Address) u17 {
            return @bitCast(self);
        }

        pub const count = std.math.maxInt(u17) + 1;
    };

    pub const Data = [Address.count]Control_Signals;
};

pub const decode = struct {
    pub const Address = packed struct (u17) {
        d: D,
        mode: Control_Signals.ID_Mode,

        pub fn init(raw_value: u17) Address {
            return @bitCast(raw_value);
        }
        pub fn raw(self: Address) u17 {
            return @bitCast(self);
        }

        pub const count = std.math.maxInt(u17) + 1;
    };

    pub const Result = packed struct (u24) {
        slot: microcode.Slot,
        ij: IJ,
        ik: IK,
        iw: IW,
        pub fn init(raw_value: u24) Result {
            return @bitCast(raw_value);
        }
        pub fn raw(self: Result) u24 {
            return @bitCast(self);
        }
    };
};

pub const RSN = enum (u6) {
    _,
    pub fn init(raw_value: u6) RSN {
        return @enumFromInt(raw_value);
    }
    pub fn raw(self: RSN) u6 {
        return @intFromEnum(self);
    }

    pub const count = std.math.maxInt(u6) + 1;
};

pub const IJ = enum (arch.Register_Index) {
    _,
    pub fn init(raw_value: arch.Register_Index) IJ {
        return @enumFromInt(raw_value);
    }
    pub fn raw(self: IJ) arch.Register_Index {
        return @intFromEnum(self);
    }
};

pub const IK = enum (arch.Register_Index) {
    _,
    pub fn init(raw_value: arch.Register_Index) IK {
        return @enumFromInt(raw_value);
    }
    pub fn raw(self: IK) arch.Register_Index {
        return @intFromEnum(self);
    }
};

pub const IW = enum (arch.Register_Index) {
    _,
    pub fn init(raw_value: arch.Register_Index) IW {
        return @enumFromInt(raw_value);
    }
    pub fn raw(self: IW) arch.Register_Index {
        return @intFromEnum(self);
    }
};

pub const R = enum(u16) {
    _,
    pub fn init(raw_value: u16) R {
        return @enumFromInt(raw_value);
    }
    pub fn raw(self: R) u16 {
        return @intFromEnum(self);
    }
};

pub const SRL = enum (u16) {
    _,
    pub fn init(raw_value: u16) SRL {
        return @enumFromInt(raw_value);
    }
    pub fn raw(self: SRL) u16 {
        return @intFromEnum(self);
    }
};
pub const SRH = enum (u16) {
    _,
    pub fn init(raw_value: u16) SRH {
        return @enumFromInt(raw_value);
    }
    pub fn raw(self: SRH) u16 {
        return @intFromEnum(self);
    }
};
pub const SR = packed struct (u32) {
    lo: SRL,
    hi: SRH,
    pub fn init(raw_value: u32) SR {
        return @bitCast(raw_value);
    }
    pub fn raw(self: SR) u32 {
        return @bitCast(self);
    }
};

pub const Register_Set = struct {
    reg: [arch.register_count]R,
    sr1: [Control_Signals.SR1_Index.count]SR,
    sr2: [Control_Signals.SR2_Index.count]SR,
};

pub const JL = enum (u16) {
    _,
    pub fn init(raw_value: u16) JL {
        return @enumFromInt(raw_value);
    }
    pub fn raw(self: JL) u16 {
        return @intFromEnum(self);
    }
};
pub const JH = enum (u16) {
    _,
    pub fn init(raw_value: u16) JH {
        return @enumFromInt(raw_value);
    }
    pub fn raw(self: JH) u16 {
        return @intFromEnum(self);
    }
};
pub const J = packed struct (u32) {
    lo: JL,
    hi: JH,
    pub fn init(raw_value: u32) J {
        return @bitCast(raw_value);
    }
    pub fn raw(self: J) u32 {
        return @bitCast(self);
    }
};

pub const K = enum (u16) {
    _,
    pub fn init(raw_value: u16) K {
        return @enumFromInt(raw_value);
    }
    pub fn raw(self: K) u16 {
        return @intFromEnum(self);
    }
};

pub const LL = enum (u16) {
    _,
    pub fn init(raw_value: u16) LL {
        return @enumFromInt(raw_value);
    }
    pub fn raw(self: LL) u16 {
        return @intFromEnum(self);
    }
};
pub const LH = enum (u16) {
    _,
    pub fn init(raw_value: u16) LH {
        return @enumFromInt(raw_value);
    }
    pub fn raw(self: LH) u16 {
        return @intFromEnum(self);
    }
};
pub const L = packed struct (u32) {
    lo: LL,
    hi: LH,
    pub fn init(raw_value: u32) L {
        return @bitCast(raw_value);
    }
    pub fn raw(self: L) u32 {
        return @bitCast(self);
    }
};

pub const D = enum (u16) {
    _,
    pub fn init(raw_value: u16) D {
        return @enumFromInt(raw_value);
    }
    pub fn raw(self: D) u16 {
        return @intFromEnum(self);
    }
    pub fn raw8(self: D) u8 {
        return @truncate(@intFromEnum(self));
    }
};

pub const Status = packed struct(u16) {
    z: bool,
    n: bool,
    v: bool,
    c: bool,
    k: bool,
    a: bool,
    power: Power_Mode,
    pipeline: Pipeline,
    exec: Execution_Mode,
    _unused: u5 = 0,

    pub fn init(raw_value: u16) Status {
        return @bitCast(raw_value);
    }
    pub fn raw(self: Status) u16 {
        return @bitCast(self);
    }
};

pub const addr = struct {
    pub const Slot = enum (u6) {
        _,
        pub fn init(raw_value: u6) Slot {
            return @enumFromInt(raw_value);
        }
        pub fn raw(self: Slot) u6 {
            return @intFromEnum(self);
        }
    };

    pub const Tag = enum (u14) {
        _,
        pub fn init(raw_value: u14) Tag {
            return @enumFromInt(raw_value);
        }
        pub fn raw(self: Tag) u14 {
            return @intFromEnum(self);
        }
    };

    pub const Page = packed struct (u20) {
        slot: Slot,
        tag: Tag,

        pub fn init(raw_value: u20) Page {
            return @bitCast(raw_value);
        }
        pub fn raw(self: Page) u20 {
            return @bitCast(self);
        }

        pub const num_addresses_per_page = @as(usize, Offset.max.raw()) + 1;
    };

    pub const Offset = enum (u12) {
        _,
        pub fn init(raw_value: u12) Offset {
            return @enumFromInt(raw_value);
        }
        pub fn raw(self: Offset) u12 {
            return @intFromEnum(self);
        }

        pub const zero = init(0);
        pub const max = init(std.math.maxInt(u12));
    };

    pub const Frame = enum (u14) {
        _,
        pub fn init(raw_value: u14) Frame {
            return @enumFromInt(raw_value);
        }
        pub fn raw(self: Frame) u14 {
            return @intFromEnum(self);
        }

        pub const num_addresses_per_frame = @as(usize, Offset.max.raw()) + 1;
    };

    pub const Virtual = packed struct (u32) {
        offset: Offset,
        page: Page,
        pub fn init(raw_value: u32) Virtual {
            return @bitCast(raw_value);
        }
        pub fn raw(self: Virtual) u32 {
            return @bitCast(self);
        }
    };

    pub const Physical = packed struct (u26) {
        offset: Offset,
        frame: Frame,

        pub fn init(raw_value: u26) Physical {
            return @bitCast(raw_value);
        }
        pub fn raw(self: Physical) u26 {
            return @bitCast(self);
        }

        pub fn device_slot(self: Physical) ?u3 {
            if (self.frame.raw() < device_0.frame.raw()) return null;
            return @intCast((self.frame.raw() - device_0.frame.raw()) / num_frames_per_device);
        }

        pub const ram_start = init(0x0_000_000);
        pub const ram_end = init(0x0_800_000);

        pub const device_0 = init(0x2_000_000);
        pub const device_1 = init(0x2_400_000);
        pub const device_2 = init(0x2_800_000);
        pub const device_3 = init(0x2_C00_000);
        pub const device_4 = init(0x3_000_000);
        pub const device_5 = init(0x3_400_000);
        pub const device_6 = init(0x3_800_000);
        pub const device_7 = init(0x3_C00_000);

        pub const num_frames_per_device: comptime_int = device_7.frame.raw() - device_6.frame.raw();
        pub const num_addresses_per_device = Frame.num_addresses_per_frame * num_frames_per_device;
    };

    pub const translation = struct {
        pub const ASN = enum (u4) {
            _,
            pub fn init(raw_value: u4) ASN {
                return @enumFromInt(raw_value);
            }
            pub fn raw(self: ASN) u4 {
                return @intFromEnum(self);
            }
        };

        pub const Access_Policy = enum(u2) {
            unprivileged = 0,
            kernel_entry_256 = 1,
            kernel_entry_4096 = 2,
            kernel_private = 3,
        };

        pub const Entry_Pair = struct {
            primary: Entry,
            secondary: Entry,
        };

        pub const Entry = packed struct (u32) {
            frame: Frame,
            update_frame_state: bool,
            present: bool,
            access: Access_Policy,
            tag: Tag,

            pub fn init(raw_value: u32) Entry {
                return @bitCast(raw_value);
            }
            pub fn raw(self: Entry) u32 {
                return @bitCast(self);
            }
        };

        pub const Entry_Group = enum (u2) {
            data_write = 0,
            data_read = 1,
            stack = 2,
            insn = 3,
        };

        pub const Entry_Address = packed struct (u12) {
            slot: Slot,
            group: Entry_Group,
            asn: ASN,

            pub fn init(raw_value: u12) Entry_Address {
                return @bitCast(raw_value);
            }
            pub fn raw(self: Entry_Address) u12 {
                return @bitCast(self);
            }

            pub const count = std.math.maxInt(u12) + 1;
        };

        // Information saved when handling a fault;
        // useful for determining what page needs to be loaded for a page fault.
        // N.B. Upper 20 bits are identical to virtual address
        pub const Info = packed struct(u32) {
            bus_dir: Control_Signals.Bus_Direction,
            bus_width: Control_Signals.Bus_Width,
            addr_space: Control_Signals.Address_Space,
            at_op: Control_Signals.Address_Translator_Op,
            _padding: u5 = 0,
            slot: Slot,
            tag: Tag,

            pub fn init(raw_value: u32) Info {
                return @bitCast(raw_value);
            }
            pub fn raw(self: Info) u32 {
                return @bitCast(self);
            }
        };
    };
};

const arch = @import("arch");
const std = @import("std");
