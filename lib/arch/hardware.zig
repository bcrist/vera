pub const microcode_roms = @import("hardware/microcode_roms.zig");
pub const decode_roms = @import("hardware/decode_roms.zig");
pub const Control_Signals = @import("hardware/Control_Signals.zig");
pub const Control_Signal = std.meta.FieldEnum(Control_Signals);

// pub const Signed_Offset_For_Literal = i7; // note not all possible values are valid


pub const Pipeline = enum (u2) {
    zero = 0,
    one = 1,
    two = 2,
    three = 3,

    pub fn init(raw_value: Raw) Pipeline {
        return @enumFromInt(raw_value);
    }
    pub fn raw(self: Pipeline) Raw {
        return @intFromEnum(self);
    }

    pub const Raw = std.meta.Tag(Pipeline); 
    pub const count = std.math.maxInt(Raw) + 1;
};

pub const Execution_Mode = enum (u2) {
    normal = 0,
    interrupt = 1,
    fault = 2,
    interrupt_fault = 3,

    pub fn init(raw_value: Raw) Execution_Mode {
        return @enumFromInt(raw_value);
    }
    pub fn raw(self: Execution_Mode) Raw {
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
    pub const Raw = std.meta.Tag(Execution_Mode); 
    pub const count = std.math.maxInt(Raw) + 1;
};

pub const Power_Mode = enum (u1) {
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
        invalid_instruction_fault = 5,
        double_fault = 6,
        interrupt = 7,
        invalid_instruction = 0xFFF,
        _,
        pub fn init(raw_value: Raw) Slot {
            return @enumFromInt(raw_value);
        }
        pub fn raw(self: Slot) Raw {
            return @intFromEnum(self);
        }
        pub fn ij(self: Slot) IJ {
            return IJ.init(@truncate(self.raw()));
        }
        pub fn ik(self: Slot) IK {
            const raw_ik = self.raw() >> @bitSizeOf(IJ);
            return IK.init(@truncate(raw_ik));
        }
        pub fn iw(self: Slot) IW {
            const raw_iw = self.raw() >> (@bitSizeOf(IJ) + @bitSizeOf(IK));
            return IW.init(@truncate(raw_iw));
        }

        pub const Raw = std.meta.Tag(Slot); 
        pub const first = init(8);
        pub const last = init(std.math.maxInt(Raw));
        pub const count = std.math.maxInt(Raw) + 1;
    };

    pub fn continuation_mask(cs: Control_Signals) Slot.Raw {
        return bits.concat(.{
            @as(IJ.Raw, if (cs.ij_op == .from_continuation) 0xF else 0),
            @as(IK.Raw, if (cs.ik_op == .from_continuation) 0xF else 0),
            @as(IW.Raw, if (cs.iw_op == .from_continuation) 0xF else 0),
        });
    }

    pub fn unmasked_continuation(cs: Control_Signals) Slot.Raw {
        return bits.concat(.{ cs.c_ij.raw(), cs.c_ik.raw(), cs.c_iw.raw() });
    }

    pub fn continuation(cs: Control_Signals) Slot {
        return Slot.init(unmasked_continuation(cs) | continuation_mask(cs));
    }

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

        pub fn init(raw_value: Raw) Flags {
            return @bitCast(raw_value);
        }
        pub fn raw(self: Flags) Raw {
            return @bitCast(self);
        }

        pub fn zero(self: Flags) bool { return self.z; }
        pub fn negative(self: Flags) bool { return self.n; }
        pub fn positive(self: Flags) bool { return !self.z and !self.n; }
        pub fn carry(self: Flags) bool { return self.c; }
        pub fn overflow(self: Flags) bool { return self.v; }
        pub fn kernel(self: Flags) bool { return self.k; }
        pub fn unsigned_less_than(self: Flags) bool { return !self.c and !self.z; }
        pub fn unsigned_greater_than(self: Flags) bool { return self.c and !self.z; }
        pub fn signed_less_than(self: Flags) bool { return !self.z and self.n != self.v; }
        pub fn signed_greater_than(self: Flags) bool { return !self.z and self.n == self.v; }

        pub const Raw = u5;
    };

    pub const Address = packed struct (u17) {
        flags: Flags,
        slot: Slot,

        pub fn init(raw_value: Raw) Address {
            return @bitCast(raw_value);
        }
        pub fn raw(self: Address) Raw {
            return @bitCast(self);
        }

        pub const Raw = u17;
        pub const count = std.math.maxInt(Raw) + 1;
        pub const count_per_slot = std.math.maxInt(Flags.Raw) + 1;
    };

    pub const Data = [Address.count]Control_Signals;
};

pub const decode = struct {
    pub const Address = packed struct (u17) {
        d: D,
        mode: Control_Signals.ID_Mode,

        pub fn init(raw_value: Raw) Address {
            return @bitCast(raw_value);
        }
        pub fn raw(self: Address) Raw {
            return @bitCast(self);
        }

        pub const Raw = u17;
        pub const count = std.math.maxInt(Raw) + 1;
    };

    pub const Result = packed struct (u24) {
        slot: microcode.Slot,
        ij: IJ,
        ik: IK,
        iw: IW,
        pub fn init(raw_value: Raw) Result {
            return @bitCast(raw_value);
        }
        pub fn raw(self: Result) Raw {
            return @bitCast(self);
        }
        pub const Raw = u24;
    };
};

pub const RSN = enum (u6) {
    _,
    pub fn init(raw_value: Raw) RSN {
        return @enumFromInt(raw_value);
    }
    pub fn raw(self: RSN) Raw {
        return @intFromEnum(self);
    }

    pub const Raw = std.meta.Tag(RSN);
    pub const count = std.math.maxInt(Raw) + 1;
};

pub const Register_Index = u4;
pub const register_count = std.math.maxInt(Register_Index) + 1;

pub const IJ = enum (Register_Index) {
    _,
    pub fn init(raw_value: Register_Index) IJ {
        return @enumFromInt(raw_value);
    }
    pub fn raw(self: IJ) Register_Index {
        return @intFromEnum(self);
    }

    pub const Raw = std.meta.Tag(IJ);
    pub const max = std.math.maxInt(Raw);
};

pub const IK = enum (Register_Index) {
    _,
    pub fn init(raw_value: Register_Index) IK {
        return @enumFromInt(raw_value);
    }
    pub fn raw(self: IK) Register_Index {
        return @intFromEnum(self);
    }

    pub const Raw = std.meta.Tag(IK);
    pub const max = std.math.maxInt(Raw);
};

pub const IW = enum (Register_Index) {
    _,
    pub fn init(raw_value: Register_Index) IW {
        return @enumFromInt(raw_value);
    }
    pub fn raw(self: IW) Register_Index {
        return @intFromEnum(self);
    }

    pub const Raw = std.meta.Tag(IW);
    pub const max = std.math.maxInt(Raw);
};

pub const R = enum (u16) {
    _,
    pub fn init(raw_value: Raw) R {
        return @enumFromInt(raw_value);
    }
    pub fn raw(self: R) Raw {
        return @intFromEnum(self);
    }
    pub const Raw = std.meta.Tag(R);
};

pub const SRL = enum (u16) {
    _,
    pub fn init(raw_value: Raw) SRL {
        return @enumFromInt(raw_value);
    }
    pub fn raw(self: SRL) Raw {
        return @intFromEnum(self);
    }
    pub const Raw = std.meta.Tag(SRL);
};
pub const SRH = enum (u16) {
    _,
    pub fn init(raw_value: Raw) SRH {
        return @enumFromInt(raw_value);
    }
    pub fn raw(self: SRH) Raw {
        return @intFromEnum(self);
    }
    pub const Raw = std.meta.Tag(SRH);
};
pub const SR = packed struct (u32) {
    lo: SRL,
    hi: SRH,
    pub fn init(raw_value: Raw) SR {
        return @bitCast(raw_value);
    }
    pub fn raw(self: SR) Raw {
        return @bitCast(self);
    }
    pub const Raw = u32;
};

pub const Register_Set = struct {
    reg: [register_count]R,
    sr1: [Control_Signals.SR1_Index.count]SR,
    sr2: [Control_Signals.SR2_Index.count]SR,
};

pub const JL = enum (u16) {
    _,
    pub fn init(raw_value: Raw) JL {
        return @enumFromInt(raw_value);
    }
    pub fn raw(self: JL) Raw {
        return @intFromEnum(self);
    }
    pub const Raw = std.meta.Tag(JL);
};
pub const JH = enum (u16) {
    _,
    pub fn init(raw_value: Raw) JH {
        return @enumFromInt(raw_value);
    }
    pub fn raw(self: JH) Raw {
        return @intFromEnum(self);
    }
    pub const Raw = std.meta.Tag(JH);
};
pub const J = packed struct (u32) {
    lo: JL,
    hi: JH,
    pub fn init(raw_value: Raw) J {
        return @bitCast(raw_value);
    }
    pub fn raw(self: J) Raw {
        return @bitCast(self);
    }
    pub const Raw = u32;
};

pub const K = enum (u16) {
    _,
    pub fn init(raw_value: Raw) K {
        return @enumFromInt(raw_value);
    }
    pub fn raw(self: K) Raw {
        return @intFromEnum(self);
    }
    pub const Raw = std.meta.Tag(K);
};

pub const LL = enum (u16) {
    _,
    pub fn init(raw_value: Raw) LL {
        return @enumFromInt(raw_value);
    }
    pub fn raw(self: LL) Raw {
        return @intFromEnum(self);
    }
    pub const Raw = std.meta.Tag(LL);
};
pub const LH = enum (u16) {
    _,
    pub fn init(raw_value: Raw) LH {
        return @enumFromInt(raw_value);
    }
    pub fn raw(self: LH) Raw {
        return @intFromEnum(self);
    }
    pub const Raw = std.meta.Tag(LH);
};
pub const L = packed struct (u32) {
    lo: LL,
    hi: LH,
    pub fn init(raw_value: Raw) L {
        return @bitCast(raw_value);
    }
    pub fn raw(self: L) Raw {
        return @bitCast(self);
    }
    pub const Raw = u32;
};

pub const D = enum (u16) {
    _,
    pub fn init(raw_value: Raw) D {
        return @enumFromInt(raw_value);
    }
    pub fn raw(self: D) Raw {
        return @intFromEnum(self);
    }
    pub fn raw8(self: D) u8 {
        return @truncate(@intFromEnum(self));
    }
    pub const Raw = std.meta.Tag(D);
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

    pub fn init(raw_value: Raw) Status {
        return @bitCast(raw_value);
    }
    pub fn raw(self: Status) Raw {
        return @bitCast(self);
    }
    pub const Raw = u16;
};

pub const addr = struct {
    pub const Slot = enum (u6) {
        _,
        pub fn init(raw_value: Raw) Slot {
            return @enumFromInt(raw_value);
        }
        pub fn raw(self: Slot) Raw {
            return @intFromEnum(self);
        }
        pub const Raw = std.meta.Tag(Slot);
    };

    pub const Tag = enum (u14) {
        _,
        pub fn init(raw_value: Raw) Tag {
            return @enumFromInt(raw_value);
        }
        pub fn raw(self: Tag) Raw {
            return @intFromEnum(self);
        }
        pub const Raw = std.meta.Tag(Tag);
    };

    pub const Page = packed struct (u20) {
        slot: Slot,
        tag: Tag,

        pub fn init(raw_value: Raw) Page {
            return @bitCast(raw_value);
        }
        pub fn raw(self: Page) Raw {
            return @bitCast(self);
        }
        pub const Raw = u20; // why doesn't std.meta.Tag work on packed structs?
        pub const zero = init(0);
        pub const max = init(std.math.maxInt(Raw));
        pub const count = std.math.maxInt(Raw) + 1;
        pub const num_addresses_per_page = Offset.count;
    };

    pub const Offset = enum (u12) {
        _,
        pub fn init(raw_value: Raw) Offset {
            return @enumFromInt(raw_value);
        }
        pub fn raw(self: Offset) Raw {
            return @intFromEnum(self);
        }
        pub const Raw = std.meta.Tag(Offset);
        pub const zero = init(0);
        pub const max = init(std.math.maxInt(Raw));
        pub const count = std.math.maxInt(Raw) + 1;
    };

    pub const Frame = enum (u14) {
        _,
        pub fn init(raw_value: Raw) Frame {
            return @enumFromInt(raw_value);
        }
        pub fn raw(self: Frame) Raw {
            return @intFromEnum(self);
        }

        pub const Raw = std.meta.Tag(Frame);
        pub const num_addresses_per_frame = Offset.count;
    };

    pub const Virtual = packed struct (u32) {
        offset: Offset,
        page: Page,
        pub fn init(raw_value: Raw) Virtual {
            return @bitCast(raw_value);
        }
        pub fn raw(self: Virtual) Raw {
            return @bitCast(self);
        }
        pub const Raw = u32;
    };

    pub const Physical = packed struct (u26) {
        offset: Offset,
        frame: Frame,

        pub fn init(raw_value: Raw) Physical {
            return @bitCast(raw_value);
        }
        pub fn raw(self: Physical) Raw {
            return @bitCast(self);
        }

        pub fn device_slot(self: Physical) ?u3 {
            if (self.frame.raw() < interrupt_controller.frame.raw()) return null;
            return @intCast((self.frame.raw() - interrupt_controller.frame.raw()) / num_frames_per_device);
        }

        pub const Raw = u26;

        pub const ram_start = init(0x0_000_000);

        pub const interrupt_controller        = init(0x2_000_000);
        pub const block_transfer_controller_0 = init(0x2_001_000);
        pub const block_transfer_controller_1 = init(0x2_002_000);
        pub const frame_tracker               = init(0x2_003_000);
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
            pub fn init(raw_value: Raw) ASN {
                return @enumFromInt(raw_value);
            }
            pub fn raw(self: ASN) Raw {
                return @intFromEnum(self);
            }
            pub const Raw = std.meta.Tag(ASN);
        };

        pub const Access_Policy = enum (u2) {
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

            pub fn init(raw_value: Raw) Entry {
                return @bitCast(raw_value);
            }
            pub fn raw(self: Entry) Raw {
                return @bitCast(self);
            }
            pub const Raw = u32;
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

            pub fn init(raw_value: Raw) Entry_Address {
                return @bitCast(raw_value);
            }
            pub fn raw(self: Entry_Address) Raw {
                return @bitCast(self);
            }

            pub const Raw = u12;
            pub const count = std.math.maxInt(Raw) + 1;
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

            pub fn init(raw_value: Raw) Info {
                return @bitCast(raw_value);
            }
            pub fn raw(self: Info) Raw {
                return @bitCast(self);
            }
            pub const Raw = u32;
        };
    };
};

const arch = @import("lib_arch");
const bits = @import("bits");
const std = @import("std");
