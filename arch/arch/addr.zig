pub const Space = enum (u2) {
    physical = 0, // data_write group when AT_OP is .update or .invalidate
    data = 1, // data_read group when AT_OP is .update or .invalidate and DSRC is not .system.  data_write group when DSRC is .system
    stack = 2,
    insn = 3,

    pub inline fn init(raw_value: Raw) Space {
        return @enumFromInt(raw_value);
    }

    pub inline fn raw(self: Space) Raw {
        return @intFromEnum(self);
    }

    pub const format = fmt.format_enum_dec;

    pub const Raw = meta.Backing(Space);
};

pub const Virtual = packed struct (u32) {
    page_offset: Page.Offset,
    page: Page,

    pub inline fn init(raw_value: Raw) Virtual {
        return @bitCast(raw_value);
    }

    pub inline fn raw(self: Virtual) Raw {
        return @bitCast(self);
    }

    pub const format = fmt.format_raw_hex;

    pub const Raw = meta.Backing(Virtual);

    pub const Base = enum (u4) {
        one = raw_from_sr1(.one),
        two = raw_from_sr1(.two),
        rp = raw_from_sr1(.rp),
        sp = raw_from_sr1(.sp),
        fp = raw_from_sr1(.fp),
        bp = raw_from_sr1(.bp),
        dp = raw_from_sr1(.dp),
        temp_1 = raw_from_sr1(.temp_1),
        zero = raw_from_sr2(.zero),
        ip = raw_from_sr2(.ip),
        asn = raw_from_sr2(.asn),
        next_ip = raw_from_sr2(.next_ip),
        kxp = raw_from_sr2(.kxp),
        uxp = raw_from_sr2(.uxp),
        axp = raw_from_sr2(.axp),
        temp_2 = raw_from_sr2(.temp_2),

        pub inline fn init(raw_value: Base.Raw) Base {
            return @enumFromInt(raw_value);
        }

        pub inline fn raw(self: Base) Base.Raw {
            return @intFromEnum(self);
        }

        fn raw_from_sr1(sr1: reg.sr1.Index) u4 {
            std.debug.assert(sr1.raw() < 8);
            return sr1.raw();
        }
        pub fn from_sr1(sr1: reg.sr1.Index) ?Base {
            return if (sr1.raw() < 8) @enumFromInt(sr1.raw()) else null;
        }

        fn raw_from_sr2(sr2: reg.sr2.Index) u4 {
            std.debug.assert(sr2.raw() < 8);
            return sr2.raw() + 8;
        }
        pub fn from_sr2(sr2: reg.sr2.Index) ?Base {
            return if (sr2.raw() < 8) @enumFromInt(sr2.raw() + 8) else null;
        }

        pub fn to_sr1(self: Base) ?reg.sr1.Index {
            const ord = @intFromEnum(self);
            return if (ord < 8) @enumFromInt(ord) else null;
        }

        pub fn to_sr2(self: Base) ?reg.sr2.Index {
            const ord = @intFromEnum(self);
            return if (ord >= 8) @enumFromInt(ord - 8) else null;
        }

        pub fn to_any(self: Base) reg.sr.Any_Index {
            return if (self.to_sr1()) |sr1| reg.sr.Any_Index.from_sr1(sr1) else reg.sr.Any_Index.from_sr2(self.to_sr2().?);
        }

        pub const format = fmt.format_enum_dec;

        pub const Raw = meta.Backing(Base);
        pub const count = std.math.maxInt(Base.Raw) + 1;
    };

    pub const Offset_Source = enum (u2) {
        zero,
        constant,
        i8_from_dr,
        i16_from_dr,

        pub inline fn init(raw_value: Offset_Source.Raw) Offset_Source {
            return @enumFromInt(raw_value);
        }

        pub inline fn raw(self: Offset_Source) Offset_Source.Raw {
            return @intFromEnum(self);
        }

        pub const format = fmt.format_enum_dec;

        pub const Raw = meta.Backing(Offset_Source);
    };
};

pub const Physical = packed struct (u26) {
    frame_offset: Frame.Offset,
    frame: Frame,

    pub inline fn init(raw_value: Raw) Physical {
        return @bitCast(raw_value);
    }

    pub inline fn raw(self: Physical) Raw {
        return @bitCast(self);
    }

    pub fn device_slot(self: Physical) ?u3 {
        return self.frame.device_slot();
    }

    pub const format = fmt.format_raw_hex;

    pub const Raw = meta.Backing(Physical);

    pub const device_0: Physical = .{ .offset = .zero, .frame = .device_0 };
    pub const device_1: Physical = .{ .offset = .zero, .frame = .device_1 };
    pub const device_2: Physical = .{ .offset = .zero, .frame = .device_2 };
    pub const device_3: Physical = .{ .offset = .zero, .frame = .device_3 };
    pub const device_4: Physical = .{ .offset = .zero, .frame = .device_4 };
    pub const device_5: Physical = .{ .offset = .zero, .frame = .device_5 };
    pub const device_6: Physical = .{ .offset = .zero, .frame = .device_6 };
    pub const device_7: Physical = .{ .offset = .zero, .frame = .device_7 };

    pub const num_frames_per_device = Frame.num_frames_per_device;
    pub const num_bytes_per_device = Frame.num_bytes_per_device;
};

pub const Page = packed struct (u20) {
    slot: Slot,
    tag: Tag,

    pub inline fn init(raw_value: Raw) Page {
        return @bitCast(raw_value);
    }

    pub inline fn raw(self: Page) Raw {
        return @bitCast(self);
    }

    pub const format = fmt.format_raw_hex;

    pub const Raw = meta.Backing(Page); // why doesn't std.meta.Tag work on packed structs?
    pub const zero = init(0);
    pub const max = init(std.math.maxInt(Raw));
    pub const count = std.math.maxInt(Raw) + 1;
    pub const num_bytes_per_page = Offset.count;

    pub const Slot = enum (u8) {
        _,

        pub inline fn init(raw_value: Slot.Raw) Slot {
            return @enumFromInt(raw_value);
        }

        pub inline fn raw(self: Slot) Slot.Raw {
            return @intFromEnum(self);
        }

        pub const format = fmt.format_enum_dec;

        pub const Raw = meta.Backing(Slot);
    };

    pub const Tag = enum (u12) {
        _,

        pub inline fn init(raw_value: Tag.Raw) Tag {
            return @enumFromInt(raw_value);
        }

        pub inline fn raw(self: Tag) Tag.Raw {
            return @intFromEnum(self);
        }

        pub const format = fmt.format_enum_dec;

        pub const Raw = meta.Backing(Tag);
    };

    pub const Offset = enum (u12) {
        _,

        pub inline fn init(raw_value: Offset.Raw) Offset {
            return @enumFromInt(raw_value);
        }

        pub inline fn raw(self: Offset) Offset.Raw {
            return @intFromEnum(self);
        }

        pub const format = fmt.format_enum_hex;

        pub const Raw = meta.Backing(Offset);
        pub const zero = Offset.init(0);
        pub const max = Offset.init(std.math.maxInt(Offset.Raw));
        pub const count = std.math.maxInt(Offset.Raw) + 1;
    };

    pub const Word_Offset = enum (u10) {
        _,

        pub inline fn init(raw_value: Word_Offset.Raw) Word_Offset {
            return @enumFromInt(raw_value);
        }

        pub inline fn raw(self: Word_Offset) Word_Offset.Raw {
            return @intFromEnum(self);
        }

        pub const format = fmt.format_enum_hex;

        pub const Raw = meta.Backing(Word_Offset);
        pub const zero = Word_Offset.init(0);
        pub const max = Word_Offset.init(std.math.maxInt(Word_Offset.Raw));
        pub const count = std.math.maxInt(Word_Offset.Raw) + 1;
    };
};

pub const Frame = enum (u14) {
    zero = 0,
    _,

    pub inline fn init(raw_value: Raw) Frame {
        return @enumFromInt(raw_value);
    }

    pub inline fn raw(self: Frame) Raw {
        return @intFromEnum(self);
    }

    pub fn device_slot(self: Frame) ?u3 {
        if (self.raw() < device_0.raw()) return null;
        return @intCast((self.raw() - device_0.raw()) / num_frames_per_device);
    }

    pub const format = fmt.format_enum_hex;

    pub const Raw = meta.Backing(Frame);
    pub const count = std.math.maxInt(Raw) + 1;

    pub const Offset = Page.Offset;
    pub const Word_Offset = Page.Word_Offset;

    pub const device_0 = init(0x3000);
    pub const device_1 = init(0x3200);
    pub const device_2 = init(0x3400);
    pub const device_3 = init(0x3600);
    pub const device_4 = init(0x3800);
    pub const device_5 = init(0x3A00);
    pub const device_6 = init(0x3C00);
    pub const device_7 = init(0x3E00);

    pub const num_frames_per_device: comptime_int = device_7.raw() - device_6.raw();
    pub const num_bytes_per_frame = Offset.count;
    pub const num_bytes_per_device = num_bytes_per_frame * num_frames_per_device;
};

pub const translation = @import("addr/translation.zig");

const reg = @import("reg.zig");
const fmt = @import("fmt.zig");
const meta = @import("meta");
const std = @import("std");
