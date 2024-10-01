pub const Space = enum (u2) {
    raw = 0,
    data = 1,
    stack = 2,
    insn = 3,

    pub inline fn init(raw_value: Raw) Space {
        return @enumFromInt(raw_value);
    }

    pub inline fn raw(self: Space) Raw {
        return @intFromEnum(self);
    }

    pub const format = fmt.format_enum;

    pub const Raw = std.meta.Tag(Space);
};

pub const Virtual = packed struct (u32) {
    offset: Offset,
    page: Page,

    pub inline fn init(raw_value: Raw) Virtual {
        return @bitCast(raw_value);
    }

    pub inline fn raw(self: Virtual) Raw {
        return @bitCast(self);
    }

    pub const format = fmt.format_raw_hex;

    pub const Raw = u32;

    pub const Base_SR_Index = enum (u4) {
        one = raw_from_sr1(.one),
        rp = raw_from_sr1(.rp),
        sp = raw_from_sr1(.sp),
        bp = raw_from_sr1(.bp),
        temp_1 = raw_from_sr1(.temp_1),
        zero = raw_from_sr2(.zero),
        ip = raw_from_sr2(.ip),
        asn = raw_from_sr2(.asn),
        next_ip = raw_from_sr2(.next_ip),
        kxp = raw_from_sr2(.kxp),
        uxp = raw_from_sr2(.uxp),
        temp_2 = raw_from_sr2(.temp_2),
        _,

        pub inline fn init(raw_value: Base_SR_Index.Raw) Base_SR_Index {
            return @enumFromInt(raw_value);
        }

        pub inline fn raw(self: Base_SR_Index) Base_SR_Index.Raw {
            return @intFromEnum(self);
        }

        fn raw_from_sr1(sr1: arch.SR1_Index) u4 {
            std.debug.assert(sr1.raw() < 8);
            return sr1.raw();
        }
        pub fn from_sr1(sr1: arch.SR1_Index) ?Base_SR_Index {
            return if (sr1.raw() < 8) @enumFromInt(sr1.raw()) else null;
        }

        fn raw_from_sr2(sr2: arch.SR2_Index) u4 {
            std.debug.assert(sr2.raw() < 8);
            return sr2.raw() + 8;
        }
        pub fn from_sr2(sr2: arch.SR2_Index) ?Base_SR_Index {
            return if (sr2.raw() < 8) @enumFromInt(sr2.raw() + 8) else null;
        }

        pub fn to_sr1(self: Base_SR_Index) ?arch.SR1_Index {
            const ord = @intFromEnum(self);
            return if (ord < 8) @enumFromInt(ord) else null;
        }

        pub fn to_sr2(self: Base_SR_Index) ?arch.SR2_Index {
            const ord = @intFromEnum(self);
            return if (ord >= 8) @enumFromInt(ord - 8) else null;
        }

        pub fn to_any(self: Base_SR_Index) arch.Any_SR_Index {
            return if (self.to_sr1()) |sr1| arch.Any_SR_Index.from_sr1(sr1) else arch.Any_SR_Index.from_sr2(self.to_sr2().?);
        }

        pub const format = fmt.format_enum;

        pub const Raw = std.meta.Tag(Base_SR_Index);
        pub const count = std.math.maxInt(Base_SR_Index.Raw) + 1;
    };

    pub const Microcode_Offset = enum (i6) {
        zero = 0,
        one = 1,
        two = 2,
        three = 3,
        four = 4,
        five = 5,
        six = 6,
        i16_from_dr = std.math.minInt(i6),
        i8_from_dr = std.math.minInt(i6) + 1,
        i8_x4_from_dr = std.math.minInt(i6) + 2,
        _,

        pub inline fn init(raw_value: Microcode_Offset.Raw) Microcode_Offset {
            return @enumFromInt(raw_value);
        }

        pub inline fn raw(self: Microcode_Offset) Microcode_Offset.Raw {
            return @intFromEnum(self);
        }

        pub const format = fmt.format_enum;

        pub const Raw = std.meta.Tag(Microcode_Offset);
        pub const max = std.math.maxInt(i6);
        pub const min = Microcode_Offset.i8_x4_from_dr.raw() + 1;
    };
};

pub const Physical = packed struct (u26) {
    offset: Offset,
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

    pub const Raw = u26;

    pub const device_0 = .{ .offset = Offset.zero, .frame = Frame.device_0 };
    pub const device_1 = .{ .offset = Offset.zero, .frame = Frame.device_1 };
    pub const device_2 = .{ .offset = Offset.zero, .frame = Frame.device_2 };
    pub const device_3 = .{ .offset = Offset.zero, .frame = Frame.device_3 };
    pub const device_4 = .{ .offset = Offset.zero, .frame = Frame.device_4 };
    pub const device_5 = .{ .offset = Offset.zero, .frame = Frame.device_5 };
    pub const device_6 = .{ .offset = Offset.zero, .frame = Frame.device_6 };
    pub const device_7 = .{ .offset = Offset.zero, .frame = Frame.device_7 };

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

    pub const Raw = u20; // why doesn't std.meta.Tag work on packed structs?
    pub const zero = init(0);
    pub const max = init(std.math.maxInt(Raw));
    pub const count = std.math.maxInt(Raw) + 1;
    pub const num_bytes_per_page = Offset.count;

    pub const Slot = enum (u6) {
        _,

        pub inline fn init(raw_value: Slot.Raw) Slot {
            return @enumFromInt(raw_value);
        }

        pub inline fn raw(self: Slot) Slot.Raw {
            return @intFromEnum(self);
        }

        pub const format = fmt.format_enum;

        pub const Raw = std.meta.Tag(Slot);
    };

    pub const Tag = enum (u14) {
        _,

        pub inline fn init(raw_value: Tag.Raw) Tag {
            return @enumFromInt(raw_value);
        }

        pub inline fn raw(self: Tag) Tag.Raw {
            return @intFromEnum(self);
        }

        pub const format = fmt.format_enum;

        pub const Raw = std.meta.Tag(Tag);
    };
};

pub const Frame = enum (u14) {
    zero = 0,
    block_transfer_control_frame = 0x3001,
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

    pub const Raw = std.meta.Tag(Frame);
    pub const count = std.math.maxInt(Raw) + 1;

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

pub const Offset = enum (u12) {
    _,

    pub inline fn init(raw_value: Raw) Offset {
        return @enumFromInt(raw_value);
    }

    pub inline fn raw(self: Offset) Raw {
        return @intFromEnum(self);
    }

    pub const format = fmt.format_enum_hex;

    pub const Raw = std.meta.Tag(Offset);
    pub const zero = init(0);
    pub const max = init(std.math.maxInt(Raw));
    pub const count = std.math.maxInt(Raw) + 1;
};

pub const Word_Offset = enum (u10) {
    _,

    pub inline fn init(raw_value: Raw) Word_Offset {
        return @enumFromInt(raw_value);
    }

    pub inline fn raw(self: Word_Offset) Raw {
        return @intFromEnum(self);
    }

    pub const format = fmt.format_enum_hex;

    pub const Raw = std.meta.Tag(Word_Offset);
    pub const zero = init(0);
    pub const max = init(std.math.maxInt(Raw));
    pub const count = std.math.maxInt(Raw) + 1;
};

pub const translation = @import("addr/translation.zig");

const fmt = @import("fmt.zig");
const arch = @import("../arch.zig");
const std = @import("std");
