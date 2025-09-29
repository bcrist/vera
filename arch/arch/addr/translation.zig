pub const Op = enum (u2) {
    none = 0,
    translate = 1,
    update = 2,
    invalidate = 3,

    pub inline fn init(raw_value: Raw) Op {
        return @enumFromInt(raw_value);
    }

    pub inline fn raw(self: Op) Raw {
        return @intFromEnum(self);
    }

    pub const format = fmt.format_enum_dec;

    pub const Raw = meta.Backing(Op);
};

pub const Access_Policy = enum (u2) {
    unprivileged = 0,
    kernel_entry_256 = 1,
    kernel_entry_4096 = 2,
    kernel_private = 3,

    pub inline fn init(raw_value: Raw) Access_Policy {
        return @enumFromInt(raw_value);
    }

    pub inline fn raw(self: Access_Policy) Raw {
        return @intFromEnum(self);
    }

    pub const format = fmt.format_enum_dec;

    pub const Raw = meta.Backing(Access_Policy);
};

pub const Pipeline_Policy = enum (u3) {
    any = 0,
    pipe_0_only = 4,
    pipe_1_only = 5,
    pipe_2_only = 6,
    pipe_3_only = 7,

    pub inline fn init(raw_value: Raw) Pipeline_Policy {
        return @enumFromInt(raw_value);
    }

    pub inline fn raw(self: Pipeline_Policy) Raw {
        return @intFromEnum(self);
    }

    pub const format = fmt.format_enum_dec;

    pub const Raw = meta.Backing(Pipeline_Policy);
};

pub const File = [Entry.Address.count]Entry_Pair;

pub const Entry_Pair = struct {
    primary: Entry,
    secondary: Entry,
};

pub const Entry = packed struct (u32) {
    frame: addr.Frame,
    access: Access_Policy,
    pipe: Pipeline_Policy,
    present: bool,
    tag: addr.Page.Tag,

    pub inline fn init(raw_value: Raw) Entry {
        return @bitCast(raw_value);
    }

    pub inline fn raw(self: Entry) Raw {
        return @bitCast(self);
    }

    pub const format = fmt.format_raw_hex;

    pub const Raw = meta.Backing(Entry);

    pub const Address = packed struct (u16) {
        slot: addr.Page.Slot,
        group: Group,
        asn6: ASN6,

        pub inline fn init(raw_value: Address.Raw) Address {
            return @bitCast(raw_value);
        }

        pub inline fn raw(self: Address) Address.Raw {
            return @bitCast(self);
        }

        pub const format = fmt.format_raw_hex;

        pub const Raw = meta.Backing(Address);
        pub const count = std.math.maxInt(Address.Raw) + 1;
    };

    pub const Group = enum (u2) {
        data_write = 0,
        data_read = 1,
        stack = 2,
        insn = 3,

        pub inline fn init(raw_value: Group.Raw) Group {
            return @enumFromInt(raw_value);
        }

        pub fn from_space_and_dsrc(space: addr.Space, dsrc: bus.D.Source) Group {
            return switch (space) {
                .physical => .data_read,
                .data => switch (dsrc) {
                    .system => .data_read,
                    .l, .dr_ir, .vabor => .data_write,
                },
                .stack => .stack,
                .insn => .insn,
            };
        }

        pub inline fn raw(self: Group) Group.Raw {
            return @intFromEnum(self);
        }

        pub const format = fmt.format_enum_dec;

        pub const Raw = meta.Backing(Group);
    };

    pub const ASN6 = enum (u6) {
        _,

        pub inline fn init(raw_value: ASN6.Raw) ASN6 {
            return @enumFromInt(raw_value);
        }

        pub inline fn from_asn(asn: reg.sr2.Value) ASN6 {
            return ASN6.init(@truncate(asn.raw()));
        }

        pub inline fn raw(self: ASN6) ASN6.Raw {
            return @intFromEnum(self);
        }

        pub const format = fmt.format_enum_dec;

        pub const Raw = meta.Backing(ASN6);
    };
};

const reg = @import("../reg.zig");
const bus = @import("../bus.zig");
const fmt = @import("../fmt.zig");
const addr = @import("../addr.zig");
const meta = @import("meta");
const std = @import("std");