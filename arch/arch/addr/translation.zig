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

    pub const format = fmt.format_enum;

    pub const Raw = std.meta.Tag(Op);
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

    pub const format = fmt.format_enum;

    pub const Raw = std.meta.Tag(Access_Policy);
};

pub const Translation_File = [Entry.Address.count]Entry_Pair;

pub const Entry_Pair = struct {
    primary: Entry,
    secondary: Entry,
};

pub const Entry = packed struct (u32) {
    frame: addr.Frame,
    update_frame_state: bool,
    present: bool,
    access: Access_Policy,
    tag: addr.Page.Tag,

    pub inline fn init(raw_value: Raw) Entry {
        return @bitCast(raw_value);
    }

    pub inline fn raw(self: Entry) Raw {
        return @bitCast(self);
    }

    pub const format = fmt.format_raw_hex;

    pub const Raw = u32;

    pub const Address = packed struct (u12) {
        slot: addr.Page.Slot,
        group: Group,
        asn4: ASN4,

        pub inline fn init(raw_value: Address.Raw) Address {
            return @bitCast(raw_value);
        }

        pub inline fn raw(self: Address) Address.Raw {
            return @bitCast(self);
        }

        pub const format = fmt.format_raw_hex;

        pub const Raw = u12;
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

        pub fn from_space_and_dir(space: addr.Space, dir: arch.D.Direction) Group {
            return switch (space) {
                .raw, .data => switch (dir) {
                    .write_from_l, .write_from_dr_ir => .data_write,
                    .none, .read => .data_read,
                },
                .stack => .stack,
                .insn => .insn,
            };
        }

        pub inline fn raw(self: Group) Group.Raw {
            return @intFromEnum(self);
        }

        pub const format = fmt.format_enum;

        pub const Raw = std.meta.Tag(Group);
    };

    pub const ASN4 = enum (u4) {
        _,

        pub inline fn init(raw_value: ASN4.Raw) ASN4 {
            return @enumFromInt(raw_value);
        }

        pub inline fn from_asn(asn: arch.Reg) ASN4 {
            return ASN4.init(@truncate(asn.raw()));
        }

        pub inline fn raw(self: ASN4) ASN4.Raw {
            return @intFromEnum(self);
        }

        pub const format = fmt.format_enum;

        pub const Raw = std.meta.Tag(ASN4);
    };
};

// Information saved when handling a fault;
// useful for determining what page needs to be loaded for a page fault.
// N.B. Upper 20 bits are identical to virtual address
pub const Info = packed struct(u32) {
    emode: arch.Execution_Mode,
    bus_dir: arch.D.Direction,
    bus_width: arch.D.Width,
    op: Op,
    space: addr.Space,
    _padding: u2 = 0,
    page: addr.Page,

    pub inline fn init(raw_value: Raw) Info {
        return @bitCast(raw_value);
    }

    pub inline fn raw(self: Info) Raw {
        return @bitCast(self);
    }

    pub const format = fmt.format_raw_hex;

    pub const Raw = u32;
};

const fmt = @import("../fmt.zig");
const addr = @import("../addr.zig");
const arch = @import("../../arch.zig");
const std = @import("std");