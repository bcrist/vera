pub const J = enum (u32) {
    _,

    pub inline fn init(raw_value: Raw) J {
        return @enumFromInt(raw_value);
    }

    pub inline fn raw(self: J) Raw {
        return @intFromEnum(self);
    }

    pub const format = fmt.format_raw_hex;

    pub const Raw = meta.Backing(J);

    pub const Read_Index = enum (reg.gpr.Index) {
        _,

        pub inline fn init(raw_value: Read_Index.Raw) Read_Index {
            return @enumFromInt(raw_value);
        }

        pub inline fn raw(self: Read_Index) Read_Index.Raw {
            return @intFromEnum(self);
        }

        pub const format = fmt.format_enum_dec;

        pub const Raw = meta.Backing(Read_Index);
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

        pub const format = fmt.format_enum_dec;

        pub const Raw = meta.Backing(Source);
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

    pub const Raw = meta.Backing(K);

    pub const Source = enum (u4) {
        zero = 0,
        dr_byte_1_sx = 1,
        dr_byte_2_sx = 2,
        dr_byte_21_sx = 3,
        krio_zx = 4,
        krio_sx = 5,
        krio_bit = 6,
        krio_bit_inv = 7,
        vao = 8,
        kr = 9,
        sr1 = 10,
        sr2 = 11,
        // 12 -> vao
        // 13 -> kr
        // 14 -> sr1
        // 15 -> sr2

        pub inline fn init(raw_value: Source.Raw) Source {
            return @enumFromInt(raw_value);
        }

        pub inline fn raw(self: Source) Source.Raw {
            return @intFromEnum(self);
        }

        pub const format = fmt.format_enum_dec;

        pub const Raw = meta.Backing(Source);
    };

    pub const Read_Index = enum (reg.gpr.Index) {
        _,

        pub inline fn init(raw_value: Read_Index.Raw) Read_Index {
            return @enumFromInt(raw_value);
        }

        pub inline fn raw(self: Read_Index) Read_Index.Raw {
            return @intFromEnum(self);
        }

        pub const format = fmt.format_enum_dec;

        pub const Raw = meta.Backing(Read_Index);
        pub const max = std.math.maxInt(Read_Index.Raw);
    };

    pub const Read_Index_Offset = enum (u5) {
        _,

        pub inline fn init(raw_value: Read_Index_Offset.Raw) Read_Index_Offset {
            return @enumFromInt(raw_value);
        }

        pub inline fn init_signed(raw_value: Read_Index_Offset.Raw_Signed) Read_Index_Offset {
            return .init(@bitCast(raw_value));
        }

        pub inline fn raw(self: Read_Index_Offset) Read_Index_Offset.Raw {
            return @intFromEnum(self);
        }

        pub inline fn raw_signed(self: Read_Index_Offset) Read_Index_Offset.Raw_Signed {
            return @bitCast(@intFromEnum(self));
        }

        pub const format = fmt.format_enum_dec;

        pub const Raw = meta.Backing(Read_Index_Offset);
        pub const Raw_Signed = std.meta.Int(.signed, @bitSizeOf(Read_Index_Offset.Raw));
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

    pub const Raw = meta.Backing(L);

    pub const Source = enum (u2) {
        compute_or_d = 0, // D bus when compute_unit == .none
        flags = 1,
        status = 2,
        last_d = 3,

        pub inline fn init(raw_value: Source.Raw) Source {
            return @enumFromInt(raw_value);
        }

        pub inline fn raw(self: Source) Source.Raw {
            return @intFromEnum(self);
        }

        pub const format = fmt.format_enum_dec;

        pub const Raw = meta.Backing(Source);
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

    pub const Raw = meta.Backing(D);

    pub const Source = enum (u2) {
        system = 0,
        l = 1,
        dr_ir = 2, // low half is IR if DRW == 0, otherwise full DR is presented
        vabor = 3,

        pub inline fn init(raw_value: Source.Raw) Source {
            return @enumFromInt(raw_value);
        }

        pub inline fn raw(self: Source) Source.Raw {
            return @intFromEnum(self);
        }

        pub const format = fmt.format_enum_dec;

        pub const Raw = meta.Backing(Source);
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

        pub const format = fmt.format_enum_dec;

        pub const Raw = meta.Backing(Width);
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

    pub const Raw = meta.Backing(DA);
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

    pub const Raw = meta.Backing(DB);
};

const reg = @import("reg.zig");
const fmt = @import("fmt.zig");
const meta = @import("meta");
const std = @import("std");
