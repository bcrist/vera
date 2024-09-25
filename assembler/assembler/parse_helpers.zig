pub fn parse_mnemonic(self: anytype) ?isa.Mnemonic {
    const has_sync = @hasDecl(@TypeOf(self.*), "sync_to_end_of_line");
    if (has_sync and self.sync_to_end_of_line) return null;
    if (!self.try_token(.id)) return null;

    const mnemonic_str = self.token_location(self.next_token - 1);
    if (mnemonic_map.get(mnemonic_str)) |mnemonic| {
        return mnemonic;
    }

    self.record_error_rel("Unrecognized mnemonic", -1);
    if (has_sync) self.sync_to_end_of_line = true;
    return null;
}

pub fn try_suffix(self: anytype) isa.Mnemonic_Suffix {
    if (self.try_token(.dot)) {
        if (self.try_token(.id)) {
            const suffix_str = self.token_location(self.next_token - 1);
            if (suffix_map.get(suffix_str)) |suffix| {
                return suffix;
            }
            self.record_error_rel("Unrecognized mnemonic suffix", -1);
        } else {
            self.record_error("Expected mnemonic suffix");
        }
    }
    return .none;
}

pub fn parse_suffix(self: anytype, swap_params: *bool) isa.Mnemonic_Suffix {
    const suffix1 = try_suffix(self);
    const suffix2 = try_suffix(self);
    return switch (suffix1) {
        .none => suffix2,
        .lu => switch (suffix2) {
            .none => suffix1,
            .gu => .lu_gu,
            .z => .lu_z,
            else => blk: {
                self.record_error_rel("Invalid mnemonic suffix combination", -1);
                break :blk suffix1;
            },
        },
        .gu => switch (suffix2) {
            .none => suffix1,
            .lu => blk: {
                swap_params.* = true;
                break :blk .lu_gu;
            },
            .z => .gu_z,
            else => blk: {
                self.record_error_rel("Invalid mnemonic suffix combination", -1);
                break :blk suffix1;
            },
        },
        .z => switch (suffix2) {
            .none => suffix1,
            .lu => blk: {
                swap_params.* = true;
                break :blk .lu_z;
            },
            .gu => blk: {
                swap_params.* = true;
                break :blk .gu_z;
            },
            .ls => blk: {
                swap_params.* = true;
                break :blk .ls_z;
            },
            .gs => blk: {
                swap_params.* = true;
                break :blk .gs_z;
            },
            .n => blk: {
                swap_params.* = true;
                break :blk .n_z;
            },
            .p => blk: {
                swap_params.* = true;
                break :blk .p_z;
            },
            else => blk: {
                self.record_error_rel("Invalid mnemonic suffix combination", -1);
                break :blk suffix1;
            },
        },
        .ls => switch (suffix2) {
            .none => suffix1,
            .gs => .ls_gs,
            .z => .ls_z,
            else => blk: {
                self.record_error_rel("Invalid mnemonic suffix combination", -1);
                break :blk suffix1;
            },
        },
        .gs =>  switch (suffix2) {
            .none => suffix1,
            .ls => blk: {
                swap_params.* = true;
                break :blk .ls_gs;
            },
            .z => .gs_z,
            else => blk: {
                self.record_error_rel("Invalid mnemonic suffix combination", -1);
                break :blk suffix1;
            },
        },
        .n => switch (suffix2) {
            .none => suffix1,
            .z => .n_z,
            .p => .n_p,
            else => blk: {
                self.record_error_rel("Invalid mnemonic suffix combination", -1);
                break :blk suffix1;
            },
        },
        .p => switch (suffix2) {
            .none => suffix1,
            .z => .p_z,
            .n => blk: {
                swap_params.* = true;
                break :blk .n_p;
            },
            else => blk: {
                self.record_error_rel("Invalid mnemonic suffix combination", -1);
                break :blk suffix1;
            },
        },
        else => switch (suffix2) {
            .none => suffix1,
            else => blk: {
                self.record_error_rel("Invalid mnemonic suffix combination", -1);
                break :blk suffix1;
            },
        }
    };
}

pub const mnemonic_map = case_insensitive_enum_map(isa.Mnemonic, .{
    .excluded_values = &.{ "_reserved" },
}, .{});

pub const suffix_map = case_insensitive_enum_map(isa.Mnemonic_Suffix, .{
    .excluded_values = &.{ "none" },
    .excluded_chars = "_",
}, .{
    .{ "eq", .z },
    .{ "neq", .nz },
    .{ "ltu", .lu },
    .{ "lts", .ls },
    .{ "leu", .ngu },
    .{ "les", .ngs },
    .{ "gtu", .gu },
    .{ "gts", .gs },
    .{ "geu", .nlu },
    .{ "ges", .nls },
    .{ "nltu", .nlu },
    .{ "nlts", .nls },
    .{ "nleu", .gu },
    .{ "nles", .gs },
    .{ "ngtu", .ngu },
    .{ "ngts", .ngs },
    .{ "ngeu", .lu },
    .{ "nges", .ls },
    .{ "dw", .w },
    .{ "dr", .r },
});

pub const Case_Insensitive_Enum_Map_Options = struct {
    excluded_values: []const []const u8 = &.{},
    excluded_chars: []const u8 = "",
};
pub fn case_insensitive_enum_map(comptime T: type, comptime options: Case_Insensitive_Enum_Map_Options, comptime extra_entries: anytype) std.StaticStringMapWithEql(T, std.ascii.eqlIgnoreCase) {
    comptime var tuple_types: []const type = &.{};

    outer: inline for (std.enums.values(T)) |val| {
        for (options.excluded_values) |excluded| {
            if (std.mem.eql(u8, @tagName(val), excluded)) {
                continue :outer;
            }
        }
        if (options.excluded_chars.len > 0) {
            if (std.mem.indexOfAny(u8, @tagName(val), options.excluded_chars) != null) {
                continue :outer;
            }
        }

        tuple_types = tuple_types ++ .{ @TypeOf(.{ @tagName(val), val }) };
    }
    inline for (extra_entries) |entry| {
        tuple_types = tuple_types ++ .{ @TypeOf(entry) };
    }

    comptime var entries: std.meta.Tuple(tuple_types) = undefined;
    comptime var n = 0;
    outer: inline for (std.enums.values(T)) |val| {
        for (options.excluded_values) |excluded| {
            if (std.mem.eql(u8, @tagName(val), excluded)) {
                continue :outer;
            }
        }
        if (options.excluded_chars.len > 0) {
            if (std.mem.indexOfAny(u8, @tagName(val), options.excluded_chars) != null) {
                continue :outer;
            }
        }

        entries[n] = .{ @tagName(val), val };
        n += 1;
    }
    inline for (extra_entries) |entry| {
        entries[n] = entry;
        n += 1;
    }

    return std.StaticStringMapWithEql(T, std.ascii.eqlIgnoreCase).initComptime(entries);
}

const isa = @import("isa");
const arch = @import("arch"); 
const std = @import("std");
