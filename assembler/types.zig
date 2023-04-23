const std = @import("std");
const deep = @import("deep_hash_map");

pub const InternPool = deep.DeepRecursiveAutoHashMapUnmanaged(*const Type, void);

pub const Type = union(enum) {
    poison,
    symbol_def,
    constant: ConstantType,
    reg8: IndexedRegister,
    reg16: IndexedRegister,
    reg32: IndexedRegister,
    sr: SpecialRegister,
    absolute_address_base,
    data_address: AddressType,
    insn_address: AddressType,
    stack_address: AddressType,

    pub fn intern(self: *const Type, arena: std.mem.Allocator, gpa: std.mem.Allocator, pool: *InternPool) *const Type {
        var result = pool.getOrPut(gpa, self) catch @panic("OOM");
        if (result.found_existing) {
            return result.key_ptr.*;
        }

        errdefer _ = pool.remove(self);

        var t = arena.create(Type) catch @panic("OOM");
        errdefer arena.destroy(t);

        t.* = self.*;

        result.key_ptr.* = t;
        return t;
    }
};

pub const ConstantType = struct {
    signedness: ?std.builtin.Signedness,
};

pub const IndexedRegister = struct {
    index: u4,
    signedness: ?std.builtin.Signedness,
};

pub const SpecialRegister = enum {
    ip,
    sp,
    rp,
    bp,
    uxp,
    kxp,
    asn,
    stat,
};

pub const AddressType = struct {
    base: *const Type,
    offset: *const Type,
};

pub const builtin = struct {
    pub const poison = Type { .poison = {} };
    pub const symbol_def = Type { .symbol_def = {} };

    pub const constant = Type { .constant = .{ .signedness = null }};
    pub const constant_signed = Type { .constant = .{ .signedness = .signed }};
    pub const constant_unsigned = Type { .constant = .{ .signedness = .unsigned }};

    pub const absolute_address_base = Type { .absolute_address_base = {} };
    pub const absolute_address_data = Type { .data_address = .{
        .base = &absolute_address_base,
        .offset = &constant_unsigned,
    }};
    pub const absolute_address_insn = Type { .insn_address = .{
        .base = &absolute_address_base,
        .offset = &constant_unsigned,
    }};
    pub const absolute_address_stack = Type { .stack_address = .{
        .base = &absolute_address_base,
        .offset = &constant_unsigned,
    }};

    pub const ip = Type { .sr = .ip };
    pub const sp = Type { .sr = .sp };
    pub const rp = Type { .sr = .rp };
    pub const bp = Type { .sr = .bp };
    pub const uxp = Type { .sr = .uxp };
    pub const kxp = Type { .sr = .kxp };
    pub const asn = Type { .sr = .asn };
    pub const stat = Type { .sr = .stat };

    pub const ip_relative_data = Type { .data_address = .{
        .base = &ip,
        .offset = &constant_signed,
    }};
    pub const ip_relative_insn = Type { .insn_address = .{
        .base = &ip,
        .offset = &constant_signed,
    }};
    pub const sp_relative = Type { .stack_address = .{
        .base = &sp,
        .offset = &constant_signed,
    }};
};

pub fn initInternPool(gpa: std.mem.Allocator, pool: *InternPool) void {
    inline for (@typeInfo(@TypeOf(builtin)).Struct.decls) |decl| {
        pool.put(gpa, &@field(builtin, decl.name), {}) catch @panic("OOM");
    }
}
