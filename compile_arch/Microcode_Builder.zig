gpa: std.mem.Allocator,

cycles: std.ArrayListUnmanaged(Cycle),
slot_data: std.ArrayListUnmanaged(Slot_Data),
cycles_dedup: std.HashMapUnmanaged(Cycle_Handle, void, Cycle_Handle_Hash_Context, std.hash_map.default_max_load_percentage) = .{},
slot_data_dedup: std.HashMapUnmanaged(Slot_Data.Handle, void, Slot_Data_Handle_Hash_Context, std.hash_map.default_max_load_percentage) = .{},

// these are not populated until `assign_slots` is called:
slot_to_handle: []?Slot_Data.Handle,
handle_to_slot: []hw.microcode.Slot,

pub const Cycle_Handle = enum(u16) {
    _,
    pub const Raw = std.meta.Tag(Cycle_Handle);
};
pub const Slot_Data = struct {
    cycles: [hw.microcode.Address.count_per_slot]Cycle_Handle,
    slot: Slot_Location,
    acyclic: bool, // when true, this slot is part of a simple sequence or tree, but does not have any recursive/looping constructs
    pub const Handle = enum (u15) {
        _,
        pub fn init(raw_value: Raw) Handle {
            return @enumFromInt(raw_value);
        }
        pub fn raw(self: Handle) Raw {
            return @intFromEnum(self);
        }
        pub const Raw = std.meta.Tag(Handle);
    };
};
pub const Slot_Location = union (enum) {
    exact: hw.microcode.Slot,
    forced_bits: hw.microcode.Slot.Raw, // TODO don't include this in hashing; just update it to add more bits if necessary

    pub fn forced_slot_bits(self: Slot_Location) hw.microcode.Slot.Raw {
        return switch (self) {
            .exact => |slot| slot.raw(),
            .forced_bits => |forced_bits| forced_bits,
        };
    }

    pub fn for_continuation(cs: Control_Signals) Slot_Location {
        return .{ .forced_bits = hw.microcode.continuation_mask(cs) };
    }
};

pub fn init(gpa: std.mem.Allocator) Microcode_Builder {
    const cycles = std.ArrayListUnmanaged(Cycle).initCapacity(gpa, hw.microcode.Slot.count) catch @panic("OOM");
    const slot_data = std.ArrayListUnmanaged(Slot_Data).initCapacity(gpa, hw.microcode.Slot.count) catch @panic("OOM");
    const slot_to_handle = gpa.alloc(?Slot_Data.Handle, hw.microcode.Slot.count) catch @panic("OOM");
    @memset(slot_to_handle, null);
    return .{
        .gpa = gpa,
        .cycles = cycles,
        .slot_data = slot_data,
        .slot_to_handle = slot_to_handle,
        .handle_to_slot = &.{},
    };
}
pub fn deinit(self: *Microcode_Builder) void {
    self.cycles_dedup.deinit();
    self.slot_data_dedup.deinit();
    self.cycles.deinit();
    self.slot_data.deinit();
    self.gpa.free(self.slot_to_handle);
    if (self.handle_to_slot.len > 0) {
        self.gpa.free(self.handle_to_slot);
    }
}

pub fn intern_cycle(self: *Microcode_Builder, data: Cycle) Cycle_Handle {
    const result = self.cycles_dedup.getOrPutContextAdapted(self.gpa, data, self.cycle_ctx(), self.cycle_handle_ctx()) catch @panic("OOM");
    if (result.found_existing) {
        return result.key_ptr.*;
    } else {
        const handle: Cycle_Handle = @enumFromInt(self.cycles.items.len);
        self.cycles.append(self.gpa, data) catch @panic("OOM");
        result.key_ptr.* = handle;
        return handle;
    }
}

pub fn intern_slot_data(self: *Microcode_Builder, data: Slot_Data) Slot_Data.Handle {
    const result = self.slot_data_dedup.getOrPutContextAdapted(self.gpa, data, self.slot_data_ctx(), self.slot_data_handle_ctx()) catch @panic("OOM");
    if (result.found_existing) {
        return result.key_ptr.*;
    } else {
        const handle: Slot_Data.Handle = @enumFromInt(self.slot_data.items.len);
        self.slot_data.append(self.gpa, data) catch @panic("OOM");
        result.key_ptr.* = handle;
        return handle;
    }
}

pub fn complete_loop(self: *Microcode_Builder, handle: Cycle_Handle, next: Slot_Data.Handle) void {
    const cycle = &self.cycles.items[@intFromEnum(handle)];
    std.debug.assert(cycle.recursion_ptr != null);
    std.debug.assert(cycle.next_slot == next or cycle.next_slot == null);
    cycle.next_slot = next;

    const mask = hw.microcode.continuation_mask(cycle.signals);

    if (mask != 0) {
        switch (self.slot_data.items[@intFromEnum(next)].slot) {
            .exact => |slot| {
                if ((slot.raw() | mask) != slot.raw()) {
                    @panic("Impossible loop; don't use IJ/IK/IW from continuation when recursing, or recurse to a function that's not constrained to a specific slot");
                }
            },
            .forced_bits => |raw| {
                self.slot_data.items[@intFromEnum(next)].slot = .{ .forced_bits = raw | mask };
            },
        }
    }

    // Normally this won't do anything, unless `complete_loop` gets called after `assign_slots`:
    self.update_cycle_continuation(cycle, next);
}

pub fn assign_slots(self: *Microcode_Builder) void {
    const slot_data = self.slot_data.items;
    const old_handle_to_slot_len = self.handle_to_slot.len;
    const num_new_handles = slot_data.len - old_handle_to_slot_len;
    if (num_new_handles == 0) return;

    if (self.handle_to_slot.len == 0) {
        self.handle_to_slot = self.gpa.alloc(hw.microcode.Slot, slot_data.len) catch @panic("OOM");
    } else {
        self.handle_to_slot = self.gpa.realloc(self.handle_to_slot, slot_data.len) catch @panic("OOM");
    }

    const new_handles = self.gpa.alloc(Slot_Data.Handle.Raw, num_new_handles) catch @panic("OOM");
    for (new_handles, old_handle_to_slot_len..) |*dest, handle| {
        dest.* = @intCast(handle);
    }

    std.sort.block(Slot_Data.Handle.Raw, new_handles, self.slot_assignment_sort_ctx(), Slot_Assignment_Sort_Context.less_than);

    for (new_handles) |raw_handle| {
        const handle: Slot_Data.Handle = @enumFromInt(raw_handle);
        switch (slot_data[raw_handle].slot) {
            .exact => |slot| {
                if (self.slot_to_handle[slot.raw()] != null) {
                    std.debug.panic("Collision on microcode slot {}", .{ slot });
                }
                self.slot_to_handle[slot.raw()] = handle;
                self.handle_to_slot[raw_handle] = slot;
            },
            else => {},
        }
    }

    var next_slot: hw.microcode.Slot.Raw = hw.microcode.Slot.interrupt.raw() + 1;
    var last_forced_bits: hw.microcode.Slot.Raw = 0;

    for (new_handles) |raw_handle| {
        const handle: Slot_Data.Handle = @enumFromInt(raw_handle);
        switch (slot_data[raw_handle].slot) {
            .forced_bits => |forced_bits| {
                if (forced_bits != last_forced_bits) {
                    next_slot = if (forced_bits == 0) hw.microcode.Slot.interrupt.raw() + 1 else forced_bits;
                    last_forced_bits = forced_bits;
                } else {
                    next_slot |= forced_bits;
                }

                while (self.slot_to_handle[next_slot] != null) {
                    // If an overflow happens here, we've run out of slots!
                    next_slot = (next_slot + 1) | forced_bits;
                }

                self.slot_to_handle[next_slot] = handle;
                self.handle_to_slot[raw_handle] = hw.microcode.Slot.init(next_slot);

                if (next_slot < hw.microcode.Slot.last.raw()) {
                    next_slot += 1;
                }
            },
            else => {},
        }
    }

    for (self.cycles.items) |*cycle| {
        if (cycle.next_slot) |slot_data_handle| {
            if (slot_data_handle.raw() >= old_handle_to_slot_len) {
                self.update_cycle_continuation(cycle, slot_data_handle);
            }
        }
    }
}

pub fn generate_microcode(self: *Microcode_Builder, allocator: std.mem.Allocator) []?Control_Signals {
    const uc = allocator.alloc(?Control_Signals, hw.microcode.Address.count) catch @panic("OOM");
    @memset(uc, null);

    const cycles = self.cycles.items;
    const handle_to_slot = self.handle_to_slot;

    for (self.slot_data.items, 0..) |slot_data, raw_slot_data_handle| {
        const slot = handle_to_slot[raw_slot_data_handle];
        log.debug("Slot {}: {s}", .{ slot.raw(), cycles[@intFromEnum(slot_data.cycles[0])].func_name });
        for (slot_data.cycles, 0..) |cycle_handle, raw_flags| {
            const address = hw.microcode.Address {
                .flags = hw.microcode.Flags.init(@intCast(raw_flags)),
                .slot = slot,
            };
            uc[address.raw()] = cycles[@intFromEnum(cycle_handle)].signals;
        }
    }

    return uc;
}

fn update_cycle_continuation(self: *Microcode_Builder, cycle: *Cycle, handle: Slot_Data.Handle) void {
    if (handle.raw() >= self.handle_to_slot.len) return;

    const slot = self.handle_to_slot[@intFromEnum(handle)];
    const mask = hw.microcode.continuation_mask(cycle.signals);

    const continuation_slot = hw.microcode.Slot.init(
        (hw.microcode.unmasked_continuation(cycle.signals) & mask) | (slot.raw() & ~mask)
    );

    cycle.signals.c_ij = continuation_slot.ij();
    cycle.signals.c_ik = continuation_slot.ik();
    cycle.signals.c_iw = continuation_slot.iw();
}

fn slot_assignment_sort_ctx(self: *const Microcode_Builder) Slot_Assignment_Sort_Context {
    return .{ .data = self.slot_data.items };
}
const Slot_Assignment_Sort_Context = struct {
    data: []const Slot_Data,
    pub fn less_than(ctx: Slot_Assignment_Sort_Context, lhs: Slot_Data.Handle.Raw, rhs: Slot_Data.Handle.Raw) bool {
        const lhs_bits = ctx.data[lhs].slot.forced_slot_bits();
        const rhs_bits = ctx.data[rhs].slot.forced_slot_bits();
        // If a lot of slot bits are forced set (due to use of IJ/IK/IW from continuation) then
        // we want to allocate those slots first, since there are fewer possibilities to choose from.
        if (@popCount(lhs_bits) > @popCount(rhs_bits)) return true;
        if (@popCount(lhs_bits) < @popCount(rhs_bits)) return false;

        // We don't really care the ordering at this point,
        // but we'd like items with equal forced_slot_bits partitioned together.
        return lhs_bits < rhs_bits;
    }
};

fn slot_data_handle_ctx(self: *const Microcode_Builder) Slot_Data_Handle_Hash_Context {
    return .{ .data = self.slot_data.items };
}
const Slot_Data_Handle_Hash_Context = struct {
    data: []const Slot_Data,
    pub fn hash(ctx: Slot_Data_Handle_Hash_Context, handle: Slot_Data.Handle) u64 {
        var hasher = std.hash.Wyhash.init(0);
        std.hash.autoHash(&hasher, ctx.data[@intFromEnum(handle)]);
        return hasher.final();
    }
    pub fn eql(_: Slot_Data_Handle_Hash_Context, a: Slot_Data.Handle, b: Slot_Data.Handle) bool {
        return a == b;
    }
};

fn slot_data_ctx(self: *const Microcode_Builder) Slot_Data_Hash_Context {
    return .{ .data = self.slot_data.items };
}
const Slot_Data_Hash_Context = struct {
    data: []const Slot_Data,
    pub fn hash(_: Slot_Data_Hash_Context, slot_data: Slot_Data) u64 {
        var hasher = std.hash.Wyhash.init(0);
        std.hash.autoHash(&hasher, slot_data);
        return hasher.final();
    }
    pub fn eql(ctx: Slot_Data_Hash_Context, slot_data: Slot_Data, handle: Slot_Data.Handle) bool {
        return std.meta.eql(slot_data, ctx.data[@intFromEnum(handle)]);
    }
};

fn hash_cycle(cycle: Cycle) u64 {
    var hasher = std.hash.Wyhash.init(0);
    cycle.signals.hash(&hasher);
    if (cycle.recursion_ptr) |recursion_ptr| {
        std.hash.autoHash(&hasher, true);
        std.hash.autoHash(&hasher, recursion_ptr);
    } else {
        std.hash.autoHash(&hasher, false);
        std.hash.autoHash(&hasher, cycle.next_slot);
    }
    return hasher.final();
}

fn cycle_handle_ctx(self: *const Microcode_Builder) Cycle_Handle_Hash_Context {
    return .{ .data = self.cycles.items };
}
const Cycle_Handle_Hash_Context = struct {
    data: []const Cycle,
    pub fn hash(ctx: Cycle_Handle_Hash_Context, handle: Cycle_Handle) u64 {
        return hash_cycle(ctx.data[@intFromEnum(handle)]);
    }
    pub fn eql(_: Cycle_Handle_Hash_Context, a: Cycle_Handle, b: Cycle_Handle) bool {
        return a == b;
    }
};

fn cycle_ctx(self: *const Microcode_Builder) Cycle_Hash_Context {
    return .{ .data = self.cycles.items };
}
const Cycle_Hash_Context = struct {
    data: []const Cycle,
    pub fn hash(_: Cycle_Hash_Context, cycle: Cycle) u64 {
        return hash_cycle(cycle);
    }
    pub fn eql(ctx: Cycle_Hash_Context, cycle: Cycle, handle: Cycle_Handle) bool {
        const handle_cycle = ctx.data[@intFromEnum(handle)];
        if (!cycle.signals.eql(handle_cycle.signals)) return false;
        if (cycle.recursion_ptr != null or handle_cycle.recursion_ptr != null) {
            return std.meta.eql(cycle.recursion_ptr, handle_cycle.recursion_ptr);
        } else {
            return std.meta.eql(cycle.next_slot, handle_cycle.next_slot);
        }
    }
};

const log = std.log.scoped(.microcode_builder);

const Microcode_Builder = @This();
const Cycle = @import("Cycle.zig");
const isa = arch.isa;
const Control_Signals = hw.Control_Signals;
const Control_Signal = hw.Control_Signal;
const hw = arch.hw;
const arch = @import("lib_arch");
const bits = @import("bits");
const std = @import("std");
