gpa: std.mem.Allocator,

cycles: std.ArrayListUnmanaged(Cycle),
slot_data: std.ArrayListUnmanaged(Slot_Data),
cycles_dedup: std.HashMapUnmanaged(Cycle_Handle, void, Cycle_Handle_Hash_Context, std.hash_map.default_max_load_percentage) = .{},
slot_data_dedup: std.HashMapUnmanaged(Slot_Data.Handle, void, Slot_Data_Handle_Hash_Context, std.hash_map.default_max_load_percentage) = .{},

// these are not populated until `assign_slots` is called:
slot_to_handle: []?Slot_Data.Handle,
handle_to_slot: []arch.microcode.Slot,

pub const Cycle_Handle = enum(u16) {
    _,
    pub fn init(raw_value: Raw) Cycle_Handle {
        return @enumFromInt(raw_value);
    }
    pub fn raw(self: Cycle_Handle) Raw {
        return @intFromEnum(self);
    }
    pub const Raw = std.meta.Tag(Cycle_Handle);
};
pub const Slot_Data = struct {
    cycles: [arch.microcode.Address.count_per_slot]Cycle_Handle,
    forced_slot: ?arch.microcode.Slot,
    remaining_cycles: Min_Max,

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

pub fn init(gpa: std.mem.Allocator) Microcode_Builder {
    const cycles = std.ArrayListUnmanaged(Cycle).initCapacity(gpa, arch.microcode.Slot.count) catch @panic("OOM");
    const slot_data = std.ArrayListUnmanaged(Slot_Data).initCapacity(gpa, arch.microcode.Slot.count) catch @panic("OOM");
    const slot_to_handle = gpa.alloc(?Slot_Data.Handle, arch.microcode.Slot.count) catch @panic("OOM");
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
        if (!std.mem.eql(u8, self.cycles.items[result.key_ptr.raw()].func_name, data.func_name)) {
            self.cycles.items[result.key_ptr.raw()].func_name = "";
        }
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

pub fn get_slot_data(self: *const Microcode_Builder, handle: Slot_Data.Handle) Slot_Data {
    return self.slot_data.items[handle.raw()];
}

pub fn complete_loop(self: *Microcode_Builder, handle: Cycle_Handle, next: Slot_Data.Handle) void {
    const cycle = &self.cycles.items[@intFromEnum(handle)];
    std.debug.assert(cycle.next_func != null);
    std.debug.assert(cycle.next_slot == next or cycle.next_slot == null);
    cycle.next_slot = next;

    // Normally this won't do anything, unless `complete_loop` gets called after `assign_slots`:
    self.update_cycle_continuation(cycle, next);
}

pub fn assign_slots(self: *Microcode_Builder) void {
    const slot_data = self.slot_data.items;
    const old_handle_to_slot_len = self.handle_to_slot.len;
    const num_new_handles = slot_data.len - old_handle_to_slot_len;
    if (num_new_handles == 0) return;

    if (self.handle_to_slot.len == 0) {
        self.handle_to_slot = self.gpa.alloc(arch.microcode.Slot, slot_data.len) catch @panic("OOM");
    } else {
        self.handle_to_slot = self.gpa.realloc(self.handle_to_slot, slot_data.len) catch @panic("OOM");
    }

    const new_handles = self.gpa.alloc(Slot_Data.Handle.Raw, num_new_handles) catch @panic("OOM");
    for (new_handles, old_handle_to_slot_len..) |*dest, handle| {
        dest.* = @intCast(handle);
    }

    std.sort.block(Slot_Data.Handle.Raw, new_handles, self.slot_assignment_sort_ctx(), Slot_Assignment_Sort_Context.less_than);

    var next_slot: arch.microcode.Slot.Raw = arch.microcode.Slot.interrupt.raw() + 1;
    for (new_handles) |raw_handle| {
        const handle: Slot_Data.Handle = @enumFromInt(raw_handle);

        const slot = slot_data[raw_handle].forced_slot orelse while (self.slot_to_handle[next_slot] != null) {
            // If an overflow happens here, we've run out of slots!
            next_slot += 1;
        } else arch.microcode.Slot.init(next_slot);


        if (self.slot_to_handle[slot.raw()] != null) {
            const old_cycles = slot_data[self.slot_to_handle[slot.raw()].?.raw()].cycles;
            const new_cycles = slot_data[raw_handle].cycles;
            const old_cycle = self.cycles.items[old_cycles[0].raw()];
            const new_cycle = self.cycles.items[new_cycles[0].raw()];
            std.debug.panic("Collision on microcode slot {}\nold cycle fn:  {s}\nnew cycle fn: {s}", .{ slot, old_cycle.func_name, new_cycle.func_name });
        }

        self.slot_to_handle[slot.raw()] = handle;
        self.handle_to_slot[raw_handle] = slot;
            
        if (slot.raw() == next_slot and next_slot < arch.microcode.Slot.last.raw()) {
            next_slot += 1;
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

pub fn generate_microcode(self: *Microcode_Builder, allocator: std.mem.Allocator) *[arch.microcode.Address.count]?Control_Signals {
    const uc = allocator.create([arch.microcode.Address.count]?Control_Signals) catch @panic("OOM");
    @memset(uc, null);

    const cycles = self.cycles.items;
    const handle_to_slot = self.handle_to_slot;

    for (self.slot_data.items, 0..) |slot_data, raw_slot_data_handle| {
        const slot = handle_to_slot[raw_slot_data_handle];
        for (slot_data.cycles, 0..) |cycle_handle, raw_flags| {
            const address = arch.microcode.Address {
                .flags = arch.microcode.Flags.init(@intCast(raw_flags)),
                .slot = slot,
            };
            uc[address.raw()] = cycles[@intFromEnum(cycle_handle)].signals;
        }
    }

    return uc;
}

pub fn generate_microcode_fn_names(self: *Microcode_Builder, allocator: std.mem.Allocator) *[arch.microcode.Slot.count][]const u8 {
    const fn_names = allocator.create([arch.microcode.Slot.count][]const u8) catch @panic("OOM");
    @memset(fn_names, "");

    const cycles = self.cycles.items;
    const handle_to_slot = self.handle_to_slot;

    for (self.slot_data.items, 0..) |slot_data, raw_slot_data_handle| {
        const slot = handle_to_slot[raw_slot_data_handle];
        for (slot_data.cycles) |cycle_handle| {
            const func_name = cycles[@intFromEnum(cycle_handle)].func_name;
            if (func_name.len > 0) {
                fn_names[slot.raw()] = func_name;
            }
        }
    }

    return fn_names;
}

fn update_cycle_continuation(self: *Microcode_Builder, cycle: *Cycle, handle: Slot_Data.Handle) void {
    if (handle.raw() >= self.handle_to_slot.len) return;

    cycle.signals.next = self.handle_to_slot[@intFromEnum(handle)];
    cycle.assigned_signals.insert(.next);
}

fn slot_assignment_sort_ctx(self: *const Microcode_Builder) Slot_Assignment_Sort_Context {
    return .{ .data = self.slot_data.items };
}
const Slot_Assignment_Sort_Context = struct {
    data: []const Slot_Data,
    pub fn less_than(ctx: Slot_Assignment_Sort_Context, lhs: Slot_Data.Handle.Raw, rhs: Slot_Data.Handle.Raw) bool {
        if (ctx.data[lhs].forced_slot) |_| {
            if (ctx.data[rhs].forced_slot) |_| {
                return false;
            } else {
                return true;
            }
        } else return false;
    }
};

fn slot_data_handle_ctx(self: *const Microcode_Builder) Slot_Data_Handle_Hash_Context {
    return .{ .data = self.slot_data.items };
}
const Slot_Data_Handle_Hash_Context = struct {
    data: []const Slot_Data,
    pub fn hash(ctx: Slot_Data_Handle_Hash_Context, handle: Slot_Data.Handle) u64 {
        return Slot_Data_Hash_Context.hash(undefined, ctx.data[handle.raw()]);
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
        std.hash.autoHash(&hasher, slot_data.cycles);
        std.hash.autoHash(&hasher, slot_data.forced_slot);
        return hasher.final();
    }
    pub fn eql(ctx: Slot_Data_Hash_Context, slot_data: Slot_Data, handle: Slot_Data.Handle) bool {
        return std.meta.eql(slot_data, ctx.data[@intFromEnum(handle)]);
    }
};

fn hash_cycle(cycle: Cycle) u64 {
    var hasher = std.hash.Wyhash.init(0);
    cycle.signals.hash(&hasher);
    if (cycle.next_func) |next_func| {
        std.hash.autoHash(&hasher, true);
        std.hash.autoHash(&hasher, next_func);
        std.hash.autoHash(&hasher, cycle.instruction_signature);
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
        if (cycle.next_func == null and handle_cycle.next_func == null) {
            return std.meta.eql(cycle.next_slot, handle_cycle.next_slot);
        } else {
            return cycle.next_func == handle_cycle.next_func
                and std.meta.eql(cycle.instruction_signature, handle_cycle.instruction_signature);
        }
    }
};

const log = std.log.scoped(.microcode_builder);

const Microcode_Builder = @This();
const Min_Max = @import("Min_Max.zig");
const Cycle = @import("Cycle.zig");
const isa = @import("isa");
const Control_Signals = arch.Control_Signals;
const Control_Signal = arch.Control_Signal;
const arch = @import("arch");
const bits = @import("bits");
const std = @import("std");
