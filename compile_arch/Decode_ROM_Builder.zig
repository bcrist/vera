allocator: std.mem.Allocator,
entries: []Entry,

pub const Entry = struct {
    instruction_encoding: ?Instruction_Encoding = null,
    slot_handle: ?Slot_Data.Handle = null,
    ij: hw.IJ = hw.IJ.init(0),
    ik: hw.IK = hw.IK.init(0),
    iw: hw.IW = hw.IW.init(0),
};

pub fn init(allocator: std.mem.Allocator) Decode_ROM_Builder {
    const entries = allocator.alloc(Entry, hw.decode.Address.count) catch @panic("OOM");
    @memset(entries, .{});
    return .{
        .allocator = allocator,
        .entries = entries,
    };
}
pub fn deinit(self: *Decode_ROM_Builder) void {
    self.allocator.free(self.entries);
}

pub fn add_entry(self: *Decode_ROM_Builder, addr: hw.decode.Address, entry: Entry) void {
    std.debug.assert(entry.slot_handle != null);
    const addr_raw = addr.raw();
    const existing = self.entries[addr_raw];
    if (existing.slot_handle) |existing_handle| {
        if (existing_handle != entry.slot_handle
            or existing.ij != entry.ij
            or existing.ik != entry.ik
            or existing.iw != entry.iw
        ) {
            var buf: [32768]u8 = undefined;
            var stream = std.io.fixedBufferStream(&buf);
            var writer = stream.writer();

            if (existing_handle != entry.slot_handle) {
                writer.print("Handle: {d: >30} != {d}\n", .{ existing_handle.raw(), entry.slot_handle.?.raw() }) catch @panic("IO Error");
            }
            if (existing.ij != entry.ij) {
                writer.print("IJ:     {d: >30} != {d}\n", .{ existing.ij.raw(), entry.ij.raw() }) catch @panic("IO Error");
            }
            if (existing.ik != entry.ik) {
                writer.print("IK:     {d: >30} != {d}\n", .{ existing.ik.raw(), entry.ik.raw() }) catch @panic("IO Error");
            }
            if (existing.iw != entry.iw) {
                writer.print("IW:     {d: >30} != {d}\n", .{ existing.iw.raw(), entry.iw.raw() }) catch @panic("IO Error");
            }

            // inline for (std.enums.values(hw.Control_Signal)) |field| {
            //     if (@field(existing.
            // }

            std.debug.panic("Decode address .{s} 0x{X} already used:\n{s}", .{ @tagName(addr.mode), addr.d.raw(), stream.getWritten() });
        }
    } else self.entries[addr_raw] = entry;
}

pub fn get_entry(self: *Decode_ROM_Builder, addr: hw.decode.Address) Entry {
    return self.entries[addr.raw()];
}

pub fn generate_rom_data(self: *Decode_ROM_Builder, allocator: std.mem.Allocator, microcode: *const Microcode_Builder) []hw.decode.Result {
    const data = allocator.alloc(hw.decode.Result, hw.decode.Address.count) catch @panic("OOM");

    for (self.entries, 0..) |entry, addr| {
        if (entry.slot_handle) |handle| {
            if (handle.raw() >= microcode.handle_to_slot.len) {
                @panic("microcode.assign_slots() must be called before decode_rom.build_rom_data()");
            }
            data[addr] = .{
                .slot = microcode.handle_to_slot[handle.raw()],
                .ij = entry.ij,
                .ik = entry.ik,
                .iw = entry.iw,
            };
        } else {
            data[addr] = .{
                .slot = .invalid_instruction,
                .ij = hw.IJ.init(0),
                .ik = hw.IK.init(0),
                .iw = hw.IW.init(0),
            };
        }
    }

    return data;
}

const log = std.log.scoped(.compile_arch);

const Decode_ROM_Builder = @This();
const Slot_Data = Microcode_Builder.Slot_Data;
const Microcode_Builder = @import("Microcode_Builder.zig");
const Instruction_Encoding = arch.isa.Instruction_Encoding;
const hw = arch.hw;
const arch = @import("lib_arch");
const std = @import("std");
