allocator: std.mem.Allocator,
entries: []Entry,

pub const Entry = struct {
    instruction_encoding: ?Instruction_Encoding = null,
    slot_handle: ?Slot_Data.Handle = null,
    wio: arch.Write_Index_Offset = arch.Write_Index_Offset.init(0),
    krio: arch.K.Read_Index_Offset = arch.K.Read_Index_Offset.init(0),
    cv: arch.insn_decode.CV_Mode = .zero,
};

pub fn init(allocator: std.mem.Allocator) Decode_ROM_Builder {
    const entries = allocator.alloc(Entry, arch.insn_decode.Address.count) catch @panic("OOM");
    @memset(entries, .{});
    return .{
        .allocator = allocator,
        .entries = entries,
    };
}
pub fn deinit(self: *Decode_ROM_Builder) void {
    self.allocator.free(self.entries);
}

pub fn add_entry(self: *Decode_ROM_Builder, addr: arch.insn_decode.Address, undefined_bits: arch.insn_decode.Address.Raw, entry: Entry) void {
    std.debug.assert(entry.slot_handle != null);
    var iter: Undefined_Bits_Iterator = .{ .mask = undefined_bits };
    while (iter.next()) |extra_bits| {
        const addr_raw = addr.raw() | extra_bits;
        const existing = self.entries[addr_raw];
        if (existing.slot_handle) |existing_handle| {
            if (existing_handle != entry.slot_handle
                or existing.wio != entry.wio
                or existing.krio != entry.krio
                or existing.cv != entry.cv
            ) {
                var buf: [32768]u8 = undefined;
                var stream = std.io.fixedBufferStream(&buf);
                var writer = stream.writer();

                if (existing_handle != entry.slot_handle) {
                    writer.print("Handle: {d: >30} != {d}\n", .{ existing_handle.raw(), entry.slot_handle.?.raw() }) catch @panic("IO Error");
                }
                if (existing.wio != entry.wio) {
                    writer.print("WIO:    {d: >30} != {d}\n", .{ existing.wio.raw(), entry.wio.raw() }) catch @panic("IO Error");
                }
                if (existing.krio != entry.krio) {
                    writer.print("KRIO:   {d: >30} != {d}\n", .{ existing.krio.raw(), entry.krio.raw() }) catch @panic("IO Error");
                }
                if (existing.cv != entry.cv) {
                    writer.print("CV:     {s: >30} != {d}\n", .{ @tagName(existing.cv), @tagName(entry.cv) }) catch @panic("IO Error");
                }

                // inline for (std.enums.values(hw.Control_Signal)) |field| {
                //     if (@field(existing.
                // }

                std.debug.panic("Decode address 0x{X} already used:\n{s}", .{ addr.raw(), stream.getWritten() });
            }
        } else self.entries[addr_raw] = entry;
    }
}

pub fn get_entry(self: *Decode_ROM_Builder, addr: arch.insn_decode.Address) Entry {
    return self.entries[addr.raw()];
}

pub fn generate_rom_data(self: *Decode_ROM_Builder, allocator: std.mem.Allocator, microcode: *const Microcode_Builder) *arch.insn_decode.Rom {
    const data = allocator.create(arch.insn_decode.Rom) catch @panic("OOM");

    for (self.entries, 0..) |entry, addr| {
        if (entry.slot_handle) |handle| {
            if (handle.raw() >= microcode.handle_to_slot.len) {
                @panic("microcode.assign_slots() must be called before decode_rom.build_rom_data()");
            }
            data[addr] = .{
                .entry = microcode.handle_to_slot[handle.raw()],
                .wio = entry.wio,
                .krio = entry.krio,
                .cv = entry.cv,
            };
        } else {
            data[addr] = .{
                .entry = .invalid_instruction,
                .wio = arch.Write_Index_Offset.init(0),
                .krio = arch.K.Read_Index_Offset.init(0),
                .cv = arch.insn_decode.CV_Mode.init(0),
            };
        }
    }

    return data;
}

const Undefined_Bits_Iterator = struct {
    mask: arch.insn_decode.Address.Raw,
    last: ?arch.insn_decode.Address.Raw = null,

    pub fn next(self: *Undefined_Bits_Iterator) ?arch.insn_decode.Address.Raw {
        if (self.last) |last| {                                                                                     // e.g. 01001100
            const mask = self.mask;                                                                                 // e.g. 11011100
            const remaining_bits = mask ^ last;                                                                     // e.g. 10010000
            if (remaining_bits == 0) return null;
            const bits_to_clear = remaining_bits ^ (remaining_bits - 1);                                            // e.g. 00011111
            const bit_to_set = remaining_bits & bits_to_clear;                                                      // e.g. 00010000
            const value = last & ~bits_to_clear | bit_to_set;                                                       // e.g. 01010000
            self.last = value;
            return value;
        } else {
            self.last = 0;
            return 0;
        }
    }
};

const log = std.log.scoped(.compile);

const Decode_ROM_Builder = @This();
const Slot_Data = Microcode_Builder.Slot_Data;
const Microcode_Builder = @import("Microcode_Builder.zig");
const Instruction_Encoding = isa.Instruction_Encoding;
const isa = @import("isa");
const arch = @import("arch");
const std = @import("std");
