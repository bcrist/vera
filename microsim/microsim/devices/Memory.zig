const cy7c1061g_address_bits = 20;   //  8MB (4x CY7C1061G; directly accessible)

const SRAM_Address = std.meta.Int(.unsigned, cy7c1061g_address_bits);

const cy7c1061g_size = std.math.maxInt(SRAM_Address) + 1;

const ram_end_frame = arch.addr.Frame.init(cy7c1061g_size * 8 / arch.addr.Frame.num_bytes_per_frame);

sram_da: [2][cy7c1061g_size]arch.DA,
sram_db: [2][cy7c1061g_size]arch.DB,

pub fn init(allocator: std.mem.Allocator) !*Memory {
    const self = try allocator.create(Memory);
    self.reset();
    return self;
}

pub fn deinit(self: *Memory, allocator: std.mem.Allocator) void {
    allocator.destroy(self);
}

pub fn reset(self: *Memory) void {
    _ = self;
}

pub fn device(self: *Memory) Device {
    return Device.init(Memory, self, null);
}

pub fn read(self: *Memory, ctrl: Bus_Control) ?Bus_Data {
    if (!ctrl.frame.raw() >= ram_end_frame) return null;

    var da: arch.DA = .{ .lo = 0, .hi = 0 };
    var db: arch.DB = .{ .lo = 0, .hi = 0 };

    const lsba: u1 = @truncate(ctrl.aa.raw());
    const lsbb: u1 = @truncate(ctrl.ab.raw());

    const a: SRAM_Address = @intCast(ctrl.aa.raw() >> 1);
    const b: SRAM_Address = @intCast(ctrl.ab.raw() >> 1);

    if (ctrl.lba) da.lo = self.sram_da[lsba][a].lo;
    if (ctrl.uba) da.hi = self.sram_da[lsba][a].hi;
    if (ctrl.lbb) db.lo = self.sram_db[lsbb][b].lo;
    if (ctrl.ubb) db.hi = self.sram_db[lsbb][b].hi;

    return .{ .da = da, .db = db };
}

pub fn write(self: *Memory, ctrl: Bus_Control, data: Bus_Data) void {
    if (ctrl.guard_mismatch) return;
    if (ctrl.frame >= ram_end_frame) return;

    const a: SRAM_Address = @intCast(ctrl.aa.raw() >> 1);
    const b: SRAM_Address = @intCast(ctrl.ab.raw() >> 1);

    const lsba: u1 = @truncate(ctrl.aa.raw());
    const lsbb: u1 = @truncate(ctrl.ab.raw());

    if (ctrl.lba) self.sram_da[lsba][a].lo = data.da.lo;
    if (ctrl.uba) self.sram_da[lsba][a].hi = data.da.hi;
    if (ctrl.lbb) self.sram_db[lsbb][b].lo = data.db.lo;
    if (ctrl.ubb) self.sram_db[lsbb][b].hi = data.db.hi;
}

// These visit the memory in logical order instead of how it's arranged in individual chips; meant for debugging
pub fn sram_iterator(self: *Memory, address: usize) Iterator {
    return Iterator.init(&self.sram_da, &self.sram_db, address);
}
pub const Iterator = struct {
    da: [2][]arch.DA,
    db: [2][]arch.DB,
    chip: u3 = 0,
    offset: usize = 0,
    end_offset: usize,

    pub fn init(da: anytype, db: anytype, address: usize) Iterator {
        const end_offset = da[0].len;
        std.debug.assert(end_offset == da[1].len);
        std.debug.assert(end_offset == db[0].len);
        std.debug.assert(end_offset == db[1].len);
        var self = Iterator{
            .da = .{ &da[0], &da[1] },
            .db = .{ &db[0], &db[1] },
            .end_offset = end_offset,
        };
        _ = self.skip_bytes(address);
        return self;
    }

    pub fn read_byte(self: *Iterator) ?u8 {
        const offset = self.offset;
        if (offset >= self.end_offset) return null;

        const chip = self.chip;
        defer {
            if (chip == 7) {
                self.chip = 0;
                self.offset = offset + 1;
            } else {
                self.chip = chip + 1;
            }
        }

        return switch (chip) {
            0 => self.da[0][offset].lo,
            1 => self.da[0][offset].hi,
            2 => self.db[0][offset].lo,
            3 => self.db[0][offset].hi,
            4 => self.da[1][offset].lo,
            5 => self.da[1][offset].hi,
            6 => self.db[1][offset].lo,
            7 => self.db[1][offset].hi,
        };
    }

    pub fn read_all(self: *Iterator, out: []u8) bool {
        for (out) |*p| {
            if (self.read_byte()) |b| p.* = b else return false;
        }
        return true;
    }

    pub fn skip_bytes(self: *Iterator, bytes: usize) bool {
        self.offset += bytes / 8;
        const chip = @as(usize, self.chip) + (bytes & 7);
        if (chip >= 8) self.offset += 1;
        self.chip = @truncate(chip);
        return self.offset < self.end_offset;
    }

    pub fn write_byte(self: *Iterator, byte: u8) bool {
        const offset = self.offset;
        if (offset >= self.end_offset) return false;

        const chip = self.chip;
        defer {
            if (chip == 7) {
                self.chip = 0;
                self.offset = offset + 1;
            } else {
                self.chip = chip + 1;
            }
        }

        switch (self.chip) {
            0 => self.da[0][offset].lo = byte,
            1 => self.da[0][offset].hi = byte,
            2 => self.db[0][offset].lo = byte,
            3 => self.db[0][offset].hi = byte,
            4 => self.da[1][offset].lo = byte,
            5 => self.da[1][offset].hi = byte,
            6 => self.db[1][offset].lo = byte,
            7 => self.db[1][offset].hi = byte,
        }

        return true;
    }

    pub fn write_all(self: *Iterator, bytes: []const u8) bool {
        for (bytes) |b| {
            if (!self.write_byte(b)) return false;
        }
        return true;
    }
};

const Memory = @This();
const Device = @import("../Device.zig");
const Bus_Control = @import("../Bus_Control.zig");
const Bus_Data = @import("../Bus_Data.zig");
const arch = @import("arch");
const bits = @import("bits");
const std = @import("std");
