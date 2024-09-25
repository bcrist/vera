const cy7c1061g_address_bits = 20;   //  8MB (4x CY7C1061G; directly accessible)
const is66wve4m16_address_bits = 22; // 32MB (4x IS66WVE4M16E; accessible via aligned 8 byte block transfers to/from SRAM)
const sst39vf802c_address_bits = 19; //  4MB (4x SST39VF802C; accessible via aligned 8 byte block transfers to/from SRAM)

const SRAM_Address = std.meta.Int(.unsigned, cy7c1061g_address_bits);
const PSRAM_Address = std.meta.Int(.unsigned, is66wve4m16_address_bits);
const FLASH_Address = std.meta.Int(.unsigned, sst39vf802c_address_bits);

const Block_Address = std.meta.Int(.unsigned, @max(is66wve4m16_address_bits, sst39vf802c_address_bits));

const cy7c1061g_size = std.math.maxInt(SRAM_Address) + 1;
const is66wve4m16_size = std.math.maxInt(PSRAM_Address) + 1;
const sst39vf802c_size = std.math.maxInt(FLASH_Address) + 1;

const ram_end_frame = arch.addr.Frame.init(cy7c1061g_size * 8 / arch.addr.Frame.num_bytes_per_frame);

sram_da: [2][cy7c1061g_size]arch.DA,
sram_db: [2][cy7c1061g_size]arch.DB,

psram_da: [2][is66wve4m16_size]arch.DA,
psram_db: [2][is66wve4m16_size]arch.DB,

flash_da: [2][sst39vf802c_size]arch.DA,
flash_db: [2][sst39vf802c_size]arch.DB,

flash_fsm: [4]FLASH_FSM,
block_advance: bool,
block_flash: bool,
block_psram: bool,
block_ptr: Block_Address,

pub fn init(allocator: std.mem.Allocator) !*Memory {
    const self = try allocator.create(Memory);
    self.reset();
    return self;
}

pub fn deinit(self: *Memory, allocator: std.mem.Allocator) void {
    allocator.destroy(self);
}

pub fn reset(self: *Memory) void {
    self.flash_fsm = .{ .normal } ** 4;
    self.block_advance = false;
    self.block_flash = false;
    self.block_psram = false;
    self.block_ptr = 0;
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

    if (ctrl.block_transfer) {
        if (self.block_flash) {
            const flash_addr: FLASH_Address = @truncate(self.block_ptr);
            self.flash_fsm[0].update(arch.DA, &self.flash_da[0], self.sram_da[0][a], flash_addr);
            self.flash_fsm[1].update(arch.DB, &self.flash_db[0], self.sram_db[0][b], flash_addr);
            self.flash_fsm[2].update(arch.DA, &self.flash_da[1], self.sram_da[1][a], flash_addr);
            self.flash_fsm[3].update(arch.DB, &self.flash_db[1], self.sram_db[1][b], flash_addr);
        } else if (self.block_psram) {
            const psram_addr: PSRAM_Address = @truncate(self.block_ptr);
            self.psram_da[0][psram_addr] = self.sram_da[0][a];
            self.psram_db[0][psram_addr] = self.sram_db[0][b];
            self.psram_da[1][psram_addr] = self.sram_da[1][a];
            self.psram_db[1][psram_addr] = self.sram_db[1][b];
        }

        if (self.block_advance) {
            self.block_ptr += 1;
        }
    }

    return .{ .da = da, .db = db };
}

pub fn write(self: *Memory, ctrl: Bus_Control, data: Bus_Data) void {
    if (ctrl.guard_mismatch) return;
    
    if (ctrl.frame == .block_transfer_control_frame) {
        // address offset | mode
        // ===============|=========================
        //              2 | FLASH (no auto advance)
        //              3 | FLASH (auto advance)
        //              4 | PSRAM (no auto advance)
        //              5 | PSRAM (auto advance)

        self.block_ptr = @truncate(bits.concat(.{ data.da.raw(), data.db.raw() }));
        self.block_advance = 0 != (ctrl.ab.raw() & 1);
        const mode: u2 = @truncate(ctrl.ab.raw() >> 1);
        switch (mode) {
            1 => {
                self.block_flash = true;
                self.block_psram = false;
            },
            2 => {
                self.block_psram = true;
                self.block_flash = false;
            },
            else => {
                self.block_flash = false;
                self.block_psram = false;
            },
        }
        // std.debug.print("Block Transfer Address: {X:0>6}  Advance: {}  Flash: {}  PSRAM: {}\n", .{ self.block_ptr, self.block_advance, self.block_flash, self.block_psram });
    }

    if (ctrl.frame >= ram_end_frame) return;

    const a: SRAM_Address = @intCast(ctrl.aa.raw() >> 1);
    const b: SRAM_Address = @intCast(ctrl.ab.raw() >> 1);

    if (ctrl.block_transfer) {
        if (self.block_flash) {
            const flash_addr: FLASH_Address = @truncate(self.block_ptr);
            self.sram_da[0][a] = self.flash_da[0][flash_addr];
            self.sram_db[0][b] = self.flash_db[0][flash_addr];
            self.sram_da[1][a] = self.flash_da[1][flash_addr];
            self.sram_db[1][b] = self.flash_db[1][flash_addr];
        } else if (self.block_psram) {
            const psram_addr: PSRAM_Address = @truncate(self.block_ptr);
            self.sram_da[0][a] = self.psram_da[0][psram_addr];
            self.sram_db[0][b] = self.psram_db[0][psram_addr];
            self.sram_da[1][a] = self.psram_da[1][psram_addr];
            self.sram_db[1][b] = self.psram_db[1][psram_addr];
        }

        if (self.block_advance) {
            self.block_ptr += 1;
        }
    } else {
        const lsba: u1 = @truncate(ctrl.aa.raw());
        const lsbb: u1 = @truncate(ctrl.ab.raw());

        if (ctrl.lba) self.sram_da[lsba][a].lo = data.da.lo;
        if (ctrl.uba) self.sram_da[lsba][a].hi = data.da.hi;
        if (ctrl.lbb) self.sram_db[lsbb][b].lo = data.db.lo;
        if (ctrl.ubb) self.sram_db[lsbb][b].hi = data.db.hi;
    }
}

// These visit the memory in logical order instead of how it's arranged in individual chips; meant for debugging
pub fn sram_iterator(self: *Memory, address: usize) Iterator {
    return Iterator.init(&self.sram_da, &self.sram_db, address);
}
pub fn psram_iterator(self: *Memory, address: usize) Iterator {
    return Iterator.init(&self.psram_da, &self.psram_db, address);
}
pub fn flash_iterator(self: *Memory, address: usize) Iterator {
    return Iterator.init(&self.flash_da, &self.flash_db, address);
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

const FLASH_FSM = enum {
    normal,
    preamble1,
    preamble2,
    word_program,
    erase0,
    erase1,
    erase2,

    fn update(self: *FLASH_FSM, comptime T: type, flash: []T, data: T, address: FLASH_Address) void {
        const command = bits.concat(.{ data.lo, @as(u11, @truncate(address)) });
        self.* = switch (self.*) {
            .normal => switch (command) {
                0x555AA => .preamble1,
                else => .normal,
            },
            .preamble1 => switch (command) {
                0x2AA55 => .preamble2,
                else => .normal,
            },
            .preamble2 => switch (command) {
                0x555A0 => .word_program,
                0x55580 => .erase0,
                else => .normal,
            },
            .word_program => next: {
                flash[address] = flash[address].raw() & data.raw();
                break :next .normal;
            },
            .erase0 => switch (command) {
                0x555AA => .erase1,
                else => .normal,
            },
            .erase1 => switch (command) {
                0x2AA55 => .erase2,
                else => .normal,
            },
            .erase2 => switch (data.lo) {
                0x50 => next: {
                    // Sector erase
                    const sector_size: FLASH_Address = 0x800;
                    const sector_mask = ~(sector_size - 1);
                    const sector = flash[(address & sector_mask)..][0..sector_size];
                    @memset(sector, 0xFFFF);
                    break :next .normal;
                },
                0x30 => next: {
                    // Block erase

                    // The datasheet says only bits 18 through 15 indicate the block number,
                    // but that means a maximum of 16 blocks, and the datasheet clearly indicates there are 19 of them.
                    // I suspect this is an error in the datasheet and it's actually bits 18-14 that are used.
                    // The mistake is probably a holdover from the SST39VF800,
                    // which has uniform block sizes and thus only has 16 blocks.
                    const block_number: u5 = @intCast(address >> 14);
                    switch (block_number) {
                        0...14 => {
                            const block_size: FLASH_Address = 0x8000;
                            const block = flash[(block_number * block_size)..][0..block_size];
                            @memset(block, 0xFFFF);
                        },
                        15 => @memset(flash[0x78000..0x7C000], 0xFFFF),
                        16 => @memset(flash[0x7C000..0x7D000], 0xFFFF),
                        17 => @memset(flash[0x7D000..0x7E000], 0xFFFF),
                        18 => @memset(flash[0x7E000..0x80000], 0xFFFF),
                        else => {},
                    }
                    break :next .normal;
                },
                0x10 => next: {
                    if (command == 0x55510) {
                        // Chip erase
                        @memset(flash, 0xFFFF);
                    }
                    break :next .normal;
                },
                else => .normal,
            },
        };
    }
};


const Memory = @This();
const Device = @import("../Device.zig");
const Bus_Control = @import("../Bus_Control.zig");
const Bus_Data = @import("../Bus_Data.zig");
const arch = @import("arch");
const bits = @import("bits");
const std = @import("std");
