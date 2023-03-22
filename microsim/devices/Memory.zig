const std = @import("std");
const bits = @import("bits");
const bus = @import("bus_types");
const physical_address = @import("physical_address");
const SystemBusControl = @import("Simulator").SystemBusControl;

const cy7c1061g_address_bits = 20;   //  8MB (4x CY7C1061G; directly accessible)
const is66wve4m16_address_bits = 22; // 32MB (4x IS66WVE4M16E; accessible via aligned 8 byte block transfers to/from SRAM)
const sst39vf802c_address_bits = 19; //  4MB (4x SST39VF802C; accessible via aligned 8 byte block transfers to/from SRAM)

const SramAddress = std.meta.Int(.unsigned, cy7c1061g_address_bits);
const PsramAddress = std.meta.Int(.unsigned, is66wve4m16_address_bits);
const FlashAddress = std.meta.Int(.unsigned, sst39vf802c_address_bits);

const BlockAddress = std.meta.Int(.unsigned, @max(is66wve4m16_address_bits, sst39vf802c_address_bits));

const cy7c1061g_size = std.math.maxInt(SramAddress) + 1;
const is66wve4m16_size = std.math.maxInt(PsramAddress) + 1;
const sst39vf802c_size = std.math.maxInt(FlashAddress) + 1;

const last_frame = physical_address.toFrame(physical_address.ram_end);

comptime {
    std.debug.assert(physical_address.ram_start == 0);
    std.debug.assert(physical_address.ram_end == cy7c1061g_size * 8 - 1);
}

fn high8(word: u16) u8 {
    return @intCast(u8, word >> 8);
}
fn low8(word: u16) u8 {
    return @truncate(u8, word);
}

const Memory = @This();

sram: [4][cy7c1061g_size]u16,
psram: [4][is66wve4m16_size]u16,
flash: [4][sst39vf802c_size]u16,
flash_fsm: [4]FlashStateMachine,
block_advance: bool,
block_flash: bool,
block_psram: bool,
block_ptr: BlockAddress,

pub fn reset(self: *Memory) void {
    self.flash_fsm = .{.{}} ** 4;
    self.block_advance = false;
    self.block_flash = false;
    self.block_psram = false;
    self.block_ptr = 0;
}

pub fn randomize(self: *Memory, rnd: std.rand.Random) void {
    // Filling all SRAM/PSRAM/FLASH with random data would take longer than we'd like.
    // We'll just randomize the block transfer registers:
    self.block_advance = rnd.boolean();
    self.block_flash = rnd.boolean();
    self.block_psram = rnd.boolean();
    if (self.block_flash and self.block_psram) {
        self.block_flash = false;
        self.block_psram = false;
    }
    self.block_ptr = rnd.int(BlockAddress);
}

pub fn read(self: *const Memory, bus_ctrl: SystemBusControl) ?bus.D {
    if (!bus_ctrl.read or bus_ctrl.address.frame > last_frame) return null;

    const even_group = @truncate(u1, bus_ctrl.even_offset) * @as(u2, 2);
    const odd_group = @truncate(u1, bus_ctrl.odd_offset) * @as(u2, 2) + 1;

    const even_high_byte = 0 != (bus_ctrl.even_offset & 2);
    const odd_high_byte = 0 != (bus_ctrl.odd_offset & 2);

    const even_addr = @intCast(SramAddress, bits.concat2(@intCast(u9, bus_ctrl.even_offset >> 2), bus_ctrl.address.frame));
    const odd_addr = @intCast(SramAddress, bits.concat2(@intCast(u9, bus_ctrl.odd_offset >> 2), bus_ctrl.address.frame));

    const sram = &self.sram;
    const even16 = sram[even_group][even_addr];
    const odd16 = sram[odd_group][odd_addr];

    const even = if (even_high_byte) high8(even16) else low8(even16);
    const odd = if (odd_high_byte) high8(odd16) else low8(odd16);

    if (bus_ctrl.swap_bytes) {
        return bits.concat2(odd, even);
    } else {
        return bits.concat2(even, odd);
    }
}

pub fn transact(self: *Memory, bus_ctrl: SystemBusControl, data: bus.D, block_transfer: bool) void {
    switch (@intToEnum(physical_address.DeviceFrame, bus_ctrl.address.frame)) {
        .sys_block_transfer_config => if (bus_ctrl.write) {
            self.block_ptr = @truncate(BlockAddress, bits.concat2(data, bus_ctrl.address.offset));
            self.block_advance = 0 != (bus_ctrl.address.offset & 0x800);
            switch (@truncate(u2, bus_ctrl.address.offset >> 9)) {
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
        },
        else => {},
    }
    if (bus_ctrl.address.frame > last_frame) return;

    if (bus_ctrl.write) {
        if (block_transfer) {
            const sram = &self.sram;
            const sram_addr = @intCast(SramAddress, bits.concat2(@intCast(u9, bus_ctrl.address.offset >> 3), bus_ctrl.address.frame));
            if (self.block_flash) {
                const flash = &self.flash;
                const flash_addr = @truncate(FlashAddress, self.block_ptr);
                sram[0][sram_addr] = flash[0][flash_addr];
                sram[1][sram_addr] = flash[1][flash_addr];
                sram[2][sram_addr] = flash[2][flash_addr];
                sram[3][sram_addr] = flash[3][flash_addr];
            } else if (self.block_psram) {
                const psram = &self.psram;
                const psram_addr = @truncate(PsramAddress, self.block_ptr);
                sram[0][sram_addr] = psram[0][psram_addr];
                sram[1][sram_addr] = psram[1][psram_addr];
                sram[2][sram_addr] = psram[2][psram_addr];
                sram[3][sram_addr] = psram[3][psram_addr];
            }

            if (self.block_advance) {
                self.block_ptr += 1;
            }
        } else {
            var even_data = low8(data);
            var odd_data = high8(data);
            if (bus_ctrl.swap_bytes) {
                even_data = odd_data;
                odd_data = low8(data);
            }

            const sram = &self.sram;
            if (bus_ctrl.write_even) {
                const group = @truncate(u1, bus_ctrl.even_offset) * @as(u2, 2);
                const high_byte = 0 != (bus_ctrl.even_offset & 2);
                const addr = @intCast(SramAddress, bits.concat2(@intCast(u9, bus_ctrl.even_offset >> 2), bus_ctrl.address.frame));
                sram[group][addr] = if (high_byte)
                    bits.concat2(low8(sram[group][addr]), even_data)
                else
                    bits.concat2(even_data, high8(sram[group][addr]));
            }
            if (bus_ctrl.write_odd) {
                const group = @truncate(u1, bus_ctrl.odd_offset) * @as(u2, 2) + 1;
                const high_byte = 0 != (bus_ctrl.odd_offset & 2);
                const addr = @intCast(SramAddress, bits.concat2(@intCast(u9, bus_ctrl.odd_offset >> 2), bus_ctrl.address.frame));
                sram[group][addr] = if (high_byte)
                    bits.concat2(low8(sram[group][addr]), odd_data)
                else
                    bits.concat2(odd_data, high8(sram[group][addr]));
            }
        }
    } else if (bus_ctrl.read and block_transfer) {
        const sram = &self.sram;
        const sram_addr = @intCast(SramAddress, bits.concat2(@intCast(u9, bus_ctrl.address.offset >> 3), bus_ctrl.address.frame));
        if (self.block_flash) {
            const flash = &self.flash;
            const flash_addr = @truncate(FlashAddress, self.block_ptr);
            self.flash_fsm[0].write(&flash[0], flash_addr, sram[0][sram_addr]);
            self.flash_fsm[1].write(&flash[1], flash_addr, sram[1][sram_addr]);
            self.flash_fsm[2].write(&flash[2], flash_addr, sram[2][sram_addr]);
            self.flash_fsm[3].write(&flash[3], flash_addr, sram[3][sram_addr]);
        } else if (self.block_psram) {
            const psram = &self.psram;
            const psram_addr = @truncate(PsramAddress, self.block_ptr);
            psram[0][psram_addr] = sram[0][sram_addr];
            psram[1][psram_addr] = sram[1][sram_addr];
            psram[2][psram_addr] = sram[2][sram_addr];
            psram[3][psram_addr] = sram[3][sram_addr];
        }

        if (self.block_advance) {
            self.block_ptr += 1;
        }
    }
}

// These visit the memory in logical order instead of how it's arranged in individual chips; meant for debugging
pub fn sramIterator(self: *Memory, address: usize) Iterator {
    return Iterator.init(&self.sram, address);
}
pub fn psramIterator(self: *Memory, address: usize) Iterator {
    return Iterator.init(&self.psram, address);
}
pub fn flashIterator(self: *Memory, address: usize) Iterator {
    return Iterator.init(&self.flash, address);
}
pub const Iterator = struct {
    mem: [4][]u16,
    high_byte: bool = false,
    group: u2 = 0,
    offset: usize = 0,

    pub fn init(mem: anytype, address: usize) Iterator {
        var self = Iterator{
            .mem = .{ &mem[0], &mem[1], &mem[2], &mem[3] },
        };
        _ = self.skipBytes(address);
        return self;
    }

    pub fn readByte(self: *Iterator) ?u8 {
        const chip: []const u16 = self.mem[self.group];
        if (self.offset >= chip.len) {
            return null;
        }
        if (self.high_byte) {
            const b = high8(chip[self.offset]);
            self.group +%= 1;
            if (self.group == 0) {
                self.high_byte = false;
                self.offset += 1;
            }
            return b;
        } else {
            const b = low8(chip[self.offset]);
            self.group +%= 1;
            if (self.group == 0) {
                self.high_byte = true;
            }
            return b;
        }
    }
    pub fn readAll(self: *Iterator, out: []u8) bool {
        for (out) |*p| {
            if (self.readByte()) |b| p.* = b else return false;
        }
        return true;
    }
    pub fn skipBytes(self: *Iterator, bytes: usize) bool {
        self.offset += bytes / 8;
        var remaining = bytes & 7;
        while (remaining > 0) : (remaining -= 1) {
            self.group +%= 1;
            if (self.group == 0) {
                if (self.high_byte) {
                    self.high_byte = false;
                    self.offset += 1;
                } else {
                    self.high_byte = true;
                }
            }
        }
        const offset = self.offset;
        const end = self.mem[0].len;

        return offset < end or offset == end and self.group == 0 and !self.high_byte;
    }
    pub fn writeByte(self: *Iterator, byte: u8) bool {
        const chip: []u16 = self.mem[self.group];
        if (self.offset >= chip.len) {
            return false;
        }
        if (self.high_byte) {
            chip[self.offset] = bits.concat2(low8(chip[self.offset]), byte);
            self.group +%= 1;
            if (self.group == 0) {
                self.high_byte = false;
                self.offset += 1;
            }
        } else {
            chip[self.offset] = bits.concat2(byte, high8(chip[self.offset]));
            self.group +%= 1;
            if (self.group == 0) {
                self.high_byte = true;
            }
        }
        return true;
    }
    pub fn writeAll(self: *Iterator, bytes: []const u8) bool {
        for (bytes) |b| {
            if (!self.writeByte(b)) return false;
        }
        return true;
    }
};

const FlashStateMachine = struct {
    state: enum {
        normal,
        preamble1,
        preamble2,
        word_program,
        erase0,
        erase1,
        erase2,
    } = .normal,

    fn write(self: *FlashStateMachine, flash: []u16, address: FlashAddress, data: u16) void {
        const command = bits.concat2(@truncate(u8, data), @truncate(u11, address));
        switch (self.state) {
            .normal => {
                self.state = switch (command) {
                    0x555AA => .preamble1,
                    else => .normal,
                };
            },
            .preamble1 => {
                self.state = switch (command) {
                    0x2AA55 => .preamble2,
                    else => .normal,
                };
            },
            .preamble2 => {
                self.state = switch (command) {
                    0x555A0 => .word_program,
                    0x55580 => .erase0,
                    else => .normal,
                };
            },
            .word_program => {
                flash[address] &= data;
                self.state = .normal;
            },
            .erase0 => {
                self.state = switch (command) {
                    0x555AA => .erase1,
                    else => .normal,
                };
            },
            .erase1 => {
                self.state = switch (command) {
                    0x2AA55 => .erase2,
                    else => .normal,
                };
            },
            .erase2 => {
                switch (@truncate(u8, data)) {
                    0x50 => {
                        // Sector erase
                        const sector_size: FlashAddress = 0x800;
                        const sector_mask = ~(sector_size - 1);
                        const sector = flash[(address & sector_mask)..][0..sector_size];
                        std.mem.set(u16, sector, 0xFFFF);
                    },
                    0x30 => {
                        // Block erase

                        // The datasheet says only bits 18 through 15 indicate the block number,
                        // but that means a maximum of 16 blocks, and the datasheet clearly indicates there are 19 of them.
                        // I suspect this is an error in the datasheet and it's actually bits 18-14 that are used.
                        // The mistake is probably a holdover from the SST39VF800,
                        // which has uniform block sizes and thus only has 16 blocks.
                        const block_number = @intCast(u5, address >> 14);
                        switch (block_number) {
                            0...14 => {
                                const block_size: FlashAddress = 0x8000;
                                const block = flash[(block_number * block_size)..][0..block_size];
                                std.mem.set(u16, block, 0xFFFF);
                            },
                            15 => std.mem.set(u16, flash[0x78000..0x7C000], 0xFFFF),
                            16 => std.mem.set(u16, flash[0x7C000..0x7D000], 0xFFFF),
                            17 => std.mem.set(u16, flash[0x7D000..0x7E000], 0xFFFF),
                            18 => std.mem.set(u16, flash[0x7E000..0x80000], 0xFFFF),
                            else => {},
                        }
                    },
                    0x10 => {
                        if (command == 0x55510) {
                            // Chip erase
                            std.mem.set(u16, flash, 0xFFFF);
                        }
                    },
                    else => {},
                }
                self.state = .normal;
            },
        }
    }
};
