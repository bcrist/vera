const bus = @import("bus");
const PhysicalAddress = bus.PhysicalAddress;
const Frame = bus.Frame;

pub const ram_start:  PhysicalAddress = 0x000_000;
pub const ram_end:    PhysicalAddress = 0x7FF_FFF;
pub const device_sys: PhysicalAddress = 0x800_000;
pub const device_1:   PhysicalAddress = 0x900_000;
pub const device_2:   PhysicalAddress = 0xA00_000;
pub const device_3:   PhysicalAddress = 0xB00_000;
pub const device_4:   PhysicalAddress = 0xC00_000;
pub const device_5:   PhysicalAddress = 0xD00_000;
pub const device_6:   PhysicalAddress = 0xE00_000;
pub const device_7:   PhysicalAddress = 0xF00_000;
pub const num_addresses_per_device = 0x100_000;

pub const DeviceFrame = enum(Frame) {
    sys_interrupt_controller = toFrame(device_sys),
    sys_block_transfer_config = toFrame(device_sys) + 1,
    sys_accessed_frames = toFrame(device_sys) + 2,
    sys_dirty_frames = toFrame(device_sys) + 3,
    _,
};

pub fn toFrame(physical_address: PhysicalAddress) Frame {
    return @intCast(Frame, physical_address >> @bitSizeOf(bus.PageOffset));
}
pub fn fromFrame(frame: Frame) PhysicalAddress {
    return @as(PhysicalAddress, frame) << @bitSizeOf(bus.PageOffset);
}
