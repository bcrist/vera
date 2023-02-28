const std = @import("std");
const sim = @import("Simulator");
const bits = @import("bits");
const ctrl = @import("control_signals");
const misc = @import("misc");
const bus = @import("bus");

const SystemBusControl = @import("SystemBusControl.zig");

const OffsetSlotAndTag = packed struct {
    offset: bus.PageOffset,
    slot: u6,
    tag: misc.address_translator.Tag,
};

pub const OperationInfo = struct {
    BUS_RW: ctrl.Bus_Direction,
    BUS_BYTE: ctrl.Bus_Width,
    BUS_MODE: ctrl.Bus_Mode,
    AT_OP: ctrl.AT_Op,
    slot: u6,
    tag: misc.address_translator.Tag,

    pub fn init() OperationInfo {
        return .{
            .BUS_RW = .read,
            .BUS_BYTE = .word,
            .BUS_MODE = .raw,
            .AT_OP = .hold,
            .slot = 0,
            .tag = 0,
        };
    }

    pub fn random(rnd: std.rand.Random) OperationInfo {
        return .{
            .BUS_RW = rnd.enumValue(ctrl.Bus_Direction),
            .BUS_BYTE = rnd.enumValue(ctrl.Bus_Width),
            .BUS_MODE = rnd.enumValue(ctrl.Bus_Mode),
            .AT_OP = rnd.enumValue(ctrl.AT_Op),
            .slot = rnd.int(u6),
            .tag = rnd.int(misc.address_translator.Tag),
        };
    }

    pub fn toU32(self: OperationInfo) u32 {
        return bits.concat(.{
            @enumToInt(self.BUS_RW),
            @enumToInt(self.BUS_BYTE),
            @enumToInt(self.BUS_MODE),
            @enumToInt(self.AT_OP),
            @as(u6, 0),
            self.slot,
            self.tag,
        });
    }
};

pub const AccessPolicy = enum(u2) {
    unprivileged = 0,
    kernel_entry_256 = 1,
    kernel_entry_4096 = 2,
    kernel_private = 3,
};

pub const Entry = packed struct {
    frame: bus.Frame = 0,
    wait_states: u2 = 0,
    update_frame_state: bool = false,
    present: bool = false,
    access: AccessPolicy = .unprivileged,
    tag: misc.address_translator.Tag = 0,

    pub fn random(rnd: std.rand.Random) Entry {
        return .{
            .frame = rnd.int(bus.Frame),
            .wait_states = rnd.int(u2),
            .update_frame_state = rnd.boolean(),
            .present = rnd.boolean(),
            .access = rnd.enumValue(AccessPolicy),
            .tag = rnd.int(misc.address_translator.Tag),
        };
    }
};

pub const State = struct {
    primary: [0x1000]Entry = [_]Entry{.{}} ** 0x1000,
    secondary: [0x1000]Entry = [_]Entry{.{}} ** 0x1000,

    pub fn reset(self: *State) void {
        self.primary = .{.{}} ** 0x1000;
        self.secondary = .{.{}} ** 0x1000;
    }

    pub fn randomize(self: *State, rnd: std.rand.Random) void {
        rnd.bytes(std.mem.sliceAsBytes(self.primary[0..]));
        rnd.bytes(std.mem.sliceAsBytes(self.secondary[0..]));
    }
};

pub const ComputeInputs = struct {
    virtual_address: bus.VirtualAddressParts,
    asn: misc.address_translator.AddressSpaceNumber,
    matching_entry: Entry,
    other_entry: Entry,
    enable_flag: bool,
    kernel_flag: bool,

    BUS_MODE: ctrl.Bus_Mode,
    BUS_RW: ctrl.Bus_Direction,
    BUS_BYTE: ctrl.Bus_Width,
    LL_SRC: ctrl.LL_Source,
    AT_OP: ctrl.AT_Op,
    SR2_WI: ctrl.SR2Index,
    SR2_WSRC: ctrl.SR2_Write_Data_Source,
};

pub const ComputeOutputs = struct {
    op: OperationInfo,
    bus_ctrl: SystemBusControl,
    slot: misc.address_translator.Slot,
    matching_entry: Entry,
    other_entry: Entry,
    page_fault: bool,
    access_fault: bool,
    page_align_fault: bool,
    new_kernel_flag: bool,
};

pub fn compute(state: *const State, in: ComputeInputs) ComputeOutputs {
    const group: u2 = switch (in.BUS_MODE) {
        .raw, .data => switch (in.BUS_RW) {
            .write => @as(u2, 0),
            .read => @as(u2, 1),
        },
        .stack => @as(u2, 2),
        .insn => @as(u2, 3),
    };

    const virtual = @bitCast(OffsetSlotAndTag, in.virtual_address);

    const slot: u12 = bits.concat(.{
        virtual.slot,
        in.asn,
        group,
    });

    const primary = state.primary[slot];
    const secondary = state.secondary[slot];

    const primary_match = primary.present and primary.tag == virtual.tag;
    const secondary_match = secondary.present and secondary.tag == virtual.tag;
    const any_match = primary_match or secondary_match;

    var matching = in.matching_entry;
    var other = in.other_entry;

    if (in.AT_OP != .hold) {
        if (!primary_match and (secondary_match or in.AT_OP == .update)) {
            matching = secondary;
            other = primary;
        } else {
            matching = primary;
            other = secondary;
        }
    }

    const real_bus_op = in.AT_OP == .translate and in.LL_SRC != .AT_ME_L and in.LL_SRC != .AT_OE_L;
    const insn_load = real_bus_op and in.SR2_WSRC == .PN and (in.SR2_WI == .ip or in.SR2_WI == .next_ip);

    const enabled = in.enable_flag and in.BUS_MODE != .raw;

    const enabled_translate = enabled and in.AT_OP == .translate;
    const disabled_translate = !enabled and in.AT_OP == .translate;

    const page_fault = enabled_translate and !any_match;
    var page_align_fault = false;
    var access_fault = false;
    var bus_ctrl = SystemBusControl{
        .address = .{
            .offset = virtual.offset,
            .frame = @truncate(u12, in.virtual_address.page),
        },
        .read = false,
        .write = false,
        .write_odd = false,
        .write_even = false,
        .wait_states = 0,
        .even_offset = @intCast(u11, virtual.offset >> 1),
        .odd_offset = @intCast(u11, virtual.offset >> 1),
        .swap_bytes = false,
    };
    var new_kernel_flag = in.kernel_flag;

    if (enabled_translate and any_match) {
        bus_ctrl.address.frame = matching.frame;
        if (real_bus_op) {
            bus_ctrl.wait_states = matching.wait_states;
        }

        switch (matching.access) {
            .unprivileged => {
                if (insn_load) {
                    new_kernel_flag = false;
                }
            },
            .kernel_entry_256 => {
                if (in.kernel_flag or @truncate(u8, in.virtual_address.offset) == 0) {
                    if (insn_load) {
                        new_kernel_flag = true;
                    }
                } else {
                    access_fault = true;
                }
            },
            .kernel_entry_4096 => {
                if (in.kernel_flag or in.virtual_address.offset == 0) {
                    if (insn_load) {
                        new_kernel_flag = true;
                    }
                } else {
                    access_fault = true;
                }
            },
            .kernel_private => {
                if (!in.kernel_flag) {
                    access_fault = true;
                }
            },
        }
    } else if (disabled_translate and real_bus_op) {
        bus_ctrl.wait_states = 0;
        if (in.BUS_MODE == .insn) {
            new_kernel_flag = true;
        }
    }

    if ((virtual.offset & 1) == 1) {
        bus_ctrl.swap_bytes = true;
        if (in.BUS_BYTE == .word) {
            bus_ctrl.even_offset +%= 1;
            if (virtual.offset == 0xFFFF) {
                page_align_fault = true;
            }
        }
    }

    if (in.AT_OP == .translate and real_bus_op) {
        if (in.BUS_RW == .read) {
            bus_ctrl.read = true;
        } else if (in.BUS_RW == .write) {
            bus_ctrl.write = true;
            if (in.BUS_BYTE == .word) {
                bus_ctrl.write_even = true;
                bus_ctrl.write_odd = true;
            } else if ((virtual.offset & 1) == 1) {
                bus_ctrl.write_odd = true;
            } else {
                bus_ctrl.write_even = true;
            }
        }
    }

    return .{
        .op = .{
            .BUS_RW = in.BUS_RW,
            .BUS_BYTE = in.BUS_BYTE,
            .BUS_MODE = in.BUS_MODE,
            .AT_OP = in.AT_OP,
            .slot = virtual.slot,
            .tag = virtual.tag,
        },
        .bus_ctrl = bus_ctrl,
        .slot = slot,
        .matching_entry = matching,
        .other_entry = other,
        .page_fault = page_fault,
        .access_fault = access_fault,
        .page_align_fault = page_align_fault,
        .new_kernel_flag = new_kernel_flag,
    };
}

pub const TransactInputs = struct {
    inhibit_writes: bool,
    matching_entry: Entry,
    other_entry: Entry,
    slot: misc.address_translator.Slot,
    l: bus.LParts,
    tag: misc.address_translator.Tag,
    AT_OP: ctrl.AT_Op,
};

pub const TransactOutputs = struct {
    matching_entry: Entry,
    other_entry: Entry,
    z: bool,
    n: bool,
};

pub fn transact(state: *State, in: TransactInputs) TransactOutputs {
    var matching = in.matching_entry;
    var other = in.other_entry;

    if (in.inhibit_writes) {
        return .{
            .matching_entry = matching,
            .other_entry = other,
            .z = matching.present,
            .n = other.present,
        };
    }

    var z = false;
    var n = false;

    switch (in.AT_OP) {
        .hold => {
            z = matching.present;
            n = other.present;
        },
        .translate => {
            z = matching.present;
            n = other.present;

            state.primary[in.slot] = matching;
            state.secondary[in.slot] = other;
        },
        .update => {
            z = matching.present;
            n = in.tag != matching.tag;

            matching = @bitCast(Entry, in.l);

            state.primary[in.slot] = matching;
            state.secondary[in.slot] = other;
        },
        .invalidate => {
            const tag_mask = @truncate(u14, in.l.low);

            if ((in.tag & tag_mask) == (matching.tag & tag_mask) and matching.present) {
                z = matching.present;
                matching.present = false;
            }

            if ((in.tag & tag_mask) == (other.tag & tag_mask) and other.present) {
                n = other.present;
                other.present = false;
            }

            state.primary[in.slot] = matching;
            state.secondary[in.slot] = other;
        },
    }

    return .{
        .matching_entry = matching,
        .other_entry = other,
        .z = z,
        .n = n,
    };
}
