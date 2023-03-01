const std = @import("std");
const sim = @import("Simulator");
const bits = @import("bits");
const ControlSignals = @import("ControlSignals");
const misc = @import("misc");
const bus = @import("bus");

const SystemBusControl = @import("SystemBusControl.zig");

const OffsetSlotAndTag = packed struct {
    offset: bus.PageOffset,
    slot: u6,
    tag: misc.address_translator.Tag,
};

pub const OperationInfo = struct {
    cs_bus_rw: ControlSignals.BusDirection,
    cs_bus_byte: ControlSignals.BusWidth,
    cs_bus_mode: ControlSignals.BusMode,
    cs_at_op: ControlSignals.AddressTranslatorOp,
    slot: u6,
    tag: misc.address_translator.Tag,

    pub fn init() OperationInfo {
        return .{
            .cs_bus_rw = .read,
            .cs_bus_byte = .word,
            .cs_bus_mode = .raw,
            .cs_at_op = .none,
            .slot = 0,
            .tag = 0,
        };
    }

    pub fn random(rnd: std.rand.Random) OperationInfo {
        return .{
            .cs_bus_rw = rnd.enumValue(ControlSignals.BusDirection),
            .cs_bus_byte = rnd.enumValue(ControlSignals.BusWidth),
            .cs_bus_mode = rnd.enumValue(ControlSignals.BusMode),
            .cs_at_op = rnd.enumValue(ControlSignals.AddressTranslatorOp),
            .slot = rnd.int(u6),
            .tag = rnd.int(misc.address_translator.Tag),
        };
    }

    pub fn toU32(self: OperationInfo) u32 {
        return bits.concat(.{
            @enumToInt(self.cs_bus_rw),
            @enumToInt(self.cs_bus_byte),
            @enumToInt(self.cs_bus_mode),
            @enumToInt(self.cs_at_op),
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
    enable_flag: bool,
    kernel_flag: bool,

    cs_bus_mode: ControlSignals.BusMode,
    cs_bus_rw: ControlSignals.BusDirection,
    cs_bus_byte: ControlSignals.BusWidth,
    cs_at_op: ControlSignals.AddressTranslatorOp,
    cs_sr2_wi: ControlSignals.SR2Index,
    cs_sr2_wsrc: ControlSignals.SR2WriteDataSource,
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
    const group: u2 = switch (in.cs_bus_mode) {
        .raw, .data => switch (in.cs_bus_rw) {
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

    var matching = primary;
    var other = secondary;
    if (in.cs_at_op != .none and !primary_match and (secondary_match or in.cs_at_op == .update)) {
        matching = secondary;
        other = primary;
    }

    const translate = in.cs_at_op == .translate;

    // TODO audit this to make sure it catches all instruction loads
    const insn_load = translate and in.cs_sr2_wsrc == .virtual_address and (in.cs_sr2_wi == .ip or in.cs_sr2_wi == .next_ip);

    const enabled = in.enable_flag and in.cs_bus_mode != .raw;

    const enabled_translate = enabled and translate;
    const disabled_translate = !enabled and translate;

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
        bus_ctrl.wait_states = matching.wait_states;

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
    } else if (disabled_translate) {
        bus_ctrl.wait_states = 0;
        if (in.cs_bus_mode == .insn) {
            new_kernel_flag = true;
        }
    }

    if ((virtual.offset & 1) == 1) {
        bus_ctrl.swap_bytes = true;
        if (in.cs_bus_byte == .word) {
            bus_ctrl.even_offset +%= 1;
            if (virtual.offset == 0xFFFF) {
                page_align_fault = true;
            }
        }
    }

    if (translate) {
        if (in.cs_bus_rw == .read) {
            bus_ctrl.read = true;
        } else if (in.cs_bus_rw == .write) {
            bus_ctrl.write = true;
            if (in.cs_bus_byte == .word) {
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
            .cs_bus_rw = in.cs_bus_rw,
            .cs_bus_byte = in.cs_bus_byte,
            .cs_bus_mode = in.cs_bus_mode,
            .cs_at_op = in.cs_at_op,
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
    cs_at_op: ControlSignals.AddressTranslatorOp,
};

pub fn transact(state: *State, in: TransactInputs) void {
    if (in.inhibit_writes) return;

    switch (in.cs_at_op) {
        .none => {},
        .translate => {
            state.primary[in.slot] = in.matching_entry;
            state.secondary[in.slot] = in.other_entry;
        },
        .update => {
            state.primary[in.slot] = @bitCast(Entry, in.l);
            state.secondary[in.slot] = in.other_entry;
        },
        .invalidate => {
            const tag_mask = @truncate(u14, in.l.low);

            var matching = in.matching_entry;
            var other = in.other_entry;

            if ((in.tag & tag_mask) == (matching.tag & tag_mask) and matching.present) {
                matching.present = false;
            }

            if ((in.tag & tag_mask) == (other.tag & tag_mask) and other.present) {
                other.present = false;
            }

            state.primary[in.slot] = matching;
            state.secondary[in.slot] = other;
        },
    }
}
