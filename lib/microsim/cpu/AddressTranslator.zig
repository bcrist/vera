const std = @import("std");
const bits = @import("bits");
const ControlSignals = @import("ControlSignals");
const misc = @import("misc");
const bus = @import("bus_types");
const at_types = @import("address_translator_types");
const SystemBusControl = @import("Simulator").SystemBusControl;

const Tag = at_types.Tag;
const Slot = at_types.Slot;
const AddressSpaceNumber = at_types.AddressSpaceNumber;
const EntryAddress = at_types.EntryAddress;
const Entry = at_types.Entry;
const TranslationInfo = at_types.TranslationInfo;

// Reinterpretation of bus.VirtualAddress/VirtualAddressParts
const OffsetSlotAndTag = packed struct {
    offset: bus.PageOffset,
    slot: Slot,
    tag: Tag,
};

const AddressTranslator = @This();

primary: [0x1000]Entry = [_]Entry{.{}} ** 0x1000,
secondary: [0x1000]Entry = [_]Entry{.{}} ** 0x1000,

pub fn reset(self: *AddressTranslator) void {
    self.primary = .{.{}} ** 0x1000;
    self.secondary = .{.{}} ** 0x1000;
}

pub fn randomize(self: *AddressTranslator, rnd: std.rand.Random) void {
    rnd.bytes(std.mem.sliceAsBytes(self.primary[0..]));
    rnd.bytes(std.mem.sliceAsBytes(self.secondary[0..]));
}

pub const ComputeInputs = struct {
    virtual_address: bus.VirtualAddressParts,
    asn: AddressSpaceNumber,
    enable_flag: bool,
    kernel_flag: bool,

    cs_bus_mode: ControlSignals.BusMode,
    cs_bus_rw: ControlSignals.BusDirection,
    cs_bus_byte: ControlSignals.BusWidth,
    cs_at_op: ControlSignals.AddressTranslatorOp,
    cs_sr2_wi: ControlSignals.SR2Index,
    cs_sr2_wsrc: ControlSignals.SR2WriteDataSource,
};

pub const ComputeOutputsExceptFaults = struct {
    info: TranslationInfo,
    entry_address: EntryAddress,
    matching_entry: Entry,
    other_entry: Entry,
    bus_ctrl: SystemBusControl,
    new_kernel_flag: bool,
};

pub const ComputeOutputs = struct {
    base: ComputeOutputsExceptFaults,
    page_fault: bool,
    access_fault: bool,
    page_align_fault: bool,
};

pub fn compute(self: *const AddressTranslator, in: ComputeInputs) ComputeOutputs {
    const group: u2 = switch (in.cs_bus_mode) {
        .raw, .data => switch (in.cs_bus_rw) {
            .write => @as(u2, 0),
            .read => @as(u2, 1),
        },
        .stack => @as(u2, 2),
        .insn => @as(u2, 3),
    };

    const virtual = @bitCast(OffsetSlotAndTag, in.virtual_address);

    const entry_address: EntryAddress = bits.concat(.{
        virtual.slot,
        in.asn,
        group,
    });

    const primary = self.primary[entry_address];
    const secondary = self.secondary[entry_address];

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

    var out = ComputeOutputs{
        .base = .{
            .info = .{
                .cs_bus_rw = in.cs_bus_rw,
                .cs_bus_byte = in.cs_bus_byte,
                .cs_bus_mode = in.cs_bus_mode,
                .cs_at_op = in.cs_at_op,
                .slot = virtual.slot,
                .tag = virtual.tag,
            },
            .entry_address = entry_address,
            .matching_entry = matching,
            .other_entry = other,
            .bus_ctrl = .{
                .address = .{
                    .offset = virtual.offset,
                    .frame = @truncate(u12, in.virtual_address.page),
                },
                .even_offset = @intCast(u11, virtual.offset >> 1),
                .odd_offset = @intCast(u11, virtual.offset >> 1),
            },
            .new_kernel_flag = in.kernel_flag,
        },
        .page_fault = enabled_translate and !any_match,
        .page_align_fault = false,
        .access_fault = false,
    };

    if (enabled_translate and any_match) {
        out.base.bus_ctrl.address.frame = matching.frame;
        out.base.bus_ctrl.wait_states = matching.wait_states;

        switch (matching.access) {
            .unprivileged => {
                if (insn_load) {
                    out.base.new_kernel_flag = false;
                }
            },
            .kernel_entry_256 => {
                if (in.kernel_flag or @truncate(u8, in.virtual_address.offset) == 0) {
                    if (insn_load) {
                        out.base.new_kernel_flag = true;
                    }
                } else {
                    out.access_fault = true;
                }
            },
            .kernel_entry_4096 => {
                if (in.kernel_flag or in.virtual_address.offset == 0) {
                    if (insn_load) {
                        out.base.new_kernel_flag = true;
                    }
                } else {
                    out.access_fault = true;
                }
            },
            .kernel_private => {
                if (!in.kernel_flag) {
                    out.access_fault = true;
                }
            },
        }
    } else if (disabled_translate) {
        out.base.bus_ctrl.wait_states = 0;
        if (in.cs_bus_mode == .insn) {
            out.base.new_kernel_flag = true;
        }
    }

    if ((virtual.offset & 1) == 1) {
        out.base.bus_ctrl.swap_bytes = true;
        if (in.cs_bus_byte == .word) {
            out.base.bus_ctrl.even_offset +%= 1;
            if (virtual.offset == 0xFFFF) {
                out.page_align_fault = true;
            }
        }
    }

    if (translate) {
        if (in.cs_bus_rw == .read) {
            out.base.bus_ctrl.read = true;
        } else if (in.cs_bus_rw == .write) {
            out.base.bus_ctrl.write = true;
            if (in.cs_bus_byte == .word) {
                out.base.bus_ctrl.write_even = true;
                out.base.bus_ctrl.write_odd = true;
            } else if ((virtual.offset & 1) == 1) {
                out.base.bus_ctrl.write_odd = true;
            } else {
                out.base.bus_ctrl.write_even = true;
            }
        }
    }

    return out;
}

pub const TransactInputs = struct {
    inhibit_writes: bool,
    entry_address: EntryAddress,
    matching_entry: Entry,
    other_entry: Entry,
    l: bus.LParts,
    tag: Tag,
    cs_at_op: ControlSignals.AddressTranslatorOp,
};

pub fn transact(self: *AddressTranslator, in: TransactInputs) void {
    if (in.inhibit_writes) return;

    switch (in.cs_at_op) {
        .none => {},
        .translate => {
            self.primary[in.entry_address] = in.matching_entry;
            self.secondary[in.entry_address] = in.other_entry;
        },
        .update => {
            self.primary[in.entry_address] = @bitCast(Entry, in.l);
            self.secondary[in.entry_address] = in.other_entry;
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

            self.primary[in.entry_address] = matching;
            self.secondary[in.entry_address] = other;
        },
    }
}
