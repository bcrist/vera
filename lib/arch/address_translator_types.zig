const std = @import("std");
const bus = @import("bus_types");
const ControlSignals = @import("ControlSignals");

pub const Slot = u6;
pub const Tag = u14;
pub const AddressSpaceNumber = u4; // aka ASN; The low portion of it that's used by the address translator
pub const EntryAddress = u12; // combination of ASN, group, and non-tag bits from page number

pub const AccessPolicy = enum(u2) {
    unprivileged = 0,
    kernel_entry_256 = 1,
    kernel_entry_4096 = 2,
    kernel_private = 3,
};

// Data that is stored for each address translation;
// you must construct this structure in X0 before using the SAT instruction.
pub const Entry = packed struct(u32) {
    frame: bus.Frame = 0,
    wait_states: u2 = 0,
    update_frame_state: bool = false,
    present: bool = false,
    access: AccessPolicy = .unprivileged,
    tag: Tag = 0,

    pub fn random(rnd: std.rand.Random) Entry {
        return .{
            .frame = rnd.int(bus.Frame),
            .wait_states = rnd.int(u2),
            .update_frame_state = rnd.boolean(),
            .present = rnd.boolean(),
            .access = rnd.enumValue(AccessPolicy),
            .tag = rnd.int(Tag),
        };
    }
};

// Information saved when handling a fault;
// useful for determining what page needs to be loaded for a page fault.
// N.B. Upper 20 bits are identical to virtual address
pub const TranslationInfo = packed struct(u32) {
    cs_bus_rw: ControlSignals.BusDirection = .read,
    cs_bus_byte: ControlSignals.BusWidth = .word,
    cs_bus_mode: ControlSignals.BusMode = .raw,
    cs_at_op: ControlSignals.AddressTranslatorOp = .none,
    _padding: u6 = 0,
    slot: Slot = 0,
    tag: Tag = 0,

    pub fn random(rnd: std.rand.Random) TranslationInfo {
        return .{
            .cs_bus_rw = rnd.enumValue(ControlSignals.BusDirection),
            .cs_bus_byte = rnd.enumValue(ControlSignals.BusWidth),
            .cs_bus_mode = rnd.enumValue(ControlSignals.BusMode),
            .cs_at_op = rnd.enumValue(ControlSignals.AddressTranslatorOp),
            .slot = rnd.int(Slot),
            .tag = rnd.int(Tag),
        };
    }
};
