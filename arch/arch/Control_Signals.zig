jsrc: arch.J.Source,
ksrc: arch.K.Source,
unit: arch.Compute_Unit,
mode: arch.Compute_Mode,
lsrc: arch.L.Source,
vari: arch.addr.Virtual.Base_SR_Index,
vao: arch.addr.Virtual.Microcode_Offset,
vaspace: arch.addr.Space,
atop: arch.addr.translation.Op,
dir: arch.D.Direction,
width: arch.D.Width,
sr1ri: arch.SR1_Index,
sr2ri: arch.SR2_Index,
sr1wi: arch.SR1_Index,
sr2wi: arch.SR2_Index,
sr1wsrc: arch.SR_Write_Source,
sr2wsrc: arch.SR_Write_Source,
special: arch.Special_Op,
seqop: arch.Sequencer_Op,
statop: arch.Status.Op,
gprw: bool,
irw: bool,
drw: bool,
tiw: bool,
allowint: bool,
power: arch.Power_Mode,
next: arch.microcode.Slot,

pub const zeroes: Control_Signals = .{
    .jsrc = arch.J.Source.init(0),
    .ksrc = arch.K.Source.init(0),
    .unit = arch.Compute_Unit.init(0),
    .mode = arch.Compute_Mode.init(0),
    .lsrc = arch.L.Source.init(0),
    .vari = arch.addr.Virtual.Base_SR_Index.init(0),
    .vao = arch.addr.Virtual.Microcode_Offset.init(0),
    .vaspace = arch.addr.Space.init(0),
    .atop = arch.addr.translation.Op.init(0),
    .dir = arch.D.Direction.init(0),
    .width = arch.D.Width.init(0),
    .sr1ri = arch.SR1_Index.init(0),
    .sr2ri = arch.SR2_Index.init(0),
    .sr1wi = arch.SR1_Index.init(0),
    .sr2wi = arch.SR2_Index.init(0),
    .sr1wsrc = arch.SR_Write_Source.init(0),
    .sr2wsrc = arch.SR_Write_Source.init(0),
    .special = arch.Special_Op.init(0),
    .seqop = arch.Sequencer_Op.init(0),
    .statop = arch.Status.Op.init(0),
    .gprw = false,
    .irw = false,
    .drw = false,
    .tiw = false,
    .allowint = false,
    .power = arch.Power_Mode.init(0),
    .next = arch.microcode.Slot.init(0),
};

pub fn eql(self: Control_Signals, other: Control_Signals) bool {
    inline for (comptime std.enums.values(arch.Control_Signal)) |field| switch (field) {
        .mode => {
            const self_value = self.mode.raw();
            const other_value = other.mode.raw();
            if (self_value != other_value) return false;
        },
        else => {
            const self_value = @field(self, @tagName(field));
            const other_value = @field(other, @tagName(field));
            if (!std.meta.eql(self_value, other_value)) return false;
        },
    };
    return true;
}

pub fn hash(self: Control_Signals, hasher: anytype) void {
    inline for (comptime std.enums.values(arch.Control_Signal)) |field| switch (field) {
        .mode => std.hash.autoHash(hasher, self.mode.raw()),
        else => std.hash.autoHash(hasher, @field(self, @tagName(field))),
    };
}

const Control_Signals = @This();
const arch = @import("../arch.zig");
const std = @import("std");
