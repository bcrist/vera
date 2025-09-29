jsrc: bus.J.Source,
ksrc: bus.K.Source,
sr1ri: reg.sr1.Index,
sr1wi: reg.sr1.Index,
sr1wsrc: reg.sr.Write_Source,
sr2ri: reg.sr2.Index,
sr2wi: reg.sr2.Index,
sr2wsrc: reg.sr.Write_Source,
unit: compute.Unit,
mode: compute.Mode,
flag_op: reg.Flags.Op,
lsrc: bus.L.Source,
vari: addr.Virtual.Base,
vao: addr.Virtual.Offset,
space: addr.Space,
at_op: addr.translation.Op,
dsrc: bus.D.Source,
width: bus.D.Width,
special: misc.Special_Op,
power: misc.Power_Mode,
gprw: misc.Generic_Write_Enable,
tiw: misc.Generic_Write_Enable,
drw: misc.Generic_Write_Enable,
irw: misc.Generic_Write_Enable,
allow_int: misc.Interrupt_Enable,
seq_op: misc.Sequencer_Op,
next: microcode.Slot,

pub const defaults: Control_Signals = .{
    .jsrc = .zero,
    .ksrc = .zero,
    .sr1ri = .init(0),
    .sr1wi = .init(0),
    .sr1wsrc = .no_write,
    .sr2ri = .init(0),
    .sr2wi = .init(0),
    .sr2wsrc = .no_write,
    .unit = .none,
    .mode = .init(0),
    .flag_op = .hold,
    .lsrc = .compute_or_d,
    .vari = .zero,
    .vao = .zero,
    .space = .physical,
    .at_op = .none,
    .dsrc = .system,
    .width = .@"32b",
    .special = .none,
    .power = .run,
    .gprw = .hold,
    .tiw = .hold,
    .drw = .hold,
    .irw = .hold,
    .allow_int = .disallow,
    .seq_op = .next_uop,
    .next = .invalid_instruction,
};

pub const zeroes: Control_Signals = .{
    .jsrc = .init(0),
    .ksrc = .init(0),
    .sr1ri = .init(0),
    .sr1wi = .init(0),
    .sr1wsrc = .init(0),
    .sr2ri = .init(0),
    .sr2wi = .init(0),
    .sr2wsrc = .init(0),
    .unit = .init(0),
    .mode = .init(0),
    .flag_op = .init(0),
    .lsrc = .init(0),
    .vari = .init(0),
    .vao = .init(0),
    .space = .init(0),
    .at_op = .init(0),
    .dsrc = .init(0),
    .width = .init(0),
    .special = .init(0),
    .power = .init(0),
    .gprw = .init(0),
    .tiw = .init(0),
    .drw = .init(0),
    .irw = .init(0),
    .allow_int = .init(0),
    .seq_op = .init(0),
    .next = .init(0),
};

pub fn eql(self: Control_Signals, other: Control_Signals) bool {
    inline for (comptime std.enums.values(misc.Control_Signal)) |field| switch (field) {
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
    inline for (comptime std.enums.values(Control_Signal)) |field| switch (field) {
        .mode => std.hash.autoHash(hasher, self.mode.raw()),
        else => std.hash.autoHash(hasher, @field(self, @tagName(field))),
    };
}

const Control_Signals = @This();
const Control_Signal = std.meta.FieldEnum(Control_Signals);

const microcode = @import("microcode.zig");
const misc = @import("misc.zig");
const compute = @import("compute.zig");
const reg = @import("reg.zig");
const bus = @import("bus.zig");
const addr = @import("addr.zig");
const std = @import("std");
