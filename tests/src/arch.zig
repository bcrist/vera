test "arch.reg.sr.Any_Index" {
    // Any_Index.Raw can't be defined directly using meta.Backing since it will cause a dependency loop, so double check that it's accurate:
    try std.testing.expectEqual(meta.Backing(arch.reg.sr.Any_Index), arch.reg.sr.Any_Index.Raw);

    inline for (comptime std.enums.values(arch.reg.sr1.Index)) |sr1_index| {
        try std.testing.expectEqual(sr1_index, arch.reg.sr.Any_Index.from_sr1(sr1_index).to_sr1().?);
    }

    inline for (comptime std.enums.values(arch.reg.sr2.Index)) |sr2_index| {
        try std.testing.expectEqual(sr2_index, arch.reg.sr.Any_Index.from_sr2(sr2_index).to_sr2().?);
    }

    inline for (comptime std.enums.values(arch.reg.sr.Any_Index)) |index| {
        if (index.to_sr1()) |sr1_index| {
            errdefer std.debug.print("Any_Index.{t}.to_sr1() == sr1.Index.{t}\n", .{ index, sr1_index });
            try std.testing.expectEqualStrings(@tagName(sr1_index), @tagName(index));
        } else if (index.to_sr2()) |sr2_index| {
            errdefer std.debug.print("Any_Index.{t}.to_sr2() == sr2.Index.{t}\n", .{ index, sr2_index });
            try std.testing.expectEqualStrings(@tagName(sr2_index), @tagName(index));
        } else {
            std.debug.print("{f} has no corresponding SR1/SR2 index!\n", .{ index });
            return error.BadEnum;
        }
    }
}

test "arch.addr.Virtual.Base" {
    inline for (comptime std.enums.values(arch.addr.Virtual.Base)) |index| {
        if (index.to_sr1()) |sr1_index| {
            errdefer std.debug.print("Any_Index.from_sr1(Base.{t}.to_sr1()) == Any_Index.{t}\n", .{ index, index.to_any() });
            try std.testing.expectEqual(arch.reg.sr.Any_Index.from_sr1(sr1_index), index.to_any());
            try std.testing.expectEqual(@as(u3, @truncate(sr1_index.raw())), @as(u3, @truncate(index.raw())));
        } else if (index.to_sr2()) |sr2_index| {
            errdefer std.debug.print("Any_Index.from_sr2(Base.{t}.to_sr2()) == Any_Index.{t}\n", .{ index, index.to_any() });
            try std.testing.expectEqual(arch.reg.sr.Any_Index.from_sr2(sr2_index), index.to_any());
            try std.testing.expectEqual(@as(u3, @truncate(sr2_index.raw())), @as(u3, @truncate(index.raw())));
        } else {
            std.debug.print("{f} has no corresponding SR1/SR2 index!\n", .{ index });
            return error.BadEnum;
        }

        errdefer std.debug.print("Base.{t}.to_any() == Any_Index.{t}\n", .{ index, index.to_any() });
        try std.testing.expectEqualStrings(@tagName(index.to_any()), @tagName(index));
    }
}

test "arch.reg.Flags.Writable" {
    inline for (@typeInfo(arch.reg.Flags.Writable).@"struct".fields) |field| {
        if (comptime !std.mem.eql(u8, field.name, "_unused")) {
            try std.testing.expectEqual(
                @bitOffsetOf(arch.reg.Flags, field.name),
                @bitOffsetOf(arch.reg.Flags.Writable, field.name),
            );
            try std.testing.expectEqual(@FieldType(arch.reg.Flags, field.name), field.type);
        }
    }
}

test "arch.microcode.Slot.Sequencer_Literal" {
    inline for (@typeInfo(arch.microcode.Slot.Sequencer_Literal).@"enum".fields) |field| {
        try std.testing.expectEqual(
            @intFromEnum(@field(arch.microcode.Slot, field.name)),
            @intFromEnum(@field(arch.microcode.Slot.Sequencer_Literal, field.name)),
        );
    }

    inline for (@typeInfo(arch.microcode.Slot).@"enum".fields) |field| {
        if (@intFromEnum(@field(arch.microcode.Slot, field.name)) < arch.microcode.Slot.Sequencer_Literal.count) {
            try std.testing.expectEqual(
                @intFromEnum(@field(arch.microcode.Slot, field.name)),
                @intFromEnum(@field(arch.microcode.Slot.Sequencer_Literal, field.name)),
            );
        }
    }
}

test "All control signals assigned to microcode ROMs" {
    inline for (@typeInfo(arch.Control_Signals).@"struct".fields) |field| {
        if (!@hasField(arch.microcode.Setup_Microcode_Entry, field.name) and
            !@hasField(arch.microcode.Compute_Microcode_Entry, field.name) and
            !@hasField(arch.microcode.Transact_Microcode_Entry, field.name) and
            !@hasField(arch.microcode.Decode_Microcode_Entry, field.name)
        ) {
            @compileError("Control Signal not mapped to any microcode ROM: " ++ field.name);
        }
    }
}

test "microcode control signals match Control_Signals.zig" {
    inline for (@typeInfo(arch.microcode.Setup_Microcode_Entry).@"struct".fields) |field| {
        if (comptime !std.mem.startsWith(u8, field.name, "_unused")) try std.testing.expectEqual(
            @FieldType(arch.Control_Signals, field.name),
            @FieldType(arch.microcode.Setup_Microcode_Entry, field.name),
        );
    }

    inline for (@typeInfo(arch.microcode.Compute_Microcode_Entry).@"struct".fields) |field| {
        if (comptime !std.mem.startsWith(u8, field.name, "_unused")) try std.testing.expectEqual(
            @FieldType(arch.Control_Signals, field.name),
            @FieldType(arch.microcode.Compute_Microcode_Entry, field.name),
        );
    }

    inline for (@typeInfo(arch.microcode.Transact_Microcode_Entry).@"struct".fields) |field| {
        if (comptime !std.mem.startsWith(u8, field.name, "_unused")) try std.testing.expectEqual(
            @FieldType(arch.Control_Signals, field.name),
            @FieldType(arch.microcode.Transact_Microcode_Entry, field.name),
        );
    }

    inline for (@typeInfo(arch.microcode.Decode_Microcode_Entry).@"struct".fields) |field| {
        if (comptime !std.mem.startsWith(u8, field.name, "_unused")) try std.testing.expectEqual(
            @FieldType(arch.Control_Signals, field.name),
            @FieldType(arch.microcode.Decode_Microcode_Entry, field.name),
        );
    }
}

const arch = @import("arch");
const meta = @import("meta");
const std = @import("std");
