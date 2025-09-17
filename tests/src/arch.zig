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
        } else if (index.to_sr2()) |sr2_index| {
            errdefer std.debug.print("Any_Index.from_sr2(Base.{t}.to_sr2()) == Any_Index.{t}\n", .{ index, index.to_any() });
            try std.testing.expectEqual(arch.reg.sr.Any_Index.from_sr2(sr2_index), index.to_any());
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
        if (!std.mem.eql(u8, field.name, "_unused")) {
            try std.testing.expectEqual(
                @bitOffsetOf(arch.reg.Flags, field.name),
                @bitOffsetOf(arch.reg.Flags.Writable, field.name),
            );
            try std.testing.expectEqual(@FieldType(arch.reg.Flags, field.name), field.type);
        }
    }
}

const arch = @import("arch");
const meta = @import("meta");
const std = @import("std");
