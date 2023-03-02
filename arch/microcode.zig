const std = @import("std");
const bits = @import("bits");
const misc = @import("misc");

const Opcode = misc.Opcode;

pub const Address = u16;
pub const Continuation = u10;
pub const Flags = enum { Z, N, C, V, K };
pub const FlagSet = std.EnumSet(Flags);

pub const Vectors = enum(Address) {
    reset = 0,
    page_fault = 1,
    access_fault = 2,
    page_align_fault = 3,
    instruction_protection_fault = 4,
    invalid_instruction = 5,
    double_fault = 6,
    interrupt = 7,

    pub const last = Vectors.interrupt;
};

// Use this to safely iterate over ranges of opcodes that might end on 0xFFFF
pub fn opcodeIterator(first: Opcode, last: Opcode) OpcodeIterator {
    return .{ .opcode = first, .last = last };
}
pub const OpcodeIterator = struct {
    opcode: u17,
    last: u17,

    pub fn next(self: *OpcodeIterator) ?Opcode {
        if (self.opcode <= self.last) {
            const opcode = @intCast(Opcode, self.opcode);
            self.opcode += getOpcodeGranularity(opcode);
            return opcode;
        } else {
            return null;
        }
    }
};

pub fn flagPermutationIterator(flags: FlagSet) UCFlagSetPermutationIterator {
    return .{ .all = flags };
}

pub const UCFlagSetPermutationIterator = struct {
    all: FlagSet,
    permutation: FlagSet = .{},
    done: bool = false,

    pub fn next(self: *UCFlagSetPermutationIterator) ?FlagSet {
        if (self.done) return null;

        var it = self.all.iterator();
        var to_return = self.permutation;
        var updated = to_return;
        while (it.next()) |flag| {
            if (!updated.contains(flag)) {
                // set the new bit
                updated.insert(flag);

                // reset all the lower bits
                var it2 = self.all.iterator();
                while (it2.next()) |flag2| {
                    if (flag2 != flag) {
                        updated.remove(flag2);
                    } else {
                        break;
                    }
                }

                self.permutation = updated;
                return to_return;
            }
        }

        self.done = true;
        return to_return;
    }
};

pub fn getCheckedFlagsForOpcode(opcode: Opcode) FlagSet {
    var flags = FlagSet{};

    if ((opcode & 0xF000) == 0) {
        if ((opcode & 0x0800) == 0) {
            // KZ opcode
            flags.insert(.K);
            flags.insert(.Z);
        } else if ((opcode & 0x0400) == 0) {
            // CNKZ opcode
            flags.insert(.C);
            flags.insert(.N);
            flags.insert(.K);
            flags.insert(.Z);
        } else {
            // VNKZ opcode
            flags.insert(.V);
            flags.insert(.N);
            flags.insert(.K);
            flags.insert(.Z);
        }
    } else {
        // K opcode
        flags.insert(.K);
    }

    return flags;
}

pub fn getAddressForOpcode(opcode: Opcode, flags: FlagSet) Address {
    var ua: Address = undefined;
    if ((opcode & 0xF000) == 0) {
        const dcba = @truncate(u4, opcode);
        const kjihgfe = @truncate(u7, opcode >> 4);
        ua = bits.concat(.{
            kjihgfe,
            dcba,
        });

        if ((opcode & 0x0800) == 0) {
            // KZ opcode
            ua |= 0x2000;
            if (flags.contains(.K)) ua |= 0x1000;
            if (flags.contains(.Z)) ua |= 0x800;
        } else if ((opcode & 0x0400) == 0) {
            // CNKZ opcode
            ua |= 0x8000;
            if (flags.contains(.C)) ua |= 0x4000;
            if (flags.contains(.N)) ua |= 0x2000;
            if (flags.contains(.K)) ua |= 0x1000;
            if (flags.contains(.Z)) ua |= 0x800;
        } else {
            // VNKZ opcode
            ua |= 0x8000;
            if (flags.contains(.V)) ua |= 0x4000;
            if (flags.contains(.N)) ua |= 0x2000;
            if (flags.contains(.K)) ua |= 0x1000;
            if (flags.contains(.Z)) ua |= 0x800;
        }
    } else {
        // K opcode
        const kjihgfe = @truncate(u7, opcode >> 4);
        const l = @truncate(u1, opcode >> 11);
        const ponm = @truncate(u4, opcode >> 12);
        ua = bits.concat(.{
            kjihgfe,
            ponm,
            l,
        });
        if (flags.contains(.K)) ua |= 0x1000;
    }
    return ua;
}

pub fn getOpcodeGranularity(opcode: Opcode) Opcode {
    if ((opcode & 0xF000) != 0) {
        // low 4 bits (OA) are not included in the Address for this opcode range
        return 16;
    } else {
        return 1;
    }
}

pub fn getOpcodeForAddress(ua: Address) ?Opcode {
    if ((ua & 0x8000) == 0x8000) {
        // CNKZ or VNKZ opcode
        const dcba = @truncate(u4, ua >> 7);
        const kjihgfe = @truncate(u7, ua);
        return bits.concat(.{
            dcba,
            kjihgfe,
            @as(u5, 1),
        });
    } else if ((ua & 0x4000) == 0x4000) {
        // conditional continuation cycle
        return null;
    } else if ((ua & 0x2000) == 0x2000) {
        // KZ opcode
        const dcba = @truncate(u4, ua >> 7);
        const kjihgfe = @truncate(u7, ua);
        return bits.concat(.{
            dcba,
            kjihgfe,
            @as(u5, 0),
        });
    } else if ((ua & 0x0780) == 0) {
        // unconditional continuation cycle
        return null;
    } else {
        // K opcode
        const l = @truncate(u1, ua >> 11);
        const ponm = @truncate(u4, ua >> 7);
        const kjihgfe = @truncate(u7, ua);
        return bits.concat(.{
            @as(u4, 0),
            kjihgfe,
            l,
            ponm,
        });
    }
}

pub fn getOAForAddress(ua: Address) ?misc.OperandA {
    if ((ua & 0x8000) == 0x8000) {
        // CNKZ or VNKZ opcode
        return @truncate(misc.OperandA, ua >> 7);
    } else if ((ua & 0x4000) == 0x4000) {
        // conditional continuation cycle
        return null;
    } else if ((ua & 0x2000) == 0x2000) {
        // KZ opcode
        return @truncate(misc.OperandA, ua >> 7);
    } else {
        // K opcode or unconditional continuation cycle
        return null;
    }
}

pub fn getOBForAddress(ua: Address) ?misc.OperandB {
    if ((ua & 0x8000) == 0x8000) {
        // CNKZ or VNKZ opcode
        return @truncate(misc.OperandB, ua);
    } else if ((ua & 0x4000) == 0x4000) {
        // conditional continuation cycle
        return null;
    } else if ((ua & 0x2000) == 0x2000) {
        // KZ opcode
        return @truncate(misc.OperandB, ua);
    } else if ((ua & 0x0780) == 0) {
        // unconditional continuation cycle
        return null;
    } else {
        // K opcode
        return @truncate(misc.OperandB, ua);
    }
}

pub fn isContinuationOrHandler(ua: Address) bool {
    if ((ua & 0x8000) == 0x8000) {
        // CNKZ or VNKZ opcode
        return false;
    } else if ((ua & 0x4000) == 0x4000) {
        // conditional continuation cycle
        return true;
    } else if ((ua & 0x2000) == 0x2000) {
        // KZ opcode
        return false;
    } else if ((ua & 0x0780) == 0) {
        // unconditional continuation cycle
        return true;
    } else {
        // K opcode
        return false;
    }
}

pub fn getAddressForContinuation(n: Continuation, flags: FlagSet) Address {
    if ((n & 0x200) == 0) {
        // unconditional continuation cycle
        const ih = @truncate(u2, n >> 7);
        const gfedcba = @truncate(u7, n);
        return bits.concat(.{
            gfedcba,
            @as(u4, 0),
            ih,
        });
    } else {
        // conditional continuation cycle
        const ihgfedcba = @truncate(u9, n);
        const V = @boolToInt(flags.contains(.V));
        const C = @boolToInt(flags.contains(.C));
        const Z = @boolToInt(flags.contains(.Z));
        const K = @boolToInt(flags.contains(.K));
        const N = @boolToInt(flags.contains(.N));
        return bits.concat(.{
            ihgfedcba,
            V,
            C,
            Z,
            K,
            N,
            @as(u1, 1),
        });
    }
}

pub fn getContinuationNumberForAddress(ua: Address) ?Continuation {
    if ((ua & 0x8000) == 0x8000) {
        // CNKZ or VNKZ opcode
        return null;
    } else if ((ua & 0x4000) == 0x4000) {
        // conditional continuation cycle
        return bits.concat(.{
            @truncate(u9, ua),
            @as(u1, 1),
        });
    } else if ((ua & 0x2000) == 0x2000) {
        // KZ opcode
        return null;
    } else if ((ua & 0x0780) == 0) {
        // unconditional continuation cycle
        return bits.concat(.{
            @truncate(u7, ua),
            @truncate(u2, ua >> 11),
        });
    } else {
        // K opcode
        return null;
    }
}

pub fn getCheckedFlagsForAddress(ua: Address) FlagSet {
    var result = FlagSet{};

    if ((ua & 0x8000) == 0x8000) {
        // CNKZ or VNKZ opcode
        result.insert(.N);
        result.insert(.K);
        result.insert(.Z);
        if ((ua & 0x0040) == 0x0040) {
            result.insert(.V);
        } else {
            result.insert(.C);
        }
    } else if ((ua & 0x4000) == 0x4000) {
        // conditional continuation cycle
        result.insert(.N);
        result.insert(.K);
        result.insert(.Z);
        result.insert(.C);
        result.insert(.V);
    } else if ((ua & 0x2000) == 0x2000) {
        // KZ opcode
        result.insert(.K);
        result.insert(.Z);
    } else if ((ua & 0x0780) == 0) {
        // unconditional continuation cycle

    } else {
        // K opcode
        result.insert(.K);
    }

    return result;
}

pub fn getFlagsForAddress(ua: Address) FlagSet {
    var result = FlagSet{};

    if ((ua & 0x8000) == 0x8000) {
        // CNKZ or VNKZ opcode
        if ((ua & 0x4000) == 0x4000) result.insert(if ((ua & 0x0040) == 0x0040) .V else .C);
        if ((ua & 0x2000) == 0x2000) result.insert(.N);
        if ((ua & 0x1000) == 0x1000) result.insert(.K);
        if ((ua & 0x0800) == 0x0800) result.insert(.Z);
    } else if ((ua & 0x4000) == 0x4000) {
        // conditional continuation cycle
        if ((ua & 0x2000) == 0x2000) result.insert(.N);
        if ((ua & 0x1000) == 0x1000) result.insert(.K);
        if ((ua & 0x0800) == 0x0800) result.insert(.Z);
        if ((ua & 0x0400) == 0x0400) result.insert(.C);
        if ((ua & 0x0200) == 0x0200) result.insert(.V);
    } else if ((ua & 0x2000) == 0x2000) {
        // KZ opcode
        if ((ua & 0x1000) == 0x1000) result.insert(.K);
        if ((ua & 0x0800) == 0x0800) result.insert(.Z);
    } else if ((ua & 0x0780) == 0) {
        // unconditional continuation cycle

    } else {
        // K opcode
        if ((ua & 0x1000) == 0x1000) result.insert(.K);
    }

    return result;
}
