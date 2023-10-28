const std = @import("std");
const bits = @import("bits");

pub const Address = packed struct (u17) {
    flags: Flags,
    index: Index,
};

pub const Index = enum (u12) {
    reset = 0,
    page_fault = 1,
    access_fault = 2,
    page_align_fault = 3,
    instruction_protection_fault = 4,
    invalid_instruction = 5,
    double_fault = 6,
    interrupt = 7,
    _,
};

pub const Continuation = Index;

pub const Flags = packed struct (u5) {
    z: bool,
    n: bool,
    c: bool,
    v: bool,
    k: bool,

    pub fn zero(self: Flags) bool { return self.z; }
    pub fn negative(self: Flags) bool { return self.n; }
    pub fn positive(self: Flags) bool { return !self.z and !self.n; }
    pub fn carryOrBorrow(self: Flags) bool { return self.c; }
    pub fn overflow(self: Flags) bool { return self.v; }
    pub fn kernel(self: Flags) bool { return self.k; }
    pub fn unsignedLessThan(self: Flags) bool { return self.c and !self.z; }
    pub fn unsignedGreaterThan(self: Flags) bool { return !self.c and !self.z; }
    pub fn signedLessThan(self: Flags) bool { return !self.z and self.n != self.v; }
    pub fn signedGreaterThan(self: Flags) bool { return !self.z and self.n == self.v; }

    pub const none: Flags = .{
        .z = false,
        .n = false,
        .c = false,
        .v = false,
        .k = false,
    };

    pub fn inverted(self: Flags) Flags {
        const raw: u5 = @bitCast(self);
        return @bitCast(raw ^ 0x1F);
    }

    pub fn combinedWith(self: Flags, other: Flags) Flags {
        const self_raw: u5 = @bitCast(self);
        const other_raw: u5 = @bitCast(other);
        return @bitCast(self_raw | other_raw);
    }

    pub fn permutations(self: Flags) PermutationIterator {
        return .{ .bits = self };
    }
    pub const PermutationIterator = struct {
        bits: Flags,
        next_permutation: ?Flags = none,

        pub fn next(self: *PermutationIterator) ?Flags {
            if (self.next_permutation) |permutation| {
                self.next_permutation = increment(permutation, self.bits);
                return permutation;
            }
            return null;
        }

        fn increment(permutation: Flags, permutable_bits: Flags) ?Flags {
            var result = permutation;
            if (permutable_bits.z) {
                result.z = !result.z;
                if (!permutation.z) return result;
            }
            if (permutable_bits.n) {
                result.n = !result.n;
                if (!permutation.n) return result;
            }
            if (permutable_bits.c) {
                result.c = !result.c;
                if (!permutation.c) return result;
            }
            if (permutable_bits.v) {
                result.v = !result.v;
                if (!permutation.v) return result;
            }
            if (permutable_bits.k) {
                result.k = !result.k;
                if (!permutation.k) return result;
            }
            return null;
        }
    };
};
