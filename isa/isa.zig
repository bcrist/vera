pub const Instruction = struct {
    mnemonic: Mnemonic,
    suffix: Mnemonic_Suffix,
    params: []const Parameter,

    pub fn eql(a: Instruction, b: Instruction) bool {
        return deep_hash_map.deepEql(a, b, .DeepRecursive);
    }

    pub fn format(self: Instruction, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        try print.print_instruction(self, null, writer);
    }

    pub const Signature = struct {
        mnemonic: Mnemonic,
        suffix: Mnemonic_Suffix,
        params: []const Parameter.Signature,

        pub fn eql(self: Signature, other: Signature) bool {
            if (self.mnemonic != other.mnemonic or self.suffix != other.suffix or self.params.len != other.params.len) return false;
            for (self.params, other.params) |sp, op| {
                if (!std.meta.eql(sp, op)) return false;
            }
            return true;
        }

        pub fn format(self: Signature, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
            _ = fmt;
            _ = options;
            try print.print_instruction_signature(self, writer);
        }
    };
};

pub const Parameter = struct {
    signature: Signature,
    base_register_index: arch.Register_Index,
    offset_register_index: arch.Register_Index,
    constant: i64,

    pub fn format(self: Parameter, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        try print.print_parameter(self, null, writer);
    }

    pub const Signature = struct {
        address_space: ?Address_Space,
        base: Kind,
        offset: Kind,

        pub fn format(self: Signature, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
            _ = fmt;
            _ = options;
            try print.print_parameter_signature(self, .{}, writer);
        }
    };

    pub const Kind = union (enum) {
        none,
        arrow,
        constant,
        reg: ?Signedness,
        sr: Special_Register,

        pub fn format(self: Signature, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
            _ = fmt;
            _ = options;
            try print.print_parameter_kind(self, .{}, writer);
        }
    };

    pub const Index = enum (u4) {
        invalid = 0xF,
        _,
        pub fn init(raw_value: u4) Index {
            return @enumFromInt(raw_value);
        }
        pub fn raw(self: Index) u4 {
            return @intFromEnum(self);
        }
        pub const Raw = std.meta.Tag(Index);
        pub const count = std.math.maxInt(Raw) + 1;
    };
};

pub const Instruction_Encoding = @import("isa/Instruction_Encoding.zig");

pub const Encoded_Instruction = struct {
    data: Data,
    len: Length_Type,

    pub const Data = u128;
    pub const Bit_Length_Type = std.math.Log2Int(Data);
    pub const Length_Type = std.meta.Int(.unsigned, @bitSizeOf(Bit_Length_Type) - 3);

    pub fn as_bytes(self: *const Encoded_Instruction) []const u8 {
        return std.mem.asBytes(&self.data)[0..self.len];
    }
};

pub const Mnemonic = enum {
    _reserved,
    // Arithmetic:
    add, addc, nadd, naddc, sub, subc,
    addv, addcv, naddv, naddcv, subv, subcv,
    inc, dec, neg, negc,
    incv, decv, negv, negcv,
    cmp, cmpc,
    // Shifts & Swaps
    shl, shlc, shr, shrs, shrc,
    swap, swap2, byterev, bitrev, bitrev1, bitrev2,
    // Logical:
    xor, @"or", @"and",
    // Single bit:
    tb, cb, sb,
    // Bit counting:
    csb, czb, csbl, czbl, csbt, czbt,
    ssbl, szbl, ssbt, szbt,
    // Extension & Truncation:
    sx, zx, sxv, zxv,
    // Multiply:
    mul224, mul444, mul244, mul424, mul448, mul248, mul428,
    // Data movement:
    val, set, drop,
    ld1, ld2, ld3, ld,
    st1, st2, st3, st,
    ldi1, ldi2, ldi3, ldi,
    sti1, sti2, sti3, sti,
    ild1, ild2, ild3, ild,
    ist1, ist2, ist3, ist,
    ldg1, ldg2, ldg3, ldg,
    stg1, stg2, stg3, stg,
    // Stack:
    frame, unframe,
    spill, unspill,
    pop, push,
    // Memcopy & streaming:
    mcf1, mcf2, mcf,
    mcb1, mcb2, mcb,
    si1, si2, si,
    so1, so2, so,
    // PSRAM/FLASH access
    bpush, bpull,
    // Branches & Calls:
    call, ptrcall, relcall,
    b, relb, bb, bbn, bp, bpn,
    eab, dab, 
    ret,
    // Faults, interrupts, and context switching:
    fret, iret, ifex, ldrs, strs, srs, park,
    // MMU:
    sat, rat,
    // Misc:
    nop, ovf,

    pub fn name(self: Mnemonic) []const u8 {
        return switch (self) {
            ._reserved => "?",
            .add => "Addition",
            .addv => "Addition (fault on overflow)",
            .addc => "Addition (with carry)",
            .addcv => "Addition (with carry, fault on overflow)",
            .nadd => "Negate and Add",
            .naddv => "Negate and Add (fault on overflow)",
            .naddc => "Negate and Add (with carry)",
            .naddcv => "Negate and Add (with carry, fault on overflow)",
            .sub => "Subtract",
            .subv => "Subtract (fault on overflow)",
            .subc => "Subtract (with carry)",
            .subcv => "Subtract (with carry, fault on overflow)",
            .inc => "Increment",
            .incv => "Increment (fault on overflow)",
            .dec => "Decrement",
            .decv => "Decrement (fault on overflow)",
            .neg => "Negate",
            .negv => "Negate (fault on overflow)",
            .negc => "Negate (with carry)",
            .negcv => "Negate (with carry, fault on overflow)",
            .cmp => "Compare",
            .cmpc => "Compare (with carry)",
            .shr => "Shift Right (logical)",
            .shrs => "Shift Right (arithmetic/signed)",
            .shl => "Shift Left",
            .shrc => "Shift Right (1 bit from carry)",
            .shlc => "Shift Left (1 bit from carry)",
            .swap => "Swap 16b Halves",
            .swap2 => "Swap Bytes Within 16b Halves",
            .byterev => "Reverse Bytes",
            .bitrev => "Reverse Bits",
            .bitrev1 => "Reverse Bits Within Bytes",
            .bitrev2 => "Reverse Bits Within 16b Halves",
            .xor => "Bitwise Exclusive OR",
            .@"or" => "Bitwise OR",
            .@"and" => "Bitwise AND",
            .tb => "Test Bit",
            .cb => "Clear Bit",
            .sb => "Set Bit",
            .csb => "Count Set Bits",
            .czb => "Count Zero Bits",
            .csbl => "Count Set Bits (leading)",
            .czbl => "Count Zero Bits (leading)",
            .csbt => "Count Set Bits (trailing)",
            .czbt => "Count Set Bits (trailing)",
            .ssbl => "Saturate Set Bits (leading)",
            .szbl => "Saturate Zero Bits (leading)",
            .ssbt => "Saturate Set Bits (trailing)",
            .szbt => "Saturate Zero Bits (trailing)",
            .sx => "Sign Extend/Truncate",
            .zx => "Zero Extend/Truncate",
            .sxv => "Sign Extend/Truncate (fault on overflow)",
            .zxv => "Zero Extend/Truncate (fault on overflow)",
            .mul224 => "Multiply 16b * 16b -> 32b",
            .mul444 => "Multiply 32b * 32b -> 32b",
            .mul244 => "Multiply 16b * 32b -> 32b",
            .mul424 => "Multiply 32b * 16b -> 32b",
            .mul448 => "Multiply 32b * 32b -> 64b",
            .mul248 => "Multiply 16b * 32b -> 64b",
            .mul428 => "Multiply 32b * 16b -> 64b",
            .val => "Push Value/Address to Register Stack",
            .set => "Assign Special Register",
            .drop => "Drop Value(s) from Register Stack",
            .ld1 => "Load 8b",
            .ld2 => "Load 16b",
            .ld3 => "Load 24b",
            .ld => "Load 32b",
            .st1 => "Store 8b",
            .st2 => "Store 16b",
            .st3 => "Store 24b",
            .st => "Store 32b",
            .ldi1 => "Load 8b and Increment",
            .ldi2 => "Load 16b and Increment",
            .ldi3 => "Load 24b and Increment",
            .ldi => "Load 32b and Increment",
            .sti1 => "Store 8b and Increment",
            .sti2 => "Store 16b and Increment",
            .sti3 => "Store 24b and Increment",
            .sti => "Store 32b and Increment",
            .ild1 => "Increment and Load 8b",
            .ild2 => "Increment and Load 16b",
            .ild3 => "Increment and Load 24b",
            .ild => "Increment and Load 32b",
            .ist1 => "Increment and Store 8b",
            .ist2 => "Increment and Store 16b",
            .ist3 => "Increment and Store 24b",
            .ist => "Increment and Store 32b",
            .ldg1 => "Load 8b (guarded)",
            .ldg2 => "Load 16b (guarded)",
            .ldg3 => "Load 24b (guarded)",
            .ldg => "Load 32b (guarded)",
            .stg1 => "Store 8b (guarded)",
            .stg2 => "Store 16b (guarded)",
            .stg3 => "Store 24b (guarded)",
            .stg => "Store 32b (guarded)",
            .frame => "Add Stack Frame",
            .unframe => "Remove Stack Frame",
            .spill => "Spill Register Stack",
            .unspill => "Restore Spilled Register Stack",
            .pop => "Pop from Stack",
            .push => "Push to Stack",
            .mcb1 => "Memory Copy Backward (align 1)",
            .mcb2 => "Memory Copy Backward (align 2)",
            .mcb => "Memory Copy Backward (align 4)",
            .mcf1 => "Memory Copy Forward (align 1)",
            .mcf2 => "Memory Copy Forward (align 2)",
            .mcf => "Memory Copy Forward (align 4)",
            .si1 => "Stream Into Memory (align 1)",
            .si2 => "Stream Into Memory (align 2)",
            .si => "Stream Into Memory (align 4)",
            .so1 => "Stream Memory Out (align 1)",
            .so1 => "Stream Memory Out (align 2)",
            .so => "Stream Memory Out (align 4)",
            .bpush => "Push Block to PSRAM/FLASH",
            .bpull => "Pull Block from PSRAM/FLASH",
            .call => "Call",
            .ptrcall => "Pointer Call (indirect)",
            .relcall => "IP-Relative Register Call",
            .b => "Branch",
            .relb => "IP-Relative Register Branch",
            .bb => "Branch to Start of Current Block (align 256)",
            .bbn => "Branch to Start of Next Block (align 256)",
            .bp => "Branch to Start of Current Page (align 4096)",
            .bpn => "Branch to Start of Next Page (align 4096)",
            .eab => "Enable Address Translation and Branch",
            .dab => "Disable Address Translation and Branch", 
            .ret => "Return from Call",
            .fret => "Return from Fault",
            .iret => "Return from Interrupt",
            .ifex => "Exit from Fault/Interrupt",
            .ldrs => "Load Registerset",
            .strs => "Store Registerset",
            .srs => "Switch Registerset",
            .park => "Park",
            .sat => "Set Address Translation",
            .rat => "Release Address Translation",
            .nop => "No Operation",
            .ovf => "Trigger Overflow Fault",
        };
    }
};

pub const Mnemonic_Suffix = enum {
    none,

    z, // zero
    nz, // not zero
    lu, // less (unsigned)
    nlu, // not less (unsigned)
    gu, // greater (unsigned)
    ngu, // not greater (unsigned)
    n, // negative
    nn, // not negative
    c, // carry
    nc, // not carry
    v, // overflow
    nv, // not overflow
    ls, // less (signed)
    nls, // not less (signed)
    gs, // greater (signed)
    ngs, // not greater (signed)
    p, // positive
    np, // not positive
    lu_gu, // less (unsigned), greater (unsigned)
    lu_z, // less (unsigned), zero
    gu_z, // greater (unsigned), zero
    ls_gs, // less (signed), greater (signed)
    ls_z, // less (signed), zero
    gs_z, // greater (signed), zero
    n_z, // negative, zero
    p_z, // positive, zero
    n_p, // negative, positive

    u, // unsigned
    uu, // unsigned-unsigned
    us, // unsigned-signed
    su, // signed-unsigned
    ss, // signed-signed
    s, // signed or stack
    i, // insn
    d, // data
    w, // data-write
    r, // data-read
};

pub const Special_Register = enum {
    ip,
    sp,
    rp,
    bp,
    uxp,
    kxp,
    asn,
    stat,
};

pub const Address_Space = enum {
    data,
    insn,
    stack,

    pub fn directive_name(self: ?Address_Space) []const u8 {
        return if (self) |s| switch (s) {
            .data => ".d",
            .insn => ".i",
            .stack => ".s",
        } else ".raw";
    }
};

pub const Branch_Kind = enum {
    nonbranching,
    conditional,
    unconditional,
    call,
};
pub fn branch_kind(mnemonic: Mnemonic, suffix: Mnemonic_Suffix) Branch_Kind {
    return switch (mnemonic) {
        ._reserved => unreachable,

        .add, .addc, .nadd, .naddc, .sub, .subc,
        .addv, .addcv, .naddv, .naddcv, .subv, .subcv,
        .inc, .dec, .neg, .negc,
        .incv, .decv, .negv, .negcv,
        .cmp, .cmpc,
        .shl, .shlc, .shr, .shrs, .shrc,
        .swap, .swap2, .byterev, .bitrev, .bitrev1, .bitrev2,
        .xor, .@"or", .@"and",
        .tb, .cb, .sb,
        .csb, .czb, .csbl, .czbl, .csbt, .czbt,
        .ssbt, .szbt, .ssbl, .szbl,
        .sx, .zx, .sxv, .zxv,
        .mul224, .mul444, .mul244, .mul424, .mul448, .mul248, .mul428,
        .val, .set, .drop,
        .ld1, .ld2, .ld3, .ld,
        .st1, .st2, .st3, .st,
        .ldi1, .ldi2, .ldi3, .ldi,
        .sti1, .sti2, .sti3, .sti,
        .ild1, .ild2, .ild3, .ild,
        .ist1, .ist2, .ist3, .ist,
        .ldg1, .ldg2, .ldg3, .ldg,
        .stg1, .stg2, .stg3, .stg,
        .frame, .unframe,
        .spill, .unspill,
        .pop, .push,
        .mcf1, .mcf2, .mcf,
        .mcb1, .mcb2, .mcb,
        .si1, .si2, .si,
        .so1, .so2, .so,
        .bpush, .bpull,
        .ifex, .ldrs, .strs, .srs,
        .sat, .rat,
        .nop,
        => .nonbranching,

        .b, .relb, .bb, .bbn, .bp, .bpn,
        .eab, .dab, .ret, .fret, .iret, .park, .ovf,
        => switch (suffix) {
            .none, .i, .s, .d, .w, .r, .u, .uu, .us, .su, .ss => .unconditional,
            else => .conditional,
        },

        .call, .ptrcall, .relcall => .call,
    };
}

pub const print = @import("isa/print.zig");
pub const lex = @import("isa/lex.zig");
pub const parse_helpers = @import("isa/parse_helpers.zig");

const arch = @import("arch");
const deep_hash_map = @import("deep_hash_map");
const Signedness = std.builtin.Signedness;
const std = @import("std");
