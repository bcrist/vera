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

pub const Mnemonic = enum (u128) {
    nop = raw_mnemonic("nop"),
    park = raw_mnemonic("park"),
    ret = raw_mnemonic("ret"),
    iret = raw_mnemonic("iret"),
    fret = raw_mnemonic("fret"),
    freto = raw_mnemonic("freto"),
    ifex = raw_mnemonic("ifex"),
    val = raw_mnemonic("val"),

    add = raw_mnemonic("add"),
    addv = raw_mnemonic("addv"),
    addc = raw_mnemonic("addc"),
    addcv = raw_mnemonic("addcv"),

    sub = raw_mnemonic("sub"),
    subv = raw_mnemonic("subv"),
    subc = raw_mnemonic("subc"),
    subcv = raw_mnemonic("subcv"),

    nadd = raw_mnemonic("nadd"),
    naddv = raw_mnemonic("naddv"),
    naddc = raw_mnemonic("naddc"),
    naddcv = raw_mnemonic("naddcv"),

    shl = raw_mnemonic("shl"),
    shlv = raw_mnemonic("shlv"),
    shlc = raw_mnemonic("shlc"),

    shr = raw_mnemonic("shr"),
    shrv = raw_mnemonic("shrv"),
    shrc = raw_mnemonic("shrc"),
    shrs = raw_mnemonic("shrs"),
    shrsv = raw_mnemonic("shrsv"),

    inc = raw_mnemonic("inc"),
    incv = raw_mnemonic("incv"),
    dec = raw_mnemonic("dec"),
    decv = raw_mnemonic("decv"),
    cmp = raw_mnemonic("cmp"),
    cmpc = raw_mnemonic("cmpc"),

    neg = raw_mnemonic("neg"),
    negv = raw_mnemonic("negv"),
    negc = raw_mnemonic("negc"),
    negcv = raw_mnemonic("negcv"),

    csb = raw_mnemonic("csb"),
    csbl = raw_mnemonic("csbl"),
    csbt = raw_mnemonic("csbt"),
    ssb = raw_mnemonic("ssb"),
    ssbl = raw_mnemonic("ssbl"),
    ssbt = raw_mnemonic("ssbt"),

    czb = raw_mnemonic("czb"),
    czbl = raw_mnemonic("czbl"),
    czbt = raw_mnemonic("czbt"),
    szb = raw_mnemonic("szb"),
    szbl = raw_mnemonic("szbl"),
    szbt = raw_mnemonic("szbt"),

    sx = raw_mnemonic("sx"),
    sxv = raw_mnemonic("sxv"),
    zx = raw_mnemonic("zx"),
    zxv = raw_mnemonic("zxv"),

    @"xor" = raw_mnemonic("xor"),
    @"or" = raw_mnemonic("or"),
    @"and" = raw_mnemonic("and"),

    @"swap.16" = raw_mnemonic("swap.16"),
    @"swap.8" = raw_mnemonic("swap.8"),
    @"rev.8.32" = raw_mnemonic("rev.8.32"),
    @"rev.1.32" = raw_mnemonic("rev.1.32"),
    @"rev.1.16" = raw_mnemonic("rev.1.16"),
    @"rev.1.8" = raw_mnemonic("rev.1.8"),

    frame = raw_mnemonic("frame"),
    unframe = raw_mnemonic("unframe"),

    b = raw_mnemonic("b"),
    call = raw_mnemonic("call"),

    _,

    pub fn init(text: []const u8) Mnemonic {
        return @enumFromInt(raw_mnemonic(text));
    }

    pub fn name(self: *const Mnemonic) []const u8 {
        return std.mem.sliceTo(std.mem.asBytes(self), 0);
    }

    pub fn format(self: Mnemonic, writer: *std.io.Writer) !void {
        try writer.writeAll(self.name());
    }

    // if this returns true, the instruction will never cause the next instruction to execute; therefore it can be considered to end a block of code
    pub fn is_unconditional_branch(self: Mnemonic) bool {
        return switch (self) {
            .b, .ret, .iret, .fret, .freto, .park => true,
            else => false,
        };
    }

    // if this returns true, the instruction will never directly execute the next instruction, but it will move the address of the next instruction into the return pointer register, so it will/may be executed later.
    pub fn is_unconditional_call(self: Mnemonic) bool {
        return switch (self) {
            .call => true,
            else => false,
        };
    }
};

fn raw_mnemonic(text: []const u8) u128 {
    @setEvalBranchQuota(2000);
    var bytes: [16]u8 = @splat(0);
    const len = @min(text.len, bytes.len);
    for (bytes[0..len], text[0..len]) |*out, in| {
        const lower = std.ascii.toLower(in);
        out.* = switch (lower) {
            0 => break,
            'a'...'z', '0'...'9', '_', '.' => lower,
            else => '?',
        };
    }
    return std.mem.bytesToValue(u128, &bytes);
}

const std = @import("std");
