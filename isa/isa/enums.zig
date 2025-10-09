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
    addc = raw_mnemonic("addc"),
    @"add.vf" = raw_mnemonic("add.vf"),
    @"addc.vf" = raw_mnemonic("addc.vf"),

    sub = raw_mnemonic("sub"),
    subc = raw_mnemonic("subc"),
    @"sub.vf" = raw_mnemonic("sub.vf"),
    @"subc.vf" = raw_mnemonic("subc.vf"),

    nadd = raw_mnemonic("nadd"),
    naddc = raw_mnemonic("naddc"),
    @"nadd.vf" = raw_mnemonic("nadd.vf"),
    @"naddc.vf" = raw_mnemonic("naddc.vf"),

    shl = raw_mnemonic("shl"),
    @"shl.vf" = raw_mnemonic("shl.vf"),
    shlc = raw_mnemonic("shlc"),

    shr = raw_mnemonic("shr"),
    shrc = raw_mnemonic("shrc"),
    shrs = raw_mnemonic("shrs"),
    @"shr.vf" = raw_mnemonic("shr.vf"),
    @"shrs.vf" = raw_mnemonic("shrs.vf"),

    inc = raw_mnemonic("inc"),
    dec = raw_mnemonic("dec"),
    @"inc.vf" = raw_mnemonic("inc.vf"),
    @"dec.vf" = raw_mnemonic("dec.vf"),
    cmp = raw_mnemonic("cmp"),
    cmpc = raw_mnemonic("cmpc"),

    neg = raw_mnemonic("neg"),
    negc = raw_mnemonic("negc"),
    @"neg.vf" = raw_mnemonic("neg.vf"),
    @"negc.vf" = raw_mnemonic("negc.vf"),

    csb = raw_mnemonic("csb"),
    ssb = raw_mnemonic("ssb"),
    @"csb.l" = raw_mnemonic("csb.l"),
    @"csb.t" = raw_mnemonic("csb.t"),
    @"ssb.l" = raw_mnemonic("ssb.l"),
    @"ssb.t" = raw_mnemonic("ssb.t"),

    czb = raw_mnemonic("czb"),
    szb = raw_mnemonic("szb"),
    @"czb.l" = raw_mnemonic("czb.l"),
    @"czb.t" = raw_mnemonic("czb.t"),
    @"szb.l" = raw_mnemonic("szb.l"),
    @"szb.t" = raw_mnemonic("szb.t"),

    sx = raw_mnemonic("sx"),
    zx = raw_mnemonic("zx"),
    @"sx.vf" = raw_mnemonic("sx.vf"),
    @"zx.vf" = raw_mnemonic("zx.vf"),

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
    @setEvalBranchQuota(5000);
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

pub const Symbolic_Register = enum (u64) {
    rsn = raw_symbolic_register("rsn"),
    rp = raw_symbolic_register("rp"),
    sp = raw_symbolic_register("sp"),
    bp = raw_symbolic_register("bp"),
    ip = raw_symbolic_register("ip"),
    asn = raw_symbolic_register("asn"),
    kxp = raw_symbolic_register("kxp"),
    uxp = raw_symbolic_register("uxp"),
    _,

    pub fn init(text: []const u8) Symbolic_Register {
        return @enumFromInt(raw_mnemonic(text));
    }

    pub fn name(self: *const Symbolic_Register) []const u8 {
        return std.mem.sliceTo(std.mem.asBytes(self), 0);
    }

    pub fn format(self: Symbolic_Register, writer: *std.io.Writer) !void {
        try writer.writeAll(self.name());
    }
};

fn raw_symbolic_register(text: []const u8) u64 {
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
