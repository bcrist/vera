const std = @import("std");
const bus = @import("bus_types");
const bits = @import("bits");
const ControlSignals = @import("ControlSignals");

pub const Opcode = u16;
pub const RegisterIndex = u4; // for GPRs
pub const SignedOffsetForLiteral = i7; // note not all possible values are valid
pub const RegistersetNumber = u6; // aka RSN
pub const CombinedOperands = u8; // OperandB in high bits, OperandA in low bits
pub const OperandB = u4;
pub const OperandA = u4;

pub const PipeID = enum(u2) {
    zero = 0,
    one = 1,
    two = 2,

    pub fn next(self: PipeID) PipeID {
        return switch (self) {
            .zero => .one,
            .one => .two,
            .two => .zero,
        };
    }
};

pub const ExecutionMode = enum(u2) {
    normal = 0,
    interrupt = 1,
    fault = 2,
    interrupt_fault = 3,
};

pub const PowerMode = enum(u1) {
    run = 0,
    sleep = 1,
};

pub const StatusBits = packed struct(bus.LLow) {
    z: bool,
    n: bool,
    v: bool,
    c: bool,
    k: bool,
    a: bool,
    pwr: PowerMode,
    pipe: PipeID,
    mode: ExecutionMode,
    unused: u5 = 0,
};

pub const RegistersetState = extern struct {
    registers: [16]u16,
    rp: u32,
    sp: u32,
    bp: u32,
    fault_ua_dl: u32,
    fault_rsn_stat: u32,
    int_rsn_fault_ob_oa: u32,
    temp_1: u32,
    ip: u32,
    next_ip: u32,
    asn: u32,
    kxp: u32,
    uxp: u32,
    temp_2: u32,
};

pub const ZeropageVectorTable = extern struct {
    double_fault: u16,
    page_fault: u16,
    access_fault: u16,
    page_align_fault: u16,
    instruction_protection_fault: u16,
    invalid_instruction: u16,
    pipe_0_reset: u16,
};

pub const microcode_length = 0x10000;

pub fn decodeSpecialKLiteral(encoded: ControlSignals.Literal) bus.K {
    const l4 = @truncate(u4, encoded);
    return switch (@truncate(u2, encoded >> 4)) {
        0 => switch (l4) {
            6  => 0x0040,
            7  => 0x0080,
            8  => 0x0100,
            9  => 0x0200,
            10 => 0x0400,
            11 => 0x0800,
            else => 0x0000,
        },
        1 => bits.concat(.{ l4, l4, @as(u8, 0) }),
        2 => bits.concat(.{ @as(u8, 0), l4, l4 }),
        3 => bits.concat2(@as(u12, 0), l4),
    };
}

pub fn encodeSpecialKLiteral(literal: u17) ?ControlSignals.Literal {
    if (0 == (literal & 0x10FFF)) {
        return bits.concat2(@truncate(u4, literal >> 12), @as(u2, 3));
    } else if (0 == (literal & 0x100FF) and @truncate(u4, literal >> 8) == @truncate(u4, literal >> 12)) {
        return bits.concat2(@truncate(u4, literal >> 8), @as(u2, 2));
    } else if (0 == (literal & 0x1FF00) and @truncate(u4, literal) == @truncate(u4, literal >> 4)) {
        return bits.concat2(@truncate(u4, literal), @as(u2, 1));
    } else if (0 == (literal & 0x1F03F) and @popCount(literal) == 1) {
        return bits.concat2(@intCast(u4, @ctz(literal)), @as(u2, 0));
    } else return null;
}
