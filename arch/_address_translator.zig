const ib = @import("instruction_builder.zig");
const cb = @import("cycle_builder.zig");
const ControlSignals = @import("ControlSignals");
const uc = @import("microcode");

const encodingWithSuffix = ib.encodingWithSuffix;
const desc = ib.desc;
const next_cycle = ib.next_cycle;
const kernel = ib.kernel;
const opcode = ib.opcode;
const X0_relative = ib.X0_relative;

const zero_to_LH = cb.zero_to_LH;
const reg_to_LL = cb.reg_to_LL;
const op_reg_to_LL = cb.op_reg_to_LL;
const reg32_to_L = cb.reg32_to_L;
const op_reg32_to_L = cb.op_reg32_to_L;
const L_to_SR = cb.L_to_SR;
const load_next_insn = cb.load_next_insn;
const exec_next_insn = cb.exec_next_insn;
const illegal_instruction = cb.illegal_instruction;
const update_address_translation_from_L = cb.update_address_translation_from_L;
const invalidate_address_translation_from_L = cb.invalidate_address_translation_from_L;

pub fn _1F00_1F3F() void {
    var mode: ControlSignals.BusMode = undefined;
    var dir: ControlSignals.BusDirection = undefined;
    switch (opcode() & 0xF0) {
        0x00 => {
            mode = .data;
            dir = .write;
            encodingWithSuffix(.SAT, .W, .{ .Xa, .to, X0_relative(.D, .imm_0) });
            //syntax("SAT.W");
        },
        0x10 => {
            mode = .data;
            dir = .read;
            encodingWithSuffix(.SAT, .R, .{ .Xa, .to, X0_relative(.D, .imm_0) });
            //syntax("SAT.R");
        },
        0x20 => {
            mode = .stack;
            dir = .read;
            encodingWithSuffix(.SAT, .S, .{ .Xa, .to, X0_relative(.S, .imm_0) });
            //syntax("SAT.S");
        },
        0x30 => {
            mode = .insn;
            dir = .read;
            encodingWithSuffix(.SAT, .I, .{ .Xa, .to, X0_relative(.I, .imm_0) });
            //syntax("SAT.I");
        },
        else => unreachable,
    }
    desc("Set address translation entry corresponding to X0 with data in Xa; discard secondary entry if necessary");

    if (!kernel()) {
        illegal_instruction();
        return;
    }

    reg32_to_L(0);
    L_to_SR(.temp_1);
    load_next_insn(2);
    next_cycle();

    op_reg32_to_L(.OA);
    update_address_translation_from_L(.temp_1, mode, dir);
    exec_next_insn();
}

pub fn _alias_1F00_1F3F_SAT() void {
    switch (opcode() & 0xF0) {
        0x00 => encodingWithSuffix(.SAT, .W, .{ .Xa }),
        0x10 => encodingWithSuffix(.SAT, .R, .{ .Xa }),
        0x20 => encodingWithSuffix(.SAT, .S, .{ .Xa }),
        0x30 => encodingWithSuffix(.SAT, .I, .{ .Xa }),
        else => unreachable,
    }
    desc("Set address translation entry corresponding to X0 with data in Xa; discard secondary entry if necessary");
}

pub fn _1F40_1F7F() void {
    var mode: ControlSignals.BusMode = undefined;
    var dir: ControlSignals.BusDirection = undefined;
    switch (opcode() & 0xF0) {
        0x40 => {
            mode = .data;
            dir = .write;
            encodingWithSuffix(.RAT, .W, .{ .Ra });
            //syntax("RAT.W");
        },
        0x50 => {
            mode = .data;
            dir = .read;
            encodingWithSuffix(.RAT, .R, .{ .Ra });
            //syntax("RAT.R");
        },
        0x60 => {
            mode = .stack;
            dir = .read;
            encodingWithSuffix(.RAT, .S, .{ .Ra });
            //syntax("RAT.S");
        },
        0x70 => {
            mode = .insn;
            dir = .read;
            encodingWithSuffix(.RAT, .I, .{ .Ra });
            //syntax("RAT.I");
        },
        else => unreachable,
    }
    desc("Remove address translation entry or entries corresponding to X0 with tag mask Ra");

    if (!kernel()) {
        illegal_instruction();
        return;
    }

    reg32_to_L(0);
    L_to_SR(.temp_1);
    load_next_insn(2);
    next_cycle();

    zero_to_LH();
    op_reg_to_LL(.OA);
    invalidate_address_translation_from_L(.temp_1, mode, dir);
    exec_next_insn();
}
