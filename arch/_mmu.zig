const ib = @import("instruction_builder.zig");
const cb = @import("cycle_builder.zig");
const ControlSignals = @import("ControlSignals");
const uc = @import("microcode");

const encodingWithSuffix = ib.encodingWithSuffix;
const desc = ib.desc;
const next_cycle = ib.next_cycle;
const kernel = ib.kernel;
const opcode = ib.opcode;

const zero_to_LH = cb.zero_to_LH;
const reg_to_LL = cb.reg_to_LL;
const reg32_to_L = cb.reg32_to_L;
const L_to_SR = cb.L_to_SR;
const load_next_insn = cb.load_next_insn;
const exec_next_insn = cb.exec_next_insn;
const illegal_instruction = cb.illegal_instruction;
const update_address_translation_from_L = cb.update_address_translation_from_L;
const invalidate_address_translation_from_L = cb.invalidate_address_translation_from_L;

pub fn _0018_001B() void {
    var mode: ControlSignals.BusMode = undefined;
    var dir: ControlSignals.BusDirection = undefined;
    switch (opcode() & 0x3) {
        0 => {
            mode = .data;
            dir = .write;
            encodingWithSuffix(.SAT, .W, .{});
            //syntax("SAT.W");
        },
        1 => {
            mode = .data;
            dir = .read;
            encodingWithSuffix(.SAT, .R, .{});
            //syntax("SAT.R");
        },
        2 => {
            mode = .stack;
            dir = .read;
            encodingWithSuffix(.SAT, .S, .{});
            //syntax("SAT.S");
        },
        3 => {
            mode = .insn;
            dir = .read;
            encodingWithSuffix(.SAT, .I, .{});
            //syntax("SAT.I");
        },
        else => unreachable,
    }
    desc("Set address translation entry corresponding to X4 with data in X0; discard secondary entry if necessary");

    if (!kernel()) {
        illegal_instruction();
        return;
    }

    reg32_to_L(4);
    L_to_SR(.temp_1);
    load_next_insn(2);
    next_cycle();

    update_address_translation_from_L(.temp_1, mode, dir);
    reg32_to_L(0);
    exec_next_insn();
}

pub fn _0008_000B() void {
    var mode: ControlSignals.BusMode = undefined;
    var dir: ControlSignals.BusDirection = undefined;
    switch (opcode() & 0x3) {
        0 => {
            mode = .data;
            dir = .write;
            encodingWithSuffix(.RAT, .W, .{});
            //syntax("RAT.W");
        },
        1 => {
            mode = .data;
            dir = .read;
            encodingWithSuffix(.RAT, .R, .{});
            //syntax("RAT.R");
        },
        2 => {
            mode = .stack;
            dir = .read;
            encodingWithSuffix(.RAT, .S, .{});
            //syntax("RAT.S");
        },
        3 => {
            mode = .insn;
            dir = .read;
            encodingWithSuffix(.RAT, .I, .{});
            //syntax("RAT.I");
        },
        else => unreachable,
    }
    desc("Remove address translation entry or entries corresponding to X4 and tag mask R0");

    if (!kernel()) {
        illegal_instruction();
        return;
    }

    reg32_to_L(4);
    L_to_SR(.temp_1);
    load_next_insn(2);
    next_cycle();

    invalidate_address_translation_from_L(.temp_1, mode, dir);
    reg_to_LL(0);
    zero_to_LH();
    exec_next_insn();
}
