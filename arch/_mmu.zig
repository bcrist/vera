const ib = @import("instruction_builder.zig");
const cb = @import("cycle_builder.zig");
const ctrl = @import("control_signals");
const uc_layout = @import("microcode_layout");

const encodingWithSuffix = ib.encodingWithSuffix;
const desc = ib.desc;
const next_cycle = ib.next_cycle;
const kernel = ib.kernel;
const opcode = ib.opcode;

const reg_to_LL = cb.reg_to_LL;
const reg32_to_L = cb.reg32_to_L;
const L_to_SR = cb.L_to_SR;
const L_to_reg32 = cb.L_to_reg32;
const load_next_insn = cb.load_next_insn;
const exec_next_insn = cb.exec_next_insn;
const illegal_instruction = cb.illegal_instruction;

const address = cb.address;
const AT_OP = cb.AT_OP;
const BUS_MODE = cb.BUS_MODE;
const BUS_RW = cb.BUS_RW;
const BUS_BYTE = cb.BUS_BYTE;
const LH_SRC = cb.LH_SRC;
const LL_SRC = cb.LL_SRC;
const STAT_OP = cb.STAT_OP;

pub fn _000C_000F() void {
    // Note STAT.A should be clear when calling this to avoid triggering a fault or updating the entries that are read
    var mode: ctrl.Bus_Mode = undefined;
    var dir: ctrl.Bus_Direction = undefined;
    switch (opcode() & 0x3) {
        0 => {
            mode = .data;
            dir = .write;
            encodingWithSuffix(.CAT, .W, .{});
            //syntax("CAT.W");
        },
        1 => {
            mode = .data;
            dir = .read;
            encodingWithSuffix(.CAT, .R, .{});
            //syntax("CAT.R");
        },
        2 => {
            mode = .stack;
            dir = .read;
            encodingWithSuffix(.CAT, .S, .{});
            //syntax("CAT.S");
        },
        3 => {
            mode = .insn;
            dir = .read;
            encodingWithSuffix(.CAT, .I, .{});
            //syntax("CAT.I");
        },
        else => unreachable,
    }
    desc("Query address translation entries corresponding to X4; store in X0 and X2");

    if (!kernel()) {
        illegal_instruction();
        return;
    }

    reg32_to_L(4);
    L_to_SR(.temp_1);
    load_next_insn(2);
    next_cycle();

    address(.temp_1, 0);
    AT_OP(.translate);
    BUS_MODE(mode);
    BUS_RW(dir);
    BUS_BYTE(.word);
    LH_SRC(.AT_ME_H);
    LL_SRC(.AT_ME_L);
    L_to_reg32(0);
    STAT_OP(.ZN_from_address_translator);
    next_cycle();

    AT_OP(.hold);
    LH_SRC(.AT_OE_H);
    LL_SRC(.AT_OE_L);
    L_to_reg32(2);
    exec_next_insn();
}

pub fn _0010_0013() void {
    // Note STAT.A should be clear when calling this to avoid triggering a fault or updating the entries that are read
    var mode: ctrl.Bus_Mode = undefined;
    var dir: ctrl.Bus_Direction = undefined;
    switch (opcode() & 0x3) {
        0 => {
            mode = .data;
            dir = .write;
            encodingWithSuffix(.CATM, .W, .{});
            //syntax("CATM.W");
        },
        1 => {
            mode = .data;
            dir = .read;
            encodingWithSuffix(.CATM, .R, .{});
            //syntax("CATM.R");
        },
        2 => {
            mode = .stack;
            dir = .read;
            encodingWithSuffix(.CATM, .S, .{});
            //syntax("CATM.S");
        },
        3 => {
            mode = .insn;
            dir = .read;
            encodingWithSuffix(.CATM, .I, .{});
            //syntax("CATM.I");
        },
        else => unreachable,
    }
    desc("Query matching/primary address translation entry corresponding to X4; store in X0");

    if (!kernel()) {
        illegal_instruction();
        return;
    }

    reg32_to_L(4);
    L_to_SR(.temp_1);
    load_next_insn(2);
    next_cycle();

    address(.temp_1, 0);
    AT_OP(.translate);
    BUS_MODE(mode);
    BUS_RW(dir);
    BUS_BYTE(.word);
    LH_SRC(.AT_ME_H);
    LL_SRC(.AT_ME_L);
    L_to_reg32(0);
    exec_next_insn();
}

pub fn _0014_0017() void {
    // Note STAT.A should be clear when calling this to avoid triggering a fault or updating the entries that are read
    var mode: ctrl.Bus_Mode = undefined;
    var dir: ctrl.Bus_Direction = undefined;
    switch (opcode() & 0x3) {
        0 => {
            mode = .data;
            dir = .write;
            encodingWithSuffix(.CATO, .W, .{});
            //syntax("CATO.W");
        },
        1 => {
            mode = .data;
            dir = .read;
            encodingWithSuffix(.CATO, .R, .{});
            //syntax("CATO.R");
        },
        2 => {
            mode = .stack;
            dir = .read;
            encodingWithSuffix(.CATO, .S, .{});
            //syntax("CATO.S");
        },
        3 => {
            mode = .insn;
            dir = .read;
            encodingWithSuffix(.CATO, .I, .{});
            //syntax("CATO.I");
        },
        else => unreachable,
    }
    desc("Query other/secondary address translation entry corresponding to X4; store in X2");

    if (!kernel()) {
        illegal_instruction();
        return;
    }

    reg32_to_L(4);
    L_to_SR(.temp_1);
    load_next_insn(2);
    next_cycle();

    address(.temp_1, 0);
    AT_OP(.translate);
    BUS_MODE(mode);
    BUS_RW(dir);
    BUS_BYTE(.word);
    LH_SRC(.AT_OE_H);
    LL_SRC(.AT_OE_L);
    L_to_reg32(2);
    exec_next_insn();
}

pub fn _0018_001B() void {
    var mode: ctrl.Bus_Mode = undefined;
    var dir: ctrl.Bus_Direction = undefined;
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

    address(.temp_1, 0);
    AT_OP(.update);
    BUS_MODE(mode);
    BUS_RW(dir);
    BUS_BYTE(.word);
    reg32_to_L(0);
    exec_next_insn();
}

pub fn _001C_001F() void {
    var mode: ctrl.Bus_Mode = undefined;
    var dir: ctrl.Bus_Direction = undefined;
    switch (opcode() & 0x3) {
        0 => {
            mode = .data;
            dir = .write;
            encodingWithSuffix(.CSAT, .W, .{});
            //syntax("CSAT.W");
        },
        1 => {
            mode = .data;
            dir = .read;
            encodingWithSuffix(.CSAT, .R, .{});
            //syntax("CSAT.R");
        },
        2 => {
            mode = .stack;
            dir = .read;
            encodingWithSuffix(.CSAT, .S, .{});
            //syntax("CSAT.S");
        },
        3 => {
            mode = .insn;
            dir = .read;
            encodingWithSuffix(.CSAT, .I, .{});
            //syntax("CSAT.I");
        },
        else => unreachable,
    }
    desc("Set address translation entry corresponding to X4 with data in X0; load overwritten entry into X2");

    if (!kernel()) {
        illegal_instruction();
        return;
    }

    reg32_to_L(4);
    L_to_SR(.temp_1);
    load_next_insn(2);
    next_cycle();

    address(.temp_1, 0);
    AT_OP(.update);
    BUS_MODE(mode);
    BUS_RW(dir);
    BUS_BYTE(.word);
    reg32_to_L(0);
    STAT_OP(.ZN_from_address_translator);
    next_cycle();

    AT_OP(.hold);
    LH_SRC(.AT_ME_H);
    LL_SRC(.AT_ME_L);
    L_to_reg32(2);
    exec_next_insn();
}

pub fn _0008_000B() void {
    var mode: ctrl.Bus_Mode = undefined;
    var dir: ctrl.Bus_Direction = undefined;
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
    desc("Remove address translation entry corresponding to X4 and tag mask R0");

    if (!kernel()) {
        illegal_instruction();
        return;
    }

    reg32_to_L(4);
    L_to_SR(.temp_1);
    load_next_insn(2);
    next_cycle();

    address(.temp_1, 0);
    AT_OP(.invalidate);
    BUS_MODE(mode);
    BUS_RW(dir);
    BUS_BYTE(.word);
    reg_to_LL(0);
    STAT_OP(.ZN_from_address_translator);
    next_cycle();

    AT_OP(.hold);
    LH_SRC(.AT_ME_H);
    LL_SRC(.AT_ME_L);
    L_to_reg32(2);
    exec_next_insn();
}
