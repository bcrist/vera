const assert = @import("std").debug.assert;
const ib = @import("instruction_builder.zig");
const cb = @import("cycle_builder.zig");
const ControlSignals = @import("ControlSignals");
const misc = @import("misc");
const uc = @import("microcode");

const Xa_relative = ib.Xa_relative;
const encoding = ib.encoding;
const desc = ib.desc;
const next_cycle = ib.next_cycle;
const next_cycle_force_normal_execution = ib.next_cycle_force_normal_execution;
const fault_return = ib.fault_return;
const uc_address = ib.uc_address;
const kernel = ib.kernel;

const SR_plus_literal_to_L = cb.SR_plus_literal_to_L;
const last_translation_info_to_L = cb.last_translation_info_to_L;
const op_reg32_to_L = cb.op_reg32_to_L;
const SR1_to_L = cb.SR1_to_L;
const OB_OA_to_LL = cb.OB_OA_to_LL;
const STAT_to_LL = cb.STAT_to_LL;
const DL_to_LL = cb.DL_to_LL;
const JH_to_LH = cb.JH_to_LH;
const JH_to_LL = cb.JH_to_LL;
const JL_to_LL = cb.JL_to_LL;
const SRH_to_LL = cb.SRH_to_LL;
const op_reg_to_LL = cb.op_reg_to_LL;
const L_to_SR = cb.L_to_SR;
const L_to_SR1 = cb.L_to_SR1;
const L_to_SR2 = cb.L_to_SR2;
const L_to_reg32 = cb.L_to_reg32;
const LL_to_STAT = cb.LL_to_STAT;
const LL_to_D = cb.LL_to_D;
const LL_to_RSN = cb.LL_to_RSN;
const LL_to_op_reg = cb.LL_to_op_reg;
const SR1H_to_LH = cb.SR1H_to_LH;
const SR1H_to_LL = cb.SR1H_to_LL;
const SR2H_to_LL = cb.SR2H_to_LL;
const SR1L_to_LL = cb.SR1L_to_LL;
const SR2L_to_LL = cb.SR2L_to_LL;
const SR1_to_J = cb.SR1_to_J;
const SR2_to_J = cb.SR2_to_J;
const SR2_to_SR2 = cb.SR2_to_SR2;
const read_to_D = cb.read_to_D;
const write_from_LL = cb.write_from_LL;
const D_to_L = cb.D_to_L;
const D_to_LL = cb.D_to_LL;
const D_to_LH = cb.D_to_LH;
const D_to_DL = cb.D_to_DL;
const D_to_OB_OA = cb.D_to_OB_OA;
const reload_ASN = cb.reload_ASN;
const toggle_rsn = cb.toggle_rsn;
const RSN_to_SR1H = cb.RSN_to_SR1H;
const branch = cb.branch;
const illegal_instruction = cb.illegal_instruction;
const load_and_exec_next_insn = cb.load_and_exec_next_insn;
const atomic_next_cycle_until_end = cb.atomic_next_cycle_until_end;
const clear_OB = cb.clear_OB;
const increment_OB = cb.increment_OB;
const prev_UA_to_LH = cb.prev_UA_to_LH;

fn vectored_fault_handler(zeropage_vector: u5) void {
    // Store the UC address of the faulted cycle and the contents of DL into SR1:
    prev_UA_to_LH();
    DL_to_LL();
    L_to_SR1(.fault_ua_dl);
    next_cycle();

    // Store STAT into SR1 (without disturbing high byte):
    SR1H_to_LH(.fault_rsn_stat);
    STAT_to_LL();
    L_to_SR1(.fault_rsn_stat);
    next_cycle();

    // Store OB/OA into SR1 (without disturbing high byte):
    SR1H_to_LH(.int_rsn_fault_ob_oa);
    OB_OA_to_LL();
    L_to_SR1(.int_rsn_fault_ob_oa);
    next_cycle();

    // Switch to fault registerset:
    toggle_rsn();
    // Copy data about last MMU op into X4
    // For page faults, this is critical for knowing what page to load
    last_translation_info_to_L();
    L_to_reg32(4);
    next_cycle();

    // May need to update ASN after changing RSN
    reload_ASN();
    // Read the fault vector
    read_to_D(.zero, zeropage_vector, .word, .raw);
    D_to_L(.zx);
    L_to_SR(.temp_1);
    next_cycle();

    // Begin executing handler
    branch(.temp_1, 0);
}

pub fn _handler_1() void {
    // page fault
    //desc("Triggered when address translation is performed but there is no matching entry");
    assert(uc_address() == @enumToInt(uc.Vectors.page_fault));
    vectored_fault_handler(@intCast(u5, @offsetOf(misc.ZeropageVectorTable, "page_fault")));
}

pub fn _handler_2() void {
    // access fault
    //desc("Triggered when address translation is performed in user-mode, and the matching entry does not have the user flag set");
    assert(uc_address() == @enumToInt(uc.Vectors.access_fault));
    vectored_fault_handler(@intCast(u5, @offsetOf(misc.ZeropageVectorTable, "access_fault")));
}

pub fn _handler_3() void {
    // page align fault
    //desc("Triggered when reading or writing a word that stradles the boundary between two 4KB pages");
    assert(uc_address() == @enumToInt(uc.Vectors.page_align_fault));
    vectored_fault_handler(@intCast(u5, @offsetOf(misc.ZeropageVectorTable, "page_align_fault")));
}

pub fn _handler_4() void {
    // instruction protection fault
    //desc("Triggered when attempting to execute a kernel-only instruction in user mode");
    assert(uc_address() == @enumToInt(uc.Vectors.instruction_protection_fault));
    vectored_fault_handler(@intCast(u5, @offsetOf(misc.ZeropageVectorTable, "instruction_protection_fault")));
}

pub fn _handler_5() void {
    // invalid instruction
    //desc("Triggered when attempting to execute an opcode that does not correspond to a valid instruction");
    assert(uc_address() == @enumToInt(uc.Vectors.invalid_instruction));
    vectored_fault_handler(@intCast(u5, @offsetOf(misc.ZeropageVectorTable, "invalid_instruction")));
}

pub fn _handler_6() void {
    // double fault
    //desc("Triggered when a fault occurs, but exec_state is already .fault or .interrupt_fault");
    assert(uc_address() == @enumToInt(uc.Vectors.double_fault));
    vectored_fault_handler(@intCast(u5, @offsetOf(misc.ZeropageVectorTable, "double_fault")));
}

pub fn _018D() void {
    encoding(.FRET, .{});
    desc("Return from fault handler and retry operation");

    if (!kernel()) {
        illegal_instruction();
        return;
    }

    toggle_rsn();
    next_cycle();

    reload_ASN();
    SR1_to_L(.fault_rsn_stat);
    LL_to_STAT();
    next_cycle();

    SR1_to_L(.int_rsn_fault_ob_oa);
    LL_to_D();
    D_to_OB_OA();
    next_cycle();

    SR1_to_L(.fault_ua_dl);
    LL_to_D();
    D_to_DL();
    // If a page fault occurs during an instruction preceeded by SYNC,
    // we want to make sure it's still atomic when we retry.
    // It doesn't hurt to "upgrade" a non-atomic instruction, so we
    // just assume all faults happen during atomic instructions
    atomic_next_cycle_until_end();
    fault_return();
}

pub fn _018E() void {
    encoding(.IFEX, .{});
    desc("Exit interrupt or fault handler without changing registersets or retrying the faulted operation");

    if (!kernel()) {
        illegal_instruction();
        return;
    }

    next_cycle_force_normal_execution();

    load_and_exec_next_insn(2);
}

pub fn _FD00_FDFF() void {
    encoding(.LDRS, .{ Xa_relative(.D, .imm_0), .to, .Rb });
    desc("Load registerset");

    if (!kernel()) {
        illegal_instruction();
        return;
    }

    // Move the base address into SR2 so that we can move it to .RS_reserved without using L:
    op_reg32_to_L(.OA);
    L_to_SR2(.temp_2);
    next_cycle();

    // switch RSN, copy base address to .RS_reserved:
    RSN_to_SR1H(.fault_rsn_stat);
    op_reg_to_LL(.OB);
    LL_to_RSN();
    SR2_to_SR2(.temp_2, .rs_reserved);
    clear_OB();
    next_cycle();

    // Load the GPR data
    var r: u5 = 0;
    while (r < 16) : (r += 1) {
        read_to_D(.rs_reserved, r * 2, .word, .data);
        D_to_LL();
        LL_to_op_reg(.OB);
        increment_OB();
        next_cycle();
    }

    // offset .RS_reserved to avoid overflowing the literal offset:
    SR_plus_literal_to_L(.rs_reserved, 32, .fresh, .no_flags);
    L_to_SR(.rs_reserved);
    next_cycle();

    // Load the SR data
    inline for ([_][]const u8{
        "rp", "sp", "bp", "fault_ua_dl", "fault_rsn_stat", "int_rsn_fault_ob_oa",
        "ip", "next_ip", "asn", "kxp", "uxp", "temp_2", "temp_1",
    }) |reg| {
        const offset = @offsetOf(misc.RegistersetState, reg) - 32;

        read_to_D(.rs_reserved, offset, .word, .data);
        D_to_L(.zx);
        L_to_SR1(.temp_1);
        next_cycle();

        read_to_D(.rs_reserved, offset + 2, .word, .data);
        SR1L_to_LL(.temp_1);
        D_to_LH();
        if (@hasField(ControlSignals.SR1Index, reg)) {
            L_to_SR1(@field(ControlSignals.SR1Index, reg));
        } else {
            L_to_SR2(@field(ControlSignals.SR2Index, reg));
        }
        next_cycle();
    }

    // Restore RSN from SR1.fault_rsn_stat
    SRH_to_LL(.fault_rsn_stat);
    LL_to_RSN();
    next_cycle();

    load_and_exec_next_insn(2);
}

pub fn _FE00_FEFF() void {
    encoding(.STRS, .{ .Rb, .to, Xa_relative(.D, .imm_0) });
    desc("Store registerset");

    if (!kernel()) {
        illegal_instruction();
        return;
    }

    // Move the base address into SR2 so that we can move it to .RS_reserved without using L:
    op_reg32_to_L(.OA);
    L_to_SR2(.temp_2);
    next_cycle();

    // switch RSN, copy base address to .RS_reserved:
    RSN_to_SR1H(.fault_rsn_stat);
    op_reg_to_LL(.OB);
    LL_to_RSN();
    SR2_to_SR2(.temp_2, .rs_reserved);
    clear_OB();
    next_cycle();

    // Store the GPR data
    var r: u5 = 0;
    while (r < 16) : (r += 1) {
        op_reg_to_LL(.OB);
        write_from_LL(.rs_reserved, r * 2, .word, .data);
        increment_OB();
        next_cycle();
    }

    // offset .rs_reserved to avoid overflowing the literal offset:
    SR_plus_literal_to_L(.rs_reserved, 32, .fresh, .no_flags);
    L_to_SR(.rs_reserved);
    next_cycle();

    // Store the SR data
    inline for ([_][]const u8{
        "rp", "sp", "bp", "fault_ua_dl", "fault_rsn_stat", "int_rsn_fault_ob_oa",
        "ip", "next_ip", "asn", "kxp", "uxp", "temp_2", "temp_1",
    }) |reg| {
        const offset = @offsetOf(misc.RegistersetState, reg) - 32;

        if (@hasField(ControlSignals.SR1Index, reg)) {
            SR1L_to_LL(@field(ControlSignals.SR1Index, reg));
        } else {
            SR2L_to_LL(@field(ControlSignals.SR2Index, reg));
        }
        write_from_LL(.rs_reserved, offset, .word, .data);
        next_cycle();

        if (@hasField(ControlSignals.SR1Index, reg)) {
            SR1H_to_LL(@field(ControlSignals.SR1Index, reg));
        } else {
            SR2H_to_LL(@field(ControlSignals.SR2Index, reg));
        }
        write_from_LL(.rs_reserved, offset + 2, .word, .data);
        next_cycle();
    }

    // Restore RSN from SR1.fault_rsn_stat
    SRH_to_LL(.fault_rsn_stat);
    LL_to_RSN();
    next_cycle();

    load_and_exec_next_insn(2);
}

pub fn _FA00_FA0F() void {
    encoding(.SRS, .{ .Ra });
    desc("Switch to registerset");

    if (!kernel()) {
        illegal_instruction();
        return;
    }

    // Increment IP so that if/when someone switches back to this registerset,
    // this instruction doesn't get executed a second time.
    SR_plus_literal_to_L(.ip, 2, .fresh, .no_flags);
    L_to_SR(.ip);
    next_cycle();

    // switch RSN, copy base address to .RS_reserved:
    op_reg_to_LL(.OA);
    LL_to_RSN();
    next_cycle();

    // Address space may have changed when we switched RSN:
    reload_ASN();
    next_cycle();

    // Assume IP points to the first instruction that we want to execute in the new registerset:
    branch(.ip, 0);
}
