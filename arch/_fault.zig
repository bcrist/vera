const assert = @import("std").debug.assert;
const ib = @import("instruction_builder.zig");
const cb = @import("cycle_builder.zig");
const ctrl = @import("control_signals");
const misc = @import("misc");
const uc_layout = @import("microcode_layout");

const Xa_relative = ib.Xa_relative;
const encoding = ib.encoding;
const desc = ib.desc;
const next_cycle = ib.next_cycle;
const uc_address = ib.uc_address;
const kernel = ib.kernel;

const SR_plus_literal_to_L = cb.SR_plus_literal_to_L;
const last_mmu_op_to_L = cb.last_mmu_op_to_L;
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
const SR1H_to_JH = cb.SR1H_to_JH;
const SR1_to_J = cb.SR1_to_J;
const SR2_to_J = cb.SR2_to_J;
const SR2_to_SR2 = cb.SR2_to_SR2;
const read_to_D = cb.read_to_D;
const write_from_LL = cb.write_from_LL;
const D_to_L = cb.D_to_L;
const D_to_LH = cb.D_to_LH;
const D_to_DL = cb.D_to_DL;
const D_to_OB_OA = cb.D_to_OB_OA;
const reload_ASN = cb.reload_ASN;
const toggle_RSN = cb.toggle_RSN;
const RSN_to_SR1H = cb.RSN_to_SR1H;
const branch = cb.branch;

const LH_SRC = cb.LH_SRC;
const SPECIAL = cb.SPECIAL;
const SEQ_OP = cb.SEQ_OP;
const OB_OA_OP = cb.OB_OA_OP;
const SR1_WI = cb.SR1_WI;

const illegal_instruction = cb.illegal_instruction;
const load_and_exec_next_insn = cb.load_and_exec_next_insn;

fn vectored_fault_handler(zeropage_vector: u5) void {
    // Store the UC address of the faulted cycle and the contents of DL into SR1:
    LH_SRC(.prev_UA);
    DL_to_LL();
    L_to_SR1(.fault_UA_DL);
    next_cycle();

    // Store STAT into SR1 (without disturbing high byte):
    SR1H_to_JH(.fault_RSN_STAT);
    JH_to_LH();
    STAT_to_LL();
    L_to_SR1(.fault_RSN_STAT);
    next_cycle();

    // Store OB/OA into SR1 (without disturbing high byte):
    SR1H_to_JH(.int_RSN_fault_OB_OA);
    JH_to_LH();
    OB_OA_to_LL();
    L_to_SR1(.int_RSN_fault_OB_OA);
    next_cycle();

    // Switch to fault registerset:
    toggle_RSN();
    // Copy data about last MMU op into X4
    // For page faults, this is critical for knowing what page to load
    last_mmu_op_to_L();
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
    assert(uc_address() == @enumToInt(uc_layout.UC_Vectors.page_fault));
    vectored_fault_handler(@intCast(u5, @offsetOf(misc.Zeropage_Vector_Table, "page_fault")));
}

pub fn _handler_2() void {
    // access fault
    //desc("Triggered when address translation is performed in user-mode, and the matching entry does not have the user flag set");
    assert(uc_address() == @enumToInt(uc_layout.UC_Vectors.access_fault));
    vectored_fault_handler(@intCast(u5, @offsetOf(misc.Zeropage_Vector_Table, "access_fault")));
}

pub fn _handler_3() void {
    // page align fault
    //desc("Triggered when reading or writing a word that stradles the boundary between two 4KB pages");
    assert(uc_address() == @enumToInt(uc_layout.UC_Vectors.page_align_fault));
    vectored_fault_handler(@intCast(u5, @offsetOf(misc.Zeropage_Vector_Table, "page_align_fault")));
}

pub fn _handler_4() void {
    // instruction protection fault
    //desc("Triggered when attempting to execute a kernel-only instruction in user mode");
    assert(uc_address() == @enumToInt(uc_layout.UC_Vectors.instruction_protection_fault));
    vectored_fault_handler(@intCast(u5, @offsetOf(misc.Zeropage_Vector_Table, "instruction_protection_fault")));
}

pub fn _handler_5() void {
    // invalid instruction
    //desc("Triggered when attempting to execute an opcode that does not correspond to a valid instruction");
    assert(uc_address() == @enumToInt(uc_layout.UC_Vectors.invalid_instruction));
    vectored_fault_handler(@intCast(u5, @offsetOf(misc.Zeropage_Vector_Table, "invalid_instruction")));
}

pub fn _handler_6() void {
    // double fault
    //desc("Triggered when a fault occurs, but exec_state is already .fault or .interrupt_fault");
    assert(uc_address() == @enumToInt(uc_layout.UC_Vectors.double_fault));
    vectored_fault_handler(@intCast(u5, @offsetOf(misc.Zeropage_Vector_Table, "double_fault")));
}

pub fn _018D() void {
    encoding(.FRET, .{});
    desc("Return from fault handler and retry operation");

    if (!kernel()) {
        illegal_instruction();
        return;
    }

    toggle_RSN();
    next_cycle();

    reload_ASN();
    SR1_to_L(.fault_RSN_STAT);
    LL_to_STAT();
    next_cycle();

    SR1_to_L(.int_RSN_fault_OB_OA);
    LL_to_D();
    D_to_OB_OA();
    next_cycle();

    SR1_to_L(.fault_UA_DL);
    LL_to_D();
    D_to_DL();
    // If a page fault occurs during an instruction preceeded by SYNC,
    // we want to make sure it's still atomic when we retry.
    // It doesn't hurt to "upgrade" a non-atomic instruction, so we
    // just assume all faults happen during atomic instructions
    SPECIAL(.atomic_next);
    // Note .fault_return requires the faulted microcode address be asserted on LH, which we did above.
    SEQ_OP(.fault_return);
}

pub fn _018E() void {
    encoding(.IFEX, .{});
    desc("Exit interrupt or fault handler without changing registersets or retrying the faulted operation");

    if (!kernel()) {
        illegal_instruction();
        return;
    }

    SEQ_OP(.next_uop_force_normal);
    next_cycle();

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
    RSN_to_SR1H(.fault_RSN_STAT);
    op_reg_to_LL(.OB);
    LL_to_RSN();
    SR2_to_SR2(.temp_2, .RS_reserved);
    OB_OA_OP(.clear_OB);
    next_cycle();

    // Load the GPR data
    var r: u5 = 0;
    while (r < 16) : (r += 1) {
        read_to_D(.RS_reserved, r * 2, .word, .data);
        D_to_L(.zx);
        LL_to_op_reg(.OB);
        OB_OA_OP(.increment_OB);
        next_cycle();
    }

    // offset .RS_reserved to avoid overflowing the literal offset:
    SR_plus_literal_to_L(.RS_reserved, 32, .fresh, .no_flags);
    L_to_SR(.RS_reserved);
    next_cycle();

    // Load the SR data
    inline for ([_][]const u8{
        "RP", "SP", "BP", "fault_UA_DL", "fault_RSN_STAT", "int_RSN_fault_OB_OA",
        "IP", "next_IP", "ASN", "KXP", "UXP", "temp_2", "temp_1",
    }) |reg| {
        const offset = @offsetOf(misc.Registerset_State, reg) - 32;

        read_to_D(.RS_reserved, offset, .word, .data);
        D_to_L(.zx);
        L_to_SR1(.temp_1);
        next_cycle();

        read_to_D(.RS_reserved, offset + 2, .word, .data);
        SR1_to_J(.temp_1);
        JL_to_LL();
        D_to_LH();
        if (@hasField(ctrl.SR1_Index, reg)) {
            L_to_SR1(@field(ctrl.SR1_Index, reg));
        } else {
            L_to_SR2(@field(ctrl.SR2_Index, reg));
        }
        next_cycle();
    }

    // Restore RSN from SR1.fault_RSN_STAT
    SRH_to_LL(.fault_RSN_STAT);
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
    RSN_to_SR1H(.fault_RSN_STAT);
    op_reg_to_LL(.OB);
    LL_to_RSN();
    SR2_to_SR2(.temp_2, .RS_reserved);
    OB_OA_OP(.clear_OB);
    next_cycle();

    // Store the GPR data
    var r: u5 = 0;
    while (r < 16) : (r += 1) {
        op_reg_to_LL(.OB);
        write_from_LL(.RS_reserved, r * 2, .word, .data);
        OB_OA_OP(.increment_OB);
        next_cycle();
    }

    // offset .RS_reserved to avoid overflowing the literal offset:
    SR_plus_literal_to_L(.RS_reserved, 32, .fresh, .no_flags);
    L_to_SR(.RS_reserved);
    next_cycle();

    // Store the SR data
    inline for ([_][]const u8{
        "RP", "SP", "BP", "fault_UA_DL", "fault_RSN_STAT", "int_RSN_fault_OB_OA",
        "IP", "next_IP", "ASN", "KXP", "UXP", "temp_2", "temp_1",
    }) |reg| {
        const offset = @offsetOf(misc.Registerset_State, reg) - 32;

        if (@hasField(ctrl.SR1_Index, reg)) {
            SR1_to_J(@field(ctrl.SR1_Index, reg));
        } else {
            SR2_to_J(@field(ctrl.SR2_Index, reg));
        }
        JL_to_LL();
        write_from_LL(.RS_reserved, offset, .word, .data);
        next_cycle();

        if (@hasField(ctrl.SR1_Index, reg)) {
            SR1_to_J(@field(ctrl.SR1_Index, reg));
        } else {
            SR2_to_J(@field(ctrl.SR2_Index, reg));
        }
        JH_to_LL();
        write_from_LL(.RS_reserved, offset + 2, .word, .data);
        next_cycle();
    }

    // Restore RSN from SR1.fault_RSN_STAT
    SRH_to_LL(.fault_RSN_STAT);
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
    SR_plus_literal_to_L(.IP, 2, .fresh, .no_flags);
    L_to_SR(.IP);
    next_cycle();

    // switch RSN, copy base address to .RS_reserved:
    op_reg_to_LL(.OA);
    LL_to_RSN();
    next_cycle();

    // Address space may have changed when we switched RSN:
    reload_ASN();
    next_cycle();

    // Assume IP points to the first instruction that we want to execute in the new registerset:
    branch(.IP, 0);
}
