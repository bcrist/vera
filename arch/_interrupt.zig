const assert = @import("std").debug.assert;
const ib = @import("instruction_builder.zig");
const cb = @import("cycle_builder.zig");
const ctrl = @import("control_signals");
const misc = @import("misc");
const uc_layout = @import("microcode_layout");

const encoding = ib.encoding;
const desc = ib.desc;
const next_cycle = ib.next_cycle;
const kernel = ib.kernel;
const uc_address = ib.uc_address;

const zero_to_LL = cb.zero_to_LL;
const pipe_id_to_LL = cb.pipe_id_to_LL;
const STAT_to_LL = cb.STAT_to_LL;
const JH_to_LL = cb.JH_to_LL;
const SRL_to_LL = cb.SRL_to_LL;
const JH_to_LH = cb.JH_to_LH;
const literal_to_LH = cb.literal_to_LH;
const SR1_to_J = cb.SR1_to_J;
const LL_to_STAT = cb.LL_to_STAT;
const LL_to_RSN = cb.LL_to_RSN;
const L_to_SR = cb.L_to_SR;
const L_to_SR1 = cb.L_to_SR1;
const L_to_SR2 = cb.L_to_SR2;
const RSN_to_SR1H = cb.RSN_to_SR1H;
const reload_ASN = cb.reload_ASN;
const read_to_D = cb.read_to_D;
const D_to_L = cb.D_to_L;
const branch = cb.branch;
const illegal_instruction = cb.illegal_instruction;
const SEQ_OP = cb.SEQ_OP;

pub fn _handler_7() void {
    //syntax("(interrupt)");
    //desc("Interrupt handler");
    assert(uc_address() == @enumToInt(uc_layout.UC_Vectors.interrupt));
    const vector_register = misc.frameToPhysicalAddress(@enumToInt(misc.Device_Frames.sys_interrupt_controller));

    // Persist STAT for when we return
    SR1_to_J(.fault_RSN_STAT);
    JH_to_LH();
    STAT_to_LL();
    L_to_SR1(.fault_RSN_STAT);
    next_cycle();

    // Switch to RSN 0/1/2 depending on which pipe we're in, storing the old RSN in SR1
    RSN_to_SR1H(.int_RSN_fault_OB_OA);
    pipe_id_to_LL();
    LL_to_RSN();
    next_cycle();

    // Reload ASN since it may have changed, and calculate the base address of the interrupt controller
    reload_ASN();
    literal_to_LH(@intCast(i17, vector_register >> 16));
    zero_to_LL();
    L_to_SR(.temp_1);
    next_cycle();

    // Read the interrupt vector from the interrupt controller
    read_to_D(.temp_1, @intCast(i7, vector_register & 0xFFFF), .word, .raw);
    D_to_L(.zx);
    L_to_SR2(.next_IP);
    next_cycle();

    // Start executing the interrupt handler
    branch(.next_IP, 0);
}

pub fn _018C() void {
    encoding(.IRET, .{});
    //syntax("IRET");
    desc("Return from interrupt");

    if (!kernel()) {
        illegal_instruction();
        return;
    }

    SR1_to_J(.int_RSN_fault_OB_OA);
    JH_to_LL();
    LL_to_RSN();
    next_cycle();

    reload_ASN();
    SRL_to_LL(.fault_RSN_STAT);
    LL_to_STAT();
    SEQ_OP(.next_uop_force_normal);
    next_cycle();

    branch(.IP, 0);
}
