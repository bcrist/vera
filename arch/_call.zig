const ib = @import("instruction_builder.zig");
const cb = @import("cycle_builder.zig");

const encoding = ib.encoding;
const IP_relative = ib.IP_relative;
const Xa_relative = ib.Xa_relative;
const desc = ib.desc;
const next_cycle = ib.next_cycle;

const IP_read_to_D = cb.IP_read_to_D;
const read_to_D = cb.read_to_D;
const JL_to_LL = cb.JL_to_LL;
const D_to_L = cb.D_to_L;
const D_to_LH = cb.D_to_LH;
const L_to_SR = cb.L_to_SR;
const SR_to_J = cb.SR_to_J;
const SR_plus_SRL_to_L = cb.SR_plus_SRL_to_L;
const SR_plus_literal_to_L = cb.SR_plus_literal_to_L;
const branch = cb.branch;
const op_reg32_to_L = cb.op_reg32_to_L;
const zero_to_L = cb.zero_to_L;

pub fn _0181() void {
    encoding(.CALL, .{ IP_relative(.imm16u) });
    //syntax("CALL IP+imm16[-65536,65535]");
    desc("Call IP-relative subroutine");

    IP_read_to_D(2, .word);
    D_to_L(.zx);
    L_to_SR(.temp_1);
    next_cycle();

    SR_plus_SRL_to_L(.ip, .temp_1, .zx, .fresh, .no_flags);
    L_to_SR(.next_ip);
    next_cycle();

    SR_plus_literal_to_L(.ip, 4, .fresh, .no_flags);
    L_to_SR(.rp);
    branch(.next_ip, 0);
}

pub fn _0182() void {
    encoding(.CALL, .{ IP_relative(.imm16n) });
    //syntax("CALL IP+imm16[-65536,65535]");
    desc("Call IP-relative subroutine");

    IP_read_to_D(2, .word);
    D_to_L(.zx);
    L_to_SR(.temp_1);
    next_cycle();

    SR_plus_SRL_to_L(.ip, .temp_1, ._1x, .fresh, .no_flags);
    L_to_SR(.next_ip);
    next_cycle();

    SR_plus_literal_to_L(.ip, 4, .fresh, .no_flags);
    L_to_SR(.rp);
    branch(.next_ip, 0);
}

pub fn _0183() void {
    encoding(.CALL, .{ .imm32u });
    //syntax("CALL imm32[0,4294967295]");
    desc("Call absolute-addressed subroutine");

    IP_read_to_D(2, .word);
    D_to_L(.zx);
    L_to_SR(.temp_1);
    next_cycle();

    IP_read_to_D(4, .word);
    D_to_LH();
    SR_to_J(.temp_1);
    JL_to_LL();
    L_to_SR(.next_ip);
    next_cycle();

    SR_plus_literal_to_L(.ip, 6, .fresh, .no_flags);
    L_to_SR(.rp);
    branch(.next_ip, 0);
}

pub fn _FA80_FA8F() void {
    encoding(.CALL, .{ Xa_relative(.I, .imm_0) });
    //syntax("CALL Xa");
    desc("Call register-addressed subroutine");

    op_reg32_to_L(.OA);
    L_to_SR(.next_ip);
    next_cycle();

    SR_plus_literal_to_L(.ip, 2, .fresh, .no_flags);
    L_to_SR(.rp);
    branch(.next_ip, 0);
}

pub fn _018F() void {
    encoding(.RET, .{});
    //syntax("RET");
    desc("Return from subroutine");

    zero_to_L();
    L_to_SR(.rp);
    branch(.rp, 0);
}
