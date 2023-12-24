pub const Lo8 = enum (u8) {
    ip_relative_call__forward = 0x01,
    ip_relative_call__back,
    load_registerset,
    store_registerset,
    copy_reg16_reg16,
    copy_reg32_reg32,
    copy_reg16u_reg32,
    copy_reg16s_reg32,
    add_reg32_s8,
    add_reg32_u16,
    add_reg32_n16,
    add_reg32_reg16u,
    add_reg32_reg16s,
    add_reg16_s8,
    add_reg16_s8_with_carry,
    add_reg16_i16,
    add_reg16_i16_with_carry,
    add_reg16_reg16,
    add_reg16_reg16_with_carry,
    subtract_reg32_reg16u,
    subtract_reg32_reg16s,
    subtract_s8_reg16,
    subtract_s8_reg16_with_carry,
    subtract_i16_reg16,
    subtract_i16_reg16_with_carry,
    subtract_reg16_reg16,
    subtract_reg16_reg16_with_carry,
    compare_reg16_reg16,
    compare_reg16_reg16_with_carry,
    compare_reg32_reg16s,
    compare_reg32_reg16u,
    xor_reg16_reg16,
    xnor_reg16_reg16,
    or_reg16_reg16,
    nor_reg16_reg16,
    and_reg16_reg16,
    nand_reg16_reg16,
    andnot_reg16_reg16,
    copy_u4_reg16,
    copy_n4_reg16,
    copy_u4_reg32,
    copy_n4_reg32,
    subtract_sp_u8,
    add_sp_u8,
    copy_imm_bit_reg16,

    load_data_reg32_reg8u,
    load_data_reg32_reg8s,
    load_data_reg32_reg16,
    load_data_reg32_reg32,
    load_insn_reg32_reg8u,
    load_insn_reg32_reg8s,
    load_insn_reg32_reg16,
    load_insn_reg32_reg32,
    load_stack_reg32_reg8u,
    load_stack_reg32_reg8s,
    load_stack_reg32_reg16,
    load_stack_reg32_reg32,

    load_data_reg32_plus_reg16_reg8u,
    load_data_reg32_plus_reg16_reg8s,
    load_data_reg32_plus_reg16_reg16,
    load_data_reg32_plus_reg16_reg32,

    load_data_reg32_plus_u4_reg8u,
    load_data_reg32_plus_u4_reg8s,
    load_data_reg32_plus_u4m2_reg16,
    load_data_reg32_plus_u4m4_reg32,

    load_stack_plus_u4_reg8u,
    load_stack_plus_u4_reg8s,
    load_stack_plus_u4m2_reg16,
    load_stack_plus_u4m4_reg32,

    load_stack_plus_reg16s_reg8u,
    load_stack_plus_reg16s_reg8s,
    load_stack_plus_reg16s_reg16,
    load_stack_plus_reg16s_reg32,

    load_data_uxp_plus_u4m2_reg16,
    load_data_kxp_plus_u4m2_reg16,
    load_data_uxp_plus_u4m4_reg32,
    load_data_kxp_plus_u4m4_reg32,

    store_data_reg32_reg8,
    store_data_reg32_reg16,
    store_data_reg32_reg32,
    store_stack_reg32_reg8,
    store_stack_reg32_reg16,
    store_stack_reg32_reg32,

    store_data_reg32_plus_reg16_reg8,
    store_data_reg32_plus_reg16_reg16,
    store_data_reg32_plus_reg16_reg32,

    store_data_reg32_plus_u4_reg8,
    store_data_reg32_plus_u4m2_reg16,
    store_data_reg32_plus_u4m4_reg32,

    store_stack_plus_u4_reg8,
    store_stack_plus_u4m2_reg16,
    store_stack_plus_u4m4_reg32,

    store_stack_plus_reg16s_reg8u,
    store_stack_plus_reg16s_reg16,
    store_stack_plus_reg16s_reg32,

    load_and_increment_data_reg32_reg8u,
    load_and_increment_data_reg32_reg8s,
    load_and_increment_data_reg32_reg16,
    load_and_increment_data_reg32_reg32,
    increment_and_load_data_reg32_reg8u,
    increment_and_load_data_reg32_reg8s,
    increment_and_load_data_reg32_reg16,
    increment_and_load_data_reg32_reg32,

    store_and_increment_data_reg32_reg8,
    store_and_increment_data_reg32_reg16,
    store_and_increment_data_reg32_reg32,
    increment_and_store_data_reg32_reg8,
    increment_and_store_data_reg32_reg16,
    increment_and_store_data_reg32_reg32,

    test_reg16,
    test_reg16_no_set_z,
    test_bit_reg16,
    test_bit_reg16_no_set_z,
    clear_bit_reg16_u4,
    set_bit_reg16_u4,
    toggle_bit_reg16_u4,

    shift_right_reg16u_reg4,
    shift_right_reg16s_reg4,
    shift_right_reg32_reg5,
    shift_left_reg16_reg4,
    shift_left_reg32_reg5,
    shift_right_reg16u_u4,
    shift_right_reg16s_u4,
    shift_right_reg32_u4,
    shift_right_reg32_u4_16_31,
    shift_left_reg16_u4,
    shift_left_reg32_u4,
    shift_left_reg32_u4_16_31,

    multiply_low_reg16_i16,
    multiply_low_reg16_reg16,
    multiply_full_reg16u_u16,
    multiply_full_reg16s_u16,
    multiply_full_reg16u_s16,
    multiply_full_reg16s_s16,
    multiply_high_reg16u_reg16u,
    multiply_high_reg16u_reg16s,
    multiply_high_reg16s_reg16u,
    multiply_high_reg16s_reg16s,
    multiply_full_reg16u_reg16u_x0,
    multiply_full_reg16u_reg16s_x0,
    multiply_full_reg16s_reg16s_x0,

    atomic_load_reg16,
    atomic_load_reg32,
    atomic_store_reg16,
    atomic_store_reg32,
    atomic_store_if_zero_reg16,
    atomic_store_if_zero_reg32,
    atomic_add_reg16,
    atomic_add_reg32,

    branch_imm_63_318 = 0xF0,
    branch_imm_n64_n319 = 0xF1,
    branch_imm = Hi8.branch_4.to_lo8(),
    branch_z_nz_imm = Hi8.branch_z_3.to_lo8(),
    branch_conditional_imm_n6_12 = 0xF4,

    pub fn value(self: Lo8) u8 {
        return @intFromEnum(self);
    }
};

pub const Hi8 = enum (u16) {
    branch_4 = 0xF2_04,
    // ...
    branch_63 = 0xF2_3F,

    branch_n64 = 0xF2_40,
    // ...
    branch_n1 = 0xF2_7F,

    branch_z_3 = 0xF3_03,
    // ...
    branch_z_63 = 0xF3_3F,

    branch_z_n64 = 0xF3_40,
    // ...
    branch_z_n1 = 0xF3_7F,

    branch_nz_3 = 0xF3_83,
    // ...
    branch_nz_63 = 0xF3_BF,

    branch_nz_n64 = 0xF3_C0,
    // ...
    branch_nz_n1 = 0xF3_FF,

    pub fn value(self: Hi8) u8 {
        return @truncate(@intFromEnum(self));
    }

    pub fn to_lo8(self: Hi8) u8 {
        return @intCast(@intFromEnum(self) >> 8);
    }
};

pub const Lo12 = enum (u12) {
    memcpy_forward = 0xF8_0,
    memcpy_forward_bytewise,
    memcpy_reverse,
    memcpy_reverse_bytewise,
    stream_in,
    stream_out,
    stream_in_bytewise,
    stream_out_bytewise,
    block_load_to_ram,
    block_store_from_ram,

    count_bits = 0xF9_0,
    count_zeroes,
    count_bits_leading,
    count_zeroes_leading,
    count_bits_trailing,
    count_zeroes_trailing,
    atomic_increment_reg16,
    atomic_increment_reg32,
    atomic_decrement_if_not_zero_reg16,
    atomic_decrement_if_not_zero_reg32,
    atomic_exchange_reg16,
    atomic_exchange_reg32,
    atomic_exchange_if_equal_reg16,
    atomic_exchange_if_equal_reg32,

    increment_reg16 = 0xFA_0,
    decrement_reg16,
    increment_reg16_if_carry,
    decrement_reg16_if_not_carry,
    increment_reg32,
    decrement_reg32,
    increment_reg32_if_carry,
    decrement_reg32_if_not_carry,
    negate_reg16,
    negate_reg16_with_carry,
    complement_reg16,
    shift_right_reg16_c,
    shift_left_reg16_c,

    pop_reg8u = 0xFB_0,
    pop_reg8s,
    pop_reg16,
    pop_reg32,
    push_reg8,
    push_reg16,
    push_reg32,
    register_call,
    switch_to_registerset,
    branch_reg16u,
    branch_reg16s,
    branch_abs_reg32,
    branch_conditional_imm_13_268,
    branch_conditional_imm_n262_n7,
    branch_conditional_s16,
    branch_double_conditional,

    copy_imm16_reg16 = 0xFC_0,
    copy_imm16u_reg32,
    copy_imm16n_reg32,
    copy_imm32_reg32,
    copy_stat_to_reg16,
    copy_reg16_to_stat_zncv,
    copy_reg16_reg16_reg16,
    copy_reg32_reg32_reg32,
    load_stack_plus_s16_reg8u,
    load_stack_plus_s16_reg8s,
    load_stack_plus_s16_reg16,
    load_stack_plus_s16_reg32,
    store_stack_plus_s16_reg8u,
    store_stack_plus_s16_reg16,
    store_stack_plus_s16_reg32,

    copy_reg32_to_sp = 0xFD_0,
    copy_reg32_to_rp,
    copy_reg32_to_bp,
    copy_reg32_to_uxp,
    copy_reg32_to_kxp,
    copy_reg32_to_asn,
    copy_sp_to_reg32,
    copy_rp_to_reg32,
    copy_bp_to_reg32,
    copy_uxp_to_reg32,
    copy_kxp_to_reg32,
    copy_asn_to_reg32,
    copy_imm8u_reg16,
    copy_imm8u_reg32,

    compare_reg16_i16 = 0xFE_0,
    compare_reg16_i16_with_carry,
    compare_reg32_u16,
    compare_reg32_n16,
    copy_ip_relative_u16_reg32,
    copy_sp_relative_u16_reg32,
    copy_ip_relative_n16_reg32,
    copy_sp_relative_n16_reg32,
    update_address_translation__data_write,
    update_address_translation__data_read,
    update_address_translation__stack,
    update_address_translation__insn,
    invalidate_address_translation__data_write,
    invalidate_address_translation__data_read,
    invalidate_address_translation__stack,
    invalidate_address_translation__insn,

    pub fn value(self: Lo12) u12 {
        const raw = @intFromEnum(self);
        const low: u4 = @truncate(raw);
        const high: u8 = @truncate(raw >> 4);
        return bits.concat(.{ high, low });
    }
};

pub const Lo16 = enum (u16) {
    nop2 = 0x00_00,
    nop3 = 0x00_01,

    absolute_call = 0xFF_00,
    call_return,
    return_from_interrupt,
    return_from_fault,
    exit_handler,
    subtract_sp_r0u,
    add_sp_r0u,
    subtract_sp_u16,
    add_sp_u16,
    sync,


    branch_imm16u,
    branch_imm16n,
    branch_abs_imm32,
    branch_to_start_of_current_page,
    branch_to_start_of_next_page,
    branch_to_start_of_current_block,
    branch_to_start_of_next_block,
    enable_translation_and_branch,
    disable_translation_and_branch,

    park = 0xFF_FF,

    pub fn value(self: Lo16) u16 {
        const raw = @intFromEnum(self);
        return bits.swapHalves(u16, raw);
    }
};

const bits = @import("bits");
