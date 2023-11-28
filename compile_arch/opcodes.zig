pub const Lo8 = enum (u8) {
    nop = 0x00,
    ip_relative_call__forward,
    ip_relative_call__back,
    call_return,
    load_registerset,
    store_registerset,

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

    compare_reg16_reg16,
    compare_reg16_reg16_with_borrow,
    compare_reg32_reg16s,
    compare_reg32_reg16u,

};

pub const Lo12 = enum (u12) {
    register_call = 0x0_FE,
    switch_to_registerset = 0x1_FE,

    compare_reg16_i16 = 0x2_FE,
    compare_reg16_i16_with_borrow = 0x3_FE,
    compare_reg32_u16 = 0x4_FE,
    compare_reg32_n16 = 0x5_FE,

    update_address_translation__data_write = 0x8_FE,
    update_address_translation__data_read = 0x9_FE,
    update_address_translation__stack = 0xA_FE,
    update_address_translation__insn = 0xB_FE,
    invalidate_address_translation__data_write = 0xC_FE,
    invalidate_address_translation__data_read = 0xD_FE,
    invalidate_address_translation__stack = 0xE_FE,
    invalidate_address_translation__insn = 0xF_FE,
};

pub const Lo16 = enum (u16) {
    absolute_call = 0x00_FF,
    return_from_interrupt = 0x01_FF,
    return_from_fault = 0x02_FF,
    exit_handler = 0x03_FF,
};
