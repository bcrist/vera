// Miscellaneous details about the architecture that aren't
// related to instructions, microcode, or control signals go in this file.

pub const Opcode = u16;

pub const Register_Index = u4; // for GPRs

pub const Signed_Offset_For_Literal = i7; // note not all possible values are valid

pub const JL_Bus = u16;
pub const JH_Bus = u16;
pub const J_Bus = u32;
pub const K_Bus = u16;
pub const LL_Bus = u16;
pub const LH_Bus = u16;
pub const L_Bus = u32;
pub const D_Bus = u16;
pub const P_Bus = u20; // virtual page number
pub const N_Bus = u12; // virtual page offset
pub const Virtual_Address = u32; // combination of P and N buses
pub const F_Bus = u12; // physical frame number
pub const Physical_Address = u24; // combination of F and N buses
pub const AT_ASN = u4; // The low portion of the address space number that is used by the address translator
pub const AT_Slot = u12; // combination of ASN, group, and non-tag bits from page number
pub const AT_Tag = u14;

pub const RSN = u6; // registerset number
pub const OB_OA = u8;
pub const OB = u4;
pub const OA = u4;
pub const DL = D_Bus;

pub const Pipe_ID = enum(u2) {
    zero = 0,
    one = 1,
    two = 2,

    pub fn next(self: Pipe_ID) Pipe_ID {
        return switch (self) {
            .zero => .one,
            .one => .two,
            .two => .zero,
        };
    }
};

pub const Execution_Mode = enum(u2) {
    normal = 0,
    interrupt = 1,
    fault = 2,
    interrupt_fault = 3,
};

pub const Power_Mode = enum(u1) {
    run = 0,
    sleep = 1,
};

pub const STAT_Bits = packed struct(LL_Bus) {
    Z: bool,
    N: bool,
    V: bool,
    C: bool,
    K: bool,
    A: bool,
    PWR: Power_Mode,
    PIPE: Pipe_ID,
    MODE: Execution_Mode,
    unused: u5 = 0,
};

pub const Special_Physical_Address = struct {
    pub const ram_start:  Physical_Address = 0x000_000;
    pub const ram_end:    Physical_Address = 0x7FF_FFF;
    pub const device_sys: Physical_Address = 0x800_000;
    pub const device_1:   Physical_Address = 0x900_000;
    pub const device_2:   Physical_Address = 0xA00_000;
    pub const device_3:   Physical_Address = 0xB00_000;
    pub const device_4:   Physical_Address = 0xC00_000;
    pub const device_5:   Physical_Address = 0xD00_000;
    pub const device_6:   Physical_Address = 0xE00_000;
    pub const device_7:   Physical_Address = 0xF00_000;
    pub const num_addresses_per_device = 0x100_000;
};

pub const Device_Frames = enum(F_Bus) {
    sys_interrupt_controller = physicalAddressToFrame(Special_Physical_Address.device_sys),
    sys_block_transfer_config = physicalAddressToFrame(Special_Physical_Address.device_sys) + 1,
    sys_accessed_frames = physicalAddressToFrame(Special_Physical_Address.device_sys) + 2,
    sys_dirty_frames = physicalAddressToFrame(Special_Physical_Address.device_sys) + 3,
    _,
};

pub fn physicalAddressToFrame(physical_address: Physical_Address) F_Bus {
    return @intCast(F_Bus, physical_address >> @bitSizeOf(N_Bus));
}
pub fn frameToPhysicalAddress(frame: F_Bus) Physical_Address {
    return @as(Physical_Address, frame) << @bitSizeOf(N_Bus);
}

pub const Zeropage_Vector_Table = extern struct {
    double_fault: u16,
    page_fault: u16,
    access_fault: u16,
    page_align_fault: u16,
    instruction_protection_fault: u16,
    invalid_instruction: u16,
    pipe_0_reset: u16,
};

pub const Registerset_State = extern struct {
    reg: [16]u16,
    RP: u32,
    SP: u32,
    BP: u32,
    fault_UA_DL: u32,
    fault_RSN_STAT: u32,
    int_RSN_fault_OB_OA: u32,
    temp_1: u32,
    IP: u32,
    next_IP: u32,
    ASN: u32,
    KXP: u32,
    UXP: u32,
    temp_2: u32,
};
