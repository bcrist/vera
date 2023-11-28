pub const hw = @import("arch/hardware.zig");
pub const isa = @import("arch/isa.zig");

pub const Opcode = u16;

// pub const Opcode_Range = struct {
//     min: Opcode,
//     max: Opcode,

//     pub fn iterator(self: Opcode_Range) Iterator {
//         return .{
//             .next_opcode = self.min,
//             .final_opcode = self.max
//         };
//     }

//     pub const Iterator = struct {
//         next_opcode: u17,
//         final_opcode: u17,

//         pub fn next(self: *Iterator) ?Opcode {
//             if (self.next_opcode <= self.final_opcode) {
//                 const opcode: Opcode = @intCast(self.next_opcode);
//                 self.next_opcode += 1;
//                 return opcode;
//             } else {
//                 return null;
//             }
//         }
//     };
// };

pub const Context_State = extern struct {
    registers: [hw.register_count]u16,
    rp: u32,
    sp: u32,
    bp: u32,
    fault_uc_slot_dr: u32,
    fault_rsn_stat: u32,
    int_rsn_fault_iw_ik_ij: u32,
    temp_1: u32,
    ip: u32,
    next_ip: u32,
    asn: u32,
    kxp: u32,
    uxp: u32,
    temp_2: u32,
};

pub const Vector_Table = extern struct {
    reset: u16,
    double_fault: u16,
    page_fault: u16,
    access_fault: u16,
    page_align_fault: u16,
    invalid_instruction_fault: u16,
    instruction_protection_fault: u16,
};

const std = @import("std");
