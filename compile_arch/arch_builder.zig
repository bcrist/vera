

pub fn analyzeCustom(temp_arena: *TempAllocator, writer: anytype) !void {
    temp_arena.reset();
    for (microcode, 0..) |maybe_cycle, ua_usize| {
        if (maybe_cycle) |cycle| {
            switch (cycle.kr_rsel) {
                .oa => {
                    const ua = @intCast(uc.Address, ua_usize);
                    if (uc.getOpcodeForAddress(ua)) |opcode| {
                        try writer.print("kr_rsel is {} for opcode {X:0>4}\n", .{ cycle.kr_rsel, opcode });
                    } else if (uc.getContinuationNumberForAddress(ua)) |cont| {
                        try writer.print("kr_rsel is {} for continuation {X:0>3}\n", .{ cycle.kr_rsel, cont });
                    } else {
                        try writer.print("kr_rsel is {} for address {X:0>4}\n", .{ cycle.kr_rsel, ua });
                    }
                },
                else => {},
            }
        }
    }
}




//const Instruction_Encoding = isa.Instruction_Encoding;
const isa = arch.isa;
const Control_Signals = hw.Control_Signals;
const hw = arch.hw;
const arch = @import("lib_arch");
const deep_hash_map = @import("deep_hash_map");
const bits = @import("bits");
const std = @import("std");
