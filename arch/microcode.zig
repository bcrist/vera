const std = @import("std");
const allocators = @import("allocators.zig");
const ctrl = @import("control_signals");
const uc_layout = @import("microcode_layout");
const instructions = @import("instructions.zig");
const panic = @import("instruction_builder.zig").panic;

const Control_Signals = ctrl.Control_Signals;
const UC_Address = uc_layout.UC_Address;

const gpa = &allocators.global_gpa;
const perm_arena = &allocators.global_arena;

const CycleContext = struct {
    pub fn hash(self: @This(), cs: *Control_Signals) u64 {
        _ = self;
        var hasher = std.hash.Wyhash.init(0);
        std.hash.autoHash(&hasher, cs.*);
        return hasher.final();
    }
    pub fn eql(self: @This(), a: *Control_Signals, b: *Control_Signals) bool {
        _ = self;
        return std.meta.eql(a.*, b.*);
    }
};
var cycle_dedup = std.HashMap(*Control_Signals, *Control_Signals, CycleContext, 75).init(gpa.allocator());
var unconditional_continuations = std.HashMap(*Control_Signals, UC_Address, CycleContext, 75).init(gpa.allocator());
pub var microcode: [65536]?*Control_Signals = [_]?*Control_Signals { null } ** 65536;

var next_unconditional_continuation: uc_layout.UC_Continuation = 0x1FF;

pub fn getOrCreateUnconditionalContinuation(cycle: *Control_Signals) UC_Address {
    if (unconditional_continuations.get(cycle)) |ua| {
        return ua;
    } else {
        const min_n = @enumToInt(uc_layout.UC_Vectors.interrupt) + 1;

        var n = next_unconditional_continuation;
        var ua = uc_layout.getAddressForContinuation(n, .{});

        while (n >= min_n and microcode[ua] != null) {
            n -= 1;
            ua = uc_layout.getAddressForContinuation(n, .{});
        }

        if (n < min_n) {
            // TODO use conditional slots?
            std.debug.panic("No more continuations left!", .{});
        }
        next_unconditional_continuation = n - 1;

        const deduped = put(ua, cycle);
        unconditional_continuations.put(deduped, ua) catch @panic("Out of memory!");
        return ua;
    }
}

pub fn getContinuationsLeft() usize {
    const min_n = @enumToInt(uc_layout.UC_Vectors.interrupt) + 1;
    return next_unconditional_continuation - min_n + 1;
}

pub fn get(ua: UC_Address) ?*Control_Signals {
    return microcode[ua];
}

pub fn put(ua: UC_Address, cycle: *Control_Signals) *Control_Signals {
    var deduped = cycle;
    if (cycle_dedup.get(cycle)) |c| {
        deduped = c;
    } else {
        deduped = perm_arena.allocator().create(Control_Signals) catch @panic("Out of memory!");
        deduped.* = cycle.*;
        cycle_dedup.put(deduped, deduped) catch @panic("Out of memory!");
    }

    putNoDedup(ua, deduped);
    return deduped;
}

// provided cycle should be from perm_arena
pub fn putNoDedup(ua: UC_Address, cycle: *Control_Signals) void {
    if (microcode[ua] != null) {
        if (uc_layout.getOpcodeForAddress(ua)) |opcode| {
            if (instructions.getInstructionByOpcode(opcode)) |insn| {
                panic("Microcode address {X} (opcode {X}) is already in use by {}", .{ ua, opcode, insn.mnemonic });
            } else {
                panic("Microcode address {X} (opcode {X}) is already in use", .{ ua, opcode });
            }
        } else {
            panic("Microcode address {X} is already in use", .{ ua });
        }
    } else {
        microcode[ua] = cycle;
    }
}
