const std = @import("std");
const allocators = @import("allocators.zig");
const uc = @import("microcode");
const ControlSignals = @import("ControlSignals");
const misc = @import("misc");
const instruction_encoding = @import("instruction_encoding");
const arch = @import("arch_builder.zig");
const cycle_builder = @import("cycle_builder.zig");

const assert = std.debug.assert;

const Opcode = misc.Opcode;
const CycleData = cycle_builder.CycleData;
const Mnemonic = instruction_encoding.Mnemonic;
const MnemonicSuffix = instruction_encoding.MnemonicSuffix;
const InstructionEncoding = instruction_encoding.InstructionEncoding;
const ParameterEncoding = instruction_encoding.ParameterEncoding;
const comptimeRelativeParameterEncoding = instruction_encoding.comptimeRelativeParameterEncoding;
const comptimeParameterEncodings = instruction_encoding.comptimeParameterEncodings;
const comptimeParameterEncoding = instruction_encoding.comptimeParameterEncoding;

const temp_alloc = allocators.temp_arena.allocator();

pub const InstructionRegState = enum {
    unknown,
    current_insn,
    next_insn,
    loaded,
    same_as_dl,
};

const InstructionData = struct {
    original_opcode_range: ?instruction_encoding.OpcodeRange,
    initial_uc_address: uc.Address,
    allowed_flags: uc.FlagSet,
    flags: uc.FlagSet,

    encoding: ?InstructionEncoding = null,
    description: ?[]const u8 = null,
    cycle_started: bool = false,
    queried_opcode: bool = false,
    queried_flags: uc.FlagSet = .{},

    dl_state: InstructionRegState,
    oa_state: InstructionRegState,
    ob_state: InstructionRegState,

    next_unread_insn_offset: misc.SignedOffsetForLiteral = -1,
    next_insn_offset: misc.SignedOffsetForLiteral = 0,
    next_insn_executed: bool = false,

    pub fn onIPRelativeAccess(self: *InstructionData, offset: misc.SignedOffsetForLiteral, size: u2) void {
        self.next_unread_insn_offset = std.math.max(self.next_unread_insn_offset, offset + size);
    }

    pub fn setNextInsnOffset(self: *InstructionData, offset: misc.SignedOffsetForLiteral) void {
        self.next_insn_offset = offset;
    }

    pub fn setNextInsnExecuted(self: *InstructionData) void {
        if (self.oa_state == .same_as_dl) {
            self.oa_state = self.dl_state;
        }
        if (self.ob_state == .same_as_dl) {
            self.ob_state = self.dl_state;
        }

        if (self.dl_state != .next_insn) {
            panic("DL value loaded by load_next_insn() has been clobbered!", .{});
        }
        if (self.oa_state != .next_insn) {
            panic("OA has not been loaded for the next instruction!", .{});
        }
        if (self.ob_state != .next_insn) {
            panic("OB has not been loaded for the next instruction!", .{});
        }
        self.next_insn_executed = true;
    }
};
pub var insn: ?*InstructionData = null;
var completed_cycles = std.ArrayList(CycleData).init(allocators.global_arena.allocator());

pub fn encoding(mnemonic: Mnemonic, comptime args: anytype) void {
    if (insn) |i| {
        if (i.encoding != null) {
            panic("Encoding has already been specified for this instruction!", .{});
        }
        i.encoding = .{
            .mnemonic = mnemonic,
            .suffix = .none,
            .params = comptime comptimeParameterEncodings(args),
            .opcode_base = i.original_opcode_range.?.min,
            .opcodes = i.original_opcode_range.?,
        };
    } else {
        panic("Not currently processing an instruction", .{});
    }
}

pub fn encodingWithSuffix(mnemonic: Mnemonic, suffix: MnemonicSuffix, comptime args: anytype) void {
    if (insn) |i| {
        if (i.encoding != null) {
            panic("Encoding has already been specified for this instruction!", .{});
        }
        i.encoding = .{
            .mnemonic = mnemonic,
            .suffix = suffix,
            .params = comptime comptimeParameterEncodings(args),
            .opcode_base = i.original_opcode_range.?.min,
            .opcodes = i.original_opcode_range.?,
        };
    } else {
        panic("Not currently processing an instruction", .{});
    }
}

pub fn X0_relative(comptime addr_type: MnemonicSuffix, comptime raw_encoding: anytype) type {
    comptime {
        const ptr_type = switch (addr_type) {
            .S => .ptr32s,
            .D => .ptr32d,
            .I => .ptr32i,
            .none => .reg32u,
            else => unreachable,
        };
        const param = comptimeRelativeParameterEncoding(comptimeParameterEncoding(raw_encoding), ptr_type, .implicit);
        return struct {
            pub const param_encoding = param;
        };
    }
}
pub fn Xa_relative(comptime addr_type: MnemonicSuffix, comptime raw_encoding: anytype) type {
    comptime {
        const ptr_type = switch (addr_type) {
            .S => .ptr32s,
            .D => .ptr32d,
            .I => .ptr32i,
            .none => .reg32u,
            else => unreachable,
        };
        const param = comptimeRelativeParameterEncoding(comptimeParameterEncoding(raw_encoding), ptr_type, .OA);
        return struct {
            pub const param_encoding = param;
        };
    }
}
pub fn Xb_relative(comptime addr_type: MnemonicSuffix, comptime raw_encoding: anytype) type {
    comptime {
        const ptr_type = switch (addr_type) {
            .S => .ptr32s,
            .D => .ptr32d,
            .I => .ptr32i,
            .none => .reg32u,
            else => unreachable,
        };
        const param = comptimeRelativeParameterEncoding(comptimeParameterEncoding(raw_encoding), ptr_type, .OB);
        return struct {
            pub const param_encoding = param;
        };
    }
}
pub fn IP_relative(comptime raw_encoding: anytype) type {
    comptime {
        const param = comptimeRelativeParameterEncoding(comptimeParameterEncoding(raw_encoding), .IP, .implicit);
        return struct {
            pub const param_encoding = param;
        };
    }
}
pub fn SP_relative(comptime raw_encoding: anytype) type {
    comptime {
        const param = comptimeRelativeParameterEncoding(comptimeParameterEncoding(raw_encoding), .SP, .implicit);
        return struct {
            pub const param_encoding = param;
        };
    }
}
pub fn KXP_relative(comptime raw_encoding: anytype) type {
    comptime {
        const param = comptimeRelativeParameterEncoding(comptimeParameterEncoding(raw_encoding), .KXP, .implicit);
        return struct {
            pub const param_encoding = param;
        };
    }
}
pub fn UXP_relative(comptime raw_encoding: anytype) type {
    comptime {
        const param = comptimeRelativeParameterEncoding(comptimeParameterEncoding(raw_encoding), .UXP, .implicit);
        return struct {
            pub const param_encoding = param;
        };
    }
}

pub fn parameter(index: usize) ParameterEncoding {
    if (insn) |i| {
        if (i.encoding) |enc| {
            return enc.params[index];
        } else panic("Encoding has not been specified yet for this instruction!", .{});
    } else panic("Not currently processing an instruction", .{});
}

pub fn getParameterOffset(param_index: usize) misc.SignedOffsetForLiteral {
    if (insn) |i| {
        if (i.encoding) |enc| {
            return instruction_encoding.getParameterOffsetForOpcode(misc.SignedOffsetForLiteral, enc, enc.params[param_index], opcode());
        } else panic("Encoding has not been specified yet for this instruction!", .{});
    } else panic("Not currently processing an instruction", .{});
}

pub fn getParameterConstant(comptime T: type, param_index: usize) T {
    if (insn) |i| {
        if (i.encoding) |enc| {
            return instruction_encoding.getParameterConstantForOpcode(T, enc, enc.params[param_index], opcode());
        } else panic("Encoding has not been specified yet for this instruction!", .{});
    } else panic("Not currently processing an instruction", .{});
}

pub fn desc(s: []const u8) void {
    if (s.len == 0) {
        panic("Expected description string", .{});
    }
    if (insn) |i| {
        if (i.description) |existing| {
            if (!std.mem.eql(u8, existing, s)) {
                panic("Description already specified", .{});
            }
        } else {
            i.description = s;
        }
    } else {
        panic("Not currently processing an instruction", .{});
    }
}

pub fn uc_address() uc.Address {
    if (insn) |i| {
        i.queried_opcode = true;
        return i.initial_uc_address;
    }
    panic("Not currently processing an instruction", .{});
}

pub fn opcode() Opcode {
    if (insn) |i| {
        if (uc.getOpcodeForAddress(i.initial_uc_address)) |v| {
            i.queried_opcode = true;
            return v;
        }
    }
    panic("Not currently processing an instruction", .{});
}

pub fn opcode_high() u8 {
    if (insn) |i| {
        if (uc.getOpcodeForAddress(i.initial_uc_address)) |v| {
            i.queried_opcode = true;
            return @intCast(u8, v >> 8);
        }
    }
    panic("Not currently processing an instruction", .{});
}

pub fn opcode_low() u8 {
    if (insn) |i| {
        if (uc.getOpcodeForAddress(i.initial_uc_address)) |v| {
            i.queried_opcode = true;
            return @truncate(u8, v);
        }
    }
    panic("Not currently processing an instruction", .{});
}

pub fn OA() misc.OperandA {
    if (insn) |i| {
        if (uc.getOAForAddress(i.initial_uc_address)) |v| {
            i.queried_opcode = true;
            return v;
        } else {
            panic("This opcode cannot have different implementations based on OA", .{});
        }
    }
    panic("Not currently processing an instruction", .{});
}

pub fn OB() misc.OperandB {
    if (insn) |i| {
        if (uc.getOBForAddress(i.initial_uc_address)) |v| {
            i.queried_opcode = true;
            return v;
        } else {
            panic("This opcode cannot have different implementations based on OB", .{});
        }
    }
    panic("Not currently processing an instruction", .{});
}

fn checkFlags(flags_to_check: []const uc.Flags) uc.FlagSet {
    if (insn) |i| {
        for (flags_to_check) |f| {
            if (!i.allowed_flags.contains(f)) {
                if (uc.getCheckedFlagsForAddress(i.initial_uc_address).contains(f)) {
                    panic("{} was not queried the first time this instruction was processed.  Try saving it to a variable before doing other checks.", .{ f });
                } else {
                    panic("This opcode cannot have different implementations based on {}", .{ f });
                }
            }
        }

        for (flags_to_check) |f| {
            i.queried_flags.insert(f);
        }

        return i.flags;
    }
    panic("Not currently processing an instruction", .{});
}

pub fn zero() bool {
    return checkFlags(&[_]uc.Flags{ .Z }).contains(.Z);
}

pub fn negative() bool {
    return checkFlags(&[_]uc.Flags{ .N }).contains(.N);
}

pub fn positive() bool {
    var flags = checkFlags(&[_]uc.Flags{ .Z, .N });
    return !flags.contains(.Z) and !flags.contains(.N);
}

pub fn carry_borrow() bool {
    return checkFlags(&[_]uc.Flags{ .C }).contains(.C);
}

pub fn overflow() bool {
    return checkFlags(&[_]uc.Flags{ .V }).contains(.V);
}

pub fn kernel() bool {
    return checkFlags(&[_]uc.Flags{ .K }).contains(.K);
}

pub fn unsigned_less_than() bool {
    var flags = checkFlags(&[_]uc.Flags{ .Z, .C });
    return flags.contains(.C) and !flags.contains(.Z);
}

pub fn unsigned_greater_than() bool {
    var flags = checkFlags(&[_]uc.Flags{ .Z, .C });
    return !flags.contains(.C) and !flags.contains(.Z);
}

pub fn signed_less_than() bool {
    var flags = checkFlags(&[_]uc.Flags{ .Z, .N, .V });
    return !flags.contains(.Z) and (flags.contains(.N) != flags.contains(.V));
}

pub fn signed_greater_than() bool {
    var flags = checkFlags(&[_]uc.Flags{ .Z, .N, .V });
    return !flags.contains(.Z) and (flags.contains(.N) == flags.contains(.V));
}

pub fn next_cycle() void {
    completed_cycles.append(cycle_builder.finish()) catch @panic("Out of memory");
    cycle_builder.start();
}

pub fn next_cycle_force_normal_execution() void {
    cycle_builder.setControlSignal(.seq_op, .next_uop_force_normal);
    next_cycle();
}

pub fn conditional_next_cycle(continuation: uc.Continuation) void {
    cycle_builder.setControlSignal(.next_uop, continuation);
    end_instruction();
}

pub fn fault_return() void {
    cycle_builder.SR1_to_L(.fault_ua_dl); // .fault_return implies LH -> UA
    cycle_builder.setControlSignal(.seq_op, .fault_return);
    end_instruction();
}

// It's not normally necessary to call this; it only exists to prevent accidentally
// creating additional cycles after a branch or load of a new instruction.
pub fn end_instruction() void {
    if (insn) |i| {
        if (i.cycle_started) {
            completed_cycles.append(cycle_builder.finish()) catch @panic("Out of memory");
            i.cycle_started = false;
        }
    }
}

pub fn processScope(comptime T: type) !void {
    @setEvalBranchQuota(10000);
    inline for (@typeInfo(T).Struct.decls) |decl| {
        if (decl.is_pub and decl.name[0] == '_') {
            const fn_info = @typeInfo(@TypeOf(@field(T, decl.name)));
            if (fn_info.Fn.params.len > 0) {
                std.debug.panic("Expected 0 params", .{});
            }

            if (comptime std.mem.startsWith(u8, decl.name, "_handler_")) {
                var handler: uc.Address = try std.fmt.parseUnsigned(uc.Address, decl.name[9..], 16);
                processHandler(handler, @field(T, decl.name));
            } else if (comptime std.mem.startsWith(u8, decl.name, "_continuation_")) {
                var continuation = try std.fmt.parseUnsigned(uc.Continuation, decl.name[14..], 16);
                processContinuation(continuation, @field(T, decl.name));
            } else {
                const is_alias = comptime std.mem.startsWith(u8, decl.name, "_alias_");
                const opcodes_str = if (is_alias) decl.name[7..] else decl.name[1..];
                var first_opcode: Opcode = undefined;
                var last_opcode: Opcode = undefined;

                var iter = std.mem.tokenize(u8, opcodes_str, "_");
                if (iter.next()) |first| {
                    first_opcode = try std.fmt.parseUnsigned(Opcode, first, 16);
                }

                if (iter.next()) |last| {
                    last_opcode = try std.fmt.parseUnsigned(Opcode, last, 16);
                } else {
                    last_opcode = @intCast(Opcode, @as(u32, first_opcode) + uc.getOpcodeGranularity(first_opcode) - 1);
                }

                try processOpcodes(first_opcode, last_opcode, @field(T, decl.name), is_alias);
            }
        }
    }
}

pub fn processHandler(handler: uc.Address, func: *const fn () void) void {
    allocators.temp_arena.reset();

    _ = process(.{
        .func = func,
        .original_opcode_range = null,
        .initial_uc_address = handler,
        .allowed_flags = .{},
        .flags = .{},
        .initial_dl_ob_oa_state = .unknown,
    });
}

pub fn processContinuation(continuation: uc.Continuation, func: *const fn () void) void {
    allocators.temp_arena.reset();

    const initial_uc_address = uc.getAddressForContinuation(continuation, .{});

    var config = ProcessConfig{
        .func = func,
        .original_opcode_range = null,
        .initial_uc_address = initial_uc_address,
        .allowed_flags = uc.getCheckedFlagsForAddress(initial_uc_address),
        .flags = .{},
        .initial_dl_ob_oa_state = .unknown,
    };
    var result = process(config);
    var unqueried_flags = result.queried_flags;
    unqueried_flags.toggleAll();
    unqueried_flags.setIntersection(config.allowed_flags);
    storeInitialContinuationCycleFlagPermutations(continuation, result.initial_cycle, result.initial_cycle_signals_used, .{}, unqueried_flags);

    // Don't allow new flags to be queried on subsequent processing, since that will mess up our unqueried_flags handling
    config.allowed_flags = result.queried_flags;

    var queried_permutations = uc.flagPermutationIterator(result.queried_flags);
    _ = queried_permutations.next(); // we already processed the no-flags case
    while (queried_permutations.next()) |queried_flag_permutation| {
        config.initial_uc_address = uc.getAddressForContinuation(continuation, queried_flag_permutation);
        config.flags = queried_flag_permutation;
        var permutation_result = process(config);

        if (result.encoding) |result_enc| {
            if (!instruction_encoding.eql(permutation_result.encoding.?, result_enc)) {
                printCyclePath(config.initial_uc_address, permutation_result.encoding);
                panic("Expected all permutations to have the same encoding.", .{});
            }
        }

        storeInitialContinuationCycleFlagPermutations(continuation, permutation_result.initial_cycle, permutation_result.initial_cycle_signals_used, queried_flag_permutation, unqueried_flags);
    }
}

fn storeInitialContinuationCycleFlagPermutations(
    continuation: uc.Continuation,
    initial_cycle: *ControlSignals,
    initial_cycle_signals_used: std.EnumSet(ControlSignals.SignalName),
    queried_flag_permutation: uc.FlagSet,
    unqueried_flags: uc.FlagSet
) void {
    var unqueried_permutations = uc.flagPermutationIterator(unqueried_flags);
    _ = unqueried_permutations.next(); // we already processed the no-flags case
    while (unqueried_permutations.next()) |unqueried_flag_permutation| {
        var combined_flags = queried_flag_permutation;
        combined_flags.setUnion(unqueried_flag_permutation);
        arch.putMicrocodeCycleNoDedup(uc.getAddressForContinuation(continuation, combined_flags), initial_cycle, initial_cycle_signals_used);
    }
}

pub fn processOpcodes(first_opcode: Opcode, last_opcode: Opcode, func: *const fn () void, is_alias: bool) !void {
    allocators.temp_arena.reset();

    const first_granularity = uc.getOpcodeGranularity(first_opcode);
    const last_granularity = uc.getOpcodeGranularity(last_opcode);

    if (first_opcode % first_granularity != 0) {
        panic("First opcode {X:0>4} should be aligned to granularity {}", .{ first_opcode, first_granularity });
    }
    if (last_opcode != (last_opcode & ~(last_granularity - 1)) +% last_granularity -% 1) {
        panic("Last opcode {X:0>4} should be aligned to granularity {}", .{ last_opcode, last_granularity });
    }

    var result = processOpcode(.{
        .min = first_opcode,
        .max = last_opcode,
    }, first_opcode, func, is_alias);

    if (!result.queried_opcode) {
        var checked_flags = uc.getCheckedFlagsForOpcode(first_opcode);
        {
            var opIter = uc.opcodeIterator(first_opcode, last_opcode);
            _ = opIter.next();
            while (opIter.next()) |cur_opcode| {
                if (!std.meta.eql(checked_flags, uc.getCheckedFlagsForOpcode(cur_opcode))) {
                    panic("Opcode {X:0>4} has different flags than opcode {X:0>4}", .{ cur_opcode, first_opcode });
                }
            }
        }
        {
            var flagIter = uc.flagPermutationIterator(checked_flags);
            while (flagIter.next()) |flag_variant| {
                const ua = uc.getAddressForOpcode(first_opcode, flag_variant);
                const cycle = arch.getMicrocodeCycle(ua).?;
                const used_signals = arch.getUsedSignalsForAddress(ua);

                var opIter = uc.opcodeIterator(first_opcode, last_opcode);
                _ = opIter.next();
                while (opIter.next()) |cur_opcode| {
                    const address = uc.getAddressForOpcode(cur_opcode, flag_variant);
                    arch.putMicrocodeCycleNoDedup(address, cycle, used_signals);
                }
            }
        }

        assert(result.encoding.opcodes.min == first_opcode);
        assert(result.encoding.opcodes.max == last_opcode);

        if (is_alias) {
            try arch.recordAlias(result.encoding, result.description);
        } else {
            arch.recordInstruction(result.encoding, result.description);
        }
    } else {
        result.encoding.opcodes.min = first_opcode;
        result.encoding.opcodes.max = first_opcode + first_granularity - 1;
        if (is_alias) {
            try arch.recordAlias(result.encoding, result.description);
        } else {
            arch.recordInstruction(result.encoding, result.description);
        }

        var opIter = uc.opcodeIterator(first_opcode, last_opcode);
        _ = opIter.next();
        while (opIter.next()) |cur_opcode| {
            result = processOpcode(.{
                .min = first_opcode,
                .max = last_opcode,
            }, cur_opcode, func, is_alias);
            result.encoding.opcodes.min = cur_opcode;
            result.encoding.opcodes.max = cur_opcode + uc.getOpcodeGranularity(cur_opcode) - 1;

            if (is_alias) {
                try arch.recordAlias(result.encoding, result.description);
            } else {
                arch.recordInstruction(result.encoding, result.description);
            }
        }
    }
}

const ProcessOpcodeResult = struct {
    encoding: InstructionEncoding,
    description: ?[]const u8,
    queried_opcode: bool,
};

fn processOpcode(original_range: instruction_encoding.OpcodeRange, the_opcode: Opcode, func: *const fn () void, is_alias: bool) ProcessOpcodeResult {
    var config = ProcessConfig{
        .func = func,
        .original_opcode_range = original_range,
        .initial_uc_address = uc.getAddressForOpcode(the_opcode, .{}),
        .allowed_flags = uc.getCheckedFlagsForOpcode(the_opcode),
        .flags = .{},
        .initial_dl_ob_oa_state = .current_insn,
    };

    if (is_alias) {
        var result = processAlias(config);
        return .{
            .encoding = result.encoding orelse panic("Expected encoding to be specified for alias of opcode {X:0>4}", .{ the_opcode }),
            .description = result.description,
            .queried_opcode = result.queried_opcode,
        };
    }

    var result = process(config);
    var unqueried_flags = result.queried_flags;
    unqueried_flags.toggleAll();
    unqueried_flags.setIntersection(config.allowed_flags);
    storeInitialCycleFlagPermutations(the_opcode, result.initial_cycle, result.initial_cycle_signals_used, .{}, unqueried_flags);

    // Don't allow new flags to be queried on subsequent processing, since that will mess up our unqueried_flags handling
    config.allowed_flags = result.queried_flags;

    var queried_permutations = uc.flagPermutationIterator(result.queried_flags);
    _ = queried_permutations.next(); // we already processed the no-flags case
    while (queried_permutations.next()) |queried_flag_permutation| {
        config.initial_uc_address = uc.getAddressForOpcode(the_opcode, queried_flag_permutation);
        config.flags = queried_flag_permutation;
        var permutation_result = process(config);

        result.next_unread_insn_offset = std.math.max(result.next_unread_insn_offset, permutation_result.next_unread_insn_offset);
        if (permutation_result.next_insn_executed) {
            if (!result.next_insn_executed) {
                result.next_insn_executed = true;
                result.next_insn_offset = permutation_result.next_insn_offset;
            } else if (result.next_insn_offset != permutation_result.next_insn_offset) {
                printCyclePath(config.initial_uc_address, permutation_result.encoding);
                panic("Expected all permutations to load the next instruction from the same offset!  Use branch() instead of exec_next_insn() for branches.", .{});
            }
        }

        if (permutation_result.queried_opcode) {
            result.queried_opcode = true;
        }

        if (result.encoding) |result_enc| {
            if (!instruction_encoding.eql(permutation_result.encoding.?, result_enc)) {
                printCyclePath(config.initial_uc_address, permutation_result.encoding);
                panic("Expected all permutations to have the same encoding.", .{});
            }
        }

        storeInitialCycleFlagPermutations(the_opcode, permutation_result.initial_cycle, permutation_result.initial_cycle_signals_used, queried_flag_permutation, unqueried_flags);
    }

    const insn_encoding = result.encoding orelse panic("Expected encoding to be specified", .{});

    if (result.next_unread_insn_offset < 0) {
        result.next_unread_insn_offset = 2;
    }

    if (result.next_insn_executed) {
        if (result.next_insn_offset != result.next_unread_insn_offset) {
            printCyclePath(uc.getAddressForOpcode(the_opcode, .{}), result.encoding);
            panic("Expected next instruction to be loaded from an offset of {} but it was actually from {}", .{ result.next_unread_insn_offset, result.next_insn_offset });
        } else if (result.next_insn_offset != instruction_encoding.getInstructionLength(insn_encoding)) {
            printCyclePath(uc.getAddressForOpcode(the_opcode, .{}), result.encoding);
            panic("Expected instruction length of {} based on encoding, but next instruction was loaded from {}", .{ instruction_encoding.getInstructionLength(insn_encoding), result.next_insn_offset });
        }
    }

    return .{
        .encoding = insn_encoding,
        .description = result.description,
        .queried_opcode = result.queried_opcode,
    };
}

fn storeInitialCycleFlagPermutations(the_opcode: Opcode, initial_cycle: *ControlSignals, used_signals: std.EnumSet(ControlSignals.SignalName), queried_flag_permutation: uc.FlagSet, unqueried_flags: uc.FlagSet) void {
    var unqueried_permutations = uc.flagPermutationIterator(unqueried_flags);
    _ = unqueried_permutations.next(); // we already processed the no-flags case
    while (unqueried_permutations.next()) |unqueried_flag_permutation| {
        var combined_flags = queried_flag_permutation;
        combined_flags.setUnion(unqueried_flag_permutation);
        arch.putMicrocodeCycleNoDedup(uc.getAddressForOpcode(the_opcode, combined_flags), initial_cycle, used_signals);
    }
}

const ProcessConfig = struct {
    func: *const fn () void,
    original_opcode_range: ?instruction_encoding.OpcodeRange,
    initial_uc_address: uc.Address,
    allowed_flags: uc.FlagSet,
    flags: uc.FlagSet,
    initial_dl_ob_oa_state: InstructionRegState,
};

const ProcessResult = struct {
    encoding: ?InstructionEncoding,
    description: ?[]const u8,
    initial_cycle: *ControlSignals,
    initial_cycle_signals_used: std.EnumSet(ControlSignals.SignalName),
    queried_opcode: bool,
    queried_flags: uc.FlagSet,
    next_unread_insn_offset: misc.SignedOffsetForLiteral,
    next_insn_offset: misc.SignedOffsetForLiteral,
    next_insn_executed: bool,
};

fn process(config: ProcessConfig) ProcessResult {
    if (insn != null) {
        @panic("There is already another instruction being processed");
    }

    var i = InstructionData{
        .original_opcode_range = config.original_opcode_range,
        .initial_uc_address = config.initial_uc_address,
        .allowed_flags = config.allowed_flags,
        .flags = config.flags,
        .dl_state = config.initial_dl_ob_oa_state,
        .oa_state = config.initial_dl_ob_oa_state,
        .ob_state = config.initial_dl_ob_oa_state,
    };

    completed_cycles.clearRetainingCapacity();
    insn = &i;
    defer insn = null;

    cycle_builder.start();
    i.cycle_started = true;

    config.func();

    if (i.cycle_started) {
        completed_cycles.append(cycle_builder.finish()) catch @panic("Out of memory");
    }

    var cycles = completed_cycles.items;

    var c = cycles.len - 1;
    while (c > 0) : (c -= 1) {
        const ua = arch.getOrCreateUnconditionalContinuation(&cycles[c].cs, cycles[c].used_signals);
        cycles[c - 1].cs.next_uop = uc.getContinuationNumberForAddress(ua).?;
    }

    if (!uc.isContinuationOrHandler(config.initial_uc_address)) {
        if (i.encoding == null) panic("Encoding not specified!", .{});
    }

    const used_signals = cycles[0].used_signals;
    const cs = arch.putMicrocodeCycle(i.initial_uc_address, &cycles[0].cs, used_signals);

    return .{
        .encoding = i.encoding,
        .description = i.description,
        .initial_cycle = cs,
        .initial_cycle_signals_used = used_signals,
        .queried_opcode = i.queried_opcode,
        .queried_flags = i.queried_flags,
        .next_unread_insn_offset = i.next_unread_insn_offset,
        .next_insn_offset = i.next_insn_offset,
        .next_insn_executed = i.next_insn_executed,
    };
}

const ProcessAliasResult = struct {
    encoding: ?InstructionEncoding,
    description: ?[]const u8,
    queried_opcode: bool,
};

fn processAlias(config: ProcessConfig) ProcessAliasResult {
    if (insn != null) {
        @panic("There is already another instruction being processed");
    }

    var i = InstructionData{
        .original_opcode_range = config.original_opcode_range,
        .initial_uc_address = config.initial_uc_address,
        .allowed_flags = config.allowed_flags,
        .flags = config.flags,
        .dl_state = config.initial_dl_ob_oa_state,
        .oa_state = config.initial_dl_ob_oa_state,
        .ob_state = config.initial_dl_ob_oa_state,
    };

    completed_cycles.clearRetainingCapacity();
    insn = &i;
    defer insn = null;

    config.func();

    std.debug.assert(completed_cycles.items.len == 0);
    std.debug.assert(i.queried_flags.count() == 0);

    if (i.encoding == null) panic("Encoding not specified!", .{});

    return .{
        .encoding = i.encoding,
        .description = i.description,
        .queried_opcode = i.queried_opcode,
    };
}

pub fn panic(comptime format: []const u8, args: anytype) noreturn {
    if (insn) |i| {
        printCyclePath(i.initial_uc_address, i.encoding);
    }
    var stderr = std.io.getStdErr().writer();
    stderr.print(format, args) catch @panic("IO Error");
    stderr.writeAll("\n") catch @panic("IO Error");
    std.os.exit(0);
}

pub fn warn(comptime format: []const u8, args: anytype) void {
    if (insn) |i| {
        printCyclePath(i.initial_uc_address, i.encoding);
    }
    var stderr = std.io.getStdErr().writer();
    stderr.print(format, args) catch @panic("IO Error");
    stderr.writeAll("\n") catch @panic("IO Error");
}

pub fn printCyclePath(initial_uc_address: uc.Address, insn_encoding: ?InstructionEncoding) void {
    var stderr = std.io.getStdErr().writer();

    if (uc.getOpcodeForAddress(initial_uc_address)) |op| {
        stderr.print("opcode {X:0>4}", .{ op }) catch @panic("IO Error");
    } else {
        stderr.print("address {X:0>4}", .{ initial_uc_address }) catch @panic("IO Error");
    }

    var checked_flags = uc.getCheckedFlagsForAddress(initial_uc_address);
    if (checked_flags.count() > 0) {
        stderr.print(" flags ", .{}) catch @panic("IO Error");

        var initial_flags = uc.getFlagsForAddress(initial_uc_address);
        var iter = checked_flags.iterator();
        while (iter.next()) |f| {
            if (initial_flags.contains(f)) {
                stderr.writeAll(@tagName(f)) catch @panic("IO Error");
            } else {
                var buf: [4]u8 = undefined;
                stderr.writeAll(std.ascii.lowerString(&buf, @tagName(f))) catch @panic("IO Error");
            }
        }
    }

    if (insn_encoding) |enc| {
        if (enc.suffix != .none) {
            stderr.print(" : {s}.{s}", .{ @tagName(enc.mnemonic), @tagName(enc.suffix) }) catch @panic("IO Error");
        } else {
            stderr.print(" : {s}", .{ @tagName(enc.mnemonic) }) catch @panic("IO Error");
        }
    }

    if (completed_cycles.items.len > 0) {
        stderr.print("\n   +{} cycles\n", .{ completed_cycles.items.len }) catch @panic("IO Error");
    } else {
        stderr.print("\n", .{}) catch @panic("IO Error");
    }
}
