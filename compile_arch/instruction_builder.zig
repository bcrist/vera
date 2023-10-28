const std = @import("std");
const bits = @import("bits");
const allocators = @import("allocators.zig");
const uc = @import("microcode");
const ControlSignals = @import("ControlSignals");
const isa = @import("isa_types");
const misc = @import("misc");
const ie = @import("isa_encoding");
const ce = @import("comptime_encoding");
const arch = @import("arch_builder.zig");
const cycle_builder = @import("cycle_builder.zig");

const assert = std.debug.assert;

const Opcode = isa.Opcode;
const Mnemonic = isa.Mnemonic;
const MnemonicSuffix = isa.MnemonicSuffix;
const AddressSpace = isa.AddressSpace;
const InstructionEncoding = ie.InstructionEncoding;
const ParameterEncoding = ie.ParameterEncoding;
const CycleData = cycle_builder.CycleData;
const comptimeAddressSpaceParameterEncoding = ce.addressSpaceParameterEncoding;
const comptimeRelativeParameterEncoding = ce.relativeParameterEncoding;
const comptimeParameterEncodings = ce.parameterEncodings;
const comptimeParameterEncoding = ce.parameterEncoding;

const temp_alloc = allocators.temp_arena.allocator();

pub const InstructionRegState = enum {
    unknown,
    current_insn,
    next_insn,
    loaded,
    same_as_dl,
};

const InstructionData = struct {
    original_opcode_range: ?isa.OpcodeRange,
    initial_uc_address: ?uc.Address,
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

    next_conditional_cycle_func: ?*const fn () void = null,

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

pub fn addr(comptime addr_space: AddressSpace, comptime raw_encoding: anytype) type {
    return struct {
        pub const param_encoding = comptimeAddressSpaceParameterEncoding(
            comptimeParameterEncoding(raw_encoding),
            addr_space
        );
    };
}

pub fn X0_relative(comptime addr_space: ?AddressSpace, comptime raw_encoding: anytype) type {
    return struct {
        pub const param_encoding = comptimeAddressSpaceParameterEncoding(
            comptimeRelativeParameterEncoding(
                comptimeParameterEncoding(raw_encoding),
                .{ .reg32 = .{ .min = 0, .max = 0 } },
                .implicit,
            ),
            addr_space,
        );
    };
}
pub fn Xa_relative(comptime addr_space: ?AddressSpace, comptime raw_encoding: anytype) type {
    return struct {
        pub const param_encoding = comptimeAddressSpaceParameterEncoding(
            comptimeRelativeParameterEncoding(
                comptimeParameterEncoding(raw_encoding),
                .{ .reg32 = .{} },
                .OA,
            ),
            addr_space,
        );
    };
}
pub fn Xb_relative(comptime addr_space: ?AddressSpace, comptime raw_encoding: anytype) type {
    return struct {
        pub const param_encoding = comptimeAddressSpaceParameterEncoding(
            comptimeRelativeParameterEncoding(
                comptimeParameterEncoding(raw_encoding),
                .{ .reg32 = .{} },
                .OB,
            ),
            addr_space,
        );
    };
}
pub fn IP_relative(comptime addr_space: ?AddressSpace, comptime raw_encoding: anytype) type {
    return struct {
        pub const param_encoding = comptimeAddressSpaceParameterEncoding(
            comptimeRelativeParameterEncoding(
                comptimeParameterEncoding(raw_encoding),
                .{ .sr = .IP },
                .implicit,
            ),
            addr_space,
        );
    };
}
pub fn SP_relative(comptime addr_space: ?AddressSpace, comptime raw_encoding: anytype) type {
    return struct {
        pub const param_encoding = comptimeAddressSpaceParameterEncoding(
            comptimeRelativeParameterEncoding(
                comptimeParameterEncoding(raw_encoding),
                .{ .sr = .SP },
                .implicit,
            ),
            addr_space,
        );
    };
}
pub fn KXP_relative(comptime addr_space: ?AddressSpace, comptime raw_encoding: anytype) type {
    return struct {
        pub const param_encoding = comptimeAddressSpaceParameterEncoding(
            comptimeRelativeParameterEncoding(
                comptimeParameterEncoding(raw_encoding),
                .{ .sr = .KXP },
                .implicit,
            ),
            addr_space,
        );
    };
}
pub fn UXP_relative(comptime addr_space: ?AddressSpace, comptime raw_encoding: anytype) type {
    return struct {
        pub const param_encoding = comptimeAddressSpaceParameterEncoding(
            comptimeRelativeParameterEncoding(
                comptimeParameterEncoding(raw_encoding),
                .{ .sr = .UXP },
                .implicit,
            ),
            addr_space,
        );
    };
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
            return getParameterValueForOpcode(misc.SignedOffsetForLiteral, enc, opcode(), enc.params[param_index].offset, enc.params[param_index].offset_src);
        } else panic("Encoding has not been specified yet for this instruction!", .{});
    } else panic("Not currently processing an instruction", .{});
}

pub fn getParameterConstant(comptime T: type, param_index: usize) T {
    if (insn) |i| {
        if (i.encoding) |enc| {
            return getParameterValueForOpcode(T, enc, opcode(), enc.params[param_index].base, enc.params[param_index].base_src);
        } else panic("Encoding has not been specified yet for this instruction!", .{});
    } else panic("Not currently processing an instruction", .{});
}

fn getParameterValueForOpcode(comptime T: type, insn_encoding: InstructionEncoding, insn_opcode: Opcode, base_offset: ParameterEncoding.BaseOffsetEncoding, src: ParameterEncoding.Source) T {
    var raw: i64 = undefined;
    switch (src) {
        .OA, .OB, .OB_OA => {
            const ua = uc.getAddressForOpcode(insn_opcode, .{});
            raw = switch (src) {
                .OA => uc.getOAForAddress(ua) orelse unreachable,
                .OB => uc.getOBForAddress(ua) orelse unreachable,
                .OB_OA => bits.concat(.{
                    uc.getOAForAddress(ua) orelse unreachable,
                    uc.getOBForAddress(ua) orelse unreachable,
                }),
                else => unreachable,
            };
        },
        .opcode => {
            std.debug.assert(insn_opcode >= insn_encoding.opcodes.min);
            std.debug.assert(insn_opcode <= insn_encoding.opcodes.max);
            raw = @as(i64, insn_opcode) - insn_encoding.opcode_base;
        },
        .implicit => {
            raw = 0;
        },
        .IP_plus_2_OA, .IP_plus_2_OB, .IP_plus_2_8, .IP_plus_2_16, .IP_plus_2_32, .IP_plus_4_16 => {
            @panic("Constant value varies based on immediate stored outside of opcode");
        },
    }

    return @intCast(T, switch (base_offset) {
        .constant => |constant_encoding| constant_encoding.decodeConstant(raw) orelse
            std.debug.panic("Opcode {X:0>4} results in a constant outside the configured range(s) for this parameter", .{ insn_opcode }),
        else => std.debug.panic("Parameter does not use constant as expected", .{}),
    });
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
        return i.initial_uc_address.?;
    }
    panic("Not currently processing an instruction", .{});
}

pub fn opcode() Opcode {
    if (insn) |i| {
        if (uc.getOpcodeForAddress(i.initial_uc_address.?)) |v| {
            i.queried_opcode = true;
            return v;
        }
    }
    panic("Not currently processing an instruction", .{});
}

pub fn opcode_high() u8 {
    if (insn) |i| {
        if (uc.getOpcodeForAddress(i.initial_uc_address.?)) |v| {
            i.queried_opcode = true;
            return @intCast(u8, v >> 8);
        }
    }
    panic("Not currently processing an instruction", .{});
}

pub fn opcode_low() u8 {
    if (insn) |i| {
        if (uc.getOpcodeForAddress(i.initial_uc_address.?)) |v| {
            i.queried_opcode = true;
            return @truncate(u8, v);
        }
    }
    panic("Not currently processing an instruction", .{});
}

pub fn OA() misc.OperandA {
    if (insn) |i| {
        if (uc.getOAForAddress(i.initial_uc_address.?)) |v| {
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
        if (uc.getOBForAddress(i.initial_uc_address.?)) |v| {
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
                if (uc.getCheckedFlagsForAddress(i.initial_uc_address.?).contains(f)) {
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

pub fn next_cycle_explicit(continuation: uc.Continuation) void {
    cycle_builder.setControlSignal(.next_uop, continuation);
    end_instruction();
}

pub fn next_cycle_conditional(func: *const fn () void) void {
    cycle_builder.setControlSignal(.next_uop, 0);
    end_instruction();
    insn.?.next_conditional_cycle_func = func;
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
                _ = processContinuation(continuation, @field(T, decl.name));
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
    if (uc.getContinuationNumberForAddress(handler)) |continuation| {
        if (uc.getCheckedFlagsForAddress(handler).count() > 0) {
            panic("Address {X:0>4} is conditional continuation {X}\n", .{ handler, continuation });
        }
    }
    if (uc.getOpcodeForAddress(handler)) |the_opcode| {
        panic("Address {X:0>4} is opcode {X:0>4}\n", .{ handler, the_opcode });
    }

    allocators.temp_arena.reset();

    const result = process(.{
        .func = func,
        .original_opcode_range = null,
        .initial_uc_address = handler,
        .allowed_flags = .{},
        .flags = .{},
        .initial_dl_ob_oa_state = .unknown,
        .require_encoding = false,
    });

    _ = arch.putMicrocodeCycle(handler, result.initial_cycle, result.initial_cycle_signals_used);
}

pub fn processContinuation(maybe_continuation: ?uc.Continuation, func: *const fn () void) uc.Continuation {
    var config = ProcessConfig{
        .func = func,
        .original_opcode_range = null,
        .initial_uc_address = null,
        .allowed_flags = uc.conditional_continuation_checked_flags,
        .flags = .{},
        .initial_dl_ob_oa_state = .unknown,
        .require_encoding = false,
    };

    if (maybe_continuation) |continuation| {
        allocators.temp_arena.reset();
        const initial_uc_address = uc.getAddressForContinuation(continuation, .{});
        config.initial_uc_address = initial_uc_address;
        config.allowed_flags = uc.getCheckedFlagsForAddress(initial_uc_address);
    }

    const result = process(config);

    const num_queried_flags = @intCast(u4, result.queried_flags.count());
    if (num_queried_flags == 0) {
        return arch.getOrCreateUnconditionalContinuation(maybe_continuation, result.initial_cycle, result.initial_cycle_signals_used);
    }

    var unqueried_flags = result.queried_flags;
    unqueried_flags.toggleAll();
    unqueried_flags.setIntersection(config.allowed_flags);

    var initial_cycle_data = std.ArrayList(arch.ConditionalCycleData)
        .initCapacity(allocators.temp_arena.allocator(), @as(usize, 1) << num_queried_flags)
        catch @panic("Out of memory!");

    initial_cycle_data.appendAssumeCapacity(.{
        .flags = .{},
        .cs = arch.dedupMicrocodeCycle(result.initial_cycle),
        .used_signals = result.initial_cycle_signals_used,
    });

    // Don't allow new flags to be queried on subsequent processing, since that will mess up our unqueried_flags handling
    config.allowed_flags = result.queried_flags;

    var queried_permutations = uc.flagPermutationIterator(result.queried_flags);
    _ = queried_permutations.next(); // we already processed the no-flags case
    while (queried_permutations.next()) |queried_flag_permutation| {
        if (maybe_continuation) |continuation| {
            config.initial_uc_address = uc.getAddressForContinuation(continuation, queried_flag_permutation);
        }
        config.flags = queried_flag_permutation;
        var permutation_result = process(config);

        initial_cycle_data.appendAssumeCapacity(.{
            .flags = queried_flag_permutation,
            .cs = arch.dedupMicrocodeCycle(permutation_result.initial_cycle),
            .used_signals = permutation_result.initial_cycle_signals_used,
        });
    }

    return arch.getOrCreateConditionalContinuation(maybe_continuation, initial_cycle_data.items, unqueried_flags);
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
        assert(result.encoding.opcodes.min == first_opcode);
        assert(result.encoding.opcodes.max == last_opcode);

        if (is_alias) {
            arch.recordAlias(result.encoding, result.description);
        } else {
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

            arch.recordInstruction(result.encoding, result.description);
        }
    } else {
        result.encoding.opcodes.min = first_opcode;
        result.encoding.opcodes.max = first_opcode + first_granularity - 1;
        if (is_alias) {
            arch.recordAlias(result.encoding, result.description);
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
                arch.recordAlias(result.encoding, result.description);
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

fn processOpcode(original_range: isa.OpcodeRange, the_opcode: Opcode, func: *const fn () void, is_alias: bool) ProcessOpcodeResult {
    var config = ProcessConfig{
        .func = func,
        .original_opcode_range = original_range,
        .initial_uc_address = uc.getAddressForOpcode(the_opcode, .{}),
        .allowed_flags = uc.getCheckedFlagsForOpcode(the_opcode),
        .flags = .{},
        .initial_dl_ob_oa_state = .current_insn,
        .require_encoding = true,
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
    _ = arch.putMicrocodeCycleOpcodePermutations(the_opcode, .{}, unqueried_flags, result.initial_cycle, result.initial_cycle_signals_used);

    // Don't allow new flags to be queried on subsequent processing, since that will mess up our unqueried_flags handling
    config.allowed_flags = result.queried_flags;

    var queried_permutations = uc.flagPermutationIterator(result.queried_flags);
    _ = queried_permutations.next(); // we already processed the no-flags case
    while (queried_permutations.next()) |queried_flag_permutation| {
        const initial_uc_address = uc.getAddressForOpcode(the_opcode, queried_flag_permutation);
        config.initial_uc_address = initial_uc_address;
        config.flags = queried_flag_permutation;
        var permutation_result = process(config);

        result.next_unread_insn_offset = std.math.max(result.next_unread_insn_offset, permutation_result.next_unread_insn_offset);
        if (permutation_result.next_insn_executed) {
            if (!result.next_insn_executed) {
                result.next_insn_executed = true;
                result.next_insn_offset = permutation_result.next_insn_offset;
            } else if (result.next_insn_offset != permutation_result.next_insn_offset) {
                printCyclePath(initial_uc_address, permutation_result.encoding);
                panic("Expected all permutations to load the next instruction from the same offset!  Use branch() instead of exec_next_insn() for branches.", .{});
            }
        }

        if (permutation_result.queried_opcode) {
            result.queried_opcode = true;
        }

        if (result.encoding) |result_enc| {
            if (!permutation_result.encoding.?.eqlExceptOpcodes(result_enc)) {
                printCyclePath(initial_uc_address, permutation_result.encoding);
                panic("Expected all permutations to have the same encoding.", .{});
            }
        }

        _ = arch.putMicrocodeCycleOpcodePermutations(the_opcode, queried_flag_permutation, unqueried_flags, permutation_result.initial_cycle, permutation_result.initial_cycle_signals_used);
    }

    const insn_encoding = result.encoding orelse panic("Expected encoding to be specified", .{});

    if (result.next_unread_insn_offset < 0) {
        result.next_unread_insn_offset = 2;
    }

    if (result.next_insn_executed) {
        if (result.next_insn_offset != result.next_unread_insn_offset) {
            printCyclePath(uc.getAddressForOpcode(the_opcode, .{}), result.encoding);
            panic("Expected next instruction to be loaded from an offset of {} but it was actually from {}", .{ result.next_unread_insn_offset, result.next_insn_offset });
        } else if (result.next_insn_offset != insn_encoding.getInstructionLength()) {
            printCyclePath(uc.getAddressForOpcode(the_opcode, .{}), result.encoding);
            panic("Expected instruction length of {} based on encoding, but next instruction was loaded from {}", .{ insn_encoding.getInstructionLength(), result.next_insn_offset });
        }
    }

    return .{
        .encoding = insn_encoding,
        .description = result.description,
        .queried_opcode = result.queried_opcode,
    };
}

const ProcessConfig = struct {
    func: *const fn () void,
    original_opcode_range: ?isa.OpcodeRange,
    initial_uc_address: ?uc.Address,
    allowed_flags: uc.FlagSet,
    flags: uc.FlagSet,
    initial_dl_ob_oa_state: InstructionRegState,
    require_encoding: bool,
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

    if (config.require_encoding and i.encoding == null) {
        panic("Encoding not specified!", .{});
    }

    if (i.cycle_started) {
        completed_cycles.append(cycle_builder.finish()) catch @panic("Out of memory");
    }

    var cycles = completed_cycles.items;

    if (i.next_conditional_cycle_func) |func| {
        const cycles_clone = allocators.temp_arena.allocator().dupe(CycleData, cycles) catch @panic("Out of memory");
        cycles = cycles_clone;
        insn = null;
        cycles[cycles.len - 1].cs.next_uop = processContinuation(null, func);
    }

    var c = cycles.len - 1;
    while (c > 0) : (c -= 1) {
        cycles[c - 1].cs.next_uop = arch.getOrCreateUnconditionalContinuation(null, &cycles[c].cs, cycles[c].used_signals);
    }

    return .{
        .encoding = i.encoding,
        .description = i.description,
        .initial_cycle = &cycles[0].cs,
        .initial_cycle_signals_used = cycles[0].used_signals,
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
        if (i.initial_uc_address) |ua| {
            printCyclePath(ua, i.encoding);
        }
    }
    var stderr = std.io.getStdErr().writer();
    stderr.print(format, args) catch @panic("IO Error");
    stderr.writeAll("\n") catch @panic("IO Error");
    std.os.exit(0);
}

pub fn warn(comptime format: []const u8, args: anytype) void {
    if (insn) |i| {
        if (i.initial_uc_address) |ua| {
            printCyclePath(ua, i.encoding);
        }
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