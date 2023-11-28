arena: std.mem.Allocator,
temp: *TempAllocator,
microcode: Microcode_Builder,
decode_rom: Decode_ROM_Builder,
encoding_list: std.ArrayList(isa.Instruction_Encoding),

pub fn init(gpa: std.mem.Allocator, arena: std.mem.Allocator, temp: *TempAllocator) Processor {
    return .{
        .arena = arena,
        .temp = temp,
        .microcode = Microcode_Builder.init(gpa),
        .decode_rom = Decode_ROM_Builder.init(gpa),
        .encoding_list = std.ArrayList(isa.Instruction_Encoding).init(gpa),
    };
}

pub fn process(self: *Processor, comptime instruction_structs: anytype) void {
    @setEvalBranchQuota(10000);

    inline for (instruction_structs) |Struct| {
        self.temp.reset();
        const alloc = self.temp.allocator();

        if (@hasDecl(Struct, "spec")) {
            log.debug("Beginning processing of {s}:\n    {s}", .{ @typeName(Struct), Struct.spec });
        } else if (@hasDecl(Struct, "slot")) {
            log.debug("Beginning processing of {s}:\n    {}", .{ @typeName(Struct), Struct.slot });
        } else {
            log.debug("Beginning processing of {s}", .{ @typeName(Struct) });
        }

        var lookup = Slot_Info.Lookup.init(alloc);
        inline for (@typeInfo(Struct).Struct.decls) |decl| {
            const T = @TypeOf(@field(Struct, decl.name));
            const info = @typeInfo(T);
            if (info == .Fn and info.Fn.params.len > 0 and info.Fn.params[0].type.? == *Cycle) {
                const ptr = &@field(Struct, decl.name);
                const result = lookup.getOrPut(ptr) catch @panic("OOM");
                if (!result.found_existing) {
                    result.key_ptr.* = ptr;
                    result.value_ptr.* = Slot_Info.init(@field(Struct, decl.name), decl.name);
                }
            }
        }

        if (@hasDecl(Struct, "encoding")) {
            const encoders_fn = comptime resolve_encoders(Struct.encoding);
            const ij_fn = if (@hasDecl(Struct, "ij")) comptime resolve_encoders(Struct.ij) else no_encoders;
            const ik_fn = if (@hasDecl(Struct, "ik")) comptime resolve_encoders(Struct.ik) else no_encoders;
            const iw_fn = if (@hasDecl(Struct, "iw")) comptime resolve_encoders(Struct.iw) else no_encoders;

            const constraints = if (@hasDecl(Struct, "constraints")) comptime resolve_constraints(Struct.constraints) else &.{};

            const id_mode: hw.Control_Signals.ID_Mode = if (@hasDecl(Struct, "decode_mode")) Struct.decode_mode else .normal;

            if (@hasDecl(Struct, "spec")) {
                var parser = Spec_Parser.init(alloc, Struct.spec);
                while (parser.next()) |parsed| {
                    const encoders = encoders_fn(alloc, parsed.signature);
                    // for (encoders) |encoder| {
                    //     log.debug("Encoder: {any}", .{ encoder });
                    // }
                    const ij_encoders = ij_fn(alloc, parsed.signature);
                    // for (ij_encoders) |ij_encoder| {
                    //     log.debug("IJ: {any}", .{ ij_encoder });
                    // }
                    const ik_encoders = ik_fn(alloc, parsed.signature);
                    // for (ik_encoders) |ik_encoder| {
                    //     log.debug("IK: {any}", .{ ik_encoder });
                    // }
                    const iw_encoders = iw_fn(alloc, parsed.signature);
                    // for (iw_encoders) |iw_encoder| {
                    //     log.debug("IW: {any}", .{ iw_encoder });
                    // }

                    self.process_instruction(&Struct.entry, .{
                        .signature = parsed.signature,
                        .encoding_len = encoding_length(encoders),
                        .next_insn_loaded = false,
                    }, constraints, encoders, ij_encoders, ik_encoders, iw_encoders, id_mode, &lookup);

                    self.encoding_list.append(self.finalize_encoding(parsed, constraints, encoders)) catch @panic("OOM");
                }
            } else {
                const encoders = encoders_fn(alloc, null);
                const ij_encoders = ij_fn(alloc, null);
                const ik_encoders = ik_fn(alloc, null);
                const iw_encoders = iw_fn(alloc, null);
                self.process_instruction(&Struct.entry, null, constraints,
                    encoders, ij_encoders, ik_encoders, iw_encoders,
                    id_mode, &lookup);
            }
        } else if (@hasDecl(Struct, "slot")) {
            const slot_handle = Microcode_Processor.process(.{
                .processor = self,
                .ptr = &Struct.entry,
                .ctx = null,
                .slot = .{ .exact = Struct.slot },
                .lookup = &lookup,
            });
            self.resolve_cycles(slot_handle, &lookup);
        } else {
            @compileLog(Struct);
            @compileError("Expected either encoding or slot declaration");
        }
    }
}

fn process_instruction(
    self: *Processor,
    entry_fn: *const anyopaque,
    maybe_ctx: ?Slot_Info.Impl_Context,
    constraints: []const Constraint,
    encoders: []const Encoder,
    ij_encoders: []const Encoder,
    ik_encoders: []const Encoder,
    iw_encoders: []const Encoder,
    id_mode: hw.Control_Signals.ID_Mode,
    lookup: *Slot_Info.Lookup,
) void {
    const slot_handle = Microcode_Processor.process(.{
        .processor = self,
        .ptr = entry_fn,
        .ctx = maybe_ctx,
        .slot = .{ .forced_bits = 0 },
        .lookup = lookup,
    });
    self.resolve_cycles(slot_handle, lookup);

    var iter = Initial_Word_Encoding_Iterator.init(self.temp.allocator(), constraints, encoders, id_mode);
    while (iter.next()) |base_addr| {
        const entry: Decode_ROM_Builder.Entry = .{
            .slot_handle = slot_handle,
            .ij = hw.IJ.init(@intCast(iter.encode(ij_encoders, "IJ"))),
            .ik = hw.IK.init(@intCast(iter.encode(ik_encoders, "IK"))),
            .iw = hw.IW.init(@intCast(iter.encode(iw_encoders, "IW"))),
        };

        var undefined_bits_iter: Initial_Word_Undefined_Bits_Iterator = .{
            .base_address = base_addr,
            .undefined_bits = iter.undefined_bits,
        };
        while (undefined_bits_iter.next()) |addr| {
            self.decode_rom.add_entry(addr, entry);
        }
    }
}

fn resolve_constraints(comptime constraints: anytype) []const Constraint {
    switch (@typeInfo(@TypeOf(constraints))) {
        .Struct => |info| if (info.is_tuple) {
            comptime var out: [constraints.len]Constraint = undefined;
            inline for (constraints, &out) |in, *constraint| {
                constraint.* = comptime resolve_single_constraint(in);
            }
            return &out;
        },
        else => {},
    }

    return &.{ resolve_single_constraint(constraints) };
}

fn resolve_single_constraint(comptime constraint: anytype) Constraint {
    const T = @TypeOf(constraint);
    if (T == Constraint) return constraint;
    switch (@typeInfo(T)) {
        .Struct => |info| {
            std.debug.assert(info.is_tuple);
            std.debug.assert(constraint.len == 3);

            const Extended_Kind = enum {
                equal,
                not_equal,
                greater,
                greater_than,
                greater_or_equal,
                less,
                less_than,
                less_or_equal
            };
            const ext_kind: Extended_Kind = constraint[1];
            const kind: Constraint.Kind = switch (ext_kind) {
                .equal => .equal,
                .not_equal => .not_equal,
                .greater, .greater_than, .less, .less_than => .greater,
                .greater_or_equal, .less_or_equal => .greater_or_equal,
            };

            var left = resolve_constraint_value(constraint[0]);
            var right = resolve_constraint_value(constraint[2]);

            switch (ext_kind) {
                .less, .less_than, .less_or_equal => {
                    const temp = left;
                    left = right;
                    right = temp;
                },
                else => {},
            }

            return .{
                .left = left,
                .right = right,
                .kind = kind,
            };
        },
        else => @compileError("Expected .{ left, .op, right } or Constraint"),
    }
}

fn resolve_constraint_value(comptime value: anytype) Value {
    const T = @TypeOf(value);
    if (T == Value) return value;
    switch (@typeInfo(T)) {
        .Int, .ComptimeInt => {
            return .{ .constant = @intCast(value) };
        },
        .EnumLiteral => {
            return .{ .placeholder = .{
                .index = .invalid,
                .kind = .param_constant,
                .name = @tagName(value),
            }};
        },
        else => @compileError("Expected integer, enum literal, or Value"),
    }
}

const Encoder_Provider = *const fn (allocator: std.mem.Allocator, signature: ?isa.Instruction_Signature) []const Encoder;

fn no_encoders(allocator: std.mem.Allocator, signature: ?isa.Instruction_Signature) []const Encoder {
    _ = allocator;
    _ = signature;
    return &.{};
}

fn resolve_encoders(comptime encoders: anytype) Encoder_Provider {
    return struct {
        pub fn provider(allocator: std.mem.Allocator, signature: ?isa.Instruction_Signature) []const Encoder {
            switch (@typeInfo(@TypeOf(encoders))) {
                .Struct => |info| if (info.is_tuple) {
                    var out = allocator.alloc(Encoder, encoders.len) catch @panic("OOM");
                    inline for (encoders, out) |in, *encoder| {
                        encoder.* = resolve_single_encoder(in, signature);
                    }
                    return out;
                },
                else => {},
            }

            var out = allocator.alloc(Encoder, 1) catch @panic("OOM");
            out[0] = resolve_single_encoder(encoders, signature);
            return out;
        }
    }.provider;
}

fn resolve_single_encoder(encoder: anytype, signature: ?isa.Instruction_Signature) Encoder {
    const T = @TypeOf(encoder);
    if (T == Encoder) return encoder;
    switch (@typeInfo(T)) {
        .Fn => {
            var args: std.meta.ArgsTuple(T) = undefined;
            inline for (&args) |*a| {
                const Arg = @TypeOf(a.*);
                if (Arg == isa.Instruction_Signature) {
                    a.* = signature.?;
                } else if (Arg == isa.Mnemonic) {
                    a.* = signature.?.mnemonic;
                } else if (Arg == isa.Mnemonic_Suffix) {
                    a.* = signature.?.suffix;
                } else if (Arg == []const isa.Parameter.Signature) {
                    a.* = signature.?.params;
                } else switch (@typeInfo(Arg)) {
                    else => {
                        @compileLog(Arg);
                        @compileError("Unsupported argument for encoding function");
                    },
                }
            }
            return resolve_single_encoder(@call(.auto, encoder, args), signature);
        },
        else => return Encoder.identity(encoder),
    }
}

fn encoding_length(encoders: []const Encoder) Encoded_Instruction.Length_Type {
    var len: Encoded_Instruction.Length_Type = 0;
    for (encoders) |encoder| {
        const bytes: Encoded_Instruction.Length_Type = @intCast(encoder.required_bits() / 8);
        len = @max(len, bytes);
    }
    return len;
}

pub const Initial_Word_Encoding_Iterator = struct {
    first: bool = true,
    undefined_bits: hw.D,
    constraints: []const Constraint,
    value_iters: []Encoder.Value_Iterator,
    id_mode: hw.Control_Signals.ID_Mode,

    pub fn init(allocator: std.mem.Allocator, constraints: []const Constraint, encoders: []const Encoder, id_mode: hw.Control_Signals.ID_Mode) Initial_Word_Encoding_Iterator {
        const out = allocator.alloc(Encoder.Value_Iterator, encoders.len) catch @panic("OOM");
        var undefined_bits = ~@as(hw.D.Raw, 0);
        var n: usize = 0;
        for (encoders) |*encoder| {
            const encoder_bits: hw.D.Raw = @truncate(encoder.bit_mask());
            undefined_bits &= ~encoder_bits;

            if (encoder.bit_offset < @bitSizeOf(hw.D)) {
                out[n] = encoder.value_iterator();
                n += 1;
            }
        }
        return .{
            .first = true,
            .undefined_bits = hw.D.init(undefined_bits),
            .constraints = constraints,
            .value_iters = out[0..n],
            .id_mode = id_mode,
        };
    }

    pub fn next(self: *Initial_Word_Encoding_Iterator) ?hw.decode.Address {
        while (self.next_internal()) |val| {
            for (self.constraints) |constraint| {
                if (!self.constraint_matches(constraint)) break;
            } else return val;
        }
        return null;
    }

    fn next_internal(self: *Initial_Word_Encoding_Iterator) ?hw.decode.Address {
        if (self.first) {
            var encoded: isa.Encoded_Instruction.Data = 0;
            for (self.value_iters) |*value_iter| {
                value_iter.reset();
                std.debug.assert(value_iter.next() != null);
                std.debug.assert(value_iter.encoder.encode_value(value_iter.last_value, &encoded));
            }
            self.first = false;
            // log.debug("Encoding: {X}", .{ encoded });
            return .{
                .d = hw.D.init(@truncate(encoded)),
                .mode = self.id_mode,
            };
        }

        for (self.value_iters, 0..) |*value_iter, i| {
            if (value_iter.next() != null) {
                for (0..i) |j| {
                    self.value_iters[j].reset();
                    std.debug.assert(self.value_iters[j].next() != null);
                }
                var encoded: isa.Encoded_Instruction.Data = 0;
                for (self.value_iters) |iter| {
                    std.debug.assert(iter.encoder.encode_value(iter.last_value, &encoded));
                }
                //log.debug("Encoding: {X}", .{ encoded });
                return .{
                    .d = hw.D.init(@truncate(encoded)),
                    .mode = self.id_mode,
                };
            }
        }
        return null;
    }

    fn constraint_matches(self: *Initial_Word_Encoding_Iterator, constraint: Constraint) bool {
        const left = self.constraint_value(constraint.left) orelse return true;
        const right = self.constraint_value(constraint.right) orelse return true;
        return switch (constraint.kind) {
            .equal => left == right,
            .not_equal => left != right,
            .greater => left > right,
            .greater_or_equal => left >= right,
        };
    }

    fn constraint_value(self: Initial_Word_Encoding_Iterator, val: Value) ?i64 {
        return switch (val) {
            .constant => |k| k,
            .placeholder => |info| self.value(info.name),
        };
    }

    pub fn value(self: Initial_Word_Encoding_Iterator, placeholder: []const u8) ?i64 {
        for (self.value_iters) |iter| {
            switch (iter.encoder.value) {
                .placeholder => |info| {
                    if (std.mem.eql(u8, placeholder, info.name)) {
                        return iter.last_value;
                    }
                },
                .constant => {},
            }
        }
        return null;
    }

    pub fn encode(self: Initial_Word_Encoding_Iterator, encoders: []const Encoder, context: []const u8) isa.Encoded_Instruction.Data {
        var data: isa.Encoded_Instruction.Data = 0;
        for (encoders) |encoder| {
            std.debug.assert(encoder.encode_value(switch (encoder.value) {
                .placeholder => |info| self.value(info.name)
                    orelse std.debug.panic("{s} references placeholder '{s}', but it is not present in the initial word of the instruction encoding", .{ context, info.name }),
                .constant => |constant| constant,
            }, &data));
        }
        return data;
    }
};

pub const Initial_Word_Undefined_Bits_Iterator = struct {
    base_address: hw.decode.Address,
    undefined_bits: hw.D,
    next_permutation: ?hw.D.Raw = 0,

    pub fn next(self: *Initial_Word_Undefined_Bits_Iterator) ?hw.decode.Address {
        if (self.next_permutation) |permutation| {
            var next_permutation = permutation;
            const undefined_bits = self.undefined_bits.raw();
            var bit = @as(hw.D.Raw, 1);
            for (0..@bitSizeOf(hw.D)) |_| {
                bit <<= 1;
                if (0 != (undefined_bits & bit)) {
                    next_permutation ^= bit;
                    if (0 == (permutation & bit)) {
                        self.next_permutation = next_permutation;
                        break;
                    }
                }
            } else {
                self.next_permutation = null;
            }
            const base = self.base_address.raw();
            std.debug.assert((base | permutation) == (base ^ permutation)); // base and undefined should deal with distinct bits
            return hw.decode.Address.init(base | permutation);
        }
        return null;
    }

};

fn resolve_cycles(self: *Processor, slot_handle: Slot_Data.Handle, lookup: *const Slot_Info.Lookup) void {
    const slot_data = self.microcode.slot_data.items[@intFromEnum(slot_handle)];
    if (slot_data.acyclic) return;

    next_cycle: for (slot_data.cycles, 0..) |cycle_handle, i| {
        for (slot_data.cycles[0..i]) |prev_cycle_handle| {
            if (cycle_handle == prev_cycle_handle) continue :next_cycle;
        }

        const cycle = &self.microcode.cycles.items[@intFromEnum(cycle_handle)];
        if (cycle.recursion_ptr) |ptr| {
            const next = lookup.get(ptr).?;
            self.microcode.complete_loop(cycle_handle, next.slot);
        } else if (cycle.next_slot) |next| {
            self.resolve_cycles(next, lookup);
        }
    }
}

pub const Microcode_Processor = struct {
    processor: *Processor,
    prev: ?*const Microcode_Processor = null,
    ptr: *const anyopaque,
    ctx: ?Slot_Info.Impl_Context,
    slot: Slot_Location,
    lookup: *Slot_Info.Lookup,

    pub fn process(self: Microcode_Processor) Slot_Data.Handle {
        const gop = self.lookup.getOrPut(self.ptr) catch @panic("Microcode function not found");
        if (!gop.found_existing) @panic("Found c.next() for a function not puplicly visible within the instruction struct");
        const cycles = gop.value_ptr.impl(self.processor.temp.allocator(), self.ctx);
        log.debug("Processed microcode function {s}", .{ cycles[0].func_name });

        var cycle_recursion_ptrs: [hw.microcode.Address.count_per_slot]?*const anyopaque = undefined;
        var slot_data: Slot_Data = undefined;
        slot_data.slot = self.slot;
        slot_data.acyclic = true;
        for (0.., cycles) |i, *cycle| {
            // Except in the case of loops, we will wipe out cycle.recursion_ptr after recursively computing the next slot handle.
            // But to avoid excessive fan-out of descendant cycles (for slots using flags) we keep track of the original
            // recursion_ptr so that we can see if this cycle is a duplicate of one we just processed, without needing to
            // recursively process recursion_ptr.
            cycle_recursion_ptrs[i] = cycle.recursion_ptr;

            if (cycle.recursion_ptr) |next_ptr| {
                if (self.has_processed(next_ptr)) {
                    slot_data.acyclic = false;
                } else {
                    for (0..i) |j| {
                        if (cycle_recursion_ptrs[j] == next_ptr and cycles[j].signals.eql(cycle.signals)) {
                            // No need to recursively process recursion_ptr, it's a duplicate of another cycle already processed for this slot.
                            cycle.next_slot = cycles[j].next_slot;
                            cycle.recursion_ptr = null;
                            break;
                        }
                    } else {
                        var ctx = self.ctx;
                        if (ctx != null) {
                            ctx.?.next_insn_loaded = cycle.next_insn_loaded;
                        }
                        const next = Microcode_Processor.process(.{
                            .processor = self.processor,
                            .prev = &self,
                            .ptr = next_ptr,
                            .ctx = ctx,
                            .slot = Slot_Location.for_continuation(cycle.signals),
                            .lookup = self.lookup,
                        });
                        cycle.next_slot = next;
                        cycle.recursion_ptr = null;
                        if (slot_data.acyclic and !self.processor.microcode.slot_data.items[@intFromEnum(next)].acyclic) {
                            slot_data.acyclic = false;
                        }
                    }
                }
            }
            slot_data.cycles[i] = self.processor.microcode.intern_cycle(cycle.*);
        }

        if (cycles.len == 1) {
            @memset(slot_data.cycles[1..], slot_data.cycles[0]);
        } else std.debug.assert(cycles.len == slot_data.cycles.len);

        const slot_handle = self.processor.microcode.intern_slot_data(slot_data);
        gop.value_ptr.slot = slot_handle;
        return slot_handle;
    }

    // returns true if `needle` is a pointer to a function that's already been processed,
    // and thus processing it again would cause infinite recursion.
    fn has_processed(self: Microcode_Processor, needle: *const anyopaque) bool {
        if (self.ptr == needle) return true;
        var prev = self.prev;
        while (prev) |p| {
            if (p.ptr == needle) return true;
            prev = p.prev;
        }
        return false;
    }
};

pub const Slot_Info = struct {
    impl: Impl,
    slot: Slot_Data.Handle, // filled in by build_microcode

    pub const Lookup = std.AutoHashMap(*const anyopaque, Slot_Info);

    pub const Impl = *const fn (allocator: std.mem.Allocator, ctx: ?Impl_Context) []Cycle;
    pub const Impl_Context = struct {
        signature: isa.Instruction_Signature,
        encoding_len: Encoded_Instruction.Length_Type,
        next_insn_loaded: bool,
    };

    pub fn init(comptime func: anytype, comptime name: []const u8) Slot_Info {
        const Func = @TypeOf(func);
        const Args = std.meta.ArgsTuple(Func);
        const params = @typeInfo(Func).Fn.params;

        comptime var has_flags = false;
        inline for (params) |arg| {
            if (arg.type.? == hw.microcode.Flags) {
                has_flags = true;
            }
        }

        const temp = struct {
            pub fn unconditional(allocator: std.mem.Allocator, ctx: ?Impl_Context) []Cycle {
                const cycle = allocator.create(Cycle) catch @panic("OOM");
                const encoding_len = if (ctx) |c| c.encoding_len else null;
                const next_insn_loaded = if (ctx) |c| c.next_insn_loaded else false;
                cycle.* = Cycle.init(name, encoding_len, next_insn_loaded);
                @call(.auto, func, build_args(cycle, hw.microcode.Flags.init(0), ctx));
                cycle.finish();
                return @as(*[1]Cycle, cycle);
            }

            pub fn conditional(allocator: std.mem.Allocator, ctx: ?Impl_Context) []Cycle {
                const cycles = allocator.alloc(Cycle, hw.microcode.Address.count_per_slot) catch @panic("OOM");
                const encoding_len = if (ctx) |c| c.encoding_len else null;
                const next_insn_loaded = if (ctx) |c| c.next_insn_loaded else false;
                for (cycles, 0..) |*cycle, raw_flags| {
                    cycle.* = Cycle.init(name, encoding_len, next_insn_loaded);
                    @call(.auto, func, build_args(cycle, hw.microcode.Flags.init(@intCast(raw_flags)), ctx));
                    cycle.finish();
                }
                return cycles;
            }

            fn build_args(cycle: *Cycle, flags: hw.microcode.Flags, ctx: ?Impl_Context) Args {
                var args: Args = undefined;
                inline for (&args) |*a| {
                    const Arg = @TypeOf(a.*);
                    if (Arg == *Cycle) {
                        a.* = cycle;
                    } else if (Arg == hw.microcode.Flags) {
                        a.* = flags;
                    } else if (Arg == isa.Instruction_Signature) {
                        a.* = ctx.?.signature;
                    } else if (Arg == isa.Mnemonic) {
                        a.* = ctx.?.signature.mnemonic;
                    } else if (Arg == isa.Mnemonic_Suffix) {
                        a.* = ctx.?.signature.suffix;
                    } else if (Arg == []const isa.Parameter.Signature) {
                        a.* = ctx.?.signature.params;
                    } else switch (@typeInfo(Arg)) {
                        else => {
                            @compileLog(Arg);
                            @compileError("Unsupported argument for microcode function");
                        },
                    }
                }
                return args;
            }
        };

        return .{
            .slot = undefined,
            .impl = if (has_flags) &temp.conditional else &temp.unconditional,
        };
    }
};

fn finalize_encoding(self: *Processor, parsed: Instruction_Encoding, constraints: []const Constraint, encoders: []const Encoder) Instruction_Encoding {
    const final_encoders = self.arena.dupe(Encoder, encoders) catch @panic("OOM");
    for (final_encoders) |*encoder| {
        fixup_placeholder_value(&encoder.value, parsed.encoders);
    }

    const final_constraints = self.arena.alloc(Constraint, parsed.constraints.len + constraints.len) catch @panic("OOM");
    @memcpy(final_constraints.ptr, parsed.constraints);
    @memcpy(final_constraints[parsed.constraints.len..], constraints);

    for (final_constraints) |*constraint| {
        fixup_placeholder_value(&constraint.left, parsed.encoders);
        fixup_placeholder_value(&constraint.right, parsed.encoders);
    }

    for (parsed.encoders) |parsed_encoder| {
        const placeholder = parsed_encoder.value.placeholder.name;

        if (find_placeholder_info(placeholder, final_encoders) != null) continue;

        for (final_constraints) |constraint| {
            if (constraint.kind != .equal) continue;
            if (is_placeholder(placeholder, constraint.left)) {
                if (get_placeholder(constraint.right)) |other_placeholder| {
                    if (find_placeholder_info(other_placeholder, final_encoders) != null) break;
                }
            } else if (is_placeholder(placeholder, constraint.right)) {
                if (get_placeholder(constraint.left)) |other_placeholder| {
                    if (find_placeholder_info(other_placeholder, final_encoders) != null) break;
                }
            }
        } else {
            std.debug.panic("Placeholder '{s}' is not encoded or constrained equal to an encoded placeholder", .{ placeholder });
        }
    }

    const final_param_signatures = self.arena.dupe(isa.Parameter.Signature, parsed.signature.params) catch @panic("OOM");

    return .{
        .signature = .{
            .mnemonic = parsed.signature.mnemonic,
            .suffix = parsed.signature.suffix,
            .params = final_param_signatures,
        },
        .constraints = final_constraints,
        .encoders = final_encoders,
    };
}

fn fixup_placeholder_value(value: *Value, parsed_encoders: []const Encoder) void {
    switch (value.*) {
        .constant => {},
        .placeholder => |*info| if (info.index == .invalid) {
            if (find_placeholder_info(info.name, parsed_encoders)) |parsed_info| {
                info.index = parsed_info.index;
                info.kind = parsed_info.kind;
            } else {
                std.debug.panic("Encoder {s} not found in parsed instruction", .{ info.name });
            }
        },
    }
}

fn find_placeholder_info(needle: []const u8, haystack: []const Encoder) ?Instruction_Encoding.Placeholder_Info {
    for (haystack) |encoder| switch (encoder.value) {
        .constant => {},
        .placeholder => |info| {
            if (std.mem.eql(u8, info.name, needle)) {
                return info;
            }
        },
    };
    return null;
}

fn get_placeholder(value: Value) ?[]const u8 {
    switch (value) {
        .constant => return null,
        .placeholder => |info| return info.name,
    }
}

fn is_placeholder(needle: []const u8, value: Value) bool {
    return switch (value) {
        .constant => false,
        .placeholder => |info| std.mem.eql(u8, info.name, needle),
    };
}

const log = std.log.scoped(.compile_arch);

const Processor = @This();
const Spec_Parser = @import("Spec_Parser.zig");
const Cycle = @import("Cycle.zig");
const Slot_Data = Microcode_Builder.Slot_Data;
const Slot_Location = Microcode_Builder.Slot_Location;
const Microcode_Builder = @import("Microcode_Builder.zig");
const Decode_ROM_Builder = @import("Decode_ROM_Builder.zig");
const Value = Instruction_Encoding.Value;
const Constraint = Instruction_Encoding.Constraint;
const Encoder = Instruction_Encoding.Encoder;
const Instruction_Encoding = isa.Instruction_Encoding;
const Encoded_Instruction = isa.Encoded_Instruction;
const isa = arch.isa;
const hw = arch.hw;
const arch = @import("lib_arch");
const TempAllocator = @import("TempAllocator");
const bits = @import("bits");
const std = @import("std");
