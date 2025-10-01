gpa: std.mem.Allocator,
arena: std.mem.Allocator,
temp: *Temp_Allocator,
microcode: Microcode_Builder,
decode_rom: Decode_ROM_Builder,
instruction_forms: std.ArrayList(isa.Instruction.Form),
form_slots: std.AutoHashMapUnmanaged(usize, std.ArrayList(arch.insn_decode.Address)), // key is index into instruction_forms

pub fn init(gpa: std.mem.Allocator, arena: std.mem.Allocator, temp: *Temp_Allocator) Processor {
    return .{
        .gpa = gpa,
        .arena = arena,
        .temp = temp,
        .microcode = .init(gpa),
        .decode_rom = .init(gpa),
        .instruction_forms = .empty,
        .form_slots = .empty,
    };
}

pub fn process(self: *Processor, comptime instruction_structs: anytype) void {
    @setEvalBranchQuota(10000);

    inline for (instruction_structs) |Struct| {
        self.temp.reset(.{});
        const alloc = self.temp.allocator();

        if (@hasDecl(Struct, "slot")) {
            log.debug("Beginning processing of {s}:\n    {}", .{ @typeName(Struct), Struct.slot });
        } else {
            log.debug("Beginning processing of {s}", .{ @typeName(Struct) });
        }

        // Microcode function enumeration
        var lookup = Slot_Info.Lookup.init(alloc);
        inline for (@typeInfo(Struct).@"struct".decls) |decl| {
            const T = @TypeOf(@field(Struct, decl.name));
            const info = @typeInfo(T);
            if (info == .@"fn" and info.@"fn".params.len > 0 and info.@"fn".params[0].type.? == *Cycle) {
                const ptr = &@field(Struct, decl.name);
                const result = lookup.getOrPut(ptr) catch @panic("OOM");
                if (!result.found_existing or result.value_ptr.is_entry) {
                    result.key_ptr.* = ptr;
                    result.value_ptr.* = Slot_Info.init(@field(Struct, decl.name), @typeName(Struct) ++ "." ++ decl.name);
                }
            }
        }

        if (@hasDecl(Struct, "forms")) {
            inline for (Struct.forms) |Form| {
                self.process_form(Form, Struct, &lookup);
            }
        } else if (@hasDecl(Struct, "encoding")) {
            self.process_form(Struct, Struct, &lookup);
        } else if (@hasDecl(Struct, "slot")) {
            const result = Microcode_Processor.process(.{
                .processor = self,
                .ptr = &Struct.entry,
                .ctx = .{
                    .allocator = self.temp.allocator(),
                    .cycle_flags = Cycle_Flag_Set.initEmpty(),
                    .initial_encoding = null,
                    .initial_dr = null,
                    .initial_krio = null,
                    .initial_wio = null,
                    .parsed_encoders = &.{},
                    .signature = null,
                    .encoding_len = null,
                    .cv_mode = .zero
                },
                .forced_slot = Struct.slot,
                .lookup = &lookup,
            });
            self.resolve_cycles(result.slot_handle, &lookup);
        } else {
            @compileLog(Struct);
            @compileError("Expected either encoding or slot declaration");
        }
    }
}

fn process_form(
    self: *Processor,
    comptime Form: type,
    comptime Outer: type,
    lookup: *Slot_Info.Lookup,
) void {
    const alloc = self.temp.allocator();

    const encoders_fn = comptime resolve_encoders(locate_decl(.{ Form, Outer }, "encoding").encoding);

    const search_types = .{ Form, Outer, Fallback_Form };

    const krio_fn = comptime resolve_encoders(locate_decl(search_types, "krio").krio);
    const wio_fn = comptime resolve_encoders(locate_decl(search_types, "wio").wio);
    const constraints = comptime resolve_constraints(locate_decl(search_types, "constraints").constraints);
    const Spec_Type = locate_decl(search_types, "spec");
    const entry = &locate_decl(search_types, "entry").entry;

    const cv_mode: arch.insn_decode.CV_Mode = switch (locate_decl(search_types, "Flags").Flags) {
        arch.microcode.Flags => .zero,
        arch.microcode.Flags_With_Carry => .c,
        arch.microcode.Flags_With_Overflow => .v,
        else => @compileError("Invalid Flags type"),
    };

    if (Spec_Type != Fallback_Form) {
        var parser = Spec_Parser.init(alloc, Spec_Type.spec);
        while (parser.next()) |parsed| {
            log.debug("Processing encoding: {s}", .{ parser.prev_line(Spec_Type.spec) });
            const encoders = encoders_fn(alloc, parsed.signature, parsed.encoders);
            const krio_encoders = krio_fn(alloc, parsed.signature, parsed.encoders);
            const wio_encoders = wio_fn(alloc, parsed.signature, parsed.encoders);

            const final_form = self.finalize_form(parsed, constraints, encoders);
            self.instruction_forms.append(self.gpa, final_form) catch @panic("OOM");

            self.process_instruction(entry, final_form, constraints, encoders, krio_encoders, wio_encoders, cv_mode, lookup);
        }
    } else {
        const encoders = encoders_fn(alloc, null, &.{});
        const krio_encoders = krio_fn(alloc, null, &.{});
        const wio_encoders = wio_fn(alloc, null, &.{});
        self.process_instruction(entry, null, constraints, encoders, krio_encoders, wio_encoders, cv_mode, lookup);
    }
}

fn locate_decl(comptime Types: anytype, comptime name: []const u8) type {
    for (Types) |T| {
        if (@hasDecl(T, name)) return T;
    }
    @compileError("Could not find " ++ name ++ " decl");
}

fn process_instruction(
    self: *Processor,
    entry_fn: *const anyopaque,
    maybe_form: ?isa.Instruction.Form,
    constraints: []const isa.Constraint,
    encoders: []const isa.Encoder,
    krio_encoders: []const isa.Encoder,
    wio_encoders: []const isa.Encoder,
    cv_mode: arch.insn_decode.CV_Mode,
    lookup: *Slot_Info.Lookup,
) void {
    var cycle_flags = Cycle_Flag_Set.initMany(&.{ .ir_valid, .dr_valid, .initial_dr1_valid });
    if (krio_encoders.len > 0) {
        cycle_flags.insert(.initial_krio_valid);
    }
    if (wio_encoders.len > 0) {
        cycle_flags.insert(.initial_wio_valid);
    }

    const encoding_len = if (maybe_form == null) null else encoding_length(encoders);

    var iter = Initial_Word_Encoding_Iterator.init(self.temp.allocator(), constraints, encoders);

    const undefined_ir_bits: arch.reg.IR.Raw = @truncate(iter.undefined_bits);
    if (undefined_ir_bits != 0 and undefined_ir_bits != 0xFF00) {
        std.debug.panic("Bits {x} of the initial instruction word are undefined!", .{ undefined_ir_bits });
    }

    if ((iter.undefined_bits & 0xFF_0000) == 0x00_0000) {
        cycle_flags.insert(.initial_dr2_valid);
    }

    var maybe_slot_handle: ?Slot_Data.Handle = null;
    while (iter.next()) |addr| {
        var maybe_krio: ?arch.bus.K.Read_Index_Offset = null;
        var maybe_wio: ?arch.reg.gpr.Write_Index_Offset = null;

        const slot_handle = maybe_slot_handle orelse handle: {
            const dr = arch.reg.DR.init(@intCast(iter.encode_ignore_errors(encoders)));
            maybe_krio = arch.bus.K.Read_Index_Offset.init(@intCast(iter.encode(krio_encoders, "KRIO")));
            maybe_wio = arch.reg.gpr.Write_Index_Offset.init_unsigned(@intCast(iter.encode(wio_encoders, "WIO")));
            const result = Microcode_Processor.process(.{
                .processor = self,
                .ptr = entry_fn,
                .ctx = .{
                    .allocator = self.temp.allocator(),
                    .cycle_flags = cycle_flags,
                    .initial_encoding = &iter,
                    .initial_dr = dr,
                    .initial_krio = maybe_krio,
                    .initial_wio = maybe_wio,
                    .signature = if (maybe_form) |form| form.signature else null,
                    .parsed_encoders = if (maybe_form) |form| form.encoders else &.{},
                    .encoding_len = encoding_len,
                    .cv_mode = cv_mode,
                },
                .forced_slot = null,
                .lookup = lookup,
            });
            self.resolve_cycles(result.slot_handle, lookup);

            if (!result.uses_placeholders) {
                maybe_slot_handle = result.slot_handle;
            }
            break :handle result.slot_handle;
        };

        const krio = maybe_krio orelse arch.bus.K.Read_Index_Offset.init(
            std.math.cast(arch.bus.K.Read_Index_Offset.Raw, iter.encode(krio_encoders, "KRIO")) orelse {
                if (maybe_form) |form| {
                    std.debug.print("Bad KRIO for instruction form: {f}\n", .{ form });
                }
                std.debug.panic("Bad KRIO: Value {} is not valid\n", .{ iter.encode(krio_encoders, "KRIO") });
            }
        );
        const wio = maybe_wio orelse arch.reg.gpr.Write_Index_Offset.init_unsigned(
            std.math.cast(arch.reg.gpr.Write_Index_Offset.Raw_Unsigned, iter.encode(wio_encoders, "WIO")) orelse {
                if (maybe_form) |form| {
                    std.debug.print("Bad WIO for instruction form: {f}\n", .{ form });
                }
                std.debug.panic("Bad WIO: Value {} is not valid\n", .{ iter.encode(wio_encoders, "WIO") });
            }
        );

        self.decode_rom.add_entry(addr, @truncate(iter.undefined_bits), .{
            .form = maybe_form,
            .slot_handle = slot_handle,
            .wio = wio,
            .krio = krio,
            .cv = cv_mode,
        });

        const form_index = self.instruction_forms.items.len - 1;
        const result = self.form_slots.getOrPut(self.gpa, form_index) catch @panic("OOM");
        if (!result.found_existing) {
            result.key_ptr.* = form_index;
            result.value_ptr.* = .empty;
        }
        result.value_ptr.append(self.gpa, addr) catch @panic("OOM");
    }
}

fn resolve_constraints(comptime constraints: anytype) []const isa.Constraint {
    switch (@typeInfo(@TypeOf(constraints))) {
        .@"struct" => |info| if (info.is_tuple) {
            comptime var out: [constraints.len]isa.Constraint = undefined;
            inline for (constraints, &out) |in, *constraint| {
                constraint.* = comptime resolve_single_constraint(in);
            }

            const final_out = out;
            return &final_out;
        },
        else => {},
    }

    return &.{ comptime resolve_single_constraint(constraints) };
}

fn resolve_single_constraint(comptime constraint: anytype) isa.Constraint {
    const T = @TypeOf(constraint);
    if (T == isa.Constraint) return constraint;
    switch (@typeInfo(T)) {
        .@"struct" => |info| {
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
            const kind: isa.Constraint.Kind = switch (ext_kind) {
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

fn resolve_constraint_value(comptime value: anytype) isa.Encoder.Value {
    const T = @TypeOf(value);
    if (T == isa.Encoder.Value) return value;
    switch (@typeInfo(T)) {
        .int, .comptime_int => {
            return .{ .constant = @intCast(value) };
        },
        .enum_literal => {
            return .{ .placeholder = .{
                .param = .invalid,
                .kind = .param_constant,
                .name = @tagName(value),
            }};
        },
        else => @compileError("Expected integer, enum literal, or Value"),
    }
}

const Encoder_Provider = *const fn (allocator: std.mem.Allocator, signature: ?isa.Instruction.Signature, parsed_encoders: []const isa.Encoder) []const isa.Encoder;

fn resolve_encoders(comptime encoders: anytype) Encoder_Provider {
    return struct {
        pub fn provider(allocator: std.mem.Allocator, signature: ?isa.Instruction.Signature, parsed_encoders: []const isa.Encoder) []const isa.Encoder {
            switch (@typeInfo(@TypeOf(encoders))) {
                .@"struct" => |info| if (info.is_tuple) {
                    const out = allocator.alloc(isa.Encoder, encoders.len) catch @panic("OOM");
                    inline for (encoders, out) |in, *encoder| {
                        encoder.* = resolve_single_encoder(in, signature, parsed_encoders);
                    }
                    return out;
                },
                else => {},
            }

            var out = allocator.alloc(isa.Encoder, 1) catch @panic("OOM");
            out[0] = resolve_single_encoder(encoders, signature, parsed_encoders);
            return out;
        }
    }.provider;
}

fn resolve_single_encoder(encoder: anytype, signature: ?isa.Instruction.Signature, parsed_encoders: []const isa.Encoder) isa.Encoder {
    const T = @TypeOf(encoder);
    if (T == isa.Encoder) return encoder;
    switch (@typeInfo(T)) {
        .@"fn" => {
            var args: std.meta.ArgsTuple(T) = undefined;
            inline for (&args) |*a| {
                const Arg = @TypeOf(a.*);
                if (Arg == isa.Instruction.Signature) {
                    a.* = signature.?;
                } else if (Arg == isa.Mnemonic) {
                    a.* = signature.?.mnemonic;
                } else if (Arg == isa.Mnemonic_Suffix) {
                    a.* = signature.?.suffix;
                } else if (Arg == []const isa.Parameter.Signature) {
                    a.* = signature.?.params;
                } else if (@hasField(Arg, "signature")) {
                    if (@hasDecl(Arg, "index")) {
                        if (Arg.index.raw() >= signature.?.params.len) {
                            @panic("Param index out of range");
                        }
                        a.* = .{ .signature = signature.?.params[Arg.index.raw()] };
                    } else for (parsed_encoders) |enc| {
                        const placeholder_info = enc.value.placeholder;
                        if (placeholder_info.index != .invalid and std.mem.eql(u8, placeholder_info.name, Arg.placeholder)) {
                            a.* = .{ .signature = signature.?.params[placeholder_info.index.raw()] };
                            break;
                        }
                    } else {
                        std.debug.panic("Failed to find placeholder '{s}'", .{ Arg.placeholder });
                    }
                }
            }
            return resolve_single_encoder(@call(.auto, encoder, args), signature, parsed_encoders);
        },
        else => return isa.Encoder.init(0, encoder),
    }
}

fn encoding_length(encoders: []const isa.Encoder) isa.Instruction.Encoded.Length_Bytes {
    var len: isa.Instruction.Encoded.Length_Bytes = 0;
    for (encoders) |encoder| {
        const bytes: isa.Instruction.Encoded.Length_Bytes = @intCast(encoder.required_bits() / 8);
        len = @max(len, bytes);
    }
    return len;
}

pub const Initial_Word_Encoding_Iterator = struct {
    first: bool = true,
    undefined_bits: arch.reg.DR.Raw,
    last_encoded: arch.reg.IR.Raw,
    constraints: []const isa.Constraint,
    value_iters: []isa.Encoder.Value_Iterator,

    pub fn init(allocator: std.mem.Allocator, constraints: []const isa.Constraint, encoders: []const isa.Encoder) Initial_Word_Encoding_Iterator {
        const out = allocator.alloc(isa.Encoder.Value_Iterator, encoders.len) catch @panic("OOM");
        var undefined_bits = ~@as(arch.reg.DR.Raw, 0);
        var n: usize = 0;
        for (encoders, 0..) |*encoder, i| {
            const mask = encoder.bit_mask();
            for (encoders[0..i], 0..) |other_encoder, j| {
                if ((mask & other_encoder.bit_mask()) != 0) {
                    std.debug.panic("Encoder #{}'s bit mask overlaps with encoder #{}", .{ i, j });
                }
            }

            var encoder_bits: arch.reg.DR.Raw = @truncate(mask);
            log.debug("Encoder #{}'s bit mask: {x}", .{ i, encoder_bits });
            if (!encoder.value.is_constant()) {
                encoder_bits &= ~@as(arch.reg.IR.Raw, 0);
            }
            undefined_bits &= ~encoder_bits;

            if (encoder.bit_offset < @bitSizeOf(arch.reg.IR)) {
                out[n] = encoder.value_iterator();
                n += 1;
            }
        }
        return .{
            .first = true,
            .undefined_bits = undefined_bits,
            .last_encoded = 0,
            .constraints = constraints,
            .value_iters = out[0..n],
        };
    }

    pub fn next(self: *Initial_Word_Encoding_Iterator) ?arch.insn_decode.Address {
        while (self.next_internal()) |val| {
            for (self.constraints) |constraint| {
                if (!self.constraint_matches(constraint)) break;
            } else return val;
        }
        return null;
    }

    fn next_internal(self: *Initial_Word_Encoding_Iterator) ?arch.insn_decode.Address {
        if (self.first) {
            var encoded: isa.Instruction.Encoded.Data = 0;
            for (self.value_iters) |*value_iter| {
                value_iter.reset();
                std.debug.assert(value_iter.next() != null);
                std.debug.assert(value_iter.encoder.encode_value(value_iter.last_value, &encoded));
            }
            self.first = false;
            const encoded_ir = arch.reg.IR.init(@truncate(encoded));
            self.last_encoded = encoded_ir.raw();
            return encoded_ir;
        }

        for (self.value_iters, 0..) |*value_iter, i| {
            if (value_iter.next() != null) {
                for (0..i) |j| {
                    self.value_iters[j].reset();
                    std.debug.assert(self.value_iters[j].next() != null);
                }
                var encoded: isa.Instruction.Encoded.Data = 0;
                for (self.value_iters) |iter| {
                    std.debug.assert(iter.encoder.encode_value(iter.last_value, &encoded));
                }
                const encoded_ir = arch.reg.IR.init(@truncate(encoded));
                self.last_encoded = encoded_ir.raw();
                return encoded_ir;
            }
        }
        return null;
    }

    fn constraint_matches(self: *Initial_Word_Encoding_Iterator, constraint: isa.Constraint) bool {
        const left = self.constraint_value(constraint.left) orelse return true;
        const right = self.constraint_value(constraint.right) orelse return true;
        return switch (constraint.kind) {
            .equal => left == right,
            .not_equal => left != right,
            .greater => left > right,
            .greater_or_equal => left >= right,
        };
    }

    fn constraint_value(self: Initial_Word_Encoding_Iterator, val: isa.Encoder.Value) ?i64 {
        return switch (val) {
            .constant => |k| k,
            .placeholder => |info| self.value(info.name),
            .negate => |inner| self.constraint_value(inner.*), // This doesn't apply its transformation because self.value() already accounts for it.
            .xor => |info| self.constraint_value(info.inner.*), // This doesn't apply its transformation because self.value() already accounts for it.
            .offset => |info| self.constraint_value(info.inner.*), // This doesn't apply its transformation because self.value() already accounts for it.
        };
    }

    pub fn value(self: Initial_Word_Encoding_Iterator, placeholder: []const u8) ?i64 {
        for (self.value_iters) |iter| {
            if (get_placeholder(iter.encoder.value)) |info| {
                if (std.mem.eql(u8, placeholder, info.name)) {
                    return iter.last_value;
                }
            }
        }
        return null;
    }

    pub fn encode(self: Initial_Word_Encoding_Iterator, encoders: []const isa.Encoder, context: []const u8) isa.Instruction.Encoded.Data {
        return self.maybe_encode(encoders) orelse std.debug.panic("An encoder for {s} references a placeholder that is not encoded in the initial word of the instruction", .{ context });
    }

    pub fn maybe_encode(self: Initial_Word_Encoding_Iterator, encoders: []const isa.Encoder) ?isa.Instruction.Encoded.Data {
        var data: isa.Instruction.Encoded.Data = 0;
        for (encoders) |encoder| {
            const v = self.constraint_value(encoder.value) orelse return null;
            std.debug.assert(encoder.encode_value(v, &data));
        }
        return data;
    }

    pub fn encode_ignore_errors(self: Initial_Word_Encoding_Iterator, encoders: []const isa.Encoder) isa.Instruction.Encoded.Data {
        var data: isa.Instruction.Encoded.Data = 0;
        for (encoders) |encoder| {
            const v = self.constraint_value(encoder.value) orelse continue;
            std.debug.assert(encoder.encode_value(v, &data));
        }
        return data;
    }
};

fn resolve_cycles(self: *Processor, slot_handle: Slot_Data.Handle, lookup: *const Slot_Info.Lookup) void {
    const slot_data = self.microcode.slot_data.items[@intFromEnum(slot_handle)];
    if (slot_data.remaining_cycles.max != .unknown_cyclical) return;

    next_cycle: for (slot_data.cycles, 0..) |cycle_handle, i| {
        for (slot_data.cycles[0..i]) |prev_cycle_handle| {
            if (cycle_handle == prev_cycle_handle) continue :next_cycle;
        }

        const cycle = &self.microcode.cycles.items[@intFromEnum(cycle_handle)];
        if (cycle.next_func) |next_func| {
            const next = lookup.get(next_func).?;
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
    ctx: Slot_Info.Impl_Context,
    forced_slot: ?arch.microcode.Slot,
    lookup: *Slot_Info.Lookup,

    pub const Result = struct {
        slot_handle: Slot_Data.Handle,
        uses_placeholders: bool,
    };

    pub fn process(self: Microcode_Processor) Result {
        const gop = self.lookup.getOrPut(self.ptr) catch @panic("Microcode function not found");
        if (!gop.found_existing) @panic("Found c.next() for a function not puplicly visible within the instruction struct");
        const slot_info = gop.value_ptr.*;
        const cycles = slot_info.impl(self.ctx);
        // log.debug("Processed microcode function {s}", .{ cycles[0].func_name });

        var uses_placeholders = slot_info.uses_placeholders;
        var cycle_recursion = [_]?*const anyopaque { null } ** arch.microcode.Address.count_per_slot;
        var slot_data: Slot_Data = .{
            .cycles = undefined,
            .forced_slot = self.forced_slot,
            .remaining_cycles = .{},
        };
        for (0.., cycles) |i, *cycle| {
            if (cycle.next_func) |next_func_ptr| {
                if (self.has_processed(next_func_ptr)) {
                    slot_data.remaining_cycles.max = .unknown_cyclical;
                } else {
                    // Except in the case of loops, we will wipe out cycle.next_func after recursively computing the next slot handle.
                    // But to avoid excessive fan-out of descendant cycles (for slots using flags) we keep track of the original
                    // next_func so that we can see if this cycle is a duplicate of one we just processed, without needing to
                    // recursively process next_func.
                    cycle_recursion[i] = next_func_ptr;
                    for (0..i) |j| {
                        if (cycle_recursion[j] == cycle_recursion[i]) {
                            // No need to recursively process next_func, it's a duplicate of another cycle already processed for this slot.
                            cycle.next_slot = cycles[j].next_slot;
                            cycle.next_func = null;
                            break;
                        }
                    } else {
                        var ctx = self.ctx;
                        ctx.cycle_flags = cycle.flags;
                        const result = Microcode_Processor.process(.{
                            .processor = self.processor,
                            .prev = &self,
                            .ptr = next_func_ptr,
                            .ctx = ctx,
                            .forced_slot = null,
                            .lookup = self.lookup,
                        });
                        if (result.uses_placeholders) uses_placeholders = true;
                        cycle.next_slot = result.slot_handle;
                        cycle.next_func = null;
                        const result_slot_data = self.processor.microcode.get_slot_data(result.slot_handle);

                        slot_data.remaining_cycles.merge(result_slot_data.remaining_cycles.inc());
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
        return .{
            .slot_handle = slot_handle,
            .uses_placeholders = uses_placeholders,
        };
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
    uses_placeholders: bool,
    is_entry: bool,
    slot: Slot_Data.Handle, // filled in by build_microcode

    pub const Lookup = std.AutoHashMap(*const anyopaque, Slot_Info);

    pub const Impl = *const fn (ctx: Impl_Context) []Cycle;
    pub const Impl_Context = struct {
        allocator: std.mem.Allocator,
        cycle_flags: Cycle_Flag_Set,
        initial_encoding: ?*const Initial_Word_Encoding_Iterator,
        initial_dr: ?arch.reg.DR,
        initial_krio: ?arch.bus.K.Read_Index_Offset,
        initial_wio: ?arch.reg.gpr.Write_Index_Offset,
        signature: ?isa.Instruction.Signature,
        parsed_encoders: []const isa.Encoder,
        encoding_len: ?isa.Instruction.Encoded.Length_Bytes,
        cv_mode: arch.insn_decode.CV_Mode,
    };

    pub fn init(comptime func: anytype, comptime name: []const u8) Slot_Info {
        const Func = @TypeOf(func);
        const Args = std.meta.ArgsTuple(Func);
        const params = @typeInfo(Func).@"fn".params;

        comptime var has_flags = false;
        comptime var uses_placeholders = false;
        inline for (params) |arg| {
            const Arg = arg.type.?;
            if (Arg == arch.microcode.Flags) {
                has_flags = true;
            } else if (@typeInfo(Arg) == .@"struct" and @hasField(Arg, "value") and @hasDecl(Arg, "placeholder")) {
                uses_placeholders = true;
            }
        }

        const is_entry = comptime std.mem.endsWith(u8, name, ".entry");
        const final_name = if (comptime std.mem.startsWith(u8, name, "handlers.")) name["handlers.".len..]
            else if (comptime std.mem.startsWith(u8, name, "instructions.")) name["instructions.".len..]
            else name;

        const temp = struct {
            pub fn unconditional(ctx: Impl_Context) []Cycle {
                const cycle = ctx.allocator.create(Cycle) catch @panic("OOM");
                const encoding_len = if (ctx.encoding_len) |len| len else null;
                const dr = ctx.initial_dr orelse arch.reg.DR.init(0);
                const krio = ctx.initial_krio orelse arch.bus.K.Read_Index_Offset.init(0);
                const wio = ctx.initial_wio orelse arch.reg.gpr.Write_Index_Offset.init(0);
                cycle.* = Cycle.init(final_name, ctx.signature, dr, krio, wio, encoding_len, ctx.cycle_flags);
                @call(.auto, func, build_args(cycle, arch.microcode.Flags.init(0), ctx));
                cycle.finish();
                return @as(*[1]Cycle, cycle);
            }

            pub fn conditional(ctx: Impl_Context) []Cycle {
                const cycles = ctx.allocator.alloc(Cycle, arch.microcode.Address.count_per_slot) catch @panic("OOM");
                const encoding_len = if (ctx.encoding_len) |len| len else null;
                const dr = ctx.initial_dr orelse arch.reg.DR.init(0);
                const krio = ctx.initial_krio orelse arch.bus.K.Read_Index_Offset.init(0);
                const wio = ctx.initial_wio orelse arch.reg.gpr.Write_Index_Offset.init(0);
                for (cycles, 0..) |*cycle, raw_flags| {
                    cycle.* = Cycle.init(final_name, ctx.signature, dr, krio, wio, encoding_len, ctx.cycle_flags);
                    @call(.auto, func, build_args(cycle, arch.microcode.Flags.init(@intCast(raw_flags)), ctx));
                    cycle.finish();
                }
                return cycles;
            }

            fn build_args(cycle: *Cycle, flags: arch.microcode.Flags, ctx: Impl_Context) Args {
                var args: Args = undefined;
                inline for (&args) |*a| {
                    const Arg = @TypeOf(a.*);
                    if (Arg == *Cycle) {
                        a.* = cycle;
                    } else if (Arg == arch.microcode.Flags) {
                        a.* = flags;
                    } else if (Arg == arch.microcode.Flags_With_Carry) {
                        switch (ctx.cv_mode) {
                            .v => @panic("Use arch.microcode.Flags or arch.microcode.Flags_With_Overflow instead!"),
                            .c => a.* = arch.microcode.Flags_With_Carry.init(flags.raw()),
                            else => @panic("Use arch.microcode.Flags instead!"),
                        }
                    } else if (Arg == arch.microcode.Flags_With_Overflow) {
                        switch (ctx.cv_mode) {
                            .v => a.* = arch.microcode.Flags_With_Overflow.init(flags.raw()),
                            .c => @panic("Use arch.microcode.Flags or arch.microcode.Flags_With_Carry instead!"),
                            else => @panic("Use arch.microcode.Flags instead!"),
                        }
                    } else if (Arg == isa.Instruction.Signature) {
                        a.* = ctx.signature.?;
                    } else if (Arg == isa.Mnemonic) {
                        a.* = ctx.signature.?.mnemonic;
                    } else if (Arg == []const isa.Parameter.Signature) {
                        a.* = ctx.signature.?.params;
                    } else if (Arg == arch.bus.K.Read_Index_Offset) {
                        a.* = ctx.initial_krio.?;
                    } else if (Arg == arch.Write_Index) {
                        a.* = ctx.initial_wio.?;
                    } else {
                        if (@hasField(Arg, "signature")) {
                            if (@hasDecl(Arg, "index")) {
                                if (Arg.index.raw() >= ctx.signature.?.params.len) {
                                    @panic("Param index out of range");
                                }
                                a.* = Arg { .signature = ctx.signature.?.params[Arg.index.raw()] };
                            } else {
                                a.* = Arg { .signature = for (ctx.parsed_encoders) |enc| {
                                    if (enc.value == .placeholder) {
                                        const placeholder_info = enc.value.placeholder;
                                        if (placeholder_info.index != .invalid and std.mem.eql(u8, placeholder_info.name, Arg.placeholder)) {
                                            break ctx.signature.?.params[placeholder_info.index.raw()];
                                        }
                                    }
                                } else {
                                    std.debug.panic("Placeholder {s} not found!", .{ Arg.placeholder });
                                }};
                            }
                        } else if (ctx.initial_encoding) |encoding| {
                            if (encoding.value(Arg.placeholder)) |value| {
                                a.* = Arg { .value = value };
                            } else {
                                std.debug.panic("Placeholder {s} is not found within the initial word of this instruction's encoding", .{ Arg.placeholder });
                            }
                        } else {
                            std.debug.panic("Placeholder {s} is not available because this microcode sequence doesn't have an associated encoding", .{ Arg.placeholder });
                        }
                    }
                }
                return args;
            }
        };

        return .{
            .impl = if (has_flags) &temp.conditional else &temp.unconditional,
            .is_entry = is_entry,
            .uses_placeholders = uses_placeholders,
            .slot = undefined,
        };
    }
};

fn finalize_form(self: *Processor, parsed: isa.Instruction.Form, constraints: []const isa.Constraint, encoders: []const isa.Encoder) isa.Instruction.Form {
    var final_encoders = self.arena.dupe(isa.Encoder, encoders) catch @panic("OOM");
    for (final_encoders) |*encoder| {
        dupe_inner_values(self.arena, &encoder.value);
        fixup_placeholder_value(&encoder.value, parsed.encoders);
    }

    const final_constraints = self.arena.alloc(isa.Constraint, parsed.constraints.len + constraints.len) catch @panic("OOM");
    @memcpy(final_constraints.ptr, parsed.constraints);
    @memcpy(final_constraints[parsed.constraints.len..], constraints);

    for (final_constraints) |*constraint| {
        dupe_inner_values(self.arena, &constraint.left);
        dupe_inner_values(self.arena, &constraint.right);
        fixup_placeholder_value(&constraint.left, parsed.encoders);
        fixup_placeholder_value(&constraint.right, parsed.encoders);
    }

    for (parsed.encoders) |parsed_encoder| {
        const placeholder = parsed_encoder.value.placeholder.name;

        if (find_placeholder(placeholder, final_encoders) != null) continue;

        for (final_constraints) |constraint| {
            if (constraint.kind != .equal) continue;
            if (is_placeholder(placeholder, constraint.left)) {
                if (get_placeholder_name(constraint.right)) |other_placeholder| {
                    if (find_placeholder(other_placeholder, final_encoders) != null) break;
                }
            } else if (is_placeholder(placeholder, constraint.right)) {
                if (get_placeholder_name(constraint.left)) |other_placeholder| {
                    if (find_placeholder(other_placeholder, final_encoders) != null) break;
                }
            }
        } else {
            std.debug.panic("Placeholder '{s}' is not encoded or constrained equal to an encoded placeholder", .{ placeholder });
        }
    }

    validate_constant_values(parsed, final_encoders);

    final_encoders = merge_adjacent_constant_encoders(final_encoders);

    return .{
        .signature = self.finalize_instruction_signature(parsed.signature),
        .constraints = final_constraints,
        .encoders = final_encoders,
    };
}

fn validate_constant_values(parsed: isa.Instruction.Form, encoders: []isa.Encoder) void {
    for (encoders) |encoder| {
        if (encoder.value != .constant) continue;
        var data: isa.Instruction.Encoded.Data = 0;
        if (!encoder.encode_value(encoder.value.constant, &data)) {
            std.debug.panic("Can't encode constant {d} for instruction encoding: {f}", .{
                encoder.value.constant,
                parsed,
            });
        }
    }
}

fn merge_adjacent_constant_encoders(encoders: []isa.Encoder) []isa.Encoder {
    if (encoders.len <= 1) return encoders;

    std.sort.block(isa.Encoder, encoders, {}, struct {
        pub fn less_than(_: void, a: isa.Encoder, b: isa.Encoder) bool {
            return a.bit_offset < b.bit_offset;
        } 
    }.less_than);

    var out_index: usize = 0;
    for (encoders[1..]) |second| {
        const first = encoders[out_index];
        if (first.value != .constant or second.value != .constant or first.bit_offset + first.bit_count != second.bit_offset) {
            out_index += 1;
            encoders[out_index] = second;
            continue;
        }

        var data: isa.Instruction.Encoded.Data = 0;

        const ok1 = first.encode_value(first.value.constant, &data);
        std.debug.assert(ok1);

        const ok2 = second.encode_value(second.value.constant, &data);
        std.debug.assert(ok2);
        
        var temp: isa.Encoder = .{
            .bit_offset = first.bit_offset,
            .bit_count = first.bit_count + second.bit_count,
            .domain = .{ .int = .{
                .signedness = .unsigned,
                .bits = @intCast(first.bit_count + second.bit_count),
                .multiple = 1,
            }},
            .value = .{ .placeholder = .{
                .param = .invalid,
                .kind =  .param_constant,
                .name = "",
            }},
        };
        const constant_value = temp.decode_value(data) orelse unreachable;
        temp.value = .{ .constant = constant_value };

        encoders[out_index] = temp;
    }

    return encoders[0..out_index + 1];
}

fn finalize_instruction_signature(self: *Processor, signature: isa.Instruction.Signature) isa.Instruction.Signature {
    const final_param_signatures = self.arena.dupe(isa.Parameter.Signature, signature.params) catch @panic("OOM");
    return .{
        .mnemonic = signature.mnemonic,
        .params = final_param_signatures,
    };
}

fn dupe_inner_values(arena: std.mem.Allocator, value: *isa.Encoder.Value) void {
    switch (value.*) {
        .constant, .placeholder => {},
        .negate => |inner| {
            const new_inner = arena.create(isa.Encoder.Value) catch @panic("OOM");
            new_inner.* = inner.*;
            dupe_inner_values(arena, new_inner);
            value.* = .{ .negate = new_inner };
        },
        .xor => |*info| {
            const new_inner = arena.create(isa.Encoder.Value) catch @panic("OOM");
            new_inner.* = info.inner.*;
            dupe_inner_values(arena, new_inner);
            info.inner = new_inner;
        },
        .offset => |*info| {
            const new_inner = arena.create(isa.Encoder.Value) catch @panic("OOM");
            new_inner.* = info.inner.*;
            dupe_inner_values(arena, new_inner);
            info.inner = new_inner;
        },
    }
}

fn fixup_placeholder_value(value: *isa.Encoder.Value, parsed_encoders: []const isa.Encoder) void {
    switch (value.*) {
        .constant => {},
        .placeholder => |*info| fixup_placeholder(info, parsed_encoders),
        .negate => |inner| fixup_placeholder_value(@constCast(inner), parsed_encoders), // TODO refactor to avoid @constCast
        .xor => |info| fixup_placeholder_value(@constCast(info.inner), parsed_encoders), // TODO refactor to avoid @constCast
        .offset => |info| fixup_placeholder_value(@constCast(info.inner), parsed_encoders), // TODO refactor to avoid @constCast
    }
}

fn fixup_placeholder(info: *isa.Placeholder, parsed_encoders: []const isa.Encoder) void {
    if (info.param == .invalid) {
        if (find_placeholder(info.name, parsed_encoders)) |parsed_info| {
            info.param = parsed_info.param;
            info.kind = parsed_info.kind;
        } else if (!std.mem.eql(u8, info.name, "__")) {
            std.debug.panic("Encoder {s} not found in parsed instruction", .{ info.name });
        }
    }
}

fn find_placeholder(needle: []const u8, haystack: []const isa.Encoder) ?isa.Placeholder {
    for (haystack) |encoder| {
        if (get_placeholder(encoder.value)) |info| {
            if (std.mem.eql(u8, info.name, needle)) return info;
        }
    }
    return null;
}

fn get_placeholder_name(value: isa.Encoder.Value) ?[]const u8 {
    return switch (value) {
        .constant => null,
        .placeholder => |info| info.name,
        .negate => |inner| get_placeholder_name(inner.*),
        .xor => |info| get_placeholder_name(info.inner.*),
        .offset => |info| get_placeholder_name(info.inner.*),
    };
}

fn get_placeholder(val: isa.Encoder.Value) ?isa.Placeholder {
    return switch (val) {
        .constant => null,
        .placeholder => |info| info,
        .negate => |inner| get_placeholder(inner.*),
        .xor => |info| get_placeholder(info.inner.*),
        .offset => |info| get_placeholder(info.inner.*),
    };
}

fn is_placeholder(needle: []const u8, value: isa.Encoder.Value) bool {
    return switch (value) {
        .constant => false,
        .placeholder => |info| std.mem.eql(u8, info.name, needle),
        .negate => |inner| is_placeholder(needle, inner.*),
        .xor => |info| is_placeholder(needle, info.inner.*),
        .offset => |info| is_placeholder(needle, info.inner.*),
    };
}

const Fallback_Form = struct {
    pub const spec = "";
    pub const encoders = .{};
    pub const krio = .{};
    pub const wio = .{};
    pub const constraints = .{};
    pub const Flags = arch.microcode.Flags;
};

const log = std.log.scoped(.compile);

const Processor = @This();
const Spec_Parser = @import("Spec_Parser.zig");
const Cycle_Flag_Set = std.EnumSet(Cycle.Cycle_Flags);
const Cycle = @import("Cycle.zig");
const Slot_Data = Microcode_Builder.Slot_Data;
const Microcode_Builder = @import("Microcode_Builder.zig");
const Decode_ROM_Builder = @import("Decode_ROM_Builder.zig");
const isa = @import("isa");
const arch = @import("arch");
const Temp_Allocator = @import("Temp_Allocator");
const bits = @import("bits");
const std = @import("std");
