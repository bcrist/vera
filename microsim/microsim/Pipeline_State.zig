next_stage: arch.Pipeline.Stage,
cs: arch.Control_Signals = .zeroes,
flags: std.EnumSet(Flag) = std.EnumSet(Flag).initEmpty(),
debug_log: ?*Debug_Log,

// persistent state/registers
status: arch.reg.Status,
emode: arch.Execution_Mode = .interrupt,
dr: arch.reg.DR = .init(0),
ir: arch.reg.IR = .init(0),
ti: arch.reg.gpr.Write_Index = .init(0),
wi: arch.reg.gpr.Write_Index = .init(0),
krio: arch.bus.K.Read_Index_Offset = .init(0),
kri: arch.bus.K.Read_Index = .init(0),
asn6: at.Entry.ASN6 = .init(0),
vabor: arch.addr.Virtual = .init(0), // fault virtual address / bus override register

// non-persistent state
uca: arch.microcode.Address = .init(0),
last_d: arch.bus.D = .init(0),
j: arch.bus.J = arch.bus.J.init(0),
k: arch.bus.K = arch.bus.K.init(0),
sr1d: arch.reg.sr1.Value = .init(0),
sr2d: arch.reg.sr2.Value = .init(0),
compute_result: Compute_Result = .zeroes,
va: arch.addr.Virtual = .init(0),
at_entry_addr: at.Entry.Address = .init(0),
matching_entry: at.Entry = .init(0),
other_entry: at.Entry = .init(0),
frame: arch.addr.Frame = .init(0),
aa: arch.addr.Frame.Word_Offset = .init(0),
ab: arch.addr.Frame.Word_Offset = .init(0),
da: arch.bus.DA = .init(0),
db: arch.bus.DB = .init(0),

pub const Flag = enum {
    carry,
    overflow,
    negative,
    zero,
    super,
    at_enable,
    at_super, // note this is non-transient because the next instruction need not be loaded just before it is executed
    bus_override,

    // at_super mutators:
    add_at_super,
    remove_at_super,

    // potential faults
    kri_underflow,
    wi_underflow,
    wi_overflow,

    // faults
    page_fault,
    access_fault,
    pipe_fault,
    page_align_fault,
    align_fault,
    overflow_fault,
    insn_fault,
    rs_underflow_fault,
    rs_overflow_fault,

    any_fault,

    // bus signals
    lba,
    uba,
    lbb,
    ubb,
    read,
    write,
    guard_mismatch,
};

pub const fault_flags = std.EnumSet(Flag).initMany(&.{
    .page_fault,
    .access_fault,
    .pipe_fault,
    .page_align_fault,
    .align_fault,
    .overflow_fault,
    .insn_fault,
    .rs_underflow_fault,
    .rs_overflow_fault,
});

fn fault_slot(self: *Pipeline_State, flag: Flag) arch.microcode.Slot {
    return switch (flag) {
        .insn_fault => self.cs.next,
        inline else => |f| if (@hasField(arch.microcode.Slot, @tagName(f))) @field(arch.microcode.Slot, @tagName(f)) else unreachable,
    };
}

const transient_flags = std.EnumSet(Flag).initMany(&.{
    .add_at_super,
    .remove_at_super,
    .guard_mismatch,
    .any_fault,
    .lba,
    .uba,
    .lbb,
    .ubb,
    .read,
    .write,
    .kri_underflow,
    .wi_underflow,
    .wi_overflow,
}).unionWith(fault_flags);

pub fn init(pipe: arch.Pipeline, initial_stage: arch.Pipeline.Stage, debug_log: ?*Debug_Log) Pipeline_State {
    return .{
        .next_stage = initial_stage,
        .debug_log = debug_log,
        .status = .{
            .rsn = .init(0),
            .pipe = pipe,
            .fwrite = false,
            .fwidth = .init(0),
            .fspace = .init(0),
            .fucs = .init(0),
        },
    };
}

inline fn report(self: *Pipeline_State, action: Debug_Log.Action) void {
    if (self.debug_log) |dl| {
        dl.report(self.status.pipe, self.uca.slot, action);
    }
}

const Compute_Result = struct {
    value: arch.bus.L,
    cout: bool,
    vout: bool,

    pub const zeroes: Compute_Result = .{
        .value = .init(0),
        .cout = false,
        .vout = false,
    };
};

pub fn simulate_decode(self: *Pipeline_State,
    insn_decode_rom: *const arch.insn_decode.Rom,
    microcode_rom: *const arch.microcode.Rom,
    pending_interrupt: bool,
    reset: bool,
) void {
    std.debug.assert(self.next_stage == .decode);

    const flags = self.flags;
    const id_result = insn_decode_rom[self.ir.raw()];

    const seq_result = simulate_sequencer(
        self.cs.seq_op,
        self.cs.allow_int,
        self.emode,
        pending_interrupt,
        reset,
        flags,
    );

    const next_uca: arch.microcode.Address = .{
        .flags = .{
            .z = flags.contains(.zero),
            .n = flags.contains(.negative),
            .k = flags.contains(.super),
            .cv = switch (id_result.cv) {
                .zero => false,
                .one => true,
                .c => flags.contains(.carry),
                .v => flags.contains(.overflow),
            },
        },
        .slot = switch (seq_result.slot_src) {
            .fucs => self.status.fucs,
            .continuation =>  self.cs.next,
            .seq_literal => seq_result.slot_literal.slot(),
            .insn_decoder => id_result.entry,
        },
    };

    const ti_raw = self.ti.raw();

    const krio_raw = id_result.krio.raw();
    const kri_raw = ti_raw -% krio_raw;

    const wio_raw = id_result.wio.raw();
    const wi_raw = @as(i32, ti_raw) + wio_raw;
    
    self.flags = self.flags.differenceWith(transient_flags);

    if (ti_raw < krio_raw) {
        self.flags.insert(.kri_underflow);
    }

    if (wi_raw > arch.reg.gpr.Write_Index.max) {
        self.flags.insert(.wi_overflow);
    } else if (wi_raw < 0) {
        self.flags.insert(.wi_underflow);
    }

    self.kri = .init(kri_raw);
    self.krio = .init(krio_raw);
    self.wi = .init(@truncate(@as(u32, @bitCast(wi_raw))));
    self.uca = next_uca;
    self.cs = microcode_rom[next_uca.raw()];
    self.emode = seq_result.emode;
    self.next_stage = .setup;
}

const Sequencer_Result = struct {
    slot_literal: arch.microcode.Slot.Sequencer_Literal,
    slot_src: arch.microcode.Slot.Source,
    emode: arch.Execution_Mode,
};

fn simulate_sequencer(
    op: arch.Sequencer_Op,
    allow_int: arch.Interrupt_Enable,
    emode: arch.Execution_Mode,
    interrupt_pending: bool,
    reset: bool,
    flags: std.EnumSet(Flag),
) Sequencer_Result {
    return if (reset) .{
        .slot_literal = .from_slot(.reset),
        .slot_src = .seq_literal,
        .emode = .interrupt,
    } else if (flags.contains(.any_fault)) result: {
        break :result if (emode.is_fault()) .{
            .slot_literal = .from_slot(.double_fault),
            .slot_src = .seq_literal,
            .emode = .fault,
        } else if (flags.contains(.page_fault)) .{
            .slot_literal = .from_slot(.page_fault),
            .slot_src = .seq_literal,
            .emode = if (emode.is_interrupt()) .interrupt_fault else .fault,
        } else if (flags.contains(.access_fault)) .{
            .slot_literal = .from_slot(.access_fault),
            .slot_src = .seq_literal,
            .emode = if (emode.is_interrupt()) .interrupt_fault else .fault,
        } else if (flags.contains(.pipe_fault)) .{
            .slot_literal = .from_slot(.pipe_fault),
            .slot_src = .seq_literal,
            .emode = if (emode.is_interrupt()) .interrupt_fault else .fault,
        } else if (flags.contains(.page_align_fault)) .{
            .slot_literal = .from_slot(.page_align_fault),
            .slot_src = .seq_literal,
            .emode = if (emode.is_interrupt()) .interrupt_fault else .fault,
        } else if (flags.contains(.align_fault)) .{
            .slot_literal = .from_slot(.align_fault),
            .slot_src = .seq_literal,
            .emode = if (emode.is_interrupt()) .interrupt_fault else .fault,
        } else if (flags.contains(.overflow_fault)) .{
            .slot_literal = .from_slot(.overflow_fault),
            .slot_src = .seq_literal,
            .emode = if (emode.is_interrupt()) .interrupt_fault else .fault,
        } else if (flags.contains(.rs_underflow_fault)) .{
            .slot_literal = .from_slot(.rs_underflow_fault),
            .slot_src = .seq_literal,
            .emode = if (emode.is_interrupt()) .interrupt_fault else .fault,
        } else if (flags.contains(.rs_overflow_fault)) .{
            .slot_literal = .from_slot(.rs_overflow_fault),
            .slot_src = .seq_literal,
            .emode = if (emode.is_interrupt()) .interrupt_fault else .fault,
        } else if (flags.contains(.insn_fault)) .{
            .slot_literal = .from_slot(.reset),
            .slot_src = .continuation,
            .emode = if (emode.is_interrupt()) .interrupt_fault else .fault,
        } else unreachable;
    } else if (emode == .normal and interrupt_pending and allow_int == .allow) .{
        .slot_literal = .from_slot(.interrupt),
        .slot_src = .seq_literal,
        .emode = .interrupt,
    } else switch (op) {
        .next_uop => .{
            .slot_literal = .from_slot(.reset),
            .slot_src = .continuation,
            .emode = emode,
        },
        .next_instruction => .{
            .slot_literal = .from_slot(.reset),
            .slot_src = .insn_decoder,
            .emode = emode,
        },
        .next_uop_force_normal => .{
            .slot_literal = .from_slot(.reset),
            .slot_src = .continuation,
            .emode = .normal,
        },
        .fault_return => if (emode.is_fault()) .{
            .slot_literal = .from_slot(.reset),
            .slot_src = .fucs,
            .emode = if (emode.is_interrupt()) .interrupt else .normal,
        } else .{
            .slot_literal = .from_slot(.instruction_protection_fault),
            .slot_src = .seq_literal,
            .emode = .fault,
        },
    };
}

pub fn simulate_setup(self: *Pipeline_State, registers: *const arch.reg.File) void {
    std.debug.assert(self.next_stage == .setup);

    const vari_read_rsn: arch.reg.RSN.Raw = switch (self.emode) {
        .normal => self.status.rsn.raw(),
        .interrupt => arch.reg.RSN.interrupt_pipe_0.raw() + self.status.pipe.raw(),
        .fault => arch.reg.RSN.fault_pipe_0.raw() + self.status.pipe.raw(),
        .interrupt_fault => arch.reg.RSN.interrupt_fault_pipe_0.raw() + self.status.pipe.raw(),
    };

    const vari = self.cs.vari;
    const constant = self.cs.constant;

    const va_base = if (vari.to_sr1()) |sr1ri|
        registers[vari_read_rsn].sr1[sr1ri.raw()].raw()
    else if (vari.to_sr2()) |sr2ri|
        registers[vari_read_rsn].sr2[sr2ri.raw()].raw()
    else unreachable;

    const va_offset: u32 = switch (self.cs.vao_src) {
        .zero => 0,
        .constant => @bitCast(@as(i32, constant.raw())),
        .i16_from_dr => bits.sx(u32, self.dr.@"signed[23:8]"()),
        .i8_from_dr => bits.sx(u32, self.dr.@"signed[23:16]"()),
    };

    const read_rsn: arch.reg.RSN.Raw = switch (self.cs.special) {
        .read_from_other_rsn, .read_and_write_other_rsn => switch (self.emode) {
            .normal, .interrupt, .fault => self.status.rsn.raw(),
            .interrupt_fault => arch.reg.RSN.interrupt_pipe_0.raw() + self.status.pipe.raw(),
        },
        else => vari_read_rsn,
    };

    const sr1d = registers[read_rsn].sr1[self.cs.sr1ri.raw()];
    const sr2d = registers[read_rsn].sr2[self.cs.sr2ri.raw()];

    self.j = arch.bus.J.init(switch (self.cs.jsrc) {
        .zero => 0,
        .jr => registers[read_rsn].reg[self.ti.raw()].raw(),
        .sr1 => sr1d.raw(),
        .sr2 => sr2d.raw(),
    });

    self.k = arch.bus.K.init(switch (self.cs.ksrc) {
        .zero => 0,
        .dr_byte_1_sx => @bitCast(@as(i32, self.dr.@"signed[15:8]"())),
        .dr_byte_2_sx => @bitCast(@as(i32, self.dr.@"signed[23:16]"())),
        .dr_byte_21_sx => @bitCast(@as(i32, self.dr.@"signed[23:8]"())),
        .krio_zx => self.krio.raw(),
        .krio_sx => @bitCast(@as(i32, self.krio.raw())),
        .krio_bit => @as(u32, 1) << self.krio.raw(),
        .krio_bit_inv => ~(@as(u32, 1) << self.krio.raw()),
        .constant => @bitCast(@as(i32, constant.raw())),
        .kr => registers[read_rsn].reg[self.kri.raw()].raw(),
        .sr1 => sr1d.raw(),
        .sr2 => sr2d.raw(),
    });

    if (self.cs.ksrc == .kr and self.flags.contains(.kri_underflow)) {
        self.flags.insert(.rs_underflow_fault);
    }

    self.sr1d = sr1d;
    self.sr2d = sr2d;
    self.va = .init(va_base +% va_offset);
    self.at_entry_addr = .{
        .slot = self.va.page.slot,
        .group = at.Entry.Group.from_space_and_dsrc(self.cs.space, self.cs.dsrc),
        .asn6 = self.asn6,
    };
    self.next_stage = .compute;
}

pub fn simulate_compute(self: *Pipeline_State, translations: *const at.File) void {
    std.debug.assert(self.next_stage == .compute);

    const unit = self.cs.unit;
    const j = self.j;
    const k = self.k;
    const cin = self.flags.contains(.carry);

    self.compute_result = switch (unit) {
        .none => .zeroes,
        .alu => compute_alu(self.cs.mode.alu, j, k, cin),
        .shift => compute_shift(self.cs.mode.shift, j, k, cin),
        .mult => compute_mult(self.cs.mode.mult, j, k),
        .count, .extend => compute_count_extend(unit, self.cs.mode.count_extend, j, k),
    };

    switch (self.cs.special) {
        .none => {},
        .read_from_other_rsn => {},
        .write_to_other_rsn => {},
        .read_and_write_other_rsn => {},
        .load_rsn_from_l => {},
        .load_rsn_from_l__read_from_other_rsn => {},
        .load_rsn_from_l__write_to_other_rsn => {},
        .load_rsn_from_l__read_and_write_other_rsn => {},
        .set_guard => {},
        .check_guard => {},
        .load_fucs_from_l => {},
        .load_vabor_from_d => {},
        .fault_on_overflow => if (self.compute_result.vout) self.flags.insert(.overflow_fault),
        .trigger_fault => self.flags.insert(.insn_fault),
        _ => self.report(.{ .corrupted_microcode = "SPECIAL" }),
    }

    self.compute_address_translation(translations);

    if (self.cs.tiw == .write or self.cs.gprw == .write) {
        if (self.flags.contains(.wi_underflow)) self.flags.insert(.rs_underflow_fault);
        if (self.flags.contains(.wi_overflow)) self.flags.insert(.rs_overflow_fault);
    }

    // N.B. All faults flags must be computed before this point!
    const any_fault = self.flags.intersectWith(fault_flags).count() > 0;
    self.flags.setPresent(.any_fault, any_fault);

    if (!any_fault and self.cs.at_op == .translate and !self.flags.contains(.bus_override)) {
        switch (self.cs.dsrc) {
            .system => self.flags.insert(.read),
            .l, .dr_ir, .vabor => self.flags.insert(.write),
        }
    }

    self.next_stage = .transact;
}

fn compute_alu(mode: arch.compute.Mode.ALU, j: arch.bus.J, k: arch.bus.K, carry_flag: bool) Compute_Result {
    var cout = false;
    var vout = false;

    const maybe_cin = if (mode.use_carry_flag) carry_flag else false;
    const cin = if (mode.invert_cin) !maybe_cin else maybe_cin;

    const value = arch.bus.L.init(switch (mode.op) {
        .all_zeroes => 0,
        .not_j_plus_k, .j_plus_not_k, .j_plus_k => value: {
            const raw_j = if (mode.op == .not_j_plus_k) ~j.raw() else j.raw();
            const raw_k = if (mode.op == .j_plus_not_k) ~k.raw() else k.raw();
            const full: u33 = @as(u33, raw_j) +% raw_k +% @intFromBool(cin);
            cout = 0 != (full & 0x1_0000_0000);
            vout = 0 != ((raw_j ^ full) & (raw_k ^ full) & 0x8000_0000);
            break :value @truncate(full);
        },
        .j_xor_k => j.raw() ^ k.raw(),
        .j_or_k => j.raw() | k.raw(),
        .j_and_k => j.raw() & k.raw(),
        .all_ones => 0xFFFF_FFFF,
    });

    return .{
        .value = value,
        .cout = cout,
        .vout = vout,
    };
}

fn compute_shift(mode: arch.compute.Mode.Shift, j: arch.bus.J, k: arch.bus.K, carry_flag: bool) Compute_Result {
    const raw_j = j.raw();
    var conditioned_j = raw_j;
    if (mode.left) {
        conditioned_j = @bitReverse(conditioned_j);

        if (!mode.left_xor_swap_bytes) {
            conditioned_j = ((conditioned_j << 8) & 0xFF00_FF00) | ((conditioned_j >> 8) & 0x00FF_00FF);
        }

        if (!mode.left_xor_swap_halves) {
            conditioned_j = (conditioned_j << 16) | (conditioned_j >> 16);
        }
    } else {
        if (mode.left_xor_swap_bytes) {
            conditioned_j = ((conditioned_j << 8) & 0xFF00_FF00) | ((conditioned_j >> 8) & 0x00FF_00FF);
        }

        if (mode.left_xor_swap_halves) {
            conditioned_j = (conditioned_j << 16) | (conditioned_j >> 16);
        }
    }

    const cin: u1 = switch (mode.cin) {
        .zero => 0,
        .zero_bitreverse => 0,
        .j31 => @intCast(raw_j >> 31),
        .carry_flag => @intFromBool(carry_flag),
    };

    const input: u64 = switch (cin) {
        0 => conditioned_j,
        1 => @as(u64, conditioned_j) | 0xFFFF_FFFF_0000_0000,
    };

    const raw_k = k.raw();
    const k5: u5 = @truncate(raw_k);
    const shift_bits = if (raw_k != k5) 0 else k5;

    const value64: u64 = input >> shift_bits;
    const cout: u1 = @truncate(input >> (shift_bits -% 1));
    const discarded_bits: u32 = @truncate(input << (@as(u6, 32) - shift_bits));

    var result: u32 = @truncate(value64);
    if (mode.left != (mode.cin == .zero_bitreverse)) {
        result = @bitReverse(result);
    }

    return if (raw_k != k5) .{
        .value = .init(bits.sx(arch.bus.L.Raw, cin)),
        .cout = 0 != if (raw_k == 32) cout else cin,
        .vout = raw_j != 0 or (raw_k != 32 and 0 != cin),
    } else .{
        .value = .init(result),
        .cout = 0 != if (raw_k == 0) 0 else cout,
        .vout = discarded_bits != 0,
    };
}

fn compute_mult(mode: arch.compute.Mode.Multiply, j: arch.bus.J, k: arch.bus.K) Compute_Result {
    const jhalf: u16 = switch (mode.j) {
        .lsb => @truncate(j.raw()),
        .msb => @truncate(j.raw() >> 16),
    };

    const khalf: u16 = switch (mode.k) {
        .lsb => @truncate(k.raw()),
        .msb => @truncate(k.raw() >> 16),
    };

    const js: i32 = switch (mode.j_type) {
        .signed => @as(i16, @bitCast(jhalf)),
        .unsigned => jhalf,
    };

    const ks: i32 = switch (mode.k_type) {
        .signed => @as(i16, @bitCast(khalf)),
        .unsigned => khalf,
    };

    const product = js * ks;
    var result: u32 = @bitCast(product);

    const high_bits = result >> 16;
    var vout = false;
    if (mode.j_type == .signed or mode.k_type == .signed) {
        vout = switch (result & 0x8000) {
            0x0000 => high_bits != 0x0000,
            0x8000 => high_bits != 0xFFFF,
            else => unreachable,
        };
    } else {
        vout = high_bits != 0x0000;
    }

    if (mode.shift_result) {
        result = result << 16;
    }

    return .{
        .value = .init(result),
        .cout = false,
        .vout = vout,
    };
}

fn compute_count_extend(unit: arch.compute.Unit, mode: arch.compute.Mode.Count_Extend, j: arch.bus.J, k: arch.bus.K) Compute_Result {
    const raw_j = if (mode.invert_j) ~j.raw() else j.raw();
    const raw_k = if (mode.invert_k) ~k.raw() else k.raw();

    const saturated_k = switch (mode.saturate) {
        .left => saturated: {
            var temp = raw_k;
            temp |= temp << 1;
            temp |= temp << 2;
            temp |= temp << 4;
            temp |= temp << 8;
            temp |= temp << 16;
            break :saturated temp;
        },
        .right => saturated: {
            var temp = raw_k;
            temp |= temp >> 1;
            temp |= temp >> 2;
            temp |= temp >> 4;
            temp |= temp >> 8;
            temp |= temp >> 16;
            break :saturated temp;
        },
    };

    const final_k = if (mode.invert_saturated) ~saturated_k else saturated_k;
    const masked_j = raw_j & final_k;

    const count = @popCount(masked_j);
    const ext = if (mode.sign_extend and 0 != (raw_j & raw_k))
        masked_j | ~final_k
    else
        masked_j
    ;

    return .{
        .value = .init(switch (unit) {
            .count => count,
            .extend => ext,
            else => unreachable,
        }),
        .cout = false,
        .vout = ext != raw_j,
    };
}

fn compute_address_translation(self: *Pipeline_State, translations: *const at.File) void {
    const va = self.va;
    const op = self.cs.at_op;

    const entries = translations[self.at_entry_addr.raw()];

    const primary_match = entries.primary.present and entries.primary.tag == va.page.tag;
    const secondary_match = entries.secondary.present and entries.secondary.tag == va.page.tag;
    const any_match = primary_match or secondary_match;

    const swap_entries = switch (op) {
         .none => false,
         .update => !primary_match,
         .translate, .invalidate, => !primary_match and secondary_match,
    };

    const matching = if (swap_entries) entries.secondary else entries.primary;
    const other = if (swap_entries) entries.primary else entries.secondary;
    self.matching_entry = matching;
    self.other_entry = other;

    if (self.flags.contains(.at_enable)) {
        // address translation enabled
        if (op == .translate and self.cs.space != .physical and !self.flags.contains(.bus_override)) {
            if (any_match) {
                self.frame = matching.frame;

                const is_insn_load = switch(self.cs.sr2wi) {
                    .ip, .next_ip => self.cs.sr2wsrc == .virtual_addr,
                    else => false,
                };

                switch (matching.access) {
                    .unprivileged => {
                        if (is_insn_load) self.flags.insert(.remove_at_super);
                    },
                    .kernel_entry_256 => {
                        if (is_insn_load and (va.page_offset.raw() & 0xFF) == 0) {
                            self.flags.insert(.add_at_super);
                        } else if (!self.flags.contains(.super)) {
                            self.flags.insert(.access_fault);
                        }
                    },
                    .kernel_entry_4096 => {
                        if (is_insn_load and va.page_offset.raw() == 0) {
                            self.flags.insert(.add_at_super);
                        } else if (!self.flags.contains(.super)) {
                            self.flags.insert(.access_fault);
                        }
                    },
                    .kernel_private => {
                        if (!self.flags.contains(.super)) {
                            self.flags.insert(.access_fault);
                        }
                    },
                }

                switch (matching.pipe) {
                    .any => {},
                    .pipe_0_only => if (self.status.pipe != .zero) self.flags.insert(.pipe_fault),
                    .pipe_1_only => if (self.status.pipe != .one) self.flags.insert(.pipe_fault),
                    .pipe_2_only => if (self.status.pipe != .two) self.flags.insert(.pipe_fault),
                    .pipe_3_only => if (self.status.pipe != .three) self.flags.insert(.pipe_fault),
                }
            } else {
                self.flags.insert(.page_fault);
            }
        } else {
            // address translation is enabled, but at least one of these conditions applies:
            //      * not doing a bus operation this cycle
            //      * the bus operation has been suppressed
            //      * it's a physically addressed operation
            // in any of these cases, we don't want to trigger any faults, and we don't want to modify the super flag.
            self.frame = .init(@truncate(va.page.raw()));
        }
    } else {
        // address translation disabled
        self.frame = .init(@truncate(va.page.raw()));
        self.flags.insert(.add_at_super); // translation can only be disabled in kernel mode
    }

    const n1: u1 = @truncate(va.page_offset.raw() >> 1);
    self.aa = arch.addr.Frame.Word_Offset.init(@truncate((va.page_offset.raw() >> 2) +% n1));
    self.ab = arch.addr.Frame.Word_Offset.init(@intCast(va.page_offset.raw() >> 2));


    if (op == .translate and !self.flags.contains(.bus_override)) {
        if (self.cs.space != .physical) switch (self.cs.width) {
            .@"8b" => {},
            .@"16b" => if (va.page_offset.raw() == arch.addr.Page.Offset.max.raw()) self.flags.insert(.page_align_fault),
            .@"24b" => if (va.page_offset.raw() >= arch.addr.Page.Offset.max.raw() - 1) self.flags.insert(.page_align_fault),
            .@"32b" => if (va.page_offset.raw() >= arch.addr.Page.Offset.max.raw() - 2) self.flags.insert(.page_align_fault),
        };

        const addr_alignment: u2 = @truncate(va.page_offset.raw());
        switch (addr_alignment) {
            0 => switch (self.cs.width) {
                .@"8b" => {
                    self.flags.insert(.lba);
                },
                .@"16b" => {
                    self.flags.insert(.lba);
                    self.flags.insert(.uba);
                },
                .@"24b" => {
                    self.flags.insert(.lba);
                    self.flags.insert(.uba);
                    self.flags.insert(.lbb);
                },
                .@"32b" => {
                    self.flags.insert(.lba);
                    self.flags.insert(.uba);
                    self.flags.insert(.lbb);
                    self.flags.insert(.ubb);
                },
            },
            1 => switch (self.cs.width) {
                .@"8b" => {
                    self.flags.insert(.uba);
                },
                .@"16b" => {
                    self.flags.insert(.uba);
                    self.flags.insert(.lbb);
                },
                .@"24b" => {
                    self.flags.insert(.uba);
                    self.flags.insert(.lbb);
                    self.flags.insert(.ubb);
                },
                .@"32b" => {
                    self.flags.insert(.align_fault);
                },
            },
            2 => switch (self.cs.width) {
                .@"8b" => {
                    self.flags.insert(.lbb);
                },
                .@"16b" => {
                    self.flags.insert(.lbb);
                    self.flags.insert(.ubb);
                },
                .@"24b" => {
                    self.flags.insert(.lbb);
                    self.flags.insert(.ubb);
                    self.flags.insert(.lba);
                },
                .@"32b" => {
                    self.flags.insert(.lbb);
                    self.flags.insert(.ubb);
                    self.flags.insert(.lba);
                    self.flags.insert(.uba);
                },
            },
            3 => switch (self.cs.width) {
                .@"8b" => {
                    self.flags.insert(.ubb);
                },
                .@"16b" => {
                    self.flags.insert(.ubb);
                    self.flags.insert(.lba);
                },
                .@"24b" => {
                    self.flags.insert(.ubb);
                    self.flags.insert(.lba);
                    self.flags.insert(.uba);
                },
                .@"32b" => {
                    self.flags.insert(.align_fault);
                },
            },
        }
    }
}

pub fn get_l(self: Pipeline_State) arch.bus.L {
    std.debug.assert(self.next_stage == .transact);

    return switch (self.cs.lsrc) {
        .compute_or_d => switch (self.cs.unit) {
            .none => self.read_d(arch.bus.L),
            .alu, .shift, .mult, .count, .extend, => self.compute_result.value,
        },
        .flags => .init((arch.reg.Flags {
            .z = self.flags.contains(.zero),
            .n = self.flags.contains(.negative),
            .c = self.flags.contains(.carry),
            .v = self.flags.contains(.overflow),
            .at_enable = self.flags.contains(.at_enable),
            .bus_override = self.flags.contains(.bus_override),
            .super = self.flags.contains(.super),
            .at_super = self.flags.contains(.at_super),
            .top = self.ti,
            .mode = self.emode,
        }).raw()),
        .status => .init(self.status.raw()),
        .last_d => .init(self.last_d.raw()),
    };
}

/// If self.flags.contains(.read), you must set self.da and self.db with the data read from system RAM or a device before calling simulate_transact
/// If self.flags.contains(.write) and !self.flags.contains(.guard_mismatch), you must move self.da and self.db
/// into the appropriate memory/device location after simulate_transact() returns.
pub fn simulate_transact(self: *Pipeline_State,
    registers: *arch.reg.File,
    translations: *at.File,
    guards: *arch.reg.guard.File,
) void {
    std.debug.assert(self.next_stage == .transact);

    const any_fault = self.flags.contains(.any_fault);

    const d: arch.bus.D = switch (self.cs.dsrc) {
        .system => if (self.flags.contains(.bus_override)) .init(self.vabor.raw()) else self.read_d(arch.bus.D),
        .l => .init(self.get_l().raw()),
        .vabor => .init(self.vabor.raw()),
        .dr_ir => .init(switch (self.cs.drw) {
            .write => self.dr.raw(),
            .hold => self.dr.raw() & 0xFFFF0000 | self.ir.raw(),
        }),
    };

    const l = self.get_l();

    self.last_d = d; // do not use get_l() after modifying this!

    if (self.cs.drw == .write and !any_fault) {
        self.dr = .init(d.raw());
    }

    if (self.cs.irw == .write and !any_fault) {
        self.ir = .init(@truncate(self.dr.raw()));
    }

    const write_rsn: arch.reg.RSN = switch (self.cs.special) {
        .write_to_other_rsn, .read_and_write_other_rsn => switch (self.emode) {
            .normal, .interrupt, .fault => self.status.rsn,
            .interrupt_fault => .init(arch.reg.RSN.interrupt_pipe_0.raw() + self.status.pipe.raw()),
        },
        else => switch (self.emode) {
            .normal => self.status.rsn,
            .interrupt => .init(arch.reg.RSN.interrupt_pipe_0.raw() + self.status.pipe.raw()),
            .fault => .init(arch.reg.RSN.fault_pipe_0.raw() + self.status.pipe.raw()),
            .interrupt_fault => .init(arch.reg.RSN.interrupt_fault_pipe_0.raw() + self.status.pipe.raw()),
        },
    };

    const set_guard = switch (self.cs.special) {
        .set_guard => !any_fault,
        .check_guard => set_guard: {
            const guard = arch.reg.guard.Value.from_frame_and_offset(self.frame, self.va.page_offset);
            if (guards[self.status.pipe.raw()].raw() != guard.raw()) {
                self.flags.insert(.guard_mismatch);
            }
            break :set_guard false;
        },
        else => false,
    };

    if (!any_fault and self.cs.special == .load_fucs_from_l) {
        self.status.fucs = arch.reg.Status.init(l.raw()).fucs;
    }

    if (!any_fault and self.cs.special == .load_rsn_from_l) {
        self.status.rsn = arch.reg.Status.init(l.raw()).rsn;
        self.report(.{ .rsn = self.status.rsn });
    }

    if (!any_fault and self.cs.special == .load_vabor_from_d) {
        self.vabor = .init(d.raw());
    }

    if (self.flags.contains(.write)) {
        const guard = arch.reg.guard.Value.from_frame_and_offset(self.frame, self.va.page_offset);
        const cur_pipe = self.status.pipe.raw();
        for (0.., guards) |p, *gr| {
            if (p != cur_pipe and gr.raw() == guard.raw()) gr.* = .invalid;
        }
    }

    if (self.debug_log) |_| {
        if (self.flags.contains(.write)) {
            self.report(.{ .write = .{
                .space = self.cs.space,
                .ctrl = self.get_bus_control(),
                .data = .{ .da = self.da, .db = self.db },
            }});
        }

        if (self.flags.contains(.read)) {
            self.report(.{ .read = .{
                .space = self.cs.space,
                .ctrl = self.get_bus_control(),
                .data = .{ .da = self.da, .db = self.db },
            }});
        }
    }

    if (set_guard) {
        const guard = arch.reg.guard.Value.from_frame_and_offset(self.frame, self.va.page_offset);
        guards[self.status.pipe.raw()] = guard;
    }

    if (!any_fault) {
        if (self.cs.gprw == .write) {
            self.report(.{ .reg = .{
                .rsn = write_rsn,
                .wi = self.wi,
                .old_data = registers[write_rsn.raw()].reg[self.wi.raw()],
                .new_data = .init(l.raw()),
            }});
            registers[write_rsn.raw()].reg[self.wi.raw()] = .init(l.raw());
        }

        const sr1wsrc = self.cs.sr1wsrc;
        if (sr1wsrc != .no_write) {
            const wi = self.cs.sr1wi;
            const value = arch.reg.sr1.Value.init(switch (sr1wsrc) {
                .no_write => unreachable,
                .self => self.sr1d.raw(),
                .l => l.raw(),
                .virtual_addr => self.va.raw(),
            });

            self.report(.{ .sr1 = .{
                .rsn = write_rsn,
                .index = wi,
                .old_data = registers[write_rsn.raw()].sr1[wi.raw()],
                .new_data = value,
            }});

            registers[write_rsn.raw()].sr1[wi.raw()] = value;
        }

        const sr2wsrc = self.cs.sr2wsrc;
        if (sr2wsrc != .no_write) {
            const wi = self.cs.sr2wi;
            const value = arch.reg.sr2.Value.init(switch (sr2wsrc) {
                .no_write => unreachable,
                .self => self.sr2d.raw(),
                .l => l.raw(),
                .virtual_addr => self.va.raw(),
            });

            self.report(.{ .sr2 = .{
                .rsn = write_rsn,
                .index = wi,
                .old_data = registers[write_rsn.raw()].sr2[wi.raw()],
                .new_data = value,
            }});

            registers[write_rsn.raw()].sr2[wi.raw()] = value;

            if (wi == .asn) switch (self.cs.special) {
                .read_from_other_rsn, .write_to_other_rsn => {},
                .read_and_write_other_rsn => if (self.cs.sr2ri == .asn) {
                    self.asn6 = .from_asn(value);
                },
                else => self.asn6 = .from_asn(value),
            };
        }

        if (self.flags.contains(.add_at_super)) {
            self.flags.insert(.at_super);
        } else if (self.flags.contains(.remove_at_super)) {
            self.flags.remove(.at_super);
        }

        if (self.cs.seq_op == .next_instruction) {
            self.flags.setPresent(.super, self.flags.contains(.at_super));
        }

        if (self.cs.at_op == .translate) {
            self.flags.remove(.bus_override);
        }

        switch (self.cs.flag_op) {
            .hold => {},
            .zn_from_l => {
                self.flags.setPresent(.zero, 0 == l.raw());
                self.flags.setPresent(.negative, 0 != (l.raw() & 0x8000_0000));
            },
            .compute => {
                self.flags.setPresent(.zero, 0 == l.raw());
                self.flags.setPresent(.negative, 0 != (l.raw() & 0x8000_0000));
                self.flags.setPresent(.carry, self.compute_result.cout);
                self.flags.setPresent(.overflow, self.compute_result.vout);
            },
            .compute_no_set_z => {
                if (l.raw() != 0) self.flags.remove(.zero);
                self.flags.setPresent(.negative, 0 != (l.raw() & 0x8000_0000));
                self.flags.setPresent(.carry, self.compute_result.cout);
                self.flags.setPresent(.overflow, self.compute_result.vout);
            },
            .clear_bits => {
                const w = arch.reg.Flags.Writable.init(bits.zx(u32, self.cs.constant.raw()));
                if (w.z) self.flags.remove(.zero);
                if (w.n) self.flags.remove(.negative);
                if (w.c) self.flags.remove(.carry);
                if (w.v) self.flags.remove(.overflow);
                if (w.at_enable) self.flags.remove(.at_enable);
                if (w.bus_override) self.flags.remove(.bus_override);
            },
            .set_bits => {
                const w = arch.reg.Flags.Writable.init(bits.zx(u32, self.cs.constant.raw()));
                if (w.z) self.flags.insert(.zero);
                if (w.n) self.flags.insert(.negative);
                if (w.c) self.flags.insert(.carry);
                if (w.v) self.flags.insert(.overflow);
                if (w.at_enable) self.flags.insert(.at_enable);
                if (w.bus_override) self.flags.insert(.bus_override);
            },
            .load_zncv, .load => {
                const f = arch.reg.Flags.init(l.raw());
                self.flags.setPresent(.zero, f.z);
                self.flags.setPresent(.negative, f.n);
                self.flags.setPresent(.carry, f.c);
                self.flags.setPresent(.overflow, f.v);
                if (self.cs.flag_op == .load) {
                    self.flags.setPresent(.at_enable, f.at_enable);
                    self.flags.setPresent(.bus_override, f.bus_override);
                    self.ti = f.top;
                }
            },
        }

        if (self.flags.contains(.guard_mismatch)) {
            self.flags.insert(.overflow);
        }

        if (self.cs.flag_op != .load and self.cs.tiw == .write) {
            self.ti = self.wi;
        }

    } else if (self.debug_log) |_| {
        const faults = self.flags.intersectWith(fault_flags);
        std.debug.assert(faults.count() > 0);
        var iter = faults.iterator();
        while (iter.next()) |fault_flag| {
            self.report(.{ .fault = self.fault_slot(fault_flag) });
        }
    }

    const at_op = self.cs.at_op;
    if (any_fault) {
        self.vabor = self.va;
        self.status.fucs = self.uca.slot;
        self.status.fspace = self.cs.space;
        self.status.fwidth = self.cs.width;
        self.status.fwrite = self.cs.dsrc != .system;
    } else switch (at_op) {
        .none => {},
        .translate => translations[self.at_entry_addr.raw()] = .{
            .primary = self.matching_entry,
            .secondary = self.other_entry,
        },
        .update => translations[self.at_entry_addr.raw()] = .{
            .primary = at.Entry.init(l.raw()),
            .secondary = self.other_entry,
        },
        .invalidate => {
            const tag_mask: arch.addr.Page.Tag.Raw = @truncate(l.raw());
            const vtag = self.va.page.tag.raw() & tag_mask;

            var matching = self.matching_entry;
            var other = self.other_entry;

            if (vtag == (matching.tag.raw() & tag_mask) and matching.present) {
                matching.present = false;
            }

            if (vtag == (other.tag.raw() & tag_mask) and other.present) {
                other.present = false;
            }

            translations[self.at_entry_addr.raw()] = .{
                .primary = matching,
                .secondary = other,
            };
        },
    }

    self.next_stage = .decode;
}

fn read_d(self: Pipeline_State, comptime T: type) T {
    const n0: u1 = @truncate(self.va.page_offset.raw());
    const n1: u1 = @truncate(self.va.page_offset.raw() >> 1);

    const lo = switch (n1) {
        0 => self.da.raw(),
        1 => self.db.raw(),
    };
    const hi = switch (n1) {
        0 => self.db.raw(),
        1 => self.da.raw(),
    };

    var raw_d: arch.bus.D.Raw = bits.concat(.{ lo, hi });
    if (n0 == 1) {
        raw_d >>= 8;
    }

    switch (self.cs.width) {
        .@"32b" => {},
        .@"24b" => raw_d &= 0xFFFFFF,
        .@"16b" => raw_d &= 0xFFFF,
        .@"8b" => raw_d &= 0xFF,
    }

    return T.init(raw_d);
}

fn write_d(self: *Pipeline_State, d: anytype) void {
    const n0: u1 = @truncate(self.va.page_offset.raw());
    const n1: u1 = @truncate(self.va.page_offset.raw() >> 1);

    var raw_d: arch.bus.D.Raw = d.raw();
    if (n0 == 1) {
        raw_d <<= 8;
    }

    self.da = .init(switch (n1) {
        0 => @truncate(raw_d),
        1 => @truncate(raw_d >> 16),
    });
    self.db = .init(switch (n1) {
        0 => @truncate(raw_d >> 16),
        1 => @truncate(raw_d),
    });
}

pub fn get_bus_control(self: *Pipeline_State) Bus_Control {
    return .{
        .frame = self.frame,
        .aa = self.aa,
        .ab = self.ab,
        .lba = self.flags.contains(.lba),
        .uba = self.flags.contains(.uba),
        .lbb = self.flags.contains(.lbb),
        .ubb = self.flags.contains(.ubb),
        .guard_mismatch = self.flags.contains(.guard_mismatch),
    };
}

const Pipeline_State = @This();

const Debug_Log = @import("Debug_Log.zig");
const Bus_Control = @import("Bus_Control.zig");
const at = arch.addr.translation;
const arch = @import("arch");
const bits = @import("bits");
const std = @import("std");
