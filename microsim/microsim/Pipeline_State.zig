pipe: arch.Pipeline,
next_stage: arch.Pipeline.Stage,
emode: arch.Execution_Mode = .interrupt_fault,
uca: arch.microcode.Address = arch.microcode.Address.init(0),
pucs: arch.microcode.Slot = arch.microcode.Slot.init(0),
cs: arch.Control_Signals = arch.Control_Signals.zeroes,
rsn: arch.Register_Set_Number = arch.Register_Set_Number.init(0),
dr: arch.DR = arch.DR.init(0),
ir: arch.IR = arch.IR.init(0),
ti: arch.Write_Index = arch.Write_Index.init(0),
wi: arch.Write_Index = arch.Write_Index.init(0),
jri: arch.J.Read_Index = arch.J.Read_Index.init(0),
j: arch.J = arch.J.init(0),
krio: arch.K.Read_Index_Offset = arch.K.Read_Index_Offset.init(0),
kri: arch.K.Read_Index = arch.K.Read_Index.init(0),
k: arch.K = arch.K.init(0),
sr1d: arch.Reg = arch.Reg.init(0),
sr2d: arch.Reg = arch.Reg.init(0),
asn4: at.Entry.ASN4 = at.Entry.ASN4.init(0),
va: arch.addr.Virtual = arch.addr.Virtual.init(0),
at_entry_addr: at.Entry.Address = at.Entry.Address.init(0),
last_at_info: at.Info = at.Info.init(0),
matching_entry: at.Entry = at.Entry.init(0),
other_entry: at.Entry = at.Entry.init(0),
frame: arch.addr.Frame = arch.addr.Frame.init(0),
aa: arch.addr.Word_Offset = arch.addr.Word_Offset.init(0),
ab: arch.addr.Word_Offset = arch.addr.Word_Offset.init(0),
compute_result: Compute_Result = .{
    .value = arch.L.init(0),
    .cout = false,
    .vout = false,
},
count_result: u6 = 0,
da: arch.DA = arch.DA.init(0),
db: arch.DB = arch.DB.init(0),
flags: std.EnumSet(Flag) = std.EnumSet(Flag).initEmpty(),

pub const Flag = enum {
    stat_c,
    stat_v,
    stat_n,
    stat_z,
    stat_k,
    stat_a,
    at_k, // note this is non-transient because the next instruction need not be loaded just before it is executed

    // at_k mutators:
    add_at_k,
    remove_at_k,

    // faults
    page_fault,
    access_fault,
    page_align_fault,
    align_fault,
    overflow_fault,
    insn_fault,
    any_fault,

    // bus signals
    lba,
    uba,
    lbb,
    ubb,
    read,
    write,
    block_transfer,
    guard_mismatch,
    update_frame_state,
};

const fault_flags = std.EnumSet(Flag).initMany(&.{
    .page_fault,
    .access_fault,
    .page_align_fault,
    .align_fault,
    .overflow_fault,
    .insn_fault,
});

const transient_flags = std.EnumSet(Flag).initMany(&.{
    .add_at_k,
    .remove_at_k,
    .block_transfer,
    .guard_mismatch,
    .update_frame_state,
    .any_fault,
    .lba,
    .uba,
    .lbb,
    .ubb,
    .read,
    .write,
}).unionWith(fault_flags);

const Compute_Result = struct {
    value: arch.L,
    cout: bool,
    vout: bool,
};

pub fn simulate_decode(self: *Pipeline_State,
    insn_decode_rom: *const arch.insn_decode.Rom,
    microcode_rom: *const arch.microcode.Rom,
    pending_interrupt: bool,
    reset: bool,
) void {
    std.debug.assert(self.next_stage == .decode);

    const base_ri = self.ti.raw_signed();
    const flags = self.flags;
    const id_result = insn_decode_rom[self.ir.raw()];
    const seq_result = simulate_sequencer(
        self.cs.seqop,
        self.cs.allowint,
        self.emode,
        pending_interrupt,
        reset,
        flags.contains(.page_fault),
        flags.contains(.access_fault),
        flags.contains(.page_align_fault),
        flags.contains(.align_fault),
        flags.contains(.overflow_fault),
        flags.contains(.insn_fault),
        flags.contains(.any_fault),
    );

    const prev_uca_slot = self.uca.slot;

    const next_uca: arch.microcode.Address = .{
        .flags = .{
            .z = flags.contains(.stat_z),
            .n = flags.contains(.stat_n),
            .k = flags.contains(.stat_k),
            .cv = switch (id_result.cv) {
                .zero => false,
                .one => true,
                .c => flags.contains(.stat_c),
                .v => flags.contains(.stat_v),
            },
        },
        .slot = switch (seq_result.slot_src) {
            .hold => prev_uca_slot,
            .continuation =>  self.cs.next,
            .seq_literal => arch.microcode.Slot.init(seq_result.slot_literal),
            .insn_decoder => id_result.entry,
        },
    };

    self.jri = arch.J.Read_Index.init(@bitCast(base_ri));
    self.kri = arch.K.Read_Index.init(@bitCast(base_ri -% id_result.krio.raw()));
    self.krio = id_result.krio;
    self.wi = arch.Write_Index.init(@bitCast(base_ri +% id_result.wio.raw()));
    self.pucs = prev_uca_slot;
    self.uca = next_uca;
    self.cs = microcode_rom[next_uca.raw()];
    self.flags = self.flags.differenceWith(transient_flags);
    self.emode = seq_result.emode;
    self.next_stage = .setup;
}

const Sequencer_Result = struct {
    slot_literal: u4,
    slot_src: arch.microcode.Slot.Source,
    emode: arch.Execution_Mode,
};

fn simulate_sequencer(
    op: arch.Sequencer_Op,
    allow_int: bool,
    emode: arch.Execution_Mode,
    interrupt_pending: bool,
    reset: bool,
    page_fault: bool,
    access_fault: bool,
    page_align_fault: bool,
    align_fault: bool,
    overflow_fault: bool,
    insn_fault: bool,
    any_fault: bool,
) Sequencer_Result {
    return if (reset) .{
        .slot_literal = slot_literal(.reset),
        .slot_src = .seq_literal,
        .emode = .interrupt_fault,
    } else if (emode.is_fault() and any_fault) .{
        .slot_literal = slot_literal(.double_fault),
        .slot_src = .seq_literal,
        .emode = .fault,
    } else if (page_fault) .{
        .slot_literal = slot_literal(.page_fault),
        .slot_src = .seq_literal,
        .emode = if (emode.is_interrupt()) .interrupt_fault else .fault,
    } else if (access_fault) .{
        .slot_literal = slot_literal(.access_fault),
        .slot_src = .seq_literal,
        .emode = if (emode.is_interrupt()) .interrupt_fault else .fault,
    } else if (page_align_fault) .{
        .slot_literal = slot_literal(.page_align_fault),
        .slot_src = .seq_literal,
        .emode = if (emode.is_interrupt()) .interrupt_fault else .fault,
    } else if (align_fault) .{
        .slot_literal = slot_literal(.align_fault),
        .slot_src = .seq_literal,
        .emode = if (emode.is_interrupt()) .interrupt_fault else .fault,
    } else if (overflow_fault) .{
        .slot_literal = slot_literal(.overflow_fault),
        .slot_src = .seq_literal,
        .emode = if (emode.is_interrupt()) .interrupt_fault else .fault,
    } else if (insn_fault) .{
        .slot_literal = slot_literal(.reset),
        .slot_src = .continuation,
        .emode = if (emode.is_interrupt()) .interrupt_fault else .fault,
    } else if (emode == .normal and interrupt_pending and allow_int) .{
        .slot_literal = slot_literal(.interrupt),
        .slot_src = .seq_literal,
        .emode = .interrupt,
    } else switch (op) {
        .next_uop => .{
            .slot_literal = slot_literal(.reset),
            .slot_src = .continuation,
            .emode = emode,
        },
        .next_instruction => .{
            .slot_literal = slot_literal(.reset),
            .slot_src = .insn_decoder,
            .emode = emode,
        },
        .next_uop_force_normal => .{
            .slot_literal = slot_literal(.reset),
            .slot_src = .continuation,
            .emode = .normal,
        },
        .fault_return => if (emode.is_fault()) .{
            .slot_literal = slot_literal(.reset),
            .slot_src = .hold,
            .emode = if (emode.is_interrupt()) .interrupt else .normal,
        } else .{
            .slot_literal = slot_literal(.instruction_protection_fault),
            .slot_src = .seq_literal,
            .emode = .fault,
        },
    };
}

fn slot_literal(slot: arch.microcode.Slot) u4 {
    return @intCast(slot.raw());
}

pub fn simulate_setup(self: *Pipeline_State, registers: *const arch.Register_File) void {
    std.debug.assert(self.next_stage == .setup);

    const rsn = self.rsn.raw();
    const vari = self.cs.vari;
    const vao = self.cs.vao;

    const sr1d = registers[rsn].sr1[self.cs.sr1ri.raw()];
    const sr2d = registers[rsn].sr2[self.cs.sr2ri.raw()];
    const vab = if (vari.to_sr1()) |sr1ri|
        registers[rsn].sr1[sr1ri.raw()]
    else if (vari.to_sr2()) |sr2ri|
        registers[rsn].sr2[sr2ri.raw()]
    else unreachable;

    const va_offset: u32 = switch (vao) {
        .i16_from_dr => @bitCast(self.dr.byte21_i32()),
        .i8_from_dr => @bitCast(self.dr.byte2_i32()),
        .i8_x4_from_dr => @bitCast(self.dr.byte2_i32() * 4),
        else => @bitCast(@as(i32, vao.raw())),
    };

    self.j = arch.J.init(switch (self.cs.jsrc) {
        .zero => 0,
        .jr => registers[rsn].reg[self.jri.raw()].raw(),
        .sr1 => sr1d.raw(),
        .sr2 => sr2d.raw(),
    });

    self.k = arch.K.init(switch (self.cs.ksrc) {
        .zero => 0,
        .vao => @bitCast(@as(i32, vao.raw())),
        .krio => @bitCast(@as(i32, self.krio.raw())),
        .kr => registers[rsn].reg[self.kri.raw()].raw(),
        .sr1 => sr1d.raw(),
        .sr2 => sr2d.raw(),
        .krio_bit => @as(u32, 1) << @bitCast(self.krio.raw()),
        .krio_bit_inv => ~(@as(u32, 1) << @bitCast(self.krio.raw())),
        .dr_byte_1_sx => @bitCast(self.dr.byte1_i32()),
        .dr_byte_2_sx => @bitCast(self.dr.byte2_i32()),
        .dr_byte_21_sx => @bitCast(self.dr.byte21_i32()),
    });

    self.sr1d = sr1d;
    self.sr2d = sr2d;
    self.va = arch.addr.Virtual.init(vab.raw() +% va_offset);
    self.next_stage = .compute;
}

pub fn simulate_compute(self: *Pipeline_State, translations: *const at.Translation_File) void {
    std.debug.assert(self.next_stage == .compute);

    const unit = self.cs.unit;
    const j = self.j;
    const k = self.k;
    const cin = self.flags.contains(.stat_c);

    self.compute_result = switch (unit) {
        .alu => compute_alu(self.cs.mode.alu, j, k, cin),
        .shift => compute_shift(self.cs.mode.shift, j, k, cin),
        .mult => compute_mult(self.cs.mode.mult, j, k),
        .count_extend => compute_count_extend(self.cs.mode.count_extend, j, k, &self.count_result),
    };

    switch (self.cs.special) {
        .none, .set_guard, .check_guard, .load_rsn_from_l, .toggle_rsn => {},
        .fault_on_overflow => if (self.compute_result.vout) self.flags.insert(.overflow_fault),
        .trigger_fault => self.flags.insert(.insn_fault),
        .block_transfer => self.flags.insert(.block_transfer),
    }

    self.compute_address_translation(translations);

    // N.B. All faults flags must be computed before this point!
    const any_fault = self.flags.intersectWith(fault_flags).count() > 0;
    self.flags.setPresent(.any_fault, any_fault);

    if (!any_fault and self.cs.atop == .translate) {
        switch (self.cs.dir) {
            .none => {},
            .read => self.flags.insert(.read),
            .write_from_dr_ir, .write_from_l => self.flags.insert(.write),
        }
    }

    self.next_stage = .transact;
}

fn compute_alu(mode: arch.ALU_Mode, j: arch.J, k: arch.K, stat_c: bool) Compute_Result {
    var cout = false;
    var vout = false;

    const maybe_cin = if (mode.use_stat_c) stat_c else false;
    const cin = if (mode.invert_cin) !maybe_cin else maybe_cin;

    const value = arch.L.init(switch (mode.op) {
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

fn compute_shift(mode: arch.Shift_Mode, j: arch.J, k: arch.K, stat_c: bool) Compute_Result {
    const cin: u64 = switch (mode.cin) {
        .zero => 0,
        .zero_bitreverse => 0,
        .j31 => if (0 != (j.raw() & 0x8000_0000)) 0xFFFF_FFFF_0000_0000 else 0,
        .stat_c => if (stat_c) 0xFFFF_FFFF_0000_0000 else 0,
    };

    const raw_k = k.raw();
    const k5: u5 = @truncate(raw_k);
    if (raw_k != k5) {
        return .{
            .value = arch.L.init(0),
            .cout = if (raw_k == 32) 0 != (j.raw() & 0x8000_0000) else 0 != cin,
            .vout = if (raw_k == 32) j.raw() != 0 else j.raw() != 0 or 0 != cin,
        };
    } else {
        var raw_j = j.raw();

        if (mode.left) {
            raw_j = @bitReverse(raw_j);

            if (!mode.left_xor_swap_bytes) {
                raw_j = ((raw_j << 8) & 0xFF00_FF00) | ((raw_j >> 8) & 0x00FF_00FF);
            }

            if (!mode.left_xor_swap_halves) {
                raw_j = (raw_j << 16) | (raw_j >> 16);
            }
        } else {
            if (mode.left_xor_swap_bytes) {
                raw_j = ((raw_j << 8) & 0xFF00_FF00) | ((raw_j >> 8) & 0x00FF_00FF);
            }

            if (mode.left_xor_swap_halves) {
                raw_j = (raw_j << 16) | (raw_j >> 16);
            }
        }

        var value: u64 = j.raw() | cin;
        value = value >> k5;

        const cout: u1 = if (k5 > 0) @truncate(value >> (k5 - 1)) else @intFromBool(stat_c);
        const shifted_out: u32 = @truncate(value << (@as(u6, 32) - k5));

        var result: u32 = @truncate(value);
        if (mode.left != (mode.cin == .zero_bitreverse)) {
            result = @bitReverse(result);
        }
        
        return .{
            .value = arch.L.init(@truncate(value)),
            .cout = cout != 0,
            .vout = shifted_out != 0,
        };
    }
}

fn compute_mult(mode: arch.Multiply_Mode, j: arch.J, k: arch.K) Compute_Result {

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

    if (mode.shift_result) {
        result = result << 16;
    }

    return .{
        .value = arch.L.init(result),
        .cout = false,
        .vout = false,
    };
}

fn compute_count_extend(mode: arch.Count_Extend_Mode, j: arch.J, k: arch.K, count: *u6) Compute_Result {
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

    count.* = @popCount(masked_j);
    const value = arch.L.init(if (mode.sign_extend and 0 != (raw_j & raw_k)) masked_j | ~final_k else masked_j);
    return .{
        .value = value,
        .cout = false,
        .vout = value.raw() != raw_j,
    };
}

fn compute_address_translation(self: *Pipeline_State, translations: *const at.Translation_File) void {
    const va = self.va;
    const op = self.cs.atop;
    const space = self.cs.vaspace;
    const dir = self.cs.dir;
    const width = self.cs.width;

    const entry_addr: at.Entry.Address = .{
        .slot = va.page.slot,
        .group = at.Entry.Group.from_space_and_dir(space, dir),
        .asn4 = self.asn4,
    };

    const entries = translations[entry_addr.raw()];
    self.at_entry_addr = entry_addr;

    const primary_match = entries.primary.present and entries.primary.tag == va.page.tag;
    const secondary_match = entries.secondary.present and entries.secondary.tag == va.page.tag;
    const any_match = primary_match or secondary_match;
    const swap_entries = op != .none and !primary_match and (secondary_match or op == .update);
    const matching = if (swap_entries) entries.secondary else entries.primary;
    const other = if (swap_entries) entries.primary else entries.secondary;
    self.matching_entry = matching;
    self.other_entry = other;

    if (self.flags.contains(.stat_a) and space != .raw) {
        // address translation enabled
        if (op == .translate and any_match) {
            self.frame = matching.frame;

            if (matching.update_frame_state) {
                self.flags.insert(.update_frame_state);
            }

            const is_insn_load = switch(self.cs.sr2wi) {
                .ip, .next_ip => op == .translate and self.cs.sr2wsrc == .virtual_addr,
                else => false,
            };

            switch (matching.access) {
                .unprivileged => {
                    if (is_insn_load) self.flags.insert(.remove_at_k);
                },
                .kernel_entry_256 => {
                    if (is_insn_load and (va.offset.raw() & 0xFF) == 0) {
                        self.flags.insert(.add_at_k);
                    } else if (!self.flags.contains(.stat_k)) {
                        self.flags.insert(.access_fault);
                    }
                },
                .kernel_entry_4096 => {
                    if (is_insn_load and va.offset.raw() == 0) {
                        self.flags.insert(.add_at_k);
                    } else if (!self.flags.contains(.stat_k)) {
                        self.flags.insert(.access_fault);
                    }
                },
                .kernel_private => {
                    if (!self.flags.contains(.stat_k)) {
                        self.flags.insert(.access_fault);
                    }
                },
            }
        } else {
            if (op == .translate) {
                std.debug.assert(!any_match);
                self.flags.insert(.page_fault);
            }
            self.frame = arch.addr.Frame.init(@truncate(va.page.raw()));
        }
    } else {
        // address translation disabled
        self.frame = arch.addr.Frame.init(@truncate(va.page.raw()));
        self.flags.insert(.add_at_k); // translation can only be disabled in kernel mode
    }

    switch (width) {
        .@"8b" => {},
        .@"16b" => if (va.offset.raw() == arch.addr.Offset.max.raw()) self.flags.insert(.page_align_fault),
        .@"24b" => if (va.offset.raw() >= arch.addr.Offset.max.raw() - 1) self.flags.insert(.page_align_fault),
        .@"32b" => if (va.offset.raw() >= arch.addr.Offset.max.raw() - 2) self.flags.insert(.page_align_fault),
    }

    const n1: u1 = @truncate(va.offset.raw() >> 1);
    self.aa = arch.addr.Word_Offset.init(@truncate((va.offset.raw() >> 2) +% n1));
    self.ab = arch.addr.Word_Offset.init(@intCast(va.offset.raw() >> 2));

    const addr_alignment: u2 = @truncate(va.offset.raw());
    switch (addr_alignment) {
        0 => switch (width) {
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
        1 => switch (width) {
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
        2 => switch (width) {
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
        3 => switch (width) {
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

/// If self.flags.contains(.read), you must set self.da and self.db with the data read from system RAM or a device
/// If self.flags.contains(.write) and !self.flags.contains(.guard_mismatch), you must move self.da and self.db
/// into the appropriate memory/device location after simulate_transact() returns.
pub fn simulate_transact(self: *Pipeline_State,
    registers: *arch.Register_File,
    translations: *at.Translation_File,
    guards: *arch.Guarded_Memory_File,
) void {
    std.debug.assert(self.next_stage == .transact);

    const any_fault = self.flags.contains(.any_fault);

    const l: arch.L = switch (self.cs.lsrc) {
        .alu => l: {
            std.debug.assert(self.cs.unit == .alu);
            break :l self.compute_result.value;
        },
        .shift => l: {
            std.debug.assert(self.cs.unit == .shift);
            break :l self.compute_result.value;
        },
        .mult => l: {
            std.debug.assert(self.cs.unit == .mult);
            break :l self.compute_result.value;
        },
        .count => l: {
            std.debug.assert(self.cs.unit == .count_extend);
            break :l arch.L.init(self.count_result);
        },
        .ext => l: {
            std.debug.assert(self.cs.unit == .count_extend);
            break :l self.compute_result.value;
        },
        .at_info => arch.L.init(self.last_at_info.raw()),
        .status => arch.L.init((arch.Status {
            .z = self.flags.contains(.stat_z),
            .n = self.flags.contains(.stat_n),
            .c = self.flags.contains(.stat_c),
            .v = self.flags.contains(.stat_v),
            .k = self.flags.contains(.stat_k),
            .a = self.flags.contains(.stat_a),
            .pipeline = self.pipe,
            .rsn = self.rsn,
            .top = arch.Write_Index.init(self.jri.raw()),
            .pucs = self.pucs,
        }).raw()),
        .d => self.read_d(arch.L),
    };

    switch (self.cs.dir) {
        .none, .read => {},
        .write_from_l => self.write_d(l),
        .write_from_dr_ir => {
            if (self.cs.drw) {
                self.write_d(self.dr);
            } else {
                self.write_d(arch.D.init((self.dr.raw() & 0xFFFF0000) | self.ir.raw()));
            }
        },
    }

    if (self.cs.drw and !any_fault) {
        self.dr = self.read_d(arch.DR);
    }

    if (self.cs.irw and !any_fault) {
        self.ir = arch.IR.init(@truncate(self.dr.raw()));
    }

    var set_guard = false;
    var rsn = self.rsn;
    switch (self.cs.special) {
        .none, .fault_on_overflow, .block_transfer, .trigger_fault => {},
        .set_guard => set_guard = !any_fault,
        .check_guard => {
            const guard = arch.Guarded_Memory_Register.from_frame_and_offset(self.frame, self.va.offset);
            if (guards[self.pipe.raw()].raw() != guard.raw()) {
                self.flags.insert(.guard_mismatch);
            }
        },
        .load_rsn_from_l => if (!any_fault) {
            // Note we use the new RSN for any register writes
            rsn = arch.Register_Set_Number.init(@truncate(l.raw()));
        },
        .toggle_rsn => if (!any_fault) {
            // Note we use the new RSN for any register writes
            rsn = arch.Register_Set_Number.init(rsn.raw() ^ arch.Register_Set_Number.msb.raw());
        },
    }

    if (self.flags.contains(.write)) {
        const guard = arch.Guarded_Memory_Register.from_frame_and_offset(self.frame, self.va.offset);
        const cur_pipe = self.pipe.raw();
        for (0.., guards) |p, *gr| {
            if (p != cur_pipe and gr.raw() == guard.raw()) gr.* = arch.Guarded_Memory_Register.invalid;
        }
    }

    if (set_guard) {
        const guard = arch.Guarded_Memory_Register.from_frame_and_offset(self.frame, self.va.offset);
        guards[self.pipe.raw()] = guard;
    }

    if (!any_fault) {
        if (self.cs.seqop == .fault_return and self.emode.is_fault()) {
            self.uca.slot = arch.microcode.Slot.init(@intCast(l.raw() >> 20));
        }

        if (self.cs.gprw) registers[rsn.raw()].reg[self.wi.raw()] = arch.Reg.init(l.raw());

        const sr1wsrc = self.cs.sr1wsrc;
        if (sr1wsrc != .no_write) {
            registers[rsn.raw()].sr1[self.cs.sr1wi.raw()] = arch.Reg.init(switch (sr1wsrc) {
                .no_write => unreachable,
                .self => self.sr1d.raw(),
                .l => l.raw(),
                .virtual_addr => self.va.raw(),
            });
        }

        const sr2wsrc = self.cs.sr2wsrc;
        if (sr2wsrc != .no_write) {
            const wi = self.cs.sr2wi;
            const value = arch.Reg.init(switch (sr2wsrc) {
                .no_write => unreachable,
                .self => self.sr2d.raw(),
                .l => l.raw(),
                .virtual_addr => self.va.raw(),
            });
            registers[rsn.raw()].sr2[wi.raw()] = value;

            if (wi == .asn) {
                self.asn4 = at.Entry.ASN4.init(@truncate(value.raw()));
            }
        }

        if (self.flags.contains(.add_at_k)) {
            self.flags.insert(.at_k);
        } else if (self.flags.contains(.remove_at_k)) {
            self.flags.remove(.at_k);
        }

        if (self.cs.seqop == .next_instruction) {
            self.flags.setPresent(.stat_k, self.flags.contains(.at_k));
        }

        switch (self.cs.statop) {
            .hold => {},
            .zn_from_l => {
                self.flags.setPresent(.stat_z, 0 == l.raw());
                self.flags.setPresent(.stat_n, 0 != (l.raw() & 0x8000_000));
            },
            .clear_a => {
                self.flags.remove(.stat_a);
            },
            .set_a => {
                self.flags.insert(.stat_a);
            },
            .compute => {
                self.flags.setPresent(.stat_z, 0 == l.raw());
                self.flags.setPresent(.stat_n, 0 != (l.raw() & 0x8000_000));
                self.flags.setPresent(.stat_c, self.compute_result.cout);
                self.flags.setPresent(.stat_v, self.compute_result.vout);
            },
            .compute_no_set_z => {
                if (l.raw() != 0) self.flags.remove(.stat_z);
                self.flags.setPresent(.stat_n, 0 != (l.raw() & 0x8000_000));
                self.flags.setPresent(.stat_c, self.compute_result.cout);
                self.flags.setPresent(.stat_v, self.compute_result.vout);
            },
            .load_zncv, .load_zncva_ti => {
                const stat = arch.Status.init(l.raw());
                self.flags.setPresent(.stat_z, stat.z);
                self.flags.setPresent(.stat_n, stat.n);
                self.flags.setPresent(.stat_c, stat.c);
                self.flags.setPresent(.stat_v, stat.v);
                if (self.cs.statop == .load_zncva_ti) {
                    self.flags.setPresent(.stat_a, stat.a);
                    self.ti = stat.top;
                }
            },
        }

        if (self.cs.statop != .load_zncva_ti and self.cs.tiw) {
            self.ti = self.wi;
        }
    }

    const atop = self.cs.atop;
    if (atop != .none) {
        translations[self.at_entry_addr.raw()] = switch (atop) {
            .none => unreachable,
            .translate => .{
                .primary = self.matching_entry,
                .secondary = self.other_entry,
            },
            .update => .{
                .primary = at.Entry.init(l.raw()),
                .secondary = self.other_entry,
            },
            .invalidate => entries: {
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

                break :entries .{
                    .primary = matching,
                    .secondary = other,
                };
            },
        };
        self.last_at_info = .{
            .emode = self.emode,
            .bus_dir = self.cs.dir,
            .bus_width = self.cs.width,
            .op = self.cs.atop,
            .space = self.cs.vaspace,
            .page = self.va.page,
        };
    }

    self.next_stage = .decode;
}

fn read_d(self: *Pipeline_State, comptime T: type) T {
    const n0: u1 = @truncate(self.va.offset.raw());
    const n1: u1 = @truncate(self.va.offset.raw() >> 1);

    const lo = switch (n1) {
        0 => self.da.raw(),
        1 => self.db.raw(),
    };
    const hi = switch (n1) {
        0 => self.db.raw(),
        1 => self.da.raw(),
    };

    var raw_d: arch.D.Raw = bits.concat(.{ lo, hi });
    if (n0 == 1) {
        raw_d >>= 8;
    }

    return T.init(raw_d);
}

fn write_d(self: *Pipeline_State, d: anytype) void {
    const n0: u1 = @truncate(self.va.offset.raw());
    const n1: u1 = @truncate(self.va.offset.raw() >> 1);

    var raw_d: arch.D.Raw = d.raw();
    if (n0 == 1) {
        raw_d <<= 8;
    }

    self.da = arch.DA.init(switch (n1) {
        0 => @truncate(raw_d),
        1 => @truncate(raw_d >> 16),
    });
    self.db = arch.DB.init(switch (n1) {
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
        .block_transfer = self.flags.contains(.block_transfer),
        .update_frame_state = self.flags.contains(.update_frame_state),
    };
}

const Pipeline_State = @This();

const Bus_Control = @import("Bus_Control.zig");
const at = arch.addr.translation;
const arch = @import("arch");
const bits = @import("bits");
const std = @import("std");
