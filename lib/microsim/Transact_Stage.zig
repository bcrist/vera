pipeline: hw.Pipeline,
cs: *const Control_Signals,

exec_mode: hw.Execution_Mode,
rsn: hw.RSN,
uc_slot: hw.microcode.Slot,
dr: hw.D,
ij: hw.IJ,
ik: hw.IK,
iw: hw.IW,
asn: at.ASN,
last_translation: at.Info,
stat_c: bool,
stat_v: bool,
stat_n: bool,
stat_z: bool,
stat_k: bool,
stat_a: bool,
next_k: bool,
want_atomic: bool,
stall_atomic: bool,

sr1: hw.SR,
sr2: hw.SR,
jh: hw.JH,
alu_data: hw.L,
alu_z: bool,
alu_n: bool,
alu_c: bool,
alu_v: bool,
shift_data: hw.L,
shift_z: bool,
shift_n: bool,
shift_c: bool,
shift_v: bool,
mult_data: hw.L,
mult_z: bool,
mult_n: bool,
mult_v: bool,
count_data: hw.L,
count_z: bool,
virtual_addr: hw.addr.Virtual,
physical_addr: hw.addr.Physical,
at_info: at.Info,
at_entry_addr: at.Entry_Address,
at_matching_entry: at.Entry,
at_other_entry: at.Entry,
at_k: bool,
page_fault: bool,
page_align_fault: bool,
access_fault: bool,
special_fault: bool,
any_fault: bool,
inhibit_writes: bool,

pub const Debug = struct {
    l: hw.L,
    d: hw.D,
    write_addr: ?hw.addr.Physical,
};

pub fn is_atomic(self: Transact_Stage) bool {
    return self.want_atomic and !self.stall_atomic;
}

pub fn read(self: Transact_Stage) ?hw.addr.Physical {
    if (self.cs.at_op != .translate) return null;
    if (self.inhibit_writes) return null;
    return switch (self.cs.bus_dir) {
        .read, .read_to_dr => self.physical_addr,
        .write_from_dr, .write_from_ll => null,
    };
}

pub fn simulate(
    in: Transact_Stage,
    out: *Decode_Stage,
    read_data: hw.D,
    power: hw.Power_Mode,
    reset: bool,
    registers: []hw.Register_Set,
    translations: []at.Entry_Pair,
    interrupts_pending: *const[hw.Pipeline.count]bool
) Debug {
    // Note this happens even if in.inhibit_writes is true (because it's used for page fault resolution)
    out.last_translation = if (in.cs.at_op == .none) in.last_translation else in.at_info;

    var d = if (in.cs.bus_dir == .write_from_dr) in.dr else read_data;
    const l = in.compute_l(d, out.last_translation, power);

    var write_addr: ?hw.addr.Physical = null;
    out.dr = in.dr;
    switch (in.cs.bus_dir) {
        .read => {},
        .read_to_dr => {
            if (in.cs.at_op != .translate) d = hw.D.init(l.lo.raw());
            if (!in.inhibit_writes) out.dr = d;
        },
        .write_from_dr => {
            d = in.dr;
            if (!in.inhibit_writes) write_addr = in.physical_addr;
        },
        .write_from_ll => {
            d = hw.D.init(l.lo.raw());
            if (!in.inhibit_writes) write_addr = in.physical_addr;
        },
    }

    in.simulate_register_file(out, l, registers);
    in.simulate_status_register(out, l);
    in.simulate_address_translator(l, translations);
    in.simulate_sequencer(out, l.hi, reset, interrupts_pending);

    out.pipeline = in.pipeline;
    out.cs = in.cs;
    // out.exec_mode = in.exec_mode;
    // out.rsn = in.rsn;
    // out.uc_slot = in.uc_slot;
    // out.dr = in.dr;
    out.ij = in.ij;
    out.ik = in.ik;
    out.iw = in.iw;
    // out.asn = in.asn;
    // out.last_translation = in.last_translation;
    // out.stat_c = in.stat_c;
    // out.stat_v = in.stat_v;
    // out.stat_n = in.stat_n;
    // out.stat_z = in.stat_z;
    // out.stat_k = in.stat_k;
    // out.stat_a = in.stat_a;
    // out.next_k = in.next_k;
    // out.want_atomic = in.want_atomic;

    out.inhibit_writes = in.inhibit_writes;

    return .{
        .l = l,
        .d = d,
        .write_addr = write_addr,
    };
}

fn compute_l(in: Transact_Stage, d: hw.D, at_info: at.Info, power: hw.Power_Mode) hw.L {
    return .{
        .lo = hw.LL.init(switch (in.cs.ll_src) {
            .zero => 0,
            .compute_l => switch (in.cs.unit) {
                .alu => in.alu_data.lo,
                .shift => in.shift_data.lo,
                .mult => in.mult_data.lo,
                .count => in.count_data.lo,
            }.raw(),
            .d => d.raw(),
            .d8_sx => bits.sx(u16, d.raw8()),
            .translation_info_l => @truncate(at_info.raw()),
            .stat => (hw.Status {
                .z = in.stat_z,
                .n = in.stat_n,
                .c = in.stat_c,
                .v = in.stat_v,
                .k = in.stat_k,
                .a = in.stat_a,
                .power = power,
                .pipeline = in.pipeline,
                .exec = in.exec_mode,
            }).raw(),
            .pipeline => in.pipeline.raw(),
            _ => 0xBADC,
        }),
        .hi = hw.LH.init(switch (in.cs.lh_src) {
            .zero => 0,
            .compute_h => switch (in.cs.unit) {
                .alu => in.alu_data.hi,
                .shift => in.shift_data.hi,
                .mult => in.mult_data.hi,
                .count => in.count_data.hi,
            }.raw(),
            .d_sx => bits.sx(u16, @as(u1, @truncate(d.raw() >> 15))),
            .d8_sx => bits.sx(u16, @as(u1, @truncate(d.raw8() >> 7))),
            .translation_info_h => @truncate(at_info.raw() >> 16),
            .jh => in.jh.raw(),
            .prev_uc_slot => in.uc_slot.raw(),
            _ => 0xBADC,
        }),
    };
}

fn simulate_register_file(in: Transact_Stage, out: *Decode_Stage, l: hw.L, registers: []hw.Register_Set) void {
    out.rsn = in.rsn;
    out.asn = in.asn;

    if (in.inhibit_writes) {
        return;
    }

    switch (in.cs.special) {
        .load_rsn_from_ll => {
            out.rsn = hw.RSN.init(@truncate(l.lo.raw()));
        },
        .toggle_rsn => {
            out.rsn = hw.RSN.init(in.rsn.raw() ^ 0x20);
        },
        else => {},
    }

    // N.B. When the RSN changes, we use the new value for addressing writes.
    // This is important for being able to switch to another RSN while saving the previous RSN.
    const rs = &registers[out.rsn.raw()];
    const iw = in.iw.raw();

    switch (in.cs.reg_write) {
        .no_write => {},
        .write_16 => {
            rs.reg[iw] = hw.R.init(l.lo.raw());
        },
        .write_32 => {
            rs.reg[iw] = hw.R.init(l.lo.raw());
            rs.reg[iw^1] = hw.R.init(l.hi.raw());
        },
        _ => {},
    }

    switch (in.cs.sr1_wsrc) {
        .no_write => {},
        .rsn_sr1 => {
            rs.sr1[in.cs.sr1_wi.raw()] = .{
                .lo = in.sr1.lo,
                .hi = hw.SRH.init(in.rsn.raw()),
            };
        },
        .l => {
            rs.sr1[in.cs.sr1_wi.raw()] = hw.SR.init(l.raw());
        },
        .virtual_addr => {
            rs.sr1[in.cs.sr1_wi.raw()] = hw.SR.init(in.virtual_addr.raw());
        },
    }

    switch (in.cs.sr2_wsrc) {
        .no_write => {},
        .sr2 => {
            rs.sr2[in.cs.sr2_wi.raw()] = in.sr2;
            if (in.cs.sr2_wi == .asn) {
                out.asn = at.ASN.init(@truncate(in.sr2.raw()));
            }
        },
        .l => {
            rs.sr2[in.cs.sr2_wi.raw()] = hw.SR.init(l.raw());
            if (in.cs.sr2_wi == .asn) {
                out.asn = at.ASN.init(@truncate(l.lo.raw()));
            }
        },
        .virtual_addr => {
            rs.sr2[in.cs.sr2_wi.raw()] = hw.SR.init(in.virtual_addr.raw());
            if (in.cs.sr2_wi == .asn) {
                out.asn = at.ASN.init(@truncate(in.virtual_addr.raw()));
            }
        },
    }
}

fn simulate_status_register(in: Transact_Stage, out: *Decode_Stage, l: hw.L) void {
    out.stat_z = in.stat_z;
    out.stat_n = in.stat_n;
    out.stat_c = in.stat_c;
    out.stat_v = in.stat_v;
    out.stat_k = in.stat_k;
    out.stat_a = in.stat_a;
    out.next_k = in.next_k;

    if (in.inhibit_writes) return;

    out.next_k = in.at_k;
    switch (in.cs.seq_op) {
        // next_k is a latch, so it has already been updated from in.at_k if necessary
        .next_instruction => out.stat_k = out.next_k,
        .next_uop, .next_uop_force_normal, .fault_return => {},
    }

    switch (in.cs.stat_op) {
        .hold => {},
        .load_zncv => {
            const ll_bits = hw.Status.init(l.lo.raw());
            out.stat_z = ll_bits.z;
            out.stat_n = ll_bits.n;
            out.stat_v = ll_bits.v;
            out.stat_c = ll_bits.c;
        },
        .load_zncvka => {
            const ll_bits = hw.Status.init(l.lo.raw());
            out.stat_z = ll_bits.z;
            out.stat_n = ll_bits.n;
            out.stat_v = ll_bits.v;
            out.stat_c = ll_bits.c;
            out.stat_k = ll_bits.k;
            out.stat_a = ll_bits.a;
        },
        .clear_a => {
            out.stat_a = false;
        },
        .set_a => {
            out.stat_a = true;
        },
        .compute, .compute_no_set_z => {
            switch (in.cs.unit) {
                .alu => {
                    out.stat_z = in.alu_z;
                    out.stat_n = in.alu_n;
                    out.stat_v = in.alu_v;
                    out.stat_c = in.alu_c;
                },
                .shift => {
                    out.stat_z = in.shift_z;
                    out.stat_n = in.shift_n;
                    out.stat_v = in.shift_v;
                    out.stat_c = in.shift_c;
                },
                .mult => {
                    out.stat_z = in.mult_z;
                    out.stat_n = in.mult_n;
                    out.stat_v = in.mult_v;
                },
                .count => {
                    out.stat_z = in.count_z;
                },
            }
            if (in.cs.stat_op == .compute_no_set_z and !in.stat_z) {
                out.stat_z = false;
            }
        },
        .zn_from_ll => {
            out.stat_z = l.lo.raw() == 0;
            out.stat_n = (l.lo.raw() >> 15) == 1;
        },
    }
}

fn simulate_address_translator(in: Transact_Stage, l: hw.L, translations: []at.Entry_Pair) void {
    if (in.inhibit_writes) return;

    switch (in.cs.at_op) {
        .none => {},
        .translate => {
            translations[in.at_entry_addr.raw()] = .{
                .primary = in.at_matching_entry,
                .secondary = in.at_other_entry,
            };
        },
        .update => {
            translations[in.at_entry_addr.raw()] = .{
                .primary = at.Entry.init(l.raw()),
                .secondary = in.at_other_entry,
            };
        },
        .invalidate => {
            const tag_mask: u14 = @truncate(l.lo.raw());

            var matching = in.at_matching_entry;
            var other = in.at_other_entry;

            if ((in.at_info.tag.raw() & tag_mask) == (matching.tag.raw() & tag_mask) and matching.present) {
                matching.present = false;
            }

            if ((in.at_info.tag.raw() & tag_mask) == (other.tag.raw() & tag_mask) and other.present) {
                other.present = false;
            }

            translations[in.at_entry_addr.raw()] = .{
                .primary = matching,
                .secondary = other,
            };
        },
    }
}

fn simulate_sequencer(in: Transact_Stage, out: *Decode_Stage, lh: hw.LH, reset: bool, interrupts_pending: *const [hw.Pipeline.count]bool) void {
    out.uc_slot = in.uc_slot;
    out.exec_mode = in.exec_mode;
    out.want_atomic = in.want_atomic;
    out.uc_slot_src = .continuation;
    out.seq_literal = .reset;

    if (reset) {
        out.exec_mode = .interrupt_fault;
        out.uc_slot_src = .seq_literal;
        out.want_atomic = false;
        return;
    }

    if (in.stall_atomic) {
        out.uc_slot_src = .hold;
        return;
    }

    if (!in.inhibit_writes) switch (in.cs.special) {
        .atomic_next => out.want_atomic = true,
        .atomic_end, .atomic_this => out.want_atomic = false,
        else => {},
    };

    if (in.exec_mode.is_fault()) {
        if (in.any_fault) {
            out.exec_mode = .fault;
            out.uc_slot_src = .seq_literal;
            out.seq_literal = .double_fault;
            return;
        }

        switch (in.cs.seq_op) {
            .next_uop => {},
            .next_instruction => out.uc_slot_src = .insn_decoder,
            .next_uop_force_normal => out.exec_mode = .normal,
            .fault_return => {
                out.exec_mode = if (in.exec_mode.is_interrupt()) .interrupt else .normal;
                out.uc_slot_src = .hold;
                out.uc_slot = hw.microcode.Slot.init(@truncate(lh.raw()));
            },
        }
        return;
    }

    if (in.any_fault) {
        out.exec_mode = if (in.exec_mode.is_interrupt()) .interrupt_fault else .fault;
        out.uc_slot_src = .seq_literal;
        if (in.page_fault) {
            out.seq_literal = .page_fault;
        } else if (in.access_fault) {
            out.seq_literal = .access_fault;
        } else if (in.page_align_fault) {
            out.seq_literal = .page_align_fault;
        } else if (in.special_fault) {
            out.uc_slot_src = .continuation;
        }
        return;
    }

    const interrupt_pending = interrupts_pending[in.pipeline.raw()];
    if (in.exec_mode == .normal and interrupt_pending and in.cs.allow_int and !in.want_atomic) {
        out.exec_mode = .interrupt;
        out.uc_slot_src = .seq_literal;
        out.seq_literal = .interrupt;
        return;
    }

    switch (in.cs.seq_op) {
        .next_uop => {},
        .next_uop_force_normal => out.exec_mode = .normal,
        .next_instruction => out.uc_slot_src = .insn_decoder,
        .fault_return => {
            out.exec_mode = .fault;
            out.uc_slot_src = .seq_literal;
            out.seq_literal = .instruction_protection_fault;
        },
    }
}

const Transact_Stage = @This();
const Decode_Stage = @import("Decode_Stage.zig");
const Control_Signals = hw.Control_Signals;
const at = hw.addr.translation;
const hw = arch.hw;
const arch = @import("lib_arch");
const bits = @import("bits");
const std = @import("std");
