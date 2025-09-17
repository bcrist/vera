pub const Control_Signal = std.meta.FieldEnum(Control_Signals);

pub const Pipeline = enum (u2) {
    zero = 0,
    one = 1,
    two = 2,
    three = 3,

    pub inline fn init(raw_value: Raw) Pipeline {
        return @enumFromInt(raw_value);
    }

    pub inline fn raw(self: Pipeline) Raw {
        return @intFromEnum(self);
    }

    pub const format = fmt.format_enum_dec;

    pub const Raw = meta.Backing(Pipeline); 
    pub const count = std.math.maxInt(Raw) + 1;

    pub const Stage = enum (u2) {
        // - IR is decoded
        // - Register file read and write indices are computed
        // - The microcode slot to execute is selected
        // - Control signals needed in the Setup stage are decoded
        decode = 0,

        // - Data is looked up in the register file
        // - Data is driven to the `J` and `K` buses
        // - A virtual address is calculated (if needed)
        // - Control signals needed in the Compute stage are decoded
        setup = 1,

        // - Compute units operate on `J` and `K` buses
        // - Address translation is performed, generating the physical address for the next stage
        // - The word-addresses for `DA`, `DB` are generated, along with other bus-control signals for the next stage
        // - Any faults must be detected by the end of the stage
        compute = 2,

        // - Data is driven to the `L` and `D`/`DA`/`DB` buses
        //    - If there is a fault:
        //       - Information about the current address translation is stored to `ATR`
        //    - If there is not a fault:
        //       - System bus writes are performed
        //       - `L` and `D` buses may be connected if necessary
        //       - `DR`/`IR`/`ATR` may be loaded from `D` if necessary
        //       - General-purpose and special-register files are written if necessary
        //       - Flags are updated if necessary
        //       - Guard registers may be updated
        //       - Address translation entries are updated if necessary
        //       - When returning from a fault handler, the previous microcode slot to return to is prepared
        transact = 3,

        pub inline fn init(raw_value: Stage.Raw) Stage {
            return @enumFromInt(raw_value);
        }

        pub inline fn raw(self: Stage) Stage.Raw {
            return @intFromEnum(self);
        }

        pub const format = fmt.format_enum_dec;

        pub const Raw = meta.Backing(Stage); 
        pub const count = std.math.maxInt(Stage.Raw) + 1;
    };
};

pub const Execution_Mode = enum (u2) {
    normal = 0,
    interrupt = 1,
    fault = 2,
    interrupt_fault = 3,

    pub inline fn init(raw_value: Raw) Execution_Mode {
        return @enumFromInt(raw_value);
    }

    pub inline fn raw(self: Execution_Mode) Raw {
        return @intFromEnum(self);
    }

    pub inline fn is_fault(self: Execution_Mode) bool {
        return switch (self) {
            .normal, .interrupt => false,
            .fault, .interrupt_fault => true,
        };
    }

    pub inline fn is_interrupt(self: Execution_Mode) bool {
        return switch (self) {
            .normal, .fault => false,
            .interrupt, .interrupt_fault => true,
        };
    }

    pub const format = fmt.format_enum_dec;

    pub const Raw = meta.Backing(Execution_Mode); 
};

// When there is no work for a pipeline to do, it should execute the `PARK` instruction.
// This will set this control signal to .sleep, which allows certain parts of the pipeline
// to enter low-power mode for the cycle, and if all pipelines are parked simultaneously,
// the CPU clock can be stopped entirely, until an interrupt is ready to be serviced.
pub const Power_Mode = enum (u1) {
    run = 0,
    sleep = 1,

    pub inline fn init(raw_value: Raw) Power_Mode {
        return @enumFromInt(raw_value);
    }

    pub inline fn raw(self: Power_Mode) Raw {
        return @intFromEnum(self);
    }

    pub const format = fmt.format_enum_dec;

    pub const Raw = meta.Backing(Power_Mode); 
};

pub const Sequencer_Op = enum (u2) {
    next_uop = 0,
    next_instruction = 1,
    next_uop_force_normal = 2,
    fault_return = 3,

    pub inline fn init(raw_value: Raw) Sequencer_Op {
        return @enumFromInt(raw_value);
    }

    pub inline fn raw(self: Sequencer_Op) Raw {
        return @intFromEnum(self);
    }

    pub const format = fmt.format_enum_dec;

    pub const Raw = meta.Backing(Sequencer_Op); 
};

pub const Special_Op = enum (u4) {
    none = 0,
    set_guard = 1,
    check_guard = 2,
    load_fucs_from_l = 3,
    load_rsn_from_l = 4,
    read_from_other_rsn = 5, // note: never affects virtual address base
    write_to_other_rsn = 6,
    read_and_write_other_rsn = 7, // note: never affects virtual address base
    fault_on_overflow = 8,
    // 9...14 unused
    trigger_fault = 15,

    pub inline fn init(raw_value: Raw) Special_Op {
        return @enumFromInt(raw_value);
    }

    pub inline fn raw(self: Special_Op) Raw {
        return @intFromEnum(self);
    }

    pub const format = fmt.format_enum_dec;

    pub const Raw = meta.Backing(Special_Op); 
};

pub const Generic_Write_Enable = enum (u1) {
    hold = 0,
    write = 1,

    pub inline fn init(raw_value: Raw) Generic_Write_Enable {
        return @enumFromInt(raw_value);
    }

    pub inline fn raw(self: Generic_Write_Enable) Raw {
        return @intFromEnum(self);
    }

    pub const format = fmt.format_enum_dec;

    pub const Raw = meta.Backing(Generic_Write_Enable);
};

pub const Interrupt_Enable = enum (u1) {
    disallow = 0,
    allow = 1,

    pub inline fn init(raw_value: Raw) Interrupt_Enable {
        return @enumFromInt(raw_value);
    }

    pub inline fn raw(self: Interrupt_Enable) Raw {
        return @intFromEnum(self);
    }

    pub const format = fmt.format_enum_dec;

    pub const Raw = meta.Backing(Interrupt_Enable);
};

const Control_Signals = @import("Control_Signals.zig");

const fmt = @import("fmt.zig");
const meta = @import("meta");
const std = @import("std");
