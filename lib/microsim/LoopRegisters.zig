const std = @import("std");
const misc = @import("misc");
const at_types = @import("address_translator_types");
const uc = @import("microcode");
const bus = @import("bus_types");

const Simulator = @import("Simulator");
const StatusRegister = Simulator.StatusRegister;
const AddressTranslator = Simulator.AddressTranslator;

const LoopRegisters = @This();

exec_mode: misc.ExecutionMode,
rsn: misc.RegistersetNumber,
ua: uc.Address,
dl: bus.D,
oa: misc.OperandA,
ob: misc.OperandB,
stat: StatusRegister,
asn: at_types.AddressSpaceNumber,
last_translation: at_types.TranslationInfo,

pub fn init() LoopRegisters {
    return .{
        .exec_mode = .fault,
        .rsn = 0,
        .ua = 0,
        .dl = 0,
        .oa = 0,
        .ob = 0,
        .stat = .{
            .c = false,
            .v = false,
            .n = false,
            .z = false,
            .next_k = false,
            .k = false,
            .a = false,
        },
        .asn = 0,
        .last_translation = at_types.TranslationInfo{},
    };
}

pub fn randomize(self: *LoopRegisters, rnd: std.rand.Random) void {
    self.exec_mode = rnd.enumValue(misc.ExecutionMode);
    self.rsn = rnd.int(misc.RegistersetNumber);
    self.ua = rnd.int(uc.Address);
    self.dl = rnd.int(bus.D);
    self.oa = rnd.int(misc.OperandA);
    self.ob = rnd.int(misc.OperandB);
    self.stat = .{
        .c = rnd.boolean(),
        .v = rnd.boolean(),
        .n = rnd.boolean(),
        .z = rnd.boolean(),
        .next_k = true, // This must be guaranteed by reset logic
        .k = true, // This must be guaranteed by reset logic
        .a = rnd.boolean(),
    };
    self.asn = rnd.int(at_types.AddressSpaceNumber);
    self.last_translation = at_types.TranslationInfo.random(rnd);
}
