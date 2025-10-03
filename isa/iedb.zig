pub const Encoding_Database = @import("iedb/Encoding_Database.zig");
pub const Decoding_Database = @import("iedb/Decoding_Database.zig");

pub fn get_mnemonic(id: usize) isa.Mnemonic {
    return data.mnemonics[id];
}

pub fn get(id: usize) isa.Instruction.Form {
    const params = data.param_ranges[id];
    const constraints = data.constraint_ranges[id];
    return .{
        .id = @intCast(id),
        .signature = .{
            .mnemonic = data.mnemonics[id],
            .params = data.param_signatures[params.offset..][0..params.len],
        },
        .constraints = data.constraints[constraints.offset..][0..constraints.len],
        .encoder_data = &data.encoders,
        .encoder_indices = data.encoder_ranges[id],
    };
}

const isa = @import("isa");
const data = @import("iedb_data");
const std = @import("std");
