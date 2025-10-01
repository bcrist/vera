pub const Encoding_Database = @import("iedb/Encoding_Database.zig");
pub const Decoding_Database = @import("iedb/Decoding_Database.zig");

pub fn get(i: usize) isa.Instruction.Form {
    const params = data.param_ranges[i];
    const constraints = data.constraint_ranges[i];
    const encoders = data.encoder_ranges[i];
    return .{
        .signature = .{
            .mnemonic = data.mnemonics[i],
            .params = data.param_signatures[params.offset..][0..params.len],
        },
        .constraints = data.constraints[constraints.offset..][0..constraints.len],
        .encoders = data.encoders[encoders.offset..][0..encoders.len],
    };
}

const isa = @import("isa");
const data = @import("iedb_data");
const std = @import("std");
