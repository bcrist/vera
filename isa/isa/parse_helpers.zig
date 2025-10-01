
// pub fn parse_mnemonic(self: anytype) ?isa.Mnemonic {
//     const has_sync = @hasDecl(@TypeOf(self.*), "sync_to_end_of_line");
//     if (has_sync and self.sync_to_end_of_line) return null;
//     if (!self.try_token(.id)) return null;

//     const mnemonic_str = self.token_location(self.next_token - 1);
//     if (mnemonic_map.get(mnemonic_str)) |mnemonic| {
//         return mnemonic;
//     }

//     self.record_error_rel("Unrecognized mnemonic", -1);
//     if (has_sync) self.sync_to_end_of_line = true;
//     return null;
// }




const isa = @import("../isa.zig");
const arch = @import("arch"); 
const std = @import("std");
