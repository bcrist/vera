const std = @import("std");
const ie = @import("instruction_encoding");
const Assembler = @import("Assembler.zig");
const SourceFile = @import("SourceFile.zig");
const Section = @import("Section.zig");



pub fn doFixedOrgLayout(a: *Assembler, chunks: std.ArrayListUnmanaged(SourceFile.Chunk)) bool {
    _ = chunks;
    var layout_changed = false;
    for (a.files.items, 0..) |*file, file_handle| {
        if (doFixedOrgLayoutFile(a, file, @intCast(SourceFile.Handle, file_handle))) {
            layout_changed = true;
        }
    }
    return layout_changed;
}

fn doFixedOrgLayoutFile(a: *Assembler, file: *const SourceFile, file_handle: SourceFile.Handle) bool {
    _ = a;
    _ = file;
    _ = file_handle;
    return false;
    // var address: ?u32 = null;
    // for (file.instructions.items(.operation), 0..) |op, insn_handle| {
    //     if (address) |ip| {

    //     }

    //     if (address == null) switch (op) {
    //         .org => {

    //         },
    //     };


    //         if (insn.mnemonic != ._reserved) {
    //             var encoding_iter = edb.getMatchingEncodings(.{
    //                 .mnemonic = insn.mnemonic,
    //                 .suffix = insn.suffix,
    //                 .params = &.{}, // TODO params
    //             });
    //             const old_length = insn.length;
    //             insn.encoding = null;
    //             while (encoding_iter.next()) |enc| {
    //                 const length = ie.getInstructionLength(enc);
    //                 const use_this_encoding = if (insn.length) |cur_length| length < cur_length else true;
    //                 if (use_this_encoding) {
    //                     insn.encoding = enc;
    //                     insn.length = length;
    //                     insn.address = ip;
    //                 }
    //             }
    //             if (insn.encoding == null) {
    //                 try data.errors.append(alloc, .{
    //                     .token = insn.token,
    //                     .desc = "There is no possible encoding for this instruction",
    //                 });
    //                 missing_encoding = true;
    //             }
    //             if (std.meta.eql(old_length, insn.length)) {
    //                 try_again = true;
    //             }
    //             if (insn.length) |length| {
    //                 ip += length;
    //             }
    //         }
    //     }

}

pub fn doAutoOrgLayout(a: *Assembler, chunks: std.ArrayListUnmanaged(SourceFile.Chunk)) bool {
    _ = chunks;
    var layout_changed = false;
    for (a.files.items, 0..) |*file, file_handle| {
        if (doAutoOrgLayoutFile(a, file, @intCast(SourceFile.Handle, file_handle))) {
            layout_changed = true;
        }
    }
    return layout_changed;
}

fn doAutoOrgLayoutFile(a: *Assembler, file: *const SourceFile, file_handle: SourceFile.Handle) bool {
    _ = a;
    _ = file;
    _ = file_handle;
    return false;
}