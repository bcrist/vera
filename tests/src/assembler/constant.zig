test "init_int" {
    {
        var constant = Constant.init_int(@as(u1, 0));
        try std.testing.expectEqual(@as(i64, 1), constant.bit_count);
        try std.testing.expectEqualSlices(u8, "\x00", constant.as_string());
    }
    {
        var constant = Constant.init_int(@as(u1, 1));
        try std.testing.expectEqual(@as(i64, 2), constant.bit_count);
        try std.testing.expectEqualSlices(u8, "\x01", constant.as_string());
    }
    {
        var constant = Constant.init_int(@as(i1, 0));
        try std.testing.expectEqual(@as(i64, -1), constant.bit_count);
        try std.testing.expectEqualSlices(u8, "\x00", constant.as_string());
    }
    {
        var constant = Constant.init_int(@as(i1, -1));
        try std.testing.expectEqual(@as(i64, -1), constant.bit_count);
        try std.testing.expectEqualSlices(u8, "\xFF", constant.as_string());
    }
    {
        var constant = Constant.init_int(@as(u7, 0));
        try std.testing.expectEqual(@as(i64, 1), constant.bit_count);
        try std.testing.expectEqualSlices(u8, "\x00", constant.as_string());
    }
    {
        var constant = Constant.init_int(@as(u7, 127));
        try std.testing.expectEqual(@as(i64, 8), constant.bit_count);
        try std.testing.expectEqualSlices(u8, "\x7F", constant.as_string());
    }
    {
        var constant = Constant.init_int(@as(i7, 0));
        try std.testing.expectEqual(@as(i64, -1), constant.bit_count);
        try std.testing.expectEqualSlices(u8, "\x00", constant.as_string());
    }
    {
        var constant = Constant.init_int(@as(i7, 63));
        try std.testing.expectEqual(@as(i64, -7), constant.bit_count);
        try std.testing.expectEqualSlices(u8, "\x3F", constant.as_string());
    }
    {
        var constant = Constant.init_int(@as(i7, -64));
        try std.testing.expectEqual(@as(i64, -7), constant.bit_count);
        try std.testing.expectEqualSlices(u8, "\xC0", constant.as_string());
    }
}

test "init_int_bits" {
    try std.testing.expectEqualSlices(u8, "\x03\x00\x00\x00\x00\x00\x00\x00", (try Constant.init_int_bits(@as(i64, 3), 64)).as_string());
    try std.testing.expectEqualSlices(u8, "\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF", (try Constant.init_int_bits(@as(i64, -1), 64)).as_string());
    try std.testing.expectEqualSlices(u8, "\xFE\xFF\xFF\xFF\xFF\xFF\xFF\xFF", (try Constant.init_int_bits(@as(i64, -2), 64)).as_string());
}

test "init_int_literal" {
    var temp = std.ArrayListUnmanaged(u8) {};
    defer temp.deinit(std.testing.allocator);

    var constant = try Constant.init_int_literal(std.testing.allocator, &temp, "0");
    try std.testing.expectEqual(@as(i64, 1), constant.bit_count);
    try std.testing.expectEqual(@as(i64, 0), try constant.as_int(i64));

    constant = try Constant.init_int_literal(std.testing.allocator, &temp, "13");
    try std.testing.expectEqual(@as(i64, 5), constant.bit_count);
    try std.testing.expectEqual(@as(i64, 13), try constant.as_int(i64));

    constant = try Constant.init_int_literal(std.testing.allocator, &temp, "0xFF");
    try std.testing.expectEqual(@as(i64, 8), constant.bit_count);
    try std.testing.expectEqualSlices(u8, "\xFF", constant.as_string());
    try std.testing.expectEqual(@as(i64, 255), try constant.as_int(i64));

    constant = try Constant.init_int_literal(std.testing.allocator, &temp, "0xDEADBEEFDEADBEEFDEADBEEFDEADBEEF");
    try std.testing.expectEqual(@as(i64, 128), constant.bit_count);
    try std.testing.expectEqualSlices(u8, "\xEF\xBE\xAD\xDE\xEF\xBE\xAD\xDE\xEF\xBE\xAD\xDE\xEF\xBE\xAD\xDE", constant.as_string());

    constant = try Constant.init_int_literal(std.testing.allocator, &temp, "0b1010101");
    try std.testing.expectEqual(@as(i64, 7), constant.bit_count);
    try std.testing.expectEqualSlices(u8, "\x55", constant.as_string());

    constant = try Constant.init_int_literal(std.testing.allocator, &temp, "0b001010101");
    try std.testing.expectEqual(@as(i64, 9), constant.bit_count);
    try std.testing.expectEqualSlices(u8, "\x55\x00", constant.as_string());

    constant = try Constant.init_int_literal(std.testing.allocator, &temp, "0q_0000_0000_0000_0000_0000_3210_3210_3210_3210_3210_3210_3210_3210");
    try std.testing.expectEqual(@as(i64, 104), constant.bit_count);
    try std.testing.expectEqualSlices(u8, "\xE4\xE4\xE4\xE4\xE4\xE4\xE4\xE4\x00\x00\x00\x00\x00", constant.as_string());
}

test "clone_with_signedness" {
    var temp = std.ArrayListUnmanaged(u8) {};
    defer temp.deinit(std.testing.allocator);
    var temp2 = std.ArrayListUnmanaged(u8) {};
    defer temp2.deinit(std.testing.allocator);

    {
        var constant = try Constant.init_int_literal(std.testing.allocator, &temp, "255");
        var clone_constant = constant.clone_with_signedness(std.testing.allocator, &temp2, .signed);
        try std.testing.expectEqual(@as(i64, -9), clone_constant.bit_count);
        try std.testing.expectEqual(@as(i64, 255), try clone_constant.as_int(i64));
    }
    {
        var constant = try Constant.init_int_literal(std.testing.allocator, &temp, "0xFF");
        try std.testing.expectEqual(@as(i64, 8), constant.bit_count);
        try std.testing.expectEqual(@as(i64, 0xFF), try constant.as_int(i64));
        var clone_constant = constant.clone_with_signedness(std.testing.allocator, &temp2, .signed);
        try std.testing.expectEqual(@as(i64, -8), clone_constant.bit_count);
        try std.testing.expectEqual(@as(i64, -1), try clone_constant.as_int(i64));
    }
}

test "as_string" {
    try std.testing.expectEqualSlices(u8, "\x00", Constant.init_int(@as(i64, 0)).as_string());
    try std.testing.expectEqualSlices(u8, "\x00", Constant.init_int(@as(u64, 0)).as_string());
    try std.testing.expectEqualSlices(u8, "\xD2\x04", Constant.init_int(@as(i64, 1234)).as_string());
    try std.testing.expectEqualSlices(u8, "\x2E\xFB", Constant.init_int(@as(i64, -1234)).as_string());
    try std.testing.expectEqualSlices(u8, "abcdefghijklmnopqrstuvwxyz", Constant.init_string("abcdefghijklmnopqrstuvwxyz").as_string());
    try std.testing.expectEqualSlices(u8, "ab", (try Constant.init_string_bits("abcdefghijklmnopqrstuvwxyz", 16, .unsigned)).as_string());
}

test "as_int" {
    const alloc = std.testing.allocator;
    var temp = std.ArrayListUnmanaged(u8) {};
    defer temp.deinit(alloc);

    try std.testing.expectEqual(@as(i64, 0), try Constant.init_int(@as(i7, 0)).as_int(i64));
    try std.testing.expectEqual(@as(i64, 0), try (try Constant.init_int_bits(@as(i64, 0), 64)).as_int(i64));
    try std.testing.expectEqual(@as(i64, 1234), try Constant.init_int(@as(u16, 1234)).as_int(i64));
    try std.testing.expectEqual(@as(i64, -1), try Constant.init_int(@as(i1, -1)).as_int(i64));
    try std.testing.expectEqual(@as(i64, -1), try (try Constant.init_int_bits(@as(i64, -1), 64)).as_int(i64));
    try std.testing.expectEqual(@as(i64, -1234), try Constant.init_int(@as(i16, -1234)).as_int(i64));
    try std.testing.expectError(error.Overflow, Constant.init_string("abcdefghijklmnopqrstuvwxyz").as_int(i64));
    try std.testing.expectError(error.Overflow, (try Constant.init_int_literal(alloc, &temp, "0xFFFF_FFFF_FFFF_FFFF_FFFF_FFFF")).as_int(i64));
}

test "concat" {
    const alloc = std.testing.allocator;
    var temp = std.ArrayListUnmanaged(u8) {};
    defer temp.deinit(alloc);

    const abc = Constant.init_string("abc");
    const def = Constant.init_string("def");

    // 01100001 00111011 01110011 01101100 01100100 01101011 01100110 01101010 00100000 01100001 00111011 01110011 01101100 01100100 01101011 01100110 01101010
    const long = Constant.init_string("a;sldkfj a;sldkfj");

    const bits3 = try Constant.init_int_bits(@as(i64, -1), 3);
    const hexFFF = try Constant.init_int_bits(@as(i64, -1), 12);
    const hex000 = try Constant.init_int_bits(@as(i64, 0), 12);
    const hexA51 = try Constant.init_int_bits(@as(u64, 0xA51), 12); // 0b1010_0101_0001
    const bits7 = try Constant.init_int_bits(@as(i64, -1), 7);
    const bits23 = try Constant.init_int_bits(@as(i64, -1), 23);

    try std.testing.expectEqualSlices(u8, "abcdef", (abc.concat(alloc, &temp, def)).as_string());
    try std.testing.expectEqualSlices(u8, "defabc", (def.concat(alloc, &temp, abc)).as_string());
    try std.testing.expectEqualSlices(u8, "abca;sldkfj a;sldkfj", abc.concat(alloc, &temp, long).as_string());
    try std.testing.expectEqualSlices(u8, "\x8F\x52", (bits3.concat(alloc, &temp, hexA51)).as_string()); // 0b101_0010_1000_1111
    try std.testing.expectEqualSlices(u8, "\x00\xF0\xFF", (hex000.concat(alloc, &temp, hexFFF)).as_string());
    try std.testing.expectEqualSlices(u8, "\xFF\xFF\xFF\xFF", (bits7.concat(alloc, &temp, bits23)).as_string());
    try std.testing.expectEqualSlices(u8, "\xFF\xFF\xFF\xFF\xFF\xFF", (bits23.concat(alloc, &temp, bits23)).as_string());

    // 01100001 00111011 01110011 01101100 01100100 01101011 01100110 01101010 00100000 01100001 00111011 01110011 01101100 01100100 01101011 01100110 01101010
    // 01101010 01100110 01101011 01100100 01101100 01110011 00111011 01100001 00100000 01101010 01100110 01101011 01100100 01101100 01110011 00111011 01100001 111
    // 011_0101_0011_0011_0011_0101_1011_0010_0011_0110_0011_1001_1001_1101_1011_0000_1001_0000_0011_0101_0011_0011_0011_0101_1011_0010_0011_0110_0011_1001_1001_1101_1011_0000_1111
    // 0x3  5    3    3    3    5    B    2    3    6    3    9    9    D    B    0    9    0    3    5    3    3    3    5    B    2    3    6    3    9    9    D    B    0    F
    // 0x3_53_33_5B_23_63_99_DB_09_03_53_33_5B_23_63_99_DB_0F
    // 011010
    try std.testing.expectEqualSlices(u8, "\x0F\xDB\x99\x63\x23\x5B\x33\x53\x03\x09\xDB\x99\x63\x23\x5B\x33\x53\x03", bits3.concat(alloc, &temp, long).as_string());
    try std.testing.expectEqualSlices(u8, "\xFF\xFF\xFF\xB0\x9D\x39\x36\xB2\x35\x33\x35\x90\xB0\x9D\x39\x36\xB2\x35\x33\x35", bits23.concat(alloc, &temp, long).as_string());
}

test "repeat" {
    const alloc = std.testing.allocator;
    var temp = std.ArrayListUnmanaged(u8) {};
    defer temp.deinit(alloc);

    const abc = Constant.init_string("abc");
    const de = Constant.init_string("de");
    const bits1 = try Constant.init_int_bits(@as(i64, -1), 1);
    const hexFFF = try Constant.init_int_bits(@as(u64, 0xFFF), 12);

    try std.testing.expectEqualSlices(u8, "", abc.repeat(alloc, &temp, 0).as_string());
    try std.testing.expectEqualSlices(u8, "abc", abc.repeat(alloc, &temp, 1).as_string());
    try std.testing.expectEqualSlices(u8, "abcabc", abc.repeat(alloc, &temp, 2).as_string());
    try std.testing.expectEqualSlices(u8, "dededede", de.repeat(alloc, &temp, 4).as_string());
    try std.testing.expectEqualSlices(u8, "dededededededededede", de.repeat(alloc, &temp, 10).as_string());
    try std.testing.expectEqualSlices(u8, "\xFF\xFF\xFF", hexFFF.repeat(alloc, &temp, 2).as_string());
    try std.testing.expectEqualSlices(u8, "\xFF\xFF\xFF\xFF\x0F", hexFFF.repeat(alloc, &temp, 3).as_string());
    try std.testing.expectEqualSlices(u8, "\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF", bits1.repeat(alloc, &temp, 72).as_string());
}

test "binary_op" {
    const alloc = std.testing.allocator;
    var temp = std.ArrayListUnmanaged(u8) {};
    defer temp.deinit(alloc);

    const one = Constant.init_int(@as(i64, 1));
    const neg1 = Constant.init_int(@as(i64, -1));
    const hex7FF = Constant.init_int(@as(u64, 0x7FF));
    const neg1_12 = try Constant.init_int_bits(@as(i64, -1), 12);

    const long = Constant.init_string("\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF");

    try std.testing.expectEqual(@as(i64, 12), hex7FF.bit_count);
    try std.testing.expectEqual(@as(i64, -12), hex7FF.binary_op(alloc, &temp, neg1, .add).bit_count);
    try std.testing.expectEqual(@as(i64, 0x7FE), try hex7FF.binary_op(alloc, &temp, neg1, .add).as_int(i64));

    try std.testing.expectEqual(@as(i64, 12), hex7FF.bit_count);
    try std.testing.expectEqual(@as(i64, -13), hex7FF.binary_op(alloc, &temp, neg1, .subtract).bit_count);
    try std.testing.expectEqual(@as(i64, 0x800), try hex7FF.binary_op(alloc, &temp, neg1, .subtract).as_int(i64));

    try std.testing.expectEqual(@as(i64, -2), neg1_12.binary_op(alloc, &temp, neg1_12, .add).bit_count);
    try std.testing.expectEqual(@as(i64, -2), try neg1_12.binary_op(alloc, &temp, neg1_12, .add).as_int(i64));

    try std.testing.expectEqual(@as(i64, -1), neg1_12.binary_op(alloc, &temp, neg1_12, .subtract).bit_count);
    try std.testing.expectEqual(@as(i64, 0), try neg1_12.binary_op(alloc, &temp, neg1_12, .subtract).as_int(i64));

    try std.testing.expectEqualSlices(u8, "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01", long.binary_op(alloc, &temp, one, .add).as_string());
}

const Constant = assembler.Constant;
const assembler = @import("assembler");
const std = @import("std");
