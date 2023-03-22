const std = @import("std");
const ie = @import("instruction_encoding");
const ie_data = @import("instruction_encoding_data").data;

test "Instruction encoding" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    var temp = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const edb = try ie.EncoderDatabase.init(arena.allocator(), ie_data, temp.allocator());
    const ddb = try ie.DecoderDatabase.init(arena.allocator(), ie_data, temp.allocator());
    temp.deinit();

    try ie.testIdempotence(&ddb, &edb, 2, .{
        .mnemonic = .ADD,
        .suffix = .none,
        .params = &[_]ie.Parameter{
            ie.parameter(.reg32, 12),
            ie.parameter(.constant, -128),
            ie.toParameter(.reg32, 1),
        },
    });
    try ie.testIdempotence(&ddb, &edb, 1, .{
        .mnemonic = .ADD,
        .suffix = .none,
        .params = &[_]ie.Parameter{
            ie.parameter(.reg32, 12),
            ie.parameter(.constant, 65500),
            ie.toParameter(.reg32, 1),
        },
    });
    try ie.testIdempotence(&ddb, &edb, 1, .{
        .mnemonic = .ADD,
        .suffix = .none,
        .params = &[_]ie.Parameter{
            ie.parameter(.reg32, 0),
            ie.parameter(.reg16u, 4),
            ie.toParameter(.reg32, 0),
        },
    });
    try ie.testIdempotence(&ddb, &edb, 1, .{
        .mnemonic = .ADD,
        .suffix = .none,
        .params = &[_]ie.Parameter{
            ie.parameter(.reg32, 1),
            ie.parameter(.reg16s, 1),
            ie.toParameter(.reg32, 1),
        },
    });
    for ([_]ie.Mnemonic{ .ADD, .ADDC }) |mnemonic| {
        try ie.testIdempotence(&ddb, &edb, 2, .{
            .mnemonic = mnemonic,
            .suffix = .none,
            .params = &[_]ie.Parameter{
                ie.parameter(.reg16, 1),
                ie.parameter(.constant, 123),
                ie.toParameter(.reg16, 2),
            },
        });
        try ie.testIdempotence(&ddb, &edb, 1, .{
            .mnemonic = mnemonic,
            .suffix = .none,
            .params = &[_]ie.Parameter{
                ie.parameter(.reg16, 1),
                ie.parameter(.reg16, 2),
                ie.toParameter(.reg16, 1),
            },
        });
    }

    for ([_]ie.Mnemonic{ .CMP, .CMPB }) |mnemonic| {
        try ie.testIdempotence(&ddb, &edb, 1, .{
            .mnemonic = mnemonic,
            .suffix = .none,
            .params = &[_]ie.Parameter{
                ie.parameter(.reg16, 1),
                ie.parameter(.reg16, 2),
            },
        });
        try ie.testIdempotence(&ddb, &edb, 1, .{
            .mnemonic = mnemonic,
            .suffix = .none,
            .params = &[_]ie.Parameter{
                ie.parameter(.reg16, 1),
                ie.parameter(.constant, 2),
            },
        });
    }
    try ie.testIdempotence(&ddb, &edb, 1, .{
        .mnemonic = .CMP,
        .suffix = .none,
        .params = &[_]ie.Parameter{
            ie.parameter(.reg32, 1),
            ie.parameter(.reg16u, 2),
        },
    });
    try ie.testIdempotence(&ddb, &edb, 1, .{
        .mnemonic = .CMP,
        .suffix = .none,
        .params = &[_]ie.Parameter{
            ie.parameter(.reg32, 1),
            ie.parameter(.reg16s, 2),
        },
    });
    try ie.testIdempotence(&ddb, &edb, 1, .{
        .mnemonic = .CMP,
        .suffix = .none,
        .params = &[_]ie.Parameter{
            ie.parameter(.reg32, 1),
            ie.parameter(.constant, 22345),
        },
    });
    try ie.testIdempotence(&ddb, &edb, 1, .{
        .mnemonic = .CMP,
        .suffix = .none,
        .params = &[_]ie.Parameter{
            ie.parameter(.reg32, 1),
            ie.parameter(.constant, -22345),
        },
    });

    try ie.testIdempotence(&ddb, &edb, 1, .{
        .mnemonic = .SUB,
        .suffix = .none,
        .params = &[_]ie.Parameter{
            ie.parameter(.reg32, 1),
            ie.parameter(.reg16u, 2),
            ie.toParameter(.reg32, 1),
        },
    });
    try ie.testIdempotence(&ddb, &edb, 1, .{
        .mnemonic = .SUB,
        .suffix = .none,
        .params = &[_]ie.Parameter{
            ie.parameter(.reg32, 1),
            ie.parameter(.reg16s, 2),
            ie.toParameter(.reg32, 1),
        },
    });
    for ([_]ie.Mnemonic{ .SUB, .SUBB }) |mnemonic| {
        try ie.testIdempotence(&ddb, &edb, 2, .{
            .mnemonic = mnemonic,
            .suffix = .none,
            .params = &[_]ie.Parameter{
                ie.parameter(.constant, 23),
                ie.parameter(.reg16, 2),
                ie.toParameter(.reg16, 1),
            },
        });
        try ie.testIdempotence(&ddb, &edb, 1, .{
            .mnemonic = mnemonic,
            .suffix = .none,
            .params = &[_]ie.Parameter{
                ie.parameter(.constant, 65535),
                ie.parameter(.reg16, 2),
                ie.toParameter(.reg16, 1),
            },
        });
        try ie.testIdempotence(&ddb, &edb, 1, .{
            .mnemonic = mnemonic,
            .suffix = .none,
            .params = &[_]ie.Parameter{
                ie.parameter(.reg16, 1),
                ie.parameter(.reg16, 2),
                ie.toParameter(.reg16, 1),
            },
        });
    }

    for ([_]ie.Mnemonic{ .INC, .INCC, .DEC, .DECB }) |mnemonic| {
        try ie.testIdempotence(&ddb, &edb, 1, .{
            .mnemonic = mnemonic,
            .suffix = .none,
            .params = &[_]ie.Parameter{
                ie.parameter(.reg16, 6),
                ie.toParameter(.reg16, 6),
            },
        });
        try ie.testIdempotence(&ddb, &edb, 1, .{
            .mnemonic = mnemonic,
            .suffix = .none,
            .params = &[_]ie.Parameter{
                ie.parameter(.reg32, 7),
                ie.toParameter(.reg32, 7),
            },
        });
    }

    for ([_]ie.Mnemonic{ .NEG, .NEGB }) |mnemonic| {
        try ie.testIdempotence(&ddb, &edb, 1, .{
            .mnemonic = mnemonic,
            .suffix = .none,
            .params = &[_]ie.Parameter{
                ie.parameter(.reg16, 6),
                ie.toParameter(.reg16, 6),
            },
        });
    }

    try ie.testIdempotence(&ddb, &edb, 1, .{
        .mnemonic = .NOT,
        .suffix = .none,
        .params = &[_]ie.Parameter{
            ie.parameter(.reg16, 6),
            ie.toParameter(.reg16, 6),
        },
    });

    for ([_]ie.Mnemonic{ .XOR, .XNOR, .OR, .NOR, .AND, .NAND, .ANDNOT }) |mnemonic| {
        try ie.testIdempotence(&ddb, &edb, 1, .{
            .mnemonic = mnemonic,
            .suffix = .none,
            .params = &[_]ie.Parameter{
                ie.parameter(.reg16, 6),
                ie.parameter(.reg16, 15),
                ie.toParameter(.reg16, 6),
            },
        });
    }
    for ([_]ie.Mnemonic{ .XOR, .OR, .NOR, .AND, .NAND }) |mnemonic| {
        try ie.testIdempotence(&ddb, &edb, 1, .{
            .mnemonic = mnemonic,
            .suffix = .none,
            .params = &[_]ie.Parameter{
                ie.parameter(.reg16, 6),
                ie.parameter(.constant, 1337),
                ie.toParameter(.reg16, 2),
            },
        });
    }

    for ([_]ie.Mnemonic{ .TEST, .TESTZ }) |mnemonic| {
        try ie.testIdempotence(&ddb, &edb, 1, .{
            .mnemonic = mnemonic,
            .suffix = .none,
            .params = &[_]ie.Parameter{
                ie.parameter(.reg16, 6),
                ie.parameter(.reg16, 15),
            },
        });
    }

    for ([_]ie.Mnemonic{ .TESTB, .TESTBZ }) |mnemonic| {
        try ie.testIdempotence(&ddb, &edb, 1, .{
            .mnemonic = mnemonic,
            .suffix = .none,
            .params = &[_]ie.Parameter{
                ie.parameter(.reg16, 6),
                ie.parameter(.constant, 15),
            },
        });
    }

    for ([_]ie.Mnemonic{ .CLRB, .SETB, .TGLB }) |mnemonic| {
        try ie.testIdempotence(&ddb, &edb, 1, .{
            .mnemonic = mnemonic,
            .suffix = .none,
            .params = &[_]ie.Parameter{
                ie.parameter(.reg16, 6),
                ie.parameter(.constant, 0),
                ie.toParameter(.reg16, 6),
            },
        });
    }

    try ie.testIdempotence(&ddb, &edb, 1, .{
        .mnemonic = .SHR,
        .suffix = .none,
        .params = &[_]ie.Parameter{
            ie.parameter(.reg32u, 6),
            ie.parameter(.reg16, 1),
            ie.toParameter(.reg32u, 6),
        },
    });
    try ie.testIdempotence(&ddb, &edb, 1, .{
        .mnemonic = .SHR,
        .suffix = .none,
        .params = &[_]ie.Parameter{
            ie.parameter(.reg16u, 6),
            ie.parameter(.reg16, 1),
            ie.toParameter(.reg16u, 6),
        },
    });
    try ie.testIdempotence(&ddb, &edb, 1, .{
        .mnemonic = .SHR,
        .suffix = .none,
        .params = &[_]ie.Parameter{
            ie.parameter(.reg16s, 6),
            ie.parameter(.reg16, 1),
            ie.toParameter(.reg16s, 6),
        },
    });
    try ie.testIdempotence(&ddb, &edb, 1, .{
        .mnemonic = .SHR,
        .suffix = .none,
        .params = &[_]ie.Parameter{
            ie.parameter(.reg16u, 6),
            ie.parameter(.constant, 1),
            ie.toParameter(.reg16u, 6),
        },
    });
    try ie.testIdempotence(&ddb, &edb, 1, .{
        .mnemonic = .SHR,
        .suffix = .none,
        .params = &[_]ie.Parameter{
            ie.parameter(.reg16s, 6),
            ie.parameter(.constant, 1),
            ie.toParameter(.reg16s, 6),
        },
    });
    try ie.testIdempotence(&ddb, &edb, 1, .{
        .mnemonic = .SHR,
        .suffix = .none,
        .params = &[_]ie.Parameter{
            ie.parameter(.reg32u, 6),
            ie.parameter(.constant, 1),
            ie.toParameter(.reg32u, 6),
        },
    });
    try ie.testIdempotence(&ddb, &edb, 1, .{
        .mnemonic = .SHR,
        .suffix = .none,
        .params = &[_]ie.Parameter{
            ie.parameter(.reg32u, 6),
            ie.parameter(.constant, 18),
            ie.toParameter(.reg32u, 6),
        },
    });

    try ie.testIdempotence(&ddb, &edb, 1, .{
        .mnemonic = .SHL,
        .suffix = .none,
        .params = &[_]ie.Parameter{
            ie.parameter(.reg32, 6),
            ie.parameter(.reg16, 1),
            ie.toParameter(.reg32, 6),
        },
    });
    try ie.testIdempotence(&ddb, &edb, 1, .{
        .mnemonic = .SHL,
        .suffix = .none,
        .params = &[_]ie.Parameter{
            ie.parameter(.reg16, 6),
            ie.parameter(.reg16, 1),
            ie.toParameter(.reg16, 6),
        },
    });
    try ie.testIdempotence(&ddb, &edb, 1, .{
        .mnemonic = .SHL,
        .suffix = .none,
        .params = &[_]ie.Parameter{
            ie.parameter(.reg16, 6),
            ie.parameter(.constant, 3),
            ie.toParameter(.reg16, 6),
        },
    });
    try ie.testIdempotence(&ddb, &edb, 1, .{
        .mnemonic = .SHL,
        .suffix = .none,
        .params = &[_]ie.Parameter{
            ie.parameter(.reg32, 6),
            ie.parameter(.constant, 1),
            ie.toParameter(.reg32, 6),
        },
    });
    try ie.testIdempotence(&ddb, &edb, 1, .{
        .mnemonic = .SHL,
        .suffix = .none,
        .params = &[_]ie.Parameter{
            ie.parameter(.reg32, 6),
            ie.parameter(.constant, 31),
            ie.toParameter(.reg32, 6),
        },
    });

    try ie.testIdempotence(&ddb, &edb, 1, .{
        .mnemonic = .SHRC,
        .suffix = .none,
        .params = &[_]ie.Parameter{
            ie.parameter(.reg16, 6),
            ie.toParameter(.reg16, 6),
        },
    });

    try ie.testIdempotence(&ddb, &edb, 1, .{
        .mnemonic = .SHLC,
        .suffix = .none,
        .params = &[_]ie.Parameter{
            ie.parameter(.reg16, 6),
            ie.toParameter(.reg16, 6),
        },
    });

    try ie.testIdempotence(&ddb, &edb, 1, .{
        .mnemonic = .MUL,
        .suffix = .none,
        .params = &[_]ie.Parameter{
            ie.parameter(.reg16, 6),
            ie.parameter(.constant, 11111),
            ie.toParameter(.reg16, 13),
        },
    });
    try ie.testIdempotence(&ddb, &edb, 1, .{
        .mnemonic = .MUL,
        .suffix = .none,
        .params = &[_]ie.Parameter{
            ie.parameter(.reg16, 6),
            ie.parameter(.reg16, 1),
            ie.toParameter(.reg16, 6),
        },
    });
    try ie.testIdempotence(&ddb, &edb, 1, .{
        .mnemonic = .MULH,
        .suffix = .none,
        .params = &[_]ie.Parameter{
            ie.parameter(.reg16u, 6),
            ie.parameter(.reg16u, 1),
            ie.toParameter(.reg16u, 6),
        },
    });
    try ie.testIdempotence(&ddb, &edb, 1, .{
        .mnemonic = .MULH,
        .suffix = .none,
        .params = &[_]ie.Parameter{
            ie.parameter(.reg16s, 6),
            ie.parameter(.reg16u, 1),
            ie.toParameter(.reg16s, 6),
        },
    });
    try ie.testIdempotence(&ddb, &edb, 1, .{
        .mnemonic = .MULH,
        .suffix = .none,
        .params = &[_]ie.Parameter{
            ie.parameter(.reg16u, 6),
            ie.parameter(.reg16s, 1),
            ie.toParameter(.reg16s, 6),
        },
    });
    try ie.testIdempotence(&ddb, &edb, 1, .{
        .mnemonic = .MULH,
        .suffix = .none,
        .params = &[_]ie.Parameter{
            ie.parameter(.reg16s, 6),
            ie.parameter(.reg16s, 1),
            ie.toParameter(.reg16s, 6),
        },
    });

    try ie.testIdempotence(&ddb, &edb, 1, .{
        .mnemonic = .MUL,
        .suffix = .none,
        .params = &[_]ie.Parameter{
            ie.parameter(.reg16u, 6),
            ie.parameter(.reg16u, 1),
            ie.toParameter(.reg32u, 0),
        },
    });
    try ie.testIdempotence(&ddb, &edb, 1, .{
        .mnemonic = .MUL,
        .suffix = .none,
        .params = &[_]ie.Parameter{
            ie.parameter(.reg16u, 6),
            ie.parameter(.reg16s, 1),
            ie.toParameter(.reg32s, 0),
        },
    });
    try ie.testIdempotence(&ddb, &edb, 1, .{
        .mnemonic = .MUL,
        .suffix = .none,
        .params = &[_]ie.Parameter{
            ie.parameter(.reg16s, 6),
            ie.parameter(.reg16s, 1),
            ie.toParameter(.reg32s, 0),
        },
    });

    for ([_]ie.Mnemonic{ .CB, .CZ, .CTB, .CTZ, .CLB, .CLZ }) |mnemonic| {
        try ie.testIdempotence(&ddb, &edb, 1, .{
            .mnemonic = mnemonic,
            .suffix = .none,
            .params = &[_]ie.Parameter{
                ie.parameter(.reg16, 6),
                ie.toParameter(.reg16, 6),
            },
        });
    }

    // TODO .BB, .BBN
    for ([_]ie.Mnemonic{ .NOP, .WFI, .BP, .BPN }) |mnemonic| {
        try ie.testIdempotence(&ddb, &edb, 1, .{
            .mnemonic = mnemonic,
            .suffix = .none,
            .params = &[_]ie.Parameter{},
        });
    }

    for ([_]i64{ 0, 5, 33, 63, 64, 77, -4, -16, -63, -64, -318, -319 }) |offset| {
        var n: usize = 2;
        if (offset == 0) {
            n = 1;
        } else if (offset == 63 or offset == -64) {
            n = 3;
        }
        try ie.testIdempotence(&ddb, &edb, 1, .{
            .mnemonic = .B,
            .suffix = .none,
            .params = &[_]ie.Parameter{
                ie.parameterWithOffset(.IP, 0, .constant, 0),
            },
        });
    }
    try ie.testIdempotence(&ddb, &edb, 1, .{
        .mnemonic = .B,
        .suffix = .none,
        .params = &[_]ie.Parameter{
            ie.parameterWithOffset(.IP, 0, .reg16u, 0),
        },
    });
    try ie.testIdempotence(&ddb, &edb, 1, .{
        .mnemonic = .B,
        .suffix = .none,
        .params = &[_]ie.Parameter{
            ie.parameterWithOffset(.IP, 0, .reg16s, 0),
        },
    });
    try ie.testIdempotence(&ddb, &edb, 1, .{
        .mnemonic = .B,
        .suffix = .none,
        .params = &[_]ie.Parameter{
            ie.parameter(.ptr32i, 2),
        },
    });
    try ie.testIdempotence(&ddb, &edb, 1, .{
        .mnemonic = .B,
        .suffix = .none,
        .params = &[_]ie.Parameter{
            ie.parameter(.constant, 0),
        },
    });
    try ie.testIdempotence(&ddb, &edb, 1, .{
        .mnemonic = .B,
        .suffix = .none,
        .params = &[_]ie.Parameter{
            ie.parameter(.constant, 12345678),
        },
    });

    for ([_]ie.MnemonicSuffix{ .Z, .NZ, .LU, .NLU, .GU, .NGU, .N, .NN, .C, .NC, .LS, .NLS, .GS, .NGS, .P, .NP }) |suffix| {
        for ([_]i64{ 4, 14, 15, 270, -2, -6, -7, -262 }) |offset| {
            var n: usize = 2;
            if (suffix == .Z or suffix == .NZ) {
                if (offset >= 15 and offset <= 63) {
                    n = 3;
                } else if (offset >= -64 and offset <= -7) {
                    n = 3;
                }
            }
            try ie.testIdempotence(&ddb, &edb, n, .{
                .mnemonic = .B,
                .suffix = suffix,
                .params = &[_]ie.Parameter{
                    ie.parameterWithOffset(.IP, 0, .constant, offset),
                },
            });
        }
    }

    try ie.testIdempotence(&ddb, &edb, 1, .{
        .mnemonic = .EAB,
        .suffix = .none,
        .params = &[_]ie.Parameter{
            ie.parameter(.ptr32i, 0),
        },
    });
    try ie.testIdempotence(&ddb, &edb, 1, .{
        .mnemonic = .DAB,
        .suffix = .none,
        .params = &[_]ie.Parameter{
            ie.parameter(.ptr32i, 0),
        },
    });

    for ([_]ie.MnemonicSuffix{ .LU_GU, .LU_Z, .GU_Z, .LS_GS, .LS_Z, .GS_Z, .N_Z, .P_Z, .N_P }) |suffix| {
        for ([_]i64{ 4, 270, -6, -262 }) |offset1| {
            for ([_]i64{ 3, 44, -5, -22222 }) |offset2| {
                try ie.testIdempotence(&ddb, &edb, 1, .{
                    .mnemonic = .B,
                    .suffix = suffix,
                    .params = &[_]ie.Parameter{
                        ie.parameterWithOffset(.IP, 0, .constant, offset1),
                        ie.parameterWithOffset(.IP, 0, .constant, offset2),
                    },
                });
            }
        }
    }

    for ([_]i64{ 0, 12345, -123 }) |offset| {
        try ie.testIdempotence(&ddb, &edb, 1, .{
            .mnemonic = .CALL,
            .suffix = .none,
            .params = &[_]ie.Parameter{
                ie.parameterWithOffset(.IP, 0, .constant, offset),
            },
        });
    }
    try ie.testIdempotence(&ddb, &edb, 1, .{
        .mnemonic = .CALL,
        .suffix = .none,
        .params = &[_]ie.Parameter{
            ie.parameter(.constant, 76543210),
        },
    });
    try ie.testIdempotence(&ddb, &edb, 1, .{
        .mnemonic = .CALL,
        .suffix = .none,
        .params = &[_]ie.Parameter{
            ie.parameter(.ptr32i, 7),
        },
    });

    for ([_]ie.Mnemonic{.RET}) |mnemonic| {
        try ie.testIdempotence(&ddb, &edb, 1, .{
            .mnemonic = mnemonic,
            .suffix = .none,
            .params = &[_]ie.Parameter{},
        });
    }

    try ie.testIdempotence(&ddb, &edb, 1, .{
        .mnemonic = .C,
        .suffix = .none,
        .params = &[_]ie.Parameter{
            ie.parameter(.reg16, 11),
            ie.toParameter(.reg16, 12),
        },
    });
    try ie.testIdempotence(&ddb, &edb, 1, .{
        .mnemonic = .C,
        .suffix = .none,
        .params = &[_]ie.Parameter{
            ie.parameter(.reg16u, 11),
            ie.toParameter(.reg32, 12),
        },
    });
    try ie.testIdempotence(&ddb, &edb, 1, .{
        .mnemonic = .C,
        .suffix = .none,
        .params = &[_]ie.Parameter{
            ie.parameter(.reg16s, 11),
            ie.toParameter(.reg32, 12),
        },
    });
    try ie.testIdempotence(&ddb, &edb, 1, .{
        .mnemonic = .C,
        .suffix = .none,
        .params = &[_]ie.Parameter{
            ie.parameter(.reg32, 11),
            ie.toParameter(.reg32, 12),
        },
    });
    for ([_]ie.BaseExpressionType{ .IP, .SP }) |base| {
        try ie.testIdempotence(&ddb, &edb, 1, .{
            .mnemonic = .C,
            .suffix = .none,
            .params = &[_]ie.Parameter{
                ie.parameterWithOffset(base, 0, .reg16s, 11),
                ie.toParameter(.reg32, 12),
            },
        });
        try ie.testIdempotence(&ddb, &edb, 2, .{
            .mnemonic = .C,
            .suffix = .none,
            .params = &[_]ie.Parameter{
                ie.parameterWithOffset(base, 0, .constant, 3),
                ie.toParameter(.reg32, 12),
            },
        });
        try ie.testIdempotence(&ddb, &edb, 2, .{
            .mnemonic = .C,
            .suffix = .none,
            .params = &[_]ie.Parameter{
                ie.parameterWithOffset(base, 0, .constant, -3),
                ie.toParameter(.reg32, 12),
            },
        });
        try ie.testIdempotence(&ddb, &edb, 1, .{
            .mnemonic = .C,
            .suffix = .none,
            .params = &[_]ie.Parameter{
                ie.parameterWithOffset(base, 0, .constant, 60000),
                ie.toParameter(.reg32, 12),
            },
        });
        try ie.testIdempotence(&ddb, &edb, 1, .{
            .mnemonic = .C,
            .suffix = .none,
            .params = &[_]ie.Parameter{
                ie.parameterWithOffset(base, 0, .constant, -60000),
                ie.toParameter(.reg32, 12),
            },
        });
    }

    try ie.testIdempotence(&ddb, &edb, 3, .{
        .mnemonic = .C,
        .suffix = .none,
        .params = &[_]ie.Parameter{
            ie.parameter(.constant, 0),
            ie.toParameter(.reg16, 11),
        },
    });
    try ie.testIdempotence(&ddb, &edb, 1, .{
        .mnemonic = .C,
        .suffix = .none,
        .params = &[_]ie.Parameter{
            ie.parameter(.constant, -16),
            ie.toParameter(.reg16, 11),
        },
    });
    try ie.testIdempotence(&ddb, &edb, 4, .{
        .mnemonic = .C,
        .suffix = .none,
        .params = &[_]ie.Parameter{
            ie.parameter(.constant, 0),
            ie.toParameter(.reg32, 11),
        },
    });

    try ie.testIdempotence(&ddb, &edb, 1, .{
        .mnemonic = .C,
        .suffix = .none,
        .params = &[_]ie.Parameter{
            ie.parameter(.reg16, 2),
            ie.toParameter(.reg16, 4),
            ie.toParameter(.reg16, 7),
        },
    });
    try ie.testIdempotence(&ddb, &edb, 1, .{
        .mnemonic = .C,
        .suffix = .none,
        .params = &[_]ie.Parameter{
            ie.parameter(.reg32, 2),
            ie.toParameter(.reg32, 4),
            ie.toParameter(.reg32, 7),
        },
    });

    try ie.testIdempotence(&ddb, &edb, 1, .{
        .mnemonic = .DUP,
        .suffix = .none,
        .params = &[_]ie.Parameter{
            ie.parameter(.reg16, 2),
            ie.toParameter(.reg32, 4),
        },
    });

    try ie.testIdempotence(&ddb, &edb, 1, .{
        .mnemonic = .C,
        .suffix = .none,
        .params = &[_]ie.Parameter{
            ie.parameter(.STAT, 0),
            ie.toParameter(.reg16, 4),
        },
    });
    try ie.testIdempotence(&ddb, &edb, 1, .{
        .mnemonic = .C,
        .suffix = .none,
        .params = &[_]ie.Parameter{
            ie.parameter(.reg16, 4),
            ie.toParameter(.STAT, 0),
        },
    });

    try ie.testIdempotence(&ddb, &edb, 1, .{
        .mnemonic = .C,
        .suffix = .none,
        .params = &[_]ie.Parameter{
            ie.parameter(.RP, 0),
            ie.toParameter(.ptr32i, 4),
        },
    });
    try ie.testIdempotence(&ddb, &edb, 1, .{
        .mnemonic = .C,
        .suffix = .none,
        .params = &[_]ie.Parameter{
            ie.parameter(.ptr32i, 4),
            ie.toParameter(.RP, 0),
        },
    });

    for ([_]ie.BaseExpressionType{ .BP, .KXP, .UXP }) |reg| {
        try ie.testIdempotence(&ddb, &edb, 1, .{
            .mnemonic = .C,
            .suffix = .none,
            .params = &[_]ie.Parameter{
                ie.parameter(reg, 0),
                ie.toParameter(.ptr32d, 4),
            },
        });
        try ie.testIdempotence(&ddb, &edb, 1, .{
            .mnemonic = .C,
            .suffix = .none,
            .params = &[_]ie.Parameter{
                ie.parameter(.ptr32d, 4),
                ie.toParameter(reg, 0),
            },
        });
    }

    try ie.testIdempotence(&ddb, &edb, 1, .{
        .mnemonic = .C,
        .suffix = .none,
        .params = &[_]ie.Parameter{
            ie.parameter(.ASN, 0),
            ie.toParameter(.reg32, 4),
        },
    });
    try ie.testIdempotence(&ddb, &edb, 1, .{
        .mnemonic = .C,
        .suffix = .none,
        .params = &[_]ie.Parameter{
            ie.parameter(.reg32, 4),
            ie.toParameter(.ASN, 0),
        },
    });

    for ([_]ie.Mnemonic{.FRET}) |mnemonic| {
        try ie.testIdempotence(&ddb, &edb, 1, .{
            .mnemonic = mnemonic,
            .suffix = .none,
            .params = &[_]ie.Parameter{},
        });
    }

    try ie.testIdempotence(&ddb, &edb, 1, .{
        .mnemonic = .LDRS,
        .suffix = .none,
        .params = &[_]ie.Parameter{
            ie.parameter(.ptr32d, 3),
            ie.toParameter(.reg16, 0),
        },
    });

    try ie.testIdempotence(&ddb, &edb, 1, .{
        .mnemonic = .STRS,
        .suffix = .none,
        .params = &[_]ie.Parameter{
            ie.parameter(.reg16, 0),
            ie.toParameter(.ptr32d, 3),
        },
    });

    try ie.testIdempotence(&ddb, &edb, 1, .{
        .mnemonic = .SRS,
        .suffix = .none,
        .params = &.{
            ie.parameter(.reg16, 13),
        },
    });

    try ie.testIdempotence(&ddb, &edb, 1, .{
        .mnemonic = .IRET,
        .suffix = .none,
        .params = &[_]ie.Parameter{},
    });

    for ([_]ie.BaseExpressionType{ .reg8u, .reg8s, .reg16, .reg32 }) |dest| {
        try ie.testIdempotence(&ddb, &edb, 1, .{
            .mnemonic = .LD,
            .suffix = .I,
            .params = &[_]ie.Parameter{
                ie.parameter(.ptr32i, 5),
                ie.toParameter(dest, 4),
            },
        });
        try ie.testIdempotence(&ddb, &edb, 1, .{
            .mnemonic = .LD,
            .suffix = .S,
            .params = &[_]ie.Parameter{
                ie.parameter(.ptr32s, 5),
                ie.toParameter(dest, 4),
            },
        });
        try ie.testIdempotence(&ddb, &edb, 1, .{
            .mnemonic = .LD,
            .suffix = .D,
            .params = &[_]ie.Parameter{
                ie.parameter(.ptr32d, 5),
                ie.toParameter(dest, 4),
            },
        });
        try ie.testIdempotence(&ddb, &edb, 1, .{
            .mnemonic = .LD,
            .suffix = .D,
            .params = &[_]ie.Parameter{
                ie.parameterWithOffset(.ptr32d, 5, .reg16u, 8),
                ie.toParameter(dest, 0),
            },
        });
        try ie.testIdempotence(&ddb, &edb, 1, .{
            .mnemonic = .LD,
            .suffix = .D,
            .params = &[_]ie.Parameter{
                ie.parameterWithOffset(.ptr32d, 5, .constant, 8),
                ie.toParameter(dest, 0),
            },
        });
        try ie.testIdempotence(&ddb, &edb, 2, .{
            .mnemonic = .LD,
            .suffix = .S,
            .params = &[_]ie.Parameter{
                ie.parameterWithOffset(.SP, 0, .constant, 8),
                ie.toParameter(dest, 3),
            },
        });
        try ie.testIdempotence(&ddb, &edb, 1, .{
            .mnemonic = .LD,
            .suffix = .S,
            .params = &[_]ie.Parameter{
                ie.parameterWithOffset(.SP, 0, .reg16s, 8),
                ie.toParameter(dest, 3),
            },
        });
    }

    for ([_]ie.BaseExpressionType{ .reg8, .reg16, .reg32 }) |src| {
        try ie.testIdempotence(&ddb, &edb, 1, .{
            .mnemonic = .ST,
            .suffix = .S,
            .params = &[_]ie.Parameter{
                ie.parameter(src, 4),
                ie.toParameter(.ptr32s, 5),
            },
        });
        try ie.testIdempotence(&ddb, &edb, 1, .{
            .mnemonic = .ST,
            .suffix = .D,
            .params = &[_]ie.Parameter{
                ie.parameter(src, 4),
                ie.toParameter(.ptr32d, 5),
            },
        });
        try ie.testIdempotence(&ddb, &edb, 1, .{
            .mnemonic = .ST,
            .suffix = .D,
            .params = &[_]ie.Parameter{
                ie.parameter(src, 0),
                ie.toParameterWithOffset(.ptr32d, 5, .reg16u, 8),
            },
        });
        try ie.testIdempotence(&ddb, &edb, 1, .{
            .mnemonic = .ST,
            .suffix = .D,
            .params = &[_]ie.Parameter{
                ie.parameter(src, 0),
                ie.toParameterWithOffset(.ptr32d, 5, .constant, 8),
            },
        });
        try ie.testIdempotence(&ddb, &edb, 2, .{
            .mnemonic = .ST,
            .suffix = .S,
            .params = &[_]ie.Parameter{
                ie.parameter(src, 3),
                ie.toParameterWithOffset(.SP, 0, .constant, 8),
            },
        });
        try ie.testIdempotence(&ddb, &edb, 1, .{
            .mnemonic = .ST,
            .suffix = .S,
            .params = &[_]ie.Parameter{
                ie.parameter(src, 3),
                ie.toParameterWithOffset(.SP, 0, .reg16s, 8),
            },
        });
    }

    for ([_]ie.BaseExpressionType{ .reg8u, .reg8s, .reg16, .reg32 }) |dest| {
        try ie.testIdempotence(&ddb, &edb, 1, .{
            .mnemonic = .LDI,
            .suffix = .D,
            .params = &[_]ie.Parameter{
                ie.parameter(.ptr32d, 5),
                ie.toParameter(dest, 4),
            },
        });
        try ie.testIdempotence(&ddb, &edb, 1, .{
            .mnemonic = .ILD,
            .suffix = .D,
            .params = &[_]ie.Parameter{
                ie.parameter(.ptr32d, 5),
                ie.toParameter(dest, 4),
            },
        });
    }

    for ([_]ie.BaseExpressionType{ .reg8, .reg16, .reg32 }) |src| {
        try ie.testIdempotence(&ddb, &edb, 1, .{
            .mnemonic = .STI,
            .suffix = .D,
            .params = &[_]ie.Parameter{
                ie.parameter(src, 4),
                ie.toParameter(.ptr32d, 5),
            },
        });
        try ie.testIdempotence(&ddb, &edb, 1, .{
            .mnemonic = .IST,
            .suffix = .D,
            .params = &[_]ie.Parameter{
                ie.parameter(src, 4),
                ie.toParameter(.ptr32d, 5),
            },
        });
    }

    for ([_]ie.BaseExpressionType{ .reg16, .reg32 }) |dest| {
        try ie.testIdempotence(&ddb, &edb, 1, .{
            .mnemonic = .LD,
            .suffix = .D,
            .params = &[_]ie.Parameter{
                ie.parameterWithOffset(.UXP, 0, .constant, 8),
                ie.toParameter(dest, 4),
            },
        });
        try ie.testIdempotence(&ddb, &edb, 1, .{
            .mnemonic = .LD,
            .suffix = .D,
            .params = &[_]ie.Parameter{
                ie.parameterWithOffset(.KXP, 0, .constant, 8),
                ie.toParameter(dest, 4),
            },
        });
    }

    for ([_]ie.Mnemonic{ .MCR, .MCRB, .MCF, .MCFB, .SI, .SIB, .SO, .SOB }) |mnemonic| {
        try ie.testIdempotence(&ddb, &edb, 1, .{
            .mnemonic = mnemonic,
            .suffix = .D,
            .params = &[_]ie.Parameter{
                ie.parameter(.reg32s, 5),
                ie.parameter(.BP, 0),
                ie.toParameter(.RP, 0),
            },
        });
    }

    for ([_]ie.MnemonicSuffix{ .W, .R, .S, .I }) |suffix| {
        try ie.testIdempotence(&ddb, &edb, 1, .{
            .mnemonic = .SAT,
            .suffix = suffix,
            .params = &.{
                ie.parameter(.reg32, 5),
                ie.toParameter(.reg32, 0),
            },
        });
    }

    for ([_]ie.MnemonicSuffix{ .W, .R, .S, .I }) |suffix| {
        try ie.testIdempotence(&ddb, &edb, 1, .{
            .mnemonic = .RAT,
            .suffix = suffix,
            .params = &.{
                ie.parameter(.reg16, 5),
            },
        });
    }







    try ie.testIdempotence(&ddb, &edb, 1, .{
        .mnemonic = .C,
        .suffix = .none,
        .params = &[_]ie.Parameter{
            ie.parameter(.ptr32s, 5),
            ie.toParameter(.SP, 0),
        },
    });
    for ([_]ie.Mnemonic{ .UNFRAME, .FRAME }) |mnemonic| {
        try ie.testIdempotence(&ddb, &edb, 1, .{
            .mnemonic = mnemonic,
            .suffix = .none,
            .params = &[_]ie.Parameter{
                ie.parameter(.reg16u, 0),
            },
        });
        try ie.testIdempotence(&ddb, &edb, 2, .{
            .mnemonic = mnemonic,
            .suffix = .none,
            .params = &[_]ie.Parameter{
                ie.parameter(.constant, 129),
            },
        });
        try ie.testIdempotence(&ddb, &edb, 1, .{
            .mnemonic = mnemonic,
            .suffix = .none,
            .params = &[_]ie.Parameter{
                ie.parameter(.constant, 12999),
            },
        });
    }
    for ([_]ie.Mnemonic{ .PUSH, .POP }) |mnemonic| {
        for ([_]ie.BaseExpressionType{ .reg16, .reg32 }) |reg| {
            try ie.testIdempotence(&ddb, &edb, 1, .{
                .mnemonic = mnemonic,
                .suffix = .none,
                .params = &[_]ie.Parameter{
                    ie.parameter(reg, 0),
                },
            });
        }
    }
    try ie.testIdempotence(&ddb, &edb, 1, .{
        .mnemonic = .POP,
        .suffix = .none,
        .params = &[_]ie.Parameter{
            ie.parameter(.reg8s, 0),
        },
    });
    try ie.testIdempotence(&ddb, &edb, 1, .{
        .mnemonic = .POP,
        .suffix = .none,
        .params = &[_]ie.Parameter{
            ie.parameter(.reg8u, 0),
        },
    });
    try ie.testIdempotence(&ddb, &edb, 1, .{
        .mnemonic = .PUSH,
        .suffix = .none,
        .params = &[_]ie.Parameter{
            ie.parameter(.reg8, 0),
        },
    });

    try ie.testIdempotence(&ddb, &edb, 1, .{
        .mnemonic = .SYNC,
        .suffix = .none,
        .params = &[_]ie.Parameter{},
    });

    try ie.testIdempotence(&ddb, &edb, 1, .{
        .mnemonic = .ALD,
        .suffix = .D,
        .params = &[_]ie.Parameter{
            ie.parameter(.ptr32d, 5),
            ie.toParameter(.reg16, 3),
        },
    });
    try ie.testIdempotence(&ddb, &edb, 1, .{
        .mnemonic = .ALD,
        .suffix = .D,
        .params = &[_]ie.Parameter{
            ie.parameter(.ptr32d, 5),
            ie.toParameter(.reg32, 3),
        },
    });

    for ([_]ie.Mnemonic{ .AST, .ASTZ }) |mnemonic| {
        try ie.testIdempotence(&ddb, &edb, 1, .{
            .mnemonic = mnemonic,
            .suffix = .D,
            .params = &[_]ie.Parameter{
                ie.parameter(.reg16, 5),
                ie.toParameter(.ptr32d, 3),
            },
        });
        try ie.testIdempotence(&ddb, &edb, 1, .{
            .mnemonic = mnemonic,
            .suffix = .D,
            .params = &[_]ie.Parameter{
                ie.parameter(.reg32, 5),
                ie.toParameter(.ptr32d, 3),
            },
        });
    }

    try ie.testIdempotence(&ddb, &edb, 1, .{
        .mnemonic = .AADD,
        .suffix = .D,
        .params = &[_]ie.Parameter{
            ie.parameter(.ptr32d, 3),
            ie.toParameter(.reg16, 0),
            ie.parameter(.reg16, 6),
            ie.toParameter(.ptr32d, 3),
        },
    });
    try ie.testIdempotence(&ddb, &edb, 1, .{
        .mnemonic = .AADD,
        .suffix = .D,
        .params = &[_]ie.Parameter{
            ie.parameter(.ptr32d, 3),
            ie.toParameter(.reg32, 0),
            ie.parameter(.reg32, 6),
            ie.toParameter(.ptr32d, 3),
        },
    });

    try ie.testIdempotence(&ddb, &edb, 1, .{
        .mnemonic = .AINC,
        .suffix = .D,
        .params = &[_]ie.Parameter{
            ie.parameter(.ptr32d, 3),
            ie.toParameter(.reg16, 3),
        },
    });
    try ie.testIdempotence(&ddb, &edb, 1, .{
        .mnemonic = .AINC,
        .suffix = .D,
        .params = &[_]ie.Parameter{
            ie.parameter(.ptr32d, 3),
            ie.toParameter(.reg32, 3),
        },
    });

    try ie.testIdempotence(&ddb, &edb, 1, .{
        .mnemonic = .ADECNZ,
        .suffix = .D,
        .params = &[_]ie.Parameter{
            ie.parameter(.ptr32d, 3),
            ie.toParameter(.reg16, 3),
        },
    });
    try ie.testIdempotence(&ddb, &edb, 1, .{
        .mnemonic = .ADECNZ,
        .suffix = .D,
        .params = &[_]ie.Parameter{
            ie.parameter(.ptr32d, 3),
            ie.toParameter(.reg32, 3),
        },
    });

    try ie.testIdempotence(&ddb, &edb, 1, .{
        .mnemonic = .AX,
        .suffix = .D,
        .params = &[_]ie.Parameter{
            ie.parameter(.ptr32d, 3),
            ie.toParameter(.reg16, 5),
            ie.parameter(.reg16, 7),
            ie.toParameter(.ptr32d, 3),
        },
    });
    try ie.testIdempotence(&ddb, &edb, 1, .{
        .mnemonic = .AX,
        .suffix = .D,
        .params = &[_]ie.Parameter{
            ie.parameter(.ptr32d, 3),
            ie.toParameter(.reg32, 5),
            ie.parameter(.reg32, 7),
            ie.toParameter(.ptr32d, 3),
        },
    });

    try ie.testIdempotence(&ddb, &edb, 1, .{
        .mnemonic = .AXE,
        .suffix = .D,
        .params = &[_]ie.Parameter{
            ie.parameter(.ptr32d, 3),
            ie.toParameter(.reg16, 0),
            ie.parameter(.reg16, 5),
            ie.parameter(.reg16, 7),
            ie.toParameter(.ptr32d, 3),
        },
    });
    try ie.testIdempotence(&ddb, &edb, 1, .{
        .mnemonic = .AXE,
        .suffix = .D,
        .params = &[_]ie.Parameter{
            ie.parameter(.ptr32d, 3),
            ie.toParameter(.reg32, 0),
            ie.parameter(.reg32, 5),
            ie.parameter(.reg32, 7),
            ie.toParameter(.ptr32d, 3),
        },
    });
}
