Sources and destinations are always separated by `->` and the destination(s) are always on the right.  This arrow token may be omitted when:
 - There are no sources for the operation, or they are implied by the mnemonic (eg. `CSP`)
 - There are no destinations for the operation, or they are implied by the mnemonic (eg. `SSP`)
 - There is a single destination and it is the same as the first source (eg. `ADD r0, r1`)

Syntax | Width | Description | Examples | Meaning
- | - | - | - | -
`u4` | 4b | Unsigned immediate | `0, 1, 0xF` | `0b0000, 0b0001, 0b1111`
`u8` | 8b | Unsigned immediate | `1, 0xFF` | `0b0000'0001, 0b1111'1111`
`u16` | 16b | Unsigned immediate | `0xF0F0` | `0b1111'0000'1111'0000`
`s4` | 4b | Signed immediate | `0, -1, -0x2` | `0b0000, 0b1111, 0b1110`
`s8` | 8b | Signed immediate | `1, -0x20` | `0b0000'0001, 0b1110'0000`
`s16` | 16b | Signed immediate | `0x70F0` | `0b0111'0000'1111'0000`
`b*` | 8b | Low half of register | `b0, b1, b2` | `R0[7:0], R1[7:0], R2[7:0]`
`ub*` | 8b | Low half of register (treat as unsigned) | `ub0, ub1, ub2` | `R0[7:0], R1[7:0], R2[7:0]`
`sb*` | 8b | Low half of register (treat as signed) | `sb0, sb1, sb2` | `R0[7:0], R1[7:0], R2[7:0]`
`r*` | 16b | Single register | `r0, r2, r3` | `R0, R1, R2`
`ur*` | 16b | Single register (treat as unsigned) | `ur0, ur2, ur3` | `R0, R1, R2`
`sr*` | 16b | Single register (treat as signed) | `sr0, sr2, sr3` | `R0, R1, R2`
`x*` | 32b | Pair of adjacent registers* | `x0, x1, x2` | `{ R1, R0 }, { R0, R1 }, { R3, R2 }`
@* | | IP-relative offset | `@5, @-10, @sr0` | `IP + 5, IP - 10, IP + sx(R0, 32)`
$* | | SP-relative offset | `$5, $-10, $sr0` | `SP + 5, SP - 10, SP + sx(R0, 32)`

\* The 'low' register is identified directly in the instruction, and the 'high' register is found by XOR-ing with 1
