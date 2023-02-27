# Buses
Name | Width | Description
--- | --- | ---
`J` | 32b | Left-hand operand for ALU operations
`K` | 16b | Right-hand operand for ALU operations
`L` | 32b | Result of ALU operations
`D` | 16b | Data from/to memory or devices
`B` | 32b | Base virtual address
`P` | 20b | Virtual page number
`F` | 12b | Physical frame number
`N` | 12b | Page/Frame address offset

# Registers
Name | Width | Description
-- | --- | ---
`R0` - `R15` | 16b | General purpose registers
`STAT` | 11b | Status flags (some read-only)
`RSN` | 6b | Registerset number
`ASN` | 4b/32b | Virtual address space number
`SP` | 32b | Stack pointer
`BP` | 32b | Stack base/frame pointer
`IP` | 32b | Instruction pointer
`RP` | 32b | Return pointer
`UXP` | 32b | User context pointer
`KXP` | 32b | Kernel context pointer
`temp_1`, `temp_2` | 32b | Temporary registers[^1]
`DL` | 16b | Data latch[^1]
`OA`, `OB` | 8b/8b/16b | Instruction operand registers[^1]

## CPU Status Flags
Name | Width | Description
-- | --- | ---
`Z`  | 1b | Zero flag
`N` | 1b | Negative flag
`C` | 1b | Carry/borrow flag
`V` | 1b | Overflow flag
`K` | 1b | Kernel mode
`A` | 1b | Address translation enable
`SLEEP` | 1b | Sleep mode enable
`PIPE` | 2b | Current pipe number (0-2)



[^1]: These registers are implementation details from the perspective of the assembly language user.