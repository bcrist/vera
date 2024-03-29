# Homebrew discrete-logic CPU
### _I call it Vera_

## Specs
* up to 20 MHz clock
* 4-stage pipeline (runs 4 threads simultaneously)
* 16x general purpose registers (16b; can be paired to form 8x 32b registers)
* 32b + 16b -> 32b arithmetic unit
* 16b logic unit
* 32b logarithmic shifter
* 16 x 16 -> 32b multiplier
* 16b popcount/trailing/leading bit count unit
* 32b virtual addresses
* 16x4x64 entry, 2 way set-associative, software filled TLB
* 26b physical addresses
* 16b memory/system bus
* 8 MiB SRAM
* 4 MiB FLASH (not directly accessible by CPU)
* 32 MiB PSRAM (not directly accessible by CPU)
