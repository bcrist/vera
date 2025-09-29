# Homebrew discrete-logic CPU
### _I call it Vera_

## Specs
* up to 20 MHz clock
* 4-stage pipeline (runs 4 threads simultaneously)
* 32x 32b general purpose registers (stack addressed)
* 32b ALU (2x L4C381)
* 32b logarithmic shifter
* 16b * 16b -> 32b single cycle multiplier (32b * 32b -> 32b in 5 cycles)
* 32b popcount/extension/truncation unit
* 32b virtual addresses
* 16x4x128 entry, 2 way set-associative, software filled TLB
    * 4 KiB pages
* 64 MiB physical address space (24b address x 32b data)
    * up to 16 MiB SRAM
    * up to 16 MiB I/O space
    * up to 32 MiB pipeline-private, zero-wait SDRAM
