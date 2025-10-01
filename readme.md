# Homebrew discrete-logic CPU
### _"I call it Vera"_ - Jayne Cobb

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

## Design Constraints
Since everyone in the homebrew CPU community has their own ideas of what kinds of technology are acceptable to use, here are my self-imposed restrictions and allowances:

* Backplane/card cage construction is preferred
    * This facilitates building and testing smaller parts separately
    * Blinkenlights are cool, but not particularly interesting at MHz frequencies
* 3.3V LVTTL/LVCMOS signalling is preferred
    * Backplane signals must not exceed 3.3V (ignoring overshoot transients)
    * 5V TTL outputs are allowed within a board, when paired with an 5V-tolerant receiver
    * Differential LVPECL/LVDS signalling is allowed for clocks
    * All boards should locally generate their own Vcc from a common 12V supply distributed by the backplane
* SMD footprints are fine
    * &gt;= 0.5mm pitch for QFP/QFN/(T)(S)SOP
    * &gt;= 0.8mm pitch for BGA
* Modern SRAM, dual-port SRAM, and SDRAM is fine to use
* Dedicated adder/multiplier/ALU ASICs are allowed
    * But "not quite microprocessor" parts like Am2901 or Am29116/7 are not allowed (except in peripheral devices)
* FPGAs are not allowed
    * Designing with HDLs feels more like work than fun for me, and all of the FPGA toolchains make me want to pull my hair out
* CPLDs and other programmable logic are allowed only if I have [created the entire programming toolchain myself](https://github.com/bcrist/zig-lc4k)
    * Designing for CPLDs (without HDLs) is extremely similar to designing for discrete logic, but simplifies board layout significantly, and often provides better timing characteristics
* Microcontrollers or other ASICs are allowed for bootstrapping or debugging purposes, or within peripheral devices
    * Absolutely none within the data or control paths of the CPU itself
