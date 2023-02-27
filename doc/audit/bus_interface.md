# Bus Interface
Every cycle, the CPU generates a 32b virtual address during the setup stage.  This address is composed of two fields.  The high 20b is called the page number, and is placed on the `P` bus.  The low 12b is called the page/frame byte offset, placed on the `N` bus.

When [address translation](address_translator.md) is enabled, the page number is transformed int

The physical address bus is 24b wide, but memory and device I/O can be addressed separately, so the total physical address space has 32M addresses.  Pointers
are 32b wide however, and there is an address translator to map the upper 20b of these virtual addresses into the upper 12b of the physical space (See the Address
Translation section for details).  The lower 12b of the virtual address and physical address are always the same, so the address space can be thought of as a series
of 4KB pages.

TODO talk about bus address decoding and PnP through I2C enumeration of slots

### Stack
TODO stack grows upward
TODO SPM register discussion

### Byte Alignment
When accessing memory and system bus devices, two bytes at a time may be transferred, but the addresses coming from the CPU identify individual bytes.  Therefore a word access can either be aligned (when `A[0]` is 0) or unaligned (when `A[0]` is 1).

Aligned accesses are the norm, and are easy to handle by simply using a 16b wide memory and address it using `A >> 1`.  The low byte of the bus gets data from an even address and the high byte implicitly comes from an odd address.

When handling an unaligned access, we need the low half of the bus to come from an odd address and the high half from an even address.  We can do that by putting a multiplexer on the data bus that either passes it through as is or swaps the bytes.  But there's also a more pernicious problem: The "even" data needs to come from a different word in the memory than the "odd" data.  The "odd" data is still stored in `A >> 1` (same as for an aligned access), but the "even" data is now in `(A + 1) >> 1`.  Since we know `A[0]` is 1, this is equivalent to `(A >> 1) + 1`.  Therefore we need to use completely separate memory chips for odd and even bytes, each with their own address lines, and we need an interposer on the "even" RAM's address lines that can either pass it as-is or increment it.  Luckily we only need to propagate the carry from the increment within the low 11b of the even-address, since page-unaligned word accesses are not allowed.

The logic handling this is localized to the RAM card.  Other system bus devices are free to implement the same logic for supporting byte-unaligned word access if needed, but most devices will simply be able to require that only aligned access is used.

### Page Alignment
When `BUS_OP` is `*_code_*`, `*_stack_*`, or `*_data_*`, and the address translation flag is set, and `A[11:0]` is 0xFFF, a Page Alignment fault is generated.  The handler for this fault can decompose the request into two single-byte operations and return from the fault using the special `BOFRET` instruction, which writes data into a temporary "bus override" register, then swaps back to the state the processor was in before the fault.  When that bus access that faulted is retried, the bus override data is returned and the original operation that caused the fault is suppressed.  Usage of the bus override data clears it automatically, so the next bus operation will proceed normally.  Note that in this technique it may be possible for other faults to occur when carrying out the single-byte operations (the most common being a page fault).  So generally the context from the page alignment fault will need to be saved and restored.

Supporting non-page-aligned accesses without a fault handler might be  possible, but would require translating two different virtual pages for the same bus operation, which would require a lot of hardware to coordinate (see below for details on address translation).  But it would also cause problems when address translation is disabled.  The calculation of the "even" address for the byte-alignment compensation would need to propagate all the way through all 23 address lines.  That would make the incrementer slower and more complex.


