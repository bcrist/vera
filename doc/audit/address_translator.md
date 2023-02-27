# Address Translation
n the `A` bus.  The Address Translator then converts that to a 24b physical address on the `P` bus, which identifies the specific byte or word that will be accessed in RAM, ROM, or a device register.

## Address Format
{bitstruct}
Virtual Address:  TTTT TTTT TTTT TTSS SSSS OOOO OOOO OOOO
Physical Address: ---- ---- FFFF FFFF FFFF OOOO OOOO OOOO
T = Tag
S = Slot
O = Offset
F = Frame
{bitstruct}

## Pages and Frames
The upper 20b of the virtual address defines a Page Number, and the upper 12b of the physical address defines a Frame Number.  The lower 12b of both addresses is called the Offset, and is always passed through without modification.  The address translator is effectively just mapping 4 KiB chunks of the virtual address space (a.k.a. pages) into 4 KiB chunks of the physical address space (a.k.a. frames).

4 KiB happens to be the same page size that's typically used on modern computers, but that wasn't why I chose 4 KiB, and in fact my first few iterations on the design used 1 KiB and 2 KiB page sizes.  Selecting a smaller page size would reduce wasted memory and speed up the persistence and restoration of pages, but it would also increase the size of the operating system data structures tracking page/frame usage, and widen the translation entries needed to convert addresses.  4 KiB ended up being a good middle ground for this design.

# Structure
The address translator is a software-filled 2-way set-associative cache.  For each translation, exactly two translation entries will be checked.  These two entries form a set, and the cache contains 128K unique sets.

## Set ID
{bitstruct}
AAAA AAAA GGSS SSSS
A = Space
G = Group
S = Slot
{bitstruct}

The Space bits come from the current value of the `ASN` register.  The Slot bits come from the Virtual Address.  The Group bits are determined by the `BUS_OP` control signals and they split the cache into four quadrants, with each used for a different type of operation.  The four possible groups are:

  - Instruction
  - Stack
  - Data Write
  - Data Read

Each group consists of 64 slots, and each slot can contain up to two address translation entries.  This is enough to minimize thrashing, but few enough that initializing or cleaning up an address space is still relatively fast.  
## Translation Entry
{bitstruct}
TTTT TTTT TTTT TTAP CQUD FFFF FFFF FFFF
T = Tag
A = A (Accessed flag)
P = P (Presence flag)
C = C (Access update flag)
Q = Q (Notify FIFO queue on access update flag)
U = U (User accessible flag)
D = D (Device flag)
F = Frame
{bitstruct}

There are two of these entries for every slot address computed above.

# Translating Addresses
Address translation is normally enabled only when `STAT[A]` is set.  This is initially disabled on reset, since the contents of the translation cache are unknown.  Individual microcode instructions can also force a bus operation to skip address translation by setting `BUS_OP` to `*_raw_*`.  When address translation is disabled, `A[23:0]` maps directly to the `P[23:0]`, `A[30:24]` is simply ignored, and `A[31]` determines if the access is going to
memory or a device on the system bus.

When `STAT[A]` is set and `BUS_OP` is `(read|write)_(insn|stack|data)_(byte|word)`, the address translator functions normally: it takes the virtual address, `ASN`, and `BUS_OP` from the CPU and either comes up with a valid frame number or triggers a fault.

If one of the translation entries for the slot has a tag that matches the tag from the virtual address, this is considered the "matching entry."  If there is a matching entry and its presence flag is set, that entry's frame number and device flag are used, to determine what physical address to access and whether to send it to memory or the system bus.

When a translation is completed, if the matching entry is the "secondary" entry in the slot, it is swapped with the "primary" entry during `phi2`.  This ensures that the most recently accessed entry is always the "primary."  Additionally, the `A` flag is set to the value of the `C` flag whenever that entry is accessed.

## Faults
When there is no matching entry, or the matching entry's `P` flag is not set, a Page fault is generated.  The kernel can look up the mapping in its page table  data structure, update the address translator with the new entry, and return to retry the operation again.  Alternatively, if the page table doesn't have a valid mapping for this address either, it can terminate the process and switch to another task.

When `STAT[U]` is set and the matching entry's `U` flag is not set, or `STAT[U]` is cleared and the matching entry's `K` flag is not set, a User Protection or Kernel Protection fault is generated, respectively.  The kernel can use this to implement copy-on-write, zero-page sharing, memory-mapped files, etc.
 
# Adding and Updating Entries
When the kernel wants to add a new translation, it will set `BUS_OP` to `update_translation_(insn|stack|data_w|data_r)`, depending on which group is being
updated.  The Space and Slot bits will be determined in the same way as when doing a normal translation.

The address translator will always perform these actions when updating a slot:
 - Set the primary entry's tag bits to the virtual address tag
 - Set the primary entry's `C` flag to `N[0]`
 - Set the primary entry's `P` flag (if not already set)
 - Set the primary entry's `AKUD` flags to `D[15:12]`
 - Set the primary entry's frame number to `D[11:0]`

In addition, if the old primary entry's `P` flag was set and its tag was different from the virtual address tag:
 - Set `STAT[Z]` to the complement of the old secondary entry's `P` flag
 - Set `STAT[N]` to the complement of the old secondary entry's `A` flag 
 - Overwrite the secondary entry with the data from the old primary entry

Otherwise:
 - Set `STAT[ZN]`


# Invalidating Entries
When the kernel wants to invalidate an address range (i.e. when releasing a committed address range), it will set `BUS_OP` to
`invalidate_translation_(insn|stack|data_w|data_r)`, depending on which group is being invalidated.  The Space and Slot bits will be determined in the same
way as when doing a normal translation.

For each of the 2 potential matching entries, if the entry's tag ANDed with `D[15:2]` matches the tag from the virtual address ANDed with `D[15:2]`, that entry
will be cleared.  This makes it possible to initialize a new empty address space quickly, as well as invalidating a large address range (>64 pages) without
needing to iterate through each page address individually (as long as the desired range corresponds to a bitwise mask).

Additionally:
 - The bus queue registers will be loaded in the same way as when updating an entry
 - `STAT[Z]` will be cleared if either entry was invalidated while its `P` flag was set, and set otherwise
 - `STAT[N]` will be cleared if either entry was invalidated while its `A` flag was set, and set otherwise


# Reading Entries
The kernel can read the frame number and flags of the entry matching a particular address by setting `BUS_OP` to `query_translation_(insn|stack|data_w|data_r)`.
The Space and Slot bits will be determined in the same way as when doing a normal translation.

If the slot contains an entry with the tag from the virtual address:
 - Drive `D[11:0]` from the frame number
 - Drive `D[15:12]` from the `AKUD` flags
 - Set `STAT[Z]` to the complement of the `P` flag 
 - Set `STAT[N]` to the complement of the `A` flag

Otherwise:
 - Drive `D` with all 0's
 - Set `STAT[Z]`
 - Set `STAT[N]`

Additionally:
 - The bus queue registers will be loaded in the same way as when updating an entry
 - Unlike all other address translation operations, the contents of the slot are never changed because of this operation