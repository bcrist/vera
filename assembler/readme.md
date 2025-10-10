# Overview
The purpose of the assembler is to create sequences of binary data within a sparse address space, and in particular, sequences of instructions that can be executed.  The CPU supports separate address translation for code vs. data, so it may make sense to treat these as separate address spaces someday, however currently only one address space is used.

Currently the assembler does not support a separate linking step, nor load-time relocation.  As the OS is developed, these features may or may not be added.

# Output Formats
* Intel hex and Motorola S-Records
	* Either one file per section or all sections merged together
* Simulator-loadable S-expressions
* Disassembly Listing
* Raw memory buffers (when used as a library)

# Sections
The CPU and MMU deal with memory in 4KiB pages, and each page can have different access policies.  Therefore code and data are gouped into sections, and one or more pages are allocated to each section.

## Section Directives
Sections are created by adding a section directive to the program.  The section directive determines what type of section it is, which in turn determines what can go in that section and how it can be accessed by the CPU.

### Section Names
Immediately following the section directive is a name.  Multiple section directives may declare the same section name, as long as they are all the same section type.  If a section directive does not include a name, a default name will be picked, and thus all such directives will reference the same section.

### Code Sections
Most instructions will go in a `.code` section.  Pages for these sections are freely executable and readable, but not writable, in both unprivileged and kernel mode.  Kernel-only code can be placed in a `.kcode` section.  User-mode processes will fault when trying to read or execute such code.

The `.entry` and `.kentry` directives work like `.code` and `.kcode`, respectively, except that dead code analysis will never remove them.  Additionally, chunks with automatic layout in a `.kentry` section will be laid out such that they can be called from unprivileged code.  Care should be taken to ensure that all chunks within `.kentry` sections are intended to be user-mode callable.  Fixed layout chunks also require care to ensure that the chunk is placed with alignment 256, when the chunk is less than 256 bytes in length, or alignment 4096 otherwise, otherwise it won't be callable from user mode.

Code sections may not contain data directives.

### Data Sections
Static program state (e.g. global variables) can be placed in a `.data` section.  Pages for these sections are not executable, but are readable and writable.  Constant data that needs to be readable but not writable can be placed in a `.const` section.

Like code sections, there are kernel-only versions of `.data` and `.const` sections: `.kdata` and `.kconst`.

Data sections may not contain instructions or directives that generate instructions like `.push` or `.pop`.

### Boot Sections
The `.boot` directive creates special sections that are automatically copied into the first 64K of physical memory when the CPU starts.  These sections can contain both instructions and data directives.  These pages are marked as kernel-only readable/writable/executable, but when the CPU boots the address translator is disabled and kernel mode is enabled, so this is largely irrelevant.

### Informational Sections
The `.section` directive can be used to add data that won't be accessed at runtime, like debug information.  These sections can contain both instructions and data directives, and the assembler will allow other sections to reference the public labels defined within these sections, but the pages for these sections won't be mapped into the virtual address space at runtime, so attempting to access them will cause a fault.

# Source Structure
The assembler accepts one or more source files as input.  If multiple files are provided, they are processed individually but combined into a single output artifact.  Public labels and section names may be referenced from any other file, but no other symbols cross source file boundaries.

## Blocks
Each source file is composed of one or more blocks.  New blocks are created by section directives or the `.stack` directive.  Lines containing only a label that preceed a block directive are considered to be part of the block that follows, not the previous block.  If you want a label to refer to the "end" address of a block, you may need to use the `.nil` directive.

Dead code analysis is done with block-level granularity.

The block at the top of each file (before the first section directive or stack block directive) is not associated with any section and will never be included in the output.

## Chunks
Each block consists of zero or more chunks.  A chunk is a contiguous group of instructions and/or directives that must appear sequentially in the output address space.

A chunk may contain either instructions or data directives but not both.  Most types of blocks only allow one or the other, but when both are allowed, switching from instructions to data directives or vice-versa will always create a new chunk.  In code chunks, unconditional control flow instructions, like `RET` or `B` will end the current chunk (and begin a new one if there are more instructions in the block).  In fact, all code chunks must end with unconditional control flow, otherwise an error will be reported.  Finally, an `.org` directive will force the creation of a new chunk.

If a chunk is very large, it may not fit in a single page and thus creates a possibility for an instruction to overlap a page boundary.  This situation may result in a page alignment fault when the CPU attempts to execute that instruction.  The assembler will detect such errors, but will not resolve them automatically.  It is the programmer's responsibility to ensure this does not happen (by splitting up the chunk with an unconditional branch, or inserting a `NOP` instruction somewhere to ensure the last instruction of the page has alignment 2).

### Fixed Layout
The `.org` directive forces the beginning of a new chunk at a specific address.  This is known as a fixed layout chunk.  Any lines within the block which preceed the `.org` but do not generate output (i.e. not an instruction, data directive, `.push`, or `.pop`) are considered part of the following `.org` chunk, not the previous chunk.  For example:
```asm
label1:
	//...
	ret

// ------ Chunk boundary is here ------

label2:
	.org 0x1234
	// ...
```

### Automatic Layout
Chucks that don't have an `.org` are automatic layout chunks; the assembler is free to place them wherever in the address space it thinks is best.  If the chunk is smaller than 4096 bytes, the assembler will attempt to place it in any remaining free space of pages already allocated to that section, without overlapping into another page.  If that's not possible, it will try to allocate a new page to the current section.

#### Range Directives
Normally, automatic layout will never allocate page 0 to a section (except a `.boot` section), but is free to select any other page.  The range of pages allowed to be allocated for a particular section may be adjusted by placing a `.region` directive within that section:
```asm
.region 0x100_000, 0x1FF_FFF // pages 0x100-0x1FF are allowed
```

There may be at most one `.region` directive per section.  The address range specified must be aligned to page boundaries.

### Dead Code Analysis
The assembler will skip blocks that don't seem to be referenced by anything important.  Any blocks for `.entry`/`.kentry` sections are considered roots and will always be kept, as well as blocks containing symbols that are transitively referenced by those blocks.

Any section block may be manually marked as a root by adding a `.keep` directive within it.

# Line Syntax
Each line consists of up to three parts:
* Optional label
* Optional instruction mnemonic or pseudo-instruction directive
	* 0, 1, or 2 mnemonic suffixes
	* 0 or more parameter expressions
* Optional comment

A "logical" line may be continued on the next line by ending the line with `\`.  No other non-whitespace characters may appear on the rest of the line (including comments).

Examples:
```asm
label:
label2: // label and comment
	val %2 // instruction and comment
	// indentation is not significant:
	park

// Label with multi-line directive
label3: .dw 1, 2, \
	3, 4, 5, 6, 7, 8
```

# Comments
Anything on a line after the `//` token is a comment and is ignored.  There are no multi-line comments.  Other comment sigils used in other assembly languages (e.g. `;`) are not supported.

# Labels
A label is a symbolic representation of the address assigned to the next instruction or data directive.  A line that begins with a symbol expression followed by a colon is considered labelled.  Multiple labels may not be placed on the same line, but may be placed on consecutive lines while referring to the same address.  Labels always refer to the same address as the next instruction or directive that creates output or modifies the current address (like `.org` or `.align`).  This may be on the same line, or a following line, and there may be other labels or non-outputting directives (like `.def` or `.undef`) between them.

## File-local Labels
If a label is preceeded by `.local` then it defines a symbol which is visible anywhere throughout the source file it is in, but not from other files.  This applies even if the label begins with `_` or is in a `.stack` block (both of which would normally make it a private label)

## Private Labels
Labels in that begin with `_` are private to their section and are not accessible from other sections.  These are useful for implementing control flow structures without polluting the wider namespaces.

### Stack Block Labels
All labels in `.stack` blocks (except file-local labels) are considered private, but they can be injected into other scopes with the `.push` directive.

## Public Labels
Public labels can be referenced from other source files.  Any label that is not private or file-local is a public label.

# Alignment Directives
```
.align <alignment> [offset]
```
This directive ensures that the alignment of the address assigned to this line is at least equal to the constant expression that follows
The alignment must be a power of two > 0 (i.e. `popcnt(alignment) == 1`)
The second parameter allows an offset to be added to the aligned value, and defaults to 0.
The exact algorithm for modifying the address is as follows:
```
temp = (address & ~(alignment - 1)) + offset
while (temp < address) temp += alignment
address = temp
```

# Named Expressions
The `.def` directive creates a lexically scoped symbolic alias for a single expression:
```
.def <symbol> <expr>
```
The scope of the symbol begins on the next line and ends when:
    * Another section begins
    * The symbol is undefined with `.undef`
    * The symbol is redefined with another `.def` using the same name
    * The end of the file is reached

Note that symbol resolution in `<expr>` is done based on the scope of the `.def` directive, but when the symbol is referenced, any relative addresses are computed based on the address of the instruction where the expression is being used.  For example:
```
.org 1000
.def a $
.def b a + 32
.undef a

.org 2000
.dw .raw @b // 2032
```

## Un-defining Named Expressions
The scope of one or more named expression created with `.def` can be ended early:
```
.undef <symbol> [, <symbol> [...]]
```
Using this may help prevent accidental usage of the expression after it is no longer valid (e.g. due to a register being overwritten), and frees up that name for reuse in a different way.

## File-local Named Expressions
`.def` symbols automatically go out of scope at the end of a block, but sometimes it's useful to be able to define symbolic aliases for constants or other expressions that are valid throughout an entire file:
```
.local <symbol> <expr>
```
Additionally, `.local` symbols are not lexically scoped, so they may be referenced before they are defined, and `.undef` has no effect on them.

See also [File-local Labels](#File-local%20Labels).

`.local` and `.def` symbols are allowed to shadow public labels defined in other files.  Symbols are never allowed to shadow other symbols defined in the same file (unless they are in different scopes, like private labels in different section blocks)

# Data Directives
Data directives insert constant data directly into the output:
```
.db <expr> [, <expr> [...]
.dh <expr> [, <expr> [...]
.dw <expr> [, <expr> [...]
```

For `.db` , each expression's bit-width is extended to a multiple of 8.  Signed values will be sign-extended and unsigned values will be zero-extended.

For `.dh`, expressions will be extended to a multiple of 16, and the address of the line will be forced to at least an alignment of 2 to avoid the possibility of a page alignment fault when reading or writing the word.  Words are always little-endian.

For `.dw`, expressions will be extended to a multiple of 32, and the address of the line will be forced to at least an alignment of 4. Words are always little-endian.

## Zero Data Directives
For cases where the initial contents of a memory location don't matter, it can be awkward to use `.db`/`.dh`/`.dw`.  Instead a zero data directive can be cleaner; it just takes the number of zero bytes/half-words/words to output:
```
.zb [<expr>]   // equivalent to .db 0x00 ** (<expr>)
.zh [<expr>]   // equivalent to .dh 0x0000 ** (<expr>)
.zw [<expr>]   // equivalent to .dw 0x00000000 ** (<expr>)
```

If no byte/half/word count is provided, it defaults to 1.

# Stack Blocks
The `.stack` directive creates new named block which can only contain data directives.  Addresses within the stack block always start at 0, and are considered relative to `%sp`, not `%ip`.  All labels within stack blocks are considered private, and have type `.s %sp + constant`.

Unlike [data section blocks](#Data%20Sections), stack blocks are never written to the output, so typically [Zero Data Directives](#Zero%20Data%20Directives) would be used rather than plain [Data Directives](#Data%20Directives).

## Push Directives
The name assigned to a stack block is part of a different namespace from both section names and symbols, and is file-local.  Elsewhere in the same file, a `.push` directive is used to add a stack block as a stack frame.  A `FRAME` instruction is synthesized which subtracts the size of the stack block from the current stack pointer.  All the private labels from the stack block then become available to the following lexical scope.

Multiple stack blocks may be pushed, either using separate `.push` directives, or by listing multiple comma-separated stack block names as parameters toa single `.push` directive.  The same block may not be pushed multiple times, and and any blocks that are pushed at the same time must not contain any duplicate label names.  When referencing a stack label, the assembler will automatically compute the correct SP offset to use, even if that label isn't in the most recently pushed block.

## Pop Directives
When a stack block is pushed within a chunk, there must be a `.pop` directive with that block's name before the end of the chunk.  Stack blocks must be popped in the reverse order that they were pushed, but multiple names can be specified to a single `.pop` directive, and the relative ordering of names within a `.pop` directive's parameters does not matter, as long as they can be rearranged to match the last pushed names.

Control flow is not allowed to jump across `.push` or `.pop` directives.  The assembler will check for this on a best-effort basis.

# Expressions
When an instruction has both source and destination parameters, the sources appear first (on the left) and destinations after (on the right) and the comma separating them is replaced with an `->` operator.  In some cases the arrow operator can also appear before the first parameter or after the last parameter if the source or destination are implicit.  A very select few instructions use multiple arrows when the instruction involves multiple separate writes.

## Register literals
* General Purpose Registers: `%0`-`%31`
	* `%0` is the top of the register stack, `%31` is the bottom.
	* Note that while the register stack contains 64 slots, register values deeper than 32 levels can't be directly read by hardware, therefore the offset is limited to the range of 0-31.
* Special named registers:
    * `%` followed by an identifier denotes a special register or instruction disambiguator, for example:
		* `%ip` - Instruction Pointer
		* `%sp` - Stack Pointer

These literals are considered keywords and can never refer to a label or named expression.  [Symbol Directives](#Symbol%20Directives) must be used if it's necessary to use these names in those cases.

## Integer literals
Integer constants may be specified in several bases:
```
0123456789 // Decimal
0d0123456789 // Also decimal
0x0123456789ABCDEF // Hex
0o01234567 // Octal
0q0123 // Quaternary
0b01 // Binary
```

For decimal literals, the bit width is the minimum number of bits required to represent the number in 2's complement.

For non-decimal literals, the bit width depends on the number of digits provided.  Leading 0's will add padding bits.

Integer literals are always considered unsigned values (they will be zero extended when necessary) unless converted with [Signedness Casts](#Signedness%20Casts) or [Addition, Subtraction, Negation](#Addition,%20Subtraction,%20Negation).

## String literals
Strings can be though of as little-endian, base-256 integers.  As such, integers and strings can generally be used interchangeably as needed, except for [Symbol Directives](#Symbol%20Directives) which must use string literals.

## Current address literal
`$` resolves to the address of the current line's label (or if there is none, the address it would have if it existed).  In code sections, it is equivalent to `.i %ip`.  In data and stack sections, the `%ip` register would normally not be used, but `$` can still be used to compute distances from the current line

## Symbols
Symbols are names which represent values defined elsewhere in the program.  
Symbol references are resolved from a number of potential sources:
- Lexically-scoped named expressions defined with `.def`
- File-scoped named expressions defined with `.local`
- Labels from stack blocks that have been pushed in the current scope
- Private labels from the current section block
- Public labels defined in any file

### Symbol Directives
The `.sym` directive allows the usage of arbitrary names for symbols by using a string literal instead of a raw identifier.  For example:
```
.sym "I'm a label":
	.def .sym "I'm a .def symbol" 0
	.db .sym "I'm a .def symbol" + 1
```

### Collisions
Symbols used within expressions may refer to any type of label or `.def` or `.local` expressions.  If a symbol name could refer to more than one label or expression, an error is reported, except in one case: if a public label is defined in a different file, then it may be shadowed by a symbol within the current scope, as long as it is not also a public label.

## Operator Precedence
1. Grouping (parentheses)
3. Unary Prefix Operators (right to left)
4. Length Cast
5. Truncation, Extension, Signedness Casts
6. Repetition
7. Concatenation
8. Shifting
9. Multiplication
10. Addition, Subtraction
11. Bitwise AND
12. Bitwise XOR
13. Bitwise OR
14. Arrow Operator

All precedence levels have left-to-right associativity, except prefix operators.

## Addition, Subtraction, Negation
Performs 2's complement arithmetic on the operand(s).  If the operands are not the same length, the shorter one will be extended (as if by the [Length Cast Operator](#Length%20Cast%20Operator)).  The result is always signed.  The length of the result will always be the minimum number of bits required to store the number in 2's complement.  There is no maximum operand length.
```
expr + expr
expr - expr
- expr
```

If one or both of the operands are not constants, (i.e. registers, relative addresses, etc.) then the symbolic sum will be attempted to be encoded in the expression's type, however there are limitations on what can be symbolically represented:
* At most two registers, or at most one register with a constant offset.
* An optional address space tag

Since the actual constant offset is not part of the type (only its presence or absence), subtraction and addition of a constant are equivalent in their effect on the type: if there was not already a constant offset in the type, it is added, otherwise the type is unaffected.

When a subtrahend has an address space tag, the minuend must have the same tag, otherwise an error is generated.  The difference will have no address space.

When a subtrahend has one or more symbolic registers, the minuend must have the same register(s), otherwise an error is generated.  The difference will not contain those registers.

## Multiplication
Multiplies two constants.  Constants larger than 64 bits are not supported.  The result will have the minimum number of bits required to represent the number in 2's complement.
```
expr * expr
```

## Shifting
Left and Right arithmetic shift on constants.  Constants larger than 64b are not supported.  The result will have the minimum number of bits required to represent the number in 2's complement.
```
expr << amount
expr >> amount
```

**TODO: Support arbitrary width shifting**

## Bitwise Operators
Performs bitwise boolean operations on the operand(s).  If the operands are not the same length, the shorter one will be extended (as if by the [Length Cast Operator](#Length%20Cast%20Operator)).  The result has the same signedness and length as the larger of the operands.  If both have the same length but different signedness, the result has the same signedness as the left operand.  There is no maximum operand length.
```
expr & expr
expr ^ expr
expr | expr
~ expr
```

## Concatenation
Combines the bits of two constants by concatenation.  The left side represents the less significant bits of the result, and the right side the more significant bits (i.e. it is little-endian).  There is no limit on the width of the constants.
```
expr ++ expr
```

This can be used for string concatenation, but it is not inherently a byte-oriented operation; the bit widths of the operands are _not_ required to be a multiple of 8.

## Repetition
Concatenates a constant with itself, `n` times.  There is effectively no limit on the length of the expression.  The `n` constant supports up to 63 bits (though the practical limit is typically much lower due to memory use).
```
expr ** n
```

## Absolute Address Cast
This unary prefix transforms an IP-relative address to an absolute address.  The address space (if any) is not changed.
```
@ expr
```

There isn't a dedicated operator which represents the inverse operation (absolute to IP-relative address), but it can still be performed with the somewhat wordy form `%ip + expr - .raw @$`.  This isn't typically needed since labels usually resolve to IP-relative addresses.

## Address Space Casts
These unary prefix operators add or remove the address space associated with an address.  The original expression must not already have an address space, except for `.raw`, which removes it.
```
.i expr   // creates a code/instruction address
.d expr   // creates a data address
.s expr   // creates a stack address
.raw expr // removes any address space
```

Labels typically will already be tagged with the appropriate address space, so this is mostly useful when a register is being used as an address/pointer.

## Length Cast Operator
Changes the bit-width of an expression.  If the constant overflows the new size, it is an error.  If the new size is larger than the old, the extension method depends on the signedness of the expression.
```
expr ' new_width
```

## Length Truncation
Changes the bit-width of an expression.  Overflowing bits are ignored.  If the new size is larger than the old, the length is not changed.
```
expr .trunc new_width
```

## Length Extension
Changes the bit-width of an expression.  If the width is already larger than specified, an error is recorded.  Otherwise sign- or zero-extension are used based on the operator name (regardless of the signedness of the expression).  The signedness of the result is the same as the signedness of the original expression.
```
expr .zx new_width
expr .sx new_width
```

## Signedness Casts
Changes the signedness of a constant or GPR expression.
```
expr .signed
expr .unsigned
```

Note that when used on a constant, it does not change the bit width, so it may change the value:
 * Negative values will become positive with `.unsigned`
 * Positive values without a 0 in the MSB will become negative with `.signed`

## Index to GPR
Converts a constant to a GPR expression:
```
.reg 1   // result is %1
```

## Index from GPR
Converts a GPR expression to an index:
```
.idx %1 // result is integer constant 1
```
