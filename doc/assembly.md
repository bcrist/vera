# Line Syntax
Each line consists of:
    * Optional label
    * Optional instruction mnemonic or pseudo-instruction directive
        * 0, 1, or 2 mnemonic suffixes
        * 0 or more parameter expressions
    * Optional comment

# Comments
The token `//` indicates the start of a comment.  Anything else on that line is ignored completely.
There are no multi-line comments.  Other common comment characters in assembly (e.g. `;`) are _not_ supported.

# Pseudo-instruction Directives

## Non-outputting Directives
    .org <expr>
Forces the address assigned to this line to be the constant expression that follows.
It is possible to accidentally cause code or data to overlap using this directive, which the assembler will report as an error.

    .align <alignment> [offset]
Ensures that the alignment of the address assigned to this line is at least equal to the constant expression that follows
The alignment must be a power of two > 0 (i.e. popcnt alignment == 1)
The second parameter allows an offset to be added to the aligned value, and defaults to 0.
The exact algorithm for modifying the address is as follows:

    temp = (address & ~(alignment - 1)) + offset
    while (temp < address) temp += alignment
    address = temp


    .keep
Prevents the section containing this directive (or any sections referenced by it) from being removed via dead-code elimination

    .nomove
Prevents the section from being reordered relative to other sections with the same name that also have .nomove, within the same file.

    .def <symbol> <expr>
Creates a named expression.
The scope of the symbol defined begins on the next line and ends at the first of the following:
    * Another section begins
    * The symbol is undefined with `.undef`
    * The symbol is redefined with another `.def` using the same name
    * The end of the file is reached

Note that the expression is evaluated in the context where the symbol is used, not where it is defined.  In particular, `$` does not evaluate to the line containing the `.def` directive.

If a named expression is created with the same name as a label, the named expression shadows the label during symbol resolution.

    .undef <symbol>
 Ends the scope of a named expression created with `.def`

    .local <symbol> <expr>
Similar to `.def`, but with a few differences:
* The declaration is visible to the entire file
* `.undef` has no effect on the symbol
* The symbol can be referenced before it is defined

File-local labels can also be defined:

    .local <label>:
This is equivalent to `.local <label> $` except that it can be used on the same line as another instruction/directive.  This makes it possible to reference private labels in different sections of the same file, without making the label visible globally in other files:

    .code
        ld hidden -> r0   // normally _private wouldn't be in scope here
	.local hidden: .data
        .dw 0x1234


    .db <expr-list>      // "Declare bytes" (8-bit)
    .dw <expr-list>      // "Declare words" (16-bit, implies .align 2)
    .dd <expr-list>      // "Declare double words" (32-bit, implies .align 2)
Declares one or more bytes, words, or double words of data.
`.dw` and `.dd` imply `.align 2`.

## Section Directives
Section directives group code into blocks which the assembler can rearrange, or remove if it determines that the section is unused.
The directive used to declare the section determines how the OS will set up the virtual memory for that section.

Section directives may be followed by a symbol expression.
This is optional for code and data sections, but required for stack sections.
This name may be selected arbitrarily and exists in a separate namespace from labels and named expressions.
The assembler will attempt to place sections with the same name next to each other, and will never place multiple sections with different names in the same page.  (If `.org` directives would require this, it is an error)

### Code Sections
    .code [symbol]
    .kcode [symbol]
    .entry [symbol]
    .kentry [symbol]
Code sections contain instructions.
`.entry` sections contain labels that should be retained in the final binary's entry point list
`.kcode` indicates that the section should only be accessible from kernel mode.
`.kentry` indicates that the first label within the section should be callable from user code, but the remainder is protected as `.kcode`
`.entry` and `.kentry` imply `.keep`.

### Data Sections
    .data [symbol]
    .kdata [symbol]
    .const [symbol]
    .kconst [symbol]
Data sections contain writable or read-only data.
`.data` and `.kdata` are writable.
`.const` and `.kconst` are read-only.
`.kdata` and `.kconst` are accessible only from kernel mode.
If a data section contains only zeroes, it won't be stored in the binary, but will be allocated from pre-zeroes memory when first accessed (i.e. BSS segment)
Data sections and code sections exist in separate address spaces and the assembler may place both a code and data section at the same address.

### Stack Sections
    .stack [symbol]
    .push <symbol-list>
    .pop <symbol-list>
Stack sections are never loaded into memory directly.
Addresses within stack sections are relative to the SP just after the stack section has been applied.
When a code section section begins, the stack is reset and no labels from `.stack` sections are visible.
The `.push` pseudo-instruction adds a stack context to the current code section, allowing use of the labels within it until the end of the section, or the next `.pop` pseudo-instruction having the same name.
`.pop` names must appear in the reverse order that they were pushed.
`.push` and `.pop` may not be used within a loop (ideally assembler should detect this and error).
The assembler will automatically adjust the SP for .push and .pop
References to labels within a stack section will automatically resolve with the correct SP offset based on what has been .pushed/.popped

### Informational Sections
    .section [symbol]
These sections can be used to include additional information within a binary, which is not mapped into memory at all when the OS loads it.
This can be used for debugging information, resources, etc.
Addresses within informational sections exist in their own namespace per section name, which is independent of the code and data address spaces, and any other informational sections.

# Labels
    <symbol>:
A line may begin with a symbol expression, followed by a colon.  This creates a symbol that resolves to the address where that line's instruction exists in memory.  If the line contains an instruction that modifies the address without outputting anything (e.g. `.org`, `.align`) then the label
will be the modified address

Labels that begin with `_` are private to their section and are not accessible from other sections.

# Expressions
Some instructions and pseudo-instruction directives require parameters.
Mnemonics can be overloaded based on the number of parameters they take, and the types of those parameters.
If there are multiple parameters, they are separated by commas (typically)
When an instruction has both source and destination parameters, the sources appear first (on the left) and destinations after (on the right) and the comma separating them is replaced with an `->` operator.  In some cases the arrow operator can also appear before the first parameter or after the last parameter if the source or destination are implicit.  A very select few instructions use multiple arrows when the instruction involves multiple separate writes.

## Literals

There are 3 types of literals: registers, integers, and strings.

### Register literals
byte: B0-B15
word: R0-R15
dword: X0-X15
special: IP, RP, SP, BP, KXP, UXP, ASN, STAT

### Integer literals
Decimal:
    [0-9][0-9_]*
    0d[0-9_]*
Hex:
    0x[0-9A-Fa-f_]*
Binary:
    0b[01_]*
Octal:
    0o[0-7_]*

All constants have an associated bit width.
The default bit-width for decimal is the minimum number of bits required to represent the number.
The default bit-width for hex/binary/octal is proportional to the number of digits provided.
There are operators which can manipulate the bit width of their result (see below).

### String literals
Integers and strings can always be used interchangeably; strings are just syntactic sugar for large integers.
Strings have the same syntax as zyxlang.

### Current address literal
`$` resolves to the address of the current line's label (or if there is none, the address it would have if it existed).  In code sections, it is equivalent to `.i IP`.  In data and stack sections, the `IP` register would normally not be used, but `$` can still be used to compute distances from the current line

## Symbols
    [a-zA-Z_][a-zA-Z_0-9\.]*
    .sym <string-literal>
Symbols are names which represent values defined elsewhere in the program.  The `.sym` directive allows the usage of arbitrary names for symbols by using a string literal instead of a raw identifier.
Symbol references are resolved from a number of potential sources (in order of decreasing priority):
- Named expressions defined with `.def`
- Private labels from the current section-block
- Labels from stack blocks that have been pushed in the current section-block
- File-scope named expressions defined with `.local`
- Public labels defined in any file

## Operators
    (x)
Grouping

    x + y
    x - y
Add or subtract two constants or registers.
Constants larger than 64b are not supported.  The result will have the minimum number of bits required to represent the number in 2's complement.
The symbolic sum of two registers or a register and constant may be represented, but not more than two registers, or more than one register with a constant offset.
Subtracting a register is only valid if the same register exists in the type of the left side.  This can be used to, e.g. subtract two addresses to get the distance between them as a raw constant.

    -x
2's complement negation on constants.
Constants larger than 64b are not supported.  The result will have the minimum number of bits required to represent the number in 2's complement.

    x * y
Multiply two constants.
Constants larger than 64b are not supported.  The result will have the minimum number of bits required to represent the number in 2's complement.

    x << y
    x >> y
Left and Right arithmetic shift on constants.
Constants larger than 64b are not supported.  The result will have the minimum number of bits required to represent the number in 2's complement.

    x | y
    x & y
    x ^ y
    ~x
Bitwise operations on constants.
There is no limit on the width of constants.  The result will have the same width as the largest operand.  Any smaller operand constants will be sign extended.

    x ++ y
Concatenate constants.
Little-endian (first parameter takes least significant bit).  If the width of `x` is not a multiple of 8, then `y` will not start at the LSB of a byte.
There is no limit on the width of the constants.

    x ** y
Concatenates a constant with itself, `y` times.
The `y` constant supports up to 63 bits (though practically speaking much lower due to memory use)

    x ' w
Change the bit-width of a constant to `w`
If the constant overflows the new size, it is an error.
If the new size is larger than the old, sign extension is used.

    x .sx w
    x .zx w
Sign or zero-extend a constant to `w` bits.
If the constant is already larger than `w` bits, it is an error.

    x .trunc w
Change the bit-width of a constant to `w`
Overflowing bits are ignored.
If the new size is larger than the old, sign extension is used.

    x .signed
    x .unsigned
    x .maybe_signed
Change the signedness of a GPR expression.
`maybe_signed` means unspecified signedness.  An instruction that takes an unspecified signedness register as a parameter will also accept either a signed or unsigned version, but an instruction that requires a signed or unsigned register parameter will not take an expression with unspecified signedness.

    @x
Convert an IP-relative address into an absolute address.
The address space (data/code) is preserved.
Not valid for references to stack labels.

    .d expr
    .i expr
    .s expr
Create a data, instruction, or stack address from a non-address expression.
If the expression is already another type of address, an error is generated.

    .raw x
Remove the data/instruction/stack address marker from an expressionransmute absolute address to a constant.  Note if the address is relative, the base register is _not_ removed from the expression.

	.rb expr
    .r expr
    .rx expr
Create a GPR expression from a register index (0-15).
`.rb` creates a byte register (B0-B15)
`.r` creates a word register (R0-R15)
`.rx` creates a double-word register (X0-X15)

    .idx expr
Extract the index from a register reference.

## Expression Types

### Constant
Unsigned or 2's complement integer with a constant value and number of bits

### GPR Reference
A specific byte, word, or dword register, assumed to contain either a signed value, unsigned value, or value of unspecified signedness.

### Special Register Reference
The type of the special IP, SP, RP, BP, UXP, KXP, ASN, and STAT registers

### Address (Base + Offset)
Must be associated with one of 3 address spaces: data, code/insn, or stack
Represents a value relative to a base register, or nothing (for absolute addresses)
Offset may be a constant or register reference.

# Assembly Phases
* Lexing
* Parsing
* Typechecking
* Dead code elimination
* Iterative page allocation, memory layout, and instruction selection
* Layout error checking and instruction encoding
* Output and reporting

# Ouput Formats
* Intel hex/Motorola srec
* Simulator-loadable S-expressions
* (future) OS-loadable binary
* Disassembly/listing - source ordering or by final address