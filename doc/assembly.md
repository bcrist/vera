# Line Syntax
Each line consists of:
    * Optional label
    * Optional instruction mnemonic or pseudo-instruction directive
        * 0, 1, or 2 mnemonic suffixes
        * 0 or more parameter expressions
    * Optional comment

# Comments
The token `//` indicates the start of a comment.  Anything else on that line is ignored completely.
There are no multi-line comments.

# Pseudo-instruction Directives

## Non-outputting Directives
    .org <expr>
Forces the address assigned to this line to be the constant expression that follows.
If the address within the current section is already larger than the requested address, it is an error.

    .align <expr>
Ensures that the alignment of the address assigned to this line is at least equal to the constant expression that follows

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

Note that the expression is evaluated immediately; it does not work like a macro.  In particular, `$` evaluates to the address of the line containing the `.def` directive, not the line where the named expression eventually gets referenced.

If a named expression is created with the same name as a label, the named expression shadows the label during symbol resolution.

    .undef <symbol>
 Ends the scope of a named expression created with `.def`


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
byte: B0-B15, B0S-B15S, B0U-B15U
word: R0-R15, R0S-R15S, R0U-R15U
dword: X0-X15, X0S-X15S, X0U-X15U
special: IP, RP, SP, BP, KXP, UXP, ASN, STAT

### Integer literals
Decimal:
    [0-9][0-9_]*('[0-9]+)?
    0d[0-9_]*('[0-9]+)?
Hex:
    0x[0-9A-Fa-f_]*('[0-9]+)?
Binary:
    0b[01_]*('[0-9]+)?
Octal:
    0o[0-7_]*('[0-9]+)?

If an integer literal ends with a `'x` suffix, it indicates the bit width of the number.
If the literal would overflow an integer with the specified size, it is an error.
For positive literals, this assumes an unsigned representation.
For negative literals, this assumes a 2's complement representation.
For negative literals, the sign bit may be extended beyond the specified bit-width depending on the context it is used in.
The default bit-width for decimal is the minimum number of bits required to represent the number.
The default bit-width for hex/binary/octal is proportional to the number of digits provided.

### String literals
Integers and strings can always be used interchangeably; strings are just syntactic sugar for large integers.
Strings have the same syntax as zyxlang.

## Symbols
    [a-zA-Z_][a-zA-Z_0-9\.]*
    .sym <expr>
Symbols must be resolved to either a labelled address, or a named expression defined with the `.def` directive
The .sym directive can be used with a string literal when you want to use a symbol name containing arbitrary special characters


## Operators
(<x>)               // Grouping
$                   // Implicit label at the beginning of the current line
$$                  // Implicit label at the beginning of the section (excluding blank lines and directives that do not generate output)
<x> + <y>           // add two values.  Types must be compatible.
<x> - <y>           // subtract two values.  Types must be compatible.
<x> * <y>           // multiply two values.  Types must be compatible.
<x> >> <y>          // right shift
<x> << <y>          // left shift
<x> | <y>           // bitwise OR
<x> & <y>           // bitwise AND
<x> ^ <y>           // bitwise XOR
~ <x>               // bitwise NOT
<x> ++ <y>          // Concatenate two constants.  Little-endian (first parameter takes least significant bit).  If x's bit width is not a multiple of 8, then y will not start at the LSB of a byte.
<x> ** <y>          // Concatenates a constant with itself, y times
<x> .width <w>      // Change the bit-width of a constant to w (error on overflow)
<x> .trunc <w>      // Drop MSB bits such that the result has bit-width w (ignore overflow; keep same signed/unsigned distinction)
<x> .signed         // Convert a register reference or constant to be signed (e.g. R15 -> R15S)
<x> .unsigned       // Convert a register reference or constant to be unsigned (e.g. R15 -> R15U)
<x> .generic        // Remove the signed/unsigned distinction from a register reference or constant (e.g. R15U -> R15)

<label>             // reference a named literal (type depends on literal definition) or labelled address (as either an IP-relative code address, IP-relative data address, or SP-relative stack address)
@<label>            // reference a labelled address as either an absolute code address or absolute data pointer.  Not valid for stack section labels.
#<label>            // dereference a labelled data declaration from a .const section.  Does not count as a reference for dead-code elimination.  The label must have a .db, .dw, or .dd directive on the same line, with exactly one constant expression.

.d <expr>               // Transmute constant or register reference to absolute data address
.i <expr>               // Transmute constant or register reference to absolute code address
.s <expr>               // Transmute constant or register reference to absolute stack address
.d (<base> + <offset>)  // Transmute base + offset to data address
.i (<base> + <offset>)  // Transmute base + offset to code address
.s (<base> + <offset>)  // Transmute base + offset to stack address

.rb <expr>          // Convert a register index (0-15) to a byte register reference (B0-B15)
.r <expr>           // Convert a register index (0-15) to a word register reference (R0-R15)
.rx <expr>          // Convert a register index (0-15) to a double-word register reference (X0-X15)
.idx <expr>         // Extract the index from a register reference. (e.g. `.r 1 ^ .idx X0` is equivalent to `R1`)

## Expression Types

### Constant
Unsigned or 2's complement integer with a constant value and number of bits

### GPR Reference
A specific byte, word, or dword register, assumed to contain either a signed value, unsigned value, or value of unspecified signedness.

### Special Register Reference
The type of the special IP, SP, RP, BP, UXP, KXP, ASN, and STAT registers

### Address (Base + Offset)

Represents a value relative to a base register, or nothing (for absolute addresses)
Offset may be a constant or register reference.
