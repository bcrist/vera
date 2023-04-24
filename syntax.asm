// r0: fib(n - 1)
// r1: fib(n - 2)
// r2: temp (copy of r0)

fib: .code
    c 0x10001 -> x0
_loop:
    c r0 -> r2
    add r0, r1 -> r0
    c r2 -> r1
    b _loop
.local .sym " asdf":
    .org f
    .idx r15 .unsigned label

    .org 0b110101_1101
    .align 4
    .keep
    .nomove
    .def _asdf 1234
    .undef _asdf
    .local asdf 2314
    .code
        ld hidden -> r0   // normally _private wouldn't be in scope here
	.local hidden: .data
        .dw 0x1234


    .db 255, "asdf"      // "Declare bytes" (8-bit)
    .dw 0x1234      // "Declare words" (16-bit, implies .align 2)
    .dd 0xffff_0000      // "Declare double words" (32-bit, implies .align 2)

    .code asdf
    .kcode asdf
    .entry asdf
    .kentry mbol
    .data symbol
    .kdata symbol
    .const symbol
    .kconst symbol
    .stack something
    .push something
    .pop something
    .section symbol
    c IP -> RP
    c SP -> BP
    c KXP -> UXP -> ASN
    ld asdf -> STAT

    ld (x)
    c .raw ip -> r14
    c asdf + rc - a -> r10
    c x * y -> r1
    c x >> 1 -> r0
    c (x << 1) -> r0
    c x | y -> r0
    c x & y -> r0
    c x ^ y -> r0
    c ~x -> r0
    c -x -> r0
    c x ++ y -> r0
    c x ** 3 -> r0
    c x'4 -> x0
    c x .trunc 4 -> x0
    c r0 .signed -> x0
    c r0 .unsigned -> x0
    c r0 .generic -> x0
    c .raw @asdf -> x0
    c #asdf -> x0
    ld .d $+4 -> r0
    ld .i $+4 -> r0
    ld .s sp+4 -> r0

    c .rb 15 -> .r 0
    c .rb 4 -> .rx .idx r2
