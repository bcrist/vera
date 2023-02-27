

# Arithmetic Unit
Inputs: `J`, `K`, `STAT[C]`, `MODE`

Outputs: `L`, `STAT[ZNVC]`

`MODE` | Function
------ | -----------
0b0000 | `J[15:0] + K`
0b0001 | `J[15:0] + ~K + 1`
0b0010 | `J[15:0] + K + STAT[C]`
0b0011 | `J[15:0] + ~K + ~STAT[C]`
0b0100 | `J + zx(K)`
0b0101 | `J + ~zx(K) + 1`
0b0110 | `J + zx(K) + STAT[C]`
0b0111 | `J + ~zx(K) + ~STAT[C]`
0b1000 | `J + sx(K)`
0b1001 | `J + ~sx(K) + 1`
0b1010 | `J + sx(K) + STAT[C]`
0b1011 | `J + ~sx(K) + ~STAT[C]`
0b1100 | `J + { 0xFFFF, K }`
0b1101 | `J + ~{ 0xFFFF, K } + 1`
0b1110 | `J + { 0xFFFF, K } + STAT[C]`
0b1111 | `J + ~{ 0xFFFF, K } + ~STAT[C]`

# Rotator Unit
Inputs: `J`, `K[4:0]`, `MODE[2:0]`

Outputs: `L`, `STAT[ZNC]`

Let `S1` = `MODE[2] ? K[4:0] : K[3:0]`

Let `S2` = `32 - S1`

Let `JJ` = `{ J[15:0], J[15:0] }`

`MODE[1:0]` | Function
- | -
0b00 | `(J >> S1) | (J << S2)`
0b01 | `(J << S1) | (J >> S2)`
0b10 | `(JJ >> S1) | (JJ << S2)`
0b11 | `(JJ << S1) | (JJ >> S2)`

`MODE[2]` | `MODE[0]` | new `STAT[C]`
- | - | -
0b0 | 0b0 | Function[31]
0b0 | 0b1 | Function[16]
0b1 | 0b0 | Function[31]
0b1 | 0b1 | Function[0]

# Logic Unit
Inputs: `J[15:0]`, `K`, `MODE`

Outputs: `L`, `STAT[ZN]`

Let `JJ` = `J[15:0] ^ MODE[2]`

Let `KK` = `K[15:0] ^ MODE[3]`

`MODE[1:0]` | Function
- | -
0b00 | `JJ`
0b01 | `JJ & KK`
0b10 | `JJ | KK`
0b11 | `JJ ^ KK`

