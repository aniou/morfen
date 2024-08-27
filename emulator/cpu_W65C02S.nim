

# invalid bcd - fail
# V flag for valid bcd     - fail


{.hint[Name]:off.}

import std / [with]
import strformat
import strutils
import emulator / buses
#import emulator / common

# The 6502, R6502, and G65SC02 all use the exact same instruction set.  The
# W65C02S and R65C02 use the same set of instructions except the W65C02S has
# WAI and STP.  The WAI instruction was added for improved interrupt response
# time and low power.  The STP instruction can help to conserve power in
# current designs.

# XXX - sprawdzic liczenie page crossing dla wszystkich skokow
#
# Note, however, a page boundary is crossed when the branch destination is on
# a different page than the next instruction (again, the instruction after the
# branch instruction). This means that 


type
  AddressMode = enum
    Absolute                      # $9876          - p. 288 or 5.2
    Absolute_X_Indirect           # ($1234, X)     - p. 291 or 5.5
    Absolute_X                    # $9876, X       - p. 289 or 5.3
    Absolute_Y                    # $9876, Y       - p. 290 or 5.3
    Absolute_Indirect             # ($1234)        - p. 292 or 5.4
    Accumulator                   # A              - p. 296 or 5.6
    Immediate                     # #$aa           - p. 306 or 5.14
    Implied                       # -              - p. 307 or 5.15
    ZP_Relative                   # zp,rel8         
    PC_Relative                   # rel8           - p. 308 or 5.18  (BRA)
    ZP                            # $12            - p. 298 or 5.7
    ZP_X_Indirect                 # ($12, X)       - p. 301 or 5.11
    ZP_X                          # $12, X         - p. 299 or 5.8
    ZP_Y                          # $12, Y         - p. 300 or 5.8
    ZP_Indirect                   # ($12)          - p. 302 or 5.9
    ZP_Indirect_Y                 # ($12), Y       - p. 304 or 5.12

type
  Opcode = enum
    ADC, AND, ASL, BBR, BBS, BCC, BCS, BEQ, BIT, BMI, BNE, BPL, BRA, BRK, BVC, BVS,
    CLC, CLD, CLI, CLV, CMP, CPX, CPY, DEC, DEX, DEY, EOR, INC, INX, INY, JMP, JSR,
    LDA, LDX, LDY, LSR, NOP, ORA, PHA, PHP, PHX, PHY, PLA, PLP, PLX, PLY, RMB, ROL,
    ROR, RTI, RTS, SBC, SEC, SED, SEI, SMB, STA, STP, STX, STY, STZ, TAX, TAY, TRB,
    TSB, TSX, TXA, TXS, TYA, WAI, ILL

type 
  opcode_65xxx = tuple 
    id    : int
    name  : Opcode
    mode  : AddressMode
    size  : int
    cycles: uint8
    d     : int
    p     : uint8

type
  opArr = array[256, opcode_65xxx]

include cpu_W65C02S_table_ops

#######################################################################
 
#var tmp1 : string

type 
  W65C02S*[B: Bus] = ref object
 
    bus*: B
    name*: string
 
    # user-visible registers
    pc*: uint16
    sp*: uint8

    a*:  uint8
    x*:  uint8
    y*:  uint8

    # alternative - use set
    N*: bool
    V*: bool
    U*: bool # unused bit5, always true
    B*: bool
    D*: bool
    I*: bool
    Z*: bool
    C*: bool

    # internal ones
    ab*:       uint16    # address bus
    ir*:       uint8     # instruction register
    wdm_mode*: bool      # denotes non-standard WDM (0x42) command
    abort*:    bool      # emulator should abort?

    # 
    #L:  bool            # 
    #H:  bool            #

    px: uint8           # page-crossing cost

    b0*:  bool          # temporary flag
    t8*:  uint8         # temporary register
    u8*:  uint8         # secondary temporary register
    t16*: uint16        # temporary register
    u16*: uint16        # temporary register (2)

    ppc*:   uint16      # previous PC - for debug purposes
    step*:  uint8       # step for operation
    steps*: int         # step for operation


proc make_W65C02S* [Bus] (name: string, bus: Bus): W65C02S[Bus] =
  result      = new W65C02S[Bus]
  result.bus  = bus
  result.name = name

#######################################################################

template notImplemented() =
  doAssert false, "not implemented"

#######################################################################

template ab:   uint16 = cpu.ab
template pc:   uint16 = cpu.pc
template t16:  uint16 = cpu.t16
template u16:  uint16 = cpu.u16
template t8:   uint8  = cpu.t8
template u8:   uint8  = cpu.u8
template b0:   bool  = cpu.b0
template x:    uint8  = cpu.x
template y:    uint8  = cpu.y
template a:    uint8  = cpu.a
#template ir:   uint8  = cpu.ir
template sp:   uint8  = cpu.sp
template px:   uint8  = cpu.px
template step: uint8  = cpu.step
template C:    bool   = cpu.C
template Z:    bool   = cpu.Z
template N:    bool   = cpu.N
template V:    bool   = cpu.V
template I:    bool   = cpu.I
template D:    bool   = cpu.D
template B:    bool   = cpu.B
template U:    bool   = cpu.U

#######################################################################

template read8(address: uint16): uint8 =
  uint8(cpu.bus.read(b8, uint32(address)))

template write8(address: uint16, value: uint8) =
  cpu.bus.write(b8, uint32(address), uint32(value))

template asl(val: uint8) =
  ## C <- [76543210] <- 0
  #
  cpu.C = (val and 0x80) == 0x80
  val     =  val shl 1

template rol(val: uint8) =
  ## C <- [76543210] <- C
  #
  let b7  = (val and 0x80) == 0x80
  val     =  val shl 1
  val     =  val or  (if cpu.C: 1 else: 0)
  cpu.C   = b7

template lsr(val: uint8) =
  ## 0 -> [76543210] -> C
  #
  cpu.C   = (val and 0x01) == 0x01
  val     =  val shr 1

template ror(val: uint8) =
  ## C -> [76543210] -> C
  #
  let b7  = (val and 0x01) == 0x01
  val     =  val shr 1
  val     =  val or  (if cpu.C: 0x80 else: 0)
  cpu.C   = b7

template sub(a: untyped, b: untyped) =
  a = a - b

template subcc(a: untyped, b: untyped) =
  if not cpu.C:
    a = a - b

template add(a: untyped, b: untyped) =
  a = a + b

template add(a: untyped, b: untyped, c: untyped) =
  a = b + c

template addcs(a: untyped, b: untyped) =
  if cpu.C:
    a = a + b

#template addif(cond: bool, a: untyped, b: untyped) =
#  if cond:
#    a = a + b

template setpx(a, b: uint16) =
  if (a and 0xFF00) != ((b) and 0xFF00):
    cpu.px = 1

template adds(a: uint16, b: untyped) =
  ## add value, interpreted as signed one
  if b >= 0x80:
    a = a + b - 0x100
  else:
    a = a + b

#template addspx(a: uint16, b: untyped) =
#  ## add value, interpreted as signed one
#  ## check page crossing
#  let orig_a = a
#
#  if b >= 0x80:
#    a = a + b - 0x100
#  else:
#    a = a + b
#
#  if (orig_a and 0xFF00) != (a and 0xFF00):
#    cpu.px = 1 

template ld(a: untyped, b: untyped) =
  a = b

template pull(dest: uint8) =
  inc cpu.sp
  dest = read8(0x100'u16 + cpu.sp)
  
template pull(dest: uint16) =
  inc cpu.sp
  let lo: uint16 =  read8(0x100'u16 + cpu.sp)
  inc cpu.sp
  let hi: uint16 =  read8(0x100'u16 + cpu.sp)
  dest = ((hi shl 8) or lo)

template push(value: uint8) =
  write8(0x100'u16 + cpu.sp, value)
  dec cpu.sp

template push(value: uint16) =
  let vl = uint8(value and 0x00ff)
  let vh = uint8(value shr 8)
  write8(0x100'u16 + cpu.sp, vh)
  dec cpu.sp
  write8(0x100'u16 + cpu.sp, vl)
  dec cpu.sp

template r8(a, b: untyped): untyped =
  a = read8(b)

template r8h(a: uint16, b: untyped): untyped =
  a   = a and 0x00FF
  a   = (a or (uint16(read8(b)) shl 8))

template setn (val: uint8) {.used.} =
  cpu.N = (val shr 7) == 1

template setnz(val: uint8) =
  cpu.N = (val shr 7) == 1
  cpu.Z =  val       == 0

template setv(a, b: uint8, s: uint16) =
  let arg_sign_eq     = ((a xor b)   and 0x80) == 0
  let prod_sign_neq   = ((a xor s)   and 0x80) != 0

  if arg_sign_eq and prod_sign_neq:
    cpu.V = true
  else:
    cpu.V = false

template setz (val: uint8) =
  cpu.Z = val       == 0

template w8(address, value: untyped) =
  write8(address, value)

template `|=`(a, b: uint8) =
  a = a or b

#######################################################################

# xxx - fix it!

proc getP*(cpu: W65C02S): uint8 {.used.} =
  if cpu.N:
     result |= 0b1000_0000'u8
  if cpu.V:
     result |= 0b0100_0000'u8

  result |= 0b0010_0000'u8  # bit 5 always true

  if cpu.B:
     result |= 0b0001_0000'u8
  if cpu.D:
     result |= 0b0000_1000'u8
  if cpu.I:
     result |= 0b0000_0100'u8
  if cpu.Z:
     result |= 0b0000_0010'u8
  if cpu.C:
     result |= 0b0000_0001'u8

#######################################################################

template mode_Absolute               (cpu : W65C02S) =
  r8    ab,   pc+1
  r8h   ab,   pc+2
  add   pc,   2

template mode_Absolute_X_Indirect    (cpu : W65C02S) =
  r8    t16,  pc+1
  r8h   t16,  pc+2
  add   t16,  x
  r8    ab,   t16
  r8h   ab,   t16+1
  add   pc,   2

template mode_Absolute_X             (cpu : W65C02S) =
  r8    ab,   pc+1
  r8h   ab,   pc+2
  setpx ab,   ab+x
  add   ab,   x
  add   pc,   2

template mode_Absolute_Y             (cpu : W65C02S) =
  r8    ab,   pc+1
  r8h   ab,   pc+2
  setpx ab,   ab+y
  add   ab,   y
  add   pc,   2

template mode_Absolute_Indirect      (cpu : W65C02S) =
  r8    t16,  pc+1
  r8h   t16,  pc+2
  r8    ab,   t16
  r8h   ab,   t16+1
  add   pc,   2

template mode_Accumulator            (cpu : W65C02S) =
  discard

template mode_Immediate              (cpu : W65C02S) =
  ld    ab,   pc+1
  add   pc,   1

template mode_Implied                (cpu : W65C02S) =
  discard

template mode_ZP_and_Relative        (cpu : W65C02S) =
  inc    pc
  r8     t16,  pc
  r8     t8,   t16  # data to test
  
  inc    pc
  r8     t16,  pc   # relative jump
  ld     ab,   pc
  ld     u16,  ab   # preserve original
  adds   ab,   t16
  setpx  ab,   u16  # compare with original

  #addspx ab,   t16

template mode_PC_Relative            (cpu : W65C02S) =
  add    pc,   1
  r8     t16,  pc
  ld     ab,   pc
  ld     u16,  ab   # preserve original
  adds   ab,   t16
  setpx  ab,   u16  # compare with original

  #addspx ab,   t16

template mode_ZP                     (cpu : W65C02S) =
  add   pc,   1
  r8    ab,   pc

template mode_ZP_X_Indirect          (cpu : W65C02S) =
  add   pc,   1
  r8    t8,   pc
  add   t8,   x
  r8    ab,   t8
  r8h   ab,   t8+1

template mode_ZP_X                   (cpu : W65C02S) =
  inc   pc
  r8    t8,   pc
  add   t8,   x
  ld    ab,   t8

template mode_ZP_Y                   (cpu : W65C02S) =
  inc   pc
  r8    t8,   pc
  add   t8,   y
  ld    ab,   t8

template mode_ZP_Indirect            (cpu : W65C02S) =
  inc   pc
  r8    t8,   pc
  r8    ab,   t8
  r8h   ab,   t8+1

template mode_ZP_Indirect_Y          (cpu : W65C02S) =
  inc   pc
  r8    t8,   pc
  r8    ab,   t8
  r8h   ab,   t8+1
  setpx ab,   ab+y
  add   ab,   y

#######################################################################

# template op_ADC_bin(cpu : W65C02S) =
#   ld    t16,  a
#   r8    t8,   ab
#   add   t16,  t8
#   addcs t16,  1
#   setv  a,    t8,   t16
# 
#   ld    a,    uint8(t16 and 0x00ff)
#   ld    C,    t16 > 0xff
#   setnz a
#   inc   pc
# 
# template op_ADC_bin2(cpu : W65C02S) =
#   ld    t16,  a
#   r8    t8,   ab
#   add   t16,  t8
#   if C:
#     add t16,  1
#   setv  a,    t8,   t16
# 
#   ld    a,    uint8(t16 and 0x00ff)
#   ld    C,    t16 > 0xff
#   setnz a
#   inc   pc
# 
# template op_ADC_bin3(cpu : W65C02S) =
#   ld       t16,  a
#   r8       t8,   ab
#   add      t16,  t8
#   addif C, t16,  1
#   setv     a,    t8,   t16
# 
#   ld       a,    uint8(t16 and 0x00ff)
#   ld       C,    t16 > 0xff
#   setnz    a
#   inc      pc

template op_ADC(cpu : W65C02S) =

  ## https://palaiologos.rocks/posts/6502-emu/ 
  ##
  ## Sequence 1:
  ## 
  ##     AL = (A & $0F) + (B & $0F) + C
  ##     If AL >= $0A, then AL = ((AL + $06) & $0F) + $10
  ##     A = (A & $F0) + (B & $F0) + AL
  ##     A can be >= $100 at this point
  ##     If A >= $A0, then A = A + $60
  ##     The accumulator result is the lower 8 bits of A
  ##     The carry result is 1 if A >= $100, and 0 if A < $100
  ## 
  ## Sequence 2:
  ## 
  ##     AL = (A & $0F) + (B & $0F) + C
  ##     If AL >= $0A, then AL = ((AL + $06) & $0F) + $10
  ##     A = (A & $F0) + (B & $F0) + AL, using signed (twos complement) arithmetic
  ##     The N flag result is 1 if bit 7 of A is 1, and 0 if bit 7 if A is 0
  ##     The V flag result is 1 if A < -128 or A > 127, and 0 if -128 <= A <= 127
  ## 
  ## On 6502, sequence 1 is used to compute A and C, while sequence 2 is used
  ## to compute N and V. Z is based on the BCD accumulator result.
     
  if not D:
    ld    t16,  a
    r8    t8,   ab
    add   t16,  t8
    addcs t16,  1
    setv  a,    t8,   t16

    ld    a,    uint8(t16 and 0x00ff)
    ld    C,    t16 > 0xff
    setnz a
    inc   pc

  else:
    #   a - first arg
    #  t8 - second arg
    #  u8 - temporary low nybble (4bits)
    # t16 - temporary high nybble and result
   
    # second argument
    r8     t8,   ab
   
    # add low nybble
    ld     u8,   a    and 0x0f
    add    u8,   t8   and 0x0f
    addcs  u8,   0x01
   
    # decimal correction of low nybble
    ld      C,   u8 > 0x09               # two carries - digital >0x0f and binary >0x09 combined
    addcs  u8,   0x06
    ld     u8,   u8   and 0x0f
   
    # add high nybble
    ld    t16,   uint16(a  and 0xf0)
    add   t16,   uint16(t8 and 0xf0)
    addcs t16,   0x10
   
    # V-flag update
    setv    a,   t8, t16 + u8
   
    # decimal correction of high nybble
    ld      C,   t16 > 0x90
    addcs t16,   0x60
    ld    t16,   t16 and 0xf0
   
    # combine low and half nybbles
    add   t16,   u8
    ld      a,   uint8(cpu.t16 and 0xff)
    setnz   a
    inc    pc

template op_AND(cpu : W65C02S) =
  r8    t8,   ab
  ld    a,    a and t8
  setnz a
  inc   pc

template op_ASL_A(cpu : W65C02S) =
  asl   a
  setnz a
  inc   pc

template op_ASL(cpu : W65C02S) =
  r8    t8,   ab
  asl   t8
  setnz t8
  w8    ab,   t8
  inc   pc

template op_BBR0(cpu : W65C02S) =
  if (t8 and 0b0000_0001) == 0:
    ld  pc,   ab
    add step, px+1
  inc pc

template op_BBR1(cpu : W65C02S) =
  if (t8 and 0b0000_0010) == 0:
    ld  pc,   ab
    add step, px+1
  inc pc

template op_BBR2(cpu : W65C02S) =
  if (t8 and 0b0000_0100) == 0:
    ld  pc,   ab
    add step, px+1
  inc pc

template op_BBR3(cpu : W65C02S) =
  if (t8 and 0b0000_1000) == 0:
    ld  pc,   ab
    add step, px+1
  inc pc

template op_BBR4(cpu : W65C02S) =
  if (t8 and 0b0001_0000) == 0:
    ld  pc,   ab
    add step, px+1
  inc pc

template op_BBR5(cpu : W65C02S) =
  if (t8 and 0b0010_0000) == 0:
    ld  pc,   ab
    add step, px+1
  inc pc

template op_BBR6(cpu : W65C02S) =
  if (t8 and 0b0100_0000) == 0:
    ld  pc,   ab
    add step, px+1
  inc pc

template op_BBR7(cpu : W65C02S) =
  if (t8 and 0b1000_0000) == 0:
    ld  pc,   ab
    add step, px+1
  inc pc

template op_BBS0(cpu : W65C02S) =
  if (t8 and 0b0000_0001) != 0:
    ld  pc,   ab
    add step, px+1
  inc pc

template op_BBS1(cpu : W65C02S) =
  if (t8 and 0b0000_0010) != 0:
    ld  pc,   ab
    add step, px+1
  inc pc

template op_BBS2(cpu : W65C02S) =
  if (t8 and 0b0000_0100) != 0:
    ld  pc,   ab
    add step, px+1
  inc pc

template op_BBS3(cpu : W65C02S) =
  if (t8 and 0b0000_1000) != 0:
    ld  pc,   ab
    add step, px+1
  inc pc

template op_BBS4(cpu : W65C02S) =
  if (t8 and 0b0001_0000) != 0:
    ld  pc,   ab
    add step, px+1
  inc pc

template op_BBS5(cpu : W65C02S) =
  if (t8 and 0b0010_0000) != 0:
    ld  pc,   ab
    add step, px+1
  inc pc

template op_BBS6(cpu : W65C02S) =
  if (t8 and 0b0100_0000) != 0:
    ld  pc,   ab
    add step, px+1
  inc pc

template op_BBS7(cpu : W65C02S) =
  if (t8 and 0b1000_0000) != 0:
    ld  pc,   ab
    add step, px+1
  inc pc

template op_BCC(cpu : W65C02S) =
  if not C:
    ld  pc,   ab
    add step, px+1
  inc pc

template op_BCS(cpu : W65C02S) =
  if C:
    ld  pc,   ab
    add step, px+1
  inc pc

template op_BEQ(cpu : W65C02S) =
  if Z:
    ld  pc,   ab
    add step, px+1
  inc pc

template op_BIT_IMM(cpu : W65C02S) =
  r8    t8,       ab
  setz  t8 and a
  inc   pc

template op_BIT(cpu : W65C02S) =
  r8    t8,       ab
  setz  t8 and a
  setn  t8
  ld    V,        (t8 and 0x40) != 0
  inc   pc

template op_BMI(cpu : W65C02S) =
  if N:
    ld  pc,   ab
    add step, px+1
  inc pc

template op_BNE(cpu : W65C02S) =
  if not Z:
    ld  pc,   ab
    add step, px+1
  inc pc

template op_BPL(cpu : W65C02S) =
  if not N:
    ld  pc,   ab
    add step, px+1
  inc pc

template op_BRA(cpu : W65C02S) =
  ld    pc,  ab
  add   pc,  1

template op_BRK(cpu : W65C02S) =
  ## 7.22 BRK Instruction
  ##
  ## The BRK instruction for the NMOS 6502, 65C02 and 65C816 is actually a 2 byte
  ## instruction. The NMOS device simply skips the second byte (i.e. doesn’t care
  ## about the second byte) by incrementing the program counter twice. The 65C02 and
  ## 65C816 does the same thing except the assembler is looking for the second byte
  ## as a “signature byte”. With either device (NMOS or CMOS), the second byte is
  ## not used. It is important to realize that if a return from interrupt is used it
  ## will return to the location after the second or signature byte.
   
  ## see also: http://forum.6502.org/viewtopic.php?t=1917#p15948
  add   pc,      2
  push  pc
  ld    B,       true
  ld    t8,      cpu.getP or 0x10  # BRK flag set to 1 when PHP/BRK
  push  t8
  ld    I,       true
  ld    D,       false
  r8    pc,      0xFFFE
  r8h   pc,      0xFFFF

template op_BVC(cpu : W65C02S) =
  if not V:
    ld  pc,   ab
    add step, px+1
  inc pc

template op_BVS(cpu : W65C02S) =
  if V:
    ld  pc,   ab
    add step, px+1
  inc pc

template op_CLC(cpu : W65C02S) =
  ld    C,     false
  add   pc,    1

template op_CLD(cpu : W65C02S) =
  ld    D,     false
  add   pc,    1

template op_CLI(cpu : W65C02S) =
  ld    I,     false
  add   pc,    1

template op_CLV(cpu : W65C02S) =
  ld    V,     false
  add   pc,    1

template op_CMP(cpu : W65C02S) =
  r8    t8,     ab
  ld    C,      a>=t8
  setnz a - t8
  inc   pc

template op_CPX(cpu : W65C02S) =
  r8    t8,     ab
  ld    C,      x>=t8
  setnz x - t8
  inc   pc

template op_CPY(cpu : W65C02S) =
  r8    t8,     ab
  ld    C,      y>=t8
  setnz y - t8
  inc   pc

template op_DEC_A(cpu : W65C02S) =
  dec   a
  setnz a
  inc   pc

template op_DEC(cpu : W65C02S) =
  r8    t8,   ab
  dec   t8
  setnz t8
  w8    ab,   t8
  inc   pc

template op_DEX(cpu : W65C02S) =
  dec   x
  setnz x
  inc   pc

template op_DEY(cpu : W65C02S) =
  dec   y
  setnz y
  inc   pc

template op_EOR(cpu : W65C02S) =
  r8    t8,   ab
  ld    a,    a xor t8
  setnz a
  inc   pc

template op_INC_A(cpu : W65C02S) =
  inc   a
  setnz a
  inc   pc

template op_INC(cpu : W65C02S) =
  r8    t8,   ab
  inc   t8
  setnz t8
  w8    ab,   t8
  inc   pc

template op_INX(cpu : W65C02S) =
  inc   x
  setnz x
  inc   pc

template op_INY(cpu : W65C02S) =
  inc   y
  setnz y
  inc   pc

template op_JMP(cpu : W65C02S) =
  ld    pc,   ab

template op_JSR(cpu : W65C02S) =
  push  pc
  ld    pc,   ab

template op_LDA(cpu : W65C02S) =
  r8    a,    ab
  setnz a
  inc   pc

template op_LDX(cpu : W65C02S) =
  r8    x,    ab
  setnz x
  inc   pc

template op_LDY(cpu : W65C02S) =
  r8    y,    ab
  setnz y
  inc   pc

template op_LSR_A(cpu : W65C02S) =
  lsr   a
  setnz a
  inc   pc

template op_LSR(cpu : W65C02S) =
  r8    t8,   ab
  lsr   t8
  setnz t8
  w8    ab,   t8
  inc   pc

template op_NOP(cpu : W65C02S) =
  inc   pc

template op_ORA(cpu : W65C02S) =
  r8    t8,   ab
  ld    a,    a or t8
  setnz a
  inc   pc

template op_PHA(cpu : W65C02S) =
  push  a
  inc   pc

template op_PHP(cpu : W65C02S) =
  ld    t8,   cpu.getP or 0x10  # BRK flag set to 1 when PHP/BRK
  push  t8
  inc   pc

template op_PHX(cpu : W65C02S) =
  push  x
  inc   pc

template op_PHY(cpu : W65C02S) =
  push  y
  inc   pc

template op_PLA(cpu : W65C02S) =
  pull  a
  setnz a
  inc   pc

template op_PLP(cpu : W65C02S) =
  pull  t8
  ld    N,    (t8 and 0b1000_0000'u8) != 0
  ld    V,    (t8 and 0b0100_0000'u8) != 0
  ld    U,    true
  #ld   B,    (t8 and 0b0001_0000'u8) != 0  # there is no set for B flag
  ld    D,    (t8 and 0b0000_1000'u8) != 0
  ld    I,    (t8 and 0b0000_0100'u8) != 0
  ld    Z,    (t8 and 0b0000_0010'u8) != 0
  ld    C,    (t8 and 0b0000_0001'u8) != 0
  inc   pc

template op_PLX(cpu : W65C02S) =
  pull  x
  setnz x
  inc   pc

template op_PLY(cpu : W65C02S) =
  pull  y
  setnz y
  inc   pc

template op_RMB0(cpu : W65C02S) =
  r8    t8,   ab
  w8    ab,   t8 and 0b1111_1110
  inc   pc

template op_RMB1(cpu : W65C02S) =
  r8    t8,   ab
  w8    ab,   t8 and 0b1111_1101
  inc   pc

template op_RMB2(cpu : W65C02S) =
  r8    t8,   ab
  w8    ab,   t8 and 0b1111_1011
  inc   pc

template op_RMB3(cpu : W65C02S) =
  r8    t8,   ab
  w8    ab,   t8 and 0b1111_0111
  inc   pc

template op_RMB4(cpu : W65C02S) =
  r8    t8,   ab
  w8    ab,   t8 and 0b1110_1111
  inc   pc

template op_RMB5(cpu : W65C02S) =
  r8    t8,   ab
  w8    ab,   t8 and 0b1101_1111
  inc   pc

template op_RMB6(cpu : W65C02S) =
  r8    t8,   ab
  w8    ab,   t8 and 0b1011_1111
  inc   pc

template op_RMB7(cpu : W65C02S) =
  r8    t8,   ab
  w8    ab,   t8 and 0b0111_1111
  inc   pc

template op_ROL_A(cpu : W65C02S) =
  rol    a
  setnz  a
  inc    pc

template op_ROL(cpu : W65C02S) =
  r8     t8,  ab
  rol    t8
  setnz  t8
  w8     ab,  t8
  inc    pc

template op_ROR_A(cpu : W65C02S) =
  ror    a
  setnz  a
  inc    pc

template op_ROR(cpu : W65C02S) =
  r8     t8,  ab
  ror    t8
  setnz  t8
  w8     ab,  t8
  inc    pc

template op_RTI(cpu : W65C02S) =
  pull   t8
  ld     N,   (t8 and 0b1000_0000'u8) != 0
  ld     V,   (t8 and 0b0100_0000'u8) != 0
  ld     U,   true
  #ld    B,   (t8 and 0b0001_0000'u8) != 0  # there is no set for B flag
  ld     D,   (t8 and 0b0000_1000'u8) != 0
  ld     I,   (t8 and 0b0000_0100'u8) != 0
  ld     Z,   (t8 and 0b0000_0010'u8) != 0
  ld     C,   (t8 and 0b0000_0001'u8) != 0
  pull   pc

template op_RTS(cpu : W65C02S) =
  pull   pc
  inc    pc

# template op_SBC_bin(cpu : W65C02S) =
#   ld       t16,   a
#   r8        t8,   ab
# 
#   add      t16,   not t8
#   addcs    t16,   1
# 
#   ld         C,   t16 > 0xff
#   setv       a,   not t8,    t16
#   ld         a,   t16
#   setnz      a
#   inc       pc

template op_SBC(cpu : W65C02S) =
  ##
  ## accumulator = accumulator - data - 1 + carry 
  ##

  if not D:
    ld    t16,  a
    r8     t8,  ab
  
    add    t16,  (not t8)
    add    t16,  if C: 1 else: 0            # XXX -> addcs t16, 1
  
    ld     C,  if t16 > 0xff: true else: false
    setv   a,  (not t8), t16
    ld     a,  uint8(t16 and 0x00ff)
    setnz  a
    inc   pc
  
  else:

    ## http://6502.org/tutorials/decimal_mode.html#A
    ##
    ## 4a. AL = (A & $0F) - (B & $0F) + C-1
    ## 4b. A = A - B + C-1
    ## 4c. If A < 0, then A = A - $60
    ## 4d. If AL < 0, then A = A - $06
    ## 4e. The accumulator result is the lower 8 bits of A

    ## smth like
    ## 1 bin sub low
    ## 2 bin sub high with carry from low
    ## 3 dec correct low
    ## 4 dec correct high if carry from low?

    # sbc   end 6f - 05 (C=true, D=true) = 6a (C=true, D=true)
    # sbc   end ea - 0b (C=true, D=true) = d9 (C=true, D=true)
    # sbc   end 00 - 0b (C=true, D=true) = 8f (C=false, D=true)

    # second argument
    r8     t8,   ab

    # calc V
    ld     t16,  a
    sub    t16, t8
    subcc  t16, 0x01
    setv   a,  (not t8), t16

    #echo "---"
    #tmp1 = fmt"{cpu.a:02x} - {cpu.t8:02x} (C={cpu.C}, D={cpu.D})"
    #echo   fmt"sbc start {tmp1}"

    # sub low nybble - only for dec correction, later
    ld     u8,   a    and 0x0f
    sub    u8,   t8   and 0x0f
    subcc  u8,   0x01

    # sub all
    ld    t16,   uint16(a)
    sub   t16,   uint16(t8)
    subcc t16,   0x01

    # decimal correction
    ld      C,   (t16 and 0x100) != 0x100    # or and 0x8000 != 0x8000
    ld     b0,   C                           # preserve C
    subcc t16,   0x60

    ld      C,   (u8  and  0x10) !=  0x10    # or and 0x80 != 0x80
    subcc t16,   0x06
    ld      C,   b0                          # restore C

    ld      a,   uint8(cpu.t16 and 0xff)
    setnz   a
    inc    pc

    #echo fmt"sbc   end {tmp1} = {cpu.a:02x} (C={cpu.C}, D={cpu.D})"

template op_SEC(cpu : W65C02S) =
  ld    C,     true
  add   pc,    1

template op_SED(cpu : W65C02S) =
  ld    D,     true
  add   pc,    1

template op_SEI(cpu : W65C02S) =
  ld    I,     true
  add   pc,    1

template op_SMB0(cpu : W65C02S) =
  r8    t8,   ab
  w8    ab,   t8 or 0b0000_0001
  inc   pc

template op_SMB1(cpu : W65C02S) =
  r8    t8,   ab
  w8    ab,   t8 or 0b0000_0010
  inc   pc

template op_SMB2(cpu : W65C02S) =
  r8    t8,   ab
  w8    ab,   t8 or 0b0000_0100
  inc   pc

template op_SMB3(cpu : W65C02S) =
  r8    t8,   ab
  w8    ab,   t8 or 0b0000_1000
  inc   pc

template op_SMB4(cpu : W65C02S) =
  r8    t8,   ab
  w8    ab,   t8 or 0b0001_0000
  inc   pc

template op_SMB5(cpu : W65C02S) =
  r8    t8,   ab
  w8    ab,   t8 or 0b0010_0000
  inc   pc

template op_SMB6(cpu : W65C02S) =
  r8    t8,   ab
  w8    ab,   t8 or 0b0100_0000
  inc   pc

template op_SMB7(cpu : W65C02S) =
  r8    t8,   ab
  w8    ab,   t8 or 0b1000_0000
  inc   pc

template op_STA(cpu : W65C02S) =
  w8    ab,   a
  inc   pc

template op_STP(cpu : W65C02S) =
  notImplemented

template op_STX(cpu : W65C02S) =
  w8    ab,   x
  inc   pc

template op_STY(cpu : W65C02S) =
  w8    ab,   y
  inc   pc

template op_STZ(cpu : W65C02S) =
  w8    ab,   0'u8
  inc   pc

template op_TAX(cpu : W65C02S) =
  ld    x,    a
  setnz x
  inc   pc

template op_TAY(cpu : W65C02S) =
  ld    y,    a
  setnz y
  inc   pc

template op_TRB(cpu : W65C02S) =
  r8    t8,      ab
  w8    ab,      t8 and (not a)
  setz  t8 and a
  inc   pc

template op_TSB(cpu : W65C02S) =
  r8    t8,      ab
  w8    ab,      t8 or a
  setz  t8 and a
  inc   pc

template op_TSX(cpu : W65C02S) =
  ld    x,    sp
  setnz x
  inc   pc

template op_TXA(cpu : W65C02S) =
  ld    a,    x
  setnz a
  inc   pc

template op_TXS(cpu : W65C02S) =
  ld    sp,   x
  inc   pc

template op_TYA(cpu : W65C02S) =
  ld    a,    y
  setnz a
  inc   pc

template op_ILL1(cpu : W65C02S) =
  add   pc,    1

template op_ILL2(cpu : W65C02S) =
  add   pc,    2

template op_ILL3(cpu : W65C02S) =
  add   pc,    3

template op_WAI(cpu : W65C02S) =
  notImplemented

template op_WDM(cpu : W65C02S) =
  r8    t8,      ab
  echo fmt"ab: {cpu.ab}"
  echo fmt"t8: {cpu.t8}"
  case t8
  of   0x80:                        # status OK
    cpu.abort = true
  of   0x81 .. 0x89:                       # status ERR
    cpu.abort = true
  else     :
    add   pc,    2

#######################################################################

proc run_opcode(cpu : var W65C02S, opcode : uint8) =
  case opcode
  of 0x00:                                 # BRK Break       7
     cpu.mode_Implied
     cpu.op_BRK

  of 0x01:                                 # ORA ($12,X)     6
     cpu.mode_ZP_X_Indirect
     cpu.op_ORA

  of 0x02:                                 # ILL             2
     cpu.mode_Implied
     cpu.op_ILL2

  of 0x03:                                 # ILL W65C02      1
     cpu.mode_Implied
     cpu.op_ILL1

  of 0x04:                                 # TSB $12         5
     cpu.mode_ZP
     cpu.op_TSB

  of 0x05:                                 # ORA $12         3
     cpu.mode_ZP
     cpu.op_ORA

  of 0x06:                                 # ASL $12         5
     cpu.mode_ZP
     cpu.op_ASL

  of 0x07:                                 # RMB 0,$12       5
     cpu.mode_ZP
     cpu.op_RMB0

  of 0x08:                                 # PHP SR          3
     cpu.mode_Implied
     cpu.op_PHP

  of 0x09:                                 # ORA #$12        2
     cpu.mode_Immediate
     cpu.op_ORA

  of 0x0A:                                 # ASL A           2
     cpu.mode_Accumulator
     cpu.op_ASL_A

  of 0x0B:                                 # ILL             1
     cpu.mode_Implied
     cpu.op_ILL1

  of 0x0C:                                 # TSB $1234       6
     cpu.mode_Absolute
     cpu.op_TSB

  of 0x0D:                                 # ORA $1234       4
     cpu.mode_Absolute
     cpu.op_ORA

  of 0x0E:                                 # ASL $1234       6
     cpu.mode_Absolute
     cpu.op_ASL

  of 0x0F:                                 # BBR 0,$12,$34   5+t+t*p
     cpu.mode_ZP_and_Relative
     cpu.op_BBR0

  of 0x10:                                 # BPL $12         2+t+t*p
     cpu.mode_PC_Relative
     cpu.op_BPL

  of 0x11:                                 # ORA ($12),Y     5+p
     cpu.mode_ZP_Indirect_Y
     cpu.op_ORA

  of 0x12:                                 # ORA ($12)       5
     cpu.mode_ZP_Indirect
     cpu.op_ORA

  of 0x13:                                 # ILL NOPs.       1
     cpu.mode_Implied
     cpu.op_ILL1

  of 0x14:                                 # TRB $12         5
     cpu.mode_ZP
     cpu.op_TRB

  of 0x15:                                 # ORA $12,X       4
     cpu.mode_ZP_X
     cpu.op_ORA

  of 0x16:                                 # ASL $12,X       6
     cpu.mode_ZP_X
     cpu.op_ASL

  of 0x17:                                 # RMB 1,$12       5
     cpu.mode_ZP
     cpu.op_RMB1

  of 0x18:                                 # CLC 0           2
     cpu.mode_Implied
     cpu.op_CLC

  of 0x19:                                 # ORA $1234,Y     4+p
     cpu.mode_Absolute_Y
     cpu.op_ORA

  of 0x1A:                                 # INC A           2
     cpu.mode_Accumulator
     cpu.op_INC_A

  of 0x1B:                                 # ILL             1
     cpu.mode_Implied
     cpu.op_ILL1

  of 0x1C:                                 # TRB $1234       6
     cpu.mode_Absolute
     cpu.op_TRB

  of 0x1D:                                 # ORA $1234,X     4+p
     cpu.mode_Absolute_X
     cpu.op_ORA

  of 0x1E:                                 # ASL $1234,X     6+p
     cpu.mode_Absolute_X
     cpu.op_ASL

  of 0x1F:                                 # BBR 1,$12,$34   5+t+t*p
     cpu.mode_ZP_and_Relative
     cpu.op_BBR1

  of 0x20:                                 # JSR $1234       6
     cpu.mode_Absolute
     cpu.op_JSR

  of 0x21:                                 # AND ($12,X)     6
     cpu.mode_ZP_X_Indirect
     cpu.op_AND

  of 0x22:                                 # ILL             2
     cpu.mode_Implied
     cpu.op_ILL2

  of 0x23:                                 # ILL             1
     cpu.mode_Implied
     cpu.op_ILL1

  of 0x24:                                 # BIT $12         3
     cpu.mode_ZP
     cpu.op_BIT

  of 0x25:                                 # AND $12         3
     cpu.mode_ZP
     cpu.op_AND

  of 0x26:                                 # ROL $12         5
     cpu.mode_ZP
     cpu.op_ROL

  of 0x27:                                 # RMB 2,$12       5
     cpu.mode_ZP
     cpu.op_RMB2

  of 0x28:                                 # PLP SP          4
     cpu.mode_Implied
     cpu.op_PLP

  of 0x29:                                 # AND #$12        2
     cpu.mode_Immediate
     cpu.op_AND

  of 0x2A:                                 # ROL A           2
     cpu.mode_Accumulator
     cpu.op_ROL_A

  of 0x2B:                                 # ILL             1
     cpu.mode_Implied
     cpu.op_ILL1

  of 0x2C:                                 # BIT $1234       4
     cpu.mode_Absolute
     cpu.op_BIT

  of 0x2D:                                 # AND $1234       4
     cpu.mode_Absolute
     cpu.op_AND

  of 0x2E:                                 # ROL $1234       6
     cpu.mode_Absolute
     cpu.op_ROL

  of 0x2F:                                 # BBR 2,$12,$34   5+t+t*p
     cpu.mode_ZP_and_Relative
     cpu.op_BBR2

  of 0x30:                                 # BMI $12         2+t+t*p
     cpu.mode_PC_Relative
     cpu.op_BMI

  of 0x31:                                 # AND ($12),Y     5+p
     cpu.mode_ZP_Indirect_Y
     cpu.op_AND

  of 0x32:                                 # AND ($12)       5
     cpu.mode_ZP_Indirect
     cpu.op_AND

  of 0x33:                                 # ILL             1
     cpu.mode_Implied
     cpu.op_ILL1

  of 0x34:                                 # BIT $12,X       4
     cpu.mode_ZP_X
     cpu.op_BIT

  of 0x35:                                 # AND $12,X       4
     cpu.mode_ZP_X
     cpu.op_AND

  of 0x36:                                 # ROL $12,X       6
     cpu.mode_ZP_X
     cpu.op_ROL

  of 0x37:                                 # RMB 3,$12       5
     cpu.mode_ZP
     cpu.op_RMB3

  of 0x38:                                 # SEC 1           2
     cpu.mode_Implied
     cpu.op_SEC

  of 0x39:                                 # AND $1234,Y     4+p
     cpu.mode_Absolute_Y
     cpu.op_AND

  of 0x3A:                                 # DEC A           2
     cpu.mode_Accumulator
     cpu.op_DEC_A

  of 0x3B:                                 # ILL             1
     cpu.mode_Implied
     cpu.op_ILL1

  of 0x3C:                                 # BIT $1234,X     4+p
     cpu.mode_Absolute_X
     cpu.op_BIT

  of 0x3D:                                 # AND $1234,X     4+p
     cpu.mode_Absolute_X
     cpu.op_AND

  of 0x3E:                                 # ROL $1234,X     6+p
     cpu.mode_Absolute_X
     cpu.op_ROL

  of 0x3F:                                 # BBR 3,$12,$34   5+t+t*p
     cpu.mode_ZP_and_Relative
     cpu.op_BBR3

  of 0x40:                                 # RTI Return      6
     cpu.mode_Implied
     cpu.op_RTI

  of 0x41:                                 # EOR ($12,X)     6
     cpu.mode_ZP_X_Indirect
     cpu.op_EOR

  # WARNING: there is no WDM opcode on W65C02S
  #          it is a non-standard extension for debug purposes
  of 0x42:
    if cpu.wdm_mode:
      cpu.mode_Immediate
      cpu.op_WDM                           # WDM #$12        2
    else:
      cpu.op_ILL2                          # ILL             2

  of 0x43:                                 # ILL             1
     cpu.mode_Implied
     cpu.op_ILL1

  of 0x44:                                 # ILL             3
     cpu.mode_Implied
     cpu.op_ILL2

  of 0x45:                                 # EOR $12         3
     cpu.mode_ZP
     cpu.op_EOR

  of 0x46:                                 # LSR $12         5
     cpu.mode_ZP
     cpu.op_LSR

  of 0x47:                                 # RMB 4,$12       5
     cpu.mode_ZP
     cpu.op_RMB4

  of 0x48:                                 # PHA A           3
     cpu.mode_Implied
     cpu.op_PHA

  of 0x49:                                 # EOR #$12        2
     cpu.mode_Immediate
     cpu.op_EOR

  of 0x4A:                                 # LSR A           2
     cpu.mode_Accumulator
     cpu.op_LSR_A

  of 0x4B:                                 # ILL             1
     cpu.mode_Implied
     cpu.op_ILL1

  of 0x4C:                                 # JMP $1234       3
     cpu.mode_Absolute
     cpu.op_JMP

  of 0x4D:                                 # EOR $1234       4
     cpu.mode_Absolute
     cpu.op_EOR

  of 0x4E:                                 # LSR $1234       6
     cpu.mode_Absolute
     cpu.op_LSR

  of 0x4F:                                 # BBR 4,$12,$34   5+t+t*p
     cpu.mode_ZP_and_Relative
     cpu.op_BBR4

  of 0x50:                                 # BVC $12         2+t+t*p
     cpu.mode_PC_Relative
     cpu.op_BVC

  of 0x51:                                 # EOR ($12),Y     5+p
     cpu.mode_ZP_Indirect_Y
     cpu.op_EOR

  of 0x52:                                 # EOR ($12)       5
     cpu.mode_ZP_Indirect
     cpu.op_EOR

  of 0x53:                                 # ILL             1
     cpu.mode_Implied
     cpu.op_ILL1

  of 0x54:                                 # ILL             4
     cpu.mode_Implied
     cpu.op_ILL2

  of 0x55:                                 # EOR $12,X       4
     cpu.mode_ZP_X
     cpu.op_EOR

  of 0x56:                                 # LSR $12,X       6
     cpu.mode_ZP_X
     cpu.op_LSR

  of 0x57:                                 # RMB 5,$12       5
     cpu.mode_ZP
     cpu.op_RMB5

  of 0x58:                                 # CLI 0           2
     cpu.mode_Implied
     cpu.op_CLI

  of 0x59:                                 # EOR $1234,Y     4+p
     cpu.mode_Absolute_Y
     cpu.op_EOR

  of 0x5A:                                 # PHY Y           3
     cpu.mode_Implied
     cpu.op_PHY

  of 0x5B:                                 # ILL             1
     cpu.mode_Implied
     cpu.op_ILL1

  of 0x5C:                                 # ILL             8
     cpu.mode_Implied
     cpu.op_ILL3

  of 0x5D:                                 # EOR $1234,X     4+p
     cpu.mode_Absolute_X
     cpu.op_EOR

  of 0x5E:                                 # LSR $1234,X     6+p
     cpu.mode_Absolute_X
     cpu.op_LSR

  of 0x5F:                                 # BBR 5,$12,$34   5+t+t*p
     cpu.mode_ZP_and_Relative
     cpu.op_BBR5

  of 0x60:                                 # RTS Return      6
     cpu.mode_Implied
     cpu.op_RTS

  of 0x61:                                 # ADC ($12,X)     6+d
     cpu.mode_ZP_X_Indirect
     cpu.op_ADC

  of 0x62:                                 # ILL             2
     cpu.mode_Implied
     cpu.op_ILL2

  of 0x63:                                 # ILL             1
     cpu.mode_Implied
     cpu.op_ILL1

  of 0x64:                                 # STZ $12         3
     cpu.mode_ZP
     cpu.op_STZ

  of 0x65:                                 # ADC $12         3+d
     cpu.mode_ZP
     cpu.op_ADC

  of 0x66:                                 # ROR $12         5
     cpu.mode_ZP
     cpu.op_ROR

  of 0x67:                                 # RMB 6,$12       5
     cpu.mode_ZP
     cpu.op_RMB6

  of 0x68:                                 # PLA SP          4
     cpu.mode_Implied
     cpu.op_PLA

  of 0x69:                                 # ADC #$12        2+d
     cpu.mode_Immediate
     cpu.op_ADC

  of 0x6A:                                 # ROR A           2
     cpu.mode_Accumulator
     cpu.op_ROR_A

  of 0x6B:                                 # ILL             1
     cpu.mode_Implied
     cpu.op_ILL1

  of 0x6C:                                 # JMP ($1234)     5
     cpu.mode_Absolute_Indirect
     cpu.op_JMP

  of 0x6D:                                 # ADC $1234       4+d
     cpu.mode_Absolute
     cpu.op_ADC

  of 0x6E:                                 # ROR $1234       6
     cpu.mode_Absolute
     cpu.op_ROR

  of 0x6F:                                 # BBR 6,$12,$34   5+t+t*p
     cpu.mode_ZP_and_Relative
     cpu.op_BBR6

  of 0x70:                                 # BVS $12         2+t+t*p
     cpu.mode_PC_Relative
     cpu.op_BVS

  of 0x71:                                 # ADC ($12),Y     5+d+p
     cpu.mode_ZP_Indirect_Y
     cpu.op_ADC

  of 0x72:                                 # ADC ($12)       5+d
     cpu.mode_ZP_Indirect
     cpu.op_ADC

  of 0x73:                                 # ILL             1
     cpu.mode_Implied
     cpu.op_ILL1

  of 0x74:                                 # STZ $12,X       4
     cpu.mode_ZP_X
     cpu.op_STZ

  of 0x75:                                 # ADC $12,X       4+d
     cpu.mode_ZP_X
     cpu.op_ADC

  of 0x76:                                 # ROR $12,X       6
     cpu.mode_ZP_X
     cpu.op_ROR

  of 0x77:                                 # RMB 7,$12       5
     cpu.mode_ZP
     cpu.op_RMB7

  of 0x78:                                 # SEI 1           2
     cpu.mode_Implied
     cpu.op_SEI

  of 0x79:                                 # ADC $1234,Y     4+d+p
     cpu.mode_Absolute_Y
     cpu.op_ADC

  of 0x7A:                                 # PLY SP          4
     cpu.mode_Implied
     cpu.op_PLY

  of 0x7B:                                 # ILL             1
     cpu.mode_Implied
     cpu.op_ILL1

  of 0x7C:                                 # JMP ($1234,X)   6
     cpu.mode_Absolute_X_Indirect
     cpu.op_JMP

  of 0x7D:                                 # ADC $1234,X     4+d+p
     cpu.mode_Absolute_X
     cpu.op_ADC

  of 0x7E:                                 # ROR $1234,X     6+p
     cpu.mode_Absolute_X
     cpu.op_ROR

  of 0x7F:                                 # BBR 7,$12,$34   5+t+t*p
     cpu.mode_ZP_and_Relative
     cpu.op_BBR7

  of 0x80:                                 # BRA $12         3
     cpu.mode_PC_Relative
     cpu.op_BRA

  of 0x81:                                 # STA ($12,X)     6
     cpu.mode_ZP_X_Indirect
     cpu.op_STA

  of 0x82:                                 # ILL             2
     cpu.mode_Implied
     cpu.op_ILL2

  of 0x83:                                 # ILL             1
     cpu.mode_Implied
     cpu.op_ILL1

  of 0x84:                                 # STY $12         3
     cpu.mode_ZP
     cpu.op_STY

  of 0x85:                                 # STA $12         3
     cpu.mode_ZP
     cpu.op_STA

  of 0x86:                                 # STX $12         3
     cpu.mode_ZP
     cpu.op_STX

  of 0x87:                                 # SMB 0,$12       5
     cpu.mode_ZP
     cpu.op_SMB0

  of 0x88:                                 # DEY Y           2
     cpu.mode_Implied
     cpu.op_DEY

  of 0x89:                                 # BIT #$12        2
     cpu.mode_Immediate
     cpu.op_BIT_IMM

  of 0x8A:                                 # TXA X           2
     cpu.mode_Implied
     cpu.op_TXA

  of 0x8B:                                 # ILL             1
     cpu.mode_Implied
     cpu.op_ILL1

  of 0x8C:                                 # STY $1234       4
     cpu.mode_Absolute
     cpu.op_STY

  of 0x8D:                                 # STA $1234       4
     cpu.mode_Absolute
     cpu.op_STA

  of 0x8E:                                 # STX $1234       4
     cpu.mode_Absolute
     cpu.op_STX

  of 0x8F:                                 # BBS 0,$12,$34   5+t+t*p
     cpu.mode_ZP_and_Relative
     cpu.op_BBS0

  of 0x90:                                 # BCC $12         2+t+t*p
     cpu.mode_PC_Relative
     cpu.op_BCC

  of 0x91:                                 # STA ($12),Y     6
     cpu.mode_ZP_Indirect_Y
     cpu.op_STA

  of 0x92:                                 # STA ($12)       5
     cpu.mode_ZP_Indirect
     cpu.op_STA

  of 0x93:                                 # ILL             1
     cpu.mode_Implied
     cpu.op_ILL1

  of 0x94:                                 # STY $12,X       4
     cpu.mode_ZP_X
     cpu.op_STY

  of 0x95:                                 # STA $12,X       4
     cpu.mode_ZP_X
     cpu.op_STA

  of 0x96:                                 # STX $12,Y       4
     cpu.mode_ZP_Y
     cpu.op_STX

  of 0x97:                                 # SMB 1,$12       5
     cpu.mode_ZP
     cpu.op_SMB1

  of 0x98:                                 # TYA Y           2
     cpu.mode_Implied
     cpu.op_TYA

  of 0x99:                                 # STA $1234,Y     5
     cpu.mode_Absolute_Y
     cpu.op_STA

  of 0x9A:                                 # TXS X           2
     cpu.mode_Implied
     cpu.op_TXS

  of 0x9B:                                 # ILL             1
     cpu.mode_Implied
     cpu.op_ILL1

  of 0x9C:                                 # STZ $1234       4
     cpu.mode_Absolute
     cpu.op_STZ

  of 0x9D:                                 # STA $1234,X     5
     cpu.mode_Absolute_X
     cpu.op_STA

  of 0x9E:                                 # STZ $1234,X     5
     cpu.mode_Absolute_X
     cpu.op_STZ

  of 0x9F:                                 # BBS 1,$12,$34   5+t+t*p
     cpu.mode_ZP_and_Relative
     cpu.op_BBS1

  of 0xA0:                                 # LDY #$12        2
     cpu.mode_Immediate
     cpu.op_LDY

  of 0xA1:                                 # LDA ($12,X)     6
     cpu.mode_ZP_X_Indirect
     cpu.op_LDA

  of 0xA2:                                 # LDX #$12        2
     cpu.mode_Immediate
     cpu.op_LDX

  of 0xA3:                                 # ILL             1
     cpu.mode_Implied
     cpu.op_ILL1

  of 0xA4:                                 # LDY $12         3
     cpu.mode_ZP
     cpu.op_LDY

  of 0xA5:                                 # LDA $12         3
     cpu.mode_ZP
     cpu.op_LDA

  of 0xA6:                                 # LDX $12         3
     cpu.mode_ZP
     cpu.op_LDX

  of 0xA7:                                 # SMB 2,$12       5
     cpu.mode_ZP
     cpu.op_SMB2

  of 0xA8:                                 # TAY A           2
     cpu.mode_Implied
     cpu.op_TAY

  of 0xA9:                                 # LDA #$12        2
     cpu.mode_Immediate
     cpu.op_LDA

  of 0xAA:                                 # TAX A           2
     cpu.mode_Implied
     cpu.op_TAX

  of 0xAB:                                 # ILL             1
     cpu.mode_Implied
     cpu.op_ILL1

  of 0xAC:                                 # LDY $1234       4
     cpu.mode_Absolute
     cpu.op_LDY

  of 0xAD:                                 # LDA $1234       4
     cpu.mode_Absolute
     cpu.op_LDA

  of 0xAE:                                 # LDX $1234       4
     cpu.mode_Absolute
     cpu.op_LDX

  of 0xAF:                                 # BBS 2,$12,$34   5+t+t*p
     cpu.mode_ZP_and_Relative
     cpu.op_BBS2

  of 0xB0:                                 # BCS $12         2+t+t*p
     cpu.mode_PC_Relative
     cpu.op_BCS

  of 0xB1:                                 # LDA ($12),Y     5+p
     cpu.mode_ZP_Indirect_Y
     cpu.op_LDA

  of 0xB2:                                 # LDA ($12)       5
     cpu.mode_ZP_Indirect
     cpu.op_LDA

  of 0xB3:                                 # ILL             1
     cpu.mode_Implied
     cpu.op_ILL1

  of 0xB4:                                 # LDY $12,X       4
     cpu.mode_ZP_X
     cpu.op_LDY

  of 0xB5:                                 # LDA $12,X       4
     cpu.mode_ZP_X
     cpu.op_LDA

  of 0xB6:                                 # LDX $12,Y       4
     cpu.mode_ZP_Y
     cpu.op_LDX

  of 0xB7:                                 # SMB 3,$12       5
     cpu.mode_ZP
     cpu.op_SMB3

  of 0xB8:                                 # CLV 0           2
     cpu.mode_Implied
     cpu.op_CLV

  of 0xB9:                                 # LDA $1234,Y     4+p
     cpu.mode_Absolute_Y
     cpu.op_LDA

  of 0xBA:                                 # TSX SP          2
     cpu.mode_Implied
     cpu.op_TSX

  of 0xBB:                                 # ILL             1
     cpu.mode_Implied
     cpu.op_ILL1

  of 0xBC:                                 # LDY $1234,X     4+p
     cpu.mode_Absolute_X
     cpu.op_LDY

  of 0xBD:                                 # LDA $1234,X     4+p
     cpu.mode_Absolute_X
     cpu.op_LDA

  of 0xBE:                                 # LDX $1234,Y     4+p
     cpu.mode_Absolute_Y
     cpu.op_LDX

  of 0xBF:                                 # BBS 3,$12,$34   5+t+t*p
     cpu.mode_ZP_and_Relative
     cpu.op_BBS3

  of 0xC0:                                 # CPY #$12        2
     cpu.mode_Immediate
     cpu.op_CPY

  of 0xC1:                                 # CMP ($12,X)     6
     cpu.mode_ZP_X_Indirect
     cpu.op_CMP

  of 0xC2:                                 # ILL             2
     cpu.mode_Implied
     cpu.op_ILL2

  of 0xC3:                                 # ILL             1
     cpu.mode_Implied
     cpu.op_ILL1

  of 0xC4:                                 # CPY $12         3
     cpu.mode_ZP
     cpu.op_CPY

  of 0xC5:                                 # CMP $12         3
     cpu.mode_ZP
     cpu.op_CMP

  of 0xC6:                                 # DEC $12         5
     cpu.mode_ZP
     cpu.op_DEC

  of 0xC7:                                 # SMB 4,$12       5
     cpu.mode_ZP
     cpu.op_SMB4

  of 0xC8:                                 # INY Y           2
     cpu.mode_Implied
     cpu.op_INY

  of 0xC9:                                 # CMP #$12        2
     cpu.mode_Immediate
     cpu.op_CMP

  of 0xCA:                                 # DEX X           2
     cpu.mode_Implied
     cpu.op_DEX

  of 0xCB:                                 # WAI Wait        2
     cpu.mode_Implied
     cpu.op_WAI

  of 0xCC:                                 # CPY $1234       4
     cpu.mode_Absolute
     cpu.op_CPY

  of 0xCD:                                 # CMP $1234       4
     cpu.mode_Absolute
     cpu.op_CMP

  of 0xCE:                                 # DEC $1234       6
     cpu.mode_Absolute
     cpu.op_DEC

  of 0xCF:                                 # BBS 4,$12,$34   5+t+t*p
     cpu.mode_ZP_and_Relative
     cpu.op_BBS4

  of 0xD0:                                 # BNE $12         2+t+t*p
     cpu.mode_PC_Relative
     cpu.op_BNE

  of 0xD1:                                 # CMP ($12),Y     5+p
     cpu.mode_ZP_Indirect_Y
     cpu.op_CMP

  of 0xD2:                                 # CMP ($12)       5
     cpu.mode_ZP_Indirect
     cpu.op_CMP

  of 0xD3:                                 # ILL             1
     cpu.mode_Implied
     cpu.op_ILL1

  of 0xD4:                                 # ILL             4
     cpu.mode_Implied
     cpu.op_ILL2

  of 0xD5:                                 # CMP $12,X       4
     cpu.mode_ZP_X
     cpu.op_CMP

  of 0xD6:                                 # DEC $12,X       6
     cpu.mode_ZP_X
     cpu.op_DEC

  of 0xD7:                                 # SMB 5,$12       5
     cpu.mode_ZP
     cpu.op_SMB5

  of 0xD8:                                 # CLD 0           2
     cpu.mode_Implied
     cpu.op_CLD

  of 0xD9:                                 # CMP $1234,Y     4+p
     cpu.mode_Absolute_Y
     cpu.op_CMP

  of 0xDA:                                 # PHX X           3
     cpu.mode_Implied
     cpu.op_PHX

  of 0xDB:                                 # STP Stop        3
     cpu.mode_Implied
     cpu.op_STP

  of 0xDC:                                 # ILL             4
     cpu.mode_Implied
     cpu.op_ILL3

  of 0xDD:                                 # CMP $1234,X     4+p
     cpu.mode_Absolute_X
     cpu.op_CMP

  of 0xDE:                                 # DEC $1234,X     7
     cpu.mode_Absolute_X
     cpu.op_DEC

  of 0xDF:                                 # BBS 5,$12,$34   5+t+t*p
     cpu.mode_ZP_and_Relative
     cpu.op_BBS5

  of 0xE0:                                 # CPX #$12        2
     cpu.mode_Immediate
     cpu.op_CPX

  of 0xE1:                                 # SBC ($12,X)     6+d
     cpu.mode_ZP_X_Indirect
     cpu.op_SBC

  of 0xE2:                                 # ILL             2
     cpu.mode_Implied
     cpu.op_ILL2

  of 0xE3:                                 # ILL             1
     cpu.mode_Implied
     cpu.op_ILL1

  of 0xE4:                                 # CPX $12         3
     cpu.mode_ZP
     cpu.op_CPX

  of 0xE5:                                 # SBC $12         3+d
     cpu.mode_ZP
     cpu.op_SBC

  of 0xE6:                                 # INC $12         5
     cpu.mode_ZP
     cpu.op_INC

  of 0xE7:                                 # SMB 6,$12       5
     cpu.mode_ZP
     cpu.op_SMB6

  of 0xE8:                                 # INX X           2
     cpu.mode_Implied
     cpu.op_INX

  of 0xE9:                                 # SBC #$12        2+d
     cpu.mode_Immediate
     cpu.op_SBC

  of 0xEA:                                 # NOP No          2
     cpu.mode_Implied
     cpu.op_NOP

  of 0xEB:                                 # ILL             1
     cpu.mode_Implied
     cpu.op_ILL1

  of 0xEC:                                 # CPX $1234       4
     cpu.mode_Absolute
     cpu.op_CPX

  of 0xED:                                 # SBC $1234       4+d
     cpu.mode_Absolute
     cpu.op_SBC

  of 0xEE:                                 # INC $1234       6
     cpu.mode_Absolute
     cpu.op_INC

  of 0xEF:                                 # BBS 6,$12,$34   5+t+t*p
     cpu.mode_ZP_and_Relative
     cpu.op_BBS6

  of 0xF0:                                 # BEQ $12         2+t+t*p
     cpu.mode_PC_Relative
     cpu.op_BEQ

  of 0xF1:                                 # SBC ($12),Y     5+d+p
     cpu.mode_ZP_Indirect_Y
     cpu.op_SBC

  of 0xF2:                                 # SBC ($12)       5+d
     cpu.mode_ZP_Indirect
     cpu.op_SBC

  of 0xF3:                                 # ILL             1
     cpu.mode_Implied
     cpu.op_ILL1

  of 0xF4:                                 # ILL             4
     cpu.mode_Implied
     cpu.op_ILL2

  of 0xF5:                                 # SBC $12,X       4+d
     cpu.mode_ZP_X
     cpu.op_SBC

  of 0xF6:                                 # INC $12,X       6
     cpu.mode_ZP_X
     cpu.op_INC

  of 0xF7:                                 # SMB 7,$12       5
     cpu.mode_ZP
     cpu.op_SMB7

  of 0xF8:                                 # SED 1           2
     cpu.mode_Implied
     cpu.op_SED

  of 0xF9:                                 # SBC $1234,Y     4+d+p
     cpu.mode_Absolute_Y
     cpu.op_SBC

  of 0xFA:                                 # PLX SP          4
     cpu.mode_Implied
     cpu.op_PLX

  of 0xFB:                                 # ILL             1
     cpu.mode_Implied
     cpu.op_ILL1

  of 0xFC:                                 # ILL             4
     cpu.mode_Implied
     cpu.op_ILL3

  of 0xFD:                                 # SBC $1234,X     4+d+p
     cpu.mode_Absolute_X
     cpu.op_SBC

  of 0xFE:                                 # INC $1234,X     7
     cpu.mode_Absolute_X
     cpu.op_INC

  of 0xFF:                                 # BBS 7,$12,$34   5+t+t*p
     cpu.mode_ZP_and_Relative
     cpu.op_BBS7

#######################################################################
# public functions

proc new_W65C02S*(bus: Bus): W65C02S =
  return W65C02S(bus: bus)

proc status*(cpu: W65C02S): string {.used.} =

  with result:
    add(if cpu.N: "n" else: "-")
    add(if cpu.V: "v" else: "-")
    add("*")
    add(if cpu.B: "b" else: "-")
    add(if cpu.D: "d" else: "-")
    add(if cpu.I: "i" else: "-")
    add(if cpu.Z: "z" else: "-")
    add(if cpu.C: "c" else: "-")

proc fullStatus*(cpu: W65C02S): string {.used.} =
  with result:
    add(fmt"pc: {cpu.pc:04x}  ")
    add(fmt"sp: {cpu.sp:02x}  ")
    add(fmt"ir: {cpu.ir:02x}  ")
    add(cpu.status)
    add("  ")
    add(fmt"a: {cpu.a:02x}  ")
    add(fmt"x: {cpu.x:02x}  ")
    add(fmt"y: {cpu.y:02x}  ")
    add(fmt"ab: {cpu.ab:04x}  ")
    add(fmt"t16: {cpu.t16:04x}  ")
    add(fmt"t8: {cpu.t8:02x}  ")
    add(fmt"step: {cpu.step:02d}  ")

proc disasm*(cpu: W65C02S): string {.used.} =
  var l,h : uint8

  result.add(fmt"{op_table[cpu.ir].name} ".toLowerAscii)

  case op_table[cpu.ir].mode
  of Absolute:                           # $9876          - p. 288 or 5.2
     l = read8(cpu.ppc+1)
     h = read8(cpu.ppc+2)
     result.add(fmt" ${h:02x}{l:02x}")

  of Absolute_X_Indirect:                # ($1234, X)     - p. 291 or 5.5
     l = read8(cpu.ppc+1)
     h = read8(cpu.ppc+2)
     result.add(fmt" (${h:02x}{l:02x}, X)")
     
  of Absolute_X:                         # $9876, X       - p. 289 or 5.3
     l = read8(cpu.ppc+1)
     h = read8(cpu.ppc+2)
     result.add(fmt" ${h:02x}{l:02x}, X")

  of Absolute_Y:                         # $9876, Y       - p. 290 or 5.3
     l = read8(cpu.ppc+1)
     h = read8(cpu.ppc+2)
     result.add(fmt" ${h:02x}{l:02x}, Y")

  of Absolute_Indirect:                  # ($1234)        - p. 292 or 5.4
     l = read8(cpu.ppc+1)
     h = read8(cpu.ppc+2)
     result.add(fmt" (${h:02x}{l:02x})")

  of Accumulator:                        # A              - p. 296 or 5.6
     result.add(" A")

  of Immediate:                          # #$aa           - p. 306 or 5.14
     l = read8(cpu.ppc+1)
     result.add(fmt" #${l:02x}")

  of Implied:
     result.add("")

  of ZP_Relative:                        # zp,rel8
     let bit = case cpu.ir
     of 0x0f, 0x1f, 0x2f, 0x3f, 0x4f, 0x5f, 0x6f, 0x7f:
         fmt"{(cpu.ir and 0xf0) shr 4}"
     of 0x8f, 0x9f, 0xaf, 0xbf, 0xcf, 0xdf, 0xef, 0xff:
         fmt"{((cpu.ir and 0xf0) shr 4) and 0x07}"
     else:
         "?"
     l = read8(cpu.ppc+1)
     h = read8(cpu.ppc+2)
     result.add(fmt" {bit} ${l:02x},${h:02x}")

  of PC_Relative:                        # rel8           - p. 308 or 5.18  (BRA)
     l = read8(cpu.ppc+1)
     result.add(fmt" ${l:02x}")

  of ZP:                                 # $12            - p. 298 or 5.7
     l = read8(cpu.ppc+1)
     result.add(fmt" ${l:02x}")

  of ZP_X_Indirect:                      # ($12, X)       - p. 301 or 5.11
     l = read8(cpu.ppc+1)
     result.add(fmt" (${l:02x}, X)")

  of ZP_X:                               # $12, X         - p. 299 or 5.8
     l = read8(cpu.ppc+1)
     result.add(fmt" ${l:02x}, X")

  of ZP_Y:                               # $12, Y         - p. 300 or 5.8
     l = read8(cpu.ppc+1)
     result.add(fmt" ${l:02x}, Y")

  of ZP_Indirect:                        # ($12)          - p. 302 or 5.9
     l = read8(cpu.ppc+1)
     result.add(fmt" (${l:02x})")

  of ZP_Indirect_Y:                      # ($12), Y       - p. 304 or 5.12
     l = read8(cpu.ppc+1)
     result.add(fmt" (${l:02x}), Y")
   

proc debug*(cpu: W65C02S): string {.used.} =
  with result:
    add(fmt"pc: {cpu.ppc:04x}  ")
    add(fmt"{cpu.disasm:-15s}  ")
    add(fmt"sp: {cpu.sp:02x}  ")
    add(fmt"ir: {cpu.ir:02x}  ")
    add(cpu.status)
    add("  ")
    add(fmt"a: {cpu.a:02x}  ")
    add(fmt"x: {cpu.x:02x}  ")
    add(fmt"y: {cpu.y:02x}  ")
    add(fmt"ab: {cpu.ab:04x}  ")
    add(fmt"t16: {cpu.t16:04x}  ")
    add(fmt"t8: {cpu.t8:02x}  ")
    add(fmt"step: {cpu.step:02d}  ")
    add(fmt"all: {cpu.steps:8d}  ")
  
proc exec*(cpu: var W65C02S) =
    ##
    ## executes single CPU operation and set a cpu.step variable
    ## used by cpu.tick() routine for mimic behaviour closer to
    ## speed of real CPU. Still not accurate because sub-ops should
    ## be executed at particular ticks, not at once
    ##
    cpu.px           = 0
    cpu.ppc          = cpu.pc
    cpu.ir           = read8(cpu.pc)
    cpu.steps       += int(op_table[cpu.ir].cycles)
    cpu.step         = op_table[cpu.ir].cycles
    cpu.run_opcode     cpu.ir

    # in case of conditional branch codes the page-crossing cycle
    # is added in routine - for rest of operands it is taken from
    # table
    if cpu.px != 0:
      cpu.step += op_table[cpu.ir].p

proc tick*(cpu: var W65C02S) =
  ##
  ## uses a "tick" counter for mimicking a real cpu speed - a tick
  ## means a number of cycles required for particular operation to
  ## run. 
  ##
  ## Deviation 1: an operation is executed immediately and then 
  ##              tick counts toward 0
  ## Deviation 2: in real CPU sub-operations (for example: memory 
  ##              reads) are executed according to ticks (cycles)
  ##
  ## An accurate, cycle-stepped implementation exists:
  ## https://floooh.github.io/2019/12/13/cycle-stepped-6502.html
  ##

  if cpu.step > 0:
    dec cpu.step
    return

  if cpu.step == 0:
    cpu.exec

# eof
