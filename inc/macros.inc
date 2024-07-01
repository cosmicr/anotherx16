.macro  inc16   addr
.scope
        inc     addr
        bne     @skip
        inc     addr+1
@skip:
.endscope
.endmacro

.macro dec16    addr
.scope
        dec     addr
        bne     @skip
        dec     addr+1
@skip:
.endscope
.endmacro

.macro asl_addr addr, count
  .repeat count
    asl addr
  .endrepeat
.endmacro

.macro lsr_addr addr, count
  .repeat count
    lsr addr
  .endrepeat
.endmacro

.macro asl_a count
  .repeat count
    asl
  .endrepeat
.endmacro

.macro lsr_a count
  .repeat count
    lsr
  .endrepeat
.endmacro

.macro mulx addr, multiplier
  lda #0
  .repeat multiplier
    clc
    adc addr
  .endrepeat
.endmacro

.macro mulx16 addr, multiplier
.scope
  ldx addr         ; Load the low byte of the value
  ldy addr+1       ; Load the high byte of the value
  lda #multiplier  ; Load the multiplier value
  dec              ; Decrement it
  pha              ; Push the (multiplier-1) onto the stack

@loop:
  txa              ; Transfer X to A
  adc addr         ; Add the low byte of the value to A
  sta addr         ; Store the result back to the low byte
  tya              ; Transfer Y to A
  adc addr+1       ; Add the high byte of the value to A
  sta addr+1       ; Store the result back to the high byte

  pla              ; Pull (multiplier-1) from the stack
  dec              ; Decrement it
  pha              ; Push it back onto the stack
  bne @loop        ; If not zero, repeat

  pla              ; Clean up the stack (pull the final zero)
.endscope
.endmacro

.macro add16 addr, value
  clc
  lda addr
  adc #<value
  sta addr
  lda addr+1
  adc #>value
  sta addr+1
.endmacro

.macro add16_addr addra, addrb
  clc
  lda addra
  adc addrb
  sta addra
  lda addra+1
  adc addrb+1
  sta addra+1
.endmacro

.macro sub16_addr addra, addrb
  sec
  lda addra
  sbc addrb
  sta addra
  lda addra+1
  sbc addrb+1
  sta addra+1
.endmacro

.macro txy
  phx
  ply
.endmacro

.macro asl16_addr addr, count
  .repeat count
    asl addr
    rol addr+1
  .endrepeat
.endmacro

.macro asl32_addr addr, count
  .repeat count
    asl addr
    rol addr+1
    rol addr+2
    rol addr+3
  .endrepeat
.endmacro

.macro swap c1, c2
    lda c1
    ldx c2
    sta c2
    stx c1
.endmacro
