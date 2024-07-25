.macro  inc16   addr
.scope
  inc     addr
  bne     @skip ; if we hit zero, increment the high byte
  inc     addr+1
@skip:
.endscope
.endmacro

.macro inc24 addr
  inc     addr
  bne     :+
  inc     addr+1
  bne     :+
  inc     addr+2
:
.endmacro

.macro dec16 addr
  lda    addr
  bne :+
  dec    addr+1
  : dec    addr
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

; these are quite slow, only meant for testing
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

; Multiply two 8 bit values fast, result in A and X (clobbers variables and Y too)
.macro mulx_addr addra, addrb
.scope
  lda #0
  stz mtemp ; requires a zeropage variable
  ldx #0
  beq start
addition:
  clc
  adc addra
  tay
  txa
  adc mtemp
  tax
  tya
loop:
  asl addra
  rol mtemp
start:
  lsr addrb
  bcs addition
  bne loop
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

.macro sub16 addr, value
  sec
  lda addr
  sbc #<value
  sta addr
  lda addr+1
  sbc #>value
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

.macro swap16 c1, c2
    swap c1, c2
    swap c1+1, c2+1
.endmacro

; --------------------------------------------------------------------------------------------
; Macros for test comparison
; C=1 if M <= A     C=0 if A < M      Z=1 if A = M    Z=0 if A != M
; C=0 if M > A      C=1 if A >= M     Z=0 if A != M   Z=1 if A = M
; --------------------------------------------------------------------------------------------
; A > M (C=1 and Z=0)
.macro cmp_gt val_A, val_M, true_label
.scope
  lda val_A
  cmp val_M
  bcc @cmp_gt_false
  beq @cmp_gt_false
  bra true_label
@cmp_gt_false:
.endscope
.endmacro

; A >= M (C=1)
.macro cmp_ge val_A, val_M, true_label
  lda val_A
  cmp val_M
  bcs true_label
.endmacro

; A < M (C=0)
.macro cmp_lt val_A, val_M, true_label
  lda val_A
  cmp val_M
  bcc true_label
.endmacro

; A <= B (C=0 or Z=1)
.macro cmp_le val_A, val_M, true_label
  lda val_A
  cmp val_M
  beq true_label
  bcc true_label
.endmacro

; A == B (Z=1)
.macro cmp_eq val_A, val_M, true_label
  lda val_A
  cmp val_M
  beq true_label
.endmacro

; A != B (Z=0)
.macro cmp_ne val_A, val_M, true_label
  lda val_A
  cmp val_M
  bne true_label
.endmacro

; --------------------------------------------------------------------------------------------
; 16-bit signed comparison
; A is greater than M when:
;   A+1 < M+1 (high bytes) because A+1 would be $00 (positive) and M+1 would be $FF (negative
;   or
;   A+1 = M+1 (high bytes) = $FF and A > M (low bytes)
;   or 
;   A+1 = M+1 (high bytes) = $00 and A > M (low bytes)
; --------------------------------------------------------------------------------------------
; A > M (signed)
.macro cmp_gt_16s val_A, val_M, true_label
.scope
  cmp_lt val_A+1, val_M+1, true_label   ; if A+1 < M+1, A < M
  bne false_label                       ; if high bytes are equal, compare low bytes
  cmp_gt val_A, val_M, true_label       ; if A > M, A > M
  false_label:
.endscope
.endmacro