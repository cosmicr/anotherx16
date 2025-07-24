.macro  jra     Target
        .if     .def(Target) .and ((*+2)-(Target) <= 127)
        bra     Target
        .else
        jmp     Target
        .endif
.endmacro

.macro push addr
  lda addr
  pha
.endmacro

.macro pop addr
  pla
  sta addr
.endmacro

.macro push16 addr
  lda addr+1
  pha
  lda addr
  pha
.endmacro

.macro pop16 addr
  pla
  sta addr
  pla
  sta addr+1
.endmacro

.macro abs addr, val_A, val_B
  sec
  lda val_B
  sbc val_A
  bcs :+
  eor #$FF
  inc
  :
  sta addr
.endmacro

.macro min val_A, val_B
  lda val_A
  cmp val_B
  bcc :+
  lda val_B
  :
.endmacro

.macro min16 val_A, val_B
.scope
  lda val_A+1
  cmp val_B+1
  bcc set_result
  bne done
  lda val_A
  cmp val_B
  bcc set_result
done:
  lda val_B
  ldx val_B+1
  bra end
set_result:
  lda val_A
  ldx val_A+1
end:
.endscope
.endmacro

.macro max val_A, val_B
  lda val_A
  cmp val_B
  bcs :+
  lda val_B
  : 
.endmacro

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
.scope
  dec    addr
  lda    addr
  cmp    #$FF
  bne    :+
  dec    addr+1
:
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

; negate a 16 bit number
.macro neg16 addr
  lda addr
  eor #$FF
  sta addr
  lda addr+1
  eor #$FF
  sta addr+1
  clc
  inc16 addr
.endmacro

; these are quite slow, only meant for testing
.macro mulx addr, multiplier
  lda #0
  .repeat multiplier
    clc
    adc addr
  .endrepeat
.endmacro

; this is faster for small values
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

; this version is faster for large values
.macro mulx16large addr, multiplier
.scope
    ; Preserve multiplier value
    lda #multiplier
    sta mtemp+2   ; Store in a temp location
    
    ; Clear the upper bytes of mtemp
    lda #0
    sta mtemp
    sta mtemp+1
    
    ; For each bit in multiplier
    ldx #8
@loop:
    ; Shift mtemp left (multiply by 2)
    asl mtemp
    rol mtemp+1
    
    ; Check next bit of multiplier
    asl mtemp+2
    bcc @skip      ; If bit was 0, skip the addition
    
    ; Add addr to mtemp if bit was 1
    lda mtemp
    clc
    adc addr
    sta mtemp
    lda mtemp+1
    adc addr+1
    sta mtemp+1
    
@skip:
    dex
    bne @loop
    
    ; Copy result back to addr
    lda mtemp
    sta addr
    lda mtemp+1
    sta addr+1
.endscope
.endmacro

.macro mulx16_addr addr, multiplier
.scope
  ldx addr         ; Load the low byte of the value
  ldy addr+1       ; Load the high byte of the value
  lda multiplier   ; Load the multiplier value
  dec              ; Decrement it
  pha              ; Push the (multiplier-1) onto the stack

@loop:
  txa              ; Transfer X to A
  clc              ; Clear carry flag before addition
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
; .macro mulx_addr addra, addrb
; .scope
;     lda #0          ; Clear accumulator for result
;     ldx #0          ; Clear X register for high byte
;     stz mtemp       ; Clear mtemp
;     ldy #8          ; 8 iterations
; multiply_loop:
;     ; Check if we need to add
;     lsr addrb       ; Get lowest bit into carry
;     bcc no_add      ; Skip addition if bit was 0
    
;     ; Add shifted value
;     clc
;     adc addra       ; Add low byte
;     pha             ; Save A
;     txa             ; Get high byte
;     adc mtemp       ; Add high byte with carry
;     tax             ; Put back in X
;     pla             ; Restore A

; no_add:
;     asl addra       ; Shift multiplicand left
;     rol mtemp       ; Rotate carry into high byte
    
;     dey             ; Next iteration
;     bne multiply_loop ; Loop until done
; done:               ; Result: A = low byte, X = high byte
; .endscope
; .endmacro

.macro mulx_addr addra, addrb
.scope
    ; Save original cache values
    lda FX_CACHE_L
    pha
    lda FX_CACHE_M
    pha
    lda FX_CACHE_H
    pha
    lda FX_CACHE_U
    pha

    ; DCSEL = 2 for FX control registers
    lda #(2 << 1)
    sta VERA::CTRL
    
    ; Clear FX control and enable multiplier
    stz FX_CTRL
    lda #%00010000      ; Enable multiplier
    sta FX_MULT
    
    ; DCSEL = 6 for cache registers
    lda #(6 << 1)
    sta VERA::CTRL
    
    ; Reset accumulator
    lda FX_CACHE_L ;FX_ACCUM_RESET
    
    ; Load numbers into cache
    lda addra
    sta FX_CACHE_L      ; First number low byte
    lda addra+1
    sta FX_CACHE_M      ; First number high byte (0 for 8-bit)
    lda addrb
    sta FX_CACHE_H      ; Second number low byte
    lda addrb+1
    sta FX_CACHE_U      ; Second number high byte (0 for 8-bit)
    
    ; Back to DCSEL = 2 for writing result
    lda #(2 << 1)
    sta VERA::CTRL
    
    ; Enable cache write
    lda #%01000000      ; Cache Write Enable
    sta FX_CTRL
    
    ; Set up VRAM address to $1F9B0
    lda #$B0            ; Low byte
    sta VERA::ADDR
    lda #$F9            ; Middle byte
    sta VERA::ADDR+1
    lda #$01            ; High byte (no increment)
    sta VERA::ADDR+2

    ; Trigger multiply and write
    stz VERA::DATA0
    
    ; Set increment to read result
    lda #%00010001      ; Increment 1 + high nibble of bank
    sta VERA::ADDR+2
    
    ; Read result into A (low) and X (high)
    lda VERA::DATA0     ; Low byte
    ldx VERA::DATA0     ; High byte
    
    ; Cleanup
    stz FX_MULT
    stz FX_CTRL
    
    ; Restore cache values
    ply
    sty FX_CACHE_U
    ply
    sty FX_CACHE_H
    ply
    sty FX_CACHE_M
    ply
    sty FX_CACHE_L
.endscope
.endmacro

.macro add addr, value
  clc
  lda addr
  adc #value
  sta addr
.endmacro

.macro add_addr addra, addrb
  clc
  lda addra
  adc addrb
  sta addra
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

.macro sub addr, value
  sec
  lda addr
  sbc #value
  sta addr
.endmacro

.macro sub_addr addra, addrb
  sec
  lda addra
  sbc addrb
  sta addra
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

.macro lsr16_addr addr, count
  .repeat count
    lsr addr+1
    ror addr
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
  bcc :+
  bne true_label
  :
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
; 16-bit comparisons
; --------------------------------------------------------------------------------------------
.macro cmp16_gt val_A, val_M, true_label
  lda val_A+1
  cmp val_M+1
  bcc :+          ; carry is clear when A+1 < M+1
  bne true_label  ; if A+1 > M+1 then A > M
  lda val_A
  cmp val_M
  bcc :+
  bne true_label
  :
.endmacro

.macro cmp16_ge val_A, val_M, true_label
  lda val_A+1
  cmp val_M+1
  bne :+      ; if high bytes are not equal, is A+1 > M+1?
  lda val_A
  cmp val_M
  :
  bcs true_label
.endmacro

.macro cmp16_lt val_A, val_M, true_label
  lda val_A+1
  cmp val_M+1
  bcc true_label
  bne @cmp_lt_false
  lda val_A
  cmp val_M
  bcc true_label
@cmp_lt_false:
.endmacro

.macro cmp16_eq val_A, val_M, true_label
  lda val_A+1
  cmp val_M+1
  bne @cmp_eq_false
  lda val_A
  cmp val_M
  beq true_label
@cmp_eq_false:
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

