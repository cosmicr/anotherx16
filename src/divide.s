; ; ---------------------------------------------------------------
; ; divide.s
; ; AnotherX16 - Commander X16 port of Another World
; ; ---------------------------------------------------------------

; .macpack longbranch

; ; X16 and CBM includes
; .include "cx16.inc"
; .include "cbm_kernal.inc"

; ; Project includes
; .include "main.inc"
; .include "divide.inc"
; .include "macros.inc"

; .segment "DATA"
;     dtemp:   .res 16

; .segment "CODE"

; ; ---------------------------------------------------------------
; ; Divide a 16 bit number by an 8 bit number
; ; A: low byte of dividend
; ; X: high byte of dividend
; ; Y: divisor
; ; Returns: A = low byte of quotient, X = high byte of quotient
; ; ---------------------------------------------------------------
; .proc divide
;     dividend = dtemp
;     divisor = dtemp + 2
;     remainder = dtemp + 3  ; Now 2 bytes
;     quotient = dtemp + 5

;     ; Load the dividend (already in A and X)
;     sta dividend
;     stx dividend+1
;     ; Load the divisor (already in Y)
;     sty divisor
;     ; Check for divide by zero
;     cpy #0
;     beq divide_by_zero

;     ; Clear quotient and remainder
;     stz quotient
;     stz quotient+1
;     stz remainder
;     stz remainder+1

;     ldx #16     ; 16 bits in dividend
; @divloop:
;     ; Shift dividend left, high bit goes into remainder
;     asl dividend
;     rol dividend+1
;     rol remainder
;     rol remainder+1

;     ; Subtract divisor from remainder (16-bit subtraction)
;     lda remainder
;     sec
;     sbc divisor
;     sta dtemp    ; Store low byte result temporarily
;     lda remainder+1
;     sbc #0       ; Subtract carry from high byte
;     bcc @no_sub  ; Branch if divisor > remainder

;     ; Store subtraction result as new remainder
;     sta remainder+1
;     lda dtemp
;     sta remainder

;     ; Set low bit in quotient
;     inc quotient
; @no_sub:
;     ; Shift quotient left
;     asl quotient
;     rol quotient+1

;     dex
;     bne @divloop

;     ; Correct for extra shift
;     lsr quotient+1
;     ror quotient

;     ; Load result into A and X
;     lda quotient
;     ldx quotient+1
;     rts

; divide_by_zero:
;     lda #0
;     tax
;     rts
; .endproc

; ; ---------------------------------------------------------------
; ; Divide a 24-bit number by an 8-bit number
; ; A: low byte of dividend
; ; X: middle byte of dividend
; ; Y: high byte of dividend
; ; Stack: divisor (1 byte)
; ; Returns: A = low byte of quotient, X = high byte of quotient
; ; Assumes: divisor is always positive
; ; ---------------------------------------------------------------
; .proc divide24
;     dividend = dtemp
;     divisor = dtemp + 3
;     remainder = dtemp + 4
;     quotient = dividend
;     return = dtemp + 7
;     ptemp = dtemp + 9
;     sign = dtemp + 10

;     ; Clear the quotient
;     stz quotient
;     stz quotient+1

;     ; Get the dividend
;     sta dividend
;     stx dividend+1
;     sty dividend+2

;     ; Save the return address
;     pla
;     sta return
;     pla
;     sta return+1

;     ; Get the 8-bit divisor
;     pla
;     sta divisor

;     ; Check if the dividend is negative (signed division)
;     lda dividend+2
;     bmi @negative_dividend

; @positive_dividend:
;     ; Clear the sign flag and continue
;     stz sign
;     jmp @prepare_division

; @negative_dividend:
;     ; Set the sign flag and convert dividend to positive
;     lda #$FF
;     sta sign
;     lda dividend
;     eor #$FF
;     sta dividend
;     lda dividend+1
;     eor #$FF
;     sta dividend+1
;     lda dividend+2
;     eor #$FF
;     sta dividend+2
;     ; Add 1 to the entire 24-bit number
;     clc
;     lda dividend
;     adc #1
;     sta dividend
;     lda dividend+1
;     adc #0
;     sta dividend+1
;     lda dividend+2
;     adc #0
;     sta dividend+2

; @prepare_division:
;     ; Clear the remainder
;     stz remainder
;     stz remainder+1
;     stz remainder+2

;     ldx #24     ; 24 bits in dividend
; divloop:
;     asl dividend
;     rol dividend+1
;     rol dividend+2
;     rol remainder
;     rol remainder+1
;     rol remainder+2

;     lda remainder
;     sec
;     sbc divisor     ; does divisor fit in remainder?
;     tay             ; save for later
;     lda remainder+1
;     sbc #0
;     sta ptemp
;     lda remainder+2
;     sbc #0
;     bcc no_sub     ; if carry is 0 then divisor doesn't fit

;     sta remainder+2
;     lda ptemp
;     sta remainder+1
;     sty remainder
;     inc16 quotient

; no_sub:
;     dex
;     bne divloop

;     ; Adjust quotient for signed division if needed
;     lda sign
;     beq @done

;     lda quotient
;     eor #$FF
;     sta quotient
;     lda quotient+1
;     eor #$FF
;     sta quotient+1
;     clc
;     lda quotient
;     adc #1
;     sta quotient
;     lda quotient+1
;     adc #0
;     sta quotient+1

; @done:
;     lda return+1
;     pha
;     lda return
;     pha

;     lda quotient
;     ldx quotient+1
;     rts
; .endproc
