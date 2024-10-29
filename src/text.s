; ---------------------------------------------------------------
; text.s
; AnotherX16 - Commander X16 port of Another World
; ---------------------------------------------------------------

.macpack longbranch

; X16 and CBM includes
.include "cx16.inc"
.include "cbm_kernal.inc"

; Project includes
.include "main.inc"
.include "macros.inc"
.include "text.inc"
.include "engine.inc"
.include "tasks.inc"

.segment "BSS"
    text:   .res 256
    text_length: .res 1

.segment "CODE"

; ---------------------------------------------------------------
; Display text at text pointer
; X: x position in tiles
; Y: y position in tiles
; ---------------------------------------------------------------
.proc display_text
    jsr PLOT ; set cursor position

    ldx #0
    loop:
        lda text,x
        cmp #0
        beq end
        jsr CHROUT
        inx
        bra loop
    end:
    stz text_length
    rts
.endproc

; ---------------------------------------------------------------
; Convert a byte to a hexidecimal string and put it at the end
; of the text buffer
; A: byte to convert
; ---------------------------------------------------------------
.proc hex2text
    pha
    ; convert byte to hexidecimal string
    lsr
    lsr
    lsr
    lsr
    jsr hex2char
    ldx text_length
    sta text,x
    inc text_length
    
    pla
    jsr hex2char
    ldx text_length
    sta text,x
    inc text_length
    rts
.endproc

; ---------------------------------------------------------------
; Convert a low nibble to a hexidecimal character
; A: byte to convert
; ---------------------------------------------------------------
.proc hex2char
    and #$0F
    cmp #10
    bcc @digit
    adc #6  ; convert to A-F
    @digit:
        adc #'0'
    rts
.endproc