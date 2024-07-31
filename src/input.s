; ---------------------------------------------------------------
; input.s
; AnotherX16 - Commander X16 port of Another World
; ---------------------------------------------------------------

.macpack longbranch

; X16 and CBM includes
.include "cx16.inc"
.include "cbm_kernal.inc"

; Project includes
.include "main.inc"
.include "macros.inc"
.include "debug.inc"
.include "engine.inc"
.include "tasks.inc"
.include "text.inc"
.include "vera.inc"
.include "input.inc"

.segment "DATA"

.segment "CODE"

; ---------------------------------------------------------------
; Update input
; ---------------------------------------------------------------
.proc update_input  ; todo: should this be an interrupt?
    ldx #0
    jsr CHKIN   ; set input device to keyboard
    jsr GETIN
    cmp #0
    beq end

    cmp #'d'
    beq set_debug
    cmp #'s'
    beq step

    bra end

set_debug:
    lda #1
    eor debug_mode
    sta debug_mode
    bra end

step:
    lda #1
    eor debug_step
    sta debug_step
    bra end

end:
    rts
.endproc