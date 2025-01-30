; ---------------------------------------------------------------
; debug.s
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

.segment "DATA"
    debug_mode: .byte 0
    debug_step: .byte 0

.segment "CODE"

; todo: this won't work with layer 1, need to implement using text display routines

; ---------------------------------------------------------------
; Display current bytecode pos and last opcode
; ---------------------------------------------------------------
.proc last_opcode
    lda debug_mode
    beq end

    lda state+engine::bytecode_pos+1
    pha
    lda state+engine::bytecode_pos
    pha
    ; clear screen
    jsr clear_text_screen

    ; convert bycode pos to hexidecimal text string
    dec16 state+engine::bytecode_pos
    lda state+engine::bytecode_pos+1
    jsr hex2text
    lda state+engine::bytecode_pos
    jsr hex2text

    ; restore bytecode pos
    pla
    sta state+engine::bytecode_pos
    pla
    sta state+engine::bytecode_pos+1

    lda #$3A ; ':'
    ldx text_length
    sta text,x
    inc text_length

    ; convert last opcode to string
    lda opcode
    jsr hex2text

    ; display text
    ldx #0
    ldy #0
    jsr display_text

    jsr show_task

    ; if debug_step is set, wait for space
step_loop:
    lda debug_step
    beq end
    jsr GETIN
    cmp #'s'
    bne :+
    stz debug_step
    :
    cmp #' '
    bne step_loop

end:
    rts
.endproc

; ---------------------------------------------------------------
; show current task
; ---------------------------------------------------------------
.segment "DATA"
    task_string: .asciiz "Task:"

.segment "CODE"
.proc show_task
    ; copy string to text buffer
    stz text_length
    ldx #0
    loop:
        lda task_string,x
        sta text,x
        cmp #0
        beq end
        inc text_length
        inx
        bra loop
    end:

    ; convert task id to string
    lda state+engine::current_task
    jsr hex2text

    lda #0
    ldx text_length
    sta text,x

    ; display text
    ldx #1
    ldy #0
    jsr display_text

    rts
.endproc