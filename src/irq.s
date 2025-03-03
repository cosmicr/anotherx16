; ---------------------------------------------------------------
; sample.s
; AnotherX16 - Commander X16 port of Another World
; ---------------------------------------------------------------

.macpack longbranch

; X16 and CBM includes
.include "cx16.inc"
.include "cbm_kernal.inc"

; Project includes
.include "sample.inc"
.include "main.inc"
.include "macros.inc"
.include "resource.inc"
.include "bank.inc"
.include "input.inc"

.segment "ZEROPAGE"

.segment "DATA"

.segment "BSS"
    old_irq:        .res 2

.segment "RODATA"

.segment "CODE"

; ---------------------------------------------------------------
; Setup Interrupt for AFLOW (Low FIFO buffer)
; ---------------------------------------------------------------
.proc init_irq
    sei               ; Disable interrupts

    ; Save old IRQ vector
    lda IRQVec
    sta old_irq
    lda IRQVec+1
    sta old_irq+1

    ; Install IRQ handler
    lda #<irq_handler
    sta IRQVec
    lda #>irq_handler
    sta IRQVec+1

    cli               ; Enable interrupts

    rts
.endproc

; ---------------------------------------------------------------
; IRQ handler for AFLOW (Low FIFO buffer)
; ---------------------------------------------------------------
; todo: probably need to move this to a separate file at some point
.proc irq_handler
    pha
    phx
    phy

    jsr update_audio
    jsr update_keyboard

    ply
    plx
    pla

    ; Chain to previous IRQ handler
    jmp (old_irq)
.endproc

; ---------------------------------------------------------------
; Remove IRQ handler
; ---------------------------------------------------------------
.proc remove_irq
    sei               ; Disable interrupts

    ; Restore old IRQ vector
    lda old_irq
    sta IRQVec
    lda old_irq+1
    sta IRQVec+1

    ; Make sure AFLOW interrupt is off
    lda VERA::IRQ_EN
    and #%11110111       ; Clear bit 3 (AFLOW interrupt enable)
    sta VERA::IRQ_EN

    cli               ; Enable interrupts

    rts
.endproc