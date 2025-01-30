; ---------------------------------------------------------------
; main.s
; AnotherX16 - Commander X16 port of Another World
; ---------------------------------------------------------------

; X16 and CBM includes
.include "cx16.inc"
.include "cbm_kernal.inc"

; Project includes
.include "main.inc"
.include "vera.inc"
.include "resource.inc"
.include "engine.inc"
.include "tasks.inc"
.include "macros.inc"
.include "text.inc"
.include "debug.inc"
.include "polygon.inc"
.include "sample.inc"

.segment "STARTUP"

.segment "INIT"

.segment "ONCE"

; todo: clean up zeropage variables
.segment "ZEROPAGE"
    work:   .res 24
    temp:   .res 4
    read:   .res 6
    mtemp:  .res 2
    flag:   .res 1

.segment "DATA"
    frame_counter: .res 2

.segment "CODE"

; ---------------------------------------------------------------
; Main program
; ---------------------------------------------------------------

    ; debugging stuff
    stz frame_counter
    stz frame_counter+1
    stz flag

    ; jsr unpack_data
    jsr init_vera
    jsr init_irq  ; for audio
    jsr init_resources
    jsr init_engine
    jsr init_game

    ; GAME LOOP
    @loop:
        jsr run_tasks
        bra @loop
    
exit:
    jsr remove_irq
    jsr RESTOR
    jsr CINT
    rts
; ---------------------------------------------------------------
; End of main program
; ----------------------------------------------------------------


