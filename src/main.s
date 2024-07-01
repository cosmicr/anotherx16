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

.segment "STARTUP"

.segment "INIT"

.segment "ONCE"

.segment "ZEROPAGE"
    work:   .res 24
    temp:   .res 4
    read:   .res 4

.segment "CODE"

; ---------------------------------------------------------------
; Main program
; ---------------------------------------------------------------
    jsr init_vera
    jsr init_resources
    jsr init_engine
    jsr init_game

    ; GAME LOOP
    @loop:
        jsr run_tasks
        jmp @loop
    
exit:
    jsr RESTOR
    jsr CINT
    rts
; ---------------------------------------------------------------
; End of main program
; ----------------------------------------------------------------


