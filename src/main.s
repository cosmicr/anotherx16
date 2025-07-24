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
.include "input.inc"

.segment "STARTUP"

.segment "INIT"

.segment "ONCE"

; todo: clean up zeropage variables
.segment "ZEROPAGE"
    work:           .res 8
    temp:           .res 10
    read:           .res 6
    mtemp:          .res 2
    frame_early:    .res 1
    sound_enabled:   .res 1 ; 0 = off, 1 = on
    vsync_enabled:   .res 1 ; 0 = off, 1 = on

.segment "CODE"

; ---------------------------------------------------------------
; Main program
; ---------------------------------------------------------------
    ; Unpack all the game data and save to disk
    ; TODO: *** jsr unpack_data
    ; Set video modes and clear screen
    jsr init_vera
    ; Setup keyboard and audio
    jsr init_irq 
    ; Load game resources
    jsr init_resources
    ; Initialize engine state
    jsr init_engine
    ; Initialize the Audio
    jsr init_audio
    ; Clear the keyboard states
    jsr init_input
    ; Initialize tasks and bytecode pointer
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
