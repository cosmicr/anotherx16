; ---------------------------------------------------------------
; engine.s
; AnotherX16 - Commander X16 port of Another World
; ---------------------------------------------------------------

; X16 and CBM includes
.include "cx16.inc"
.include "cbm_kernal.inc"

; Project includes
.include "main.inc"
.include "engine.inc"
.include "vera.inc"
.include "tasks.inc"
.include "resource.inc"
.include "text.inc"
.include "macros.inc"
.include "polygon.inc"
.include "sample.inc"

STARTING_PART = 1

.segment "EXTZP" : zeropage
    state:   .res .sizeof(engine)

.segment "BSS"
    task_state:         .res 1 * MAX_TASKS ; 64 bytes
    task_next_state:    .res 1 * MAX_TASKS ; 64 bytes
    task_pc:            .res 2 * MAX_TASKS ; 128 bytes
    task_next_pc:       .res 2 * MAX_TASKS ; 128 bytes
    task_stack:         .res 32 * MAX_TASKS ; reserves 32 * 64 = 2kb (such a lot of space for a stack)
    task_stack_pos:     .res 1 * MAX_TASKS ; 64 bytes
    engine_vars:  .res 512 ; 256 * 2

.segment "RODATA"
    part_resources:
        ;   pal, code, poly1, poly2
        .byte $14, $15, $16, 0      ; protection screen
        .byte $17, $18, $19, 0      ; introduction
        .byte $1a, $1b, $1c, $11    ; water
        .byte $1d, $1e, $1f, $11    ; jail
        .byte $20, $21, $22, $11    ; citadel
        .byte $23, $24, $25, $11    ; arena
        .byte $26, $27, $28, $11    ; luxury
        .byte $29, $2a, $2b, $11    ; final
        .byte $7d, $7e, $7f, 0      ; password screen

.segment "CODE"

; ---------------------------------------------------------------
; Initialise engine state
; ---------------------------------------------------------------
.proc init_engine
    lda #1
    sta state+engine::buffer_page
    lda #2
    sta state+engine::display_page
    sta state+engine::draw_page ; draw_page = display_page

    lda #$FF
    sta state+engine::next_palette

    ldx #0
    clear_vars:
        stz engine_vars,x
        stz engine_vars+256,x
        inx
        bne clear_vars ; loop 256 times

    set_var $BC, $0010
    set_var $C6, $0080
    set_var $F2, 4000 ; note: some implementations use 6000?
    set_var $DC, 33
    set_var $E4, 20
    set_var $54, %00000001 ; $80 = region (0 = World, 1 = USA) $01 = platform (0 = Amiga, 1 = Other)

    stz state+engine::bytecode_pos
    stz state+engine::bytecode_pos+1

    stz state+engine::current_task
    stz state+engine::task_paused

    ; clear tasks
    ldx #0
    clear_tasks:
        stz task_state,x
        stz task_next_state,x
        stz task_pc,x
        stz task_pc+MAX_TASKS,x
        stz task_next_pc,x
        stz task_next_pc+MAX_TASKS,x
        stz task_stack_pos,x
        inx
        cpx #MAX_TASKS
        bne clear_tasks ; loop 64 times
            
    ; clear the task stacks (loop 8 * 256 times = 2k)
    lda #<task_stack
    sta temp
    lda #>task_stack
    sta temp+1
    ldx #0
    lda #0
    clear_stack_page:
        ldy #0
        clear_stack_byte:
            sta (temp),y
            iny
            bne clear_stack_byte ; loop 256 times
        inc temp+1 ; increment the page
        inx
        cpx #8
        bne clear_stack_page ; loop 64 times

    stz state+engine::part
    lda #STARTING_PART
    sta state+engine::next_part

    rts
.endproc

; ---------------------------------------------------------------
; Initialise the game
; ---------------------------------------------------------------
.proc init_game
    stz state+engine::current_task
    stz task_pc
    stz task_pc+1
    stz state+engine::bytecode_pos
    stz state+engine::bytecode_pos+1
    lda #1
    sta state+engine::task_paused

    rts
.endproc

; ---------------------------------------------------------------
; Set Part - loads the relevant resources for the part
; A: part number
; ---------------------------------------------------------------
.proc set_part
    ldx #RESOURCE_BANK_START
    stx next_bank
    stz next_offset
    stz next_offset+1
    stz next_offset+2
    stz next_offset+3
    
    ; get the offset into the part_resources table
    sta state+engine::part
    asl
    asl 
    tay

    ; Load Pallete
    lda part_resources,y
    phy
    jsr load_resource
    ply
    sta state+engine::palette
    stx state+engine::palette+1
    iny

    ; Load Code
    lda part_resources,y
    phy
    jsr load_resource ; second resource is the code
    ply
    sta state+engine::bytecode
    stx state+engine::bytecode+1
    iny

    ; Load Polygons1
    lda part_resources,y
    phy
    jsr load_resource 
    ply
    sta state+engine::polygons1
    stx state+engine::polygons1+1
    iny

    ; Load Polygons2
    lda part_resources,y
    beq skip_polygons2 ; if no polygons2 then skip loading
    jsr load_resource 
    sta state+engine::polygons2
    stx state+engine::polygons2+1
    rts

    ; skip loading polygons2
    skip_polygons2:
    stz state+engine::polygons2
    stz state+engine::polygons2+1

    rts
.endproc

; ---------------------------------------------------------------
; Update Display - updates the specified display page
; A: page number
; ---------------------------------------------------------------
frame_counter:
    .byte 0,0,0
.export frame_counter
.proc update_display
    inc24 frame_counter
    cmp #$fe
    beq set_next_palette

    cmp #$ff
    bne @get_page
    ; swap display_page and buffer_page
    ldx state+engine::display_page
    lda state+engine::buffer_page
    sta state+engine::display_page
    stx state+engine::buffer_page
    bra set_next_palette
    
    @get_page:
        get_page
        sta state+engine::display_page

    set_next_palette:
        lda state+engine::next_palette
        cmp #$ff
        beq @skip_palette ; if next_palette is $FF then skip setting palette
        jsr set_palette
        lda #$ff
        sta state+engine::next_palette

    @skip_palette:
        lda state+engine::display_page
        jsr set_vera_page   ; set the VERA page to the display_page

        ; 60Hz wait
        lda frame_early ; if interrupt has not been triggered then wait
        beq @no_wait
        wai
        wai ; target 30fps
        
    @no_wait:
        lda #2
        sta frame_early

    rts
.endproc