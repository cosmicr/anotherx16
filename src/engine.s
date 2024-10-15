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

.segment "BSS"
    state:   .res .sizeof(engine)
    tasks:   .res .sizeof(task) * MAX_TASKS

.segment "RODATA"
    part_resources:
;           pal, code, poly1, poly2
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
    lda #$FE
    jsr get_page
    sta state+engine::draw_page

    lda #$FF
    sta state+engine::next_palette

    ldx #0
    clear_vars:
        stz state+engine::vars,x
        stz state+engine::vars+256,x
        inx
        bne clear_vars ; loop 256 times

    set_var $BC, $0010
    set_var $C6, $0080
    set_var $F2, 4000 ; some implementations use $6000?
    set_var $DC, 33
    set_var $E4, 20

    stz state+engine::bytecode_pos
    stz state+engine::bytecode_pos+1

    stz state+engine::current_task
    stz state+engine::task_paused

    lda #<tasks
    sta work
    lda #>tasks
    sta work+1  ; set the task list pointer
    ldx #0
    clear_task_loop:
        ldy #0
        @inner_loop:    ; clear the task
            lda #0
            sta (work),y
            iny
            cpy #.sizeof(task)
            bne @inner_loop
        add16 work, .sizeof(task)
        inx
        cpx #MAX_TASKS
        bne clear_task_loop

    stz state+engine::part
    lda #2
    sta state+engine::next_part

    stz text_length ; clear text buffer length

    rts
.endproc

; ---------------------------------------------------------------
; Initialise the game
; ---------------------------------------------------------------
.proc init_game
    stz state+engine::current_task
    stz tasks+task::pc
    stz tasks+task::pc+1
    stz state+engine::bytecode_pos
    stz state+engine::bytecode_pos+1
    lda #1
    sta state+engine::task_paused

    rts
.endproc

; ---------------------------------------------------------------
; Get page number directly or indirectly
; A: page number
; Returns: page number in A
; ---------------------------------------------------------------
.proc get_page
    cmp #$FF
    beq @get_ptr2
    cmp #$FE
    beq @get_ptr1
    ;else:
    cmp #4 ; make sure number is lower than 4
    bcs @error
    rts
    @get_ptr2:
        lda state+engine::buffer_page
        rts
    @get_ptr1:
        lda state+engine::display_page
        rts
    @error:
        jsr CINT
        brk
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
    sta state+engine::part
    asl
    asl ; get the offset into the part_resources table
    tay
    lda part_resources,y
    phy
    jsr load_resource
    ply
    sta state+engine::palette
    stx state+engine::palette+1

    iny
    lda part_resources,y
    phy
    jsr load_resource
    ply
    sta state+engine::bytecode
    stx state+engine::bytecode+1

    iny
    lda part_resources,y
    phy
    jsr load_resource
    ply
    sta state+engine::polygons1
    stx state+engine::polygons1+1

    iny
    lda part_resources,y
    bne @load_polygons2
    stz state+engine::polygons2
    stz state+engine::polygons2+1
    rts
    @load_polygons2:
        jsr load_resource
        sta state+engine::polygons2
        stx state+engine::polygons2+1
    rts
.endproc

; ---------------------------------------------------------------
; Update Display - updates the specified display page
; A: page number
; ---------------------------------------------------------------
.proc update_display
    cmp #$fe
    beq @set_next_palette

    cmp #$ff
    bne @get_page
    ; swap display_page and buffer_page
    lda state+engine::display_page
    sta work
    lda state+engine::buffer_page
    sta state+engine::display_page
    lda work
    sta state+engine::buffer_page ; swap page pointers
    bra @set_next_palette
    
    @get_page:
        jsr get_page
        sta state+engine::display_page

    @set_next_palette:
        lda state+engine::next_palette
        cmp #$ff
        beq @skip_palette ; if next_palette is $FF then skip setting palette
        jsr set_palette
        lda #$ff
        sta state+engine::next_palette

    @skip_palette:
        lda state+engine::display_page
        jsr set_vera_page   ; set the VERA page to the display_page

    rts
.endproc