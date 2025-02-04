; ---------------------------------------------------------------
; engine.inc
; AnotherX16 - Commander X16 port of Another World
; ---------------------------------------------------------------

.global init_engine, init_game, get_page, set_part
.global state, tasks, part_resources, update_display
.global set_var

.macro set_var index, value
    ldx #index
    lda #<value
    sta state+engine::vars,x
    lda #>value
    sta state+engine::vars+256,x
.endmacro

; todo: consider moving everything to ZP except for vars for speed
.struct engine
    draw_page       .res 1  ; page_ptr0
    display_page    .res 1  ; page_ptr1
    buffer_page     .res 1  ; page_ptr2

    vscroll         .res 1

    next_palette    .res 1

    vars            .res 512

    bytecode_pos    .res 2
    current_task    .res 1
    task_paused     .res 1

    part            .res 1
    next_part       .res 1

    palette         .res 2
    bytecode        .res 2
    polygons1       .res 2
    polygons2       .res 2

.endstruct

