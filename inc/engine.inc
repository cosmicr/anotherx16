; ---------------------------------------------------------------
; engine.inc
; AnotherX16 - Commander X16 port of Another World
; ---------------------------------------------------------------

.global init_engine, init_game, get_page, set_part
.global engine_vars, part_resources, update_display
.global task_state, task_next_state, task_pc, task_next_pc, task_stack, task_stack_pos
.global set_var
.globalzp state
.globalzp update_required

.macro get_page
.scope
    cmp #$FE
    bcc done ; if page number is less than $FE then return the page number
    beq get_display_page
    
    ; if page number is $FF then return the current buffer_page
    lda state+engine::buffer_page
    bra done
    
    ; if page number is $FE then return the current display_page
    get_display_page:
    lda state+engine::display_page
   
    done:
.endscope
.endmacro

.macro set_var index, value
    ldx #index
    lda #<value
    sta engine_vars,x
    lda #>value
    sta engine_vars+256,x
.endmacro

.struct engine
    draw_page       .res 1  ; page_ptr0
    display_page    .res 1  ; page_ptr1
    buffer_page     .res 1  ; page_ptr2

    next_palette    .res 1

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

