; ---------------------------------------------------------------
; engine.s
; AnotherX16 - Commander X16 port of Another World
; ---------------------------------------------------------------

.macpack longbranch

; X16 and CBM includes
.include "cx16.inc"
.include "cbm_kernal.inc"

; Project includes
.include "main.inc"
.include "engine.inc"
.include "vera.inc"
.include "tasks.inc"
.include "bank.inc"
.include "macros.inc"
.include "resource.inc"
.include "opcodes.inc"
.include "polygon.inc"
.include "debug.inc"
.include "input.inc"
.include "sample.inc"

.segment "ZEROPAGE"
    opcode:     .res 1
    current_task:   .res 2
    script_pointer: .res 3

.segment "DATA"
    pc:         .word 0

.segment "RODATA"
    resource_offsets:
    .repeat MAX_RESOURCES, i
        .word resource_table + (i * .sizeof(resource))
    .endrepeat

.segment "CODE"

; Clear Tasks - iterate through tasks and reset values
.align 128
.proc clear_tasks
    ldx #0
    clear_loop:
        stz task_state,x
        stz task_next_state,x
        stz task_stack_pos,x
        lda #$FF
        sta task_pc,x
        sta task_pc+MAX_TASKS,x ; this is ok because all values are the same
        sta task_next_pc,x
        sta task_next_pc+MAX_TASKS,x ; but normally the bytes are next to each other (ie task_pc+1)
        inx
        cpx #MAX_TASKS
        bne clear_loop

    ; Reset first task PC to 0
    stz task_pc
    stz task_pc+1

    ; Clear next_part to -1
    lda #$FF
    sta state+engine::next_part

    rts
.endproc

; Update task states
.align 256
.proc update_tasks
    ldx #0
    update_loop:
        ; state = next_state
        lda task_next_state,x
        sta task_state,x

        ; pc = next_pc
        txa
        asl ; pc is word aligned
        tay
        lda task_next_pc,y
        sta pc
        lda task_next_pc+1,y
        sta pc+1

        ; is pc $FFFF? (-1)
        cmp #$FF
        bne reset_task_pc
        lda pc
        cmp #$FF
        beq next_task  ; if it is, then go to next task (task is paused)

        reset_task_pc:
            ; otherwise is it $FFFE? (-2)
            cmp #$FE
            beq pause_task  ; if it is then set it back to $FFFF (paused)
            ; otherwise it is a valid address, so continue
            lda pc
            sta task_pc,y
            lda pc+1
            sta task_pc+1,y ; task pc = pc
            bra next_offset

        pause_task:
            lda #$FF
            sta task_pc,y
            sta task_pc+1,y     ; task pc = $FFFF (-1)

        next_offset:
            lda #$FF
            sta task_next_pc,y
            sta task_next_pc+1,y ; task next_pc = $FFFF

        ; go to next task
        next_task:
            inx
            cpx #MAX_TASKS
            bne update_loop

    rts
.endproc

; Execute each task/channel
.align 256
.proc execute_tasks
    ldx #0
    execute_loop:
        lda task_state,x
        bne next_task          ; if state is not 0 then skip task

        txa
        asl     ; pc is word aligned
        tay

        lda task_pc,y
        sta pc
        lda task_pc+1,y
        sta pc+1                ; pc = task pc

        lda #$FF
        cmp pc+1
        bne execute
        cmp pc
        beq next_task ; if pc is $FFFF then skip task
        
        execute:
            lda pc
            sta state+engine::bytecode_pos
            lda pc+1
            sta state+engine::bytecode_pos+1    ; bytecode_pos = pc

            stz task_stack_pos,x ; reset task stack_pos

            stx state+engine::current_task      ; current_task = x
            stz state+engine::task_paused       ; task_paused = 0

            phx
            phy
            jsr execute_task                    ; Get opcode and execute task
            ply
            plx

            lda state+engine::bytecode_pos
            sta task_pc,y
            lda state+engine::bytecode_pos+1
            sta task_pc+1,y ; task_pc = bytecode_pos

        next_task:
            inx
            cpx #MAX_TASKS
            bne execute_loop

    rts
.endproc

; ---------------------------------------------------------------
; Iterate and run the tasks
; ---------------------------------------------------------------
.proc run_tasks  
    ; **Load the part if necessary
    lda state+engine::next_part
    cmp #$FF
    beq skip_clear
    jsr set_part
    jsr clear_tasks

    skip_clear:
    jsr update_tasks

    ; **Update input
    jsr update_input

    ; **Execute tasks
    jsr execute_tasks

    ; **Update audio
    jsr update_audio

    rts
.endproc

; ---------------------------------------------------------------
; Read script word
; Returns: A = first byte read, X = second byte read
; Little Endian
; ---------------------------------------------------------------
.align 64
.proc read_script_word
    ; Get first byte
    ldy #resource::pointer
    clc
    lda (state+engine::bytecode),y
    adc state+engine::bytecode_pos
    sta read
    iny
    lda (state+engine::bytecode),y
    adc state+engine::bytecode_pos+1
    sta read+1

    clc 
    lda state+engine::bytecode_pos 
    adc #2 
    sta state+engine::bytecode_pos 
    bcc :+ 
    inc state+engine::bytecode_pos+1 
    : 

    lda read
    ldx read+1
    ldy #0
    jsr read_word

    done:
    rts ; value is in A
.endproc

; ---------------------------------------------------------------
; Execute task
; ---------------------------------------------------------------
.proc execute_task
    while_loop:
        lda state+engine::task_paused ; if task_paused then return
        bne exit

        read_script_byte
        sta opcode
        
        bbr7 opcode, @test_draw_poly ; branch if bit 7 is clear
        jsr opcode_draw_poly_background
        bra while_loop

        @test_draw_poly:
        bbr6 opcode, @execute_opcode ; branch if bit 6 is clear
        jsr opcode_draw_poly_sprite
        bra while_loop

        @execute_opcode:
        jsr opcode_execute
        bra while_loop

    exit:
    rts
.endproc

; ---------------------------------------------------------------
; Draws a single polygon
; ---------------------------------------------------------------
.align 128
.proc opcode_draw_poly_background
    lda opcode
    sta polygon_info+polygon_data::offset+1
    read_script_byte
    sta polygon_info+polygon_data::offset
    asl16_addr polygon_info+polygon_data::offset, 1 ; offset *= 2

    jsr read_script_word
    sta polygon_info+polygon_data::center_x
    stz polygon_info+polygon_data::center_x+1
    stx polygon_info+polygon_data::center_y
    stz polygon_info+polygon_data::center_y+1
    txa

    ; x_val += y_val-199 if y_val > 199
    ; note: this must by 199, because it is hardcoded in the engine
    cmp #199
    bcc @skip
    sec
    sbc #199        ; y_val - 199
    clc
    adc polygon_info+polygon_data::center_x
    sta polygon_info+polygon_data::center_x
    lda #0
    adc polygon_info+polygon_data::center_x+1
    sta polygon_info+polygon_data::center_x+1
    lda #199
    sta polygon_info+polygon_data::center_y
    @skip:

    ; load all the relevant values for parse_polygon
    lda state+engine::polygons1
    sta polygon_info+polygon_data::polygons
    lda state+engine::polygons1+1
    sta polygon_info+polygon_data::polygons+1

    lda #64
    sta polygon_info+polygon_data::zoom
    stz polygon_info+polygon_data::zoom+1
    lda #$FF
    sta polygon_info+polygon_data::color
    jmp parse_polygon

    rts
.endproc

; ---------------------------------------------------------------
; Draws a polygon or a group of polygons
; Assumes opcode is set before calling
; ---------------------------------------------------------------
.align 256
.proc opcode_draw_poly_sprite
    ; Get offset
    jsr read_script_word
    sta polygon_info+polygon_data::offset+1
    stx polygon_info+polygon_data::offset
    asl16_addr polygon_info+polygon_data::offset, 1  ; offset *= 2

    ; Get X-coordinate
    lda opcode          ; Load opcode into A once

    ; Test bit 4 (0x10) Value Type
    bbr4 opcode, bit4_clear ; If bit 4 is clear, execute bit4_clear

    bit4_set:
        bit #$20            ; Test bit 5 (0x20) Position Mode
        beq x_from_var      ; If Z flag is set, bit 5 is 0
        ; Both bits 4 and 5 are set (0b11), execute x_8bit_plus_256
        read_script_byte
        sta polygon_info+polygon_data::center_x
        lda #1
        sta polygon_info+polygon_data::center_x+1
        jra get_y

    x_from_var:
        ; Bit 4 is set, bit 5 is clear (0b01)
        read_script_byte
        tax
        lda engine_vars,x
        sta polygon_info+polygon_data::center_x
        lda engine_vars+256,x
        sta polygon_info+polygon_data::center_x+1
        bra get_y

    bit4_clear:
        bit #$20            ; Test bit 5 (0x20)
        beq x_16bit_immediate ; If Z flag is set, bit 5 is 0
        ; Bit 4 is clear, bit 5 is set (0b10), execute x_8_bit
        read_script_byte
        sta polygon_info+polygon_data::center_x
        lda #0
        sta polygon_info+polygon_data::center_x+1
        bra get_y

    x_16bit_immediate:
        ; Both bits 4 and 5 are clear (0b00)
        jsr read_script_word
        sta polygon_info+polygon_data::center_x+1
        stx polygon_info+polygon_data::center_x

    get_y:
        lda opcode          ; Load opcode into A once

        bit #$08            ; Test bit 3 (0x08)
        bne y_8bit_immediate; If bit 3 is set, execute y_8bit_immediate

        bit #$04            ; Test bit 2 (0x04)
        bne y_from_var      ; If bit 2 is set, execute y_from_var

        ; Both bits 3 and 2 are clear (0b00)
        ; Execute y_16bit_immediate
        jsr read_script_word
        sta polygon_info+polygon_data::center_y+1
        stx polygon_info+polygon_data::center_y
        bra get_zoom

    y_from_var:
        read_script_byte
        tax
        lda engine_vars,x
        sta polygon_info+polygon_data::center_y
        lda engine_vars+256,x
        sta polygon_info+polygon_data::center_y+1
        bra get_zoom

    y_8bit_immediate:
        read_script_byte
        sta polygon_info+polygon_data::center_y
        stz polygon_info+polygon_data::center_y+1
        ; Fall through to get_zoom
    
    get_zoom:
        ; Set default values
        lda #64
        sta polygon_info+polygon_data::zoom
        stz polygon_info+polygon_data::zoom+1
        lda state+engine::polygons1
        sta polygon_info+polygon_data::polygons
        lda state+engine::polygons1+1
        sta polygon_info+polygon_data::polygons+1

        lda opcode
        and #$03            ; Mask to just bits 0-1
        asl                 ; Multiply by 2 to get jump table offset
        tax
        jmp (zoom_table,x)

    zoom_table:
        .addr do_parse        ; 00 - use defaults
        .addr zoom_from_var   ; 01 - zoom from var
        .addr zoom_immediate  ; 10 - immediate zoom
        .addr select_polygon2 ; 11 - use polygons2

        ; If we get here both bits are set (%11==3)
        ; Execute select_polygon2
    select_polygon2:
        lda state+engine::polygons2
        sta polygon_info+polygon_data::polygons
        lda state+engine::polygons2+1
        sta polygon_info+polygon_data::polygons+1
        bra do_parse

    zoom_from_var:
        read_script_byte
        tax
        lda engine_vars,x
        sta polygon_info+polygon_data::zoom
        lda engine_vars+256,x
        sta polygon_info+polygon_data::zoom+1
        bra do_parse

    zoom_immediate:
        read_script_byte
        sta polygon_info+polygon_data::zoom
        stz polygon_info+polygon_data::zoom+1
        ; Fall through to do_parse

    do_parse:
        lda #$FF
        sta polygon_info+polygon_data::color

    jmp parse_polygon

    rts
.endproc

.proc opcode_execute
    asl ; table is word aligned
    tax
    jmp (jump_table, x)
    .align 64
    jump_table:
        .word opcode_00_SETI
        .word opcode_01_MOV
        .word opcode_02_ADD
        .word opcode_03_ADDC
        .word opcode_04_CALL
        .word opcode_05_RET
        .word opcode_06_YIELD
        .word opcode_07_JMP
        .word opcode_08_SETVEC
        .word opcode_09_DJNZ
        .word opcode_0A_CJMP
        .word opcode_0B_SETPAL
        .word opcode_0C_CCTRL
        .word opcode_0D_SELECTP
        .word opcode_0E_FILLP
        .word opcode_0F_COPYP
        .word opcode_10_BLITP
        .word opcode_11_KILL
        .word opcode_12_DTEXT
        .word opcode_13_SUB
        .word opcode_14_AND
        .word opcode_15_OR
        .word opcode_16_SHL
        .word opcode_17_SHR
        .word opcode_18_SOUND
        .word opcode_19_LOAD
        .word opcode_1A_MUSIC
.endproc
