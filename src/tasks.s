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

.segment "ZEROPAGE"
    opcode:     .res 1

.segment "DATA"
    pc:         .res 2

.segment "CODE"

.macro lda_task item
    ldy #item
    lda (current_task),y
.endmacro

; presumes that "current_task" is an address to the task
.macro sta_task item
    ldy #item
    sta (current_task),y
.endmacro

; Clear Tasks - iterate through tasks and reset values
.proc clear_tasks
    current_task = work
    lda #<tasks
    sta current_task
    lda #>tasks
    sta current_task+1 ; get the task base address (first task)
    ldx #0
    clear_task_loop:
        lda #0
        sta_task task::state
        sta_task task::next_state
        lda #$FF
        sta_task task::pc
        sta_task task::pc+1
        sta_task task::next_pc
        sta_task task::next_pc+1
        lda #0
        sta_task task::stack_pos
        add16 current_task, .sizeof(task)
        inx
        cpx #MAX_TASKS
        bne clear_task_loop

    ; Reset first task PC to 0
    stz tasks+task::pc
    stz tasks+task::pc+1    ; task 0 pc = 0
    ; Clear next_part to 0
    stz state+engine::next_part

    rts
.endproc

; Update task states
.proc update_tasks
    current_task = work
    lda #<tasks
    sta current_task
    lda #>tasks
    sta current_task+1
    ldx #0
    update_loop:
        lda_task task::next_state
        sta_task task::state        ; state = next_state
        lda_task task::next_pc
        sta pc
        lda_task task::next_pc+1
        sta pc+1                    ; pc = task::next_pc

        ; is pc $FFFF? (-1)
        cmp #$FF
        bne @reset_task_pc
        lda pc
        cmp #$FF
        beq next_task  ; if it is, then go to next task
    
    @reset_task_pc:
        ; otherwise is it $FFFE? (-2)
        cmp #$FE
        beq pause_task  ; if it is then set it back to $FFFF (paused)
        ; otherwise it is a valid address, so continue
        lda pc
        sta_task task::pc
        lda pc+1
        sta_task task::pc+1     ; task::pc = pc
        bra next_offset
        
        pause_task:
            lda #$FF
            sta_task task::pc
            sta_task task::pc+1     ; task::pc = $FFFF (this line redundant?)

        next_offset:
        lda #$FF
        sta_task task::next_pc
        sta_task task::next_pc+1    ; next_pc = $FFFF

        ; go to next task
        next_task:
            add16 current_task, .sizeof(task)
            inx
            cpx #MAX_TASKS
            bne update_loop

    rts
.endproc

; Execute each task/channel
.proc execute_tasks
    current_task = work
    lda #<tasks
    sta current_task
    lda #>tasks
    sta current_task+1
    ldx #0
    @execute_loop:
        lda_task task::state
        bne @next_task          ; if state is not 0 then skip task

        lda_task task::pc
        sta pc
        lda_task task::pc+1
        sta pc+1                ; pc = task::pc

        lda pc
        cmp #$FF
        bne @execute
        lda pc+1
        cmp #$FF
        beq @next_task ; if pc is $FFFF then skip task
        
        @execute:
            lda pc
            sta state+engine::bytecode_pos
            lda pc+1
            sta state+engine::bytecode_pos+1    ; bytecode_pos = pc

            lda #0
            sta_task task::stack_pos            ; task stack_pos = 0

            txa
            sta state+engine::current_task      ; current_task = x
            stz state+engine::task_paused       ; task_paused = 0

            ; save current_task
            lda current_task
            pha
            lda current_task+1
            pha
            phx
            jsr execute_task                    ; Get opcode and execute task
            plx
            pla
            sta current_task+1
            pla
            sta current_task

            lda state+engine::bytecode_pos
            sta_task task::pc
            lda state+engine::bytecode_pos+1
            sta_task task::pc+1                 ; task::pc = bytecode_pos

        @next_task:
            add16 current_task, .sizeof(task)
            inx
            cpx #MAX_TASKS
            bne @execute_loop

    rts
.endproc

; ---------------------------------------------------------------
; Iterate and run the tasks
; ---------------------------------------------------------------
.proc run_tasks  
    ; **Load the part if necessary
    lda state+engine::next_part
    cmp #0
    jeq @skip_clear
    jsr set_part
    jsr clear_tasks

    @skip_clear:
        jsr update_tasks

    ; TODO: *** UPDATE INPUT ***
    jsr update_input

    ; **Execute tasks
    jsr execute_tasks

    rts
.endproc

; ---------------------------------------------------------------
; Read script byte
; Returns: A = byte read
; ---------------------------------------------------------------
.proc read_script_byte
    table_offset = read
    bank_num = read+2
    pointer = read+3

    ; bytecode resource is 2nd value in parts table
    lda state+engine::part
    asl
    asl ; multiply by 4 ; todo: cache this value for speed
    inc
    tay
    lda part_resources,y    ; get the res number
    tax ; counter
    lda #<resource_table
    sta table_offset
    lda #>resource_table
    sta table_offset+1
    @table_loop:
        add16 table_offset, .sizeof(resource)
        dex
        bne @table_loop

    ldy #resource::rank
    lda (table_offset),y
    sta bank_num

    ; todo: do we need to use all 32 bits of pointer?
    ldy #resource::pointer
    lda (table_offset),y
    sta pointer
    iny
    lda (table_offset),y
    sta pointer+1
    iny
    lda (table_offset),y
    sta pointer+2

    lda pointer
    clc
    adc state+engine::bytecode_pos
    sta pointer
    lda pointer+1
    adc state+engine::bytecode_pos+1
    sta pointer+1       ; pointer = bytecode_pos + pointer 
    lda pointer+2
    adc #$00
    sta pointer+2

    lda pointer
    ldx pointer+1
    ldy pointer+2
    jsr read_byte

    inc16 state+engine::bytecode_pos

    rts ; value is in A
.endproc

; ---------------------------------------------------------------
; Execute task
; ---------------------------------------------------------------
.proc execute_task
    while_loop:
        lda state+engine::task_paused ; if task_paused then return
        bne @exit

; lda state+engine::bytecode_pos+1
; cmp #$0F
; bne :+
; lda state+engine::bytecode_pos
; cmp #$c4
; bne :+
; lda #1
; sta debug_mode
; sta debug_step
; :

        jsr read_script_byte
        sta opcode

        ; debug output
        jsr last_opcode
        
        lda opcode
        bit #$80
        beq @test_draw_poly ; not set
        jsr opcode_draw_poly_background
        bra while_loop

        @test_draw_poly:
        bit #$40
        beq @execute_opcode ; not set
        jsr opcode_draw_poly_sprite
        bra while_loop

        @execute_opcode:

        jsr opcode_execute
        bra while_loop

    @exit:
    rts
.endproc

; ---------------------------------------------------------------
; Draws a single polygon
; ---------------------------------------------------------------
.proc opcode_draw_poly_background
    lda opcode
    sta polygon_info+polygon_data::offset+1
    jsr read_script_byte
    sta polygon_info+polygon_data::offset
    asl16_addr polygon_info+polygon_data::offset, 1 ; offset *= 2

    jsr read_script_byte
    sta polygon_info+polygon_data::center_x
    stz polygon_info+polygon_data::center_x+1
    jsr read_script_byte
    sta polygon_info+polygon_data::center_y
    stz polygon_info+polygon_data::center_y+1

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
    lda #$FF
    sta polygon_info+polygon_data::color
    jsr parse_polygon

    rts
.endproc

; ---------------------------------------------------------------
; Draws a polygon or a group of polygons
; Assumes opcode is set before calling
; ---------------------------------------------------------------
.proc opcode_draw_poly_sprite
    ; Set default values
    lda #64
    sta polygon_info+polygon_data::zoom
    lda state+engine::polygons1
    sta polygon_info+polygon_data::polygons
    lda state+engine::polygons1+1
    sta polygon_info+polygon_data::polygons+1

    ; Get offset
    jsr read_script_byte
    sta polygon_info+polygon_data::offset+1
    jsr read_script_byte
    sta polygon_info+polygon_data::offset
    asl16_addr polygon_info+polygon_data::offset, 1  ; offset *= 2

    ; Get X-coordinate
    lda opcode          ; Load opcode into A once

    bit #$10            ; Test bit 4 (0x10)
    beq bit4_clear      ; If Z flag is set, bit 4 is 0

    bit4_set:
        bit #$20            ; Test bit 5 (0x20)
        beq x_from_var      ; If Z flag is set, bit 5 is 0
        ; Both bits 4 and 5 are set (0b11), execute x_8bit_plus_256
        jsr read_script_byte
        sta polygon_info+polygon_data::center_x
        lda #1
        sta polygon_info+polygon_data::center_x+1
        bra get_y

    x_from_var:
        ; Bit 4 is set, bit 5 is clear (0b01)
        jsr read_script_byte
        tax
        lda state+engine::vars,x
        sta polygon_info+polygon_data::center_x
        lda state+engine::vars+256,x
        sta polygon_info+polygon_data::center_x+1
        bra get_y

    bit4_clear:
        bit #$20            ; Test bit 5 (0x20)
        beq x_16bit_immediate ; If Z flag is set, bit 5 is 0
        ; Bit 4 is clear, bit 5 is set (0b10), execute x_8_bit
        jsr read_script_byte
        sta polygon_info+polygon_data::center_x
        lda #0
        sta polygon_info+polygon_data::center_x+1
        bra get_y

    x_16bit_immediate:
        ; Both bits 4 and 5 are clear (0b00)
        jsr read_script_byte
        sta polygon_info+polygon_data::center_x+1
        jsr read_script_byte
        sta polygon_info+polygon_data::center_x


    get_y:
        lda opcode          ; Load opcode into A once

        bit #$08            ; Test bit 3 (0x08)
        bne y_8bit_immediate; If bit 3 is set, execute y_8bit_immediate

        bit #$04            ; Test bit 2 (0x04)
        bne y_from_var      ; If bit 2 is set, execute y_from_var

        ; Both bits 3 and 2 are clear (0b00)
        ; Execute y_16bit_immediate
        jsr read_script_byte
        sta polygon_info+polygon_data::center_y+1
        jsr read_script_byte
        sta polygon_info+polygon_data::center_y
        bra get_zoom

    y_from_var:
        jsr read_script_byte
        tax
        lda state+engine::vars,x
        sta polygon_info+polygon_data::center_y
        lda state+engine::vars+256,x
        sta polygon_info+polygon_data::center_y+1
        bra get_zoom

    y_8bit_immediate:
        jsr read_script_byte
        sta polygon_info+polygon_data::center_y
        stz polygon_info+polygon_data::center_y+1
        ; Fall through to get_zoom
    
    get_zoom:
        lda opcode
        bit #$03            ; Test bits 0 and 1
        beq do_parse        ; If both bits are zero, do nothing

        bit #$02            ; Test bit 1 (0x02)
        beq zoom_from_var   ; If bit 1 is zero, bits are 01

        bit #$01            ; Test bit 0 (0x01)
        beq zoom_immediate  ; If bit 0 is zero, bits are 10

        ; Both bits 0 and 1 are set (bits == 11)
        ; Execute select_polygon2
    select_polygon2:
        lda state+engine::polygons2
        sta polygon_info+polygon_data::polygons
        lda state+engine::polygons2+1
        sta polygon_info+polygon_data::polygons+1
        bra do_parse

    zoom_from_var:
        jsr read_script_byte
        tax
        lda state+engine::vars,x
        sta polygon_info+polygon_data::zoom
        bra do_parse

    zoom_immediate:
        jsr read_script_byte
        sta polygon_info+polygon_data::zoom
        ; Fall through to do_parse

    do_parse:
        lda #$FF
        sta polygon_info+polygon_data::color

    jsr parse_polygon

    rts
.endproc

.proc opcode_execute
    opcode_addr = work
    .segment "DATA"
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
    
    .segment "CODE"
    asl ; table is word aligned
    tax
    jmp (jump_table, x)
.endproc
