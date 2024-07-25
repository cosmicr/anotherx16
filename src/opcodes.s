; ---------------------------------------------------------------
; opcodes.s
; AnotherX16 - Commander X16 port of Another World
; ---------------------------------------------------------------

; X16 and CBM includes
.include "cx16.inc"
.include "cbm_kernal.inc"

; Project includes
.include "main.inc"
.include "engine.inc"
.include "tasks.inc"
.include "resource.inc"
.include "bank.inc"
.include "opcodes.inc"
.include "macros.inc"
.include "vera.inc"

; ---------------------------------------------------------------
; SETI var, val
; ---------------------------------------------------------------
.proc opcode_00_SETI
    var = work
    val = work+1

    jsr read_script_byte
    sta var

    jsr read_script_byte
    sta val+1
    jsr read_script_byte
    sta val

    ldy var
    lda val
    sta state+engine::vars,y
    lda val+1
    sta state+engine::vars+256,y
    
    rts
.endproc

; ---------------------------------------------------------------
; MOV dst, src
; ---------------------------------------------------------------
.proc opcode_01_MOV
    jsr read_script_byte
    pha

    jsr read_script_byte
    tay
    plx
    
    lda state+engine::vars,y
    sta state+engine::vars,x
    lda state+engine::vars+256,y
    sta state+engine::vars+256,x

    rts
.endproc

; ---------------------------------------------------------------
; ADD dst, src
; ---------------------------------------------------------------
.proc opcode_02_ADD
    jsr read_script_byte
    pha

    jsr read_script_byte
    tay
    plx
    
    lda state+engine::vars,y
    sta work
    lda state+engine::vars+256,y
    sta work+1

    lda state+engine::vars,x
    clc
    adc work
    sta state+engine::vars,x
    lda state+engine::vars+256,x
    adc work+1
    sta state+engine::vars+256,x

    rts
.endproc

; ---------------------------------------------------------------
; ADDC var, val
; ---------------------------------------------------------------
.proc opcode_03_ADDC
    jsr read_script_byte
    pha

    jsr read_script_byte
    sta work+1
    jsr read_script_byte
    sta work
    
    plx
    lda state+engine::vars,x
    clc
    adc work
    sta state+engine::vars,x
    lda state+engine::vars+256,x
    adc work+1
    sta state+engine::vars+256,x

    rts
.endproc

; ---------------------------------------------------------------
; CALL addr
; ---------------------------------------------------------------
.proc opcode_04_CALL
    addr = work
    current_task = work+2
    stack_pos = work+4
    stack_addr = work+6
    jsr read_script_byte
    sta addr+1
    jsr read_script_byte
    sta addr

    stz current_task+1
    lda state+engine::current_task
    sta current_task

    ; tasks[current_task].stack[tasks[current_task].stack_pos] = bytecode_pos;
    ; get current task
    mulx16 current_task, .sizeof(task)
    add16 current_task, tasks   ; current_task now points to tasks[current_task]

    ; get the stack_pos
    ldy #task::stack_pos
    lda (current_task),y
    asl         ; stack is word-aligned
    sta stack_pos
    
    ; find the stack address
    lda current_task
    clc
    adc #task::stack
    sta stack_addr
    lda current_task+1
    adc #0
    sta stack_addr+1

    ; save bytecode_pos to stack + stack_pos
    lda state+engine::bytecode_pos 
    ldy stack_pos
    sta (stack_addr),y
    lda state+engine::bytecode_pos+1
    iny
    sta (stack_addr),y

    ; tasks[current_task].stack_pos++;
    ldy #task::stack_pos
    lda (current_task),y
    inc
    sta (current_task),y

    ; bytecode_pos = addr;
    lda addr
    sta state+engine::bytecode_pos
    lda addr+1
    sta state+engine::bytecode_pos+1

    rts
.endproc

; ---------------------------------------------------------------
; Return from function
; ---------------------------------------------------------------
.proc opcode_05_RET
    current_task = work
    stack_pos = work+2

    stz current_task+1
    lda state+engine::current_task
    sta current_task

    mulx16 current_task, .sizeof(task)
    add16 current_task, tasks ; current_task now points to tasks[current_task]

    ; tasks[current_task].stack_pos--;
    ldy #task::stack_pos
    lda (current_task),y
    dec
    sta (current_task),y

    ; get the task stack_pos offset
    asl ; stack is word-aligned
    sta stack_pos

    ; current_task = tasks[current_task].stack
    add16 current_task, task::stack

    ldy stack_pos
    lda (current_task),y
    sta state+engine::bytecode_pos
    iny
    lda (current_task),y
    sta state+engine::bytecode_pos+1 ; bytecode_pos = tasks[current_task].stack[tasks[current_task].stack_pos];

    rts
.endproc

; ---------------------------------------------------------------
; Yield
; ---------------------------------------------------------------
.proc opcode_06_YIELD
    lda #1
    sta state+engine::task_paused
    rts
.endproc

; ---------------------------------------------------------------
; JMP addr
; ---------------------------------------------------------------
.proc opcode_07_JMP
    jsr read_script_byte
    sta state+engine::bytecode_pos+1
    jsr read_script_byte
    sta state+engine::bytecode_pos
    rts
.endproc

; ---------------------------------------------------------------
; SETVEC num, addr
; ---------------------------------------------------------------
.proc opcode_08_SETVEC
    task = work
    addr = work+2
    jsr read_script_byte
    sta task
    stz task+1  

    jsr read_script_byte
    sta addr+1  ; addr is big-endian
    jsr read_script_byte
    sta addr

    mulx16 task, .sizeof(task)
    add16 task, tasks ; task now points to tasks[current_task]

    ldy #task::next_pc
    lda addr
    sta (task),y
    iny
    lda addr+1
    sta (task),y

    rts
.endproc

; ---------------------------------------------------------------
; DJNZ num, addr
.proc opcode_09_DJNZ
    num = work
    addr = work+1
    jsr read_script_byte
    sta num

    jsr read_script_byte
    sta addr+1
    jsr read_script_byte
    sta addr

    ; Decrement the variable
    ldx num
    lda state+engine::vars,x
    sec
    sbc #1
    sta state+engine::vars,x
    lda state+engine::vars+256,x
    sbc #0
    sta state+engine::vars+256,x

    ; Check if the result is zero
    lda state+engine::vars,x
    ora state+engine::vars+256,x
    beq @end

    ; If not zero, jump to the specified address
    lda addr
    sta state+engine::bytecode_pos
    lda addr+1
    sta state+engine::bytecode_pos+1

    @end:
    rts
.endproc

; ---------------------------------------------------------------
; CJMP cond_type, b, a, addr
; ---------------------------------------------------------------
.proc opcode_0A_CJMP
    cond_type = work
    right_val = work+1
    left_val = work+3
    jump_addr = work+5
    jmp_table_addr = work+7

    ; Read the condition type from the script
    jsr read_script_byte
    sta cond_type

    ; Read the variable index for the right operand and load its value
    jsr read_script_byte
    tax
    lda state+engine::vars,x
    sta right_val
    stz right_val+1

    ; Check the condition type to determine how to read the left operand
    lda cond_type
    and #$80
    bne @read_left_var
    lda cond_type
    and #$40
    bne @read_left_word
    ; Read an 8-bit immediate value for the left operand
    jsr read_script_byte
    sta left_val
    stz left_val+1
    bra @read_jump_addr

@read_left_var:
    ; Read a variable index and load its value for the left operand
    jsr read_script_byte
    tax
    lda state+engine::vars,x
    sta left_val
    lda state+engine::vars+256,x
    sta left_val+1
    bra @read_jump_addr

@read_left_word:
    ; Read a 16-bit immediate value for the left operand
    jsr read_script_byte
    sta left_val+1
    jsr read_script_byte
    sta left_val

@read_jump_addr:
    ; Read the jump address from the script
    jsr read_script_byte
    sta jump_addr+1
    jsr read_script_byte
    sta jump_addr

    ; Use a jump table based on the lower 3 bits of the condition type
    lda cond_type
    and #$07
    asl
    tax
    lda jump_table,x
    sta jmp_table_addr
    inx
    lda jump_table,x
    sta jmp_table_addr+1
    jmp (jmp_table_addr)

jump_table:
    .word @eq
    .word @ne
    .word @gt
    .word @ge
    .word @lt
    .word @le

@eq:
    ; Jump if right_var == left_value
    lda right_val
    cmp left_val
    bne @end
    lda right_val+1
    cmp left_val+1
    bne @end
    lda jump_addr
    sta state+engine::bytecode_pos
    lda jump_addr+1
    sta state+engine::bytecode_pos+1
    jmp @end

@ne:
    ; Jump if right_var != left_value
    lda right_val
    cmp left_val
    bne @jump
    lda right_val+1
    cmp left_val+1
    beq @end
@jump:
    lda jump_addr
    sta state+engine::bytecode_pos
    lda jump_addr+1
    sta state+engine::bytecode_pos+1
    jmp @end

@gt:
    ; Jump if right_var > left_value
    lda right_val+1
    cmp left_val+1
    bcc @end
    bne @jump
    lda right_val
    cmp left_val
    bcc @end
    beq @end
    jmp @jump

@ge:
    ; Jump if right_var >= left_value
    lda right_val+1
    cmp left_val+1
    bcc @end
    bne @jump
    lda right_val
    cmp left_val
    bcc @end
    jmp @jump

@lt:
    ; Jump if right_var < left_value
    lda right_val+1
    cmp left_val+1
    bcc @jump
    bne @end
    lda right_val
    cmp left_val
    bcs @end
    jmp @jump

@le:
    ; Jump if right_var <= left_value
    lda right_val+1
    cmp left_val+1
    bcc @jump
    bne @end
    lda right_val
    cmp left_val
    beq @jump
    bcs @end
    jmp @jump

@end:
    rts
.endproc

; ---------------------------------------------------------------
; SETPAL pal
; ---------------------------------------------------------------
.proc opcode_0B_SETPAL
    jsr read_script_byte
    sta state+engine::next_palette
    jsr read_script_byte ; does nothing?
    rts
.endproc

; ---------------------------------------------------------------
; CCTRL start, end, state
; ---------------------------------------------------------------
.proc opcode_0C_CCTRL
    start = work
    end = work+1
    state = work+2
    task_ptr = work+3
stp
    jsr read_script_byte
    sta start
    jsr read_script_byte
    sta end
    jsr read_script_byte
    sta state

    lda state
    cmp #2
    bne set_state

    kill_tasks:
    lda start
    sta task_ptr
    kill_loop:
        mulx16 task_ptr, .sizeof(task)
        add16 task_ptr, tasks
        ldy #task::next_pc
        lda #$FD
        sta (task_ptr),y
        iny
        lda #$FF
        sta (task_ptr),y
        inc task_ptr
        lda task_ptr
        cmp end
        bcc kill_loop
    rts

    set_state:
    lda start
    sta task_ptr
    state_loop:
        mulx16 task_ptr, .sizeof(task)
        add16 task_ptr, tasks
        ldy #task::next_state
        lda state
        sta (task_ptr),y
        inc task_ptr
        lda task_ptr
        cmp end
        bcc state_loop
    rts
.endproc

; ---------------------------------------------------------------
; SELECTP num
; ---------------------------------------------------------------
.proc opcode_0D_SELECTP
    jsr read_script_byte
    jsr get_page
    sta state+engine::draw_page
    rts
.endproc

; ---------------------------------------------------------------
; FILLP num, color
; ---------------------------------------------------------------
.proc opcode_0E_FILLP
    jsr read_script_byte    ; num
    jsr get_page
    jsr set_addr_page
    jsr read_script_byte    ; color
    jsr clear_page
    rts
.endproc

; ---------------------------------------------------------------
; COPYP src, dst
; ---------------------------------------------------------------
.proc opcode_0F_COPYP
    src = work
    dst = work+1
    vscroll = state+engine::vscroll
    jsr read_script_byte
    sta src
    jsr read_script_byte
    sta dst

    jsr get_page
    sta dst
    tax ; save dst for copy_page

    ; if (src >= 0xfe)
    lda src
    cmp #$fd ; carry is set when src <= #$fd
    bcc @vscroll_copy
    jsr get_page
    jsr copy_page
    rts

    @vscroll_copy:
    and #$80
    bne @dont_clear_vscroll
    stz vscroll
    @dont_clear_vscroll:
    lda src
    and #3
    jsr get_page
    sta src
    cmp dst
    beq @skip_copy
    lda vscroll
    bne @copy_vscroll
    lda src
    ldx dst
    jsr copy_page
    rts

    @copy_vscroll:
    stp
    brk

    
    @skip_copy:
    rts
.endproc

; ---------------------------------------------------------------
; BLITP num
; ---------------------------------------------------------------
.proc opcode_10_BLITP
stp
    wai ; this will wait at 60hz, but original game was 50hz
    ; todo: wait for refresh 50hz?
    stz state+engine::vars+$f7
    stz state+engine::vars+256+$f7
    jsr read_script_byte
    jsr update_display
    rts
.endproc

; ---------------------------------------------------------------
; KILL
; ---------------------------------------------------------------
.proc opcode_11_KILL
    lda #$FF
    sta state+engine::bytecode_pos
    sta state+engine::bytecode_pos+1
    lda #1
    sta state+engine::task_paused
    rts
.endproc

; ---------------------------------------------------------------
; DTEXT num, x, y, color
; ---------------------------------------------------------------
.proc opcode_12_DTEXT
    num = work
    x_pos = work+2
    y_pos = work+3
    color = work+4

    jsr read_script_byte
    sta num+1
    jsr read_script_byte
    sta num
    jsr read_script_byte
    sta x_pos
    jsr read_script_byte
    sta y_pos
    jsr read_script_byte
    sta color

    ; TODO: Implement display_text function
    ; jsr display_text
    rts
.endproc

; ---------------------------------------------------------------
; SUB var1, var2
; ---------------------------------------------------------------
.proc opcode_13_SUB
    jsr read_script_byte
    pha
    jsr read_script_byte
    tay

    plx
    sec
    lda state+engine::vars,x
    sbc state+engine::vars,y
    sta state+engine::vars,x
    lda state+engine::vars+256,x
    sbc state+engine::vars+256,y
    sta state+engine::vars+256,x

    rts
.endproc

; ---------------------------------------------------------------
; AND var, val
; ---------------------------------------------------------------
.proc opcode_14_AND
    jsr read_script_byte
    pha
    jsr read_script_byte
    sta work+1
    jsr read_script_byte
    sta work
    
    plx
    lda state+engine::vars,x
    and work
    sta state+engine::vars,x
    lda state+engine::vars+256,x
    and work+1
    sta state+engine::vars+256,x

    rts
.endproc

; ---------------------------------------------------------------
; OR var, val
; ---------------------------------------------------------------
.proc opcode_15_OR
    jsr read_script_byte
    pha
    jsr read_script_byte
    sta work+1
    jsr read_script_byte
    sta work

    plx
    lda state+engine::vars,x
    ora work
    sta state+engine::vars,x
    lda state+engine::vars+256,x
    ora work+1
    sta state+engine::vars+256,x

    rts
.endproc

; ---------------------------------------------------------------
; SHL var, val
; ---------------------------------------------------------------
.proc opcode_16_SHL
    jsr read_script_byte
    pha
    jsr read_script_byte
    sta work+1
    jsr read_script_byte
    sta work

    lda work
    and #$0F
    tay
    beq @end

    plx
    @shift_loop:
        asl state+engine::vars,x
        rol state+engine::vars+256,x
        dey
        bne @shift_loop

    @end:
    rts
.endproc

; ---------------------------------------------------------------
; SHR var, val
; ---------------------------------------------------------------
.proc opcode_17_SHR
    jsr read_script_byte
    pha
    jsr read_script_byte
    sta work+1
    jsr read_script_byte
    sta work

    lda work
    and #$0F
    tay
    beq @end

    plx
    @shift_loop:
        lsr state+engine::vars+256,x
        ror state+engine::vars,x
        dey
        bne @shift_loop

    @end:
    rts
.endproc

; ---------------------------------------------------------------
; SOUND num, freq, volume, channel
; ---------------------------------------------------------------
.proc opcode_18_SOUND
    num = work
    freq = work+2
    volume = work+3
    channel = work+4
    jsr read_script_byte
    sta num+1
    jsr read_script_byte
    sta num
    jsr read_script_byte
    sta freq
    jsr read_script_byte
    sta volume
    jsr read_script_byte
    sta channel

    rts
.endproc

; ---------------------------------------------------------------
; LOAD num
; ---------------------------------------------------------------
.proc opcode_19_LOAD
    num = work
    jsr read_script_byte
    sta num+1
    jsr read_script_byte
    sta num
    
    ; if (num>16000) state+engine::next_part = num-16000
    lda num+1
    cmp #$3e
    bcc @load
    lda num
    and #$0F
    sta state+engine::next_part
    rts

    @load:
    lda num
    jsr load_resource

    rts
.endproc

; ---------------------------------------------------------------
; MUSIC num, period, position */
; ---------------------------------------------------------------
.proc opcode_1A_MUSIC
    jsr read_script_byte
    jsr read_script_byte
    ;sta state+engine::music_num
    jsr read_script_byte
    jsr read_script_byte
    ;sta state+engine::music_period
    jsr read_script_byte
    ;sta state+engine::music_position
    rts
.endproc