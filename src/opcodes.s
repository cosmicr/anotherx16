; ---------------------------------------------------------------
; opcodes.s
; AnotherX16 - Commander X16 port of Another World
; ---------------------------------------------------------------

.macpack longbranch

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
.include "sample.inc"
.include "text.inc"

.segment "ZEROPAGE"
    end:        .res 1

.segment "RODATA"
    ; lookup table for stack base address - 128 bytes
    stack_base_addresses:
        .repeat 64, i
            .word task_stack + (i * 32) ; each task stack is 32 bytes
        .endrepeat

.segment "CODE"

; ---------------------------------------------------------------
; SETI var, val
; Set immediate value to a variable
; ---------------------------------------------------------------
.proc opcode_00_SETI
    read_script_byte
    sta temp ; 3 cycles

    jsr read_script_word

    ldy temp ; 3 cycles
    sta engine_vars+256,y ; swap endianess ; 5 cycles
    txa ; 2 cycles
    sta engine_vars,y ; 5 cycles
    ; total = 18 cycles
    rts
.endproc

; ---------------------------------------------------------------
; MOV dst, src
; Copy value from src variable to dst variable
; ---------------------------------------------------------------
.proc opcode_01_MOV
    jsr read_script_word
    tay ; src is in x, dst in y
    
    lda engine_vars,x
    sta engine_vars,y
    lda engine_vars+256,x
    sta engine_vars+256,y
    rts
.endproc

; ---------------------------------------------------------------
; ADD dst, src
; Add value from src variable to dst variable
; ---------------------------------------------------------------
.proc opcode_02_ADD
    jsr read_script_word
    tay ; src is in x, dst in y

    clc
    lda engine_vars,x
    adc engine_vars,y
    sta engine_vars,y
    lda engine_vars+256,x
    adc engine_vars+256,y
    sta engine_vars+256,y

    rts
.endproc

; ---------------------------------------------------------------
; ADDC var, val
; Add immediate value to a variable
; ---------------------------------------------------------------
.proc opcode_03_ADDC
    read_script_byte
    sta temp ; var number

    jsr read_script_word
    sta temp+1 ; value high byte

    ldy temp ; var number to Y
    txa ; low byte of value to A
    clc
    adc engine_vars,y
    sta engine_vars,y
    lda temp+1
    adc engine_vars+256,y
    sta engine_vars+256,y
    
    rts
.endproc

; ---------------------------------------------------------------
; CALL addr
; Call a function at the specified address by setting the bytecode_pos
; and saving the current bytecode_pos to the stack as a return address
; ---------------------------------------------------------------
.proc opcode_04_CALL
    ; read the address from the script
    jsr read_script_word
    stx temp
    sta temp+1

    ; get current task number
    ldx state+engine::current_task

    ; get the task stack_pos offset
    lda task_stack_pos,x
    asl ; stack is word-aligned
    tay

    ; increment the stack_pos
    inc task_stack_pos,x

    ; get task stack base address (num * 32) word aligned
    txa
    asl ; multiply by 2 to get the index into the stack_base_addresses
    tax ; x now contains the task number * 2
    lda stack_base_addresses,x ; get the base address of the task stack
    sta temp+2
    lda stack_base_addresses+1,x ; get the base address of the task stack
    sta temp+3

    ; save the current bytecode_pos to the stack
    lda state+engine::bytecode_pos
    sta (temp+2),y
    iny
    lda state+engine::bytecode_pos+1
    sta (temp+2),y

    ; bytecode_pos = addr;
    lda temp
    sta state+engine::bytecode_pos
    lda temp+1
    sta state+engine::bytecode_pos+1

    rts
.endproc

; ---------------------------------------------------------------
; Return from function
; Pop the stack and return to the previous function
; ---------------------------------------------------------------
.proc opcode_05_RET
    ; get current task number
    ldx state+engine::current_task

    ; decrement the stack_pos first
    dec task_stack_pos,x

    ; get the task stack_pos offset
    lda task_stack_pos,x
    asl            ; stack is word-aligned
    tay ; stack index in y
    
    ; x = current task number
    txa
    asl ; word aligned
    tax
    lda stack_base_addresses,x ; get the base address of the task stack
    sta temp
    lda stack_base_addresses+1,x ; get the base address of the task stack
    sta temp+1
    
    ; load bytecode_pos from the stack
    lda (temp),y
    sta state+engine::bytecode_pos
    iny
    lda (temp),y
    sta state+engine::bytecode_pos+1
    
    rts
.endproc

; ---------------------------------------------------------------
; Yield
; Pause the current task and return to the main loop
; ---------------------------------------------------------------
.proc opcode_06_YIELD
    inc state+engine::task_paused ; set pause flag
    rts
.endproc

; ---------------------------------------------------------------
; JMP addr
; Jump to the specified address in the script
; ---------------------------------------------------------------
.proc opcode_07_JMP
    jsr read_script_word
    stx state+engine::bytecode_pos
    sta state+engine::bytecode_pos+1

    rts
.endproc

; ---------------------------------------------------------------
; SETVEC num, addr
; Set a new address vector for the specified task
; task next_pc is set to the new address
; ---------------------------------------------------------------
.proc opcode_08_SETVEC
    ; Read the task number
    read_script_byte
    asl
    sta temp ; pc is word-aligned

    ; Read the address to set
    jsr read_script_word
    sta temp+1
    txa ; A now contains low byte

    ; Set the task next_pc to the new address
    ldy temp ; task number
    sta task_next_pc,y
    lda temp+1
    sta task_next_pc+1,y

    rts
.endproc

; ---------------------------------------------------------------
; DJNZ num, addr
; Decrement the variable and jump if not zero
; ---------------------------------------------------------------
.proc opcode_09_DJNZ
    addr = temp
    ; Read the variable num
    read_script_byte
    sta temp+2

    ; Read the address to jump to
    jsr read_script_word
    sta addr+1
    stx addr

    ; Decrement the variable
    ldx temp+2
    lda engine_vars,x ; if not zero, decrement low byte only
    bne :+
    dec engine_vars+256,x
    : 
    dec engine_vars,x

    ; Check if the result is zero
    lda engine_vars,x
    ora engine_vars+256,x
    beq end ; result is zero, don't jump

    ; If not zero, jump to the specified address
    lda addr
    sta state+engine::bytecode_pos
    lda addr+1
    sta state+engine::bytecode_pos+1

    end:
    rts
.endproc

; ---------------------------------------------------------------
; CJMP cond_type, right(idx), left(val/idx), addr
; Jump based on the condition type and the values of b and a
; cond_type: bit 7 = variable, bit 6 = immediate, bits 0,1,2 = condition type
; ---------------------------------------------------------------
.proc opcode_0A_CJMP
    cond_type = temp
    right_val = work+1
    left_val = work+3
    jump_addr = work+5

    ; Read the condition type from the script
    jsr read_script_word
    sta cond_type

    ; Read the b variable index for the right operand and load its value
    ; index is in X (from word read above)
    lda engine_vars,x
    sta right_val
    lda engine_vars+256,x
    sta right_val+1

    ; Check the condition type to determine how to read the left operand
    lda cond_type
    bit #$80            ; if cond_type & 0x80
    bne read_left_var  ; bit 7 is set - use variable value
    and #$40            ; else if cond_type & 0x40
    bne read_left_word ; bit 6 is set - use immediate value

    ; Read an 8-bit immediate value for the left operand (neither 6 nor 7 bits set)
    read_script_byte
    sta left_val
    stz left_val+1
    bra read_jump_addr

read_left_var:
    ; Read a variable index and load its value for the left operand
    read_script_byte
    tax
    lda engine_vars,x
    sta left_val
    lda engine_vars+256,x
    sta left_val+1
    bra read_jump_addr

read_left_word:
    ; Read a signed 16-bit immediate value for the left operand
    jsr read_script_word
    sta left_val+1
    stx left_val

read_jump_addr:
    ; Read the jump address from the script
    jsr read_script_word
    sta jump_addr+1
    stx jump_addr

    ; Use a jump table based on the lower 3 bits of the condition type
    lda cond_type
    and #$07
    asl                ; multiply by 2 to get the index into the jump table
    tax
    jmp (jump_table, x)

jump_table:
    .word @eq
    .word @ne
    .word @gt
    .word @ge
    .word @lt
    .word @le

@eq:
    ; Jump if right_val == left_val
    lda right_val
    cmp left_val
    jne @end
    lda right_val+1
    cmp left_val+1
    jne @end
    jra @do_jump

@ne:
    ; Jump if right_val != left_val
    lda right_val
    cmp left_val
    jne @do_jump
    lda right_val+1
    cmp left_val+1
    jne @do_jump
    jra @end

@gt:
    ; Jump if right_val > left_val (signed comparison)
    lda right_val+1
    eor left_val+1 ; 0 xor 0 = 0; 1 xor 1 = 0;
    bmi @gt_diff_signs
    lda right_val+1
    cmp left_val+1
    bcc @end
    bne @do_jump
    lda right_val
    cmp left_val
    bcc @end
    beq @end
    bra @do_jump
@gt_diff_signs:
    lda left_val+1
    bmi @do_jump
    bra @end

@ge:
    ; Jump if right_val >= left_val (signed comparison)
    lda right_val+1
    eor left_val+1
    bmi @ge_diff_signs
    lda right_val+1
    cmp left_val+1
    bcc @end
    bne @do_jump
    lda right_val
    cmp left_val
    bcc @end
    bra @do_jump
@ge_diff_signs:
    lda left_val+1
    bmi @do_jump
    bra @end

@lt:
    ; Jump if right_val < left_val (signed comparison)
    lda right_val+1
    eor left_val+1
    bmi @lt_diff_signs
    lda right_val+1
    cmp left_val+1
    bcc @do_jump
    bne @end
    lda right_val
    cmp left_val
    bcs @end
    bra @do_jump
@lt_diff_signs:
    lda right_val+1
    bmi @do_jump
    bra @end

@le:
    ; Jump if right_val <= left_val (signed comparison)
    lda right_val+1
    eor left_val+1
    bmi @le_diff_signs
    lda right_val+1
    cmp left_val+1
    bcc @do_jump
    bne @end
    lda right_val
    cmp left_val
    bcc @do_jump
    beq @do_jump
    bra @end
@le_diff_signs:
    lda right_val+1
    bmi @do_jump
    bra @end

@do_jump:
    lda jump_addr
    sta state+engine::bytecode_pos
    lda jump_addr+1
    sta state+engine::bytecode_pos+1

@end:
    rts
.endproc

; ---------------------------------------------------------------
; SETPAL pal
; ---------------------------------------------------------------
.proc opcode_0B_SETPAL
    jsr read_script_word
    sta state+engine::next_palette
    ; second byte has no effect
    rts
.endproc

; ---------------------------------------------------------------
; CCTRL start, end, state
; ---------------------------------------------------------------
.proc opcode_0C_CCTRL
stp
    ; Read the start task number 
    read_script_byte
    sta temp+1 ; start task

    jsr read_script_word
    sta end ; end task number
    stx temp ; state to set the tasks to
    cpx #2
    bne set_state ; if state is 2, kill tasks

    ; Kill tasks - set next_pc to -2
    ldx temp+1 ; pull the starting task for counter
    kill_loop:
        txa
        asl
        tay ; pc is word-aligned
        cpx end
        beq :+ ; if equal, continue
        bcs kill_end ; if greater than end, stop
        :
        ; set the next_pc value to -2
        lda #$FE
        sta task_next_pc,y
        lda #$FF
        sta task_next_pc+1,y
        inx
        bra kill_loop
    kill_end:
    rts

    set_state:
    ; Set task state - set next_state to temp(state)
    ldx temp+1 ; counter
    state_loop:
        cpx end
        beq :+ ; continue if equal
        bcs state_end ; if greater than end, stop
        :
        lda temp
        sta task_next_state,x
        inx
        bra state_loop
    state_end:
    rts
.endproc

; ---------------------------------------------------------------
; SELECTP num
; ---------------------------------------------------------------
.proc opcode_0D_SELECTP
    read_script_byte
    get_page
    sta state+engine::draw_page
    rts
.endproc

; ---------------------------------------------------------------
; FILLP num, color
; ---------------------------------------------------------------
.proc opcode_0E_FILLP
    jsr read_script_word
    phx ; second byte is color
    get_page
    tax
    set_addr_page
    pla
    jsr clear_page

    rts
.endproc

; ---------------------------------------------------------------
; COPYP src, dst
; ---------------------------------------------------------------
.proc opcode_0F_COPYP
    src = temp
    dst = work+1
    vscroll = state+engine::vscroll
    jsr read_script_word
    sta src         ; store src for vscroll path
    txa
    get_page
    sta dst         ; store dst for comparison
    
    ; Quick check for direct page copy
    lda src
    cmp #$FE ; if src is $FE or $FF, we can do a direct page copy
    bcc conditional_copy
    
    ; Direct page copy
    get_page 
    ldx dst
    jsr copy_page
    rts 

    conditional_copy: 
    ; If bit 7 of src is clear, we clear vscroll
    bmi skip_clear ; if bit 7 is set, we don't clear vscroll
    stz vscroll 
    bra continue ; temporary - remove later
    skip_clear:
stp
    continue:
    ; now do the copy
    and #3 ; remove vscroll bit
    get_page 
    sta src
    cmp dst
    beq done
    ; vscroll should only happen if the amount is > -SCREEN_H or < SCREEN_H
    ; normal copy if vscroll is zero
    ldx dst
    jsr copy_page
    ; vscroll is number of lines to scroll (y offset)
    ; if vscroll is negative, copy from src-vscroll to dst
    ; todo: handle vscroll up
    ; if vscroll is positive, copy to dst+vscroll from src
    ; todo: handle vscroll down

    done:
    rts
.endproc

; ---------------------------------------------------------------
; BLITP num
; ---------------------------------------------------------------
.proc opcode_10_BLITP
    stz engine_vars+$f7
    stz engine_vars+256+$f7
    read_script_byte
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

    jsr read_script_word
    sta num+1
    stx num

    jsr read_script_word
    pha ; x_pos
    phx ; y_pos

    read_script_byte
    pha
    asl
    asl
    asl
    asl
    sta text_col
    pla
    ora text_col
    sta text_col ; color is duplicated in the high nibble

    ; find the string
    lda #<strings
    sta temp
    lda #>strings
    sta temp+1
    loop:
        ldy #0
        lda (temp),y
        cmp num+1
        bne @next
        iny
        lda (temp),y
        cmp num
        beq @found
        @next:
        inc temp
        bne loop
        inc temp+1
        bne loop
        rts ; not found
    @found: ; temp points to the string

    lda temp
    sta text
    lda temp+1
    sta text+1

    inc16 text
    inc16 text

    ; display the string
    ply
    plx
    jsr display_text

    rts
.endproc

; ---------------------------------------------------------------
; SUB var1, var2
; ---------------------------------------------------------------
.proc opcode_13_SUB
    jsr read_script_word
    tay ; dst is in Y
    ; src is in X

    sec
    lda engine_vars,y
    sbc engine_vars,x
    sta engine_vars,y
    lda engine_vars+256,y
    sbc engine_vars+256,x
    sta engine_vars+256,y

    rts
.endproc

; ---------------------------------------------------------------
; AND var, val
; ---------------------------------------------------------------
.proc opcode_14_AND
    read_script_byte
    pha ; var num dst
    jsr read_script_word
    sta work+1 ; val hi
    stx work ; val lo
    
    ply
    lda engine_vars,y
    and work
    sta engine_vars,y
    lda engine_vars+256,y
    and work+1
    sta engine_vars+256,y

    rts
.endproc

; ---------------------------------------------------------------
; OR var, val
; ---------------------------------------------------------------
.proc opcode_15_OR
    read_script_byte
    pha
    jsr read_script_word
    sta work+1 ; val hi
    stx work ; val lo
    
    ply
    lda engine_vars,y
    ora work
    sta engine_vars,y
    lda engine_vars+256,y
    ora work+1
    sta engine_vars+256,y

    rts
.endproc

; ---------------------------------------------------------------
; SHL var, val
; ---------------------------------------------------------------
.proc opcode_16_SHL
    read_script_byte
    pha
    jsr read_script_word
    sta work+1 ; val hi (gets ignored)
    stx work ; val lo

    lda work
    and #$0F ; no shifting by more than 15
    tay ; shift count
    beq @end

    plx
    @shift_loop:
        asl engine_vars,x
        rol engine_vars+256,x
        dey
        bne @shift_loop

    @end:
    rts
.endproc

; ---------------------------------------------------------------
; SHR var, val
; ---------------------------------------------------------------
.proc opcode_17_SHR
    read_script_byte
    pha
    jsr read_script_word
    sta work+1 ; val hi (gets ignored)
    stx work ; val lo

    lda work
    and #$0F
    tay
    beq @end

    plx
    @shift_loop:
        lda engine_vars+256,x
        cmp #$80 ; sets carry if bit 7 is set
        ror engine_vars+256,x
        ror engine_vars,x
        dey
        bne @shift_loop

    @end:
    rts
.endproc

; ---------------------------------------------------------------
; SOUND num, freq, volume, channel
; ---------------------------------------------------------------
.proc opcode_18_SOUND
    num = temp
    freq = work+2
    volume = work+3
    channel = work+4

    jsr read_script_word
    sta num+1
    stx num

    jsr read_script_word
    sta freq
    stx volume
    lsr volume
    lsr volume ; I think volume is a 6 bit value, so we shift it down to 4 bits
    
    read_script_byte
    asl
    asl
    asl
    asl
    ora volume
    tay ; volume in low and channel in high nibble

    lda num
    ldx freq
    jsr play_sample

    rts
.endproc

; ---------------------------------------------------------------
; LOAD num
; ---------------------------------------------------------------
.proc opcode_19_LOAD
    num = temp
    jsr read_script_word
    sta num+1
    stx num

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
    ; todo: if resource is a bitmap, then load and display it instead
    jsr load_resource

    rts
.endproc

; ---------------------------------------------------------------
; MUSIC num, period, position */
; ---------------------------------------------------------------
.proc opcode_1A_MUSIC
    ; read_script_byte
    ; read_script_byte
    ; ;sta state+engine::music_num
    ; read_script_byte
    ; read_script_byte
    ; ;sta state+engine::music_period
    ; read_script_byte
    ; ;sta state+engine::music_position

    ; todo: music, temporary until implementation:
    jsr read_script_word
    jsr read_script_word
    read_script_byte
    rts
.endproc