; ---------------------------------------------------------------
; tasks.inc
; AnotherX16 - Commander X16 port of Another World
; ---------------------------------------------------------------

.globalzp opcode
.global run_tasks, read_script_byte, read_script_word

MAX_TASKS = 64

.struct task
    state           .res 1
    next_state      .res 1
    pc              .res 2
    next_pc         .res 2
    stack           .res 32
    stack_pos       .res 1
.endstruct

; ---------------------------------------------------------------
; Read script byte
; Returns: A = byte read
; ---------------------------------------------------------------
.macro read_script_byte
    ; note: do we need to use all 32 bits of pointer?
    ldy #resource::pointer
    clc
    lda (state+engine::bytecode),y
    adc state+engine::bytecode_pos
    pha ; low byte
    iny
    lda (state+engine::bytecode),y
    adc state+engine::bytecode_pos+1
    
    tax ; high byte
    pla ; low byte
    ldy #0
    jsr read_byte

    inc16 state+engine::bytecode_pos

   ; rts ; value is in A
.endmacro