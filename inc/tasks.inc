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