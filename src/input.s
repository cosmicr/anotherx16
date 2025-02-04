; ---------------------------------------------------------------
; input.s
; AnotherX16 - Commander X16 port of Another World
; ---------------------------------------------------------------

.macpack longbranch

; X16 and CBM includes
.include "cx16.inc"
.include "cbm_kernal.inc"

; Project includes
.include "main.inc"
.include "macros.inc"
.include "debug.inc"
.include "engine.inc"
.include "tasks.inc"
.include "text.inc"
.include "vera.inc"
.include "input.inc"

.segment "ZEROPAGE"
    input_mask: .res 1

.segment "DATA"

.segment "BSS"
    key_states:    .res 5  ; One byte per key we care about

.segment "CODE"

; ---------------------------------------------------------------
; Initialize input system
; Called during system startup
; ---------------------------------------------------------------
.proc init_input
    ldx #4          ; Initialize 5 key states (0-4)
    @clear_loop:
        stz key_states,x
        dex
        bpl @clear_loop
        stz input_mask
    rts
.endproc

; ---------------------------------------------------------------
; Update keyboard - Called from IRQ
; Handles both key press and release events
; ---------------------------------------------------------------
.proc update_keyboard
    lda #9                 ; ps2data_raw function
    jsr EXTAPI
    beq end                ; No key event

    ; Check if it's a key release (bit 7 set)
    pha                    ; Save scan code
    and #$80               ; Test release bit
    beq @key_press         ; If clear, it's a press
    pla                    ; Restore scan code
    and #$7F               ; Clear release bit
    jsr handle_key_release
    bra end

@key_press:
    pla                    ; Restore scan code
    jsr handle_key_press

end:
    rts
.endproc

; ---------------------------------------------------------------
; Handle key press event
; ---------------------------------------------------------------
.proc handle_key_press
    ; Check each key and set appropriate state
    cmp #PS2_LEFT
    bne @check_right
    lda #1
    sta key_states+3       ; KEY_LEFT_INDEX
    rts

@check_right:
    cmp #PS2_RIGHT
    bne @check_up
    lda #1
    sta key_states+1       ; KEY_RIGHT_INDEX
    rts

@check_up:
    cmp #PS2_UP
    bne @check_down
    lda #1
    sta key_states         ; KEY_UP_INDEX
    rts

@check_down:
    cmp #PS2_DOWN
    bne @check_action
    lda #1
    sta key_states+2       ; KEY_DOWN_INDEX
    rts

@check_action:
    cmp #PS2_SPACE
    beq @set_action
    cmp #PS2_ENTER
    bne @end
@set_action:
    lda #1
    sta key_states+4       ; KEY_ACTION_INDEX
@end:
    rts
.endproc

; ---------------------------------------------------------------
; Handle key release event
; ---------------------------------------------------------------
.proc handle_key_release
    ; Check each key and clear appropriate state
    cmp #PS2_LEFT
    bne @check_right
    stz key_states+3       ; KEY_LEFT_INDEX
    rts

@check_right:
    cmp #PS2_RIGHT
    bne @check_up
    stz key_states+1       ; KEY_RIGHT_INDEX
    rts

@check_up:
    cmp #PS2_UP
    bne @check_down
    stz key_states         ; KEY_UP_INDEX
    rts

@check_down:
    cmp #PS2_DOWN
    bne @check_action
    stz key_states+2       ; KEY_DOWN_INDEX
    rts

@check_action:
    cmp #PS2_SPACE
    beq @clear_action
    cmp #PS2_ENTER
    bne @end
@clear_action:
    stz key_states+4       ; KEY_ACTION_INDEX
@end:
    rts
.endproc

; ---------------------------------------------------------------
; Update input - Called each frame to update game state
; ---------------------------------------------------------------
.proc update_input
    ; Clear input mask
    stz input_mask

    ; Check left/right movement
    ldx #HERO_POS_LEFT_RIGHT
    stz state+engine::vars,x
    stz state+engine::vars+256,x

    lda key_states+1       ; Check RIGHT
    beq @check_left
    ldx #HERO_POS_LEFT_RIGHT
    lda #1
    sta state+engine::vars,x
    stz state+engine::vars+256,x
    smb0 input_mask        ; Set bit 0 for right

@check_left:
    lda key_states+3       ; Check LEFT
    beq @check_vertical
    ldx #HERO_POS_LEFT_RIGHT
    lda #$FF              ; -1
    sta state+engine::vars,x
    sta state+engine::vars+256,x
    smb1 input_mask        ; Set bit 1 for left

    ; Check up/down movement
@check_vertical:
    ldx #HERO_POS_JUMP_DOWN
    stz state+engine::vars,x
    stz state+engine::vars+256,x
    ldx #HERO_POS_UP_DOWN
    stz state+engine::vars,x
    stz state+engine::vars+256,x

    lda key_states+2       ; Check DOWN
    beq @check_up_state
    ldx #HERO_POS_JUMP_DOWN
    lda #1
    sta state+engine::vars,x
    stz state+engine::vars+256,x
    ldx #HERO_POS_UP_DOWN
    lda #1
    sta state+engine::vars,x
    stz state+engine::vars+256,x
    smb2 input_mask        ; Set bit 2 for down
    bra @check_action

@check_up_state:
    lda key_states         ; Check UP
    beq @check_action
    ldx #HERO_POS_JUMP_DOWN
    lda #$FF              ; -1
    sta state+engine::vars,x
    sta state+engine::vars+256,x
    ldx #HERO_POS_UP_DOWN
    lda #$FF              ; -1
    sta state+engine::vars,x
    sta state+engine::vars+256,x
    smb3 input_mask        ; Set bit 3 for up

    ; Check action key
@check_action:
    ldx #HERO_ACTION
    stz state+engine::vars,x
    stz state+engine::vars+256,x
    
    lda key_states+4       ; Check ACTION
    beq @store_masks
    ldx #HERO_ACTION
    lda #1
    sta state+engine::vars,x
    stz state+engine::vars+256,x
    smb7 input_mask        ; Set bit 7 for action

@store_masks:
    ; Store position mask
    ldx #HERO_POS_MASK
    lda input_mask
    sta state+engine::vars,x
    stz state+engine::vars+256,x
    
    ; Store action/position combined mask
    ldx #HERO_ACTION_POS_MASK
    lda input_mask
    sta state+engine::vars,x
    stz state+engine::vars+256,x

    rts
.endproc