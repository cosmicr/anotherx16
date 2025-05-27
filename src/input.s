; ---------------------------------------------------------------
; input.s
; AnotherX16 - Commander X16 port of Another World
; ---------------------------------------------------------------

.macpack longbranch

.include "cx16.inc"
.include "input.inc"
.include "engine.inc"

.segment "ZEROPAGE"
    key_flags:     .res 1          ; bit‑field (pressed = 1)
.export key_flags

.segment "CODE"

; ---------------------------------------------------------------------------
; Setup Input
; ---------------------------------------------------------------------------
.proc init_input
        stz key_flags
        rts
.endproc

; ---------------------------------------------------------------------------
; Update Joystick - called from update_input (once per frame)
; Reads joystick 0 (keyboard joystick) and updates key_flags
; Note: joystick_scan is automatically called by the default IRQ handler
; ---------------------------------------------------------------------------
.proc update_joystick
    ; Get keyboard joystick state (joystick 0)
    ; No need to call joystick_scan - the IRQ handler does this automatically
    lda #0
    jsr $FF56 ; JOYSTICK_GET
    
    ; .A now contains byte 0 (d-pad + face buttons)
    ; .X now contains byte 1 (shoulder buttons + A/X buttons)
    ; .Y contains presence flag ($00 = present, $FF = not present)
    
    ; Store the joystick data for processing
    sta joy_byte0
    stx joy_byte1
    
    ; Clear our key_flags
    stz key_flags
    
    ; Process directional buttons (bits are 0 when pressed)
    ; Check RIGHT (bit 0 of byte 0)
    lda joy_byte0
    bit #(1 << SNES_RIGHT_BIT)
    bne check_left              ; bit is 1 = not pressed
    lda key_flags
    ora #RIGHT_MASK
    sta key_flags
    
    check_left:
    ; Check LEFT (bit 1 of byte 0)
    lda joy_byte0
    bit #(1 << SNES_LEFT_BIT)
    bne check_down
    lda key_flags
    ora #LEFT_MASK
    sta key_flags
    
    check_down:
    ; Check DOWN (bit 2 of byte 0)
    lda joy_byte0
    bit #(1 << SNES_DOWN_BIT)
    bne check_up
    lda key_flags
    ora #DOWN_MASK
    sta key_flags
    
    check_up:
    ; Check UP (bit 3 of byte 0)
    lda joy_byte0
    bit #(1 << SNES_UP_BIT)
    bne check_action
    lda key_flags
    ora #UP_MASK
    sta key_flags
    
    check_action:
    ; Check action buttons: START (Enter) or A button
    ; START = bit 4 of byte 0
    lda joy_byte0
    bit #(1 << SNES_START_BIT)
    beq action_pressed          ; bit is 0 = pressed
    
    ; A button = bit 7 of byte 1  
    lda joy_byte1
    bit #(1 << SNES_A_BIT)
    bne done                    ; bit is 1 = not pressed
    
    action_pressed:
    lda key_flags
    ora #ACTION_MASK
    sta key_flags
    
    done:
    rts

    ; Local storage for joystick data
    joy_byte0: .res 1
    joy_byte1: .res 1
.endproc

; ---------------------------------------------------------------------------
; Update Input - gets called once per frame
; ---------------------------------------------------------------------------
.proc update_input
    ; First update joystick state
    jsr update_joystick
    
    ; *** horizontal movement
    ldx #HERO_POS_LEFT_RIGHT
    stz engine_vars,x
    stz engine_vars+256,x

    lda key_flags
    bit #RIGHT_MASK
    beq chk_left
    ldx #HERO_POS_LEFT_RIGHT
    lda #1
    sta engine_vars,x

    chk_left:
    lda key_flags
    bit #LEFT_MASK
    beq vertical
    ldx #HERO_POS_LEFT_RIGHT
    lda #$FF
    sta engine_vars,x
    sta engine_vars+256,x

    ; *** vertical movement
    vertical:
    ldx #HERO_POS_JUMP_DOWN
    stz engine_vars,x
    stz engine_vars+256,x
    ldx #HERO_POS_UP_DOWN
    stz engine_vars,x
    stz engine_vars+256,x

    lda key_flags
    bit #DOWN_MASK
    beq chk_up
    ldx #HERO_POS_JUMP_DOWN
    lda #1
    sta engine_vars,x
    ldx #HERO_POS_UP_DOWN
    lda #1
    sta engine_vars,x

    chk_up:
    lda key_flags
    bit #UP_MASK
    beq action
    ldx #HERO_POS_JUMP_DOWN
    lda #$FF
    sta engine_vars,x
    sta engine_vars+256,x
    ldx #HERO_POS_UP_DOWN
    lda #$FF
    sta engine_vars,x
    sta engine_vars+256,x

    ; *** action
    action:
    ldx #HERO_ACTION
    stz engine_vars,x
    stz engine_vars+256,x

    lda key_flags
    bit #ACTION_MASK
    beq store_masks
    ldx #HERO_ACTION
    lda #1
    sta engine_vars,x

    ; *** store masks into engine_vars
    store_masks:
    ldx #HERO_POS_MASK
    lda key_flags
    sta engine_vars,x
    stz engine_vars+256,x

    ldx #HERO_ACTION_POS_MASK
    lda key_flags
    sta engine_vars,x
    stz engine_vars+256,x
    rts
.endproc