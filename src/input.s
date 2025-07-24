; ---------------------------------------------------------------
; input.s
; AnotherX16 - Commander X16 port of Another World
; ---------------------------------------------------------------

.macpack longbranch

.include "cx16.inc"
.include "cbm_kernal.inc"
.include "input.inc"
.include "engine.inc"
.include "main.inc"
.include "sample.inc"

.segment "ZEROPAGE"
    joy_type:      .byte 1 ; 0 = keyboard joystick, 1 = gamepad joystick
    key_flags:     .res 1          ; bit‑field (pressed = 1)
    joy_byte0: .res 1
    joy_byte1: .res 1

.segment "RODATA"
    dpad_to_keyflags:
        .byte $00, RIGHT_MASK, LEFT_MASK, LEFT_MASK|RIGHT_MASK
        .byte DOWN_MASK, DOWN_MASK|RIGHT_MASK, DOWN_MASK|LEFT_MASK, DOWN_MASK|LEFT_MASK|RIGHT_MASK
        .byte UP_MASK, UP_MASK|RIGHT_MASK, UP_MASK|LEFT_MASK, UP_MASK|LEFT_MASK|RIGHT_MASK
        .byte UP_MASK|DOWN_MASK, UP_MASK|DOWN_MASK|RIGHT_MASK, UP_MASK|DOWN_MASK|LEFT_MASK, UP_MASK|DOWN_MASK|LEFT_MASK|RIGHT_MASK

.segment "CODE"

; ---------------------------------------------------------------------------
; Setup Input
; ---------------------------------------------------------------------------
.proc init_input
    lda joy_type
    jsr $FF56 ; JOYSTICK_GET
    cpy #$00 ; if y is $00, joystick 1 is present
    beq joystick_present
    stz joy_type ; use keyboard joystick

    joystick_present:
    stz key_flags
    rts
.endproc

; ---------------------------------------------------------------------------
; Update Joystick - called from update_input (once per frame)
; Reads joystick 0 (keyboard joystick) and updates key_flags
; Note: joystick_scan is automatically called by the default IRQ handler
; ---------------------------------------------------------------------------
.proc update_joystick
    ; Get joystick state
    lda joy_type
    jsr $FF56 ; JOYSTICK_GET

    ; .A contains byte 0 (d-pad + face buttons)
    ; .X contains byte 1 (shoulder buttons + A/X buttons)
    sta joy_byte0
    stx joy_byte1

    ; Invert joystick bits (0=pressed -> 1=pressed) and mask D-Pad
    lda joy_byte0
    eor #$FF
    and #%00001111 ; Isolate UP, DOWN, LEFT, RIGHT bits
    tax             ; Use as index into lookup table

    ; Get directional flags from the lookup table
    lda dpad_to_keyflags,x
    sta key_flags

    ; Check action buttons: START (bit 4) or A button (bit 7)
    ; Bits are 0 when pressed in original joy_byte0/1
    lda joy_byte0
    bit #(1 << SNES_START_BIT)
    beq action_pressed

    lda joy_byte1
    bit #(1 << SNES_A_BIT)
    bne done

action_pressed:
    lda key_flags
    ora #ACTION_MASK
    sta key_flags

done:
    rts
.endproc

; ---------------------------------------------------------------------------
; Update Input - gets called once per frame
; ---------------------------------------------------------------------------
.proc update_input
    ; First update joystick state
    jsr update_joystick

    ; TODO: check for keypress
    ; C: enter code screen $43
    KEY_C = $43
    ; S: toggle sound $53
    KEY_S = $53
    ; F: toggle frame limiter $46
    KEY_F = $46

    jsr GETIN
    cmp #KEY_C
    bne :+
    lda #8
    sta state+engine::next_part
    rts
    :
    cmp #KEY_S
    bne :+
    lda sound_enabled
    eor #$01
    sta sound_enabled
    sta audio_ready ; if sound is disabled, clear audio ready flag
    rts
    :

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