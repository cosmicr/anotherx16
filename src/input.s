; ---------------------------------------------------------------
; input.s  – Commander X16  (Another World)
; ---------------------------------------------------------------

.macpack longbranch

.include "cx16.inc"
.include "input.inc"
.include "engine.inc"

.segment "ZEROPAGE"
    KEY_FLAGS:     .res 1          ; bit‑field (pressed = 1)
    input_mask:    .res 1          ; copy exposed to game code

; ---------------- CODE -----------------------------------------
.segment "CODE"

; ---------------------------------------------------------------------------
; init_input  – clear state
; ---------------------------------------------------------------------------
.proc init_input
        stz KEY_FLAGS
        stz input_mask
        rts
.endproc


; ---------------------------------------------------------------------------
; translate_scan
;  X = PS/2 scan‑code (0‑7F)
;  A = bit mask (RIGHT_MASK …) or 0 if key is ignored
; ---------------------------------------------------------------------------
.proc translate_scan
        cpx #PS2_RIGHT
        beq @right
        cpx #PS2_LEFT
        beq @left
        cpx #PS2_UP
        beq @up
        cpx #PS2_DOWN
        beq @down
        cpx #PS2_SPACE
        beq @action
        cpx #PS2_ENTER
        beq @action
        lda #0
        rts

@right: lda #RIGHT_MASK
        rts
@left:  lda #LEFT_MASK
        rts
@up:    lda #UP_MASK
        rts
@down:  lda #DOWN_MASK
        rts
@action:lda #ACTION_MASK
        rts
.endproc


; ---------------------------------------------------------------------------
; update_keyboard  – IRQ routine (~40 cycles per event)
; ---------------------------------------------------------------------------
.proc update_keyboard
        lda #9                  ; EXTAPI ps2data_raw
        jsr EXTAPI
        beq @exit               ; nothing waiting

        tax                     ; keep raw byte in X
        bmi @break              ; bit 7 set = key release

; ---------- key press ----------
@make:
        jsr translate_scan
        beq @exit               ; untracked key
        ora KEY_FLAGS
        sta KEY_FLAGS
        rts

; ---------- key release ----------
@break:
        txa
        and #$7F                ; clear high bit
        tax
        jsr translate_scan
        beq @exit
        eor #$FF                ; invert mask
        and KEY_FLAGS
        sta KEY_FLAGS
@exit:
        rts
.endproc


; ---------------------------------------------------------------------------
; update_input  – per‑frame mapper
; ---------------------------------------------------------------------------
.proc update_input
; ----- copy flags to public mask -------------------------------------------
        lda KEY_FLAGS
        sta input_mask

; ===== horizontal movement =================================================
        ldx #HERO_POS_LEFT_RIGHT
        stz engine_vars,x
        stz engine_vars+256,x

        lda KEY_FLAGS
        bit #RIGHT_MASK
        beq @chk_left
        ldx #HERO_POS_LEFT_RIGHT
        lda #1
        sta engine_vars,x
        stz engine_vars+256,x
        smb0 input_mask          ; set bit0 (right)

@chk_left:
        lda KEY_FLAGS
        bit #LEFT_MASK
        beq @vertical
        ldx #HERO_POS_LEFT_RIGHT
        lda #$FF
        sta engine_vars,x
        sta engine_vars+256,x
        smb1 input_mask          ; set bit1 (left)

; ===== vertical movement ===================================================
@vertical:
        ldx #HERO_POS_JUMP_DOWN
        stz engine_vars,x
        stz engine_vars+256,x
        ldx #HERO_POS_UP_DOWN
        stz engine_vars,x
        stz engine_vars+256,x

        lda KEY_FLAGS
        bit #DOWN_MASK
        beq @chk_up
        ldx #HERO_POS_JUMP_DOWN
        lda #1
        sta engine_vars,x
        stz engine_vars+256,x
        ldx #HERO_POS_UP_DOWN
        lda #1
        sta engine_vars,x
        stz engine_vars+256,x
        smb2 input_mask          ; set bit2 (down)

@chk_up:
        lda KEY_FLAGS
        bit #UP_MASK
        beq @action
        ldx #HERO_POS_JUMP_DOWN
        lda #$FF
        sta engine_vars,x
        sta engine_vars+256,x
        ldx #HERO_POS_UP_DOWN
        lda #$FF
        sta engine_vars,x
        sta engine_vars+256,x
        smb3 input_mask          ; set bit3 (up)

; ===== action key ==========================================================
@action:
        ldx #HERO_ACTION
        stz engine_vars,x
        stz engine_vars+256,x

        lda KEY_FLAGS
        bit #ACTION_MASK
        beq @store_masks
        ldx #HERO_ACTION
        lda #1
        sta engine_vars,x
        stz engine_vars+256,x
        smb7 input_mask          ; set bit7 (action)

; ===== push masks into engine vars ========================================
@store_masks:
        ldx #HERO_POS_MASK
        lda input_mask
        sta engine_vars,x
        stz engine_vars+256,x

        ldx #HERO_ACTION_POS_MASK
        lda input_mask
        sta engine_vars,x
        stz engine_vars+256,x
        rts
.endproc
