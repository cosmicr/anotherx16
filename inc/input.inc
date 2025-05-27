; ---------------------------------------------------------------
; input.inc
; AnotherX16 - Commander X16 port of Another World
; ---------------------------------------------------------------

; ------ engine vars --------------------------------------------------------
HERO_POS_UP_DOWN      = $E5
SCROLL_Y              = $F9
HERO_ACTION           = $FA
HERO_POS_JUMP_DOWN    = $FB
HERO_POS_LEFT_RIGHT   = $FC
HERO_POS_MASK         = $FD
HERO_ACTION_POS_MASK  = $FE

; ------ SNES controller bit layout (byte 0 in .A) -------------------------
; Note: bits are 0 when pressed, 1 when not pressed
SNES_RIGHT_BIT = 0           ; bit 0
SNES_LEFT_BIT  = 1           ; bit 1
SNES_DOWN_BIT  = 2           ; bit 2
SNES_UP_BIT    = 3           ; bit 3
SNES_START_BIT = 4           ; bit 4 (Enter key)
SNES_SELECT_BIT = 5          ; bit 5 (Left Shift)
SNES_Y_BIT     = 6           ; bit 6 (A key)
SNES_B_BIT     = 7           ; bit 7 (Z key, Left Alt)

; ------ SNES controller bit layout (byte 1 in .X) -------------------------
SNES_A_BIT     = 7           ; bit 7 (X key, Left Ctrl)
SNES_X_BIT     = 6           ; bit 6 (S key)
SNES_L_BIT     = 5           ; bit 5 (D key)
SNES_R_BIT     = 4           ; bit 4 (C key)

; ------ KEY_FLAGS bit layout (our internal format) ------------------------
RIGHT_MASK   = %00000001     ; bit 0
LEFT_MASK    = %00000010     ; bit 1
DOWN_MASK    = %00000100     ; bit 2
UP_MASK      = %00001000     ; bit 3
ACTION_MASK  = %10000000     ; bit 7

; Reserve bits 4-6 for future use (S key, V key, etc.)

.global init_input, update_input