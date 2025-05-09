; ---------------------------------------------------------------
; input.inc  –  Commander X16 (Another World)
; ---------------------------------------------------------------

EXTAPI          = $FEAB      ; KERNAL ps2data_raw (function #9)

; ------ engine vars --------------------------------------------------------
HERO_POS_UP_DOWN      = $E5
SCROLL_Y              = $F9
HERO_ACTION           = $FA
HERO_POS_JUMP_DOWN    = $FB
HERO_POS_LEFT_RIGHT   = $FC
HERO_POS_MASK         = $FD
HERO_ACTION_POS_MASK  = $FE

; ------ PS/2 scan‑codes we track (set 2) -----------------------------------
PS2_RIGHT      = $59
PS2_LEFT       = $4F
PS2_UP         = $53
PS2_DOWN       = $54
PS2_SPACE      = $3D
PS2_ENTER      = $2B

; ------ KEY_FLAGS bit layout -----------------------------------------------
RIGHT_MASK   = %00000001     ; bit 0
LEFT_MASK    = %00000010     ; bit 1
DOWN_MASK    = %00000100     ; bit 2
UP_MASK      = %00001000     ; bit 3
ACTION_MASK  = %10000000     ; bit 7  (space / enter)

.global init_input, update_keyboard, update_input
