; ---------------------------------------------------------------
; input.inc
; AnotherX16 - Commander X16 port of Another World
; ---------------------------------------------------------------

.global update_input, update_keyboard, init_input

EXTAPI = $FEAB ; External API for CX16

HERO_POS_UP_DOWN = $E5
SCROLL_Y = $F9
HERO_ACTION = $FA
HERO_POS_JUMP_DOWN = $FB
HERO_POS_LEFT_RIGHT = $FC
HERO_POS_MASK = $FD
HERO_ACTION_POS_MASK = $FE
PAUSE_SLICES = $FF

KEY_RIGHT_INDEX = 0
KEY_LEFT_INDEX = 1
KEY_UP_INDEX = 2
KEY_DOWN_INDEX = 3
KEY_SPACE_INDEX = 4

; X16 keyboard matrix scan codes
PS2_UP    = $53    ; Equivalent to PETSCII $91
PS2_RIGHT = $59    ; Equivalent to PETSCII $1D
PS2_DOWN  = $54    ; Equivalent to PETSCII $11
PS2_LEFT  = $4F    ; Equivalent to PETSCII $9D
PS2_SPACE = $3D    ; Equivalent to PETSCII $20
PS2_ENTER = $5A    ; Keep standard PS/2 code for ENTER