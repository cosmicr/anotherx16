; ---------------------------------------------------------------
; polygon.inc
; AnotherX16 - Commander X16 port of Another World
; ---------------------------------------------------------------

.global parse_polygon
.globalzp polygon_info
.globalzp x1, y1, x2, y2, x3, y3, x4, y4, line_info
.global scale_lookup

PRECISION = 8
MASK = $FFFF << PRECISION ; $FFFF00
LEFT_FRAC = (1 << (PRECISION - 1)) - 1 ; $7F
RIGHT_FRAC = (1 << (PRECISION - 1)) ; $80

.struct polygon_data
    offset          .word
    center_x        .word
    center_y        .word
    zoom            .word
    color           .byte
    width           .word
    height          .word
    num_vertices    .byte
    polygons        .word
.endstruct

.struct line_data
    x1              .word
    x2              .word
    y1              .word
    color           .byte
.endstruct

.struct POINT
    x_val           .word
    y_val           .word
.endstruct

.struct EDGE
    x_val           .word
    dx              .word
    dy              .word
    sx              .word
    error           .word
.endstruct

; #define Scale_Width(x) ((x >> 1) + (x >> 2))
.macro scale_x byte
    lda byte
    lsr
    sta byte
    lsr 
    clc
    adc byte
    sta byte
.endmacro

.macro scale_x16 byte
.scope
    ; x >> 1 (arithmetic shift right)
    lsr byte+1
    bcc no_sign_extension_1
    ora #$80    ; if the high bit was 1, set the new high bit to 1
no_sign_extension_1:
    ror byte    ; x = x >> 1
    ; (x >> 1) + (x >> 2)
    lda byte
    ldx byte+1  ; save (x >> 1) high
    lsr byte+1  ; shift right a 2nd time
    bcc no_sign_extension_2
    ora #$80    ; if the high bit was 1, set the new high bit to 1
no_sign_extension_2:
    ror
    clc
    adc byte    ; add (x >> 1) low
    sta byte
    txa         ; restore (x >> 1) high
    adc byte+1  ; add (x >> 1) high
    sta byte+1
.endscope
.endmacro