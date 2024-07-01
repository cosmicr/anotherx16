; ---------------------------------------------------------------
; polygon.inc
; AnotherX16 - Commander X16 port of Another World
; ---------------------------------------------------------------

.global parse_polygon
.global polygon_info
.globalzp x1, y1, x2, y2, x3, y3, x4, y4

.struct polygon_data
    offset          .word
    x_val           .word
    y_val           .word
    zoom            .byte
    color           .byte
    width           .byte
    height          .byte
    num_vertices    .byte
    vertices        .res 64
    polygons        .word
.endstruct
    