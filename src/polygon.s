; ---------------------------------------------------------------
; polygon.s
; AnotherX16 - Commander X16 port of Another World
; ---------------------------------------------------------------

.macpack longbranch

; X16 and CBM includes
.include "cx16.inc"
.include "cbm_kernal.inc"

; Project includes
.include "main.inc"
.include "bank.inc"
.include "vera.inc"
.include "engine.inc"
.include "resource.inc"
.include "macros.inc"
.include "tasks.inc"
.include "polygon.inc"

.segment "ZEROPAGE"
    poly_ptr:       .res 3
    x1:             .res 2
    y1:             .res 1
    x2:             .res 2
    y2:             .res 1
    dx:             .res 1
    dy:             .res 1
    sx:             .res 1
    sy:             .res 1
    err:            .res 2
    e2:             .res 2
    ztemp:          .res 3
    setup:          .res 1
    resource:       .res 2
    nx:             .res 2
    ny:             .res 2
    num_children:   .res 1
    topleftX:       .res 2
    topleftY:       .res 2
    left_index:     .res 1
    right_index:    .res 1
    num_vertices:   .res 1
    quad_info:      .tag quad_data

.segment "DATA"
    polygon_info:       .tag polygon_data
    edge_table_left:    .res (2*200)
    edge_table_right:   .res (2*200)
    counter:            .res 1
    off:                .res 2

.segment "CODE"

.macro read_polygon_byte
    lda poly_ptr
    ldx poly_ptr+1
    ldy poly_ptr+2
    jsr read_byte
    inc24 poly_ptr
.endmacro

; ---------------------------------------------------------------
; Parse polygon info and call draw_polygon
; Preliminaries must be set from opcode call
; ---------------------------------------------------------------
.proc parse_polygon
; debug C942
;stz flag
; lda polygon_info+polygon_data::offset
; cmp #$ea
; bne :+
; lda polygon_info+polygon_data::offset+1
; cmp #$88
; bne :+
; lda #1
; sta flag
; :   ; end debug

    ; get the location in memory for the polygon data
    lda polygon_info+polygon_data::polygons
    sta resource
    lda polygon_info+polygon_data::polygons+1
    sta resource+1
    ldy #resource::pointer
    lda (resource),y
    sta poly_ptr
    iny
    lda (resource),y
    sta poly_ptr+1
    iny
    lda (resource),y
    sta poly_ptr+2
    
    add16_addr poly_ptr, polygon_info+polygon_data::offset     ; poly_ptr = poly_ptr + offset
    lda #$00
    adc poly_ptr+2
    sta poly_ptr+2

    read_polygon_byte
    sta setup

    ; if setup is less than $C0 then it's a group polygon
    lda setup
    cmp #$C0
    jcc group_polygon

    ; *** single polygon ***
    lda polygon_info+polygon_data::color
    bpl @skip_color ; bit #$80
    lda setup
    and #$3F
    sta polygon_info+polygon_data::color       ; color = setup & $3F
    @skip_color:

    ; read polygon width
    read_polygon_byte
    jsr multiply_zoom
    sta polygon_info+polygon_data::width

    read_polygon_byte
    jsr multiply_zoom
    sta polygon_info+polygon_data::height

    read_polygon_byte
    sta polygon_info+polygon_data::num_vertices
    asl
    sta counter    ; counter = num_vertices * 2
    
    ; *** read the data into the array
    ldx #0
    vertex_loop:
        phx
        read_polygon_byte
        jsr multiply_zoom
        plx
        sta polygon_info+polygon_data::vertices,x ; x value
        inx
        phx
        read_polygon_byte
        jsr multiply_zoom
        plx
        sta polygon_info+polygon_data::vertices,x ; y value
        inx
        cpx counter
        jne vertex_loop

    jmp draw_polygon

    group_polygon:
    lda setup
    and #$3F
    cmp #2
    bne @finish

    jmp parse_polygon_group

    @finish:
    rts
.endproc

; ---------------------------------------------------------------
; Parse group polygon info and call draw_polygon
; Preliminaries must be set
; ---------------------------------------------------------------
.proc parse_polygon_group
    cx = nx
    cy = ny

    read_polygon_byte
    jsr multiply_zoom
    sta nx
    stx nx+1
    sub16_addr polygon_info+polygon_data::center_x, nx ; x_val = x_val - nx

    read_polygon_byte
    jsr multiply_zoom
    sta ny
    stx ny+1
    sub16_addr polygon_info+polygon_data::center_y, ny ; y_val = y_val - ny

    read_polygon_byte
    inc
    sta num_children

    ldx #0
    child_loop:
        phx
        read_polygon_byte
        sta off+1
        read_polygon_byte
        sta off             ; off = read_word

        read_polygon_byte
        jsr multiply_zoom
        sta cx
        stx cx+1

        read_polygon_byte
        jsr multiply_zoom
        sta cy
        stx cy+1

        ; save the polygon_data
        lda num_children
        pha
        lda polygon_info+polygon_data::width
        pha
        lda polygon_info+polygon_data::height
        pha        
        lda polygon_info+polygon_data::zoom
        pha
        lda polygon_info+polygon_data::center_x
        pha
        lda polygon_info+polygon_data::center_x+1
        pha
        lda polygon_info+polygon_data::center_y
        pha
        lda polygon_info+polygon_data::center_y+1
        pha
        add16_addr polygon_info+polygon_data::center_x, cx
        add16_addr polygon_info+polygon_data::center_y, cy

        lda #$FF
        sta polygon_info+polygon_data::color
        lda off+1
        bpl skip_color  ; $8000 is the color flag
        read_polygon_byte
        and #$7F
        sta polygon_info+polygon_data::color
        inc24 poly_ptr
        
        skip_color:
        asl16_addr off, 1

        lda off
        sta polygon_info+polygon_data::offset
        lda off+1
        sta polygon_info+polygon_data::offset+1

        ; save the current offset
        lda poly_ptr
        pha
        lda poly_ptr+1
        pha
        lda poly_ptr+2
        pha
lda flag
beq @cont
stp
@cont:
        jsr parse_polygon

        ; restore the offset
        pla
        sta poly_ptr+2
        pla
        sta poly_ptr+1
        pla
        sta poly_ptr

        pla
        sta polygon_info+polygon_data::center_y+1
        pla
        sta polygon_info+polygon_data::center_y
        pla
        sta polygon_info+polygon_data::center_x+1
        pla
        sta polygon_info+polygon_data::center_x
        pla 
        sta polygon_info+polygon_data::zoom
        pla
        sta polygon_info+polygon_data::height
        pla
        sta polygon_info+polygon_data::width
        pla
        sta num_children
        plx
        inx
        cpx num_children
        jne child_loop
    rts
.endproc

; ---------------------------------------------------------------
; Draw a polygon
; Prerequisites: polygon data is set up
; ---------------------------------------------------------------
.proc draw_polygon
    ; *** topleftX = center_x - width/2
    lda polygon_info+polygon_data::width 
    lsr
    sta topleftX    ; topleftX = polygon::width >> 1
    sec
    lda polygon_info+polygon_data::center_x
    sbc topleftX
    sta topleftX
    lda polygon_info+polygon_data::center_x+1
    sbc #0
    sta topleftX+1  ; topleftX = center_x - width/2
    ; *** topleftY = y_val - height/2
    lda polygon_info+polygon_data::height
    lsr
    sta topleftY    ; topleftY = polygon::height >> 1
    sec
    lda polygon_info+polygon_data::center_y
    sbc topleftY
    sta topleftY
    lda polygon_info+polygon_data::center_y+1
    sbc #0
    sta topleftY+1  ; topleftXY = y_val - height/2

    ; ; *** if num vertices is 4 and height < 2
    ; lda polygon_info+polygon_data::num_vertices
    ; cmp #4
    ; jne start
    ; lda polygon_info+polygon_data::height
    ; cmp #2
    ; jcs start
    ; lda topleftX
    ; sta line_info+line_data::x1
    ; lda topleftX+1
    ; sta line_info+line_data::x1+1
    ; lda topleftX
    ; clc
    ; adc polygon_info+polygon_data::width
    ; sta line_info+line_data::x2
    ; lda topleftX+1
    ; adc #0
    ; sta line_info+line_data::x2+1
    ; lda topleftY
    ; sta line_info+line_data::y1
    ; ; if width is > 1, draw a line
    ; lda polygon_info+polygon_data::width
    ; cmp #1
    ; bcs line
    ; ; else draw a pixel
    ; jmp draw_pixel
    
    ; line:
    ;     jmp draw_line    

    start:
    ; *** init indices
    lda polygon_info+polygon_data::num_vertices
    dec
    asl
    sta left_index ; left_index = num_vertices - 1 * 2; start at the last vertex
    stz right_index ; right_index = 0
    ; vertices are in clockwise order, starting at top right
    ; *** loop through each left and right vertex until the last pairy
    vertex_loop:
        stz quad_info+quad_data::top_left+1
        stz quad_info+quad_data::top_right+1
        stz quad_info+quad_data::bottom_left+1
        stz quad_info+quad_data::bottom_right+1
        stz quad_info+quad_data::top_y+1
        stz quad_info+quad_data::bottom_y+1

        ; top right - 0
        ldy right_index
        lda polygon_info+polygon_data::vertices,y
        sta quad_info+quad_data::top_right

        ; top y 1
        lda polygon_info+polygon_data::vertices+1,y
        sta quad_info+quad_data::top_y

        inc right_index
        inc right_index

        ; bottom right 2
        ldy right_index
        lda polygon_info+polygon_data::vertices,y
        sta quad_info+quad_data::bottom_right

        ; bottom y 3
        lda polygon_info+polygon_data::vertices+1,y
        sta quad_info+quad_data::bottom_y

        ; top left - 6
        ldy left_index 
        lda polygon_info+polygon_data::vertices,y
        sta quad_info+quad_data::top_left

        dec left_index
        dec left_index
        
        ; bottom left 4
        ldy left_index
        lda polygon_info+polygon_data::vertices,y
        sta quad_info+quad_data::bottom_left

        ; draw the quad
        jsr draw_quad

        ; only continue if there's more vertices to process
        lda polygon_info+polygon_data::num_vertices ; number is half length of vertices
        cmp left_index ; if left_index has met the middle, then we're done
        jne vertex_loop

    finish:
    rts
.endproc

; ---------------------------------------------------------------
; multiply value in A by zoom factor and then shifts right by 6
; returns result in A (low byte) and X (high byte)
; ---------------------------------------------------------------
.proc multiply_zoom
    result = ztemp
    zoom = ztemp+1
    
    ldx polygon_info+polygon_data::zoom
    cpx #64
    beq no_zoom     ; Skip multiplication if zoom factor is 64

    ; Multiplication
    sta result      
    stx zoom        ; Store zoom factor
    mulx_addr result, zoom
    stx result+1

    ; Division by 64
    lsr result+1    ; 5
    ror             ; 2  
    lsr result+1    ; 5
    ror             ; 2
    lsr result+1    ; 5
    ror             ; 2
    lsr result+1    ; 5
    ror             ; 2
    lsr result+1    ; 5
    ror             ; 2
    lsr result+1    ; 5
    ror             ; 2
    ldx result+1    ; 3
    
    rts

no_zoom:
    ldx #00         ; Set high byte to 0 since no multiplication occurred
    rts
.endproc
