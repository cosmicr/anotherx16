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
    ztemp:          .res 2
    setup:          .res 1
    resource:       .res 2
    nx:             .res 2
    ny:             .res 2
    num_children:   .res 1
    topleftX:       .res 2
    topleftY:       .res 2
    current_index:  .res 1
    prev_index:     .res 1
    num_vertices:   .res 1
    line_info:      .tag line_data
    ptemp:          .res 2

.segment "BSS"
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
    bcs single_polygon
    ; it's a group polygon
    and #$3F
    cmp #2
    jne finish
    jmp parse_polygon_group

    ; *** single polygon ***
    single_polygon:
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

    finish:
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
    max_y = current_index
;stp
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

        ; fill the cache
        set_dcsel 2
        lda #%00100000   ; set cache write enable
        sta FX_CTRL

        set_dcsel 6      ; set cache write mode
        ldx polygon_info+polygon_data::color
        lda color_lookup_shifted,x
        sta FX_CACHE_L
        sta FX_CACHE_M
        sta FX_CACHE_H
        sta FX_CACHE_U

        set_dcsel 2
        stz FX_CTRL
        set_dcsel 0

    ; *** if num vertices is 4 and height < 2
    lda polygon_info+polygon_data::num_vertices
    cmp #4
    jne start
    lda polygon_info+polygon_data::height
    cmp #2
    jcs start
    lda topleftX
    sta line_info+line_data::x1
    lda topleftX+1
    sta line_info+line_data::x1+1
    lda topleftX
    clc
    adc polygon_info+polygon_data::width
    sta line_info+line_data::x2
    lda topleftX+1
    adc #0
    sta line_info+line_data::x2+1
    lda topleftY
    sta line_info+line_data::y1
    lda topleftY+1
    sta line_info+line_data::y1+1
    ; if width is > 1, draw a line
    lda polygon_info+polygon_data::width
    cmp #1
    bcs line
    ; else draw a pixel
    jmp draw_pixel
    
    line:
        jmp draw_line    

    start:
        ; *** init edge_table
        jsr init_edge_table

        ; *** rasterize edges using bresenham's line algorithm
        lda polygon_info+polygon_data::num_vertices
        dec
        asl
        sta prev_index ; prev_index = num_vertices - 1 * 2; start at the last vertex
        stz current_index ; current_index = 0
        raster_loop:    ; *** vertices are in clockwise order, starting top-right
            ; while current_index < num_vertices
            lda current_index
            lsr
            cmp polygon_info+polygon_data::num_vertices
            beq done_raster

            ldy prev_index
            lda polygon_info+polygon_data::vertices,y
            sta x1
            lda polygon_info+polygon_data::vertices+1,y
            sta y1

            ldy current_index
            lda polygon_info+polygon_data::vertices,y
            sta x2
            lda polygon_info+polygon_data::vertices+1,y
            sta y2

            jsr rasterize_edge

            lda current_index
            sta prev_index
            inc current_index
            inc current_index
            bra raster_loop

        done_raster:
        ; temp test

        ; *** draw horizontal lines between min_x and max_x for each y
        ldy polygon_info+polygon_data::num_vertices ; is half length of vertices
        lda polygon_info+polygon_data::vertices+1,y ; the middle y value is max_y
        sta max_y
        ldy polygon_info+polygon_data::vertices+1 ; first y value is min_y
        ; now start is in Y register, and end is max_y
        tya ; copy Y value to A
        clc
        adc topleftY
        sta line_info+line_data::y1
        lda line_info+line_data::y1+1
        adc topleftY+1
        sta line_info+line_data::y1+1
        draw_loop:        
            lda edge_table_left,y
            sta line_info+line_data::x1
            stz line_info+line_data::x1+1
            lda edge_table_right,y
            sta line_info+line_data::x2
            stz line_info+line_data::x2+1

            ; todo: instead of adding for each line, 
            ;       the edgetable could be 16-bit and add before calculating?
            add16_addr line_info+line_data::x1, topleftX
            bpl x1_positive
            stz line_info+line_data::x1
            stz line_info+line_data::x1+1 ; if x1 < 0, set to 0
            x1_positive:

            add16_addr line_info+line_data::x2, topleftX
            bpl x2_positive
            stz line_info+line_data::x2
            stz line_info+line_data::x2+1 ; if x2 < 0, set to 0
            x2_positive:

            phy
            jsr draw_line 
            ply

            ; TODO: may not need 16-bit Y after all
            inc16 line_info+line_data::y1

            iny
            cpy max_y

            bne draw_loop
    finish:
    rts
.endproc

; ---------------------------------------------------------------
; Initialize the edge table left with $FF and right with $00
; ---------------------------------------------------------------
.proc init_edge_table
    ldx #201
    lda #$FF
    @loop:
        dex
        sta edge_table_left,x
        stz edge_table_right,x
        bne @loop

    rts
.endproc

; ---------------------------------------------------------------
; Rasterize an edge using Bresenham's line algorithm
; ---------------------------------------------------------------
.proc rasterize_edge
    x_inc = sx
    y_inc = sy

    ; *** check for horizontal line
    abs dy, y1, y2
    bne not_horizontal
    ldx x1
    cpx x2
    bcc horizontal_loop
    lda x2
    stx x2
    sta x1
    horizontal_loop:
        jsr update_edge_table
        inc x1
        lda x1
        cmp x2
        bcc horizontal_loop
    rts
    not_horizontal:

    ; *** check for vertical line
    abs dx, x1, x2
    bne not_vertical
    ldx y1
    cpx y2
    bcc vertical_loop
    lda y2
    stx y2
    sta y1
    vertical_loop:
        jsr update_edge_table
        inc y1
        lda y1
        cmp y2
        bcc vertical_loop
        beq vertical_loop
    rts
    not_vertical:

    ; todo: there is one more optimisation here to be had
    ;       instead of setting x_inc and y_inc, do separate code sections that use inc and dec
    ;       probably only a fraction faster
    ;    x_inc = 1 if x2 >= x1 else -1
    lda #1
    ldx x2
    cpx x1
    bcs :+
    lda #$FF
    :
    sta x_inc

    ;    y_inc = 1 if y2 >= y1 else -1
    lda #1
    ldx y2
    cpx y1
    bcs :+
    lda #$FF
    :
    sta y_inc

    cmp_le dx, dy, inc_y

    ; *** dx loop
    ;lda dx
    lsr
    sta err ; err = dx / 2
    dx_loop:
        jsr update_edge_table
        add_addr x1, x_inc
        sub_addr err, dy
        bcs :+
        add_addr y1, y_inc
        add_addr err, dx
        :
        lda x1
        cmp x2
        bne dx_loop
    rts
    
    ; *** dy loop
    inc_y:
    lda dy
    lsr
    sta err ; err = dy / 2
    dy_loop:
        jsr update_edge_table
        add_addr y1, y_inc
        sub_addr err, dx
        bcs :+
        add_addr x1, x_inc
        add_addr err, dy
        :
        lda y1
        cmp y2
        bne dy_loop
    rts
.endproc

; ---------------------------------------------------------------
; Update the edge table
; ---------------------------------------------------------------
.proc update_edge_table
    ldx y1
    cpx #(199) ; hard coded to 200
    bcs done

    lda x1
    cmp edge_table_left,x
    bcs skip_min_x  ; x1 is higher or equal
    sta edge_table_left,x
skip_min_x:

    cmp edge_table_right,x
    bcc done        ; x1 is lower
    sta edge_table_right,x

done:
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
