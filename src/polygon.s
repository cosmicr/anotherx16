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
    poly_ptr:       .res 2
    ptemp:          .res 2
    x1:             .res 2
    y1:             .res 1
    x2:             .res 2
    y2:             .res 1
    dx:             .res 1
    dy:             .res 1
    sx:             .res 1
    sy:             .res 1
    err:            .res 1
    e2:             .res 1
    ztemp:          .res 2
    min_x:          .res 2
    max_x:          .res 2

.segment "DATA"
    polygon_info:       .tag polygon_data
    edge_table_left:    .res (2*200)
    edge_table_right:   .res (2*200)
    line_info:          .tag line_data

.segment "CODE"

.macro read_polygon_byte
    ldy #RESOURCE_BANK_START
    lda poly_ptr
    ldx poly_ptr+1
    jsr read_byte
    inc16 poly_ptr
.endmacro

.macro multiply_zoom var
.scope
    ; Clear the temporary 16-bit accumulator
    lda #0
    sta ztemp
    sta ztemp+1

    ; Load the input into A
    lda var

    ; Load the zoom factor
    ldy polygon_info+polygon_data::zoom
    beq @done  ; If zoom is zero, result is zero

    ; Perform 16-bit multiplication; todo: faster multiply
    @loop:
        lda ztemp
        clc
        adc var  ; Add var to low byte of temp
        sta ztemp
        lda ztemp+1
        adc #0   ; Add carry to high byte of temp
        sta ztemp+1
        dey
        bne @loop

    ; At this point, temp:temp+1 contains var * zoom

    ; Now we need to divide by 64 (shift right 6 times)
    ldx #6
    @shift_loop:
        lsr ztemp+1
        ror ztemp
        dex
        bne @shift_loop

    ; Store the final result back into var
    @done:
    lda ztemp
    sta var
.endscope
.endmacro

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


.macro get_vertex ax, ay
.scope
    lda polygon_info+polygon_data::vertices,y
    clc
    adc topleftX
    sta ax
    lda topleftX+1
    adc #0
    sta ax+1

    iny
    lda polygon_info+polygon_data::vertices,y
    clc
    adc topleftY
    sta ay
.endscope
.endmacro

; ---------------------------------------------------------------
; Parse polygon info and call draw_polygon
; Preliminaries must be set from opcode call
; ---------------------------------------------------------------
.proc parse_polygon
    setup = work
    counter = work+1
    resource = work+2

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

    add16_addr poly_ptr, polygon_info+polygon_data::offset     ; poly_ptr = poly_ptr + offset

    read_polygon_byte
    sta setup

    ; if setup is less than $C0 then it's a group polygon
    and #$C0
    cmp #$C0
    jne group_polygon

    ; *** single polygon ***
    ; Scale zoom ; todo: zoom is meant to be a fixed point number, so you scale after multiplying
    lda polygon_info+polygon_data::zoom
    ; lsr_a 6
    ; sta polygon_info+polygon_data::zoom    ; todo: faster way by using rol*3, and #3

    lda polygon_info+polygon_data::color
    bit #$80
    beq @skip_color
    lda setup
    and #$3F
    sta polygon_info+polygon_data::color       ; color = setup & $3F
    @skip_color:

    read_polygon_byte
    sta ptemp
    multiply_zoom ptemp
    scale_x ptemp
    sta polygon_info+polygon_data::width

    read_polygon_byte
    sta ptemp
    multiply_zoom ptemp
    sta polygon_info+polygon_data::height

    read_polygon_byte
    sta polygon_info+polygon_data::num_vertices
    asl
    sta counter    ; counter = num_vertices * 2

    ldx #0
    loop:
        phx
        read_polygon_byte
        sta ptemp
        multiply_zoom ptemp
        scale_x ptemp
        plx
        sta polygon_info+polygon_data::vertices,x ; x value
        inx
        phx
        read_polygon_byte
        sta ptemp
        multiply_zoom ptemp
        plx
        sta polygon_info+polygon_data::vertices,x ; y value
        inx
        cpx counter
        jne loop

    scale_x16 polygon_info+polygon_data::center_x

    jsr draw_polygon
    rts

    group_polygon:
    lda setup
    and #$3F
    cmp #2
    bne @finish

    jsr parse_polygon_group

    @finish:
    rts
.endproc

; ---------------------------------------------------------------
; Parse group polygon info and call draw_polygon
; Preliminaries must be set
; ---------------------------------------------------------------
.proc parse_polygon_group
    nx = work+4
    ny = work+6
    num_children = work+8
    off = work+9
    zoom = work+23

    stz nx+1
    stz ny+1

    lda polygon_info+polygon_data::zoom ; todo: this isn't right, meant to use zoom properly
    sta zoom
    ; lsr_a 6
    ; sta polygon_info+polygon_data::zoom 

    lda polygon_info+polygon_data::center_x
    read_polygon_byte
    sta nx
    multiply_zoom nx    ; nx = x * zoom
    sub16_addr polygon_info+polygon_data::center_x, nx ; x_val = x_val - nx

    read_polygon_byte
    sta ny
    multiply_zoom ny    ; ny = ny * zoom
    sub16_addr polygon_info+polygon_data::center_y, ny ; y_val = y_val - ny

    read_polygon_byte
    sta num_children

    ldx #0
    loop:
        phx
        read_polygon_byte
        sta off+1
        read_polygon_byte
        sta off             ; off = read_word

        stz nx+1
        read_polygon_byte
        sta nx
        multiply_zoom nx

        stz ny+1
        read_polygon_byte
        sta ny
        multiply_zoom ny

        lda polygon_info+polygon_data::center_x
        pha
        lda polygon_info+polygon_data::center_y
        pha
        add16_addr polygon_info+polygon_data::center_x, nx
        add16_addr polygon_info+polygon_data::center_y, ny

        lda off+1
        bit #$80
        beq skip_color
        read_polygon_byte
        and #$7F
        sta polygon_info+polygon_data::color
        inc16 poly_ptr
        
        skip_color:
        asl16_addr off, 1

        lda off
        sta polygon_info+polygon_data::offset
        lda off+1
        sta polygon_info+polygon_data::offset+1

        lda zoom
        sta polygon_info+polygon_data::zoom

        ; save the current offset
        lda poly_ptr
        pha
        lda poly_ptr+1
        pha

        jsr parse_polygon

        ; restore the offset
        pla
        sta poly_ptr+1
        pla
        sta poly_ptr

        pla
        sta polygon_info+polygon_data::center_y
        pla
        sta polygon_info+polygon_data::center_x
        plx
        inx
        cpx num_children
        jne loop
    rts
.endproc

; ---------------------------------------------------------------
; Draw a polygon
; Prerequisites: polygon data is set up
; ---------------------------------------------------------------
.proc draw_polygon
    ; draw_polygon uses draw_page
    topleftX = work+12
    topleftY = work+14
    i = work+15
    j = work+16
    num_vertices = work+17
    x_val = work+18
    y_val = work+20
    y_index = work+21

    ; *** topleftX = center_x - width/2
    lda polygon_info+polygon_data::width ; width is already scaled
    lsr
    sta topleftX    ; topleftX = polygon::width >> 1
    sec
    lda polygon_info+polygon_data::center_x
    sbc topleftX
    sta topleftX    ; topleftX = center_x - width/2
    lda polygon_info+polygon_data::center_x+1
    sbc #0
    sta topleftX+1
    ; *** topleftY = y_val - height/2
    lda polygon_info+polygon_data::height
    lsr
    sta topleftY    ; topleftY = polygon::height >> 1
    sec
    lda polygon_info+polygon_data::center_y
    sbc topleftY
    sta topleftY    ; topleftXY = y_val - height/2

    ; *** if num vertices is 4 and height < 2
    lda polygon_info+polygon_data::num_vertices
    cmp #4
    bne @start
    lda polygon_info+polygon_data::height
    cmp #2
    bcs @start
    ; if width is > 0, draw a line
    lda polygon_info+polygon_data::width
    bne @line
    ; else draw a pixel
    ldx topleftX
    lda polygon_info+polygon_data::color
    ldy topleftY
;    jsr plot_pixel
    jmp finish

    @line:
        ; todo: load the vertices into the line_info struct
        ;jsr draw_line
        jmp finish

    @start:
        ; *** init edge_table
        jsr init_edge_table

        ; *** rasterize edges using bresenham's line algorithm
        lda polygon_info+polygon_data::num_vertices
        dec
        sta j
        stz i
        raster_loop:
            ; while i < num_vertices
            lda i
            cmp polygon_info+polygon_data::num_vertices
            beq done_raster

            lda j
            asl
            tay
            lda polygon_info+polygon_data::vertices,y
            clc
            adc topleftX
            sta x1
            iny
            lda polygon_info+polygon_data::vertices,y
            clc
            adc topleftY
            sta y1

            lda i
            asl
            tay
            lda polygon_info+polygon_data::vertices,y
            clc
            adc topleftX
            sta x2
            iny
            lda polygon_info+polygon_data::vertices,y
            clc
            adc topleftY
            sta y2

            jsr rasterize_edge
            lda i
            sta j
            inc i
            bra raster_loop

        done_raster:
        ; *** draw horizontal lines between min_x and max_x for each y
        ldy #0   ; todo: this could start at first y coord
        draw_loop:
            lda edge_table_left,y
            sta min_x
            lda edge_table_right,y
            sta max_x

            ; if min_x = $FF then skip
            lda min_x
            cmp #$FF
            beq skip
            @continue:

            lda min_x
            sta line_info+line_data::x1 ; todo: can be optimised above
            lda max_x
            sta line_info+line_data::x2
            tya
            sta line_info+line_data::y1
            phy
            jsr draw_line
            ply

            skip:
            iny
            cpy #200
            jne draw_loop

    finish:
    rts
.endproc

; ---------------------------------------------------------------
; Initialize the edge table left with zeros, and right with $FFFF
; ---------------------------------------------------------------
.proc init_edge_table
    ldx #0
    lda #$FF
    @loop_lo:
        sta edge_table_left,x
        sta edge_table_left+200,x   ; note: remember that each low and high byte of the value is x, x+200
        inx
        cpx #200
        bne @loop_lo

    ldx #0
    lda #0
    @loop_hi:
        sta edge_table_right,x
        sta edge_table_right+200,x
        inx
        cpx #200
        bne @loop_hi
    rts
.endproc

; ---------------------------------------------------------------
; Rasterize an edge using Bresenham's line algorithm
; ---------------------------------------------------------------
.proc rasterize_edge
    stp
    ; *** dx = abs(x2 - x1)
    lda x2
    sec
    sbc x1
    bcs store_dx  ; If the result is positive or zero, store it directly
    eor #$FF
    clc
    adc #1
    store_dx:
        sta dx

    ; *** dy = -abs(y2 - y1)
    lda y2
    sec
    sbc y1
    bcs store_dy  ; If the result is positive or zero, store it directly
    eor #$FF
    clc
    adc #1
    store_dy:
        sta dy
        eor #$FF
        clc
        adc #1
        sta dy

    ; *** sx = 1 if x1 < x2, else -1
    cmp_lt x1, x2, @x1_less_than_x2
    lda #$FF
    sta sx
    bra @done_sx
    @x1_less_than_x2:
        lda #1
        sta sx
    @done_sx:

    ; *** sy = 1 if y1 < y2, else -1
    cmp_lt y1, y2, @y1_less_than_y2
    lda #$FF
    sta sy
    bra @done_sy
    @y1_less_than_y2:
        lda #1
        sta sy
    @done_sy:

    ; *** err = dx +dy
    lda dx
    clc
    adc dy
    sta err

    loop:
        jsr update_edge_table

        ; if x1 == x2 and y1 == y2, break
        lda x1
        cmp x2
        bne @not_done
        lda y1
        cmp y2
        beq done
        @not_done:

        ; e2 = err << 1
        lda err
        asl
        sta e2

        ; if e2 >= dy, err += dy, x1 += sx
        cmp_ge e2, dy, e2_greater_than_dy
        bra skip_e2_dy
        e2_greater_than_dy:
        lda x1
        cmp x2
        beq done
        lda err
        clc
        adc dy
        sta err
        lda x1
        clc
        adc sx
        sta x1
        skip_e2_dy:

        ; if e2 <= dx, err += dx, y1 += sy
        cmp_le e2, dx, e2_less_than_dx
        bra skip_e2_dx
        e2_less_than_dx:
        lda y1
        cmp y2
        beq done
        lda err
        clc
        adc dx
        sta err
        lda y1
        clc
        adc sy
        sta y1
        skip_e2_dx:

        bra loop

    done:
    rts
.endproc

; ---------------------------------------------------------------
; Update the edge table
; ---------------------------------------------------------------
.proc update_edge_table
    ; set min_x and max_x to the y1 index in the edge table
    ldx y1
    lda edge_table_left,x
    sta min_x
    lda edge_table_right,x
    sta max_x

    ; if y1 >= 200 return ; todo: move up for early exit
    cmp_ge y1, #200, done

    ; todo: if x1 < 0 then ptemp = 0?

    ; if min_x > x1 then min_x = x1
    cmp_gt min_x, x1, min_x_greater_than_x1
    bra skip_min_x
    min_x_greater_than_x1:
        lda x1
        sta min_x
    skip_min_x:

    ; if max_x < x1 then max_x = x1
    cmp_lt max_x, x1, max_x_less_than_x1
    bra skip_max_x
    max_x_less_than_x1:
        lda x1
        sta max_x
    skip_max_x:

    ; update edge_table
    lda min_x
    sta edge_table_left,x
    lda max_x
    sta edge_table_right,x

    done:
    rts
.endproc