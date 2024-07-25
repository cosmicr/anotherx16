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
    err:            .res 2
    e2:             .res 2
    ztemp:          .res 2
    min_x:          .res 2
    max_x:          .res 2
    setup:          .res 1
    counter:        .res 1
    resource:       .res 2
    nx:             .res 2
    ny:             .res 2
    num_children:   .res 1
    off:            .res 2
    zoom:           .res 1

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

; ---------------------------------------------------------------
; Parse polygon info and call draw_polygon
; Preliminaries must be set from opcode call
; ---------------------------------------------------------------
.proc parse_polygon

; debug 0D50
lda polygon_info+polygon_data::offset
cmp #$3c
bne @keepgoing
lda polygon_info+polygon_data::offset+1
cmp #$0d
bne @keepgoing
inc flag
@keepgoing:
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
    lda setup
    cmp #$C0
    jcc group_polygon

    ; *** single polygon ***
    lda polygon_info+polygon_data::color
    bit #$80
    beq @skip_color
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

    ldx #0
    loop:
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
        jne loop

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
    cx = nx
    cy = ny

    stz nx+1
    stz ny+1

    lda polygon_info+polygon_data::zoom
    sta zoom

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
    sta num_children

    ldx #0
    loop:
        phx
        read_polygon_byte
        sta off+1
        read_polygon_byte
        sta off             ; off = read_word

        stz cx+1
        read_polygon_byte
        jsr multiply_zoom
        sta cx
        stx cx+1

        stz cy+1
        read_polygon_byte
        jsr multiply_zoom
        sta cy
        stx cy+1

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

        lda off+1
        bpl skip_color  ; $8000 is the color flag
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
        sta polygon_info+polygon_data::center_y+1
        pla
        sta polygon_info+polygon_data::center_y
        pla
        sta polygon_info+polygon_data::center_x+1
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
    topleftX = work
    topleftY = work+2
    current_index = work+4
    prev_index = work+5
    num_vertices = work+6
    x_val = work+7
    y_val = work+9
    y_index = work+11

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
    lda polygon_info+polygon_data::center_y+1
    sbc #0
    sta topleftY+1

    ; *** if num vertices is 4 and height < 2
    lda polygon_info+polygon_data::num_vertices
    cmp #4
    jne start
    lda polygon_info+polygon_data::height
    cmp #2
    jcs start
    ; if width is > 0, draw a line
    lda polygon_info+polygon_data::width
    bne line
    ; else draw a pixel
    ldx topleftX
    lda polygon_info+polygon_data::color
    ldy topleftY
    jsr draw_pixel
    jmp finish

    line:
        ; todo: load the vertices into the line_info struct
        lda topleftX
        sta line_info+line_data::x1
        add16_addr topleftX, polygon_info+polygon_data::width
        sta line_info+line_data::x2
        scale_x16 line_info+line_data::x1
        scale_x16 line_info+line_data::x2        
        lda topleftY
        sta line_info+line_data::y1
        jsr draw_line    
        jmp finish

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
        ; *** draw horizontal lines between min_x and max_x for each y
        ldy #0 ; todo: start at min y and finish at max y
        draw_loop:        
            lda edge_table_left,y
            sta min_x
            stz min_x+1
            lda edge_table_right,y
            sta max_x
            stz max_x+1

            ; if min_x = $FF then skip
            lda min_x
            cmp #$FF
            jeq skip
            @continue:

            add16_addr min_x, topleftX
            bpl skip_x1_zero
            stz min_x
            stz min_x+1
            bra set_min_x
            skip_x1_zero:
            scale_x16 min_x
            set_min_x:
                lda min_x
                sta line_info+line_data::x1 ; todo: can be optimised above

            add16_addr max_x, topleftX
            bpl skip_x2_zero
            stz max_x
            stz max_x+1
            bra set_max_x
            skip_x2_zero:
            scale_x16 max_x
            set_max_x:
                lda max_x
                sta line_info+line_data::x2 ; todo: can be optimised above

            ; ora line_info+line_data::x1 ; todo: is this needed?
            ; beq skip    ; if x1 = 0 and x2 = 0, skip

            tya
            clc
            adc topleftY
            sta line_info+line_data::y1
            phy
            jsr draw_line ; todo: needs to take in 16 bit x and y
            ply

            skip:
            iny
            cpy #200
            jne draw_loop
    finish:
    rts
.endproc

; ---------------------------------------------------------------
; Initialize the edge table left with $FF and right with $00
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
    @loop_hi:
        stz edge_table_right,x
        stz edge_table_right+200,x
        inx
        cpx #200
        bne @loop_hi
    rts
.endproc

; ---------------------------------------------------------------
; Rasterize an edge using Bresenham's line algorithm
; ---------------------------------------------------------------
.proc rasterize_edge
; *** TODO: I think we need to calculate before adding topleft values
; *** add center_x and center_y before calling draw_line
    ; Calculate dx = abs(x2 - x1)
        sec
        lda x2
        sbc x1
        bcs dx_positive
        eor #$FF
        inc a
    dx_positive:
        sta dx

        ; Calculate dy = abs(y2 - y1)
        sec
        lda y2
        sbc y1
        bcs dy_positive
        eor #$FF
        inc a
    dy_positive:
        sta dy

        ; Set sx = x1 < x2 ? 1 : -1
        cmp_lt x1, x2, x1_less_than_x2
        lda #$FF
        bra sx_done
    x1_less_than_x2:
        lda #$01
    sx_done:
        sta sx

        ; Set sy = y1 < y2 ? 1 : -1
        cmp_lt y1, y2, y1_less_than_y2
        lda #$FF
        bra sy_done
    y1_less_than_y2:
        lda #$01
    sy_done:
        sta sy

        stz err+1   ; start dx positive
        ; Calculate err = (dx > dy ? dx : -dy) / 2
        cmp_gt dx, dy, dx_greater
        lda dy
        lsr 
        eor #$FF
        inc 
        sta err
        beq end_err
        lda #$FF
        sta err+1
        bra end_err
    dx_greater:
        lsr 
        sta err
    end_err:

    loop:
        ; Call update_edge_table(x1, y1)
        jsr update_edge_table

        ; Check if x1 == x2 and y1 == y2
        lda x1
        cmp x2
        bne continue
        lda y1
        cmp y2
        beq done

    continue:
        ; e2 = err
        lda err
        sta e2
        lda err+1
        sta e2+1

    update_x:
        ; if e2 > -dx
        lda #$FF
        sta ztemp+1
        lda dx
        eor #$FF
        inc a        ; negate dx
        sta ztemp
        ; Sign-extend the negated dx to 16 bits
        bne dx_not_zero
        stz ztemp+1
        dx_not_zero:

        cmp_gt_16s e2, ztemp, e2_greater_than_neg_dx
        bra update_y
    e2_greater_than_neg_dx:
        ; e2 > -dx, so update err and x1
        lda err
        sec
        sbc dy
        sta err
        lda err+1
        sbc #0
        sta err+1    ; err -= dy (16-bit subtraction)

        ; x1 += sx
        lda x1
        clc
        adc sx
        sta x1

    update_y:
        ; if e2 < dy
        lda e2+1
        bmi e2_less_than_dy  ; If e2 is negative, it's definitely < dy
        bne skip_y           ; If e2 high byte > 0, e2 >= dy
        lda e2
        cmp dy
        bcs skip_y

    e2_less_than_dy:
        ; err += dx
        lda err
        clc
        adc dx
        sta err
        lda err+1
        adc #0
        sta err+1

        ; y1 += sy
        lda y1
        clc
        adc sy
        sta y1

    skip_y:
        jmp loop
        
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

    ; if y1 >= 200 return
    cmp_ge y1, #200, done

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

; ---------------------------------------------------------------
; multiply value in A by zoom factor and then shifts right by 6
; returns result in A and X
; ---------------------------------------------------------------
.proc multiply_zoom
    var = ztemp
    zoom = ztemp+1
    result = ztemp
    sta var

    lda polygon_info+polygon_data::zoom
    sta zoom

    ; multiplication
    mulx_addr var, zoom
    sta result
    stx result+1

    ; divide by 64
    ldx #6
    @shift_loop:
        lsr result+1
        ror result
        dex
        bne @shift_loop
    
    lda result
    ldx result+1
    rts
.endproc