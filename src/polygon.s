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
    line_info:      .tag line_data  ; for storing line data for drawing
    zoom_temp:      .res 2          ; temporary storage for zoom multiplication
    zoom_result:    .res 2          ; result of zoom multiplication
    num_children:   .res 1          ; number of children in a group polygon
    poly_ptr:       .res 3          ; pointer to polygon data in memory when parsing
    resource:       .res 2          ; resource memlist pointer for polygon data   
        
    ; Rasterizer variables
    left_edge:              .res 2
    left_dx:                .res 2
    right_edge:             .res 2
    right_dx:               .res 2
    left_error:             .res 2
    right_error:            .res 2

    draw_mode:            .res 1          ; mode for drawing polygons (0: solid, 1: transparent, 2: copy)

.segment "EXTZP" : zeropage
    polygon_info:   .tag polygon_data   ; for storing polygon data

    ; Rasterizer variables
    left_index:             .res 1
    right_index:            .res 1
    left_index_next:        .res 1
    right_index_next:       .res 1
    left_dx_neg:            .res 1
    right_dx_neg:           .res 1
    y_top:                  .res 2
    y_bottom:               .res 2

.segment "BSS"
    count:                  .res 1
    seg_height:             .res 2
    half_width:             .res 1
    half_height:            .res 1

    origin:                 .tag POINT ; 4 bytes
    polygons_x:     .res 256            ; x values for vertices (16-bit)
    polygons_y:     .res 256            ; y values for vertices (16-bit)
    counter:        .res 1              ; counter for reading vertices
    off:            .res 2              ; offset for group polygon
    setup:          .res 1              ; setup byte for polygon
    nx:             .res 2              ; x value for group polygon
    ny:             .res 2              ; y value for group polygon
    cx:             .res 2              ; x value for child polygon
    cy:             .res 2              ; y value for child polygon

.segment "RODATA"
    ; 80% scale lookup table (256/320 = 0.8)
    SCALE_FACTOR = (SCREEN_WIDTH * 100) / 320
    scale_lookup:
        .repeat 256, i
            .byte (i * SCALE_FACTOR) / 100
        .endrepeat
    scale_lookup_hi_lo:
        .repeat 256, i
            .byte ((i * 256 * SCALE_FACTOR) / 100) & $FF
        .endrepeat
    scale_lookup_hi_hi:
        .repeat 256, i
            .byte ((i * 256 * SCALE_FACTOR) / 100) >> 8
        .endrepeat

.segment "CODE"

.macro read_polygon_byte
    lda poly_ptr
    ldx poly_ptr+1
    ldy poly_ptr+2
    jsr read_byte
    inc24 poly_ptr
.endmacro

.macro read_polygon_word
    lda poly_ptr
    ldx poly_ptr+1
    ldy poly_ptr+2
    jsr read_word
    inc24 poly_ptr
    inc24 poly_ptr
.endmacro

.macro get_vertex_x num, addr
    ldx num
    lda polygons_x,x
    sta addr
    lda polygons_x+1,x
    sta addr+1
.endmacro

.macro get_vertex_y num, addr
    ldx num
    lda polygons_y,x
    sta addr
    lda polygons_y+1,x
    sta addr+1
.endmacro

; ---------------------------------------------------------------
; Parse polygon info and call draw_polygon
; Preliminaries must be set from opcode call
; ---------------------------------------------------------------
.proc parse_polygon
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
    tax
    lda scale_lookup,x
    jsr multiply_zoom
    sta polygon_info+polygon_data::width
    stx polygon_info+polygon_data::width+1

    read_polygon_byte
    jsr multiply_zoom
    sta polygon_info+polygon_data::height
    stx polygon_info+polygon_data::height+1

    read_polygon_byte
    sta polygon_info+polygon_data::num_vertices
    asl
    sta counter    ; counter = num_vertices * 2

    ; *** read the data into the array
    ldy #0
    vertex_loop:
        phy
        read_polygon_word
        stx work
        tax ; x vert 
        lda scale_lookup,x ; scale the x value
        jsr multiply_zoom
        ply
        sta polygons_x,y ; store x value
        txa
        sta polygons_x+1,y ; store x high byte

        phy
        lda work ; y vert
        jsr multiply_zoom
        ply
        sta polygons_y,y ; store y value
        txa
        sta polygons_y+1,y ; store y high byte

        iny
        iny
        cpy counter
        bne vertex_loop

    ; scale the center coordinates to 256 width
    lda polygon_info+polygon_data::center_x+1
    bmi @handle_negative                     ; If high byte is $FF, handle negative value
    beq @normal_range                        ; If high byte is 0, no need to handle high byte

    ; Handle extended range (high byte is not zero)
    ldx polygon_info+polygon_data::center_x  ; Get low byte 
    lda scale_lookup,x                       ; Scale it using lookup table 
    sta polygon_info+polygon_data::center_x  ; Store result
    ldx polygon_info+polygon_data::center_x+1 ; Get high byte 
    lda scale_lookup_hi_lo,x                       ; Scale it using lookup table 
    clc
    adc polygon_info+polygon_data::center_x 
    sta polygon_info+polygon_data::center_x
    lda scale_lookup_hi_hi,x                       ; Scale high byte using lookup table 
    adc #0 
    sta polygon_info+polygon_data::center_x+1 
    bra @done

    @normal_range:
    ldx polygon_info+polygon_data::center_x  ; Get low byte
    lda scale_lookup,x                       ; Scale it using lookup table
    sta polygon_info+polygon_data::center_x  ; Store result
    bra @done

    @handle_negative:
    ; invert the value
    sec
    lda polygon_info+polygon_data::center_x
    eor #$FF
    adc #0
    sta polygon_info+polygon_data::center_x
    lda polygon_info+polygon_data::center_x+1
    eor #$FF
    adc #0
    sta polygon_info+polygon_data::center_x+1
    ; do the scale
    ldx polygon_info+polygon_data::center_x  ; Get low byte 
    lda scale_lookup,x                       ; Scale it using lookup table
    sta polygon_info+polygon_data::center_x  ; Store result
    ldx polygon_info+polygon_data::center_x+1 ; Get high byte
    lda scale_lookup_hi_lo,x                       ; Scale it using lookup table
    clc
    adc polygon_info+polygon_data::center_x 
    sta polygon_info+polygon_data::center_x
    lda scale_lookup_hi_hi,x                       ; Scale high byte using lookup table
    adc #0 
    sta polygon_info+polygon_data::center_x+1 
    ; invert the result
    sec
    lda polygon_info+polygon_data::center_x
    eor #$FF
    adc #0
    sta polygon_info+polygon_data::center_x
    lda polygon_info+polygon_data::center_x+1
    eor #$FF
    adc #0
    sta polygon_info+polygon_data::center_x+1
    
@done:
    ; Continue with drawing
    jmp draw_polygon

    finish:
    rts
.endproc

; ---------------------------------------------------------------
; Parse group polygon info and call draw_polygon
; Preliminaries must be set
; ---------------------------------------------------------------
.proc parse_polygon_group
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
        ; read_polygon_byte
        ; sta off+1
        ; read_polygon_byte
        ; sta off             ; off = read_word child_offset
        read_polygon_word
        sta off+1
        stx off

        read_polygon_word

        stx temp
        jsr multiply_zoom
        sta cx
        stx cx+1

        lda temp
        jsr multiply_zoom
        sta cy
        stx cy+1

        ; save the polygon_data
        push num_children
        push16 polygon_info+polygon_data::width
        push16 polygon_info+polygon_data::height
        push16 polygon_info+polygon_data::zoom
        push16 polygon_info+polygon_data::center_x
        push16 polygon_info+polygon_data::center_y

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

        asl16_addr off, 1 ; child off = off * 2 

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

        jsr parse_polygon

        ; restore the offset
        pla
        sta poly_ptr+2
        pla
        sta poly_ptr+1
        pla
        sta poly_ptr

        ; restore the polygon_data
        pop16 polygon_info+polygon_data::center_y
        pop16 polygon_info+polygon_data::center_x
        pop16 polygon_info+polygon_data::zoom
        pop16 polygon_info+polygon_data::height
        pop16 polygon_info+polygon_data::width
        pop num_children

        plx
        inx
        cpx num_children
        jne child_loop
    rts
.endproc

; A: low byte of value to multiply
.proc multiply_zoom
    sta zoom_result
    stz zoom_result+1

    lda polygon_info+polygon_data::zoom
    sta zoom_temp
    cmp #64
    bne :+
    lda zoom_result
    ldx #0
    rts
    :
    lda polygon_info+polygon_data::zoom+1
    sta zoom_temp+1

    continue:
    mulx_addr zoom_result, zoom_temp
    stx zoom_result+1
    
    ; best: 23 worst: 49 cycles
    cpx #$40 ; check if higher than $4000 (16384) ; 2 cycles
    bcs slow_divide ; 2

    ; Fast division by 64 by multiplying by 4 and dropping the low byte
    asl  ; 2
    rol zoom_result+1 ; 5
    asl ; 2
    rol zoom_result+1 ; 5
    lda zoom_result+1 ; 3
    ldx #0 ; 2
    rts

slow_divide:
    lsr zoom_result+1     ;6
    ror             ; 2
    lsr zoom_result+1    ; 6
    ror             ; 2
    lsr zoom_result+1    ; 6
    ror             ; 2
    lsr zoom_result+1    ;6
    ror             ; 2
    lsr zoom_result+1    ;6
    ror             ; 2
    lsr zoom_result+1    ;6
    ror             ; 2
    ldx zoom_result+1    ; 3
    rts

no_zoom:
    ldx #00         
    rts
.endproc

; ---------------------------------------------------------------
; Draw a polygon
; ---------------------------------------------------------------
.proc draw_polygon
    ; *** calculate half width and half height
    lda polygon_info+polygon_data::width+1
    lsr
    lda polygon_info+polygon_data::width
    ror
    sta half_width    ; store half width

    lda polygon_info+polygon_data::height+1
    lsr
    lda polygon_info+polygon_data::height
    ror
    sta half_height    ; store half height

    ; *** Load center coordinates 
    lda polygon_info+polygon_data::center_x
    sta origin+POINT::x_val
    lda polygon_info+polygon_data::center_x+1
    sta origin+POINT::x_val+1

    ; *** Calculate origin top left
    sec
    lda origin+POINT::x_val
    sbc half_width
    sta origin+POINT::x_val
    lda origin+POINT::x_val+1
    sbc #0
    sta origin+POINT::x_val+1   ; left side

    ; *** Calculate top edge
    lda polygon_info+polygon_data::center_y
    sta origin+POINT::y_val
    lda polygon_info+polygon_data::center_y+1
    sta origin+POINT::y_val+1
    sec
    lda origin+POINT::y_val
    sbc half_height
    sta origin+POINT::y_val
    lda origin+POINT::y_val+1
    sbc #0
    sta origin+POINT::y_val+1   ; top edge

    ; *** Setup color and VERA FX cache
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

    ; *** Determine drawing function once
    lda polygon_info+polygon_data::color
    cmp #$10
    bcc use_solid
    jeq use_trans
    cmp #$11
    jeq use_copy
    ; fallback to solid
    use_solid:
        lda #0
        sta draw_mode
        jmp start_segments
    use_trans:
        lda #2  
        sta draw_mode
        jmp start_segments
    use_copy:
        lda #4
        sta draw_mode
    start_segments:

    ; *** Initialize vertex tracking
    ; *** THE VERTICES ARE CLOCKWISE, SO LEFT IS THE LAST VERTEX
    ; index points to a value X or Y, not a vertex
    stz right_index     ; Start with first vertex for RIGHT edge
    lda polygon_info+polygon_data::num_vertices
    asl
    sec
    sbc #2               ; Last vertex index for LEFT edge
    sta left_index
    lda polygon_info+polygon_data::num_vertices
    lsr ; divide by 2
    sta count            ; Total vertices to process

    ; *** loop through segments ***
    ; TODO: we're using bresenham here, but would a fixed point calculated step be faster? like a 16.8 step?
    next_segment:
        ; Early exit if no vertices
        lda count
        jeq done           ; Exit if count = 0

        ; Get left edge start vertex
        get_vertex_x left_index, left_edge
        get_vertex_y left_index, y_top
        
        ; Get left edge end vertex
        lda left_index
        sec
        sbc #2              ; Point to prev index (next vertex)
        sta left_index_next      ; Store for next vertex access
        get_vertex_x left_index_next, left_dx ; left_dx = next_x
        get_vertex_y left_index_next, y_bottom
        
        ; Calculate left dx and direction
        sec
        lda left_dx
        sbc left_edge        ; dx = next_x - current_x
        sta left_dx
        ; sta left_error
        lda left_dx+1
        sbc left_edge+1
        sta left_dx+1
        ; sta left_error+1
        
        stz left_dx_neg     ; Assume positive direction
        
        ; Make left dx positive if needed
        bit left_dx+1           ; BIT sets N to bit 7 of the byte
        bpl left_dx_positive    ; if N is clear, then dx is positive
        sec                     ; Negate dx to make it positive temporarily
        lda #0
        sbc left_dx
        sta left_dx
        ; sta left_error
        lda #0
        sbc left_dx+1
        sta left_dx+1
        ; sta left_error+1
        lda #1             ; Mark as negative
        sta left_dx_neg

    left_dx_positive:
        ; Get right edge vertices
        get_vertex_x right_index, right_edge
        
        lda right_index
        clc
        adc #2                      ; Point to previous vertex
        sta right_index_next        ; Store for next vertex access
        get_vertex_x right_index_next, right_dx ; right_dx = prev_x
        
        ; Calculate right dx and direction
        sec
        lda right_dx
        sbc right_edge       ; dx = prev_x - current_x
        sta right_dx
        ; sta right_error
        lda right_dx+1
        sbc right_edge+1
        sta right_dx+1
        ; sta right_error+1
        
        stz right_dx_neg    ; Assume positive direction
        
        ; Make right dx positive if needed
        bit right_dx+1      ; Check sign
        bpl right_dx_positive
        sec                 ; Negate dx
        lda #0
        sbc right_dx
        sta right_dx
        ; sta right_error
        lda #0
        sbc right_dx+1
        sta right_dx+1
        ; sta right_error+1
        lda #1             ; Mark as negative
        sta right_dx_neg

    right_dx_positive:
        ; Calculate height of segment
        sec
        lda y_bottom
        sbc y_top
        sta seg_height
        lda y_bottom+1
        sbc y_top+1
        sta seg_height+1
        
        ; If height is negative (y_bottom < y_top) or zero, skip segment 545 147
        lda seg_height     ; Load low byte
        ora seg_height+1   ; OR with high byte
        jeq segment_done   ; Branch if zero
        bit seg_height+1   
        jmi segment_done   ; Branch if negative

        ; Initialize error terms 
        stz left_error
        stz left_error+1
        stz right_error
        stz right_error+1

        ; Setup starting Y for line_info (with offset)
        lda y_top
        clc
        adc origin+POINT::y_val            ; Add Y offset
        sta line_info+line_data::y1
        lda y_top+1
        adc origin+POINT::y_val+1
        sta line_info+line_data::y1+1 

        ; Add origin X offset to left and right edges
        clc
        lda left_edge
        adc origin+POINT::x_val            ; Add X offset for left edge
        sta left_edge
        lda left_edge+1
        adc origin+POINT::x_val+1
        sta left_edge+1

        clc
        lda right_edge
        adc origin+POINT::x_val            ; Add X offset for right edge
        sta right_edge
        lda right_edge+1
        adc origin+POINT::x_val+1
        sta right_edge+1

        ; *** Scanline loop ***
        scanline_loop:
            lda y_top           ; Check if reached end of segment
            cmp y_bottom
            jeq segment_done

            ; *** if y > 199 then return (early exit)
            lda line_info+line_data::y1+1
            beq check_y_low
            jmp next_scanline
            check_y_low:
            lda line_info+line_data::y1
            cmp #SCREEN_HEIGHT
            bcc y_ok
            jmp next_scanline
            y_ok:

            ; *** If right_edge < 0, entire line is off left side of screen
            lda right_edge+1
            jmi next_scanline        ; If high byte negative, right_edge < 0, skip line

            ; Draw the horizontal line
            lda left_edge
            sta line_info+line_data::x1
            lda left_edge+1
            sta line_info+line_data::x1+1
            lda right_edge
            sta line_info+line_data::x2
            lda right_edge+1
            sta line_info+line_data::x2+1
            jsr draw_line ; *** draw the line
            
            ; Update left edge position
            clc
            lda left_error
            adc left_dx
            sta left_error
            lda left_error+1
            adc left_dx+1
            sta left_error+1
            
            ; Check if we need to advance left X
        check_left_advance:
            sec
            lda left_error
            sbc seg_height      ; Compare against segment height
            tay                 ; Save low byte
            lda left_error+1
            sbc seg_height+1
            bcc no_left_advance ; If error < height, don't advance
            
            ; Update error and advance X
            sty left_error
            sta left_error+1
            
            lda left_dx_neg
            bne move_left
            
            inc16 left_edge      ; Move right
            jmp check_left_advance
            
        move_left:
            dec16 left_edge      ; Move left
            jmp check_left_advance

        no_left_advance:
            ; Update right edge position
            clc
            lda right_error
            adc right_dx
            sta right_error
            lda right_error+1
            adc right_dx+1
            sta right_error+1
            
            ; Check if we need to advance right X
        check_right_advance:
            sec
            lda right_error
            sbc seg_height      ; Compare against segment height
            tay                 ; Save low byte
            lda right_error+1
            sbc seg_height+1
            bcc no_right_advance ; If error < height, don't advance
            
            ; Update error and advance X
            sty right_error
            sta right_error+1
            
            lda right_dx_neg
            bne move_right
            
            inc16 right_edge      ; Move right
            jmp check_right_advance
            
        move_right:
            dec16 right_edge      ; Move right
            jmp check_right_advance

        no_right_advance:
        next_scanline:
            inc16 line_info+line_data::y1  ; Next scanline
            inc y_top
            jmp scanline_loop

    segment_done:
        inc right_index
        inc right_index    ; Move to next vertex (2 bytes per vertex)
        dec left_index
        dec left_index      ; Move to previous vertex
        dec count
        jmp next_segment

    done:
    rts
.endproc

