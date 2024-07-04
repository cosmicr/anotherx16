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
    x3:             .res 2
    y3:             .res 1
    x4:             .res 2
    y4:             .res 1
    ztemp:          .res 2

.segment "DATA"
    polygon_info:   .tag polygon_data

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
    
    lda polygon_info+polygon_data::offset
    ; topleftX = center_x - width/2
    lda polygon_info+polygon_data::width ; width is already scaled
    lsr
    sta topleftX    ; topleftX = polygon::width >> 1
    sec
    lda polygon_info+polygon_data::center_x
    sbc topleftX
    sta topleftX    ; topleftX = center_x - width/2
    ; bcs @no_underflow   ; todo: the number should be signed - consider 16 bit?
    ; stz topleftX
    ; @no_underflow:
    lda polygon_info+polygon_data::center_x+1
    sbc #0
    sta topleftX+1

    ; temp hack remove later
    clc
    lda topleftX
    adc #160
    sta topleftX
    lda topleftX+1
    adc #0
    sta topleftX+1

    stz topleftX
    stz topleftX+1

    ; topleftY = y_val - height/2
    lda polygon_info+polygon_data::height
    lsr
    sta topleftY    ; topleftY = polygon::height >> 1
    sec
    lda polygon_info+polygon_data::center_y
    sbc topleftY
    sta topleftY    ; topleftXY = y_val - height/2

    stz topleftY

    ; temp hack remove later
    ; clc
    ; lda topleftY
    ; adc #6
    ; sta topleftY

    ; if num vertices is 4 and height < 2
    lda polygon_info+polygon_data::num_vertices
    cmp #4
    bne @start
    lda polygon_info+polygon_data::height
    cmp #2
    bcs @start
    ; if width is > 0, draw a line

    lda polygon_info+polygon_data::width
    bne @line
    lda topleftX
    sbc #128
    tax
    lda polygon_info+polygon_data::color
    ldy topleftY
    jsr plot_pixel
    jmp finish

    @line:
    ; todo: draw a line
    ; jmp finish

    lda polygon_info+polygon_data::color
    ; if color is higher than 15, return
    cmp #$10
    jcs finish

    @start:
    ; Initialize loop variables
    stz i                       ; i = 0
    lda polygon_info+polygon_data::num_vertices
    sta num_vertices            ; num_vertices = polygon::num_vertices
    dec
    sta j                       ; j = num_vertices - 1
    lsr num_vertices
    dec num_vertices            ; num_vertices = (num_vertices/2) - 1

    loop:
        ; Get the vertices
        lda i
        asl
        tay
        get_vertex x1, y1

        iny
        get_vertex x2, y2

        ; Get the vertices from the back
        lda j
        asl
        tay
        get_vertex x3, y3

        dey
        dey
        dey
        get_vertex x4, y4

        ; todo: note y3=y1 and y4=y2 for speed

; Swap x1 and x3 if x1 > x3
        lda x1+1
        cmp x3+1
        bcc @no_swap_x1_x3  ; If x1 high < x3 high, no swap
        bne @swap_x1_x3     ; If x1 high > x3 high, swap
        lda x1              ; If high bytes equal, check low bytes
        cmp x3
        bcc @no_swap_x1_x3  ; If x1 low < x3 low, no swap
        beq @no_swap_x1_x3  ; If x1 low = x3 low, no swap
    @swap_x1_x3:
        swap16 x1, x3
        swap y1, y3
    @no_swap_x1_x3:

; Swap x2 and x4 if x2 > x4
        lda x2+1
        cmp x4+1
        bcc @no_swap_x2_x4  ; If x2 high < x4 high, no swap
        bne @swap_x2_x4     ; If x2 high > x4 high, swap
        lda x2              ; If high bytes equal, check low bytes
        cmp x4
        bcc @no_swap_x2_x4  ; If x2 low < x4 low, no swap
        beq @no_swap_x2_x4  ; If x2 low = x4 low, no swap
    @swap_x2_x4:
        swap16 x2, x4
        swap y2, y4
    @no_swap_x2_x4:

        @draw:
        jsr draw_quad

        @next:
        inc i
        dec j
        ldx i
        cpx num_vertices
        jne loop

    finish:
    rts
.endproc
