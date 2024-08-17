; parse polygon info
; takes in:
;   Resource* res, uint16_t offset, uint8_t zoom, uint8_t color, int16_t x, int16_t y
;   for assembly version, data is stored in the polygon_info struct
.proc parse_polygon
    ; *** Set the pointer to the polygon data
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
    ; add the offset to the pointer
    add16_addr poly_ptr, polygon_info+polygon_data::offset

    ; *** Process the setup byte
    read_polygon_byte
    sta setup
    ; if setup is less than $C0 then it's a group polygon
    lda setup
    cmp #$C0
    jcc group_polygon

    ; *** Process single polygon
    lda polygon_info+polygon_data::color
    bpl @skip_color ; bit 7 indicates if color needs to be set (0 = skip, 1 = set)
    lda setup
    and #$3F
    sta polygon_info+polygon_data::color
    @skip_color:

    ; Get polygon width, height, and number of vertices
    read_polygon_byte
    jsr multiply_zoom
    sta polygon_info+polygon_data::width
    read_polygon_byte
    jsr multiply_zoom
    sta polygon_info+polygon_data::height
    read_polygon_byte
    jsr multiply_zoom
    sta polygon_info+polygon_data::num_vertices
    asl
    sta counter    ; counter = num_vertices * 2

    ; Get vertices
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

    ; *** Draw the polygon
    jsr draw_polygon
    rts

    ; *** Process group polygons
    group_polygon:
        lda setup
        and #$3F    ; mask out top 2 bits
        cmp #2
        bne @finish
        jsr parse_polygon_group

    @finish:
        rts
.endproc

; read polygroup
; Resource* res, uint16_t offset, uint8_t zoom, int16_t x, int16_t y
.proc parse_polygon_group
    ; *** Set X and Y for the group
    read_polygon_byte
    jsr multiply_zoom
    sta group_x
    lda polygon_info+polygon_data::center_x
    sec
    sbc group_x
    sta group_x
    lda polygon_info+polygon_data::center_x+1
    sbc #0
    sta group_x+1

    read_polygon_byte
    jsr multiply_zoom
    sta group_y
    lda polygon_info+polygon_data::center_y
    sec
    sbc group_y
    sta group_y
    lda polygon_info+polygon_data::center_y+1
    sbc #0
    sta group_y+1

    ; *** Get the number of children in the group
    read_polygon_byte
    inc
    sta num_children ; number of children+1

    ; *** Process the children
    ldx #0
    child_loop:
        phx
        ; get the offset to the child
        read_polygon_byte
        sta child_offset+1
        read_polygon_byte
        sta child_offset

        ; get the vertex offset
        read_polygon_byte
        jsr multiply_zoom
        sta child_x
        read_polygon_byte
        jsr multiply_zoom
        sta child_y

        ; add the group x and y to the child x and y
        lda child_x
        clc
        adc group_x
        sta child_x
        lda #0
        adc group_x+1
        sta child_x+1
        lda child_y
        clc
        adc group_y
        sta child_y
        lda #0
        adc group_y+1
        sta child_y+1

        ; set the color
        lda #$FF
        sta child_color

        lda child_offset+1
        bpl @skip_color ; bit 7 indicates if color needs to be set (0 = skip, 1 = set)
        read_polygon_byte
        and #$7F
        sta child_color
        read_polygon_byte   ; skip next byte?
        @skip_color:

        ; child_offset *=2
        asl child_offset
        rol child_offset+1
        
        ; save poly_ptr
        lda poly_ptr
        pha
        lda poly_ptr+1
        pha

        ; parse the polygon
        lda child_offset
        sta polygon_info+polygon_data::offset
        lda child_offset+1
        sta polygon_info+polygon_data::offset+1
        lda child_color
        sta polygon_info+polygon_data::color
        lda child_x
        sta polygon_info+polygon_data::center_x
        lda child_x+1
        sta polygon_info+polygon_data::center_x+1
        lda child_y
        sta polygon_info+polygon_data::center_y
        lda child_y+1
        sta polygon_info+polygon_data::center_y+1
        
        jsr parse_polygon
        
        ; restore poly_ptr
        pha
        sta poly_ptr+1
        pla
        sta poly_ptr

        plx
        inx
        cpx num_children
        jne child_loop
    rts
.endproc

