; ---------------------------------------------------------------
; vera.s
; AnotherX16 - Commander X16 port of Another World
; ---------------------------------------------------------------

.macpack longbranch

; X16 and CBM includes
.include "cx16.inc"
.include "cbm_kernal.inc"

; Project includes
.include "main.inc"
.include "vera.inc"
.include "engine.inc"
.include "resource.inc"
.include "macros.inc"
.include "bank.inc"
.include "divide.inc"
.include "polygon.inc"


; todo: clean up zero page variables
.segment "ZEROPAGE"
    vtemp:              .res 8
    dx:                 .res 4
    dy:                 .res 4

.segment "RODATA"
    y160_lookup_lo: ; clamped at 200
        .byte $00, $A0, $40, $E0, $80, $20, $C0, $60, $00, $A0, $40, $E0, $80, $20, $C0, $60
        .byte $00, $A0, $40, $E0, $80, $20, $C0, $60, $00, $A0, $40, $E0, $80, $20, $C0, $60
        .byte $00, $A0, $40, $E0, $80, $20, $C0, $60, $00, $A0, $40, $E0, $80, $20, $C0, $60
        .byte $00, $A0, $40, $E0, $80, $20, $C0, $60, $00, $A0, $40, $E0, $80, $20, $C0, $60
        .byte $00, $A0, $40, $E0, $80, $20, $C0, $60, $00, $A0, $40, $E0, $80, $20, $C0, $60
        .byte $00, $A0, $40, $E0, $80, $20, $C0, $60, $00, $A0, $40, $E0, $80, $20, $C0, $60
        .byte $00, $A0, $40, $E0, $80, $20, $C0, $60, $00, $A0, $40, $E0, $80, $20, $C0, $60
        .byte $00, $A0, $40, $E0, $80, $20, $C0, $60, $00, $A0, $40, $E0, $80, $20, $C0, $60
        .byte $00, $A0, $40, $E0, $80, $20, $C0, $60, $00, $A0, $40, $E0, $80, $20, $C0, $60
        .byte $00, $A0, $40, $E0, $80, $20, $C0, $60, $00, $A0, $40, $E0, $80, $20, $C0, $60
        .byte $00, $A0, $40, $E0, $80, $20, $C0, $60, $00, $A0, $40, $E0, $80, $20, $C0, $60
        .byte $00, $A0, $40, $E0, $80, $20, $C0, $60, $00, $A0, $40, $E0, $80, $20, $C0, $60
        .byte $00, $A0, $40, $E0, $80, $20, $C0, $60, $00, $00, $00, $00, $00, $00, $00, $00
        .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
        .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
        .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00


    y160_lookup_hi:
        .byte $00, $00, $01, $01, $02, $03, $03, $04, $05, $05, $06, $06, $07, $08, $08, $09
        .byte $0A, $0A, $0B, $0B, $0C, $0D, $0D, $0E, $0F, $0F, $10, $10, $11, $12, $12, $13
        .byte $14, $14, $15, $15, $16, $17, $17, $18, $19, $19, $1A, $1A, $1B, $1C, $1C, $1D
        .byte $1E, $1E, $1F, $1F, $20, $21, $21, $22, $23, $23, $24, $24, $25, $26, $26, $27
        .byte $28, $28, $29, $29, $2A, $2B, $2B, $2C, $2D, $2D, $2E, $2E, $2F, $30, $30, $31
        .byte $32, $32, $33, $33, $34, $35, $35, $36, $37, $37, $38, $38, $39, $3A, $3A, $3B
        .byte $3C, $3C, $3D, $3D, $3E, $3F, $3F, $40, $41, $41, $42, $42, $43, $44, $44, $45
        .byte $46, $46, $47, $47, $48, $49, $49, $4A, $4B, $4B, $4C, $4C, $4D, $4E, $4E, $4F
        .byte $50, $50, $51, $51, $52, $53, $53, $54, $55, $55, $56, $56, $57, $58, $58, $59
        .byte $5A, $5A, $5B, $5B, $5C, $5D, $5D, $5E, $5F, $5F, $60, $60, $61, $62, $62, $63
        .byte $64, $64, $65, $65, $66, $67, $67, $68, $69, $69, $6A, $6A, $6B, $6C, $6C, $6D
        .byte $6E, $6E, $6F, $6F, $70, $71, $71, $72, $73, $73, $74, $74, $75, $76, $76, $77
        .byte $78, $78, $79, $79, $7A, $7B, $7B, $7C, $7D, $00, $00, $00, $00, $00, $00, $00
        .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
        .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
        .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00

    color_lookup_hi:
        .byte $00, $10, $20, $30, $40, $50, $60, $70, $80, $90, $A0, $B0, $C0, $D0, $E0, $F0

    color_lookup_shifted:
        .byte $00, $11, $22, $33, $44, $55, $66, $77, $88, $99, $AA, $BB, $CC, $DD, $EE, $FF

    mask_leading:
        .byte %00000000 ; $00
        .byte %00000010 ; $02
        .byte %00000011 ; $03
        .byte %00001011 ; $0B
        .byte %00001111 ; $0F
        .byte %00101111 ; $2F
        .byte %00111111 ; $3F
        .byte %10111111 ; $BF

    mask_trailing:
        .byte %11111101 ; $FD
        .byte %11111100 ; $FC
        .byte %11110100 ; $F4
        .byte %11110000 ; $F0
        .byte %11010000 ; $D0
        .byte %11000000 ; $C0
        .byte %01000000 ; $40
        .byte %00000000 ; $00

.segment "CODE"

; ---------------------------------------------------------------
; Initialise the VERA
; ---------------------------------------------------------------
.proc init_vera
    ; Set layer 0 to 4bpp bitmap mode
    lda #VERA::BITMAP4BPP
    sta VERA::L0::CONFIG

    ; Set layer 0 tilebase to $0000 in VRAM
    stz VERA::L0::TILE_BASE

    ; Set layer 1 to 1 bit tile mode
    lda #VERA::TILE1BPP |  VERA::MAP::WIDTH128 | VERA::MAP::HEIGHT64
    sta VERA::L1::CONFIG

    ; Enable layer 0 (only) put CX16 into VGA mode
    lda #(VERA::DISP::ENABLE::LAYER0 | VERA::DISP::MODE::VGA)
    sta VERA::DISP::VIDEO

    ; Set the screen resolution 
    lda #((SCREEN_WIDTH << 7) / 640)
    sta VERA::DISP::HSCALE
    lda #((SCREEN_HEIGHT << 7) / 480)
    sta VERA::DISP::VSCALE  

    jsr clear_text_screen ; clear text screen

    rts
.endproc

; ---------------------------------------------------------------
; Set the tilebase pointer for layer 0
; A: page number
; Page address is A * 30720
; ---------------------------------------------------------------
.proc set_vera_page
    stz VERA::CTRL
    cmp #0
    beq set_vera_page_0
    cmp #1
    beq set_vera_page_1
    cmp #2
    beq set_vera_page_2
    cmp #3
    beq set_vera_page_3
    rts

set_vera_page_0:
    stz VERA::L0::TILE_BASE
    rts

set_vera_page_1:
    lda #((1 * PAGE_SIZE) >> 9)
    sta VERA::L0::TILE_BASE
    rts

set_vera_page_2:
    lda #((2 * PAGE_SIZE) >> 9)
    sta VERA::L0::TILE_BASE
    rts

set_vera_page_3:    
    lda #((3 * PAGE_SIZE) >> 9)
    sta VERA::L0::TILE_BASE
    rts
.endproc

; ---------------------------------------------------------------
; Subroutine to set VERA::ADDR to the start of a page
; *** these need to be aligned to 2048 bytes ***
; ---------------------------------------------------------------
.proc set_addr_page
    cmp #0
    beq set_addr_page_0
    cmp #1
    beq set_addr_page_1
    cmp #2
    beq set_addr_page_2
    cmp #3
    beq set_addr_page_3
    rts

set_addr_page_0:
    stz VERA::ADDR
    stz VERA::ADDR + 1
    stz VERA::ADDR + 2
    rts

set_addr_page_1:
    lda #<(PAGE_SIZE * 1)
    sta VERA::ADDR
    lda #>(PAGE_SIZE * 1)
    sta VERA::ADDR + 1
    stz VERA::ADDR + 2
    rts

set_addr_page_2:
    lda #<(PAGE_SIZE * 2)
    sta VERA::ADDR
    lda #>(PAGE_SIZE * 2)
    sta VERA::ADDR + 1
    lda #((PAGE_SIZE * 2) >> 16)
    sta VERA::ADDR + 2
    rts

set_addr_page_3:
    lda #<(PAGE_SIZE * 3)
    sta VERA::ADDR
    lda #>(PAGE_SIZE * 3)
    sta VERA::ADDR + 1
    lda #((PAGE_SIZE * 3) >> 16)
    sta VERA::ADDR + 2
    rts
.endproc

; ---------------------------------------------------------------
; Clear Screen buffer VRAM using VERA's 32-bit cache feature
; A: colour to clear the screen with
; use whatever current page is set in VERA::ADDR
; ---------------------------------------------------------------
.proc clear_page
    sta vtemp
    tax
    lda color_lookup_hi,x
    ora vtemp               ; color is duplicated for double pixels
    tax

    ; Enable DCSEL mode 2 to allow cache operations
    set_dcsel 2

    ; Enable cache writing 
    lda #(1<<6)             ;#VERA::FX_CTRL_FLAGS::CACHE_WRITE_EN
    sta FX_CTRL               ;FX_CTRL

    ; Change DCSEL to mode 6 for cache write operations
    set_dcsel 6

    ; Prepare the 32-bit cache with the colour to clear the screen with
    txa
    sta FX_CACHE_L
    sta FX_CACHE_M
    sta FX_CACHE_H
    sta FX_CACHE_U

    ; Set address auto-increment to 4 bytes
    lda #VERA::INC4
    ora VERA::ADDR + 2  
    sta VERA::ADDR + 2

    ; This will write 8 pixels at a time (32 bits)
    ldx #>(PAGE_SIZE / 4)   ; High Byte
    ldy #<(PAGE_SIZE / 4)   ; Low Byte

    lda #%00000000          ; Set the mask to 0 (no mask)

    clear_loop:
        sta VERA::DATA0     ; Write the 32-bit cache to VRAM (4 bytes of zeros) 
        dey                 ; Decrement low byte of loop counter
        bne clear_loop      ; Continue loop if low byte is not zero
        dex                 ; Decrement high byte of loop counter
        bne clear_loop      ; Continue loop if high byte is not zero

    set_dcsel 2             ; Set DCSEL back to 0
    lda #(1<<6)             ;#VERA::FX_CTRL_FLAGS::CACHE_WRITE_EN
    trb FX_CTRL             ;FX_CTRL        ;Disable cache writing

    stz VERA::CTRL          ;#VERA::DCSEL::MODE0
    
    rts
.endproc 

; ---------------------------------------------------------------
; Set lower case mode and clear text screen
; ---------------------------------------------------------------
.proc clear_text_screen
    lda #14 ; lowercase mode
    jsr CHROUT

    lda #$01
    sta CHARCOLOR ; set background to black (0) and text to white (1)

    lda #147
    jsr CHROUT  ; use the kernal clear screen function lol

    rts
.endproc 

; ---------------------------------------------------------------
; Set palette
; A: palette number
; ---------------------------------------------------------------
.proc set_palette
    pal_index = vtemp
    palette_addr = vtemp + 2
    red = vtemp + 5
    res_addr = vtemp + 6

    sta pal_index ; store palette number
    stz pal_index+1

    ; get the memory address for the game palette resource struct
    lda state+engine::palette
    sta res_addr
    lda state+engine::palette+1
    sta res_addr+1

    ; get the palette address from the resource struct
    ldy #resource::pointer
    lda (res_addr),y
    sta palette_addr  
    iny  
    lda (res_addr),y
    sta palette_addr+1
    iny
    lda (res_addr),y
    sta palette_addr+2

    ; add 32 * palette number to the palette address (palettes are 32 bytes each)
    asl16_addr pal_index, 5 ; multiply by 32
    lda pal_index
    clc
    adc palette_addr
    sta palette_addr
    lda pal_index+1
    adc palette_addr+1
    sta palette_addr+1
    lda #$00
    adc palette_addr+2
    sta palette_addr+2

    stz VERA::CTRL ; set DATA PORT 0 and DCSEL to 0

    ; set the VERA address to the palette address ($1FA00)
    stz VERA::ADDR
    lda #$FA
    sta VERA::ADDR + 1
    lda #($01 | VERA::INC1)
    sta VERA::ADDR + 2

    ldy #0  ; there are 16 colours (2-bytes each)
    ; Each 2 byte palette color has 4 bits each for the RGB info. This leaves 4 unused bits.
    ; 0000RRRR, GGGGBBBB
    palette_loop:
        phy
        lda palette_addr
        ldx palette_addr+1
        ldy palette_addr+2
        jsr read_byte       ; get red
        sta red
        inc24 palette_addr

        lda palette_addr
        ldx palette_addr+1
        ldy palette_addr+2
        jsr read_byte       ; get green and blue
        sta VERA::DATA0     ; set green and blue
        inc24 palette_addr

        lda red
        sta VERA::DATA0     ; set red

        ply
        iny
        cpy #16
        jne palette_loop

    rts
.endproc

; ---------------------------------------------------------------
; Copy a page to another page
; A: source page number
; X: destination page number
; ---------------------------------------------------------------
.proc copy_page
    REPEAT_COUNT = 10
    jsr set_addr_page

    ; set increment to 1 byte for the page
    lda #VERA::INC1
    tsb VERA::ADDR + 2

    ; Set data port to 1
    lda #1
    sta VERA::CTRL
    
    ; set the destination address to the start of the page
    txa
    jsr set_addr_page
    lda #VERA::INC4
    tsb VERA::ADDR + 2
    
    ; set DCSEL to 2
    set_dcsel 2

    ; loop through the screen and copy to the page
    ldy #>(PAGE_SIZE / (REPEAT_COUNT * 4))
    ldx #<(PAGE_SIZE / (REPEAT_COUNT * 4))

    copy_loop:
        .repeat REPEAT_COUNT
            ; FX CTRL CACHE FILL
            lda #(1<<5)
            sta FX_CTRL

            lda VERA::DATA0
            lda VERA::DATA0
            lda VERA::DATA0
            lda VERA::DATA0 ; do nothing it just fills the cache

            ; FX CTRL CACHE WRITE
            lda #(1<<6)
            sta FX_CTRL

            ; write to data1
            stz VERA::DATA1 ; (0 bitmask)
        .endrepeat

        dex
        jne copy_loop
        dey
        jne copy_loop

    ; turn off cache fill and write
    stz FX_CTRL
    
    ; set DSCEL to 0
    stz VERA::CTRL
    
    rts
.endproc

; ---------------------------------------------------------------
; Setup VERA for a transparent line or a copy line
; sets port 0 and port 1 to the same address
; helper function for draw_line
; uses line_info struct
; ---------------------------------------------------------------
.macro setup_line
    ; set control port 1
    lda #1
    sta VERA::CTRL

    ; set the address start
    lda state+engine::draw_page
    jsr set_addr_page   ; todo: integrate y lookup into this routine

    ; todo: use 4 lookup tables instead of adding from one
    ; add Y offset to address
    ldy quad_info+quad_data::top_y
    lda y160_lookup_lo,y
    clc
    adc VERA::ADDR
    sta VERA::ADDR
    lda y160_lookup_hi,y
    adc VERA::ADDR+1
    sta VERA::ADDR+1
    lda #0
    adc VERA::ADDR+2
    sta VERA::ADDR+2    ; y * 160

    ; x1 divided by 2
    lda quad_info+quad_data::top_left+1
    lsr
    sta vtemp+1                     ; save high byte
    lda quad_info+quad_data::top_left
    ror
    clc
    adc VERA::ADDR
    sta VERA::ADDR
    sta vtemp
    lda vtemp+1                     ; high byte
    adc VERA::ADDR+1
    sta VERA::ADDR+1
    sta vtemp+1
    lda #0
    adc VERA::ADDR+2
    ora #VERA::INC1
    sta VERA::ADDR+2                ; y * 160 + x1/2
    sta vtemp+2

    ; set control port 0
    stz VERA::CTRL
    lda vtemp
    sta VERA::ADDR
    lda vtemp+1
    sta VERA::ADDR+1
    lda vtemp+2
    sta VERA::ADDR+2
    ; rts
.endmacro

; ; ---------------------------------------------------------------
; ; Draw a horizontal line
; ; uses data saved in line_info+line_data struct
; ; todo: this could be optimised...
; ; ---------------------------------------------------------------
; .proc draw_line
;     color_shifted = vtemp+2
;     color = vtemp+3
;     x2_minus_8 = vtemp+4
;     x2_minus_2 = vtemp+6
    
;     ; note: despite this not being optimal, it's faster than the optimised because there are so many values higher than 320
;     ; *** if x1 > 319 then return (16-bit)
;     lda line_info+line_data::x1+1
;     cmp #>SCREEN_WIDTH
;     bcc x1_ok
;     bne :+
;     lda line_info+line_data::x1
;     cmp #<SCREEN_WIDTH
;     bcc x1_ok
;     :
;     rts
;     x1_ok:

;     ; *** if x2 > 319 then x2 = 319 
;     lda line_info+line_data::x2+1
;     cmp #>SCREEN_WIDTH
;     bcc x2_ok
;     bne :+
;     lda line_info+line_data::x2
;     cmp #<SCREEN_WIDTH
;     bcc x2_ok
;     :
;     lda #<(SCREEN_WIDTH - 1)
;     sta line_info+line_data::x2
;     lda #>(SCREEN_WIDTH - 1)
;     sta line_info+line_data::x2+1
;     x2_ok:

;     ; *** if y > 199 then return
;     cmp_lt line_info+line_data::y1, #(SCREEN_HEIGHT - 1), y_ok
;     rts
;     y_ok:

;     ; macro
;     setup_line 

;     lda polygon_info+polygon_data::color

;     ; todo: break out into separate functions and check color before calling
;     ; *** if color == $11 then copy
;     cmp #$11
;     bne not_copy
;     lsr16_addr line_info+line_data::x1, 1
;     lsr16_addr line_info+line_data::x2, 1
;     ldx line_info+line_data::x1                 ; 3 cycles
;     ; set the pixels
;     loop_copy:
;         lda VERA::DATA1 
;         sta VERA::DATA0
;         inx                                     ; 2 cycles
;         cpx line_info+line_data::x2             ; 3 cycles
;         bcc loop_trans 
;     rts
;     not_copy:

;     ; *** if color == $10 then use transparency
;     cmp #$10
;     bne not_transparent 
;     lsr16_addr line_info+line_data::x1, 1
;     lsr16_addr line_info+line_data::x2, 1
;     ldx line_info+line_data::x1                 ; 3 cycles
;     ; set the pixels
;     loop_trans:
;         lda VERA::DATA1 
;         ora #$88
;         sta VERA::DATA0
;         inx                                     ; 2 cycles
;         cpx line_info+line_data::x2             ; 3 cycles
;         bcc loop_trans                          ; 2 cycles
;     rts
;     not_transparent:

;     ; *** if color < $10 then draw solid line
;     cmp #$10
;     jcc draw_line_solid

;     ; *** two pixel mode    
;     ; ldx polygon_info+polygon_data::color
;     ; lda color_lookup_hi,x
;     ; sta color_shifted
;     ; ora polygon_info+polygon_data::color
;     ; sta color
;     ; jsr setup_line ; todo: use macro instead for less cycles
;     ; lda color
;     ; lsr16_addr line_info+line_data::x1, 1
;     ; lsr16_addr line_info+line_data::x2, 1
;     ; ldx line_info+line_data::x1
;     ; line_loop:
;     ;     sta VERA::DATA0
;     ;     inx
;     ;     cpx line_info+line_data::x2
;     ;     bcc line_loop
;     ; rts

;     end:
;     rts
; .endproc

; .proc draw_line_solid
;     num_loops = vtemp
;     leading_mask = vtemp+1
;     trailing_mask = vtemp+2

;     set_dcsel 2
;     lda #%01000000   ; set cache write enable
;     sta FX_CTRL
;     set_dcsel 6      ; set cache mode

;     ; set 4 byte increment
;     lda VERA::ADDR+2
;     and #$01        ; mask out the low bit
;     ora #VERA::INC4 ; set port 0 auto increment to 4 bytes
;     sta VERA::ADDR+2

;     ; set leading mask
;     lda line_info+line_data::x1
;     and #$07
;     tay
;     lda mask_leading,y
;     sta leading_mask

;     ; set trailing mask
;     lda line_info+line_data::x2
;     and #$07
;     tay
;     lda mask_trailing,y
;     sta trailing_mask

;     ; calc start byte = x1 / 8
;     lsr16_addr line_info+line_data::x1, 3

;     ; calc end byte = x2 / 8
;     lsr16_addr line_info+line_data::x2, 3

;     ; if start byte == end byte, then draw a short line
;     lda line_info+line_data::x1
;     cmp line_info+line_data::x2
;     bne long_line
;     lda leading_mask
;     ora trailing_mask
;     sta VERA::DATA0
;     bra end_line

;     long_line:
;     ; calc number of loops
;     sec
;     lda line_info+line_data::x2
;     sbc line_info+line_data::x1
;     dec
;     sta num_loops

;         ; draw leading mask
;     lda leading_mask
;     sta VERA::DATA0

;     ldx num_loops
;     beq no_middle
;     line_loop:
;         stz VERA::DATA0
;         dex
;         bne line_loop

;     no_middle:
;     ; draw trailing mask
;     lda trailing_mask
;     sta VERA::DATA0

;     end_line:
;     ; reset fx control
;     set_dcsel 2
;     stz FX_CTRL
;     set_dcsel 0

;     rts
; .endproc

; ; ---------------------------------------------------------------
; ; Draw a pixel using the line_info struct
; ; ---------------------------------------------------------------
; .proc draw_pixel
;     ; set control port 0
;     stz VERA::CTRL

;     ; set the address start
;     lda state+engine::draw_page
;     jsr set_addr_page

;     ; set vera address to y*160
;     ldy line_info+line_data::y1
;     lda y160_lookup_lo,y
;     clc
;     adc VERA::ADDR
;     sta VERA::ADDR
;     lda y160_lookup_hi,y
;     adc VERA::ADDR+1
;     sta VERA::ADDR+1
;     lda #0
;     adc VERA::ADDR+2
;     sta VERA::ADDR+2  
;     ; now add x1/2 to the address
;     lsr line_info+line_data::x1+1
;     ror line_info+line_data::x1
;     add16_addr VERA::ADDR, line_info+line_data::x1
;     lda #0
;     adc VERA::ADDR+2
;     sta VERA::ADDR+2

;     lda line_info+line_data::x1
;     and #$01
;     beq @even
;     lda VERA::DATA0
;     and #$F0
;     ora line_info+line_data::color
;     sta VERA::DATA0
;     bra @done

;     @even:
;     ldx line_info+line_data::color
;     lda color_lookup_hi,x
;     sta vtemp
;     lda VERA::DATA0
;     and #$0F
;     ora vtemp
;     sta VERA::DATA0

; @done:
;     rts
; .endproc

.macro calc_slope xa, ya, xb, yb, slope
.scope
    ; Calculate dx = xb - xa (24-bit)
    sec
    lda xb
    sbc xa
    sta dx
    lda #0
    sbc #0
    sta dx+1
    ; extend sign if dx is negative
    stz dx+2
    lda dx+1
    bpl @positive
    lda #$FF
    sta dx+2
    @positive:

    ; Calculate dy = yb - ya (24-bit)
    lda yb
    sec
    sbc ya
    sta dy
    stz dy+1
    stz dy+2

    lda dy
    beq @zero  ; if dy = 0, slope = 0

    ; slope = ((dx << 9) / dy) >> 1
    ; Shift dx left by 9 (24-bit shift)
    lda dx+1
    sta dx+2
    lda dx
    sta dx+1
    stz dx
    asl dx+1
    rol dx+2

    ; Push dy onto the stack (divisor) for divide24
    lda dy
    pha

    ; Load dx into A, X, Y for division
    lda dx
    ldx dx+1
    ldy dx+2
    jsr divide24  ; 24-bit / 8-bit division

    ; Result is now in A (low) and X (high)
    sta slope
    stx slope+1
    cpx #$80        ; Check for negative result and set carry bit
    ror slope+1     ; rotate sign bit into high byte
    ror slope       ; slope = slope >> 1
    bra @finish

@zero:
    stz slope
    stz slope+1
@finish:
.endscope
.endmacro

.proc draw_quad
    height = dy
    fill_length = vtemp

    ; if top_left > top_right then swap
    lda quad_info+quad_data::top_left
    cmp quad_info+quad_data::top_right
    bcc :+
    swap quad_info+quad_data::top_left, quad_info+quad_data::top_right
    :

    ; if bottom_left > bottom_right then swap
    lda quad_info+quad_data::bottom_left
    cmp quad_info+quad_data::bottom_right
    bcc :+
    swap quad_info+quad_data::bottom_left, quad_info+quad_data::bottom_right
    :

    lda polygon_info+polygon_data::color
    ; *** if color == $11 then copy
    cmp #$11
    bne not_copy
    ; todo: copy
    rts
    not_copy:
    ; *** if color == $10 then transparent
    cmp #$10
    bne not_transparent
    ; todo: transparent
    rts
    not_transparent:

    ; calculate slopes
    calc_slope quad_info+quad_data::top_left, quad_info+quad_data::top_y, quad_info+quad_data::bottom_left, quad_info+quad_data::bottom_y, quad_info+quad_data::left_slope
    calc_slope quad_info+quad_data::top_right, quad_info+quad_data::top_y, quad_info+quad_data::bottom_right, quad_info+quad_data::bottom_y, quad_info+quad_data::right_slope

    ; set the left and right edges, and y start
    add16_addr quad_info+quad_data::top_left, topleftX
    add16_addr quad_info+quad_data::top_right, topleftX
    add16_addr quad_info+quad_data::top_y, topleftY

    ; *** Boundary Checks
    ; if top_y < 0 then top_y = 0
    lda quad_info+quad_data::top_y+1
    bpl :+
    stz quad_info+quad_data::top_y
    stz quad_info+quad_data::top_y+1
    :

    ; if top_left < 0 then top_left = 0
    lda quad_info+quad_data::top_left+1
    bpl :+
    stz quad_info+quad_data::top_left
    stz quad_info+quad_data::top_left+1
    :

    ; if top_right < 0 then return
    lda quad_info+quad_data::top_right+1
    bpl :+
    rts
    :

    ; if top_left > 319 then return
    lda quad_info+quad_data::top_left+1
    beq :+
    lda quad_info+quad_data::top_left
    cmp #<SCREEN_WIDTH
    bcc :+
    rts
    :

    ; if top_right > 319 then top_right = 319
    lda quad_info+quad_data::top_right+1
    beq :+
    lda quad_info+quad_data::top_right
    cmp #<SCREEN_WIDTH
    bcc :+
    lda #<(SCREEN_WIDTH - 1)
    sta quad_info+quad_data::top_right
    lda #>(SCREEN_WIDTH - 1)
    sta quad_info+quad_data::top_right+1
    :

    ; *** set starting VERA ADDR Y
    set_dcsel 2 ; auto sets port to 0 too
    lda state+engine::draw_page
    jsr set_addr_page
    ldx quad_info+quad_data::top_y
    lda y160_lookup_lo,x
    adc VERA::ADDR
    sta VERA::ADDR
    lda y160_lookup_hi,x
    adc VERA::ADDR+1
    sta VERA::ADDR+1
    lda #0
    adc VERA::ADDR+2
    sta VERA::ADDR+2
    
    ; set the address increment to +160
    lda VERA::ADDR+2
    and #$0F
    ora #VERA::INC160
    sta VERA::ADDR+2

    ; *** Enter polygon filler mode
    lda #($02 | $04) ; $02 = Polygon mode, $04 = 4-bit mode
    sta FX_CTRL

    ; *** set left and right slope ; todo: can write directly to register instead of using variables
    set_dcsel 3
    lda quad_info+quad_data::left_slope
    sta FX_X_INCR_L
    lda quad_info+quad_data::left_slope+1
    and #$7F
    sta FX_X_INCR_H
    lda quad_info+quad_data::right_slope
    sta FX_Y_INCR_L
    lda quad_info+quad_data::right_slope+1
    and #$7F
    sta FX_Y_INCR_H
stp
    ; ** set the left and right edges
    set_dcsel 4
    lda quad_info+quad_data::top_left
    sta FX_X_POS_L
    lda quad_info+quad_data::top_left+1
    and #$07
    sta FX_X_POS_H
    lda quad_info+quad_data::top_right
    sta FX_Y_POS_L
    lda quad_info+quad_data::top_right+1
    and #$07
    sta FX_Y_POS_H

    ; *** set ADDR1 increment
    lda VERA::CTRL
    ora #1
    sta VERA::CTRL

    lda VERA::ADDR+2
    and #$0F
    ora #$04    ; set increment to nibble
    sta VERA::ADDR+2

    ; ; *** fill the (triangle)
    ; set_dcsel 6
    ; lda quad_info+quad_data::color
    ; sta FX_CACHE_L
    ; sta FX_CACHE_M
    ; sta FX_CACHE_H
    ; sta FX_CACHE_U

    ; ; enable cache writing
    ; set_dcsel 2
    ; lda #%01001110
    ; sta FX_CTRL

    ; poly fill length mode
    set_dcsel 5

    ; setup color
    ldy polygon_info+polygon_data::color
    lda color_lookup_shifted,y
    sta quad_info+quad_data::color

    ; set height
    ldy height
    loop:
        ldx VERA::DATA1 ; adds x to current row in addr0, increments, and saves addr1
        lda FX_POLY_FILL_L
        sta vtemp
        lsr
        and #$07
        sta fill_length
        bbs7 vtemp, skip_high
        lda FX_POLY_FILL_H
        asl
        asl
        ora fill_length
        sta fill_length
        skip_high:

        ldx fill_length
        beq next_line   ; if length = 0, then go to next line

        fill_loop:
            lda quad_info+quad_data::color
            sta VERA::DATA1
            dex
            bne fill_loop
        
        next_line:
        lda VERA::DATA0 ; increments row in addr0

        dey
        bne loop

    set_dcsel 0

    rts
.endproc