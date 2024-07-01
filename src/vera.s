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

.segment "ZEROPAGE"
    vtemp:      .res 1
    dx:         .res 4
    dy:         .res 4

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

.segment "CODE"

.macro calc_slope xa, ya, xb, yb, slope
.scope
    ; Calculate dx = xb - xa (24-bit)
    lda xb
    sec
    sbc xa
    sta dx
    lda #0
    sbc #0
    sta dx+1
    sta dx+2  ; Sign extend to 24 bits

    ; Calculate dy = yb - ya (24-bit)
    lda yb
    sec
    sbc ya
    sta dy

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

    ; Push dy onto the stack (divisor)
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
    cpx #$80        ; Check for negative result and store sign bit
    ror slope+1     ; rotate sign bit into high byte
    ror slope       ; slope = slope >> 1
    bra @finish

@zero:
    stz slope
    stz slope+1
@finish:
.endscope
.endmacro

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

    ; Set the screen resolution to 240x192  ; TODO: go back to 320x200, use 16 bit X values?
    lda #48 ; (240 / 640) * 128 = 48
    sta VERA::DISP::HSCALE
    lda #51 ; (192 / 480) * 128 = 51.2
    sta VERA::DISP::VSCALE  

    ;jsr clear_text_screen ; clear text screen

    rts
.endproc

; ---------------------------------------------------------------
; Set the tilebase for layer 0
; A: page number
; Page address is A * 30720
; ---------------------------------------------------------------
.proc set_vera_page
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
    lda #($07800>>9)
    sta VERA::L0::TILE_BASE
    rts

set_vera_page_2:
    lda #($0F000>>9)
    sta VERA::L0::TILE_BASE
    rts

set_vera_page_3:
    lda #($16800>>9)
    sta VERA::L0::TILE_BASE
    rts
.endproc

; ---------------------------------------------------------------
; Subroutine to set VERA::ADDR to the start of a page
; *** these need to be aligned to 2048 bytes ***
; page 0: $00000
; page 1: $07800 (30720)
; page 2: $0F000 (+30720)
; page 3: $16800 (+30720)
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
    lda #<$07800
    sta VERA::ADDR
    lda #>$07800
    sta VERA::ADDR + 1
    stz VERA::ADDR + 2
    rts

set_addr_page_2:
    lda #<$0F000
    sta VERA::ADDR
    lda #>$0F000
    sta VERA::ADDR + 1
    stz VERA::ADDR + 2
    rts

set_addr_page_3:
    lda #<$16800
    sta VERA::ADDR
    lda #>$16800
    sta VERA::ADDR + 1
    lda #1
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
    asl
    asl
    asl
    asl
    ora vtemp               ; color is duplicated for double pixels
    tax

    ; Enable DCSEL mode 2 to allow cache operations
    lda #(2<<1)             ;#VERA::DCSEL::MODE2
    sta VERA::CTRL

    ; Enable cache writing 
    lda #(1<<6)             ;#VERA::FX_CTRL_FLAGS::CACHE_WRITE_EN
    sta $9F29               ;FX_CTRL

    ; Change DCSEL to mode 6 for cache write operations
    lda #(6<<1)             ;#VERA::DCSEL::MODE6
    sta VERA::CTRL

    ; Prepare the 32-bit cache with the colour to clear the screen with
    txa
    sta $9F29               ;VERA::FX_CACHE_L
    sta $9F2A               ;VERA::FX_CACHE_M
    sta $9F2B               ;VERA::FX_CACHE_H
    sta $9F2C               ;VERA::FX_CACHE_U

    ; Set address auto-increment to 4 bytes
    lda #VERA::INC4
    ora VERA::ADDR + 2  
    sta VERA::ADDR + 2

    ; This will write 8 pixels at a time (32 bits)
    ldx #>(320*192/8)       ; High Byte
    ldy #<(320*192/8)       ; Low Byte

    CLEAR_LOOP

    lda #(2<<1)             ;#VERA::DCSEL::MODE2
    sta VERA::CTRL     
    lda $9F29               ;FX_CTRL 
    eor #(1<<6)             ;#VERA::FX_CTRL_FLAGS::CACHE_WRITE_EN
    sta $9F29               ;FX_CTRL        ;Disable cache writing

    lda #0                  ;#VERA::DCSEL::MODE0
    sta VERA::CTRL

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
    resource = work
    pal_index = work+2
    palette_addr = work+3
    bank_num = work+5
    red = work+6

    sta pal_index ; store palette number

    lda state+engine::palette
    sta resource
    lda state+engine::palette+1
    sta resource+1

    ; ldy #resource::rank
    ; lda (resource),y
    ; sta bank_num
    ldy #resource::pointer
    lda (resource),y
    sta palette_addr  
    iny  
    lda (resource),y
    sta palette_addr+1

    ; add 32 * palette number to the palette address
    lda pal_index
    asl
    asl
    asl
    asl
    asl         ; multiply by 32
    clc
    adc palette_addr
    sta palette_addr
    lda #0
    adc palette_addr+1
    sta palette_addr+1

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
        ldy #RESOURCE_BANK_START    ; bank_num
        lda palette_addr
        ldx palette_addr+1
        jsr read_byte           ; red
        sta red
        inc16 palette_addr
        ldy #RESOURCE_BANK_START
        lda palette_addr
        ldx palette_addr+1
        jsr read_byte
        sta VERA::DATA0
        lda red
        sta VERA::DATA0
        inc16 palette_addr
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
    jsr set_addr_page

    ; set increment to 1 byte for the page
    lda #VERA::INC1
    ora VERA::ADDR + 2
    sta VERA::ADDR + 2

    ; Set data port to 1
    lda #1
    sta VERA::CTRL
    
    ; set the destination address to the start of the page
    txa
    jsr set_addr_page
    lda #VERA::INC4
    ora VERA::ADDR + 2
    sta VERA::ADDR + 2
    
    ; set DCSEL to 2
    lda #(2<<1)
    sta VERA::CTRL

    ; loop through the screen and copy to the page, incrementing Y every 320 X, 192 times
    ldy #>(320*192/8)
    ldx #<(320*192/8)
    inx
    iny

    copy_loop:
        ; FX CTRL CACHE FILL
        lda #(1<<5)
        sta $9F29

        lda VERA::DATA0
        lda VERA::DATA0
        lda VERA::DATA0
        lda VERA::DATA0 ; do nothing it just fills the cache

        ; FX CTRL CACHE WRITE
        lda #(1<<6)
        sta $9F29

        ; write to data1
        stz VERA::DATA1 ; (0 bitmask)

        dex
        bne copy_loop
        dey
        bne copy_loop

    ; turn off cache fill and write
    stz $9F29
    
    ; set DSCEL to 0
    stz VERA::CTRL
    
    rts
.endproc

; ---------------------------------------------------------------
; Draw a quad to draw_page
; ---------------------------------------------------------------
.proc draw_quad
    height = work+18
    left_slope = work+19     ; 16 bit
    right_slope = work+21    ; 16 bit

    ; calculate height (y4-y1)
    lda y4
    sec
    sbc y1
    sta height

    ; TODO: how to clip the quad?

    ; calculate left slope (x1,y1,x2,y2)
    calc_slope x1, y1, x2, y2, left_slope

    ; calculate right slope (x3,y3,x4,y4)
    calc_slope x3, y3, x4, y4, right_slope

    ; Enter FX Control Mode (2)
    lda #(2<<1)     ;#VERA::DCSEL::MODE2
    sta VERA::CTRL

    lda state+engine::draw_page
    ;lda state+engine::display_page
    jsr set_addr_page

    ; Add y1 * 160 to the address
    ldx y1
    lda y160_lookup_lo,x
    clc
    adc VERA::ADDR
    sta VERA::ADDR
    lda y160_lookup_hi,x
    adc VERA::ADDR+1
    sta VERA::ADDR+1
    lda #0
    adc VERA::ADDR+2
    ora #VERA::INC160   ; set increment to 160 bytes
    sta VERA::ADDR+2

    ; Enter polygon filler mode
    lda #($02 | (1<<2)) ; $02 = Polygon mode, 1<<2 = 4-bit mode
    sta $9F29   ; FX_CTRL

    ; Set slopes
    ; Enter FX Increment Mode (3), ADDR0
    lda #(3<<1)     ;#VERA::DCSEL::MODE3
    sta VERA::CTRL

    lda left_slope
    sta $9F29       ; FX_X_INCR_L
    lda left_slope+1
    and #%01111111
    sta $9F2A       ; FX_X_INCR_H
    lda right_slope
    sta $9F2B       ; FX_Y_INCR_L
    lda right_slope+1
    and #%01111111
    sta $9F2C       ; FX_Y_INCR_H

    ; Set x1 and x2 pixel position
    ; Enter FX Position Mode (4), ADDR1
    lda #(4<<1 | 1) ;#VERA::DCSEL::MODE4
    sta VERA::CTRL

    lda x1
    sta $9F29       ; FX_X_POS_L
    stz $9F2A       ; FX_X_POS_H
    lda x3
    sta $9F2B       ; FX_Y_POS_L
    stz $9F2C       ; FX_Y_POS_H

    ; Set ADDR1 increment to a nibble
    lda VERA::ADDR+2
    and #1              ; mask out the increment
    ora #%00000100      ; set the increment to nibbles
    sta VERA::ADDR+2

    ldy height
    jsr fill_quad

    rts
.endproc

; ---------------------------------------------------------------
; Fill a quad
; Y: height
; ---------------------------------------------------------------
.proc fill_quad
    length = work
    color = work+1
    lda polygon_info+polygon_data::color
    asl
    asl
    asl
    asl
    ora polygon_info+polygon_data::color
    sta color

    ; Set DCSEL to 5, ADDR0
    lda #(5<<1)     ;#VERA::DCSEL::MODE5
    sta VERA::CTRL

    loop:
        ldx VERA::DATA1     ; increment, calculate length, and set ADDR1 = ADDR0 + x1

        ; todo: use x pixel pos for start x position
        lda $9F2B   ; FX_POLY_FILL_L
        lsr ; xxxxxLLL
        and #%00000111
        sta length
        lda $9F2C   ; FX_POLY_FILL_H
        asl ; HLLLLLxx
        asl ; LLLLLxxx
        ora length
        sta length 
        ; lda $9F2D   ; FX_POLY_FILL_H
        ; lsr ; xHHLLLLL
        ; lsr ; xxHHLLLL
        ; lsr ; xxxHHLLL
        ; lsr ; xxxxHHLL
        ; lsr ; xxxxxHHL
        ; lsr ; xxxxxxHH
        ; sta length+1    ; todo: probably don't need high byte of length

        ldx length
        beq next_line      ; if length = 0, skip to next line
        fill_loop:
            lda color
            sta VERA::DATA1
            dex
            bne fill_loop
        
        next_line:
        lda VERA::DATA0 ; increment vertically

        dey
        bne loop

    stz VERA::CTRL

    rts
.endproc

; ---------------------------------------------------------------
; Plot a pixel
; A: color
; X: x
; Y: y
; ---------------------------------------------------------------
.proc plot_pixel
    sta work
    phx
    phy
    lda state+engine::draw_page
    jsr set_addr_page

    ; Add y * 160 to the address
    ply
    lda y160_lookup_lo,y
    clc
    adc VERA::ADDR
    sta VERA::ADDR
    lda y160_lookup_hi,y
    adc VERA::ADDR+1
    sta VERA::ADDR+1
    lda #0
    adc VERA::ADDR+2
    sta VERA::ADDR+2

    ; Add x to the address
    plx
    clc
    adc VERA::ADDR
    sta VERA::ADDR
    lda #0
    adc VERA::ADDR+1
    sta VERA::ADDR+1
    lda #0
    adc VERA::ADDR+2
    sta VERA::ADDR+2

    ; Set the pixel color
    lda work
    asl
    asl
    asl
    asl
    ora work
    sta VERA::DATA0

    rts
.endproc