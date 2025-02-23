; ---------------------------------------------------------------
; vera.s
; AnotherX16 - Commander X16 port of Another World
; ---------------------------------------------------------------

; *** VERA MEMORY MAP
; +---------------+-------+--------------------------------+
; | ADDRESS       | SIZE  | DESCRIPTION                    |
; +---------------+-------+--------------------------------+
; | $00000-$077FF | $7800 | ################ Page 0 Bitmap |
; | $07800-$0EFFF | $7800 | ################ Page 1 Bitmap |
; | $0F000-$167FF | $7800 | ################ Page 2 Bitmap |
; | $16800-$1DFFF | $7800 | ################ Page 3 Bitmap |
; | $1E000-$1EFFF | $1000 | ## Charmap (not used)          |
; | $1F000-$1F3FF | $0400 | #  Charset (not used)          |
; | $1F400-$1F9BF | $05C0 | #  unused                      |
; | $1F9C0-$1FFFF | $0640 | #  VERA Registers              |
; +---------------+-------+--------------------------------+
; # = 2048 bytes ($800)

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
.include "text.inc"


; todo: clean up zero page variables
.segment "EXTZP" : zeropage
    vtemp:              .res 4
    ; palette
    pal_index:          .res 2
    palette_addr:       .res 3
    res_addr:           .res 2
    ; for line drawing
    num_loops:          .res 1
    leading_mask:       .res 1
    trailing_mask:      .res 1
    ; copy page
    copy_counter:       .res 1

.segment "DATA"
    copy_active:        .byte 0

.segment "RODATA"
    y160_lookup_lo:
    .repeat SCREEN_HEIGHT, i
        .byte (i * 160) & $FF
    .endrepeat

    y160_lookup_hi:
    .repeat SCREEN_HEIGHT, i
        .byte (i * 160) >> 8
    .endrepeat

    page_base_lo:
        .byte <(PAGE_SIZE * 0)
        .byte <(PAGE_SIZE * 1)
        .byte <(PAGE_SIZE * 2) 
        .byte <(PAGE_SIZE * 3)

    page_base_hi:
        .byte >(PAGE_SIZE * 0)
        .byte >(PAGE_SIZE * 1)
        .byte >(PAGE_SIZE * 2)
        .byte >(PAGE_SIZE * 3)

    page_base_bank:
        .byte ((PAGE_SIZE * 0) >> 16)
        .byte ((PAGE_SIZE * 1) >> 16)
        .byte ((PAGE_SIZE * 2) >> 16)
        .byte ((PAGE_SIZE * 3) >> 16)

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
    jsr clear_text_screen ; clear text screen

    ; Disable all VERA layers
    stz VERA::DISP::VIDEO

    ; Set layer 0 to 4bpp bitmap mode
    lda #VERA::BITMAP4BPP
    sta VERA::L0::CONFIG

    ; Set layer 0 tilebase to $0000 in VRAM
    stz VERA::L0::TILE_BASE

    ; Set layer 1 to 1 bit tile mode
    lda #VERA::TILE1BPP |  VERA::MAP::WIDTH64 | VERA::MAP::HEIGHT32
    sta VERA::L1::CONFIG

    ; Set layer 1 mapbase to $1E000 in VRAM
    lda #($1E000 >> 9)
    sta VERA::L1::MAP_BASE

    ; Set layer 1 tilebase to $1F000 in VRAM
    lda #(($1F000 >> 11) << 2)
    ora #(VERA::TILE::WIDTH8 | VERA::TILE::HEIGHT8)
    sta VERA::L1::TILE_BASE

    ; Set the screen resolution 
    lda #((SCREEN_WIDTH << 7) / 640)
    sta VERA::DISP::HSCALE
    lda #((SCREEN_HEIGHT << 7) / 480)
    sta VERA::DISP::VSCALE  

    ; Clear memory to black
    ldx #0
    loop:
        lda page_base_lo,x
        sta VERA::ADDR
        lda page_base_hi,x
        sta VERA::ADDR + 1
        lda page_base_bank,x
        ora #VERA::INC4
        sta VERA::ADDR + 2
        lda #0
        phx
        jsr clear_page
        plx
        inx
        cpx #4
        bne loop

    ; Enable layer 0 (only) put CX16 into VGA mode
    lda #(VERA::DISP::ENABLE::LAYER0 | VERA::DISP::MODE::VGA)
    sta VERA::DISP::VIDEO
    
    rts
.endproc

; ---------------------------------------------------------------
; Set the tilebase pointer for layer 0
; A: page number
; Page address is A * 30720
; ---------------------------------------------------------------
.proc set_vera_page
    stz VERA::CTRL
    tax
    lda page_values,x
    sta VERA::L0::TILE_BASE
    @exit:
    rts

    page_values:
    .byte 0
    .byte ((1 * PAGE_SIZE) >> 9)
    .byte ((2 * PAGE_SIZE) >> 9)
    .byte ((3 * PAGE_SIZE) >> 9)
.endproc

; ---------------------------------------------------------------
; Clear Screen buffer VRAM using VERA's 32-bit cache feature
; A: colour to clear the screen with
; use whatever current page is set in VERA::ADDR
; ---------------------------------------------------------------
.proc clear_page
    tay
    ldx color_lookup_shifted,y

    ; Enable DCSEL mode 2 to allow cache operations
    set_dcsel 2

    ; Enable cache writing 
    lda #(1<<6)             ;#VERA::FX_CTRL_FLAGS::CACHE_WRITE_EN
    sta FX_CTRL               ;FX_CTRL

    ; Change DCSEL to mode 6 for cache write operations
    set_dcsel 6

    ; Prepare the 32-bit cache with the colour to clear the screen with
    stx FX_CACHE_L
    stx FX_CACHE_M
    stx FX_CACHE_H
    stx FX_CACHE_U

    ; Set address auto-increment to 4 bytes
    lda #VERA::INC4
    tsb VERA::ADDR + 2

    ; This will write 8 pixels at a time (32 bits)
    ldx #>(PAGE_SIZE / 4)   ; High Byte
    ldy #<(PAGE_SIZE / 4)   ; Low Byte

    clear_loop:
        stz VERA::DATA0     ; Write the 32-bit cache to VRAM (4 bytes of zeros) 
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
    lda #$01
    sta CHARCOLOR ; set background to black (0) and text to white (1)

    lda #147
    jsr CHROUT  ; use the kernal clear screen function lol

    lda #14 ; lowercase mode
    jsr CHROUT

    rts
.endproc 

; ---------------------------------------------------------------
; Set palette
; A: palette number
; ---------------------------------------------------------------
.proc set_palette
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
        pha ; save red
        inc24 palette_addr

        lda palette_addr
        ldx palette_addr+1
        ldy palette_addr+2
        jsr read_byte       ; get green and blue
        sta VERA::DATA0     ; set green and blue
        inc24 palette_addr

        pla ; get red
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
    CACHE_OPS = 300      ; *must* be 30 or larger
    ; todo: consider doing interleaved copies? might not look as good though
    ; note: if we can gain some cycles elsewhere, maybe we can increase this number
    
    ; set up source
    tay
    lda page_base_lo,y
    sta VERA::ADDR
    lda page_base_hi,y
    sta VERA::ADDR + 1
    lda page_base_bank,y
    ora #VERA::INC1
    sta VERA::ADDR + 2
    
    ; Set up destination
    lda #1
    sta VERA::CTRL
    lda page_base_lo,x
    sta VERA::ADDR
    lda page_base_hi,x
    sta VERA::ADDR + 1
    lda page_base_bank,x
    ora #VERA::INC4    ; Keep INC4 for 4-byte aligned writes
    sta VERA::ADDR + 2
    
    ; Set DCSEL to 2 for cache operations
    lda #(2<<1)
    sta VERA::CTRL
    
    ; Pre-load control values
    ldx #(1<<5)        ; Cache fill enable
    ldy #(1<<6)        ; Cache write enable
    
    ; Main copy loop
    lda #<(PAGE_SIZE/(CACHE_OPS * 4))
    sta copy_counter
    copy_loop:
        .repeat CACHE_OPS
            ; Fill cache (4 bytes)
            stx FX_CTRL    ; Enable cache fill (X already has 1<<5)
            lda VERA::DATA0
            lda VERA::DATA0
            lda VERA::DATA0
            lda VERA::DATA0
            
            ; Write cache (4 bytes)
            sty FX_CTRL    ; Enable cache write (Y already has 1<<6)
            stz VERA::DATA1 ; Write all 4 bytes (mask = 0)
        .endrepeat

        dec copy_counter
        jne copy_loop
    
    ; Cleanup
    stz FX_CTRL
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
    ldx state+engine::draw_page
    set_addr_page

    ; todo: use 4 lookup tables instead of adding from one
    ; add Y offset to address
    ldy line_info+line_data::y1
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
    lda line_info+line_data::x1+1
    lsr
    lda line_info+line_data::x1
    ror
    clc
    adc VERA::ADDR
    sta VERA::ADDR
    sta vtemp
    lda #0 ; no need to add to the high byte as it will always be 0
    adc VERA::ADDR+1
    sta VERA::ADDR+1
    sta vtemp+1
    lda #0
    adc VERA::ADDR+2
    ora #VERA::INC1
    sta VERA::ADDR+2                ; y * 160 + x1/2

    ; set control port 0
    stz VERA::CTRL
    sta VERA::ADDR+2

    lda vtemp
    sta VERA::ADDR
    lda vtemp+1
    sta VERA::ADDR+1
.endmacro

; ---------------------------------------------------------------
; Draw a horizontal line
; uses data saved in line_info+line_data struct
; ---------------------------------------------------------------
.proc draw_line
    ; *** if y > 199 then return
    lda line_info+line_data::y1+1
    beq check_y_low
    rts
    check_y_low:
    lda line_info+line_data::y1
    cmp #<SCREEN_HEIGHT ; line is not likely to be less than -56 (256 - 200)
    bcc y_not_high
    rts
    y_not_high:

    ; *** if x2 < 0 then return
    lda line_info+line_data::x2+1
    bpl x2_not_neg
    rts
    x2_not_neg:

    ; *** if x1 < 0 then x1 = 0
    lda line_info+line_data::x1+1
    bpl x1_not_neg
    stz line_info+line_data::x1
    stz line_info+line_data::x1+1
    x1_not_neg:

    ; *** if x1 > 319 then return
    lda line_info+line_data::x1+1
    beq x1_ok
    lda line_info+line_data::x1
    cmp #<SCREEN_WIDTH
    bcc x1_ok
    rts
    x1_ok:

    ; *** if x2 > 319 then x2 = 319 
    lda line_info+line_data::x2+1
    beq x2_ok
    cmp #>SCREEN_WIDTH
    bcc x2_ok
    bne clamp_x2
    lda line_info+line_data::x2
    cmp #<SCREEN_WIDTH
    bcc x2_ok
    clamp_x2:
    lda #<(SCREEN_WIDTH - 1)
    sta line_info+line_data::x2
    lda #>(SCREEN_WIDTH - 1)
    sta line_info+line_data::x2+1
    x2_ok:

    ; macro
    setup_line 

    lda polygon_info+polygon_data::color
    ; *** if color < $10 then draw solid line
    cmp #$10
    jcc draw_line_solid

    ; *** if color == $11 then copy from page 0 to current page
    cmp #$11
    jeq draw_line_copy

    ; *** if color == $10 then use transparency
    cmp #$10
    jeq draw_line_trans 

    end:
    rts
.endproc

.proc draw_line_copy ; todo: use FX for this
    ; set VERA::DATA1 to page 0
    lda #1
    sta VERA::CTRL
    ldy line_info+line_data::y1
    lda y160_lookup_lo,y
    sta VERA::ADDR
    lda y160_lookup_hi,y
    sta VERA::ADDR+1

    lda line_info+line_data::x1+1
    lsr
    lda line_info+line_data::x1
    ror ; x1 / 2
    clc
    adc VERA::ADDR
    and #%11111100      ; Align to 4-byte boundary
    sta VERA::ADDR
    lda #0
    adc VERA::ADDR+1
    sta VERA::ADDR+1
    lda #0
    adc VERA::ADDR+2
    and #$FE
    ora #VERA::INC1
    sta VERA::ADDR+2    ; y * 160 + x1/2
    
    stz VERA::CTRL ; set DATA PORT 0 and DCSEL to 0

    ; set 4 byte increment
    lda #VERA::INC4
    tsb VERA::ADDR+2

    ; set leading mask
    lda line_info+line_data::x1
    and #$07
    tay
    lda mask_leading,y
    sta leading_mask

    ; set trailing mask
    lda line_info+line_data::x2
    and #$07
    tay
    lda mask_trailing,y
    sta trailing_mask

    ; calc start byte = x1 / 8
    lsr16_addr line_info+line_data::x1, 3

    ; calc end byte = x2 / 8
    lsr16_addr line_info+line_data::x2, 3

    ; Set DCSEL to 2 for cache operations
    set_dcsel 2

    ; if start byte == end byte, then draw a short line
    lda line_info+line_data::x1
    cmp line_info+line_data::x2
    bne long_line
    ; draw short line
    lda #(1<<5) ; Cache fill enable
    sta FX_CTRL
    lda VERA::DATA1
    lda VERA::DATA1
    lda VERA::DATA1
    lda VERA::DATA1
    lda #(1<<6) ; Cache write enable
    sta FX_CTRL
    lda leading_mask
    ora trailing_mask
    sta VERA::DATA0
    bra end_line

    long_line:
    ; calc number of loops
    sec
    lda line_info+line_data::x2
    sbc line_info+line_data::x1
    bcs :+
    rts        ; Exit if subtraction would underflow
    :
    dec
    bpl :+
    rts        ; Exit if decrement would underflow
    :
    sta num_loops

    ; draw leading mask
    lda #(1<<5) ; Cache fill enable
    sta FX_CTRL
    lda VERA::DATA1
    lda VERA::DATA1
    lda VERA::DATA1
    lda VERA::DATA1
    lda #(1<<6) ; Cache write enable
    sta FX_CTRL
    lda leading_mask
    sta VERA::DATA0

    ldx num_loops
    beq no_middle
    line_loop:
        lda #(1<<5) ; Cache fill enable
        sta FX_CTRL
        lda VERA::DATA1
        lda VERA::DATA1
        lda VERA::DATA1
        lda VERA::DATA1
        lda #(1<<6) ; Cache write enable
        sta FX_CTRL
        lda #0
        sta VERA::DATA0
        dex
        bne line_loop

    no_middle:
    ; draw trailing mask
    lda #(1<<5) ; Cache fill enable
    sta FX_CTRL
    lda VERA::DATA1
    lda VERA::DATA1
    lda VERA::DATA1
    lda VERA::DATA1
    lda #(1<<6) ; Cache write enable
    sta FX_CTRL
    lda trailing_mask
    sta VERA::DATA0

    end_line:
    ; reset fx control
    set_dcsel 2
    stz FX_CTRL
    set_dcsel 0
    rts
.endproc

.proc draw_line_trans ; todo: use FX for this
    lsr16_addr line_info+line_data::x1, 1
    lsr16_addr line_info+line_data::x2, 1
    ldx line_info+line_data::x1                 ; 3 cycles
    ; set the pixels
    loop_trans:
        lda VERA::DATA1 
        ora #$88
        sta VERA::DATA0
        inx                                     ; 2 cycles
        cpx line_info+line_data::x2             ; 3 cycles
        bcc loop_trans                          ; 2 cycles
    rts
.endproc

.proc draw_line_solid
    set_dcsel 2
    lda #%01000000   ; set cache write enable
    sta FX_CTRL
    set_dcsel 6      ; set cache mode

    ; set 4 byte increment
    lda VERA::ADDR+2
    and #$01        ; mask out the low bit
    ora #VERA::INC4 ; set port 0 auto increment to 4 bytes
    sta VERA::ADDR+2

    ; set leading mask
    lda line_info+line_data::x1
    and #$07
    tay
    lda mask_leading,y
    sta leading_mask

    ; set trailing mask
    lda line_info+line_data::x2
    and #$07
    tay
    lda mask_trailing,y
    sta trailing_mask

    ; todo: this can be optimised further
    ; calc start byte = x1 / 8
    lsr16_addr line_info+line_data::x1, 3

    ; calc end byte = x2 / 8
    lsr16_addr line_info+line_data::x2, 3

    ; if start byte == end byte, then draw a short line
    lda line_info+line_data::x1 ; can be removed?
    cmp line_info+line_data::x2
    bne long_line
    lda leading_mask
    ora trailing_mask
    sta VERA::DATA0
    bra end_line

    long_line:
    ; calc number of loops
    sec
    lda line_info+line_data::x2
    sbc line_info+line_data::x1
    bcs :+
    rts        ; Exit if subtraction would underflow
    :
    dec
    bpl :+
    rts        ; Exit if decrement would underflow
    : ; can be removed?
    ;sta num_loops
    tax

    ; draw leading mask
    lda leading_mask
    sta VERA::DATA0

    txa
    beq no_middle
    line_loop:
        stz VERA::DATA0
        dex
        bne line_loop

    no_middle:
    ; draw trailing mask
    lda trailing_mask
    sta VERA::DATA0

    end_line:
    ; reset fx control
    set_dcsel 2
    stz FX_CTRL
    ;set_dcsel 0
    stz VERA::CTRL

    rts
.endproc

; ---------------------------------------------------------------
; Draw a pixel using the line_info struct
; ---------------------------------------------------------------
.proc draw_pixel
    ; set control port 0
    stz VERA::CTRL

    ; set the address start
    ldx state+engine::draw_page
    set_addr_page

    ; set vera address to y*160
    ldy line_info+line_data::y1
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
    ; now add x1/2 to the address
    lsr line_info+line_data::x1+1
    ror line_info+line_data::x1
    add16_addr VERA::ADDR, line_info+line_data::x1
    lda #0
    adc VERA::ADDR+2
    sta VERA::ADDR+2

    lda line_info+line_data::x1
    and #$01
    beq @even
    lda VERA::DATA0
    and #$F0
    ora line_info+line_data::color
    sta VERA::DATA0
    bra @done

    @even:
    ldx line_info+line_data::color
    lda color_lookup_hi,x
    sta vtemp
    lda VERA::DATA0
    and #$0F
    ora vtemp
    sta VERA::DATA0

@done:
    rts
.endproc