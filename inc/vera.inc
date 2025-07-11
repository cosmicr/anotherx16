; ---------------------------------------------------------------
; vera.inc
; AnotherX16 - Commander X16 port of Another World
; ---------------------------------------------------------------

.globalzp vtemp

.global init_vera, clear_page, clear_text_screen, clear_page, clear_text_screen
.global set_addr_page, set_palette, set_vera_page
.global copy_page_screen, copy_screen_page, copy_page, copy_page_scroll
.global draw_line, draw_pixel
.global color_lookup_shifted
.global copy_active

.global page_base_lo, page_base_hi, page_base_bank

.global page_0_y160_lo,page_0_y160_hi,page_0_y160_bank,page_1_y160_lo,page_1_y160_hi,page_1_y160_bank
.global page_2_y160_lo,page_2_y160_hi,page_2_y160_bank,page_3_y160_lo,page_3_y160_hi,page_3_y160_bank
; ---------------------------------------------------------------
; Screen resolution constants
; ---------------------------------------------------------------
SCREEN_WIDTH    = 256 ; fixed
SCREEN_HEIGHT   = 192 ; (180 to 192) but looks worse the lower you go
; page size is screen width * screen height / 2 to the nearest multiple of 2048
PAGE_SIZE       = ((320 * SCREEN_HEIGHT) / 2 + 2047) & $FFFFF800 ; = $7800 (30720 bytes)

FX_CTRL = $9F29
FX_MULT = $9F2C
FX_CACHE_L = $9F29
FX_CACHE_M = $9F2A
FX_CACHE_H = $9F2B
FX_CACHE_U = $9F2C

.macro set_dcsel mode
    lda #(mode<<1)
    sta VERA::CTRL
.endmacro


; Sets base address of the VERA to the specified page base
; X: page number
.macro set_addr_page
    lda page_base_lo,x
    sta VERA::ADDR
    lda page_base_hi,x
    sta VERA::ADDR + 1
    lda page_base_bank,x
    sta VERA::ADDR + 2
.endmacro


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
    
    ; Get current draw page and Y position
    ldx state+engine::draw_page
    ldy line_info+line_data::y1
    
    ; Select correct page-specific lookup tables based on draw_page
    cpx #0
    bne @not_page_0
    lda page_0_y160_lo,y
    sta VERA::ADDR
    lda page_0_y160_hi,y
    sta VERA::ADDR+1
    stz VERA::ADDR+2
    bra @do_x_add
@not_page_0:
    cpx #1
    bne @not_page_1
    lda page_1_y160_lo,y
    sta VERA::ADDR
    lda page_1_y160_hi,y
    sta VERA::ADDR+1
    stz VERA::ADDR+2
    bra @do_x_add
@not_page_1:
    cpx #2
    bne @page_3
    lda page_2_y160_lo,y
    sta VERA::ADDR
    lda page_2_y160_hi,y
    sta VERA::ADDR+1
    lda page_2_y160_bank,y
    bra @continue
@page_3:
    lda page_3_y160_lo,y
    sta VERA::ADDR
    lda page_3_y160_hi,y
    sta VERA::ADDR+1
    lda page_3_y160_bank,y
@continue:
    sta VERA::ADDR+2

@do_x_add:
    ; x1 divided by 2 and add to address
    lda line_info+line_data::x1
    lsr
    clc
    adc VERA::ADDR
    sta VERA::ADDR
    sta vtemp
    lda #0
    adc VERA::ADDR+1
    sta VERA::ADDR+1
    sta vtemp+1
    lda #0
    adc VERA::ADDR+2
    
    ; Set the increment mode
    ora #VERA::INC1
    sta VERA::ADDR+2
    
    ; set control port 0 with same address
    stz VERA::CTRL
    sta VERA::ADDR+2
    lda vtemp
    sta VERA::ADDR
    lda vtemp+1
    sta VERA::ADDR+1
.endmacro