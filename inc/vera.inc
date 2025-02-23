; ---------------------------------------------------------------
; vera.inc
; AnotherX16 - Commander X16 port of Another World
; ---------------------------------------------------------------

.global init_vera, clear_page, clear_text_screen, clear_page, clear_text_screen
.global set_addr_page, set_palette, set_vera_page
.global copy_page_screen, copy_screen_page, copy_page
.global draw_line, draw_pixel
.global color_lookup_shifted
.global copy_active

.global page_base_lo, page_base_hi, page_base_bank

; ---------------------------------------------------------------
; Screen resolution constants
; ---------------------------------------------------------------
SCREEN_WIDTH    = 320 ; fixed
SCREEN_HEIGHT   = 190 ; (180 to 192) but looks worse the lower you go
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