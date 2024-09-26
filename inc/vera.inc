; ---------------------------------------------------------------
; vera.inc
; AnotherX16 - Commander X16 port of Another World
; ---------------------------------------------------------------

.global init_vera, clear_page, clear_text_screen, clear_page, clear_text_screen
.global set_addr_page, set_palette, set_vera_page
.global copy_page_screen, copy_screen_page, copy_page
.global draw_line, draw_pixel, draw_quad
.global color_lookup_shifted

; ---------------------------------------------------------------
; Screen resolution constants
; ---------------------------------------------------------------
SCREEN_WIDTH    = 320 ; fixed
SCREEN_HEIGHT   = 192 ; (180 to 192) but looks worse the lower you go
; page size is screen width * screen height / 2 to the nearest multiple of 2048
PAGE_SIZE       = ((320 * SCREEN_HEIGHT) / 2 + 2047) & $FFFFF800

FX_CTRL = $9F29
FX_CACHE_L = $9F29
FX_CACHE_M = $9F2A
FX_CACHE_H = $9F2B
FX_CACHE_U = $9F2C

FX_X_INCR_L = $9F29
FX_X_INCR_H = $9F2A
FX_Y_INCR_L = $9F2B
FX_Y_INCR_H = $9F2C

FX_X_POS_L = $9F29
FX_X_POS_H = $9F2A
FX_Y_POS_L = $9F2B
FX_Y_POS_H = $9F2C

FX_X_POS_S = $9F29
FX_Y_POS_S = $9F2A
FX_POLY_FILL_L = $9F2B
FX_POLY_FILL_H = $9F2C

.macro set_dcsel mode
    lda #(mode<<1)
    sta VERA::CTRL
.endmacro