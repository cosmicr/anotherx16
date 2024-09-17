; ---------------------------------------------------------------
; vera.inc
; AnotherX16 - Commander X16 port of Another World
; ---------------------------------------------------------------

.global init_vera, clear_page, clear_text_screen, clear_page, clear_text_screen
.global set_addr_page, set_palette, set_vera_page
.global copy_page_screen, copy_screen_page, copy_page
.global draw_line, draw_pixel

; ---------------------------------------------------------------
; Screen resolution constants
; ---------------------------------------------------------------
SCREEN_WIDTH    = 320 ; fixed30
SCREEN_HEIGHT   = 192 ; (180 to 192) but looks worse the lower you go
; page size is screen width * screen height / 2 to the nearest multiple of 2048
PAGE_SIZE       = ((320 * SCREEN_HEIGHT) / 2 + 2047) & $FFFFF800

