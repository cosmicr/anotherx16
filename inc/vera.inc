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
SCREEN_WIDTH    = 320
SCREEN_HEIGHT   = 200
; page size is screen width * screen height / 2 to the nearest multiple of 2048
PAGE_SIZE       = ((SCREEN_WIDTH * SCREEN_HEIGHT) / 2 + 2047) & $FFFFF800

; ---------------------------------------------------------------
; Helper macro to clear VRAM using the 32-bit cache
; Prerequisite: VERA::ADDR is set to the start of the page
; Input: X = High Byte, Y = Low Byte of loop counter
; ---------------------------------------------------------------
.macro CLEAR_LOOP
    lda #%00000000          ; Set the mask to 0 (no mask)

    clear_loop:
        sta VERA::DATA0     ; Write the 32-bit cache to VRAM (4 bytes of zeros) 
        dey                 ; Decrement low byte of loop counter
        bne clear_loop      ; Continue loop if low byte is not zero
        dex                 ; Decrement high byte of loop counter
        bne clear_loop      ; Continue loop if high byte is not zero
.endmacro
