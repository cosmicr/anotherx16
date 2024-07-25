; ---------------------------------------------------------------
; vera.inc
; AnotherX16 - Commander X16 port of Another World
; ---------------------------------------------------------------

.global init_vera, clear_page, clear_text_screen, clear_page
.global set_addr_page, set_palette, set_vera_page
.global copy_page_screen, copy_screen_page, copy_page
.global draw_line, draw_pixel

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

; ---------------------------------------------------------------
; Helper macro to set VERA::ADDR to the start of a page directly
; page 0: 0x00000
; page 1: 0x07D00 (32000)
; page 2: 0x0DAC0 (56000)
; page 3: 0x13880 (80000)
; ---------------------------------------------------------------
.macro SET_PAGE page
    .if page = 0
        stz VERA::ADDR
        stz VERA::ADDR + 1
        stz VERA::ADDR + 2
    .elseif page = 1
        lda #<32000
        sta VERA::ADDR
        lda #>32000
        sta VERA::ADDR + 1
        stz VERA::ADDR + 2
    .elseif page = 2
        lda #<56000
        sta VERA::ADDR
        lda #>56000
        sta VERA::ADDR + 1
        stz VERA::ADDR + 2
    .elseif page = 3
        lda #<80000
        sta VERA::ADDR
        lda #>80000
        sta VERA::ADDR + 1
        lda #1
        sta VERA::ADDR + 2
    .endif
.endmacro