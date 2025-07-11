; ---------------------------------------------------------------
; bank.s
; AnotherX16 - Commander X16 port of Another World
; ---------------------------------------------------------------

.macpack longbranch

; X16 and CBM includes
.include "cx16.inc"
.include "cbm_kernal.inc"

; Project includes
.include "main.inc"
.include "bank.inc"
.include "macros.inc"
.include "resource.inc"

.segment "ZEROPAGE"
    byte:       .res 1
    addr_ptr:   .res 2
    page_buffer: .res 2

.segment "BSS"
    start_bank: .res 1

.segment "CODE"

; ---------------------------------------------------------------
; Read a byte from banked memory
; Use a 24-bit address to read a byte from banked memory
; A: offset low
; X: offset high
; Y: offset upper
; Returns: A = byte read
; ---------------------------------------------------------------
.proc read_byte
    ; this method: 54 cycles
    ; sta byte

    ; ; calculate bank number = ((X >> 5) & 0x07) + (Y << 3) + 1
    ; ; an even more simplified version: ((Y << 3) | (X >> 5)) + 1
    ; tya
    ; asl_a 3
    ; sta RAM_BANK
    ; txa
    ; lsr_a 5
    ; ora RAM_BANK

    ; inc ; assumes first resource bank is $01
    ; sta RAM_BANK

    ; ; calculate address
    ; txa
    ; and #$1F
    ; clc
    ; adc #$A0
    ; sta load+2

    ; ; read byte
    ; ldx byte
    ; load:
    ; lda $A000,x ; Self-modifying code for direct addressing

    ; this method: 49 cycles
    sta addr_ptr

    ; Calculate bank number
    txa
    lsr
    lsr
    lsr
    lsr
    lsr ; divide by 32 (8kb bank)
    sta RAM_BANK
    tya
    asl
    asl
    asl ; multiply by 8
    ora RAM_BANK ; combine with bank number
    inc ; assumes first resource bank is $01
    sta RAM_BANK

    ; Calculate address
    txa
    and #$1F
    ora #$A0 ; add base address for resources
    sta addr_ptr+1
    lda (addr_ptr) ; Load the byte from the calculated address

    rts
.endproc

; ---------------------------------------------------------------
; Write a byte to banked memory
; Y: start bank number
; A: offset low
; X: offset high
; byte: byte to write
; ---------------------------------------------------------------
.proc write_byte
    pha ; offset low
    
    ; Calculate bank number
    tya
    asl_a 3
    sta RAM_BANK
    txa
    lsr_a 5
    ora RAM_BANK

    inc ; assumes first resource bank is $01
    sta RAM_BANK
    
    ; Calculate address
    txa
    and #$1F
    clc
    adc #$A0
    sta store+2
    
    ; Write the byte
    plx            ; Get offset low back
    lda byte       ; Get byte to write
    
    store:
    sta $A000,x    ; Self-modifying code for direct addressing
    rts
.endproc

; ---------------------------------------------------------------
; Read a word from banked memory (Little Endian)
; A: offset low, X: offset high, Y: offset upper
; Returns: A = first byte, X = second byte  
; ---------------------------------------------------------------
.proc read_word 
; most cases: 80 cycles (actually not much faster than read_byte)
; stp
;     offset_low = byte
;     output_low = addr_ptr
;     output_high = addr_ptr+1
;     sta offset_low ; Save original offset

;     ; Calculate and set bank number
;     tya
;     asl_a 3
;     sta RAM_BANK
;     txa
;     lsr_a 5
;     ora RAM_BANK

;     inc ; assumes first resource bank is $01
;     sta RAM_BANK
    
;     ; calculate address
;     txa
;     and #$1F
;     clc
;     adc #$A0
;     sta load+2

;     ; read byte
;     ldx offset_low
;     load:
;     lda $A000,x ; Self-modifying code for direct addressing
;     sta output_low ; Store first byte

;     cpx #$FF
;     beq boundary_check ; are we at the end of the bank or page?

;     ; *** Common case (most reads are within the same bank)
;     inx
;     lda load+2
;     sta load_next+2 ; Store next offset
;     load_next:
;     lda $A000,x ; Self-modifying code for direct addressing
;     tax
;     lda output_low ; Load first byte back
; stp
;     rts

;     boundary_check:
;     lda load+2
;     cmp #$BF ; Check if we're at the bank boundary
;     bne page_boundary ; we're not - it's just a page boundary

;     ; *** bank boundary - least common case
;     inc RAM_BANK ; Increment the bank number
;     ldx $A000 ; Read the next byte from the new bank
;     lda output_low ; Load first byte back
; stp
;     rts

;     ; *** page boundary - less common case
;     page_boundary:
;     ; We're at a page boundary, so we need to read the next byte
;     lda load+2
;     inc
;     sta load_next_page+2 ; Store the next offset
;     load_next_page:
;     ldx $A000
;     lda output_low ; Load first byte back
; stp
    ; this method: 70 cycles (worst case 74)
    output_low = byte
    sta addr_ptr

    ; Calculate bank number
    txa
    lsr
    lsr
    lsr
    lsr
    lsr ; divide by 32 (8kb bank)
    sta RAM_BANK
    tya
    asl
    asl
    asl ; multiply by 8
    ora RAM_BANK ; combine with bank number
    inc ; assumes first resource bank is $01
    sta RAM_BANK

    ; Calculate address
    txa
    and #$1F
    ora #$A0 ; add base address for resources
    sta addr_ptr+1
    
    ; *** Read first byte
    lda (addr_ptr) ; Load the byte from the calculated address
    sta output_low

    ; *** check bank boundary
    inc addr_ptr
    bne read_next_byte
    inc addr_ptr+1
    lda addr_ptr+1
    cmp #$C0            ; Check if beyond bank boundary ($C0 > $BF)
    bne read_next_byte  ; Still within current bank
    inc RAM_BANK
    lda #$A0
    sta addr_ptr+1     ; Reset to first page of new bank
    
    read_next_byte:
    lda (addr_ptr) ; Load the next byte from the calculated address
    tax
    lda output_low ; Load first byte back

    rts
.endproc

; reads 256 bytes into page_buffer
.proc read_page
    sta byte
    ; Calculate bank number
    txa
    lsr
    lsr
    lsr
    lsr
    lsr ; divide by 32 (8kb bank)
    sta RAM_BANK
    tya
    asl
    asl
    asl ; multiply by 8
    ora RAM_BANK ; combine with bank number
    inc ; assumes first resource bank is $01
    sta RAM_BANK
    ; Calculate address
    txa
    and #$1F
    ora #$A0 ; add base address for resources
    sta load_byte+2
    
    ; Initialize buffer pointer
    lda page_buffer
    sta buff_ptr+1
    lda page_buffer+1
    sta buff_ptr+2

    ; Initialize buffer index
    ldy #0                  ; Buffer index (0-255)
    ldx byte                ; Byte offset within current page
    read_loop:
        load_byte:
        lda $A000,x         ; Self-modifying code
        buff_ptr:
        sta $0000,y         ; Store in page buffer (self-modifying code)
        iny
        beq read_complete   ; Done when Y wraps from 255 to 0

        inx
        bne read_loop       ; Continue if not at page boundary
        inc load_byte+2     ; Advance to next page ($A1, $A2, ..., $AF)
        lda load_byte+2
        cmp #$C0            ; Check if beyond bank boundary ($C0 > $BF)
        bne read_loop       ; Still within current bank
        inc RAM_BANK        ; Reached bank boundary - switch to next bank
        lda #$A0
        sta load_byte+2     ; Reset to first page of new bank
        bra read_loop
        
    read_complete:
    rts
.endproc