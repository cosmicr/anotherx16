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
    sta byte

    ; calculate bank number = ((X >> 5) & 0x07) + (Y << 3) + 1
    ; an even more simplified version: ((Y << 3) | (X >> 5)) + 1
    tya
    asl_a 3
    sta RAM_BANK
    txa
    lsr_a 5
    ora RAM_BANK

    inc ; assumes first resource bank is $01
    sta RAM_BANK

    ; calculate address
    txa
    and #$1F
    clc
    adc #$A0
    sta load+2

    ; read byte
    ldx byte
    load:
    lda $A000,x ; Self-modifying code for direct addressing

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
    offset_low = byte
    output_low = addr_ptr
    output_high = addr_ptr+1
    sta offset_low ; Save original offset

    ; Calculate and set bank number
    tya
    asl_a 3
    sta RAM_BANK
    txa
    lsr_a 5
    ora RAM_BANK

    inc ; assumes first resource bank is $01
    sta RAM_BANK
    
    ; calculate address
    txa
    and #$1F
    clc
    adc #$A0
    sta load+2

    ; read byte
    ldx offset_low
    load:
    lda $A000,x ; Self-modifying code for direct addressing
    sta output_low ; Store first byte

    cpx #$FF
    beq boundary_check ; are we at the end of the bank or page?

    ; *** Common case (most reads are within the same bank)
    inx
    lda load+2
    sta load_next+2 ; Store next offset
    load_next:
    lda $A000,x ; Self-modifying code for direct addressing
    tax
    lda output_low ; Load first byte back
    rts

    boundary_check:
    lda load+2
    cmp #$BF ; Check if we're at the bank boundary
    bne page_boundary ; we're not - it's just a page boundary

    ; *** bank boundary - least common case
    inc RAM_BANK ; Increment the bank number
    ldx $A000 ; Read the next byte from the new bank
    lda output_low ; Load first byte back
    rts

    ; *** page boundary - less common case
    page_boundary:
    ; We're at a page boundary, so we need to read the next byte
    lda load+2
    inc
    sta load_next_page+2 ; Store the next offset
    load_next_page:
    ldx $A000
    lda output_low ; Load first byte back

    rts
.endproc