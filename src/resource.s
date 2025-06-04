; ---------------------------------------------------------------
; init.s
; AnotherX16 - Commander X16 port of Another World
; ---------------------------------------------------------------

.macpack longbranch

; X16 and CBM includes
.include "cx16.inc"
.include "cbm_kernal.inc"

; Project includes
.include "main.inc"
.include "resource.inc"
.include "macros.inc"

.segment "DATA"
    next_bank:              .byte RESOURCE_BANK_START
    next_offset:            .byte 0, 0, 0, 0
    resource_filename:      .asciiz "data"  ; "bank" for compressed data
                            .res 2
    dummy_loc:      .res 320

.export next_bank

.segment "BSS"
    resource_table:         .res MAX_RESOURCES * .sizeof(resource)

.segment "RODATA"
    str_error_invalid_resource_num: .asciiz "invalid resource number"
    str_error_memlist_bin:          .asciiz "error opening memlist.bin"
    str_memlist_bin:                .asciiz "memlist.bin"
    str_memlist_bin_size:
    str_end:

.segment "CODE"

; ---------------------------------------------------------------
; Helper macro for reversing endianess
; ---------------------------------------------------------------
.macro reverse_bytes
        clc
        adc work+2
        sta work
        lda #0
        adc work+3
        sta work+1
        lda (work),y
        sta temp+3
        iny
        lda (work),y
        sta temp+2
        iny
        lda (work),y
        sta temp+1
        iny
        lda (work),y
        sta temp
        lda temp+3
        sta (work),y
        dey
        lda temp+2
        sta (work),y
        dey
        lda temp+1
        sta (work),y
        dey
        lda temp
        sta (work),y
.endmacro

; ---------------------------------------------------------------
; Load the MEMLIST.BIN file for resources
; ---------------------------------------------------------------
.proc init_resources
    ; Attempt to open the file
    lda #(str_memlist_bin_size - str_memlist_bin)
    ldy #>str_memlist_bin
    ldx #<str_memlist_bin
    jsr SETNAM
    lda #1 ; file #1
    ldx #8 ; device
    ldy #0 ; secondary address = CBM_READ
    jsr SETLFS ; set logical file system
    jsr OPEN
    jsr READST
    jne error ; error opening file
    ldx #1 ; file #1
    jsr CHKIN ; set input channel

    ; Read the file into memory
    lda #<resource_table ; low byte of address
    sta work ; store the low byte of the address in work
    lda #>resource_table ; high byte of address
    sta work+1 ; store the high byte of the address in work+1
    ldx #0 ; low byte of index
    ldy #0 ; high byte of index
    @read_loop:
        ; phx
        ; phy
        jsr ACPTR ; get a byte from the file
        ; ply
        ; plx
        sta (work),y ; store the byte in resource_table
        iny
        cpy #.sizeof(resource)
        bne @read_loop

        clc
        lda #.sizeof(resource)
        adc work
        sta work
        lda #0
        adc work+1
        sta work+1
        ldy #0
        inx 
        cpx #MAX_RESOURCES ; check if we've read all resources
        bne @read_loop
    ; next byte must be $ff
    jsr ACPTR
    cmp #$ff
    jne error
    jsr CLRCHN      ; Reset channels back to default
    lda #1
    jsr CLOSE ; close the file

    ; reverse the bytes for the big endian values
    lda #<resource_table
    sta work+2
    lda #>resource_table
    sta work+3
    ldx #0
    @reverse_loop:
        lda #resource::offset
        reverse_bytes
        lda #resource::compressed
        reverse_bytes
        lda #resource::uncompressed
        reverse_bytes
        clc
        lda work+2
        adc #.sizeof(resource)
        sta work+2
        lda #0
        adc work+3
        sta work+3
        inx
        cpx #MAX_RESOURCES
        jne @reverse_loop

    ; set the status on each resource to 0
    lda #<resource_table
    sta work
    lda #>resource_table
    sta work+1
    ldx #0
    @status_loop:
        lda #0
        sta (work)
        clc
        lda work
        adc #.sizeof(resource)
        sta work
        lda #0
        adc work+1
        sta work+1
        inx
        cpx #MAX_RESOURCES
        bne @status_loop

    ; ldx #0
    ; jsr CHKIN
    rts

    error:
        jsr CLRCHN ; clear the channel
        lda #1
        jsr CLOSE ; close the file, if it was open
        jsr CINT
        ldx #0
        @error_loop:
            lda str_error_memlist_bin,x
            beq @error_end
            jsr CHROUT
            inx
            bne @error_loop
        @error_end:
    rts
.endproc

; ---------------------------------------------------------------
; Load resource
; A: resource number
; Returns: A = low byte of address, X = high byte of address
; ---------------------------------------------------------------
.proc load_resource
    ; check if resource number is valid
    cmp #MAX_RESOURCES
    jcs invalid_resource_error

    ; concatenate the filename with the resource number in hex
    pha
    lsr
    lsr
    lsr
    lsr         ; high nibble
    jsr nibble_to_hex
    sta resource_filename + 4
    pla
    pha
    and #$0F    ; low nibble
    jsr nibble_to_hex
    sta resource_filename + 5
    stz resource_filename + 6
    pla
    jra get_offset
    nibble_to_hex:
        cmp #10
        bcc @digit
        adc #6
    @digit:
        adc #'0'
        rts

    ; get offset into resource info table and store in work
    get_offset:

    tax ; counter
    lda #<resource_table
    sta work
    lda #>resource_table
    sta work+1
    @table_loop: ; todo: use a lookup table 
        clc
        lda #.sizeof(resource)
        adc work
        sta work
        lda #0
        adc work+1
        sta work+1
        dex
        bne @table_loop

    ; check if resource is already loaded
    ldy #resource::status
    lda (work),y
    jne resource_loaded

    ; if the resource is a bitmap, then load it into vera instead
    ldy #resource::rtype
    lda (work),y
    cmp #2 ; bitmap type
    jeq load_bitmap

    ; save the current bank to the resource ; probably not necessary
    ldy #resource::rank
    lda next_bank
    sta (work),y

    ; load the file directly from disk
    ; todo: load and unpack from original datafiles
    ; todo: if we use the disk for loading then we could probably still have music!
    lda next_bank
    sta RAM_BANK
    lda #0
    ldx #8
    ldy #2
    jsr SETLFS
    lda #7
    ldx #<resource_filename
    ldy #>resource_filename
    jsr SETNAM
    ; offset is $A000 + next_offset % $2000
    lda next_offset+1
    and #$1F
    ora #$A0
    sta work+3
    lda next_offset
    sta work+2
    ldx work+2
    ldy work+3
    lda #0
    jsr LOAD

    ; update the resource status
    ldy #resource::status
    lda #1
    sta (work),y
    ; update the location of the resource
    ldy #resource::pointer
    lda next_offset
    sta (work),y
    iny
    lda next_offset+1
    sta (work),y
    iny
    lda next_offset+2
    sta (work),y
    iny
    lda next_offset+3
    sta (work),y

    ; next_offset += resource::uncompressed
    ldy #resource::uncompressed
    lda (work),y
    clc
    adc next_offset
    sta next_offset
    iny
    lda (work),y
    adc next_offset+1
    sta next_offset+1
    iny
    lda (work),y
    adc next_offset+2
    sta next_offset+2
    iny
    lda (work),y
    adc next_offset+3
    sta next_offset+3
    lda RAM_BANK
    sta next_bank

    ; note: what if we run out of memory?
    ; note: 512kb should be enough, but maybe check if we have 512kb or 2mb of memory?

    resource_loaded:
    lda work
    ldx work + 1
    rts

    invalid_resource_error:
        ldx #0
        jsr CINT
        @error_loop:
            lda str_error_invalid_resource_num,x
            beq @error_end
            jsr CHROUT
            inx
            bne @error_loop
        @error_end:

    rts
.endproc

.macro check_set_bit byte, mask, check_bit, set_bit
    lda byte
    bit #(1 << check_bit) ; check the bit
    beq :+   ; if not set, skip
    lda #(1 << set_bit) ; set the lefg bit
    ora mask
    sta mask
    :
.endmacro


.proc load_bitmap
    mask = temp
    byte = temp+1
    counter = temp+2

    ; Setup file parameters
    lda #7          ; Filename length
    ldx #<resource_filename
    ldy #>resource_filename
    jsr SETNAM

    lda #2          ; Logical file number
    ldx #8          ; Device number (8 = disk)
    ldy #0          ; Secondary address (2 = load)
    jsr SETLFS
    
    ; Open the file
    jsr OPEN
    jsr READST
    jne error

    ldx #2          ; Logical file number
    jsr CHKIN       ; Set input channel

    ; Set up VERA for writing
    stz VERA::CTRL  ; Control register = 0
    stz VERA::ADDR  ; Low byte of address
    stz VERA::ADDR+1; Middle byte of address
    lda #(VERA::INC1); Auto-increment by 1
    sta VERA::ADDR+2; High byte + increment mode

    ; Plane 0 - bit 0
    stz counter ; skip counter for scaling
    ; loop 8000 times
    ldy #0
    ldx #0
    plane_0_loop:
        jsr ACPTR
        sta byte

        ; chunk 0: bit 7 -> left bit 0, bit 6 -> right bit 0
        stz mask ; clear the mask
        check_set_bit byte, mask, 7, 4
        check_set_bit byte, mask, 6, 0
        lda mask
        sta VERA::DATA0

        ; chunk 1: bit 5 -> left bit 0, bit 4 -> right bit 0
        stz mask ; clear the mask
        check_set_bit byte, mask, 5, 4
        check_set_bit byte, mask, 4, 0
        lda mask
        sta VERA::DATA0

        ; chunk 2: bit 3 -> left bit 0, bit 2 -> right bit 0
        stz mask
        check_set_bit byte, mask, 3, 4
        check_set_bit byte, mask, 2, 0
        lda mask
        sta VERA::DATA0

        ; chunk 3: bit 1 -> left bit 0, bit 0 -> right bit 0
        stz mask
        check_set_bit byte, mask, 1, 4
        check_set_bit byte, mask, 0, 0
        lda mask
        sta VERA::DATA0

        inx
        cpx #40
        jne plane_0_loop
        ldx #0
        iny
        cpy #192
        jne plane_0_loop

    ldx #0
    bottom_0_loop: ; skip bottom 8 rows
        jsr ACPTR
        jsr ACPTR
        inx
        cpx #160 ; 320/2
        bne bottom_0_loop

    ; Planes 1-3
    .repeat 3, p
    .scope

    ; reset VERA address
    lda #1
    sta VERA::CTRL  ; data port 1
    stz VERA::ADDR
    stz VERA::ADDR+1
    lda #(VERA::INC1); Auto-increment by 1
    sta VERA::ADDR+2; High byte + increment mode

    stz VERA::CTRL  ; data port 0
    stz VERA::ADDR
    stz VERA::ADDR+1
    lda #(VERA::INC1); Auto-increment by 1
    sta VERA::ADDR+2; High byte + increment mode

    ; loop 8000 times
    ldx #0
    ldy #0
    plane_loop:
        jsr ACPTR
        sta byte

        ; chunk 0: bit 7 -> left bit 0, bit 6 -> right bit 0
        lda VERA::DATA0
        sta mask
        check_set_bit byte, mask, 7, (p+5)
        check_set_bit byte, mask, 6, (p+1)
        lda mask
        sta VERA::DATA1

        ; chunk 1: bit 5 -> left bit 0, bit 4 -> right bit 0
        lda VERA::DATA0
        sta mask
        check_set_bit byte, mask, 5, (p+5)
        check_set_bit byte, mask, 4, (p+1)
        lda mask
        sta VERA::DATA1

        ; chunk 2: bit 3 -> left bit 0, bit 2 -> right bit 0
        lda VERA::DATA0
        sta mask
        check_set_bit byte, mask, 3, (p+5)
        check_set_bit byte, mask, 2, (p+1)
        lda mask
        sta VERA::DATA1

        ; chunk 3: bit 1 -> left bit 0, bit 0 -> right bit 0
        lda VERA::DATA0
        sta mask
        check_set_bit byte, mask, 1, (p+5)
        check_set_bit byte, mask, 0, (p+1)
        lda mask
        sta VERA::DATA1

        inx
        cpx #40
        jne plane_loop
        ldx #0
        iny
        cpy #192
        jne plane_loop

    ldx #0
    bottom_loop: ; skip bottom 8 rows
        jsr ACPTR
        jsr ACPTR
        inx
        cpx #160 ; 320/2
        bne bottom_loop

    .endscope
    .endrepeat

    ; now scale the bitmap by copying
    ; reset VERA address
    lda #1
    sta VERA::CTRL  ; data port 1
    stz VERA::ADDR
    stz VERA::ADDR+1
    lda #(VERA::INC1); Auto-increment by 1
    sta VERA::ADDR+2; High byte + increment mode

    stz VERA::CTRL  ; data port 0
    stz VERA::ADDR
    stz VERA::ADDR+1
    lda #(VERA::INC1); Auto-increment by 1
    sta VERA::ADDR+2; High byte + increment mode

    ldx #0
    ldy #0
    ; AB CD EF GH IJ
    ; 11 11 01 11 10 
    scale_loop:
        ; copy 2 bytes
        lda VERA::DATA1 ; AB
        sta VERA::DATA0 ; AB
        lda VERA::DATA1 ; CD
        sta VERA::DATA0 ; CD
        ; now copy just the low nibble
        lda VERA::DATA1 ; EF
        asl
        asl
        asl
        asl
        sta byte ; F0
        lda VERA::DATA1 ; GH
        pha ; save the byte
        clc
        lsr
        lsr
        lsr
        lsr
        ora byte ; combine the nibbles F0 and 0G
        sta VERA::DATA0 ; FG
        pla ; restore the byte GH
        asl
        asl
        asl
        asl
        sta byte ; H0
        lda VERA::DATA1 ; IJ
        clc
        lsr
        lsr
        lsr
        lsr
        ora byte ; combine the nibbles H0 and 0I
        sta VERA::DATA0

        inx
        cpx #32
        bne scale_loop
        ; set the address to the next row
        ldx #0
        dest_loop:
            lda VERA::DATA0
            inx
            cpx #32
            bne dest_loop
        ldx #0
        iny
        cpy #192
        bne scale_loop
      
done:
    jsr CLRCHN
    lda #2
    jsr CLOSE
    ldx #0
    jsr CHKIN
    
    rts

    error:
        lda #2
        jsr CLOSE ; close the file, if it was open
        jsr CINT
        ldx #0
        @error_loop:
            lda str_error_memlist_bin,x
            beq @error_end
            jsr CHROUT
            inx
            bne @error_loop
        @error_end:

.endproc


        