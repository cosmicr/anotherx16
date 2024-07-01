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

.segment "DATA"
    next_bank:              .byte RESOURCE_BANK_START
    next_offset:            .byte 0, 0, 0, 0
    resource_table:         .res MAX_RESOURCES * .sizeof(resource)
    resource_filename:      .asciiz "data"  ; "bank" for compressed data
                            .res 2


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
    jsr OPEN
    jcs error ; error opening file ; todo: this is not how to check for errors
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
        phx
        phy
        jsr ACPTR ; get a byte from the file
        ply
        plx
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

    rts

    error:
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
    jmp get_offset
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
    @table_loop:
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

    ; if rtype is less than 2 then skip sound and music for now
    ldy #resource::rtype
    lda (work),y
    cmp #2
    jcc resource_loaded ; todo: do we need to set return ptr to null?

    ; save the current bank to the resource
    ldy #resource::rank
    lda next_bank
    sta (work),y

    ; load the file directly from disk
    ; todo: load and unpack from original datafiles
    ; todo: if we use the disk for loading then we could probably still have music!
    lda next_bank
    sta $00
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
    ;    next_offset -= BANK_RAM_SIZE; ; high byte & $1f = size%8192
    ;    next_bank++; 
    stz next_offset+2
    stz next_offset+3
    lda $00
    sta next_bank

    ; todo: what if we run out of memory?
    ; todo: check if we have 512kb or 2mb of memory

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