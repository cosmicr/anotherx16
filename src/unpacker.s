; ---------------------------------------------------------------
; unpacker.s
; AnotherX16 - Commander X16 port of Another World
; Unpack BANK files to DATA files
; ---------------------------------------------------------------

.macpack longbranch

; X16 and CBM includes
.include "cx16.inc"
.include "cbm_kernal.inc"

.include "main.inc"
.include "bank.inc"
.include "resource.inc"
.include "unpack.inc"
.include "macros.inc"

.segment "ZEROPAGE"
    memlist_ptr:            .res 2

.segment "BSS"
    row:                    .res 1
    col:                    .res 1
    offset:                 .res 2

.segment "DATA"
    str_bank:               .asciiz "bank00"
    str_data:               .asciiz "data00"
    mod:                    .byte 0
    do_patch:               .byte 1

.segment "RODATA"
    str_memlist_bin:        .asciiz "memlist.bin"
    str_memlist_bin_end:
    str_error_memlist_bin:  .asciiz "error loading memlist.bin - is the game data present?"
    str_error_bankload:     .asciiz "error loading bank file - is the game data present?"
    str_loading:            .byte "loading banks...", $0D, $00
    str_unpacking:          .byte "unpacking 146 data files - this may take some time, please wait...", $0D, $00
    str_unpack_complete:    .byte "unpacking complete.", $0D, $0D, $00
    str_remove_copy_protection: .byte "do you want to remove copy protection? (y/n) ", $00
    str_data_missing:      .byte "data files have not yet been unpacked, do you wish to proceed? (y/n) ", $00
    str_press_enter:      .asciiz "press <return> to begin game..."

    patch_data:
        .byte $07, $0C, $ED, $00, $00, $00, $00 ; JMP $0CED + NOPs

hex_table:
    .byte '0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f'

.segment "CODE"

; ---------------------------------------------------------------
; Main program
; ---------------------------------------------------------------
.proc unpacker_start
    jsr CINT ; reset the screen

    jsr check_data_exists
    jcc done ; all data files exist, continue
    ; data files missing
    clc
    ldx #2
    ldy #2
    jsr PLOT
    ldx #<str_data_missing
    ldy #>str_data_missing
    jsr print_string
    ; get key
    jsr BASIN
    cmp #'y'
    bne done
    
    ; ask for patch option
    clc
    ldx #3
    ldy #2
    jsr PLOT
    stz do_patch
    ldx #<str_remove_copy_protection
    ldy #>str_remove_copy_protection
    jsr print_string
    ; get key
    jsr BASIN
    cmp #'y'
    bne continue
    lda #1
    sta do_patch
    continue:

    clc
    ldx #5
    ldy #2
    jsr PLOT

    jsr load_memlist
    bcs end

    ; show unpacking message
    clc
    ldx #6
    ldy #2
    jsr PLOT

    ldx #<str_unpacking
    ldy #>str_unpacking
    jsr print_string

    jsr unpack_data
    bcs end

end:
    wai
    jsr $FF4A ; close all files
    wai
    jsr CLRCHN

    LDA #1           ; clear_status function
    JSR $FEAB        ; extapi call
    
    clc
    ldx #14
    ldy #2
    jsr PLOT

    lda #146 ; reverse off
    jsr CHROUT
    ldx #<str_unpack_complete
    ldy #>str_unpack_complete
    jsr print_string
    ldx #<str_press_enter
    ldy #>str_press_enter
    jsr print_string
    jsr BASIN
done:
    wai
    rts
.endproc

; ---------------------------------------------------------------
; Print null-terminated string
; X = string address low, Y = string address high
; ---------------------------------------------------------------
.proc print_string
    stx temp
    sty temp+1
    ldy #0
    string_loop:
    lda (temp),y
    beq string_end
    jsr CHROUT
    iny
    bne string_loop
    string_end:
    rts
.endproc

; ---------------------------------------------------------------
; Reverse endianess of 32-bit values by swapping 0 and 3, 1 and 2 bytes
; ---------------------------------------------------------------
.proc reverse_bytes
    ; A contains the offset into the resource structure
    tay                 ; Save the offset in Y
    tax                 ; Save the offset in X

    ; Load the 32-bit value
    lda (memlist_ptr),y
    sta temp
    iny 
    lda (memlist_ptr),y
    sta temp+1
    iny
    lda (memlist_ptr),y 
    sta temp+2 
    iny 
    lda (memlist_ptr),y
    sta temp+3

    ; Swap the bytes
    txa
    tay
    lda temp+3
    sta (memlist_ptr),y
    iny
    lda temp+2
    sta (memlist_ptr),y
    iny
    lda temp+1
    sta (memlist_ptr),y
    iny
    lda temp
    sta (memlist_ptr),y

    rts
.endproc

; ---------------------------------------------------------------
; Load memlist
; ---------------------------------------------------------------
.proc load_memlist
    ; show unpacking message
    ldx #<str_loading
    ldy #>str_loading
    jsr print_string

    ; Set filename
    lda #(str_memlist_bin_end - str_memlist_bin)
    ldy #>str_memlist_bin
    ldx #<str_memlist_bin
    jsr SETNAM

    ; Open file
    lda #1 ; file #1
    ldx #8 ; device
    ldy #0 ; secondary address = CBM_READ
    jsr OPEN

    ; Set input channel
    ldx #1 ; file #1
    jsr CHKIN ; set input channel

    ; Read file
    lda #<resource_table
    sta memlist_ptr
    lda #>resource_table
    sta memlist_ptr+1
    ldx #0  ; current entry
    ldy #0  ; current byte in entry
    read_loop:
        ; Read an entry
        jsr ACPTR   ; get a byte
        sta (memlist_ptr),y
        iny
        cpy #.sizeof(resource)
        bne read_loop
        ; Next entry
        clc
        lda #<.sizeof(resource)
        adc memlist_ptr
        sta memlist_ptr
        lda #>.sizeof(resource)
        adc memlist_ptr+1
        sta memlist_ptr+1
        ldy #0  ; reset byte counter
        inx
        cpx #MAX_RESOURCES
        bne read_loop
    
    ; Next byte must be $FF
    jsr ACPTR
    cmp #$FF
    jne error
    jsr CLRCHN
    lda #1
    jsr CLOSE

    ; Reverse large values endianess
    lda #<resource_table
    sta memlist_ptr
    lda #>resource_table
    sta memlist_ptr+1
    ldx #0
    reverse_loop:
        ; Reverse the bytes
        phx
        lda #resource::offset
        jsr reverse_bytes
        lda #resource::compressed
        jsr reverse_bytes
        lda #resource::uncompressed
        jsr reverse_bytes
        plx
        ; Next entry
        clc
        lda #<.sizeof(resource)
        adc memlist_ptr
        sta memlist_ptr
        lda #>.sizeof(resource)
        adc memlist_ptr+1
        sta memlist_ptr+1
        ldy #0  ; reset byte counter
        inx     ; next entry
        cpx #MAX_RESOURCES
        bne reverse_loop

    ; Finished
    jsr CLRCHN
    lda #1
    jsr CLOSE
    rts ; finished

    ; Error handling
    error:
    jsr CLRCHN
    lda #1
    jsr CLOSE ; close the file, if it was open
    jsr CINT
    ldx #<str_error_memlist_bin
    ldy #>str_error_memlist_bin
    jsr print_string
    error_end:
    sec
    rts
.endproc

; ---------------------------------------------------------------
; Convert a number to hex
; ---------------------------------------------------------------
.proc dec_to_hex
    tay
    lda hex_table,y
    
    rts
.endproc

; ---------------------------------------------------------------
; Load bankfile into banked memory
; ---------------------------------------------------------------
.proc load_bank
    ; A contains the bank number
    jsr dec_to_hex
    sta str_bank+5

    lda #7          ; Filename length
    ldx #<str_bank
    ldy #>str_bank
    jsr SETNAM

    lda #1          ; Logical file number
    ldx #8          ; Device number (8 = sd card)
    ldy #2          ; Secondary address (2 = load into address for LOAD)
    jsr SETLFS

    lda #1
    sta RAM_BANK

    lda #0          ; 0 = RAM
    ldx #<BANK::RAM
    ldy #>BANK::RAM
    jsr LOAD

    bcs error

    rts

    error:
    jsr CLRCHN
    jsr CINT
    ldx #<str_error_bankload
    ldy #>str_error_bankload
    jsr print_string
    sec
    rts
.endproc

; ---------------------------------------------------------------
; Setup the datafile name
; ---------------------------------------------------------------
.proc setup_dataname
    ; A contains the bank number
    pha
    lsr
    lsr
    lsr
    lsr
    jsr dec_to_hex
    sta str_data+4 ; upper nibble
    pla
    and #$0F
    jsr dec_to_hex
    sta str_data+5 ; lower nibble

    rts
.endproc

; ---------------------------------------------------------------
; Load a 32-bit value from resource structure
; A: offset into resource structure, results in temp (low to high)
; ---------------------------------------------------------------
.proc get_resource_value
    tay
    lda (memlist_ptr),y
    sta temp
    iny
    lda (memlist_ptr),y
    sta temp+1
    iny
    lda (memlist_ptr),y
    sta temp+2
    iny
    lda (memlist_ptr),y
    sta temp+3
    rts
.endproc

; ---------------------------------------------------------------
; Compare sizes, returns carry if different
; ---------------------------------------------------------------
.proc compare_sizes
    lda #resource::uncompressed
    jsr get_resource_value
    lda temp
    sta temp+4
    lda temp+1
    sta temp+5
    lda temp+2
    sta temp+6
    lda temp+3
    sta temp+7

    lda #resource::compressed
    jsr get_resource_value
     
    ldx #0
    lda #0
    compare_loop:
        lda temp,x
        cmp temp+4,x
        bne values_different
        inx
        cpx #4
        bne compare_loop
        ; All bytes matched
        clc
        rts

    values_different:
    sec
    rts

.endproc

; ---------------------------------------------------------------
; Unpack compressed data
; ---------------------------------------------------------------
.proc unpack_compressed
    ; A contains patch flag
    pha

    ; get the resource offset
    lda #resource::offset
    jsr get_resource_value
    lda temp
    sta src_offset
    lda temp+1
    sta src_offset+1
    lda temp+2
    sta src_offset+2
    lda temp+3
    sta src_offset+3

    ; get the resource size
    lda #resource::compressed
    jsr get_resource_value
    lda temp
    sta src_size
    lda temp+1
    sta src_size+1
    lda temp+2
    sta src_size+2
    lda temp+3
    sta src_size+3

    jsr bytekiller_unpack

    ; save the file
    lda #7          ; Filename length
    ldx #<str_data
    ldy #>str_data
    jsr SETNAM

    lda #2          ; Logical file number
    ldx #8          ; Device number (8 = sd card)
    ldy #1          ; Secondary address (1 = open for writing)
    jsr SETLFS

    jsr OPEN

    ldx #2
    jsr CHKOUT ; set output channel

    lda #1
    sta RAM_BANK

    stz offset
    stz offset+1

    loop:
        pla
        pha
        beq normal_byte
        ; data15
        lda offset
        cmp #$4F
        bne normal_byte
        lda offset+1
        cmp #$0D
        bne normal_byte

        ; copy protection patch
        ldx #0
        patch_loop:
            lda patch_data,x
            jsr CIOUT
            inx
            cpx #7
            bne patch_loop
        clc
        lda uc_dst_offset
        adc #6 ; we already advanced 1 in the previous loop
        sta uc_dst_offset
        lda uc_dst_offset+1
        adc #0
        sta uc_dst_offset+1
        lda uc_dst_offset+2
        adc #0
        sta uc_dst_offset+2

        clc
        lda offset
        adc #6
        sta offset
        lda offset+1
        adc #0
        sta offset+1
        bra continue

        ; normal writes
        normal_byte:
        ; get a byte
        lda uc_dst_offset
        ldx uc_dst_offset+1
        ldy uc_dst_offset+2
        jsr read_byte

        ; write the byte
        jsr CIOUT

        inc offset
        bne :+
        inc offset+1
        :

        continue:
        ; increment the address
        inc uc_dst_offset
        bne :+
        inc uc_dst_offset+1
        bne :+
        inc uc_dst_offset+2
        :
        ; check for end of file
        lda uc_dst_offset+2
        cmp #$07
        bne loop
        lda uc_dst_offset+1
        cmp #$E0
        bne loop
        lda uc_dst_offset
        cmp #$00
        bne loop

    pla ; remove the patch flag from the stack
    wai ; wait for the file to be written?
    lda #2
    jsr CLOSE
    wai
    jsr CLRCHN

    rts
.endproc


; ---------------------------------------------------------------
; Copy uncompressed data to file
; ---------------------------------------------------------------
.proc unpack_uncompressed
    lda #7          ; Filename length
    ldx #<str_data
    ldy #>str_data
    jsr SETNAM

    lda #2          ; Logical file number
    ldx #8          ; Device number (8 = sd card)
    ldy #1          ; Secondary address (1 = open for writing)
    jsr SETLFS

    jsr OPEN

    ldx #2
    jsr CHKOUT ; set output channel

    lda #1
    sta RAM_BANK

    ; get the resource size
    lda #resource::uncompressed
    jsr get_resource_value
    lda temp
    sta temp+4
    lda temp+1
    sta temp+5
    lda temp+2
    sta temp+6
    lda temp+3
    sta temp+7

    ; get the resource offset
    lda #resource::offset
    jsr get_resource_value

    ; save the data
    loop:
        ; get a byte
        lda temp
        ldx temp+1
        ldy temp+2
        jsr read_byte

        ; write the byte
        jsr CIOUT

        ; increment the address
        inc temp
        bne :+
        inc temp+1
        bne :+
        inc temp+2
        bne :+
        inc temp+3
        :
        ; check for end of file
        dec32 temp+4
        lda temp+4
        ora temp+5
        ora temp+6
        ora temp+7
        bne loop

    wai ; wait for the file to be written?
    lda #2
    jsr CLOSE
    wai
    jsr CLRCHN

    rts
.endproc

; ---------------------------------------------------------------
; Unpack data
; ---------------------------------------------------------------
.proc unpack_data
    ; draw the progress bar outline
    clc
    ldx #10
    ldy #2
    jsr PLOT
    lda #117
    jsr CHROUT
    ldx #73
    top_loop:
        lda #99
        jsr CHROUT
        dex
        bne top_loop
    lda #105
    jsr CHROUT
    clc
    ldx #12
    ldy #2
    jsr PLOT
    lda #106
    jsr CHROUT
    ldx #73
    bottom_loop:
        lda #99
        jsr CHROUT
        dex
        bne bottom_loop
    lda #107
    jsr CHROUT
    clc
    ldx #11
    ldy #2
    jsr PLOT
    lda #98 ; |
    jsr CHROUT
    clc
    ldx #11
    ldy #76
    jsr PLOT
    lda #98 ; |
    jsr CHROUT
    clc
    ldx #11
    ldy #3
    jsr PLOT
    
    lda #<resource_table
    sta memlist_ptr
    lda #>resource_table
    sta memlist_ptr+1
    ldx #0
    resource_loop:
        ; setup the datafile name
        txa
        jsr setup_dataname

        ; save the current row and col
        phx
        sec
        jsr PLOT
        stx row
        sty col
        clc
        ldx #9
        ldy #3
        jsr PLOT
        
        lda #146 ; reverse off
        jsr CHROUT
        ; display the resource number
        ldx #<str_data
        ldy #>str_data
        jsr print_string
        clc
        ldx row
        ldy col
        jsr PLOT
        
        plx

        lda mod
        beq half
        ; full
        lda #157 ; backspace
        jsr CHROUT
        lda #18 ; reverse on
        jsr CHROUT
        lda #' '
        jsr CHROUT
        bra continue
        half:
        jsr CHROUT
        lda #146 ; reverse off
        jsr CHROUT
        lda #161
        jsr CHROUT
        continue:
        lda #1
        eor mod
        sta mod

        ; skip any with zero size
        ldy #resource::uncompressed
        lda (memlist_ptr),y
        iny
        ora (memlist_ptr),y
        iny
        ora (memlist_ptr),y
        iny
        ora (memlist_ptr),y
        beq next_entry

        phx ; save resource number

        ; get the resource bank number and load the bank file
        ldy #resource::bank_file
        lda (memlist_ptr),y
        jsr load_bank
        bcs error

        ; if the resource is compressed, unpack it
        jsr compare_sizes
        bcc not_compressed

        compressed:
        plx
        phx
        cpx #$15 ; data15 contains the copy protection
        bne regular_unpack

        lda #1
        and do_patch
        bra do_unpack

        regular_unpack:
        lda #0

        do_unpack:
        jsr unpack_compressed
        plx
        bra next_entry

        not_compressed:
        jsr unpack_uncompressed
        plx

        ; Next entry
        next_entry:

        clc
        lda #<.sizeof(resource)
        adc memlist_ptr
        sta memlist_ptr
        lda #>.sizeof(resource)
        adc memlist_ptr+1
        sta memlist_ptr+1
        ldy #0  ; reset byte counter
        inx
        cpx #MAX_RESOURCES
        jne resource_loop

    rts

    error:
    pla
    rts
.endproc

.proc check_data_exists
    ldx #1 ; skip 0
    resource_loop:
        ; setup the datafile name
        ; todo: this is hacky, we should extract 0 byte files too
        cpx #$34
        beq next ; skip data34
        cpx #$85
        beq next ; skip data85
        cpx #$86
        beq next ; skip data86
        cpx #$87
        beq next ; skip data87
        cpx #$8F
        beq next ; skip data8F
        phx
        txa
        jsr setup_dataname
        lda #7          ; Filename length
        ldx #<str_data
        ldy #>str_data
        jsr SETNAM
        lda #1              ; Logical file number
        ldx #8              ; Device number (SD card)
        ldy #1              ; Secondary address 1 (load to file's header address)
        jsr SETLFS
        
        ; Try to load with verify (A=1)
        lda #1              ; Verify mode
        ldx #$00            ; Load address (doesn't matter for verify)
        ldy #$02
        jsr LOAD
        jsr READST
        cmp #$42
        beq file_not_found
        
        ; File exists, check next
        plx
        next:
        inx
        cpx #MAX_RESOURCES
        jne resource_loop
        clc                 ; All files exist
        rts
        
    file_not_found:
        plx
        ; ldx #<str_data
        ; ldy #>str_data
        ; jsr print_string
        sec                 ; Indicate failure
        rts
.endproc

; How to patch data15 to remove copy protection:
; Original code:
; 0D4F: 0C 00 3F 02              CCTRL 0, 63, 2
; 0D53: 01 02 37                 MOV Var2, Var55
; ...
; Patched code:
; 0D4F: 07 0C ED                 JMP 0CED           ; Jump to success handler
; 0D52: 00 00                    NOP                ; Padding

; consider CJMP(EQ, Var254, 1, 0CED)  ; Var254 as dev override - ie if 1, jump to success handler