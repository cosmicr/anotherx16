; ---------------------------------------------------------------
; unpack.s - ByteKiller Decompressor
; ---------------------------------------------------------------

.macpack longbranch

.include "cx16.inc" 
.include "cbm_kernal.inc"
.include "main.inc"
.include "unpack.inc"
.include "macros.inc"

; --- Zero Page Variables ---
.segment "EXTZP" : zeropage
    uc_src_offset:      .res 3
    uc_dst_offset:      .res 3
    uc_size:            .res 4
    uc_crc:             .res 4
    uc_bits:            .res 4
    carry:              .res 1
    count:              .res 2
    bits:               .res 2
    length_offset:      .res 2

.segment "BSS"
    src_offset:         .res 4
    src_size:           .res 4

.segment "DATA"
    str_crc_failed:     .byte " verify failed",0

.segment "CODE"

.proc read_byte_uc
    lda uc_src_offset
    ldx uc_src_offset+1
    ldy uc_src_offset+2
    jsr read_byte
    rts
.endproc

; ---------------------------------------------------------------
.proc bytekiller_unpack
    ; get the uncompressed size
    ; last 4 bytes of the source data
    clc
    lda src_offset
    adc src_size
    sta uc_src_offset
    lda src_offset+1
    adc src_size+1
    sta uc_src_offset+1
    lda src_offset+2
    adc src_size+2
    sta uc_src_offset+2
    ; read the uncompressed size - this is kinda redundant since we already have the size from memlist.bin
    dec24 uc_src_offset
    jsr read_byte_uc
    sta uc_size
    dec24 uc_src_offset
    jsr read_byte_uc
    sta uc_size+1
    dec24 uc_src_offset
    jsr read_byte_uc
    sta uc_size+2
    dec24 uc_src_offset
    jsr read_byte_uc
    sta uc_size+3

    ; setup the destination offset $07 E0 00 (504kb) work backwards
    lda #$00
    sta uc_dst_offset
    lda #$E0
    sta uc_dst_offset+1
    lda #$07
    sta uc_dst_offset+2

    ; read the crc 4 bytes
    dec24 uc_src_offset
    jsr read_byte_uc
    sta uc_crc
    dec24 uc_src_offset
    jsr read_byte_uc
    sta uc_crc+1
    dec24 uc_src_offset
    jsr read_byte_uc
    sta uc_crc+2
    dec24 uc_src_offset
    jsr read_byte_uc
    sta uc_crc+3

    ; next 4 bytes are bits
    dec24 uc_src_offset
    jsr read_byte_uc
    sta uc_bits
    dec24 uc_src_offset
    jsr read_byte_uc
    sta uc_bits+1
    dec24 uc_src_offset
    jsr read_byte_uc
    sta uc_bits+2
    dec24 uc_src_offset
    jsr read_byte_uc
    sta uc_bits+3

    ; uc.crc = uc.crc ^ uc.bits;
    ldx #0
    crc_bits_loop:
        lda uc_crc,x
        eor uc_bits,x
        sta uc_crc,x
        inx
        cpx #4
        bne crc_bits_loop

    ; loop while uc_size > 0 
    loop:
        ; get next bit for operation type
        jsr next_bit ; 0 = short, 1 = long
        bne long_op
        ; short operation
        jsr next_bit ; 0 = literal, 1 = reference
        bne short_ref
        lda #3
        ldx #0
        jsr copy_literal
        jmp next
        short_ref:
        lda #8
        ldx #2
        ldy #0
        jsr copy_reference
        jmp next

        long_op:
        lda #2
        jsr get_bits ; 0: ref 9,3 1: ref 10,4 2: ref 12,get(8)+1, 3: lit 8,8
        lda bits
        cmp #0
        beq long_ref_9_3
        cmp #1
        beq long_ref_10_4
        cmp #2
        beq long_ref_12_x
        lda #8
        ldx #8
        jsr copy_literal
        jmp next
        long_ref_9_3: ; (case 0)
        lda #9
        ldx #3
        ldy #0
        jsr copy_reference
        jmp next
        long_ref_10_4: ; (case 1)
        lda #10
        ldx #4
        ldy #0
        jsr copy_reference
        jmp next
        long_ref_12_x: ; (case 2)
        lda #8
        jsr get_bits
        inc bits
        bne :+
        inc bits+1
        :
        ldx bits
        ldy bits+1
        lda #12
        jsr copy_reference

        next:
        lda uc_size+3
        ora uc_size+2
        ora uc_size+1
        ora uc_size
        jne loop
    
    ; verify crc
    lda uc_crc
    ora uc_crc+1
    ora uc_crc+2
    ora uc_crc+3
    beq done

    ; crc error, show message:
    clc
    ldx #14
    ldy #3
    jsr PLOT
    ldx #<str_crc_failed
    ldy #>str_crc_failed
    jsr print_string

    done:
    rts
.endproc

.proc next_bit
    ; get the lower bit
    stz carry
    lda uc_bits
    and #1
    sta carry

    ; shift bits right by one
    lsr uc_bits+3
    ror uc_bits+2
    ror uc_bits+1
    ror uc_bits

    ; if bits are zero, reload
    lda uc_bits+3
    ora uc_bits+2
    ora uc_bits+1
    ora uc_bits
    jne done ; if not zero, done

    ; reload 32 bits - lsb to msb going in reversed order
    dec24 uc_src_offset
    jsr read_byte_uc
    sta uc_bits ; lowest byte
    dec24 uc_src_offset
    jsr read_byte_uc
    sta uc_bits+1
    dec24 uc_src_offset
    jsr read_byte_uc
    sta uc_bits+2
    dec24 uc_src_offset
    jsr read_byte_uc
    sta uc_bits+3

    ; recalc the crc
    ldx #0
    crc_loop:
        lda uc_crc,x
        eor uc_bits,x
        sta uc_crc,x
        inx
        cpx #4
        bne crc_loop
    
    ; get the lower bit again
    stz carry
    lda uc_bits
    and #1
    sta carry
    
    ; shift bits right by one and set high bit
    lsr uc_bits+3
    ror uc_bits+2
    ror uc_bits+1
    ror uc_bits

    ; set the high bit for tracking when we need to reload
    lda #$80
    ora uc_bits+3
    sta uc_bits+3

    done:
    lda carry
    rts
.endproc

; A: number of bits to get
; Returns in bits (2 bytes)
.proc get_bits
    tax
    stz bits
    stz bits+1
    loop:
        phx
        asl bits        ; Shift low byte left
        rol bits+1      ; Rotate high byte left, pulling in carry
        jsr next_bit    ; A = next bit (0 or 1)
        ora bits        ; Combine with existing bits
        sta bits
        plx
        dex
        bne loop
    
    rts
.endproc

; A: number of bits, X: length offset
.proc copy_literal
    ; get number of bytes to write
    stx length_offset
    stz length_offset+1

    jsr get_bits

    clc
    lda length_offset
    adc bits
    sta length_offset
    lda length_offset+1
    adc bits+1
    sta length_offset+1

    inc length_offset ; add 1
    bne :+
    inc length_offset+1
    :

    ; uc_size -= length_offset
    sec
    lda uc_size
    sbc length_offset
    sta uc_size
    lda uc_size+1
    sbc length_offset+1
    sta uc_size+1
    lda uc_size+2
    sbc #0
    sta uc_size+2
    lda uc_size+3
    sbc #0
    sta uc_size+3

    ; check if uc_size < 0
    bpl size_ok
    ; if less than zero, adjust count
    clc
    lda uc_size
    adc length_offset
    sta length_offset
    lda uc_size+1
    adc length_offset+1
    sta length_offset+1
    stz uc_size
    stz uc_size+1
    stz uc_size+2
    stz uc_size+3
    size_ok:

    ; copy length_offset bytes from source to dest 
    lda length_offset
    ora length_offset+1
    beq done
    copy_loop:
        lda #8
        jsr get_bits
        lda bits
        sta byte
        
        dec24 uc_dst_offset
        lda uc_dst_offset
        ldx uc_dst_offset+1
        ldy uc_dst_offset+2
        jsr write_byte

        sec
        lda length_offset
        sbc #1
        sta length_offset
        lda length_offset+1
        sbc #0
        sta length_offset+1
        
        lda length_offset
        ora length_offset+1
        bne copy_loop
    done:
    rts
.endproc

; A: number of bits, X: length offset low, Y: length offset high
.proc copy_reference
    stx length_offset
    sty length_offset+1

    ; get the offset (into bits var)
    jsr get_bits

    ; uc_size -= length
    sec
    lda uc_size
    sbc length_offset
    sta uc_size
    lda uc_size+1
    sbc length_offset+1
    sta uc_size+1
    lda uc_size+2
    sbc #0
    sta uc_size+2
    lda uc_size+3
    sbc #0
    sta uc_size+3

    ; check if uc_size < 0
    bpl size_ok
    ; if less than zero, adjust count
    clc
    lda length_offset
    adc uc_size
    sta length_offset
    lda length_offset+1
    adc uc_size+1
    sta length_offset+1
    stz uc_size
    stz uc_size+1
    stz uc_size+2
    stz uc_size+3
    size_ok:

    ; read and write bytes loop
    lda length_offset
    ora length_offset+1
    beq done
    copy_loop:
        dec24 uc_dst_offset
        clc
        lda uc_dst_offset
        adc bits ; offset
        sta temp
        lda uc_dst_offset+1
        adc bits+1
        sta temp+1
        lda uc_dst_offset+2
        adc #0
        sta temp+2

        lda temp
        ldx temp+1
        ldy temp+2
        jsr read_byte
        sta byte
    
        lda uc_dst_offset
        ldx uc_dst_offset+1
        ldy uc_dst_offset+2
        jsr write_byte
        
        sec
        lda length_offset
        sbc #1
        sta length_offset
        lda length_offset+1
        sbc #0
        sta length_offset+1

        lda length_offset
        ora length_offset+1
        bne copy_loop
    done:
    rts
.endproc