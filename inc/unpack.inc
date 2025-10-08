.global src_offset, src_size

.global read_byte, write_byte, print_string
.global bytekiller_unpack
.global unpacker_start

.globalzp uc_dst_offset

; ---------------------------------------------------------------
; Decrement a 32-bit value
; ---------------------------------------------------------------
.macro dec32 variable
    lda variable
    bne :+++
    lda variable+1
    bne :++
    lda variable+2
    bne :+
    dec variable+3
    :
    dec variable+2
    :
    dec variable+1
    :
    dec variable
.endmacro

.ifndef dec24
.macro dec24 addr
    sec
    lda addr
    sbc #1
    sta addr
    lda addr+1
    sbc #0
    sta addr+1
    lda addr+2
    sbc #0
    sta addr+2
.endmacro
.endif
