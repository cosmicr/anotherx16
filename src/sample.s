; ---------------------------------------------------------------
; sample.s
; AnotherX16 - Commander X16 port of Another World
; ---------------------------------------------------------------

; X16 and CBM includes
.include "cx16.inc"
.include "cbm_kernal.inc"

; Project includes
.include "sample.inc"
.include "main.inc"
.include "macros.inc"
.include "resource.inc"
.include "bank.inc"

.segment "ZEROPAGE"
    stemp:              .res 4
    sample_resource:    .res 2

.segment "DATA"
    audio_ready:    .byte 0

    channels:
    channel1:         .tag CHANNEL
    channel2:         .tag CHANNEL
    channel3:         .tag CHANNEL
    channel4:         .tag CHANNEL

.segment "RODATA"
    ; formula is: int(128 * freq / 48828.125)
    pcm_freq_table:
        .byte 9, 9, 10, 10, 11, 11, 11, 12
        .byte 12, 13, 14, 14, 15, 15, 16, 17
        .byte 18, 19, 19, 20, 22, 22, 23, 24
        .byte 26, 28, 29, 30, 32, 34, 36, 38
        .byte 40, 43, 45, 48, 51, 54, 58, 62

.segment "CODE"

;;; TODO: *** MASSIVE CHANGES HERE, GO BACK TO ONE CHANNEL ***
; use the 2-channel stereo mode, or even just 1 channel and keep it simple?
; consider megadrive audio for music (if we have the speed)

; ---------------------------------------------------------------
; Initialize audio system
; ---------------------------------------------------------------
.proc init_audio
    ; Initialize VERA audio
    stz VERA::PCM::DATA
    lda #$0F                        ; Set volume to zero initially
    sta VERA::PCM::CTRL
    lda #((44100 * 128) / 48828)    ; Set sample rate to approx 44.1kHz
    sta VERA::PCM::RATE

    ; Clear all channels
    ldx #(.sizeof(CHANNEL) * NUM_CHANNELS)
    clear_loop:
        stz channels,x
        dex
        bpl clear_loop

    lda #1
    sta audio_ready
    rts
.endproc

; ---------------------------------------------------------------
; Play a sample
; A: Resource number
; X: Frequency
; Y: Volume
; ---------------------------------------------------------------
.proc play_sample
rts 
    phx ; save frequency

    ; Get resource info
    tax ; X = resource number
    lda #<resource_table
    sta sample_resource
    lda #>resource_table
    sta sample_resource+1

    ; Calculate resource offset in table
    offset_loop:
        clc
        lda #.sizeof(resource)
        adc sample_resource
        sta sample_resource
        lda #0
        adc sample_resource+1
        sta sample_resource+1
        dex
        bne offset_loop
    ; sample_resource now points to the resource info

    ; Find a free channel
    ldx #0
    lda channel1+CHANNEL::playing
    beq found_channel
    ldx #(.sizeof(CHANNEL) * 1)
    lda channel2+CHANNEL::playing
    beq found_channel
    ldx #(.sizeof(CHANNEL) * 2)
    lda channel3+CHANNEL::playing
    beq found_channel
    ldx #(.sizeof(CHANNEL) * 3)
    lda channel4+CHANNEL::playing
    beq found_channel
    rts ; No free channels

    found_channel:
    ; Copy resource info to channel
    tya
    sta channels+CHANNEL::volume,x
    ; exit if resource not loaded
    ldy #resource::status
    lda (sample_resource),y
    bne :+
    stp
    rts
    :
    ; set start address
    ldy #resource::pointer
    lda (sample_resource),y
    sta channels+CHANNEL::start,x
    iny
    lda (sample_resource),y
    sta channels+CHANNEL::start+1,x
    iny
    lda (sample_resource),y
    sta channels+CHANNEL::start+2,x
    iny
    lda (sample_resource),y
    sta channels+CHANNEL::start+3,x
    ; set end address
    clc
    ldy #resource::uncompressed
    lda (sample_resource),y
    adc channels+CHANNEL::start,x
    sta channels+CHANNEL::end,x
    iny
    lda (sample_resource),y
    adc channels+CHANNEL::start+1,x
    sta channels+CHANNEL::end+1,x
    iny
    lda (sample_resource),y
    adc channels+CHANNEL::start+2,x
    sta channels+CHANNEL::end+2,x
    ; set frequency
    pla
    sta channels+CHANNEL::frequency,x
    ; set playing flag
    lda #1
    sta channels+CHANNEL::playing,x

    rts
.endproc

; ---------------------------------------------------------------
; Update audio - called from IRQ
; ---------------------------------------------------------------
.proc update_audio
    ; Skip if audio not ready
    lda audio_ready
    beq done

    ; Update all channels
    ldy #255
    loop:
        ldx #0
        jsr update_channel
        ldx #(.sizeof(CHANNEL) * 1)
        jsr update_channel
        ldx #(.sizeof(CHANNEL) * 2)
        jsr update_channel
        ldx #(.sizeof(CHANNEL) * 3)
        jsr update_channel
        dey
        bne loop

    done:
    rts
.endproc

; ---------------------------------------------------------------
; Update a single channel
; X: Channel number
; ---------------------------------------------------------------
.proc update_channel
    phx
    phy

    ; Skip if not playing
    lda channels+CHANNEL::playing,x
    beq write_silence

    ; Set frequency
    ldy channels+CHANNEL::frequency,x
    lda pcm_freq_table,y
    sta VERA::PCM::RATE
    ; Set volume
    lda channels+CHANNEL::volume,x
    sta VERA::PCM::CTRL

    ; Get sample byte
    phx
    ldy channels+CHANNEL::start+2,x
    lda channels+CHANNEL::start+1,x
    pha
    lda channels+CHANNEL::start,x
    plx
    jsr read_byte
    sta VERA::PCM::DATA
    plx

    ; Increment sample pointer
    inc channels+CHANNEL::start,x
    bne :+
    inc channels+CHANNEL::start+1,x
    bne :+
    inc channels+CHANNEL::start+2,x
    :

    ; Check if end of sample
    lda channels+CHANNEL::end+2,x
    cmp channels+CHANNEL::start+2,x
    bne done
    lda channels+CHANNEL::end+1,x
    cmp channels+CHANNEL::start+1,x
    bne done
    lda channels+CHANNEL::end,x
    cmp channels+CHANNEL::start,x
    bne done

    ; Reset channel
    stz channels+CHANNEL::playing,x
    stz channels+CHANNEL::volume,x
    stz channels+CHANNEL::frequency,x
    stz channels+CHANNEL::start,x
    stz channels+CHANNEL::end,x
    stz channels+CHANNEL::start+1,x
    stz channels+CHANNEL::end+1,x
    stz channels+CHANNEL::start+2,x
    stz channels+CHANNEL::end+2,x
    ply
    plx
    rts

write_silence:
    lda #0
    sta VERA::PCM::DATA

done:
    ply
    plx
    rts
.endproc