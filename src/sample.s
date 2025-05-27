; ---------------------------------------------------------------
; sample.s
; AnotherX16 - Commander X16 port of Another World
; ---------------------------------------------------------------

.macpack longbranch

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
    sample_resource:    .res 2
    samples_to_fill:    .res 1 ; Number of samples to fill in FIFO
    left_sample:         .res 1 ; Left channel sample
    right_sample:        .res 1 ; Right channel sample
    temp_x:            .res 1 ; Temporary X register for calculations
    stemp:            .res 1 ; Temporary storage for sample value
    audio_channel:      .res 1 ; Current audio channel being processed

.segment "DATA"
    audio_ready:        .res 1 ; Flag to indicate if audio is ready

    channels:
    channel1:         .tag CHANNEL
    channel2:         .tag CHANNEL

.segment "RODATA"
    ; formula is: int(128 * freq / 48828.125)
    pcm_freq_table:
        ; Original table based on formula above
        ; .byte 9, 9, 10, 10, 11, 12, 12, 13
        ; .byte 14, 15, 16, 16, 17, 18, 20, 21
        ; .byte 22, 23, 25, 26, 28, 29, 31, 33
        ; .byte 35, 37, 39, 42, 44, 46, 49, 52
        ; .byte 55, 59, 62, 66, 70, 74, 78, 83

        ; old table based on 44100Hz sample rate
        .byte 9, 9, 10, 10, 11, 11, 11, 12
        .byte 12, 13, 14, 14, 15, 15, 16, 17
        .byte 18, 19, 19, 20, 22, 22, 23, 24
        .byte 26, 28, 29, 30, 32, 34, 36, 38
        .byte 40, 43, 45, 48, 51, 54, 58, 62

        ; Linear
        ; .byte 9, 10, 12, 14, 16, 18, 20, 22, 24, 26
        ; .byte 27, 29, 31, 33, 35, 37, 39, 41, 43, 45
        ; .byte 46, 48, 50, 52, 54, 56, 58, 60, 62, 64
        ; .byte 65, 67, 69, 71, 73, 75, 77, 79, 81, 83

        ; Logarithmic
        ; .byte 9, 17, 24, 29, 33, 37, 40, 43, 46, 48
        ; .byte 51, 53, 55, 56, 58, 60, 61, 62, 64, 65
        ; .byte 66, 67, 68, 70, 71, 72, 72, 73, 74, 75
        ; .byte 76, 77, 78, 78, 79, 80, 81, 81, 82, 83


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
    lda #%00010000 ; set PCM mode to 8-bit stereo and volume to zero
    sta VERA::PCM::CTRL
    lda #((11025 * 128) / 48828)    ; Set sample rate to approx 11kHz
    sta VERA::PCM::RATE

    ; Clear all channels
    ldx #(.sizeof(CHANNEL) * NUM_CHANNELS)
    clear_loop:
        stz channels,x
        dex
        bpl clear_loop

    lda #1
    sta audio_ready ; Set audio ready flag

    rts
.endproc

; ---------------------------------------------------------------
; Play a sample
; A: Resource number
; X: Frequency
; Y: Volume
; ---------------------------------------------------------------
.proc play_sample ; TODO: instead of finding a free channel, use channel1 for original 1 and 3, and channel2 for 2 and 4
    stx stemp ; Save frequency in temp

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
    lda audio_channel
    and #1 ; Toggle between channel 1 and 2
    beq use_channel1
    ; use_channel2:
    ldx #.sizeof(CHANNEL) ; Use channel 2
    lda channel2+CHANNEL::playing
    beq found_channel
    use_channel1:
    ldx #0 ; Use channel 1
    lda channel1+CHANNEL::playing
    beq found_channel
    rts ; No free channels
    
    found_channel:
    ; Copy resource info to channel
    tya
; ; TEMPORARY:
; lda #$06
    sta channels+CHANNEL::volume,x
    ; exit if resource not loaded
    ldy #resource::status
    lda (sample_resource),y
    bne :+
    rts
    :
    ; set start address
    ldy #resource::pointer
    lda (sample_resource),y
    sta channels+CHANNEL::start,x
    sta channels+CHANNEL::current,x
    iny
    lda (sample_resource),y
    sta channels+CHANNEL::start+1,x
    sta channels+CHANNEL::current+1,x
    iny
    lda (sample_resource),y
    sta channels+CHANNEL::start+2,x
    sta channels+CHANNEL::current+2,x
    iny
    lda (sample_resource),y
    sta channels+CHANNEL::start+3,x
    sta channels+CHANNEL::current+3,x
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
    lda stemp
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

    stz audio_ready ; Clear audio ready flag

    ; Check if any channel is playing
    lda channel1+CHANNEL::playing
    ora channel2+CHANNEL::playing
    beq done ; no channels playing

    ; Keep filling until FIFO is full or no channels playing
    fill_until_full:
    ; Read FIFO status
    lda VERA::PCM::CTRL
    bit #%10000000 ; check if FIFO is full
    bne done ; FIFO is full, we're done

    ; FIFO has space - fill it
    lda #192
    sta samples_to_fill
    jsr fill_fifo
    
    ; Check if any channels are still playing
    lda channel1+CHANNEL::playing
    ora channel2+CHANNEL::playing
    bne fill_until_full ; still playing, keep filling
    
    done:
    rts
.endproc

; ---------------------------------------------------------------
; Bulk fill FIFO with samples_to_fill
; ----------------------------------------------------------------
.proc fill_fifo
    fill_loop:
        lda samples_to_fill
        jeq fill_done

        ; set volume based on channel 1 and 2 average
        clc
        lda channel1+CHANNEL::volume
        adc channel2+CHANNEL::volume
        lsr ; divide by 2 for average
        ora #%00010000 ; stereo mode
        sta VERA::PCM::CTRL

        stz left_sample
        stz right_sample
        
        check_channel1:
        lda channel1+CHANNEL::playing
        beq check_channel2 ; if channel 1 is not playing, go to channel 2 only
        ldx #0
        jsr get_next_sample
        sta left_sample
        bcs :+
        stz channel1+CHANNEL::playing ; if channel 1 ended, clear playing flag
        :
        
        check_channel2:
        lda channel2+CHANNEL::playing
        beq determine_output
        ldx #.sizeof(CHANNEL)
        jsr get_next_sample
        sta right_sample
        bcs :+
        stz channel2+CHANNEL::playing ; if channel 2 ended, clear playing flag
        :
        
        ; Determine output based on which channels are playing
        determine_output:
        lda channel1+CHANNEL::playing
        bne ch1_active
        lda channel2+CHANNEL::playing
        bne ch2_only
        ; Both channels inactive - output silence (already zeroed)
        bra write_data
        
        ch1_active:
        ldx channel1+CHANNEL::frequency
        lda pcm_freq_table, x ; get frequency from table
        sta VERA::PCM::RATE
        lda channel2+CHANNEL::playing
        bne write_data ; both channels active, use normal left/right
        ; Only channel 1 active - copy to both sides
        ldx channel1+CHANNEL::frequency
        lda pcm_freq_table, x ; get frequency from table
        sta VERA::PCM::RATE
        lda left_sample
        sta right_sample
        bra write_data
        
        ch2_only:
        ; Only channel 2 active - copy to both sides
        ldx channel2+CHANNEL::frequency
        lda pcm_freq_table, x ; get frequency from table
        sta VERA::PCM::RATE
        lda right_sample
        sta left_sample
        
        ; write data to FIFO
        write_data:
        lda left_sample
        sta VERA::PCM::DATA ; write left channel sample
        lda right_sample
        sta VERA::PCM::DATA ; write right channel sample
        dec samples_to_fill
        bra fill_loop
        
    fill_done:
        rts
.endproc

; ---------------------------------------------------------------
; Get next sample from the active channel
; X: Channel offset
; Returns A: Sample value, C: 1 if still playing
; ----------------------------------------------------------------
.proc get_next_sample
    stx temp_x ; save channel offset
    ; get raw sample data
    ldy channels+CHANNEL::current+2,x ; upper byte
    lda channels+CHANNEL::current+1,x ; mid byte
    sta stemp
    lda channels+CHANNEL::current,x ; low byte
    ldx stemp
    jsr read_byte ; TODO: cache sounds in a specific bank
    sta stemp

    ldx temp_x ; restore channel offset
    inc channels+CHANNEL::current, x ; increment current sample
    bne :+
    inc channels+CHANNEL::current+1, x ; increment high byte
    bne :+
    inc channels+CHANNEL::current+2, x ; increment upper byte
    :

    ; check if we reached the end of the sample
    lda channels+CHANNEL::end+2, x
    cmp channels+CHANNEL::current+2, x
    bne still_playing
    lda channels+CHANNEL::end+1, x
    cmp channels+CHANNEL::current+1, x
    bne still_playing
    lda channels+CHANNEL::end, x
    cmp channels+CHANNEL::current, x
    bne still_playing

    ; end of sample reached
    lda stemp ; sample
    clc ; not playing
    rts

    still_playing:
    lda stemp ; sample
    sec ; still playing
    rts
.endproc


