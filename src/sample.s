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
    stemp:              .res 4 ; Temporary storage for sample value
    audio_ready:        .res 1 ; Flag to indicate if audio is ready

.segment "DATA"
    channels:
    channel0:         .tag CHANNEL
    channel1:         .tag CHANNEL
    channel2:         .tag CHANNEL
    channel3:         .tag CHANNEL

    channel_buffer0: .res 256 ; Buffer for channel 0
    channel_buffer1: .res 256 ; Buffer for channel 1
    channel_buffer2: .res 256 ; Buffer for channel 2
    channel_buffer3: .res 256 ; Buffer for channel 3

.segment "RODATA"
    channel_clear_masks:
    .byte %11111110  ; Clear mask for channel 0
    .byte %11111101  ; Clear mask for channel 1  
    .byte %11111011  ; Clear mask for channel 2
    .byte %11110111  ; Clear mask for channel 3

    FREQ = 11025 ; Playback frequency in Hz (target: 22050)

    pcm_freq_table: ; note this method makes the entries big endian as they are stored in literal order
    .word ($0CFF << 8) / FREQ, ($0DC3 << 8) / FREQ, ($0E91 << 8) / FREQ, ($0F6F << 8) / FREQ, ($1056 << 8) / FREQ, ($114E << 8) / FREQ, ($1259 << 8) / FREQ, ($136C << 8) / FREQ
    .word ($149F << 8) / FREQ, ($15D9 << 8) / FREQ, ($1726 << 8) / FREQ, ($1888 << 8) / FREQ, ($19FD << 8) / FREQ, ($1B86 << 8) / FREQ, ($1D21 << 8) / FREQ, ($1EDE << 8) / FREQ
    .word ($20AB << 8) / FREQ, ($229C << 8) / FREQ, ($24B3 << 8) / FREQ, ($26D7 << 8) / FREQ, ($293F << 8) / FREQ, ($2BB2 << 8) / FREQ, ($2E4C << 8) / FREQ, ($3110 << 8) / FREQ
    .word ($33FB << 8) / FREQ, ($370D << 8) / FREQ, ($3A43 << 8) / FREQ, ($3DDF << 8) / FREQ, ($4157 << 8) / FREQ, ($4538 << 8) / FREQ, ($4998 << 8) / FREQ, ($4DAE << 8) / FREQ
    .word ($5240 << 8) / FREQ, ($5764 << 8) / FREQ, ($5C9A << 8) / FREQ, ($61C8 << 8) / FREQ, ($6793 << 8) / FREQ, ($6E19 << 8) / FREQ, ($7485 << 8) / FREQ, ($7BBD << 8) / FREQ

.segment "CODE"

; ---------------------------------------------------------------
; Initialize audio system
; ---------------------------------------------------------------
.proc init_audio
    ; Initialize VERA audio
    stz VERA::PCM::DATA
    lda #%00000000 ; set PCM mode to 8-bit mono and volume to zero
    sta VERA::PCM::CTRL
    lda #((FREQ * 128) / 48828)    ; Set sample rate to approx 11kHz
    sta VERA::PCM::RATE

    ; Clear all channels
    ldx #(.sizeof(CHANNEL) * NUM_CHANNELS)
    clear_loop:
        stz channels,x
        dex
        bpl clear_loop

    inc audio_ready ; Set audio ready flag

    rts
.endproc

.macro setup_channel channel
    sta channel+CHANNEL::volume
    ; exit if resource not loaded
    ldy #resource::status
    lda (sample_resource),y
    bne :+
    rts
    :
    ; set start address
    stz channel+CHANNEL::current
    ldy #resource::pointer
    lda (sample_resource),y
    sta channel+CHANNEL::current+1
    iny
    lda (sample_resource),y
    sta channel+CHANNEL::current+2
    iny
    lda (sample_resource),y
    sta channel+CHANNEL::current+3
    ; set end address
    clc
    ldy #resource::uncompressed
    lda (sample_resource),y
    adc channel+CHANNEL::current+1
    sta channel+CHANNEL::end
    iny
    lda (sample_resource),y
    adc channel+CHANNEL::current+2
    sta channel+CHANNEL::end+1
    iny
    lda (sample_resource),y
    adc channel+CHANNEL::current+3
    sta channel+CHANNEL::end+2
    ; set frequency

    asl freq
    ldx freq
    lda pcm_freq_table,x ; get frequency from table
    sta channel+CHANNEL::phase_step
    lda pcm_freq_table+1,x 
    sta channel+CHANNEL::phase_step+1 ; for some reason the table is in big-endian format?

    ; clear the buffer
    lda #$FF
    sta channel+CHANNEL::buffer_pos

    ; set playing flag
    inc channel+CHANNEL::playing
.endmacro

; ---------------------------------------------------------------
; Play a sample - sets a channel to play a sample
; A: Resource number
; X: Frequency
; Y: low byte: Volume (0-15) high byte: Channel number (0-3)
; ---------------------------------------------------------------
.proc play_sample 
    freq = stemp
    sample_resource = stemp+1
    offset = stemp+3

    stx freq ; Save frequency in temp

    ; Get resource info
    tax ; X = resource number
    lda #<resource_table
    sta sample_resource
    lda #>resource_table
    sta sample_resource+1

    ; todo: use a lookup table for offset?
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

    ; get the channel number and volume from Y
    tya
    lsr
    lsr
    lsr
    lsr ; we need the extra lsr here for rounding before the asl
    asl ; shift left for word address
    tax ; X = channel number (0-3) * 2
    tya ; Y = volume (0-15)
    and #%00001111 ; mask to get volume (0-15)
    jmp (channel_setup_table,x) ; jump to channel setup routine

    ; jump table for each channel
    channel_setup_table:
    .addr channel_0_setup, channel_1_setup, channel_2_setup, channel_3_setup

    ; This is a large amount of code, but it unrolls the setup for each channel
    channel_0_setup:
        setup_channel channel0
    rts
    channel_1_setup:
        setup_channel channel1
    rts
    channel_2_setup:
        setup_channel channel2
    rts
    channel_3_setup:
        setup_channel channel3
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

    ; set volume (todo: adjust volume based on channel)
    lda #%00001000
    sta VERA::PCM::CTRL

    ; Keep filling until FIFO is full or no channels playing
    fill_until_full:
        ; Read FIFO status
        lda VERA::PCM::CTRL
        bit #%10000000 ; check if FIFO is full
        bne done ; FIFO is full, we're done

        ; if all channels are not playing, stop filling
        lda channel0+CHANNEL::playing
        ora channel1+CHANNEL::playing
        ora channel2+CHANNEL::playing
        ora channel3+CHANNEL::playing
        beq done ; no channels playing, we're done

        ; FIFO has space - fill it
        jsr fill_fifo

        bra fill_until_full ; still playing, keep filling
    done:
    rts
.endproc

.macro increment_and_check_end channel, channel_num
.scope
    ; Increment current sample and check if we reached the end
    clc
    lda channel+CHANNEL::phase_step
    adc channel+CHANNEL::current
    sta channel+CHANNEL::current
    lda channel+CHANNEL::phase_step+1
    adc channel+CHANNEL::current+1
    sta channel+CHANNEL::current+1
    lda #0
    adc channel+CHANNEL::current+2
    sta channel+CHANNEL::current+2
    ; lda #0
    ; adc channel+CHANNEL::current+3
    ; sta channel+CHANNEL::current+3

    ; todo: this isn't quite right as it might skip some bytes at the start
    lda channel+CHANNEL::current+1
    sta channel+CHANNEL::buffer_pos

    ; Check if we reached the end of the sample
    lda channel+CHANNEL::current+3
    cmp channel+CHANNEL::end+2
    bcc done ; if current sample >= end, skip increment
    bne stop
    lda channel+CHANNEL::current+2
    cmp channel+CHANNEL::end+1
    bcc done ; if current sample >= end, skip increment
    bne stop
    lda channel+CHANNEL::current+1
    cmp channel+CHANNEL::end
    bcc done ; if current sample >= end, skip increment

    stop:
    stz channel+CHANNEL::playing ; stop playing if we reached the end
    done:
.endscope
.endmacro

.macro get_buffer_byte channel, buffer
.scope
    ; todo: only get a byte if we incremented, otherwise cache the previous
    ; if the buffer is empty, fill it
    lda channel+CHANNEL::buffer_pos
    cmp #$FF
    jne get_byte ; if pos at $FF, buffer is depleted, fill it
    lda #<(buffer)
    sta page_buffer
    lda #>(buffer)
    sta page_buffer+1
    ldy channel+CHANNEL::current+3 ; upper byte
    lda channel+CHANNEL::current+2 ; mid byte
    sta stemp
    lda channel+CHANNEL::current+1 ; low byte
    ldx stemp
    jsr read_page
    stz channel+CHANNEL::buffer_pos ; increment buffer position
    get_byte:
    ; buffer is not empty, get byte from buffer
    ldx channel+CHANNEL::buffer_pos
    lda buffer, x
    done:
.endscope
.endmacro

.macro load_sample_byte channel, channel_num
.scope
        lda channel+CHANNEL::playing
        jeq skip_channel
        ; channel is playing, get next sample
        get_buffer_byte channel, channel_buffer0+channel_num*256 ; Load byte from buffer
        jpl positive
        clc ; sign extend negative sample
        adc final_sample
        sta final_sample
        lda #$FF
        adc final_sample+1
        sta final_sample+1
        jra done
        positive:
        clc
        adc final_sample
        sta final_sample
        lda #$00
        adc final_sample+1
        sta final_sample+1
        done:
        increment_and_check_end channel, channel_num
        skip_channel:
.endscope
.endmacro

; ---------------------------------------------------------------
; Bulk fill FIFO with samples_to_fill
; ----------------------------------------------------------------
; TODO: Change to Channel 0 and 2 left, Channel 1 and 3 right
; TODO: Volume is important (some samples play at zero volume)
.proc fill_fifo
    final_sample = stemp+1
    num_loops = stemp+3

    lda #36 ; sweet spot for 11025Hz
    sta num_loops ; Set number of samples to fill
    stz final_sample ; clear final_sample
    stz final_sample+1
    fill_loop:
        dec num_loops
        jeq fill_done ; if no more samples to fill, we're done
        ; get a sample from each channel
        load_sample_byte channel0, 0
        load_sample_byte channel1, 1
        load_sample_byte channel2, 2
        load_sample_byte channel3, 3

        ; Divide by 4 to average the samples
        ; Since result always fits in 8-bit, we can optimize
        lda final_sample+1
        cmp #$80               ; Set carry if negative
        ror final_sample+1     ; Arithmetic shift right
        ror final_sample
        cmp #$80               ; Set carry if still negative  
        ror final_sample+1     ; Arithmetic shift right again
        ror final_sample
        
        ; Output (no clamping needed - guaranteed to fit!)
        lda final_sample
        sta VERA::PCM::DATA

        jmp fill_loop ; continue filling
        
    fill_done:
        rts
.endproc

