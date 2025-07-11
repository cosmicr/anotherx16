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
    stemp:              .res 2 ; Temporary storage 
    audio_ready:        .res 1 ; Flag to indicate if audio is ready
    final_sample:       .res 2 ; Final sample value to write to PCM FIFO

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
    FREQ = 9600 ; Playback frequency in Hz (target: 22050)
.align 256
    pcm_freq_table: 
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
    bne :+
    rts ; exit if volume is zero
    :

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

    ; set loop address (from sample data)
    ldy channel+CHANNEL::current+3
    ldx channel+CHANNEL::current+2
    lda channel+CHANNEL::current+1
    jsr read_word
    stx channel+CHANNEL::loop
    sta channel+CHANNEL::loop+1
    stz channel+CHANNEL::loop+2
    asl channel+CHANNEL::loop
    rol channel+CHANNEL::loop+1
    
    ; add 8 to current
    clc
    lda #8
    adc channel+CHANNEL::current+1
    sta channel+CHANNEL::current+1
    lda channel+CHANNEL::current+2
    adc #0
    sta channel+CHANNEL::current+2
    lda channel+CHANNEL::current+3
    adc #0
    sta channel+CHANNEL::current+3

    ; set loop offset from current
    clc
    lda channel+CHANNEL::current+1
    adc channel+CHANNEL::loop
    sta channel+CHANNEL::loop
    lda channel+CHANNEL::current+2
    adc channel+CHANNEL::loop+1
    sta channel+CHANNEL::loop+1
    lda channel+CHANNEL::current+3
    adc channel+CHANNEL::loop+2
    sta channel+CHANNEL::loop+2

    ; if loop == end, disable loop
    inc channel+CHANNEL::has_loop
    lda channel+CHANNEL::loop+2
    cmp channel+CHANNEL::end+2
    bne :+
    lda channel+CHANNEL::loop+1
    cmp channel+CHANNEL::end+1
    bne :+
    lda channel+CHANNEL::loop
    cmp channel+CHANNEL::end
    bne :+
    stz channel+CHANNEL::has_loop ; no loop if loop == end
    :
    
    ; set frequency
    asl freq
    ldx freq
    lda pcm_freq_table,x ; get frequency from table
    sta channel+CHANNEL::phase_step
    lda pcm_freq_table+1,x 
    sta channel+CHANNEL::phase_step+1 

    ; clear the buffer
    stz channel+CHANNEL::buffer_pos

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

    stx freq ; Save frequency in temp

    ; Get resource info
    tax ; X = resource number
    lda resource_table_offsets_low,x ; Load low byte of resource address
    sta sample_resource
    lda resource_table_offsets_high,x ; Load high byte of resource address
    sta sample_resource+1
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

    ; if all channels are not playing, stop filling
    lda channel0+CHANNEL::playing
    ora channel1+CHANNEL::playing
    ora channel2+CHANNEL::playing
    ora channel3+CHANNEL::playing
    bne has_audio ; if any channel is playing, fill FIFO

    stz VERA::PCM::CTRL ; set volume to zero (stop playback)
    bra done

    has_audio:
    ; set volume
    lda #%00001111
    sta VERA::PCM::CTRL

    ; Keep filling until FIFO is full or no channels playing
    fill_until_full:
        ; Read FIFO status
        lda VERA::PCM::CTRL
        bmi done ; bit 7 is set if FIFO is full, so we can stop

        ; FIFO has space - fill it
        jsr fill_fifo

        bra fill_until_full ; still playing, keep filling
    done:
    rts
.endproc

.macro increment_and_check_end channel
.scope
    ; save old integer part for tracking
    lda channel+CHANNEL::current+1
    sta stemp

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

    ; calculate how many integer samples we advanced
    lda channel+CHANNEL::current+1
    sec
    sbc stemp ; subtract old integer part
    beq no_advance

    ; we advanced, so we need to update the buffer position
    clc
    adc channel+CHANNEL::buffer_pos
    bcc store
    stz channel+CHANNEL::buffer_pos ; if we overflow, reset buffer position
    store:
    sta channel+CHANNEL::buffer_pos

    no_advance:
    ; Check if we reached the end of the sample
    ; even though addresses are 24-bit, the aren't any samples that are larger than 65535 bytes
    lda channel+CHANNEL::current+2
    cmp channel+CHANNEL::end+1
    bcc done ; if current sample < end
    bne stop ; test low byte if high byte is equal
    lda channel+CHANNEL::current+1
    cmp channel+CHANNEL::end
    bcc done ; if current sample low byte < end low byte

    stop:
    lda channel+CHANNEL::has_loop
    beq no_loop
    ; Looping is enabled, so reset to loop start
    lda channel+CHANNEL::loop
    sta channel+CHANNEL::current+1
    lda channel+CHANNEL::loop+1
    sta channel+CHANNEL::current+2
    lda channel+CHANNEL::loop+2
    sta channel+CHANNEL::current+3
    ; Reset buffer position to zero
    stz channel+CHANNEL::buffer_pos
    bra done
    
    no_loop:
    stz channel+CHANNEL::playing ; stop playing if we reached the end
    done:
.endscope
.endmacro

.macro get_buffer_byte channel, buffer
.scope
    ; if the buffer is empty, fill it  with a page of data
    lda channel+CHANNEL::buffer_pos
    bne get_byte

    ; buffer is empty, read a page of data
    lda #<(buffer)
    sta page_buffer
    lda #>(buffer)
    sta page_buffer+1
    ldy channel+CHANNEL::current+3 ; upper byte
    ldx channel+CHANNEL::current+2 ; mid byte
    lda channel+CHANNEL::current+1 ; low byte
    jsr read_page

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

    ; Get sample byte (signed 8-bit, directly usable with VERA)
    get_buffer_byte channel, channel_buffer0+(channel_num*256)

    ; Apply volume - keep it very simple for now
    ldy channel+CHANNEL::volume
    beq zero_volume             ; Volume 0 = silence
    
    cpy #8
    bcs apply_sample            ; Volume 8-15: full volume
    ; Volumes 1-7 get reduced by half
    cmp #$80
    ror
    cpy #4
    bcs apply_sample            ; Volume 4-7: quarter volume
    cmp #$80
    ror
    cpy #2
    bcs apply_sample            ; Volume 2-3: eighth volume
    cmp #$80
    ror
    cpy #1
    bcs apply_sample            ; Volume 1: sixteenth volume
    ; Volume 0: silence, do nothing
    bra zero_volume
    
    apply_sample:
    ; Add to final mix (signed arithmetic)
    clc
    adc final_sample
    sta final_sample
    bra done
    
    zero_volume:
    ; Add nothing (silence)
    
    done:
    increment_and_check_end channel
    skip_channel:
.endscope
.endmacro

; ---------------------------------------------------------------
; Bulk fill FIFO with samples_to_fill
; ----------------------------------------------------------------
; TODO: Change to Channel 0 and 2 left, Channel 1 and 3 right
.proc fill_fifo
    stz final_sample            ; Start with 0 (signed)

    ; Get buffer position
    load_sample_byte channel0, 0
    load_sample_byte channel1, 1
    load_sample_byte channel2, 2
    load_sample_byte channel3, 3
    
    lda final_sample
    sta VERA::PCM::DATA
    
    rts
.endproc