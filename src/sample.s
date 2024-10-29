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
    stemp:          .res 3

.segment "DATA"
    audio_ready:    .byte 0

.segment "BSS"
    old_irq:        .res 2
    channels:
    channel1:       .tag chan_data
    channel2:       .tag chan_data

.segment "RODATA"
    ; formula is: int(128 * freq / 48828.125)
    pcm_freq_table:
        .byte 9, 9, 10, 10, 11, 11, 11, 12
        .byte 12, 13, 14, 14, 15, 15, 16, 17
        .byte 18, 19, 19, 20, 22, 22, 23, 24
        .byte 26, 28, 29, 30, 32, 34, 36, 38
        .byte 40, 43, 45, 48, 51, 54, 58, 62


.segment "CODE"

; ---------------------------------------------------------------
; Setup Interrupt for AFLOW (Low FIFO buffer)
; ---------------------------------------------------------------
.proc init_irq
    sei               ; Disable interrupts

    ; Save old IRQ vector
    lda IRQVec
    sta old_irq
    lda IRQVec+1
    sta old_irq+1

    ; Install IRQ handler
    lda #<irq_handler
    sta IRQVec
    lda #>irq_handler
    sta IRQVec+1

    ; Enable AFLOW interrupt
    ; lda VERA::IRQ_EN
    ; ora #%00001000       ; Set bit 3 (AFLOW interrupt enable)
    ; sta VERA::IRQ_EN

    ; set both channels as not done
    stz channel1+chan_data::done
    stz channel2+chan_data::done
    stz channel1+chan_data::available
    stz channel2+chan_data::available

    ; reset FIFO
    lda #$80
    sta VERA::PCM::CTRL

    lda #1
    sta audio_ready

    cli               ; Enable interrupts

    rts
.endproc

; ---------------------------------------------------------------
; Play a sample (updated version)
; A: Resource number
; X: Frequency
; Y: Volume
; ---------------------------------------------------------------
.proc play_sample
    freq = stemp+2
    volume = stemp+3
    stx freq
    sty volume

    ; Get offset into resource info table and store in stemp
    tax
    lda #<resource_table
    sta stemp
    lda #>resource_table
    sta stemp+1

    @table_loop:
        clc
        lda #.sizeof(resource)
        adc stemp
        sta stemp
        lda #0
        adc stemp+1
        sta stemp+1
        dex
        bne @table_loop

    ; Find an available channel
    lda channel1+chan_data::available
    bne :+
    lda #1
    sta channel1+chan_data::available
    ldx #0
    bra found_channel
    :
    lda channel2+chan_data::available
    bne :+
    lda #1
    sta channel2+chan_data::available
    ldx #.sizeof(chan_data)
    bra found_channel
    :
    bra finished
    found_channel:

    ; Set channel as playing
    stz channels+chan_data::done,x
    
    ; Load sample data
    ldy #resource::pointer
    lda (stemp), y
    sta channels+chan_data::start,x
    iny
    lda (stemp), y
    sta channels+chan_data::start+1,x
    iny
    lda (stemp), y
    sta channels+chan_data::start+2,x
    iny
    lda (stemp), y
    sta channels+chan_data::start+3,x

    ; Set end address for the sample
    clc
    ldy #resource::uncompressed
    lda (stemp), y
    adc channels+chan_data::start,x
    sta channels+chan_data::end,x
    iny
    lda (stemp), y
    adc channels+chan_data::start+1,x
    sta channels+chan_data::end+1,x
    iny
    lda (stemp), y
    adc channels+chan_data::start+2,x
    sta channels+chan_data::end+2,x
    iny
    lda (stemp), y
    adc channels+chan_data::start+3,x
    sta channels+chan_data::end+3,x

    ; Set volume control
    lda volume
    ora #%00010000          ; Set bit 4 for stereo
    sta VERA::PCM::CTRL     ; Bits 0-3: volume, bit 4: stereo, bit 5: 16-bit

    ; Set sample rate to 0 to stop playback initially
    ; check nothing is playing
    lda channel1+chan_data::done
    ora channel2+chan_data::done
    bne :+ ; something is playing
    stz VERA::PCM::RATE     
    :

    ; Fill FIFO initially
    jsr fill_fifo

    ; Set sample rate for playback
    ldx freq
    lda pcm_freq_table, x
    sta VERA::PCM::RATE

    finished:
    rts
.endproc

; ---------------------------------------------------------------
; IRQ handler for AFLOW (Low FIFO buffer)
; ---------------------------------------------------------------
; todo: probably need to move this to a separate file at some point
.proc irq_handler
    pha
    phx
    phy

    ; If audio is not ready, exit
    lda audio_ready
    beq irq_chain

    ; check if AFLOW is enabled
    lda VERA::IRQ_FLAGS
    and #%00001000       ; Check bit 3: AFLOW
    beq irq_chain        ; FIFO isn't low, exit

    ; If both channels are available, then there's nothign playing
    lda channel1+chan_data::available
    ora channel2+chan_data::available
    beq irq_chain

    ; FIFO is low, check if the sample playback is complete
    lda channel1+chan_data::done
    beq refill_fifo       ; If channel1 isn't done, refill FIFO
    stz channel1+chan_data::available

    lda channel2+chan_data::done
    beq refill_fifo       ; If channel2 isn't done, refill FIFO
    stz channel2+chan_data::available

    ; Check if FIFO buffer is empty
    lda VERA::PCM::CTRL
    and #%01000000          ; Check bit 6: EMPTY
    beq irq_chain           ; If FIFO isn't empty, exit

    ; Sample is done
    ; Set Playback to none
    stz VERA::PCM::RATE
    ; reset done flags
    stz channel1+chan_data::done
    stz channel2+chan_data::done
    stz channel1+chan_data::available
    stz channel2+chan_data::available
    ; reset FIFO
    ; lda #$80
    ; sta VERA::PCM::CTRL

    ; Exit IRQ handler
    bra irq_chain

    refill_fifo:
    jsr fill_fifo        ; Refill the FIFO since sample is not done

    irq_chain:
    ply
    plx
    pla

    ; Chain to previous IRQ handler
    jmp (old_irq)
.endproc

; ---------------------------------------------------------------
; Fill FIFO buffer until full or sample finished (updated version)
; ---------------------------------------------------------------
.proc fill_fifo
    ; Save original bank
    lda $00
    sta stemp

    ; Check if the FIFO buffer is low
    lda VERA::IRQ_FLAGS
    and #%00001000       ; Check bit 3: AFLOW
    jeq fill_fifo_done   ; If FIFO isn't low, exit

    lda channel1+chan_data::done
    beq fill_fifo_loop  ; if channel1 is not done, continue

    lda channel2+chan_data::done
    beq fill_fifo_loop  ; if channel2 is not done, continue

    jra fill_fifo_done ; both channels are done

    fill_fifo_loop:
        ; Check if FIFO is full or sample depleted
        lda VERA::PCM::CTRL
        and #%10000000        ; Check FIFO full flag
        jne fill_fifo_done    ; If FIFO is full, exit

        ; *** channel1 (left)
        lda channel1+chan_data::done
        beq :+ ; not done
        stz VERA::PCM::DATA ; write nothing
        bra fill_channel2
        :
        ; Check if start has reached or passed end (24-bit check)
        lda channel1+chan_data::start
        cmp channel1+chan_data::end
        bcc continue_fill_channel1
        lda channel1+chan_data::start+1
        cmp channel1+chan_data::end+1
        bcc continue_fill_channel1
        lda channel1+chan_data::start+2
        cmp channel1+chan_data::end+2
        bcc continue_fill_channel1
        lda #1
        sta channel1+chan_data::done
        bra fill_channel2

        ; Fill FIFO with sample data
        continue_fill_channel1:
        lda channel1+chan_data::start
        ldx channel1+chan_data::start+1
        ldy channel1+chan_data::start+2

        jsr read_byte       ; 24-bit address read
        sta VERA::PCM::DATA

        ; Increment sample pointer address
        inc24 channel1+chan_data::start

        fill_channel2:
        ; *** channel2 (right)
        lda channel2+chan_data::done
        beq :+ ; not done
        stz VERA::PCM::DATA ; write nothing
        bra fill_fifo_loop
        :
        ; Check if start has reached or passed end (24-bit check)
        lda channel2+chan_data::start
        cmp channel2+chan_data::end
        bcc continue_fill_channel2
        lda channel2+chan_data::start+1
        cmp channel2+chan_data::end+1
        bcc continue_fill_channel2
        lda channel2+chan_data::start+2
        cmp channel2+chan_data::end+2
        bcc continue_fill_channel2
        lda #1
        sta channel2+chan_data::done
        jmp fill_fifo_loop

        ; Fill FIFO with sample data
        continue_fill_channel2:
        lda channel2+chan_data::start
        ldx channel2+chan_data::start+1
        ldy channel2+chan_data::start+2

        jsr read_byte       ; 24-bit address read
        sta VERA::PCM::DATA

        ; Increment sample pointer address
        inc24 channel2+chan_data::start

        jmp fill_fifo_loop  ; Continue until FIFO is full or sample finished

    fill_fifo_done:
    ; Restore original bank
    lda stemp
    sta $00

    rts
.endproc


; ---------------------------------------------------------------
; Remove IRQ handler
; ---------------------------------------------------------------
.proc remove_irq
    sei               ; Disable interrupts

    ; Restore old IRQ vector
    lda old_irq
    sta IRQVec
    lda old_irq+1
    sta IRQVec+1

    ; Make sure AFLOW interrupt is off
    lda VERA::IRQ_EN
    and #%11110111       ; Clear bit 3 (AFLOW interrupt enable)
    sta VERA::IRQ_EN

    cli               ; Enable interrupts

    rts
.endproc