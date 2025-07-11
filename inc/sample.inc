; ---------------------------------------------------------------
; sample.inc
; AnotherX16 - Commander X16 port of Another World
; ---------------------------------------------------------------
.globalzp stemp
.globalzp audio_ready
.global init_audio, load_sample, play_sample, update_audio
.global init_irq, remove_irq


NUM_CHANNELS = 4

.struct CHANNEL
    current     .res 4 ; upper 24 bits is the actual sample address, the lower 8 bits is the fractional offset
    loop        .res 3 ; loop length of sample - ie play from start to end, then loop from loop start to loop end
    end         .res 3 ; end of sample
    has_loop    .res 1 ; 0 = no loop, 1 = loop
    playing     .res 1
    phase_step  .res 2 ; 8.8 fixed point
    volume      .res 1
    buffer_pos  .res 1 ; current position in the sample buffer
    buffer_size .res 1 ; size of the sample buffer
.endstruct 