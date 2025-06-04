; ---------------------------------------------------------------
; sample.inc
; AnotherX16 - Commander X16 port of Another World
; ---------------------------------------------------------------
.globalzp stemp
.global init_audio, load_sample, play_sample, update_audio
.global init_irq, remove_irq
.global audio_ready

NUM_CHANNELS = 4

.struct CHANNEL
    current     .res 4 ; upper 24 bits is the actual sample address, the lower 8 bits is the fractional offset
    end         .res 4
    playing     .res 1
    phase_step  .res 2 ; 8.8 fixed point
    volume      .res 1
    buffer_pos  .res 1 ; current position in the sample buffer
    buffer_size .res 1 ; size of the sample buffer
.endstruct 