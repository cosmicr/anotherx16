; ---------------------------------------------------------------
; sample.inc
; AnotherX16 - Commander X16 port of Another World
; ---------------------------------------------------------------
.globalzp stemp, audio_channel
.global init_audio, load_sample, play_sample, update_audio
.global init_irq, remove_irq
.global audio_ready

NUM_CHANNELS = 2

.struct CHANNEL
    start       .res 4
    current     .res 4
    end         .res 4
    playing     .res 1
    frequency   .res 1
    volume      .res 1
.endstruct 