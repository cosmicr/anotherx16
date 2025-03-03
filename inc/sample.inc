; ---------------------------------------------------------------
; sample.inc
; AnotherX16 - Commander X16 port of Another World
; ---------------------------------------------------------------
.globalzp stemp
.global init_audio, load_sample, play_sample, update_audio
.global init_irq, remove_irq

NUM_CHANNELS = 4

.struct CHANNEL
    start       .res 4
    end         .res 4
    playing     .res 1
    frequency   .res 1
    volume      .res 1
.endstruct 