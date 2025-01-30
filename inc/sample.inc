; ---------------------------------------------------------------
; sample.inc
; AnotherX16 - Commander X16 port of Another World
; ---------------------------------------------------------------

.globalzp stemp
.global play_sample, init_irq, remove_irq


.struct chan_data
    start           .res 4
    end             .res 4
    done            .res 1
    available       .res 1
.endstruct


