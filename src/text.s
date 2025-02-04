; ---------------------------------------------------------------
; text.s
; AnotherX16 - Commander X16 port of Another World
; ---------------------------------------------------------------

.macpack longbranch

; X16 and CBM includes
.include "cx16.inc"
.include "cbm_kernal.inc"

; Project includes
.include "main.inc"
.include "macros.inc"
.include "text.inc"
.include "engine.inc"
.include "tasks.inc"

.segment "RODATA"
    strings:
        .byte $00, $01, "P E A N U T  3000", 0
        .byte $00, $02, "Copyright  } 1990 Peanut Computer, Inc.", $0A, "All rights reserved.", $0A, $0A, "CMDR-DOS Version 25.02", 0
        .byte $00, $03, "2", 0
        .byte $00, $04, "3", 0
        .byte $00, $05, ".", 0
        .byte $00, $06, "A", 0
        .byte $00, $07, "@", 0
        .byte $00, $08, "PEANUT 3000", 0
        .byte $00, $0A, "R", 0
        .byte $00, $0B, "U", 0
        .byte $00, $0C, "N", 0
        .byte $00, $0D, "P", 0
        .byte $00, $0E, "R", 0
        .byte $00, $0F, "O", 0
        .byte $00, $10, "J", 0
        .byte $00, $11, "E", 0
        .byte $00, $12, "C", 0
        .byte $00, $13, "T", 0
        .byte $00, $14, "Shield 9A.5f Ok", 0
        .byte $00, $15, "Flux % 5.0177 Ok", 0
        .byte $00, $16, "CDI Vector ok", 0
        .byte $00, $17, " %%%ddd ok", 0
        .byte $00, $18, "Race-Track ok", 0
        .byte $00, $19, "SYNCHROTRON", 0
        .byte $00, $1A, "E: 23%", $0A, "g: .005", $0A, $0A, "RK: 77.2L", $0A, $0A, "opt: g+", $0A, $0A, " Shield:", $0A, "1: OFF", $0A, "2: ON", $0A, "3: ON", $0A, $0A, "P~: 1", 0
        .byte $00, $1B, "ON", 0
        .byte $00, $1C, "-", 0
        .byte $00, $21, "|", 0
        .byte $00, $22, "--- Theoretical study ---", 0
        .byte $00, $23, " THE EXPERIMENT WILL BEGIN IN    SECONDS", 0
        .byte $00, $24, "  20", 0
        .byte $00, $25, "  19", 0
        .byte $00, $26, "  18", 0
        .byte $00, $27, "  4", 0
        .byte $00, $28, "  3", 0
        .byte $00, $29, "  2", 0
        .byte $00, $2A, "  1", 0
        .byte $00, $2B, "  0", 0
        .byte $00, $2C, "L E T ' S   G O", 0
        .byte $00, $31, "- Phase 0:", $0A, "INJECTION of particles", $0A, "into synchrotron", 0
        .byte $00, $32, "- Phase 1:", $0A, "Particle ACCELERATION.", 0
        .byte $00, $33, "- Phase 2:", $0A, "EJECTION of particles", $0A, "on the shield.", 0
        .byte $00, $34, "A  N  A  L  Y  S  I  S", 0
        .byte $00, $35, "- RESULT:", $0A, "Probability of creating:", $0A, " ANTIMATTER: 91.V %", $0A, " NEUTRINO 27:  0.04 %", $0A, " NEUTRINO 424: 18 %", 0
        .byte $00, $36, "   Practical verification Y/N ?", 0
        .byte $00, $37, "SURE ?", 0
        .byte $00, $38, "MODIFICATION OF PARAMETERS", $0A, "RELATING TO PARTICLE", $0A, "ACCELERATOR (SYNCHROTRON).", 0
        .byte $00, $39, "       RUN EXPERIMENT ?", 0
        .byte $00, $3C, "t---t", 0
        .byte $00, $3D, "000 ~", 0
        .byte $00, $3E, ".20x14dd", 0
        .byte $00, $3F, "gj5r5r", 0
        .byte $00, $40, "tilgor 25%", 0
        .byte $00, $41, "12% 33% checked", 0
        .byte $00, $42, "D=4.2158005584", 0
        .byte $00, $43, "d=10.00001", 0
        .byte $00, $44, "+", 0
        .byte $00, $45, "*", 0
        .byte $00, $46, "% 304", 0
        .byte $00, $47, "gurgle 21", 0
        .byte $00, $48, "{{{{", 0
        .byte $00, $49, "Delphine Software", 0
        .byte $00, $4A, "By Eric Chahi", 0
        .byte $00, $4B, "  5", 0
        .byte $00, $4C, "  17", 0
        .byte $01, $2C, "0", 0
        .byte $01, $2D, "1", 0
        .byte $01, $2E, "2", 0
        .byte $01, $2F, "3", 0
        .byte $01, $30, "4", 0
        .byte $01, $31, "5", 0
        .byte $01, $32, "6", 0
        .byte $01, $33, "7", 0
        .byte $01, $34, "8", 0
        .byte $01, $35, "9", 0
        .byte $01, $36, "A", 0
        .byte $01, $37, "B", 0
        .byte $01, $38, "C", 0
        .byte $01, $39, "D", 0
        .byte $01, $3A, "E", 0
        .byte $01, $3B, "F", 0
        .byte $01, $3C, "        ACCESS CODE:", 0
        .byte $01, $3D, "PRESS GAMEPAD BUTTON OR SPACE TO CONTINUE", 0
        .byte $01, $3E, "   ENTER ACCESS CODE", 0
        .byte $01, $3F, "   INVALID PASSWORD !", 0
        .byte $01, $40, "ANNULER", 0
        .byte $01, $41, "      INSERT DISK ?", $0A, $0A, $0A, $0A, $0A, $0A, $0A, $0A, $0A, "PRESS ANY KEY TO CONTINUE", 0
        .byte $01, $42, " SELECT SYMBOLS CORRESPONDING TO", $0A, " THE POSITION", $0A, " ON THE CODE WHEEL", 0
        .byte $01, $43, "    LOADING...", 0
        .byte $01, $44, "              ERROR", 0
        .byte $01, $5E, "LDKD", 0
        .byte $01, $5F, "HTDC", 0
        .byte $01, $60, "CLLD", 0
        .byte $01, $61, "FXLC", 0
        .byte $01, $62, "KRFK", 0
        .byte $01, $63, "XDDJ", 0
        .byte $01, $64, "LBKG", 0
        .byte $01, $65, "KLFB", 0
        .byte $01, $66, "TTCT", 0
        .byte $01, $67, "DDRX", 0
        .byte $01, $68, "TBHK", 0
        .byte $01, $69, "BRTD", 0
        .byte $01, $6A, "CKJL", 0
        .byte $01, $6B, "LFCK", 0
        .byte $01, $6C, "BFLX", 0
        .byte $01, $6D, "XJRT", 0
        .byte $01, $6E, "HRTB", 0
        .byte $01, $6F, "HBHK", 0
        .byte $01, $70, "JCGB", 0
        .byte $01, $71, "HHFL", 0
        .byte $01, $72, "TFBB", 0
        .byte $01, $73, "TXHF", 0
        .byte $01, $74, "JHJL", 0
        .byte $01, $81, " BY", 0
        .byte $01, $82, "ERIC CHAHI", 0
        .byte $01, $83, "         MUSIC AND SOUND EFFECTS", 0
        .byte $01, $84, " ", 0
        .byte $01, $85, "JEAN-FRANCOIS FREITAS", 0
        .byte $01, $86, "Commander X16 Version", 0
        .byte $01, $87, "      By", 0
        .byte $01, $88, " Rainer De Temple", 0
        .byte $01, $8B, "       THEN PRESS FIRE", 0
        .byte $01, $8C, " PUT THE PADDLE ON THE UPPER LEFT CORNER", 0
        .byte $01, $8D, "PUT THE PADDLE IN CENTRAL POSITION", 0
        .byte $01, $8E, "PUT THE PADDLE ON THE LOWER RIGHT CORNER", 0
        .byte $02, $58, "      Designed by ..... Eric Chahi", 0
        .byte $02, $59, "    Programmed by...... Eric Chahi", 0
        .byte $02, $5A, "      Artwork ......... Eric Chahi", 0
        .byte $02, $5B, "Music by ........ Jean-francois Freitas", 0
        .byte $02, $5C, "            Sound effects", 0
        .byte $02, $5D, "        Jean-Francois Freitas", $0A, "             Eric Chahi", 0
        .byte $02, $63, "              Thanks To", 0
        .byte $02, $64, "           Jesus Martinez", $0A, $0A, "          Daniel Morais", $0A, $0A, "        Frederic Savoir", $0A, $0A, "      Cecile Chahi", $0A, $0A, "    Philippe Delamarre", $0A, $0A, "  Philippe Ulrich", $0A, $0A, "Sebastien Berthet", $0A, $0A, "Pierre Gousseau", 0
        .byte $02, $65, "Now Go Out Of This World", 0
        .byte $01, $90, "Good evening professor.", 0
        .byte $01, $91, "I see you have driven here in your", $0A, "Ferrari.", 0
        .byte $01, $92, "IDENTIFICATION", 0
        .byte $01, $93, "Monsieur est en parfaite sante.", 0
        .byte $01, $94, "Y", 0
        .byte $01, $93, "AU BOULOT !!!", 0

    charmap_row_lookup: ; lookup table for 64x32 (2 bytes each)
        .repeat 32, i
            .word (($E000) + (i * 2 * 64))
        .endrepeat

.segment "BSS"
    text:   .res 256
    text_length: .res 1

.segment "CODE"

; ---------------------------------------------------------------
; Display text at text pointer
; X: x position in tiles
; Y: y position in tiles
; ---------------------------------------------------------------
.proc display_text
    jsr PLOT ; set cursor position

    ldx #0
    loop:
        lda text,x
        cmp #0
        beq end
        jsr CHROUT
        inx
        bra loop
    end:
    stz text_length
    rts
.endproc

; ---------------------------------------------------------------
; Convert a byte to a hexidecimal string and put it at the end
; of the text buffer
; A: byte to convert
; ---------------------------------------------------------------
.proc hex2text
    pha
    ; convert byte to hexidecimal string
    lsr
    lsr
    lsr
    lsr
    jsr hex2char
    ldx text_length
    sta text,x
    inc text_length
    
    pla
    jsr hex2char
    ldx text_length
    sta text,x
    inc text_length
    rts
.endproc

; ---------------------------------------------------------------
; Convert a low nibble to a hexidecimal character
; A: byte to convert
; ---------------------------------------------------------------
.proc hex2char
    and #$0F
    cmp #10
    bcc @digit
    adc #6  ; convert to A-F
    @digit:
        adc #'0'
    rts
.endproc