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
.include "vera.inc"
.include "resource.inc"
.include "polygon.inc"

.segment "ZEROPAGE"
    text: .res 2
    text_col: .res 1

.segment "RODATA"
    game_font:                                                                ;     Index Scrcde
        ; .byte $00,$00,$00,$00,$00,$00,$00,$00,$10,$10,$10,$10,$10,$00,$10,$00 ;   ! 00 01 20 21
        ; .byte $28,$28,$00,$00,$00,$00,$00,$00,$00,$24,$7E,$24,$24,$7E,$24,$00 ; " # 02 03 22 23
        ; .byte $08,$3E,$48,$3C,$12,$7C,$10,$00,$42,$A4,$48,$10,$24,$4A,$84,$00 ; $ % 04 05 24 25
        ; .byte $60,$90,$90,$70,$8A,$84,$7A,$00,$08,$08,$10,$00,$00,$00,$00,$00 ; & ' 06 07 26 27
        ; .byte $06,$08,$10,$10,$10,$08,$06,$00,$C0,$20,$10,$10,$10,$20,$C0,$00 ; ( ) 08 09 28 29
        ; .byte $00,$44,$28,$10,$28,$44,$00,$00,$00,$10,$10,$7C,$10,$10,$00,$00 ; * + 0A 0B 2A 2B
        ; .byte $00,$00,$00,$00,$00,$10,$10,$20,$00,$00,$00,$7C,$00,$00,$00,$00 ; , - 0C 0D 2C 2D
        ; .byte $00,$00,$00,$00,$10,$28,$10,$00,$00,$04,$08,$10,$20,$40,$00,$00 ; . / 0E 0F 2E 2F
        ; .byte $78,$84,$8C,$94,$A4,$C4,$78,$00,$10,$30,$50,$10,$10,$10,$7C,$00 ; 0 1 10 11 30 31
        ; .byte $78,$84,$04,$08,$30,$40,$FC,$00,$78,$84,$04,$38,$04,$84,$78,$00 ; 2 3 12 13 32 33 
        ; .byte $08,$18,$28,$48,$FC,$08,$08,$00,$FC,$80,$F8,$04,$04,$84,$78,$00 ; 4 5 14 15 34 35 
        ; .byte $38,$40,$80,$F8,$84,$84,$78,$00,$FC,$04,$04,$08,$10,$20,$40,$00 ; 6 7 16 17 36 37
        ; .byte $78,$84,$84,$78,$84,$84,$78,$00,$78,$84,$84,$7C,$04,$08,$70,$00 ; 8 9 18 19 38 39
        ; .byte $00,$18,$18,$00,$00,$18,$18,$00,$00,$00,$18,$18,$00,$10,$10,$60 ; : ; 1A 1B 3A 3B
        ; .byte $04,$08,$10,$20,$10,$08,$04,$00,$00,$00,$FE,$00,$00,$FE,$00,$00 ; < = 1C 1D 3C 3D
        ; .byte $20,$10,$08,$04,$08,$10,$20,$00,$7C,$82,$02,$0C,$10,$00,$10,$00 ; > ? 1E 1F 3E 3F
        ; .byte $30,$18,$0C,$0C,$0C,$18,$30,$00,$78,$84,$84,$FC,$84,$84,$84,$00 ; @ A 20 21 40 C1
        ; .byte $F8,$84,$84,$F8,$84,$84,$F8,$00,$78,$84,$80,$80,$80,$84,$78,$00 ; B C 22 23 C2 C3
        ; .byte $F8,$84,$84,$84,$84,$84,$F8,$00,$7C,$40,$40,$78,$40,$40,$7C,$00 ; D E 24 25 C4 C5
        ; .byte $FC,$80,$80,$F0,$80,$80,$80,$00,$7C,$80,$80,$8C,$84,$84,$7C,$00 ; F G 26 27 C6 C7
        ; .byte $84,$84,$84,$FC,$84,$84,$84,$00,$7C,$10,$10,$10,$10,$10,$7C,$00 ; H I 28 29 C8 C9
        ; .byte $04,$04,$04,$04,$84,$84,$78,$00,$8C,$90,$A0,$E0,$90,$88,$84,$00 ; J K 2A 2B CA CB
        ; .byte $80,$80,$80,$80,$80,$80,$FC,$00,$82,$C6,$AA,$92,$82,$82,$82,$00 ; L M 2C 2D CC CD
        ; .byte $84,$C4,$A4,$94,$8C,$84,$84,$00,$78,$84,$84,$84,$84,$84,$78,$00 ; N O 2E 2F CE CF
        ; .byte $F8,$84,$84,$F8,$80,$80,$80,$00,$78,$84,$84,$84,$84,$8C,$7C,$03 ; P Q 30 31 D0 D1
        ; .byte $F8,$84,$84,$F8,$90,$88,$84,$00,$78,$84,$80,$78,$04,$84,$78,$00 ; R S 32 33 D2 D3
        ; .byte $7C,$10,$10,$10,$10,$10,$10,$00,$84,$84,$84,$84,$84,$84,$78,$00 ; T U 34 35 D4 D5
        ; .byte $84,$84,$84,$84,$84,$48,$30,$00,$82,$82,$82,$82,$92,$AA,$C6,$00 ; V W 36 37 D6 D7
        ; .byte $82,$44,$28,$10,$28,$44,$82,$00,$82,$44,$28,$10,$10,$10,$10,$00 ; X Y 38 39 D8 D9
        ; .byte $FC,$04,$08,$10,$20,$40,$FC,$00,$3C,$30,$30,$30,$30,$30,$3C,$00 ; Z [ 3A 3B DA 5B
        ; .byte $3C,$30,$30,$30,$30,$30,$3C,$00,$3C,$30,$30,$30,$30,$30,$3C,$00 ; [ [ 3C 3D 5B 5B
        ; .byte $3C,$30,$30,$30,$30,$30,$3C,$00,$00,$00,$00,$00,$00,$00,$00,$FE ; [ _ 3E 3F 5B A4
        ; .byte $3C,$30,$30,$30,$30,$30,$3C,$00,$00,$00,$38,$04,$3C,$44,$3C,$00 ; [ a 40 41 5B 41
        ; .byte $40,$40,$78,$44,$44,$44,$78,$00,$00,$00,$3C,$40,$40,$40,$3C,$00 ; b c 42 43 42 43
        ; .byte $04,$04,$3C,$44,$44,$44,$3C,$00,$00,$00,$38,$44,$7C,$40,$3C,$00 ; d e 44 45 44 45
        ; .byte $38,$44,$40,$60,$40,$40,$40,$00,$00,$00,$3C,$44,$44,$3C,$04,$78 ; f g 46 47 46 47
        ; .byte $40,$40,$58,$64,$44,$44,$44,$00,$10,$00,$10,$10,$10,$10,$10,$00 ; h i 48 49 48 49
        ; .byte $02,$00,$02,$02,$02,$02,$42,$3C,$40,$40,$46,$48,$70,$48,$46,$00 ; j k 4A 4B 4A 4B
        ; .byte $10,$10,$10,$10,$10,$10,$10,$00,$00,$00,$EC,$92,$92,$92,$92,$00 ; l m 4C 4D 4C 4D
        ; .byte $00,$00,$78,$44,$44,$44,$44,$00,$00,$00,$38,$44,$44,$44,$38,$00 ; n o 4E 4F 4E 4F
        ; .byte $00,$00,$78,$44,$44,$78,$40,$40,$00,$00,$3C,$44,$44,$3C,$04,$04 ; p q 50 51 50 51
        ; .byte $00,$00,$4C,$70,$40,$40,$40,$00,$00,$00,$3C,$40,$38,$04,$78,$00 ; r s 52 53 52 53
        ; .byte $10,$10,$3C,$10,$10,$10,$0C,$00,$00,$00,$44,$44,$44,$44,$78,$00 ; t u 54 55 54 55
        ; .byte $00,$00,$44,$44,$44,$28,$10,$00,$00,$00,$82,$82,$92,$AA,$C6,$00 ; v w 56 57 56 57
        ; .byte $00,$00,$44,$28,$10,$28,$44,$00,$00,$00,$42,$22,$24,$18,$08,$30 ; x y 58 59 58 59
        ; .byte $00,$00,$7C,$08,$10,$20,$7C,$00,$60,$90,$20,$40,$F0,$00,$00,$00 ; z ² 5A 5B 5A
        ; .byte $FE,$FE,$FE,$FE,$FE,$FE,$FE,$00,$38,$44,$BA,$A2,$BA,$44,$38,$00 ;   © 5C 5D
        ; .byte $38,$44,$82,$82,$44,$28,$EE,$00,$55,$AA,$55,$AA,$55,$AA,$55,$AA ; Ω   5E 5F
            .byte $00,$00,$00,$00,$00,$00,$00,$00,$10,$10,$10,$10,$10,$00,$10,$00
    .byte $28,$28,$00,$00,$00,$00,$00,$00,$00,$50,$F8,$50,$50,$F8,$50,$00
    .byte $20,$78,$A0,$70,$28,$F0,$20,$00,$40,$A4,$48,$10,$20,$48,$94,$08
    .byte $60,$90,$90,$60,$94,$88,$74,$00,$08,$08,$10,$00,$00,$00,$00,$00
    .byte $0C,$10,$20,$20,$20,$10,$0C,$00,$C0,$20,$10,$10,$10,$20,$C0,$00
    .byte $00,$44,$28,$10,$28,$44,$00,$00,$00,$10,$10,$7C,$10,$10,$00,$00
    .byte $00,$00,$00,$00,$00,$10,$10,$20,$00,$00,$00,$7C,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$10,$28,$10,$00,$00,$04,$08,$10,$20,$40,$00,$00
    .byte $70,$88,$98,$A8,$88,$C8,$70,$00,$10,$30,$50,$10,$10,$10,$78,$00
    .byte $70,$88,$08,$10,$20,$40,$F8,$00,$70,$88,$08,$30,$08,$88,$70,$00
    .byte $10,$30,$50,$90,$F8,$10,$10,$00,$F8,$80,$F0,$08,$08,$88,$70,$00
    .byte $30,$40,$80,$F0,$88,$88,$70,$00,$F8,$08,$08,$10,$20,$40,$80,$00
    .byte $70,$88,$88,$70,$88,$88,$70,$00,$70,$88,$88,$78,$08,$10,$60,$00
    .byte $00,$18,$18,$00,$00,$18,$18,$00,$00,$00,$18,$18,$00,$10,$10,$60
    .byte $08,$10,$20,$40,$20,$10,$08,$00,$00,$00,$F8,$00,$00,$F8,$00,$00
    .byte $20,$10,$08,$04,$08,$10,$20,$00,$78,$84,$04,$18,$20,$00,$20,$00
    .byte $30,$18,$0C,$0C,$0C,$18,$30,$00,$70,$88,$88,$F8,$88,$88,$88,$00
    .byte $F0,$88,$88,$F0,$88,$88,$F0,$00,$70,$88,$80,$80,$80,$88,$70,$00
    .byte $F0,$88,$88,$88,$88,$88,$F0,$00,$78,$40,$40,$70,$40,$40,$78,$00
    .byte $F8,$80,$80,$E0,$80,$80,$80,$00,$78,$80,$80,$98,$88,$88,$78,$00
    .byte $88,$88,$88,$F8,$88,$88,$88,$00,$70,$20,$20,$20,$20,$20,$70,$00
    .byte $08,$08,$08,$08,$88,$88,$70,$00,$88,$90,$A0,$C0,$A0,$90,$88,$00
    .byte $80,$80,$80,$80,$80,$80,$F8,$00,$88,$D8,$A8,$A8,$88,$88,$88,$00
    .byte $88,$C8,$A8,$A8,$98,$88,$88,$00,$70,$88,$88,$88,$88,$88,$70,$00
    .byte $F0,$88,$88,$F0,$80,$80,$80,$00,$70,$88,$88,$88,$88,$98,$78,$04
    .byte $F0,$88,$88,$F0,$A0,$90,$88,$00,$70,$88,$80,$70,$08,$88,$70,$00
    .byte $F8,$20,$20,$20,$20,$20,$20,$00,$88,$88,$88,$88,$88,$88,$70,$00
    .byte $88,$88,$50,$50,$50,$20,$20,$00,$88,$88,$88,$88,$A8,$D8,$88,$00
    .byte $88,$50,$50,$20,$50,$50,$88,$00,$88,$88,$50,$20,$20,$20,$20,$00
    .byte $F8,$10,$10,$20,$40,$40,$F8,$00,$3C,$30,$30,$30,$30,$30,$3C,$00
    .byte $3C,$30,$30,$30,$30,$30,$3C,$00,$3C,$30,$30,$30,$30,$30,$3C,$00
    .byte $3C,$30,$30,$30,$30,$30,$3C,$00,$00,$00,$00,$00,$00,$00,$00,$FC
    .byte $3C,$30,$30,$30,$30,$30,$3C,$00,$00,$00,$38,$04,$3C,$44,$3C,$00
    .byte $40,$40,$78,$44,$44,$44,$78,$00,$00,$00,$3C,$40,$40,$40,$3C,$00
    .byte $04,$04,$3C,$44,$44,$44,$3C,$00,$00,$00,$38,$44,$7C,$40,$3C,$00
    .byte $38,$44,$40,$60,$40,$40,$40,$00,$00,$00,$3C,$44,$44,$3C,$04,$78
    .byte $40,$40,$58,$64,$44,$44,$44,$00,$10,$00,$10,$10,$10,$10,$10,$00
    .byte $04,$00,$04,$04,$04,$04,$44,$38,$40,$40,$44,$48,$70,$48,$44,$00
    .byte $10,$10,$10,$10,$10,$10,$10,$00,$00,$00,$EC,$92,$92,$92,$92,$00
    .byte $00,$00,$78,$44,$44,$44,$44,$00,$00,$00,$38,$44,$44,$44,$38,$00
    .byte $00,$00,$78,$44,$44,$78,$40,$40,$00,$00,$3C,$44,$44,$3C,$04,$04
    .byte $00,$00,$4C,$70,$40,$40,$40,$00,$00,$00,$3C,$40,$38,$04,$78,$00
    .byte $10,$10,$3C,$10,$10,$10,$0C,$00,$00,$00,$44,$44,$44,$44,$78,$00
    .byte $00,$00,$44,$44,$44,$28,$10,$00,$00,$00,$82,$82,$92,$AA,$C6,$00
    .byte $00,$00,$44,$28,$10,$28,$44,$00,$00,$00,$42,$22,$24,$18,$08,$30
    .byte $00,$00,$7C,$08,$10,$20,$7C,$00,$60,$90,$20,$40,$F0,$00,$00,$00
    .byte $FC,$FC,$FC,$FC,$FC,$FC,$FC,$00,$38,$44,$BA,$A2,$BA,$44,$38,$00
    .byte $38,$44,$82,$82,$44,$28,$EE,$00,$54,$A8,$54,$A8,$54,$A8,$54,$A8

    ; This will remap characters back to ASCII
    .repeat 256, i
        .charmap i, i
    .endrepeat

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
        .byte $00, $23, " THE EXPERIMENT WILL BEGIN IN     SECONDS", 0
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
        .byte $01, $3D, "     PRESS BUTTON TO CONTINUE", 0
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
        .byte $01, $86, "Commander X16", 0
        .byte $01, $87, "Version By", 0
        .byte $01, $88, "Rainer De Temple", 0
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

.segment "BSS"
    text_start: .res 1
    text_length: .res 1

.segment "CODE"

; A: char to display
.proc display_char
    ; 8 bytes per char, 1 byte per row
    ; pos = game_font + (A * 8)
    sta temp
    stz temp+1
    asl temp
    rol temp+1
    asl temp
    rol temp+1
    asl temp ; *8
    rol temp+1

    clc
    lda #<game_font
    adc temp
    sta temp
    lda #>game_font
    adc temp+1
    sta temp+1 ; temp = game_font + (A * 8)

    ; we'll use the line data to setup the VERA address
    setup_line

    col = temp+2
    byte = temp+3

    col_left = temp+4
    col_right = temp+5

    lda text_col 
    asl 
    asl
    asl
    asl
    sta col_left 

    lda text_col
    and #$0F
    sta col_right

    .repeat 8 ; 8 rows
        lda (temp)
        sta byte
        inc temp

        lda #1
        sta VERA::CTRL  ; data port 1
        lda VERA::ADDR+2
        and #$01
        ora #(VERA::INC1)
        sta VERA::ADDR+2
        stz VERA::CTRL  ; data port 0
        lda VERA::ADDR+2
        and #$01
        ora #(VERA::INC1)
        sta VERA::ADDR+2

        ; first two pixels
        lda VERA::DATA1
        sta col
        lda byte
        bit #$80
        beq :+
        lda col
        and #$0F
        ora col_left
        sta col
        :
        lda byte
        bit #$40
        beq :+
        lda col
        and #$F0
        ora col_right
        sta col
        :
        lda col
        sta VERA::DATA0

        ; second two pixels
        lda VERA::DATA1
        sta col
        lda byte
        bit #$20
        beq :+
        lda col
        and #$0F
        ora col_left
        sta col
        :
        lda byte
        bit #$10
        beq :+
        lda col
        and #$F0
        ora col_right
        sta col
        :
        lda col
        sta VERA::DATA0

        ; third two pixels
        lda VERA::DATA1
        sta col
        lda byte
        bit #$08
        beq :+
        lda col
        and #$0F
        ora col_left
        sta col
        :
        lda byte
        bit #$04
        beq :+
        lda col
        and #$F0
        ora col_right
        sta col
        :
        lda col
        sta VERA::DATA0

        lda #1
        sta VERA::CTRL  ; data port 1
        lda VERA::ADDR+2
        and #$01
        ora #(VERA::INC160)
        sta VERA::ADDR+2

        stz VERA::CTRL  ; data port 0
        lda VERA::ADDR+2
        and #$01
        ora #(VERA::INC160)
        sta VERA::ADDR+2

        ; fourth two pixels
        lda VERA::DATA1
        sta col
        lda byte
        bit #$02
        beq :+
        lda col
        and #$0F
        ora col_left
        sta col
        :
        lda byte
        bit #$01
        beq :+
        lda col
        and #$F0
        ora col_right
        sta col
        :
        lda col
        sta VERA::DATA0

        lda #1
        sta VERA::CTRL  ; data port 1
        sec
        lda VERA::ADDR
        sbc #$03
        sta VERA::ADDR
        lda VERA::ADDR+1
        sbc #0
        sta VERA::ADDR+1
        lda VERA::ADDR+2
        sbc #0
        sta VERA::ADDR+2

        stz VERA::CTRL
        sec
        lda VERA::ADDR
        sbc #$03
        sta VERA::ADDR
        lda VERA::ADDR+1
        sbc #0
        sta VERA::ADDR+1
        lda VERA::ADDR+2
        sbc #0
        sta VERA::ADDR+2
    .endrepeat
    rts
.endproc

; ---------------------------------------------------------------
; Display text at text pointer
; X: x position in tiles
; Y: y position in tiles
; ---------------------------------------------------------------
.proc display_text
    stx text_start

    ; use line data to setup VERA address
    txa
    asl
    asl
    asl
    tax
    lda scale_lookup,x
    sta line_info+line_data::x1
    sty line_info+line_data::y1

    loop:
        lda (text)
        cmp #0
        beq end
        cmp #$0A ; return/newline
        bne :+
        lda text_start
        asl
        asl
        asl
        tax
        lda scale_lookup,x
        sta line_info+line_data::x1
        clc
        lda line_info+line_data::y1
        adc #8
        sta line_info+line_data::y1
        bra next
        :
        sec
        sbc #32
        jsr display_char
        clc
        lda #6
        adc line_info+line_data::x1
        sta line_info+line_data::x1
        next:
        inc16 text
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