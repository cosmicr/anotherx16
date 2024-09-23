; ---------------------------------------------------------------
; main.s
; AnotherX16 - Commander X16 port of Another World
; ---------------------------------------------------------------

; X16 and CBM includes
.include "cx16.inc"
.include "cbm_kernal.inc"

; Project includes
.include "main.inc"
.include "vera.inc"
.include "resource.inc"
.include "engine.inc"
.include "tasks.inc"
.include "macros.inc"
.include "text.inc"
.include "debug.inc"
.include "polygon.inc"

.segment "STARTUP"

.segment "INIT"

.segment "ONCE"

; todo: clean up zeropage variables
.segment "ZEROPAGE"
    work:   .res 24
    temp:   .res 4
    read:   .res 6
    mtemp:  .res 1
    flag:   .res 1

.segment "DATA"
    frame_counter: .res 2

.segment "CODE"

; ---------------------------------------------------------------
; Main program
; ---------------------------------------------------------------

    ; debugging stuff
    stz frame_counter
    stz frame_counter+1
    stz flag
    lda #1
    stz debug_mode

    jsr init_vera
    jsr init_resources
    jsr init_engine
    jsr init_game

    lda #0
    sta state+engine::draw_page
    lda #0
    jsr clear_page
    lda #0
    jsr clear_page
    wai
    wai
    wai
; ; Test 1: Full-Width Line at the Top of the Screen
; stz line_info+line_data::x1
; stz line_info+line_data::x1+1
; stz line_info+line_data::y1
; lda #<319
; sta line_info+line_data::x2
; lda #>319
; sta line_info+line_data::x2+1
; lda #1
; sta polygon_info+polygon_data::color
; jsr draw_line
; wai
; wai
; stp

; ; Test 2: Half-Width Line at the Top-Left of the Screen
; stz line_info+line_data::x1
; stz line_info+line_data::x1+1
; stz line_info+line_data::y1
; lda #<159
; sta line_info+line_data::x2
; lda #>159
; sta line_info+line_data::x2+1
; lda #2
; sta polygon_info+polygon_data::color
; jsr draw_line
; wai
; wai
; stp

; ; Test 3: Half-Width Line at the Top-Right of the Screen
; lda #<160
; sta line_info+line_data::x1
; lda #>160
; sta line_info+line_data::x1+1
; stz line_info+line_data::y1
; lda #<319
; sta line_info+line_data::x2
; lda #>319
; sta line_info+line_data::x2+1
; lda #3
; sta polygon_info+polygon_data::color
; jsr draw_line
; wai
; wai
; stp

; ; Test 4: Full-Width Line at the Middle of the Screen
; stz line_info+line_data::x1
; stz line_info+line_data::x1+1
; lda #100
; sta line_info+line_data::y1
; lda #<319
; sta line_info+line_data::x2
; lda #>319
; sta line_info+line_data::x2+1
; lda #4
; sta polygon_info+polygon_data::color
; jsr draw_line
; wai
; wai
; stp

; ; Test 5: Quarter-Width Line in the Center
; lda #<120
; sta line_info+line_data::x1
; lda #>120
; sta line_info+line_data::x1+1
; lda #100
; sta line_info+line_data::y1
; lda #<199
; sta line_info+line_data::x2
; lda #>199
; sta line_info+line_data::x2+1
; lda #5
; sta polygon_info+polygon_data::color
; jsr draw_line
; wai
; wai
; stp

; ; *** Test 6: Full-Width Line at the Bottom of the Screen
; stz line_info+line_data::x1
; stz line_info+line_data::x1+1
; lda #199
; sta line_info+line_data::y1
; lda #<319
; sta line_info+line_data::x2
; lda #>319
; sta line_info+line_data::x2+1
; lda #6
; sta polygon_info+polygon_data::color
; jsr draw_line
; wai
; wai
; stp

; ; Test 7: Random Short Line
; lda #<50
; sta line_info+line_data::x1
; lda #>50
; sta line_info+line_data::x1+1
; lda #50
; sta line_info+line_data::y1
; lda #<150
; sta line_info+line_data::x2
; lda #>150
; sta line_info+line_data::x2+1
; lda #7
; sta polygon_info+polygon_data::color
; jsr draw_line
; wai
; wai
; stp

; ; Test 8: Line in the Bottom-Right Corner
; lda #<240
; sta line_info+line_data::x1
; lda #>240
; sta line_info+line_data::x1+1
; lda #199
; sta line_info+line_data::y1
; lda #<319
; sta line_info+line_data::x2
; lda #>319
; sta line_info+line_data::x2+1
; lda #8
; sta polygon_info+polygon_data::color
; jsr draw_line
; wai
; wai
; stp

; ; Test 9: 1-Pixel Line in the Center
; lda #<160
; sta line_info+line_data::x1
; lda #>160
; sta line_info+line_data::x1+1
; lda #100
; sta line_info+line_data::y1
; lda #<160
; sta line_info+line_data::x2
; lda #>160
; sta line_info+line_data::x2+1
; lda #9
; sta polygon_info+polygon_data::color
; jsr draw_line
; wai
; wai
; stp

; ; Test 10: 2-Pixel Line in the Top-Left Corner
; stz line_info+line_data::x1
; stz line_info+line_data::x1+1
; stz line_info+line_data::y1
; lda #<1
; sta line_info+line_data::x2
; lda #>1
; sta line_info+line_data::x2+1
; lda #10
; sta polygon_info+polygon_data::color
; jsr draw_line
; wai
; wai
; stp

; ; Test 11: 2-Pixel Line at the Bottom of the Screen
; lda #<318
; sta line_info+line_data::x1
; lda #>318
; sta line_info+line_data::x1+1
; lda #199
; sta line_info+line_data::y1
; lda #<319
; sta line_info+line_data::x2
; lda #>319
; sta line_info+line_data::x2+1
; lda #11
; sta polygon_info+polygon_data::color
; jsr draw_line
; wai
; wai
; stp

; ; Test 12: 1-Pixel Line at the Bottom-Right Corner
; lda #<318
; sta line_info+line_data::x1
; lda #>318
; sta line_info+line_data::x1+1
; lda #199
; sta line_info+line_data::y1
; lda #<318
; sta line_info+line_data::x2
; lda #>318
; sta line_info+line_data::x2+1
; lda #12
; sta polygon_info+polygon_data::color
; jsr draw_line
; wai
; wai
; stp

; ; Test 13: 1-Pixel Line in the Top-Right Corner
; lda #<319
; sta line_info+line_data::x1
; lda #>319
; sta line_info+line_data::x1+1
; stz line_info+line_data::y1
; lda #<319
; sta line_info+line_data::x2
; lda #>319
; sta line_info+line_data::x2+1
; lda #13
; sta polygon_info+polygon_data::color
; jsr draw_line
; wai
; wai
; stp

; ; Test 14: a line starting at 1,1 and ending at 2,1
; lda #100
; sta line_info+line_data::y1
; lda #<0
; sta line_info+line_data::x1
; lda #>0
; sta line_info+line_data::x1+1
; lda #<319
; sta line_info+line_data::x2
; lda #>319
; sta line_info+line_data::x2+1
; lda #14
; sta polygon_info+polygon_data::color
; jsr draw_line
; wai
; wai
; stp



    ; GAME LOOP
    @loop:
        jsr run_tasks
        bra @loop
    
exit:
    jsr RESTOR
    jsr CINT
    rts
; ---------------------------------------------------------------
; End of main program
; ----------------------------------------------------------------


