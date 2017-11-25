;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; c64 helpers
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.macpack cbm                            ; adds support for scrcode
.include "c64.inc"                      ; c64 constants

.segment "CODE"                         ; $7400

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; ZP and other variables
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
ZP_SYNC_MUSIC           = $40           ; byte
ZP_SYNC_ANIM            = $41           ; byte
ZP_VIC_VIDEO_TYPE       = $60           ; byte. values:
                                        ;   $01 --> PAL
                                        ;   $2F --> PAL-N
                                        ;   $28 --> NTSC
                                        ;   $2e --> NTSC-OLD
ZP_TIMER_SPEED_LO       = $61           ;byte: value for $dc04
ZP_TIMER_SPEED_HI       = $62           ;byte: value for $dc05
DEBUG = 1

SCROLL_SCREEN   = $7000 + 23 * 40

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; start
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.export start
.proc start

.ifdef DEBUG
        sei

        lda #$35                        ; BASIC & KERNAL out
        sta $01

        lda #$01
        sta ZP_VIC_VIDEO_TYPE           ; set it to PAL in DEBUG mode
.endif

        ldx #$ff                        ; restore stack
        tsx

        ; screen setup
        ;
        lda #1
        sta $d020
        lda #0
        sta $d021
        lda #$03
        sta $d022
        lda #$03
        sta $d023

        lda #0
        sta ZP_SYNC_MUSIC
        sta ZP_SYNC_ANIM

        lda $dd00                       ; Vic bank 1: $4000-$7FFF
        and #%11111100
        ora #2
        sta $dd00

        lda #%00011000                  ; no scroll, multi-color,40-cols
        sta $d016

        lda #%10000000                  ; screen addr $2000, charset at $0000 / bitmap at $0000
        sta $d018

        lda #%00111011                  ; bitmap mode enabled
        sta $d011

        lda #0                          ; no sprites
        sta VIC_SPR_ENA


        ldx #0

@l0:    lda bitmap_color + $0000,x      ; copy color ram, first 22 rows
        sta $d800,x
        lda bitmap_color + $0100,x
        sta $d900,x
        lda bitmap_color + $0200,x
        sta $da00,x
        lda bitmap_color + $0270,x
        sta $da70,x
        inx
        bne @l0

        lda #$01                        ; white (1) row 22
        ldx #40
@l1:    sta $db70,x
        dex
        bpl @l1

        lda #$0a                        ; white (9) rows 23 & 24
        ldx #80                         ; using 9 instead of 1 to enable multi-color mode
@l2:    sta $db70+40,x                  ; for the scroll
        dex
        bpl @l2


        ; IRQ setup

        asl $d019                       ; ACK raster interrupt
        lda $dc0d                       ; ACK timer A interrupt
        lda $dd0d                       ; ACK timer B interrupt

        ldx #<irq_bitmap                ; setup irq
        ldy #>irq_bitmap
        stx $fffe
        sty $ffff

        ldx #<nmi_irq                   ; setup nmi
        ldy #>nmi_irq
        stx $fffa
        sty $fffb

        lda #$01
        sta $d01a                       ; enable raster IRQ

.ifndef DEBUG
        ldx ZP_TIMER_SPEED_LO           ; FIXME: why should I set it up again?
        ldy ZP_TIMER_SPEED_HI           ; chipdisk disk already setup it up
        stx $dc04                       ; but if I don't it plays super fast
        sty $dc05                       ; Do research

        lda #$81
        sta $dc0d                       ; turn on CIA 1 interrups

        lda #0
        jsr $1000                       ; init sid
.endif

        cli

        ; Main loop

main_loop:
        lda ZP_SYNC_MUSIC
        bne play_music

test_anim:
        lda ZP_SYNC_ANIM
        beq main_loop

        dec ZP_SYNC_ANIM

;        dec $d020
        jsr animate_scroll              ; animation
;        inc $d020
        jmp main_loop

play_music:
        dec ZP_SYNC_MUSIC               ; reset music var

.ifndef DEBUG
;        inc $d020
        jsr $1003
;        dec $d020
.endif
        jmp test_anim

.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
nmi_irq:
        rti

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc irq_bitmap
        pha                             ; saves A

        asl $d019                       ; clears raster interrupt
        bcs @is_raster

        lda $dc0d                       ; clears CIA1 timer A interrupt
        inc ZP_SYNC_MUSIC
        jmp @exit

@is_raster:
        lda #0
        sta $d021

        lda #%00011000                  ; no scroll, multi-color,40-cols
        sta $d016

        lda #%10000000                  ; screen addr $2000, charset at $0000 / bitmap at $0000
        sta $d018

        lda #%00111011                  ; bitmap mode enabled
        sta $d011

        lda #<irq_text
        sta $fffe
        lda #>irq_text
        sta $ffff

        lda #50 + (8 * 22) + 4
        sta $d012

        inc ZP_SYNC_ANIM

@exit:
        pla                             ; restores A
        rti                             ; restores previous PC, status
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc irq_text
        pha                             ; saves A

        asl $d019                       ; clears raster interrupt
        bcs @is_raster

        lda $dc0d                       ; clears CIA1 timer A interrupt
        inc ZP_SYNC_MUSIC
        jmp @exit

@is_raster:
        lda #1
        sta $d021

        lda scroll_x
        ora #%00010000                  ; set MCM on
        sta $d016

        lda #%11001010                  ; screen addr $3000, charset at $2800
        sta $d018

        lda #%00011011                  ; bitmap mode disabled
        sta $d011

        lda #<irq_bitmap
        sta $fffe
        lda #>irq_bitmap
        sta $ffff

        lda #20
        sta $d012

@exit:
        pla                             ; restores A
        rti                             ; restores previous PC, status
.endproc


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
;void animate_scroll
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc animate_scroll
        ; speed control
        ldx scroll_x

.repeat 3                               ;scroll speed
        dec scroll_x
.endrepeat

        lda scroll_x
        and #07
        sta scroll_x

        cpx scroll_x
        bcc @do_scroll
        rts

@do_scroll:
        ; move the chars to the left
        ldx #0
@l0:
        lda SCROLL_SCREEN+1,x                   ; scroll top part of 1x2 char
        sta SCROLL_SCREEN,x
        lda SCROLL_SCREEN+40+1,x                ; scroll bottom part of 1x2 char
        sta SCROLL_SCREEN+40,x
        inx
        cpx #39
        bne @l0

        ; put next char in column 40
scroll_idx = *+1
        lda scroll_txt
        cmp #$ff
        bne :+

        ; reached $ff ? Then start from the beginning
        ldx #<scroll_txt
        ldy #>scroll_txt
        stx scroll_idx
        sty scroll_idx+1

        lda #0
        sta half_char
        lda scroll_txt

:       ora half_char                           ; right part ? left part will be 0

        sta SCROLL_SCREEN+39                    ; top part of the 2x2
        ora #$80                                ; bottom part is 128 chars ahead in the charset
        sta SCROLL_SCREEN+40+39                 ; bottom part of the 1x2 char

        ; half char
        lda half_char
        eor #$40
        sta half_char
        bne @endscroll

        ; only inc scroll_idx after 2 chars are printed
        inc scroll_idx
        bne :+
        inc scroll_idx+1
:

@endscroll:
        rts
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;

; variables
sync:           .byte 1
scroll_x:       .byte 7
speed:          .byte 3
scroll_idx:     .byte 0
half_char:      .byte 0

charset_colors:
        .incbin "easter_charset_2x2-colors.bin"

scroll_txt:
        scrcode "this is our little homage to the classics of brazilian popular music, "
        scrcode "we're sorry we could only pick a few out of so many great songs from brazil's rich culture. "
        scrcode "there's a lot of room for creating c64 music disks based in latin american music. "
        scrcode "we at pvm want to invite you to the flash party 20 year anniversary, "
        scrcode "a demoscene event we're planning for 2018 in buenos aires. it will be a great time to "
        scrcode "meet and share our geeky obsessions, so please stay tuned for dates and location! "
        scrcode "the song you're listening to now is uctumi's attempt at a version of the felicidade song. "
        scrcode "samba and bossa nova are really tough to get right, so this is the best he could do. "
        scrcode "and the graphic that you're watching is alakran's homage to the great brazilian illustrator lobo. "
        scrcode "credits: code by riq, gfx by alakran, charset by arlequin and music by uctumi. "
        scrcode "greetings to our friends at garoa hacker clube of brazil from pungas de villa martelli, argentina, 2017. have a happy new year!! "
        scrcode "                                  "
        .byte $ff

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.segment "BITMAP"       ; $4000
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
        .incbin "cristo.vsf.bitmap"

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.segment "SCREENRAM"    ; $6000
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
        .incbin "cristo.vsf.colmap"

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.segment "COLORRAM"     ; $6400
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
bitmap_color:
        .incbin "cristo.vsf.attrib"

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.segment "CHARSET"      ; $6800
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
        .incbin "easter_charset_2x2-charset.bin"
