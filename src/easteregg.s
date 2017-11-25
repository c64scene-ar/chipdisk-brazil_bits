;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; c64 helpers
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.macpack cbm                            ; adds support for scrcode
.include "c64.inc"                      ; c64 constants

.segment "CODE"                         ; $7000

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
;DEBUG = 1

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

        lda #$01                        ; rest should be 1 (white)
@l00:   sta $db70,x                     ; for the labels
        inx
        bne @l00


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

        ldx ZP_TIMER_SPEED_LO           ; FIXME: why should I set it up again?
        ldy ZP_TIMER_SPEED_HI           ; chipdisk disk already setup it up
        stx $dc04                       ; but if I don't it plays super fast
        sty $dc05                       ; Do research

        lda #$01
        sta $d01a                       ; enable raster IRQ

        lda #$81
        sta $dc0d                       ; turn on CIA 1 interrups


.ifndef DEBUG
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
irq_bitmap:
        pha                             ; saves A

        asl $d019                       ; clears raster interrupt
        bcs @is_raster

        lda $dc0d                       ; clears CIA1 timer A interrupt
        inc ZP_SYNC_MUSIC
        jmp @exit

@is_raster:
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

        lda #50 + (8 * 23) + 2
        sta $d012

        inc ZP_SYNC_ANIM

@exit:
        pla                             ; restores A

nmi_irq:
        rti                             ; restores previous PC, status

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
irq_text:
        pha                             ; saves A

        asl $d019                       ; clears raster interrupt
        bcs @is_raster

        lda $dc0d                       ; clears CIA1 timer A interrupt
        inc ZP_SYNC_MUSIC
        jmp @exit

@is_raster:
        lda #%00001000                  ; no scroll, multi-color off, 40-cols
        sta $d016

        lda #%11101100                  ; screen addr 0x3800, charset at $3000
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


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
;void animate_scroll
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc animate_scroll
        rts
.endproc

charset_colors:
        .incbin "easter_charset_2x2-colors.bin"

scroll_txt:
        scrcode "THIS IS OUR LITTLE HOMAGE TO THE CLASSICS OF BRAZILIAN POPULAR MUSIC, "
        scrcode "WE'RE SORRY WE COULD ONLY PICK A FEW OUT OF SO MANY GREAT SONGS FROM BRAZIL'S RICH CULTURE. "
        scrcode "THERE'S A LOT OF ROOM FOR CREATING C64 MUSIC DISKS BASED IN LATIN AMERICAN MUSIC. "
        scrcode "WE AT PVM WANT TO INVITE YOU TO THE FLASH PARTY 20 YEAR ANNIVERSARY, "
        scrcode "A DEMOSCENE EVENT WE'RE PLANNING FOR 2018 IN BUENOS AIRES. IT WILL BE A GREAT TIME TO "
        scrcode "MEET AND SHARE OUR GEEKY OBSESSIONS, SO PLEASE STAY TUNED FOR DATES AND LOCATION! "
        scrcode "THE SONG YOU'RE LISTENING TO NOW IS UCTUMI'S ATTEMPT AT A VERSION OF THE FELICIDADE SONG. "
        scrcode "SAMBA AND BOSSA NOVA ARE REALLY TOUGH TO GET RIGHT, SO THIS IS THE BEST HE COULD DO. "
        scrcode "AND THE GRAPHIC THAT YOU'RE WATCHING IS ALAKRAN'S HOMAGE TO THE GREAT BRAZILIAN ILLUSTRATOR LOBO. "
        scrcode "CREDITS: CODE BY RIQ, GFX BY ALAKRAN, CHARSET BY ARLEQUIN AND MUSIC BY UCTUMI. "
        scrcode "GREETINGS TO OUR FRIENDS AT GAROA HACKER CLUBE OF BRAZIL FROM PUNGAS DE VILLA MARTELLI, ARGENTINA, 2017. HAVE A HAPPY NEW YEAR!! "
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
