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

DEBUG = 1

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; start
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.export start
.proc start

.ifdef DEBUG
        sei

        lda #$35                        ; BASIC & KERNAL out
        sta $01

        lda #0
        sta $d020
        sta $d021

        lda #$01
        sta ZP_VIC_VIDEO_TYPE           ; set it to PAL in DEBUG mode
.endif

        lda #0
        sta ZP_SYNC_MUSIC
        sta ZP_SYNC_ANIM

        lda #%00010101
        sta $dd00                       ; Vic bank 2: $8000-$bFFF

        lda #%00010100                  ; screen point to $0800
        sta $d018                       ; charset at $1800 (VIC)

        jsr init_screen
        jsr init_charset
        jsr init_irq
        jsr init_nmi


.ifndef DEBUG
        lda #0
        jsr $1000                       ; init sid
.endif

        cli

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
        dec ZP_SYNC_MUSIC               ; music

.ifndef DEBUG
;        inc $d020
        jsr $1003
;        dec $d020
.endif
        jmp test_anim

.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
;void init_screen()
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc init_screen
        rts
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
;void init_charset()
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc init_charset
        rts
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
;void init_irq()
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc init_irq
        rts
.endproc

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
;void init_nmi()
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.proc init_nmi
        rts
.endproc

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
        scrcode "AND THE GRAPHICS THAT YOU'RE WATCHING IS ALAKRAN'S HOMAGE TO THE GREAT BRAZILIAN ILLUSTRATOR LOBO. "
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
        .incbin "cristo.vsf.attrib"

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.segment "COLORRAM"     ; $6400
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
        .incbin "cristo.vsf.colmap"

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.segment "CHARSET"      ; $6800
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
        .incbin "easter_charset_2x2-charset.bin"
