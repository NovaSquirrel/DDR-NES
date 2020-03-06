;
; Simple sprite demo for NES
; Copyright 2011-2014 Damian Yerrick
;
; Copying and distribution of this file, with or without
; modification, are permitted in any medium without royalty provided
; the copyright notice and this notice are preserved in all source
; code copies.  This file is offered as-is, without any warranty.
;

.include "ns_nes.s"
.include "global.inc"

OAM = $0200

.segment "ZEROPAGE"
nmis:          .res 1
oam_used:      .res 1  ; starts at 0
cur_keys:      .res 2
new_keys:      .res 2
note_scroll:   .res 3 ; Big endian
note_speed:    .res 3 ; Big endian
note_scroll_temp: .res 3 ; Big endian, for pressing notes early

direction_pressed:           .res 4 ; Left, Down, Up, Right
frames_since_arrow_lined_up: .res 4 ; Frames since the notes lined up with the arrows at the top
arrow_line_up_index:         .res 4 ; NoteBuffer Index when the notes lined up
FRAMES_SINCE_LINE_UP_CUTOFF = 15
animate_pressed_direction:   .res 4 ; Frames to make the top arrows "pressed"
cur_arrow:                   .res 1 ; Current arrow to run logic for

rating_display_frames:       .res 1
rating_display_type:         .res 1
.segment "CODE"

arrow_mask:
  .byt %1000, %0100, %0010, %0001

;;
; This NMI handler is good enough for a simple "has NMI occurred?"
; vblank-detect loop.  But sometimes there are things that you always
; want to happen every frame, even if the game logic takes far longer
; than usual.  These might include music or a scroll split.  In these
; cases, you'll need to put more logic into the NMI handler.
.proc nmi_handler
  inc nmis
  rti
.endproc

; A null IRQ handler that just does RTI is useful to add breakpoints
; that survive a recompile.  Set your debugging emulator to trap on
; reads of $FFFE, and then you can BRK $00 whenever you need to add
; a breakpoint.
;
; But sometimes you'll want a non-null IRQ handler.
; On NROM, the IRQ handler is mostly used for the DMC IRQ, which was
; designed for gapless playback of sampled sounds but can also be
; (ab)used as a crude timer for a scroll split (e.g. status bar).
.proc irq_handler
  rti
.endproc

.proc main

  ; Now the PPU has stabilized, and we're still in vblank.  Copy the
  ; palette right now because if you load a palette during forced
  ; blank (not vblank), it'll be visible as a rainbow streak.
  jsr load_main_palette

  ; While in forced blank we have full access to VRAM.
  ; Load the nametable (background map).
  jsr draw_bg

  lda #%1000
  sta NoteBuffer+$10
  lda #%0100
  sta NoteBuffer+$20
  lda #%0010
  sta NoteBuffer+$30
  lda #%0001
  sta NoteBuffer+$40
  lda #%1001
  sta NoteBuffer+$50
  lda #%0110
  sta NoteBuffer+$60
  lda #%0011
  sta NoteBuffer+$70
  lda #%1100
  sta NoteBuffer+$80
  lda #%1010
  sta NoteBuffer+$90
  lda #%0101
  sta NoteBuffer+$a0
  lda #%0001
  sta NoteBuffer+$b0
  sta NoteBuffer+$b8
  sta NoteBuffer+$c0
  sta NoteBuffer+$c8


  lda #$80
  sta note_speed+1

  lda #255
  sta frames_since_arrow_lined_up+0
  sta frames_since_arrow_lined_up+1
  sta frames_since_arrow_lined_up+2
  sta frames_since_arrow_lined_up+3

  lda #VBLANK_NMI|OBJ_8X16
  sta PPUCTRL
forever:

  ; Game logic
  jsr read_pads

  ; Try pressing the four arrows
  lda #0
  sta direction_pressed+0
  sta direction_pressed+1
  sta direction_pressed+2
  sta direction_pressed+3
  lda new_keys
  and #KEY_LEFT
  beq :+
    inc direction_pressed+0
  :
  lda new_keys
  and #KEY_DOWN
  beq :+
    inc direction_pressed+1
  :
  lda new_keys
  and #KEY_UP
  beq :+
    inc direction_pressed+2
  :
  lda new_keys
  and #KEY_RIGHT
  beq :+
    inc direction_pressed+3
  :
  lda new_keys
  and #KEY_B
  beq :+
    inc direction_pressed+2
  :
  lda new_keys
  and #KEY_A
  beq :+
    inc direction_pressed+3
  :

  countdown animate_pressed_direction+0
  countdown animate_pressed_direction+1
  countdown animate_pressed_direction+2
  countdown animate_pressed_direction+3

  lda rating_display_frames
  beq :+
    dec rating_display_frames
    bne :+
    lda #0
    sta rating_display_type
  :

  ; Increase the timers but max them out at 255
  inc frames_since_arrow_lined_up+0
  bne :+
    dec frames_since_arrow_lined_up+0
  :
  inc frames_since_arrow_lined_up+1
  bne :+
    dec frames_since_arrow_lined_up+1
  :
  inc frames_since_arrow_lined_up+2
  bne :+
    dec frames_since_arrow_lined_up+2
  :
  inc frames_since_arrow_lined_up+3
  bne :+
    dec frames_since_arrow_lined_up+3
  :

  ; Move the note position upward
  lda note_scroll
  pha
  lda note_scroll+2
  add note_speed+2
  sta note_scroll+2
  lda note_scroll+1
  adc note_speed+1
  sta note_scroll+1
  lda note_scroll+0
  adc note_speed+0
  sta note_scroll+0
  pla
  cmp note_scroll
  beq PixelDidntChange
  ldy #0 ; for storing zero

  ldx note_scroll
  lda NoteBuffer,x
  lsr
  bcc :+ ; Right
    stx arrow_line_up_index+3
    sty frames_since_arrow_lined_up+3
  :
  lsr
  bcc :+ ; Up
    stx arrow_line_up_index+2
    sty frames_since_arrow_lined_up+2
  :
  lsr
  bcc :+ ; Down
    stx arrow_line_up_index+1
    sty frames_since_arrow_lined_up+1
  :
  lsr
  bcc :+ ; Left
    stx arrow_line_up_index+0
    sty frames_since_arrow_lined_up+0
  :
PixelDidntChange:

  ; Run the logic for pressing each direction
  lda #3
  sta cur_arrow
LogicForDirection:
  ldx cur_arrow
  lda direction_pressed,x
  beq @Skip
  lda #4
  sta animate_pressed_direction,x

  lda frames_since_arrow_lined_up,x
  cmp #FRAMES_SINCE_LINE_UP_CUTOFF-1
  bcc @NotEarly
    jsr note_press_look_ahead
    ldx cur_arrow
    lda frames_since_arrow_lined_up,x
    cmp #FRAMES_SINCE_LINE_UP_CUTOFF
    bcs @Skip
  @NotEarly:

  ; TODO: Handle scoring based off of frames_since_arrow_lined_up
  lda frames_since_arrow_lined_up,x
  jsr frames_to_rating
  sta rating_display_type
  lda #10
  sta rating_display_frames


  ; Remove this arrow
  lda arrow_mask,x
  ldy arrow_line_up_index,x
  tax
  eor #255
  and NoteBuffer,y
  sta NoteBuffer,y

  ; Reset the frame counter
  ldx cur_arrow
  lda #255
  sta frames_since_arrow_lined_up,x
@Skip:
  dec cur_arrow
  bpl LogicForDirection


  ; Draw the notes
  ldx #0
  stx oam_used
  jsr draw_note_buffer
  ldx oam_used
  jsr ppu_clear_oam


  ; Good; we have the full screen ready.  Wait for a vertical blank
  ; and set the scroll registers to display it.
  lda nmis
: cmp nmis
  beq :-
  
  ; Copy the display list from main RAM to the PPU
  lda #0
  sta OAMADDR
  lda #>OAM
  sta OAM_DMA

  ; Update arrows on the top
  ; Top row
  lda #>($2000+12+8*32)
  sta PPUADDR
  lda #<($2000+12+8*32)
  sta PPUADDR
  ldx #0
AnimateTopArrowTop:
  txa
  asl
  adc #$80
  sta 0
  lda animate_pressed_direction,x
  beq :+
    lda 0
    ora #8
    sta 0
  :
  lda 0
  sta PPUDATA
  add #1
  sta PPUDATA
  inx
  cpx #4
  bne AnimateTopArrowTop

  ; Bottom row
  lda #>($2000+12+9*32)
  sta PPUADDR
  lda #<($2000+12+9*32)
  sta PPUADDR
  ldx #0
AnimateTopArrowBottom:
  txa
  asl
  adc #$90
  sta 0
  lda animate_pressed_direction,x
  beq :+
    lda 0
    ora #8
    sta 0
  :
  lda 0
  sta PPUDATA
  add #1
  sta PPUDATA
  inx
  cpx #4
  bne AnimateTopArrowBottom

  ; Display note hit rating
  lda #>($2000+12+7*32)
  sta PPUADDR
  lda #<($2000+12+7*32)
  sta PPUADDR
  lda rating_display_type
  asl
  asl
  asl
  adc rating_display_type
  tax
  .repeat 9, I
    lda score_rating_names+I, x
    sta PPUDATA
  .endrep


  ; Turn the screen on
  ldx #0
  ldy #0
  lda #VBLANK_NMI|BG_0000|OBJ_1000|OBJ_8X16
  sec
  jsr ppu_screen_on
  jmp forever
.endproc

.proc frames_to_rating
  cmp #0
  beq Marvelous
  cmp #2
  bcc Perfect
  cmp #7
  bcc Great
  cmp #10
  bcc Good
Boo:
  lda #5
  rts
Marvelous:
  lda #1
  rts
Perfect:
  lda #2
  rts
Great:
  lda #3
  rts
Good:
  lda #4
  rts

.endproc

.proc note_press_look_ahead
Attempt = 0
  lda #1
  sta Attempt

  lda note_scroll+0
  sta note_scroll_temp+0
  lda note_scroll+1
  sta note_scroll_temp+1
  lda note_scroll+2
  sta note_scroll_temp+2

AttemptLoop:
  lda note_scroll_temp+2
  add note_speed+2
  sta note_scroll_temp+2
  lda note_scroll_temp+1
  adc note_speed+1
  sta note_scroll_temp+1
  lda note_scroll_temp+0
  adc note_speed+0
  sta note_scroll_temp+0

  ldx note_scroll_temp
  ldy cur_arrow
  lda arrow_mask,y
  and NoteBuffer,x
  bne HitNote
  ; Note was note hit
  inc Attempt
  lda Attempt
  cmp #FRAMES_SINCE_LINE_UP_CUTOFF
  bcc AttemptLoop

  ; Set it to 255 to note that it failed
  ldx cur_arrow
  lda #255
  sta frames_since_arrow_lined_up,x
  rts

HitNote:
  ldx cur_arrow
  lda Attempt
  sta frames_since_arrow_lined_up,x
  lda note_scroll_temp
  sta arrow_line_up_index,x
  rts
.endproc

.proc score_rating_names
  .byt "         "
  .byt "Marvelous"
  .byt " Perfect "
  .byt "  Great  "
  .byt "  Good   "
  .byt "   Boo   "
.endproc

.proc draw_note_buffer
YPos = 0
Row = 1
Color = 2
NoteAnimation = 3

  lda note_scroll
  and #%1100
  sta NoteAnimation

  ldx #0

NoteBufferLoop:
  lda NoteBuffer,x
  jeq EmptyRow
  sta Row
  txa
  sub note_scroll
  add #8*8-1
  sta YPos
  ldy oam_used

  lda #OAM_COLOR_2
  sta Color
  txa
  and #15
  bne :+
    lda #OAM_COLOR_0
    sta Color
    beq WasRed
  :
  cmp #8
  bne :+
    lda #OAM_COLOR_1
    sta Color
  :
WasRed:

  lsr Row ; Right
  bcc :+
    lda #$22|1
    ora NoteAnimation
    sta OAM_TILE+(4*0),y
    lda #$20|1
    ora NoteAnimation
    sta OAM_TILE+(4*1),y
    lda #18*8
    sta OAM_XPOS+(4*0),y
    lda #19*8
    sta OAM_XPOS+(4*1),y
    lda YPos
    sta OAM_YPOS+(4*0),y
    sta OAM_YPOS+(4*1),y
    lda #OAM_XFLIP
    ora Color
    sta OAM_ATTR+(4*0),y
    sta OAM_ATTR+(4*1),y
    tya
    add #8
    tay
  :

  lsr Row ; Up
  bcc :+
    lda #$30|1
    ora NoteAnimation
    sta OAM_TILE+(4*0),y
    lda #$32|1
    ora NoteAnimation
    sta OAM_TILE+(4*1),y
    lda #16*8
    sta OAM_XPOS+(4*0),y
    lda #17*8
    sta OAM_XPOS+(4*1),y
    lda YPos
    sta OAM_YPOS+(4*0),y
    sta OAM_YPOS+(4*1),y
    lda #0
    ora Color
    sta OAM_ATTR+(4*0),y
    sta OAM_ATTR+(4*1),y
    tya
    add #8
    tay
  :

  lsr Row ; Down
  bcc :+
    lda #$30|1
    ora NoteAnimation
    sta OAM_TILE+(4*0),y
    lda #$32|1
    ora NoteAnimation
    sta OAM_TILE+(4*1),y
    lda #14*8
    sta OAM_XPOS+(4*0),y
    lda #15*8
    sta OAM_XPOS+(4*1),y
    lda YPos
    sta OAM_YPOS+(4*0),y
    sta OAM_YPOS+(4*1),y
    lda #OAM_YFLIP
    ora Color
    sta OAM_ATTR+(4*0),y
    sta OAM_ATTR+(4*1),y
    tya
    add #8
    tay
  :

  lsr Row ; Left
  bcc :+
    lda #$20|1
    ora NoteAnimation
    sta OAM_TILE+(4*0),y
    lda #$22|1
    ora NoteAnimation
    sta OAM_TILE+(4*1),y
    lda #12*8
    sta OAM_XPOS+(4*0),y
    lda #13*8
    sta OAM_XPOS+(4*1),y
    lda YPos
    sta OAM_YPOS+(4*0),y
    sta OAM_YPOS+(4*1),y
    lda #0
    ora Color
    sta OAM_ATTR+(4*0),y
    sta OAM_ATTR+(4*1),y
    tya
    add #8
    tay
  :

  sty oam_used
EmptyRow:
  inx
  jne NoteBufferLoop

  rts
.endproc

.proc load_main_palette
  ; seek to the start of palette memory ($3F00-$3F1F)
  ldx #$3F
  stx PPUADDR
  ldx #$00
  stx PPUADDR
copypalloop:
  lda initial_palette,x
  sta PPUDATA
  inx
  cpx #32
  bcc copypalloop
  rts
.endproc

.segment "RODATA"
initial_palette:
  .byt $02,$0F,$00,$30
  .byt $02,$0F,$00,$30
  .byt $02,$0F,$00,$30
  .byt $02,$0F,$00,$30

  .byt $02,$0F,$16,$30
  .byt $02,$0F,$12,$30
  .byt $02,$0F,$28,$30
  .byt $02,$0F,$00,$30

; Include the CHR ROM data
.segment "CHR"
  .incbin "obj/nes/bggfx.chr"
  .incbin "obj/nes/spritegfx.chr"
