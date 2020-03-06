.include "nes.inc"
.include "global.inc"
.segment "CODE"
.proc draw_bg
  ; Start by clearing the first nametable
  ldx #$20 ; Nametable
  lda #$00 ; Value
  ldy #$00 ; Attribute
  jsr ppu_clear_nt

  ; Top row
  lda #>($2000+12+8*32)
  sta PPUADDR
  lda #<($2000+12+8*32)
  sta PPUADDR
  ldx #$80
  ldy #8
: stx PPUDATA
  inx
  dey
  bne :-
  ; Bottom row
  lda #>($2000+12+9*32)
  sta PPUADDR
  lda #<($2000+12+9*32)
  sta PPUADDR
  ldx #$90
  ldy #8
: stx PPUDATA
  inx
  dey
  bne :-

  lda #0
  sta PPUSCROLL
  sta PPUSCROLL
  rts
.endproc

