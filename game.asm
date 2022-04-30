.setcpu "65C02"

.charmap $41, $41
.charmap $61, $61

PORTB = $6000
PORTA = $6001
DDRB = $6002
DDRA = $6003

E  = %10000000
RW = %01000000
RS = %00100000


; need this for charachter constants to be interpretted correctly as ASCII
; values and not PETSCII values.
.macro ASCII
  .repeat $ff, i
    .charmap i + 1, i + 1
  .endrepeat
.endmacro

ASCII



.segment "ZEROPAGE"
strptr:
  .byte 2
ramptr:
  .byte 2


.code

PRINTCHAR:
  sta PORTB
  lda #RS         ; Set RS; Clear RW/E bits
  sta PORTA
  lda #(RS | E)   ; Set E bit to send instruction
  sta PORTA
  lda #RS         ; Clear E bits
  sta PORTA
  rts

LCD_CMD:
  sta PORTB
  lda #0         ; Clear RS/RW/E bits
  sta PORTA
  lda #E         ; Set E bit to send instruction
  sta PORTA
  lda #0         ; Clear RS/RW/E bits
  sta PORTA

  nop
  nop
  nop
  nop
  nop
  nop
  nop
  rts

PRINTSTR:
  ldY #$00
  @Loop:
    lda (strptr), y
    BEQ @Over
    jsr PRINTCHAR
    inY
    jmp @Loop
  @Over:
    rts


halt:
  jmp halt


RAMtest:
  ldx #$00
  stx ramptr
  ldx #$02
  stx ramptr+1

  ldY #$00
  lda #$00
  @Loop:
    sta (ramptr), Y
    ina
    iny
    bne @Loop
    inx
    stx ramptr+1
    CPX #$60
    BNE @Loop



  ldx #$00
  stx ramptr
  ldx #$02
  stx ramptr+1

  ldY #$00
  lda #$00
  @Loop2:
    cmp (ramptr), Y
    bne halt
    ina
    iny
    bne @Loop2
    inx
    stx ramptr+1
    CPX #$60
    BNE @Loop2

waitcycles:
  nop
  nop
  nop
  nop
  nop
  nop
  nop
  dex
  bne waitcycles
  rts

clrscreen:
  lda #$01        ; clear screen
  jsr LCD_CMD
  lda #$80        ; set to 1,1
  jsr LCD_CMD
  rts

wait4_buttondown:
  lda PORTA
  and #%1
  BNE wait4_buttondown
  rts

wait4_buttonup:
  lda PORTA
  and #%1
  BEQ wait4_buttonup
  rts

draw_blanks:    ; draw X number of blank boxes
  lda #$FE
  jsr PRINTCHAR ; print a blank box
  dex 
  bne draw_blanks
  rts


playgame:

  lda #15   
  sta $1000

  @approach:
    jsr clrscreen
    ldx $1000
    jsr draw_blanks  ; note X must be 1 or higher!!! A is num of spaces before block

    lda #$FF
    jsr PRINTCHAR    ; print a block at the end of the spaces

    ldx $3000         ; wait 256 cycles
    jsr waitcycles

    dec $1000 ; dec number of boxes
    bne @approach

  jsr clrscreen
  lda #$FF
  jsr PRINTCHAR ; show block on first line

  ldy #$FF
  loopcheck:
    lda PORTA ; check the button
    and #$1
    beq skipend
    dey
    bne loopcheck

  rts ; go back to game over if button not down
  skipend:
  
  lda #$02  ; beep the button
  sta PORTA
  ldx #$3F
  jsr waitcycles
  lda #$00
  sta PORTA

  inc $2000 ; add 1 to score

  dec $3000 ; go faster

  lda #01
  sta $1000 ; write 1 to 1000

  @goaway:
    jsr clrscreen
    ldx $1000
    jsr draw_blanks  ; note X must be 1 or higher!!! A is num of spaces before block

    lda #$FF
    jsr PRINTCHAR    ; print a block at the end of the spaces

    ldx $3000         ; wait 256 cycles
    jsr waitcycles

    inc $1000
    lda $1000
    cmp #16
    bne @goaway


  lda #$02  ; beep the button
  sta PORTA
    ldx #$3F  ; wait ~128*8 cycles
    jsr waitcycles
  lda #$00
  sta PORTA

  jmp playgame

reset:

  lda #%11111111 ; Set all pins on port B to output
  sta DDRB

  lda #%11100010 ; Set top 3 pins on port A to output
  sta DDRA

  LDX #$FF
  TXS     ; put FF in stack pointer
  CLD     ; turn off decimal mode

  lda #%00111000  ; Set 8-bit mode; 2-line display; 5x8 font
  jsr LCD_CMD

  lda #%00001110  ; Display on; cursor on; blink off
  ;lda #$0C  ; disp on cursor off
  jsr LCD_CMD

  lda #%00000110  ; Increment and shift cursor; don't shift display
  jsr LCD_CMD

loop:

  jsr wait4_buttonup

  jsr clrscreen

  lda #.LOBYTE(startmsg)  ; print press button
  sta strptr
  lda #.HIBYTE(startmsg)
  Sta strptr+1 
  jsr PRINTSTR


  jsr wait4_buttondown
  jsr wait4_buttonup

  lda #$02  ; turn buzzer on
  sta PORTA
    ldx #$3F  ; wait ~128*8 cycles
    jsr waitcycles
  lda #$00
  sta PORTA

  lda #$FF
  sta $3000

  lda #$00
  sta $2000 ; 2000 counts how many

  jsr playgame

  jsr clrscreen
  lda #.LOBYTE(gameover)  ; game over
  sta strptr
  lda #.HIBYTE(gameover)
  Sta strptr+1 
  jsr PRINTSTR

  ldx #$00
  jsr waitcycles    ; show game over for a while
  jsr waitcycles
  jsr waitcycles
  jsr waitcycles
  jsr waitcycles
  jsr waitcycles
  jsr waitcycles

  jmp loop

startmsg:
  .asciiz "press to start"
gameover:
  .asciiz "game over"

.segment "VECTORS"
  .word $eaea
resetVector:
  .word reset
  .word $0000