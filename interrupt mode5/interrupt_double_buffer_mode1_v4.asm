
; problem is swapping between left and right every frame means that 
; even = left starts at 0 right starts at 0
; odd = left starts at 1 right starts at 0
; need to pass in a start for left and start from right?
; not sure if its fixable?
; could change left start when it is stored?

; OS memory locations
interruptAccumulator_fc                = $fc       ; RESERVED for INTERRUPTS
irqv1_204                               = $204      ;
irqv2                               = $206      ;

logicalScreenStart                  = 20480 ; $5800 ; $3000 ; $5800 ; $3000     ;

videoULAPaletteRegister_fe21             = $fe21     ; Video ULA palette register

; System timer 1
systemVIATimer1CounterLow           = $fe44     ;
systemVIATimer1CounterHigh          = $fe45     ;
systemVIATimer1LatchLow             = $fe46     ;
systemVIATimer1LatchHigh            = $fe47     ;

systemVIAAuxiliaryControlRegister   = $fe4b     ;
systemVIAInterruptFlagRegister_fe4d      = $fe4d     ;
systemVIAInterruptEnableRegister    = $fe4e     ;

; User timer 1
userVIATimer1CounterLow_fe64             = $fe64     ;
userVIATimer1CounterHigh_fe65            = $fe65     ;
userVIATimer1LatchLow_fe66               = $fe66     ;
userVIATimer1LatchHigh_fe67              = $fe67     ;

userVIAAuxiliaryControlRegister_fe6b	= $fe6b     ;
userVIAInterruptFlagRegister_fe6d        = $fe6d     ;
userVIAInterruptEnableRegister_fe6e		= $fe6e     ;

; ***************** OS CALLS ********************
OSWRCH                          = $ffee     ;
OSWORD                          = $fff1     ;
OSBYTE                          = $fff4     ;

SCR = logicalScreenStart
SCR0 = SCR
SCR1 = SCR0+6144

SCR0_ADR = SCR0 / 8
SCR1_ADR = SCR1 / 8

; zero page memory locations
SCN								= 0

; ***************************************************************************************
; This is the number of timer ticks per frame (num PAL scanlines * 62us/line)
; This timing is only true of non-interlaced modes.
; We have to subtract 2 because the latch reload costs 2us.
; This is not documented anywhere!
FramePeriod     = (312*62)-2

; Calculate here the timer value to interrupt at the desired line
TimerValue      = 14500-2 ; 16000-2 ; 88*62 + 25

; This is the delay between interrupts (three character rows)
;ShortTimerValue = 9984-2 ; 4900 -2 ; 48*62 + 46 ; 24*62 + 46
ShortTimerValue2 = (9984*2)-2 ; 4900 -2 ; 48*62 + 46 ; 24*62 + 46

MAP_POS = 240
REGA_BUF = MAP_POS -2
REGX_BUF = REGA_BUF -1
REGY_BUF = REGX_BUF -1

ORG &1C05

.start
	
.mode1Message
    equb 22, 5                             ; MODE 5
;    !byte 23, 0, 6, 0, $ff                  ; hide display
;    !byte 23, 0, 7, 30, $ff                 ; move display down

EQUB 23, 0, 1, 32      ; Set 6845 register R1 = 32
EQUB 0, 0, 0
EQUB 0, 0, 0           ; This is the "horizontal displayed" register, which defines the number of character blocks per horizontal character row. For comparison, this value is 40 for modes 4 and 5, but our custom screen is not as wide at only 32 character blocks across

EQUB 23, 0, 6, 24      ; Set 6845 register R6 = 25
EQUB 0, 0, 0
EQUB 0, 0, 0           ; This is the "vertical displayed" register, and sets the number of displayed character rows to 31. For comparison, this value is 32 for standard modes 4 and 5, but we claw back the last row for storing code just above the end of screen memory

EQUB 23, 0, 2, 90 ; 40 ; 140 ; 160 ; 180 ; 10 ; 45 ; 20      ; Set 6845 register R2 = 45
EQUB 0, 0, 0
EQUB 0, 0, 0           ; This is the "horizontal sync position" register, which defines the position of the horizontal sync pulse on the horizontal line in terms of character widths from the left-hand side of the screen. For comparison this is 49 for modes 4 and 5, but needs to be adjusted for our custom screen's width

;EQUB 23, 0, 7, 1      ; Set 6845 register R7 = 45 (value could be 0 ?)
;EQUB 0, 0, 0
;EQUB 0, 0, 0           ; This is the "vertical sync position" register

    equb 23, 1, 0, 0, 0, 0, 0, 0, 0, 0     ; cursor off
.mode1MessageEnd

.initialize

    ldx #$ff
    txs                                     ; Reset stack

    lda #144
    ldx #254                                ; Set non-interlaced
    ldy #1
    jsr OSBYTE

					ldx #0                                  ; Switch to MODE 1
.moreMessageLoop    lda mode1Message,x
					jsr OSWRCH
					inx
					cpx #mode1MessageEnd - mode1Message
					bne moreMessageLoop
 
    sei									    ; install irq code
		
    lda #$7F
    sta userVIAInterruptEnableRegister_fe6e      ; Disable all interrupts

    lda #<mainInterruptRoutine
    sta irqv1_204
    lda #>mainInterruptRoutine
    sta irqv1_204 +1

    lda #$c0
    sta userVIAInterruptEnableRegister_fe6e      ; Enable User VIA timer 1

;    lda #$c0                                ; set User VIA T1 in free run mode (i.e. repeating)
    sta userVIAAuxiliaryControlRegister_fe6b     ; Start User VIA T1 in free run mode

    lda #<TimerValue                        ; write User VIA T1 low now (the timer will not be written until you write the high byte)
    sta userVIATimer1CounterLow_fe64
    ldx #>TimerValue                        ; get high byte ready so we can write it as quickly as possible at the right moment

					lda #2                                  ; wait for VSync without having to catch it from its IRQ
					sta systemVIAInterruptFlagRegister_fe4d      ; clear VSync flag
.vsynccheck			bit systemVIAInterruptFlagRegister_fe4d
					beq vsynccheck                         	; poll VSync flag

    stx userVIATimer1CounterHigh_fe65            ; start User VIA Timer 1 counting
    sta userVIAInterruptFlagRegister_fe6d        ; clear VSync flag

    lda #<ShortTimerValue2 					;FramePeriod    	; set timer to fire every frame (set latch)
    sta userVIATimer1LatchLow_fe66
    lda #>ShortTimerValue2 					;FramePeriod
    sta userVIATimer1LatchHigh_fe67
    cli

.mloop jmp mloop

.mainInterruptRoutine		txa                                 ; Remember X and Y registers
							pha
							tya
							pha

							lda userVIAInterruptFlagRegister_fe6d    ; What kind of interrupt?
							and #%11000000
							cmp #%11000000                      ; is it the User VIA Timer 1?
							bne handledInterrupt				; unknown interrupt - pass through

							lda #$40                            ; clear User VIA timer 1 interrupt
							sta userVIAInterruptFlagRegister_fe6d

							lda #1
							jsr debugChangePalette

		LDA #>SCR0_ADR
.SCNADR CMP	#>SCR1_ADR
		BEQ SCN0
.SCN1	
			LDA #>SCR0_ADR
			LDX #<SCR0_ADR
			JMP	CONT
.SCN0	LDA	#>SCR1_ADR
		LDX #<SCR1_ADR

.CONT	STA SCNADR +1
		LDY	#12
		STY &FE00
		STA	&FE01
		INY
		STY &FE00
		STX	&FE01

 jsr DEBUG_WRITE

 JSR MOVE

 JSR PLOTS

; LDX #255
; JSR DELAY

							lda #3
							jsr debugChangePalette

.handledInterrupt			pla                                 ; Restore X and Y registers
							tay
							pla
							tax
							lda interruptAccumulator_fc
							rti

.PLOTS

	LDY #00
	LDX #00
	JSR PLOT01
	LDY #01
	LDX #00
	JSR PLOT01
	LDY #00
	LDX #01
	JSR PLOT01
	LDY #01
	LDX #01
	JSR PLOT01

	LDY #126
	LDX #00
	JSR PLOT01
	LDY #127
	LDX #00
	JSR PLOT01
	LDY #126
	LDX #01
	JSR PLOT01
	LDY #127
	LDX #01
	JSR PLOT01

	LDY #190
	LDX #00
	JSR PLOT01
	LDY #191
	LDX #00
	JSR PLOT01
	LDY #190
	LDX #01
	JSR PLOT01
	LDY #191
	LDX #01
	JSR PLOT01

	LDY #00
	LDX #126
	JSR PLOT01
	LDY #01
	LDX #126
	JSR PLOT01
	LDY #00
	LDX #127
	JSR PLOT01
	LDY #01
	LDX #127
	JSR PLOT01

	LDY #126
	LDX #126
	JSR PLOT01
	LDY #127
	LDX #126
	JSR PLOT01
	LDY #126
	LDX #127
	JSR PLOT01
	LDY #127
	LDX #127
	JSR PLOT01

	LDY #190
	LDX #126
	JSR PLOT01
	LDY #191
	LDX #126
	JSR PLOT01
	LDY #190
	LDX #127
	JSR PLOT01
	LDY #191
	LDX #127
	JSR PLOT01


	LDY #10
	LDX #10
	JSR PLOT01

	LDY #20
	LDX #20
	JSR PLOT10

	LDY #30
	LDX #30
	JSR PLOT01

	LDY #40
	LDX #40
	JSR PLOT10

	LDY #50
	LDX #50
	JSR PLOT01

	LDY #60
	LDX #60
	JSR PLOT10

	LDY #70
	LDX #70
	JSR PLOT01

	LDY #80
	LDX #80
	JSR PLOT10

	LDY #90
	LDX #90
	JSR PLOT01

	LDY #100
	LDX #100
	JSR PLOT10

	LDY #110
	LDX #110
	JSR PLOT01

	LDY #120
	LDX #120
	JSR PLOT10
	RTS

.DEBUG_WRITE

 LDA MAP_POS
 LDX #0
 LDY #0
 JSR HEX8

 LDA MAP_POS +1
 LDX #0
 LDY #8
 JSR HEX8

 LDA SCNADR +1
 LDX #0
 LDY #16
 JSR HEX8

 LDA #>SCR0_ADR
 LDX #0
 LDY #24
 JSR HEX8
 LDA #<SCR0_ADR
 LDX #0
 LDY #32
 JSR HEX8

 LDA #>SCR1_ADR
 LDX #0
 LDY #40
 JSR HEX8
 LDA #<SCR1_ADR
 LDX #0
 LDY #48
 JSR HEX8
  
	RTS

.DELAY	DEX
		BNE DELAY
		RTS
  
.debugChangePalette
							sta videoULAPaletteRegister_fe21
							eor #$10
							sta videoULAPaletteRegister_fe21
							eor #$50
							sta videoULAPaletteRegister_fe21
							eor #$10
							sta videoULAPaletteRegister_fe21
							rts

.MOVEPNT	TYA
			;STX	PNT
			CLC 
			;ADC	PNT
			BPL	MP_EXIT
				TYA			
				EOR	#$FF
				TAY
				INY
				TXA
.MP_EXIT	TAX
			RTS 

.MOVE
{
; RTS
			LDY MAP_POS
			BNE no_wrap
				LDX MAP_POS +1
				INX 
				TXA 
				AND #1
				STA MAP_POS +1		
.no_wrap	INY 
			STY MAP_POS
			RTS
}

.MOVEMENT	;LDX 	P1X
			;LDY 	I1X
			JSR 	MOVEPNT
			;STX 	P1X 
			;STY 	I1X

			;LDX 	P1Y
			;LDY 	I1Y
			JSR 	MOVEPNT
			;STX 	P1Y 
			;STY 	I1Y

			;LDX 	P2X
			;LDY 	I2X
			JSR 	MOVEPNT
			;STX 	P2X
			;STY 	I2X

			;LDX 	P2Y
			;LDY 	I2Y
			JSR 	MOVEPNT
			;STX 	P2Y 
			;STY 	I2Y
			RTS 

.PLOT01
;		LDA   SCR_LO,Y
	TYA
	AND #7
			STA   SCN
			LDA   SCR_HI,Y 
			STA   SCN+1 
            LDY   SCR_HOR,X
            LDA   (SCN),Y
			ORA   SCR_OR_01,X
            STA   (SCN),Y 
            RTS

.PLOT10
;		LDA   SCR_LO,Y
	TYA
	AND #7
			STA   SCN
			LDA   SCR_HI,Y 
			STA   SCN+1 
            LDY   SCR_HOR,X
            LDA   (SCN),Y
			ORA   SCR_OR_10,X
            STA   (SCN),Y 
            RTS

.HEXCHAR   LDA   HEX0,Y
.HEX_NUMBER0 STA   $ABCD,X
      LDA   HEX1,Y
.HEX_NUMBER1 STA   $ABCD,X
      LDA   HEX2,Y
.HEX_NUMBER2 STA   $ABCD,X
      LDA   HEX3,Y
.HEX_NUMBER3 STA   $ABCD,X
      LDA   HEX4,Y
.HEX_NUMBER4 STA   $ABCD,X
      RTS

.HEX16 STY   REGY_BUF  ; Y = YPOS / A = HI / X = LO
      STX   REGX_BUF    
      LDX   #0
      JSR   HEX8
      LDA   REGX_BUF
      LDY   REGY_BUF
      LDX   #16
      JSR   HEX8
      RTS

.HEX8 ; A = NUM / Y = YPOS / X = XPOS

      STA   REGA_BUF
      LDA   SCR_LO+0,Y
      STA   HEX_NUMBER0+1
      
      LDA   SCR_HI+0,Y
      STA   HEX_NUMBER0+2

      LDA   SCR_LO+1,Y
      STA   HEX_NUMBER1+1
      
      LDA   SCR_HI+1,Y 
      STA   HEX_NUMBER1+2

      LDA   SCR_LO+2,Y
      STA   HEX_NUMBER2+1
      
      LDA   SCR_HI+2,Y 
      STA   HEX_NUMBER2+2

      LDA   SCR_LO+3,Y
      STA   HEX_NUMBER3+1
      
      LDA   SCR_HI+3,Y 
      STA   HEX_NUMBER3+2

      LDA   SCR_LO+4,Y
      STA   HEX_NUMBER4+1
      
      LDA   SCR_HI+4,Y 
      STA   HEX_NUMBER4+2

      LDA   REGA_BUF
      LSR	A
      LSR	A
      LSR	A
      LSR	A
      TAY
      JSR   HEXCHAR

      TXA
      CLC
      ADC   #8
      TAX

      LDA   REGA_BUF
      AND   #15
      TAY
      JSR   HEXCHAR

      RTS

.HEX0  EQUB %01110111,%00100010,%01110111,%01110111,%01010101,%01110111,%01110111,%01110111,%01110111,%01110111,%00100010,%01100110,%01110111,%01100110,%01110111,%01110111 
.HEX1  EQUB %01010101,%01100110,%00010001,%00010001,%01010101,%01000100,%01000100,%00010001,%01010101,%01010101,%01010101,%01010101,%01000100,%01010101,%01000100,%01000100 
.HEX2  EQUB %01010101,%00100010,%01110111,%01110111,%01110111,%01110111,%01110111,%00010001,%01110111,%01110111,%01110111,%01100110,%01000100,%01010101,%01110111,%01110111
.HEX3  EQUB %01010101,%00100010,%01000100,%00010001,%00010001,%00010001,%01010101,%00010001,%01010101,%00010001,%01010101,%01010101,%01000100,%01010101,%01000100,%01000100
.HEX4  EQUB %01110111,%01110111,%01110111,%01110111,%00010001,%01110111,%01110111,%00010001,%01110111,%00010001,%01010101,%01100110,%01110111,%01100110,%01110111,%01000100

ALIGN &100
.SCR_HI	
    EQUB >(SCR+(32*000)+0),>(SCR+(32*000)+1),>(SCR+(32*000)+2),>(SCR+(32*000)+3),>(SCR+(32*000)+4),>(SCR+(32*000)+5),>(SCR+(32*000)+6),>(SCR+(32*000)+7)
    EQUB >(SCR+(32*008)+0),>(SCR+(32*008)+1),>(SCR+(32*008)+2),>(SCR+(32*008)+3),>(SCR+(32*008)+4),>(SCR+(32*008)+5),>(SCR+(32*008)+6),>(SCR+(32*008)+7)
    EQUB >(SCR+(32*016)+0),>(SCR+(32*016)+1),>(SCR+(32*016)+2),>(SCR+(32*016)+3),>(SCR+(32*016)+4),>(SCR+(32*016)+5),>(SCR+(32*016)+6),>(SCR+(32*016)+7)
    EQUB >(SCR+(32*024)+0),>(SCR+(32*024)+1),>(SCR+(32*024)+2),>(SCR+(32*024)+3),>(SCR+(32*024)+4),>(SCR+(32*024)+5),>(SCR+(32*024)+6),>(SCR+(32*024)+7)
    EQUB >(SCR+(32*032)+0),>(SCR+(32*032)+1),>(SCR+(32*032)+2),>(SCR+(32*032)+3),>(SCR+(32*032)+4),>(SCR+(32*032)+5),>(SCR+(32*032)+6),>(SCR+(32*032)+7)
    EQUB >(SCR+(32*040)+0),>(SCR+(32*040)+1),>(SCR+(32*040)+2),>(SCR+(32*040)+3),>(SCR+(32*040)+4),>(SCR+(32*040)+5),>(SCR+(32*040)+6),>(SCR+(32*040)+7)
    EQUB >(SCR+(32*048)+0),>(SCR+(32*048)+1),>(SCR+(32*048)+2),>(SCR+(32*048)+3),>(SCR+(32*048)+4),>(SCR+(32*048)+5),>(SCR+(32*048)+6),>(SCR+(32*048)+7)
    EQUB >(SCR+(32*056)+0),>(SCR+(32*056)+1),>(SCR+(32*056)+2),>(SCR+(32*056)+3),>(SCR+(32*056)+4),>(SCR+(32*056)+5),>(SCR+(32*056)+6),>(SCR+(32*056)+7)
    EQUB >(SCR+(32*064)+0),>(SCR+(32*064)+1),>(SCR+(32*064)+2),>(SCR+(32*064)+3),>(SCR+(32*064)+4),>(SCR+(32*064)+5),>(SCR+(32*064)+6),>(SCR+(32*064)+7)
    EQUB >(SCR+(32*072)+0),>(SCR+(32*072)+1),>(SCR+(32*072)+2),>(SCR+(32*072)+3),>(SCR+(32*072)+4),>(SCR+(32*072)+5),>(SCR+(32*072)+6),>(SCR+(32*072)+7)
    EQUB >(SCR+(32*080)+0),>(SCR+(32*080)+1),>(SCR+(32*080)+2),>(SCR+(32*080)+3),>(SCR+(32*080)+4),>(SCR+(32*080)+5),>(SCR+(32*080)+6),>(SCR+(32*080)+7)	
    EQUB >(SCR+(32*088)+0),>(SCR+(32*088)+1),>(SCR+(32*088)+2),>(SCR+(32*088)+3),>(SCR+(32*088)+4),>(SCR+(32*088)+5),>(SCR+(32*088)+6),>(SCR+(32*088)+7)
    EQUB >(SCR+(32*096)+0),>(SCR+(32*096)+1),>(SCR+(32*096)+2),>(SCR+(32*096)+3),>(SCR+(32*096)+4),>(SCR+(32*096)+5),>(SCR+(32*096)+6),>(SCR+(32*096)+7)
    EQUB >(SCR+(32*104)+0),>(SCR+(32*104)+1),>(SCR+(32*104)+2),>(SCR+(32*104)+3),>(SCR+(32*104)+4),>(SCR+(32*104)+5),>(SCR+(32*104)+6),>(SCR+(32*104)+7)
    EQUB >(SCR+(32*112)+0),>(SCR+(32*112)+1),>(SCR+(32*112)+2),>(SCR+(32*112)+3),>(SCR+(32*112)+4),>(SCR+(32*112)+5),>(SCR+(32*112)+6),>(SCR+(32*112)+7)
    EQUB >(SCR+(32*120)+0),>(SCR+(32*120)+1),>(SCR+(32*120)+2),>(SCR+(32*120)+3),>(SCR+(32*120)+4),>(SCR+(32*120)+5),>(SCR+(32*120)+6),>(SCR+(32*120)+7)

    EQUB >(SCR+(32*128)+0),>(SCR+(32*128)+1),>(SCR+(32*128)+2),>(SCR+(32*128)+3),>(SCR+(32*128)+4),>(SCR+(32*128)+5),>(SCR+(32*128)+6),>(SCR+(32*128)+7)
    EQUB >(SCR+(32*136)+0),>(SCR+(32*136)+1),>(SCR+(32*136)+2),>(SCR+(32*136)+3),>(SCR+(32*136)+4),>(SCR+(32*136)+5),>(SCR+(32*136)+6),>(SCR+(32*136)+7)
    EQUB >(SCR+(32*144)+0),>(SCR+(32*144)+1),>(SCR+(32*144)+2),>(SCR+(32*144)+3),>(SCR+(32*144)+4),>(SCR+(32*144)+5),>(SCR+(32*144)+6),>(SCR+(32*144)+7)
    EQUB >(SCR+(32*152)+0),>(SCR+(32*152)+1),>(SCR+(32*152)+2),>(SCR+(32*152)+3),>(SCR+(32*152)+4),>(SCR+(32*152)+5),>(SCR+(32*152)+6),>(SCR+(32*152)+7)
    EQUB >(SCR+(32*160)+0),>(SCR+(32*160)+1),>(SCR+(32*160)+2),>(SCR+(32*160)+3),>(SCR+(32*160)+4),>(SCR+(32*160)+5),>(SCR+(32*160)+6),>(SCR+(32*160)+7)

    EQUB >(SCR+(32*168)+0),>(SCR+(32*168)+1),>(SCR+(32*168)+2),>(SCR+(32*168)+3),>(SCR+(32*168)+4),>(SCR+(32*168)+5),>(SCR+(32*168)+6),>(SCR+(32*168)+7)
    EQUB >(SCR+(32*176)+0),>(SCR+(32*176)+1),>(SCR+(32*176)+2),>(SCR+(32*176)+3),>(SCR+(32*176)+4),>(SCR+(32*176)+5),>(SCR+(32*176)+6),>(SCR+(32*176)+7)
    EQUB >(SCR+(32*184)+0),>(SCR+(32*184)+1),>(SCR+(32*184)+2),>(SCR+(32*184)+3),>(SCR+(32*184)+4),>(SCR+(32*184)+5),>(SCR+(32*184)+6),>(SCR+(32*184)+7)

ALIGN &100
.SCR1_HI	
    EQUB >(SCR1+(32*000)+0),>(SCR1+(32*000)+1),>(SCR1+(32*000)+2),>(SCR1+(32*000)+3),>(SCR1+(32*000)+4),>(SCR1+(32*000)+5),>(SCR1+(32*000)+6),>(SCR1+(32*000)+7)
    EQUB >(SCR1+(32*008)+0),>(SCR1+(32*008)+1),>(SCR1+(32*008)+2),>(SCR1+(32*008)+3),>(SCR1+(32*008)+4),>(SCR1+(32*008)+5),>(SCR1+(32*008)+6),>(SCR1+(32*008)+7)
    EQUB >(SCR1+(32*016)+0),>(SCR1+(32*016)+1),>(SCR1+(32*016)+2),>(SCR1+(32*016)+3),>(SCR1+(32*016)+4),>(SCR1+(32*016)+5),>(SCR1+(32*016)+6),>(SCR1+(32*016)+7)
    EQUB >(SCR1+(32*024)+0),>(SCR1+(32*024)+1),>(SCR1+(32*024)+2),>(SCR1+(32*024)+3),>(SCR1+(32*024)+4),>(SCR1+(32*024)+5),>(SCR1+(32*024)+6),>(SCR1+(32*024)+7)
    EQUB >(SCR1+(32*032)+0),>(SCR1+(32*032)+1),>(SCR1+(32*032)+2),>(SCR1+(32*032)+3),>(SCR1+(32*032)+4),>(SCR1+(32*032)+5),>(SCR1+(32*032)+6),>(SCR1+(32*032)+7)
    EQUB >(SCR1+(32*040)+0),>(SCR1+(32*040)+1),>(SCR1+(32*040)+2),>(SCR1+(32*040)+3),>(SCR1+(32*040)+4),>(SCR1+(32*040)+5),>(SCR1+(32*040)+6),>(SCR1+(32*040)+7)
    EQUB >(SCR1+(32*048)+0),>(SCR1+(32*048)+1),>(SCR1+(32*048)+2),>(SCR1+(32*048)+3),>(SCR1+(32*048)+4),>(SCR1+(32*048)+5),>(SCR1+(32*048)+6),>(SCR1+(32*048)+7)
    EQUB >(SCR1+(32*056)+0),>(SCR1+(32*056)+1),>(SCR1+(32*056)+2),>(SCR1+(32*056)+3),>(SCR1+(32*056)+4),>(SCR1+(32*056)+5),>(SCR1+(32*056)+6),>(SCR1+(32*056)+7)
    EQUB >(SCR1+(32*064)+0),>(SCR1+(32*064)+1),>(SCR1+(32*064)+2),>(SCR1+(32*064)+3),>(SCR1+(32*064)+4),>(SCR1+(32*064)+5),>(SCR1+(32*064)+6),>(SCR1+(32*064)+7)
    EQUB >(SCR1+(32*072)+0),>(SCR1+(32*072)+1),>(SCR1+(32*072)+2),>(SCR1+(32*072)+3),>(SCR1+(32*072)+4),>(SCR1+(32*072)+5),>(SCR1+(32*072)+6),>(SCR1+(32*072)+7)
    EQUB >(SCR1+(32*080)+0),>(SCR1+(32*080)+1),>(SCR1+(32*080)+2),>(SCR1+(32*080)+3),>(SCR1+(32*080)+4),>(SCR1+(32*080)+5),>(SCR1+(32*080)+6),>(SCR1+(32*080)+7)	
    EQUB >(SCR1+(32*088)+0),>(SCR1+(32*088)+1),>(SCR1+(32*088)+2),>(SCR1+(32*088)+3),>(SCR1+(32*088)+4),>(SCR1+(32*088)+5),>(SCR1+(32*088)+6),>(SCR1+(32*088)+7)
    EQUB >(SCR1+(32*096)+0),>(SCR1+(32*096)+1),>(SCR1+(32*096)+2),>(SCR1+(32*096)+3),>(SCR1+(32*096)+4),>(SCR1+(32*096)+5),>(SCR1+(32*096)+6),>(SCR1+(32*096)+7)
    EQUB >(SCR1+(32*104)+0),>(SCR1+(32*104)+1),>(SCR1+(32*104)+2),>(SCR1+(32*104)+3),>(SCR1+(32*104)+4),>(SCR1+(32*104)+5),>(SCR1+(32*104)+6),>(SCR1+(32*104)+7)
    EQUB >(SCR1+(32*112)+0),>(SCR1+(32*112)+1),>(SCR1+(32*112)+2),>(SCR1+(32*112)+3),>(SCR1+(32*112)+4),>(SCR1+(32*112)+5),>(SCR1+(32*112)+6),>(SCR1+(32*112)+7)
    EQUB >(SCR1+(32*120)+0),>(SCR1+(32*120)+1),>(SCR1+(32*120)+2),>(SCR1+(32*120)+3),>(SCR1+(32*120)+4),>(SCR1+(32*120)+5),>(SCR1+(32*120)+6),>(SCR1+(32*120)+7)

    EQUB >(SCR1+(32*128)+0),>(SCR1+(32*128)+1),>(SCR1+(32*128)+2),>(SCR1+(32*128)+3),>(SCR1+(32*128)+4),>(SCR1+(32*128)+5),>(SCR1+(32*128)+6),>(SCR1+(32*128)+7)
    EQUB >(SCR1+(32*136)+0),>(SCR1+(32*136)+1),>(SCR1+(32*136)+2),>(SCR1+(32*136)+3),>(SCR1+(32*136)+4),>(SCR1+(32*136)+5),>(SCR1+(32*136)+6),>(SCR1+(32*136)+7)
    EQUB >(SCR1+(32*144)+0),>(SCR1+(32*144)+1),>(SCR1+(32*144)+2),>(SCR1+(32*144)+3),>(SCR1+(32*144)+4),>(SCR1+(32*144)+5),>(SCR1+(32*144)+6),>(SCR1+(32*144)+7)
    EQUB >(SCR1+(32*152)+0),>(SCR1+(32*152)+1),>(SCR1+(32*152)+2),>(SCR1+(32*152)+3),>(SCR1+(32*152)+4),>(SCR1+(32*152)+5),>(SCR1+(32*152)+6),>(SCR1+(32*152)+7)
    EQUB >(SCR1+(32*160)+0),>(SCR1+(32*160)+1),>(SCR1+(32*160)+2),>(SCR1+(32*160)+3),>(SCR1+(32*160)+4),>(SCR1+(32*160)+5),>(SCR1+(32*160)+6),>(SCR1+(32*160)+7)

    EQUB >(SCR1+(32*168)+0),>(SCR1+(32*168)+1),>(SCR1+(32*168)+2),>(SCR1+(32*168)+3),>(SCR1+(32*168)+4),>(SCR1+(32*168)+5),>(SCR1+(32*168)+6),>(SCR1+(32*168)+7)
    EQUB >(SCR1+(32*176)+0),>(SCR1+(32*176)+1),>(SCR1+(32*176)+2),>(SCR1+(32*176)+3),>(SCR1+(32*176)+4),>(SCR1+(32*176)+5),>(SCR1+(32*176)+6),>(SCR1+(32*176)+7)
    EQUB >(SCR1+(32*184)+0),>(SCR1+(32*184)+1),>(SCR1+(32*184)+2),>(SCR1+(32*184)+3),>(SCR1+(32*184)+4),>(SCR1+(32*184)+5),>(SCR1+(32*184)+6),>(SCR1+(32*184)+7)

ALIGN &100	
.SCR_LO	
    EQUB <(SCR+(32*000)+0),<(SCR+(32*000)+1),<(SCR+(32*000)+2),<(SCR+(32*000)+3),<(SCR+(32*000)+4),<(SCR+(32*000)+5),<(SCR+(32*000)+6),<(SCR+(32*000)+7)
    EQUB <(SCR+(32*008)+0),<(SCR+(32*008)+1),<(SCR+(32*008)+2),<(SCR+(32*008)+3),<(SCR+(32*008)+4),<(SCR+(32*008)+5),<(SCR+(32*008)+6),<(SCR+(32*008)+7)
    EQUB <(SCR+(32*016)+0),<(SCR+(32*016)+1),<(SCR+(32*016)+2),<(SCR+(32*016)+3),<(SCR+(32*016)+4),<(SCR+(32*016)+5),<(SCR+(32*016)+6),<(SCR+(32*016)+7)
    EQUB <(SCR+(32*024)+0),<(SCR+(32*024)+1),<(SCR+(32*024)+2),<(SCR+(32*024)+3),<(SCR+(32*024)+4),<(SCR+(32*024)+5),<(SCR+(32*024)+6),<(SCR+(32*024)+7)
    EQUB <(SCR+(32*032)+0),<(SCR+(32*032)+1),<(SCR+(32*032)+2),<(SCR+(32*032)+3),<(SCR+(32*032)+4),<(SCR+(32*032)+5),<(SCR+(32*032)+6),<(SCR+(32*032)+7)
    EQUB <(SCR+(32*040)+0),<(SCR+(32*040)+1),<(SCR+(32*040)+2),<(SCR+(32*040)+3),<(SCR+(32*040)+4),<(SCR+(32*040)+5),<(SCR+(32*040)+6),<(SCR+(32*040)+7)
    EQUB <(SCR+(32*048)+0),<(SCR+(32*048)+1),<(SCR+(32*048)+2),<(SCR+(32*048)+3),<(SCR+(32*048)+4),<(SCR+(32*048)+5),<(SCR+(32*048)+6),<(SCR+(32*048)+7)
    EQUB <(SCR+(32*056)+0),<(SCR+(32*056)+1),<(SCR+(32*056)+2),<(SCR+(32*056)+3),<(SCR+(32*056)+4),<(SCR+(32*056)+5),<(SCR+(32*056)+6),<(SCR+(32*056)+7)
    EQUB <(SCR+(32*064)+0),<(SCR+(32*064)+1),<(SCR+(32*064)+2),<(SCR+(32*064)+3),<(SCR+(32*064)+4),<(SCR+(32*064)+5),<(SCR+(32*064)+6),<(SCR+(32*064)+7)
    EQUB <(SCR+(32*072)+0),<(SCR+(32*072)+1),<(SCR+(32*072)+2),<(SCR+(32*072)+3),<(SCR+(32*072)+4),<(SCR+(32*072)+5),<(SCR+(32*072)+6),<(SCR+(32*072)+7)
    EQUB <(SCR+(32*080)+0),<(SCR+(32*080)+1),<(SCR+(32*080)+2),<(SCR+(32*080)+3),<(SCR+(32*080)+4),<(SCR+(32*080)+5),<(SCR+(32*080)+6),<(SCR+(32*080)+7)	
    EQUB <(SCR+(32*088)+0),<(SCR+(32*088)+1),<(SCR+(32*088)+2),<(SCR+(32*088)+3),<(SCR+(32*088)+4),<(SCR+(32*088)+5),<(SCR+(32*088)+6),<(SCR+(32*088)+7)
    EQUB <(SCR+(32*096)+0),<(SCR+(32*096)+1),<(SCR+(32*096)+2),<(SCR+(32*096)+3),<(SCR+(32*096)+4),<(SCR+(32*096)+5),<(SCR+(32*096)+6),<(SCR+(32*096)+7)
    EQUB <(SCR+(32*104)+0),<(SCR+(32*104)+1),<(SCR+(32*104)+2),<(SCR+(32*104)+3),<(SCR+(32*104)+4),<(SCR+(32*104)+5),<(SCR+(32*104)+6),<(SCR+(32*104)+7)
    EQUB <(SCR+(32*112)+0),<(SCR+(32*112)+1),<(SCR+(32*112)+2),<(SCR+(32*112)+3),<(SCR+(32*112)+4),<(SCR+(32*112)+5),<(SCR+(32*112)+6),<(SCR+(32*112)+7)
    EQUB <(SCR+(32*120)+0),<(SCR+(32*120)+1),<(SCR+(32*120)+2),<(SCR+(32*120)+3),<(SCR+(32*120)+4),<(SCR+(32*120)+5),<(SCR+(32*120)+6),<(SCR+(32*120)+7)

    EQUB <(SCR+(32*128)+0),<(SCR+(32*128)+1),<(SCR+(32*128)+2),<(SCR+(32*128)+3),<(SCR+(32*128)+4),<(SCR+(32*128)+5),<(SCR+(32*128)+6),<(SCR+(32*128)+7)
    EQUB <(SCR+(32*136)+0),<(SCR+(32*136)+1),<(SCR+(32*136)+2),<(SCR+(32*136)+3),<(SCR+(32*136)+4),<(SCR+(32*136)+5),<(SCR+(32*136)+6),<(SCR+(32*136)+7)
    EQUB <(SCR+(32*144)+0),<(SCR+(32*144)+1),<(SCR+(32*144)+2),<(SCR+(32*144)+3),<(SCR+(32*144)+4),<(SCR+(32*144)+5),<(SCR+(32*144)+6),<(SCR+(32*144)+7)
    EQUB <(SCR+(32*152)+0),<(SCR+(32*152)+1),<(SCR+(32*152)+2),<(SCR+(32*152)+3),<(SCR+(32*152)+4),<(SCR+(32*152)+5),<(SCR+(32*152)+6),<(SCR+(32*152)+7)
    EQUB <(SCR+(32*160)+0),<(SCR+(32*160)+1),<(SCR+(32*160)+2),<(SCR+(32*160)+3),<(SCR+(32*160)+4),<(SCR+(32*160)+5),<(SCR+(32*160)+6),<(SCR+(32*160)+7)

    EQUB <(SCR+(32*168)+0),<(SCR+(32*168)+1),<(SCR+(32*168)+2),<(SCR+(32*168)+3),<(SCR+(32*168)+4),<(SCR+(32*168)+5),<(SCR+(32*168)+6),<(SCR+(32*168)+7)
    EQUB <(SCR+(32*176)+0),<(SCR+(32*176)+1),<(SCR+(32*176)+2),<(SCR+(32*176)+3),<(SCR+(32*176)+4),<(SCR+(32*176)+5),<(SCR+(32*176)+6),<(SCR+(32*176)+7)
    EQUB <(SCR+(32*184)+0),<(SCR+(32*184)+1),<(SCR+(32*184)+2),<(SCR+(32*184)+3),<(SCR+(32*184)+4),<(SCR+(32*184)+5),<(SCR+(32*184)+6),<(SCR+(32*184)+7)

.SCR_HOR
	EQUB 00,00,00,00,08,08,08,08,16,16,16,16,24,24,24,24,32,32,32,32,40,40,40,40,48,48,48,48,56,56,56,56
	EQUB 64,64,64,64,72,72,72,72,80,80,80,80,88,88,88,88,96,96,96,96,104,104,104,104,112,112,112,112,120,120,120,120	
	EQUB 128,128,128,128,136,136,136,136,144,144,144,144,152,152,152,152,160,160,160,160,168,168,168,168,176,176,176,176,184,184,184,184
	EQUB 192,192,192,192,200,200,200,200,208,208,208,208,216,216,216,216,224,224,224,224,232,232,232,232,240,240,240,240,248,248,248,248
 
.SCR_OR_01
    EQUB $08,$04,$02,$01,$08,$04,$02,$01,$08,$04,$02,$01,$08,$04,$02,$01
    EQUB $08,$04,$02,$01,$08,$04,$02,$01,$08,$04,$02,$01,$08,$04,$02,$01
    EQUB $08,$04,$02,$01,$08,$04,$02,$01,$08,$04,$02,$01,$08,$04,$02,$01
    EQUB $08,$04,$02,$01,$08,$04,$02,$01,$08,$04,$02,$01,$08,$04,$02,$01
    EQUB $08,$04,$02,$01,$08,$04,$02,$01,$08,$04,$02,$01,$08,$04,$02,$01
    EQUB $08,$04,$02,$01,$08,$04,$02,$01,$08,$04,$02,$01,$08,$04,$02,$01
    EQUB $08,$04,$02,$01,$08,$04,$02,$01,$08,$04,$02,$01,$08,$04,$02,$01
    EQUB $08,$04,$02,$01,$08,$04,$02,$01,$08,$04,$02,$01,$08,$04,$02,$01

.SCR_OR_10
	EQUB $80,$40,$20,$10,$80,$40,$20,$10,$80,$40,$20,$10,$80,$40,$20,$10
	EQUB $80,$40,$20,$10,$80,$40,$20,$10,$80,$40,$20,$10,$80,$40,$20,$10
	EQUB $80,$40,$20,$10,$80,$40,$20,$10,$80,$40,$20,$10,$80,$40,$20,$10
	EQUB $80,$40,$20,$10,$80,$40,$20,$10,$80,$40,$20,$10,$80,$40,$20,$10
	EQUB $80,$40,$20,$10,$80,$40,$20,$10,$80,$40,$20,$10,$80,$40,$20,$10
	EQUB $80,$40,$20,$10,$80,$40,$20,$10,$80,$40,$20,$10,$80,$40,$20,$10
	EQUB $80,$40,$20,$10,$80,$40,$20,$10,$80,$40,$20,$10,$80,$40,$20,$10
	EQUB $80,$40,$20,$10,$80,$40,$20,$10,$80,$40,$20,$10,$80,$40,$20,$10
 
.SCR_AND_01
	EQUB $F7,$FB,$FD,$FE,$F7,$FB,$FD,$FE,$F7,$FB,$FD,$FE,$F7,$FB,$FD,$FE
	EQUB $F7,$FB,$FD,$FE,$F7,$FB,$FD,$FE,$F7,$FB,$FD,$FE,$F7,$FB,$FD,$FE
	EQUB $F7,$FB,$FD,$FE,$F7,$FB,$FD,$FE,$F7,$FB,$FD,$FE,$F7,$FB,$FD,$FE
	EQUB $F7,$FB,$FD,$FE,$F7,$FB,$FD,$FE,$F7,$FB,$FD,$FE,$F7,$FB,$FD,$FE
	EQUB $F7,$FB,$FD,$FE,$F7,$FB,$FD,$FE,$F7,$FB,$FD,$FE,$F7,$FB,$FD,$FE
	EQUB $F7,$FB,$FD,$FE,$F7,$FB,$FD,$FE,$F7,$FB,$FD,$FE,$F7,$FB,$FD,$FE
	EQUB $F7,$FB,$FD,$FE,$F7,$FB,$FD,$FE,$F7,$FB,$FD,$FE,$F7,$FB,$FD,$FE
	EQUB $F7,$FB,$FD,$FE,$F7,$FB,$FD,$FE,$F7,$FB,$FD,$FE,$F7,$FB,$FD,$FE
	
.SCR_AND_10
	EQUB $7F,$BF,$DF,$EF,$7F,$BF,$DF,$EF,$7F,$BF,$DF,$EF,$7F,$BF,$DF,$EF
	EQUB $7F,$BF,$DF,$EF,$7F,$BF,$DF,$EF,$7F,$BF,$DF,$EF,$7F,$BF,$DF,$EF
	EQUB $7F,$BF,$DF,$EF,$7F,$BF,$DF,$EF,$7F,$BF,$DF,$EF,$7F,$BF,$DF,$EF
	EQUB $7F,$BF,$DF,$EF,$7F,$BF,$DF,$EF,$7F,$BF,$DF,$EF,$7F,$BF,$DF,$EF
	EQUB $7F,$BF,$DF,$EF,$7F,$BF,$DF,$EF,$7F,$BF,$DF,$EF,$7F,$BF,$DF,$EF
	EQUB $7F,$BF,$DF,$EF,$7F,$BF,$DF,$EF,$7F,$BF,$DF,$EF,$7F,$BF,$DF,$EF
	EQUB $7F,$BF,$DF,$EF,$7F,$BF,$DF,$EF,$7F,$BF,$DF,$EF,$7F,$BF,$DF,$EF
	EQUB $7F,$BF,$DF,$EF,$7F,$BF,$DF,$EF,$7F,$BF,$DF,$EF,$7F,$BF,$DF,$EF

.end

SAVE "256beeb", start, end, initialize

;screen size	192
;top line	1
;screen	160
;line	1
;map = screen/8	20
;line	1
;score	8
;bottom line	1
;total size	192

;7. Programming the 6845
; The official way via VDU 23 code 0
;VDU 23;REGISTER,VALUE;0;0;0
; Poke registers directly in BASIC
;?&FE00=REGISTER : ?&FE01=VALUE
; Poke registers directly in assembler
;LDA #REGISTER : STA &FE00
;LDA #VALUE : STA &FE01
; All register values take effect immediately

;8. Display start address R12/R13
;• 6845 works on characters: 1 character = 8 bytes
;• CRTC character addresses are (RAM addresses DIV 8)
;• Can be anywhere in the bottom 32K of the memory
;map: RAM &0000 - &7FFF → CRTC &0000 - &0FFF
;• Display start address R12= High byte, R13= Low byte
;• These values are latched. They are only read at the
;start of a new display ‘cycle’.
;• Normally one display cycle per display frame…
;→ vertical rupture (next time!)