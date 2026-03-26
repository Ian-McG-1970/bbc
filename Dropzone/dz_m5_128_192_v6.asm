
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

logicalScreenStart                  = 26*1024 ; $5800 ; $3000 ; $5800 ; $3000     ;

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

ula                         = &fe21
system_VIA_portB            = &fe40
system_VIA_dataDirectionB   = &fe42
system_VIA_dataDirectionA   = &fe43
system_VIA_interruptFlags   = &fe4d
system_VIA_interruptEnable  = &fe4e
system_VIA_portA            = &fe4f

BLACK 	= 0
RED 	= 1
GREEN 	= 2
YELLOW 	= 3
BLUE 	= 4
MAGENTA	= 5
CYAN 	= 6
WHITE 	= 7

; ***************** OS CALLS ********************
OSWRCH                          = $ffee     ;
OSWORD                          = $fff1     ;
OSBYTE                          = $fff4     ;

SCR = logicalScreenStart
SCR_ADR = SCR /8

ORG &0

.SCN		SKIP 2
.SCN_ZP		SKIP 2
.MEM_TO		SKIP 2
.MEM_FROM	SKIP 2
.REGA_BUF	SKIP 1
.REGX_BUF	SKIP 1
.REGY_BUF	SKIP 1
.REGA_INT	SKIP 1
.REGX_INT	SKIP 1
.REGY_INT	SKIP 1
.XPOS		SKIP 2
.YPOS		SKIP 1
.MAP_POS 	SKIP 1
.MAP_PIXEL 	SKIP 1
.MAP_CHAR	SKIP 1
.MAP_LINE0_ZP	SKIP 2
.MAP_LINE1_ZP	SKIP 2
.MAP_LINE2_ZP	SKIP 2
.MAP_LINE3_ZP	SKIP 2
.NEXT_SPARE		SKIP 2

; ***************************************************************************************
; This is the number of timer ticks per frame (num PAL scanlines * 62us/line)
; This timing is only true of non-interlaced modes.
; We have to subtract 2 because the latch reload costs 2us.
; This is not documented anywhere!
FramePeriod     = (312*62)-2

; Calculate here the timer value to interrupt at the desired line
TimerValue      = 17500-2 ; 14500-2 ; 16000-2 ; 88*62 + 25

; This is the delay between interrupts (three character rows)
;ShortTimerValue = 9984-2 ; 4900 -2 ; 48*62 + 46 ; 24*62 + 46
ShortTimerValue2 = (9984*2)-2 ; 4900 -2 ; 48*62 + 46 ; 24*62 + 46

MACRO COPY_VAL src, dst
	LDA #src
	STA dst
ENDMACRO

MACRO INIT_VIA
	COPY_VAL %01111111, system_VIA_interruptEnable	; disable all interrupts
	COPY_VAL %10000010, system_VIA_interruptEnable ; enable just VBlank ; data directionA: bottom 7 bits output (key to poll); top bit input (is it pressed?)
	COPY_VAL %01111111, system_VIA_dataDirectionA	; poll keyboard via system VIA portA ; data directionA: bottom 7 bits output (key to poll); top bit input (is it pressed?)
    COPY_VAL %00001111, system_VIA_dataDirectionB	; allow write to addressable latch
    COPY_VAL %00000011, system_VIA_portB	; set bit 3 to 0
ENDMACRO

MACRO INIT_SCREEN
    lda #144
    ldx #254                                ; Set non-interlaced
    ldy #1
    jsr OSBYTE

	ldx #0
{
.LOOP  	lda mode1Message,x
		jsr OSWRCH
		inx
		cpx #mode1MessageEnd - mode1Message
		bne LOOP
}
;    lda #144
 ;   ldx #254                                ; Set non-interlaced
  ;  ldy #1
   ; jsr OSBYTE
ENDMACRO

MACRO SET_SCREEN
	LDA #>SCR_ADR
	LDX #<SCR_ADR
	LDY	#12
	STY &FE00
	STA	&FE01
	INY
	STY &FE00
	STX	&FE01
ENDMACRO

MACRO SET_COLOURS
	LDX #0
	LDY #BLACK
	JSR SetLogicalColour
	LDX #1
	LDY #GREEN
	JSR SetLogicalColour
	LDX #2
	LDY #YELLOW
	JSR SetLogicalColour
	LDX #3
	LDY #CYAN
	JSR SetLogicalColour
ENDMACRO

MACRO SET_INT
	COPY_VAL $7F, userVIAInterruptEnableRegister_fe6e      ; Disable all interrupts
	COPY_VAL <mainInterruptRoutine, irqv1_204
	COPY_VAL >mainInterruptRoutine, irqv1_204 +1

	COPY_VAL $c0, userVIAInterruptEnableRegister_fe6e		; Enable User VIA timer 1
    sta userVIAAuxiliaryControlRegister_fe6b     ; Start User VIA T1 in free run mode ;    lda #$c0                                ; set User VIA T1 in free run mode (i.e. repeating)

	COPY_VAL <TimerValue, userVIATimer1CounterLow_fe64	; write User VIA T1 low now (the timer will not be written until you write the high byte)
    ldx #>TimerValue                        ; get high byte ready so we can write it as quickly as possible at the right moment

					COPY_VAL 2, systemVIAInterruptFlagRegister_fe4d ; wait for VSync without having to catch it from its IRQ ; clear VSync flag
.vsynccheck			bit systemVIAInterruptFlagRegister_fe4d
					beq vsynccheck                         	; poll VSync flag

    stx userVIATimer1CounterHigh_fe65            ; start User VIA Timer 1 counting
    sta userVIAInterruptFlagRegister_fe6d        ; clear VSync flag

	COPY_VAL <ShortTimerValue2, userVIATimer1LatchLow_fe66	;FramePeriod    	; set timer to fire every frame (set latch)
	COPY_VAL >ShortTimerValue2, userVIATimer1LatchHigh_fe67		;FramePeriod
ENDMACRO

ORG &10F0 ; 1C05

.start

	CLD
	CLV
    LDX #$FF
    TXS                                     ; Reset stack

	SEI									    ; install irq code
	
	JSR SETUP
	
	LDA #24
	STA XPOS
	STA YPOS
	LDA #0
	STA XPOS+1

	JSR SCREEN_SETUP
		
;	JSR GENERATE_LANDSCAPE

	CLI

.MLOOP JMP MLOOP

; A=LO
; y=hi

.CALC_LANDSCAPE_CHAR_PIXEL ; convert 0-1023 to 0-3 pixel pos + 0-255 map pos
	TXA
	AND #3
	STA MAP_PIXEL
	LDA MAPTABHI,Y ; (high ??????xx multiplied by 64 to xx??????)
	ORA MAPTABLO,X ; (low xxxxxx?? divided by 4 to ??xxxxxx) 
	RTS

MACRO DRAW_LANDSCAPE_CHAR_DZ COLUMN
	LDA	(MAP_LINE0_ZP),Y
	STA SC_LN188 +COLUMN
;	LDA	(MAP_LINE1_ZP),Y
;	STA SC_LN189 +COLUMN
;	LDA	(MAP_LINE2_ZP),Y
;	STA SC_LN190 +COLUMN
;	LDA	(MAP_LINE3_ZP),Y
;	STA SC_LN191 +COLUMN
ENDMACRO

.DRAW_LANDSCAPE_DZ
	LDA MAP_DZ_MSB,X ; should end up being x?
	TAX
	STX MAP_LINE0_ZP +1
	INX
	STX MAP_LINE1_ZP +1
	INX
	STA MAP_LINE2_ZP +1
	INX
	STA MAP_LINE3_ZP +1

	DRAW_LANDSCAPE_CHAR_DZ 1*8
	INY
	DRAW_LANDSCAPE_CHAR_DZ 2*8
	INY
	DRAW_LANDSCAPE_CHAR_DZ 3*8
	INY

	DRAW_LANDSCAPE_CHAR_DZ 4*8
	INY
	DRAW_LANDSCAPE_CHAR_DZ 5*8
	INY
	DRAW_LANDSCAPE_CHAR_DZ 6*8
	INY

	DRAW_LANDSCAPE_CHAR_DZ 7*8
	INY
	DRAW_LANDSCAPE_CHAR_DZ 8*8
	INY
	DRAW_LANDSCAPE_CHAR_DZ 9*8
	INY

	DRAW_LANDSCAPE_CHAR_DZ 10*8
	INY
	DRAW_LANDSCAPE_CHAR_DZ 11*8
	INY
	DRAW_LANDSCAPE_CHAR_DZ 12*8
	INY

	DRAW_LANDSCAPE_CHAR_DZ 13*8
	INY
	DRAW_LANDSCAPE_CHAR_DZ 14*8
	INY
	DRAW_LANDSCAPE_CHAR_DZ 15*8
	INY

	DRAW_LANDSCAPE_CHAR_DZ 16*8
	INY
	DRAW_LANDSCAPE_CHAR_DZ 17*8
	INY
	DRAW_LANDSCAPE_CHAR_DZ 18*8
	INY

	DRAW_LANDSCAPE_CHAR_DZ 19*8
	INY
	DRAW_LANDSCAPE_CHAR_DZ 20*8
	INY
	DRAW_LANDSCAPE_CHAR_DZ 21*8
	INY

	DRAW_LANDSCAPE_CHAR_DZ 22*8
	INY
	DRAW_LANDSCAPE_CHAR_DZ 23*8
	INY
	DRAW_LANDSCAPE_CHAR_DZ 24*8
	INY

	DRAW_LANDSCAPE_CHAR_DZ 25*8
	INY
	DRAW_LANDSCAPE_CHAR_DZ 26*8
	INY
	DRAW_LANDSCAPE_CHAR_DZ 27*8
	INY

	DRAW_LANDSCAPE_CHAR_DZ 28*8
	INY
	DRAW_LANDSCAPE_CHAR_DZ 29*8
	INY
	DRAW_LANDSCAPE_CHAR_DZ 30*8

	RTS

.PLOT01
{	
.LEFT	LDA SCR_HI,Y 
		STA	SCN_ZP+1
		TYA
		AND #7
		ORA	SCR_HOR,X
		TAY
		LDA	(SCN_ZP),Y
		ORA	SCR_OR_01,X
		STA	(SCN_ZP),Y 
		RTS
}

.PLOT10
{	
.LEFT	LDA SCR_HI,Y 
		STA	SCN_ZP+1
		TYA
		AND #7
		ORA	SCR_HOR,X
		TAY
		LDA	(SCN_ZP),Y
		ORA	SCR_OR_10,X
		STA	(SCN_ZP),Y 
		RTS
}

.PLOT11
{	
.LEFT	LDA SCR_HI,Y 
		STA	SCN_ZP+1
		TYA
		AND #7
		ORA	SCR_HOR,X
		TAY
		LDA	(SCN_ZP),Y
		ORA	SCR_OR_11,X
		STA	(SCN_ZP),Y 
		RTS
}

.CLR00
{	
.LEFT	LDA SCR_HI,Y 
		STA	SCN_ZP+1
		TYA
		AND #7
		ORA	SCR_HOR,X
		TAY
		LDA	(SCN_ZP),Y
		AND	SCR_AND_00,X
		STA	(SCN_ZP),Y 
		RTS
}

.SetLogicalColour	; x = The logical colour to set / y = The physical colour to map to the logical colour

					LDA #19		; Start a VDU 19 command, which sets a logical colour to a physical colour using the following format: VDU 19, logical, physical, 0, 0, 0
					JSR OSWRCH

					TXA			; Write the value in X, which is the logical colour
					JSR OSWRCH

					TYA			; Copy the physical colour from Y to A
					LDX #3		; Set a counter in X to write the next four values, so the following loop writes: physical, 0, 0, 0
{
.loop					JSR OSWRCH	; Write the value in A
						LDA #0      ; Set A to 0 to write the three zeroes
						DEX         ; Decrement the loop counter
						BPL loop    ; Loop back until we have written the whole VDU command
}
					RTS

.POLL_KEY	STA system_VIA_portA
			LDA system_VIA_portA
			RTS

.READKEYS
;		LDA	#(-(-1)-1)
;		JSR	POLL_KEY
;		STX keyShift

		LDX #0
		LDY #0
{
;		LDA	#(-(-74)-1)
;		JSR	POLL_KEY
;		BPL cont
;			INC OBJECT
;.cont
}
{
		LDA	#(-(-58)-1)
		JSR	POLL_KEY
		BPL cont
			DEY
;			DEC V_PLAYER
.cont
}
{
		LDA	#(-(-42)-1)
		JSR	POLL_KEY
		BPL cont
			INY
;			INC V_PLAYER
.cont
}
{
		LDA	#(-(-26)-1)
		JSR	POLL_KEY
		BPL cont
			DEX
;			DEC H_PLAYER
.cont
}
{
		LDA	#(-(-122)-1)
		JSR	POLL_KEY
		BPL cont
			INX
;			INC H_PLAYER
.cont
}
		RTS
;    PollKey -1,   keyShift
;    PollKey -74,  keyEnter
;    PollKey -58,  keyUp
;    PollKey -42,  keyDown
;    PollKey -26,  keyLeft
;    PollKey -122, keyRight

.mainInterruptRoutine		STA REGA_INT

							lda userVIAInterruptFlagRegister_fe6d    ; What kind of interrupt?
							and #%11000000
							cmp #%11000000                      ; is it the User VIA Timer 1?
							bne handledInterrupt				; unknown interrupt - pass through

							STX REGX_INT							; Remember X and Y registers
							STY REGY_INT

							COPY_VAL $40, userVIAInterruptFlagRegister_fe6d		; clear User VIA timer 1 interrupt

	LDX #0
	LDY #BLUE
	JSR SetLogicalColour

	INC MAP_POS
 jsr DEBUG_WRITE
 				 				
 JSR READKEYS
 JSR MOVE_PLR
 JSR CALC_LANDSCAPE_CHAR_PIXEL
 STA MAP_CHAR
 TAY
 LDX MAP_PIXEL
 JSR DRAW_LANDSCAPE_DZ

 LDA MAP_PIXEL
 LDX #0
 LDY #56
 JSR HEX8
 LDA MAP_CHAR
 LDX #0
 LDY #64
 JSR HEX8
 
	LDX #0
	LDY #BLACK
	JSR SetLogicalColour

							LDY REGY_INT					; Restore X and Y registers
							LDX REGX_INT
.handledInterrupt			LDA REGA_INT
;							lda interruptAccumulator_fc
							rti

.MOVE_PLR 
		  TYA
          CLC
          ADC   YPOS
          STA   YPOS  ; vertical

          TXA
          BEQ   EXIT2
          BMI   RIGHT

.LEFT     STA   LFT +1
;          STA TEST1
          LDA   XPOS
          CLC
.LFT      ADC   #0 
          STA   XPOS
          TAX
          LDA   XPOS +1
          ADC   #0
          AND   #3
          STA   XPOS +1
          TAY
          RTS

.RIGHT    EOR   #$FF
          CLC
          ADC   #1    ; carry is clear
          STA   RGT +1
;          STA TEST2
          SEC
          LDA   XPOS
.RGT      SBC   #0 
          STA   XPOS
          TAX
          LDA   XPOS +1
          SBC   #0
          AND   #3
          STA   XPOS +1
          TAY
          RTS

.EXIT2     LDX XPOS
          LDY XPOS +1
          RTS

.DEBUG_WRITE

 LDA MAP_POS
 LDX #0
 LDY #0
 JSR HEX8

 LDA YPOS
 LDX #0
 LDY #6
 JSR HEX8

 LDA XPOS
 LDX #0
 LDY #12
 JSR HEX8
 LDA XPOS +1
 LDX #0
 LDY #18
 JSR HEX8
 
; LDA REGX_BUF
; LDX #0
; LDY #18
; JSR HEX8
; LDA REGY_BUF
; LDX #0
; LDY #24
; JSR HEX8
; LDA REGA_BUF
; LDX #0
; LDY #30
; JSR HEX8

	RTS

;LINES0TO3_PIXEL0 = 512
;LINES0TO3_PIXEL1 = 512+1024
;LINES0TO3_PIXEL2 = 512+1024+1024
;LINES0TO3_PIXEL3 = 512+1024+1024+1024
LANDSCAPE_TEMP_LINES0TO3_PIXEL0 = $FA00

.COPY_LANDSCAPE_LINE
{
	LDY #0
.LP		LDA (MEM_FROM),Y
		STA (MEM_TO),Y
		INY
		BNE LP
	RTS
}
.COPY_LANDSCAPE_LINES
	STX MEM_FROM +1
	STY MEM_TO +1
	JSR COPY_LANDSCAPE_LINE ; line0
	INC MEM_FROM +1
	INC MEM_TO +1
	JSR COPY_LANDSCAPE_LINE ; line1
	INC MEM_FROM +1
	INC MEM_TO +1
	JSR COPY_LANDSCAPE_LINE ; line2
	INC MEM_FROM +1
	INC MEM_TO +1
	JSR COPY_LANDSCAPE_LINE ; line3
	RTS

.ROTATE_LANDSCAPE_LINE
{
	LDY #0
	LDA (MEM_TO),Y
	PHA
;	STA MAP_PIXEL ; TEMP
	ROR A
	INY
.LP		LDA (MEM_TO),Y
		ROR A
		STA (MEM_TO),Y
		INY
		BNE LP
	PLA
;	LDA MAP_PIXEL ; TEMP
	ROR A
	STA (MEM_TO),Y
	RTS
}

.ROTATE_LANDSCAPE_LINES
;	INY					; dont rotate first line
	STY MEM_TO +1
	JSR ROTATE_LANDSCAPE_LINE ; line0
	JSR ROTATE_LANDSCAPE_LINE
	INC MEM_TO +1
	JSR ROTATE_LANDSCAPE_LINE ; line1
	JSR ROTATE_LANDSCAPE_LINE
	INC MEM_TO +1
	JSR ROTATE_LANDSCAPE_LINE ; line2
	JSR ROTATE_LANDSCAPE_LINE
	INC MEM_TO +1
	JSR ROTATE_LANDSCAPE_LINE ; line3
	JSR ROTATE_LANDSCAPE_LINE
	RTS

.COLOUR_CONVERT_CHAR
{
	STA SCN
	LDA #0
	
	LSR SCN
	BCC	C7
		ORA #%10000000
.C7

	LSR SCN
	BCC	C6
		ORA #%00001000
.C6

	LSR SCN
	BCC	C5
		ORA #%01000000
.C5

	LSR SCN
	BCC	C4
		ORA #%00000100
.C4

	LSR SCN
	BCC	C3
		ORA #%00100000
.C3

	LSR SCN
	BCC	C2
		ORA #%00000010
.C2

	LSR SCN
	BCC	C1
		ORA #%00010000
.C1

	LSR SCN
	BCC	C0
		ORA #%00000001
.C0
	RTS
}

.COLOUR_CONVERT_LINE
{
	LDY #0
.LP		LDA (MEM_TO),Y
		JSR COLOUR_CONVERT_CHAR
		STA (MEM_TO),Y
		INY
		BNE LP
	RTS
}

.COLOUR_CONVERT_LINES
	STY MEM_TO +1
	JSR COLOUR_CONVERT_LINE ; line0
	INC MEM_TO +1
	JSR COLOUR_CONVERT_LINE ; line1
	INC MEM_TO +1
	JSR COLOUR_CONVERT_LINE ; line2
	INC MEM_TO +1
	JSR COLOUR_CONVERT_LINE ; line3
	RTS

.GENERATE_LANDSCAPE
	LDY #0
	STY MEM_FROM
	STY MEM_TO
	STY MAP_LINE0_ZP
	STY MAP_LINE1_ZP
	STY MAP_LINE2_ZP
	STY MAP_LINE3_ZP
	
	LDX #>start ; START ;LANDSCAPE_TEMP_LINES0TO3_PIXEL0
	LDY #>LINES0TO3_PIXEL0
	JSR COPY_LANDSCAPE_LINES
	LDY #>LINES0TO3_PIXEL0
	JSR ROTATE_LANDSCAPE_LINES

;	LDX #>LINES0TO3_PIXEL0
;	LDY #>LINES0TO3_PIXEL1
;	JSR COPY_LANDSCAPE_LINES
;	LDY #>LINES0TO3_PIXEL1
;	JSR ROTATE_LANDSCAPE_LINES

;	LDX #>LINES0TO3_PIXEL1
;	LDY #>LINES0TO3_PIXEL2
;	JSR COPY_LANDSCAPE_LINES
;	LDY #>LINES0TO3_PIXEL2
;	JSR ROTATE_LANDSCAPE_LINES

;	LDX #>LINES0TO3_PIXEL2
;	LDY #>LINES0TO3_PIXEL3
;	JSR COPY_LANDSCAPE_LINES
;	LDY #>LINES0TO3_PIXEL3
;	JSR ROTATE_LANDSCAPE_LINES

	LDY #>LINES0TO3_PIXEL0
;	JSR COLOUR_CONVERT_LINES

;	LDY #>LINES0TO3_PIXEL1
;	JSR COLOUR_CONVERT_LINES
;	LDY #>LINES0TO3_PIXEL2
;	JSR COLOUR_CONVERT_LINES
;	LDY #>LINES0TO3_PIXEL3
;	JSR COLOUR_CONVERT_LINES
	RTS

.HEX16 STY   REGY_BUF  ; Y = YPOS / A = HI / X = LO
      STX   REGX_BUF    
      LDX   #0
      JSR   HEX8
      LDA   REGX_BUF
      LDY   REGY_BUF
      LDX   #16
;      JSR   HEX8
;      RTS

.HEX8 ; A = NUM / Y = YPOS / X = XPOS

      STA   REGA_BUF

      LDA   SCR_HI+0,Y
      STA   HEX_NUMBER0+2
      LDA   SCR_HI+1,Y 
      STA   HEX_NUMBER1+2
      LDA   SCR_HI+2,Y 
      STA   HEX_NUMBER2+2
      LDA   SCR_HI+3,Y 
      STA   HEX_NUMBER3+2
      LDA   SCR_HI+4,Y 
      STA   HEX_NUMBER4+2

	  TYA
	  AND #7
;      LDA   SCR_LO+0,Y
      STA   HEX_NUMBER0+1

	INY
	  TYA
	  AND #7
;      LDA   SCR_LO+1,Y
      STA   HEX_NUMBER1+1

	INY
	  TYA
	  AND #7
;      LDA   SCR_LO+2,Y
      STA   HEX_NUMBER2+1

	INY
	  TYA
	  AND #7
;      LDA   SCR_LO+3,Y
      STA   HEX_NUMBER3+1

	INY
	  TYA
	  AND #7
;      LDA   SCR_LO+4,Y
      STA   HEX_NUMBER4+1
      

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
;      JSR   HEXCHAR
;      RTS

.HEXCHAR   		LDA	HEX0,Y
.HEX_NUMBER0 	STA	$ABCD,X
				LDA	HEX1,Y
.HEX_NUMBER1 	STA	$ABCD,X
				LDA	HEX2,Y
.HEX_NUMBER2 	STA	$ABCD,X
				LDA	HEX3,Y
.HEX_NUMBER3 	STA	$ABCD,X
				LDA	HEX4,Y
.HEX_NUMBER4 	STA	$ABCD,X
      RTS
	
.HEX0  EQUB %01110111,%00100010,%01110111,%01110111,%01010101,%01110111,%01110111,%01110111,%01110111,%01110111,%00100010,%01100110,%01110111,%01100110,%01110111,%01110111 
.HEX1  EQUB %01010101,%01100110,%00010001,%00010001,%01010101,%01000100,%01000100,%00010001,%01010101,%01010101,%01010101,%01010101,%01000100,%01010101,%01000100,%01000100 
.HEX2  EQUB %01010101,%00100010,%01110111,%01110111,%01110111,%01110111,%01110111,%00010001,%01110111,%01110111,%01110111,%01100110,%01000100,%01010101,%01110111,%01110111
.HEX3  EQUB %01010101,%00100010,%01000100,%00010001,%00010001,%00010001,%01010101,%00010001,%01010101,%00010001,%01010101,%01010101,%01000100,%01010101,%01000100,%01000100
.HEX4  EQUB %01110111,%01110111,%01110111,%01110111,%00010001,%01110111,%01110111,%00010001,%01110111,%00010001,%01010101,%01100110,%01110111,%01100110,%01110111,%01000100

.SETUP
	LDX #0
	STX SCN_ZP

	INIT_VIA
	INIT_SCREEN
	SET_SCREEN
	SET_COLOURS
	SET_INT
	RTS

.SCR_HI
    EQUB >(SC_LN000+0), >(SC_LN001+0), >(SC_LN002+0), >(SC_LN003+0), >(SC_LN004+0), >(SC_LN005+0), >(SC_LN006+0), >(SC_LN007+0), >(SC_LN008+0), >(SC_LN009+0)
    EQUB >(SC_LN010+0), >(SC_LN011+0), >(SC_LN012+0), >(SC_LN013+0), >(SC_LN014+0), >(SC_LN015+0), >(SC_LN016+0), >(SC_LN017+0), >(SC_LN018+0), >(SC_LN019+0)
    EQUB >(SC_LN020+0), >(SC_LN021+0), >(SC_LN022+0), >(SC_LN023+0), >(SC_LN024+0), >(SC_LN025+0), >(SC_LN026+0), >(SC_LN027+0), >(SC_LN028+0), >(SC_LN029+0)
    EQUB >(SC_LN030+0), >(SC_LN031+0), >(SC_LN032+0), >(SC_LN033+0), >(SC_LN034+0), >(SC_LN035+0), >(SC_LN036+0), >(SC_LN037+0), >(SC_LN038+0), >(SC_LN039+0)
    EQUB >(SC_LN040+0), >(SC_LN041+0), >(SC_LN042+0), >(SC_LN043+0), >(SC_LN044+0), >(SC_LN045+0), >(SC_LN046+0), >(SC_LN047+0), >(SC_LN048+0), >(SC_LN049+0)
    EQUB >(SC_LN050+0), >(SC_LN051+0), >(SC_LN052+0), >(SC_LN053+0), >(SC_LN054+0), >(SC_LN055+0), >(SC_LN056+0), >(SC_LN057+0), >(SC_LN058+0), >(SC_LN059+0)
    EQUB >(SC_LN060+0), >(SC_LN061+0), >(SC_LN062+0), >(SC_LN063+0), >(SC_LN064+0), >(SC_LN065+0), >(SC_LN066+0), >(SC_LN067+0), >(SC_LN068+0), >(SC_LN069+0)
    EQUB >(SC_LN070+0), >(SC_LN071+0), >(SC_LN072+0), >(SC_LN073+0), >(SC_LN074+0), >(SC_LN075+0), >(SC_LN076+0), >(SC_LN077+0), >(SC_LN078+0), >(SC_LN079+0)
    EQUB >(SC_LN080+0), >(SC_LN081+0), >(SC_LN082+0), >(SC_LN083+0), >(SC_LN084+0), >(SC_LN085+0), >(SC_LN086+0), >(SC_LN087+0), >(SC_LN088+0), >(SC_LN089+0)
    EQUB >(SC_LN090+0), >(SC_LN091+0), >(SC_LN092+0), >(SC_LN093+0), >(SC_LN094+0), >(SC_LN095+0), >(SC_LN096+0), >(SC_LN097+0), >(SC_LN098+0), >(SC_LN099+0)
    EQUB >(SC_LN100+0), >(SC_LN101+0), >(SC_LN102+0), >(SC_LN103+0), >(SC_LN104+0), >(SC_LN105+0), >(SC_LN106+0), >(SC_LN107+0), >(SC_LN108+0), >(SC_LN109+0)
    EQUB >(SC_LN110+0), >(SC_LN111+0), >(SC_LN112+0), >(SC_LN113+0), >(SC_LN114+0), >(SC_LN115+0), >(SC_LN116+0), >(SC_LN117+0), >(SC_LN118+0), >(SC_LN119+0)
    EQUB >(SC_LN120+0), >(SC_LN121+0), >(SC_LN122+0), >(SC_LN123+0), >(SC_LN124+0), >(SC_LN125+0), >(SC_LN126+0), >(SC_LN127+0), >(SC_LN128+0), >(SC_LN129+0)
    EQUB >(SC_LN130+0), >(SC_LN131+0), >(SC_LN132+0), >(SC_LN133+0), >(SC_LN134+0), >(SC_LN135+0), >(SC_LN136+0), >(SC_LN137+0), >(SC_LN138+0), >(SC_LN139+0)
    EQUB >(SC_LN140+0), >(SC_LN141+0), >(SC_LN142+0), >(SC_LN143+0), >(SC_LN144+0), >(SC_LN145+0), >(SC_LN146+0), >(SC_LN147+0), >(SC_LN148+0), >(SC_LN149+0)
    EQUB >(SC_LN150+0), >(SC_LN151+0), >(SC_LN152+0), >(SC_LN153+0), >(SC_LN154+0), >(SC_LN155+0), >(SC_LN156+0), >(SC_LN157+0), >(SC_LN158+0), >(SC_LN159+0)
    EQUB >(SC_LN160+0), >(SC_LN161+0), >(SC_LN162+0), >(SC_LN163+0), >(SC_LN164+0), >(SC_LN165+0), >(SC_LN166+0), >(SC_LN167+0), >(SC_LN168+0), >(SC_LN169+0)
    EQUB >(SC_LN170+0), >(SC_LN171+0), >(SC_LN172+0), >(SC_LN173+0), >(SC_LN174+0), >(SC_LN175+0), >(SC_LN176+0), >(SC_LN177+0), >(SC_LN178+0), >(SC_LN179+0)
    EQUB >(SC_LN180+0), >(SC_LN181+0), >(SC_LN182+0), >(SC_LN183+0), >(SC_LN184+0), >(SC_LN185+0), >(SC_LN186+0), >(SC_LN187+0), >(SC_LN188+0), >(SC_LN189+0)
    EQUB >(SC_LN190+0), >(SC_LN191+0), >(SC_LN192+0), >(SC_LN193+0), >(SC_LN194+0), >(SC_LN195+0), >(SC_LN196+0), >(SC_LN197+0), >(SC_LN198+0), >(SC_LN199+0)
    EQUB >(SC_LN200+0), >(SC_LN201+0), >(SC_LN202+0), >(SC_LN203+0), >(SC_LN204+0), >(SC_LN205+0), >(SC_LN206+0), >(SC_LN207+0), >(SC_LN208+0), >(SC_LN209+0)
    EQUB >(SC_LN210+0), >(SC_LN211+0), >(SC_LN212+0), >(SC_LN213+0), >(SC_LN214+0), >(SC_LN215+0), >(SC_LN216+0), >(SC_LN217+0), >(SC_LN218+0), >(SC_LN219+0)
    EQUB >(SC_LN220+0), >(SC_LN221+0), >(SC_LN222+0), >(SC_LN223+0), >(SC_LN224+0), >(SC_LN225+0), >(SC_LN226+0), >(SC_LN227+0), >(SC_LN228+0), >(SC_LN229+0)
    EQUB >(SC_LN230+0), >(SC_LN231+0), >(SC_LN232+0), >(SC_LN233+0), >(SC_LN234+0), >(SC_LN235+0), >(SC_LN236+0), >(SC_LN237+0), >(SC_LN238+0), >(SC_LN239+0)
    EQUB >(SC_LN240+0), >(SC_LN241+0), >(SC_LN242+0), >(SC_LN243+0), >(SC_LN244+0), >(SC_LN245+0), >(SC_LN246+0), >(SC_LN247+0), >(SC_LN248+0), >(SC_LN249+0)
    EQUB >(SC_LN250+0), >(SC_LN251+0), >(SC_LN252+0), >(SC_LN253+0), >(SC_LN254+0), >(SC_LN255+0)

.SCR_LO	
    EQUB <(SC_LN000), <(SC_LN001), <(SC_LN002), <(SC_LN003), <(SC_LN004), <(SC_LN005), <(SC_LN006), <(SC_LN007), <(SC_LN008), <(SC_LN009)
    EQUB <(SC_LN010), <(SC_LN011), <(SC_LN012), <(SC_LN013), <(SC_LN014), <(SC_LN015), <(SC_LN016), <(SC_LN017), <(SC_LN018), <(SC_LN019)
    EQUB <(SC_LN020), <(SC_LN021), <(SC_LN022), <(SC_LN023), <(SC_LN024), <(SC_LN025), <(SC_LN026), <(SC_LN027), <(SC_LN028), <(SC_LN029)
    EQUB <(SC_LN030), <(SC_LN031), <(SC_LN032), <(SC_LN033), <(SC_LN034), <(SC_LN035), <(SC_LN036), <(SC_LN037), <(SC_LN038), <(SC_LN039)
    EQUB <(SC_LN040), <(SC_LN041), <(SC_LN042), <(SC_LN043), <(SC_LN044), <(SC_LN045), <(SC_LN046), <(SC_LN047), <(SC_LN048), <(SC_LN049)
    EQUB <(SC_LN050), <(SC_LN051), <(SC_LN052), <(SC_LN053), <(SC_LN054), <(SC_LN055), <(SC_LN056), <(SC_LN057), <(SC_LN058), <(SC_LN059)
    EQUB <(SC_LN060), <(SC_LN061), <(SC_LN062), <(SC_LN063), <(SC_LN064), <(SC_LN065), <(SC_LN066), <(SC_LN067), <(SC_LN068), <(SC_LN069)
    EQUB <(SC_LN070), <(SC_LN071), <(SC_LN072), <(SC_LN073), <(SC_LN074), <(SC_LN075), <(SC_LN076), <(SC_LN077), <(SC_LN078), <(SC_LN079)
    EQUB <(SC_LN080), <(SC_LN081), <(SC_LN082), <(SC_LN083), <(SC_LN084), <(SC_LN085), <(SC_LN086), <(SC_LN087), <(SC_LN088), <(SC_LN089)
    EQUB <(SC_LN090), <(SC_LN091), <(SC_LN092), <(SC_LN093), <(SC_LN094), <(SC_LN095), <(SC_LN096), <(SC_LN097), <(SC_LN098), <(SC_LN099)
    EQUB <(SC_LN100), <(SC_LN101), <(SC_LN102), <(SC_LN103), <(SC_LN104), <(SC_LN105), <(SC_LN106), <(SC_LN107), <(SC_LN108), <(SC_LN109)
    EQUB <(SC_LN110), <(SC_LN111), <(SC_LN112), <(SC_LN113), <(SC_LN114), <(SC_LN115), <(SC_LN116), <(SC_LN117), <(SC_LN118), <(SC_LN119)
    EQUB <(SC_LN120), <(SC_LN121), <(SC_LN122), <(SC_LN123), <(SC_LN124), <(SC_LN125), <(SC_LN126), <(SC_LN127), <(SC_LN128), <(SC_LN129)
    EQUB <(SC_LN130), <(SC_LN131), <(SC_LN132), <(SC_LN133), <(SC_LN134), <(SC_LN135), <(SC_LN136), <(SC_LN137), <(SC_LN138), <(SC_LN139)
    EQUB <(SC_LN140), <(SC_LN141), <(SC_LN142), <(SC_LN143), <(SC_LN144), <(SC_LN145), <(SC_LN146), <(SC_LN147), <(SC_LN148), <(SC_LN149)
    EQUB <(SC_LN150), <(SC_LN151), <(SC_LN152), <(SC_LN153), <(SC_LN154), <(SC_LN155), <(SC_LN156), <(SC_LN157), <(SC_LN158), <(SC_LN159)
    EQUB <(SC_LN160), <(SC_LN161), <(SC_LN162), <(SC_LN163), <(SC_LN164), <(SC_LN165), <(SC_LN166), <(SC_LN167), <(SC_LN168), <(SC_LN169)
    EQUB <(SC_LN170), <(SC_LN171), <(SC_LN172), <(SC_LN173), <(SC_LN174), <(SC_LN175), <(SC_LN176), <(SC_LN177), <(SC_LN178), <(SC_LN179)
    EQUB <(SC_LN180), <(SC_LN181), <(SC_LN182), <(SC_LN183), <(SC_LN184), <(SC_LN185), <(SC_LN186), <(SC_LN187), <(SC_LN188), <(SC_LN189)
    EQUB <(SC_LN190), <(SC_LN191), <(SC_LN192), <(SC_LN193), <(SC_LN194), <(SC_LN195), <(SC_LN196), <(SC_LN197), <(SC_LN198), <(SC_LN199)
    EQUB <(SC_LN200), <(SC_LN201), <(SC_LN202), <(SC_LN203), <(SC_LN204), <(SC_LN205), <(SC_LN206), <(SC_LN207), <(SC_LN208), <(SC_LN209)
    EQUB <(SC_LN210), <(SC_LN211), <(SC_LN212), <(SC_LN213), <(SC_LN214), <(SC_LN215), <(SC_LN216), <(SC_LN217), <(SC_LN218), <(SC_LN219)
    EQUB <(SC_LN220), <(SC_LN221), <(SC_LN222), <(SC_LN223), <(SC_LN224), <(SC_LN225), <(SC_LN226), <(SC_LN227), <(SC_LN228), <(SC_LN229)
    EQUB <(SC_LN230), <(SC_LN231), <(SC_LN232), <(SC_LN233), <(SC_LN234), <(SC_LN235), <(SC_LN236), <(SC_LN237), <(SC_LN238), <(SC_LN239)
    EQUB <(SC_LN240), <(SC_LN241), <(SC_LN242), <(SC_LN243), <(SC_LN244), <(SC_LN245), <(SC_LN246), <(SC_LN247), <(SC_LN248), <(SC_LN249)
    EQUB <(SC_LN250), <(SC_LN251), <(SC_LN252), <(SC_LN253), <(SC_LN254), <(SC_LN255)

ALIGN &100
.SCR_HOR
	EQUB 00,00,00,00,08,08,08,08,16,16,16,16,24,24,24,24,32,32,32,32,40,40,40,40,48,48,48,48,56,56,56,56
	EQUB 64,64,64,64,72,72,72,72,80,80,80,80,88,88,88,88,96,96,96,96,104,104,104,104,112,112,112,112,120,120,120,120	
	EQUB 128,128,128,128,136,136,136,136,144,144,144,144,152,152,152,152,160,160,160,160,168,168,168,168,176,176,176,176,184,184,184,184
	EQUB 192,192,192,192,200,200,200,200,208,208,208,208,216,216,216,216,224,224,224,224,232,232,232,232,240,240,240,240,248,248,248,248
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
	EQUB $80,$40,$20,$10,$80,$40,$20,$10,$80,$40,$20,$10,$80,$40,$20,$10
	EQUB $80,$40,$20,$10,$80,$40,$20,$10,$80,$40,$20,$10,$80,$40,$20,$10
	EQUB $80,$40,$20,$10,$80,$40,$20,$10,$80,$40,$20,$10,$80,$40,$20,$10
	EQUB $80,$40,$20,$10,$80,$40,$20,$10,$80,$40,$20,$10,$80,$40,$20,$10
	EQUB $80,$40,$20,$10,$80,$40,$20,$10,$80,$40,$20,$10,$80,$40,$20,$10
	EQUB $80,$40,$20,$10,$80,$40,$20,$10,$80,$40,$20,$10,$80,$40,$20,$10
	EQUB $80,$40,$20,$10,$80,$40,$20,$10,$80,$40,$20,$10,$80,$40,$20,$10
	EQUB $80,$40,$20,$10,$80,$40,$20,$10,$80,$40,$20,$10,$80,$40,$20,$10

.SCR_OR_11
    EQUB $88,$44,$22,$11,$88,$44,$22,$11,$88,$44,$22,$11,$88,$44,$22,$11
    EQUB $88,$44,$22,$11,$88,$44,$22,$11,$88,$44,$22,$11,$88,$44,$22,$11
    EQUB $88,$44,$22,$11,$88,$44,$22,$11,$88,$44,$22,$11,$88,$44,$22,$11
    EQUB $88,$44,$22,$11,$88,$44,$22,$11,$88,$44,$22,$11,$88,$44,$22,$11
    EQUB $88,$44,$22,$11,$88,$44,$22,$11,$88,$44,$22,$11,$88,$44,$22,$11
    EQUB $88,$44,$22,$11,$88,$44,$22,$11,$88,$44,$22,$11,$88,$44,$22,$11
    EQUB $88,$44,$22,$11,$88,$44,$22,$11,$88,$44,$22,$11,$88,$44,$22,$11
    EQUB $88,$44,$22,$11,$88,$44,$22,$11,$88,$44,$22,$11,$88,$44,$22,$11
    EQUB $88,$44,$22,$11,$88,$44,$22,$11,$88,$44,$22,$11,$88,$44,$22,$11
    EQUB $88,$44,$22,$11,$88,$44,$22,$11,$88,$44,$22,$11,$88,$44,$22,$11
    EQUB $88,$44,$22,$11,$88,$44,$22,$11,$88,$44,$22,$11,$88,$44,$22,$11
    EQUB $88,$44,$22,$11,$88,$44,$22,$11,$88,$44,$22,$11,$88,$44,$22,$11
    EQUB $88,$44,$22,$11,$88,$44,$22,$11,$88,$44,$22,$11,$88,$44,$22,$11
    EQUB $88,$44,$22,$11,$88,$44,$22,$11,$88,$44,$22,$11,$88,$44,$22,$11
    EQUB $88,$44,$22,$11,$88,$44,$22,$11,$88,$44,$22,$11,$88,$44,$22,$11
    EQUB $88,$44,$22,$11,$88,$44,$22,$11,$88,$44,$22,$11,$88,$44,$22,$11
 
.SCR_AND_00
	EQUB $77,$BB,$DD,$EE,$77,$BB,$DD,$EE,$77,$BB,$DD,$EE,$77,$BB,$DD,$EE
	EQUB $77,$BB,$DD,$EE,$77,$BB,$DD,$EE,$77,$BB,$DD,$EE,$77,$BB,$DD,$EE
	EQUB $77,$BB,$DD,$EE,$77,$BB,$DD,$EE,$77,$BB,$DD,$EE,$77,$BB,$DD,$EE
	EQUB $77,$BB,$DD,$EE,$77,$BB,$DD,$EE,$77,$BB,$DD,$EE,$77,$BB,$DD,$EE
	EQUB $77,$BB,$DD,$EE,$77,$BB,$DD,$EE,$77,$BB,$DD,$EE,$77,$BB,$DD,$EE
	EQUB $77,$BB,$DD,$EE,$77,$BB,$DD,$EE,$77,$BB,$DD,$EE,$77,$BB,$DD,$EE
	EQUB $77,$BB,$DD,$EE,$77,$BB,$DD,$EE,$77,$BB,$DD,$EE,$77,$BB,$DD,$EE
	EQUB $77,$BB,$DD,$EE,$77,$BB,$DD,$EE,$77,$BB,$DD,$EE,$77,$BB,$DD,$EE
	EQUB $77,$BB,$DD,$EE,$77,$BB,$DD,$EE,$77,$BB,$DD,$EE,$77,$BB,$DD,$EE
	EQUB $77,$BB,$DD,$EE,$77,$BB,$DD,$EE,$77,$BB,$DD,$EE,$77,$BB,$DD,$EE
	EQUB $77,$BB,$DD,$EE,$77,$BB,$DD,$EE,$77,$BB,$DD,$EE,$77,$BB,$DD,$EE
	EQUB $77,$BB,$DD,$EE,$77,$BB,$DD,$EE,$77,$BB,$DD,$EE,$77,$BB,$DD,$EE
	EQUB $77,$BB,$DD,$EE,$77,$BB,$DD,$EE,$77,$BB,$DD,$EE,$77,$BB,$DD,$EE
	EQUB $77,$BB,$DD,$EE,$77,$BB,$DD,$EE,$77,$BB,$DD,$EE,$77,$BB,$DD,$EE
	EQUB $77,$BB,$DD,$EE,$77,$BB,$DD,$EE,$77,$BB,$DD,$EE,$77,$BB,$DD,$EE
	EQUB $77,$BB,$DD,$EE,$77,$BB,$DD,$EE,$77,$BB,$DD,$EE,$77,$BB,$DD,$EE

.MAP_DZ_MSB
 EQUB >LINES0TO3_PIXEL0, >LINES0TO3_PIXEL1, >LINES0TO3_PIXEL2, >LINES0TO3_PIXEL3

.SCREEN_SETUP
	LDA #0 ; $65 ; 0 ;$65 ; colour 01 (0000????) and colour 10 (????0000) - bank 1
	LDX #<SCR
	LDY #>SCR
	STX SCN+0
	STY SCN+1
	LDX #>6144
	LDY #<6144
	JSR MEMSET

	LDY  #0
	LDX  #%11111111
	JSR  LINE
 
	LDY  #169
	LDX  #%11111111
	JSR  LINE

;	LDY #170
;	LDA #176
;	LDX #%10101010
;	JSR  BLOCK

	LDY  #170
	LDX  #0
	JSR  LINE

	LDY  #176
	LDX  #0
	JSR  LINE

;	LDY #177
;	LDA #199
;	LDX #%01010101
;	JSR  BLOCK

	LDY  #177
	LDX  #%11111111
	JSR  LINE

	LDY  #199
	LDX  #%11111111
	JSR  LINE
	RTS

.LINE   LDA   SCR_LO,Y
        STA   SCN 
        LDA   SCR_HI,Y 
        STA   SCN+1
        LDA   #0
.LOOP_LSB TAY
          TXA     
          STA  (SCN),Y 
          TYA
          SEC
          SBC   #8
          BNE   LOOP_LSB
        RTS

.MEMSET       STY    LSB_ONLY+1 ; store LSB count
             CPX    #0          ; MSB?     
             BEQ    LSB_ONLY   ; no

             LDY    #0          ; yes so reset LSB
.MSB_LOOP  
.LSB_LOOP      STA    (SCN),Y   ; clear whole MSB
               DEY 
               BNE    LSB_LOOP

              INC    SCN+1      ; inc MSB
              DEX               ; dec MSB count
              BNE    MSB_LOOP

.LSB_ONLY    LDY    #0          ; LSB count 
             BEQ    MS_END     ; not needed

.LAST_LSB_LOOP STA   (SCN),Y
               DEY 
               BNE   LAST_LSB_LOOP
                
              STA   (SCN),Y     ; clear last Y (0)
 
.MS_END      RTS

.MAPTABHI ; (convert 000000xx to xx000000)
FOR I, 0, 255
	EQUB <(I*64)
NEXT

.MAPTABLO ; (convert xxxxxx00 to 00xxxxxx)
FOR I, 0, 255
	EQUB <(I/4)
NEXT

ALIGN &100
.LINES0TO3_PIXEL0 ;SKIP 1024
 EQUB $a9, $4a, $8c, $52, $9a, $6c, $ca, $33, $ca, $33, $65, $59, $a0, $55, $66, $43, $a4, $6c, $46, $cc, $a3, $b0, $99, $63, $86, $33, $64, $86, $6c, $cc, $ca, $33, $4a, $4a, $55, $52, $66, $4a, $ca, $32, $c5, $55, $9a, $cc, $09, $53, $0a, $c5, $55, $66, $29, $9c, $09, $66, $55, $32, $4a, $6c, $86, $aa, $65, $99, $43, $a3, $56, $0a, $33, $c9, $9c, $56, $8c, $65, $a6, $0c, $86, $0c, $65, $d0, $25, $5c, $68, $c9, $a6, $66, $32, $85, $cc, $6a, $ca, $86, $64, $92, $4a, $68, $a4, $35, $66, $4a, $63, $68, $9a, $ac, $32, $9c, $8c, $8c, $a3, $0a, $6a, $56, $99, $a4, $5c, $44, $66, $b0, $99, $66, $92, $c6, $53, $53, $a0, $9a, $58, $b0, $23, $c9, $99, $65, $46, $c9, $99, $d0, $63, $d0, $a0, $aa, $0c, $68, $85, $ca, $8c, $88, $99, $a4, $8c, $22, $99, $34, $52, $66, $53, $56, $64, $c8, $d0, $25, $56, $93, $c8, $4a, $99, $09, $86, $66, $aa, $c5, $58, $cc, $59, $cc, $86, $c9, $95, $a0, $44, $92, $32, $c9, $56, $b0, $25, $33, $a0, $c9, $64, $cc, $d0, $53, $99, $52, $c5, $95, $5c, $85, $0a, $aa, $53, $99, $33, $86, $29, $93, $56, $cc, $a3, $ac, $4a, $c8, $5c, $cc, $0a, $33, $65, $63, $44, $0c, $64, $9a, $8c, $10, $33, $a0, $68, $6c, $39, $35, $22, $0c, $68, $64, $c6, $58, $ac, $59, $32, $4a, $a4, $55, $43, $d0, $cc, $23, $b0, $35, $52, $c8, $86, $cc, $a0, $ac, $33, $aa, $ca, $65
.LINES0TO3_PIXEL1 ;SKIP 1024
 EQUB $c6, $44, $88, $25, $92, $ca, $a6, $32, $95, $32, $86, $58, $a3, $25, $ac, $aa, $b0, $64, $44, $c8, $b0, $93, $3a, $46, $88, $a9, $68, $cc, $86, $a6, $95, $32, $66, $33, $85, $58, $68, $44, $0c, $34, $d0, $63, $a3, $d0, $cc, $36, $99, $d0, $63, $68, $99, $a3, $10, $35, $25, $9a, $aa, $86, $cc, $a4, $64, $09, $aa, $4a, $85, $cc, $56, $0c, $09, $85, $55, $64, $6c, $cc, $cc, $cc, $86, $a6, $22, $85, $86, $a6, $39, $ca, $56, $cc, $8c, $46, $0c, $55, $ac, $5c, $66, $64, $39, $34, $46, $aa, $35, $86, $09, $c6, $23, $92, $55, $88, $4a, $99, $46, $52, $5c, $39, $c9, $33, $53, $93, $a0, $64, $29, $6a, $58, $58, $a3, $c5, $c9, $a4, $22, $a6, $a0, $ca, $66, $a6, $c5, $c8, $ac, $c8, $92, $39, $aa, $86, $cc, $c8, $55, $55, $5c, $4a, $cc, $22, $92, $43, $25, $46, $36, $85, $ac, $6a, $c8, $33, $c9, $a0, $59, $66, $5c, $cc, $66, $35, $39, $d0, $52, $8c, $25, $c8, $55, $0c, $3a, $92, $33, $5c, $34, $0c, $52, $a4, $22, $9a, $c5, $a6, $35, $c8, $a6, $9c, $3a, $25, $59, $29, $85, $cc, $cc, $a4, $36, $a0, $65, $cc, $55, $3a, $52, $0c, $39, $0a, $66, $6a, $85, $6a, $10, $23, $86, $46, $66, $10, $35, $09, $88, $aa, $56, $92, $86, $64, $23, $9a, $55, $cc, $64, $68, $59, $85, $93, $25, $9a, $66, $b0, $52, $66, $0c, $c8, $99, $93, $56, $9c, $6a, $66, $59, $c5, $0a, $56, $6c, $c8, $86
.LINES0TO3_PIXEL2 ;SKIP 1024
 EQUB $d0, $aa, $55, $33, $a0, $a6, $39, $34, $29, $23, $cc, $c9, $6c, $55, $93, $a4, $0a, $68, $66, $59, $0a, $a0, $32, $44, $55, $a4, $64, $0c, $88, $4a, $29, $9a, $ca, $23, $55, $85, $ca, $44, $99, $32, $95, $46, $b0, $95, $6a, $43, $29, $95, $68, $ca, $c5, $39, $cc, $56, $22, $09, $0a, $66, $a6, $39, $35, $99, $93, $44, $66, $6a, $52, $cc, $aa, $cc, $25, $68, $86, $6a, $c8, $59, $cc, $6c, $99, $88, $55, $6c, $34, $95, $52, $0c, $cc, $44, $aa, $36, $93, $c9, $86, $46, $56, $65, $aa, $93, $56, $66, $cc, $59, $55, $a0, $9c, $55, $44, $a0, $44, $9c, $63, $9a, $a6, $34, $9c, $a0, $a3, $35, $33, $46, $63, $c9, $4a, $8c, $0c, $4a, $22, $b0, $92, $c8, $53, $39, $d0, $59, $0a, $d0, $a0, $56, $6c, $55, $a6, $d0, $58, $9c, $63, $aa, $95, $22, $3a, $33, $55, $44, $43, $66, $c6, $ac, $8c, $43, $a6, $c5, $9c, $ca, $52, $6a, $68, $9a, $23, $c8, $9c, $cc, $55, $6a, $25, $cc, $a9, $3a, $23, $52, $32, $aa, $9c, $6c, $33, $09, $59, $4a, $56, $d0, $4a, $09, $32, $22, $58, $99, $55, $a6, $6a, $4a, $a9, $92, $64, $0c, $63, $43, $58, $99, $56, $10, $86, $ac, $cc, $46, $aa, $22, $55, $44, $86, $aa, $34, $10, $cc, $4a, $c9, $5c, $cc, $ac, $55, $92, $85, $8c, $46, $64, $58, $88, $a0, $22, $09, $64, $93, $25, $ca, $cc, $6a, $09, $5c, $52, $c5, $46, $ac, $9c, $8c, $aa, $c9, $64, $59, $55
.LINES0TO3_PIXEL3 ;SKIP 1024
 EQUB $a6, $c6, $58, $32, $a3, $6c, $23, $65, $22, $33, $c8, $95, $86, $85, $3a, $b0, $aa, $64, $86, $9c, $aa, $09, $56, $66, $25, $6c, $68, $aa, $88, $66, $22, $a3, $a6, $22, $58, $88, $a6, $66, $3a, $9a, $29, $33, $0a, $a0, $68, $aa, $99, $29, $86, $95, $59, $9a, $8c, $85, $33, $aa, $aa, $68, $4a, $34, $23, $3a, $a0, $33, $ac, $46, $58, $59, $39, $c8, $33, $ca, $cc, $68, $d0, $36, $c8, $53, $92, $66, $58, $ca, $43, $3a, $58, $10, $c8, $66, $6c, $65, $a0, $a6, $66, $aa, $63, $86, $a4, $5c, $63, $35, $6a, $36, $52, $c5, $c5, $9c, $aa, $c5, $33, $09, $ac, $92, $4a, $43, $a3, $09, $4a, $9a, $a9, $66, $68, $95, $33, $55, $aa, $33, $55, $0a, $3a, $6a, $58, $23, $c8, $58, $cc, $95, $09, $c9, $64, $58, $6c, $c8, $85, $09, $ac, $c6, $29, $22, $32, $56, $85, $66, $33, $68, $d0, $c6, $55, $33, $b0, $d0, $a3, $0c, $9c, $68, $86, $09, $99, $59, $09, $c8, $85, $68, $55, $d0, $93, $43, $99, $36, $56, $39, $a3, $53, $43, $99, $58, $66, $85, $c8, $66, $10, $56, $99, $c9, $92, $58, $b0, $46, $66, $0a, $29, $68, $99, $ac, $33, $85, $a0, $63, $aa, $cc, $93, $0c, $aa, $4a, $33, $58, $44, $cc, $6c, $32, $cc, $0c, $44, $95, $63, $a6, $a4, $63, $29, $cc, $66, $66, $ac, $52, $66, $c5, $33, $aa, $ac, $29, $33, $c8, $8c, $35, $aa, $63, $58, $d0, $66, $0a, $92, $66, $4a, $0c, $68, $36, $36

.mode1Message

.SCREEN_SETUP_PARAMS_START ; mode1Message
EQUB 22, 5                             ; MODE 5
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

EQUB 23, 1, 0, 0, 0, 0, 0, 0, 0, 0     ; cursor off

.SCREEN_SETUP_PARAMS_END ; mode1MessageEnd

.mode1MessageEnd
	
SC_LN000 = SCR+(32*000)+0
SC_LN001 = SCR+(32*000)+1
SC_LN002 = SCR+(32*000)+2
SC_LN003 = SCR+(32*000)+3
SC_LN004 = SCR+(32*000)+4
SC_LN005 = SCR+(32*000)+5
SC_LN006 = SCR+(32*000)+6
SC_LN007 = SCR+(32*000)+7
SC_LN008 = SCR+(32*008)+0
SC_LN009 = SCR+(32*008)+1
SC_LN010 = SCR+(32*008)+2
SC_LN011 = SCR+(32*008)+3
SC_LN012 = SCR+(32*008)+4
SC_LN013 = SCR+(32*008)+5
SC_LN014 = SCR+(32*008)+6
SC_LN015 = SCR+(32*008)+7
SC_LN016 = SCR+(32*016)+0
SC_LN017 = SCR+(32*016)+1
SC_LN018 = SCR+(32*016)+2
SC_LN019 = SCR+(32*016)+3
SC_LN020 = SCR+(32*016)+4
SC_LN021 = SCR+(32*016)+5
SC_LN022 = SCR+(32*016)+6
SC_LN023 = SCR+(32*016)+7
SC_LN024 = SCR+(32*024)+0
SC_LN025 = SCR+(32*024)+1
SC_LN026 = SCR+(32*024)+2
SC_LN027 = SCR+(32*024)+3
SC_LN028 = SCR+(32*024)+4
SC_LN029 = SCR+(32*024)+5
SC_LN030 = SCR+(32*024)+6
SC_LN031 = SCR+(32*024)+7
SC_LN032 = SCR+(32*032)+0
SC_LN033 = SCR+(32*032)+1
SC_LN034 = SCR+(32*032)+2
SC_LN035 = SCR+(32*032)+3
SC_LN036 = SCR+(32*032)+4
SC_LN037 = SCR+(32*032)+5
SC_LN038 = SCR+(32*032)+6
SC_LN039 = SCR+(32*032)+7
SC_LN040 = SCR+(32*040)+0
SC_LN041 = SCR+(32*040)+1
SC_LN042 = SCR+(32*040)+2
SC_LN043 = SCR+(32*040)+3
SC_LN044 = SCR+(32*040)+4
SC_LN045 = SCR+(32*040)+5
SC_LN046 = SCR+(32*040)+6
SC_LN047 = SCR+(32*040)+7
SC_LN048 = SCR+(32*048)+0
SC_LN049 = SCR+(32*048)+1
SC_LN050 = SCR+(32*048)+2
SC_LN051 = SCR+(32*048)+3
SC_LN052 = SCR+(32*048)+4
SC_LN053 = SCR+(32*048)+5
SC_LN054 = SCR+(32*048)+6
SC_LN055 = SCR+(32*048)+7
SC_LN056 = SCR+(32*056)+0
SC_LN057 = SCR+(32*056)+1
SC_LN058 = SCR+(32*056)+2
SC_LN059 = SCR+(32*056)+3
SC_LN060 = SCR+(32*056)+4
SC_LN061 = SCR+(32*056)+5
SC_LN062 = SCR+(32*056)+6
SC_LN063 = SCR+(32*056)+7
SC_LN064 = SCR+(32*064)+0
SC_LN065 = SCR+(32*064)+1
SC_LN066 = SCR+(32*064)+2
SC_LN067 = SCR+(32*064)+3
SC_LN068 = SCR+(32*064)+4
SC_LN069 = SCR+(32*064)+5
SC_LN070 = SCR+(32*064)+6
SC_LN071 = SCR+(32*064)+7
SC_LN072 = SCR+(32*072)+0
SC_LN073 = SCR+(32*072)+1
SC_LN074 = SCR+(32*072)+2
SC_LN075 = SCR+(32*072)+3
SC_LN076 = SCR+(32*072)+4
SC_LN077 = SCR+(32*072)+5
SC_LN078 = SCR+(32*072)+6
SC_LN079 = SCR+(32*072)+7
SC_LN080 = SCR+(32*080)+0
SC_LN081 = SCR+(32*080)+1
SC_LN082 = SCR+(32*080)+2
SC_LN083 = SCR+(32*080)+3
SC_LN084 = SCR+(32*080)+4
SC_LN085 = SCR+(32*080)+5
SC_LN086 = SCR+(32*080)+6
SC_LN087 = SCR+(32*080)+7	
SC_LN088 = SCR+(32*088)+0
SC_LN089 = SCR+(32*088)+1
SC_LN090 = SCR+(32*088)+2
SC_LN091 = SCR+(32*088)+3
SC_LN092 = SCR+(32*088)+4
SC_LN093 = SCR+(32*088)+5
SC_LN094 = SCR+(32*088)+6
SC_LN095 = SCR+(32*088)+7
SC_LN096 = SCR+(32*096)+0
SC_LN097 = SCR+(32*096)+1
SC_LN098 = SCR+(32*096)+2
SC_LN099 = SCR+(32*096)+3
SC_LN100 = SCR+(32*096)+4
SC_LN101 = SCR+(32*096)+5
SC_LN102 = SCR+(32*096)+6
SC_LN103 = SCR+(32*096)+7
SC_LN104 = SCR+(32*104)+0
SC_LN105 = SCR+(32*104)+1
SC_LN106 = SCR+(32*104)+2
SC_LN107 = SCR+(32*104)+3
SC_LN108 = SCR+(32*104)+4
SC_LN109 = SCR+(32*104)+5
SC_LN110 = SCR+(32*104)+6
SC_LN111 = SCR+(32*104)+7
SC_LN112 = SCR+(32*112)+0
SC_LN113 = SCR+(32*112)+1
SC_LN114 = SCR+(32*112)+2
SC_LN115 = SCR+(32*112)+3
SC_LN116 = SCR+(32*112)+4
SC_LN117 = SCR+(32*112)+5
SC_LN118 = SCR+(32*112)+6
SC_LN119 = SCR+(32*112)+7
SC_LN120 = SCR+(32*120)+0
SC_LN121 = SCR+(32*120)+1
SC_LN122 = SCR+(32*120)+2
SC_LN123 = SCR+(32*120)+3
SC_LN124 = SCR+(32*120)+4
SC_LN125 = SCR+(32*120)+5
SC_LN126 = SCR+(32*120)+6
SC_LN127 = SCR+(32*120)+7

SC_LN128 = SCR+(32*128)+0
SC_LN129 = SCR+(32*128)+1
SC_LN130 = SCR+(32*128)+2
SC_LN131 = SCR+(32*128)+3
SC_LN132 = SCR+(32*128)+4
SC_LN133 = SCR+(32*128)+5
SC_LN134 = SCR+(32*128)+6
SC_LN135 = SCR+(32*128)+7
SC_LN136 = SCR+(32*136)+0
SC_LN137 = SCR+(32*136)+1
SC_LN138 = SCR+(32*136)+2
SC_LN139 = SCR+(32*136)+3
SC_LN140 = SCR+(32*136)+4
SC_LN141 = SCR+(32*136)+5
SC_LN142 = SCR+(32*136)+6
SC_LN143 = SCR+(32*136)+7
SC_LN144 = SCR+(32*144)+0
SC_LN145 = SCR+(32*144)+1
SC_LN146 = SCR+(32*144)+2
SC_LN147 = SCR+(32*144)+3
SC_LN148 = SCR+(32*144)+4
SC_LN149 = SCR+(32*144)+5
SC_LN150 = SCR+(32*144)+6
SC_LN151 = SCR+(32*144)+7
SC_LN152 = SCR+(32*152)+0
SC_LN153 = SCR+(32*152)+1
SC_LN154 = SCR+(32*152)+2
SC_LN155 = SCR+(32*152)+3
SC_LN156 = SCR+(32*152)+4
SC_LN157 = SCR+(32*152)+5
SC_LN158 = SCR+(32*152)+6
SC_LN159 = SCR+(32*152)+7
SC_LN160 = SCR+(32*160)+0
SC_LN161 = SCR+(32*160)+1
SC_LN162 = SCR+(32*160)+2
SC_LN163 = SCR+(32*160)+3
SC_LN164 = SCR+(32*160)+4
SC_LN165 = SCR+(32*160)+5
SC_LN166 = SCR+(32*160)+6
SC_LN167 = SCR+(32*160)+7

SC_LN168 = SCR+(32*168)+0
SC_LN169 = SCR+(32*168)+1
SC_LN170 = SCR+(32*168)+2
SC_LN171 = SCR+(32*168)+3
SC_LN172 = SCR+(32*168)+4
SC_LN173 = SCR+(32*168)+5
SC_LN174 = SCR+(32*168)+6
SC_LN175 = SCR+(32*168)+7
SC_LN176 = SCR+(32*176)+0
SC_LN177 = SCR+(32*176)+1
SC_LN178 = SCR+(32*176)+2
SC_LN179 = SCR+(32*176)+3
SC_LN180 = SCR+(32*176)+4
SC_LN181 = SCR+(32*176)+5
SC_LN182 = SCR+(32*176)+6
SC_LN183 = SCR+(32*176)+7
SC_LN184 = SCR+(32*184)+0
SC_LN185 = SCR+(32*184)+1
SC_LN186 = SCR+(32*184)+2
SC_LN187 = SCR+(32*184)+3
SC_LN188 = SCR+(32*184)+4
SC_LN189 = SCR+(32*184)+5
SC_LN190 = SCR+(32*184)+6
SC_LN191 = SCR+(32*184)+7

SC_LN192 = SCR+(32*192)+0
SC_LN193 = SCR+(32*192)+1
SC_LN194 = SCR+(32*192)+2
SC_LN195 = SCR+(32*192)+3
SC_LN196 = SCR+(32*192)+4
SC_LN197 = SCR+(32*192)+5
SC_LN198 = SCR+(32*192)+6
SC_LN199 = SCR+(32*192)+7
SC_LN200 = SCR+(32*200)+0
SC_LN201 = SCR+(32*200)+1
SC_LN202 = SCR+(32*200)+2
SC_LN203 = SCR+(32*200)+3
SC_LN204 = SCR+(32*200)+4
SC_LN205 = SCR+(32*200)+5
SC_LN206 = SCR+(32*200)+6
SC_LN207 = SCR+(32*200)+7
SC_LN208 = SCR+(32*208)+0
SC_LN209 = SCR+(32*208)+1
SC_LN210 = SCR+(32*208)+2
SC_LN211 = SCR+(32*208)+3
SC_LN212 = SCR+(32*208)+4
SC_LN213 = SCR+(32*208)+5
SC_LN214 = SCR+(32*208)+6
SC_LN215 = SCR+(32*208)+7
SC_LN216 = SCR+(32*216)+0
SC_LN217 = SCR+(32*216)+1
SC_LN218 = SCR+(32*216)+2
SC_LN219 = SCR+(32*216)+3
SC_LN220 = SCR+(32*216)+4
SC_LN221 = SCR+(32*216)+5
SC_LN222 = SCR+(32*216)+6
SC_LN223 = SCR+(32*216)+7
SC_LN224 = SCR+(32*224)+0
SC_LN225 = SCR+(32*224)+1
SC_LN226 = SCR+(32*224)+2
SC_LN227 = SCR+(32*224)+3
SC_LN228 = SCR+(32*224)+4
SC_LN229 = SCR+(32*224)+5
SC_LN230 = SCR+(32*224)+6
SC_LN231 = SCR+(32*224)+7
SC_LN232 = SCR+(32*232)+0
SC_LN233 = SCR+(32*232)+1
SC_LN234 = SCR+(32*232)+2
SC_LN235 = SCR+(32*232)+3
SC_LN236 = SCR+(32*232)+4
SC_LN237 = SCR+(32*232)+5
SC_LN238 = SCR+(32*232)+6
SC_LN239 = SCR+(32*232)+7
SC_LN240 = SCR+(32*240)+0
SC_LN241 = SCR+(32*240)+1
SC_LN242 = SCR+(32*240)+2
SC_LN243 = SCR+(32*240)+3
SC_LN244 = SCR+(32*240)+4
SC_LN245 = SCR+(32*240)+5
SC_LN246 = SCR+(32*240)+6
SC_LN247 = SCR+(32*240)+7
SC_LN248 = SCR+(32*240)+0
SC_LN249 = SCR+(32*248)+1
SC_LN250 = SCR+(32*248)+2
SC_LN251 = SCR+(32*248)+3
SC_LN252 = SCR+(32*248)+4
SC_LN253 = SCR+(32*248)+5
SC_LN254 = SCR+(32*248)+6
SC_LN255 = SCR+(32*248)+7

.end

SAVE "256beeb", start, end, start
