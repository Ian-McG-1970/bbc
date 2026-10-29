
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
BB = SCR -6144

ORG &0

.SCN		SKIP 2
.SCN_ZP		SKIP 2
.DST		SKIP 2
.STACK_ZP	SKIP 1
.REGA_BUF	SKIP 1
.REGX_BUF	SKIP 1
.REGY_BUF	SKIP 1
.REGA_INT	SKIP 1
.REGX_INT	SKIP 1
.REGY_INT	SKIP 1
.SCN_02_ZP	SKIP 2
.SCN_03_ZP	SKIP 2
.SCN_04_ZP	SKIP 2
.SCN_05_ZP	SKIP 2
.SCN_06_ZP	SKIP 2
.SCN_07_ZP	SKIP 2
.SCN_08_ZP	SKIP 2
.SCN_09_ZP	SKIP 2
.SCN_10_ZP	SKIP 2
.SCN_11_ZP	SKIP 2
.SCN_12_ZP	SKIP 2
.SCN_13_ZP	SKIP 2
.SCN_14_ZP	SKIP 2
.SCN_15_ZP	SKIP 2
.TV			SKIP 1
.TH			SKIP 1
.INC_VAL	SKIP 1
;.NEXT_SPARE		SKIP 2

SRC = SCN

ORA_BUF_00_00_ZP = INC_VAL +1; $30
AND_BUF_00_00_ZP = ORA_BUF_00_00_ZP +1
ORA_BUF_00_01_ZP = AND_BUF_00_00_ZP +1
AND_BUF_00_01_ZP = ORA_BUF_00_01_ZP +1
ORA_BUF_00_02_ZP = AND_BUF_00_01_ZP +1
AND_BUF_00_02_ZP = ORA_BUF_00_02_ZP +1

ORA_BUF_01_00_ZP = AND_BUF_00_02_ZP +1
AND_BUF_01_00_ZP = ORA_BUF_01_00_ZP +1
ORA_BUF_01_01_ZP = AND_BUF_01_00_ZP +1
AND_BUF_01_01_ZP = ORA_BUF_01_01_ZP +1
ORA_BUF_01_02_ZP = AND_BUF_01_01_ZP +1
AND_BUF_01_02_ZP = ORA_BUF_01_02_ZP +1

ORA_BUF_02_00_ZP = AND_BUF_01_02_ZP +1
AND_BUF_02_00_ZP = ORA_BUF_02_00_ZP +1
ORA_BUF_02_01_ZP = AND_BUF_02_00_ZP +1
AND_BUF_02_01_ZP = ORA_BUF_02_01_ZP +1
ORA_BUF_02_02_ZP = AND_BUF_02_01_ZP +1
AND_BUF_02_02_ZP = ORA_BUF_02_02_ZP +1

ORA_BUF_03_00_ZP = AND_BUF_02_02_ZP +1
AND_BUF_03_00_ZP = ORA_BUF_03_00_ZP +1
ORA_BUF_03_01_ZP = AND_BUF_03_00_ZP +1
AND_BUF_03_01_ZP = ORA_BUF_03_01_ZP +1
ORA_BUF_03_02_ZP = AND_BUF_03_01_ZP +1
AND_BUF_03_02_ZP = ORA_BUF_03_02_ZP +1

ORA_BUF_04_00_ZP = AND_BUF_03_02_ZP +1
AND_BUF_04_00_ZP = ORA_BUF_04_00_ZP +1
ORA_BUF_04_01_ZP = AND_BUF_04_00_ZP +1
AND_BUF_04_01_ZP = ORA_BUF_04_01_ZP +1
ORA_BUF_04_02_ZP = AND_BUF_04_01_ZP +1
AND_BUF_04_02_ZP = ORA_BUF_04_02_ZP +1

ORA_BUF_05_00_ZP = AND_BUF_04_02_ZP +1
AND_BUF_05_00_ZP = ORA_BUF_05_00_ZP +1
ORA_BUF_05_01_ZP = AND_BUF_05_00_ZP +1
AND_BUF_05_01_ZP = ORA_BUF_05_01_ZP +1
ORA_BUF_05_02_ZP = AND_BUF_05_01_ZP +1
AND_BUF_05_02_ZP = ORA_BUF_05_02_ZP +1

ORA_BUF_06_00_ZP = AND_BUF_05_02_ZP +1
AND_BUF_06_00_ZP = ORA_BUF_06_00_ZP +1
ORA_BUF_06_01_ZP = AND_BUF_06_00_ZP +1
AND_BUF_06_01_ZP = ORA_BUF_06_01_ZP +1
ORA_BUF_06_02_ZP = AND_BUF_06_01_ZP +1
AND_BUF_06_02_ZP = ORA_BUF_06_02_ZP +1

ORA_BUF_07_00_ZP = AND_BUF_06_02_ZP +1
AND_BUF_07_00_ZP = ORA_BUF_07_00_ZP +1
ORA_BUF_07_01_ZP = AND_BUF_07_00_ZP +1
AND_BUF_07_01_ZP = ORA_BUF_07_01_ZP +1
ORA_BUF_07_02_ZP = AND_BUF_07_01_ZP +1
AND_BUF_07_02_ZP = ORA_BUF_07_02_ZP +1

ORA_BUF_08_00_ZP = AND_BUF_07_02_ZP +1
AND_BUF_08_00_ZP = ORA_BUF_08_00_ZP +1
ORA_BUF_08_01_ZP = AND_BUF_08_00_ZP +1
AND_BUF_08_01_ZP = ORA_BUF_08_01_ZP +1
ORA_BUF_08_02_ZP = AND_BUF_08_01_ZP +1
AND_BUF_08_02_ZP = ORA_BUF_08_02_ZP +1

ORA_BUF_09_00_ZP = AND_BUF_08_02_ZP +1
AND_BUF_09_00_ZP = ORA_BUF_09_00_ZP +1
ORA_BUF_09_01_ZP = AND_BUF_09_00_ZP +1
AND_BUF_09_01_ZP = ORA_BUF_09_01_ZP +1
ORA_BUF_09_02_ZP = AND_BUF_09_01_ZP +1
AND_BUF_09_02_ZP = ORA_BUF_09_02_ZP +1

ORA_BUF_10_00_ZP = AND_BUF_09_02_ZP +1
AND_BUF_10_00_ZP = ORA_BUF_10_00_ZP +1
ORA_BUF_10_01_ZP = AND_BUF_10_00_ZP +1
AND_BUF_10_01_ZP = ORA_BUF_10_01_ZP +1
ORA_BUF_10_02_ZP = AND_BUF_10_01_ZP +1
AND_BUF_10_02_ZP = ORA_BUF_10_02_ZP +1

ORA_BUF_11_00_ZP = AND_BUF_10_02_ZP +1
AND_BUF_11_00_ZP = ORA_BUF_11_00_ZP +1
ORA_BUF_11_01_ZP = AND_BUF_11_00_ZP +1
AND_BUF_11_01_ZP = ORA_BUF_11_01_ZP +1
ORA_BUF_11_02_ZP = AND_BUF_11_01_ZP +1
AND_BUF_11_02_ZP = ORA_BUF_11_02_ZP +1

ORA_BUF_12_00_ZP = AND_BUF_11_02_ZP +1
AND_BUF_12_00_ZP = ORA_BUF_12_00_ZP +1
ORA_BUF_12_01_ZP = AND_BUF_12_00_ZP +1
AND_BUF_12_01_ZP = ORA_BUF_12_01_ZP +1
ORA_BUF_12_02_ZP = AND_BUF_12_01_ZP +1
AND_BUF_12_02_ZP = ORA_BUF_12_02_ZP +1

ORA_BUF_13_00_ZP = AND_BUF_12_02_ZP +1
AND_BUF_13_00_ZP = ORA_BUF_13_00_ZP +1
ORA_BUF_13_01_ZP = AND_BUF_13_00_ZP +1
AND_BUF_13_01_ZP = ORA_BUF_13_01_ZP +1
ORA_BUF_13_02_ZP = AND_BUF_13_01_ZP +1
AND_BUF_13_02_ZP = ORA_BUF_13_02_ZP +1

ORA_BUF_14_00_ZP = AND_BUF_13_02_ZP +1
AND_BUF_14_00_ZP = ORA_BUF_14_00_ZP +1
ORA_BUF_14_01_ZP = AND_BUF_14_00_ZP +1
AND_BUF_14_01_ZP = ORA_BUF_14_01_ZP +1
ORA_BUF_14_02_ZP = AND_BUF_14_01_ZP +1
AND_BUF_14_02_ZP = ORA_BUF_14_02_ZP +1

ORA_BUF_15_00_ZP = AND_BUF_14_02_ZP +1
AND_BUF_15_00_ZP = ORA_BUF_15_00_ZP +1
ORA_BUF_15_01_ZP = AND_BUF_15_00_ZP +1
AND_BUF_15_01_ZP = ORA_BUF_15_01_ZP +1
ORA_BUF_15_02_ZP = AND_BUF_15_01_ZP +1
AND_BUF_15_02_ZP = ORA_BUF_15_02_ZP +1


	
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
	
	LDA #50
	STA TV
	STA TH

	JSR SCREEN_SETUP
		
	CLI

.MLOOP JMP MLOOP

.PLOT01	LDA SCR_HI,Y 
		STA	SCN_ZP+1
		TYA
		AND #7
		ORA	SCR_HOR,X
		TAY
		LDA	(SCN_ZP),Y
		ORA	SCR_OR_01,X
		STA	(SCN_ZP),Y 
		RTS

.PLOT10	LDA SCR_HI,Y 
		STA	SCN_ZP+1
		TYA
		AND #7
		ORA	SCR_HOR,X
		TAY
		LDA	(SCN_ZP),Y
		ORA	SCR_OR_10,X
		STA	(SCN_ZP),Y 
		RTS

.PLOT11	LDA SCR_HI,Y 
		STA	SCN_ZP+1
		TYA
		AND #7
		ORA	SCR_HOR,X
		TAY
		LDA	(SCN_ZP),Y
		ORA	SCR_OR_11,X
		STA	(SCN_ZP),Y 
		RTS

.CLR00	LDA SCR_HI,Y 
		STA	SCN_ZP+1
		TYA
		AND #7
		ORA	SCR_HOR,X
		TAY
		LDA	(SCN_ZP),Y
		AND	SCR_AND_00,X
		STA	(SCN_ZP),Y 
		RTS

		LDY TV
		LDX TH
		JSR CLR00

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

	INC INC_VAL
 jsr DEBUG_WRITE
 		
		LDY TV
		LDX TH
;		JSR CLR00
		
 LDY TH
 LDX TV
 JSR BAK_SCN ;_32 ; background to screen

 LDY #100
 LDX #100
 LDA #0
; JSR BAK_SCN ;_24 ; background to screen

 LDY #80
 LDX #80
 LDA #1
; JSR BAK_SCN ;_24 ; background to screen

 LDY #30
 LDX #70
 LDA #1
; JSR BAK_SCN ;_24 ; background to screen
 
 JSR READKEYS
 JSR MOVE_PLR

		LDY TV
		LDX TH
;		JSR PLOT01

 LDX TH
 LDY TV
 LDA #0;3
 JSR SPR_SCN ; sprite to screen

 LDX #100
 LDY #100
 LDA #0
; JSR SPR_SCN ; sprite to screen

 LDX #80
 LDY #80
 LDA #1
; JSR SPR_SCN ; sprite to screen

 LDX #30
 LDY #70
 LDA #0
; JSR SPR_SCN ; sprite to screen

 LDA TH
 LDX #0
 LDY #56
 JSR HEX8
 LDA TV
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
          ADC   TV
          STA   TV  ; vertical

          TXA
          BEQ   EXIT2
          BMI   RIGHT

.LEFT     STA   LFT +1
;          STA TEST1
          LDA   TH
          CLC
.LFT      ADC   #0 
          STA   TH
          TAX
          RTS

.RIGHT    EOR   #$FF
          CLC
          ADC   #1    ; carry is clear
          STA   RGT +1
;          STA TEST2
          SEC
          LDA   TH
.RGT      SBC   #0 
          STA   TH
          TAX
          RTS

.EXIT2    LDX TH
          LDY TV
          RTS

;!ZONE SET_BCK_SCN_POS_HGT
;!MACRO SET_BCK_SCN_POS_HGT SRC_ADR, DST_ADR, POS_HGT {
; LDA  SC_LO+POS_HGT,X
; STA  SRC_ADR+1
; STA  DST_ADR+1
; LDA  SC_HI+POS_HGT,X
; STA  DST_ADR+2
; LDA  BB_HI+POS_HGT,X
;; ORA #64
;; ORA  #128
; STA  SRC_ADR+2
;}

.BAK_SCN ; x=ver / y=hor

	LDA	SCR_LO +0,X
	STA SRC00 +1
	STA SRC08 +1
	STA DST00 +1
	STA DST08 +1

	LDA	SCR_LO +1,X
	STA SRC01 +1
	STA SRC09 +1
	STA DST01 +1
	STA DST09 +1

	LDA	SCR_LO +2,X
	STA SRC02 +1
	STA SRC10 +1
	STA DST02 +1
	STA DST10 +1

	LDA	SCR_LO +3,X
	STA SRC03 +1
	STA SRC11 +1
	STA DST03 +1
	STA DST11 +1

	LDA	SCR_LO +4,X
	STA SRC04 +1
	STA SRC12 +1
	STA DST04 +1
	STA DST12 +1
	
	LDA	SCR_LO +5,X
	STA SRC05 +1
	STA SRC13 +1
	STA DST05 +1
	STA DST13 +1

	LDA	SCR_LO +6,X
	STA SRC06 +1
	STA SRC14 +1
	STA DST06 +1
	STA DST14 +1

	LDA	SCR_LO +7,X
	STA SRC07 +1
	STA SRC15 +1
	STA DST07 +1
	STA DST15 +1

	TYA
	
;	LDA SCHI		;4 1
;	STA DST0 		;4 2
;   	ADD #1			;2 3
;	STA DST8		;4 4
;	SUB #<(6144-256);2 5
;	STA 			;4 6
;	ADD #1			;2 8
;	STA 			;4 7 =2 CYCLES FASTER + 256 BYTE TABLE REDUCED
	
	LDY SCR_HI +0,X	;4 1
	STY DST00 +2	;4 2
	INY				;2 3
	STY DST08 +2	;4 4
	LDY BB_HI +0,X	;4 6
	STY SRC00 +2	;4 7
	INY				;2 5
	STY SRC08 +2	;4 8 

	LDY SCR_HI +1,X
	STY DST01 +2
	INY
	STY DST09 +2
	LDY BB_HI +1,X
	STY SRC01 +2
	INY
	STY SRC09 +2

	LDY SCR_HI +2,X
	STY DST02 +2
	INY
	STY DST10 +2
	LDY BB_HI +2,X
	STY SRC02 +2
	INY
	STY SRC10 +2

	LDY SCR_HI +3,X
	STY DST03 +2
	INY
	STY DST11 +2
	LDY BB_HI +3,X
	STY SRC03 +2
	INY
	STY SRC11 +2

	LDY SCR_HI +4,X
	STY DST04 +2
	INY
	STY DST12 +2
	LDY BB_HI +4,X
	STY SRC04 +2
	INY
	STY SRC12 +2

	LDY SCR_HI +5,X
	STY DST05 +2
	INY
	STY DST13 +2
	LDY BB_HI +5,X
	STY SRC05 +2
	INY
	STY SRC13 +2

	LDY SCR_HI +6,X
	STY DST06 +2
	INY
	STY DST14 +2
	LDY BB_HI +6,X
	STY SRC06 +2
	INY
	STY SRC14 +2

	LDY SCR_HI +7,X
	STY DST07 +2
	INY
	STY DST15 +2
	LDY BB_HI +7,X
	STY SRC07 +2
	INY
	STY SRC15 +2

	CLC
	TAY
 LDX SCR_HOR,Y
 LDY #2
 
.LOOP

.SRC15  LDA $ABCD,x ;Y
.DST15  STA $ABCD,x ;Y
.SRC14  LDA $ABCD,x ;Y
.DST14  STA $ABCD,x ;Y
.SRC13  LDA $ABCD,x ;Y
.DST13  STA $ABCD,x ;Y
.SRC12  LDA $ABCD,x ;Y
.DST12  STA $ABCD,x ;Y
.SRC11  LDA $ABCD,x ;Y
.DST11  STA $ABCD,x ;Y
.SRC10  LDA $ABCD,x ;Y
.DST10  STA $ABCD,x ;Y
.SRC09  LDA $ABCD,x ;Y
.DST09  STA $ABCD,x ;Y

.SRC08  LDA $ABCD,x ;Y
.DST08  STA $ABCD,x ;Y
.SRC07  LDA $ABCD,x ;Y
.DST07  STA $ABCD,x ;Y
.SRC06  LDA $ABCD,x ;Y
.DST06  STA $ABCD,x ;Y
.SRC05  LDA $ABCD,x ;Y
.DST05  STA $ABCD,x ;Y
.SRC04  LDA $ABCD,x ;Y
.DST04  STA $ABCD,x ;Y
.SRC03  LDA $ABCD,x ;Y
.DST03  STA $ABCD,x ;Y
.SRC02  LDA $ABCD,x ;Y
.DST02  STA $ABCD,x ;Y
.SRC01  LDA $ABCD,x ;Y
.DST01  STA $ABCD,x ;Y
.SRC00  LDA $ABCD,x ;Y
.DST00  STA $ABCD,x ;Y

  DEY
  BMI EXIT

	TXA
;	CLC
	ADC #8
	TAX
    JMP LOOP

.EXIT RTS

.SPR_SCN ; Y=v X=h A=SPR
{
	STA	REGA_BUF
	STX REGX_BUF
	STY REGY_BUF

	TXA
	AND #3
	TAX
	LDY SPR_SHIFT_ORA_LO_HI_TAB,X
	STY OTL_00_00 +2
	STY OTL_01_00 +2
    STY OTL_02_00 +2
    STY OTL_03_00 +2
    STY OTL_04_00 +2
    STY OTL_05_00 +2
    STY OTL_06_00 +2
    STY OTL_07_00 +2
    STY OTL_08_00 +2
    STY OTL_09_00 +2
    STY OTL_10_00 +2
    STY OTL_11_00 +2
    STY OTL_12_00 +2
    STY OTL_13_00 +2
    STY OTL_14_00 +2
    STY OTL_15_00 +2
	STY OTL_00_01 +2
	STY OTL_01_01 +2
    STY OTL_02_01 +2
    STY OTL_03_01 +2
    STY OTL_04_01 +2
    STY OTL_05_01 +2
    STY OTL_06_01 +2
    STY OTL_07_01 +2
    STY OTL_08_01 +2
    STY OTL_09_01 +2
    STY OTL_10_01 +2
    STY OTL_11_01 +2
    STY OTL_12_01 +2
    STY OTL_13_01 +2
    STY OTL_14_01 +2
    STY OTL_15_01 +2
	INY
	STY OTH_00_00 +2
	STY OTH_01_00 +2
	STY OTH_02_00 +2
	STY OTH_03_00 +2
	STY OTH_04_00 +2
	STY OTH_05_00 +2
	STY OTH_06_00 +2
	STY OTH_07_00 +2
	STY OTH_08_00 +2
	STY OTH_09_00 +2
	STY OTH_10_00 +2
	STY OTH_11_00 +2
	STY OTH_12_00 +2
	STY OTH_13_00 +2
	STY OTH_14_00 +2
	STY OTH_15_00 +2
	STY OTH_00_01 +2
	STY OTH_01_01 +2
	STY OTH_02_01 +2
	STY OTH_03_01 +2
	STY OTH_04_01 +2
	STY OTH_05_01 +2
	STY OTH_06_01 +2
	STY OTH_07_01 +2
	STY OTH_08_01 +2
	STY OTH_09_01 +2
	STY OTH_10_01 +2
	STY OTH_11_01 +2
	STY OTH_12_01 +2
	STY OTH_13_01 +2
	STY OTH_14_01 +2
	STY OTH_15_01 +2

	LDY REGA_BUF
	LDA SPR_TAB_LO,Y
	STA SCN +0
	CLC
	ADC #16
	STA DST +0
	LDA SPR_TAB_HI,Y
	STA SCN +1
	STA DST +1

	LDY #0

	TSX        ; store stack and at end resstore stack?
	STX STACK_ZP

			LDA (SCN),Y
			TAX
			TXS
.OTL_00_00	LDA ORA_0_TAB_LO,X  	; ora tab lo
			STA ORA_BUF_00_00_ZP  	; ora buf 0
			TAX
			LDA AND_TAB,X
			STA AND_BUF_00_00_ZP   	; and buf 0
			TSX
.OTH_00_00 	LDA ORA_0_TAB_HI,X  	; ora tab hi
			STA ORA_BUF_00_01_ZP   	; ora buf 1
			TAX
			LDA AND_TAB,X
			STA AND_BUF_00_01_ZP   	; and buf 1

			LDA (DST),Y
			TAX
			TXS
.OTL_00_01 	LDA ORA_0_TAB_LO,X
			ORA ORA_BUF_00_01_ZP
			STA ORA_BUF_00_01_ZP
			TAX
			LDA AND_TAB,X
			and AND_BUF_00_01_ZP
			STA AND_BUF_00_01_ZP
			TSX
.OTH_00_01 	LDA ORA_0_TAB_HI,X
			STA ORA_BUF_00_02_ZP
			TAX
			LDA AND_TAB,X
			STA AND_BUF_00_02_ZP

			INY

			LDA (SCN),Y
			TAX
			TXS 
.OTL_01_00 	LDA ORA_0_TAB_LO,X
			STA ORA_BUF_01_00_ZP
			TAX
			LDA AND_TAB,X
			STA AND_BUF_01_00_ZP
			TSX
.OTH_01_00 	LDA ORA_0_TAB_HI,X
			STA ORA_BUF_01_01_ZP
			TAX
			LDA AND_TAB,X
			STA AND_BUF_01_01_ZP

			LDA (DST),Y
			TAX
			TXS
.OTL_01_01 	LDA ORA_0_TAB_LO,X
			ORA ORA_BUF_01_01_ZP
			STA ORA_BUF_01_01_ZP
			TAX
			LDA AND_TAB,X
			AND AND_BUF_01_01_ZP
			STA AND_BUF_01_01_ZP
			TSX
.OTH_01_01 	LDA ORA_0_TAB_HI,X
			STA ORA_BUF_01_02_ZP
			TAX
			LDA AND_TAB,X
			STA AND_BUF_01_02_ZP

			INY

			LDA (SCN),Y
			TAX
			TXS
.OTL_02_00 	LDA ORA_0_TAB_LO,X
			STA ORA_BUF_02_00_ZP
			TAX
			LDA AND_TAB,X
			STA AND_BUF_02_00_ZP
			TSX
.OTH_02_00 	LDA ORA_0_TAB_HI,X
			STA ORA_BUF_02_01_ZP
			TAX
			LDA AND_TAB,X
			STA AND_BUF_02_01_ZP

			LDA (DST),Y
			TAX
			TXS
.OTL_02_01 	LDA ORA_0_TAB_LO,X
			ORA ORA_BUF_02_01_ZP
			STA ORA_BUF_02_01_ZP
			TAX
			LDA AND_TAB,X
			AND AND_BUF_02_01_ZP
			STA AND_BUF_02_01_ZP
			TSX
.OTH_02_01 	LDA ORA_0_TAB_HI,X
			STA ORA_BUF_02_02_ZP
			TAX
			LDA AND_TAB,X
			STA AND_BUF_02_02_ZP

			INY

			LDA (SCN),Y
			TAX
			TXS
.OTL_03_00 	LDA ORA_0_TAB_LO,X
			STA ORA_BUF_03_00_ZP
			TAX
			LDA AND_TAB,X
			STA AND_BUF_03_00_ZP
			TSX
.OTH_03_00 	LDA ORA_0_TAB_HI,X
			STA ORA_BUF_03_01_ZP
			TAX
			LDA AND_TAB,X
			STA AND_BUF_03_01_ZP

			LDA (DST),Y
			TAX
			TXS
.OTL_03_01 	LDA ORA_0_TAB_LO,X
			ORA ORA_BUF_03_01_ZP
			STA ORA_BUF_03_01_ZP
			TAX
			LDA AND_TAB,X
			AND AND_BUF_03_01_ZP
			STA AND_BUF_03_01_ZP
			TSX
.OTH_03_01 	LDA ORA_0_TAB_HI,X
			STA ORA_BUF_03_02_ZP
			TAX
			LDA AND_TAB,X
			STA AND_BUF_03_02_ZP 

			INY

			LDA (SCN),Y
			TAX
			TXS
.OTL_04_00 	LDA ORA_0_TAB_LO,X
			STA ORA_BUF_04_00_ZP
			TAX
			LDA AND_TAB,X
			STA AND_BUF_04_00_ZP
			TSX
.OTH_04_00 	LDA ORA_0_TAB_HI,X
			STA ORA_BUF_04_01_ZP
			TAX
			LDA AND_TAB,X
			STA AND_BUF_04_01_ZP

			LDA (DST),Y
			TAX
			TXS
.OTL_04_01 	LDA ORA_0_TAB_LO,X
			ORA ORA_BUF_04_01_ZP
			STA ORA_BUF_04_01_ZP
			TAX
			LDA AND_TAB,X
			AND AND_BUF_04_01_ZP
			STA AND_BUF_04_01_ZP
			TSX
.OTH_04_01 	LDA ORA_0_TAB_HI,X
			STA ORA_BUF_04_02_ZP
			TAX
			LDA AND_TAB,X
			STA AND_BUF_04_02_ZP 

			INY

			LDA (SCN),Y
			TAX
			TXS
.OTL_05_00 	LDA ORA_0_TAB_LO,X
			STA ORA_BUF_05_00_ZP
			TAX
			LDA AND_TAB,X
			STA AND_BUF_05_00_ZP
			TSX
.OTH_05_00 	LDA ORA_0_TAB_HI,X
			STA ORA_BUF_05_01_ZP
			TAX
			LDA AND_TAB,X
			STA AND_BUF_05_01_ZP

			LDA (DST),Y
			TAX
			TXS
.OTL_05_01 	LDA ORA_0_TAB_LO,X
			ORA ORA_BUF_05_01_ZP
			STA ORA_BUF_05_01_ZP
			TAX
			LDA AND_TAB,X
			AND AND_BUF_05_01_ZP
			STA AND_BUF_05_01_ZP
			TSX
.OTH_05_01 	LDA ORA_0_TAB_HI,X
			STA ORA_BUF_05_02_ZP
			TAX
			LDA AND_TAB,X
			STA AND_BUF_05_02_ZP 

			INY

			LDA (SCN),Y
			TAX
			TXS
.OTL_06_00 	LDA ORA_0_TAB_LO,X
			STA ORA_BUF_06_00_ZP
			TAX
			LDA AND_TAB,X
			STA AND_BUF_06_00_ZP
			TSX
.OTH_06_00 	LDA ORA_0_TAB_HI,X
			STA ORA_BUF_06_01_ZP
			TAX
			LDA AND_TAB,X
			STA AND_BUF_06_01_ZP

			LDA (DST),Y
			TAX
			TXS
.OTL_06_01 	LDA ORA_0_TAB_LO,X
			ORA ORA_BUF_06_01_ZP
			STA ORA_BUF_06_01_ZP
			TAX
			LDA AND_TAB,X
			AND AND_BUF_06_01_ZP
			STA AND_BUF_06_01_ZP
			TSX
.OTH_06_01 	LDA ORA_0_TAB_HI,X
			STA ORA_BUF_06_02_ZP
			TAX
			LDA AND_TAB,X
			STA AND_BUF_06_02_ZP 

			INY

			LDA (SCN),Y
			TAX
			TXS
.OTL_07_00 	LDA ORA_0_TAB_LO,X
			STA ORA_BUF_07_00_ZP
			TAX
			LDA AND_TAB,X
			STA AND_BUF_07_00_ZP
			TSX
.OTH_07_00 	LDA ORA_0_TAB_HI,X
			STA ORA_BUF_07_01_ZP
			TAX
			LDA AND_TAB,X
			STA AND_BUF_07_01_ZP

			LDA (DST),Y
			TAX
			TXS
.OTL_07_01 	LDA ORA_0_TAB_LO,X
			ORA ORA_BUF_07_01_ZP
			STA ORA_BUF_07_01_ZP
			TAX
			LDA AND_TAB,X
			AND AND_BUF_07_01_ZP
			STA AND_BUF_07_01_ZP
			TSX
.OTH_07_01 	LDA ORA_0_TAB_HI,X
			STA ORA_BUF_07_02_ZP
			TAX
			LDA AND_TAB,X
			STA AND_BUF_07_02_ZP 

			INY

			LDA (SCN),Y
			TAX
			TXS 
.OTL_08_00 	LDA ORA_0_TAB_LO,X
			STA ORA_BUF_08_00_ZP
			TAX
			LDA AND_TAB,X
			STA AND_BUF_08_00_ZP
			TSX
.OTH_08_00 	LDA ORA_0_TAB_HI,X
			STA ORA_BUF_08_01_ZP
			TAX
			LDA AND_TAB,X
			STA AND_BUF_08_01_ZP

			LDA (DST),Y
			TAX
			TXS 
.OTL_08_01 	LDA ORA_0_TAB_LO,X
			ORA ORA_BUF_08_01_ZP
			STA ORA_BUF_08_01_ZP
			TAX
			LDA AND_TAB,X
			AND AND_BUF_08_01_ZP
			STA AND_BUF_08_01_ZP
			TSX
.OTH_08_01 	LDA ORA_0_TAB_HI,X
			STA ORA_BUF_08_02_ZP
			TAX
			LDA AND_TAB,X
			STA AND_BUF_08_02_ZP 

			INY

			LDA (SCN),Y
			TAX
			TXS
.OTL_09_00 	LDA ORA_0_TAB_LO,X
			STA ORA_BUF_09_00_ZP
			TAX
			LDA AND_TAB,X
			STA AND_BUF_09_00_ZP
			TSX
.OTH_09_00 	LDA ORA_0_TAB_HI,X
			STA ORA_BUF_09_01_ZP
			TAX
			LDA AND_TAB,X
			STA AND_BUF_09_01_ZP

			LDA (DST),Y
			TAX
			TXS 
.OTL_09_01 	LDA ORA_0_TAB_LO,X
			ORA ORA_BUF_09_01_ZP
			STA ORA_BUF_09_01_ZP
			TAX
			LDA AND_TAB,X
			AND AND_BUF_09_01_ZP
			STA AND_BUF_09_01_ZP
			TSX
.OTH_09_01 	LDA ORA_0_TAB_HI,X
			STA ORA_BUF_09_02_ZP
			TAX
			LDA AND_TAB,X
			STA AND_BUF_09_02_ZP 

			INY

			LDA (SCN),Y
			TAX
			TXS
.OTL_10_00 	LDA ORA_0_TAB_LO,X
			STA ORA_BUF_10_00_ZP
			TAX
			LDA AND_TAB,X
			STA AND_BUF_10_00_ZP
			TSX
.OTH_10_00 	LDA ORA_0_TAB_HI,X
			STA ORA_BUF_10_01_ZP
			TAX
			LDA AND_TAB,X
			STA AND_BUF_10_01_ZP

			LDA (DST),Y
			TAX
			TXS
.OTL_10_01 	LDA ORA_0_TAB_LO,X
			ORA ORA_BUF_10_01_ZP
			STA ORA_BUF_10_01_ZP
			TAX
			LDA AND_TAB,X
			AND AND_BUF_10_01_ZP
			STA AND_BUF_10_01_ZP
			TSX
.OTH_10_01 	LDA ORA_0_TAB_HI,X
			STA ORA_BUF_10_02_ZP
			TAX
			LDA AND_TAB,X
			STA AND_BUF_10_02_ZP 

			INY

			LDA (SCN),Y
			TAX
			TXS
.OTL_11_00 	LDA ORA_0_TAB_LO,X
			STA ORA_BUF_11_00_ZP
			TAX
			LDA AND_TAB,X
			STA AND_BUF_11_00_ZP
			TSX
.OTH_11_00 	LDA ORA_0_TAB_HI,X
			STA ORA_BUF_11_01_ZP
			TAX
			LDA AND_TAB,X
			STA AND_BUF_11_01_ZP

			LDA (DST),Y
			TAX
			TXS
.OTL_11_01 	LDA ORA_0_TAB_LO,X
			ORA ORA_BUF_11_01_ZP
			STA ORA_BUF_11_01_ZP
			TAX
			LDA AND_TAB,X
			AND AND_BUF_11_01_ZP
			STA AND_BUF_11_01_ZP
			TSX
.OTH_11_01 	LDA ORA_0_TAB_HI,X
			STA ORA_BUF_11_02_ZP
			TAX
			LDA AND_TAB,X
			STA AND_BUF_11_02_ZP 

			INY

			LDA (SCN),Y
			TAX
			TXS
.OTL_12_00 	LDA ORA_0_TAB_LO,X
			STA ORA_BUF_12_00_ZP
			TAX
			LDA AND_TAB,X
			STA AND_BUF_12_00_ZP
			TSX
.OTH_12_00 	LDA ORA_0_TAB_HI,X
			STA ORA_BUF_12_01_ZP
			TAX
			LDA AND_TAB,X
			STA AND_BUF_12_01_ZP

			LDA (DST),Y
			TAX
			TXS
.OTL_12_01 	LDA ORA_0_TAB_LO,X
			ORA ORA_BUF_12_01_ZP
			STA ORA_BUF_12_01_ZP
			TAX
			LDA AND_TAB,X
			AND AND_BUF_12_01_ZP
			STA AND_BUF_12_01_ZP
			TSX
.OTH_12_01 	LDA ORA_0_TAB_HI,X
			STA ORA_BUF_12_02_ZP
			TAX
			LDA AND_TAB,X
			STA AND_BUF_12_02_ZP 

			INY

			LDA (SCN),Y
			TAX
			TXS
.OTL_13_00 	LDA ORA_0_TAB_LO,X
			STA ORA_BUF_13_00_ZP
			TAX
			LDA AND_TAB,X
			STA AND_BUF_13_00_ZP
			TSX
.OTH_13_00 	LDA ORA_0_TAB_HI,X
			STA ORA_BUF_13_01_ZP
			TAX
			LDA AND_TAB,X
			STA AND_BUF_13_01_ZP

			LDA (DST),Y
			TAX
			TXS
.OTL_13_01 	LDA ORA_0_TAB_LO,X
			ORA ORA_BUF_13_01_ZP
			STA ORA_BUF_13_01_ZP
			TAX
			LDA AND_TAB,X
			AND AND_BUF_13_01_ZP
			STA AND_BUF_13_01_ZP
			TSX
.OTH_13_01 	LDA ORA_0_TAB_HI,X
			STA ORA_BUF_13_02_ZP
			TAX
			LDA AND_TAB,X
			STA AND_BUF_13_02_ZP 

			INY

			LDA (SCN),Y
			TAX
			TXS
.OTL_14_00 	LDA ORA_0_TAB_LO,X
			STA ORA_BUF_14_00_ZP
			TAX
			LDA AND_TAB,X
			STA AND_BUF_14_00_ZP
			TSX
.OTH_14_00 	LDA ORA_0_TAB_HI,X
			STA ORA_BUF_14_01_ZP
			TAX
			LDA AND_TAB,X
			STA AND_BUF_14_01_ZP

			LDA (DST),Y
			TAX
			TXS
.OTL_14_01 	LDA ORA_0_TAB_LO,X
			ORA ORA_BUF_14_01_ZP
			STA ORA_BUF_14_01_ZP
			TAX
			LDA AND_TAB,X
			AND AND_BUF_14_01_ZP
			STA AND_BUF_14_01_ZP
			TSX
.OTH_14_01 	LDA ORA_0_TAB_HI,X
			STA ORA_BUF_14_02_ZP
			TAX
			LDA AND_TAB,X
			STA AND_BUF_14_02_ZP 

			INY

			LDA (SCN),Y
			TAX
			TXS
.OTL_15_00 	LDA ORA_0_TAB_LO,X
			STA ORA_BUF_15_00_ZP
			TAX
			LDA AND_TAB,X
			STA AND_BUF_15_00_ZP
			TSX
.OTH_15_00 	LDA ORA_0_TAB_HI,X
			STA ORA_BUF_15_01_ZP
			TAX
			LDA AND_TAB,X
			STA AND_BUF_15_01_ZP

			LDA (DST),Y
			TAX
			TXS
.OTL_15_01 	LDA ORA_0_TAB_LO,X
			ORA ORA_BUF_15_01_ZP
			STA ORA_BUF_15_01_ZP
			TAX
			LDA AND_TAB,X
			AND AND_BUF_15_01_ZP
			STA AND_BUF_15_01_ZP
			TSX
.OTH_15_01 	LDA ORA_0_TAB_HI,X
			STA ORA_BUF_15_02_ZP
			TAX
			LDA AND_TAB,X
			STA AND_BUF_15_02_ZP 

		LDX STACK_ZP
		TXS

		LDX REGY_BUF
  
		LDA	SCR_LO +0,X
		STA SCN +0
		STA SCN_08_ZP +0

		LDA	SCR_LO +1,X
		STA DST +0
		STA SCN_09_ZP +0

		LDA	SCR_LO +2,X
		STA SCN_02_ZP +0
		STA SCN_10_ZP +0

		LDA	SCR_LO +3,X
		STA SCN_03_ZP +0
		STA SCN_11_ZP +0

		LDA	SCR_LO +4,X
		STA SCN_04_ZP +0
		STA SCN_12_ZP +0
	
		LDA	SCR_LO +5,X
		STA SCN_05_ZP +0
		STA SCN_13_ZP +0

		LDA	SCR_LO +6,X
		STA SCN_06_ZP +0
		STA SCN_14_ZP +0

		LDA	SCR_LO +7,X
		STA SCN_07_ZP +0
		STA SCN_15_ZP +0
	
		LDY SCR_HI +0,X	;4 1
		STY SCN +1	;4 2
		INY				;2 3
		STY SCN_08_ZP +1	;4 4

		LDY SCR_HI +1,X
		STY DST +1
		INY
		STY SCN_09_ZP +1

		LDY SCR_HI +2,X
		STY SCN_02_ZP +1
		INY
		STY SCN_10_ZP +1

		LDY SCR_HI +3,X
		STY SCN_03_ZP +1
		INY
		STY SCN_11_ZP +1

		LDY SCR_HI +4,X
		STY SCN_04_ZP +1
		INY
		STY SCN_12_ZP +1

		LDY SCR_HI +5,X
		STY SCN_05_ZP +1
		INY
		STY SCN_13_ZP +1

		LDY SCR_HI +6,X
		STY SCN_06_ZP +1
		INY
		STY SCN_14_ZP +1

		LDY SCR_HI +7,X
		STY SCN_07_ZP +1
		INY
		STY SCN_15_ZP +1

		LDX	REGX_BUF
		LDY SCR_HOR,X

		LDA (SCN),Y
		AND AND_BUF_00_00_ZP
		ORA ORA_BUF_00_00_ZP
		STA (SCN),Y

		LDA (DST),Y
		AND AND_BUF_01_00_ZP
		ORA ORA_BUF_01_00_ZP
		STA (DST),Y

		LDA (SCN_02_ZP),Y
		AND AND_BUF_02_00_ZP
		ORA ORA_BUF_02_00_ZP
		STA (SCN_02_ZP),Y

		LDA (SCN_03_ZP),Y
		AND AND_BUF_03_00_ZP
		ORA ORA_BUF_03_00_ZP
		STA (SCN_03_ZP),Y

		LDA (SCN_04_ZP),Y
		AND AND_BUF_04_00_ZP
		ORA ORA_BUF_04_00_ZP
		STA (SCN_04_ZP),Y

		LDA (SCN_05_ZP),Y
		AND AND_BUF_05_00_ZP
		ORA ORA_BUF_05_00_ZP
		STA (SCN_05_ZP),Y

		LDA (SCN_06_ZP),Y
		AND AND_BUF_06_00_ZP
		ORA ORA_BUF_06_00_ZP
		STA (SCN_06_ZP),Y

		LDA (SCN_07_ZP),Y
		AND AND_BUF_07_00_ZP
		ORA ORA_BUF_07_00_ZP
		STA (SCN_07_ZP),Y

		LDA (SCN_08_ZP),Y
		AND AND_BUF_08_00_ZP
		ORA ORA_BUF_08_00_ZP
		STA (SCN_08_ZP),Y

		LDA (SCN_09_ZP),Y
		AND AND_BUF_09_00_ZP
		ORA ORA_BUF_09_00_ZP
		STA (SCN_09_ZP),Y

		LDA (SCN_10_ZP),Y
		AND AND_BUF_10_00_ZP
		ORA ORA_BUF_10_00_ZP
		STA (SCN_10_ZP),Y

		LDA (SCN_11_ZP),Y
		AND AND_BUF_11_00_ZP
		ORA ORA_BUF_11_00_ZP
		STA (SCN_11_ZP),Y

		LDA (SCN_12_ZP),Y
		AND AND_BUF_12_00_ZP
		ORA ORA_BUF_12_00_ZP
		STA (SCN_12_ZP),Y

		LDA (SCN_13_ZP),Y
		AND AND_BUF_13_00_ZP
		ORA ORA_BUF_13_00_ZP
		STA (SCN_13_ZP),Y

		LDA (SCN_14_ZP),Y
		AND AND_BUF_14_00_ZP
		ORA ORA_BUF_14_00_ZP
		STA (SCN_14_ZP),Y

		LDA (SCN_15_ZP),Y
		AND AND_BUF_15_00_ZP
		ORA ORA_BUF_15_00_ZP
		STA (SCN_15_ZP),Y

 tya
	CLC
 adc #8
 tay

		LDA (SCN),Y
		AND AND_BUF_00_01_ZP
		ORA ORA_BUF_00_01_ZP
		STA (SCN),Y

      LDA (DST),Y
      AND AND_BUF_01_01_ZP
      ORA ORA_BUF_01_01_ZP
      STA (DST),Y

      LDA (SCN_02_ZP),Y
      AND AND_BUF_02_01_ZP
      ORA ORA_BUF_02_01_ZP
      STA (SCN_02_ZP),Y

      LDA (SCN_03_ZP),Y
      AND AND_BUF_03_01_ZP
      ORA ORA_BUF_03_01_ZP
      STA (SCN_03_ZP),Y

      LDA (SCN_04_ZP),Y
      AND AND_BUF_04_01_ZP
      ORA ORA_BUF_04_01_ZP
      STA (SCN_04_ZP),Y

      LDA (SCN_05_ZP),Y
      AND AND_BUF_05_01_ZP
      ORA ORA_BUF_05_01_ZP
      STA (SCN_05_ZP),Y

      LDA (SCN_06_ZP),Y
      AND AND_BUF_06_01_ZP
      ORA ORA_BUF_06_01_ZP
      STA (SCN_06_ZP),Y

      LDA (SCN_07_ZP),Y
      AND AND_BUF_07_01_ZP
      ORA ORA_BUF_07_01_ZP
      STA (SCN_07_ZP),Y

      LDA (SCN_08_ZP),Y
      AND AND_BUF_08_01_ZP
      ORA ORA_BUF_08_01_ZP
      STA (SCN_08_ZP),Y

      LDA (SCN_09_ZP),Y
      AND AND_BUF_09_01_ZP
      ORA ORA_BUF_09_01_ZP
      STA (SCN_09_ZP),Y

      LDA (SCN_10_ZP),Y
      AND AND_BUF_10_01_ZP
      ORA ORA_BUF_10_01_ZP
      STA (SCN_10_ZP),Y

      LDA (SCN_11_ZP),Y
      AND AND_BUF_11_01_ZP
      ORA ORA_BUF_11_01_ZP
      STA (SCN_11_ZP),Y

      LDA (SCN_12_ZP),Y
      AND AND_BUF_12_01_ZP
      ORA ORA_BUF_12_01_ZP
      STA (SCN_12_ZP),Y

      LDA (SCN_13_ZP),Y
      AND AND_BUF_13_01_ZP
      ORA ORA_BUF_13_01_ZP
      STA (SCN_13_ZP),Y

      LDA (SCN_14_ZP),Y
      AND AND_BUF_14_01_ZP
      ORA ORA_BUF_14_01_ZP
      STA (SCN_14_ZP),Y

      LDA (SCN_15_ZP),Y
      AND AND_BUF_15_01_ZP
      ORA ORA_BUF_15_01_ZP
      STA (SCN_15_ZP),Y

 tya
  clc
 adc #8
 tay

      LDA (SCN),Y
      AND AND_BUF_00_02_ZP
      ORA ORA_BUF_00_02_ZP
      STA (SCN),Y

      LDA (DST),Y
      AND AND_BUF_01_02_ZP
      ORA ORA_BUF_01_02_ZP
      STA (DST),Y

      LDA (SCN_02_ZP),Y
      AND AND_BUF_02_02_ZP
      ORA ORA_BUF_02_02_ZP
      STA (SCN_02_ZP),Y

      LDA (SCN_03_ZP),Y
      AND AND_BUF_03_02_ZP
      ORA ORA_BUF_03_02_ZP
      STA (SCN_03_ZP),Y

      LDA (SCN_04_ZP),Y
      AND AND_BUF_04_02_ZP
      ORA ORA_BUF_04_02_ZP
      STA (SCN_04_ZP),Y

      LDA (SCN_05_ZP),Y
      AND AND_BUF_05_02_ZP
      ORA ORA_BUF_05_02_ZP
      STA (SCN_05_ZP),Y

      LDA (SCN_06_ZP),Y
      AND AND_BUF_06_02_ZP
      ORA ORA_BUF_06_02_ZP
      STA (SCN_06_ZP),Y

      LDA (SCN_07_ZP),Y
      AND AND_BUF_07_02_ZP
      ORA ORA_BUF_07_02_ZP
      STA (SCN_07_ZP),Y

      LDA (SCN_08_ZP),Y
      AND AND_BUF_08_02_ZP
      ORA ORA_BUF_08_02_ZP
      STA (SCN_08_ZP),Y

      LDA (SCN_09_ZP),Y
      AND AND_BUF_09_02_ZP
      ORA ORA_BUF_09_02_ZP
      STA (SCN_09_ZP),Y

      LDA (SCN_10_ZP),Y
      AND AND_BUF_10_02_ZP
      ORA ORA_BUF_10_02_ZP
      STA (SCN_10_ZP),Y

      LDA (SCN_11_ZP),Y
      AND AND_BUF_11_02_ZP
      ORA ORA_BUF_11_02_ZP
      STA (SCN_11_ZP),Y

      LDA (SCN_12_ZP),Y
      AND AND_BUF_12_02_ZP
      ORA ORA_BUF_12_02_ZP
      STA (SCN_12_ZP),Y

      LDA (SCN_13_ZP),Y
      AND AND_BUF_13_02_ZP
      ORA ORA_BUF_13_02_ZP
      STA (SCN_13_ZP),Y

      LDA (SCN_14_ZP),Y
      AND AND_BUF_14_02_ZP
      ORA ORA_BUF_14_02_ZP
      STA (SCN_14_ZP),Y

      LDA (SCN_15_ZP),Y
      AND AND_BUF_15_02_ZP
      ORA ORA_BUF_15_02_ZP
      STA (SCN_15_ZP),Y
      
  RTS
}

.DEBUG_WRITE

 LDA INC_VAL
 LDX #0
 LDY #0
 JSR HEX8

 LDA TV
 LDX #0
 LDY #6
 JSR HEX8

 LDA TH
 LDX #0
 LDY #12
 JSR HEX8

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
    EQUB >(SC_LN190+0), >(SC_LN191+0);, >(SC_LN192+0), >(SC_LN193+0), >(SC_LN194+0), >(SC_LN195+0), >(SC_LN196+0), >(SC_LN197+0), >(SC_LN198+0), >(SC_LN199+0)
;    EQUB >(SC_LN200+0), >(SC_LN201+0), >(SC_LN202+0), >(SC_LN203+0), >(SC_LN204+0), >(SC_LN205+0), >(SC_LN206+0), >(SC_LN207+0), >(SC_LN208+0), >(SC_LN209+0)
;    EQUB >(SC_LN210+0), >(SC_LN211+0), >(SC_LN212+0), >(SC_LN213+0), >(SC_LN214+0), >(SC_LN215+0), >(SC_LN216+0), >(SC_LN217+0), >(SC_LN218+0), >(SC_LN219+0)
;    EQUB >(SC_LN220+0), >(SC_LN221+0), >(SC_LN222+0), >(SC_LN223+0), >(SC_LN224+0), >(SC_LN225+0), >(SC_LN226+0), >(SC_LN227+0), >(SC_LN228+0), >(SC_LN229+0)
;    EQUB >(SC_LN230+0), >(SC_LN231+0), >(SC_LN232+0), >(SC_LN233+0), >(SC_LN234+0), >(SC_LN235+0), >(SC_LN236+0), >(SC_LN237+0), >(SC_LN238+0), >(SC_LN239+0)
;    EQUB >(SC_LN240+0), >(SC_LN241+0), >(SC_LN242+0), >(SC_LN243+0), >(SC_LN244+0), >(SC_LN245+0), >(SC_LN246+0), >(SC_LN247+0), >(SC_LN248+0), >(SC_LN249+0)
;    EQUB >(SC_LN250+0), >(SC_LN251+0), >(SC_LN252+0), >(SC_LN253+0), >(SC_LN254+0), >(SC_LN255+0)

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
    EQUB <(SC_LN190), <(SC_LN191);, <(SC_LN192), <(SC_LN193), <(SC_LN194), <(SC_LN195), <(SC_LN196), <(SC_LN197), <(SC_LN198), <(SC_LN199)
;    EQUB <(SC_LN200), <(SC_LN201), <(SC_LN202), <(SC_LN203), <(SC_LN204), <(SC_LN205), <(SC_LN206), <(SC_LN207), <(SC_LN208), <(SC_LN209)
;    EQUB <(SC_LN210), <(SC_LN211), <(SC_LN212), <(SC_LN213), <(SC_LN214), <(SC_LN215), <(SC_LN216), <(SC_LN217), <(SC_LN218), <(SC_LN219)
;    EQUB <(SC_LN220), <(SC_LN221), <(SC_LN222), <(SC_LN223), <(SC_LN224), <(SC_LN225), <(SC_LN226), <(SC_LN227), <(SC_LN228), <(SC_LN229)
;    EQUB <(SC_LN230), <(SC_LN231), <(SC_LN232), <(SC_LN233), <(SC_LN234), <(SC_LN235), <(SC_LN236), <(SC_LN237), <(SC_LN238), <(SC_LN239)
;    EQUB <(SC_LN240), <(SC_LN241), <(SC_LN242), <(SC_LN243), <(SC_LN244), <(SC_LN245), <(SC_LN246), <(SC_LN247), <(SC_LN248), <(SC_LN249)
;    EQUB <(SC_LN250), <(SC_LN251), <(SC_LN252), <(SC_LN253), <(SC_LN254), <(SC_LN255)

.BB_HI
    EQUB >(SC_LN000-6144), >(SC_LN001-6144), >(SC_LN002-6144), >(SC_LN003-6144), >(SC_LN004-6144), >(SC_LN005-6144), >(SC_LN006-6144), >(SC_LN007-6144), >(SC_LN008-6144), >(SC_LN009-6144)
    EQUB >(SC_LN010-6144), >(SC_LN011-6144), >(SC_LN012-6144), >(SC_LN013-6144), >(SC_LN014-6144), >(SC_LN015-6144), >(SC_LN016-6144), >(SC_LN017-6144), >(SC_LN018-6144), >(SC_LN019-6144)
    EQUB >(SC_LN020-6144), >(SC_LN021-6144), >(SC_LN022-6144), >(SC_LN023-6144), >(SC_LN024-6144), >(SC_LN025-6144), >(SC_LN026-6144), >(SC_LN027-6144), >(SC_LN028-6144), >(SC_LN029-6144)
    EQUB >(SC_LN030-6144), >(SC_LN031-6144), >(SC_LN032-6144), >(SC_LN033-6144), >(SC_LN034-6144), >(SC_LN035-6144), >(SC_LN036-6144), >(SC_LN037-6144), >(SC_LN038-6144), >(SC_LN039-6144)
    EQUB >(SC_LN040-6144), >(SC_LN041-6144), >(SC_LN042-6144), >(SC_LN043-6144), >(SC_LN044-6144), >(SC_LN045-6144), >(SC_LN046-6144), >(SC_LN047-6144), >(SC_LN048-6144), >(SC_LN049-6144)
    EQUB >(SC_LN050-6144), >(SC_LN051-6144), >(SC_LN052-6144), >(SC_LN053-6144), >(SC_LN054-6144), >(SC_LN055-6144), >(SC_LN056-6144), >(SC_LN057-6144), >(SC_LN058-6144), >(SC_LN059-6144)
    EQUB >(SC_LN060-6144), >(SC_LN061-6144), >(SC_LN062-6144), >(SC_LN063-6144), >(SC_LN064-6144), >(SC_LN065-6144), >(SC_LN066-6144), >(SC_LN067-6144), >(SC_LN068-6144), >(SC_LN069-6144)
    EQUB >(SC_LN070-6144), >(SC_LN071-6144), >(SC_LN072-6144), >(SC_LN073-6144), >(SC_LN074-6144), >(SC_LN075-6144), >(SC_LN076-6144), >(SC_LN077-6144), >(SC_LN078-6144), >(SC_LN079-6144)
    EQUB >(SC_LN080-6144), >(SC_LN081-6144), >(SC_LN082-6144), >(SC_LN083-6144), >(SC_LN084-6144), >(SC_LN085-6144), >(SC_LN086-6144), >(SC_LN087-6144), >(SC_LN088-6144), >(SC_LN089-6144)
    EQUB >(SC_LN090-6144), >(SC_LN091-6144), >(SC_LN092-6144), >(SC_LN093-6144), >(SC_LN094-6144), >(SC_LN095-6144), >(SC_LN096-6144), >(SC_LN097-6144), >(SC_LN098-6144), >(SC_LN099-6144)
    EQUB >(SC_LN100-6144), >(SC_LN101-6144), >(SC_LN102-6144), >(SC_LN103-6144), >(SC_LN104-6144), >(SC_LN105-6144), >(SC_LN106-6144), >(SC_LN107-6144), >(SC_LN108-6144), >(SC_LN109-6144)
    EQUB >(SC_LN110-6144), >(SC_LN111-6144), >(SC_LN112-6144), >(SC_LN113-6144), >(SC_LN114-6144), >(SC_LN115-6144), >(SC_LN116-6144), >(SC_LN117-6144), >(SC_LN118-6144), >(SC_LN119-6144)
    EQUB >(SC_LN120-6144), >(SC_LN121-6144), >(SC_LN122-6144), >(SC_LN123-6144), >(SC_LN124-6144), >(SC_LN125-6144), >(SC_LN126-6144), >(SC_LN127-6144), >(SC_LN128-6144), >(SC_LN129-6144)
    EQUB >(SC_LN130-6144), >(SC_LN131-6144), >(SC_LN132-6144), >(SC_LN133-6144), >(SC_LN134-6144), >(SC_LN135-6144), >(SC_LN136-6144), >(SC_LN137-6144), >(SC_LN138-6144), >(SC_LN139-6144)
    EQUB >(SC_LN140-6144), >(SC_LN141-6144), >(SC_LN142-6144), >(SC_LN143-6144), >(SC_LN144-6144), >(SC_LN145-6144), >(SC_LN146-6144), >(SC_LN147-6144), >(SC_LN148-6144), >(SC_LN149-6144)
    EQUB >(SC_LN150-6144), >(SC_LN151-6144), >(SC_LN152-6144), >(SC_LN153-6144), >(SC_LN154-6144), >(SC_LN155-6144), >(SC_LN156-6144), >(SC_LN157-6144), >(SC_LN158-6144), >(SC_LN159-6144)
    EQUB >(SC_LN160-6144), >(SC_LN161-6144), >(SC_LN162-6144), >(SC_LN163-6144), >(SC_LN164-6144), >(SC_LN165-6144), >(SC_LN166-6144), >(SC_LN167-6144), >(SC_LN168-6144), >(SC_LN169-6144)
    EQUB >(SC_LN170-6144), >(SC_LN171-6144), >(SC_LN172-6144), >(SC_LN173-6144), >(SC_LN174-6144), >(SC_LN175-6144), >(SC_LN176-6144), >(SC_LN177-6144), >(SC_LN178-6144), >(SC_LN179-6144)
    EQUB >(SC_LN180-6144), >(SC_LN181-6144), >(SC_LN182-6144), >(SC_LN183-6144), >(SC_LN184-6144), >(SC_LN185-6144), >(SC_LN186-6144), >(SC_LN187-6144), >(SC_LN188-6144), >(SC_LN189-6144)
    EQUB >(SC_LN190-6144), >(SC_LN191-6144);, >(SC_LN192-6144), >(SC_LN193-6144), >(SC_LN194-6144), >(SC_LN195-6144), >(SC_LN196-6144), >(SC_LN197-6144), >(SC_LN198-6144), >(SC_LN199-6144)
;    EQUB >(SC_LN200-6144), >(SC_LN201-6144), >(SC_LN202-6144), >(SC_LN203-6144), >(SC_LN204-6144), >(SC_LN205-6144), >(SC_LN206-6144), >(SC_LN207-6144), >(SC_LN208-6144), >(SC_LN209-6144)
;    EQUB >(SC_LN210-6144), >(SC_LN211-6144), >(SC_LN212-6144), >(SC_LN213-6144), >(SC_LN214-6144), >(SC_LN215-6144), >(SC_LN216-6144), >(SC_LN217-6144), >(SC_LN218-6144), >(SC_LN219-6144)
;    EQUB >(SC_LN220-6144), >(SC_LN221-6144), >(SC_LN222-6144), >(SC_LN223-6144), >(SC_LN224-6144), >(SC_LN225-6144), >(SC_LN226-6144), >(SC_LN227-6144), >(SC_LN228-6144), >(SC_LN229-6144)
;    EQUB >(SC_LN230-6144), >(SC_LN231-6144), >(SC_LN232-6144), >(SC_LN233-6144), >(SC_LN234-6144), >(SC_LN235-6144), >(SC_LN236-6144), >(SC_LN237-6144), >(SC_LN238-6144), >(SC_LN239-6144)
;    EQUB >(SC_LN240-6144), >(SC_LN241-6144), >(SC_LN242-6144), >(SC_LN243-6144), >(SC_LN244-6144), >(SC_LN245-6144), >(SC_LN246-6144), >(SC_LN247-6144), >(SC_LN248-6144), >(SC_LN249-6144)
;    EQUB >(SC_LN250-6144), >(SC_LN251-6144), >(SC_LN252-6144), >(SC_LN253-6144), >(SC_LN254-6144), >(SC_LN255-6144)

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

.SPR_SHIFT_ORA_LO_HI_TAB EQUB >ORA_0_TAB_LO,>ORA_1_TAB_LO,>ORA_2_TAB_LO,>ORA_3_TAB_LO

ALIGN &100
.ORA_0_TAB_LO
 EQUB %00000000,%00000001,%00000010,%00000011,%00000100,%00000101,%00000110,%00000111,%00001000,%00001001,%00001010,%00001011,%00001100,%00001101,%00001110,%00001111
 EQUB %00010000,%00010001,%00010010,%00010011,%00010100,%00010101,%00010110,%00010111,%00011000,%00011001,%00011010,%00011011,%00011100,%00011101,%00011110,%00011111
 EQUB %00100000,%00100001,%00100010,%00100011,%00100100,%00100101,%00100110,%00100111,%00101000,%00101001,%00101010,%00101011,%00101100,%00101101,%00101110,%00101111
 EQUB %00110000,%00110001,%00110010,%00110011,%00110100,%00110101,%00110110,%00110111,%00111000,%00111001,%00111010,%00111011,%00111100,%00111101,%00111110,%00111111
 EQUB %01000000,%01000001,%01000010,%01000011,%01000100,%01000101,%01000110,%01000111,%01001000,%01001001,%01001010,%01001011,%01001100,%01001101,%01001110,%01001111
 EQUB %01010000,%01010001,%01010010,%01010011,%01010100,%01010101,%01010110,%01010111,%01011000,%01011001,%01011010,%01011011,%01011100,%01011101,%01011110,%01011111
 EQUB %01100000,%01100001,%01100010,%01100011,%01100100,%01100101,%01100110,%01100111,%01101000,%01101001,%01101010,%01101011,%01101100,%01101101,%01101110,%01101111
 EQUB %01110000,%01110001,%01110010,%01110011,%01110100,%01110101,%01110110,%01110111,%01111000,%01111001,%01111010,%01111011,%01111100,%01111101,%01111110,%01111111
 EQUB %10000000,%10000001,%10000010,%10000011,%10000100,%10000101,%10000110,%10000111,%10001000,%10001001,%10001010,%10001011,%10001100,%10001101,%10001110,%10001111
 EQUB %10010000,%10010001,%10010010,%10010011,%10010100,%10010101,%10010110,%10010111,%10011000,%10011001,%10011010,%10011011,%10011100,%10011101,%10011110,%10011111
 EQUB %10100000,%10100001,%10100010,%10100011,%10100100,%10100101,%10100110,%10100111,%10101000,%10101001,%10101010,%10101011,%10101100,%10101101,%10101110,%10101111
 EQUB %10110000,%10110001,%10110010,%10110011,%10110100,%10110101,%10110110,%10110111,%10111000,%10111001,%10111010,%10111011,%10111100,%10111101,%10111110,%10111111
 EQUB %11000000,%11000001,%11000010,%11000011,%11000100,%11000101,%11000110,%11000111,%11001000,%11001001,%11001010,%11001011,%11001100,%11001101,%11001110,%11001111
 EQUB %11010000,%11010001,%11010010,%11010011,%11010100,%11010101,%11010110,%11010111,%11011000,%11011001,%11011010,%11011011,%11011100,%11011101,%11011110,%11011111
 EQUB %11100000,%11100001,%11100010,%11100011,%11100100,%11100101,%11100110,%11100111,%11101000,%11101001,%11101010,%11101011,%11101100,%11101101,%11101110,%11101111
 EQUB %11110000,%11110001,%11110010,%11110011,%11110100,%11110101,%11110110,%11110111,%11111000,%11111001,%11111010,%11111011,%11111100,%11111101,%11111110,%11111111
.ORA_0_TAB_HI
 EQUB %00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000
 EQUB %00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000
 EQUB %00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000
 EQUB %00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000
 EQUB %00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000
 EQUB %00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000
 EQUB %00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000
 EQUB %00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000
 EQUB %00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000
 EQUB %00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000
 EQUB %00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000
 EQUB %00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000
 EQUB %00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000
 EQUB %00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000
 EQUB %00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000
 EQUB %00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000
.ORA_1_TAB_LO	
 EQUB %00000000,%00000000,%00000001,%00000001,%00000010,%00000010,%00000011,%00000011,%00000100,%00000100,%00000101,%00000101,%00000110,%00000110,%00000111,%00000111
 EQUB %00000000,%00000000,%00000001,%00000001,%00000010,%00000010,%00000011,%00000011,%00000100,%00000100,%00000101,%00000101,%00000110,%00000110,%00000111,%00000111
 EQUB %00010000,%00010000,%00010001,%00010001,%00010010,%00010010,%00010011,%00010011,%00010100,%00010100,%00010101,%00010101,%00010110,%00010110,%00010111,%00010111
 EQUB %00010000,%00010000,%00010001,%00010001,%00010010,%00010010,%00010011,%00010011,%00010100,%00010100,%00010101,%00010101,%00010110,%00010110,%00010111,%00010111
 EQUB %00100000,%00100000,%00100001,%00100001,%00100010,%00100010,%00100011,%00100011,%00100100,%00100100,%00100101,%00100101,%00100110,%00100110,%00100111,%00100111
 EQUB %00100000,%00100000,%00100001,%00100001,%00100010,%00100010,%00100011,%00100011,%00100100,%00100100,%00100101,%00100101,%00100110,%00100110,%00100111,%00100111
 EQUB %00110000,%00110000,%00110001,%00110001,%00110010,%00110010,%00110011,%00110011,%00110100,%00110100,%00110101,%00110101,%00110110,%00110110,%00110111,%00110111
 EQUB %00110000,%00110000,%00110001,%00110001,%00110010,%00110010,%00110011,%00110011,%00110100,%00110100,%00110101,%00110101,%00110110,%00110110,%00110111,%00110111
 EQUB %01000000,%01000000,%01000001,%01000001,%01000010,%01000010,%01000011,%01000011,%01000100,%01000100,%01000101,%01000101,%01000110,%01000110,%01000111,%01000111
 EQUB %01000000,%01000000,%01000001,%01000001,%01000010,%01000010,%01000011,%01000011,%01000100,%01000100,%01000101,%01000101,%01000110,%01000110,%01000111,%01000111
 EQUB %01010000,%01010000,%01010001,%01010001,%01010010,%01010010,%01010011,%01010011,%01010100,%01010100,%01010101,%01010101,%01010110,%01010110,%01010111,%01010111
 EQUB %01010000,%01010000,%01010001,%01010001,%01010010,%01010010,%01010011,%01010011,%01010100,%01010100,%01010101,%01010101,%01010110,%01010110,%01010111,%01010111
 EQUB %01100000,%01100000,%01100001,%01100001,%01100010,%01100010,%01100011,%01100011,%01100100,%01100100,%01100101,%01100101,%01100110,%01100110,%01100111,%01100111
 EQUB %01100000,%01100000,%01100001,%01100001,%01100010,%01100010,%01100011,%01100011,%01100100,%01100100,%01100101,%01100101,%01100110,%01100110,%01100111,%01100111
 EQUB %01110000,%01110000,%01110001,%01110001,%01110010,%01110010,%01110011,%01110011,%01110100,%01110100,%01110101,%01110101,%01110110,%01110110,%01110111,%01110111
 EQUB %01110000,%01110000,%01110001,%01110001,%01110010,%01110010,%01110011,%01110011,%01110100,%01110100,%01110101,%01110101,%01110110,%01110110,%01110111,%01110111
.ORA_1_TAB_HI		
 EQUB %00000000,%00001000,%00000000,%00001000,%00000000,%00001000,%00000000,%00001000,%00000000,%00001000,%00000000,%00001000,%00000000,%00001000,%00000000,%00001000
 EQUB %10000000,%10001000,%10000000,%10001000,%10000000,%10001000,%10000000,%10001000,%10000000,%10001000,%10000000,%10001000,%10000000,%10001000,%10000000,%10001000
 EQUB %00000000,%00001000,%00000000,%00001000,%00000000,%00001000,%00000000,%00001000,%00000000,%00001000,%00000000,%00001000,%00000000,%00001000,%00000000,%00001000
 EQUB %10000000,%10001000,%10000000,%10001000,%10000000,%10001000,%10000000,%10001000,%10000000,%10001000,%10000000,%10001000,%10000000,%10001000,%10000000,%10001000
 EQUB %00000000,%00001000,%00000000,%00001000,%00000000,%00001000,%00000000,%00001000,%00000000,%00001000,%00000000,%00001000,%00000000,%00001000,%00000000,%00001000
 EQUB %10000000,%10001000,%10000000,%10001000,%10000000,%10001000,%10000000,%10001000,%10000000,%10001000,%10000000,%10001000,%10000000,%10001000,%10000000,%10001000
 EQUB %00000000,%00001000,%00000000,%00001000,%00000000,%00001000,%00000000,%00001000,%00000000,%00001000,%00000000,%00001000,%00000000,%00001000,%00000000,%00001000
 EQUB %10000000,%10001000,%10000000,%10001000,%10000000,%10001000,%10000000,%10001000,%10000000,%10001000,%10000000,%10001000,%10000000,%10001000,%10000000,%10001000
 EQUB %00000000,%00001000,%00000000,%00001000,%00000000,%00001000,%00000000,%00001000,%00000000,%00001000,%00000000,%00001000,%00000000,%00001000,%00000000,%00001000
 EQUB %10000000,%10001000,%10000000,%10001000,%10000000,%10001000,%10000000,%10001000,%10000000,%10001000,%10000000,%10001000,%10000000,%10001000,%10000000,%10001000
 EQUB %00000000,%00001000,%00000000,%00001000,%00000000,%00001000,%00000000,%00001000,%00000000,%00001000,%00000000,%00001000,%00000000,%00001000,%00000000,%00001000
 EQUB %10000000,%10001000,%10000000,%10001000,%10000000,%10001000,%10000000,%10001000,%10000000,%10001000,%10000000,%10001000,%10000000,%10001000,%10000000,%10001000
 EQUB %00000000,%00001000,%00000000,%00001000,%00000000,%00001000,%00000000,%00001000,%00000000,%00001000,%00000000,%00001000,%00000000,%00001000,%00000000,%00001000
 EQUB %10000000,%10001000,%10000000,%10001000,%10000000,%10001000,%10000000,%10001000,%10000000,%10001000,%10000000,%10001000,%10000000,%10001000,%10000000,%10001000
 EQUB %00000000,%00001000,%00000000,%00001000,%00000000,%00001000,%00000000,%00001000,%00000000,%00001000,%00000000,%00001000,%00000000,%00001000,%00000000,%00001000
 EQUB %10000000,%10001000,%10000000,%10001000,%10000000,%10001000,%10000000,%10001000,%10000000,%10001000,%10000000,%10001000,%10000000,%10001000,%10000000,%10001000
.ORA_2_TAB_LO	
 EQUB %00000000,%00000000,%00000000,%00000000,%00000001,%00000001,%00000001,%00000001,%00000010,%00000010,%00000010,%00000010,%00000011,%00000011,%00000011,%00000011
 EQUB %00000000,%00000000,%00000000,%00000000,%00000001,%00000001,%00000001,%00000001,%00000010,%00000010,%00000010,%00000010,%00000011,%00000011,%00000011,%00000011
 EQUB %00000000,%00000000,%00000000,%00000000,%00000001,%00000001,%00000001,%00000001,%00000010,%00000010,%00000010,%00000010,%00000011,%00000011,%00000011,%00000011
 EQUB %00000000,%00000000,%00000000,%00000000,%00000001,%00000001,%00000001,%00000001,%00000010,%00000010,%00000010,%00000010,%00000011,%00000011,%00000011,%00000011
 EQUB %00010000,%00010000,%00010000,%00010000,%00010001,%00010001,%00010001,%00010001,%00010010,%00010010,%00010010,%00010010,%00010011,%00010011,%00010011,%00010011
 EQUB %00010000,%00010000,%00010000,%00010000,%00010001,%00010001,%00010001,%00010001,%00010010,%00010010,%00010010,%00010010,%00010011,%00010011,%00010011,%00010011
 EQUB %00010000,%00010000,%00010000,%00010000,%00010001,%00010001,%00010001,%00010001,%00010010,%00010010,%00010010,%00010010,%00010011,%00010011,%00010011,%00010011
 EQUB %00010000,%00010000,%00010000,%00010000,%00010001,%00010001,%00010001,%00010001,%00010010,%00010010,%00010010,%00010010,%00010011,%00010011,%00010011,%00010011
 EQUB %00100000,%00100000,%00100000,%00100000,%00100001,%00100001,%00100001,%00100001,%00100010,%00100010,%00100010,%00100010,%00100011,%00100011,%00100011,%00100011
 EQUB %00100000,%00100000,%00100000,%00100000,%00100001,%00100001,%00100001,%00100001,%00100010,%00100010,%00100010,%00100010,%00100011,%00100011,%00100011,%00100011
 EQUB %00100000,%00100000,%00100000,%00100000,%00100001,%00100001,%00100001,%00100001,%00100010,%00100010,%00100010,%00100010,%00100011,%00100011,%00100011,%00100011
 EQUB %00100000,%00100000,%00100000,%00100000,%00100001,%00100001,%00100001,%00100001,%00100010,%00100010,%00100010,%00100010,%00100011,%00100011,%00100011,%00100011
 EQUB %00110000,%00110000,%00110000,%00110000,%00110001,%00110001,%00110001,%00110001,%00110010,%00110010,%00110010,%00110010,%00110011,%00110011,%00110011,%00110011
 EQUB %00110000,%00110000,%00110000,%00110000,%00110001,%00110001,%00110001,%00110001,%00110010,%00110010,%00110010,%00110010,%00110011,%00110011,%00110011,%00110011
 EQUB %00110000,%00110000,%00110000,%00110000,%00110001,%00110001,%00110001,%00110001,%00110010,%00110010,%00110010,%00110010,%00110011,%00110011,%00110011,%00110011
 EQUB %00110000,%00110000,%00110000,%00110000,%00110001,%00110001,%00110001,%00110001,%00110010,%00110010,%00110010,%00110010,%00110011,%00110011,%00110011,%00110011
.ORA_2_TAB_HI		
 EQUB %00000000,%00000100,%00001000,%00001100,%00000000,%00000100,%00001000,%00001100,%00000000,%00000100,%00001000,%00001100,%00000000,%00000100,%00001000,%00001100
 EQUB %01000000,%01000100,%01001000,%01001100,%01000000,%01000100,%01001000,%01001100,%01000000,%01000100,%01001000,%01001100,%01000000,%01000100,%01001000,%01001100
 EQUB %10000000,%10000100,%10001000,%10001100,%10000000,%10000100,%10001000,%10001100,%10000000,%10000100,%10001000,%10001100,%10000000,%10000100,%10001000,%10001100
 EQUB %11000000,%11000100,%11001000,%11001100,%11000000,%11000100,%11001000,%11001100,%11000000,%11000100,%11001000,%11001100,%11000000,%11000100,%11001000,%11001100
 EQUB %00000000,%00000100,%00001000,%00001100,%00000000,%00000100,%00001000,%00001100,%00000000,%00000100,%00001000,%00001100,%00000000,%00000100,%00001000,%00001100
 EQUB %01000000,%01000100,%01001000,%01001100,%01000000,%01000100,%01001000,%01001100,%01000000,%01000100,%01001000,%01001100,%01000000,%01000100,%01001000,%01001100
 EQUB %10000000,%10000100,%10001000,%10001100,%10000000,%10000100,%10001000,%10001100,%10000000,%10000100,%10001000,%10001100,%10000000,%10000100,%10001000,%10001100
 EQUB %11000000,%11000100,%11001000,%11001100,%11000000,%11000100,%11001000,%11001100,%11000000,%11000100,%11001000,%11001100,%11000000,%11000100,%11001000,%11001100
 EQUB %00000000,%00000100,%00001000,%00001100,%00000000,%00000100,%00001000,%00001100,%00000000,%00000100,%00001000,%00001100,%00000000,%00000100,%00001000,%00001100
 EQUB %01000000,%01000100,%01001000,%01001100,%01000000,%01000100,%01001000,%01001100,%01000000,%01000100,%01001000,%01001100,%01000000,%01000100,%01001000,%01001100
 EQUB %10000000,%10000100,%10001000,%10001100,%10000000,%10000100,%10001000,%10001100,%10000000,%10000100,%10001000,%10001100,%10000000,%10000100,%10001000,%10001100
 EQUB %11000000,%11000100,%11001000,%11001100,%11000000,%11000100,%11001000,%11001100,%11000000,%11000100,%11001000,%11001100,%11000000,%11000100,%11001000,%11001100
 EQUB %00000000,%00000100,%00001000,%00001100,%00000000,%00000100,%00001000,%00001100,%00000000,%00000100,%00001000,%00001100,%00000000,%00000100,%00001000,%00001100
 EQUB %01000000,%01000100,%01001000,%01001100,%01000000,%01000100,%01001000,%01001100,%01000000,%01000100,%01001000,%01001100,%01000000,%01000100,%01001000,%01001100
 EQUB %10000000,%10000100,%10001000,%10001100,%10000000,%10000100,%10001000,%10001100,%10000000,%10000100,%10001000,%10001100,%10000000,%10000100,%10001000,%10001100
 EQUB %11000000,%11000100,%11001000,%11001100,%11000000,%11000100,%11001000,%11001100,%11000000,%11000100,%11001000,%11001100,%11000000,%11000100,%11001000,%11001100
.ORA_3_TAB_LO	
 EQUB %00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000001,%00000001,%00000001,%00000001,%00000001,%00000001,%00000001,%00000001
 EQUB %00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000001,%00000001,%00000001,%00000001,%00000001,%00000001,%00000001,%00000001
 EQUB %00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000001,%00000001,%00000001,%00000001,%00000001,%00000001,%00000001,%00000001
 EQUB %00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000001,%00000001,%00000001,%00000001,%00000001,%00000001,%00000001,%00000001
 EQUB %00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000001,%00000001,%00000001,%00000001,%00000001,%00000001,%00000001,%00000001
 EQUB %00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000001,%00000001,%00000001,%00000001,%00000001,%00000001,%00000001,%00000001
 EQUB %00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000001,%00000001,%00000001,%00000001,%00000001,%00000001,%00000001,%00000001
 EQUB %00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000001,%00000001,%00000001,%00000001,%00000001,%00000001,%00000001,%00000001
 EQUB %00010000,%00010000,%00010000,%00010000,%00010000,%00010000,%00010000,%00010000,%00010001,%00010001,%00010001,%00010001,%00010001,%00010001,%00010001,%00010001
 EQUB %00010000,%00010000,%00010000,%00010000,%00010000,%00010000,%00010000,%00010000,%00010001,%00010001,%00010001,%00010001,%00010001,%00010001,%00010001,%00010001
 EQUB %00010000,%00010000,%00010000,%00010000,%00010000,%00010000,%00010000,%00010000,%00010001,%00010001,%00010001,%00010001,%00010001,%00010001,%00010001,%00010001
 EQUB %00010000,%00010000,%00010000,%00010000,%00010000,%00010000,%00010000,%00010000,%00010001,%00010001,%00010001,%00010001,%00010001,%00010001,%00010001,%00010001
 EQUB %00010000,%00010000,%00010000,%00010000,%00010000,%00010000,%00010000,%00010000,%00010001,%00010001,%00010001,%00010001,%00010001,%00010001,%00010001,%00010001
 EQUB %00010000,%00010000,%00010000,%00010000,%00010000,%00010000,%00010000,%00010000,%00010001,%00010001,%00010001,%00010001,%00010001,%00010001,%00010001,%00010001
 EQUB %00010000,%00010000,%00010000,%00010000,%00010000,%00010000,%00010000,%00010000,%00010001,%00010001,%00010001,%00010001,%00010001,%00010001,%00010001,%00010001
 EQUB %00010000,%00010000,%00010000,%00010000,%00010000,%00010000,%00010000,%00010000,%00010001,%00010001,%00010001,%00010001,%00010001,%00010001,%00010001,%00010001
.ORA_3_TAB_HI
 EQUB %00000000,%00000010,%00000100,%00000110,%00001000,%00001010,%00001100,%00001110,%00000000,%00000010,%00000100,%00000110,%00001000,%00001010,%00001100,%00001110
 EQUB %00100000,%00100010,%00100100,%00100110,%00101000,%00101010,%00101100,%00101110,%00100000,%00100010,%00100100,%00100110,%00101000,%00101010,%00101100,%00101110
 EQUB %01000000,%01000010,%01000100,%01000110,%01001000,%01001010,%01001100,%01001110,%01000000,%01000010,%01000100,%01000110,%01001000,%01001010,%01001100,%01001110
 EQUB %01100000,%01100010,%01100100,%01100110,%01101000,%01101010,%01101100,%01101110,%01100000,%01100010,%01100100,%01100110,%01101000,%01101010,%01101100,%01101110
 EQUB %10000000,%10000010,%10000100,%10000110,%10001000,%10001010,%10001100,%10001110,%10000000,%10000010,%10000100,%10000110,%10001000,%10001010,%10001100,%10001110
 EQUB %10100000,%10100010,%10100100,%10100110,%10101000,%10101010,%10101100,%10101110,%10100000,%10100010,%10100100,%10100110,%10101000,%10101010,%10101100,%10101110
 EQUB %11000000,%11000010,%11000100,%11000110,%11001000,%11001010,%11001100,%11001110,%11000000,%11000010,%11000100,%11000110,%11001000,%11001010,%11001100,%11001110
 EQUB %11100000,%11100010,%11100100,%11100110,%11101000,%11101010,%11101100,%11101110,%11100000,%11100010,%11100100,%11100110,%11101000,%11101010,%11101100,%11101110
 EQUB %00000000,%00000010,%00000100,%00000110,%00001000,%00001010,%00001100,%00001110,%00000000,%00000010,%00000100,%00000110,%00001000,%00001010,%00001100,%00001110
 EQUB %00100000,%00100010,%00100100,%00100110,%00101000,%00101010,%00101100,%00101110,%00100000,%00100010,%00100100,%00100110,%00101000,%00101010,%00101100,%00101110
 EQUB %01000000,%01000010,%01000100,%01000110,%01001000,%01001010,%01001100,%01001110,%01000000,%01000010,%01000100,%01000110,%01001000,%01001010,%01001100,%01001110
 EQUB %01100000,%01100010,%01100100,%01100110,%01101000,%01101010,%01101100,%01101110,%01100000,%01100010,%01100100,%01100110,%01101000,%01101010,%01101100,%01101110
 EQUB %10000000,%10000010,%10000100,%10000110,%10001000,%10001010,%10001100,%10001110,%10000000,%10000010,%10000100,%10000110,%10001000,%10001010,%10001100,%10001110
 EQUB %10100000,%10100010,%10100100,%10100110,%10101000,%10101010,%10101100,%10101110,%10100000,%10100010,%10100100,%10100110,%10101000,%10101010,%10101100,%10101110
 EQUB %11000000,%11000010,%11000100,%11000110,%11001000,%11001010,%11001100,%11001110,%11000000,%11000010,%11000100,%11000110,%11001000,%11001010,%11001100,%11001110
 EQUB %11100000,%11100010,%11100100,%11100110,%11101000,%11101010,%11101100,%11101110,%11100000,%11100010,%11100100,%11100110,%11101000,%11101010,%11101100,%11101110

.AND_TAB
 EQUB %11111111,%11101110,%11011101,%11001100,%10111011,%10101010,%10011001,%10001000,%01110111,%01100110,%01010101,%01000100,%00110011,%00100010,%00010001,%00000000
 EQUB %11101110,%11101110,%11001100,%11001100,%10101010,%10101010,%10001000,%10001000,%01100110,%01100110,%01000100,%01000100,%00100010,%00100010,%00000000,%00000000
 EQUB %11011101,%11001100,%11011101,%11001100,%10011001,%10001000,%10011001,%10001000,%01010101,%01000100,%01010101,%01000100,%00010001,%00000000,%00010001,%00000000
 EQUB %11001100,%11001100,%11001100,%11001100,%10001000,%10001000,%10001000,%10001000,%01000100,%01000100,%01000100,%01000100,%00000000,%00000000,%00000000,%00000000
 EQUB %10111011,%10101010,%10011001,%10001000,%10111011,%10101010,%10011001,%10001000,%00110011,%00100010,%00010001,%00000000,%00110011,%00100010,%00010001,%00000000
 EQUB %10101010,%10101010,%10001000,%10001000,%10101010,%10101010,%10001000,%10001000,%00100010,%00100010,%00000000,%00000000,%00100010,%00100010,%00000000,%00000000
 EQUB %10011001,%10001000,%10011001,%10001000,%10011001,%10001000,%10011001,%10001000,%00010001,%00000000,%00010001,%00000000,%00010001,%00000000,%00010001,%00000000
 EQUB %10001000,%10001000,%10001000,%10001000,%10001000,%10001000,%10001000,%10001000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000
 EQUB %01110111,%01100110,%01010101,%01000100,%00110011,%00100010,%00010001,%00000000,%01110111,%01100110,%01010101,%01000100,%00110011,%00100010,%00010001,%00000000
 EQUB %01100110,%01100110,%01000100,%01000100,%00100010,%00100010,%00000000,%00000000,%01100110,%01100110,%01000100,%01000100,%00100010,%00100010,%00000000,%00000000
 EQUB %01010101,%01000100,%01010101,%01000100,%00010001,%00000000,%00010001,%00000000,%01010101,%01000100,%01010101,%01000100,%00010001,%00000000,%00010001,%00000000
 EQUB %01000100,%01000100,%01000100,%01000100,%00000000,%00000000,%00000000,%00000000,%01000100,%01000100,%01000100,%01000100,%00000000,%00000000,%00000000,%00000000
 EQUB %00110011,%00100010,%00010001,%00000000,%00110011,%00100010,%00010001,%00000000,%00110011,%00100010,%00010001,%00000000,%00110011,%00100010,%00010001,%00000000
 EQUB %00100010,%00100010,%00000000,%00000000,%00100010,%00100010,%00000000,%00000000,%00100010,%00100010,%00000000,%00000000,%00100010,%00100010,%00000000,%00000000
 EQUB %00010001,%00000000,%00010001,%00000000,%00010001,%00000000,%00010001,%00000000,%00010001,%00000000,%00010001,%00000000,%00010001,%00000000,%00010001,%00000000
 EQUB %00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000
; EQUB 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
; EQUB 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
; EQUB 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
; EQUB 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
; EQUB 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
; EQUB 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
; EQUB 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
; EQUB 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
; EQUB 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
; EQUB 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
; EQUB 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
; EQUB 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
; EQUB 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
; EQUB 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
; EQUB 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
; EQUB 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
; equb 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
; equb 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
; equb 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
; equb 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
; equb 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
; equb 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
; equb 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
; equb 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
; equb 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
; equb 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
; equb 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
; equb 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
; equb 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
; equb 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
; equb 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
; equb 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 
; EQUB %11111111,%11111100,%11111100,%11111100,%11110011,%11110000,%11110000,%11110000,%11110011,%11110000,%11110000,%11110000,%11110011,%11110000,%11110000,%11110000
; EQUB %11001111,%11001100,%11001100,%11001100,%11000011,%11000000,%11000000,%11000000,%11000011,%11000000,%11000000,%11000000,%11000011,%11000000,%11000000,%11000000
; EQUB %11001111,%11001100,%11001100,%11001100,%11000011,%11000000,%11000000,%11000000,%11000011,%11000000,%11000000,%11000000,%11000011,%11000000,%11000000,%11000000
; EQUB %11001111,%11001100,%11001100,%11001100,%11000011,%11000000,%11000000,%11000000,%11000011,%11000000,%11000000,%11000000,%11000011,%11000000,%11000000,%11000000
; EQUB %00111111,%00111100,%00111100,%00111100,%00110011,%00110000,%00110000,%00110000,%00110011,%00110000,%00110000,%00110000,%00110000,%00110000,%00110000,%00110000
; EQUB %00001111,%00001100,%00001100,%00001100,%00000011,%00000000,%00000000,%00000000,%00000011,%00000000,%00000000,%00000000,%00000011,%00000000,%00000000,%00000000
; EQUB %00001111,%00011100,%00001100,%00001100,%00000011,%00000000,%00000000,%00000000,%00000011,%00000000,%00000000,%00000000,%00000011,%00000000,%00000000,%00000000
; EQUB %00001111,%00001100,%00001100,%00001100,%00000011,%00000000,%00000000,%00000000,%00000011,%00000000,%00000000,%00000000,%00000011,%00000000,%00000000,%00000000
; EQUB %00111111,%00111100,%00111100,%00111100,%00110011,%00110000,%00110000,%00110000,%00110011,%00110000,%00110000,%00110000,%00110011,%00110000,%00110000,%00110000
; EQUB %00001111,%00001100,%00001100,%00001100,%00000011,%00000000,%00000000,%00000000,%00000011,%00000000,%00000000,%00000000,%00000011,%00000000,%00000000,%00000000
; EQUB %00001111,%00001100,%00001100,%00001100,%00000011,%00000000,%00000000,%00000000,%00000011,%00000000,%00000000,%00000000,%00000011,%00000000,%00000000,%00000000
; EQUB %00001111,%00001100,%00001100,%00001100,%00000011,%00000000,%00000000,%00000000,%00000011,%00000000,%00000000,%00000000,%00000011,%00000000,%00000000,%00000000
; EQUB %00111111,%00111100,%00111100,%00111100,%00110011,%00110000,%00110000,%00110000,%00110011,%00110000,%00110000,%00110000,%00110011,%00110000,%00110000,%00110000
; EQUB %00001111,%00001100,%00001100,%00001100,%00000011,%00000000,%00000000,%00000000,%00000011,%00000000,%00000000,%00000000,%00000011,%00000000,%00000000,%00000000
; EQUB %00001111,%00001100,%00001100,%00001100,%00000011,%00000000,%00000000,%00000000,%00000011,%00000000,%00000000,%00000000,%00000011,%00000000,%00000000,%00000000 
; EQUB %00001111,%00001100,%00001100,%00001100,%00000011,%00000000,%00000000,%00000000,%00000011,%00000000,%00000000,%00000000,%00000011,%00000000,%00000000,%00000000

.SPR_TAB_LO EQUB <SPR_000_00,<SPR_001_00,<SPR_002_00
.SPR_TAB_HI EQUB >SPR_000_00,>SPR_001_00,>SPR_002_00

ALIGN &100
.SPR_000_00
 EQUB %11111111
 EQUB %10001000
 EQUB %10001000
 EQUB %10001000
 EQUB %10001000
 EQUB %10001000
 EQUB %10001000
 EQUB %10001000
 EQUB %10001000
 EQUB %10001000
 EQUB %10001000
 EQUB %10001000
 EQUB %10001000
 EQUB %10001000
 EQUB %10001000
 EQUB %11111111

 EQUB %11111111
 EQUB %00010001
 EQUB %00010001
 EQUB %00010001
 EQUB %00010001
 EQUB %00010001
 EQUB %00010001
 EQUB %00010001
 EQUB %00010001
 EQUB %00010001
 EQUB %00010001
 EQUB %00010001
 EQUB %00010001
 EQUB %00010001
 EQUB %00010001
 EQUB %11111111

.SPR_001_00
 EQUB %11110000
 EQUB %10000000
 EQUB %10000000
 EQUB %10000000
 EQUB %10000000
 EQUB %10000000
 EQUB %10000000
 EQUB %10000000
 EQUB %10000000
 EQUB %10000000
 EQUB %10000000
 EQUB %10000000
 EQUB %10000000
 EQUB %10000000
 EQUB %10000000
 EQUB %11110000

 EQUB %11110000
 EQUB %00010000
 EQUB %00010000
 EQUB %00010000
 EQUB %00010000
 EQUB %00010000
 EQUB %00010000
 EQUB %00010000
 EQUB %00010000
 EQUB %00010000
 EQUB %00010000
 EQUB %00010000
 EQUB %00010000
 EQUB %00010000
 EQUB %00010000
 EQUB %11110000

.SPR_002_00
 EQUB %00001111
 EQUB %00001000
 EQUB %00001000
 EQUB %00001000
 EQUB %00001000
 EQUB %00001000
 EQUB %00001000
 EQUB %00001000
 EQUB %00001000
 EQUB %00001000
 EQUB %00001000
 EQUB %00001000
 EQUB %00001000
 EQUB %00001000
 EQUB %00001000
 EQUB %00001111

 EQUB %00001111
 EQUB %00000001
 EQUB %00000001
 EQUB %00000001
 EQUB %00000001
 EQUB %00000001
 EQUB %00000001
 EQUB %00000001
 EQUB %00000001
 EQUB %00000001
 EQUB %00000001
 EQUB %00000001
 EQUB %00000001
 EQUB %00000001
 EQUB %00000001
 EQUB %00001111

.SCREEN_SETUP

	LDA #0 ; $65 ; 0 ;$65 ; colour 01 (0000????) and colour 10 (????0000) - bank 1
	LDX #<SCR
	LDY #>SCR
	STX SCN+0
	STY SCN+1
	LDX #>6144
	LDY #<6144
	JSR MEMSET

	LDA #%11010111;$AA ;55 ; $65 ; 0 ;$65 ; colour 01 (0000????) and colour 10 (????0000) - bank 1
	LDX #<BB
	LDY #>BB
	STX SCN+0
	STY SCN+1
	LDX #>6144
	LDY #<6144
	JSR MEMSET

	RTS

.MEMSET
{			STY	LSB_ONLY+1 ; store LSB count
			CPX	#0          ; MSB?     
			BEQ	LSB_ONLY   ; no

			LDY	#0          ; yes so reset LSB
.MSB_LOOP  
.LSB_LOOP   		STA	(SCN),Y   ; clear whole MSB
					DEY 
					BNE	LSB_LOOP

				INC	SCN+1      ; inc MSB
				DEX               ; dec MSB count
				BNE MSB_LOOP

.LSB_ONLY  	LDY	#0          ; LSB count 
            BEQ MS_END     ; not needed

.LAST_LSB_LOOP	STA (SCN),Y
				DEY 
				BNE LAST_LSB_LOOP
                
			STA (SCN),Y     ; clear last Y (0)
 
.MS_END		RTS
}


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
