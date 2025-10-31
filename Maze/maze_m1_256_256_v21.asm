
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

logicalScreenStart                  = 16384 ; $5800 ; $3000 ; $5800 ; $3000     ;

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

; zero page memory locations
.SCN		SKIP 2
.SCN_ZP		SKIP 2
.MEM_TO		SKIP 2
.REGA_BUF	SKIP 1
.REGX_BUF	SKIP 1
.REGY_BUF	SKIP 1
.REGA_INT	SKIP 1
.REGX_INT	SKIP 1
.REGY_INT	SKIP 1
.MAP_POS 	SKIP 1
.TMP		SKIP 1
.H_PLAYER			SKIP 1
.H_PLAYER_BACKUP	SKIP 1
.V_PLAYER			SKIP 1
.V_PLAYER_BACKUP	SKIP 1
.SCN_LFT_TOP 		SKIP 2
.SCN_LFT_BOT		SKIP 2

MEM_FROM = SCN
SCN_TOP = MEM_FROM
SCN_BOT = MEM_TO

SCN_RGT_TOP = SCN_TOP
SCN_RGT_BOT = SCN_BOT

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

    LDX #$ff
    TXS                                     ; Reset stack


;	LDA #%00001111 ; %11000011 ;$65 ; colour 01 (0000????) and colour 10 (????0000) - bank 1
;	LDX #<logicalScreenStart ; SCRN0
;	LDY #>logicalScreenStart ; SCRN0
;	STX MEM_TO+0
;	STY MEM_TO+1
;	LDX #>16384 
;	LDY #<16384
;	JSR MEMSET
	SEI									    ; install irq code
	JSR SETUP
	CLI

.MLOOP JMP MLOOP

;.MEMSET        
;{
;			STY	LSB_ONLY+1 ; store LSB count
;            CPX	#0          ; MSB?     
;            BEQ	LSB_ONLY   ; no
;
;            LDY	#0          ; yes so reset LSB
;.MSB_LOOP  
;.LSB_LOOP   	STA	(MEM_TO),Y   ; clear whole MSB
;				DEY
;				BNE	LSB_LOOP
;
;			INC	MEM_TO+1      ; inc MSB
;			DEX             ; dec MSB count
;			BNE	MSB_LOOP
;
;.LSB_ONLY	LDY	#0          ; LSB count 
;			BEQ	MS_END     ; not needed
;
;.LAST_LSB_LOOP 	STA	(MEM_TO),Y
;				DEY
;				BNE	LAST_LSB_LOOP
;                
;			STA	(MEM_TO),Y     ; clear last Y (0)
; 
;.MS_END   	RTS
;}

ALIGN &100
.DP_L_10_1100
	LDA	SCR_LFT_HI,Y		;4
	STA SCN_TOP+1				;3 7
	LDA SCR_LFT_HI+1,Y		;4 11
	STA SCN_BOT+1			;3 14
	LDA SCR_LO,Y			;4 18
	STA SCN_TOP			;3 21
	LDA SCR_LO+1,Y			;4 25
	STA SCN_BOT			;3 28

	LDA #%11000000			;4 41
	BNE	DP_L_10_0011_CONT

.DP_L_10_0110
	LDA	SCR_LFT_HI,Y		;4
	STA SCN_TOP+1				;3 7
	LDA SCR_LFT_HI+1,Y		;4 11
	STA SCN_BOT+1			;3 14
	LDA SCR_LO,Y			;4 18
	STA SCN_TOP			;3 21
	LDA SCR_LO+1,Y			;4 25
	STA SCN_BOT			;3 28

	LDA #%01100000			;4 41
	BNE DP_L_10_0011_CONT

.DP_L_10_0011
	LDA	SCR_LFT_HI,Y		;4
	STA SCN_TOP+1				;3 7
	LDA SCR_LFT_HI+1,Y		;4 11
	STA SCN_BOT+1			;3 14
	LDA SCR_LO,Y			;4 18
	STA SCN_TOP			;3 21
	LDA SCR_LO+1,Y			;4 25
	STA SCN_BOT			;3 28

	LDA #%00110000			;4 41
.DP_L_10_0011_CONT
	LDY SCR_HOR,X				;4 32
	TAX
	ORA (SCN_TOP),Y		;5 37
	STA (SCN_TOP),Y		;5 46
	TXA
	ORA (SCN_BOT),Y		;5 37
	STA (SCN_BOT),Y		;5 51
	RTS

.DP_L_10_1001
	LDA	SCR_LFT_HI,Y		;4
	STA SCN_TOP+1				;3 7
	LDA SCR_LFT_HI+1,Y		;4 11
	STA SCN_BOT+1			;3 14
	LDA SCR_LO,Y			;4 18
	STA SCN_TOP			;3 21
	LDA SCR_LO+1,Y			;4 25
	STA SCN_BOT			;3 28

	LDA #%10000000			;4 41
	LDY SCR_HOR+4,X				;4 32
	ORA (SCN_TOP),Y		;5 37
	STA (SCN_TOP),Y		;5 46
	LDA #%00001000			;4 41
	ORA (SCN_BOT),Y		;5 37
	STA (SCN_BOT),Y		;5 51

	LDA #%00010000			;4 41
	BNE	DP_L_10_0011_CONT

.DP_R_10_1100
	LDA	SCR_RGT_HI,Y		;4
	STA SCN_TOP+1				;3 7
	LDA SCR_RGT_HI+1,Y		;4 11
	STA SCN_BOT+1			;3 14
	LDA SCR_LO,Y			;4 18
	STA SCN_TOP			;3 21
	LDA SCR_LO+1,Y			;4 25
	STA SCN_BOT			;3 28

	LDA #%11000000			;4 41
	BNE	DP_R_10_0011_CONT

.DP_R_10_0110
	LDA	SCR_RGT_HI,Y		;4
	STA SCN_TOP+1				;3 7
	LDA SCR_RGT_HI+1,Y		;4 11
	STA SCN_BOT+1			;3 14
	LDA SCR_LO,Y			;4 18
	STA SCN_TOP			;3 21
	LDA SCR_LO+1,Y			;4 25
	STA SCN_BOT			;3 28

	LDA #%01100000			;4 41
	BNE	DP_R_10_0011_CONT

.DP_R_10_0011
	LDA	SCR_RGT_HI,Y		;4
	STA SCN_TOP+1				;3 7
	LDA SCR_RGT_HI+1,Y		;4 11
	STA SCN_BOT+1			;3 14
	LDA SCR_LO,Y			;4 18
	STA SCN_TOP			;3 21
	LDA SCR_LO+1,Y			;4 25
	STA SCN_BOT			;3 28

	LDA #%00110000			;4 41
.DP_R_10_0011_CONT
	LDY SCR_HOR,X				;4 32
	TAX
	ORA (SCN_TOP),Y		;5 37
	STA (SCN_TOP),Y		;5 46
	TXA
	ORA (SCN_BOT),Y		;5 37
	STA (SCN_BOT),Y		;5 51
	RTS

.DP_R_10_1001
	LDA	SCR_RGT_HI,Y		;4
	STA SCN_TOP+1				;3 7
	LDA SCR_RGT_HI+1,Y		;4 11
	STA SCN_BOT+1			;3 14
	LDA SCR_LO,Y			;4 18
	STA SCN_TOP			;3 21
	LDA SCR_LO+1,Y			;4 25
	STA SCN_BOT			;3 28

	LDA #%10000000			;4 41
	LDY SCR_HOR+4,X				;4 32
	ORA (SCN_TOP),Y		;5 37
	STA (SCN_TOP),Y		;5 46
	LDA #%00001000			;4 41
	ORA (SCN_BOT),Y		;5 37
	STA (SCN_BOT),Y		;5 51

	LDA #%00010000			;4 41
	BNE	DP_R_10_0011_CONT

.DP_LR_10_1001
	LDA	SCR_LFT_HI,Y		;4
	STA SCN_LFT_TOP+1				;3 7
	LDA SCR_LFT_HI+1,Y		;4 11
	STA SCN_LFT_BOT+1			;3 14
	LDA SCR_LO,Y			;4 18
	STA SCN_LFT_TOP			;3 21
	STA SCN_RGT_TOP			;3 21
	LDA SCR_LO+1,Y			;4 25
	STA SCN_LFT_BOT			;3 28
	STA SCN_RGT_BOT			;3 28

	LDA	SCR_RGT_HI,Y		;4
	STA SCN_RGT_TOP+1				;3 7
	LDA SCR_RGT_HI+1,Y		;4 11
	STA SCN_RGT_BOT+1			;3 14

	LDY SCR_HOR,X				;4 32
	LDA #%00010000			;4 41
	ORA (SCN_LFT_TOP),Y		;5 37
	STA (SCN_LFT_TOP),Y		;5 46
	LDA #%00010000			;4 41
	ORA (SCN_LFT_BOT),Y		;5 37
	STA (SCN_LFT_BOT),Y		;5 51

	LDY SCR_HOR+4,X				;4 32
	LDA #%10000000			;4 41
	ORA (SCN_RGT_TOP),Y		;5 37
	STA (SCN_RGT_TOP),Y		;5 46
	LDA #%10000000			;4 41
	ORA (SCN_RGT_BOT),Y		;5 37
	STA (SCN_RGT_BOT),Y		;5 51

	RTS

.PLOT01
{	
		BMI	RIGHT
.LEFT	LDA SCR_LFT_HI,Y 
		STA	SCN_ZP+1
		TYA
		AND #7
		ORA	SCR_HOR,X
		TAY
		LDA	(SCN_ZP),Y
		ORA	SCR_OR_01,X
		STA	(SCN_ZP),Y 
		RTS
.RIGHT	LDA SCR_RGT_HI,Y 
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
		BMI	RIGHT
.LEFT	LDA SCR_LFT_HI,Y 
		STA	SCN_ZP+1
		TYA
		AND #7
		ORA	SCR_HOR,X
		TAY
		LDA	(SCN_ZP),Y
		ORA	SCR_OR_10,X
		STA	(SCN_ZP),Y 
		RTS
.RIGHT	LDA SCR_RGT_HI,Y 
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
		BMI	RIGHT
.LEFT	LDA SCR_LFT_HI,Y 
		STA	SCN_ZP+1
		TYA
		AND #7
		ORA	SCR_HOR,X
		TAY
		LDA	(SCN_ZP),Y
		ORA	SCR_OR_11,X
		STA	(SCN_ZP),Y 
		RTS
.RIGHT	LDA SCR_RGT_HI,Y 
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
		BMI	RIGHT
.LEFT	LDA SCR_LFT_HI,Y 
		STA	SCN_ZP+1
		TYA
		AND #7
		ORA	SCR_HOR,X
		TAY
		LDA	(SCN_ZP),Y
		AND	SCR_AND_00,X
		STA	(SCN_ZP),Y 
		RTS
.RIGHT	LDA SCR_RGT_HI,Y 
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

ALIGN &100
.DP_L_01_1100
	LDA	SCR_LFT_HI,Y		;4
	STA SCN_TOP+1				;3 7
	LDA SCR_LFT_HI+1,Y		;4 11
	STA SCN_BOT+1			;3 14
	LDA SCR_LO,Y			;4 18
	STA SCN_TOP			;3 21
	LDA SCR_LO+1,Y			;4 25
	STA SCN_BOT			;3 28

	LDA #%00001100			;4 41
	BNE	DP_L_01_0011_CONT

.DP_L_01_0110
	LDA	SCR_LFT_HI,Y		;4
	STA SCN_TOP+1				;3 7
	LDA SCR_LFT_HI+1,Y		;4 11
	STA SCN_BOT+1			;3 14
	LDA SCR_LO,Y			;4 18
	STA SCN_TOP			;3 21
	LDA SCR_LO+1,Y			;4 25
	STA SCN_BOT			;3 28

	LDA #%00000110			;4 41
	BNE DP_L_01_0011_CONT

.DP_L_01_0011
	LDA	SCR_LFT_HI,Y		;4
	STA SCN_TOP+1				;3 7
	LDA SCR_LFT_HI+1,Y		;4 11
	STA SCN_BOT+1			;3 14
	LDA SCR_LO,Y			;4 18
	STA SCN_TOP			;3 21
	LDA SCR_LO+1,Y			;4 25
	STA SCN_BOT			;3 28

	LDA #%00000011			;4 41
.DP_L_01_0011_CONT
	LDY SCR_HOR,X				;4 32
	TAX
	ORA (SCN_TOP),Y		;5 37
	STA (SCN_TOP),Y		;5 46
	TXA
	ORA (SCN_BOT),Y		;5 37
	STA (SCN_BOT),Y		;5 51
	RTS

.DP_L_01_1001
	LDA	SCR_LFT_HI,Y		;4
	STA SCN_TOP+1				;3 7
	LDA SCR_LFT_HI+1,Y		;4 11
	STA SCN_BOT+1			;3 14
	LDA SCR_LO,Y			;4 18
	STA SCN_TOP			;3 21
	LDA SCR_LO+1,Y			;4 25
	STA SCN_BOT			;3 28

	LDA #%00001000			;4 41
	LDY SCR_HOR+4,X				;4 32
	ORA (SCN_TOP),Y		;5 37
	STA (SCN_TOP),Y		;5 46
	LDA #%10000000			;4 41
	ORA (SCN_BOT),Y		;5 37
	STA (SCN_BOT),Y		;5 51

	LDA #%00000001			;4 41
	BNE	DP_L_01_0011_CONT

.DP_R_01_1100
	LDA	SCR_RGT_HI,Y		;4
	STA SCN_TOP+1				;3 7
	LDA SCR_RGT_HI+1,Y		;4 11
	STA SCN_BOT+1			;3 14
	LDA SCR_LO,Y			;4 18
	STA SCN_TOP			;3 21
	LDA SCR_LO+1,Y			;4 25
	STA SCN_BOT			;3 28

	LDA #%00001100			;4 41
	BNE	DP_R_01_0011_CONT

.DP_R_01_0110
	LDA	SCR_RGT_HI,Y		;4
	STA SCN_TOP+1				;3 7
	LDA SCR_RGT_HI+1,Y		;4 11
	STA SCN_BOT+1			;3 14
	LDA SCR_LO,Y			;4 18
	STA SCN_TOP			;3 21
	LDA SCR_LO+1,Y			;4 25
	STA SCN_BOT			;3 28

	LDA #%00000110			;4 41
	BNE	DP_R_01_0011_CONT

.DP_R_01_0011
	LDA	SCR_RGT_HI,Y		;4
	STA SCN_TOP+1				;3 7
	LDA SCR_RGT_HI+1,Y		;4 11
	STA SCN_BOT+1			;3 14
	LDA SCR_LO,Y			;4 18
	STA SCN_TOP			;3 21
	LDA SCR_LO+1,Y			;4 25
	STA SCN_BOT			;3 28

	LDA #%00000011			;4 41
.DP_R_01_0011_CONT
	LDY SCR_HOR,X				;4 32
	TAX
	ORA (SCN_TOP),Y		;5 37
	STA (SCN_TOP),Y		;5 46
	TXA
	ORA (SCN_BOT),Y		;5 37
	STA (SCN_BOT),Y		;5 51
	RTS

.DP_R_01_1001
	LDA	SCR_RGT_HI,Y		;4
	STA SCN_TOP+1				;3 7
	LDA SCR_RGT_HI+1,Y		;4 11
	STA SCN_BOT+1			;3 14
	LDA SCR_LO,Y			;4 18
	STA SCN_TOP			;3 21
	LDA SCR_LO+1,Y			;4 25
	STA SCN_BOT			;3 28

	LDA #%00001000			;4 41
	LDY SCR_HOR+4,X				;4 32
	ORA (SCN_TOP),Y		;5 37
	STA (SCN_TOP),Y		;5 46
	LDA #%00001000			;4 41
	ORA (SCN_BOT),Y		;5 37
	STA (SCN_BOT),Y		;5 51

	LDA #%00000001			;4 41
	BNE	DP_R_01_0011_CONT

.DP_LR_01_1001
	LDA	SCR_LFT_HI,Y		;4
	STA SCN_LFT_TOP+1				;3 7
	LDA SCR_LFT_HI+1,Y		;4 11
	STA SCN_LFT_BOT+1			;3 14
	LDA SCR_LO,Y			;4 18
	STA SCN_LFT_TOP			;3 21
	STA SCN_RGT_TOP			;3 21
	LDA SCR_LO+1,Y			;4 25
	STA SCN_LFT_BOT			;3 28
	STA SCN_RGT_BOT			;3 28

	LDA	SCR_RGT_HI,Y		;4
	STA SCN_RGT_TOP+1				;3 7
	LDA SCR_RGT_HI+1,Y		;4 11
	STA SCN_RGT_BOT+1			;3 14

	LDY SCR_HOR,X				;4 32
	LDA #%00000001			;4 41
	ORA (SCN_LFT_TOP),Y		;5 37
	STA (SCN_LFT_TOP),Y		;5 46
	LDA #%00000001			;4 41
	ORA (SCN_LFT_BOT),Y		;5 37
	STA (SCN_LFT_BOT),Y		;5 51

	LDY SCR_HOR+4,X				;4 32
	LDA #%00001000			;4 41
	ORA (SCN_RGT_TOP),Y		;5 37
	STA (SCN_RGT_TOP),Y		;5 46
	LDA #%00001000			;4 41
	ORA (SCN_RGT_BOT),Y		;5 37
	STA (SCN_RGT_BOT),Y		;5 51

	RTS

.DC_00
	LDA DL_TAB_01_LO,X
{
	STA	JA +1
.JA	JMP DC_L_00_1100
}
.DP_01
	LDA DL_TAB_01_LO,X
{
	STA	JA +1
.JA	JMP DP_L_01_1100
}
.DP_10
	LDA DL_TAB_01_LO,X
{
	STA	JA +1
.JA	JMP DP_L_10_1100
}
.DP_11
	LDA DL_TAB_01_LO,X
{
	STA	JA +1
.JA	JMP DP_L_11_1100
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

.CONVERT_MOVEMENT
		TXA
{
		BEQ	EXIT
		BMI	NEG
.POS		LDA #1
			BNE EXIT ; JMP
.NEG		LDA #2
.EXIT	TAX
}
		TYA
{
		BEQ	EXIT
		BMI	NEG
.POS		TXA
			ORA #4
			RTS
.NEG		TXA
			ORA #8
			RTS
.EXIT	TXA
}
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

ALIGN &100
.DP_L_11_1100
	LDA	SCR_LFT_HI,Y		;4
	STA SCN_TOP+1				;3 7
	LDA SCR_LFT_HI+1,Y		;4 11
	STA SCN_BOT+1			;3 14
	LDA SCR_LO,Y			;4 18
	STA SCN_TOP			;3 21
	LDA SCR_LO+1,Y			;4 25
	STA SCN_BOT			;3 28

	LDA #%11001100			;4 41
	BNE	DP_L_11_0011_CONT

.DP_L_11_0110
	LDA	SCR_LFT_HI,Y		;4
	STA SCN_TOP+1				;3 7
	LDA SCR_LFT_HI+1,Y		;4 11
	STA SCN_BOT+1			;3 14
	LDA SCR_LO,Y			;4 18
	STA SCN_TOP			;3 21
	LDA SCR_LO+1,Y			;4 25
	STA SCN_BOT			;3 28

	LDA #%01100110			;4 41
	BNE DP_L_11_0011_CONT

.DP_L_11_0011
	LDA	SCR_LFT_HI,Y		;4
	STA SCN_TOP+1				;3 7
	LDA SCR_LFT_HI+1,Y		;4 11
	STA SCN_BOT+1			;3 14
	LDA SCR_LO,Y			;4 18
	STA SCN_TOP			;3 21
	LDA SCR_LO+1,Y			;4 25
	STA SCN_BOT			;3 28

	LDA #%00110011			;4 41
.DP_L_11_0011_CONT
	LDY SCR_HOR,X				;4 32
	TAX
	ORA (SCN_TOP),Y		;5 37
	STA (SCN_TOP),Y		;5 46
	TXA
	ORA (SCN_BOT),Y		;5 37
	STA (SCN_BOT),Y		;5 51
	RTS

.DP_L_11_1001
	LDA	SCR_LFT_HI,Y		;4
	STA SCN_TOP+1				;3 7
	LDA SCR_LFT_HI+1,Y		;4 11
	STA SCN_BOT+1			;3 14
	LDA SCR_LO,Y			;4 18
	STA SCN_TOP			;3 21
	LDA SCR_LO+1,Y			;4 25
	STA SCN_BOT			;3 28

	LDA #%10001000			;4 41
	LDY SCR_HOR+4,X				;4 32
	ORA (SCN_TOP),Y		;5 37
	STA (SCN_TOP),Y		;5 46
	LDA #%10001000			;4 41
	ORA (SCN_BOT),Y		;5 37
	STA (SCN_BOT),Y		;5 51

	LDA #%00010001			;4 41
	BNE	DP_L_11_0011_CONT

.DP_R_11_1100
	LDA	SCR_RGT_HI,Y		;4
	STA SCN_TOP+1				;3 7
	LDA SCR_RGT_HI+1,Y		;4 11
	STA SCN_BOT+1			;3 14
	LDA SCR_LO,Y			;4 18
	STA SCN_TOP			;3 21
	LDA SCR_LO+1,Y			;4 25
	STA SCN_BOT			;3 28

	LDA #%11001100			;4 41
	BNE	DP_R_11_0011_CONT

.DP_R_11_0110
	LDA	SCR_RGT_HI,Y		;4
	STA SCN_TOP+1				;3 7
	LDA SCR_RGT_HI+1,Y		;4 11
	STA SCN_BOT+1			;3 14
	LDA SCR_LO,Y			;4 18
	STA SCN_TOP			;3 21
	LDA SCR_LO+1,Y			;4 25
	STA SCN_BOT			;3 28

	LDA #%01100110			;4 41
	BNE	DP_R_11_0011_CONT

.DP_R_11_0011
	LDA	SCR_RGT_HI,Y		;4
	STA SCN_TOP+1				;3 7
	LDA SCR_RGT_HI+1,Y		;4 11
	STA SCN_BOT+1			;3 14
	LDA SCR_LO,Y			;4 18
	STA SCN_TOP			;3 21
	LDA SCR_LO+1,Y			;4 25
	STA SCN_BOT			;3 28

	LDA #%00110011			;4 41
.DP_R_11_0011_CONT
	LDY SCR_HOR,X				;4 32
	TAX
	ORA (SCN_TOP),Y		;5 37
	STA (SCN_TOP),Y		;5 46
	TXA
	ORA (SCN_BOT),Y		;5 37
	STA (SCN_BOT),Y		;5 51
	RTS

.DP_R_11_1001
	LDA	SCR_RGT_HI,Y		;4
	STA SCN_TOP+1				;3 7
	LDA SCR_RGT_HI+1,Y		;4 11
	STA SCN_BOT+1			;3 14
	LDA SCR_LO,Y			;4 18
	STA SCN_TOP			;3 21
	LDA SCR_LO+1,Y			;4 25
	STA SCN_BOT			;3 28

	LDA #%10001000			;4 41
	LDY SCR_HOR+4,X				;4 32
	ORA (SCN_TOP),Y		;5 37
	STA (SCN_TOP),Y		;5 46
	LDA #%10001000			;4 41
	ORA (SCN_BOT),Y		;5 37
	STA (SCN_BOT),Y		;5 51

	LDA #%00010001			;4 41
	BNE	DP_R_11_0011_CONT

.DP_LR_11_1001
	LDA	SCR_LFT_HI,Y		;4
	STA SCN_LFT_TOP+1				;3 7
	LDA SCR_LFT_HI+1,Y		;4 11
	STA SCN_LFT_BOT+1			;3 14
	LDA SCR_LO,Y			;4 18
	STA SCN_LFT_TOP			;3 21
	STA SCN_RGT_TOP			;3 21
	LDA SCR_LO+1,Y			;4 25
	STA SCN_LFT_BOT			;3 28
	STA SCN_RGT_BOT			;3 28

	LDA	SCR_RGT_HI,Y		;4
	STA SCN_RGT_TOP+1				;3 7
	LDA SCR_RGT_HI+1,Y		;4 11
	STA SCN_RGT_BOT+1			;3 14

	LDY SCR_HOR,X				;4 32
	LDA #%00010001			;4 41
	ORA (SCN_LFT_TOP),Y		;5 37
	STA (SCN_LFT_TOP),Y		;5 46
	LDA #%00010001			;4 41
	ORA (SCN_LFT_BOT),Y		;5 37
	STA (SCN_LFT_BOT),Y		;5 51

	LDY SCR_HOR+4,X				;4 32
	LDA #%10001000			;4 41
	ORA (SCN_RGT_TOP),Y		;5 37
	STA (SCN_RGT_TOP),Y		;5 46
	LDA #%10001000			;4 41
	ORA (SCN_RGT_BOT),Y		;5 37
	STA (SCN_RGT_BOT),Y		;5 51

	RTS

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
 
;				LDY #10
;				LDX #10
;				JSR PLOT10

;				LDY #20
;				LDX #20
;				JSR PLOT01
				
				LDY #30
				LDX #30
				JSR PLOT11				
 
 				
		JSR READKEYS			; get directions in x/y
		JSR	CONVERT_MOVEMENT	; convert directions to bitmap
		TAX
	stx REGX_BUF	
		LDA H_PLAYER
		STA H_PLAYER_BACKUP
		LDY V_PLAYER
		STY V_PLAYER_BACKUP
		JSR MOVEMENT			; convert bits set to movement

 LDA REGX_BUF
 LDX #0
 LDY #42
 JSR HEX8

	JSR DRAW_11
 
	LDX #0
	LDY #BLACK
	JSR SetLogicalColour

							LDY REGY_INT					; Restore X and Y registers
							LDX REGX_INT
.handledInterrupt			LDA REGA_INT
;							lda interruptAccumulator_fc
							rti

.DRAW_11
{
		LDX H_PLAYER_BACKUP
		LDY V_PLAYER_BACKUP
		CPY V_PLAYER
		BNE DRAW
		CPX H_PLAYER
		BEQ	SAME
.DRAW		TXA
			JSR	DC_00
			LDY V_PLAYER
 			LDX H_PLAYER
			JSR	DP_11
.SAME	RTS
}

ALIGN &100
.DC_L_00_1100
	LDA	SCR_LFT_HI,Y		;4
	STA SCN_TOP+1				;3 7
	LDA SCR_LFT_HI+1,Y		;4 11
	STA SCN_BOT+1			;3 14
	LDA SCR_LO,Y			;4 18
	STA SCN_TOP			;3 21
	LDA SCR_LO+1,Y			;4 25
	STA SCN_BOT			;3 28

	LDA #%00110011			;4 41
	BNE	DC_L_00_0011_CONT

.DC_L_00_0110
	LDA	SCR_LFT_HI,Y		;4
	STA SCN_TOP+1				;3 7
	LDA SCR_LFT_HI+1,Y		;4 11
	STA SCN_BOT+1			;3 14
	LDA SCR_LO,Y			;4 18
	STA SCN_TOP			;3 21
	LDA SCR_LO+1,Y			;4 25
	STA SCN_BOT			;3 28

	LDA #%10011001			;4 41
	BNE DC_L_00_0011_CONT

.DC_L_00_0011
	LDA	SCR_LFT_HI,Y		;4
	STA SCN_TOP+1				;3 7
	LDA SCR_LFT_HI+1,Y		;4 11
	STA SCN_BOT+1			;3 14
	LDA SCR_LO,Y			;4 18
	STA SCN_TOP			;3 21
	LDA SCR_LO+1,Y			;4 25
	STA SCN_BOT			;3 28

	LDA #%11001100			;4 41
.DC_L_00_0011_CONT
	LDY SCR_HOR,X				;4 32
	TAX
	AND (SCN_TOP),Y		;5 37
	STA (SCN_TOP),Y		;5 46
	TXA
	AND (SCN_BOT),Y		;5 37
	STA (SCN_BOT),Y		;5 51
	RTS

.DC_L_00_1001
	LDA	SCR_LFT_HI,Y		;4
	STA SCN_TOP+1				;3 7
	LDA SCR_LFT_HI+1,Y		;4 11
	STA SCN_BOT+1			;3 14
	LDA SCR_LO,Y			;4 18
	STA SCN_TOP			;3 21
	LDA SCR_LO+1,Y			;4 25
	STA SCN_BOT			;3 28

	LDA #%01110111			;4 41
	LDY SCR_HOR+4,X				;4 32
	AND (SCN_TOP),Y		;5 37
	STA (SCN_TOP),Y		;5 46
	LDA #%01110111			;4 41
	AND (SCN_BOT),Y		;5 37
	STA (SCN_BOT),Y		;5 51

	LDA #%11101110			;4 41
	BNE	DC_L_00_0011_CONT

.DC_R_00_1100
	LDA	SCR_RGT_HI,Y		;4
	STA SCN_TOP+1				;3 7
	LDA SCR_RGT_HI+1,Y		;4 11
	STA SCN_BOT+1			;3 14
	LDA SCR_LO,Y			;4 18
	STA SCN_TOP			;3 21
	LDA SCR_LO+1,Y			;4 25
	STA SCN_BOT			;3 28

	LDA #%00110011			;4 41
	BNE	DC_R_00_0011_CONT

.DC_R_00_0110
	LDA	SCR_RGT_HI,Y		;4
	STA SCN_TOP+1				;3 7
	LDA SCR_RGT_HI+1,Y		;4 11
	STA SCN_BOT+1			;3 14
	LDA SCR_LO,Y			;4 18
	STA SCN_TOP			;3 21
	LDA SCR_LO+1,Y			;4 25
	STA SCN_BOT			;3 28

	LDA #%10011001			;4 41
	BNE	DC_R_00_0011_CONT

.DC_R_00_0011
	LDA	SCR_RGT_HI,Y		;4
	STA SCN_TOP+1				;3 7
	LDA SCR_RGT_HI+1,Y		;4 11
	STA SCN_BOT+1			;3 14
	LDA SCR_LO,Y			;4 18
	STA SCN_TOP			;3 21
	LDA SCR_LO+1,Y			;4 25
	STA SCN_BOT			;3 28

	LDA #%11001100			;4 41
.DC_R_00_0011_CONT
	LDY SCR_HOR,X				;4 32
	TAX
	AND (SCN_TOP),Y		;5 37
	STA (SCN_TOP),Y		;5 46
	TXA
	AND (SCN_BOT),Y		;5 37
	STA (SCN_BOT),Y		;5 51
	RTS

.DC_R_00_1001
	LDA	SCR_RGT_HI,Y		;4
	STA SCN_TOP+1				;3 7
	LDA SCR_RGT_HI+1,Y		;4 11
	STA SCN_BOT+1			;3 14
	LDA SCR_LO,Y			;4 18
	STA SCN_TOP			;3 21
	LDA SCR_LO+1,Y			;4 25
	STA SCN_BOT			;3 28

	LDA #%01110111			;4 41
	LDY SCR_HOR+4,X				;4 32
	AND (SCN_TOP),Y		;5 37
	STA (SCN_TOP),Y		;5 46
	LDA #%01110111			;4 41
	AND (SCN_BOT),Y		;5 37
	STA (SCN_BOT),Y		;5 51

	LDA #%11101110			;4 41
	BNE	DC_R_00_0011_CONT

.DC_LR_00_1001
	LDA	SCR_LFT_HI,Y		;4
	STA SCN_LFT_TOP+1				;3 7
	LDA SCR_LFT_HI+1,Y		;4 11
	STA SCN_LFT_BOT+1			;3 14
	LDA SCR_LO,Y			;4 18
	STA SCN_LFT_TOP			;3 21
	STA SCN_RGT_TOP			;3 21
	LDA SCR_LO+1,Y			;4 25
	STA SCN_LFT_BOT			;3 28
	STA SCN_RGT_BOT			;3 28

	LDA	SCR_RGT_HI,Y		;4
	STA SCN_RGT_TOP+1				;3 7
	LDA SCR_RGT_HI+1,Y		;4 11
	STA SCN_RGT_BOT+1			;3 14

	LDY SCR_HOR,X				;4 32
	LDA #%11101110			;4 41
	AND (SCN_LFT_TOP),Y		;5 37
	STA (SCN_LFT_TOP),Y		;5 46
	LDA #%11101110			;4 41
	AND (SCN_LFT_BOT),Y		;5 37
	STA (SCN_LFT_BOT),Y		;5 51

	LDY SCR_HOR+4,X				;4 32
	LDA #%01110111			;4 41
	AND (SCN_RGT_TOP),Y		;5 37
	STA (SCN_RGT_TOP),Y		;5 46
	LDA #%01110111			;4 41
	AND (SCN_RGT_BOT),Y		;5 37
	STA (SCN_RGT_BOT),Y		;5 51

	RTS

.DEBUG_WRITE

 LDA MAP_POS
 LDX #0
 LDY #0
 JSR HEX8

 LDA V_PLAYER
 LDX #0
 LDY #6
 JSR HEX8

 LDA H_PLAYER
 LDX #0
 LDY #12
 JSR HEX8

 LDA #$44
 LDX #64
 LDY #64
 JSR HEX8

 LDA #$77
 LDX #88
 LDY #80
 JSR HEX8

 LDA #$74
 LDX #64
 LDY #80
 JSR HEX8

 LDA #$47
 LDX #88
 LDY #64
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

      LDA   SCR_LFT_HI+0,Y
      STA   HEX_NUMBER0+2
      LDA   SCR_LFT_HI+1,Y 
      STA   HEX_NUMBER1+2
      LDA   SCR_LFT_HI+2,Y 
      STA   HEX_NUMBER2+2
      LDA   SCR_LFT_HI+3,Y 
      STA   HEX_NUMBER3+2
      LDA   SCR_LFT_HI+4,Y 
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

MACRO COLLIDE_11
{	
		BMI	RIGHT
.LEFT	LDA SCR_LFT_HI,Y 
		STA	SCN_ZP+1
		TYA
		AND #7
		ORA	SCR_HOR,X
		TAY
		LDA	(SCN_ZP),Y
		AND	SCR_OR_11,X
		JMP EXIT
.RIGHT	LDA SCR_RGT_HI,Y 
		STA	SCN_ZP+1
		TYA
		AND #7
		ORA	SCR_HOR,X
		TAY
		LDA	(SCN_ZP),Y
		AND	SCR_OR_11,X
.EXIT
}
ENDMACRO

ALIGN &100
.MOVEMENT	; pass in X/Y pos in A/Y and X holds direction and do movement and collision and returns position ix X/Y
		STA REGA_BUF
		LDA DIR_TAB_LO,X
{
		STA ADDR+1
		LDX REGA_BUF
.ADDR	JMP	DIR_DN
}
.DIR_UP
{
		LDY	V_PLAYER
		DEY
		LDX	H_PLAYER
		JSR	COLLIDE11
		BNE	EXIT
		LDY	V_PLAYER
		DEY
		LDX	H_PLAYER
		INX
		JSR	COLLIDE11
		BNE	EXIT
			DEC	V_PLAYER
.EXIT	RTS	
}
.DIR_ER	RTS
.DIR_DN
{
		LDY	V_PLAYER
		INY
		INY
		LDX	H_PLAYER
;		COLLIDE_11 ; 
		JSR	COLLIDE11
		BNE	EXIT
		LDY	V_PLAYER
		INY
		INY
		LDX	H_PLAYER
		INX
;		COLLIDE_11 ; 
		JSR	COLLIDE11
		BNE	EXIT
			INC	V_PLAYER
.EXIT	RTS	
}
.DIR_RT
{
		LDY	V_PLAYER
		LDX	H_PLAYER
		INX
		INX
;		COLLIDE_11 ; 
		JSR	COLLIDE11
		BNE	EXIT
		LDY	V_PLAYER
		INY
		LDX	H_PLAYER
		INX
		INX
;		COLLIDE_11 ; 
		JSR	COLLIDE11
		BNE	EXIT
			INC	H_PLAYER
.EXIT	RTS	
}
.DIR_LT
{
		LDY	V_PLAYER
		LDX	H_PLAYER
		DEX
;		COLLIDE_11 ; 
		JSR	COLLIDE11
		BNE	EXIT
		LDY	V_PLAYER
		INY
		LDX	H_PLAYER
		DEX
;		COLLIDE_11 ; 
		JSR	COLLIDE11
		BNE	EXIT
			DEC	H_PLAYER
.EXIT	RTS	
}
.DIR_UL
{
		LDY	V_PLAYER
		DEY
		LDX	H_PLAYER
		JSR	COLLIDE11
		BNE	EXIT

		LDY	V_PLAYER
		DEY
		LDX	H_PLAYER
		DEX
		JSR	COLLIDE11
		BNE	EXIT

		LDY	V_PLAYER
		LDX	H_PLAYER
		DEX
		JSR	COLLIDE11
		BNE	EXIT

			DEC	V_PLAYER
			DEC H_PLAYER
.EXIT	RTS	
}

; xx
; xxy
;  yy

.DIR_DR
{
		LDY	V_PLAYER
		INY
		LDX	H_PLAYER
		INX
		INX
		JSR	COLLIDE11
		BNE	EXIT

		LDY	V_PLAYER
		INY
		INY
		LDX	H_PLAYER
		INX
		JSR	COLLIDE11
		BNE	EXIT

		LDY	V_PLAYER
		INY
		INY
		LDX	H_PLAYER
		INX
		INX
		JSR	COLLIDE11
		BNE	EXIT

			INC	V_PLAYER
			INC H_PLAYER
.EXIT	RTS	
}

;  xx
; yxx
; yy

.DIR_DL
{
		LDY	V_PLAYER
		INY
		LDX	H_PLAYER
		DEX
		JSR	COLLIDE11
		BNE	EXIT

		LDY	V_PLAYER
		INY
		INY
		LDX	H_PLAYER
		JSR	COLLIDE11
		BNE	EXIT

		LDY	V_PLAYER
		INY
		INY
		LDX	H_PLAYER
		DEX
		JSR	COLLIDE11
		BNE	EXIT

			INC	V_PLAYER
			DEC H_PLAYER
.EXIT	RTS	
}

;  yy
; xxy
; xx

.DIR_UR
{
		LDY	V_PLAYER
		DEY
		LDX	H_PLAYER
		INX
		JSR	COLLIDE11
		BNE	EXIT

		LDY	V_PLAYER
		LDX	H_PLAYER
		INX
		INX
		JSR	COLLIDE11
		BNE	EXIT

		LDY	V_PLAYER
		DEY
		LDX	H_PLAYER
		INX
		INX
		JSR	COLLIDE11
		BNE	EXIT

			DEC	V_PLAYER
			INC H_PLAYER
.EXIT	RTS	
}

; ldy vplayer
; iny
; ldx hplayer
; dey
; jsr collide
; bne exit

;.COL_UR
;.COL_UL
;.COL_DR
;.COL_DL
	RTS

.COLLIDE11
{	
		BMI	RIGHT
.LEFT	LDA SCR_LFT_HI,Y 
		STA	SCN_ZP+1
		TYA
		AND #7
		ORA	SCR_HOR,X
		TAY
		LDA	(SCN_ZP),Y
		AND	SCR_OR_11,X
		RTS
.RIGHT	LDA SCR_RGT_HI,Y 
		STA	SCN_ZP+1
		TYA
		AND #7
		ORA	SCR_HOR,X
		TAY
		LDA	(SCN_ZP),Y
		AND	SCR_OR_11,X
		RTS
}

.HEX0  EQUB %01110111,%00100010,%01110111,%01110111,%01010101,%01110111,%01110111,%01110111,%01110111,%01110111,%00100010,%01100110,%01110111,%01100110,%01110111,%01110111 
.HEX1  EQUB %01010101,%01100110,%00010001,%00010001,%01010101,%01000100,%01000100,%00010001,%01010101,%01010101,%01010101,%01010101,%01000100,%01010101,%01000100,%01000100 
.HEX2  EQUB %01010101,%00100010,%01110111,%01110111,%01110111,%01110111,%01110111,%00010001,%01110111,%01110111,%01110111,%01100110,%01000100,%01010101,%01110111,%01110111
.HEX3  EQUB %01010101,%00100010,%01000100,%00010001,%00010001,%00010001,%01010101,%00010001,%01010101,%00010001,%01010101,%01010101,%01000100,%01010101,%01000100,%01000100
.HEX4  EQUB %01110111,%01110111,%01110111,%01110111,%00010001,%01110111,%01110111,%00010001,%01110111,%00010001,%01010101,%01100110,%01110111,%01100110,%01110111,%01000100

.DIR_TAB_LO	EQUB 	<DIR_ER	; 0000
			EQUB 	<DIR_RT	; 0001
			EQUB 	<DIR_LT	; 0010
			EQUB 	<DIR_ER	; 0011
			EQUB 	<DIR_DN	; 0100
			EQUB	<DIR_DR	; 0101
			EQUB	<DIR_DL	; 0110
			EQUB	<DIR_ER	; 0111
			EQUB	<DIR_UP	; 1000
			EQUB	<DIR_UR	; 1001
			EQUB	<DIR_UL	; 1010
			EQUB	<DIR_ER	; 1011
			EQUB	<DIR_ER	; 1100
			EQUB	<DIR_ER	; 1101
			EQUB	<DIR_ER	; 1110
			EQUB	<DIR_ER	; 1111

.SETUP
	LDX #0
	STX SCN_ZP

	INIT_VIA
	INIT_SCREEN
	SET_SCREEN
	SET_COLOURS
	SET_INT
	RTS

ALIGN &100
.SCR_LFT_HI
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

.SCR_RGT_HI
    EQUB >(SC_LN000+256), >(SC_LN001+256), >(SC_LN002+256), >(SC_LN003+256), >(SC_LN004+256), >(SC_LN005+256), >(SC_LN006+256), >(SC_LN007+256), >(SC_LN008+256), >(SC_LN009+256)
    EQUB >(SC_LN010+256), >(SC_LN011+256), >(SC_LN012+256), >(SC_LN013+256), >(SC_LN014+256), >(SC_LN015+256), >(SC_LN016+256), >(SC_LN017+256), >(SC_LN018+256), >(SC_LN019+256)
    EQUB >(SC_LN020+256), >(SC_LN021+256), >(SC_LN022+256), >(SC_LN023+256), >(SC_LN024+256), >(SC_LN025+256), >(SC_LN026+256), >(SC_LN027+256), >(SC_LN028+256), >(SC_LN029+256)
    EQUB >(SC_LN030+256), >(SC_LN031+256), >(SC_LN032+256), >(SC_LN033+256), >(SC_LN034+256), >(SC_LN035+256), >(SC_LN036+256), >(SC_LN037+256), >(SC_LN038+256), >(SC_LN039+256)
    EQUB >(SC_LN040+256), >(SC_LN041+256), >(SC_LN042+256), >(SC_LN043+256), >(SC_LN044+256), >(SC_LN045+256), >(SC_LN046+256), >(SC_LN047+256), >(SC_LN048+256), >(SC_LN049+256)
    EQUB >(SC_LN050+256), >(SC_LN051+256), >(SC_LN052+256), >(SC_LN053+256), >(SC_LN054+256), >(SC_LN055+256), >(SC_LN056+256), >(SC_LN057+256), >(SC_LN058+256), >(SC_LN059+256)
    EQUB >(SC_LN060+256), >(SC_LN061+256), >(SC_LN062+256), >(SC_LN063+256), >(SC_LN064+256), >(SC_LN065+256), >(SC_LN066+256), >(SC_LN067+256), >(SC_LN068+256), >(SC_LN069+256)
    EQUB >(SC_LN070+256), >(SC_LN071+256), >(SC_LN072+256), >(SC_LN073+256), >(SC_LN074+256), >(SC_LN075+256), >(SC_LN076+256), >(SC_LN077+256), >(SC_LN078+256), >(SC_LN079+256)
    EQUB >(SC_LN080+256), >(SC_LN081+256), >(SC_LN082+256), >(SC_LN083+256), >(SC_LN084+256), >(SC_LN085+256), >(SC_LN086+256), >(SC_LN087+256), >(SC_LN088+256), >(SC_LN089+256)
    EQUB >(SC_LN090+256), >(SC_LN091+256), >(SC_LN092+256), >(SC_LN093+256), >(SC_LN094+256), >(SC_LN095+256), >(SC_LN096+256), >(SC_LN097+256), >(SC_LN098+256), >(SC_LN099+256)
    EQUB >(SC_LN100+256), >(SC_LN101+256), >(SC_LN102+256), >(SC_LN103+256), >(SC_LN104+256), >(SC_LN105+256), >(SC_LN106+256), >(SC_LN107+256), >(SC_LN108+256), >(SC_LN109+256)
    EQUB >(SC_LN110+256), >(SC_LN111+256), >(SC_LN112+256), >(SC_LN113+256), >(SC_LN114+256), >(SC_LN115+256), >(SC_LN116+256), >(SC_LN117+256), >(SC_LN118+256), >(SC_LN119+256)
    EQUB >(SC_LN120+256), >(SC_LN121+256), >(SC_LN122+256), >(SC_LN123+256), >(SC_LN124+256), >(SC_LN125+256), >(SC_LN126+256), >(SC_LN127+256), >(SC_LN128+256), >(SC_LN129+256)
    EQUB >(SC_LN130+256), >(SC_LN131+256), >(SC_LN132+256), >(SC_LN133+256), >(SC_LN134+256), >(SC_LN135+256), >(SC_LN136+256), >(SC_LN137+256), >(SC_LN138+256), >(SC_LN139+256)
    EQUB >(SC_LN140+256), >(SC_LN141+256), >(SC_LN142+256), >(SC_LN143+256), >(SC_LN144+256), >(SC_LN145+256), >(SC_LN146+256), >(SC_LN147+256), >(SC_LN148+256), >(SC_LN149+256)
    EQUB >(SC_LN150+256), >(SC_LN151+256), >(SC_LN152+256), >(SC_LN153+256), >(SC_LN154+256), >(SC_LN155+256), >(SC_LN156+256), >(SC_LN157+256), >(SC_LN158+256), >(SC_LN159+256)
    EQUB >(SC_LN160+256), >(SC_LN161+256), >(SC_LN162+256), >(SC_LN163+256), >(SC_LN164+256), >(SC_LN165+256), >(SC_LN166+256), >(SC_LN167+256), >(SC_LN168+256), >(SC_LN169+256)
    EQUB >(SC_LN170+256), >(SC_LN171+256), >(SC_LN172+256), >(SC_LN173+256), >(SC_LN174+256), >(SC_LN175+256), >(SC_LN176+256), >(SC_LN177+256), >(SC_LN178+256), >(SC_LN179+256)
    EQUB >(SC_LN180+256), >(SC_LN181+256), >(SC_LN182+256), >(SC_LN183+256), >(SC_LN184+256), >(SC_LN185+256), >(SC_LN186+256), >(SC_LN187+256), >(SC_LN188+256), >(SC_LN189+256)
    EQUB >(SC_LN190+256), >(SC_LN191+256), >(SC_LN192+256), >(SC_LN193+256), >(SC_LN194+256), >(SC_LN195+256), >(SC_LN196+256), >(SC_LN197+256), >(SC_LN198+256), >(SC_LN199+256)
    EQUB >(SC_LN200+256), >(SC_LN201+256), >(SC_LN202+256), >(SC_LN203+256), >(SC_LN204+256), >(SC_LN205+256), >(SC_LN206+256), >(SC_LN207+256), >(SC_LN208+256), >(SC_LN209+256)
    EQUB >(SC_LN210+256), >(SC_LN211+256), >(SC_LN212+256), >(SC_LN213+256), >(SC_LN214+256), >(SC_LN215+256), >(SC_LN216+256), >(SC_LN217+256), >(SC_LN218+256), >(SC_LN219+256)
    EQUB >(SC_LN220+256), >(SC_LN221+256), >(SC_LN222+256), >(SC_LN223+256), >(SC_LN224+256), >(SC_LN225+256), >(SC_LN226+256), >(SC_LN227+256), >(SC_LN228+256), >(SC_LN229+256)
    EQUB >(SC_LN230+256), >(SC_LN231+256), >(SC_LN232+256), >(SC_LN233+256), >(SC_LN234+256), >(SC_LN235+256), >(SC_LN236+256), >(SC_LN237+256), >(SC_LN238+256), >(SC_LN239+256)
    EQUB >(SC_LN240+256), >(SC_LN241+256), >(SC_LN242+256), >(SC_LN243+256), >(SC_LN244+256), >(SC_LN245+256), >(SC_LN246+256), >(SC_LN247+256), >(SC_LN248+256), >(SC_LN249+256)
    EQUB >(SC_LN250+256), >(SC_LN251+256), >(SC_LN252+256), >(SC_LN253+256), >(SC_LN254+256), >(SC_LN255+256)
	
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

.SCR_HOR
	EQUB 00,00,00,00,08,08,08,08,16,16,16,16,24,24,24,24,32,32,32,32,40,40,40,40,48,48,48,48,56,56,56,56
	EQUB 64,64,64,64,72,72,72,72,80,80,80,80,88,88,88,88,96,96,96,96,104,104,104,104,112,112,112,112,120,120,120,120	
	EQUB 128,128,128,128,136,136,136,136,144,144,144,144,152,152,152,152,160,160,160,160,168,168,168,168,176,176,176,176,184,184,184,184
	EQUB 192,192,192,192,200,200,200,200,208,208,208,208,216,216,216,216,224,224,224,224,232,232,232,232,240,240,240,240,248,248,248,248
	EQUB 00,00,00,00,08,08,08,08,16,16,16,16,24,24,24,24,32,32,32,32,40,40,40,40,48,48,48,48,56,56,56,56
	EQUB 64,64,64,64,72,72,72,72,80,80,80,80,88,88,88,88,96,96,96,96,104,104,104,104,112,112,112,112,120,120,120,120	
	EQUB 128,128,128,128,136,136,136,136,144,144,144,144,152,152,152,152,160,160,160,160,168,168,168,168,176,176,176,176,184,184,184,184
	EQUB 192,192,192,192,200,200,200,200,208,208,208,208,216,216,216,216,224,224,224,224,232,232,232,232,240,240,240,240,248,248,248,248

.DL_TAB_01_LO
	EQUB <DP_L_01_1100,<DP_L_01_0110,<DP_L_01_0011,<DP_L_01_1001,<DP_L_01_1100,<DP_L_01_0110,<DP_L_01_0011,<DP_L_01_1001,<DP_L_01_1100,<DP_L_01_0110,<DP_L_01_0011,<DP_L_01_1001,<DP_L_01_1100,<DP_L_01_0110,<DP_L_01_0011,<DP_L_01_1001
	EQUB <DP_L_01_1100,<DP_L_01_0110,<DP_L_01_0011,<DP_L_01_1001,<DP_L_01_1100,<DP_L_01_0110,<DP_L_01_0011,<DP_L_01_1001,<DP_L_01_1100,<DP_L_01_0110,<DP_L_01_0011,<DP_L_01_1001,<DP_L_01_1100,<DP_L_01_0110,<DP_L_01_0011,<DP_L_01_1001
	EQUB <DP_L_01_1100,<DP_L_01_0110,<DP_L_01_0011,<DP_L_01_1001,<DP_L_01_1100,<DP_L_01_0110,<DP_L_01_0011,<DP_L_01_1001,<DP_L_01_1100,<DP_L_01_0110,<DP_L_01_0011,<DP_L_01_1001,<DP_L_01_1100,<DP_L_01_0110,<DP_L_01_0011,<DP_L_01_1001
	EQUB <DP_L_01_1100,<DP_L_01_0110,<DP_L_01_0011,<DP_L_01_1001,<DP_L_01_1100,<DP_L_01_0110,<DP_L_01_0011,<DP_L_01_1001,<DP_L_01_1100,<DP_L_01_0110,<DP_L_01_0011,<DP_L_01_1001,<DP_L_01_1100,<DP_L_01_0110,<DP_L_01_0011,<DP_L_01_1001
	EQUB <DP_L_01_1100,<DP_L_01_0110,<DP_L_01_0011,<DP_L_01_1001,<DP_L_01_1100,<DP_L_01_0110,<DP_L_01_0011,<DP_L_01_1001,<DP_L_01_1100,<DP_L_01_0110,<DP_L_01_0011,<DP_L_01_1001,<DP_L_01_1100,<DP_L_01_0110,<DP_L_01_0011,<DP_L_01_1001
	EQUB <DP_L_01_1100,<DP_L_01_0110,<DP_L_01_0011,<DP_L_01_1001,<DP_L_01_1100,<DP_L_01_0110,<DP_L_01_0011,<DP_L_01_1001,<DP_L_01_1100,<DP_L_01_0110,<DP_L_01_0011,<DP_L_01_1001,<DP_L_01_1100,<DP_L_01_0110,<DP_L_01_0011,<DP_L_01_1001
	EQUB <DP_L_01_1100,<DP_L_01_0110,<DP_L_01_0011,<DP_L_01_1001,<DP_L_01_1100,<DP_L_01_0110,<DP_L_01_0011,<DP_L_01_1001,<DP_L_01_1100,<DP_L_01_0110,<DP_L_01_0011,<DP_L_01_1001,<DP_L_01_1100,<DP_L_01_0110,<DP_L_01_0011,<DP_L_01_1001
	EQUB <DP_L_01_1100,<DP_L_01_0110,<DP_L_01_0011,<DP_L_01_1001,<DP_L_01_1100,<DP_L_01_0110,<DP_L_01_0011,<DP_L_01_1001,<DP_L_01_1100,<DP_L_01_0110,<DP_L_01_0011,<DP_L_01_1001,<DP_L_01_1100,<DP_L_01_0110,<DP_L_01_0011,<DP_LR_01_1001;<DP_L_01_1001
	EQUB <DP_R_01_1100,<DP_R_01_0110,<DP_R_01_0011,<DP_R_01_1001,<DP_R_01_1100,<DP_R_01_0110,<DP_R_01_0011,<DP_R_01_1001,<DP_R_01_1100,<DP_R_01_0110,<DP_R_01_0011,<DP_R_01_1001,<DP_R_01_1100,<DP_R_01_0110,<DP_R_01_0011,<DP_R_01_1001
	EQUB <DP_R_01_1100,<DP_R_01_0110,<DP_R_01_0011,<DP_R_01_1001,<DP_R_01_1100,<DP_R_01_0110,<DP_R_01_0011,<DP_R_01_1001,<DP_R_01_1100,<DP_R_01_0110,<DP_R_01_0011,<DP_R_01_1001,<DP_R_01_1100,<DP_R_01_0110,<DP_R_01_0011,<DP_R_01_1001
	EQUB <DP_R_01_1100,<DP_R_01_0110,<DP_R_01_0011,<DP_R_01_1001,<DP_R_01_1100,<DP_R_01_0110,<DP_R_01_0011,<DP_R_01_1001,<DP_R_01_1100,<DP_R_01_0110,<DP_R_01_0011,<DP_R_01_1001,<DP_R_01_1100,<DP_R_01_0110,<DP_R_01_0011,<DP_R_01_1001
	EQUB <DP_R_01_1100,<DP_R_01_0110,<DP_R_01_0011,<DP_R_01_1001,<DP_R_01_1100,<DP_R_01_0110,<DP_R_01_0011,<DP_R_01_1001,<DP_R_01_1100,<DP_R_01_0110,<DP_R_01_0011,<DP_R_01_1001,<DP_R_01_1100,<DP_R_01_0110,<DP_R_01_0011,<DP_R_01_1001
	EQUB <DP_R_01_1100,<DP_R_01_0110,<DP_R_01_0011,<DP_R_01_1001,<DP_R_01_1100,<DP_R_01_0110,<DP_R_01_0011,<DP_R_01_1001,<DP_R_01_1100,<DP_R_01_0110,<DP_R_01_0011,<DP_R_01_1001,<DP_R_01_1100,<DP_R_01_0110,<DP_R_01_0011,<DP_R_01_1001
	EQUB <DP_R_01_1100,<DP_R_01_0110,<DP_R_01_0011,<DP_R_01_1001,<DP_R_01_1100,<DP_R_01_0110,<DP_R_01_0011,<DP_R_01_1001,<DP_R_01_1100,<DP_R_01_0110,<DP_R_01_0011,<DP_R_01_1001,<DP_R_01_1100,<DP_R_01_0110,<DP_R_01_0011,<DP_R_01_1001
	EQUB <DP_R_01_1100,<DP_R_01_0110,<DP_R_01_0011,<DP_R_01_1001,<DP_R_01_1100,<DP_R_01_0110,<DP_R_01_0011,<DP_R_01_1001,<DP_R_01_1100,<DP_R_01_0110,<DP_R_01_0011,<DP_R_01_1001,<DP_R_01_1100,<DP_R_01_0110,<DP_R_01_0011,<DP_R_01_1001
	EQUB <DP_R_01_1100,<DP_R_01_0110,<DP_R_01_0011,<DP_R_01_1001,<DP_R_01_1100,<DP_R_01_0110,<DP_R_01_0011,<DP_R_01_1001,<DP_R_01_1100,<DP_R_01_0110,<DP_R_01_0011,<DP_R_01_1001,<DP_R_01_1100,<DP_R_01_0110,<DP_R_01_0011,<DP_R_01_1001
 
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

.mode1Message
EQUB 22, 1                             ; MODE 5
;    !byte 23, 0, 6, 0, $ff                  ; hide display
;    !byte 23, 0, 7, 30, $ff                 ; move display down

EQUB 23, 0, 1, 64      ; Set 6845 register R1 = 32
EQUB 0, 0, 0
EQUB 0, 0, 0           ; This is the "horizontal displayed" register, which defines the number of character blocks per horizontal character row. For comparison, this value is 40 for modes 4 and 5, but our custom screen is not as wide at only 32 character blocks across

;EQUB 23, 0, 6, 24      ; Set 6845 register R6 = 25
;EQUB 0, 0, 0
;EQUB 0, 0, 0           ; This is the "vertical displayed" register, and sets the number of displayed character rows to 31. For comparison, this value is 32 for standard modes 4 and 5, but we claw back the last row for storing code just above the end of screen memory

EQUB 23, 0, 2, 90 ; 40 ; 140 ; 160 ; 180 ; 10 ; 45 ; 20      ; Set 6845 register R2 = 45
EQUB 0, 0, 0
EQUB 0, 0, 0           ; This is the "horizontal sync position" register, which defines the position of the horizontal sync pulse on the horizontal line in terms of character widths from the left-hand side of the screen. For comparison this is 49 for modes 4 and 5, but needs to be adjusted for our custom screen's width

;EQUB 23, 0, 7, 1      ; Set 6845 register R7 = 45 (value could be 0 ?)
;EQUB 0, 0, 0
;EQUB 0, 0, 0           ; This is the "vertical sync position" register

EQUB 23, 1, 0, 0, 0, 0, 0, 0, 0, 0     ; cursor off
 
.mode1MessageEnd
	
SC_LN000 = SCR+(64*000)+0
SC_LN001 = SCR+(64*000)+1
SC_LN002 = SCR+(64*000)+2
SC_LN003 = SCR+(64*000)+3
SC_LN004 = SCR+(64*000)+4
SC_LN005 = SCR+(64*000)+5
SC_LN006 = SCR+(64*000)+6
SC_LN007 = SCR+(64*000)+7
SC_LN008 = SCR+(64*008)+0
SC_LN009 = SCR+(64*008)+1
SC_LN010 = SCR+(64*008)+2
SC_LN011 = SCR+(64*008)+3
SC_LN012 = SCR+(64*008)+4
SC_LN013 = SCR+(64*008)+5
SC_LN014 = SCR+(64*008)+6
SC_LN015 = SCR+(64*008)+7
SC_LN016 = SCR+(64*016)+0
SC_LN017 = SCR+(64*016)+1
SC_LN018 = SCR+(64*016)+2
SC_LN019 = SCR+(64*016)+3
SC_LN020 = SCR+(64*016)+4
SC_LN021 = SCR+(64*016)+5
SC_LN022 = SCR+(64*016)+6
SC_LN023 = SCR+(64*016)+7
SC_LN024 = SCR+(64*024)+0
SC_LN025 = SCR+(64*024)+1
SC_LN026 = SCR+(64*024)+2
SC_LN027 = SCR+(64*024)+3
SC_LN028 = SCR+(64*024)+4
SC_LN029 = SCR+(64*024)+5
SC_LN030 = SCR+(64*024)+6
SC_LN031 = SCR+(64*024)+7
SC_LN032 = SCR+(64*032)+0
SC_LN033 = SCR+(64*032)+1
SC_LN034 = SCR+(64*032)+2
SC_LN035 = SCR+(64*032)+3
SC_LN036 = SCR+(64*032)+4
SC_LN037 = SCR+(64*032)+5
SC_LN038 = SCR+(64*032)+6
SC_LN039 = SCR+(64*032)+7
SC_LN040 = SCR+(64*040)+0
SC_LN041 = SCR+(64*040)+1
SC_LN042 = SCR+(64*040)+2
SC_LN043 = SCR+(64*040)+3
SC_LN044 = SCR+(64*040)+4
SC_LN045 = SCR+(64*040)+5
SC_LN046 = SCR+(64*040)+6
SC_LN047 = SCR+(64*040)+7
SC_LN048 = SCR+(64*048)+0
SC_LN049 = SCR+(64*048)+1
SC_LN050 = SCR+(64*048)+2
SC_LN051 = SCR+(64*048)+3
SC_LN052 = SCR+(64*048)+4
SC_LN053 = SCR+(64*048)+5
SC_LN054 = SCR+(64*048)+6
SC_LN055 = SCR+(64*048)+7
SC_LN056 = SCR+(64*056)+0
SC_LN057 = SCR+(64*056)+1
SC_LN058 = SCR+(64*056)+2
SC_LN059 = SCR+(64*056)+3
SC_LN060 = SCR+(64*056)+4
SC_LN061 = SCR+(64*056)+5
SC_LN062 = SCR+(64*056)+6
SC_LN063 = SCR+(64*056)+7
SC_LN064 = SCR+(64*064)+0
SC_LN065 = SCR+(64*064)+1
SC_LN066 = SCR+(64*064)+2
SC_LN067 = SCR+(64*064)+3
SC_LN068 = SCR+(64*064)+4
SC_LN069 = SCR+(64*064)+5
SC_LN070 = SCR+(64*064)+6
SC_LN071 = SCR+(64*064)+7
SC_LN072 = SCR+(64*072)+0
SC_LN073 = SCR+(64*072)+1
SC_LN074 = SCR+(64*072)+2
SC_LN075 = SCR+(64*072)+3
SC_LN076 = SCR+(64*072)+4
SC_LN077 = SCR+(64*072)+5
SC_LN078 = SCR+(64*072)+6
SC_LN079 = SCR+(64*072)+7
SC_LN080 = SCR+(64*080)+0
SC_LN081 = SCR+(64*080)+1
SC_LN082 = SCR+(64*080)+2
SC_LN083 = SCR+(64*080)+3
SC_LN084 = SCR+(64*080)+4
SC_LN085 = SCR+(64*080)+5
SC_LN086 = SCR+(64*080)+6
SC_LN087 = SCR+(64*080)+7	
SC_LN088 = SCR+(64*088)+0
SC_LN089 = SCR+(64*088)+1
SC_LN090 = SCR+(64*088)+2
SC_LN091 = SCR+(64*088)+3
SC_LN092 = SCR+(64*088)+4
SC_LN093 = SCR+(64*088)+5
SC_LN094 = SCR+(64*088)+6
SC_LN095 = SCR+(64*088)+7
SC_LN096 = SCR+(64*096)+0
SC_LN097 = SCR+(64*096)+1
SC_LN098 = SCR+(64*096)+2
SC_LN099 = SCR+(64*096)+3
SC_LN100 = SCR+(64*096)+4
SC_LN101 = SCR+(64*096)+5
SC_LN102 = SCR+(64*096)+6
SC_LN103 = SCR+(64*096)+7
SC_LN104 = SCR+(64*104)+0
SC_LN105 = SCR+(64*104)+1
SC_LN106 = SCR+(64*104)+2
SC_LN107 = SCR+(64*104)+3
SC_LN108 = SCR+(64*104)+4
SC_LN109 = SCR+(64*104)+5
SC_LN110 = SCR+(64*104)+6
SC_LN111 = SCR+(64*104)+7
SC_LN112 = SCR+(64*112)+0
SC_LN113 = SCR+(64*112)+1
SC_LN114 = SCR+(64*112)+2
SC_LN115 = SCR+(64*112)+3
SC_LN116 = SCR+(64*112)+4
SC_LN117 = SCR+(64*112)+5
SC_LN118 = SCR+(64*112)+6
SC_LN119 = SCR+(64*112)+7
SC_LN120 = SCR+(64*120)+0
SC_LN121 = SCR+(64*120)+1
SC_LN122 = SCR+(64*120)+2
SC_LN123 = SCR+(64*120)+3
SC_LN124 = SCR+(64*120)+4
SC_LN125 = SCR+(64*120)+5
SC_LN126 = SCR+(64*120)+6
SC_LN127 = SCR+(64*120)+7

SC_LN128 = SCR+(64*128)+0
SC_LN129 = SCR+(64*128)+1
SC_LN130 = SCR+(64*128)+2
SC_LN131 = SCR+(64*128)+3
SC_LN132 = SCR+(64*128)+4
SC_LN133 = SCR+(64*128)+5
SC_LN134 = SCR+(64*128)+6
SC_LN135 = SCR+(64*128)+7
SC_LN136 = SCR+(64*136)+0
SC_LN137 = SCR+(64*136)+1
SC_LN138 = SCR+(64*136)+2
SC_LN139 = SCR+(64*136)+3
SC_LN140 = SCR+(64*136)+4
SC_LN141 = SCR+(64*136)+5
SC_LN142 = SCR+(64*136)+6
SC_LN143 = SCR+(64*136)+7
SC_LN144 = SCR+(64*144)+0
SC_LN145 = SCR+(64*144)+1
SC_LN146 = SCR+(64*144)+2
SC_LN147 = SCR+(64*144)+3
SC_LN148 = SCR+(64*144)+4
SC_LN149 = SCR+(64*144)+5
SC_LN150 = SCR+(64*144)+6
SC_LN151 = SCR+(64*144)+7
SC_LN152 = SCR+(64*152)+0
SC_LN153 = SCR+(64*152)+1
SC_LN154 = SCR+(64*152)+2
SC_LN155 = SCR+(64*152)+3
SC_LN156 = SCR+(64*152)+4
SC_LN157 = SCR+(64*152)+5
SC_LN158 = SCR+(64*152)+6
SC_LN159 = SCR+(64*152)+7
SC_LN160 = SCR+(64*160)+0
SC_LN161 = SCR+(64*160)+1
SC_LN162 = SCR+(64*160)+2
SC_LN163 = SCR+(64*160)+3
SC_LN164 = SCR+(64*160)+4
SC_LN165 = SCR+(64*160)+5
SC_LN166 = SCR+(64*160)+6
SC_LN167 = SCR+(64*160)+7

SC_LN168 = SCR+(64*168)+0
SC_LN169 = SCR+(64*168)+1
SC_LN170 = SCR+(64*168)+2
SC_LN171 = SCR+(64*168)+3
SC_LN172 = SCR+(64*168)+4
SC_LN173 = SCR+(64*168)+5
SC_LN174 = SCR+(64*168)+6
SC_LN175 = SCR+(64*168)+7
SC_LN176 = SCR+(64*176)+0
SC_LN177 = SCR+(64*176)+1
SC_LN178 = SCR+(64*176)+2
SC_LN179 = SCR+(64*176)+3
SC_LN180 = SCR+(64*176)+4
SC_LN181 = SCR+(64*176)+5
SC_LN182 = SCR+(64*176)+6
SC_LN183 = SCR+(64*176)+7
SC_LN184 = SCR+(64*184)+0
SC_LN185 = SCR+(64*184)+1
SC_LN186 = SCR+(64*184)+2
SC_LN187 = SCR+(64*184)+3
SC_LN188 = SCR+(64*184)+4
SC_LN189 = SCR+(64*184)+5
SC_LN190 = SCR+(64*184)+6
SC_LN191 = SCR+(64*184)+7

SC_LN192 = SCR+(64*192)+0
SC_LN193 = SCR+(64*192)+1
SC_LN194 = SCR+(64*192)+2
SC_LN195 = SCR+(64*192)+3
SC_LN196 = SCR+(64*192)+4
SC_LN197 = SCR+(64*192)+5
SC_LN198 = SCR+(64*192)+6
SC_LN199 = SCR+(64*192)+7
SC_LN200 = SCR+(64*200)+0
SC_LN201 = SCR+(64*200)+1
SC_LN202 = SCR+(64*200)+2
SC_LN203 = SCR+(64*200)+3
SC_LN204 = SCR+(64*200)+4
SC_LN205 = SCR+(64*200)+5
SC_LN206 = SCR+(64*200)+6
SC_LN207 = SCR+(64*200)+7
SC_LN208 = SCR+(64*208)+0
SC_LN209 = SCR+(64*208)+1
SC_LN210 = SCR+(64*208)+2
SC_LN211 = SCR+(64*208)+3
SC_LN212 = SCR+(64*208)+4
SC_LN213 = SCR+(64*208)+5
SC_LN214 = SCR+(64*208)+6
SC_LN215 = SCR+(64*208)+7
SC_LN216 = SCR+(64*216)+0
SC_LN217 = SCR+(64*216)+1
SC_LN218 = SCR+(64*216)+2
SC_LN219 = SCR+(64*216)+3
SC_LN220 = SCR+(64*216)+4
SC_LN221 = SCR+(64*216)+5
SC_LN222 = SCR+(64*216)+6
SC_LN223 = SCR+(64*216)+7
SC_LN224 = SCR+(64*224)+0
SC_LN225 = SCR+(64*224)+1
SC_LN226 = SCR+(64*224)+2
SC_LN227 = SCR+(64*224)+3
SC_LN228 = SCR+(64*224)+4
SC_LN229 = SCR+(64*224)+5
SC_LN230 = SCR+(64*224)+6
SC_LN231 = SCR+(64*224)+7
SC_LN232 = SCR+(64*232)+0
SC_LN233 = SCR+(64*232)+1
SC_LN234 = SCR+(64*232)+2
SC_LN235 = SCR+(64*232)+3
SC_LN236 = SCR+(64*232)+4
SC_LN237 = SCR+(64*232)+5
SC_LN238 = SCR+(64*232)+6
SC_LN239 = SCR+(64*232)+7
SC_LN240 = SCR+(64*240)+0
SC_LN241 = SCR+(64*240)+1
SC_LN242 = SCR+(64*240)+2
SC_LN243 = SCR+(64*240)+3
SC_LN244 = SCR+(64*240)+4
SC_LN245 = SCR+(64*240)+5
SC_LN246 = SCR+(64*240)+6
SC_LN247 = SCR+(64*240)+7
SC_LN248 = SCR+(64*240)+0
SC_LN249 = SCR+(64*248)+1
SC_LN250 = SCR+(64*248)+2
SC_LN251 = SCR+(64*248)+3
SC_LN252 = SCR+(64*248)+4
SC_LN253 = SCR+(64*248)+5
SC_LN254 = SCR+(64*248)+6
SC_LN255 = SCR+(64*248)+7

.end

SAVE "256beeb", start, end, start

; up_l_1100
; up_l_0110
; up_l_0011
; up_l_00011

; up_r_1100
; up_r_0110
; up_r_0011
; up_r_00011

; dn_l_1100
; dn_l_0110
; dn_l_0011
; dn_l_00011

; dn_r_1100
; dn_r_0110
; dn_r_0011
; dn_r_00011

; ldy vplayer
; ldx hplayer
; dey
; jsr collide
; bne exit

; ldy vplayer
; iny
; ldx hplayer
; dey
; jsr collide
; bne exit





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