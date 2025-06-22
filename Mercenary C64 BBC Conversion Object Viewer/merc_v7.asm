
systemVIAInterruptFlagRegister_fe4d      = $fe4d     ;

logicalScreenStart                  = 20480 ; 20464 ; $5800 ; $3000 ; $5800 ; $3000     ;

; ***************** OS CALLS ********************
OSWRCH                          = $ffee     ;
OSWORD                          = $fff1     ;
OSBYTE                          = $fff4     ;

ula                         = &fe21
system_VIA_portB            = &fe40
system_VIA_dataDirectionB   = &fe42
system_VIA_dataDirectionA   = &fe43
system_VIA_interruptFlags   = &fe4d
system_VIA_interruptEnable  = &fe4e
system_VIA_portA            = &fe4f

SCNX = 128/2
SCNY = 128/2

SCR = logicalScreenStart
SCR0 = SCR
SCR1 = SCR0+6144

SCR0_ADR = SCR0 / 8
SCR1_ADR = SCR1 / 8

; zero page memory locations
ORG &0

; **** ZP ABSOLUTE ADRESSES **** 
;
SCREEN_POS_LO = $02
SCREEN_POS_HI = $03
SCREEN_FLIP = $24
LINE_INC = $66
SCREEN_POS_PTR = $02
SCREEN_0 = $40
SCREEN_1 = $60

X_POS_HI = $52 ; is actually start?
X_POS_LO = $53
Y_POS_HI = $54
Y_POS_LO = $55
Z_POS_HI = $56
Z_POS_LO = $57

ab06 = $06 ; math temp?
ab07 = $07 ; math temp?

LINE_FRACTION_ab08 = $08

ab08 = $08 ; math temp? line draw fraction?
ab09 = $09 ; math temp?

Z_ROT_LO = $08 ; math temp?
Z_ROT_HI = $09 

ab0A = $0A ; math temp?
ab0B = $0B ; math temp?

Z_CLIP_LO_ab0A = $0A
Z_CLIP_HI_ab0B = $0B
H_PERS_FP_LO_DIFF_ab0A = $0A
H_PERS_FP_HI_DIFF_ab0B = $0B
X_ROT_LO = $0A
X_ROT_HI = $0B

Z_CLIP_LO_ab0C = $0C ; math temp?
Z_CLIP_HI_ab0D = $0D ; math temp?
V_PERS_FP_LO_DIFF_ab0C = $0C
V_PERS_FP_HI_DIFF_ab0D = $0D

ab18 = $18 ; math temp?
POINT_LINE_COUNT = $19

REGA = $0E
REGX = $0F
REGY = $20

FOCUS = $18 ; focus value not location

FP_MULTIPLY_LO = $22
FP_MULTIPLY_HI = $23
FP_DIVIDE_LO = FP_MULTIPLY_LO
FP_DIVIDE_HI = FP_MULTIPLY_HI

ROT_LO = $2C
ROT_HI = $2D

SIN_LO = $3C
SIN_HI = $3D
COS_LO = $3E
COS_HI = $3F

X_ROTATION_LO = $58 ; is actually end?
X_ROTATION_HI = $59
Y_ROTATION_LO = $5A
Y_ROTATION_HI = $5B
Z_ROTATION_LO = $5C
Z_ROTATION_HI = $5D

H_PERS_FP_LO_ST = $5E	; hor pers pos lo fp point start
H_PERS_FP_HI_ST = $5F	; hor pers pos hi fp point start
V_PERS_FP_LO_ST = $60	; ver pers pos lo fp point start
V_PERS_FP_HI_ST = $61	; ver pers pos hi fp point start

H_PERS_FP_LO_EN = $62	; hor pers pos lo fp point end
H_PERS_FP_HI_EN = $63	; hor pers pos hi fp point end
V_PERS_FP_LO_EN = $64	; ver pers pos lo fp point end
V_PERS_FP_HI_EN = $65	; ver pers pos hi fp point end

LINE_TYPE_POINTER = $67 ; line type?
ab68 = $68
ab69 = $69
LINE_START_HOR = $6A ; line start x?
LINE_START_VER = $6B ; line start y?
PERS_HOR_ab6C = $6C	; pers?
PERS_VER_ab6D = $6D  ; pers?
POINT_CLASSIFY_ST = $6E	; start point classification of this point?
POINT_CLASSIFY_EN = $6F  ; end point classification

CURR_X_LSB = $72
CURR_X_MSB = $73
CURR_X_HSB = $74
CURR_Y_LSB = $75
CURR_Y_MSB = $76
CURR_Y_HSB = $77
CURR_Z_LSB = $78
CURR_Z_MSB = $79
CURR_Z_HSB = $7A

SCR_HOR_MID = 128/2 ; tobe 128/2 was 160/2
SCR_VER_MID = 128/2 ; tobe 128/2 was 136/2

;ab90 = $90
;ab91 = $91
;ab92 = $92
;ab93 = $93
;ab94 = $94
;ab95 = $95
;ab96 = $96
;ab97 = $97

ab90_VAL = $04	; -80
ab91_VAL = $19 	; -80
ab92_VAL = $f8  ; +79 ; tobe $04 was $3C
ab93_VAL = $14  ; +79
ab94_VAL = $04  ; -68
ab95_VAL = $19  ; -68
ab96_VAL = $f8	; +67
ab97_VAL = $14  ; +67

CLIP_TEST_BOTTOM = 128 ; $88 	; tobe 128 was $88
CLIP_BOTTOM = CLIP_TEST_BOTTOM-1
CLIP_TEST_RIGHT = 128 ; $A0 	; tobe 128 was $A0
CLIP_RIGHT = CLIP_TEST_RIGHT-1
CLIP_TEST_TOP = $00
CLIP_TEST_LEFT = $00

SPEED_LO = $AA
SPEED_HI = $AB
ROT_SPD = $AC
OBJECT_POINTER_POS = $AD
CURRENT_OBJECT = $AF
NUMBER_OF_POINTS = $B0
DRAW_OBJECT_COUNT = $B2
DRAW_OBJECT_COUNTER = $B3

OBJ_PNT_X_LSB 	= $CF
OBJ_PNT_X_MSB 	= $D0
OBJ_PNT_Y_LSB 	= $D1
OBJ_PNT_Y_MSB 	= $D2
OBJ_PNT_Z_LSB 	= $D3
OBJ_PNT_Z_MSB 	= $D4
DIFF_X_LSB = $D5
DIFF_X_MSB = $D6
DIFF_X_HSB = $D7
DIFF_Y_LSB = $D8
DIFF_Y_MSB = $D9
DIFF_Y_HSB = $DA
DIFF_Z_LSB = $DB
DIFF_Z_MSB = $DC
DIFF_Z_HSB = $DD
OBJECT_POINTER = $1D

FP_SIGN = $01

PNT_OFF_HOR = $01	; PNT_OFF_RIGHT = $01
PNT_OFF_VER = $02	; PNT_OFF_BOTTOM = $02

OVERFLOW = $E000 ; overflow?

;P_SQR_LO = $E0
;P_SQR_HI = $E2
;P_INVSQR_LO = $E4
;P_INVSQR_HI = $E6

;SCN		= $E8
;SCN_ZLO	= $EA

;H_POS = $EC
;V_POS = $ED
;H_ST = $EE
;V_ST = $EF
;H_EN = $F0
;V_EN = $F1
;LINE_FRAC = $F2
;LINE_VER = $F3
REGY_BUF = $F4
REGX_BUF = $F5
REGA_BUF = $F6

X_OFF_LO = $F7
X_OFF_HI = $F8
Y_OFF_LO = $F9
Y_OFF_HI = $FA
Z_OFF_LO = $FB
Z_OFF_HI = $FC
SCN_ZLO	= $FD

BLACK 	= 0
RED 	= 1
GREEN 	= 2
YELLOW 	= 3
BLUE 	= 4
MAGENTA	= 5
CYAN 	= 6
WHITE 	= 7

;ORG &0

ORG &1C05

.START

    jsr initVia
	
	LDX #$FF
    TXS                                     ; Reset stack
	
	INX
	STX SCN_ZLO								; reset low byte of scn

    LDA #144
    LDX #254                                ; Set non-interlaced
    LDY #1
    JSR OSBYTE

	LDX #0                                  ; Switch to MODE 1
{					
.LOOP	LDA SCREEN_SETUP_PARAMS_START,X
		JSR OSWRCH
		INX
		CPX #SCREEN_SETUP_PARAMS_END - SCREEN_SETUP_PARAMS_START
		BNE LOOP
}

	LDX #0
	LDY #BLACK
	JSR SetLogicalColour
	LDX #1
	LDY #GREEN
	JSR SetLogicalColour
	LDX #2
	LDY #CYAN
	JSR SetLogicalColour
	LDX #3
	LDY #WHITE
	JSR SetLogicalColour

	LDA		#7
	STA		ROT_SPD
	LDA		#$40
	STA		SPEED_LO
	LDA		#$18
	STA		SPEED_HI
	
	LDA		#1 ;7
	STA		DRAW_OBJECT_COUNT

	LDA		#0
	STA		ROT_LO
	STA		ROT_HI
	STA		SCREEN_FLIP
	
	STA		CURR_X_MSB
	STA		CURR_X_HSB
	STA		CURR_Y_MSB
	STA		CURR_Y_HSB
	STA		CURR_Z_MSB
	STA		CURR_Z_HSB

    SEI									    ; install irq code

.MLOOP	JSR VSYNC

	JSR SWAP_SCREEN
			JSR		SIN_COS
;			JSR 	DRAW_SCREEN
			JSR 	COLOUR_SETUP_BLACK ; COLOUR_SETUP_WHITE ; BLACK
			JSR 	DRAW_OBJECTS

;			LDA 	ROT_LO	; LEFT
;			CLC 
;			ADC 	ROT_SPD
;			STA 	ROT_LO
;			LDA 	ROT_HI
;			ADC 	#$00
;			AND 	#$03
;			STA 	ROT_HI

; JSR DEBUG_WRITE
		JSR READKEYS
		JMP MLOOP

.initVia:
    lda #%01111111
	sta system_VIA_interruptEnable ; disable all interrupts
    lda #%10000010
	sta system_VIA_interruptEnable ; enable just VBlank
    ;; poll keyboard via system VIA portA
    ;; data directionA: bottom 7 bits output (key to poll); top bit input (is it pressed?)
    lda #%01111111
	sta system_VIA_dataDirectionA
    lda #%00001111
	sta system_VIA_dataDirectionB ; allow write to addressable latch
    lda #%00000011
	sta system_VIA_portB ; set bit 3 to 0
    rts

.VSYNC
{
	LDA #2                                  ; wait for VSync without having to catch it from its IRQ
	STA systemVIAInterruptFlagRegister_fe4d      ; clear VSync flag
.LOOP	BIT systemVIAInterruptFlagRegister_fe4d
		BEQ LOOP                         	; poll VSync flag
	RTS
}

.SWAP_SCREEN
		LDA #>SCR0_ADR
.SCNADR CMP	#>SCR1_ADR
		BNE SCN1
		JMP SCN0
.SCN1		LDA #>SCR0_ADR
			LDX #<SCR0_ADR
		STA SCNADR +1
		LDY	#12
		STY &FE00
		STA	&FE01
		INY
		STY &FE00
		STX	&FE01
.SCN1_CLR
{
		LDX #63
		LDA	#%11000011
.LOOP
	STA SCR1+(0*64),X
	STA SCR1+(1*64),X
	STA SCR1+(2*64),X
	STA SCR1+(3*64),X
	STA SCR1+(4*64),X
	STA SCR1+(5*64),X
	STA SCR1+(6*64),X
	STA SCR1+(7*64),X
	STA SCR1+(8*64),X
	STA SCR1+(9*64),X

	STA SCR1+(10*64),X
	STA SCR1+(11*64),X
	STA SCR1+(12*64),X
	STA SCR1+(13*64),X
	STA SCR1+(14*64),X
	STA SCR1+(15*64),X
	STA SCR1+(16*64),X
	STA SCR1+(17*64),X
	STA SCR1+(18*64),X
	STA SCR1+(19*64),X

	STA SCR1+(20*64),X
	STA SCR1+(21*64),X
	STA SCR1+(22*64),X
	STA SCR1+(23*64),X
	STA SCR1+(24*64),X
	STA SCR1+(25*64),X
	STA SCR1+(26*64),X
	STA SCR1+(27*64),X
	STA SCR1+(28*64),X
	STA SCR1+(29*64),X

	STA SCR1+(30*64),X
	STA SCR1+(31*64),X
	STA SCR1+(32*64),X
	STA SCR1+(33*64),X
	STA SCR1+(34*64),X
	STA SCR1+(35*64),X
	STA SCR1+(36*64),X
	STA SCR1+(37*64),X
	STA SCR1+(38*64),X
	STA SCR1+(39*64),X

	STA SCR1+(40*64),X
	STA SCR1+(41*64),X
	STA SCR1+(42*64),X
	STA SCR1+(43*64),X
	STA SCR1+(44*64),X
	STA SCR1+(45*64),X
	STA SCR1+(46*64),X
	STA SCR1+(47*64),X
	STA SCR1+(48*64),X
	STA SCR1+(49*64),X

	STA SCR1+(50*64),X
	STA SCR1+(51*64),X
	STA SCR1+(52*64),X
	STA SCR1+(53*64),X
	STA SCR1+(54*64),X
	STA SCR1+(55*64),X
	STA SCR1+(56*64),X
	STA SCR1+(57*64),X
	STA SCR1+(58*64),X
	STA SCR1+(59*64),X

	STA SCR1+(60*64),X
	STA SCR1+(61*64),X
	STA SCR1+(62*64),X
	STA SCR1+(63*64),X
		
			DEX
			BMI	CONT
			JMP	LOOP
.CONT	LDA #>SCR1_MONO_HI
;			lda		#>VER_POS_SCN1_HI
		sta		LINE_DRAW_0_SCN +2
			sta		LINE_DRAW_1_SCN +2
			sta		LINE_DRAW_2_SCN +2
			sta		LINE_DRAW_3_SCN +2
			sta		LINE_DRAW_4_SCN +2
			sta		LINE_DRAW_5_SCN +2
			sta		LINE_DRAW_6_SCN +2
			sta		LINE_DRAW_7_SCN +2
			sta		PLOT_POINT_SCN +2


		RTS
}
.SCN0	LDA	#>SCR1_ADR
		LDX #<SCR1_ADR
		STA SCNADR +1
		LDY	#12
		STY &FE00
		STA	&FE01
		INY
		STY &FE00
		STX	&FE01
.SCN0_CLR
{
		LDX #63
		LDA	#%11000011
.LOOP
	STA SCR0+(0*64),X
	STA SCR0+(1*64),X
	STA SCR0+(2*64),X
	STA SCR0+(3*64),X
	STA SCR0+(4*64),X
	STA SCR0+(5*64),X
	STA SCR0+(6*64),X
	STA SCR0+(7*64),X
	STA SCR0+(8*64),X
	STA SCR0+(9*64),X

	STA SCR0+(10*64),X
	STA SCR0+(11*64),X
	STA SCR0+(12*64),X
	STA SCR0+(13*64),X
	STA SCR0+(14*64),X
	STA SCR0+(15*64),X
	STA SCR0+(16*64),X
	STA SCR0+(17*64),X
	STA SCR0+(18*64),X
	STA SCR0+(19*64),X

	STA SCR0+(20*64),X
	STA SCR0+(21*64),X
	STA SCR0+(22*64),X
	STA SCR0+(23*64),X
	STA SCR0+(24*64),X
	STA SCR0+(25*64),X
	STA SCR0+(26*64),X
	STA SCR0+(27*64),X
	STA SCR0+(28*64),X
	STA SCR0+(29*64),X

	STA SCR0+(30*64),X
	STA SCR0+(31*64),X
	STA SCR0+(32*64),X
	STA SCR0+(33*64),X
	STA SCR0+(34*64),X
	STA SCR0+(35*64),X
	STA SCR0+(36*64),X
	STA SCR0+(37*64),X
	STA SCR0+(38*64),X
	STA SCR0+(39*64),X

	STA SCR0+(40*64),X
	STA SCR0+(41*64),X
	STA SCR0+(42*64),X
	STA SCR0+(43*64),X
	STA SCR0+(44*64),X
	STA SCR0+(45*64),X
	STA SCR0+(46*64),X
	STA SCR0+(47*64),X
	STA SCR0+(48*64),X
	STA SCR0+(49*64),X

	STA SCR0+(50*64),X
	STA SCR0+(51*64),X
	STA SCR0+(52*64),X
	STA SCR0+(53*64),X
	STA SCR0+(54*64),X
	STA SCR0+(55*64),X
	STA SCR0+(56*64),X
	STA SCR0+(57*64),X
	STA SCR0+(58*64),X
	STA SCR0+(59*64),X

	STA SCR0+(60*64),X
	STA SCR0+(61*64),X
	STA SCR0+(62*64),X
	STA SCR0+(63*64),X
	
			DEX
			BMI	CONT
			JMP	LOOP
.CONT	LDA #>SCR0_MONO_HI
		sta		LINE_DRAW_0_SCN +2
			sta		LINE_DRAW_1_SCN +2
			sta		LINE_DRAW_2_SCN +2
			sta		LINE_DRAW_3_SCN +2
			sta		LINE_DRAW_4_SCN +2
			sta		LINE_DRAW_5_SCN +2
			sta		LINE_DRAW_6_SCN +2
			sta		LINE_DRAW_7_SCN +2
			sta		PLOT_POINT_SCN +2
		RTS
}

;.DEBUG_WRITE

; LDA keyShift
; LDX #0
; LDY #0
; JSR HEX8

; LDA keyEnter
; LDX #0
; LDY #8
; JSR HEX8

; LDA keyUp
; LDX #0
; LDY #16
; JSR HEX8

; LDA keyDown
; LDX #0
; LDY #24
; JSR HEX8
 
; LDA keyLeft
; LDX #0
; LDY #32
; JSR HEX8

; LDA keyRight
; LDX #0
; LDY #40
; JSR HEX8
  
;	RTS
 
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
		TYA
		AND #7
      STA   HEX_NUMBER0+1
      
      LDA   SCR0_MONO_HI,Y
      STA   HEX_NUMBER0+2

		INY
		TYA
		AND	#7
      STA   HEX_NUMBER1+1
      
      LDA   SCR0_MONO_HI,Y 
      STA   HEX_NUMBER1+2

		INY
		TYA
		AND	#7
      STA   HEX_NUMBER2+1
      
      LDA   SCR0_MONO_HI,Y 
      STA   HEX_NUMBER2+2

		INY
		TYA
		AND	#7
      STA   HEX_NUMBER3+1
      
      LDA   SCR0_MONO_HI,Y 
      STA   HEX_NUMBER3+2

		INY
		TYA
		AND	#7
      STA   HEX_NUMBER4+1
      
      LDA   SCR0_MONO_HI,Y 
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

.POLL_KEY
			STA system_VIA_portA
			LDA system_VIA_portA
			RTS

.READKEYS
;		LDA	#(-(-1)-1)
;		JSR	POLL_KEY
;		STX keyShift

{
		LDA	#(-(-74)-1)
		JSR	POLL_KEY
		BPL cont
			lda 	OBJECTS_TO_DRAW+1
			clc
			adc 	#1
			and 	#63
			sta 	OBJECTS_TO_DRAW+1
.cont
}
{
		LDA	#(-(-58)-1)
		JSR	POLL_KEY
		BPL cont
			LDA	#$01 ; FORWARD
			JSR FORWARD_BACK
.cont
}
{
		LDA	#(-(-42)-1)
		JSR	POLL_KEY
		BPL cont
			LDA 	#$00	; BACKWARD
			JSR 	FORWARD_BACK
.cont
}
{
		LDA	#(-(-26)-1)
		JSR	POLL_KEY
		BPL cont
			LDA 	ROT_LO	; LEFT
			CLC 
			ADC 	ROT_SPD
			STA 	ROT_LO
			LDA 	ROT_HI
			ADC 	#$00
			AND 	#$03
			STA 	ROT_HI
.cont
}
{
		LDA	#(-(-122)-1)
		JSR	POLL_KEY
		BPL cont
			LDA 	ROT_LO	; RIGHT
			SEC 
			SBC 	ROT_SPD
			STA 	ROT_LO
			LDA 	ROT_HI
			SBC 	#$00
			AND 	#$03
			STA 	ROT_HI
.cont
}		
		RTS
;    PollKey -1,   keyShift
;    PollKey -74,  keyEnter
;    PollKey -58,  keyUp
;    PollKey -42,  keyDown
;    PollKey -26,  keyLeft
;    PollKey -122, keyRight

.FP_DIVIDE	STA		FP_DIVIDE_HI	; inputs = X-(LO) + Y-(HI) + A (HI) + FP_MULTILPLY_LO - outputs = AY
			LDA 	LOG_TABLE,Y
			LDY 	FP_DIVIDE_LO
			SEC 
			SBC 	LOG_TABLE,Y
			TAY
			TXA
			LDX 	EXP_TABLE,Y
			ORA 	#$02
			BCS 	_FP_DIV_CNT
					SBC 	#$03	; subtract 00000011
					SEC 
._FP_DIV_CNT SBC 	FP_DIVIDE_HI
			AND 	#$FD
			RTS

.FP_MULTIPLY	STA		FP_MULTIPLY_HI	; inputs = X-(LO) + Y-(HI) + A (HI) + FP_MULTILPLY_LO - outputs = AY
			LDA 	LOG_TABLE,X		
			LDX 	FP_MULTIPLY_LO
			CLC 
			ADC 	LOG_TABLE,X
			TAX 
			TYA 
			LDY 	EXP_TABLE,X
			BCC 	_FP_MUL_CNT
					ADC 	#$03	; add 00000011
					CLC 
._FP_MUL_CNT	ADC 	FP_MULTIPLY_HI
			AND 	#$FD
			RTS 

.FPMUL		LDA LOG_TABLE,X		; fpMul - ; Input:  [08] = fp multiplier ;         XY   = fp multiplicand ; Output: [08] and XY = result
			LDX ab08
			CLC 
			ADC LOG_TABLE,X
			TAX 
			TYA 
			BCC _PF_MUL_CNT
				ADC #$03			; add 00000011
				CLC 
._PF_MUL_CNT   	ADC ab09
			BVS MATH_LIMIT 
			AND #$FD
			STA ab09
			TAY 
			LDA EXP_TABLE,X
			TAX 
			STA ab08
			RTS 

.MATH_LIMIT	BPL _MATH_LIMIT_POS
			AND #$01
			ORA #$78
			TAY 
			LDX #$00
			STX ab08
			STY ab09
			RTS 

._MATH_LIMIT_POS	AND #$01
				ORA #$84

.MATH_LIMIT_EXIT	TAY 
				LDX #$00
				STX ab08
				STY ab09
				RTS 

.FPDIV		STY 	ab06	 ; fpDiv - Input:  [08] = fp divisor - XY   = fp dividend - Output: [08] and XY = result
			LDY 	ab08
			LDA 	LOG_TABLE,Y
			SEC 
			SBC 	LOG_TABLE,X
			TAX 
			LDA 	ab09
			ORA 	#$02
			BCS 	FPDIV_CONT
				SBC 	#$03		; subtract 00000011
				SEC 
.FPDIV_CONT	SBC 	ab06
			BVS 	MATH_LIMIT 
			AND 	#$FD
			STA 	ab09
			TAY 
			LDA 	EXP_TABLE,X
			TAX 
			STA 	ab08
			RTS 



.FPADD_CONT	LDA		ab18	; ($838B entry point)  
			LSR 	A
			BCS 	FPADD_CONT2
        TXA 
        ADC 	ab08
        ROR 	A
        INY 
        INY 
        INY 
        INY 
        TAX 
        STA 	ab08
        STY 	ab09
        RTS 

.FP_ADD_ERR	TYA 
			ASL 	A
			BCC 	FP_ADD_ERR_1ST
				JMP 	FP_ADD_ERR_2ND
.FPADD_CONT2
		TXA 
        SBC 	ab08
        BEQ 	FPADD_CONT3
			BCS 	FPADD_CONT6
			LDY 	ab09
			EOR 	#$FF
			ADC 	#$01
			JMP 	FPADD_CONT6

.FPADD_CONT3		TYA 
				SEC 
				SBC 	#$24
				BVS 	_MATH_LIMIT_POS
					BVC 	MATH_LIMIT_EXIT
	
.FP_ADD  TYA			; XY + 89	; transfer num1_hi to a
        SEC 					; set carry
        SBC 	ab09			; sub num2_hi from a
        BVS 	FP_ADD_ERR		; overflow so exit
        STA 	ab18			; ab18 = num1_hi - num2_hi
        ADC 	#$01			; add 1 + carry?
        BVS 	FP_ADD_ERR		; overflow so exit
        BMI 	FPADD_CONT8			
        LSR 	A				; divide by 2
        LSR 	A				; divide by 2
        BEQ 	FPADD_CONT		; if 0
        CMP 	#$09			; gt 9
        BCS 	FP_ADD_ERR_1ST	; yes
        EOR 	#$0F			; reverse 
        STA 	FP_ADD_JMP1 +1		; jump forward that amount
        LDA		ab08			; get num_hi
        SEC						; set carry 
        ROR 	A				; /2 and move carry into first bit

.FP_ADD_JMP1 	BNE		b83B3 	; self modifiction code shift
        NOP 					; NA
        NOP 					; NA
        NOP 					; NA
        NOP 					; NA
        NOP 					; NA
        NOP 					; NA
        NOP 					; NA
.b83B3  LSR 	A				; divide by 2
        LSR  	A				; divide by 2
        LSR  	A				; divide by 2
        LSR  	A				; divide by 2
        LSR  	A				; divide by 2
        LSR  	A				; divide by 2
        LSR  	A				; divide by 2
        STA 	ab06			; 
        LDA 	ab18
        LSR 	A				; divide by 2 - setting carry?
        TXA 					; transfer num1_lo to a
        BCS 	FPADD_CONT5			; carry set
.FPADD_CONT4	ADC 	ab06
			BCC 	FPADD_CONT7
				LSR 	A			; divide by 2
				INY 				; y++
				INY 				; y++
				INY 				; y++
				INY 				; y++
.FPADD_CONT7   	TAX
.FP_ADD_ERR_1ST 
			STX 	ab08
			STY 	ab09
			RTS 

.FPADD_CONT5	SBC ab06
			BCS FPADD_CONT7
.FPADD_CONT6	STY ab06
        LDY #$00
.FPADD_LOOP1	ASL 	A
				DEY 
				BCC		FPADD_LOOP1
        TAX 
        TYA 
        ASL 	A
        ASL 	A
        CLC 
        ADC 	ab06
        BVS 	FPADD_ERR3
			TAY 
			STX 	ab08
			STY 	ab09
			RTS 

.FPADD_ERR3	LDX 	#<8400
			LDY 	#>8400
			STX 	ab08
			STY		ab09
			RTS 

.FPADD_CONT8	CMP 	#$E0
			BCC 	FP_ADD_ERR_2ND
        LSR A
        LSR A
        AND #$07
        STA FP_ADD_JMP2 +1
		TXA 
        SEC 
        ROR	A
.FP_ADD_JMP2	BNE b8408
        LSR A
        LSR A
        LSR A
.b8408  LSR A
        LSR A
        LSR A
        LSR A
        STA ab06
        LDY ab09
        LDA ab18
        LSR A
        LDA ab08
        BCC FPADD_CONT4
        BCS FPADD_CONT5	; jmp
		
.FP_ADD_ERR_2ND	LDX		ab08
				LDY 	ab09
				RTS 







.COS_FP	INY 
.SIN_FP	TYA 
		LSR 	A
		BCC 	SIN_CNT
				TXA 
				EOR #$FF
				TAX 
				TYA 
				LSR 	A
.SIN_CNT AND 	#$01
		ORA 	SIN_MSB,X
		TAY 
		LDA 	SIN_LSB,X
		TAX 
		RTS 

; Y format = floating point position 
; bit0 = ignored / shifted out - first bit can be set to show negative?
; bit1 = ignored / shifted out
; bit2 = 
; bit3 = 
; bit4 = 
; bit5 = 
; bit6 = 
; bit7 = must be 0 or if set value must be gt FC for 1 to be added to result?

; X format = actual number
; bit0 = 
; bit1 = 
; bit2 = 
; bit3 = 
; bit4 = 
; bit5 = 
; bit6 = 
; bit7 = 

.FPTO8BIT	TYA 				; input X/Y (mantisa/exponent) -  output X=HI/Y=LO ; transfer exponent? to a
			BMI		b849F		; if negative
			LSR 	A			; half and put bit into carry (carry unused)
			LSR 	A			; half and put bit into carry (carry unused)
			CMP 	#$07		; is it greater than 7
			BCS 	FP8BIT_ERR		; yes so error so exit with carry set
			STA 	FP8BIT_JMP +1	; no so self modifying code to jump forward a number of divides
			TXA 				; transfer mantisa? to a
			SEC 				; set carry
			ROR 	A			; /2 - putting 1 in top bit (and putting 0 in carry?) 
.FP8BIT_JMP	BNE 	b848C		; jump forward self modifying code - always done as carry is set so ROR result is never 0
			LSR 	A			; /2
			LSR 	A			; /4
			LSR 	A			; /8
			LSR 	A			; /16
.b848C   	LSR 	A			; /32
			LSR 	A			; /64
			LSR 	A			; /128
.b848F   	ADC 	#0			; add carry?
			BMI 	b84A5		; if minus then error?
			TAX 				; backup result
			TYA 				; transfer y to a
			LSR		A			; / 2
			TXA 				; restore result
			BCC 	b849E		; clear carry so ok?
				EOR 	#$FF	; negate?
				ADC 	#$00	; negate by adding carry that is already set?
				CLC 			; clear carry for ok?
.b849E   	RTS 				; exit

.b849F   	CMP 	#$FC		; sets the carry flag if greater than (11111100 binary)
			LDA 	#$00		; resets the output x value? 
			BEQ 	b848F		; jump back into code

.b84A5   	SEC 				; set carry for error?
.FP8BIT_ERR	RTS 				; exit

.FPTO16BIT	TYA					; input X/Y (mantisa/exponent) -  output X=HI/Y=LO	; transfer exponent? to a
			BMI 	b8501 		; if negative
			LSR 	A			; half and put bit into carry
			LSR 	A			; half and put bit into carry
;			CMP 	#15			; is it greater than 15
;			BCS 	FP8BIT_ERR		; yes so error so exit with carry set
			STA 	ab06		; store intermediate in temp var
			ASL 	A			; *2
			ADC 	ab06		; *3 
			STA 	FP16BIT_JMP +1	; number of bytes to jump
			LDA 	#0
			STA 	ab06		; clear result
			TXA 				; get input (mantisa?)
			SEC 				; set carry to move 1 into result
			ROR 	A			; rotate carry into high bit and low bit into carry
			ROR 	ab06		; rotate carry into result
.FP16BIT_JMP BCC 	b84D2		; jump forward
			LSR 	A			; half and shift lowest bit into carry  
			ROR 	ab06		; half and rotate carry bit into result
			LSR 	A
			ROR 	ab06
			LSR 	A
			ROR 	ab06
			LSR 	A
			ROR 	ab06
			LSR 	A
			ROR 	ab06
.b84D2   	LSR 	A
			ROR 	ab06
			LSR 	A
			ROR 	ab06
			LSR 	A
			ROR 	ab06
			LSR 	A
			ROR		ab06
			LSR 	A
			ROR 	ab06
			LSR 	A
			ROR 	ab06
			LSR 	A
			ROR 	ab06
			LSR 	A
			ROR 	ab06
			LSR 	A
			ROR 	ab06
			LSR 	A
			ROR 	ab06
			TAX 				; store low byte of result?
			TYA 				; get input (exponent?)
			LDY 	ab06		; store high byte of result?
			LSR 	A			; move 1st bit of input (sign) into carry
			BCC 	b8500		; clear so positive
				TXA 			; negtive so
				EOR 	#$FF	; negate x
				TAX 
				TYA 
				EOR 	#$FF	; negate y
				TAY 
				CLC 
.b8500   RTS 					; no

.b8501 		LSR 	A			; move 1st bit to carry
			BCC 	b850B		; set y/n
				LDX 	#$FF	; yes
				LDY 	#$FF
				CLC 
			RTS 
.b850B   	LDX 	#0			; no
			LDY 	#0
			RTS 

; A = exponent = value of (first bit position set *4?) with sign in bit 0
; Y = mantissa = value of next 8 bits of the rest of the 8/16/24 bit number

.BIT24TOFP 	STY		ab07 	; store lsb	; MATH_24BitToFP (FP.SetToObjectSpaceCoordinate24) ; Enter: A = Coordinate.MSB X = Coordinate.PSB Y = Coordinate.LSB ; Usese: [06].b  Temporary store for coordinate MSB ; Exit:  [08].fp Result, also stored in X:A (exp:man)
			STA 	ab06 	; store msb
			BPL 	_BIT24FP_POS	 ; positive ?

				LDA 	#0	 	; negative so 
				SEC 
				SBC 	ab07 	; negate lsb
				STA 	ab07
				TXA 			; negate psb
				EOR 	#$FF
				ADC 	#0
				TAX 
				LDA 	#0
				SBC 	ab06	; negate msb

._BIT24FP_POS	BNE 	_BIT24			; if msb not 0 - jump to set loop counter to 24 ($18)?
				TXA 				; move psb to a
				BNE 	_BIT16			; if psb not 0 - jump to set loop counter to 16 ($10)?

					LDA 	ab07			; get lsb
					BEQ 	_BIT00 			; if lsb is 0 - jump to overflow and exit

						LDX 	#0
						STX 	ab07
						LDX 	#8			; set loop counter to 8?
						BNE 	_EXP_LOOP 		; jump?

._BIT24   	STX 	ab07	; store psb?
			LDX 	#24		; set loop counter to 24?

._EXP_LOOP     	DEX 			; find exponent - start from whatever x is passed in and reduce until x a bit set found 
				ASL 	ab07 	; shift lsb up into carry
				ROL 	A 		; shift msb up with carry - mantisa?
				BCC 	_EXP_LOOP	; as soon as first bit is set then exit

			TAY					; store mantisa?
			TXA 				; x is exponent?
			ASL 	A			; exponent *2?
			ASL 	ab06		; shift msb putting sign into carry 
			ROL 	A			; rotate carry into first bit of exponent - bit 0 is sign
			RTS

._BIT16   	LDX 	#16		; set loop counter to 16?
			BNE 	_EXP_LOOP 	; jump?
		
._BIT00    	LDY 	#<OVERFLOW ; overflow?
			LDA 	#>OVERFLOW
			RTS 

.DRAW_OBJECTS	LDY 	DRAW_OBJECT_COUNT 			; number of objects to draw
				BEQ 	DRAW_OBJECTS_EXIT 			; exit if none
				STY 	DRAW_OBJECT_COUNTER 		; store temp object count
.DRAW_OBJECTS_LOOP   LDX 	OBJECTS_TO_DRAW,Y 		; object number
					JSR 	DRAW_OBJECT
					DEC 	DRAW_OBJECT_COUNTER 	; dec temp object count
					LDY 	DRAW_OBJECT_COUNTER 	; get temp object count
					BNE 	DRAW_OBJECTS_LOOP 		; exit y/n
.DRAW_OBJECTS_EXIT 	RTS 							; exit

.DRAW_OBJECT			STX		CURRENT_OBJECT					; store object number 
					LDA		OBJECT_SHAPE_PTR_LO,X	; get object pos hi
					STA		OBJECT_POINTER			; store in pointer
					LDA		OBJECT_SHAPE_PTR_HI,X	; get object pos lo
					STA		OBJECT_POINTER+1		; store in pointer
					JSR		CAMERA_OBJECT_DIFF
;					JSR		DRAW_OBJECT_SHAPE 		; draw object 	; commented out
; SEC																; commented out
;					BCS		DRAW_OBJECT_CONT						; commented out
;DRAW_OBJECT_EXIT	RTS 			

.DRAW_OBJECT_CONT   	LDX		#$00
					STX		POINT_LINE_COUNT
					LDY		#$01
					JSR		CHK_ABS_DIST
					BCC		DRAW_OBJECT_LINES	; draw the object
					LDA		DIFF_Y_HSB			; above ground?
					BPL		DRAW_OBJECT_POINT	; not underground?
					LDY		#$00
					LDX		#$00
					JSR		CHK_ABS_DIST
					BCS		DRAW_OBJECTS_EXIT

.DRAW_OBJECT_POINT	LDY		DIFF_X_LSB		; object is a point
					LDX		DIFF_X_MSB
					LDA		DIFF_X_HSB
					JSR		BIT24TOFP
					STY		X_POS_HI
					STA		X_POS_LO
					LDY		DIFF_Y_LSB
					LDX		DIFF_Y_MSB
					LDA		DIFF_Y_HSB
					JSR		BIT24TOFP
					STY		Y_POS_HI
					STA		Y_POS_LO
					LDY		DIFF_Z_LSB
					LDX		DIFF_Z_MSB
					LDA		DIFF_Z_HSB
					JSR		BIT24TOFP
					STY		Z_POS_HI
					STA		Z_POS_LO
					JSR		ROTATE_PERS
					LDX		#$00
;        			JMP		PLOT_POINT 	; PLOT POINT

.PLOT_POINT_CHK		LDA		POINT_CLASSIFICATION,X	; GET POINT FROM PLOT POINT TABLE ?
					BNE		PLOT_POINT_EXIT			; EXIT IF ITS not ZERO - this being set to not zero plots a point - what sets it to 0?
.PLOT_POINT				LDY		PERS_VER_TAB,X			; GET VER POS
						LDA		VER_POS_LO,Y 
						STA		SCREEN_POS_LO
.PLOT_POINT_SCN			LDA 	VER_POS_SCN0_HI,Y
						STA		SCREEN_POS_HI
						LDY		HOR_SCR_POS,X
						LDA		(SCREEN_POS_PTR),Y	; GET SCREEN BYTE
.LINE_DRAW_8_TYPE		ORA		ORA_PIXEL_POS,X		; AND PIXEL
						STA		(SCREEN_POS_PTR),Y	; PUT SCREEN BYTE
.PLOT_POINT_EXIT 	RTS 


.DRAW_OBJECT_LINES	LDA 	DIFF_X_MSB
					SEC 
					SBC 	#$08
					STA 	DIFF_X_MSB
					BCS 	b9795
						DEC 	DIFF_X_HSB		
.b9795   			LDA 	DIFF_Y_MSB
					SEC 
					SBC 	#$08
					STA 	DIFF_Y_MSB
					BCS 	b97A0
						DEC 	DIFF_Y_HSB
.b97A0   			LDA 	DIFF_Z_MSB
					SEC 
					SBC 	#$08
					STA 	DIFF_Z_MSB
					BCS 	b97AB
						DEC 	DIFF_Z_HSB
.b97AB   			LDY 	#0
					STY 	POINT_LINE_COUNT 	; start point count
					LDA 	(OBJECT_POINTER),Y 	; object point count
					STA 	NUMBER_OF_POINTS 	; number of points?
					STY 	OBJECT_POINTER_POS 	; store mem pos

.PROCESS_POINT_LOOP		JSR 	PROCESS_POINT_TO_FP		; get point then *16 then convert to 24bit fp
						JSR 	ROTATE_PERS
						LDA 	POINT_LINE_COUNT		; get point count
						INC 	POINT_LINE_COUNT		; inc point count
						CMP 	NUMBER_OF_POINTS	 	; last point yn
						BNE 	PROCESS_POINT_LOOP  	; no so do next
					INC 	OBJECT_POINTER_POS 			; inc mem pos
					LDY 	OBJECT_POINTER_POS 			; get mem pos
					LDA 	(OBJECT_POINTER),Y  		; object line count
					STA 	POINT_LINE_COUNT  			; number of lines?
					BPL 	NEXT_COMPRESSED_LINE		; line count is positive so lines are compressed

.NEXT_LINE				LDY 	OBJECT_POINTER_POS 	; get mem pos
						INY  						; inc mem pos
						LDA 	(OBJECT_POINTER),Y 	; get 1st point into a and x
						TAX
						INY 						; inc mem pos
						LDA 	(OBJECT_POINTER),Y 	; get 2nd point
						STY 	OBJECT_POINTER_POS 	; store 2nd point?
						TAY 						; 2nd point
						JSR 	LINE_DRAW 			; draw line
						DEC 	POINT_LINE_COUNT 	; dec line count
						BMI 	NEXT_LINE 			; last line y/n
					RTS 						; yes

.NEXT_COMPRESSED_LINE 	LDY 	OBJECT_POINTER_POS 	; get mem pos ; first and second point are stored in 1 byte as 2 nibbles
						INY  						; inc mem pos
						STY 	OBJECT_POINTER_POS 	; store mem pos
						LDA 	(OBJECT_POINTER),Y 	; get point count into a and x
						TAX
						AND 	#$0F 				; and with 15 = 1st point
						TAY 						; y = first point
						TXA  						; get backup
						LSR  	A				; /2
						LSR  	A				; /2
						LSR  	A					; /2
						LSR  	A					; /2
						TAX  						; divide by 16 = 2nd point
						JSR 	LINE_DRAW       	; DRAW LINE
						DEC 	POINT_LINE_COUNT 	; dec line count
						BPL 	NEXT_COMPRESSED_LINE ; last line yn
					RTS  						; yes

.CAMERA_OBJECT_DIFF	SEC 						; calc diff between object pos and current pos
					LDA 	OBJ_X_LSB,X		;
					SBC 	CURR_X_LSB		;
					STA 	DIFF_X_LSB		;
					LDA 	OBJ_X_MSB,X		;
					SBC 	CURR_X_MSB		;
					STA 	DIFF_X_MSB		;
					LDA 	OBJ_X_HSB,X		;
					SBC 	CURR_X_HSB		;
					STA 	DIFF_X_HSB		;

					SEC 
					LDA 	OBJ_Y_LSB,X		;
					SBC 	CURR_Y_LSB		;
					STA 	DIFF_Y_LSB		;
					LDA 	OBJ_Y_MSB,X		;
					SBC 	CURR_Y_MSB		;
					STA 	DIFF_Y_MSB		;
					LDA 	OBJ_Y_HSB,X		;
					SBC 	CURR_Y_HSB		;
					STA 	DIFF_Y_HSB		;

					SEC 
					LDA 	OBJ_Z_LSB,X		;
					SBC 	CURR_Z_LSB		;
					STA 	DIFF_Z_LSB		;
					LDA 	OBJ_Z_MSB,X		;
					SBC 	CURR_Z_MSB		;
					STA 	DIFF_Z_MSB		;
					LDA 	OBJ_Z_HSB,X		;
					SBC 	CURR_Z_HSB		;
					STA 	DIFF_Z_HSB		;
					RTS 

.CHK_ABS_DIST	STX 	ab08
				STY 	ab09
				LDA 	DIFF_X_LSB
				LDX 	DIFF_X_MSB
				LDY 	DIFF_X_HSB
				JSR 	ABS_DIFF
				BCS 	b99BD 		; return carry set
				LDA 	DIFF_Y_LSB
				LDX 	DIFF_Y_MSB
				LDY 	DIFF_Y_HSB
				JSR 	ABS_DIFF
				BCS 	b99BD 		; return carry set
				LDA 	DIFF_Z_LSB
				LDX 	DIFF_Z_MSB
				LDY 	DIFF_Z_HSB
				JSR		ABS_DIFF
.b99BD   		RTS

.ABS_DIFF   	BPL 	CONTINUE1 	; calc absolute difference returning carry if too far away?  if number is positive continue ; negate a/x/y which hold lsb/msb/hsb if needed and subtract 08 and 09 from msb/hsb 
				EOR 	#$FF  	; negative so negate LSB
				PHA 			; backup LSB
				TXA       		; negate MSB
				EOR 	#$FF	;
				TAX 			;
				TYA 			; negate HSB
				EOR 	#$FF	;
				TAY 			;
				PLA 			; restore LSB
				CLC 
				ADC 	#$01		; inc LSB
				BCC 	CONTINUE1	; no overlow so continue
					INX 			; inc MSB
					BNE 	CONTINUE1	; no overlow so continue
						INY 			; inc HSB
.CONTINUE1   SEC 				; subtract 
			TXA 
			SBC 	ab08		; subtract from MSB
			TYA 
			SBC 	ab09		; subtract from HSB
			RTS 

.LINE_DRAW		LDA		POINT_CLASSIFICATION,X 		; start point X classification ; LINE_DRAW_04 - DRAW LINE - x is start index - y is end index
				BEQ 	POINT_X_INFRONT 			; start point X onscreen
				BMI 	POINT_X_BEHIND 				; start point X behind

				LDA 	POINT_CLASSIFICATION,Y 		; end point Y classification 
				BEQ 	POINT_X_BEHIND 				; end point Y onscreen
				BMI 	POINT_X_INFRONT 			; end point Y behind	

				LDA 	Z_ROT_TAB_HI,Y 
				EOR 	#$80
				STA 	ab06
				LDA 	Z_ROT_TAB_HI,X
				EOR 	#$80
				CMP 	ab06
				BCS	 	POINT_X_INFRONT

.POINT_X_BEHIND 	TYA 			; start X is behind so swap start X and end Y pointer so that Y is behind and X start might be behind? so swap start and end - so Y is behind?
				STX 	ab06	; swap start / end
				TAX 			; swap start / end
				LDY 	ab06	; swap start / end

				LDA 	POINT_CLASSIFICATION,X 	; start point X which was end point Y classification
				BPL 	POINT_X_INFRONT 		; end point X point infront
						RTS 					; both points behind

.POINT_X_INFRONT	LDA 	POINT_CLASSIFICATION,X 	; at least 1 point is in front ; point 1 classification
				STA 	POINT_CLASSIFY_ST
				BMI 	X_ROT_COPY 				; point X is behind so x rotation points need to be copied and pers doesnt
					LDA 	PERS_HOR_TAB,X
					STA 	LINE_START_HOR
					LDA 	PERS_VER_TAB,X
					STA 	LINE_START_VER
					LDA 	H_PERS_FP_LO,X
					STA 	H_PERS_FP_LO_ST
					LDA 	H_PERS_FP_HI,X
					STA 	H_PERS_FP_HI_ST
					LDA 	V_PERS_FP_LO,X
					STA 	V_PERS_FP_LO_ST
					LDA 	V_PERS_FP_HI,X
					STA 	V_PERS_FP_HI_ST

.X_ROT_COPY		LDA 	X_ROT_TAB_LO,X ; a point is behind? so x and y rotation needs to be copied to be used in z clip
				STA 	X_POS_HI
				LDA 	X_ROT_TAB_HI,X
				STA 	X_POS_LO
				LDA 	Y_ROT_TAB_LO,X
				STA 	Y_POS_HI
				LDA 	Y_ROT_TAB_HI,X
				STA 	Y_POS_LO
				LDA 	Z_ROT_TAB_LO,X
				STA 	Z_POS_HI
				LDA 	Z_ROT_TAB_HI,X
				STA 	Z_POS_LO

				LDA 	POINT_CLASSIFICATION,Y
				STA 	POINT_CLASSIFY_EN
				BMI 	Y_ROT_COPY				; Y is behind so y rotation points need to be copied and pers doesnt
					LDA 	PERS_HOR_TAB,Y
					STA 	PERS_HOR_ab6C
					LDA 	PERS_VER_TAB,Y
					STA 	PERS_VER_ab6D
					LDA 	H_PERS_FP_LO,Y
					STA 	H_PERS_FP_LO_EN
					LDA 	H_PERS_FP_HI,Y
					STA 	H_PERS_FP_HI_EN
					LDA 	V_PERS_FP_LO,Y
					STA 	V_PERS_FP_LO_EN
					LDA 	V_PERS_FP_HI,Y
					STA 	V_PERS_FP_HI_EN

.Y_ROT_COPY		LDA 	X_ROT_TAB_LO,Y
				STA 	X_ROTATION_LO
				LDA 	X_ROT_TAB_HI,Y
				STA 	X_ROTATION_HI
				LDA 	Y_ROT_TAB_LO,Y
				STA 	Y_ROTATION_LO
				LDA 	Y_ROT_TAB_HI,Y
				STA 	Y_ROTATION_HI
				LDA 	Z_ROT_TAB_LO,Y
				STA 	Z_ROTATION_LO
				LDA 	Z_ROT_TAB_HI,Y
				STA 	Z_ROTATION_HI
				LDA 	POINT_CLASSIFY_EN
				BMI 	Z_CLIP ; z clip? as end is negative so is behind

.CALC_HOR_VER_DIFF_SLOPE
		LDX H_PERS_FP_LO_ST		; calc hor difference
        LDA H_PERS_FP_HI_ST
        EOR		#FP_SIGN
        STX ab08
        STA ab09
        LDX H_PERS_FP_LO_EN
        LDY H_PERS_FP_HI_EN
        JSR FP_ADD 					; H_PERS_FP_HI_EN + (-H_PERS_FP_HI_ST) ? 
        STX H_PERS_FP_LO_DIFF_ab0A	; h line difference?
        STY H_PERS_FP_HI_DIFF_ab0B
        TYA 
        ASL A
        AND #$02
        STA LINE_TYPE_POINTER 	; sign? *2? ; does the sign show the line direction?

        LDX V_PERS_FP_LO_ST		; calc ver difference
        LDA V_PERS_FP_HI_ST
        EOR 	#FP_SIGN
        STX ab08
        STA ab09
        LDX V_PERS_FP_LO_EN
        LDY V_PERS_FP_HI_EN
        JSR FP_ADD					; V_PERS_FP_HI_EN + (-V_PERS_FP_HI_ST) ? 
        STX V_PERS_FP_LO_DIFF_ab0C	; v line difference?
        STY V_PERS_FP_HI_DIFF_ab0D
        TYA 
        AND #$01				; sign? ; does the sign show the line direction?
        ORA LINE_TYPE_POINTER
        STA LINE_TYPE_POINTER

; perspective h/v ?
		LDA		H_PERS_FP_LO_DIFF_ab0A	; hdiff - calc hdiff / vdiff slope?
		STA		FP_DIVIDE_LO
		LDX		ab09					; vdiff
		LDY		ab08					; vdiff
		LDA		H_PERS_FP_HI_DIFF_ab0B	; hdiff
		JSR		FP_DIVIDE		; 0A-0B / 08-09 -> 68-69 = hdiff / vdiff slope?
		STx		ab68
		STA		ab69

        BMI 	b9B1E
			LDA 	LINE_TYPE_POINTER
			ORA 	#$04
			STA 	LINE_TYPE_POINTER

; perspective v/h ?
			LDA		V_PERS_FP_LO_DIFF_ab0C ; vdiff - calc vdiff / hdiff slope?
			STA		FP_DIVIDE_LO
			LDX		H_PERS_FP_HI_DIFF_ab0B ; hdiff
			LDY		H_PERS_FP_LO_DIFF_ab0A ; hdiff
			LDA		V_PERS_FP_HI_DIFF_ab0D ; vdiff 
			JSR		FP_DIVIDE		; 0A-0B / 0C-0D -> 08-09 = vdiff / hdiff slope?

			BMI 	b9B1E
				LDY 	#$FF		; straight line?	LDA 	#$FF
				JMP 	STORE_LINE_INC

.b9B1E   CLC 
        ADC #$20		; *256 ?
        AND #$FC
        TAY 
        JSR FPTO16BIT
		JMP 	STORE_LINE_INC

.Z_CLIP	LDA		Z_POS_HI			 ; do z_clip  zclip z-clip ; calc z-diff = z-st / z-en?
		STA		FP_DIVIDE_LO
		LDX		Z_ROTATION_HI
		LDY		Z_ROTATION_LO
		LDA		Z_POS_LO
		JSR		FP_DIVIDE		; ZPOS HI-LO / Z_ROTATION HI-LO -> 0A-0B
		STX		Z_CLIP_LO_ab0A
		EOR 	#FP_SIGN				; swap sign?
        STA Z_CLIP_HI_ab0B

		LDA 	X_POS_HI		; calc x start distance based on z
		STA		FP_MULTIPLY_LO
		LDA 	X_POS_LO
		LDY 	Z_CLIP_HI_ab0B
        LDX 	Z_CLIP_LO_ab0A
		JSR		FP_MULTIPLY		; XPOS HI-LO * 0A-0B -> 08-09
        STy 	ab08
        STA 	ab09
		
        LDX X_ROTATION_LO		; calc x end distance based on z
        LDY X_ROTATION_HI
        JSR FP_ADD				; XROT HI-LO + 08-09 -> 0c-0D
        STX Z_CLIP_LO_ab0C
        STY Z_CLIP_HI_ab0D
        TYA 
        EOR Z_POS_LO
        ASL A
        AND #$02
        STA LINE_TYPE_POINTER

		LDA 	Y_POS_HI		; calc y start distance based on z
		STA		FP_MULTIPLY_LO
		LDA 	Y_POS_LO
		LDY 	Z_CLIP_HI_ab0B
        LDX 	Z_CLIP_LO_ab0A
		JSR		FP_MULTIPLY		; Y_POS HI-LO * 0A-0B -> 08-09
        STy 	ab08
        STA 	ab09
				
        LDX Y_ROTATION_LO		; calc y end distance based on z
        LDY Y_ROTATION_HI
        JSR FP_ADD				; YROT HI-LO + 08-09 -> 0A-0B
        STX Z_CLIP_LO_ab0A
        STY Z_CLIP_HI_ab0B
        TYA 
        EOR Z_POS_LO
        AND #$01
        ORA LINE_TYPE_POINTER
        STA LINE_TYPE_POINTER

; perspective h/v ?
		LDA		Z_CLIP_LO_ab0C
		STA		FP_DIVIDE_LO
		LDX		ab09
		LDY		ab08
		LDA		Z_CLIP_HI_ab0D
		JSR		FP_DIVIDE		; 0C-0D / 08-09 -> 68-69
		STX		ab68
		STA		ab69

        BMI 	b9C05
			LDA 	LINE_TYPE_POINTER
			ORA 	#$04
			STA 	LINE_TYPE_POINTER

; perspective v/h ?
			LDA		Z_CLIP_LO_ab0A
			STA		FP_DIVIDE_LO
			LDX		Z_CLIP_HI_ab0D
			LDY		Z_CLIP_LO_ab0C
			LDA		Z_CLIP_HI_ab0B
			JSR		FP_DIVIDE		; 0A-0B / 0C-0D -> 08-09
	
			BMI 	b9C05
				LDY 	#$FF			; straight line?
				BNE 	STORE_LINE_INC ; jump

.b9C05   CLC 
        ADC #$20	; *256 ?
        AND #$FC
        TAY 
        JSR 	FPTO16BIT

.STORE_LINE_INC	STY 	LINE_INC

; LINE_TYPE_POINTER FORMAT 
; bit 2 = slope direction?
; bit 1 = H diff sign?
; bit 0 = V diff sign?

.GET_LINE_TYPE	LDX 	LINE_TYPE_POINTER    	; GET LINE TYPE 0-7
				LDY 	LINE_DRAW_CONV_PTR,X 	; CONVERT LINE TYPE TO POINTER TYPE
				LDA 	LINE_DRAW_PTR_LO,Y 		; GET POINTER TO CORRECT LINE
				STA 	LINE_JP_PTR +1    		; STORE AT JUMP TABLE ADDRESS
				LDA 	LINE_DRAW_PTR_HI,Y		; GET POINTER TO CORRECT LINE
				STA 	LINE_JP_PTR +2    		; STORE AT JUMP TABLE ADDRESS
				LDA 	POINT_CLASSIFY_EN
				BEQ 	END_ONSCREEN
				LDA 	PERS_HOR_EDGE,Y			; Y is line type
				STA 	PERS_HOR_ab6C
				LDA 	PERS_VER_EDGE,Y			; y is line type
				STA 	PERS_VER_ab6D

.END_ONSCREEN   	LDA 	POINT_CLASSIFY_ST
				BNE 	ST_OFF_SCRN				; point off screen
					JMP		LINE_DRW_JP

.ST_OFF_SCRN LDA V_PERS_FP_HI_ST
        ASL A
        ASL A
        AND #$04
        ORA POINT_CLASSIFY_ST
        STA ab06
        LDA H_PERS_FP_HI_ST
        ASL A
        ASL A
        ASL A
        AND #$08
        ORA ab06
        TAX 
        TYA 
        ORA fBBE8,X
        LSR	A 
        TAX 
        LDA fBBF8,X
        BNE b9C56
			RTS 

.b9C56   STA POINT_CLASSIFY_ST
        LDA POINT_CLASSIFY_EN
        BMI b9C7C					; end is behind so needs clipped?
			LDA V_PERS_FP_HI_EN
			ASL A
			ASL A
			AND #$04
			ORA POINT_CLASSIFY_EN
			STA ab06
			LDA H_PERS_FP_HI_EN
			ASL A
			ASL A
			ASL A
			AND #$08
			ORA ab06
			TAX 
			TYA 
			ORA fBBE8,X
			LSR A
			TAX 
			LDA fBC18,X
			BNE b9C7C
				RTS 

; IF BOTH POINTS ARE OFFSCREEN THEN GETS TO HERE
; ONLY DOES THIS CODE WHEN BOTH POINT ARE OFF SCREEN AND ONE OF THEM NEEDS TO BE CLIPPED ONSCREEN?

.b9C7C   LDA 	POINT_CLASSIFY_ST ; CHECK TOP?
        AND 	#$01 ; off this edge
        BEQ 	b9CC1 ; no
			LDX 	#ab92_VAL ; ab92 ; yes
			LDA 	#ab93_VAL ; ab93
			SEC 
			SBC 	#FOCUS ; ab21
			STX 	ab08
			STA 	ab09
			LDX 	H_PERS_FP_LO_ST
			LDA 	H_PERS_FP_HI_ST
			EOR 	#FP_SIGN			; swap sign
			TAY 
			JSR 	FP_ADD			; 5E-5F + 92-93 -> 68-69 (due to sign swap its actually fp minus?)
			LDX 	ab68
			LDY 	ab69
			JSR 	FPMUL			; 92-93 * 68-69 -> 60-61
			LDX 	V_PERS_FP_LO_ST
			LDY 	V_PERS_FP_HI_ST
			JSR 	FP_ADD			; 68-69 + 60-61 -> 
			TYA 
			CLC 
			ADC 	#FOCUS ; ab21
			BVS 	b9CC1
				TAY 
				JSR 	FPTO8BIT
				BCS 	b9CC1
				CLC 
				ADC 	#SCR_VER_MID
				BMI 	b9CC1				; CLIP_TEST_BOTTOM
					STA 	LINE_START_VER
					LDA 	#CLIP_RIGHT 		; clip right ?
					STA 	LINE_START_HOR 		; clip right ?
					JMP 	LINE_DRW_JP

.b9CC1   LDA 	POINT_CLASSIFY_ST
        AND 	#$02 ; off this edge
        BEQ 	b9D06 ; no
			LDX 	#ab96_VAL ; ab96 ; yes
			LDA 	#ab97_VAL ; ab97
			SEC 
			SBC 	#FOCUS ; ab21
			STX 	ab08
			STA 	ab09
			LDX 	V_PERS_FP_LO_ST
			LDA 	V_PERS_FP_HI_ST
			EOR 	#FP_SIGN				; sign swap?
			TAY 
			JSR 	FP_ADD				; 60-61 + 96-97 -> 68-69 (due to sign swap actually fp minus?)
			LDX 	ab68
			LDY 	ab69
			JSR 	FPDIV				; 96-97 / 68-69 -> 5E-5F
			LDX 	H_PERS_FP_LO_ST
			LDY 	H_PERS_FP_HI_ST
			JSR 	FP_ADD				; 5E-5F + 96-97 ->
			TYA 
			CLC 
			ADC 	#FOCUS ; ab21
			BVS 	b9D06
				TAY 
				JSR 	FPTO8BIT
				BCS 	b9D06
				CLC 
				ADC 	#SCR_HOR_MID
				BMI 	b9D06					; CLIP_TEST_RIGHT
					STA 	LINE_START_HOR
					LDA 	#CLIP_BOTTOM 			; clip bottom
					STA 	LINE_START_VER 			; clip bottom
					JMP 	LINE_DRW_JP

.b9D06   LDA 	POINT_CLASSIFY_ST ; LINE DRAW
        AND 	#$04 ; off this edge
        BEQ 	b9D4B ; no
			LDX 	#ab90_VAL ; ab90 ; yes
			LDA 	#ab91_VAL ; ab91
			SEC 
			SBC 	#FOCUS ; ab21
			STX 	ab08
			STA 	ab09
			LDX 	H_PERS_FP_LO_ST
			LDA 	H_PERS_FP_HI_ST
			EOR 	#FP_SIGN				; swap sign?
			TAY 
			JSR 	FP_ADD				; 90-91 + 5E+5F -> 68-69 (its actually fp minus due to sign swap?)
			LDX 	ab68
			LDY 	ab69
			JSR 	FPMUL				; 68-69 * 90-91 -> 60-61
			LDX 	V_PERS_FP_LO_ST
			LDY 	V_PERS_FP_HI_ST
			JSR 	FP_ADD				; 60-61 + 90-91 -> 
			TYA 
			CLC 
			ADC 	#FOCUS ; ab21
			BVS 	b9D4B
				TAY 
				JSR 	FPTO8BIT
				BCS 	b9D4B
				CLC 
				ADC 	#SCR_VER_MID
				BMI 	b9D4B				; CLIP_TEST_BOTTOM
					STA 	LINE_START_VER
					LDA 	#CLIP_TEST_TOP 	; clip off top
					STA 	LINE_START_HOR 	; clip off top 
					JMP 	LINE_DRW_JP

.b9D4B   LDA 	POINT_CLASSIFY_ST ; LINE DRAW
        AND 	#$08 ; off this edge
		BEQ 	b9D98 ; no
			LDX 	#ab94_VAL ; ab94 ; yes
			LDA 	#ab95_VAL ; ab95
			SEC 
			SBC 	#FOCUS ; ab21
			STX 	ab08
			STA 	ab09
			LDX 	V_PERS_FP_LO_ST
			LDA 	V_PERS_FP_HI_ST
			EOR 	#FP_SIGN				; sign swap
			TAY 
			JSR 	FP_ADD				; 94-95 + 60-61 -> 68-69 (due to sign swap actually fp minus?)
			LDX 	ab68
			LDY 	ab69
			JSR 	FPDIV				; 94-95 / 68-69 -> 5E-5F
			LDX 	H_PERS_FP_LO_ST
			LDY 	H_PERS_FP_HI_ST
			JSR 	FP_ADD				; 94-95 + 5E-5F ->
			TYA 
			CLC 
			ADC 	#FOCUS ; ab21
			BVS 	b9D98
				TAY 
				JSR 	FPTO8BIT
				BCS 	b9D98
				CLC 
				ADC 	#SCR_HOR_MID
				BMI 	b9D98				; CLIP_TEST_RIGHT
					STA 	LINE_START_HOR
					LDA 	#CLIP_TEST_LEFT 	; clip off left
					STA 	LINE_START_VER 		; clip off left

.LINE_DRW_JP	LDX 	LINE_START_HOR
			LDY 	LINE_START_VER
.LINE_JP_PTR	JMP 	$ABCD 					; LINE_DRAW_01
.b9D98   	RTS 


.FORWARD_BACK	PHA 			; push sign/direction
				LDX 	SPEED_LO
				EOR 	SPEED_HI 	; set direction
				STX 	ab08
				STA 	ab09
				LDX 	COS_LO
				LDY 	COS_HI
				JSR 	FPMUL
				JSR 	FPTO16BIT
				TYA 
				CLC 
				ADC 	CURR_Z_LSB
				STA 	CURR_Z_LSB
				TXA 
				ADC 	CURR_Z_MSB
				STA 	CURR_Z_MSB
				TXA 
				
; Constant-time version, but destroys the carry - wont work?
;            ASL A           ; sign bit into carry; use CPX etc. if using X reg;	or		 cpx #$80             // copy the sign bit into c
;            LDA #$00
;            ADC #$FF        ; C set:   A = $FF + C = $00 /  C clear: A = $FF + C = $FF
;            EOR #$FF        ; Flip all bits and they all now match C
				
				BPL 	_Z_PLUS 		; if +
					LDA 	#$FF		; set to 255 for -
					BMI 	_Z_NEG_CONT	; jmp
._Z_PLUS   		LDA 	#$00			; set to 0 for +
._Z_NEG_CONT 	ADC 	CURR_Z_HSB		; either add or sub depending on + or -
				STA 	CURR_Z_HSB

				PLA 			; pop sign
				LDX 	SPEED_LO
				EOR 	SPEED_HI	; set direction
				STX 	ab08
				STA 	ab09
				LDX 	SIN_LO
				LDY 	SIN_HI
				JSR 	FPMUL
				JSR 	FPTO16BIT
				TYA 
				CLC 
				ADC 	CURR_X_LSB
				STA 	CURR_X_LSB
				TXA 
				ADC 	CURR_X_MSB
				STA 	CURR_X_MSB
				TXA 
				BPL 	_X_PLUS			; positive
					LDA 	#$FF		; negative so set to 255
					BMI 	_X_NEG_CONT ; jump to calc
._X_PLUS	   		LDA 	#$00			; positive so set to 0
._X_NEG_CONT    	ADC 	CURR_X_HSB		; calc
				STA 	CURR_X_HSB
				RTS 

.SIN_COS	LDX 	ROT_LO
        LDY 	ROT_HI
        JSR 	SIN_FP
        STX 	SIN_LO ; sin_lo
        STY 	SIN_HI ; sin_hi
        LDX 	ROT_LO
        LDY 	ROT_HI
        JSR 	COS_FP
        STX 	COS_LO ; cos_lo
        STY 	COS_HI ; cos_hi
        RTS 

.PROCESS_POINT_TO_FP	LDY 	OBJECT_POINTER_POS 	; get point then *16 then conveert to 24bit fp
					INY 
					LDA 	(OBJECT_POINTER),Y
					STA 	OBJ_PNT_X_LSB
					INY 
					LDA 	(OBJECT_POINTER),Y
					STA 	OBJ_PNT_Y_LSB
					INY 
					LDA 	(OBJECT_POINTER),Y
					STA 	OBJ_PNT_Z_LSB
					STY 	OBJECT_POINTER_POS

					LDA 	#0				; MULTIPLY POINT *16 ?
					STA 	OBJ_PNT_X_MSB
					STA 	OBJ_PNT_Y_MSB
					STA 	OBJ_PNT_Z_MSB

					LDA 	OBJ_PNT_X_LSB
					EOR 	#$80 			; reverse the sign?
					ASL 	A
					ROL 	OBJ_PNT_X_MSB 	; *2
					ASL 	A
					ROL 	OBJ_PNT_X_MSB 	; *4
					ASL 	A
					ROL 	OBJ_PNT_X_MSB 	; *8
					ASL 	A
					ROL 	OBJ_PNT_X_MSB 	; *16
					STA 	OBJ_PNT_X_LSB

					CLC
					LDA 	DIFF_X_LSB
					ADC 	OBJ_PNT_X_LSB
					TAY
					LDA 	DIFF_X_MSB
					ADC 	OBJ_PNT_X_MSB
					TAX 
					LDA 	DIFF_X_HSB
					ADC 	#$00
					JSR 	BIT24TOFP
					STY 	X_POS_HI
					STA 	X_POS_LO

					LDA 	OBJ_PNT_Y_LSB
					EOR 	#$80
					ASL 	A
					ROL 	OBJ_PNT_Y_MSB
					ASL 	A
					ROL 	OBJ_PNT_Y_MSB
					ASL 	A
					ROL 	OBJ_PNT_Y_MSB
					ASL 	A
					ROL 	OBJ_PNT_Y_MSB
					STA 	OBJ_PNT_Y_LSB

					CLC 
					LDA 	DIFF_Y_LSB
					ADC 	OBJ_PNT_Y_LSB
					TAY
					LDA 	DIFF_Y_MSB
					ADC 	OBJ_PNT_Y_MSB
					TAX 
					LDA 	DIFF_Y_HSB
					ADC 	#$00
					JSR 	BIT24TOFP
					STY 	Y_POS_HI
					STA 	Y_POS_LO

					LDA 	OBJ_PNT_Z_LSB
					EOR 	#$80
					ASL 	A
					ROL 	OBJ_PNT_Z_MSB
					ASL 	A
					ROL 	OBJ_PNT_Z_MSB
					ASL 	A
					ROL 	OBJ_PNT_Z_MSB
					ASL 	A
					ROL 	OBJ_PNT_Z_MSB
					STA 	OBJ_PNT_Z_LSB

					CLC 
					LDA 	DIFF_Z_LSB
					ADC 	OBJ_PNT_Z_LSB
					TAY
					LDA 	DIFF_Z_MSB
					ADC 	OBJ_PNT_Z_MSB
					TAX 
					LDA 	DIFF_Z_HSB
					ADC 	#$00
					JSR 	BIT24TOFP
					STY 	Z_POS_HI
					STA 	Z_POS_LO

					RTS 

.ROTATE_PERS LDA 	SIN_LO
			STA		FP_MULTIPLY_LO
			LDA 	SIN_HI
			LDX		X_POS_HI
			LDY		X_POS_LO
			JSR		FP_MULTIPLY			; X * SIN
			STy 	X_ROT_LO
			STA		X_ROT_HI

			LDA 	COS_LO
			STA		FP_MULTIPLY_LO
			LDA 	COS_HI
			LDX		Z_POS_HI
			LDY		Z_POS_LO
			JSR		FP_MULTIPLY			; Z * COS
			STy 	Z_ROT_LO
			STA		Z_ROT_HI

			LDX 	X_ROT_LO
			LDY 	X_ROT_HI
			JSR 	FP_ADD				; Z = (X * SIN) + (Z * COS)
			STX 	Z_ROTATION_LO		; Z rotation result?
			TYA 
			EOR 	#FP_SIGN				; swap sign?
			STA 	Z_ROTATION_HI		; Z rotation result?
			AND 	#$01				; keep fp sign
			LSR 	A					; shift sign
			ROR 	A					; rotate sign into top bit
			STA 	POINT_CLASSIFY_ST	; so -ve = $80 / +ve = $00 ? = behind point classification?

			LDA 	COS_LO
			STA		FP_MULTIPLY_LO
			LDA 	COS_HI
			LDX		X_POS_HI
			LDY		X_POS_LO
			JSR		FP_MULTIPLY			; X * COS
			STy 	X_ROT_LO
			STA		X_ROT_HI

			LDA 	SIN_LO
			STA		FP_MULTIPLY_LO
			LDA 	SIN_HI
			LDX		Z_POS_HI
			LDY		Z_POS_LO
			JSR		FP_MULTIPLY			; Z * SIN
			STy 	Z_ROT_LO

			EOR 	#FP_SIGN 				; -Z * SIN
			STA 	Z_ROT_HI

			LDX 	X_ROT_LO
			LDY 	X_ROT_HI
			JSR 	FP_ADD 				; X = (X * COS) + (-Z * SIN)
			STX 	X_ROTATION_LO		; X rotation result?
			TYA 
			STA 	X_ROTATION_HI		; X rotation result?

        LDY POINT_CLASSIFY_ST	; z classification? (see #1 above) 0 is infront - !0 is behind
        BNE bAA54				; behind?
        STA ab09

		LDA		Z_ROTATION_LO
		STA		FP_DIVIDE_LO
		LDX		ab09
		LDY		ab08
		LDA		Z_ROTATION_HI
		JSR		FP_DIVIDE		; Z_ROTATION LO-HI / 08-09 -> 5E-5F
		STX		H_PERS_FP_LO_ST			; hor pers pos
		STA		H_PERS_FP_HI_ST			; hor pers pos

        CLC 
        ADC #FOCUS ; ab21		; add focus to mantisa?
        BVS bAA50
			TAY 					; copy to mantisa?
			JSR FPTO8BIT			; convert X/Y back to 
			BCS bAA50
			ADC #SCR_HOR_MID		; add hor middle
			STA LINE_START_HOR
			BPL bAA54 				; CLIP_TEST_RIGHT
.bAA50   		LDA 	#PNT_OFF_HOR		; PNT_OFF_RIGHT 		; right clip
				STA 	POINT_CLASSIFY_ST
.bAA54   LDX Y_POS_HI
        LDA Y_POS_LO
        EOR #FP_SIGN				; reverses the sign?
        TAY 
        STX ab08
        STY ab09
;        JMP jA8E9

.jA8E9   STX 	Y_ROTATION_LO
        TYA 
        CLC 
        ADC #$04
        BVS bA8F2
			TAY 
.bA8F2   STY 	Y_ROTATION_HI
        STY ab09
        LDA 	POINT_CLASSIFY_ST
        BMI 	bA933
		
			LDA		Z_ROTATION_LO
			STA		FP_DIVIDE_LO
			LDX		ab09
			LDY		ab08
			LDA		Z_ROTATION_HI
			JSR		FP_DIVIDE		; Z_ROTATION LO-HI / 08-09 -> 60-61
			STX		V_PERS_FP_LO_ST			; ver pers pos
			STA		V_PERS_FP_HI_ST			; ver pers pos

			CLC 
			ADC 	#FOCUS ; ab21
			BVS 	bA92D
				TAY 
				JSR 	FPTO8BIT
				BCS 	bA92D
				ADC 	#SCR_VER_MID
				STA 	LINE_START_VER
				BPL 	bA933				; CLIP_TEST_BOTTOM
.bA92D   			LDA 	POINT_CLASSIFY_ST
					ORA 	#PNT_OFF_VER
					STA 	POINT_CLASSIFY_ST
.bA933   LDY 	POINT_LINE_COUNT						; point position?
        LDA 	POINT_CLASSIFY_ST
        STA 	POINT_CLASSIFICATION,Y
        BMI 	bA95A					; behind
			LDA 	H_PERS_FP_LO_ST
			STA 	H_PERS_FP_LO,Y	; pers point?
			LDA 	H_PERS_FP_HI_ST
			STA 	H_PERS_FP_HI,Y	; pers point?
			LDA 	V_PERS_FP_LO_ST
			STA 	V_PERS_FP_LO,Y	; pers point?
			LDA 	V_PERS_FP_HI_ST
			STA 	V_PERS_FP_HI,Y	; pers point?
			LDA 	LINE_START_HOR 			; get perspective ver
			STA 	PERS_HOR_TAB,Y 				; put perspective ver
			LDA 	LINE_START_VER 			; get perspective hor
			STA 	PERS_VER_TAB,Y 				; put perspective hor
.bA95A   LDA 	X_ROTATION_LO
        STA 	X_ROT_TAB_LO,Y 	; X rotation result x pos lo
        LDA 	X_ROTATION_HI
        STA 	X_ROT_TAB_HI,Y 	; X rotation result x pos hi
        LDA 	Y_ROTATION_LO
        STA 	Y_ROT_TAB_LO,Y	; rotation result y pos lo
        LDA 	Y_ROTATION_HI
        STA 	Y_ROT_TAB_HI,Y	; rotation result y pos hi
        LDA 	Z_ROTATION_LO
        STA 	Z_ROT_TAB_LO,Y	; rotation result z pos lo
        LDA 	Z_ROTATION_HI
        STA 	Z_ROT_TAB_HI,Y	; rotation result z pos hi
        RTS 

.LINE_DRAW_0_LOOP  	INX 
					LDA 	LINE_FRACTION_ab08
					ADC 	LINE_INC
					STA 	LINE_FRACTION_ab08
					BCC 	LINE_DRAW_0_CONT
						INY
						BMI 	LINE_DRAW_0_EXIT
.LINE_DRAW_RADS_0		LDA 	VER_POS_LO,Y		; LINE_DRAW_0 - right always down sometimes	- RADS
						STA 	SCREEN_POS_LO
.LINE_DRAW_0_SCN		LDA 	VER_POS_SCN0_HI,Y
						STA 	SCREEN_POS_HI
						STY 	ab06
.LINE_DRAW_0_CONT	LDY 	HOR_SCR_POS,X
					LDA 	(SCREEN_POS_PTR),Y
.LINE_DRAW_0_TYPE	ORA 	ORA_PIXEL_POS,X
					STA 	(SCREEN_POS_PTR),Y
					LDY 	ab06
					CPX 	PERS_HOR_ab6C
					BNE 	LINE_DRAW_0_LOOP
.LINE_DRAW_0_EXIT   RTS 

.LINE_DRAW_1_LOOP   INY
					LDA 	LINE_FRACTION_ab08
					ADC 	LINE_INC
					STA 	LINE_FRACTION_ab08
					BCC 	LINE_DRAW_RSDA_1
						INX 
						BMI 	LINE_DRAW_1_EXIT
.LINE_DRAW_RSDA_1
;	LDA 	VER_POS_LO,Y			; LINE_DRAW_1 - right sometimes down always - RSDA
;					STA 	SCREEN_POS_LO
.LINE_DRAW_1_SCN	LDA 	VER_POS_SCN0_HI,Y
					STA   	SCN_ZLO+1
					STY 	ab06
					TYA
					AND 	#7
					ORA   	HOR_SCR_POS,X
					TAY
;					STA 	SCREEN_POS_HI
;					STY 	ab06
;					LDY 	HOR_SCR_POS,X
					LDA 	(SCN_ZLO),Y
.LINE_DRAW_1_TYPE	ORA 	ORA_PIXEL_POS,X
					STA 	(SCN_ZLO),Y
					LDY 	ab06
					CPY 	PERS_VER_ab6D
					BNE 	LINE_DRAW_1_LOOP
.LINE_DRAW_1_EXIT   	RTS 

.LINE_DRAW_2_LOOP   	INY
					LDA 	LINE_FRACTION_ab08
					ADC 	LINE_INC
					STA 	LINE_FRACTION_ab08
					BCC 	LINE_DRAW_LSDA_2
						DEX 
						BMI 	LINE_DRAW_2_EXIT
.LINE_DRAW_LSDA_2
;		LDA 	VER_POS_LO,Y		; LINE_DRAW_2 - left sometimes down always	- LSDA
;					STA 	SCREEN_POS_LO
.LINE_DRAW_2_SCN		LDA 	VER_POS_SCN0_HI,Y
					STA   	SCN_ZLO+1
;					STA 	SCREEN_POS_HI
					STY 	ab06
					TYA
					AND 	#7
					ORA   	HOR_SCR_POS,X
					TAY

;					LDY 	HOR_SCR_POS,X
					LDA 	(SCN_ZLO),Y
.LINE_DRAW_2_TYPE	ORA 	ORA_PIXEL_POS,X
					STA 	(SCN_ZLO),Y
					LDY 	ab06
					CPY 	PERS_VER_ab6D
					BNE 	LINE_DRAW_2_LOOP
.LINE_DRAW_2_EXIT   	RTS 

.LINE_DRAW_3_LOOP  	DEX
					LDA 	LINE_FRACTION_ab08
					SBC 	LINE_INC
					STA 	LINE_FRACTION_ab08
					BCS 	LINE_DRAW_3_CONT
						INY 
						BMI 	LINE_DRAW_3_EXIT
.LINE_DRAW_LADS_3		LDA 	VER_POS_LO,Y		; LINE_DRAW_3 - left always down sometimes	- LADS
						STA 	SCREEN_POS_LO
.LINE_DRAW_3_SCN			LDA 	VER_POS_SCN0_HI,Y
						STA 	SCREEN_POS_HI
						STY 	ab06
.LINE_DRAW_3_CONT	LDY 	HOR_SCR_POS,X
					LDA 	(SCREEN_POS_PTR),Y
.LINE_DRAW_3_TYPE	ORA 	ORA_PIXEL_POS,X
					STA 	(SCREEN_POS_PTR),Y
					LDY 	ab06
					CPX 	PERS_HOR_ab6C
					BNE 	LINE_DRAW_3_LOOP
.LINE_DRAW_3_EXIT   	RTS 

.LINE_DRAW_4_LOOP   	DEX
					LDA 	LINE_FRACTION_ab08
					SBC 	LINE_INC
					STA 	LINE_FRACTION_ab08
					BCS 	LINE_DRAW_4_CONT
						DEY 
						BMI 	LINE_DRAW_4_EXIT
.LINE_DRAW_LAUS_4		LDA 	VER_POS_LO,Y	; LINE_DRAW_4 - left always up sometimes	- LAUS
						STA 	SCREEN_POS_LO
.LINE_DRAW_4_SCN			LDA 	VER_POS_SCN0_HI,Y
						STA 	SCREEN_POS_HI
						STY 	ab06
.LINE_DRAW_4_CONT	LDY 	HOR_SCR_POS,X
					LDA 	(SCREEN_POS_PTR),Y
.LINE_DRAW_4_TYPE  	ORA 	ORA_PIXEL_POS,X
					STA 	(SCREEN_POS_PTR),Y
					LDY 	ab06
					CPX 	PERS_HOR_ab6C
					BNE 	LINE_DRAW_4_LOOP
.LINE_DRAW_4_EXIT   	RTS 

.LINE_DRAW_5_LOOP   	DEY
					LDA 	LINE_FRACTION_ab08
					ADC 	LINE_INC
					STA 	LINE_FRACTION_ab08
					BCC 	LINE_DRAW_LSUA_5
						DEX 
						BMI 	LINE_DRAW_5_EXIT
.LINE_DRAW_LSUA_5
;		LDA 	VER_POS_LO,Y	; LINE_DRAW_5 - left sometimes up always	- LSUA
;					STA 	SCREEN_POS_LO
.LINE_DRAW_5_SCN		LDA 	VER_POS_SCN0_HI,Y
					STA   	SCN_ZLO+1
					STY 	ab06
					TYA
					AND 	#7
					ORA   	HOR_SCR_POS,X
					TAY

;					STA 	SCREEN_POS_HI


;					STY 	ab06
;					LDY 	HOR_SCR_POS,X
					LDA 	(SCN_ZLO),Y
.LINE_DRAW_5_TYPE  	ORA 	ORA_PIXEL_POS,X
					STA 	(SCN_ZLO),Y
					LDY 	ab06
					CPY 	PERS_VER_ab6D
					BNE 	LINE_DRAW_5_LOOP
.LINE_DRAW_5_EXIT  	RTS

.LINE_DRAW_6_LOOP   	DEY
					LDA 	LINE_FRACTION_ab08
					SBC 	LINE_INC
					STA 	LINE_FRACTION_ab08
					BCS 	LINE_DRAW_RSUA_6
						INX
						BMI 	LINE_DRAW_6_EXIT
.LINE_DRAW_RSUA_6
;		LDA 	VER_POS_LO,Y		; LINE_DRAW_6 - right sometimes up always	- RSUA
;					STA 	SCREEN_POS_LO
.LINE_DRAW_6_SCN		LDA 	VER_POS_SCN0_HI,Y
					STA   	SCN_ZLO+1
					STY 	ab06
					TYA
					AND 	#7
					ORA   	HOR_SCR_POS,X
					TAY

;					STA 	SCREEN_POS_HI
;					STY 	ab06
;					LDY 	HOR_SCR_POS,X
					LDA 	(SCN_ZLO),Y
.LINE_DRAW_6_TYPE	ORA 	ORA_PIXEL_POS,X
					STA 	(SCN_ZLO),Y
					LDY 	ab06
					CPY 	PERS_VER_ab6D
					BNE 	LINE_DRAW_6_LOOP
.LINE_DRAW_6_EXIT	RTS 

.LINE_DRAW_7_LOOP   	INX
					LDA		LINE_FRACTION_ab08
					ADC		LINE_INC
					STA		LINE_FRACTION_ab08
					BCC		LINE_DRAW_7_CONT
						DEY 
						BMI		LINE_DRAW_7_EXIT
.LINE_DRAW_RAUS_7			LDA		VER_POS_LO,Y		; LINE_DRAW_7 - right always up sometimes	- RAUS
						STA		SCREEN_POS_LO
.LINE_DRAW_7_SCN			LDA 	VER_POS_SCN0_HI,Y
						STA 	SCREEN_POS_HI
						STY 	ab06
.LINE_DRAW_7_CONT	LDY 	HOR_SCR_POS,X
					LDA 	(SCREEN_POS_PTR),Y
.LINE_DRAW_7_TYPE	ORA 	ORA_PIXEL_POS,X
					STA 	(SCREEN_POS_PTR),Y
					LDY 	ab06
					CPX 	PERS_HOR_ab6C
					BNE 	LINE_DRAW_7_LOOP
.LINE_DRAW_7_EXIT   	RTS 

.COLOUR_SETUP_WHITE	LDA 	#>AND_PIXEL_POS
					LDX 	#$3D				; AND
					BNE 	COLOUR_SETUP 		; JUMP
.COLOUR_SETUP_BLACK	LDA 	#>ORA_PIXEL_POS
					LDX 	#$1D				; ORA
.COLOUR_SETUP		STA 	LINE_DRAW_0_TYPE +2
					STA 	LINE_DRAW_1_TYPE +2
					STA 	LINE_DRAW_2_TYPE +2
					STA 	LINE_DRAW_3_TYPE +2
					STA 	LINE_DRAW_4_TYPE +2
					STA 	LINE_DRAW_5_TYPE +2
					STA 	LINE_DRAW_6_TYPE +2
					STA 	LINE_DRAW_7_TYPE +2
					STA 	LINE_DRAW_8_TYPE +2
					STX 	LINE_DRAW_0_TYPE
					STX 	LINE_DRAW_1_TYPE
					STX 	LINE_DRAW_2_TYPE
					STX 	LINE_DRAW_3_TYPE
					STX 	LINE_DRAW_4_TYPE
					STX 	LINE_DRAW_5_TYPE
					STX 	LINE_DRAW_6_TYPE
					STX 	LINE_DRAW_7_TYPE
					STX 	LINE_DRAW_8_TYPE
					RTS

X_ROT_TAB_LO = $0100 ; UNKNOWN_POINT_TAB_6
X_ROT_TAB_HI = $0140

Y_ROT_TAB_LO = $0180
Y_ROT_TAB_HI = $02C0

Z_ROT_TAB_LO = $0340 
Z_ROT_TAB_HI = $0380

H_PERS_FP_LO = $0600
H_PERS_FP_HI = $03C0

V_PERS_FP_LO = $0700
V_PERS_FP_HI = $0640

PERS_HOR_TAB = $0680
PERS_VER_TAB = $06C0

POINT_CLASSIFICATION = $0740 ; 00 = point is onscreen - 01 = point needs off left/right - 02 = point is off top/bottom - 03 = point is off left/right + top/bottom - 80 / minus = point is behind

OBJ_X_LSB = $0780
OBJ_Y_LSB = $07C0
OBJ_Z_LSB = $0300

.OBJ_X_MSB 
EQUB 00,20,40,60,80,100,120,140,160,180,200,220
SKIP 52
 
.OBJ_Y_MSB  
SKIP 64

.OBJ_Z_MSB   
EQUB 00,20,40,60,80,100,120,140,160,180,200,220
SKIP 52

.OBJ_X_HSB   
SKIP 64

.OBJ_Y_HSB
SKIP 64

.OBJ_Z_HSB
SKIP 64

.OBJECTS_TO_DRAW   ; objects to draw
		EQUB 00
		EQUB 01 ; object being drawn
		EQUB 02,03,04,05,06,07
        EQUB 08,09,10,11,12,13,14,15
        EQUB 00,00,00,00,00,00,00,00
        EQUB 00,00,00,00,00,00,00,00
        EQUB 00,00,00,00,00,00,00,00
        EQUB 00,00,00,00,00,00,00,00
        EQUB 00,00,00,00,00,00,00,00
        EQUB 00,00,00,00,00,00,00,00

.OBJECT_SHAPE_PTR_LO   
 EQUB <OBJ1,<OBJ1,<OBJ3,<OBJ3,<OBJ4,<OBJ5,<OBJ6,<OBJ7
 EQUB <OBJ8,<OBJ2,<OBJ9,<OBJ1,<OBJ10,<OBJ1,<OBJ8,<OBJ4
 EQUB <OBJ11,<OBJ12,<OBJ13,<OBJ14,<OBJ15,<OBJ16,<OBJ17,<OBJ18
 EQUB <OBJ19,<OBJ20,<OBJ21,<OBJ22,<OBJ23,<OBJ24,<OBJ25,<OBJ26
 EQUB <OBJ27,<OBJ28,<OBJ29,<OBJ30,<OBJ31,<OBJ32,<OBJ33,<OBJ34
 EQUB <OBJ35,<OBJ36,<OBJ37,<OBJ38,<OBJ39,<OBJ40,<OBJ41,<OBJ42
 EQUB <OBJ43,<OBJ44,<OBJ45,<OBJ46,<OBJ47,<OBJ48,<OBJ49,<OBJ50
 EQUB <OBJ51,<OBJ52,<OBJ53,<OBJ53,<OBJ52,<OBJ54,<OBJ54,<OBJ55

.OBJECT_SHAPE_PTR_HI   
 EQUB >OBJ1,>OBJ1,>OBJ3,>OBJ3,>OBJ4,>OBJ5,>OBJ6,>OBJ7
 EQUB >OBJ8,>OBJ2,>OBJ9,>OBJ1,>OBJ10,>OBJ1,>OBJ8,>OBJ4
 EQUB >OBJ11,>OBJ12,>OBJ13,>OBJ14,>OBJ15,>OBJ16,>OBJ17,>OBJ18
 EQUB >OBJ19,>OBJ20,>OBJ21,>OBJ22,>OBJ23,>OBJ24,>OBJ25,>OBJ26
 EQUB >OBJ27,>OBJ28,>OBJ29,>OBJ30,>OBJ31,>OBJ32,>OBJ33,>OBJ34
 EQUB >OBJ35,>OBJ36,>OBJ37,>OBJ38,>OBJ39,>OBJ40,>OBJ41,>OBJ42
 EQUB >OBJ43,>OBJ44,>OBJ45,>OBJ46,>OBJ47,>OBJ48,>OBJ49,>OBJ50
 EQUB >OBJ51,>OBJ52,>OBJ53,>OBJ53,>OBJ52,>OBJ54,>OBJ54,>OBJ55

.OBJ53
	EQUB $11,$FD,$08,$00
  EQUB $03,$04,$00,$FD,$04,$00,$03,$08
  EQUB $00,$FF,$07,$00,$01,$07,$00,$02
  EQUB $09,$00,$02,$0A,$00,$01,$0B,$00
  EQUB $FF,$0B,$00,$FE,$0A,$00,$FE,$09
  EQUB $00,$FF,$0A,$00,$FF,$09,$00,$01
  EQUB $0A,$00,$01,$09,$00,$FF,$08,$00
  EQUB $01,$08,$00,$8C,$00,$01,$02,$03
  EQUB $04,$05,$05,$06,$06,$07,$07,$08
  EQUB $08,$09,$09,$0A,$0A,$0B,$0B,$04
  EQUB $0C,$0D,$0E,$0F,$10,$11
.OBJ5
  EQUB $0C,$FE
  EQUB $00,$E0,$02,$00,$E0,$06,$00,$10
  EQUB $FA,$00,$10,$FC,$04,$E8,$04,$04
  EQUB $E8,$06,$06,$16,$FA,$06,$16,$00
  EQUB $04,$E8,$00,$0A,$1A,$00,$06,$16
  EQUB $10,$00,$20,$F0,$00,$20,$15,$01
  EQUB $12,$23,$30,$45,$56,$67,$74,$15
  EQUB $04,$26,$37,$89,$9A,$A8,$1B,$B2
  EQUB $0C,$C3,$6B,$7C,$BC,$0B,$FA,$00
  EQUB $FF,$FA,$00,$02,$FD
.OBJ27
  EQUB $00,$03,$06
  EQUB $00,$02,$06,$00,$FF,$FD,$00,$FE
  EQUB $FA,$03,$FF,$FA,$03,$02,$FD,$03
  EQUB $03,$06,$03,$02,$06,$03,$FF,$FD
  EQUB $03,$FE,$11,$01,$12,$23,$34,$45
  EQUB $50,$67,$78,$89,$9A,$AB,$06,$17
  EQUB $28,$39,$4A,$5B,$B6
.OBJ32
	EQUB $0B,$06,$07
  EQUB $00,$06,$0C,$00,$06,$0F,$00,$FA
  EQUB $0F,$00,$FA,$07,$00,$04,$0B,$00
  EQUB $02,$0D,$00,$00,$09,$00,$FE,$0B
  EQUB $00,$FC,$0A,$00,$FB,$07,$00,$FA
  EQUB $04,$00,$0A,$02,$23,$34,$40,$15
  EQUB $56,$67,$78,$89,$9A,$AB
.OBJ43
	EQUB $12,$FA
  EQUB $10,$00,$FC,$0E,$00,$FE,$0C,$00
  EQUB $01,$09,$00,$05,$10,$00,$04,$0E
  EQUB $00,$03,$0C,$00,$08,$09,$00,$07
  EQUB $09,$00,$04,$09,$00,$07,$00,$00
  EQUB $05,$03,$00,$03,$06,$00,$FA,$00
  EQUB $00,$FC,$02,$00,$FF,$06,$00,$F8
  EQUB $07,$00,$F9,$07,$00,$FD,$08,$00
  EQUB $93,$00,$03,$04,$03,$07,$03,$0A
  EQUB $03,$0E,$03,$11,$03,$0D,$0E,$10
  EQUB $11,$01,$05,$05,$08,$08,$0B,$0B
  EQUB $0E,$0E,$11,$11,$01,$02,$06,$06
  EQUB $09,$09,$0C,$0C,$0F,$0F,$12,$12
  EQUB $02
.OBJ28
  EQUB $09,$FE,$00,$FE,$00,$00,$02
  EQUB $02,$00,$FE,$00,$01,$00,$00,$03
  EQUB $00,$00,$07,$00,$FE,$06,$FF,$FE
  EQUB $08,$01,$02,$08,$01,$02,$06,$FF
  EQUB $09,$04,$14,$24,$35,$67,$78,$89
  EQUB $96,$68,$79
.OBJ42
	EQUB $16,$F8,$00,$FD,$F8
  EQUB $00,$04,$08,$00,$04,$08,$00,$FD
  EQUB $F8,$07,$FD,$F8,$07,$04,$08,$07
  EQUB $04,$08,$07,$FD,$F9,$07,$FE,$F9
  EQUB $07,$02,$FF,$07,$02,$FF,$07,$FE
  EQUB $F9,$04,$FE,$F9,$04,$02,$FF,$04
  EQUB $02,$FF,$04,$FE,$FC,$07,$03,$FC
  EQUB $09,$03,$FB,$09,$03,$FD,$09,$03
  EQUB $FC,$08,$03,$FC,$08,$01,$FC,$06
  EQUB $01,$9B,$00,$01,$01,$02,$02,$03
  EQUB $03,$00,$04,$05,$05,$06,$06,$07
  EQUB $07,$04,$00,$04,$01,$05,$02,$06
  EQUB $03,$07,$08,$09,$09,$0A,$0A,$0B
  EQUB $0B,$08,$0C,$0D,$0D,$0E,$0E,$0F
  EQUB $0F,$0C,$0C,$08,$0D,$09,$0E,$0A
  EQUB $0F,$0B,$10,$11,$12,$13,$14,$15
  EQUB $15,$16
.OBJ36
	EQUB $13,$FC,$00,$FC,$FC,$00
  EQUB $04,$04,$00,$04,$04,$00,$FC,$FC
  EQUB $0A,$FC,$FC,$0A,$04,$04,$0A,$04
  EQUB $04,$0A,$FC,$FF,$02,$FC,$FF,$04
  EQUB $FC,$FD,$04,$FC,$FD,$06,$FC,$FF
  EQUB $06,$FC,$FF,$08,$FC,$01,$08,$FC
  EQUB $01,$06,$FC,$03,$06,$FC,$03,$04
  EQUB $FC,$01,$04,$FC,$01,$02,$FC,$97
  EQUB $00,$01,$01,$02,$02,$03,$03,$00
  EQUB $04,$05,$05,$06,$06,$07,$07,$04
  EQUB $00,$04,$01,$05,$02,$06,$03,$07
  EQUB $08,$09,$09,$0A,$0A,$0B,$0B,$0C
  EQUB $0C,$0D,$0D,$0E,$0E,$0F,$0F,$10
  EQUB $10,$11,$11,$12,$12,$13,$13,$08
.OBJ37
  EQUB $1F,$FB,$00,$03,$FB,$00,$FD,$05
  EQUB $00,$FD,$05,$00,$03,$FB,$06,$03
  EQUB $FB,$06,$FD,$05,$06,$FD,$05,$06
  EQUB $03,$FC,$02,$FD,$FC,$03,$FD,$FC
  EQUB $04,$FD,$FD,$04,$FD,$FD,$03,$FD
  EQUB $FE,$02,$FD,$FE,$03,$FD,$FE,$04
  EQUB $FD,$FF,$04,$FD,$FF,$03,$FD,$FF
  EQUB $02,$FD,$00,$02,$FD,$00,$03,$FD
  EQUB $00,$04,$FD,$01,$04,$FD,$01,$03
  EQUB $FD,$02,$02,$FD,$02,$03,$FD,$02
  EQUB $04,$FD,$03,$04,$FD,$03,$03,$FD
  EQUB $03,$02,$FD,$04,$02,$FD,$04,$04
  EQUB $FD,$9D,$00,$01,$01,$02,$02,$03
  EQUB $03,$00,$04,$05,$05,$06,$06,$07
  EQUB $07,$04,$00,$04,$01,$05,$02,$06
  EQUB $03,$07,$08,$0A,$0A,$0B,$0B,$0C
  EQUB $0C,$09,$0D,$0F,$0F,$10,$0E,$11
  EQUB $0D,$12,$13,$15,$15,$16,$16,$17
  EQUB $17,$14,$18,$1D,$1D,$1C,$19,$1C
  EQUB $19,$1A,$1A,$1B,$1E,$1F
.OBJ31
	EQUB $0F,$FB
  EQUB $00,$FE,$FB,$00,$02,$00,$00,$02
  EQUB $00,$00,$FE,$FB,$06,$FE,$FB,$06
  EQUB $02,$00,$06,$02,$00,$06,$FE,$FB
  EQUB $09,$FF,$FB,$09,$02,$00,$09,$02
  EQUB $00,$09,$FF,$FB,$0B,$00,$FB,$0B
  EQUB $02,$00,$0B,$02,$00,$0B,$00,$15
  EQUB $01,$12,$23,$30,$45,$56,$67,$74
  EQUB $04,$37,$1D,$2E,$89,$9A,$AB,$B8
  EQUB $CD,$DE,$EF,$FC,$8C,$BF
.OBJ30
	EQUB $07,$F0
  EQUB $00,$F0,$10,$00,$F0,$10,$00,$10
  EQUB $F0,$00,$10,$F0,$10,$F0,$10,$10
  EQUB $F0,$10,$10,$10,$F0,$10,$10,$0B
  EQUB $01,$12,$23,$30,$45,$56,$67,$74
  EQUB $04,$15,$26,$37
.OBJ35
	EQUB $0D,$FC,$00,$00
  EQUB $FE,$05,$00,$FC,$07,$00,$FC,$09
  EQUB $00,$FE,$0B,$00,$02,$0B,$00,$04
  EQUB $09,$00,$04,$07,$00,$02,$05,$00
  EQUB $04,$00,$00,$FD,$08,$00,$FF,$08
  EQUB $00,$01,$08,$00,$03,$08,$00,$0B
  EQUB $01,$12,$23,$34,$45,$56,$67,$78
  EQUB $89,$18,$AB,$CD
.OBJ41
	EQUB $0A,$00,$05,$FE
  EQUB $00,$0C,$FE,$00,$0C,$02,$00,$05
  EQUB $02,$00,$06,$FF,$00,$0B,$FF,$00
  EQUB $0B,$01,$00,$09,$01,$00,$09,$FF
  EQUB $00,$05,$00,$00,$00,$00,$08,$01
  EQUB $12,$23,$30,$45,$56,$67,$78,$9A
  EQUB $00
.OBJ39
	EQUB $0F,$F6,$00,$FA,$F6,$00,$06
  EQUB $0A,$00,$06,$0A,$00,$FA,$F8,$0A
  EQUB $FC,$F8,$0A,$04,$08,$0A,$04,$08
  EQUB $0A,$FC,$F6,$00,$FD,$F6,$00,$03
  EQUB $FA,$00,$03,$FA,$00,$FD,$FA,$00
  EQUB $00,$0A,$00,$00,$04,$00,$FF,$08
  EQUB $00,$FF,$10,$01,$12,$23,$30,$45
  EQUB $56,$67,$74,$04,$15,$26,$37,$9A
  EQUB $AB,$B8,$CD,$EF
.OBJ40
	EQUB $05,$FA,$00,$FA
  EQUB $06,$00,$FA,$06,$00,$06,$FA,$00
  EQUB $06,$FA,$0C,$00,$06,$0C,$00,$08
  EQUB $01,$12,$23,$30,$04,$43,$15,$52
  EQUB $45
.OBJ51
	EQUB $04,$AA,$00,$AA,$AA,$00,$56
  EQUB $56,$00,$56,$56,$00,$AA,$00,$7F
  EQUB $00,$07,$01,$12,$23,$30,$04,$14
  EQUB $24,$34
.OBJ45
	EQUB $0E,$00,$00,$00,$FC,$14
  EQUB $FC,$FC,$14,$04,$04,$14,$04,$04
  EQUB $14,$FC,$FC,$5A,$FC,$FC,$5A,$04
  EQUB $04,$5A,$04,$04,$5A,$FC,$00,$6E
  EQUB $00,$00,$7F,$00,$D8,$00,$D8,$D8
  EQUB $00,$28,$28,$00,$28,$28,$00,$D8
  EQUB $18,$01,$02,$03,$04,$12,$23,$34
  EQUB $41,$56,$67,$78,$85,$15,$26,$37
  EQUB $48,$59,$69,$79,$89,$B5,$C6,$D7
  EQUB $E8,$0A
.OBJ21
	EQUB $07,$FD,$06,$FE,$FD,$06
  EQUB $02,$00,$06,$04,$03,$06,$02,$03
  EQUB $06,$FE,$00,$06,$FC,$00,$06,$00
  EQUB $00,$00,$00,$09,$01,$12,$23,$34
  EQUB $45,$50,$03,$14,$25,$67
.OBJ10 EQUB $15,$FC
  EQUB $00,$FC,$FC,$00,$04,$04,$00,$04
  EQUB $04,$00,$FC,$FE,$09,$FE,$FE,$09
  EQUB $02,$02,$09,$02,$02,$09,$FE,$FE
  EQUB $0B,$FE,$FE,$0B,$02,$02,$0B,$02
  EQUB $02,$0B,$FE,$FF,$0B,$FE,$FF,$09
  EQUB $FE,$01,$0B,$FE,$01,$09,$FE,$00
  EQUB $0B,$FA,$FB,$0B,$03,$05,$0B,$03
  EQUB $00,$0E,$FE,$FE,$0E,$01,$02,$0E
  EQUB $01,$9E,$00,$01,$01,$02,$02,$03
  EQUB $03,$00,$04,$05,$05,$06,$06,$07
  EQUB $07,$04,$08,$09,$09,$0A,$0A,$0B
  EQUB $0B,$08,$0C,$0D,$0E,$0F,$10,$11
  EQUB $11,$12,$12,$10,$13,$14,$14,$15
  EQUB $15,$13,$00,$04,$01,$05,$02,$06
  EQUB $03,$07,$04,$08,$05,$09,$06,$0A
  EQUB $07,$0B,$10,$13,$11,$14,$12,$15
.OBJ22
  EQUB $0B,$FE,$00,$00,$01,$00,$02,$01
  EQUB $00,$FE,$00,$03,$00,$00,$06,$00
  EQUB $FE,$09,$00,$01,$09,$02,$01,$09
  EQUB $FE,$00,$09,$00,$FA,$12,$00,$03
  EQUB $12,$05,$03,$12,$FB,$11,$01,$12
  EQUB $20,$03,$13,$23,$45,$46,$47,$56
  EQUB $67,$75,$89,$8A,$8B,$9A,$AB,$B9
.OBJ24
  EQUB $0F,$00,$04,$FC,$FC,$04,$00,$00
  EQUB $04,$04,$04,$04,$00,$00,$0C,$FC
  EQUB $FC,$0C,$00,$00,$0C,$04,$04,$0C
  EQUB $00,$FE,$06,$FE,$FE,$06,$02,$02
  EQUB $06,$02,$02,$06,$FE,$FE,$0A,$FE
  EQUB $FE,$0A,$02,$02,$0A,$02,$02,$0A
  EQUB $FE,$07,$17,$35,$06,$24,$8E,$AC
  EQUB $9F,$BD
.OBJ47
	EQUB $23,$FA,$00,$F6,$02,$00
  EQUB $F0,$02,$00,$10,$FA,$00,$0A,$FA
  EQUB $0A,$F6,$02,$10,$F0,$02,$10,$10
  EQUB $FA,$0A,$0A,$FA,$04,$F6,$00,$04
  EQUB $FD,$00,$04,$03,$FA,$04,$0A,$00
  EQUB $0A,$FD,$00,$0A,$03,$FB,$05,$F7
  EQUB $FB,$09,$F7,$FF,$09,$FC,$FF,$05
  EQUB $FC,$FF,$05,$04,$FF,$09,$04,$FB
  EQUB $09,$09,$FB,$05,$09,$00,$05,$FE
  EQUB $00,$06,$FE,$00,$06,$FF,$00,$05
  EQUB $FF,$00,$05,$01,$00,$06,$01,$00
  EQUB $06,$02,$00,$05,$02,$00,$07,$FE
  EQUB $00,$07,$02,$00,$08,$FE,$00,$08
  EQUB $02,$00,$09,$FE,$00,$09,$02,$A4
  EQUB $00,$01,$01,$05,$05,$04,$04,$00
  EQUB $03,$07,$07,$06,$06,$02,$02,$03
  EQUB $01,$02,$05,$06,$08,$09,$09,$0A
  EQUB $0A,$0B,$04,$0C,$0C,$0D,$0D,$07
  EQUB $09,$0C,$0A,$0D,$0E,$0F,$0F,$10
  EQUB $10,$11,$11,$0E,$12,$13,$13,$14
  EQUB $14,$15,$15,$12,$16,$17,$17,$18
  EQUB $18,$19,$19,$16,$1A,$1B,$1B,$1C
  EQUB $1C,$1D,$1D,$1A,$1E,$1F,$20,$21
  EQUB $22,$23
.OBJ38
	EQUB $0F,$FE,$00,$FE,$FE,$00
  EQUB $02,$02,$00,$02,$02,$00,$FE,$FE
  EQUB $06,$FE,$FE,$06,$02,$02,$06,$02
  EQUB $02,$06,$FE,$FF,$08,$FF,$FF,$08
  EQUB $01,$01,$08,$01,$01,$08,$FF,$FF
  EQUB $09,$FF,$FF,$09,$01,$01,$09,$01
  EQUB $01,$09,$FF,$1B,$01,$12,$23,$30
  EQUB $45,$56,$67,$74,$89,$9A,$AB,$B8
  EQUB $CD,$DE,$EF,$FC,$04,$15,$26,$37
  EQUB $48,$59,$6A,$7B,$8C,$9D,$AE,$BF
.OBJ23
  EQUB $0F,$00,$00,$F7,$FC,$02,$F7,$FA
  EQUB $06,$F7,$FC,$0A,$F7,$00,$0C,$F7
  EQUB $04,$0A,$F7,$06,$06,$F7,$04,$02
  EQUB $F7,$00,$00,$09,$FC,$02,$09,$FA
  EQUB $06,$09,$FC,$0A,$09,$00,$0C,$09
  EQUB $04,$0A,$09,$06,$06,$09,$04,$02
  EQUB $09,$0F,$08,$19,$2A,$3B,$4C,$5D
  EQUB $6E,$7F,$0C,$48,$1D,$59,$2E,$6A
  EQUB $3F,$7B
.OBJ20
  EQUB $0F,$00,$00,$F7,$FC,$02
  EQUB $F7,$FA,$06,$F7,$FC,$0A,$F7,$00
  EQUB $0C,$F7,$04,$0A,$F7,$06,$06,$F7
  EQUB $04,$02,$F7,$00,$00,$09,$FC,$02
  EQUB $09,$FA,$06,$09,$FC,$0A,$09,$00
  EQUB $0C,$09,$04,$0A,$09,$06,$06,$09
  EQUB $04,$02,$09,$1B,$08,$19,$2A,$3B
  EQUB $4C,$5D,$6E,$7F,$01,$12,$23,$34
  EQUB $45,$56,$67,$70,$89,$9A,$AB,$BC
  EQUB $CD,$DE,$EF,$F8,$15,$37,$9D,$BF
.OBJ25
  EQUB $0F,$00,$00,$00,$00,$09,$00,$FE
  EQUB $08,$FE,$FE,$08,$02,$FE,$0A,$FE
  EQUB $FE,$0A,$02,$FE,$08,$00,$FE,$0A
  EQUB $00,$FE,$09,$00,$08,$09,$00,$01
  EQUB $09,$FE,$01,$09,$02,$04,$09,$FE
  EQUB $04,$09,$02,$07,$09,$FE,$07,$09
  EQUB $02,$07,$01,$89,$23,$45,$67,$AB
  EQUB $CD,$EF
.OBJ26
	EQUB $0C,$FD,$00,$FE,$FD,$00
  EQUB $02,$00,$00,$04,$03,$00,$02,$03
  EQUB $00,$FE,$00,$00,$FC,$FD,$06,$FE
  EQUB $FD,$06,$02,$00,$06,$04,$03,$06
  EQUB $02,$03,$06,$FE,$00,$06,$FC,$00
  EQUB $0C,$00,$17,$01,$12,$23,$34,$45
  EQUB $50,$67,$78,$89,$9A,$AB,$B6,$06
  EQUB $17,$28,$39,$4A,$5B,$6C,$7C,$8C
  EQUB $9C,$AC,$BC
.OBJ44
	EQUB $0D,$FF,$00,$FF,$FF
  EQUB $00,$01,$01,$00,$01,$01,$00,$FF
  EQUB $FE,$09,$FE,$FE,$09,$02,$02,$09
  EQUB $02,$02,$09,$FE,$FF,$0C,$FF,$FF
  EQUB $0C,$01,$01,$0C,$01,$01,$0C,$FF
  EQUB $00,$00,$00,$00,$09,$00,$10,$01
  EQUB $12,$23,$30,$45,$56,$67,$74,$89
  EQUB $9A,$AB,$B8,$CD,$48,$59,$6A,$7B
.OBJ46
  EQUB $11,$FB,$00,$FE,$FB,$00,$00,$FB
  EQUB $06,$FE,$FB,$06,$00,$05,$00,$FE
  EQUB $05,$00,$00,$05,$06,$FE,$05,$06
  EQUB $00,$FD,$00,$00,$FD,$04,$00,$03
  EQUB $04,$00,$03,$00,$00,$FE,$01,$00
  EQUB $FE,$03,$00,$02,$03,$00,$02,$01
  EQUB $00,$FE,$02,$00,$02,$02,$00,$92
  EQUB $00,$01,$00,$02,$02,$03,$03,$01
  EQUB $04,$05,$04,$06,$06,$07,$07,$05
  EQUB $01,$05,$02,$06,$03,$07,$08,$09
  EQUB $09,$0A,$0A,$0B,$0C,$0D,$0D,$0E
  EQUB $0E,$0F
.OBJ33
	EQUB $0F,$0C,$10,$11,$07,$FE
  EQUB $00,$FA,$FE,$00,$06,$02,$00,$06
  EQUB $02,$00,$FA,$FF,$03,$FB,$FF,$03
  EQUB $05,$01,$03,$05,$01,$03,$FB,$0B
  EQUB $01,$12,$23,$30,$45,$56,$67,$74
  EQUB $04,$15,$26,$37
.OBJ34 EQUB $19,$03,$00,$F6
  EQUB $FD,$00,$F6,$FC,$06,$F6,$FD,$06
  EQUB $F6,$FE,$03,$F6,$02,$03,$F6,$03
  EQUB $00,$F8,$FE,$00,$F8,$FE,$03,$F8
  EQUB $02,$03,$F8,$03,$00,$0A,$FD,$00
  EQUB $0A,$FC,$06,$0A,$FD,$06,$0A,$FE
  EQUB $03,$0A,$02,$03,$0A,$03,$00,$08
  EQUB $FE,$00,$08,$FE,$03,$08,$02,$03
  EQUB $08,$FE,$02,$F8,$02,$02,$F8,$02
  EQUB $00,$F8,$FE,$02,$08,$02,$02,$08
  EQUB $02,$00,$08,$A3,$00,$01,$01,$02
  EQUB $02,$03,$03,$04,$04,$05,$05,$00
  EQUB $06,$07,$07,$08,$08,$09,$09,$06
  EQUB $0A,$0B,$0B,$0C,$0C,$0D,$0D,$0E
  EQUB $0E
  EQUB $0F,$0F,$0A ;SLO $0A0F
  EQUB $10,$11,$11,$12,$12,$13,$13,$10
  EQUB $14,$15,$15,$16,$17,$18,$18,$19
  EQUB $01,$0B,$02,$0C,$03,$0D,$14,$17
  EQUB $15,$18,$16,$19,$04,$08,$05,$09
  EQUB $00,$06,$0E,$12,$0F,$13,$0A,$10
.OBJ9
  EQUB $12,$FC,$16,$04,$04,$16,$04,$00
  EQUB $16,$FC,$FF,$12,$01,$01,$12,$01
  EQUB $00,$12,$FF,$FD,$12,$03,$03,$12
  EQUB $03,$00,$12,$FD,$F0,$16,$10,$10
  EQUB $16,$10,$FE,$02,$02,$02,$02,$02
  EQUB $00,$00,$FE,$FE,$00,$02,$02,$00
  EQUB $02,$00,$02,$FE,$00,$13,$F9,$00
  EQUB $0C,$E6,$9A,$00,$01,$01,$02,$02
  EQUB $00,$03,$04,$04,$05,$05,$03,$00
  EQUB $03,$01,$04,$02,$05,$06,$07,$07
  EQUB $08,$08,$06,$09,$06,$09,$0B,$06
  EQUB $0B,$0A,$07,$0A,$0C,$07,$0C,$0B
  EQUB $0E,$0C,$0F,$08,$0D,$0D,$0E,$0E
  EQUB $0F,$0F,$0D,$10,$11,$11,$12,$12
  EQUB $10,$4C,$00,$00,$4B,$50,$35,$20

.OBJ1
  EQUB $05
  
  EQUB $F0,$00,$20,$10,$00,$20,$FD,$02
  EQUB $20,$03,$02,$20,$00,$0C,$20,$00
  EQUB $00,$E1,$09,$01								; f3B3A   
		EQUB $13,$34,$42,$20,$05,$15,$25,$35
  EQUB $45

;OBJ2
;  EQUB $0F
;  EQUB $F8,$00,$10,$FA,$0C,$0C
;  EQUB $FA,$0C,$00,$F9,$06,$FD,$F9,$04
;  EQUB $F4,$FC,$04,$F0,$FC,$00,$F0,$F9
;  EQUB $00,$F4,$08,$00,$10,$06,$0C,$0C
;  EQUB $06,$0C,$00,$07,$06,$FD,$07,$04
;  EQUB $F4,$04,$04,$F0,$04,$00,$F0,$07
;  EQUB $00,$F4,$17,$01,$12,$23,$34,$45
;  EQUB $56,$67,$70,$89,$9A,$AB,$BC,$CD
;  EQUB $DE,$EF,$F8,$47,$CF,$08,$19,$2A
;  EQUB $3B,$5D,$6E

.OBJ2
	EQUB $01
	EQUB 127,127,127
	EQUB 255-127,255-127,255-127
	EQUB $00
	EQUB $01

.OBJ3
	EQUB $0F,$F8,$04,$18,$08
  EQUB $04,$18,$00,$04,$E8,$00,$0C,$18
  EQUB $E2,$04,$18,$1E,$04,$18,$FC,$04
  EQUB $00,$04,$04,$00,$00,$14,$18,$00
  EQUB $0A,$0C,$F2,$04,$10,$F2,$00,$10
  EQUB $0E,$04,$10,$0E,$00,$10,$00,$04
  EQUB $F0,$00,$00,$F0,$0C,$20,$23,$21
  EQUB $13,$03,$45,$46,$57,$38,$89,$AB
  EQUB $CD,$EF
.OBJ4
  EQUB $07,$00,$00,$00,$EC,$07
  EQUB $00,$F6,$07,$11,$0A,$07,$11,$14
  EQUB $07,$00,$0A,$07,$EF,$F6,$07,$EF
  EQUB $00,$10,$00,$11,$01,$02,$03,$04
  EQUB $05,$06,$71,$72,$73,$74,$75,$76
  EQUB $12,$23,$34,$45,$56,$61
.OBJ6 
	EQUB $05,$F4
  EQUB $00,$10,$00,$00,$F0,$0C,$00,$10
  EQUB $F4,$0A,$10,$00,$0A,$F0,$0C,$0A
  EQUB $10,$08,$01,$12,$20,$34,$45,$53
  EQUB $03,$14,$25
.OBJ7
  EQUB $0F,$F0,$00,$10,$10
  
  EQUB $00,$10,$00,$00,$EC,$FC,$00,$04
  EQUB $04,$00,$04,$00,$00,$FC,$FD,$04
  EQUB $03,$03,$04,$03,$00,$04,$FD,$FE
  EQUB $14,$02,$02,$14,$02,$00,$14,$FE
  EQUB $FF,$04,$01,$01,$04,$01,$00,$04
  EQUB $FF,$00,$34,$00,$14,$F6,$F7,$F8
  EQUB $90,$A1,$B2,$06,$17,$28,$C3,$D4
  EQUB $E5,$34,$45,$53,$67,$78,$86,$CD
  EQUB $DE,$EC
.OBJ8
	EQUB $07,$F8,$F8,$F8,$08,$F8
  EQUB $F8,$08,$F8,$08,$F8,$F8,$08,$F8
  EQUB $08,$F8,$08,$08,$F8,$08,$08,$08
  EQUB $F8,$08,$08,$0B,$01,$12,$23,$30
  EQUB $04,$15,$26,$37,$45,$56,$67,$74
  EQUB $09,$FC,$07,$04,$04,$07,$04,$00
.OBJ11
  EQUB $07,$FC,$00,$0E,$00,$FF,$07,$01
  EQUB $01,$07,$01,$00,$07,$FF,$FE,$00
  EQUB $02,$02,$00,$02,$00,$00,$FE,$0E
  EQUB $01,$12,$20,$03,$13,$23,$45,$56
  EQUB $64,$78,$89,$97,$47,$58,$69
.OBJ12
  EQUB $05
  EQUB $FB,$00,$FF,$05,$00,$FF,$00,$0C
  EQUB $FF,$FB,$00,$01,$05,$00,$01,$00
  EQUB $0C,$01,$08,$01,$12,$20,$34,$45
  EQUB $53,$03,$14,$25
.OBJ13 EQUB $07,$FF,$00,$FC
  EQUB $FF,$00,$04,$FF,$0C,$02,$FF,$0C
  EQUB $FE,$01,$00,$FC,$01,$00,$04,$01
  EQUB $0C,$02,$01,$0C,$FE,$0B,$01,$12
  EQUB $23,$30,$45,$56,$67,$74,$04,$15
  EQUB $26,$37
.OBJ14
  EQUB $09,$04,$00,$FF,$FC,$00
  EQUB $FF,$FA,$08,$FF,$00,$0C,$FF,$06
  EQUB $08,$FF,$04,$00,$01,$FC,$00,$01
  EQUB $FA,$08,$01,$00,$0C,$01,$06,$08
  EQUB $01,$0E,$01,$12,$23,$34,$40,$56
  EQUB $67,$78,$89,$95,$05,$16,$27,$38
  EQUB $49
.OBJ15
  EQUB $09,$FD,$00,$FF,$03,$00,$FF
  EQUB $03,$0A,$FF,$00,$0C,$FF,$FD,$0A
  EQUB $FF,$FD,$00,$01,$03,$00,$01,$03
  EQUB $0A,$01,$00,$0C,$01,$FD,$0A,$01
  EQUB $0E,$01,$12,$23,$34,$40,$56,$67
  EQUB $78,$89,$95,$05,$16,$27,$38,$49
.OBJ16
  EQUB $07,$FE,$00,$FF,$02,$00,$FF,$04
  EQUB $0C,$FF,$FC,$0C,$FF,$FE,$00,$01
  EQUB $02,$00,$01,$04,$0C,$01,$FC,$0C
  EQUB $01,$0B,$01,$12,$23,$30,$45,$56
  EQUB $67,$74,$04,$15,$26,$37
.OBJ17 
	EQUB $0B,$02
  EQUB $00,$FF,$04,$06,$FF,$02,$0C,$FF
  EQUB $FE,$0C,$FF,$FC,$06,$FF,$FE,$00
  EQUB $FF,$02,$00,$01,$04,$06,$01,$02
  EQUB $0C,$01,$FE,$0C,$01,$FC,$06,$01
  EQUB $FE,$00,$01,$11,$01,$12,$23,$34
  EQUB $45,$50,$67,$78,$89,$9A,$AB,$B6
  EQUB $06,$17,$28,$39,$4A,$5B
.OBJ18
	EQUB $0B,$03
  EQUB $00,$FF,$03,$08,$FF,$01,$0C,$FF
  EQUB $FF,$0C,$FF,$FD,$08,$FF,$FD,$00
  EQUB $FF,$03,$00,$01,$03,$08,$01,$01
  EQUB $0C,$01,$FF,$0C,$01,$FD,$08,$01
  EQUB $FD,$00,$01,$11,$01,$12,$23,$34
  EQUB $45,$50,$67,$78,$89,$9A,$AB,$B6
  EQUB $06,$17,$28,$39,$4A,$5B
.OBJ19 EQUB $09,$FC
  EQUB $07,$04,$04,$07,$04,$00,$07,$FC
  EQUB $00,$09,$00,$FF,$07,$01,$01,$07
  EQUB $01,$00,$07,$FF,$FF,$00,$01,$01
  EQUB $00,$01,$00,$00,$FF,$0E,$01,$12
  EQUB $20,$03,$13,$23,$45,$56,$64,$78
  EQUB $89,$97,$47,$58,$69
.OBJ29
	EQUB $0F,$F9,$07
  EQUB $FF,$F9,$09,$FF,$07,$09,$FF,$07
  EQUB $00,$FF,$05,$00,$FF,$04,$04,$FF
  EQUB $02,$04,$FF,$02,$07,$FF,$F9,$07
  EQUB $01,$F9,$09,$01,$07,$09,$01,$07
  EQUB $00,$01,$05,$00,$01,$04,$04,$01
  EQUB $02,$04,$01,$02,$07,$01,$17,$01
  EQUB $12,$23,$34,$45,$56,$67,$70,$89
  EQUB $9A,$AB,$BC,$CD,$DE,$EF,$F8,$08
  EQUB $19,$2A,$3B,$4C,$5D,$6E,$7F
.OBJ48
	EQUB $0B
  EQUB $F9,$00,$F3,$07,$00,$F3,$07,$00
  EQUB $0D,$F9,$00,$0D,$F9,$06,$F3,$07
  EQUB $06,$F3,$07,$06,$0D,$F9,$06,$0D
  EQUB $F6,$06,$F0,$0A,$06,$F0,$0A,$06
  EQUB $10,$F6,$06,$10,$07,$04,$15,$26
  EQUB $37,$89,$9A,$AB,$B8
.OBJ49
	EQUB $0B,$FE,$00
  EQUB $FE,$FE,$00,$02,$02,$00,$02,$02
  EQUB $00,$FE,$FE,$04,$FE,$FE,$04,$02
  EQUB $02,$04,$02,$02,$04,$FE,$02,$06
  EQUB $02,$02,$06,$FE,$02,$07,$02,$02
  EQUB $07,$FE,$09,$04,$15,$2A,$3B,$45
  EQUB $56,$67,$74,$89,$AB
.OBJ50
	EQUB $0D,$F6,$00
  EQUB $FB,$F6,$00,$05,$0A,$00,$05,$0A
  EQUB $00,$FB,$F6,$02,$FB,$F6,$02,$05
  EQUB $0A,$02,$05,$0A,$02,$FB,$F6,$04
  EQUB $FB,$F6,$04,$05,$0A,$04,$05,$0A
  EQUB $04,$FB,$0A,$08,$05,$0A,$08,$FB
  EQUB $0C,$08,$19,$2C,$3D,$45,$56,$67
  EQUB $74,$89,$9A,$AB,$B8,$CD
.OBJ52
	EQUB $11,$00
  EQUB $08,$FD,$00,$04,$03,$00,$04,$FD
  EQUB $00,$08,$03,$00,$07,$FF,$00,$07
  EQUB $01,$00,$09,$02,$00,$0A,$02,$00
  EQUB $0B,$01,$00,$0B,$FF,$00,$0A,$FE
  EQUB $00,$09,$FE,$00,$0A,$FF,$00,$09
  EQUB $FF,$00,$0A,$01,$00,$09,$01,$00
  EQUB $08,$FF,$00,$08,$01,$8C,$00,$01
  EQUB $02,$03,$04,$05,$05,$06,$06,$07
  EQUB $07,$08,$08,$09,$09,$0A,$0A,$0B
  EQUB $0B,$04,$0C,$0D,$0E,$0F,$10,$11
.OBJ54
  EQUB $0F,$E0,$00,$E0,$E0,$00,$20,$20
  EQUB $00,$20,$20,$00,$E0,$E0,$06,$E0
  EQUB $E0,$06,$20,$20,$06,$20,$20,$06
  EQUB $E0,$E0,$0C,$E0,$E0,$0C,$20,$20
  EQUB $0C,$20,$20,$0C,$E0,$E0,$12,$E0
  EQUB $E0,$12,$20,$20,$12,$20,$20,$12
  EQUB $E0,$10,$01,$12,$23,$30,$45,$67
  EQUB $74,$89,$AB,$B8,$CD,$EF,$FC,$0C
  EQUB $1D,$2E,$3F
.OBJ55
	EQUB $15
	EQUB $80,$E0,$80
	EQUB $7F,$E0,$80
	EQUB $7F,$E0,$7F
	EQUB $80,$E0,$7F
	EQUB $80,$20,$80
	EQUB $7F,$20,$80
	EQUB $7F,$20,$7F
	EQUB $80,$20,$7F
	EQUB $80,$E0,$FD
	EQUB $80,$E0,$03
	EQUB $80,$EC,$03
	EQUB $80,$EC,$FD
	EQUB $B0,$20,$80
	EQUB $B0,$20,$70
	EQUB $40,$20,$70
	EQUB $40,$20,$00
	EQUB $00,$20,$00
	EQUB $00,$20,$50
	EQUB $D0,$20,$50
	EQUB $D0,$20,$80
	EQUB $00,$20,$40
	EQUB $40,$20,$40
	EQUB $96
	EQUB $00,$01, $01,$02, $02,$03, $03,$00
	EQUB $00,$04, $01,$05, $02,$06, $03,$07
	EQUB $04,$05, $05,$06, $06,$07, $07,$04
	EQUB $09,$0A, $0A,$0B, $0B,$08, $0C,$0D
	EQUB $0D,$0E, $0E,$0F, $0F,$10, $10,$11
	EQUB $11,$12, $12,$13, $14,$15, $00

ALIGN &100
.SIN_LSB
		EQUB $92,$2E,$F7,$60,$C4,$14,$47,$79
        EQUB $AB,$DD,$08,$21,$3A,$53,$6C,$85
        EQUB $9E,$B7,$D0,$E9,$01,$0D,$1A,$26
        EQUB $33,$3F,$4C,$58,$64,$71,$7D,$89
        EQUB $96,$A2,$AE,$BB,$C7,$D3,$DF,$EC
        EQUB $F8,$02,$08,$0E,$14,$1A,$20,$26
        EQUB $2C,$32,$38,$3E,$44,$4A,$50,$56
        EQUB $5C,$62,$68,$6E,$73,$79,$7F,$85
        EQUB $8B,$91,$96,$9C,$A2,$A8,$AD,$B3
        EQUB $B9,$BE,$C4,$CA,$CF,$D5,$DA,$E0
        EQUB $E5,$EB,$F1,$F6,$FB,$00,$03,$06
        EQUB $09,$0B,$0E,$11,$13,$16,$19,$1B
        EQUB $1E,$20,$23,$26,$28,$2B,$2D,$30
        EQUB $32,$35,$37,$3A,$3C,$3F,$41,$44
        EQUB $46,$48,$4B,$4D,$50,$52,$54,$57
        EQUB $59,$5B,$5E,$60,$62,$64,$67,$69
        EQUB $6B,$6D,$70,$72,$74,$76,$78,$7A
        EQUB $7C,$7F,$81,$83,$85,$87,$89,$8B
        EQUB $8D,$8F,$91,$93,$95,$97,$98,$9A
        EQUB $9C,$9E,$A0,$A2,$A4,$A5,$A7,$A9
        EQUB $AB,$AC,$AE,$B0,$B1,$B3,$B5,$B6
        EQUB $B8,$BA,$BB,$BD,$BE,$C0,$C1,$C3
        EQUB $C4,$C6,$C7,$C9,$CA,$CB,$CD,$CE
        EQUB $D0,$D1,$D2,$D3,$D5,$D6,$D7,$D8
        EQUB $DA,$DB,$DC,$DD,$DE,$DF,$E0,$E2
        EQUB $E3,$E4,$E5,$E6,$E7,$E8,$E9,$E9
        EQUB $EA,$EB,$EC,$ED,$EE,$EF,$EF,$F0
        EQUB $F1,$F2,$F3,$F3,$F4,$F5,$F5,$F6
        EQUB $F6,$F7,$F8,$F8,$F9,$F9,$FA,$FA
        EQUB $FB,$FB,$FC,$FC,$FC,$FD,$FD,$FD
        EQUB $FE,$FE,$FE,$FE,$FF,$FF,$FF,$FF
        EQUB $FF,$00,$00,$00,$00,$00,$00,$00
 
.SIN_MSB
		EQUB $DC,$E4,$E4,$E8,$E8,$EC,$EC,$EC
        EQUB $EC,$EC,$F0,$F0,$F0,$F0,$F0,$F0
        EQUB $F0,$F0,$F0,$F0,$F4,$F4,$F4,$F4
        EQUB $F4,$F4,$F4,$F4,$F4,$F4,$F4,$F4
        EQUB $F4,$F4,$F4,$F4,$F4,$F4,$F4,$F4
        EQUB $F4,$F8,$F8,$F8,$F8,$F8,$F8,$F8
        EQUB $F8,$F8,$F8,$F8,$F8,$F8,$F8,$F8
        EQUB $F8,$F8,$F8,$F8,$F8,$F8,$F8,$F8
        EQUB $F8,$F8,$F8,$F8,$F8,$F8,$F8,$F8
        EQUB $F8,$F8,$F8,$F8,$F8,$F8,$F8,$F8
        EQUB $F8,$F8,$F8,$F8,$F8,$FC,$FC,$FC
        EQUB $FC,$FC,$FC,$FC,$FC,$FC,$FC,$FC
        EQUB $FC,$FC,$FC,$FC,$FC,$FC,$FC,$FC
        EQUB $FC,$FC,$FC,$FC,$FC,$FC,$FC,$FC
        EQUB $FC,$FC,$FC,$FC,$FC,$FC,$FC,$FC
        EQUB $FC,$FC,$FC,$FC,$FC,$FC,$FC,$FC
        EQUB $FC,$FC,$FC,$FC,$FC,$FC,$FC,$FC
        EQUB $FC,$FC,$FC,$FC,$FC,$FC,$FC,$FC
        EQUB $FC,$FC,$FC,$FC,$FC,$FC,$FC,$FC
        EQUB $FC,$FC,$FC,$FC,$FC,$FC,$FC,$FC
        EQUB $FC,$FC,$FC,$FC,$FC,$FC,$FC,$FC
        EQUB $FC,$FC,$FC,$FC,$FC,$FC,$FC,$FC
        EQUB $FC,$FC,$FC,$FC,$FC,$FC,$FC,$FC
        EQUB $FC,$FC,$FC,$FC,$FC,$FC,$FC,$FC
        EQUB $FC,$FC,$FC,$FC,$FC,$FC,$FC,$FC
        EQUB $FC,$FC,$FC,$FC,$FC,$FC,$FC,$FC
        EQUB $FC,$FC,$FC,$FC,$FC,$FC,$FC,$FC
        EQUB $FC,$FC,$FC,$FC,$FC,$FC,$FC,$FC
        EQUB $FC,$FC,$FC,$FC,$FC,$FC,$FC,$FC
        EQUB $FC,$FC,$FC,$FC,$FC,$FC,$FC,$FC
        EQUB $FC,$FC,$FC,$FC,$FC,$FC,$FC,$FC
        EQUB $FC,$00,$00,$00,$00,$00,$00,$00
	
.LOG_TABLE
		EQUB $00,$01,$03,$04,$06,$07,$09,$0A
        EQUB $0B,$0D,$0E,$10,$11,$12,$14,$15
        EQUB $16,$18,$19,$1A,$1C,$1D,$1E,$20
        EQUB $21,$22,$24,$25,$26,$28,$29,$2A
        EQUB $2C,$2D,$2E,$2F,$31,$32,$33,$34
        EQUB $36,$37,$38,$39,$3B,$3C,$3D,$3E
        EQUB $3F,$41,$42,$43,$44,$45,$47,$48
        EQUB $49,$4A,$4B,$4D,$4E,$4F,$50,$51
        EQUB $52,$54,$55,$56,$57,$58,$59,$5A
        EQUB $5C,$5D,$5E,$5F,$60,$61,$62,$63
        EQUB $64,$66,$67,$68,$69,$6A,$6B,$6C
        EQUB $6D,$6E,$6F,$70,$71,$72,$74,$75
        EQUB $76,$77,$78,$79,$7A,$7B,$7C,$7D
        EQUB $7E,$7F,$80,$81,$82,$83,$84,$85
        EQUB $86,$87,$88,$89,$8A,$8B,$8C,$8D
        EQUB $8E,$8F,$90,$91,$92,$93,$94,$95
        EQUB $96,$97,$98,$99,$9A,$9B,$9B,$9C
        EQUB $9D,$9E,$9F,$A0,$A1,$A2,$A3,$A4
        EQUB $A5,$A6,$A7,$A8,$A9,$A9,$AA,$AB
        EQUB $AC,$AD,$AE,$AF,$B0,$B1,$B2,$B2
        EQUB $B3,$B4,$B5,$B6,$B7,$B8,$B9,$B9
        EQUB $BA,$BB,$BC,$BD,$BE,$BF,$C0,$C0
        EQUB $C1,$C2,$C3,$C4,$C5,$C6,$C6,$C7
        EQUB $C8,$C9,$CA,$CB,$CB,$CC,$CD,$CE
        EQUB $CF,$D0,$D0,$D1,$D2,$D3,$D4,$D4
        EQUB $D5,$D6,$D7,$D8,$D8,$D9,$DA,$DB
        EQUB $DC,$DC,$DD,$DE,$DF,$E0,$E0,$E1
        EQUB $E2,$E3,$E4,$E4,$E5,$E6,$E7,$E7
        EQUB $E8,$E9,$EA,$EA,$EB,$EC,$ED,$EE
        EQUB $EE,$EF,$F0,$F1,$F1,$F2,$F3,$F4
        EQUB $F4,$F5,$F6,$F7,$F7,$F8,$F9,$F9
        EQUB $FA,$FB,$FC,$FC,$FD,$FE,$FF,$FF

.EXP_TABLE
		EQUB $00,$01,$01,$02,$03,$03,$04,$05
        EQUB $06,$06,$07,$08,$08,$09,$0A,$0B
        EQUB $0B,$0C,$0D,$0E,$0E,$0F,$10,$10
        EQUB $11,$12,$13,$13,$14,$15,$16,$16
        EQUB $17,$18,$19,$19,$1A,$1B,$1C,$1D
        EQUB $1D,$1E,$1F,$20,$20,$21,$22,$23
        EQUB $24,$24,$25,$26,$27,$28,$28,$29
        EQUB $2A,$2B,$2C,$2C,$2D,$2E,$2F,$30
        EQUB $30,$31,$32,$33,$34,$35,$35,$36
        EQUB $37,$38,$39,$3A,$3A,$3B,$3C,$3D
        EQUB $3E,$3F,$40,$41,$41,$42,$43,$44
        EQUB $45,$46,$47,$48,$48,$49,$4A,$4B
        EQUB $4C,$4D,$4E,$4F,$50,$51,$51,$52
        EQUB $53,$54,$55,$56,$57,$58,$59,$5A
        EQUB $5B,$5C,$5D,$5E,$5E,$5F,$60,$61
        EQUB $62,$63,$64,$65,$66,$67,$68,$69
        EQUB $6A,$6B,$6C,$6D,$6E,$6F,$70,$71
        EQUB $72,$73,$74,$75,$76,$77,$78,$79
        EQUB $7A,$7B,$7C,$7D,$7E,$7F,$80,$81
        EQUB $82,$83,$84,$85,$87,$88,$89,$8A
        EQUB $8B,$8C,$8D,$8E,$8F,$90,$91,$92
        EQUB $93,$95,$96,$97,$98,$99,$9A,$9B
        EQUB $9C,$9D,$9F,$A0,$A1,$A2,$A3,$A4
        EQUB $A5,$A6,$A8,$A9,$AA,$AB,$AC,$AD
        EQUB $AF,$B0,$B1,$B2,$B3,$B4,$B6,$B7
        EQUB $B8,$B9,$BA,$BC,$BD,$BE,$BF,$C0
        EQUB $C2,$C3,$C4,$C5,$C6,$C8,$C9,$CA
        EQUB $CB,$CD,$CE,$CF,$D0,$D2,$D3,$D4
        EQUB $D6,$D7,$D8,$D9,$DB,$DC,$DD,$DE
        EQUB $E0,$E1,$E2,$E4,$E5,$E6,$E8,$E9
        EQUB $EA,$EC,$ED,$EE,$F0,$F1,$F2,$F4
        EQUB $F5,$F6,$F8,$F9,$FA,$FC,$FD,$FF

ALIGN &100
.VER_POS_SCN0_HI
.SCR0_MONO_HI
    EQUB >SC0_LN000, >SC0_LN001, >SC0_LN002, >SC0_LN003, >SC0_LN004, >SC0_LN005, >SC0_LN006, >SC0_LN007, >SC0_LN008, >SC0_LN009
    EQUB >SC0_LN010, >SC0_LN011, >SC0_LN012, >SC0_LN013, >SC0_LN014, >SC0_LN015, >SC0_LN016, >SC0_LN017, >SC0_LN018, >SC0_LN019
    EQUB >SC0_LN020, >SC0_LN021, >SC0_LN022, >SC0_LN023, >SC0_LN024, >SC0_LN025, >SC0_LN026, >SC0_LN027, >SC0_LN028, >SC0_LN029
    EQUB >SC0_LN030, >SC0_LN031, >SC0_LN032, >SC0_LN033, >SC0_LN034, >SC0_LN035, >SC0_LN036, >SC0_LN037, >SC0_LN038, >SC0_LN039
    EQUB >SC0_LN040, >SC0_LN041, >SC0_LN042, >SC0_LN043, >SC0_LN044, >SC0_LN045, >SC0_LN046, >SC0_LN047, >SC0_LN048, >SC0_LN049
    EQUB >SC0_LN050, >SC0_LN051, >SC0_LN052, >SC0_LN053, >SC0_LN054, >SC0_LN055, >SC0_LN056, >SC0_LN057, >SC0_LN058, >SC0_LN059
    EQUB >SC0_LN060, >SC0_LN061, >SC0_LN062, >SC0_LN063, >SC0_LN064, >SC0_LN065, >SC0_LN066, >SC0_LN067, >SC0_LN068, >SC0_LN069
    EQUB >SC0_LN070, >SC0_LN071, >SC0_LN072, >SC0_LN073, >SC0_LN074, >SC0_LN075, >SC0_LN076, >SC0_LN077, >SC0_LN078, >SC0_LN079
    EQUB >SC0_LN080, >SC0_LN081, >SC0_LN082, >SC0_LN083, >SC0_LN084, >SC0_LN085, >SC0_LN086, >SC0_LN087, >SC0_LN088, >SC0_LN089
    EQUB >SC0_LN090, >SC0_LN091, >SC0_LN092, >SC0_LN093, >SC0_LN094, >SC0_LN095, >SC0_LN096, >SC0_LN097, >SC0_LN098, >SC0_LN099
    EQUB >SC0_LN100, >SC0_LN101, >SC0_LN102, >SC0_LN103, >SC0_LN104, >SC0_LN105, >SC0_LN106, >SC0_LN107, >SC0_LN108, >SC0_LN109
    EQUB >SC0_LN110, >SC0_LN111, >SC0_LN112, >SC0_LN113, >SC0_LN114, >SC0_LN115, >SC0_LN116, >SC0_LN117, >SC0_LN118, >SC0_LN119
    EQUB >SC0_LN120, >SC0_LN121, >SC0_LN122, >SC0_LN123, >SC0_LN124, >SC0_LN125, >SC0_LN126, >SC0_LN127
;	, >SC0_LN128, >SC0_LN129
;    EQUB >SC0_LN130, >SC0_LN131, >SC0_LN132, >SC0_LN133, >SC0_LN134, >SC0_LN135, >SC0_LN136, >SC0_LN137, >SC0_LN138, >SC0_LN139
;    EQUB >SC0_LN140, >SC0_LN141, >SC0_LN142, >SC0_LN143, >SC0_LN144, >SC0_LN145, >SC0_LN146, >SC0_LN147, >SC0_LN148, >SC0_LN149
;    EQUB >SC0_LN150, >SC0_LN151, >SC0_LN152, >SC0_LN153, >SC0_LN154, >SC0_LN155, >SC0_LN156, >SC0_LN157, >SC0_LN158, >SC0_LN159
;    EQUB >SC0_LN160, >SC0_LN161, >SC0_LN162, >SC0_LN163, >SC0_LN164, >SC0_LN165, >SC0_LN166, >SC0_LN167, >SC0_LN168, >SC0_LN169
;    EQUB >SC0_LN170, >SC0_LN171, >SC0_LN172, >SC0_LN173, >SC0_LN174, >SC0_LN175, >SC0_LN176, >SC0_LN177, >SC0_LN178, >SC0_LN179
;    EQUB >SC0_LN180, >SC0_LN181, >SC0_LN182, >SC0_LN183, >SC0_LN184, >SC0_LN185, >SC0_LN186, >SC0_LN187, >SC0_LN188, >SC0_LN189
;    EQUB >SC0_LN190, >SC0_LN191

ALIGN &100
.SCR1_MONO_HI
    EQUB >SC1_LN000, >SC1_LN001, >SC1_LN002, >SC1_LN003, >SC1_LN004, >SC1_LN005, >SC1_LN006, >SC1_LN007, >SC1_LN008, >SC1_LN009
    EQUB >SC1_LN010, >SC1_LN011, >SC1_LN012, >SC1_LN013, >SC1_LN014, >SC1_LN015, >SC1_LN016, >SC1_LN017, >SC1_LN018, >SC1_LN019
    EQUB >SC1_LN020, >SC1_LN021, >SC1_LN022, >SC1_LN023, >SC1_LN024, >SC1_LN025, >SC1_LN026, >SC1_LN027, >SC1_LN028, >SC1_LN029
    EQUB >SC1_LN030, >SC1_LN031, >SC1_LN032, >SC1_LN033, >SC1_LN034, >SC1_LN035, >SC1_LN036, >SC1_LN037, >SC1_LN038, >SC1_LN039
    EQUB >SC1_LN040, >SC1_LN041, >SC1_LN042, >SC1_LN043, >SC1_LN044, >SC1_LN045, >SC1_LN046, >SC1_LN047, >SC1_LN048, >SC1_LN049
    EQUB >SC1_LN050, >SC1_LN051, >SC1_LN052, >SC1_LN053, >SC1_LN054, >SC1_LN055, >SC1_LN056, >SC1_LN057, >SC1_LN058, >SC1_LN059
    EQUB >SC1_LN060, >SC1_LN061, >SC1_LN062, >SC1_LN063, >SC1_LN064, >SC1_LN065, >SC1_LN066, >SC1_LN067, >SC1_LN068, >SC1_LN069
    EQUB >SC1_LN070, >SC1_LN071, >SC1_LN072, >SC1_LN073, >SC1_LN074, >SC1_LN075, >SC1_LN076, >SC1_LN077, >SC1_LN078, >SC1_LN079
    EQUB >SC1_LN080, >SC1_LN081, >SC1_LN082, >SC1_LN083, >SC1_LN084, >SC1_LN085, >SC1_LN086, >SC1_LN087, >SC1_LN088, >SC1_LN089
    EQUB >SC1_LN090, >SC1_LN091, >SC1_LN092, >SC1_LN093, >SC1_LN094, >SC1_LN095, >SC1_LN096, >SC1_LN097, >SC1_LN098, >SC1_LN099
    EQUB >SC1_LN100, >SC1_LN101, >SC1_LN102, >SC1_LN103, >SC1_LN104, >SC1_LN105, >SC1_LN106, >SC1_LN107, >SC1_LN108, >SC1_LN109
    EQUB >SC1_LN110, >SC1_LN111, >SC1_LN112, >SC1_LN113, >SC1_LN114, >SC1_LN115, >SC1_LN116, >SC1_LN117, >SC1_LN118, >SC1_LN119
    EQUB >SC1_LN120, >SC1_LN121, >SC1_LN122, >SC1_LN123, >SC1_LN124, >SC1_LN125, >SC1_LN126, >SC1_LN127
;	, >SC1_LN128, >SC1_LN129
;    EQUB >SC1_LN130, >SC1_LN131, >SC1_LN132, >SC1_LN133, >SC1_LN134, >SC1_LN135, >SC1_LN136, >SC1_LN137, >SC1_LN138, >SC1_LN139
;    EQUB >SC1_LN140, >SC1_LN141, >SC1_LN142, >SC1_LN143, >SC1_LN144, >SC1_LN145, >SC1_LN146, >SC1_LN147, >SC1_LN148, >SC1_LN149
;    EQUB >SC1_LN150, >SC1_LN151, >SC1_LN152, >SC1_LN153, >SC1_LN154, >SC1_LN155, >SC1_LN156, >SC1_LN157, >SC1_LN158, >SC1_LN159
;    EQUB >SC1_LN160, >SC1_LN161, >SC1_LN162, >SC1_LN163, >SC1_LN164, >SC1_LN165, >SC1_LN166, >SC1_LN167, >SC1_LN168, >SC1_LN169
;    EQUB >SC1_LN170, >SC1_LN171, >SC1_LN172, >SC1_LN173, >SC1_LN174, >SC1_LN175, >SC1_LN176, >SC1_LN177, >SC1_LN178, >SC1_LN179
;    EQUB >SC1_LN180, >SC1_LN181, >SC1_LN182, >SC1_LN183, >SC1_LN184, >SC1_LN185, >SC1_LN186, >SC1_LN187, >SC1_LN188, >SC1_LN189
;    EQUB >SC1_LN190, >SC1_LN191

ALIGN &100
.VER_POS_LO
    EQUB <SC1_LN000, <SC1_LN001, <SC1_LN002, <SC1_LN003, <SC1_LN004, <SC1_LN005, <SC1_LN006, <SC1_LN007, <SC1_LN008, <SC1_LN009
    EQUB <SC1_LN010, <SC1_LN011, <SC1_LN012, <SC1_LN013, <SC1_LN014, <SC1_LN015, <SC1_LN016, <SC1_LN017, <SC1_LN018, <SC1_LN019
    EQUB <SC1_LN020, <SC1_LN021, <SC1_LN022, <SC1_LN023, <SC1_LN024, <SC1_LN025, <SC1_LN026, <SC1_LN027, <SC1_LN028, <SC1_LN029
    EQUB <SC1_LN030, <SC1_LN031, <SC1_LN032, <SC1_LN033, <SC1_LN034, <SC1_LN035, <SC1_LN036, <SC1_LN037, <SC1_LN038, <SC1_LN039
    EQUB <SC1_LN040, <SC1_LN041, <SC1_LN042, <SC1_LN043, <SC1_LN044, <SC1_LN045, <SC1_LN046, <SC1_LN047, <SC1_LN048, <SC1_LN049
    EQUB <SC1_LN050, <SC1_LN051, <SC1_LN052, <SC1_LN053, <SC1_LN054, <SC1_LN055, <SC1_LN056, <SC1_LN057, <SC1_LN058, <SC1_LN059
    EQUB <SC1_LN060, <SC1_LN061, <SC1_LN062, <SC1_LN063, <SC1_LN064, <SC1_LN065, <SC1_LN066, <SC1_LN067, <SC1_LN068, <SC1_LN069
    EQUB <SC1_LN070, <SC1_LN071, <SC1_LN072, <SC1_LN073, <SC1_LN074, <SC1_LN075, <SC1_LN076, <SC1_LN077, <SC1_LN078, <SC1_LN079
    EQUB <SC1_LN080, <SC1_LN081, <SC1_LN082, <SC1_LN083, <SC1_LN084, <SC1_LN085, <SC1_LN086, <SC1_LN087, <SC1_LN088, <SC1_LN089
    EQUB <SC1_LN090, <SC1_LN091, <SC1_LN092, <SC1_LN093, <SC1_LN094, <SC1_LN095, <SC1_LN096, <SC1_LN097, <SC1_LN098, <SC1_LN099
    EQUB <SC1_LN100, <SC1_LN101, <SC1_LN102, <SC1_LN103, <SC1_LN104, <SC1_LN105, <SC1_LN106, <SC1_LN107, <SC1_LN108, <SC1_LN109
    EQUB <SC1_LN110, <SC1_LN111, <SC1_LN112, <SC1_LN113, <SC1_LN114, <SC1_LN115, <SC1_LN116, <SC1_LN117, <SC1_LN118, <SC1_LN119
    EQUB <SC1_LN120, <SC1_LN121, <SC1_LN122, <SC1_LN123, <SC1_LN124, <SC1_LN125, <SC1_LN126, <SC1_LN127
;	, >SC1_LN128, >SC1_LN129
;    EQUB >SC1_LN130, >SC1_LN131, >SC1_LN132, >SC1_LN133, >SC1_LN134, >SC1_LN135, >SC1_LN136, >SC1_LN137, >SC1_LN138, >SC1_LN139
;    EQUB >SC1_LN140, >SC1_LN141, >SC1_LN142, >SC1_LN143, >SC1_LN144, >SC1_LN145, >SC1_LN146, >SC1_LN147, >SC1_LN148, >SC1_LN149
;    EQUB >SC1_LN150, >SC1_LN151, >SC1_LN152, >SC1_LN153, >SC1_LN154, >SC1_LN155, >SC1_LN156, >SC1_LN157, >SC1_LN158, >SC1_LN159
;    EQUB >SC1_LN160, >SC1_LN161, >SC1_LN162, >SC1_LN163, >SC1_LN164, >SC1_LN165, >SC1_LN166, >SC1_LN167, >SC1_LN168, >SC1_LN169
;    EQUB >SC1_LN170, >SC1_LN171, >SC1_LN172, >SC1_LN173, >SC1_LN174, >SC1_LN175, >SC1_LN176, >SC1_LN177, >SC1_LN178, >SC1_LN179
;    EQUB >SC1_LN180, >SC1_LN181, >SC1_LN182, >SC1_LN183, >SC1_LN184, >SC1_LN185, >SC1_LN186, >SC1_LN187, >SC1_LN188, >SC1_LN189
;    EQUB >SC1_LN190, >SC1_LN191

ALIGN &100
.SCR_MONO_HOR
	EQUB 00,00,00,00,00,00,00,00
	EQUB 08,08,08,08,08,08,08,08
	EQUB 16,16,16,16,16,16,16,16
	EQUB 24,24,24,24,24,24,24,24
	EQUB 32,32,32,32,32,32,32,32
	EQUB 40,40,40,40,40,40,40,40
	EQUB 48,48,48,48,48,48,48,48
	EQUB 56,56,56,56,56,56,56,56
	EQUB 64,64,64,64,64,64,64,64
	EQUB 72,72,72,72,72,72,72,72
	EQUB 80,80,80,80,80,80,80,80
	EQUB 88,88,88,88,88,88,88,88
	EQUB 96,96,96,96,96,96,96,96
	EQUB 104,104,104,104,104,104,104,104
	EQUB 112,112,112,112,112,112,112,112
	EQUB 120,120,120,120,120,120,120,120	
	EQUB 128,128,128,128,128,128,128,128
	EQUB 136,136,136,136,136,136,136,136
	EQUB 144,144,144,144,144,144,144,144
	EQUB 152,152,152,152,152,152,152,152
	EQUB 160,160,160,160,160,160,160,160
	EQUB 168,168,168,168,168,168,168,168
	EQUB 176,176,176,176,176,176,176,176
	EQUB 184,184,184,184,184,184,184,184
	EQUB 192,192,192,192,192,192,192,192
	EQUB 200,200,200,200,200,200,200,200
	EQUB 208,208,208,208,208,208,208,208
	EQUB 216,216,216,216,216,216,216,216
	EQUB 224,224,224,224,224,224,224,224
	EQUB 232,232,232,232,232,232,232,232
	EQUB 240,240,240,240,240,240,240,240
	EQUB 248,248,248,248,248,248,248,248

.HOR_SCR_POS
		EQUB $00,$00,$00,$00,$08,$08,$08,$08
        EQUB $10,$10,$10,$10,$18,$18,$18,$18
        EQUB $20,$20,$20,$20,$28,$28,$28,$28
        EQUB $30,$30,$30,$30,$38,$38,$38,$38
        EQUB $40,$40,$40,$40,$48,$48,$48,$48
        EQUB $50,$50,$50,$50,$58,$58,$58,$58
        EQUB $60,$60,$60,$60,$68,$68,$68,$68
        EQUB $70,$70,$70,$70,$78,$78,$78,$78
        EQUB $80,$80,$80,$80,$88,$88,$88,$88
        EQUB $90,$90,$90,$90,$98,$98,$98,$98
        EQUB $A0,$A0,$A0,$A0,$A8,$A8,$A8,$A8
        EQUB $B0,$B0,$B0,$B0,$B8,$B8,$B8,$B8
        EQUB $C0,$C0,$C0,$C0,$C8,$C8,$C8,$C8
        EQUB $D0,$D0,$D0,$D0,$D8,$D8,$D8,$D8
        EQUB $E0,$E0,$E0,$E0,$E8,$E8,$E8,$E8
        EQUB $F0,$F0,$F0,$F0,$F8,$F8,$F8,$F8

.LINE_DRAW_PTR_LO   EQUB <LINE_DRAW_RADS_0,<LINE_DRAW_RSDA_1,<LINE_DRAW_LSDA_2,<LINE_DRAW_LADS_3,<LINE_DRAW_LAUS_4,<LINE_DRAW_LSUA_5,<LINE_DRAW_RSUA_6,<LINE_DRAW_RAUS_7 
.LINE_DRAW_PTR_HI   EQUB >LINE_DRAW_RADS_0,>LINE_DRAW_RSDA_1,>LINE_DRAW_LSDA_2,>LINE_DRAW_LADS_3,>LINE_DRAW_LAUS_4,>LINE_DRAW_LSUA_5,>LINE_DRAW_RSUA_6,>LINE_DRAW_RAUS_7 
.LINE_DRAW_CONV_PTR EQUB $00,$07,$03,$04,$01,$06,$02,$05

.PERS_HOR_EDGE   EQUB CLIP_RIGHT,CLIP_RIGHT,$00,$00,$00,$00,CLIP_RIGHT,CLIP_RIGHT	; right or left edge of screen todo?
.PERS_VER_EDGE   EQUB CLIP_BOTTOM,CLIP_BOTTOM,CLIP_BOTTOM,CLIP_BOTTOM,$00,$00,$00,$00	; bottom or top of screen todo?
.fBBE8   EQUB $40,$38,$08,$00,$40,$38,$28,$30
		EQUB $40,$18,$08,$10,$40,$18,$28,$20
.fBBF8   EQUB $00,$00,$03,$00,$00,$00,$02,$02
		EQUB $00,$00,$00,$06,$04,$00,$00,$04
		EQUB $0C,$00,$00,$00,$08,$08,$00,$00
		EQUB $00,$09,$00,$00,$00,$01,$01,$00	
.fBC18   EQUB $01,$00,$00,$00,$01,$01,$00,$00
		EQUB $00,$01,$00,$00,$00,$01,$01,$00
		EQUB $00,$00,$01,$00,$00,$00,$01,$01
		EQUB $00,$00,$00,$01,$01,$00,$00,$01

; 00
; 01
; 10
; 11

; 10001000
; 01000100
; 00100010
; 00010001

; $88
; $44


ALIGN &100
.ORA_PIXEL_POS
		EQUB %10001000,%01000100,%00100010,%00010001,%10001000,%01000100,%00100010,%00010001
		EQUB %10001000,%01000100,%00100010,%00010001,%10001000,%01000100,%00100010,%00010001
		EQUB %10001000,%01000100,%00100010,%00010001,%10001000,%01000100,%00100010,%00010001
		EQUB %10001000,%01000100,%00100010,%00010001,%10001000,%01000100,%00100010,%00010001
		EQUB %10001000,%01000100,%00100010,%00010001,%10001000,%01000100,%00100010,%00010001
		EQUB %10001000,%01000100,%00100010,%00010001,%10001000,%01000100,%00100010,%00010001
		EQUB %10001000,%01000100,%00100010,%00010001,%10001000,%01000100,%00100010,%00010001
		EQUB %10001000,%01000100,%00100010,%00010001,%10001000,%01000100,%00100010,%00010001
		EQUB %10001000,%01000100,%00100010,%00010001,%10001000,%01000100,%00100010,%00010001
		EQUB %10001000,%01000100,%00100010,%00010001,%10001000,%01000100,%00100010,%00010001
		EQUB %10001000,%01000100,%00100010,%00010001,%10001000,%01000100,%00100010,%00010001
		EQUB %10001000,%01000100,%00100010,%00010001,%10001000,%01000100,%00100010,%00010001
		EQUB %10001000,%01000100,%00100010,%00010001,%10001000,%01000100,%00100010,%00010001
		EQUB %10001000,%01000100,%00100010,%00010001,%10001000,%01000100,%00100010,%00010001
		EQUB %10001000,%01000100,%00100010,%00010001,%10001000,%01000100,%00100010,%00010001
		EQUB %10001000,%01000100,%00100010,%00010001,%10001000,%01000100,%00100010,%00010001

ALIGN &100
.AND_PIXEL_POS
		EQUB %01110111,%10111011,%11011101,%11101110,%01110111,%10111011,%11011101,%11101110
		EQUB %01110111,%10111011,%11011101,%11101110,%01110111,%10111011,%11011101,%11101110
		EQUB %01110111,%10111011,%11011101,%11101110,%01110111,%10111011,%11011101,%11101110
		EQUB %01110111,%10111011,%11011101,%11101110,%01110111,%10111011,%11011101,%11101110
		EQUB %01110111,%10111011,%11011101,%11101110,%01110111,%10111011,%11011101,%11101110
		EQUB %01110111,%10111011,%11011101,%11101110,%01110111,%10111011,%11011101,%11101110
		EQUB %01110111,%10111011,%11011101,%11101110,%01110111,%10111011,%11011101,%11101110
		EQUB %01110111,%10111011,%11011101,%11101110,%01110111,%10111011,%11011101,%11101110
		EQUB %01110111,%10111011,%11011101,%11101110,%01110111,%10111011,%11011101,%11101110
		EQUB %01110111,%10111011,%11011101,%11101110,%01110111,%10111011,%11011101,%11101110
		EQUB %01110111,%10111011,%11011101,%11101110,%01110111,%10111011,%11011101,%11101110
		EQUB %01110111,%10111011,%11011101,%11101110,%01110111,%10111011,%11011101,%11101110
		EQUB %01110111,%10111011,%11011101,%11101110,%01110111,%10111011,%11011101,%11101110
		EQUB %01110111,%10111011,%11011101,%11101110,%01110111,%10111011,%11011101,%11101110
		EQUB %01110111,%10111011,%11011101,%11101110,%01110111,%10111011,%11011101,%11101110
		EQUB %01110111,%10111011,%11011101,%11101110,%01110111,%10111011,%11011101,%11101110
	
SC0_LN000 = SCR0+(32*000)+0
SC0_LN001 = SCR0+(32*000)+1
SC0_LN002 = SCR0+(32*000)+2
SC0_LN003 = SCR0+(32*000)+3
SC0_LN004 = SCR0+(32*000)+4
SC0_LN005 = SCR0+(32*000)+5
SC0_LN006 = SCR0+(32*000)+6
SC0_LN007 = SCR0+(32*000)+7
SC0_LN008 = SCR0+(32*008)+0
SC0_LN009 = SCR0+(32*008)+1
SC0_LN010 = SCR0+(32*008)+2
SC0_LN011 = SCR0+(32*008)+3
SC0_LN012 = SCR0+(32*008)+4
SC0_LN013 = SCR0+(32*008)+5
SC0_LN014 = SCR0+(32*008)+6
SC0_LN015 = SCR0+(32*008)+7
SC0_LN016 = SCR0+(32*016)+0
SC0_LN017 = SCR0+(32*016)+1
SC0_LN018 = SCR0+(32*016)+2
SC0_LN019 = SCR0+(32*016)+3
SC0_LN020 = SCR0+(32*016)+4
SC0_LN021 = SCR0+(32*016)+5
SC0_LN022 = SCR0+(32*016)+6
SC0_LN023 = SCR0+(32*016)+7
SC0_LN024 = SCR0+(32*024)+0
SC0_LN025 = SCR0+(32*024)+1
SC0_LN026 = SCR0+(32*024)+2
SC0_LN027 = SCR0+(32*024)+3
SC0_LN028 = SCR0+(32*024)+4
SC0_LN029 = SCR0+(32*024)+5
SC0_LN030 = SCR0+(32*024)+6
SC0_LN031 = SCR0+(32*024)+7
SC0_LN032 = SCR0+(32*032)+0
SC0_LN033 = SCR0+(32*032)+1
SC0_LN034 = SCR0+(32*032)+2
SC0_LN035 = SCR0+(32*032)+3
SC0_LN036 = SCR0+(32*032)+4
SC0_LN037 = SCR0+(32*032)+5
SC0_LN038 = SCR0+(32*032)+6
SC0_LN039 = SCR0+(32*032)+7
SC0_LN040 = SCR0+(32*040)+0
SC0_LN041 = SCR0+(32*040)+1
SC0_LN042 = SCR0+(32*040)+2
SC0_LN043 = SCR0+(32*040)+3
SC0_LN044 = SCR0+(32*040)+4
SC0_LN045 = SCR0+(32*040)+5
SC0_LN046 = SCR0+(32*040)+6
SC0_LN047 = SCR0+(32*040)+7
SC0_LN048 = SCR0+(32*048)+0
SC0_LN049 = SCR0+(32*048)+1
SC0_LN050 = SCR0+(32*048)+2
SC0_LN051 = SCR0+(32*048)+3
SC0_LN052 = SCR0+(32*048)+4
SC0_LN053 = SCR0+(32*048)+5
SC0_LN054 = SCR0+(32*048)+6
SC0_LN055 = SCR0+(32*048)+7
SC0_LN056 = SCR0+(32*056)+0
SC0_LN057 = SCR0+(32*056)+1
SC0_LN058 = SCR0+(32*056)+2
SC0_LN059 = SCR0+(32*056)+3
SC0_LN060 = SCR0+(32*056)+4
SC0_LN061 = SCR0+(32*056)+5
SC0_LN062 = SCR0+(32*056)+6
SC0_LN063 = SCR0+(32*056)+7
SC0_LN064 = SCR0+(32*064)+0
SC0_LN065 = SCR0+(32*064)+1
SC0_LN066 = SCR0+(32*064)+2
SC0_LN067 = SCR0+(32*064)+3
SC0_LN068 = SCR0+(32*064)+4
SC0_LN069 = SCR0+(32*064)+5
SC0_LN070 = SCR0+(32*064)+6
SC0_LN071 = SCR0+(32*064)+7
SC0_LN072 = SCR0+(32*072)+0
SC0_LN073 = SCR0+(32*072)+1
SC0_LN074 = SCR0+(32*072)+2
SC0_LN075 = SCR0+(32*072)+3
SC0_LN076 = SCR0+(32*072)+4
SC0_LN077 = SCR0+(32*072)+5
SC0_LN078 = SCR0+(32*072)+6
SC0_LN079 = SCR0+(32*072)+7
SC0_LN080 = SCR0+(32*080)+0
SC0_LN081 = SCR0+(32*080)+1
SC0_LN082 = SCR0+(32*080)+2
SC0_LN083 = SCR0+(32*080)+3
SC0_LN084 = SCR0+(32*080)+4
SC0_LN085 = SCR0+(32*080)+5
SC0_LN086 = SCR0+(32*080)+6
SC0_LN087 = SCR0+(32*080)+7	
SC0_LN088 = SCR0+(32*088)+0
SC0_LN089 = SCR0+(32*088)+1
SC0_LN090 = SCR0+(32*088)+2
SC0_LN091 = SCR0+(32*088)+3
SC0_LN092 = SCR0+(32*088)+4
SC0_LN093 = SCR0+(32*088)+5
SC0_LN094 = SCR0+(32*088)+6
SC0_LN095 = SCR0+(32*088)+7
SC0_LN096 = SCR0+(32*096)+0
SC0_LN097 = SCR0+(32*096)+1
SC0_LN098 = SCR0+(32*096)+2
SC0_LN099 = SCR0+(32*096)+3
SC0_LN100 = SCR0+(32*096)+4
SC0_LN101 = SCR0+(32*096)+5
SC0_LN102 = SCR0+(32*096)+6
SC0_LN103 = SCR0+(32*096)+7
SC0_LN104 = SCR0+(32*104)+0
SC0_LN105 = SCR0+(32*104)+1
SC0_LN106 = SCR0+(32*104)+2
SC0_LN107 = SCR0+(32*104)+3
SC0_LN108 = SCR0+(32*104)+4
SC0_LN109 = SCR0+(32*104)+5
SC0_LN110 = SCR0+(32*104)+6
SC0_LN111 = SCR0+(32*104)+7
SC0_LN112 = SCR0+(32*112)+0
SC0_LN113 = SCR0+(32*112)+1
SC0_LN114 = SCR0+(32*112)+2
SC0_LN115 = SCR0+(32*112)+3
SC0_LN116 = SCR0+(32*112)+4
SC0_LN117 = SCR0+(32*112)+5
SC0_LN118 = SCR0+(32*112)+6
SC0_LN119 = SCR0+(32*112)+7
SC0_LN120 = SCR0+(32*120)+0
SC0_LN121 = SCR0+(32*120)+1
SC0_LN122 = SCR0+(32*120)+2
SC0_LN123 = SCR0+(32*120)+3
SC0_LN124 = SCR0+(32*120)+4
SC0_LN125 = SCR0+(32*120)+5
SC0_LN126 = SCR0+(32*120)+6
SC0_LN127 = SCR0+(32*120)+7

SC0_LN128 = SCR0+(32*128)+0
SC0_LN129 = SCR0+(32*128)+1
SC0_LN130 = SCR0+(32*128)+2
SC0_LN131 = SCR0+(32*128)+3
SC0_LN132 = SCR0+(32*128)+4
SC0_LN133 = SCR0+(32*128)+5
SC0_LN134 = SCR0+(32*128)+6
SC0_LN135 = SCR0+(32*128)+7
SC0_LN136 = SCR0+(32*136)+0
SC0_LN137 = SCR0+(32*136)+1
SC0_LN138 = SCR0+(32*136)+2
SC0_LN139 = SCR0+(32*136)+3
SC0_LN140 = SCR0+(32*136)+4
SC0_LN141 = SCR0+(32*136)+5
SC0_LN142 = SCR0+(32*136)+6
SC0_LN143 = SCR0+(32*136)+7
SC0_LN144 = SCR0+(32*144)+0
SC0_LN145 = SCR0+(32*144)+1
SC0_LN146 = SCR0+(32*144)+2
SC0_LN147 = SCR0+(32*144)+3
SC0_LN148 = SCR0+(32*144)+4
SC0_LN149 = SCR0+(32*144)+5
SC0_LN150 = SCR0+(32*144)+6
SC0_LN151 = SCR0+(32*144)+7
SC0_LN152 = SCR0+(32*152)+0
SC0_LN153 = SCR0+(32*152)+1
SC0_LN154 = SCR0+(32*152)+2
SC0_LN155 = SCR0+(32*152)+3
SC0_LN156 = SCR0+(32*152)+4
SC0_LN157 = SCR0+(32*152)+5
SC0_LN158 = SCR0+(32*152)+6
SC0_LN159 = SCR0+(32*152)+7
SC0_LN160 = SCR0+(32*160)+0
SC0_LN161 = SCR0+(32*160)+1
SC0_LN162 = SCR0+(32*160)+2
SC0_LN163 = SCR0+(32*160)+3
SC0_LN164 = SCR0+(32*160)+4
SC0_LN165 = SCR0+(32*160)+5
SC0_LN166 = SCR0+(32*160)+6
SC0_LN167 = SCR0+(32*160)+7

SC0_LN168 = SCR0+(32*168)+0
SC0_LN169 = SCR0+(32*168)+1
SC0_LN170 = SCR0+(32*168)+2
SC0_LN171 = SCR0+(32*168)+3
SC0_LN172 = SCR0+(32*168)+4
SC0_LN173 = SCR0+(32*168)+5
SC0_LN174 = SCR0+(32*168)+6
SC0_LN175 = SCR0+(32*168)+7
SC0_LN176 = SCR0+(32*176)+0
SC0_LN177 = SCR0+(32*176)+1
SC0_LN178 = SCR0+(32*176)+2
SC0_LN179 = SCR0+(32*176)+3
SC0_LN180 = SCR0+(32*176)+4
SC0_LN181 = SCR0+(32*176)+5
SC0_LN182 = SCR0+(32*176)+6
SC0_LN183 = SCR0+(32*176)+7
SC0_LN184 = SCR0+(32*184)+0
SC0_LN185 = SCR0+(32*184)+1
SC0_LN186 = SCR0+(32*184)+2
SC0_LN187 = SCR0+(32*184)+3
SC0_LN188 = SCR0+(32*184)+4
SC0_LN189 = SCR0+(32*184)+5
SC0_LN190 = SCR0+(32*184)+6
SC0_LN191 = SCR0+(32*184)+7

SC1_LN000 = SCR1+(32*000)+0
SC1_LN001 = SCR1+(32*000)+1
SC1_LN002 = SCR1+(32*000)+2
SC1_LN003 = SCR1+(32*000)+3
SC1_LN004 = SCR1+(32*000)+4
SC1_LN005 = SCR1+(32*000)+5
SC1_LN006 = SCR1+(32*000)+6
SC1_LN007 = SCR1+(32*000)+7
SC1_LN008 = SCR1+(32*008)+0
SC1_LN009 = SCR1+(32*008)+1
SC1_LN010 = SCR1+(32*008)+2
SC1_LN011 = SCR1+(32*008)+3
SC1_LN012 = SCR1+(32*008)+4
SC1_LN013 = SCR1+(32*008)+5
SC1_LN014 = SCR1+(32*008)+6
SC1_LN015 = SCR1+(32*008)+7
SC1_LN016 = SCR1+(32*016)+0
SC1_LN017 = SCR1+(32*016)+1
SC1_LN018 = SCR1+(32*016)+2
SC1_LN019 = SCR1+(32*016)+3
SC1_LN020 = SCR1+(32*016)+4
SC1_LN021 = SCR1+(32*016)+5
SC1_LN022 = SCR1+(32*016)+6
SC1_LN023 = SCR1+(32*016)+7
SC1_LN024 = SCR1+(32*024)+0
SC1_LN025 = SCR1+(32*024)+1
SC1_LN026 = SCR1+(32*024)+2
SC1_LN027 = SCR1+(32*024)+3
SC1_LN028 = SCR1+(32*024)+4
SC1_LN029 = SCR1+(32*024)+5
SC1_LN030 = SCR1+(32*024)+6
SC1_LN031 = SCR1+(32*024)+7
SC1_LN032 = SCR1+(32*032)+0
SC1_LN033 = SCR1+(32*032)+1
SC1_LN034 = SCR1+(32*032)+2
SC1_LN035 = SCR1+(32*032)+3
SC1_LN036 = SCR1+(32*032)+4
SC1_LN037 = SCR1+(32*032)+5
SC1_LN038 = SCR1+(32*032)+6
SC1_LN039 = SCR1+(32*032)+7
SC1_LN040 = SCR1+(32*040)+0
SC1_LN041 = SCR1+(32*040)+1
SC1_LN042 = SCR1+(32*040)+2
SC1_LN043 = SCR1+(32*040)+3
SC1_LN044 = SCR1+(32*040)+4
SC1_LN045 = SCR1+(32*040)+5
SC1_LN046 = SCR1+(32*040)+6
SC1_LN047 = SCR1+(32*040)+7
SC1_LN048 = SCR1+(32*048)+0
SC1_LN049 = SCR1+(32*048)+1
SC1_LN050 = SCR1+(32*048)+2
SC1_LN051 = SCR1+(32*048)+3
SC1_LN052 = SCR1+(32*048)+4
SC1_LN053 = SCR1+(32*048)+5
SC1_LN054 = SCR1+(32*048)+6
SC1_LN055 = SCR1+(32*048)+7
SC1_LN056 = SCR1+(32*056)+0
SC1_LN057 = SCR1+(32*056)+1
SC1_LN058 = SCR1+(32*056)+2
SC1_LN059 = SCR1+(32*056)+3
SC1_LN060 = SCR1+(32*056)+4
SC1_LN061 = SCR1+(32*056)+5
SC1_LN062 = SCR1+(32*056)+6
SC1_LN063 = SCR1+(32*056)+7
SC1_LN064 = SCR1+(32*064)+0
SC1_LN065 = SCR1+(32*064)+1
SC1_LN066 = SCR1+(32*064)+2
SC1_LN067 = SCR1+(32*064)+3
SC1_LN068 = SCR1+(32*064)+4
SC1_LN069 = SCR1+(32*064)+5
SC1_LN070 = SCR1+(32*064)+6
SC1_LN071 = SCR1+(32*064)+7
SC1_LN072 = SCR1+(32*072)+0
SC1_LN073 = SCR1+(32*072)+1
SC1_LN074 = SCR1+(32*072)+2
SC1_LN075 = SCR1+(32*072)+3
SC1_LN076 = SCR1+(32*072)+4
SC1_LN077 = SCR1+(32*072)+5
SC1_LN078 = SCR1+(32*072)+6
SC1_LN079 = SCR1+(32*072)+7
SC1_LN080 = SCR1+(32*080)+0
SC1_LN081 = SCR1+(32*080)+1
SC1_LN082 = SCR1+(32*080)+2
SC1_LN083 = SCR1+(32*080)+3
SC1_LN084 = SCR1+(32*080)+4
SC1_LN085 = SCR1+(32*080)+5
SC1_LN086 = SCR1+(32*080)+6
SC1_LN087 = SCR1+(32*080)+7	
SC1_LN088 = SCR1+(32*088)+0
SC1_LN089 = SCR1+(32*088)+1
SC1_LN090 = SCR1+(32*088)+2
SC1_LN091 = SCR1+(32*088)+3
SC1_LN092 = SCR1+(32*088)+4
SC1_LN093 = SCR1+(32*088)+5
SC1_LN094 = SCR1+(32*088)+6
SC1_LN095 = SCR1+(32*088)+7
SC1_LN096 = SCR1+(32*096)+0
SC1_LN097 = SCR1+(32*096)+1
SC1_LN098 = SCR1+(32*096)+2
SC1_LN099 = SCR1+(32*096)+3
SC1_LN100 = SCR1+(32*096)+4
SC1_LN101 = SCR1+(32*096)+5
SC1_LN102 = SCR1+(32*096)+6
SC1_LN103 = SCR1+(32*096)+7
SC1_LN104 = SCR1+(32*104)+0
SC1_LN105 = SCR1+(32*104)+1
SC1_LN106 = SCR1+(32*104)+2
SC1_LN107 = SCR1+(32*104)+3
SC1_LN108 = SCR1+(32*104)+4
SC1_LN109 = SCR1+(32*104)+5
SC1_LN110 = SCR1+(32*104)+6
SC1_LN111 = SCR1+(32*104)+7
SC1_LN112 = SCR1+(32*112)+0
SC1_LN113 = SCR1+(32*112)+1
SC1_LN114 = SCR1+(32*112)+2
SC1_LN115 = SCR1+(32*112)+3
SC1_LN116 = SCR1+(32*112)+4
SC1_LN117 = SCR1+(32*112)+5
SC1_LN118 = SCR1+(32*112)+6
SC1_LN119 = SCR1+(32*112)+7
SC1_LN120 = SCR1+(32*120)+0
SC1_LN121 = SCR1+(32*120)+1
SC1_LN122 = SCR1+(32*120)+2
SC1_LN123 = SCR1+(32*120)+3
SC1_LN124 = SCR1+(32*120)+4
SC1_LN125 = SCR1+(32*120)+5
SC1_LN126 = SCR1+(32*120)+6
SC1_LN127 = SCR1+(32*120)+7

SC1_LN128 = SCR1+(32*128)+0
SC1_LN129 = SCR1+(32*128)+1
SC1_LN130 = SCR1+(32*128)+2
SC1_LN131 = SCR1+(32*128)+3
SC1_LN132 = SCR1+(32*128)+4
SC1_LN133 = SCR1+(32*128)+5
SC1_LN134 = SCR1+(32*128)+6
SC1_LN135 = SCR1+(32*128)+7
SC1_LN136 = SCR1+(32*136)+0
SC1_LN137 = SCR1+(32*136)+1
SC1_LN138 = SCR1+(32*136)+2
SC1_LN139 = SCR1+(32*136)+3
SC1_LN140 = SCR1+(32*136)+4
SC1_LN141 = SCR1+(32*136)+5
SC1_LN142 = SCR1+(32*136)+6
SC1_LN143 = SCR1+(32*136)+7
SC1_LN144 = SCR1+(32*144)+0
SC1_LN145 = SCR1+(32*144)+1
SC1_LN146 = SCR1+(32*144)+2
SC1_LN147 = SCR1+(32*144)+3
SC1_LN148 = SCR1+(32*144)+4
SC1_LN149 = SCR1+(32*144)+5
SC1_LN150 = SCR1+(32*144)+6
SC1_LN151 = SCR1+(32*144)+7
SC1_LN152 = SCR1+(32*152)+0
SC1_LN153 = SCR1+(32*152)+1
SC1_LN154 = SCR1+(32*152)+2
SC1_LN155 = SCR1+(32*152)+3
SC1_LN156 = SCR1+(32*152)+4
SC1_LN157 = SCR1+(32*152)+5
SC1_LN158 = SCR1+(32*152)+6
SC1_LN159 = SCR1+(32*152)+7
SC1_LN160 = SCR1+(32*160)+0
SC1_LN161 = SCR1+(32*160)+1
SC1_LN162 = SCR1+(32*160)+2
SC1_LN163 = SCR1+(32*160)+3
SC1_LN164 = SCR1+(32*160)+4
SC1_LN165 = SCR1+(32*160)+5
SC1_LN166 = SCR1+(32*160)+6
SC1_LN167 = SCR1+(32*160)+7

SC1_LN168 = SCR1+(32*168)+0
SC1_LN169 = SCR1+(32*168)+1
SC1_LN170 = SCR1+(32*168)+2
SC1_LN171 = SCR1+(32*168)+3
SC1_LN172 = SCR1+(32*168)+4
SC1_LN173 = SCR1+(32*168)+5
SC1_LN174 = SCR1+(32*168)+6
SC1_LN175 = SCR1+(32*168)+7
SC1_LN176 = SCR1+(32*176)+0
SC1_LN177 = SCR1+(32*176)+1
SC1_LN178 = SCR1+(32*176)+2
SC1_LN179 = SCR1+(32*176)+3
SC1_LN180 = SCR1+(32*176)+4
SC1_LN181 = SCR1+(32*176)+5
SC1_LN182 = SCR1+(32*176)+6
SC1_LN183 = SCR1+(32*176)+7
SC1_LN184 = SCR1+(32*184)+0
SC1_LN185 = SCR1+(32*184)+1
SC1_LN186 = SCR1+(32*184)+2
SC1_LN187 = SCR1+(32*184)+3
SC1_LN188 = SCR1+(32*184)+4
SC1_LN189 = SCR1+(32*184)+5
SC1_LN190 = SCR1+(32*184)+6
SC1_LN191 = SCR1+(32*184)+7

.HEX0  EQUB %01110111,%00100010,%01110111,%01110111,%01010101,%01110111,%01110111,%01110111,%01110111,%01110111,%00100010,%01100110,%01110111,%01100110,%01110111,%01110111 
.HEX1  EQUB %01010101,%01100110,%00010001,%00010001,%01010101,%01000100,%01000100,%00010001,%01010101,%01010101,%01010101,%01010101,%01000100,%01010101,%01000100,%01000100 
.HEX2  EQUB %01010101,%00100010,%01110111,%01110111,%01110111,%01110111,%01110111,%00010001,%01110111,%01110111,%01110111,%01100110,%01000100,%01010101,%01110111,%01110111
.HEX3  EQUB %01010101,%00100010,%01000100,%00010001,%00010001,%00010001,%01010101,%00010001,%01010101,%00010001,%01010101,%01010101,%01000100,%01010101,%01000100,%01000100
.HEX4  EQUB %01110111,%01110111,%01110111,%01110111,%00010001,%01110111,%01110111,%00010001,%01110111,%00010001,%01010101,%01100110,%01110111,%01100110,%01110111,%01000100

.SCREEN_SETUP_PARAMS_START ; mode1Message
EQUB 22, 5                             ; MODE 5

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

.END

SAVE "256beeb", START, END, START
