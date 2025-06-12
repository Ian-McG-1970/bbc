
systemVIAInterruptFlagRegister_fe4d      = $fe4d     ;

logicalScreenStart                  = 20480 ; 20464 ; $5800 ; $3000 ; $5800 ; $3000     ;

; ***************** OS CALLS ********************
OSWRCH                          = $ffee     ;
OSWORD                          = $fff1     ;
OSBYTE                          = $fff4     ;


CC_TOP = 1
CC_BOTTOM = 2
CC_LEFT = 4
CC_RIGHT = 8
CC_BEHIND = 16
CC_ALL = 255

CLIP_OFFSET = 16384
MAX_PNT = 64
       
RXL = 512
RXH = RXL+MAX_PNT
RYL = RXH+MAX_PNT
RYH = RYL+MAX_PNT
RZL = RYH+MAX_PNT
RZH = RZL+MAX_PNT
PXL = RZH+MAX_PNT
PXH = PXL+MAX_PNT
PYL = PXH+MAX_PNT
PYH = PYL+MAX_PNT
PCC = PYH+MAX_PNT

SCNX = 128/2
SCNY = 128/2

SCR = logicalScreenStart
SCR0 = SCR
SCR1 = SCR0+6144

SCR0_ADR = SCR0 / 8
SCR1_ADR = SCR1 / 8

; zero page memory locations
ORG &0

.SCN		SKIP 2
.SCN_ZLO	SKIP 2

.REGA_BUF SKIP 1
.REGX_BUF SKIP 1
.REGY_BUF SKIP 1

.LINE_FRAC SKIP 1
.LINE_INC SKIP 1
.LINE_VER SKIP 1
.LINE_END SKIP 1.
.H_ST SKIP 1
.H_EN SKIP 1
.V_ST SKIP 1
.V_EN SKIP 1

.P_SQR_LO SKIP 2
.P_SQR_HI SKIP 2
.P_INVSQR_LO SKIP 2
.P_INVSQR_HI SKIP 2

.H_POS SKIP 1
.V_POS SKIP 1

.PNT_CNT SKIP 1
.LINE_CNT SKIP 1

.CC_OR SKIP 1
.CC_AND	SKIP 1
.CC SKIP 1

.X_PNT_LO SKIP 1
.X_PNT_HI SKIP 1
.Y_PNT_LO SKIP 1
.Y_PNT_HI SKIP 1
.Z_PNT_LO SKIP 1
.Z_PNT_HI SKIP 1
.X_OFF_LO SKIP 1
.X_OFF_HI SKIP 1
.Y_OFF_LO SKIP 1
.Y_OFF_HI SKIP 1
.Z_OFF_LO SKIP 1
.Z_OFF_HI SKIP 1

.X0	 SKIP 1
.X1	 SKIP 1
.Y0	 SKIP 1
.Y1	 SKIP 1
.Z0	 SKIP 1
.Z1	 SKIP 1
.Z2	 SKIP 1
.Z3	 SKIP 1

.X_CLIP_S_PNT_LO SKIP 1
.X_CLIP_S_PNT_HI SKIP 1
.Y_CLIP_S_PNT_LO  SKIP 1
.Y_CLIP_S_PNT_HI  SKIP 1
.Z_CLIP_S_PNT_LO  SKIP 1
.Z_CLIP_S_PNT_HI  SKIP 1
.X_CLIP_E_PNT_LO  SKIP 1
.X_CLIP_E_PNT_HI  SKIP 1
.Y_CLIP_E_PNT_LO  SKIP 1
.Y_CLIP_E_PNT_HI  SKIP 1
.Z_CLIP_E_PNT_LO  SKIP 1
.Z_CLIP_E_PNT_HI  SKIP 1
.X_CLIP_S_PER_LO  SKIP 1
.X_CLIP_S_PER_HI  SKIP 1
.Y_CLIP_S_PER_LO  SKIP 1
.Y_CLIP_S_PER_HI  SKIP 1

.X_CLIP_E_PER_LO SKIP 1
.X_CLIP_E_PER_HI SKIP 1
.Y_CLIP_E_PER_LO SKIP 1
.Y_CLIP_E_PER_HI SKIP 1

.CC_S  SKIP 1
.CC_E SKIP 1

.X_ROT   SKIP 2
.Y_ROT   SKIP 2
.Z_ROT   SKIP 2
.YC_TEMP  SKIP 2
.YS_TEMP  SKIP 2
.ZC_TEMP  SKIP 2
.ZS_TEMP  SKIP 2
.XC_TEMP  SKIP 2
.XS_TEMP  SKIP 2
.T2   SKIP 1
.T8   SKIP 1
.T6   SKIP 1
.T1   SKIP 1
.T4  SKIP 1
.T9   SKIP 1
.T3  SKIP 1
.T5   SKIP 1
.T7   SKIP 1
.T10   SKIP 1
.tmp1   SKIP 1
.tmp2   SKIP 1
.tmp3   SKIP 1
.XSINTMP   SKIP 1
.XCOSTMP   SKIP 1
.YSINTMP   SKIP 1
.YCOSTMP   SKIP 1
.ZSINTMP   SKIP 1
.ZCOSTMP   SKIP 1

.X_CLIP_STT SKIP 2
.X_CLIP_END SKIP 2
.Y_CLIP_STT SKIP 2
.Y_CLIP_END SKIP 2
.Z_CLIP_STT SKIP 2
.Z_CLIP_END SKIP 2
.X_CLIP_MID SKIP 2
.Y_CLIP_MID SKIP 2
.Z_CLIP_MID SKIP 2
.CLIP_MID SKIP 2


ORG &1C05

.START

 LDA   #>sqrlo
 STA   P_SQR_LO+1
 LDA   #>sqrhi
 STA   P_SQR_HI+1
 LDA   #>negsqrlo
 STA   P_INVSQR_LO+1
 LDA   #>negsqrhi
 STA   P_INVSQR_HI+1

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

    SEI									    ; install irq code

 LDA #0
 sta X_ROT
 sta Y_ROT
 sta Z_ROT
 sta X_ROT+1
 sta Y_ROT+1
 sta Z_ROT+1

 LDA  #<(-10)
 STA  Y_OFF_LO
 LDA  #>(-10)
 STA  Y_OFF_HI

 LDA #<(-10)
 STA  X_OFF_LO
 LDA #>(-10)
 STA  X_OFF_HI

 LDA  #<(9900)
 STA  Z_OFF_LO
 LDA  #>(9900)
 STA  Z_OFF_HI


.MLOOP	JSR VSYNC

	JSR SWAP_SCREEN

	LDY #0
	LDX H_POS
;	JSR PLOT

	LDY #127
	LDX H_POS
;	JSR PLOT

	LDA V_POS
	LSR A
	TAY
	LDX #0
;	JSR PLOT

	LDA V_POS
	LSR A
	TAY
	LDX #255
;	JSR PLOT

      LDA   #0
      STA   H_ST
      LDA   #0
      STA   V_ST
      LDA   H_POS
      STA   H_EN
      LDA   V_POS
	  LSR A
      STA   V_EN
;      JSR   LINE

      LDA   #255
      STA   H_ST
      LDA   #127
      STA   V_ST
      LDA   H_POS
      STA   H_EN
      LDA   V_POS
	  LSR A
      STA   V_EN
;      JSR   LINE

      LDA   #255
      STA   H_ST
      LDA   #0
      STA   V_ST
      LDA   H_POS
      STA   H_EN
      LDA   V_POS
	  LSR A
      STA   V_EN
;      JSR   LINE

      LDA   #0
      STA   H_ST
      LDA   #127
      STA   V_ST
      LDA   H_POS
      STA   H_EN
      LDA   V_POS
	  LSR A
      STA   V_EN
;      JSR   LINE

 JSR  DRAW_OBJ

	INC H_POS
	DEC	V_POS
;
; JSR DEBUG_WRITE

 LDA X_ROT
 CLC
 ADC #1
 STA X_ROT
 LDA X_ROT+1
 ADC #0
 AND #3
 STA X_ROT+1
 
 LDA Y_ROT
 SEC
 SBC #2
 STA Y_ROT
 LDA Y_ROT+1
 SBC #0
 AND #3
 STA Y_ROT+1

 LDA Z_ROT
 CLC
 ADC #3
 STA Z_ROT
 LDA Z_ROT+1
 ADC #0
 AND #3
 STA Z_ROT+1

		JMP MLOOP

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
		LDA	#0 ; %00111001 ; not needed
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
.CONT	LDA #<SCR1_MONO_HI
		STA	PLOT_HI +1	
		STA DRAW_BANK_HI_1 +1
		STA DRAW_BANK_HI_2 +1
		STA DRAW_BANK_HI_3 +1
		STA DRAW_BANK_HI_4 +1
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
		LDA	#0 ; %00111001 ; not needed
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
.CONT	LDA #<SCR0_MONO_HI
		STA	PLOT_HI +1
		STA DRAW_BANK_HI_1 +1
		STA DRAW_BANK_HI_2 +1
		STA DRAW_BANK_HI_3 +1
		STA DRAW_BANK_HI_4 +1
		RTS
}

.DEBUG_WRITE

 LDA V_POS
 LDX #0
 LDY #0
 JSR HEX8

 LDA H_POS
 LDX #0
 LDY #8
 JSR HEX8

 RTS
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
 

.LINE_H_SAME	LDA   V_EN
				CMP   V_ST
				BNE   LINE_NOT_PLOT
			RTS

.PLOT
.PLOT_HI	LDA   	SCR0_MONO_HI,Y 
			STA   	SCN_ZLO+1
			TYA
			AND 	#7
            ORA   	SCR_MONO_HOR,X
			TAY
            LDA   	(SCN_ZLO),Y
			ORA   	SCR_MONO_OR,X
            STA   	(SCN_ZLO),Y 
            RTS
                  
.LINE			LDA   #$FF 
				STA   LINE_FRAC
                  
				LDA   H_ST
                  CMP   H_EN
                  BEQ   LINE_H_SAME
                  BCC   LINE_NOSWAP
                    LDY   H_EN
                    STY   H_ST
                    STA   H_EN
                    LDX   V_EN
                    LDY   V_ST
                    STX   V_ST
                    STY   V_EN

.LINE_NOSWAP   	LDA   V_EN
				CMP   V_ST
.LINE_NOT_PLOT 	BCC   LINE_BLTR

.LINE_TLBR        SEC 
                  LDA   H_EN
                  SBC   H_ST
                  STA   SCN +1
                  SEC
                  LDA   V_EN
                  SBC   V_ST
                  CMP   SCN +1 ; Accumulator less than location SCN ?
                  BCC   CALC_INC_FRAC_VER_INC_WHOLE_HOR

.CALC_INC_FRAC_HOR_INC_WHOLE_VER	SEC
									LDA   	V_EN
									STA   	LINE_IXIY_END+1 
									SBC   	V_ST
									TAX ; divisor
									SEC
									LDA   	H_EN ; calc x diff
									SBC   	H_ST  ; dividend
									JSR   	DIVMUL_1608 ; a=divisor
									STA   	LINE_IXIY_INC+1
									LDX   	H_ST
									LDY   	V_ST

.LINE_INC_FRAC_HOR_INC_WHOLE_VER  	;									RTS
										STY   	LINE_VER
.DRAW_BANK_HI_1                   		LDA   	SCR0_MONO_HI,Y
										STA   	SCN_ZLO+1
										TYA
										AND		#7
										ORA   	SCR_MONO_HOR,X
										TAY
										LDA   	(SCN_ZLO),Y
										ORA   	SCR_MONO_OR,X
										STA   	(SCN_ZLO),Y 
										LDA   	LINE_FRAC
.LINE_IXIY_INC                    		ADC   	#0
										STA   	LINE_FRAC
										BCC		LINE_IXIY_CONT
													INX
.LINE_IXIY_CONT                   		LDY   	LINE_VER
										INY
.LINE_IXIY_END                    		CPY   	#0
										BNE   LINE_INC_FRAC_HOR_INC_WHOLE_VER
									RTS

.CALC_INC_FRAC_VER_INC_WHOLE_HOR	;									RTS
									SEC
                                  LDA   H_EN
                                  STA   LINE_IYIX_END+1 
                                  SBC   H_ST
                                  TAX         ; divisor
                                  SEC
                                  LDA   V_EN ; calc x diff
                                  SBC   V_ST ; dividend
                                  JSR   DIVMUL_1608
                                  STA   LINE_IYIX_INC+1
                                  LDX   H_ST
                                  LDY   V_ST
                                  BPL   LINE_INC_FRAC_VER_INC_WHOLE_HOR ; JMP
.LINE_BLTR        SEC
                  LDA   H_EN
                  SBC   H_ST
                  STA   SCN +1
                  SEC
                  LDA   V_ST
                  SBC   V_EN
                  CMP   SCN +1 ; Accumulator less than location SCN ?
                  BCC   CALC_DEC_FRAC_VER_INC_WHOLE_HOR

.CALC_INC_FRAC_HOR_DEC_WHOLE_VER  LDA   V_EN
                                  STA   LINE_DXDY_END+1 
                                  SEC
                                  LDA   V_ST
                                  SBC   V_EN
                                  TAX
                                  SEC
                                  LDA   H_EN
                                  SBC   H_ST
                                  JSR   DIVMUL_1608
                                  STA   LINE_DXDY_INC+1
                                  LDX   H_ST
                                  LDY   V_ST
 
.LINE_INC_FRAC_HOR_DEC_WHOLE_VER  	;							RTS
									STY   LINE_VER
.DRAW_BANK_HI_3		                   LDA   SCR0_MONO_HI,Y
									STA   	SCN_ZLO+1
									TYA
									AND 	#7
									ORA   	SCR_MONO_HOR,X
									TAY
									LDA   	(SCN_ZLO),Y
									ORA   SCR_MONO_OR,X
                                 STA   (SCN_ZLO),Y   
                                  LDA   LINE_FRAC
.LINE_DXDY_INC                    ADC   #0
                                  STA   LINE_FRAC
                                  BCC   LINE_DXDY_CONT
                                  INX
.LINE_DXDY_CONT                   LDY   LINE_VER
                                  DEY
.LINE_DXDY_END                    CPY   #0
                                  BNE   LINE_INC_FRAC_HOR_DEC_WHOLE_VER
                                  RTS
                                  
.LINE_IYIX_LOOP                   LDA   LINE_FRAC
.LINE_IYIX_INC                    ADC   #0
                                  STA   LINE_FRAC
                                  BCC   LINE_IYIX_CONT
                                    LDY   LINE_VER
                                    INY
.LINE_INC_FRAC_VER_INC_WHOLE_HOR    STY   LINE_VER
.DRAW_BANK_HI_2                     LDA   SCR0_MONO_HI,Y 
                                    STA   SCN+1 
									TYA
									AND	#7
                                    STA   SCN 
.LINE_IYIX_CONT                   LDY   SCR_MONO_HOR,X
                                  LDA   (SCN),Y
								ORA   SCR_MONO_OR,X
                                  STA   (SCN),Y 
                                  INX
.LINE_IYIX_END                    CPX   #0
                                  BNE   LINE_IYIX_LOOP
                                  RTS 

.CALC_DEC_FRAC_VER_INC_WHOLE_HOR  	;												RTS
									SEC
                                  LDA   H_EN
                                  STA   LINE_DYDX_END+1 
                                  SBC   H_ST
                                  TAX ; divisor
                                  LDA   V_ST ; calc x diff
                                  SBC   V_EN ; dividend
                                  JSR   DIVMUL_1608
                                  STA   LINE_DYDX_INC+1
                                  LDX   H_ST
                                  LDY   V_ST
                                  BPL   LINE_DEC_FRAC_VER_INC_WHOLE_HOR ; JMP
 
.LINE_DYDX_LOOP                   LDA   LINE_FRAC
.LINE_DYDX_INC                    ADC   #0
                                  STA   LINE_FRAC
                                  BCC   LINE_DYDX_CONT
                                    LDY   LINE_VER
                                    DEY
.LINE_DEC_FRAC_VER_INC_WHOLE_HOR    STY   LINE_VER
.DRAW_BANK_HI_4                     LDA   SCR0_MONO_HI,Y 
                                    STA   SCN+1 
									TYA
									AND	#7
                                    STA   SCN 
.LINE_DYDX_CONT                   LDY SCR_MONO_HOR,X
                                  LDA (SCN),Y
								ORA SCR_MONO_OR,X
                                  STA (SCN),Y 
                                  INX
.LINE_DYDX_END                    CPX #0
                                  BNE LINE_DYDX_LOOP
                                  RTS

.DIVMUL_1608
  STA   P_SQR_LO  ; DIV_MUL_U16U08 ; set multiplier as x0
  STA   P_SQR_HI
  EOR   #$ff
  STA   P_INVSQR_LO
  STA   P_INVSQR_HI ;17

  SEC
  LDY   REC16LO,X ; Y=divisor
  LDA   (P_SQR_HI),Y
  SBC   (P_INVSQR_HI),Y
  STA   C1A+1           ; x0*y0h;31 ;c1a means column 1, row a (partial product to be added later)

  LDY   REC16HI,X
  LDA   (P_SQR_LO),y
  SBC   (P_INVSQR_LO),y
		CLC
.C1A	ADC   #0
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

MACRO CLP_CDE_H
{
	BMI   OFF_LEFT       ; -VE = OFF LEFT
    BNE   OFF_RIGHT    ; !0 = OFF RIGHT
      TYA               ; LO
      BPL   EXIT       ; +VE = ON SCREEN
; and #127
; beq .exit ; a holds 0
.OFF_RIGHT              ; -VE = OFF RIGHT
    LDX   #CC_RIGHT     ; OFF RIGHT EDGE
    BNE   EXIT         ; JMP
.OFF_LEFT 
    LDX   #CC_LEFT      ; OFF LEFT EDGE
.EXIT
}
ENDMACRO

MACRO CLP_CDE_V 
{
  BMI   OFF_TOP        ; -VE = OFF LEFT
    BNE   OFF_BOTTOM   ; !0 = OFF RIGHT
      TYA               ; LO
      BPL   EXIT       ; +VE = ON SCREEN
.OFF_BOTTOM             ; -VE = OFF RIGHT
    LDX   #CC_BOTTOM    ; OFF RIGHT EDGE
    BNE   EXIT         ; JMP
.OFF_TOP 
    LDX   #CC_TOP       ; OFF LEFT EDGE
.EXIT
}
ENDMACRO

MACRO CLP_CDE ; X_LO, X_HI, Y_LO, Y_HI, TEMP_CC
;.CLIPCODE ; A = CC
 LDX   #0
 LDY   X_PNT_LO
 LDA   X_PNT_HI
 CLIPCODEH
 STX   CC

 LDX   #0
 LDY   Y_PNT_LO
 LDA   Y_PNT_HI
 CLIPCODEV
 TXA
 ORA   CC
ENDMACRO
 
.DRAW_OBJ:
  LDX #7 ;5 ;#7 ;0
  LDY #11 ;3; 11
; get object number,y
; get point adr hi,y
; get point adr lo,y
; get point count,y
; get line adr hi,y
; get line adr lo,y
; get line count,y
 
  STX   PNT_CNT
  STY   LINE_CNT

  LDX   #CC_ALL
  STX   CC_AND ; FF
  INX
  STX   CC_OR ; 0
  
.POINT_LOOP
      LDY   PNT_CNT
.XPL: LDA   SHAPE_XL,Y
      STA   X_PNT_LO
.XPH: LDA   SHAPE_XH,Y
      STA   X_PNT_HI
.YPL: LDA   SHAPE_YL,Y
      STA   Y_PNT_LO
.YPH: LDA   SHAPE_YH,Y
      STA   Y_PNT_HI
.ZPL: LDA   SHAPE_ZL,Y
      STA   Z_PNT_LO
.ZPH: LDA   SHAPE_ZH,Y
      STA   Z_PNT_HI

      JSR   ROTATE_POINT
      LDY   PNT_CNT

      CLC
      LDA   X_PNT_LO  ; X_POS + X_OFFSET
      ADC   X_OFF_LO
      STA   X_PNT_LO
      STA   RXL,Y
      LDA   X_PNT_HI
      ADC   X_OFF_HI
      STA   X_PNT_HI
      STA   RXH,Y

      CLC
      LDA   Y_PNT_LO  ; Y_POS + Y_OFFSET
      ADC   Y_OFF_LO
      STA   Y_PNT_LO
      STA   RYL,Y
      LDA   Y_PNT_HI
      ADC   Y_OFF_HI
      STA   Y_PNT_HI
      STA   RYH,Y

      CLC
      LDA   Z_PNT_LO  ; Z_POS + Z_OFFSET
      ADC   Z_OFF_LO
      STA   Z_PNT_LO
      STA   RZL,Y
      LDA   Z_PNT_HI
      ADC   Z_OFF_HI
      STA   RZH,Y
      
      BPL   INFRONT      ; in front 
        LDA   #CC_BEHIND  ; behind
        JMP   BEHIND

.INFRONT:
      STA   Z_PNT_HI
      
      JSR   PERSPECTIVE_POINT
      
      LDY   PNT_CNT
      LDA   X_PNT_LO
      STA   PXL,Y
      LDA   X_PNT_HI
      STA   PXH,Y
      LDA   Y_PNT_LO
      STA   PYL,Y
      LDA   Y_PNT_HI
      STA   PYH,Y
      
       LDX   #0       ; CLIP CODE
       LDY   X_PNT_LO
       LDA   X_PNT_HI
       CLP_CDE_H
       STX   CC
       LDX   #0
       LDY   Y_PNT_LO
       LDA   Y_PNT_HI
       CLP_CDE_V
       TXA
       ORA   CC       ; CLIP CODE

.BEHIND:
      STA   CC
      LDY   PNT_CNT
      STA   PCC,Y
         
;      LDA   CC
      AND   CC_AND
      STA   CC_AND
      LDA   CC
      ORA   CC_OR
      STA   CC_OR
      
      DEC PNT_CNT
      BMI POINT_EXIT
      JMP POINT_LOOP
 
.POINT_EXIT
      LDA   CC_AND
      BNE   OBJECT_EXIT  ; all points are off same side
      
      LDA   CC_OR
      BNE   CLIP_LINE_LOOP ; any point is off
      
.LINE_LOOP
      LDY   LINE_CNT
.SPS: LDX   SHAPE_PS,Y  ; point start      
      LDA   PXL,X
      STA   H_ST
      LDA   PYL,X
      STA   V_ST
.SPE: LDX   SHAPE_PE,Y  ; point end
      LDA   PXL,X
      STA   H_EN
      LDA   PYL,X
      STA   V_EN
      JSR   LINE

      DEC LINE_CNT
      BPL LINE_LOOP
.OBJECT_EXIT
RTS

.CLIP_LINE_LOOP
      LDY   LINE_CNT
.SPSC
		LDX   SHAPE_PS,Y   ; point start
      STX   CLIP_SSI+1  ; shape start index
      LDA   PCC,X        ; start point clip code
      STA   CC
      
      LDA   PXL,X
      STA   H_ST
      LDA   PYL,X
      STA   V_ST
.SPEC
		LDX   SHAPE_PE,Y  ; point end

      LDA   PCC,X        ; end point clip code
      AND   CC          ;
      BNE   LINE_OFF   ; both off same side

      LDA   PCC,X        ; end point clip code
      ORA   CC          ;
      BNE   CLIP_LINE   ; any are off same side - temp

      LDA   PXL,X
      STA   H_EN
      LDA   PYL,X
      STA   V_EN
.LINE_DRAW
      JSR   LINE
.LINE_OFF
      DEC LINE_CNT
      BPL CLIP_LINE_LOOP
RTS

.CLIP_LINE:
.CLIP_SSI:  LDY #0      ; Y holds shape start index / X already holds point end index

;            CLC
;            LDA   RXL,Y ; start object +16384
;            ADC   #<CLIP_OFFSET
;            STA   X_CLIP_S_PNT_LO
;            LDA   RXH,Y ; start object
;            ADC   #>CLIP_OFFSET
;            STA   X_CLIP_S_PNT_HI

;            CLC
;            LDA   RYL,Y 
;            ADC   #<CLIP_OFFSET
;            STA   Y_CLIP_S_PNT_LO
;            LDA   RYH,Y 
;            ADC   #>CLIP_OFFSET
;            STA   Y_CLIP_S_PNT_HI
            
;            CLC
;            LDA   RZL,Y 
;           ADC   #<CLIP_OFFSET
;           STA   Z_CLIP_S_PNT_LO
;           LDA   RZH,Y 
;           ADC   #>CLIP_OFFSET
;           STA   Z_CLIP_S_PNT_HI

;            CLC
;            LDA   RXL,X ; end object +16384
;            ADC   #<CLIP_OFFSET
;            STA   X_CLIP_E_PNT_LO
;            LDA   RXH,X 
;            ADC   #>CLIP_OFFSET
;            STA   X_CLIP_E_PNT_HI

;            CLC
;            LDA   RYL,X
;            ADC   #<CLIP_OFFSET
;            STA   Y_CLIP_E_PNT_LO
;            LDA   RYH,X 
;            ADC   #>CLIP_OFFSET
;            STA   Y_CLIP_E_PNT_HI
            
;            CLC
;            LDA   RZL,X 
;            ADC   #<CLIP_OFFSET
;            STA   Z_CLIP_E_PNT_LO
;            LDA   RZH,X 
;            ADC   #>CLIP_OFFSET
;            STA   Z_CLIP_E_PNT_HI
          
            LDA   PXL,Y ; start object +16384
            STA   X_CLIP_S_PER_LO
            CLC
            LDA   PXH,Y 
            ADC   #>CLIP_OFFSET
            STA   X_CLIP_S_PER_HI

            LDA   PYL,Y
            STA   Y_CLIP_S_PER_LO
            CLC
            LDA   PYH,Y 
            ADC   #>CLIP_OFFSET
            STA   Y_CLIP_S_PER_HI

            LDA   PXL,X ; end object +16384
            STA   X_CLIP_E_PER_LO
            CLC
            LDA   PXH,X 
            ADC   #>CLIP_OFFSET
            STA   X_CLIP_E_PER_HI

            LDA   PYL,X
            STA   Y_CLIP_E_PER_LO
            CLC
            LDA   PYH,X 
            ADC   #>CLIP_OFFSET
            STA   Y_CLIP_E_PER_HI
            
            LDA   PCC,Y ; start clip codes
            STA   CC_S
            LDA   PCC,X ; end clip codes
            STA   CC_E
            ORA   CC_S
            STA   CC    ; combined clip codes
            
.CLIP_TEST_BEHIND
            LDA   CC   
            AND   #CC_BEHIND
            BEQ   CLIP_TEST_LEFT
              JSR   CLIP_BEHIND
              JMP   LINE_OFF ; both off same side
.CLIP_TEST_LEFT
            LDA   CC   
            AND   #CC_LEFT
            BEQ   CLIP_TEST_RIGHT
              JSR CLIP_LEFT
              ; check cc for offscreen
              LDA   CC_E
              and   CC_S
              BNE   LINE_OFF ; both off same side
              LDA   CC_E
              ora   CC_S
              STA   CC
.CLIP_TEST_RIGHT
            LDA   CC   
            AND   #CC_RIGHT
            BEQ   CLIP_TEST_TOP
              JSR CLIP_RIGHT
              LDA   CC_E
              And   CC_S
              BNE   LINE_OFF ; both off same side
              LDA   CC_E
              ora   CC_S
              STA   CC
.CLIP_TEST_TOP
            LDA   CC   
            AND   #CC_TOP
            BEQ   CLIP_TEST_BOTTOM
              JSR CLIP_TOP
              LDA   CC_E
              And   CC_S
              BNE   LINE_CLIP_OFF ; both off same side
              LDA   CC_E
              ora   CC_S
              STA   CC
.CLIP_TEST_BOTTOM
            LDA   CC   
            AND   #CC_BOTTOM
            BEQ   CLIP_TEST_END
              JSR CLIP_BOTTOM
              LDA   CC_E
              And   CC_S
              BNE   LINE_CLIP_OFF ; both off same side
              LDA   CC_E
              ora   CC_S
              STA   CC
.CLIP_TEST_END
  LDA   X_CLIP_S_PER_LO ; set line start using start point 
  STA   H_ST
  LDA   Y_CLIP_S_PER_LO
  STA   V_ST
  LDA   X_CLIP_E_PER_LO ; set line start using start point 
  STA   H_EN
  LDA   Y_CLIP_E_PER_LO
  STA   V_EN
  JSR   LINE

.LINE_CLIP_OFF   JMP LINE_OFF ; temp

.CLIP_BEHIND
  RTS

.CLIP_LEFT:
{
  LDA   CC_S   
  AND   #CC_LEFT
  BEQ   START_ON_END_OFF

.START_OFF_END_ON
  LDA   X_CLIP_S_PER_LO
  STA   X_CLIP_STT
  LDA   X_CLIP_S_PER_HI
  STA   X_CLIP_STT+1

  LDA   Y_CLIP_S_PER_LO
  STA   Y_CLIP_STT
  LDA   Y_CLIP_S_PER_HI
  STA   Y_CLIP_STT+1
  
  LDA   X_CLIP_E_PER_LO
  STA   X_CLIP_END
  LDA   X_CLIP_E_PER_HI
  STA   X_CLIP_END+1

  LDA   Y_CLIP_E_PER_LO
  STA   Y_CLIP_END
  LDA   Y_CLIP_E_PER_HI
  STA   Y_CLIP_END+1
  
  JSR   MIDPOINT_TOP_LEFT  ; do clip
  
  LDA   #<CLIP_OFFSET
  sta   X_CLIP_S_PER_LO
  LDA   #>CLIP_OFFSET
  sta   X_CLIP_S_PER_HI

  LDA   Y_CLIP_MID
  sta   Y_CLIP_S_PER_LO
  LDA   Y_CLIP_MID+1
  sta   Y_CLIP_S_PER_HI

       LDX   #0       ; CLIP CODE
       LDY   X_CLIP_S_PER_LO
       SEC
       LDA   X_CLIP_S_PER_HI 
       SBC   #>CLIP_OFFSET
       CLP_CDE_H
       STX   CC

       LDX   #0
       LDY   Y_CLIP_S_PER_LO
       SEC
       LDA   Y_CLIP_S_PER_HI 
       SBC   #>CLIP_OFFSET
       STA   Y_PNT_HI
       CLP_CDE_V
       TXA
       ORA   CC       ; CLIP CODE

  STA CC_S
  RTS
  
.START_ON_END_OFF
  LDA   X_CLIP_E_PER_LO
  STA   X_CLIP_STT
  LDA   X_CLIP_E_PER_HI
  STA   X_CLIP_STT+1

  LDA   Y_CLIP_E_PER_LO
  STA   Y_CLIP_STT
  LDA   Y_CLIP_E_PER_HI
  STA   Y_CLIP_STT+1
  
  LDA   X_CLIP_S_PER_LO
  STA   X_CLIP_END
  LDA   X_CLIP_S_PER_HI
  STA   X_CLIP_END+1

  LDA   Y_CLIP_S_PER_LO
  STA   Y_CLIP_END
  LDA   Y_CLIP_S_PER_HI
  STA   Y_CLIP_END+1
  
  JSR   MIDPOINT_TOP_LEFT  ; do clip

  LDA   #<CLIP_OFFSET
  sta   X_CLIP_E_PER_LO
  LDA   #>CLIP_OFFSET
  sta   X_CLIP_E_PER_HI

  LDA   Y_CLIP_MID
  sta   Y_CLIP_E_PER_LO
  LDA   Y_CLIP_MID+1
  sta   Y_CLIP_E_PER_HI
    
       LDX   #0       ; CLIP CODE
       LDY   X_CLIP_E_PER_LO
       SEC
       LDA   X_CLIP_E_PER_HI 
       SBC   #>CLIP_OFFSET
       CLP_CDE_H
       STX   CC

       LDX   #0
       LDY   Y_CLIP_E_PER_LO
       SEC
       LDA   Y_CLIP_E_PER_HI 
       SBC   #>CLIP_OFFSET
       STA   Y_PNT_HI
       CLP_CDE_V
       TXA
       ORA   CC       ; CLIP CODE

  STA   CC_E
  RTS
}

.CLIP_RIGHT:
{
  LDA   CC_S   
  AND   #CC_RIGHT
  BEQ   START_ON_END_OFF

.START_OFF_END_ON
  LDA   X_CLIP_E_PER_LO
  STA   X_CLIP_STT
  LDA   X_CLIP_E_PER_HI
  STA   X_CLIP_STT+1

  LDA   Y_CLIP_E_PER_LO
  STA   Y_CLIP_STT
  LDA   Y_CLIP_E_PER_HI
  STA   Y_CLIP_STT+1
  
  LDA   X_CLIP_S_PER_LO
  STA   X_CLIP_END
  LDA   X_CLIP_S_PER_HI
  STA   X_CLIP_END+1

  LDA   Y_CLIP_S_PER_LO
  STA   Y_CLIP_END
  LDA   Y_CLIP_S_PER_HI
  STA   Y_CLIP_END+1
  
  JSR   MIDPOINT_BOTTOM_RIGHT  ; do clip
  
  LDA   #<(CLIP_OFFSET +127)
  sta   X_CLIP_S_PER_LO
  LDA   #>(CLIP_OFFSET +127)
  sta   X_CLIP_S_PER_HI

  LDA   Y_CLIP_MID
  sta   Y_CLIP_S_PER_LO
  LDA   Y_CLIP_MID+1
  sta   Y_CLIP_S_PER_HI

       LDX   #0       ; CLIP CODE
       LDY   X_CLIP_S_PER_LO
       SEC
       LDA   X_CLIP_S_PER_HI 
       SBC   #>CLIP_OFFSET
       CLP_CDE_H
       STX   CC

       LDX   #0
       LDY   Y_CLIP_S_PER_LO
       SEC
       LDA   Y_CLIP_S_PER_HI 
       SBC   #>CLIP_OFFSET
       STA   Y_PNT_HI
       CLP_CDE_V
       TXA
       ORA   CC       ; CLIP CODE
  
  STA CC_S
  RTS
  
.START_ON_END_OFF
  LDA   X_CLIP_S_PER_LO
  STA   X_CLIP_STT
  LDA   X_CLIP_S_PER_HI
  STA   X_CLIP_STT+1

  LDA   Y_CLIP_S_PER_LO
  STA   Y_CLIP_STT
  LDA   Y_CLIP_S_PER_HI
  STA   Y_CLIP_STT+1
    
  LDA   X_CLIP_E_PER_LO
  STA   X_CLIP_END
  LDA   X_CLIP_E_PER_HI
  STA   X_CLIP_END+1

  LDA   Y_CLIP_E_PER_LO
  STA   Y_CLIP_END
  LDA   Y_CLIP_E_PER_HI
  STA   Y_CLIP_END+1
  
  JSR   MIDPOINT_BOTTOM_RIGHT  ; do clip
  
  LDA   #<(CLIP_OFFSET +127)
  sta   X_CLIP_E_PER_LO
  LDA   #>(CLIP_OFFSET +127)
  sta   X_CLIP_E_PER_HI

  LDA   Y_CLIP_MID
  sta   Y_CLIP_E_PER_LO
  LDA   Y_CLIP_MID+1
  sta   Y_CLIP_E_PER_HI

       LDX   #0       ; CLIP CODE
       LDY   X_CLIP_E_PER_LO
       SEC
       LDA   X_CLIP_E_PER_HI 
       SBC   #>CLIP_OFFSET
       CLP_CDE_H
       STX   CC

       LDX   #0
       LDY   Y_CLIP_E_PER_LO
       SEC
       LDA   Y_CLIP_E_PER_HI 
       SBC   #>CLIP_OFFSET
       STA   Y_PNT_HI
       CLP_CDE_V
       TXA
       ORA   CC       ; CLIP CODE

  STA CC_E
  RTS
}
      
.CLIP_TOP
{
  LDA   CC_S   
  AND   #CC_TOP
  BEQ   START_ON_END_OFF
.START_OFF_END_ON
  LDA   X_CLIP_S_PER_LO
  STA   Y_CLIP_STT
  LDA   X_CLIP_S_PER_HI
  STA   Y_CLIP_STT+1

  LDA   Y_CLIP_S_PER_LO
  STA   X_CLIP_STT
  LDA   Y_CLIP_S_PER_HI
  STA   X_CLIP_STT+1
  
  LDA   X_CLIP_E_PER_LO
  STA   Y_CLIP_END
  LDA   X_CLIP_E_PER_HI
  STA   Y_CLIP_END+1

  LDA   Y_CLIP_E_PER_LO
  STA   X_CLIP_END
  LDA   Y_CLIP_E_PER_HI
  STA   X_CLIP_END+1
  
  JSR   MIDPOINT_TOP_LEFT  ; do clip
  
  LDA   #<CLIP_OFFSET
  sta   Y_CLIP_S_PER_LO
  LDA   #>CLIP_OFFSET
  sta   Y_CLIP_S_PER_HI

  LDA   Y_CLIP_MID
  sta   X_CLIP_S_PER_LO
  LDA   Y_CLIP_MID+1
  sta   X_CLIP_S_PER_HI
  
       LDX   #0       ; CLIP CODE
       LDY   X_CLIP_S_PER_LO
       SEC
       LDA   X_CLIP_S_PER_HI 
       SBC   #>CLIP_OFFSET
       CLP_CDE_H
       STX   CC

       LDX   #0
       LDY   Y_CLIP_S_PER_LO
       SEC
       LDA   Y_CLIP_S_PER_HI 
       SBC   #>CLIP_OFFSET
       STA   Y_PNT_HI
       CLP_CDE_V
       TXA
       ORA   CC       ; CLIP CODE

  STA CC_S
  RTS
  
.START_ON_END_OFF
  LDA   X_CLIP_E_PER_LO
  STA   Y_CLIP_STT
  LDA   X_CLIP_E_PER_HI
  STA   Y_CLIP_STT+1

  LDA   Y_CLIP_E_PER_LO
  STA   X_CLIP_STT
  LDA   Y_CLIP_E_PER_HI
  STA   X_CLIP_STT+1
  
  LDA   X_CLIP_S_PER_LO
  STA   Y_CLIP_END
  LDA   X_CLIP_S_PER_HI
  STA   Y_CLIP_END+1

  LDA   Y_CLIP_S_PER_LO
  STA   X_CLIP_END
  LDA   Y_CLIP_S_PER_HI
  STA   X_CLIP_END+1
  
  JSR   MIDPOINT_TOP_LEFT  ; do clip
  
  LDA   #<CLIP_OFFSET
  sta   Y_CLIP_E_PER_LO
  LDA   #>CLIP_OFFSET
  sta   Y_CLIP_E_PER_HI

  LDA   Y_CLIP_MID
  sta   X_CLIP_E_PER_LO
  LDA   Y_CLIP_MID+1
  sta   X_CLIP_E_PER_HI

       LDX   #0       ; CLIP CODE
       LDY   X_CLIP_E_PER_LO
       SEC
       LDA   X_CLIP_E_PER_HI 
       SBC   #>CLIP_OFFSET
       CLP_CDE_H
       STX   CC

       LDX   #0
       LDY   Y_CLIP_E_PER_LO
       SEC
       LDA   Y_CLIP_E_PER_HI 
       SBC   #>CLIP_OFFSET
       STA   Y_PNT_HI
       CLP_CDE_V
       TXA
       ORA   CC       ; CLIP CODE

  STA CC_E
  RTS
}

.CLIP_BOTTOM:
{
  LDA   CC_S   
  AND   #CC_BOTTOM
  BEQ   START_ON_END_OFF

.START_OFF_END_ON
  LDA   X_CLIP_E_PER_LO
  STA   Y_CLIP_STT
  LDA   X_CLIP_E_PER_HI
  STA   Y_CLIP_STT+1

  LDA   Y_CLIP_E_PER_LO
  STA   X_CLIP_STT
  LDA   Y_CLIP_E_PER_HI
  STA   X_CLIP_STT+1
  
  LDA   X_CLIP_S_PER_LO
  STA   Y_CLIP_END
  LDA   X_CLIP_S_PER_HI
  STA   Y_CLIP_END+1

  LDA   Y_CLIP_S_PER_LO
  STA   X_CLIP_END
  LDA   Y_CLIP_S_PER_HI
  STA   X_CLIP_END+1
  
  JSR   MIDPOINT_BOTTOM_RIGHT  ; do clip

  LDA   #<(CLIP_OFFSET +127)
  sta   Y_CLIP_S_PER_LO
  LDA   #>(CLIP_OFFSET +127)
  sta   Y_CLIP_S_PER_HI

  LDA   Y_CLIP_MID
  sta   X_CLIP_S_PER_LO
  LDA   Y_CLIP_MID+1
  sta   X_CLIP_S_PER_HI

       LDX   #0       ; CLIP CODE
       LDY   X_CLIP_S_PER_LO
       SEC
       LDA   X_CLIP_S_PER_HI 
       SBC   #>CLIP_OFFSET
       CLP_CDE_H
       STX   CC

       LDX   #0
       LDY   Y_CLIP_S_PER_LO
       SEC
       LDA   Y_CLIP_S_PER_HI 
       SBC   #>CLIP_OFFSET
       STA   Y_PNT_HI
       CLP_CDE_V
       TXA
       ORA   CC       ; CLIP CODE

  STA CC_S
  RTS
  
.START_ON_END_OFF
  LDA   X_CLIP_S_PER_LO
  STA   Y_CLIP_STT
  LDA   X_CLIP_S_PER_HI
  STA   Y_CLIP_STT+1

  LDA   Y_CLIP_S_PER_LO
  STA   X_CLIP_STT
  LDA   Y_CLIP_S_PER_HI
  STA   X_CLIP_STT+1
    
  LDA   X_CLIP_E_PER_LO
  STA   Y_CLIP_END
  LDA   X_CLIP_E_PER_HI
  STA   Y_CLIP_END+1

  LDA   Y_CLIP_E_PER_LO
  STA   X_CLIP_END
  LDA   Y_CLIP_E_PER_HI
  STA   X_CLIP_END+1
  
  JSR   MIDPOINT_BOTTOM_RIGHT  ; do clip
  
  LDA   #<(CLIP_OFFSET +127)
  sta   Y_CLIP_E_PER_LO
  LDA   #>(CLIP_OFFSET +127)
  sta   Y_CLIP_E_PER_HI

  LDA   Y_CLIP_MID
  sta   X_CLIP_E_PER_LO
  LDA   Y_CLIP_MID+1
  sta   X_CLIP_E_PER_HI

       LDX   #0       ; CLIP CODE
       LDY   X_CLIP_E_PER_LO
       SEC
       LDA   X_CLIP_E_PER_HI 
       SBC   #>CLIP_OFFSET
       CLP_CDE_H
       STX   CC

       LDX   #0
       LDY   Y_CLIP_E_PER_LO
       SEC
       LDA   Y_CLIP_E_PER_HI 
       SBC   #>CLIP_OFFSET
       STA   Y_PNT_HI
       CLP_CDE_V
       TXA
       ORA   CC       ; CLIP CODE

  STA CC_E 
  RTS
}

; neg16
; lda negate,x
; tax
; lda negate,y
; tay


MACRO NGT_16  NUM
  SEC       ;Ensure carry is set                      #2
  LDA #0    ;Load constant zero                       #2
  SBC NUM   ;... subtract the least significant byte  #3
  STA NUM   ;... and store the result                 #3
  LDA #0    ;Load constant zero again                 #2
  SBC NUM+1 ;... subtract the most significant byte   #3
  STA NUM+1 ;... and store the result                 #3
ENDMACRO
       
MACRO MID_16 NUM1, NUM2, RC 
  CLC           ; clear carry
  LDA NUM1
  ADC NUM2
  STA RC     ; store sum of LSBs
  LDA NUM1+1
  ADC NUM2+1   ; add the MSBs using carry from the previous calculation
  STA RC+1
  LSR RC+1     ; half MSB
  ROR RC     ; half LSB add carry
ENDMACRO

MACRO CP_16 NUM1, NUM2 
{
; Does exactly the same as CMP of two values (effectively its a A - M) and sets the flags as follows:
;  If A = M : Carry =  SET   Zero =  SET   Negative = CLEAR
;  If A > M : Carry =  SET   Zero = CLEAR  Negative = CLEAR
;  If A < M : Carry = CLEAR  Zero = CLEAR  Negative =  SET

  LDA NUM1+1
  CMP NUM2+1
  BNE .DIFF
    LDA NUM1
    CMP NUM2
.DIFF
; Status register sorted, from here you can branch as you like as you would after a CMP opc.
}
ENDMACRO
  
.MIDPOINT_MORE_TL
  LSR   X_CLIP_MID+1     ; half MSB
  ROR   X_CLIP_MID     ; half LSB add carry

  LDA   X_CLIP_MID   
  STA   X_CLIP_END
  LDA   X_CLIP_MID+1
  STA   X_CLIP_END+1

  LSR   Y_CLIP_MID+1     ; half MSB
  ROR   Y_CLIP_MID     ; half LSB add carry

  LDA   Y_CLIP_MID   
  STA   Y_CLIP_END
  LDA   Y_CLIP_MID+1
  STA   Y_CLIP_END+1

.MIDPOINT_TOP_LEFT:
  CLC           ; clear carry
  LDA Y_CLIP_STT
  ADC Y_CLIP_END
  STA Y_CLIP_MID     ; store sum of LSBs
  LDA Y_CLIP_STT+1
  ADC Y_CLIP_END+1   ; add the MSBs using carry from the previous calculation
  STA Y_CLIP_MID+1

  CLC           ; clear carry
  LDA X_CLIP_STT
  ADC X_CLIP_END
  STA X_CLIP_MID     ; store sum of LSBs
  LDA X_CLIP_STT+1
  ADC X_CLIP_END+1   ; add the MSBs using carry from the previous calculation
  STA X_CLIP_MID+1
  
  LDA   X_CLIP_MID+1
  CMP   #>(CLIP_OFFSET *2)
  BNE   MIDPOINT_DIFF_TL
    LDA   X_CLIP_MID
    CMP   #<(CLIP_OFFSET *2)
    BEQ   MIDPOINT_FOUND_TL

.MIDPOINT_DIFF_TL
  BCS   MIDPOINT_MORE_TL

.MIDPOINT_LESS_TL
  LSR   X_CLIP_MID+1     ; half MSB
  ROR   X_CLIP_MID     ; half LSB add carry
  LDA   X_CLIP_MID
  CLC 
  ADC   #1
  STA   X_CLIP_STT
  LDA   X_CLIP_MID+1
  ADC   #0
  STA   X_CLIP_STT+1

  LSR   Y_CLIP_MID+1     ; half MSB
  ROR   Y_CLIP_MID     ; half LSB add carry
  LDA   Y_CLIP_MID
  STA   Y_CLIP_STT
  LDA   Y_CLIP_MID+1
  STA   Y_CLIP_STT+1
  JMP   MIDPOINT_TOP_LEFT

.MIDPOINT_FOUND_TL
  LSR   X_CLIP_MID+1   ; half MSB
  ROR   X_CLIP_MID     ; half LSB add carry
  LSR   Y_CLIP_MID+1   ; half MSB
  ROR   Y_CLIP_MID     ; half LSB add carry
  RTS

.MIDPOINT_MORE_BR
  LSR   X_CLIP_MID+1     ; half MSB
  ROR   X_CLIP_MID     ; half LSB add carry

  LDA   X_CLIP_MID   
  STA   X_CLIP_END
  LDA   X_CLIP_MID+1
  STA   X_CLIP_END+1

  LSR   Y_CLIP_MID+1     ; half MSB
  ROR   Y_CLIP_MID     ; half LSB add carry

  LDA   Y_CLIP_MID   
  STA   Y_CLIP_END
  LDA   Y_CLIP_MID+1
  STA   Y_CLIP_END+1

.MIDPOINT_BOTTOM_RIGHT
  CLC           ; clear carry
  LDA   Y_CLIP_STT
  ADC Y_CLIP_END
  STA Y_CLIP_MID     ; store sum of LSBs
  LDA Y_CLIP_STT+1
  ADC Y_CLIP_END+1   ; add the MSBs using carry from the previous calculation
  STA Y_CLIP_MID+1

  CLC           ; clear carry
  LDA X_CLIP_STT
  ADC X_CLIP_END
  STA X_CLIP_MID     ; store sum of LSBs
  LDA X_CLIP_STT+1
  ADC X_CLIP_END+1   ; add the MSBs using carry from the previous calculation
  STA X_CLIP_MID+1

  LDA   X_CLIP_MID+1
  CMP   #>((CLIP_OFFSET +127) *2)
  BNE   MIDPOINT_DIFF_BR
    LDA   X_CLIP_MID
    CMP   #<((CLIP_OFFSET +127) *2)
    BEQ   MIDPOINT_FOUND_BR

.MIDPOINT_DIFF_BR
  BCS   MIDPOINT_MORE_BR

.MIDPOINT_LESS_BR
  LSR   X_CLIP_MID+1     ; half MSB
  ROR   X_CLIP_MID     ; half LSB add carry
  LDA   X_CLIP_MID
  CLC 
  ADC   #1
  STA   X_CLIP_STT
  LDA   X_CLIP_MID+1
  ADC   #0
  STA   X_CLIP_STT+1

  LSR   Y_CLIP_MID+1     ; half MSB
  ROR   Y_CLIP_MID     ; half LSB add carry
  LDA   Y_CLIP_MID
  STA   Y_CLIP_STT
  LDA   Y_CLIP_MID+1
  STA   Y_CLIP_STT+1
  JMP   MIDPOINT_BOTTOM_RIGHT

.MIDPOINT_FOUND_BR
  LSR   X_CLIP_MID+1   ; half MSB
  ROR   X_CLIP_MID     ; half LSB add carry
  LSR   Y_CLIP_MID+1   ; half MSB
  ROR   Y_CLIP_MID     ; half LSB add carry
  RTS

MACRO DIV_MUL_U16U08 
{
  sta   P_SQR_LO  ; DIV_MUL_U16U08 ; set multiplier as x0
  sta   P_SQR_HI
  eor   #$ff
  sta   P_INVSQR_LO
  sta   P_INVSQR_HI ;17

  sec
  lda   (P_SQR_LO),Y
  sbc   (P_INVSQR_LO),Y ; note these two lines taken as 11 total ;  sta   Z0              ; x0*y0l
  lda   (P_SQR_HI),Y
  sbc   (P_INVSQR_HI),Y
  sta   .c1a+1           ; x0*y0h;31 ;c1a means column 1, row a (partial product to be added later)

  ldy   Y1  ;sec not needed ;notice that the high byte of sub above is always +ve
  lda   (P_SQR_LO),y
  sbc   (P_INVSQR_LO),y

      clc               ; add the first two numbers of column 1
.c1a: adc   #0
}
ENDMACRO

MACRO DIV_MUL_U16U16 
{
  lda   X0              ; set multiplier as x0
  sta   P_SQR_LO
  sta   P_SQR_HI
  eor   #$ff
  sta   P_INVSQR_LO
  sta   P_INVSQR_HI ;17

  ldy   Y0
  sec
  lda   (P_SQR_HI),Y
  sbc   (P_INVSQR_HI),Y
  sta   c1a+1           ; x0*y0h;31 ;c1a means column 1, row a (partial product to be added later)

  ldy   Y1  ;sec  ;notice that the high byte of sub above is always +ve
  lda   (P_SQR_LO),y
  sbc   (P_INVSQR_LO),y
  sta   c1b+1           ; x0*y1l
  lda   (P_SQR_HI),y
  sbc   (P_INVSQR_HI),y
  sta   c2a+1           ; x0*y1h;31

  lda   X1              ; set multiplier as x1
  sta   P_SQR_LO
  sta   P_SQR_HI
  eor   #$ff
  sta   P_INVSQR_LO
  sta   P_INVSQR_HI ;17

  ldy   Y0  ;sec
  lda   (P_SQR_LO),y
  sbc   (P_INVSQR_LO),y
  sta   c1c+1           ; x1*y0l
  lda   (P_SQR_HI),y
  sbc   (P_INVSQR_HI),y
  sta   c2b+1           ; x1*y1h;31

  ldy   Y1  ;sec
  lda   (P_SQR_LO),y
  sbc   (P_INVSQR_LO),y
  sta   c2c+1           ; x1*y1l

      clc               ; add the first two numbers of column 1
.c1a lda   #0
.c1b adc   #0
.c1c adc   #0          ; add last number of column 1
      sta   Z1  ;8

.c2a lda   #0          ; continue to first two numbers of column 2
.c2b adc   #0

      clc
.c2c adc   #0          ; add last number of column 2
      sta   Z2          ; X=z2
}
ENDMACRO

MACRO MUL_U16U16 
{
  lda   X0              ; set multiplier as x0
  sta   P_SQR_LO
  sta   P_SQR_HI
  eor   #$ff
  sta   P_INVSQR_LO
  sta   P_INVSQR_HI ;17

  ldy   Y0
  sec
  lda   (P_SQR_LO),Y
  sbc   (P_INVSQR_LO),Y ; note these two lines taken as 11 total
  sta   Z0              ; x0*y0l
  lda   (P_SQR_HI),Y
  sbc   (P_INVSQR_HI),Y
  sta   c1a+1           ; x0*y0h;31 ;c1a means column 1, row a (partial product to be added later)

  ldy   Y1
;sec  ;notice that the high byte of sub above is always +ve
  lda   (P_SQR_LO),y
  sbc   (P_INVSQR_LO),y
  sta   c1b+1           ; x0*y1l
  lda   (P_SQR_HI),y
  sbc   (P_INVSQR_HI),y
  sta   c2a+1           ; x0*y1h;31

  lda   X1              ; set multiplier as x1
  sta   P_SQR_LO
  sta   P_SQR_HI
  eor   #$ff
  sta   P_INVSQR_LO
  sta   P_INVSQR_HI ;17

  ldy   Y0
;sec
  lda   (P_SQR_LO),y
  sbc   (P_INVSQR_LO),y
  sta   c1c+1           ; x1*y0l
  lda   (P_SQR_HI),y
  sbc   (P_INVSQR_HI),y
  sta   c2b+1           ; x1*y1h;31

  ldy   Y1
;sec
  lda   (P_SQR_LO),y
  sbc   (P_INVSQR_LO),y
  sta   c2c+1           ; x1*y1l
  lda   (P_SQR_HI),y
  sbc   (P_INVSQR_HI),y
  tay                   ; x1*y1h;Y=z3, 30 cycles
;17+33+31+17+31+30=159 cycles for main multiply part

;jmp do_adds; can put do_adds in zp for a slight speed increase
;do_adds:
      clc               ; add the first two numbers of column 1
.c1a: lda   #0
.c1b: adc   #0
      sta   Z1  ;9

.c2a: lda   #0          ; continue to first two numbers of column 2
.c2b: adc   #0
      tax               ; X=z2, 6 cycles
      bcc c1c  ;3/6 avg 4.5
        iny             ; z3++
      clc

.c1c  lda   #0          ; add last number of column 1
      adc   Z1
      sta   Z1  ;8

      txa               ; A=z2
.c2c  adc   #0          ; add last number of column 2
      tax               ; X=z2, 6
      bcc fin;3/4 avg 3.5
        iny             ; z3++
  
.fin  stx   Z2          ; X=z2
      sty   Z3          ; Y=z3
}
ENDMACRO

.MULU16U16
 MUL_U16U16
  RTS

MACRO DIV_SHIFT_16 NUM1, NUM2, NUM3 ; convert x to unsigned and set a flag shift x down until z is less than 256 multiply by recipricol
  LSR A ; 16 bit divide by 2
  ROR   NUM1 
  LSR   NUM2+1
  ROR   NUM2
  LSR   NUM3+1
  ROR   NUM3
ENDMACRO

.DIVMUL_1608_TEMP
{
  STA   P_SQR_LO  ; DIV_MUL_U16U08 ; set multiplier as x0
  STA   P_SQR_HI
  EOR   #$ff
  STA   P_INVSQR_LO
  STA   P_INVSQR_HI ;17

  SEC
  LDY   REC16LO,X ; Y=divisor
  LDA   (P_INVSQR_HI),Y ; temp
  LDA   (P_SQR_HI),Y
  SBC   (P_INVSQR_HI),Y
  STA   C1A+1           ; x0*y0h;31 ;c1a means column 1, row a (partial product to be added later)

  LDY   REC16HI,X
  LDA   (P_INVSQR_LO),y ; temp
  LDA   (P_SQR_LO),y
  SBC   (P_INVSQR_LO),y
  CLC
.C1A: ADC   #0
  RTS
}
 
ALIGN &100
.PERSPECTIVE_POINT
          LDX   X_PNT_HI      ; get x hi
          STX   X_MINUS+1
          BPL   X_POS
              LDA   NEGTAB,X
              STA   X_PNT_HI 
              LDX   X_PNT_LO
              LDA   NEGTAB,X
              STA   X_PNT_LO 
.X_POS:
          LDX   Y_PNT_HI      ; get y hi
          STX   Y_MINUS+1
          BPL   Y_POS
              LDA   NEGTAB,X
              STA   Y_PNT_HI 
              LDX   Y_PNT_LO
              LDA   NEGTAB,X
              STA   Y_PNT_LO 
.Y_POS:   
;;          LDA   Z_PNT_HI      ; shift x/x/z down until z is less than 255
;;          TAX
          LDA   Z_PNT_HI      ; shift x/x/z down until z is less than 255
		TAX
		
          LDY   PPTABLO,X
          STY   PP_SFT+1
.PP_SFT   JMP   PP_SFT_8

.PP_SFT_8	DIV_SHIFT_16 Z_PNT_LO, X_PNT_LO, Y_PNT_LO
.PP_SFT_7	DIV_SHIFT_16 Z_PNT_LO, X_PNT_LO, Y_PNT_LO
.PP_SFT_6	DIV_SHIFT_16 Z_PNT_LO, X_PNT_LO, Y_PNT_LO
.PP_SFT_5	DIV_SHIFT_16 Z_PNT_LO, X_PNT_LO, Y_PNT_LO
.PP_SFT_4	DIV_SHIFT_16 Z_PNT_LO, X_PNT_LO, Y_PNT_LO
.PP_SFT_3	DIV_SHIFT_16 Z_PNT_LO, X_PNT_LO, Y_PNT_LO
.PP_SFT_2	DIV_SHIFT_16 Z_PNT_LO, X_PNT_LO, Y_PNT_LO
.PP_SFT_1	DIV_SHIFT_16 Z_PNT_LO, X_PNT_LO, Y_PNT_LO
.PP_SFT_0

          LDA   X_PNT_LO  ; divide shifted x
          STA   Y0
          LDA   X_PNT_HI
          STA   Y1
          LDX   Z_PNT_LO  ; by reciptical of shifted z
          LDA   REC16LO,X
          STA   X0
          LDA   REC16HI,X
          STA   X1
          DIV_MUL_U16U16 ; doesnt need z0 ot z3?

.X_MINUS: LDA   #0        ; negative?
          BPL   X_PLUS   ; no
            NGT_16  Z1    ; yes
.X_PLUS:  LDA   Z1        ; pers x + screen middle
          LDX   Z2
          CLC
          ADC   #<SCNX
          BCC   X_PS_PLS
            INX
.X_PS_PLS:STA   X_PNT_LO
          STX   X_PNT_HI
           
          LDA   Y_PNT_LO  ; divide shifted z
          STA   Y0
          LDA   Y_PNT_HI
          STA   Y1
          LDX   Z_PNT_LO  ; by reciptical of shifted z
          LDA   REC16LO,X
          STA   X0
          LDA   REC16HI,X
          STA   X1
          DIV_MUL_U16U16 ; doesnt need z0 ot z3?

.Y_MINUS: LDA   #0        ; negative
          BPL   Y_PLUS   ; no
            NGT_16  Z1    ; yes
.Y_PLUS:  LDA   Z1        ; pers x + screen middle
          LDY   Z2
          CLC
          ADC   #<SCNY
          BCC   Y_PS_PLS
            INY
.Y_PS_PLS STA   Y_PNT_LO 
          STY   Y_PNT_HI
 RTS


MACRO RT_U16U08 
{
  lda   X0              ; set multiplier as x0
  sta   P_SQR_LO
  sta   P_SQR_HI
  eor   #$ff
  sta   P_INVSQR_LO
  sta   P_INVSQR_HI ;17

  sec
  lda   (P_SQR_LO),Y
  sbc   (P_INVSQR_LO),Y ; note these two lines taken as 11 total
  lda   (P_SQR_HI),Y
  sbc   (P_INVSQR_HI),Y
  sta   c1a+1           ; x0*y0h;31 ;c1a means column 1, row a (partial product to be added later)

  lda   X1              ; set multiplier as x1
  sta   P_SQR_LO
  sta   P_SQR_HI
  eor   #$ff
  sta   P_INVSQR_LO
  sta   P_INVSQR_HI ;17

;sec
  lda   (P_SQR_LO),y
  sbc   (P_INVSQR_LO),y
  TAX
 
  lda   (P_SQR_HI),y
  sbc   (P_INVSQR_HI),y
  TAY

  TXA
  clc               ; add the first two numbers of column 1
.c1a  adc   #0
  TAX
     
  bcc exit
   INY
.exit 
}
ENDMACRO

.ROTS16S08 ; return z1/z2 in x/y
{  
	LDX   #0      ; reset sign count
  
  ASL A
  BCC cont1
    INX         ; inc count
    EOR #$ff  ; negate #2
    ADC #0    ; negate #2 ; carry is set
.cont1
  TAY
  
  LDA   X0+1    ; get x hi
  BPL   cont2
    INX         ; inc count
    NGT_16 X0  ; negate x
.cont2

  STX   count+1  ;  TYA           ; store count  PHA           ; push on stack
	RT_U16U08
.count
  LDA   #0      ; PLA           ; get stack
  LSR  A         ; AND #1
  BCC   cont3  ;  BEQ   .cont3
    LDA   NEGTAB,Y
    TAY
    LDA   NEGTAB,X
    TAX
.cont3
  RTS
}

.RT_X 
;rotate around x-axis:
; y'' = y'*cos(C) + z'*sin(C)
; z'' = y'*sin(C) - z'*cos(C)
  LDX   Y_PNT_LO
  LDY   Y_PNT_HI
  STX   X0
  STY   X1
  LDA XCOSTMP
  JSR   ROTS16S08
  STX   YC_TEMP 
  STY   YC_TEMP+1

  LDX   Y_PNT_LO
  LDY   Y_PNT_HI
  STX   X0
  STY   X1
  LDA XSINTMP
  JSR   ROTS16S08
  STX   YS_TEMP
  STY   YS_TEMP+1
  
  LDX   Z_PNT_LO
  LDY   Z_PNT_HI
  STX   X0
  STY   X1
  LDA XCOSTMP
  JSR   ROTS16S08
  STX   ZC_TEMP
  STY   ZC_TEMP+1

  LDX   Z_PNT_LO
  LDY   Z_PNT_HI
  STX   X0
  STY   X1
  LDA XSINTMP
  JSR   ROTS16S08
  STX   ZS_TEMP
  STY   ZS_TEMP+1
   
  CLC
  LDA   YC_TEMP
  ADC   ZS_TEMP
  STA   Y_PNT_LO
  LDA   YC_TEMP+1
  ADC   ZS_TEMP+1
  STA   Y_PNT_HI
    
  SEC
  LDA   YS_TEMP
  SBC   ZC_TEMP
  STA   Z_PNT_LO
  LDA   YS_TEMP+1
  SBC   ZC_TEMP+1
  STA   Z_PNT_HI
RTS

.RT_Y
; rotate around y-axis:
; x'' = x'*cos(B) + z*sin(B)
; z'  = x'*sin(B) - z*cos(B)
  LDX   X_PNT_LO
  LDY   X_PNT_HI
  STX   X0
  STY   X1
  LDA YCOSTMP
  JSR   ROTS16S08
  STX   XC_TEMP
  STY   XC_TEMP+1

  LDX   Z_PNT_LO
  LDY   Z_PNT_HI
  STX   X0
  STY   X1
  LDA YSINTMP
  JSR   ROTS16S08
  STX   ZS_TEMP
  STY   ZS_TEMP+1
  
  LDX   X_PNT_LO
  LDY   X_PNT_HI
  STX   X0
  STY   X1
  LDA YSINTMP
  JSR   ROTS16S08
  STX   XS_TEMP
  STY   XS_TEMP+1

  LDX   Z_PNT_LO
  LDY   Z_PNT_HI
  STX   X0
  STY   X1
  LDA YCOSTMP
  JSR   ROTS16S08
  STX   ZC_TEMP
  STY   ZC_TEMP+1
   
  CLC
  LDA   XC_TEMP
  ADC   ZS_TEMP
  STA   X_PNT_LO
  LDA   XC_TEMP+1
  ADC   ZS_TEMP+1
  STA   X_PNT_HI
    
  SEC
  LDA   XS_TEMP
  SBC   ZC_TEMP
  STA   Z_PNT_LO
  LDA   XS_TEMP+1
  SBC   ZC_TEMP+1
  STA   Z_PNT_HI
RTS

.RT_Z
;rotate around z-axis:
; x'  = x*cos(A) + y*sin(A)
; y'  = x*sin(A) - y*cos(A)
  LDX   X_PNT_LO
  LDY   X_PNT_HI
  STX   X0
  STY   X1
  LDA ZCOSTMP
  JSR   ROTS16S08
  STX   XC_TEMP
  STY   XC_TEMP+1

  LDX   Y_PNT_LO
  LDY   Y_PNT_HI
  STX   X0
  STY   X1
  LDA ZSINTMP
  JSR   ROTS16S08
  STX   YS_TEMP
  STY   YS_TEMP+1
  
  LDX   X_PNT_LO
  LDY   X_PNT_HI
  STX   X0
  STY   X1
  LDA ZSINTMP
  JSR   ROTS16S08
  STX   XS_TEMP
  STY   XS_TEMP+1

  LDX   Y_PNT_LO
  LDY   Y_PNT_HI
  STX   X0
  STY   X1
  LDA ZCOSTMP
  JSR   ROTS16S08
  STX   YC_TEMP
  STY   YC_TEMP+1
   
  CLC
  LDA   XC_TEMP
  ADC   YS_TEMP
  STA   X_PNT_LO
  LDA   XC_TEMP+1
  ADC   YS_TEMP+1
  STA   X_PNT_HI
    
  SEC
  LDA   XS_TEMP
  SBC   YC_TEMP
  STA   Y_PNT_LO
  LDA   XS_TEMP+1
  SBC   YC_TEMP+1
  STA   Y_PNT_HI
RTS

.ROTATE_POINT
  LDA   X_ROT+1
  TAX
  LDY   X_ROT
  JSR   SIN_COS
  STY   XCOSTMP
  STX   XSINTMP
  
  LDA   Y_ROT+1
  TAX
  LDY   Y_ROT
  JSR   SIN_COS
  STY   YCOSTMP
  STX   YSINTMP
  
  LDA   Z_ROT+1
  TAX
  LDY   Z_ROT
  JSR   SIN_COS
  STY   ZCOSTMP
  STX   ZSINTMP

 JSR RT_Z  
 JSR RT_X  
 JSR RT_Y  

RTS


.SIN_COS
{
  STY SIN+1
  STY COS+1
;  TXA
  CLC
  ADC #>SINTAB
  STA SIN+2
.SIN  LDY $ABCD
  INX
  TXA
  AND #3
  ADC #>SINTAB
  STA COS+2
.COS  LDX $ABCD
}
 RTS










.SHAPE_XL 	EQUB 	<(-450),  <(450), <(-450),  <(450), <(-450),  <(450), <(-450), <(450)
.SHAPE_XH 	EQUB 	>(-450),  >(450), >(-450),  >(450), >(-450),  >(450), >(-450), >(450)
.SHAPE_YL 	EQUB 	<(-450), <(-450),  <(450),  <(450), <(-450), <(-450),  <(450), <(450)
.SHAPE_YH 	EQUB 	>(-450), >(-450),  >(450),  >(450), >(-450), >(-450),  >(450), >(450)
.SHAPE_ZL 	EQUB 	<(-450), <(-450), <(-450), <(-450),  <(450),  <(450),  <(450), <(450)
.SHAPE_ZH 	EQUB 	>(-450), >(-450), >(-450), >(-450),  >(450),  >(450),  >(450), >(450)

.SHAPE_PS 	EQUB 	0,1,3,2, 4,5,7,6 ,0,5,2,7
.SHAPE_PE 	EQUB 	1,3,2,0, 5,7,6,4 ,4,1,6,3

ALIGN &100
.NEGTAB EQUB 	255-000,255-001,255-002,255-003,255-004,255-005,255-006,255-007,255-008,255-009,255-010,255-011,255-012,255-013,255-014,255-015
        EQUB 	255-016,255-017,255-018,255-019,255-020,255-021,255-022,255-023,255-024,255-025,255-026,255-027,255-028,255-029,255-030,255-031
        EQUB 	255-032,255-033,255-034,255-035,255-036,255-037,255-038,255-039,255-040,255-041,255-042,255-043,255-044,255-045,255-046,255-047
        EQUB 	255-048,255-049,255-050,255-051,255-052,255-053,255-054,255-055,255-056,255-057,255-058,255-059,255-060,255-061,255-062,255-063
        EQUB 	255-064,255-065,255-066,255-067,255-068,255-069,255-070,255-071,255-072,255-073,255-074,255-075,255-076,255-077,255-078,255-079
        EQUB 	255-080,255-081,255-082,255-083,255-084,255-085,255-086,255-087,255-088,255-089,255-090,255-091,255-092,255-093,255-094,255-095
        EQUB 	255-096,255-097,255-098,255-099,255-100,255-101,255-102,255-103,255-104,255-105,255-106,255-107,255-108,255-109,255-110,255-111
        EQUB 	255-112,255-113,255-114,255-115,255-116,255-117,255-118,255-119,255-120,255-121,255-122,255-123,255-124,255-125,255-126,255-127
        EQUB 	255-128,255-129,255-130,255-131,255-132,255-133,255-134,255-135,255-136,255-137,255-138,255-139,255-140,255-141,255-142,255-143
        EQUB 	255-144,255-145,255-146,255-147,255-148,255-149,255-150,255-151,255-152,255-153,255-154,255-155,255-156,255-157,255-158,255-159
        EQUB 	255-160,255-161,255-162,255-163,255-164,255-165,255-166,255-167,255-168,255-169,255-170,255-171,255-172,255-173,255-174,255-175
        EQUB 	255-176,255-177,255-178,255-179,255-180,255-181,255-182,255-183,255-184,255-185,255-186,255-187,255-188,255-189,255-190,255-191
        EQUB 	255-192,255-193,255-194,255-195,255-196,255-197,255-198,255-199,255-200,255-201,255-202,255-203,255-204,255-205,255-206,255-207
        EQUB 	255-208,255-209,255-210,255-211,255-212,255-213,255-214,255-215,255-216,255-217,255-218,255-219,255-220,255-221,255-222,255-223
        EQUB 	255-224,255-225,255-226,255-227,255-228,255-229,255-230,255-231,255-232,255-233,255-234,255-235,255-236,255-237,255-238,255-239
        EQUB 	255-240,255-241,255-242,255-243,255-244,255-245,255-246,255-247,255-248,255-249,255-250,255-251,255-252,255-253,255-254,255-255   

ALIGN &100
.PPTABLO  EQUB	  <PP_SFT_0
          EQUB	  <PP_SFT_1
          EQUB	  <PP_SFT_2,<PP_SFT_2
          EQUB	  <PP_SFT_3,<PP_SFT_3,<PP_SFT_3,<PP_SFT_3
          
          EQUB	  <PP_SFT_4,<PP_SFT_4,<PP_SFT_4,<PP_SFT_4,<PP_SFT_4,<PP_SFT_4,<PP_SFT_4,<PP_SFT_4

          EQUB	  <PP_SFT_5,<PP_SFT_5,<PP_SFT_5,<PP_SFT_5,<PP_SFT_5,<PP_SFT_5,<PP_SFT_5,<PP_SFT_5
          EQUB	  <PP_SFT_5,<PP_SFT_5,<PP_SFT_5,<PP_SFT_5,<PP_SFT_5,<PP_SFT_5,<PP_SFT_5,<PP_SFT_5

          EQUB	  <PP_SFT_6,<PP_SFT_6,<PP_SFT_6,<PP_SFT_6,<PP_SFT_6,<PP_SFT_6,<PP_SFT_6,<PP_SFT_6
          EQUB	  <PP_SFT_6,<PP_SFT_6,<PP_SFT_6,<PP_SFT_6,<PP_SFT_6,<PP_SFT_6,<PP_SFT_6,<PP_SFT_6
          EQUB	  <PP_SFT_6,<PP_SFT_6,<PP_SFT_6,<PP_SFT_6,<PP_SFT_6,<PP_SFT_6,<PP_SFT_6,<PP_SFT_6
          EQUB  <PP_SFT_6,<PP_SFT_6,<PP_SFT_6,<PP_SFT_6,<PP_SFT_6,<PP_SFT_6,<PP_SFT_6,<PP_SFT_6

          EQUB	  <PP_SFT_7,<PP_SFT_7,<PP_SFT_7,<PP_SFT_7,<PP_SFT_7,<PP_SFT_7,<PP_SFT_7,<PP_SFT_7
          EQUB	  <PP_SFT_7,<PP_SFT_7,<PP_SFT_7,<PP_SFT_7,<PP_SFT_7,<PP_SFT_7,<PP_SFT_7,<PP_SFT_7
          EQUB	  <PP_SFT_7,<PP_SFT_7,<PP_SFT_7,<PP_SFT_7,<PP_SFT_7,<PP_SFT_7,<PP_SFT_7,<PP_SFT_7
          EQUB	  <PP_SFT_7,<PP_SFT_7,<PP_SFT_7,<PP_SFT_7,<PP_SFT_7,<PP_SFT_7,<PP_SFT_7,<PP_SFT_7
          EQUB	  <PP_SFT_7,<PP_SFT_7,<PP_SFT_7,<PP_SFT_7,<PP_SFT_7,<PP_SFT_7,<PP_SFT_7,<PP_SFT_7
          EQUB	  <PP_SFT_7,<PP_SFT_7,<PP_SFT_7,<PP_SFT_7,<PP_SFT_7,<PP_SFT_7,<PP_SFT_7,<PP_SFT_7
          EQUB	  <PP_SFT_7,<PP_SFT_7,<PP_SFT_7,<PP_SFT_7,<PP_SFT_7,<PP_SFT_7,<PP_SFT_7,<PP_SFT_7
          EQUB	  <PP_SFT_7,<PP_SFT_7,<PP_SFT_7,<PP_SFT_7,<PP_SFT_7,<PP_SFT_7,<PP_SFT_7,<PP_SFT_7

          EQUB	  <PP_SFT_8,<PP_SFT_8,<PP_SFT_8,<PP_SFT_8,<PP_SFT_8,<PP_SFT_8,<PP_SFT_8,<PP_SFT_8
          EQUB	  <PP_SFT_8,<PP_SFT_8,<PP_SFT_8,<PP_SFT_8,<PP_SFT_8,<PP_SFT_8,<PP_SFT_8,<PP_SFT_8
          EQUB	  <PP_SFT_8,<PP_SFT_8,<PP_SFT_8,<PP_SFT_8,<PP_SFT_8,<PP_SFT_8,<PP_SFT_8,<PP_SFT_8
          EQUB	  <PP_SFT_8,<PP_SFT_8,<PP_SFT_8,<PP_SFT_8,<PP_SFT_8,<PP_SFT_8,<PP_SFT_8,<PP_SFT_8
          EQUB	  <PP_SFT_8,<PP_SFT_8,<PP_SFT_8,<PP_SFT_8,<PP_SFT_8,<PP_SFT_8,<PP_SFT_8,<PP_SFT_8
          EQUB	  <PP_SFT_8,<PP_SFT_8,<PP_SFT_8,<PP_SFT_8,<PP_SFT_8,<PP_SFT_8,<PP_SFT_8,<PP_SFT_8
          EQUB	  <PP_SFT_8,<PP_SFT_8,<PP_SFT_8,<PP_SFT_8,<PP_SFT_8,<PP_SFT_8,<PP_SFT_8,<PP_SFT_8
          EQUB	  <PP_SFT_8,<PP_SFT_8,<PP_SFT_8,<PP_SFT_8,<PP_SFT_8,<PP_SFT_8,<PP_SFT_8,<PP_SFT_8
          EQUB	  <PP_SFT_8,<PP_SFT_8,<PP_SFT_8,<PP_SFT_8,<PP_SFT_8,<PP_SFT_8,<PP_SFT_8,<PP_SFT_8
          EQUB	  <PP_SFT_8,<PP_SFT_8,<PP_SFT_8,<PP_SFT_8,<PP_SFT_8,<PP_SFT_8,<PP_SFT_8,<PP_SFT_8
          EQUB	  <PP_SFT_8,<PP_SFT_8,<PP_SFT_8,<PP_SFT_8,<PP_SFT_8,<PP_SFT_8,<PP_SFT_8,<PP_SFT_8
          EQUB	  <PP_SFT_8,<PP_SFT_8,<PP_SFT_8,<PP_SFT_8,<PP_SFT_8,<PP_SFT_8,<PP_SFT_8,<PP_SFT_8
          EQUB	  <PP_SFT_8,<PP_SFT_8,<PP_SFT_8,<PP_SFT_8,<PP_SFT_8,<PP_SFT_8,<PP_SFT_8,<PP_SFT_8
          EQUB		<PP_SFT_8,<PP_SFT_8,<PP_SFT_8,<PP_SFT_8,<PP_SFT_8,<PP_SFT_8,<PP_SFT_8,<PP_SFT_8
          EQUB		<PP_SFT_8,<PP_SFT_8,<PP_SFT_8,<PP_SFT_8,<PP_SFT_8,<PP_SFT_8,<PP_SFT_8,<PP_SFT_8
          EQUB 	<PP_SFT_8,<PP_SFT_8,<PP_SFT_8,<PP_SFT_8,<PP_SFT_8,<PP_SFT_8,<PP_SFT_8,<PP_SFT_8













ALIGN &100
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

ALIGN &100
.SCR_MONO_OR
	EQUB $80,$40,$20,$10,$08,$04,$02,$01
	EQUB $80,$40,$20,$10,$08,$04,$02,$01
	EQUB $80,$40,$20,$10,$08,$04,$02,$01
	EQUB $80,$40,$20,$10,$08,$04,$02,$01
	EQUB $80,$40,$20,$10,$08,$04,$02,$01
	EQUB $80,$40,$20,$10,$08,$04,$02,$01
	EQUB $80,$40,$20,$10,$08,$04,$02,$01
	EQUB $80,$40,$20,$10,$08,$04,$02,$01
	EQUB $80,$40,$20,$10,$08,$04,$02,$01
	EQUB $80,$40,$20,$10,$08,$04,$02,$01
	EQUB $80,$40,$20,$10,$08,$04,$02,$01
	EQUB $80,$40,$20,$10,$08,$04,$02,$01
	EQUB $80,$40,$20,$10,$08,$04,$02,$01
	EQUB $80,$40,$20,$10,$08,$04,$02,$01
	EQUB $80,$40,$20,$10,$08,$04,$02,$01
	EQUB $80,$40,$20,$10,$08,$04,$02,$01

	EQUB $80,$40,$20,$10,$08,$04,$02,$01
	EQUB $80,$40,$20,$10,$08,$04,$02,$01
	EQUB $80,$40,$20,$10,$08,$04,$02,$01
	EQUB $80,$40,$20,$10,$08,$04,$02,$01
	EQUB $80,$40,$20,$10,$08,$04,$02,$01
	EQUB $80,$40,$20,$10,$08,$04,$02,$01
	EQUB $80,$40,$20,$10,$08,$04,$02,$01
	EQUB $80,$40,$20,$10,$08,$04,$02,$01
	EQUB $80,$40,$20,$10,$08,$04,$02,$01
	EQUB $80,$40,$20,$10,$08,$04,$02,$01
	EQUB $80,$40,$20,$10,$08,$04,$02,$01
	EQUB $80,$40,$20,$10,$08,$04,$02,$01
	EQUB $80,$40,$20,$10,$08,$04,$02,$01
	EQUB $80,$40,$20,$10,$08,$04,$02,$01
	EQUB $80,$40,$20,$10,$08,$04,$02,$01
	EQUB $80,$40,$20,$10,$08,$04,$02,$01
	
ALIGN &100
.sqrlo                                                           
	EQUB	000,000,001,002,004,006,009,012,016,020,025,030,036,042,049,056
	EQUB	064,072,081,090,100,110,121,132,144,156,169,182,196,210,225,240
	EQUB 	000,016,033,050,068,086,105,124,144,164,185,206,228,250,017,040
	EQUB 	064,088,113,138,164,190,217,244,016,044,073,102,132,162,193,224
	EQUB 	000,032,065,098,132,166,201,236,016,052,089,126,164,202,241,024
	EQUB 	064,104,145,186,228,014,057,100,144,188,233,022,068,114,161,208
	EQUB 	000,048,097,146,196,246,041,092,144,196,249,046,100,154,209,008
	EQUB 	064,120,177,234,036,094,153,212,016,076,137,198,004,066,129,192
	EQUB 	000,064,129,194,004,070,137,204,016,084,153,222,036,106,177,248
	EQUB 	064,136,209,026,100,174,249,068,144,220,041,118,196,018,097,176
	EQUB 	000,080,161,242,068,150,233,060,144,228,057,142,228,058,145,232
	EQUB 	064,152,241,074,164,254,089,180,016,108,201,038,132,226,065,160
	EQUB 	000,096,193,034,132,230,073,172,016,116,217,062,164,010,113,216
	EQUB 	064,168,017,122,228,078,185,036,144,252,105,214,068,178,033,144
	EQUB 	000,112,225,082,196,054,169,028,144,004,121,238,100,218,081,200
	EQUB 	064,184,049,170,036,158,025,148,016,140,009,134,004,130,001,128
	EQUB 	000,128,001,130,004,134,009,140,016,148,025,158,036,170,049,184
	EQUB 	064,200,081,218,100,238,121,004,144,028,169,054,196,082,225,112
	EQUB 	000,144,033,178,068,214,105,252,144,036,185,078,228,122,017,168
	EQUB 	064,216,113,010,164,062,217,116,016,172,073,230,132,034,193,096
	EQUB 	000,160,065,226,132,038,201,108,016,180,089,254,164,074,241,152 
	EQUB 	064,232,145,058,228,142,057,228,144,060,233,150,068,242,161,080 
	EQUB 	000,176,097,018,196,118,041,220,144,068,249,174,100,026,209,136 
	EQUB 	064,248,177,106,036,222,153,084,016,204,137,070,004,194,129,064 
	EQUB 	000,192,129,066,004,198,137,076,016,212,153,094,036,234,177,120 
	EQUB 	064,008,209,154,100,046,249,196,144,092,041,246,196,146,097,048 
	EQUB 	000,208,161,114,068,022,233,188,144,100,057,014,228,186,145,104 
	EQUB 	064,024,241,202,164,126,089,052,016,236,201,166,132,098,065,032 
	EQUB 	000,224,193,162,132,102,073,044,016,244,217,190,164,138,113,088 
	EQUB 	064,040,017,250,228,206,185,164,144,124,105,086,068,050,033,016 
	EQUB 	000,240,225,210,196,182,169,156,144,132,121,110,100,090,081,072 
	EQUB 	064,056,049,042,036,030,025,020,016,012,009,006,004,002,001,000 

ALIGN &100
.sqrhi                                                          
	EQUB	000,000,000,000,000,000,000,000,000,000,000,000,000,000,000,000
	EQUB	000,000,000,000,000,000,000,000,000,000,000,000,000,000,000,000
	EQUB	001,001,001,001,001,001,001,001,001,001,001,001,001,001,002,002
	EQUB	002,002,002,002,002,002,002,002,003,003,003,003,003,003,003,003
	EQUB	004,004,004,004,004,004,004,004,005,005,005,005,005,005,005,006
	EQUB	006,006,006,006,006,007,007,007,007,007,007,008,008,008,008,008
	EQUB	009,009,009,009,009,009,010,010,010,010,010,011,011,011,011,012
	EQUB	012,012,012,012,013,013,013,013,014,014,014,014,015,015,015,015
	EQUB	016,016,016,016,017,017,017,017,018,018,018,018,019,019,019,019
	EQUB	020,020,020,021,021,021,021,022,022,022,023,023,023,024,024,024
	EQUB	025,025,025,025,026,026,026,027,027,027,028,028,028,029,029,029
	EQUB	030,030,030,031,031,031,032,032,033,033,033,034,034,034,035,035
	EQUB	036,036,036,037,037,037,038,038,039,039,039,040,040,041,041,041
	EQUB	042,042,043,043,043,044,044,045,045,045,046,046,047,047,048,048
	EQUB	049,049,049,050,050,051,051,052,052,053,053,053,054,054,055,055
	EQUB	056,056,057,057,058,058,059,059,060,060,061,061,062,062,063,063
	EQUB	064,064,065,065,066,066,067,067,068,068,069,069,070,070,071,071
	EQUB	072,072,073,073,074,074,075,076,076,077,077,078,078,079,079,080
	EQUB	081,081,082,082,083,083,084,084,085,086,086,087,087,088,089,089
	EQUB	090,090,091,092,092,093,093,094,095,095,096,096,097,098,098,099
	EQUB	100,100,101,101,102,103,103,104,105,105,106,106,107,108,108,109
	EQUB	110,110,111,112,112,113,114,114,115,116,116,117,118,118,119,120
	EQUB	121,121,122,123,123,124,125,125,126,127,127,128,129,130,130,131
	EQUB	132,132,133,134,135,135,136,137,138,138,139,140,141,141,142,143
	EQUB	144,144,145,146,147,147,148,149,150,150,151,152,153,153,154,155
	EQUB	156,157,157,158,159,160,160,161,162,163,164,164,165,166,167,168
	EQUB	169,169,170,171,172,173,173,174,175,176,177,178,178,179,180,181
	EQUB	182,183,183,184,185,186,187,188,189,189,190,191,192,193,194,195
	EQUB	196,196,197,198,199,200,201,202,203,203,204,205,206,207,208,209
	EQUB	210,211,212,212,213,214,215,216,217,218,219,220,221,222,223,224
	EQUB	225,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239
	EQUB	240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255

ALIGN &100
.negsqrlo                                                       
	EQUB 128,001,130,004,134,009,140,016,148,025,158,036,170,049,184,064
	EQUB 200,081,218,100,238,121,004,144,028,169,054,196,082,225,112,000
	EQUB 144,033,178,068,214,105,252,144,036,185,078,228,122,017,168,064
	EQUB 216,113,010,164,062,217,116,016,172,073,230,132,034,193,096,000
	EQUB 160,065,226,132,038,201,108,016,180,089,254,164,074,241,152,064
	EQUB 232,145,058,228,142,057,228,144,060,233,150,068,242,161,080,000
	EQUB 176,097,018,196,118,041,220,144,068,249,174,100,026,209,136,064
	EQUB 248,177,106,036,222,153,084,016,204,137,070,004,194,129,064,000
	EQUB 192,129,066,004,198,137,076,016,212,153,094,036,234,177,120,064
	EQUB 008,209,154,100,046,249,196,144,092,041,246,196,146,097,048,000
	EQUB 208,161,114,068,022,233,188,144,100,057,014,228,186,145,104,064
	EQUB 024,241,202,164,126,089,052,016,236,201,166,132,098,065,032,000
	EQUB 224,193,162,132,102,073,044,016,244,217,190,164,138,113,088,064
	EQUB 040,017,250,228,206,185,164,144,124,105,086,068,050,033,016,000
	EQUB 240,225,210,196,182,169,156,144,132,121,110,100,090,081,072,064
	EQUB 056,049,042,036,030,025,020,016,012,009,006,004,002,001,000,000
	EQUB 000,001,002,004,006,009,012,016,020,025,030,036,042,049,056,064
	EQUB 072,081,090,100,110,121,132,144,156,169,182,196,210,225,240,000
	EQUB 016,033,050,068,086,105,124,144,164,185,206,228,250,017,040,064
	EQUB 088,113,138,164,190,217,244,016,044,073,102,132,162,193,224,000
	EQUB 032,065,098,132,166,201,236,016,052,089,126,164,202,241,024,064
	EQUB 104,145,186,228,014,057,100,144,188,233,022,068,114,161,208,000
	EQUB 048,097,146,196,246,041,092,144,196,249,046,100,154,209,008,064
	EQUB 120,177,234,036,094,153,212,016,076,137,198,004,066,129,192,000
	EQUB 064,129,194,004,070,137,204,016,084,153,222,036,106,177,248,064
	EQUB 136,209,026,100,174,249,068,144,220,041,118,196,018,097,176,000
	EQUB 080,161,242,068,150,233,060,144,228,057,142,228,058,145,232,064
	EQUB 152,241,074,164,254,089,180,016,108,201,038,132,226,065,160,000
	EQUB 096,193,034,132,230,073,172,016,116,217,062,164,010,113,216,064
	EQUB 168,017,122,228,078,185,036,144,252,105,214,068,178,033,144,000
	EQUB 112,225,082,196,054,169,028,144,004,121,238,100,218,081,200,064
	EQUB 184,049,170,036,158,025,148,016,140,009,134,004,130,001,128,000

ALIGN &100
.negsqrhi
 EQUB 063,063,062,062,061,061,060,060,059,059,058,058,057,057,056,056
 EQUB 055,055,054,054,053,053,053,052,052,051,051,050,050,049,049,049
 EQUB 048,048,047,047,046,046,045,045,045,044,044,043,043,043,042,042
 EQUB 041,041,041,040,040,039,039,039,038,038,037,037,037,036,036,036
 EQUB 035,035,034,034,034,033,033,033,032,032,031,031,031,030,030,030
 EQUB 029,029,029,028,028,028,027,027,027,026,026,026,025,025,025,025
 EQUB 024,024,024,023,023,023,022,022,022,021,021,021,021,020,020,020
 EQUB 019,019,019,019,018,018,018,018,017,017,017,017,016,016,016,016
 EQUB 015,015,015,015,014,014,014,014,013,013,013,013,012,012,012,012
 EQUB 012,011,011,011,011,010,010,010,010,010,009,009,009,009,009,009
 EQUB 008,008,008,008,008,007,007,007,007,007,007,006,006,006,006,006
 EQUB 006,005,005,005,005,005,005,005,004,004,004,004,004,004,004,004
 EQUB 003,003,003,003,003,003,003,003,002,002,002,002,002,002,002,002
 EQUB 002,002,001,001,001,001,001,001,001,001,001,001,001,001,001,001
 EQUB 000,000,000,000,000,000,000,000,000,000,000,000,000,000,000,000
 EQUB 000,000,000,000,000,000,000,000,000,000,000,000,000,000,000,000
 EQUB 000,000,000,000,000,000,000,000,000,000,000,000,000,000,000,000
 EQUB 000,000,000,000,000,000,000,000,000,000,000,000,000,000,000,001
 EQUB 001,001,001,001,001,001,001,001,001,001,001,001,001,002,002,002
 EQUB 002,002,002,002,002,002,002,003,003,003,003,003,003,003,003,004
 EQUB 004,004,004,004,004,004,004,005,005,005,005,005,005,005,006,006
 EQUB 006,006,006,006,007,007,007,007,007,007,008,008,008,008,008,009
 EQUB 009,009,009,009,009,010,010,010,010,010,011,011,011,011,012,012
 EQUB 012,012,012,013,013,013,013,014,014,014,014,015,015,015,015,016
 EQUB 016,016,016,017,017,017,017,018,018,018,018,019,019,019,019,020
 EQUB 020,020,021,021,021,021,022,022,022,023,023,023,024,024,024,025
 EQUB 025,025,025,026,026,026,027,027,027,028,028,028,029,029,029,030
 EQUB 030,030,031,031,031,032,032,033,033,033,034,034,034,035,035,036
 EQUB 036,036,037,037,037,038,038,039,039,039,040,040,041,041,041,042
 EQUB 042,043,043,043,044,044,045,045,045,046,046,047,047,048,048,049
 EQUB 049,049,050,050,051,051,052,052,053,053,053,054,054,055,055,056
 EQUB 056,057,057,058,058,059,059,060,060,061,061,062,062,063,063,064
	
ALIGN &100
.REC16LO                                                         
 EQUB 255,000,085,000,051,170,146,000,113,153,069,085,177,073,017,000
 EQUB 015,056,121,204,048,162,033,170,061,216,123,036,211,136,066,000
 EQUB 193,135,080,028,235,188,144,102,062,024,244,209,176,144,114,085
 EQUB 057,030,005,236,212,189,167,146,125,105,086,068,050,033,016,000
 EQUB 240,224,210,195,181,168,155,142,129,117,105,094,083,072,061,051
 EQUB 041,031,021,012,003,250,241,232,224,216,208,200,192,185,177,170
 EQUB 163,156,149,143,136,130,124,118,112,106,100,094,089,083,078,073
 EQUB 067,062,057,052,048,043,038,034,029,025,020,016,012,008,004,000
 EQUB 252,248,244,240,236,233,229,225,222,218,215,212,208,205,202,199
 EQUB 195,192,189,186,183,180,178,175,172,169,166,164,161,158,156,153
 EQUB 151,148,146,143,141,138,136,134,131,129,127,125,122,120,118,116
 EQUB 114,112,110,108,106,104,102,100,098,096,094,092,090,088,087,085
 EQUB 083,081,080,078,076,074,073,071,070,068,066,065,063,062,060,059
 EQUB 057,056,054,053,051,050,048,047,046,044,043,041,040,039,037,036
 EQUB 035,033,032,031,030,028,027,026,025,024,022,021,020,019,018,017
 EQUB 015,014,013,012,011,010,009,008,007,006,005,004,003,002,001,000

.REC16HI                                                         
 EQUB 255,128,085,064,051,042,036,032,028,025,023,021,019,018,017,016
 EQUB 015,014,013,012,012,011,011,010,010,009,009,009,008,008,008,008
 EQUB 007,007,007,007,006,006,006,006,006,006,005,005,005,005,005,005
 EQUB 005,005,005,004,004,004,004,004,004,004,004,004,004,004,004,004
 EQUB 003,003,003,003,003,003,003,003,003,003,003,003,003,003,003,003
 EQUB 003,003,003,003,003,002,002,002,002,002,002,002,002,002,002,002
 EQUB 002,002,002,002,002,002,002,002,002,002,002,002,002,002,002,002
 EQUB 002,002,002,002,002,002,002,002,002,002,002,002,002,002,002,002
 EQUB 001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001
 EQUB 001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001
 EQUB 001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001
 EQUB 001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001
 EQUB 001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001
 EQUB 001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001
 EQUB 001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001
 EQUB 001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001

ALIGN &100
.SINTAB
 EQUB    0,   0,   1,   2,   3,   3,   4,   5,   6,   7,   7,   8,   9,  10,  10,  11
 EQUB   12,  13,  14,  14,  15,  16,  17,  17,  18,  19,  20,  20,  21,  22,  23,  24
 EQUB   24,  25,  26,  27,  27,  28,  29,  30,  30,  31,  32,  33,  33,  34,  35,  36
 EQUB   36,  37,  38,  39,  39,  40,  41,  42,  42,  43,  44,  44,  45,  46,  47,  47
 EQUB   48,  49,  50,  50,  51,  52,  52,  53,  54,  55,  55,  56,  57,  57,  58,  59
 EQUB   59,  60,  61,  61,  62,  63,  63,  64,  65,  65,  66,  67,  67,  68,  69,  69
 EQUB   70,  71,  71,  72,  73,  73,  74,  75,  75,  76,  76,  77,  78,  78,  79,  79
 EQUB   80,  81,  81,  82,  82,  83,  84,  84,  85,  85,  86,  87,  87,  88,  88,  89
 EQUB   89,  90,  90,  91,  92,  92,  93,  93,  94,  94,  95,  95,  96,  96,  97,  97
 EQUB   98,  98,  99,  99, 100, 100, 101, 101, 102, 102, 102, 103, 103, 104, 104, 105
 EQUB  105, 106, 106, 106, 107, 107, 108, 108, 108, 109, 109, 110, 110, 110, 111, 111
 EQUB  112, 112, 112, 113, 113, 113, 114, 114, 114, 115, 115, 115, 116, 116, 116, 117
 EQUB  117, 117, 117, 118, 118, 118, 119, 119, 119, 119, 120, 120, 120, 120, 121, 121
 EQUB  121, 121, 121, 122, 122, 122, 122, 123, 123, 123, 123, 123, 123, 124, 124, 124
 EQUB  124, 124, 124, 125, 125, 125, 125, 125, 125, 125, 125, 125, 126, 126, 126, 126
 EQUB  126, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126
 EQUB  126, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126
 EQUB  126, 126, 126, 126, 126, 125, 125, 125, 125, 125, 125, 125, 125, 124, 124, 124
 EQUB  124, 124, 124, 124, 123, 123, 123, 123, 123, 122, 122, 122, 122, 122, 121, 121
 EQUB  121, 121, 121, 120, 120, 120, 120, 119, 119, 119, 119, 118, 118, 118, 117, 117
 EQUB  117, 116, 116, 116, 116, 115, 115, 115, 114, 114, 114, 113, 113, 113, 112, 112
 EQUB  111, 111, 111, 110, 110, 110, 109, 109, 108, 108, 108, 107, 107, 106, 106, 105
 EQUB  105, 105, 104, 104, 103, 103, 102, 102, 101, 101, 101, 100, 100,  99,  99,  98
 EQUB   98,  97,  97,  96,  96,  95,  95,  94,  94,  93,  92,  92,  91,  91,  90,  90
 EQUB   89,  89,  88,  88,  87,  86,  86,  85,  85,  84,  84,  83,  82,  82,  81,  81
 EQUB   80,  79,  79,  78,  78,  77,  76,  76,  75,  74,  74,  73,  73,  72,  71,  71
 EQUB   70,  69,  69,  68,  67,  67,  66,  65,  65,  64,  63,  63,  62,  61,  61,  60
 EQUB   59,  59,  58,  57,  56,  56,  55,  54,  54,  53,  52,  52,  51,  50,  49,  49
 EQUB   48,  47,  47,  46,  45,  44,  44,  43,  42,  41,  41,  40,  39,  38,  38,  37
 EQUB   36,  35,  35,  34,  33,  32,  32,  31,  30,  29,  29,  28,  27,  26,  26,  25
 EQUB   24,  23,  23,  22,  21,  20,  20,  19,  18,  17,  16,  16,  15,  14,  13,  13
 EQUB   12,  11,  10,   9,   9,   8,   7,   6,   6,   5,   4,   3,   2,   2,   1,   0
 EQUB    0,   0,  -1,  -2,  -3,  -4,  -4,  -5,  -6,  -7,  -7,  -8,  -9, -10, -11, -11
 EQUB   -12, -13, -14, -14, -15, -16, -17, -18, -18, -19, -20, -21, -21, -22, -23, -24
 EQUB   -24, -25, -26, -27, -27, -28, -29, -30, -31, -31, -32, -33, -34, -34, -35, -36
 EQUB   -37, -37, -38, -39, -40, -40, -41, -42, -42, -43, -44, -45, -45, -46, -47, -48
 EQUB   -48, -49, -50, -50, -51, -52, -53, -53, -54, -55, -55, -56, -57, -57, -58, -59
 EQUB   -60, -60, -61, -62, -62, -63, -64, -64, -65, -66, -66, -67, -68, -68, -69, -70
 EQUB   -70, -71, -72, -72, -73, -73, -74, -75, -75, -76, -77, -77, -78, -78, -79, -80
 EQUB   -80, -81, -81, -82, -83, -83, -84, -84, -85, -86, -86, -87, -87, -88, -88, -89
 EQUB   -89, -90, -91, -91, -92, -92, -93, -93, -94, -94, -95, -95, -96, -96, -97, -97
 EQUB   -98, -98, -99, -99,-100,-100,-101,-101,-102,-102,-103,-103,-103,-104,-104,-105
 EQUB  -105,-106,-106,-106,-107,-107,-108,-108,-109,-109,-109,-110,-110,-110,-111,-111
 EQUB  -112,-112,-112,-113,-113,-113,-114,-114,-114,-115,-115,-115,-116,-116,-116,-117
 EQUB  -117,-117,-118,-118,-118,-118,-119,-119,-119,-119,-120,-120,-120,-120,-121,-121
 EQUB  -121,-121,-122,-122,-122,-122,-122,-123,-123,-123,-123,-123,-123,-124,-124,-124
 EQUB  -124,-124,-124,-125,-125,-125,-125,-125,-125,-125,-125,-125,-126,-126,-126,-126
 EQUB  -126,-126,-126,-126,-126,-126,-126,-126,-126,-126,-126,-126,-126,-126,-126,-126
 EQUB  -126,-126,-126,-126,-126,-126,-126,-126,-126,-126,-126,-126,-126,-126,-126,-126
 EQUB  -126,-126,-126,-126,-126,-125,-125,-125,-125,-125,-125,-125,-125,-124,-124,-124
 EQUB  -124,-124,-124,-124,-123,-123,-123,-123,-123,-122,-122,-122,-122,-122,-121,-121
 EQUB  -121,-121,-120,-120,-120,-120,-120,-119,-119,-119,-118,-118,-118,-118,-117,-117
 EQUB  -117,-116,-116,-116,-115,-115,-115,-115,-114,-114,-114,-113,-113,-112,-112,-112
 EQUB  -111,-111,-111,-110,-110,-109,-109,-109,-108,-108,-107,-107,-107,-106,-106,-105
 EQUB  -105,-105,-104,-104,-103,-103,-102,-102,-101,-101,-100,-100, -99, -99, -98, -98
 EQUB   -97, -97, -96, -96, -95, -95, -94, -94, -93, -93, -92, -92, -91, -91, -90, -90
 EQUB   -89, -89, -88, -87, -87, -86, -86, -85, -85, -84, -83, -83, -82, -82, -81, -80
 EQUB   -80, -79, -79, -78, -77, -77, -76, -76, -75, -74, -74, -73, -72, -72, -71, -70
 EQUB   -70, -69, -69, -68, -67, -67, -66, -65, -65, -64, -63, -63, -62, -61, -60, -60
 EQUB   -59, -58, -58, -57, -56, -56, -55, -54, -54, -53, -52, -51, -51, -50, -49, -49
 EQUB   -48, -47, -46, -46, -45, -44, -43, -43, -42, -41, -41, -40, -39, -38, -38, -37
 EQUB   -36, -35, -35, -34, -33, -32, -32, -31, -30, -29, -29, -28, -27, -26, -25, -25
 EQUB   -24, -23, -22, -22, -21, -20, -19, -19, -18, -17, -16, -16, -15, -14, -13, -12
 EQUB   -12, -11, -10,  -9,  -9,  -8,  -7,  -6,  -5,  -5,  -4,  -3,  -2,  -2,  -1,   0

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
EQUB 22, 4                             ; MODE 5

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
