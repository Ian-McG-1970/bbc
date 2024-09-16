; miner2021.a
;
;debugPixelCollision
;optionCheatOn
;optionMusicOff

; constants
charSPACE                           = $20       ;

black                               = 0
red                                 = 1
green                               = 2
yellow                              = 3
blue                                = 4
magenta                             = 5
cyan                                = 6
white                               = 7
flashingblackwhite                  = 8
flashingredcyan                     = 9
flashinggreenmagenta                = 10
flashingyellowblue                  = 11
flashingblueyellow                  = 12
flashingmagentagreen                = 13
flashingcyanred                     = 14
flashingwhiteblack                  = 15

keyCodeDELETE                       = $a6
keyCodeESCAPE                       = $8f
keyCodeS                            = $ae
keyCodeQ                            = $ef
keyCodeCOPY                         = $96
keyCodeSHIFT                        = $ff
keyCodeRETURN                       = $b6
keyCodeZ                            = $9e
keyCodeX                            = $bd
keyCodeColon                        = $b7
keyCodeForwardSlash                 = $97

; OS memory locations
interruptAccumulator_fc                = $fc       ; RESERVED for INTERRUPTS
irqv1_204                               = $204      ;
irqv2                               = $206      ;

logicalScreenStart                  = $5800 ; $3000     ;

; **************** BBC SHEILA *******************
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


; ***************************************************************************************
; game specifics

; zero page memory locations
SCN								= 0

altScreenAddressLow             = $00       ;
altScreenAddressHigh            = $01       ;
cachedCellX                     = $02       ;
cachedCellY                     = $03       ;
cursorX                         = $04       ;
cursorY                         = $05       ;
readCharScreenAddressLow        = $06       ;
readCharScreenAddressHigh       = $07       ;
metronome                       = $08       ; 0, 1, 2, 3
unused09                        = $09       ;
cellScreenAddressLow            = $0a       ;
cellScreenAddressHigh           = $0b       ;
airScreenAddressLow             = $0c       ;
airScreenAddressHigh            = $0d       ;

airRemainingOffsetWithinCell    = $0e       ;
airRemainingDelayCounter        = $0f       ;
score                           = $10       ; three bytes for the BCD score
score2                          = $11       ;
score3                          = $12       ;

lives                           = $13       ;
level20EnergyFieldsTimer        = $14       ;
willyHighestPointReachedOnCurrentJump = $15 ;
willyIsOnGround                 = $16       ;
willyOffsetWithinCellY          = $17       ;
airRemaining                    = $18       ;
playerPixelX                    = $19       ;
playerPixelY                    = $1a       ;
newPlayerPixelX                 = $1b       ;
newPlayerPixelY                 = $1c       ;
hasDrawnConveyors               = $1d       ;
tempHigh                        = $1e       ;
saveX                           = $1f       ;
characterFromScreen             = $20       ;
characterFromScreen2            = $21       ;
characterFromScreen3            = $22       ;
characterFromScreen4            = $23       ;
characterFromScreen5            = $24       ;
characterFromScreen6            = $25       ;
characterFromScreen7            = $26       ;
characterFromScreen8            = $27       ;
exitPositionX                   = $28       ;
exitPositionY                   = $29       ;
verticalAnimationCounter        = $2a       ; animation for vertical guardinas
currentGuardianX                = $2b       ;
currentGuardianY                = $2c       ;
currentGuardianExtent1          = $2d       ;
currentGuardianExtent2          = $2e       ;
currentGuardianIndex            = $2f       ;
tempGuardianSpriteOffset        = $30       ;
guardianSpriteOffsets           = $31       ;
guardianSpriteOffsets2          = $32       ;
guardianSpriteOffsets3          = $33       ;
tempGuardianSpriteAddrLow       = $34       ;
tempGuardianSpriteAddrHigh      = $35       ;
currentGuardianSpeed            = $36       ;
screenCharacterAPrime           = $37       ;
willyDeltaX                     = $38       ;
willyAnimationEOR               = $39       ; 0 if pointing right; 3 if pointing left
willyGraphicsPage               = $3a       ;
willyChangedDirection           = $3b       ;

; reading a character from the screen
tempLoopCounter                 = $3c       ;
tempByte                        = $3d       ;
tempMask                        = $3e       ;
plotColourMask                  = $3f       ;
tempScreenAddressLow            = $40       ;
tempScreenAddressHigh           = $41       ;

keyCounter                      = $42       ;
flashingExitCounter             = $43       ;

willyFalling                    = $44       ;
willyJustLandedOnConveyor       = $45       ;
screenCharacterA                = $46       ;
screenCharacterB                = $47       ;
isJumping                       = $48       ;
playerHeightOnTakeOff           = $49       ;
jumpPhase                       = $4a       ;
jumpDirection                   = $4b       ;
spriteRowCounter                = $4c       ;
playerXOffsetWithinCell         = $4d       ;
playerYNegativeOffsetWithinCell = $4e       ;
willyIsOnConveyor               = $4f       ;
willyDirectionAgainstFlowOfConveyor = $50   ; $01 or $ff when moving against the flow of the conveyor
spriteColourMask                = $51       ;
currentGuardianColour           = $52       ;
exitColour                      = $53       ;
drawGuardianLoopCounter         = $54       ;
conveyorX                       = $55       ;
conveyorY                       = $56       ;
conveyorLength                  = $57       ;
conveyorSpriteOffset            = $58       ;
tempA                           = $59       ;
tempX                           = $5a       ;
tempY                           = $5b       ;
byteToReverse                   = $5c       ;
keysLeft                        = $5d       ;
currentNoteTimePlayed           = $5e       ;
unused5F                        = $5f       ;
channel2Frequency               = $60       ;
previousNoteByte                = $61       ;
rememberCharacterToDraw         = $62       ;
byteToWrite                     = $63       ;

meteorTailAnimationOffset       = $64       ;
energyFieldSpriteOffset         = $64       ;
collisionFudgeFactor            = $65       ;

footPositionY                   = $66       ;
cheatModeEnabled                = $67       ;
levelNumberCounter              = $68       ;
tempCounter                     = $68       ;
zeroBasedLevel                  = $69       ;
roomNumberReached               = $6a       ;
keyPositionCounter              = $6b       ;
currentKeyIndex                 = $6c       ;
keyColourCounter                = $6d       ;
attractModeTimer                = $6e       ;
cheatCodeCounter                = $6f       ;

levelDataOffset                 = $70       ;

plotMode                        = $70       ;
plotX                           = $71       ;
plotY                           = $72       ;
plotXOffset                     = $73       ;
plotYOffset                     = $74       ;
plotScreenAddressLow            = $75       ;
plotScreenAddressHigh           = $76       ;
plotSourceSpriteAddressLow      = $77       ;
plotSourceSpriteAddressHigh     = $78       ;

currentLevelFeatureIndex        = $79       ;

; ***************************************************************************************
; level decode
singleItemSprite                = $7a       ;
decodeLevelByte                 = $7b       ;
singleItemX                     = $7c       ;
singleItemY                     = $7d       ;
singleItemOffsetX               = $7e       ;
singleItemType                  = $7f       ;

currentLevel                    = $80       ;
levelColours                    = $81       ;
levelColours2                   = $82       ;
screenLow                       = $85       ;
screenHigh                      = $86       ;
cellPlotSourceSpriteAddressLow  = $87       ;
cellPlotSourceSpriteAddressHigh = $88       ;
rasterArea                      = $89       ;
zpGuardianPositionX             = $8a       ;
zpGuardianPositionY             = $8b       ;
zpGuardianLimit1                = $8c       ;
zpGuardianLimit2                = $8d       ;
zpGuardianAnimIndex             = $8e       ;
singleItemSpriteOffset          = $8f       ;
musicEnabled                    = $90       ;
energyFieldTimer                = $91       ;

; ***************************************************************************************
kongYCoordinate                 = $92       ;
kongBeastSwitchTriggered        = $93       ;
kongAnimationCounter            = $94       ;

; ***************************************************************************************
eugenePositionY                 = $95       ; top bit indicates current direction
copyEugeneSourceByte            = $96       ;
copyEugeneTempByte              = $97       ; } shared
copyWhichSprite                 = $97       ; }

conveyorDirection               = $98       ;
textCursorX                     = $99       ;
textCursorY                     = $9a       ;

; ***************************************************************************************
meteorLandingY                  = $9b       ;
meteorXCoordinate               = $9c       ;
meteorYCoordinate               = $9d       ;
meteorAnimationState            = $9e       ;
meteorIndex                     = $9f       ;

; ***************************************************************************************
sideWallColour1                 = $a0       ;
sideWallColour2                 = $a1       ;
newSingleItemColour             = $a2       ;
conveyorColour                  = $a3       ;
volumeIndex                     = $a4       ;
saveY                           = $a5       ;
energyFieldLoopIndex            = $a6       ;
tempX2                          = $a7       ;
tempY2                          = $a8       ;
musicCounterLow                 = $a9       ;
musicCounterHigh                = $aa       ;

lastPageZeroLocationUsed        = $aa       ;


; ***************************************************************************************
collisionPlayerX                = $100      ;
collisionGuardianX              = $101      ;
collisionGuardianY              = $102      ;
collisionLow                    = $103      ;
collisionHigh                   = $104      ;
verticalToleranceTL             = $105      ;
verticalToleranceTR             = $106      ;
verticalToleranceBL             = $107      ;
verticalToleranceBR             = $108      ;
verticalToleranceReversedTL     = $109      ;
verticalToleranceReversedTR     = $10a      ;
verticalToleranceReversedBL     = $10b      ;
verticalToleranceReversedBR     = $10c      ;
horizontalToleranceTL           = $10d      ;
horizontalToleranceTR           = $10e      ;
horizontalToleranceBL           = $10f      ;
horizontalToleranceBR           = $110      ;
horizontalToleranceReversedTL   = $111      ;
horizontalToleranceReversedTR   = $112      ;
horizontalToleranceReversedBL   = $113      ;
horizontalToleranceReversedBR   = $114      ;
toleranceTL                     = $115      ;
toleranceTR                     = $116      ;
toleranceBL                     = $117      ;
toleranceBR                     = $118      ;
toleranceReversedTL             = $119      ;
toleranceReversedTR             = $11a      ;
toleranceReversedBL             = $11b      ;
toleranceReversedBR             = $11c      ;
guardianPositionsCopy           = $11d      ;
guardianPositionsCopy1          = $11e      ;
guardianPositionsCopy2          = $11f      ;
guardianPositionsCopy3          = $120      ;
guardianPositionsCopy4          = $121      ;
willySpriteAddressLow           = $122      ;
willySpriteAddressHigh          = $123      ;
willySpriteAddressHigh1         = $124      ; [UNUSED, but copied in and out of cache]
willySpriteAddressHigh2         = $125      ; [UNUSED, but copied in and out of cache]
willySpriteAddressHigh3         = $126      ; [UNUSED, but copied in and out of cache]

horizontalGuardianPositions     = $127      ; } first guardian
horizontalGuardianPositions1    = $128      ; } first guardian
horizontalGuardianPositions2    = $129      ; } first guardian
horizontalGuardianPositions3    = $12a      ; } first guardian

horizontalGuardianPositions4    = $12b      ; } second guardian
horizontalGuardianPositions5    = $12c      ; } second guardian
horizontalGuardianPositions6    = $12d      ; } second guardian
horizontalGuardianPositions7    = $12e      ; } second guardian

horizontalGuardianPositions8    = $12f      ; } third guardian
horizontalGuardianPositions9    = $130      ; } third guardian
horizontalGuardianPositions10   = $131      ; } third guardian
horizontalGuardianPositions11   = $132      ; } third guardian

horizontalGuardianPositions12   = $133      ; } fourth guardian
horizontalGuardianPositions13   = $134      ; } fourth guardian
horizontalGuardianPositions14   = $135      ; } fourth guardian
horizontalGuardianPositions15   = $136      ; } fourth guardian

horizontalGuardianPositionsTerminator = $137 ; terminator

; ***************************************************************************************
; Each vertical guardian has four bytes in this table
; <x coord + top bit for direction + bit 6 for speed>, <y coord>, <Y extent 1>, <Y extent 2>
currentVerticalGuardians        = $138      ; } first guardian x
currentVerticalGuardians1       = $139      ; } first guardian y
currentVerticalGuardians2       = $13a      ; } first guardian y extent 1
currentVerticalGuardians3       = $13b      ; } first guardian y extent 2

verticalGuardian2               = $13c      ; start of second vertical guardian
currentVerticalGuardians4       = $13c      ; } second guardian x
currentVerticalGuardians5       = $13d      ; } second guardian y
currentVerticalGuardians6       = $13e      ; } second guardian y extent 1
currentVerticalGuardians7       = $13f      ; } second guardian y extent 2

currentVerticalGuardians8       = $140      ; } third guardian x
currentVerticalGuardians9       = $141      ; } third guardian y
currentVerticalGuardians10      = $142      ; } third guardian y extent 1
currentVerticalGuardians11      = $143      ; } third guardian y extent 2

currentVerticalGuardians12      = $144      ; } fourth guardian x
currentVerticalGuardians13      = $145      ; } fourth guardian y
currentVerticalGuardians14      = $146      ; } fourth guardian y extent 1
currentVerticalGuardians15      = $147      ; } fourth guardian y extent 2

keyXPositions                   = $148      ; X position of key 0
keyXPositions1                  = $149      ; X position of key 1
keyXPositions2                  = $14a      ; X position of key 2
keyXPositions3                  = $14b      ; X position of key 3
keyXPositions4                  = $14c      ; X position of key 4

keyYPositions                   = $14d      ; Y position of key 0
keyYPositions1                  = $14e      ; Y position of key 1
keyYPositions2                  = $14f      ; Y position of key 2
keyYPositions3                  = $150      ; Y position of key 3
keyYPositions4                  = $151      ; Y position of key 4

keyGot                          = $152      ; key 0 obtained
keyGot1                         = $153      ; key 1 obtained
keyGot2                         = $154      ; key 2 obtained
keyGot3                         = $155      ; key 3 obtained
keyGot4                         = $156      ; key 4 obtained

highScore                       = $157      ; three bytes for the BCD score
highScore2                      = $158      ;
highScore3                      = $159      ;

horizontalByteCounter           = $15a      ;

; ***************************************************************************************
verticalGuardianMODE1Address    = $0400     ;
verticalGuardianMODE1Address1   = $0440     ;
verticalGuardianMODE1Address2   = $0480     ;
verticalGuardianMODE1Address3   = $04c0     ;

screenStartAddress              = $3280     ; start of visible screen
blankAreaOfScreen               = $34d0     ; empty screen area to the right of the play area
playAreaEndAddress              = $5a80     ;
screenEndAddress                = $6480     ; end of visible screen
backgroundCache                 = $7fa0     ; six character cells copied from the screen before the player is drawn

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

EQUB 23, 0, 2, 45      ; Set 6845 register R2 = 45
EQUB 0, 0, 0
EQUB 0, 0, 0           ; This is the "horizontal sync position" register, which defines the position of the horizontal sync pulse on the horizontal line in terms of character widths from the left-hand side of the screen. For comparison this is 49 for modes 4 and 5, but needs to be adjusted for our custom screen's width

;EQUB 23, 0, 7, 1      ; Set 6845 register R7 = 45
;EQUB 0, 0, 0
;EQUB 0, 0, 0           ; This is the "vertical sync position" register, which defines the position of the horizontal sync pulse on the horizontal line in terms of character widths from the left-hand side of the screen. For comparison this is 49 for modes 4 and 5, but needs to be adjusted for our custom screen's width

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

    lda #$c0                                ; set User VIA T1 in free run mode (i.e. repeating)
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

;    jsr printFollowingMessage               ; print VDU stream
;    equb crtcSetupEnd - crtcSetup
;.crtcSetup
;    equb 23, 0,  7,  30, 0,0,0,0,0,0      	; VDU 23,0,7,30,0;0;0;0;   vertical sync position = 30
;    equb 23, 1,  0,   0, 0,0,0,0,0,0      	; cursor off
;    equb 23, 0, 13, $50, 0,0,0,0,0,0      	; set display address (low)
;    equb 26                               	; Reset text and graphics windows
;.crtcSetupEnd

.mloop jmp mloop

.printFollowingMessage		stx tempX2
							sty tempY2
							pla
							sta altScreenAddressLow
							pla
							sta altScreenAddressHigh                ; Pull the return address off the stack
							ldy #1
							lda (altScreenAddressLow), Y
							tax
.printFollowingMessagelp1		iny
								lda (altScreenAddressLow), Y
								jsr OSWRCH
								dex
								bne printFollowingMessagelp1
							tya
							clc
							adc altScreenAddressLow
							sta altScreenAddressLow
							bcc printFollowingMessagelp2
								inc altScreenAddressHigh
.printFollowingMessagelp2	lda altScreenAddressHigh                ; Pushing the return address onto the stack
							pha                                     ; immediately after the data
							lda altScreenAddressLow
							pha
							ldx tempX2
							ldy tempY2
						rts 

.mainInterruptRoutine		txa                                 ; Remember X and Y registers
							pha
							tya
							pha

							lda userVIAInterruptFlagRegister_fe6d    ; What kind of interrupt?
							and #%11000000
							cmp #%11000000                      ; is it the User VIA Timer 1?
							bne handledInterrupt				; unknown interrupt - pass through

.mainInterruptRoutinecont 	lda #$40                            ; clear User VIA timer 1 interrupt
							sta userVIAInterruptFlagRegister_fe6d

							lda #1
							jsr debugChangePalette

							ldx #250
							jsr delay

;	ldx #10
;	ldy #10
;	jsr	PLOT01
;	ldx #11
;	ldy #11
;	jsr	PLOT01
;	ldx #12
;	ldy #12
;	jsr PLOT01
;	ldx #14
;	ldy #14
;	jsr	PLOT01
;	ldx #16
;	ldy #16
;	jsr PLOT01
;	ldx #18
;	ldy #18
;	jsr PLOT01

	ldy #1
	ldx #0
	jsr LINE01
	ldy #1
	ldx #10
	jsr LINE01

;	ldy #3
;	ldx #0
;	jsr LINE01
;	ldy #3
;	ldx #10
;	jsr LINE01

;	ldy #5
;	ldx #0
;	jsr LINE01
;	ldy #5
;	ldx #10
;	jsr LINE01

	ldy #7
	ldx #0
	jsr LINE01
	ldy #7
	ldx #10
	jsr LINE01

	ldy #9
	ldx #0
	jsr LINE01
	ldy #9
	ldx #10
	jsr LINE01

	ldy #11
	ldx #0
	jsr LINE01
	ldy #11
	ldx #10
	jsr LINE01

	ldy #157
	ldx #0
	jsr LINE01

	ldy #159
	ldx #0
	jsr LINE01

	ldy #189
	ldx #0
	jsr LINE01

	ldy #191
	ldx #0
	jsr LINE01


							lda #0
							jsr debugChangePalette

.handledInterrupt			pla                                 ; Restore X and Y registers
							tay
							pla
							tax
							lda interruptAccumulator_fc
							rti

.delay	dex
		bne delay
		rts
 
.debugChangePalette			sta videoULAPaletteRegister_fe21
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

.PLOT01		LDA   SCR_LO,Y
			STA   SCN
			LDA   SCR_HI,Y 
			STA   SCN+1 
            LDY   SCR_HOR,X
            LDA   (SCN),Y
			ORA   SCR_OR_01,X
            STA   (SCN),Y 
            RTS

.LINE01		LDA   SCR_LO,Y
			STA   SCN
			LDA   SCR_HI,Y 
			STA   SCN+1 
            LDY   SCR_HOR,X
;            LDA   (SCN),Y
;			ORA   SCR_OR_01,X
			lda #$0f ; %01010101
            STA   (SCN),Y 
            RTS

.PLOT10		LDA   SCR_LO,Y
			STA   SCN
			LDA   SCR_HI,Y 
			STA   SCN+1 
            LDY   SCR_HOR,X
            LDA   (SCN),Y
			ORA   SCR_OR_10,X
            STA   (SCN),Y 
            RTS

.CLR01		LDA   SCR_LO,Y
			STA   SCN
			LDA   SCR_HI,Y 
			STA   SCN+1 
            LDY   SCR_HOR,X
            LDA   (SCN),Y
			AND   SCR_AND_01,X
            STA   (SCN),Y 
            RTS

.CLR10		LDA   SCR_LO,Y
			STA   SCN
			LDA   SCR_HI,Y 
			STA   SCN+1 
            LDY   SCR_HOR,X
            LDA   (SCN),Y
			AND   SCR_AND_10,X
            STA   (SCN),Y 
            RTS

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
