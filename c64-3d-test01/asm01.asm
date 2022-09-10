  *=$0801
  
  !basic
  
  joystickPort=$DC00

  ; for cleaning the screen, we could determine which
  ; part needs cleaning by keeping track of the bounding
  ; box that holds the latest rendering

  BANK_ONE_START_ADDRESS=$20
  BANK_TWO_START_ADDRESS=$60
  
  zeropageStart=240
  plotAddress     = zeropageStart+0
  plotAddresslow  = zeropageStart+0
  plotAddresshigh = zeropageStart+1


  FIGURELENGTH=$24

  varPageStart=$c000
;  bankAminx=varPageStart+1
;  bankAminy=varPageStart+2
;  bankAmaxx=varPageStart+3
;  bankAmaxy=varPageStart+4
;  bankBminx=varPageStart+5
;  bankBminy=varPageStart+6
;  bankBmaxx=varPageStart+7
;  bankBmaxy=varPageStart+8
  currentBank=varPageStart+23 ; either #$20 or #$60
  joyXValue=varPageStart+9
  joyXSpeed=varPageStart+10
  joyYValue=varPageStart+11
  joyYSpeed=varPageStart+12
  joyZValue=varPageStart+13
  joyZSpeed=varPageStart+14
  X1=varPageStart+15
  Y1=varPageStart+16
  X2=varPageStart+17
  Y2=varPageStart+18
  DeltaX=varPageStart+19
  DeltaY=varPageStart+20
  DeltaSign=varPageStart+21
  DeltaWork=varPageStart+22
  plotX=varPageStart+25
  plotY=varPageStart+26
  tmp1=varPageStart+29
  tmp2=varPageStart+30
  tmp3=varPageStart+31
  

  
  
;  #    #    ##       #    #    #
;  ##  ##   #  #      #    ##   #
;  # ## #  #    #     #    # #  #
;  #    #  ######     #    #  # #
;  #    #  #    #     #    #   ##
;  #    #  #    #     #    #    #

  sei
  jsr switchBank
  jsr ClearBitMapBruteForce
  jsr drawFigure
  jsr switchBank
  jsr ClearBitMapBruteForce
  jsr drawFigure
  ; Clear/set all color memory

  jsr fullColorClear

mainLoop

verticalBlank
  lda $d012
  cmp #$80
  bne verticalBlank
  dec $d020
  
  ;jsr performColorFade
  
  ;jsr ClearBitMapBruteForce
  jsr clearCachedPixels
  ;jsr clearBox

  jsr getJoystick

  dec $d020
  jsr drawFigure

  jsr switchBank

  ;jsr waitLongTime

  inc $d020
  inc $d020
  
  jmp mainLoop


  
waitLongTime
  ldx #0
  ldy #0
WLTLoop
  iny
  bne WLTLoop
  inx
  cpx #$e0
  bne WLTLoop
  rts

  
;  ####    ####   #        ####   #####
; #    #  #    #  #       #    #  #    #
; #       #    #  #       #    #  #    #
; #       #    #  #       #    #  #####
; #    #  #    #  #       #    #  #   #
;  ####    ####   ######   ####   #    #

coloridx
  !byte 0
colorFadeData
  !byte   0,11,9,6,12,15,7,3,1
  !byte   1,3,7,15,12,6,9,11,0
colorFadeEnd
performColorFade
  ldx coloridx
  inx
  cpx #17
  bne noColorFadeReset
  ldx #0
noColorFadeReset
  stx coloridx
  lda colorFadeData,x
  sta $d020
  
  lda #17
  clc
  sbc coloridx
  tax 
  lda colorFadeData,x
  sta $d021
  rts

  
  
;  #####  #     #   ###   #######  #####  #     # ######     #    #     # #    #
; #     # #  #  #    #       #    #     # #     # #     #   # #   ##    # #   #
; #       #  #  #    #       #    #       #     # #     #  #   #  # #   # #  #
;  #####  #  #  #    #       #    #       ####### ######  #     # #  #  # ###
;       # #  #  #    #       #    #       #     # #     # ####### #   # # #  #
; #     # #  #  #    #       #    #     # #     # #     # #     # #    ## #   #
;  #####   ## ##    ###      #     #####  #     # ######  #     # #     # #    #

currentSwitchBank
  !byte 0

  switchBank
  ; wait for the right moment...
waitForHighLine
  lda $d011
  and #$80
  bne waitForHighLine
waitForLine
  lda $d012
  cmp #$20
  bne waitForLine

  ; Set viewport - multicolor, 38 wide, xpos=0
  lda #$08
  sta $d016
  ; bitmap mode, blank-screen-to-border, 25 rows, ypos=7
  lda #$3B
  sta $d011
  
  lda #$18  ; screen at $0400, bitmap $2000
  sta $d018

  inc currentSwitchBank
  lda currentSwitchBank
  and #1
  bne useBankA
  jmp useBankB
useBankA
  lda #BANK_TWO_START_ADDRESS ; work in bank B
  sta currentBank
  lda $DD00
  and #$FC ; use bank B ($0000-$3fff)
  ora #$03 ; use bank A ($4000-$7fff)
  jmp storeBank
useBankB
  lda #BANK_ONE_START_ADDRESS ; work in bank A
  sta currentBank
  lda $DD00
  and #$FC ; use bank B ($4000-$7fff)
  ora #$02
storeBank
  sta $DD00 ; store in bank-switcher


  
  lda #0
  sta plotAddress
  lda currentBank
  sta plotAddress+1
;  sta $d020
;  sta $d021 
  rts
  
  
; - - - - - - - - - - - - - - - - - - - - - - - - - - - 
; - - - - - - - - - - - - - - - - - - - - - - - - - - - 
; - - - - - - - - - - - - - - - - - - - - - - - - - - - 

;  #####  #       #######    #    ######
; #     # #       #         # #   #     #
; #       #       #        #   #  #     #
; #       #       #####   #     # ######
; #       #       #       ####### #   #
; #     # #       #       #     # #    #
;  #####  ####### ####### #     # #     #

  
ClearBitMapBruteForce
  ; self-modifying code: set the bank address in the clear routine
  ldx #0
  lda currentBank
ClearBitMapSelfModify
  sta selfmodclear+2,x
  inx
  inx
  inx
  clc
  adc #1
  cpx #$60
  bne ClearBitMapSelfModify

  ldx #0
  lda #0
clear
selfmodclear
  sta $2000,x
  sta $2100,x
  sta $2200,x
  sta $2300,x
  sta $2400,x
  sta $2500,x
  sta $2600,x
  sta $2700,x
  sta $2800,x
  sta $2900,x
  sta $2a00,x
  sta $2b00,x
  sta $2c00,x
  sta $2d00,x
  sta $2e00,x
  sta $2f00,x

  sta $3000,x
  sta $3100,x
  sta $3200,x
  sta $3300,x
  sta $3400,x
  sta $3500,x
  sta $3600,x
  sta $3700,x
  sta $3800,x
  sta $3900,x
  sta $3a00,x
  sta $3b00,x
  sta $3c00,x
  sta $3d00,x
  sta $3e00,x
  sta $3f00,x
  inx
  bne clear
  rts
 
  
; 
; FULL COLOR CLEAR
;
fullColorClear:
  lda #$FB
  ldx #0
fullColorClearLoop
  sta $0400,x
  sta $0500,x
  sta $0600,x
  sta $06e8,x
  sta $4400,x
  sta $4500,x
  sta $4600,x
  sta $46e8,x
  sta $d800,x
  sta $d900,x
  sta $da00,x
  sta $db00,x
  sta $dbe8,x  
  inx
  bne fullColorClearLoop
  rts


  

;    #####  #       #######    #    ######          ######  ####### #     #
;   #     # #       #         # #   #     #         #     # #     #  #   #
;   #       #       #        #   #  #     #         #     # #     #   # #
;   #       #       #####   #     # ######   #####  ######  #     #    #
;   #       #       #       ####### #   #           #     # #     #   # #
;   #     # #       #       #     # #    #          #     # #     #  #   #
;    #####  ####### ####### #     # #     #         ######  ####### #     #
;
; clear the box set by (clearMinXBankX,clearMinYBankY)-(clearMaxXBankX,clearMaxYBankY)
;


clearMinXBankA
  !byte $ff
clearMinYBankA
  !byte $ff
clearMaxXBankA
  !byte 0
clearMaxYBankA
  !byte 0
clearMinXBankB
  !byte $ff
clearMinYBankB
  !byte $ff
clearMaxXBankB
  !byte 0
clearMaxYBankB
  !byte 0

clearBox
  ; change fill pattern
  lda #$33
  eor plotClearPattern+1
  sta plotClearPattern+1

  lda currentBank
  cmp #BANK_ONE_START_ADDRESS
  beq clearBoxBankA
  jmp clearBoxBankB

clearBoxBankA
  dec $d020
  ldy clearMinYBankA
clearBoxBankAOuterLoop
  ldx clearMinXBankA
clearBoxBankAInnerLoop
  jsr plotClearXY
  dec $d020
  txa ; jump to the next byte to the right
  clc
  adc #8
  bcs clearBoxBankANextY ; over 255? Done!
  cmp clearMaxXBankA     ; over maxX? Done!
  bcs clearBoxBankANextY
  tax
  jmp clearBoxBankBInnerLoop  
clearBoxBankANextY
  iny
  cpy clearMaxYBankA
  bcc clearBoxBankAOuterLoop
  inc $d020
  lda #$ff
  sta clearMinYBankA
  sta clearMinXBankA
  lda #0
  sta clearMaxYBankA
  sta clearMaxXBankA
  rts

clearBoxBankB
  dec $d020
  ldy clearMinYBankA
clearBoxBankBOuterLoop
  ldx clearMinXBankA
clearBoxBankBInnerLoop 
  jsr plotClearXY
  dec $d020
  txa ; jump to the next byte to the right
  clc
  adc #8
  bcs clearBoxBankBNextY ; over 255? Done!
  cmp clearMaxXBankA     ; over maxX? Done!
  bcs clearBoxBankBNextY
  tax
  jmp clearBoxBankBInnerLoop 
clearBoxBankBNextY
  iny
  cpy clearMaxYBankA
  bcc clearBoxBankBOuterLoop
  inc $d020
  lda #$ff
  sta clearMinYBankB
  sta clearMinXBankB
  lda #0
  sta clearMaxYBankB
  sta clearMaxXBankB
  rts
  
;   #####   #        ####    #####   ####   #       ######    ##    #####
;   #    #  #       #    #     #    #    #  #       #        #  #   #    #
;   #    #  #       #    #     #    #       #       #####   #    #  #    #
;   #####   #       #    #     #    #       #       #       ######  #####
;   #       #       #    #     #    #    #  #       #       #    #  #   #
;   #       ######   ####      #     ####   ######  ######  #    #  #    #  

; The following allows only for plotting between 0 and 255 (not 0..320)

plotClearXY
  ; save Y register, X register will not be touched
  sty tmp1
  
  lda ytablehigh,y
  bpl continuePlotClear; no plotting when table says "No!"
  rts
continuePlotClear
  clc
  adc currentBank
  sta plotAddress+1

  lda ytablelow,y
  sta plotAddress

  ldy xtable,x

plotClearPattern
  lda #$AA
  sta (plotAddress),y  

  ; restore X and Y registers
  ldy tmp1
  rts  
  
  
  
  
  
  
  
  
  
  
  
  
; ######                           #######
; #     #  #####     ##    #    #  #         #     ####
; #     #  #    #   #  #   #    #  #         #    #    #
; #     #  #    #  #    #  #    #  #####     #    #
; #     #  #####   ######  # ## #  #         #    #  ###
; #     #  #   #   #    #  ##  ##  #         #    #    #
; ######   #    #  #    #  #    #  #         #     ####

drawFigureX1
  !byte 0,64,60,0
drawFigureY1 
  !byte 128,64,192,0
drawFigureX2
  !byte 128,0,0,0
drawFigureY2 
  !byte 0,0,0,0
drawFigureLength
  !byte 0
pointIndex
  !byte 0

drawFigure
  ;inc $d020
  lda #FIGURELENGTH
  sta drawFigureLength

  inc drawFigureX1
  inc drawFigureX1+1
  inc drawFigureX1+1
  inc drawFigureX1+2
  inc drawFigureX1+2
  dec drawFigureY1
  dec drawFigureY1+1
  dec drawFigureY1+1
  inc drawFigureY1+2
  inc drawFigureY1+2
  inc drawFigureY1+2
  
  ;inc drawFigureX1
  ;dec drawFigureY1
  ;inc drawFigureX1

DFLoop
  lda drawFigureLength
  clc
  rol
  rol
  rol
  rol
  rol
  sta pointIndex

  lda drawFigureX1
  clc
  adc pointIndex
  clc
  adc drawFigureX1+1
  clc
  adc drawFigureX1+2
  tax
  lda sin,x
  tax
  
  lda drawFigureY1
  clc
  adc pointIndex
  clc
  adc drawFigureY1+1
  clc
  adc drawFigureY1+2
  tay
  lda sin,y
  tay

  jsr plotXY
  dec drawFigureLength
  bne DFLoop
  rts
  
  ldx drawFigureX1
  lda sin,x
  tax
  ldy drawFigureY1
  lda sin,y
  tay
  stx X1
  sty Y1

  jsr plotXY
  
  ldx drawFigureX2
  lda sin,x
  tax
  ldy drawFigureY2
  lda sin,y
  tay
  stx X2
  sty Y2

  jsr plotXY

  inc drawFigureX1
  dec drawFigureY1
  inc drawFigureX2
  inc drawFigureY2

  ;dec $d020
  rts
  
  jmp drawLine




; #####   #####     ##    #    #  #       #    #    #  ######
; #    #  #    #   #  #   #    #  #       #    ##   #  #
; #    #  #    #  #    #  #    #  #       #    # #  #  #####
; #    #  #####   ######  # ## #  #       #    #  # #  #
; #    #  #   #   #    #  ##  ##  #       #    #   ##  #
; #####   #    #  #    #  #    #  ######  #    #    #  ######

  
drawLine
  ; draw line from (X1,Y1)-(X2,Y2)
  ; calc DeltaX
  lda X1
  sec
  sbc X2
  bcc storeDeltaX
  eor #$ff
  adc #$01
storeDeltaX
  sta DeltaX
  ; calc DeltaY
  lda Y1
  sec
  sbc Y2
  bcc storeDeltaY
  eor #$ff
  adc #$01
storeDeltaY
  sta DeltaY

  ;decide X or Y as main axis
  cmp DeltaX
  bcc drawLineDeltaXIsLessThanDeltaY
  jmp drawLineDeltaYIsLessThanDeltaX

drawLineDeltaXIsLessThanDeltaY    
; X is less than Y, so Y is the main axis we will follow
  lda X1
  cmp X2
  bcc drawLineX1X2
  ; X2 is smaller than X1, so switch the points
  lda X1
  ldx X2
  sta X2
  stx X1
  lda Y1
  ldx Y2
  sta Y2
  stx Y1
drawLineX1X2
  ;is Y change a negative or positive?
  ldx #$01 ; positive change
  lda Y1
  cmp Y2
  bmi Y1Smallest
  ; Y2<Y1, so change is negative
  ldx #$ff
Y1Smallest
  stx DeltaSign

  ; now start the drawing!
  lda DeltaX
  sta DeltaWork
  ldx X1
  ldy Y1
plotLoopX
  jsr plotXY
  inx
  ; decide to iny too?
  clc
  lda DeltaWork
  sbc DeltaY
  bcc storeDeltaWorkX
  clc
  adc DeltaX
  iny
storeDeltaWorkX
  sta DeltaWork

  cpx X2
  bne plotLoopX ; all done?
  rts

; --------------------
; Y is less than X, so X is the main axis we will follow    
drawLineDeltaYIsLessThanDeltaX
  lda Y1
  cmp Y2
  bcc drawLineY1Y2
  ; Y2 is smaller than Y1, so switch the points
  lda X1
  ldx X2
  sta X2
  stx X1
  lda Y1
  ldx Y2
  sta Y2
  stx Y1
drawLineY1Y2
  ;is X change a negative or positive?
  ldx #$01 ; positive change
  lda X1
  cmp X2
  bmi X1Smallest
  ; X2<X1, so change is negative
  ldx #$ff
X1Smallest
  stx DeltaSign

  ; now start the drawing!
  lda DeltaY
  sta DeltaWork
  ldx X1
  ldy Y1
plotLoopY
  jsr plotXY
  iny
  ; decide to inx too?
  clc
  lda DeltaWork
  sbc DeltaX
  bcc storeDeltaWorkY
  clc
  adc DeltaY
  iny
storeDeltaWorkY
  sta DeltaWork

  cpx Y2
  bne plotLoopY ; all done?
  rts


  

  
;  #####   #        ####    #####
;  #    #  #       #    #     #
;  #    #  #       #    #     #
;  #####   #       #    #     #
;  #       #       #    #     #
;  #       ######   ####      #
  
; The following allows only for plotting between 0 and 255 (not 0..320)

plotXY
  ; save X and Y registers
  sty tmp1
  stx tmp2
  
  lda ytablehigh,y
  bpl continuePlot; no plotting when table says "No!"
  rts
continuePlot
  clc
  adc currentBank
  sta plotAddress+1

  lda ytablelow,y
  sta plotAddress
  
  ldy xtable,x
  jsr cachePixel
  
  lda (plotAddress),y
  ora mask,x
  sta (plotAddress),y  


  ; restore X and Y registers
  ldy tmp1
  ldx tmp2

  ; PLOT IS DONE, NEXT IS:
  
  ;
  ; MIN-MAX BOX DETERMINATION
  ;
  ; determine (minx,miny)-(maxx,maxy) box for faster cleaning

  ; which bank are we working in?
  lda currentBank
  cmp #BANK_ONE_START_ADDRESS
  beq minMaxBankOne
  jmp minMaxBankTwo

minMaxBankOne
  sec
  cpy clearMinYBankA
  ; no carry borrow (still set)? -> Y smaller than clearMinY - sty
  ; carry borrow (carry clear)? -> Y larger than clearMinY - no sty
  bcs minYRemainsBankA
  sty clearMinYBankA
minYRemainsBankA
  sec
  cpx clearMinXBankA
  ; no carry borrow (still set)? -> X smaller than clearMinX - stx
  ; carry borrow (carry clear)? -> X larger than clearMinX - no stx
  bcs minXRemainsBankA
  stx clearMinXBankA
minXRemainsBankA
  sec
  cpy clearMaxYBankA
  ; no carry borrow (still set)? -> Y smaller than clearMaxY - no sty
  ; carry borrow (carry clear)? -> Y larger than clearMaxY - sty
  bcc maxYRemainsBankA
  sty clearMaxYBankA
maxYRemainsBankA
  sec
  cpx clearMaxXBankA
  ; no carry borrow (still set)? -> Y smaller than clearMaxY - no stx
  ; carry borrow (carry clear)? -> Y larger than clearMaxY - stx
  bcc maxXRemainsBankA
  stx clearMaxXBankA
maxXRemainsBankA
  rts

minMaxBankTwo
  sec
  cpy clearMinYBankB
  ; no carry borrow (still set)? -> Y smaller than clearMinY - sty
  ; carry borrow (carry clear)? -> Y larger than clearMinY - no sty
  bcs minYRemainsBankB
  sty clearMinYBankB
minYRemainsBankB
  sec
  cpx clearMinXBankB
  ; no carry borrow (still set)? -> X smaller than clearMinX - stx
  ; carry borrow (carry clear)? -> X larger than clearMinX - no stx
  bcs minXRemainsBankB
  stx clearMinXBankB
minXRemainsBankB
  sec
  cpy clearMaxYBankB
  ; no carry borrow (still set)? -> Y smaller than clearMaxY - no sty
  ; carry borrow (carry clear)? -> Y larger than clearMaxY - sty
  bcc maxYRemainsBankB
  sty clearMaxYBankB
maxYRemainsBankB
  sec
  cpx clearMaxXBankB
  ; no carry borrow (still set)? -> Y smaller than clearMaxY - no stx
  ; carry borrow (carry clear)? -> Y larger than clearMaxY - stx
  bcc maxXRemainsBankB
  stx clearMaxXBankB
maxXRemainsBankB
  rts

  
  


;
; CACHE PLOTTED PIXELS
; for quick delete
;
; Two parts; one per bank!
;

cachePixelsTmpX !byte 0
cachePixelsTmpA !byte 0


resetAllCachePixels
  jsr bankAResetCachePixels
  jmp bankBResetCachePixels
  
resetCachePixels
  lda currentBank
  cmp #BANK_ONE_START_ADDRESS
  beq bankAResetCachePixels
  jmp bankBResetCachePixels

cachePixel
  sta cachePixelsTmpA
  lda currentBank
  cmp #BANK_ONE_START_ADDRESS
  beq cachePixelA
  lda cachePixelsTmpA
  jmp bankBCachePixel
cachePixelA
  lda cachePixelsTmpA
  jmp bankACachePixel

clearCachedPixels  
  sta cachePixelsTmpA
  lda currentBank
  cmp #BANK_ONE_START_ADDRESS
  beq clearCachePixelA
  lda cachePixelsTmpA
  jmp bankBClearCachedPixels
clearCachePixelA
  lda cachePixelsTmpA
  jmp bankAClearCachedPixels

  
  
; BANK A PIXEL CACHING CODE  

bankACachePixelsIndex !byte 0
bankAResetCachePixels
  lda #0
  sta bankACachePixelsIndex
  rts

bankACachePixel
  stx cachePixelsTmpX
  sta cachePixelsTmpA

  inc bankACachePixelsIndex
  ldx bankACachePixelsIndex
  lda plotAddress
  sta bankACachePixelsAccHigh,x
  lda plotAddress+1
  sta bankACachePixelsAccLow,x
  tya
  sta bankACachePixelsY,x

  ldx cachePixelsTmpX
  lda cachePixelsTmpA
  rts  

bankAClearCachedPixels  
  ldx #$0
  ldy #$0
bankAClearCachedPixelsLoop
  lda bankACachePixelsAccHigh,x
  sta plotAddress
  lda bankACachePixelsAccLow,x
  sta plotAddress+1
  ldy bankACachePixelsY,x
  lda #$00
  sta (plotAddress),y
  inx
  cpx bankACachePixelsIndex
  bne bankAClearCachedPixelsLoop

  ; do the last pixel too
  lda bankACachePixelsAccHigh,x
  sta plotAddress
  lda bankACachePixelsAccLow,x
  sta plotAddress+1
  ldy bankACachePixelsY,x
  lda #$00
  sta (plotAddress),y
  jmp bankAResetCachePixels ; jmp saves an rts ;)



; BANK B PIXEL CACHING CODE  

bankBCachePixelsIndex !byte 0
bankBResetCachePixels
  lda #0
  sta bankBCachePixelsIndex
  rts

bankBCachePixel
  stx cachePixelsTmpX
  sta cachePixelsTmpA

  inc bankBCachePixelsIndex
  ldx bankBCachePixelsIndex
  lda plotAddress
  sta bankBCachePixelsAccHigh,x
  lda plotAddress+1
  sta bankBCachePixelsAccLow,x
  tya
  sta bankBCachePixelsY,x

  ldx cachePixelsTmpX
  lda cachePixelsTmpA
  rts  

bankBClearCachedPixels  
  ldx #$0
  ldy #$0
bankBClearCachedPixelsLoop
  lda bankBCachePixelsAccHigh,x
  sta plotAddress
  lda bankBCachePixelsAccLow,x
  sta plotAddress+1
  ldy bankBCachePixelsY,x
  lda #$00
  sta (plotAddress),y
  inx
  cpx bankBCachePixelsIndex
  bne bankBClearCachedPixelsLoop

  ; Do the last pixel too
  lda bankBCachePixelsAccHigh,x
  sta plotAddress
  lda bankBCachePixelsAccLow,x
  sta plotAddress+1
  ldy bankBCachePixelsY,x
  lda #$00
  sta (plotAddress),y
  
  jmp bankBResetCachePixels ; jmp saves an rts ;)


; CACHED DATA (256 bytes per cache, == 128 words)
bankACachePixelsAccHigh
 !word 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0
 !word 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0
 !word 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0
 !word 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0
 !word 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0
 !word 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0
 !word 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0
 !word 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0
bankACachePixelsAccLow
 !word 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0
 !word 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0
 !word 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0
 !word 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0
 !word 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0
 !word 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0
 !word 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0
 !word 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0
bankACachePixelsY
 !word 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0
 !word 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0
 !word 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0
 !word 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0
 !word 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0
 !word 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0
 !word 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0
 !word 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0


 
 ; CACHED DATA (256 bytes per cache, == 128 words)
bankBCachePixelsAccHigh
 !word 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0
 !word 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0
 !word 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0
 !word 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0
 !word 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0
 !word 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0
 !word 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0
 !word 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0
bankBCachePixelsAccLow
 !word 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0
 !word 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0
 !word 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0
 !word 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0
 !word 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0
 !word 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0
 !word 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0
 !word 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0
bankBCachePixelsY
 !word 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0
 !word 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0
 !word 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0
 !word 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0
 !word 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0
 !word 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0
 !word 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0
 !word 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0




;  #####    ##    #####   #       ######   ####
;    #     #  #   #    #  #       #       #
;    #    #    #  #####   #       #####    ####
;    #    ######  #    #  #       #            #
;    #    #    #  #    #  #       #       #    #
;    #    #    #  #####   ######  ######   ####

ytablelow
  !byte 0, 1, 2, 3, 4, 5, 6, 7, 64, 65, 66, 67, 68, 69, 70, 71, 128
  !byte 129, 130, 131, 132, 133, 134, 135, 192, 193, 194, 195, 196, 197, 198, 199, 0
  !byte 1, 2, 3, 4, 5, 6, 7, 64, 65, 66, 67, 68, 69, 70, 71, 128
  !byte 129, 130, 131, 132, 133, 134, 135, 192, 193, 194, 195, 196, 197, 198, 199, 0
  !byte 1, 2, 3, 4, 5, 6, 7, 64, 65, 66, 67, 68, 69, 70, 71, 128
  !byte 129, 130, 131, 132, 133, 134, 135, 192, 193, 194, 195, 196, 197, 198, 199, 0
  !byte 1, 2, 3, 4, 5, 6, 7, 64, 65, 66, 67, 68, 69, 70, 71, 128
  !byte 129, 130, 131, 132, 133, 134, 135, 192, 193, 194, 195, 196, 197, 198, 199, 0
  !byte 1, 2, 3, 4, 5, 6, 7, 64, 65, 66, 67, 68, 69, 70, 71, 128
  !byte 129, 130, 131, 132, 133, 134, 135, 192, 193, 194, 195, 196, 197, 198, 199, 0
  !byte 1, 2, 3, 4, 5, 6, 7, 64, 65, 66, 67, 68, 69, 70, 71, 128
  !byte 129, 130, 131, 132, 133, 134, 135, 192, 193, 194, 195, 196, 197, 198, 199, 0
  !byte 1, 2, 3, 4, 5, 6, 7, 64, 65, 66, 67, 68, 69, 70, 71, 128
  !byte 129, 130, 131, 132, 133, 134, 135, 192, 193, 194, 195, 196, 197, 198, 199, 0
  !byte 1, 2, 3, 4, 5, 6, 7, 64, 65, 66, 67, 68, 69, 70, 71, 128
  !byte 129, 130, 131, 132, 133, 134, 135, 192, 193, 194, 195, 196, 197, 198, 199
ytablehigh
  !byte 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 2
  !byte 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 5
  !byte 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 6, 7
  !byte 7, 7, 7, 7, 7, 7, 7, 8, 8, 8, 8, 8, 8, 8, 8, 10
  !byte 10, 10, 10, 10, 10, 10, 10, 11, 11, 11, 11, 11, 11, 11, 11, 12
  !byte 12, 12, 12, 12, 12, 12, 12, 13, 13, 13, 13, 13, 13, 13, 13, 15
  !byte 15, 15, 15, 15, 15, 15, 15, 16, 16, 16, 16, 16, 16, 16, 16, 17
  !byte 17, 17, 17, 17, 17, 17, 17, 18, 18, 18, 18, 18, 18, 18, 18, 20
  !byte 20, 20, 20, 20, 20, 20, 20, 21, 21, 21, 21, 21, 21, 21, 21, 22
  !byte 22, 22, 22, 22, 22, 22, 22, 23, 23, 23, 23, 23, 23, 23, 23, 25
  !byte 25, 25, 25, 25, 25, 25, 25, 26, 26, 26, 26, 26, 26, 26, 26, 27
  !byte 27, 27, 27, 27, 27, 27, 27, 28, 28, 28, 28, 28, 28, 28, 28, 30
  !byte 30, 30, 30, 30, 30, 30, 30, 255, 255, 255, 255, 255, 255, 255, 255, 255
  !byte 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255
  !byte 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255
  !byte 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255
xtable
  !byte 0, 0, 0, 0, 0, 0, 0, 0, 8, 8, 8, 8, 8, 8, 8, 8, 16
  !byte 16, 16, 16, 16, 16, 16, 16, 24, 24, 24, 24, 24, 24, 24, 24, 32
  !byte 32, 32, 32, 32, 32, 32, 32, 40, 40, 40, 40, 40, 40, 40, 40, 48
  !byte 48, 48, 48, 48, 48, 48, 48, 56, 56, 56, 56, 56, 56, 56, 56, 64
  !byte 64, 64, 64, 64, 64, 64, 64, 72, 72, 72, 72, 72, 72, 72, 72, 80
  !byte 80, 80, 80, 80, 80, 80, 80, 88, 88, 88, 88, 88, 88, 88, 88, 96
  !byte 96, 96, 96, 96, 96, 96, 96, 104, 104, 104, 104, 104, 104, 104, 104, 112
  !byte 112, 112, 112, 112, 112, 112, 112, 120, 120, 120, 120, 120, 120, 120, 120, 128
  !byte 128, 128, 128, 128, 128, 128, 128, 136, 136, 136, 136, 136, 136, 136, 136, 144
  !byte 144, 144, 144, 144, 144, 144, 144, 152, 152, 152, 152, 152, 152, 152, 152, 160
  !byte 160, 160, 160, 160, 160, 160, 160, 168, 168, 168, 168, 168, 168, 168, 168, 176
  !byte 176, 176, 176, 176, 176, 176, 176, 184, 184, 184, 184, 184, 184, 184, 184, 192
  !byte 192, 192, 192, 192, 192, 192, 192, 200, 200, 200, 200, 200, 200, 200, 200, 208
  !byte 208, 208, 208, 208, 208, 208, 208, 216, 216, 216, 216, 216, 216, 216, 216, 224
  !byte 224, 224, 224, 224, 224, 224, 224, 232, 232, 232, 232, 232, 232, 232, 232, 240
  !byte 240, 240, 240, 240, 240, 240, 240, 248, 248, 248, 248, 248, 248, 248, 248
mask
  !byte 128, 64, 32, 16, 8, 4, 2, 1, 128, 64, 32, 16, 8, 4, 2, 1, 128
  !byte 64, 32, 16, 8, 4, 2, 1, 128, 64, 32, 16, 8, 4, 2, 1, 128
  !byte 64, 32, 16, 8, 4, 2, 1, 128, 64, 32, 16, 8, 4, 2, 1, 128
  !byte 64, 32, 16, 8, 4, 2, 1, 128, 64, 32, 16, 8, 4, 2, 1, 128
  !byte 64, 32, 16, 8, 4, 2, 1, 128, 64, 32, 16, 8, 4, 2, 1, 128
  !byte 64, 32, 16, 8, 4, 2, 1, 128, 64, 32, 16, 8, 4, 2, 1, 128
  !byte 64, 32, 16, 8, 4, 2, 1, 128, 64, 32, 16, 8, 4, 2, 1, 128
  !byte 64, 32, 16, 8, 4, 2, 1, 128, 64, 32, 16, 8, 4, 2, 1, 128
  !byte 64, 32, 16, 8, 4, 2, 1, 128, 64, 32, 16, 8, 4, 2, 1, 128
  !byte 64, 32, 16, 8, 4, 2, 1, 128, 64, 32, 16, 8, 4, 2, 1, 128
  !byte 64, 32, 16, 8, 4, 2, 1, 128, 64, 32, 16, 8, 4, 2, 1, 128
  !byte 64, 32, 16, 8, 4, 2, 1, 128, 64, 32, 16, 8, 4, 2, 1, 128
  !byte 64, 32, 16, 8, 4, 2, 1, 128, 64, 32, 16, 8, 4, 2, 1, 128
  !byte 64, 32, 16, 8, 4, 2, 1, 128, 64, 32, 16, 8, 4, 2, 1, 128
  !byte 64, 32, 16, 8, 4, 2, 1, 128, 64, 32, 16, 8, 4, 2, 1, 128
  !byte 64, 32, 16, 8, 4, 2, 1, 128, 64, 32, 16, 8, 4, 2, 1
sin
  !byte 100, 102, 104, 107, 109, 112, 114, 116, 119, 121, 124, 126, 128, 131, 133, 135, 137
  !byte 140, 142, 144, 146, 148, 150, 152, 155, 157, 158, 160, 162, 164, 166, 168, 170
  !byte 171, 173, 174, 176, 178, 179, 180, 182, 183, 184, 186, 187, 188, 189, 190, 191
  !byte 192, 193, 194, 194, 195, 196, 196, 197, 197, 197, 198, 198, 198, 198, 198, 199
  !byte 198, 198, 198, 198, 198, 197, 197, 197, 196, 196, 195, 194, 194, 193, 192, 191
  !byte 190, 189, 188, 187, 186, 184, 183, 182, 180, 179, 178, 176, 174, 173, 171, 170
  !byte 168, 166, 164, 162, 160, 158, 157, 155, 152, 150, 148, 146, 144, 142, 140, 137
  !byte 135, 133, 131, 128, 126, 124, 121, 119, 116, 114, 112, 109, 107, 104, 102, 100
  !byte 97, 95, 92, 90, 87, 85, 83, 80, 78, 75, 73, 71, 68, 66, 64, 62
  !byte 59, 57, 55, 53, 51, 49, 47, 44, 42, 41, 39, 37, 35, 33, 31, 29
  !byte 28, 26, 25, 23, 21, 20, 19, 17, 16, 15, 13, 12, 11, 10, 9, 8
  !byte 7, 6, 5, 5, 4, 3, 3, 2, 2, 2, 1, 1, 1, 1, 1, 1
  !byte 1, 1, 1, 1, 1, 2, 2, 2, 3, 3, 4, 5, 5, 6, 7, 8
  !byte 9, 10, 11, 12, 13, 15, 16, 17, 19, 20, 21, 23, 25, 26, 28, 29
  !byte 31, 33, 35, 37, 39, 41, 42, 44, 47, 49, 51, 53, 55, 57, 59, 62
  !byte 64, 66, 68, 71, 73, 75, 78, 80, 83, 85, 87, 90, 92, 95, 97

; - - - - - - - - - - - - - - - - - - - - - - - - - - - 
; - - - - - - - - - - - - - - - - - - - - - - - - - - - 
; - - - - - - - - - - - - - - - - - - - - - - - - - - - 
;
;       # ####### #     #  #####  #######   ###    #####  #    #
;       # #     #  #   #  #     #    #       #    #     # #   #
;       # #     #   # #   #          #       #    #       #  #
;       # #     #    #     #####     #       #    #       ###
; #     # #     #    #          #    #       #    #       #  #
; #     # #     #    #    #     #    #       #    #     # #   #
;  #####  #######    #     #####     #      ###    #####  #    #


  
  ; Increment or decrement 
  MAXJOYSPEED=16
  MINJOYSPEED=$e0

joystickTempStoreX
  !byte 0
joystickTempStoreY
  !byte 0
  

; JOYSTICK STUFF!
getJoystick
  ; fire    + left-right steers Z axis
  ; no fire + left-right steers X axis
  ; no fire + up-down    steers Y axis
  
  ldx joystickPort
  txa
  and #$10
  bne joystickCheckYAxis

  ; fire is pressed - steer Z axis
  txa
  and #$08
  bne joystickNoZUp
  jsr joystickZUp
joystickNoZUp
  txa
  and #$04
  bne joystickNoZDown
  jsr joystickZDown
joystickNoZDown
  jmp joystickCheckYAxis

  ; Check the Y axis changes
joystickCheckYAxis
  txa
  and #$08
  bne joystickNoYUp
  jsr joystickYUp
joystickNoYUp
  txa
  and #$04
  bne joystickNoYDown
  jsr joystickYDown
joystickNoYDown


  ; Check the X axis changes
  txa
  and #$02
  bne joystickNoXUp
  jsr joystickXUp
joystickNoXUp
  txa
  and #$01
  bne joystickNoXDown
  jsr joystickXDown
joystickNoXDown

  ; all checks done
  rts

  
  

joystickZUp
  stx joystickTempStoreX
  sty joystickTempStoreY
  ldy #4
  jsr joystickDoUp
  ldx joystickTempStoreX
  ldy joystickTempStoreY
  rts
joystickZDown
  stx joystickTempStoreX
  sty joystickTempStoreY
  ldy #4
  jsr joystickDoDown
  ldx joystickTempStoreX
  ldy joystickTempStoreY
  rts

joystickYUp
  stx joystickTempStoreX
  sty joystickTempStoreY
  ldy #2
  jsr joystickDoUp
  ldx joystickTempStoreX
  ldy joystickTempStoreY
  rts
joystickYDown
  stx joystickTempStoreX
  sty joystickTempStoreY
  ldy #2
  jsr joystickDoDown
  ldx joystickTempStoreX
  ldy joystickTempStoreY
  rts

joystickXUp
  stx joystickTempStoreX
  sty joystickTempStoreY
  ldy #0
  jsr joystickDoUp
  ldx joystickTempStoreX
  ldy joystickTempStoreY
  rts
joystickXDown
  stx joystickTempStoreX
  sty joystickTempStoreY
  ldy #0
  jsr joystickDoDown
  ldx joystickTempStoreX
  ldy joystickTempStoreY
  rts
  
joystickDoUp
  ldx joyXSpeed,y
  inx
  cpx MAXJOYSPEED
  bpl storeXSpeedUp
  ldx MAXJOYSPEED
storeXSpeedUp
  txa
  sta joyXSpeed,y
  rts

joystickDoDown
  ldx joyXSpeed,y
  dex
  cpx MINJOYSPEED
  bmi storeXSpeedDown
  ldx MINJOYSPEED
storeXSpeedDown
  txa
  sta joyXSpeed,y
  rts


  

colorSweep
  ldx #20
  ldy #0
colorloop
  inc $d020
  inc $d021
  iny
  bne colorloop
  dex
  bne colorloop
  rts
  
  
  
  