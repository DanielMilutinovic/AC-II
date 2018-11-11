;this program scans a 4x4 matrix keypad and displays the number of the key pressed on the command line

;Daniel Milutinovic 2018 
                           
;include definitions 
              INCLUDE 'derivative.inc'
              
;************************************************* 
;*                  Definitions                  *
;*************************************************

BIT0          EQU %00000001
BIT1          EQU %00000010
BIT2          EQU %00000100
BIT3          EQU %00001000
BIT4          EQU %00010000
BIT5          EQU %00100000
BIT6          EQU %01000000
BIT7          EQU %10000000

VARstart      EQU $2000
ROMstart      EQU $4000                          ;absolute address to place code/constant data

;*************************************************
;*            Variable Data Section              *
;*************************************************

              ORG VARstart 
              
colNumber     DS.B 1                             ;the number of data bytes to be sent to the ST7565 GLCD
cursorValue   DS.B 1                             ;either #$00 or #$FF - used to turn the cursor off/on
cursorRate    DS.W 1                             ;used to determine the rate at which the cursor blinks
counter       DS.B 1                             ;used to set the number of times a loop is executed

flag1         DS.B 1                             ;flag1.0 = shift - set when shift key pressed
                                                 ;flag1.1 = keyFound - set when the pressed key has been found
                                                 ;flag1.2 = keyPressed - set when a key has been pressed
                                                 ;flag1.3 = sci - set when "DEC" is pressed and held
                                                 ;flag1.4 = MEM - set/cleared when "MEM" pressed
                                                 ;flag1.5 = sub - set when "-" pressed
                                                 ;flag1.6 = grad - set when gradian mode is selected
                                                 ;flag1.7 = curOff - set to turn the cursor off when displaying 
                                                 ;memory contents/time and in program edit mode

keyNumber     DS.B 1                             ;the keypad number of the key pressed
pageNumber    DS.B 1                             ;the ST7565 GLCD page number
SPICounter    DS.B 1                             ;tracks number of bits sent to ST7565 GLCD
Ycoord        DS.B 1                             ;the value of the current Y coordinate on the screen is stored  
                                                 ;as the ST7565 GLCD cannot be read

;*************************************************
;*           Code/Constant Data Section          *
;*************************************************

              ORG ROMstart
              
;*******************Main Program****************** 
            
entry:        LDS #RAMEnd+1                      ;initialise stack pointer, RAMEnd = #0x3FFF (see MC9S12XEP100.inc)

              MOVB #$01,TIM_TSCR2                ;timer set with no interrupt & prescale factor = 2
              
              MOVB #$40,RTICTL                   ;initialise the Real Time Interrupt (RTI) for keypad scanning with 
                                                 ;clock divided by 2^13 
              
              JSR PORTinit                       ;initialise all ports
              
              JSR SPIinit                        ;initialise SPI1
              
              JSR DISPinit                       ;initialise the ST7565 GLCD
              
              JSR clrScreen                      ;clear the display
              
              JSR dispStatL                      ;display the status line on page 0 of the ST7565 GLCD
              
              JSR dispStack                      ;display the 5 stack level labels on pages 1 - 5
              
              JSR dispMenu                       ;display the menu on page 7 
              
              JSR VARinit                        ;initialise variables
              
              LDAA #$B6

              BCLR PTH,#BIT0                     ;A0 = 0,send command

              JSR sendByte                       ;set to page 6
              
              MOVB #$08,Ycoord                    
              JSR setY                           ;set Ycoord and Y to 8, and A0 = 1 (send data)                          
              
              MOVW #$0001,cursorRate             			    
					    CLR cursorValue                    ;cursor immediately visible when calculator turned on
                       
              CLI                                ;unmask interrupts
              
              BSET CRGINT,#BIT7                  ;enable RTI
              
loopforever:  BRA loopforever                    ;wait for RTI 

;*************************************************
;*                  Subroutines                  *
;************************************************* 

;clearPage: clear the current page on the ST7565 GLCD

clearPage:    CLR Ycoord
              JSR setY                           ;sets Y = 0 and A0 = 1    

              CLRA
              
              MOVB #$80,colNumber                ;colNumber = 128,index for number of columns cleared 
					    
clearPage_1:  JSR sendByte                       ;blank the column

              DEC colNumber                     
              BNE clearPage_1   
              
              RTS

;clrScreen: clears the display;;;;;;;;;;;;;;;;;;;;

clrScreen:    MOVB #$08,counter   

              MOVB #$B7,pageNumber

clrScreen_1:  BCLR PTH,#BIT0                     ;A0 = 0,send command

              LDAA pageNumber                     
              JSR sendByte                       ;set the page
              
              JSR clearPage                      ;clear the page
			        
			        DEC pageNumber
			        DEC counter
			        BNE clrScreen_1

              RTS

;delay: cause a timer delay;;;;;;;;;;;;;;;;;;;;;;;

delay:        MOVB #$80,TIM_TSCR1                ;TIM_TSCR1 = 1000 0000,timer enabled 
            
delay_1:      BRCLR TIM_TFLG2,#BIT7,delay_1      ;branch to delay_1 if TOF = 0, i.e loop until TOF = 1 
                                                 ;(i.e until timer overflows)
              
              MOVB #BIT7,TIM_TFLG2               ;set TIM_TFLG2.7 = TOF to clear the timer overflow flag (TOF) 
            
              CLR TIM_TSCR1                      ;TIM_TSCR1 = 0000 0000,timer disabled 
              
              RTS

;DISPinit: initialise the ST7565 GLCD;;;;;;;;;;;;;

DISPinit:     BCLR PORTA,#BIT0                   ;RST = 0
                          
              JSR delay                          ;delay 
            
              BSET PORTA,#BIT0                   ;RST = 1
              
              BCLR PTH,#BIT0                     ;A0 = 0, send command            
                          
              LDAA #$A3
            
              JSR sendByte                       ;set bias to 1/7                 
            
              LDAA #$A1                   
            
              JSR sendByte                       ;set display to reverse (complements COM output scan direction)
            
              LDAA #$C0                   
            
              JSR sendByte                       ;set COM output scan direction to normal
            
              LDAA #$A6                   
            
              JSR sendByte                       ;set display to normal (i.e. not reverse)
            
              LDAA #$60                   
            
              JSR sendByte                       ;set RAM display start line to 32 (01 100000)
              
              LDAA #$2C                   
            
              JSR sendByte               
            
              JSR delay                          ;delay
            
              LDAA #$2F                   
            
              JSR sendByte               
            
              JSR delay                          ;delay
            
              LDAA #$AF                   
            
              JSR sendByte                       ;display on
            
              LDAA #$A4                   
            
              JSR sendByte                       ;normal display (i.e. not all pixels on)     

              RTS


;dispKeyNo: displays the number of the key pressed

dispKeyNo:    BCLR PTH,#BIT0                     ;A0 = 0,send command

              LDAA #$B6                     
              JSR sendByte                       ;set the page
              
              JSR clearPage                      ;clear the command line       

              MOVB #$10,Ycoord
              JSR setY                           ;set Y = 16
              
              CLR counter
              LDAA keyNumber
               
dispKeyNo_1   SUBA #$0A                          ;subtract 10
              BCS dispKeyNo_2                    ;result < 0 -> exit loop and add 10
              INC counter                        ;otherwise increment counter and repeat
              BRA dispKeyNo_1 
              
dispKeyNo_2:  ADDA #$0A                          ;after adding 10 A is equal to the 1's place digit
              PSHA
              
              LDAA counter                       
              LDAB #$08
              MUL
              
              LDX #Digit
              ABX
              
              MOVB #$08,colNumber
              
              JSR sendData                       ;display the 10's place digit
              
              PULA
              
              LDAB #$08
              MUL
              
              LDX #Digit
              ABX
              
              MOVB #$08,colNumber
              
              JSR sendData                       ;display the 1's place digit
              
              RTS

;dispMenu: displays the menu on page 0;;;;;;;;;;;;

dispMenu:     BCLR PTH,#BIT0                     ;A0 = 0,send command
              
              LDAA #$B7
              JSR sendByte                       ;set the page
              
              CLR Ycoord
              JSR setY                           ;Y = 0      

              MOVB #$80,colNumber

              LDX #Menu
              
              JSR sendDataO
              
              RTS

;dispStack: displays the 5 stack level labels on pages 2 to 6

dispStack:    MOVB #$05,counter   
                            
              MOVB #$B1,pageNumber               ;start at level 5 (page 1)

dispStack_1:  BCLR PTH,#BIT0                     ;A0 = 0,send command
              
              LDAA pageNumber
              JSR sendByte                       ;set the page
              
              CLR Ycoord
              JSR setY                           ;Y = 0 
              
              LDAA counter
              DECA
              LDAB #$08
              MUL 
              
              LDX #stackLevels
              ABX
              
              MOVB #$08,colNumber
              
              JSR sendData                       ;display the label 
              
              INC pageNumber                     ;go to the next page                   
              
              DEC counter             
              BNE dispStack_1 

              RTS

;dispStatL: displays the status line annunciators; 

dispStatL:    CLR Ycoord
              JSR setY
              
              LDX #Deci
			        
			        MOVB #$12,colNumber
                             
              LDAA #$80                          ;calculate remaining width of screen
              SUBA colNumber
              PSHA

              JSR sendDataU                      ;display underlined mode annunciator 
              
              PULA                               ;display remaining portion of line ->
              STAA colNumber
              
              LDAA #$80              
              
dispStatL_1:  JSR sendByte
              DEC colNumber
              BNE dispStatL_1                    ;<-
              
              MOVB #$50,Ycoord
              JSR setY                           ;set Y coordinate to 80
              
              LDX #Deg
                            
              MOVB #$12,colNumber
              
              JSR sendDataU                      ;display underlined angle mode annunciator 
              
              MOVB #$7A,Ycoord
              JSR setY                           ;set Y coordinate to 122 
              
              LDX #Approx
              
              MOVB #$06,colNumber             
              
              JSR sendDataU                      ;display underlined number mode annunciator
              
dispStatL_exit:
              RTS

;Key 0: ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

key0:         JSR dispKeyNo

              JMP keyHdlr_1                      ;return to scanKey subroutine   

;Key 1: ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

key1:         JSR dispKeyNo

              JMP keyHdlr_1                      ;return to scanKey subroutine  

;Key 2: ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

key2:         JSR dispKeyNo

              JMP keyHdlr_1                      ;return to scanKey subroutine 

;Key 3: ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

key3:         JSR dispKeyNo

              JMP keyHdlr_1                      ;return to scanKey subroutine  

;Key 4: ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

key4:         JSR dispKeyNo

              JMP keyHdlr_1                      ;return to scanKey subroutine  

;Key 5: ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

key5:         JSR dispKeyNo

              JMP keyHdlr_1                      ;return to scanKey subroutine 

;Key 6: ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

key6:         JSR dispKeyNo

              JMP keyHdlr_1                      ;return to scanKey subroutine 

;Key 7: ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

key7:         JSR dispKeyNo

              JMP keyHdlr_1                      ;return to scanKey subroutine  

;Key 8: ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

key8:         JSR dispKeyNo

              JMP keyHdlr_1                      ;return to scanKey subroutine 

;Key 9: ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

key9:         JSR dispKeyNo

              JMP keyHdlr_1                      ;return to scanKey subroutine 

;Key 10: ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

key10:        JSR dispKeyNo

              JMP keyHdlr_1                      ;return to scanKey subroutine

;Key 11: ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

key11:        JSR dispKeyNo

              JMP keyHdlr_1                      ;return to scanKey subroutine 

;Key 12: ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

key12:        JSR dispKeyNo

              JMP keyHdlr_1                      ;return to scanKey subroutine 

;Key 13: ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

key13:        JSR dispKeyNo

              JMP keyHdlr_1                      ;return to scanKey subroutine

;Key 14: ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

key14:        JSR dispKeyNo

              JMP keyHdlr_1                      ;return to scanKey subroutine 

;Key 15: ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

key15:        JSR dispKeyNo

              JMP keyHdlr_1                      ;return to scanKey subroutine 

;keyHdlr: computed GOTO table;;;;;;;;;;;;;;;;;;;;;
              
keyHdlr:      JMP [D,X]
              
keyTable      DC.W key0                          ;key0 = 0
              DC.W key1 
              DC.W key2
              DC.W key3 
              DC.W key4
              DC.W key5 
              DC.W key6
              DC.W key7 
              DC.W key8
              DC.W key9 
              DC.W key10
              DC.W key11
              DC.W key12
              DC.W key13
              DC.W key14
              DC.W key15                         ;key15 = 30
              
keyHdlr_1:    RTS              

;PORTinit: all ports are set to output mode, except Port P

PORTinit:     LDAA #$FF                 
            
              STAA DDR0AD0                       ;set direction registers for all ports to ouput -> 
              STAA DDR1AD0              
            
              STAA DDR0AD1              
              STAA DDR1AD1              
            
              STAA DDRA                 
              STAA DDRB                 
              STAA DDRC                 
              STAA DDRD                 
              STAA DDRE
              STAA DDRF
              STAA DDRH            
              STAA DDRJ                 
              STAA DDRK
              STAA DDRL
              STAA DDRM                 
              STAA DDRR
              STAA DDRS                 
              STAA DDRT                          ;<-
              
              STAA PERP                          ;pull-up resistors enabled on Port P
              
              RTS
              
;scanCol: scans the column to determine which key has been pressed                                                           

scanCol:      BRCLR PTP,#BIT5,scanCol_1          ;branch if PTP.5 is clear
                                                 ;i.e. if voltage = 0 (meaning key has been pressed)    
              INC keyNumber                      ;otherwise key has not been pressed so increment keyNumber
              BRCLR PTP,#BIT4,scanCol_1
                
              INC keyNumber
              BRCLR PTP,#BIT3,scanCol_1
                
              INC keyNumber
              BRCLR PTP,#BIT2,scanCol_1
			  
			        INC keyNumber
                            
              BRA scanCol_2                      ;no key in this row has been pressed so exit and scan the next row              

scanCol_1:    BSET flag1,#BIT1                   ;set keyFound

scanCol_2:    RTS

;scanKey: scans the keypad to determine which key has been pressed and displays its key number

scanKey:      MOVB #$02,counter

scanKey_1:    JSR delay                          ;timer delay to debounce the key
              DEC counter
              BNE scanKey_1
              
              CLR keyNumber
              
              BSET PT1AD0,#BIT1                  ;PT1AD0.0 = 0 -> row A scanned
              BSET PT1AD0,#BIT2
              BSET PT1AD0,#BIT3
                                  
              JSR scanCol                        ;scan all columns of row A
                                          
              BRSET flag1,#BIT1,scanKey_2        ;branch if keyFound set, 
                                                 ;i.e. if pressed key has been found
                                                  
              BSET PT1AD0,#BIT0                  ;PT1AD0.1 = 0 -> row B scanned
              BCLR PT1AD0,#BIT1  

              JSR scanCol                        

              BRSET flag1,#BIT1,scanKey_2

              BSET PT1AD0,#BIT1                  ;PT1AD0.2 = 0 -> row C scanned
              BCLR PT1AD0,#BIT2

              JSR scanCol                       

              BRSET flag1,#BIT1,scanKey_2
              
              BSET PT1AD0,#BIT2                  ;PT1AD0.3 = 0 -> row D scanned
              BCLR PT1AD0,#BIT3               

              JSR scanCol
                           
              BRSET flag1,#BIT1,scanKey_2
              
              BRA scanKey_3                      ;exit if pressed key not found
              
scanKey_2:    BSET flag1,#BIT2                   ;set keyPressed flag as pressed key has been found       
               
              CLRA                                            

              LDAB keyNumber                          
              
              LDX #keyTable                      ;X = keyTable                  

              ADDB keyNumber    

              JSR keyHdlr                        
              
              BCLR flag1,#BIT1                   ;clear keyFound              

scanKey_3:    RTS

;sendByte: send a byte (command or data stored in A) to the ST7565 GLCD via the SPI1

sendByte:     BRCLR SPI1SR,#BIT5,sendByte        ;if SPI1SR.5 (SPTEF) = 0 then branch to sendByte
                                                 ;i.e. loop until SPTEF = 1 (SPTEF = 1 -> SPIDR empty)                                      
            
              STAA SPI1DRL                       ;initiate data transfer and clear SPTEF
            
sendByte_1:   BRCLR SPI1SR,#BIT7,sendByte_1      ;if SPI1SR.7 (SPIF) = 0 then branch to sendByte_1
                                                 ;i.e. loop until SPIF = 1 (SPIF = 1 -> new data copied to SPIDR)
                                         
              LDAB SPI1DRL                       ;read SPI1DRL to B to clear SPIF
              
              RTS

;sendData: sends colNumber data bytes to the ST7565 GLCD and increments Ycoord
              
sendData:     LDAA 1,X+                          ;load A with the byte at the location pointed to by X,
                                                 ;then increment X by 1
              JSR sendByte

              INC Ycoord                           
                
              DEC colNumber
              BNE sendData 
								
              RTS 

;sendDataU: sends colNumber data bytes to the ST7565 GLCD after setting the bit corresponding to the bottom pixel
;and increments Ycoord
              
sendDataU:    LDAA 1,X+                          ;load A with the byte at the location pointed to by X,
                                                 ;then increment X by 1
              ORAA #$80                          ;pixel added at bottom of character to form line
              JSR sendByte

              INC Ycoord                           
                
              DEC colNumber
              BNE sendDataU 
								
              RTS 

;sendDataO: sends colNumber data bytes to the ST7565 GLCD after setting the bit corresponding to the top pixel
;and increments Ycoord

sendDataO:    LDAA 1,X+                          ;load A with the byte at the location pointed to by X,
                                                 ;then increment X by 1
              ORAA #$01                          ;pixel added at top of character to form line
              JSR sendByte

              INC Ycoord                           
                
              DEC colNumber
              BNE sendDataO 
								
              RTS 

;setY: sets the Y coordinate on the ST7565 GLCD to the value in Ycoord and sets A0 = 1 to send data 

setY:         BCLR PTH,#BIT0                     ;A0 = 0,send command
                                
              LDAA Ycoord
              
              ADDA #$03
              
              ASRA
              ASRA
              ASRA
              ASRA
              ANDA #$0F
              ADDA #$10   
              
					    JSR sendByte                       ;set MSN of column address					      
					      
					    LDAA Ycoord
					    
					    ADDA #$03
					    
              ANDA #$0F
                
              JSR sendByte                       ;set LSN of column address					      
					      
					    BSET PTH,#BIT0                     ;A0 = 1,send data
					      					      
					    RTS
					   
;SPIinit: initialise SPI1 to send commands/data to the ST7565 GLCD    
            
SPIinit:      MOVB #$20,MODRR                    ;re-route SPI1 to Port H[3:0]      

              MOVB #$02,SPI1BR                   ;divide by 8 to determine SPI1 clock rate            
              
              MOVB #$50,SPI1CR1                  ;SPI1 enabled, master mode, CPOL = 0, CPHA = 0
                                                 
              MOVB #$09,SPI1CR2	                 ;output buffer enabled, SPC1 set to enable bidirectional operation 
                                                 ;so that PH0 can be used for A0                                                 
              RTS
              
;VARinit: initialise variables;;;;;;;;;;;;;;;;;;;;

VARinit:      CLRA
             
              STAA flag1
              
              RTS
              
;*****************Status line annunicator table****************

Approx        DC.B $00, $24, $48, $24, $12, $24               ;~

Deci          DC.B $00, $7F, $41, $41, $22, $1C               ;D
              DC.B $00, $7F, $49, $49, $49, $41               ;E
              DC.B $00, $3E, $41, $41, $41, $22               ;C
              DC.B $00, $00, $36, $36, $00, $00               ;:
              
Deg           DC.B $00, $7F, $41, $41, $22, $1C               ;D
              DC.B $00, $7F, $49, $49, $49, $41               ;E
              DC.B $00, $3E, $41, $49, $49, $7A               ;G
              
Digit         DC.B $3E, $7F, $71, $59, $4D, $7F, $3E, $00	    ;0
              DC.B $00, $02, $7F, $7F, $00, $00, $00, $00	    ;1
              DC.B $62, $73, $59, $49, $4F, $46, $00, $00	    ;2
              DC.B $22, $63, $49, $49, $7F, $36, $00, $00	    ;3
              DC.B $18, $1C, $16, $13, $7F, $7F, $10, $00	    ;4
              DC.B $27, $67, $45, $45, $7D, $39, $00, $00	    ;5
              DC.B $3C, $7E, $4B, $49, $79, $30, $00, $00	    ;6
              DC.B $01, $01, $71, $79, $0F, $07, $00, $00	    ;7
              DC.B $36, $7F, $49, $49, $7F, $36, $00, $00     ;8
              DC.B $06, $4F, $49, $69, $3F, $1E, $00, $00	    ;9
              DC.B $00, $7C, $7E, $13, $13, $7E, $7C, $00     ;A
              DC.B $00, $7F, $7F, $49, $49, $7F, $36, $00	    ;B
              DC.B $1C, $3E, $63, $41, $41, $63, $22, $00     ;C
              DC.B $00, $7F, $7F, $41, $63, $3E, $1C, $00	    ;D
              DC.B $00, $7F, $7F, $49, $49, $41, $41, $00	    ;E
              DC.B $00, $7F, $7F, $09, $09, $01, $01, $00	    ;F 

Menu          DC.B $00                                        ;21 columns 
              DC.B $00, $00, $00, $00, $00, $00               ;     
              DC.B $00, $00, $00, $00, $00, $00               ;
              DC.B $00, $00, $00, $00, $00, $00               ;
              DC.B $00, $FF                                   ;line
              
              DC.B $00                                        ;21 columns 
              DC.B $00, $00, $00, $00, $00, $00               ;     
              DC.B $00, $00, $00, $00, $00, $00               ;
              DC.B $00, $00, $00, $00, $00, $00               ;
              DC.B $00, $FF                                   ;line
              
              DC.B $00                                        ;21 columns 
              DC.B $00, $00, $00, $00, $00, $00               ;     
              DC.B $00, $00, $00, $00, $00, $00               ;
              DC.B $00, $00, $00, $00, $00, $00               ;
              DC.B $00, $FF                                   ;line
              
              DC.B $00                                        ;21 columns 
              DC.B $00, $00, $00, $00, $00, $00               ;     
              DC.B $00, $00, $00, $00, $00, $00               ;
              DC.B $00, $00, $00, $00, $00, $00               ;
              DC.B $00, $FF                                   ;line
              
              DC.B $00                                        ;21 columns 
              DC.B $00, $00, $00, $00, $00, $00               ;     
              DC.B $00, $00, $00, $00, $00, $00               ;
              DC.B $00, $00, $00, $00, $00, $00               ;
              DC.B $00, $FF                                   ;line
              
              DC.B $00, $00, $00, $00, $00                    ;23 columns
              DC.B $00, $00, $00, $00, $00, $00               ;     
              DC.B $00, $00, $00, $00, $00, $00               ;
              DC.B $00, $00, $00, $00, $00, $00               ;
              
stackLevels   DC.B $FF, $FF, $FD, $80, $80, $FF, $FF, $FF     ;1 inverted 
              DC.B $FF, $9D, $8C, $A6, $B6, $B0, $B9, $FF     ;2 inverted
              DC.B $FF, $DD, $9C, $B6, $B6, $80, $C9, $FF     ;3 inverted
              DC.B $FF, $E7, $E3, $E9, $EC, $80, $80, $EF     ;4 inverted
              DC.B $FF, $D8, $98, $BA, $BA, $82, $C6, $FF	    ;5 inverted 
              
;************************************************
;*                 Interrupts                   *
;************************************************

;RTIisr: blinks the cursor and determines if a key has been pressed 

RTIisr:       BSET CRGFLG,#BIT7                  ;clear RTI Flag      

              LDD cursorRate
              SUBD #$0001
              STD cursorRate                     ;copy the value in D to cursorRate
              BNE RTIisr_2                       ;branch if cursorRate is <> 0
                                                 ;otherwise toggle the cursor               
              
              COM cursorValue                    ;complement colValue
					    LDAA cursorValue 					    
					    MOVB #$08,colNumber                ;colNumber = 8 pixel font width		    
					     
RTIisr_1:     JSR sendByte                       ;set/clear the cursor column (does not affect Ycoord)

              DEC colNumber           
              BNE RTIisr_1                        
					    
					    JSR setY                           ;reset Y to Ycoord
					    
					    MOVW #$0190,cursorRate             ;reset the blink rate

RTIisr_2:     BCLR PT1AD0,#BIT0                  ;clear all keypad rows
              BCLR PT1AD0,#BIT1
              BCLR PT1AD0,#BIT2
              BCLR PT1AD0,#BIT3
                            
              LDAA PTP
              COMA
                            
              BEQ RTIisr_4                       ;if keypad not pressed (Port P = $FF) exit                                            
              
              JSR scanKey                        ;otherwise scan keypad                      
             
              BRCLR flag1,#BIT2,RTIisr_4         ;exit if pressed key was not detected    

              BCLR PT1AD0,#BIT0                  ;otherwise clear all keypad rows
              BCLR PT1AD0,#BIT1
              BCLR PT1AD0,#BIT2
              BCLR PT1AD0,#BIT3
             
RTIisr_3:     LDAA PTP
              COMA
              BNE RTIisr_3                       ;loop until pressed key is released               
                           
              BCLR flag1,#BIT2                   ;clear keyPressed       

RTIisr_4:     RTI
              
;************************************************
;*              Interrupt Vectors               *
;************************************************

              ORG $FFF0
              DC.W RTIisr                        ;RTI Vector
              
              ORG $FFFE
              DC.W entry                         ;Reset Vector
              
              
 




