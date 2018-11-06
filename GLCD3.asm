;this program displays the status line, stack level labels, menu and cursor on the ST7565 GLCD using SPI and the 
;Real Time Interrupt

;Danny Milutinovic 2018
              
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
              
              MOVB #$40,RTICTL                   ;initialise the Real Time Interrupt (RTI) with 
                                                 ;clock divided by 2^13                                                 
              
              JSR PORTinit                       ;initialise all ports
              
              JSR SPIinit                        ;initialise SPI1
              
              JSR DISPinit                       ;initialise the ST7565 GLCD
              
              JSR clrScreen                      ;clear the display
              
              JSR dispStatL                      ;display the status line on page 0 of the ST7565 GLCD
              
              JSR dispStack                      ;display the 5 stack level labels on pages 1 - 5
              
              JSR dispMenu                       ;display the menu on page 7 
              
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

clearPage:    CLRA
              
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
              
              CLR Ycoord
              JSR setY                           ;sets Y = 0 and A0 = 1
              					    
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
              
;*****************Status line annunicator table****************

Approx        DC.B $00, $24, $48, $24, $12, $24               ;~

Deci          DC.B $00, $7F, $41, $41, $22, $1C               ;D
              DC.B $00, $7F, $49, $49, $49, $41               ;E
              DC.B $00, $3E, $41, $41, $41, $22               ;C
              DC.B $00, $00, $36, $36, $00, $00               ;:
              
Deg           DC.B $00, $7F, $41, $41, $22, $1C               ;D
              DC.B $00, $7F, $49, $49, $49, $41               ;E
              DC.B $00, $3E, $41, $49, $49, $7A               ;G
              
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

;RTIisr: blinks the cursor;;;;;;;;;;;;;;;;;;;;;;;;

RTIisr:       BSET CRGFLG,#BIT7                  ;clear RTI Flag      

              LDD cursorRate
              SUBD #$0001
              STD cursorRate                     
              BNE RTIisr_2                       ;exit if cursorRate is <> 0
                                                 ;otherwise toggle the cursor               
              
              COM cursorValue                    ;complement colValue
					    LDAA cursorValue 					    
					    MOVB #$08,colNumber                ;colNumber = 8 pixel font width		    
					     
RTIisr_1:     JSR sendByte                       ;set/clear the cursor column (does not affect Ycoord)

              DEC colNumber           
              BNE RTIisr_1                        
					    
					    JSR setY                           ;reset Y to Ycoord
					    
					    MOVW #$0190,cursorRate             ;reset the blink rate

RTIisr_2:     RTI
              
;************************************************
;*              Interrupt Vectors               *
;************************************************

              ORG $FFF0
              DC.W RTIisr                        ;RTI Vector
              
              ORG $FFFE
              DC.W entry                         ;Reset Vector
              
              
 




