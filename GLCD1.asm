;this program displays the status line on the ST7565 GLCD using simulated SPI

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
              
colNumber     DS.B 1                             ;the number of data bytes to be sent to the ST 7565 GLCD
counter       DS.B 1                             ;used to set the number of times a loop is executed
pageNumber    DS.B 1                             ;the ST 7565 GLCD page number
SPICounter    DS.B 1                             ;tracks number of bits sent to ST 7565 GLCD
Ycoord        DS.B 1                             ;the value of the current Y coordinate on the screen is stored  
                                                 ;as the ST7565 GLCD cannot be read

;*************************************************
;*           Code/Constant Data Section          *
;*************************************************

              ORG ROMstart
              
;*******************Main Program****************** 
            
entry:        LDS #RAMEnd+1                      ;initialise stack pointer, RAMEnd = #0x3FFF (see MC9S12XEP100.inc)

              MOVB #$01,TIM_TSCR2                ;timer set with no interrupt & prescale factor = 2
              
              JSR PORTinit                       ;initialise all ports
              
              JSR DISPinit                       ;initialise the ST7565 GLCD
              
              JSR clrScreen                      ;clear the display
              
              JSR dispStatL                      ;display the annunciators
              
loopforever:  BRA loopforever 

;*************************************************
;*                  Subroutines                  *
;************************************************* 

;clearPage: clear the current page on the ST7565 GLCD

clearPage:    CLC
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

;DISPinit: initialise the ST7565 GLCDdisplay;;;;;;

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
              
dispStatL_1:  LDAA #$80
              JSR sendByte
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

;PORTinit: all ports are set to output mode;;;;;;;

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
              STAA DDRP                 
              STAA DDRR
              STAA DDRS                 
              STAA DDRT                          ;<-
              
              RTS
              
;sendByte: send a byte (command or data stored in A) to the ST7565 GLCD using bit banging 

sendByte:     MOVB #$08,SPICounter
              
sendByte_1:   BCLR PTH,#BIT1                      ;SID = 0
              
              ROLA
              
              BCC sendByte_2
              
              BSET PTH,#BIT1                      ;SID = 1                                
              
sendByte_2:   BCLR PTH,#BIT2                      ;SCLK low
              BSET PTH,#BIT2                      ;SCLK high -> voltage on PTH.1 sent to ST 7565 GLCD
              
              DEC SPICounter
              BNE sendByte_1                      ;send 8 bits              
              
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
              
;*****************Status line annunicator table****************

Approx        DC.B $00, $24, $48, $24, $12, $24               ;~

Deci          DC.B $00, $7F, $41, $41, $22, $1C               ;D
              DC.B $00, $7F, $49, $49, $49, $41               ;E
              DC.B $00, $3E, $41, $41, $41, $22               ;C
              DC.B $00, $00, $36, $36, $00, $00               ;:
              
Deg           DC.B $00, $7F, $41, $41, $22, $1C               ;D
              DC.B $00, $7F, $49, $49, $49, $41               ;E
              DC.B $00, $3E, $41, $49, $49, $7A               ;G
              
;*************************************************
;*              Interrupt Vectors                *
;*************************************************

              ORG $FFFE
              DC.W entry                         ;reset Vector
              
              
 


