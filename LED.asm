;this program toggles the blue LED on the DEVKIT-S12XE using the timer. The blue LED is connected
;to port P, pin 6 (PTP.6). When PTP.6 = 0, the blue LED is ON. When PTP.6 = 1, the blue LED is OFF. 

;include definitions 
              INCLUDE 'derivative.inc' 

ROMStart      EQU $4000                ;absolute address to place code/constant data

;code section
              ORG ROMStart
            
entry:        LDS #RAMEnd+1            ;initialise stack pointer, RAMEnd = #0x3FFF (see MC9S12XEP100.inc)

              LDAA #$FF                 
            
              STAA DDR0AD0             ;set direction registers for all ports to ouput -> 
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
              STAA DDRT                 
            
              MOVB #$BF,PTP            ;PTP = 1011 1111, i.e only blue LED (PTP.6) is on 
                        
              MOVB #$07,TIM_TSCR2      ;TSCR2 = A = 0000 0111, no interrupt, prescale factor = 128 
            
              MOVB #$80,TIM_TSCR1      ;TSCR1 = A = 1000 0000, timer enabled
                        
over:         BSET TIM_TFLG2,%10000000 ;set TIM_TFLG2.7 = TOF to clear the timer overflow flag (TOF) 
h1:           BRCLR TIM_TFLG2,%10000000,h1
                                       ;branch to h1 if TOF = 0, i.e loop until TOF = 1 (i.e until timer overflows)

              LDAA #$40                ;accumulator A = 0100 0000
              EORA PTP                 ;XOR (logical exlusive or) A and port P and store the result in A 
              STAA PTP                 ;toggle PTP.6
              BRA over                 ;jump to over to repeat

;**************************************************************
;*                 Interrupt Vectors                          *
;**************************************************************
              ORG $FFFE
              DC.W entry               ;reset Vector
