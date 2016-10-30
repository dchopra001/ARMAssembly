; ECE-222 Lab ... Winter 2013 term 
; Lab 3 sample code 
				THUMB 		; Thumb instruction set 
                AREA 		My_code, CODE, READONLY
                EXPORT 		__MAIN
				ENTRY  
__MAIN

; The following lines are similar to Lab-1 but use a defined address to make it easier.
; They just turn off all LEDs 
				LDR			R10, =LED_BASE_ADR		; R10 is a permenant pointer to the base address for the LEDs, offset of 0x20 and 0x40 for the ports

				MOV 		R3, #0xB0000000		; Turn off three LEDs on port 1  
				STR 		R3, [r10, #0x20]
				MOV 		R3, #0x0000007C
				STR 		R3, [R10, #0x40] 	; Turn off five LEDs on port 2 

				


; This line is very important in your main program
; Initializes R11 to a 16-bit non-zero value and NOTHING else can write to R11 !!
				MOV			R11, #0xABCD		; Init the random number generator with a non-zero number
loop 			BL 			RandomNum 
				;B 			loop
				
;This is the beginning of the reflex meter project				
REFLEXMETER		STMFD		R13!,{R2,R3,R5,R6,R7,R12,R14}
				LDR			R12, =FIO2PIN		;load the address from which we read the INT0 button's status
				
RANDOM_COUNT_DELAY 	
				TEQ			R11,#0				;RANDOM_COUNT_DELAY causes a delay of the random value generated in "0.1 ms" units
				BEQ			LED_ON					;once the delay has been run for all of random num's length, we skip to next step in program	
				BL			DELAY
				SUB			R11,#1
				B			RANDOM_COUNT_DELAY
									
LED_ON			MOV			R2,#0xD0000000 		;this is where we come once we are done random count function (LED 29 is turned on in this section)
				STR			R2,[R10,#0X20]
				
				
				MOV			R3,#0				;Set count value to zero

INCR_POLL		BL			DELAY				;This section creates a delay, adds 1 to the counter, and then checks if the INT0 button has been pressed
				ADD			R3,R3,#1				;(this is how we measure the user's reaction in 0.1 milliseconds).. If the button is not pressed
													;then we loop back and increase the count again and cause a delay. 

				LDR			R4,[R12]				;(here we are loading the contents of the memory address that has the status of int0 button)
				AND			R4,R4,#0x00000400		;if 10th bit is pressed(or if p2.10=int0 button is pressed), anding with 400 will give 0, else it will give 400 again 
				TEQ			R4,#0					;check to see if anding gave 0, if yes then dont execute next instruction and proceed to next segment of program
				BNE			INCR_POLL
				
				MOV			R5,#0x00000000			;here a new count value is being set to 0
				MOV			R6,#0x00000000			;before moving an important value in R6, we clear it
				MOV			R6,R3					;Store R3 in R6... this is because we will modify R3, which is basically the bit pattern we have to display.
														;once we have displayed all the bits, we will restore R3's original value back into R3 using R6. Then 
														;we can display the whole pattern again! Hence this allows us to create an endless loop
				
DISPLAY_ALL		BL			DISPLAYBITS				;First display the first 8 bits of the value in R3
				LSR			R3,#8					;Right shift by 8 to get access to the next 8 bits
				BL			twosecdelay				;Cause a 2 second delay between displays
				ADD			R5,R5,#1				;If this count reaches 4, we have displayed all 4 pattern sets in R3 
				TEQ			R5,#4
				BEQ			fivesecdelay			;Once all 4 have been displayed, cause a 5 second delay
				BNE			DISPLAY_ALL				;If all 4 not displayed, delay the next set of bits
				
fivesecdelay	MOV			R3,#0x00000000			;First clear R3-->So we can store the original pattern in here again
				MOV			R5,#0x00000000			;clear R5-->So when we display again we can count to 4 again

				MOV			R3,R6					;Store original pattern back in R3
				MOV			R7,#0x00006B6C			;Calling the delay this many times causes a 5 second delay roughly (Mathematically it is C350 but that doesnt work so well practically)
				LSL			R7,#1
fivecheck		BL			DELAY
				SUBS		R7,#1
				BNE			fivecheck
				
				BEQ			DISPLAY_ALL				;Once delay has been called enough times, go back and display everything again
				

				
				LDMFD		R13!,{R2,R3,R5,R6,R7,R12,R15}
				
twosecdelay		STMFD		R13!,{R7,R14}

				MOV			R7,#0x00004E20			;Calling the delay this many times causes a 2 second delay roughly
mini			BL			DELAY
				SUBS		R7,#1
				BNE			mini
				
				LDMFD		R13!,{R7,R15}
				
				
; 

;-------------DISPLAYBITS METHOD-----------------------------------------
DISPLAYBITS		
				STMFD		R13!,{R1,R2,R7,R8,R9,R14}
				AND			R1,R3,#0x000000FF	;ANDing with this will give us access to the Least Significant 8 bits in R3, and store the result in R1
				EOR			R1,#0x000000FF		;this operation will flip first 8 bits... this is done because we want to represent bit"1" as ON and bit "0" as off... without this we would get the oppoiste result
				LSL			R1,#24				;Left Shift to position 31,30,29,28,27,26,25,24
				
				;--------------NOW LETS FIRST AFFECT PORT 1 LEDs--------------------
				RBIT		R2,R1				;Reverse order of bits and store in R2.
												;the top 3 bits, which need to be mapped to position 31, 39,38, are right now in 0,1,2 respectively
				
				LSL			R2,#29				;LSL by 29 puts then back in 31,30,29
				AND			R7, R2, #0x80000000	;This gives us access to the 31st bit by anding it with 1 in 31st bit and 0 in the rest

;Now we want access to the 29 and 28th bit! BUT these two are in the 30th and 29th bit location respectively! So LSR by 1!
				LSR			R8,R2,#1			
;Now we can toggle these bit patterns!First add them to the 31st bit!
				ADD			R9,R8,R7			
;Now you should have the appropriate pattern in the appropriate location from 31,..,29,28
				
				STR			R9,[R10,#0x20]		;This should turn p1.31 ON... by writing 0 in the appropraite location--> Out Put: P31 on, other two off!

;---------------NOW LETS WORK ON PORT 2 LEDs----------------------------------------
				;A very similar logic follows
				AND			R4,R1,#0x1F000000	;AND So only the relvant bits are active
				LSL			R4,R4,#1			;LSL by one in prep for Reversing all the bits!
				RBIT		R4,R4				;R4 now has our PATTERN! (after reversing)
				STR			R4,[R10,#0x40]		;Write the pattern in the port to turn appropriate LEDs off!
				LDMFD		R13!,{R1,R2,R7,R8,R9,R15}
				
;------------------------DISPLAY BITS METHOD END-----------------------------

;
; Display the number in R3 onto the 8 LEDs
DISPLAY_NUM		STMFD		R13!,{R1, R2, R14}

; Usefull commaands:  RBIT (reverse bits), BFC (bit field clear), LSR & LSL to shift bits left and right, ORR & AND and EOR for bitwise operations

				LDMFD		R13!,{R1, R2, R15}

;
; R11 holds a random number via a pseudo-random sequence as per the Linear feedback shift register (Fibonacci) on WikiPedia
;
; R11 MUST be initialized to a non-zero 16-bit value at the start of the program
; R11 can be read anywhere in the code but must only be written to by this subroutine
RandomNum		STMFD		R13!,{R1, R2, R3, R14}

				AND			R1, R11, #0x8000
				AND			R2, R11, #0x2000
				LSL			R2, #2
				EOR			R3, R1, R2
				AND			R1, R11, #0x1000
				LSL			R2, #3
				EOR			R3, R3, R1
				AND			R1, R11, #0x0400
				LSL			R1, #5
				EOR			R3, R3, R1		; the new bit to go into the LSB is present
				LSR			R3, #15
				LSL			R11, #1
				ORR			R11, R11, R3
				
				LDMFD		R13!,{R1, R2, R3, R15}

;
;		Delay 0.1ms (100us) * R0 times
; 		aim for better than 10% accuracy

;--------------DELAY METHOD-----------------------
DELAY			STMFD		R13!,{R2, R14}
				MOV			R2, #0x00000064		;counting down from 100(64 in hex) results in a long enough delay so we can easily view the changes in the LED
DelayInnerLoop	SUBS		R2,R2,#1		;Subtract 1 from r2.
				BEQ			exitDelay		;Exit if countdown is finished
				BNE			DelayInnerLoop	;Minus one again if counter is NOT zero.
		
		;
		; code to generate a delay of 0.1mS * R0 times
		;
exitDelay		LDMFD		R13!,{R2, R15}
;---------------------------------------------------
				

LED_BASE_ADR	EQU 	0x2009c000 		; Base address of the memory that controls the LEDs 
PINSEL3			EQU 	0x4002c00c 		; Address of Pin Select Register 3 for P1[31:16]
PINSEL4			EQU 	0x4002c010 		; Address of Pin Select Register 4 for P2[15:0]
FIO2PIN 		EQU 	0x2009c054

;	Usefull GPIO Registers
;	FIODIR  - register to set individual pins as input or output
;	FIOPIN  - register to read and write pins
;	FIOSET  - register to set I/O pins to 1 by writing a 1
;	FIOCLR  - register to clr I/O pins to 0 by writing a 1

				ALIGN 

				END 
				
; ----------------------POST-LAB QUESTIONS AND ANSWERS------------------------------------

; 1) SUMMARY (EXPLANATION BELOW)
	; 8 Bit Register counts to 255, and would result in time 0.0255 seconds
	; 16 Bit Register counts to 65535, and would result in time 6.5535 seconds 
	; 24 Bit Register counts to 16777215, and would result in time 1677.7215 seconds (about 27.96 minutes)
	; 32 Bit Register counts to 4294967295, and would result in time 429496.7295 seconds (about 4.97 days)
	
	; EXPLANATION::
; A Tenth of a millisecond is 0.0001 seconds. An n-bit register can count up to
	; 2^n - 1. Thus, 8-Bit counts to 255. If we increase our counter once every 0.0001s, 
	; we will be able to count up 255*0.0001=0.0255 seconds. That is about 25.5 milliseconds, 
	; which is too low for a human to react too. If we use a 16 bit counter, we can count to
	; 65535. If we count using this, we can complete our counter in 6.5535 seconds. 
	; For a 24 bit counter the max value is 16777215, which in seconds if we count would be
	; 1677.7215 seconds. Lastly, a 32 bit counter would count to 4294967295
	; which in seconds would result in 429496.7295 seconds. 
	
; 2) Assume the average human reaction time varies from 200 to 1000 milliseconds.
	; Clearly, the reasonable register here would
	; be a 16 bit register as that one maxes out at 6.5 seconds, which is more than enough 
	; time for a human to react in. The 8 bit register stops too fast at 25 milliseconds, while the other two count for 
	; way longer than the time a human takes to react. 


;-------------------SIMPLECOUNTER--------------------------
; SIMPLECOUNTER	STMFD		R13!,{R3,R4,R14}
				; MOV			R3,#0x0

; DISPLAY_NMBR	BL			DISPLAYBITS
				; MOV			R4,#0x00000064
; DELAYLOOP		BL			DELAY
				; SUBS		R4,#1
				; BNE			DELAYLOOP
				
				
				; ADD			R3,#1
				; CMP			R3,#256
				; BEQ			EXITCOUNTER
				; BNE			DISPLAY_NMBR
				

; EXITCOUNTER		LDMFD		R13!,{R3,R4,R15}


; ;-----FOR TESTING PURPOSES ONLY-----------------------------------------
; POLLING			STMFD		R13!,{R4,R10,R14}
				


				
				
				; LDMFD		R13!,{R4,R10,R15}
; ;------------------------------------------------------------------------	






















;1. The max amount of time of 8 bits is 2^8 * 0.1 ms = 25.6ms. For 16 bits, it’s 2^16 * 0.1ms = 6553.6ms. For 24 bits, it’s 2^24 * 0.1 ms = 1677721.6ms = 1677s. For 32 bits, it’s 2^32 * 0.1 = 429496s
;2.The size should be 16 bits.

;**I implemented the simple counter and the reflex-meter in different project. The simple counter code is in the other file.


; ; ; ; ; ; ; ECE-222 Lab ... Winter 2013 term 
; ; ; ; ; ; ; Lab 3 sample code 
				; ; ; ; ; ; THUMB 		; Thumb instruction set 
                ; ; ; ; ; ; AREA 		My_code, CODE, READONLY
                ; ; ; ; ; ; EXPORT 		__MAIN
				; ; ; ; ; ; ENTRY  
; ; ; ; ; ; __MAIN

; ; ; ; ; ; ; The following lines are similar to Lab-1 but use a defined address to make it easier.
; ; ; ; ; ; ; They just turn off all LEDs 
				; ; ; ; ; ; LDR			R10, =LED_BASE_ADR		; R10 is a permenant pointer to the base address for the LEDs, offset of 0x20 and 0x40 for the ports

				; ; ; ; ; ; MOV 		R3, #0xB0000000		; Turn off three LEDs on port 1  
				; ; ; ; ; ; STR 		R3, [r10, #0x20]
				; ; ; ; ; ; MOV 		R3, #0x0000007C
				; ; ; ; ; ; STR 		R3, [R10, #0x40] 	; Turn off five LEDs on port 2 

				


; ; ; ; ; ; ; This line is very important in your main program
; ; ; ; ; ; ; Initializes R11 to a 16-bit non-zero value and NOTHING else can write to R11 !!
				; ; ; ; ; ; MOV			R11, #0xABCD		; Init the random number generator with a non-zero number
; ; ; ; ; ; loop 			BL 			RandomNum 
				; ; ; ; ; ; ;B 			loop
				
; ; ; ; ; ; ;This is the beginning of the reflex meter project				
; ; ; ; ; ; REFLEXMETER		STMFD		R13!,{R2,R3,R5,R6,R7,R12,R14}
				; ; ; ; ; ; LDR			R12, =FIO2PIN		;load the address from which we read the INT0 button's status
				
; ; ; ; ; ; RANDOM_COUNT_DELAY 	
				; ; ; ; ; ; TEQ			R11,#0xD				;RANDOM_COUNT_DELAY causes a delay of the random value generated in "0.1 ms" units
				; ; ; ; ; ; BEQ			LED_ON					;once the delay has been run for all of random num's length, we skip to next step in program	
				; ; ; ; ; ; BL			DELAY
				; ; ; ; ; ; SUB			R11,#1
				; ; ; ; ; ; B			RANDOM_COUNT_DELAY
									
; ; ; ; ; ; LED_ON			MOV			R2,#0xD0000000 		;this is where we come once we are done random count function (LED 29 is turned on in this section)
				; ; ; ; ; ; STR			R2,[R10,#0X20]
				
				
				; ; ; ; ; ; MOV			R3,#0				;Set count value to zero

; ; ; ; ; ; INCR_POLL		BL			DELAY				;This section creates a delay, adds 1 to the counter, and then checks if the INT0 button has been pressed
				; ; ; ; ; ; ADD			R3,R3,#1				;(this is how we measure the user's reaction in 0.1 milliseconds).. If the button is not pressed
													; ; ; ; ; ; ;then we loop back and increase the count again and cause a delay. 

				; ; ; ; ; ; LDR			R4,[R12]				;(here we are loading the contents of the memory address that has the status of int0 button)
				; ; ; ; ; ; AND			R4,R4,#0x00000400		;if 10th bit is pressed(or if p2.10=int0 button is pressed), anding with 400 will give 0, else it will give 400 again 
				; ; ; ; ; ; TEQ			R4,#0					;check to see if anding gave 0, if yes then dont execute next instruction and proceed to next segment of program
				; ; ; ; ; ; BNE			INCR_POLL
				
				; ; ; ; ; ; MOV			R5,#0x00000000			;here a new count value is being set to 0
				; ; ; ; ; ; MOV			R6,#0x00000000			;before moving an important value in R6, we clear it
				; ; ; ; ; ; MOV			R6,R3					;Store R3 in R6... this is because we will modify R3, which is basically the bit pattern we have to display.
														; ; ; ; ; ; ;once we have displayed all the bits, we will restore R3's original value back into R3 using R6. Then 
														; ; ; ; ; ; ;we can display the whole pattern again! Hence this allows us to create an endless loop
				
; ; ; ; ; ; DISPLAY_ALL		BL			DISPLAYBITS				;First display the first 8 bits of the value in R3
				; ; ; ; ; ; LSR			R3,#8					;Right shift by 8 to get access to the next 8 bits
				; ; ; ; ; ; BL			twosecdelay				;Cause a 2 second delay between displays
				; ; ; ; ; ; ADD			R5,R5,#1				;If this count reaches 4, we have displayed all 4 pattern sets in R3 
				; ; ; ; ; ; TEQ			R5,#4
				; ; ; ; ; ; BEQ			fivesecdelay			;Once all 4 have been displayed, cause a 5 second delay
				; ; ; ; ; ; BNE			DISPLAY_ALL				;If all 4 not displayed, delay the next set of bits
				
; ; ; ; ; ; fivesecdelay	MOV			R3,#0x00000000			;First clear R3-->So we can store the original pattern in here again
				; ; ; ; ; ; MOV			R5,#0x00000000			;clear R5-->So when we display again we can count to 4 again

				; ; ; ; ; ; MOV			R3,R6					;Store original pattern back in R3
				; ; ; ; ; ; MOV			R7,#0x00006B6C			;Calling the delay this many times causes a 5 second delay roughly (Mathematically it is C350 but that doesnt work so well practically)
				; ; ; ; ; ; LSL			R7,#1
; ; ; ; ; ; fivecheck		BL			DELAY
				; ; ; ; ; ; SUBS		R7,#1
				; ; ; ; ; ; BNE			fivecheck
				
				; ; ; ; ; ; BEQ			DISPLAY_ALL				;Once delay has been called enough times, go back and display everything again
				

				
				; ; ; ; ; ; LDMFD		R13!,{R2,R3,R5,R6,R7,R12,R15}
				
; ; ; ; ; ; twosecdelay		STMFD		R13!,{R7,R14}

				; ; ; ; ; ; MOV			R7,#0x00004E20			;Calling the delay this many times causes a 2 second delay roughly
; ; ; ; ; ; mini			BL			DELAY
				; ; ; ; ; ; SUBS		R7,#1
				; ; ; ; ; ; BNE			mini
				
				; ; ; ; ; ; LDMFD		R13!,{R7,R15}
				
				
; ; ; ; ; ; ; 

; ; ; ; ; ; ;-------------DISPLAYBITS METHOD-----------------------------------------
; ; ; ; ; ; DISPLAYBITS		
				; ; ; ; ; ; STMFD		R13!,{R1,R2,R7,R8,R9,R14}
				; ; ; ; ; ; AND			R1,R3,#0x000000FF	;ANDing with this will give us access to the Least Significant 8 bits in R3, and store the result in R1
				; ; ; ; ; ; EOR			R1,#0x000000FF		;this operation will flip first 8 bits... this is done because we want to represent bit"1" as ON and bit "0" as off... without this we would get the oppoiste result
				; ; ; ; ; ; LSL			R1,#24				;Left Shift to position 31,30,29,28,27,26,25,24
				
				; ; ; ; ; ; ;--------------NOW LETS FIRST AFFECT PORT 1 LEDs--------------------
				; ; ; ; ; ; RBIT		R2,R1				;Reverse order of bits and store in R2.
												; ; ; ; ; ; ;the top 3 bits, which need to be mapped to position 31, 39,38, are right now in 0,1,2 respectively
				
				; ; ; ; ; ; LSL			R2,#29				;LSL by 29 puts then back in 31,30,29
				; ; ; ; ; ; AND			R7, R2, #0x80000000	;This gives us access to the 31st bit by anding it with 1 in 31st bit and 0 in the rest

; ; ; ; ; ; ;Now we want access to the 29 and 28th bit! BUT these two are in the 30th and 29th bit location respectively! So LSR by 1!
				; ; ; ; ; ; LSR			R8,R2,#1			
; ; ; ; ; ; ;Now we can toggle these bit patterns!First add them to the 31st bit!
				; ; ; ; ; ; ADD			R9,R8,R7			
; ; ; ; ; ; ;Now you should have the appropriate pattern in the appropriate location from 31,..,29,28
				
				; ; ; ; ; ; STR			R9,[R10,#0x20]		;This should turn p1.31 ON... by writing 0 in the appropraite location--> Out Put: P31 on, other two off!

; ; ; ; ; ; ;---------------NOW LETS WORK ON PORT 2 LEDs----------------------------------------
				; ; ; ; ; ; ;A very similar logic follows
				; ; ; ; ; ; AND			R4,R1,#0x1F000000	;AND So only the relvant bits are active
				; ; ; ; ; ; LSL			R4,R4,#1			;LSL by one in prep for Reversing all the bits!
				; ; ; ; ; ; RBIT		R4,R4				;R4 now has our PATTERN! (after reversing)
				; ; ; ; ; ; STR			R4,[R10,#0x40]		;Write the pattern in the port to turn appropriate LEDs off!
				; ; ; ; ; ; LDMFD		R13!,{R1,R2,R7,R8,R9,R15}
				
; ; ; ; ; ; ;------------------------DISPLAY BITS METHOD END-----------------------------

; ; ; ; ; ; ;
; ; ; ; ; ; ; Display the number in R3 onto the 8 LEDs
; ; ; ; ; ; DISPLAY_NUM		STMFD		R13!,{R1, R2, R14}

; ; ; ; ; ; ; Usefull commaands:  RBIT (reverse bits), BFC (bit field clear), LSR & LSL to shift bits left and right, ORR & AND and EOR for bitwise operations

				; ; ; ; ; ; LDMFD		R13!,{R1, R2, R15}

; ; ; ; ; ; ;
; ; ; ; ; ; ; R11 holds a random number via a pseudo-random sequence as per the Linear feedback shift register (Fibonacci) on WikiPedia
; ; ; ; ; ; ;
; ; ; ; ; ; ; R11 MUST be initialized to a non-zero 16-bit value at the start of the program
; ; ; ; ; ; ; R11 can be read anywhere in the code but must only be written to by this subroutine
; ; ; ; ; ; RandomNum		STMFD		R13!,{R1, R2, R3, R14}

				; ; ; ; ; ; AND			R1, R11, #0x8000
				; ; ; ; ; ; AND			R2, R11, #0x2000
				; ; ; ; ; ; LSL			R2, #2
				; ; ; ; ; ; EOR			R3, R1, R2
				; ; ; ; ; ; AND			R1, R11, #0x1000
				; ; ; ; ; ; LSL			R2, #3
				; ; ; ; ; ; EOR			R3, R3, R1
				; ; ; ; ; ; AND			R1, R11, #0x0400
				; ; ; ; ; ; LSL			R1, #5
				; ; ; ; ; ; EOR			R3, R3, R1		; the new bit to go into the LSB is present
				; ; ; ; ; ; LSR			R3, #15
				; ; ; ; ; ; LSL			R11, #1
				; ; ; ; ; ; ORR			R11, R11, R3
				
				; ; ; ; ; ; LDMFD		R13!,{R1, R2, R3, R15}

; ; ; ; ; ; ;
; ; ; ; ; ; ;		Delay 0.1ms (100us) * R0 times
; ; ; ; ; ; ; 		aim for better than 10% accuracy

; ; ; ; ; ; ;--------------DELAY METHOD-----------------------
; ; ; ; ; ; DELAY			STMFD		R13!,{R2, R14}
				; ; ; ; ; ; MOV			R2, #0x000000FF		;counting down from 100(64 in hex) results in a long enough delay so we can easily view the changes in the LED
; ; ; ; ; ; DelayInnerLoop	SUBS		R2,R2,#1		;Subtract 1 from r2.
				; ; ; ; ; ; BEQ			exitDelay		;Exit if countdown is finished
				; ; ; ; ; ; BNE			DelayInnerLoop	;Minus one again if counter is NOT zero.
		
		; ; ; ; ; ; ;
		; ; ; ; ; ; ; code to generate a delay of 0.1mS * R0 times
		; ; ; ; ; ; ;
; ; ; ; ; ; exitDelay		LDMFD		R13!,{R2, R15}
; ; ; ; ; ; ;---------------------------------------------------
				

; ; ; ; ; ; LED_BASE_ADR	EQU 	0x2009c000 		; Base address of the memory that controls the LEDs 
; ; ; ; ; ; PINSEL3			EQU 	0x4002c00c 		; Address of Pin Select Register 3 for P1[31:16]
; ; ; ; ; ; PINSEL4			EQU 	0x4002c010 		; Address of Pin Select Register 4 for P2[15:0]
; ; ; ; ; ; FIO2PIN 		EQU 	0x2009c054

; ; ; ; ; ; ;	Usefull GPIO Registers
; ; ; ; ; ; ;	FIODIR  - register to set individual pins as input or output
; ; ; ; ; ; ;	FIOPIN  - register to read and write pins
; ; ; ; ; ; ;	FIOSET  - register to set I/O pins to 1 by writing a 1
; ; ; ; ; ; ;	FIOCLR  - register to clr I/O pins to 0 by writing a 1

				; ; ; ; ; ; ALIGN 

				; ; ; ; ; ; END 
				
; ; ; ; ; ; ; ----------------------POST-LAB QUESTIONS AND ANSWERS------------------------------------

; ; ; ; ; ; ; 1) SUMMARY (EXPLANATION BELOW)
	; ; ; ; ; ; ; 8 Bit Register counts to 255, and would result in time 0.0255 seconds
	; ; ; ; ; ; ; 16 Bit Register counts to 65535, and would result in time 6.5535 seconds 
	; ; ; ; ; ; ; 24 Bit Register counts to 16777215, and would result in time 1677.7215 seconds (about 27.96 minutes)
	; ; ; ; ; ; ; 32 Bit Register counts to 4294967295, and would result in time 429496.7295 seconds (about 4.97 days)
	
	; ; ; ; ; ; ; EXPLANATION::
; ; ; ; ; ; ; A Tenth of a millisecond is 0.0001 seconds. An n-bit register can count up to
	; ; ; ; ; ; ; 2^n - 1. Thus, 8-Bit counts to 255. If we increase our counter once every 0.0001s, 
	; ; ; ; ; ; ; we will be able to count up 255*0.0001=0.0255 seconds. That is about 25.5 milliseconds, 
	; ; ; ; ; ; ; which is too low for a human to react too. If we use a 16 bit counter, we can count to
	; ; ; ; ; ; ; 65535. If we count using this, we can complete our counter in 6.5535 seconds. 
	; ; ; ; ; ; ; For a 24 bit counter the max value is 16777215, which in seconds if we count would be
	; ; ; ; ; ; ; 1677.7215 seconds. Lastly, a 32 bit counter would count to 4294967295
	; ; ; ; ; ; ; which in seconds would result in 429496.7295 seconds. 
	
; ; ; ; ; ; ; 2) Assume the average human reaction time varies from 200 to 1000 milliseconds.
	; ; ; ; ; ; ; Clearly, the reasonable register here would
	; ; ; ; ; ; ; be a 16 bit register as that one maxes out at 6.5 seconds, which is more than enough 
	; ; ; ; ; ; ; time for a human to react in. The 8 bit register stops too fast at 25 milliseconds, while the other two count for 
	; ; ; ; ; ; ; way longer than the time a human takes to react. 


; ; ; ; ; ; ;-------------------SIMPLECOUNTER--------------------------
; ; ; ; ; ; ; SIMPLECOUNTER	STMFD		R13!,{R3,R4,R14}
				; ; ; ; ; ; ; MOV			R3,#0x0

; ; ; ; ; ; ; DISPLAY_NMBR	BL			DISPLAYBITS
				; ; ; ; ; ; ; MOV			R4,#0x00000064
; ; ; ; ; ; ; DELAYLOOP		BL			DELAY
				; ; ; ; ; ; ; SUBS		R4,#1
				; ; ; ; ; ; ; BNE			DELAYLOOP
				
				
				; ; ; ; ; ; ; ADD			R3,#1
				; ; ; ; ; ; ; CMP			R3,#256
				; ; ; ; ; ; ; BEQ			EXITCOUNTER
				; ; ; ; ; ; ; BNE			DISPLAY_NMBR
				

; ; ; ; ; ; ; EXITCOUNTER		LDMFD		R13!,{R3,R4,R15}


; ; ; ; ; ; ; ;-----FOR TESTING PURPOSES ONLY-----------------------------------------
; ; ; ; ; ; ; POLLING			STMFD		R13!,{R4,R10,R14}
				


				
				
				; ; ; ; ; ; ; LDMFD		R13!,{R4,R10,R15}
; ; ; ; ; ; ; ;------------------------------------------------------------------------	
























; ; ; ; ; ; ; ; ECE-222 Lab ... Winter 2013 term 
; ; ; ; ; ; ; ; Lab 3 sample code 
				; ; ; ; ; ; ; THUMB 		; Thumb instruction set 
                ; ; ; ; ; ; ; AREA 		My_code, CODE, READONLY
                ; ; ; ; ; ; ; EXPORT 		__MAIN
				; ; ; ; ; ; ; ENTRY  
; ; ; ; ; ; ; __MAIN

; ; ; ; ; ; ; ; The following lines are similar to Lab-1 but use a defined address to make it easier.
; ; ; ; ; ; ; ; They just turn off all LEDs 
				; ; ; ; ; ; ; LDR			R10, =LED_BASE_ADR		; R10 is a permenant pointer to the base address for the LEDs, offset of 0x20 and 0x40 for the ports

				; ; ; ; ; ; ; MOV 		R3, #0xB0000000		; Turn off three LEDs on port 1  
				; ; ; ; ; ; ; STR 		R3, [r10, #0x20]
				; ; ; ; ; ; ; MOV 		R3, #0x0000007C
				; ; ; ; ; ; ; STR 		R3, [R10, #0x40] 	; Turn off five LEDs on port 2 
				; ; ; ; ; ; ; ;BL			POLLING
; ; ; ; ; ; ; AGAIN			BL			SIMPLECOUNTER    ;CALL SIMPLECOUNTER TO DISPLAY PATTERN
				; ; ; ; ; ; ; B			AGAIN				;KEEP CALLING SIMPLE COUNTER TO REPEATEDLY SEE PATTERN
				; ; ; ; ; ; ; BL			DISPLAYBITS
				; ; ; ; ; ; ; MOV			R11,#0x0
				; ; ; ; ; ; ; MOV			R11,#0x86A0
				; ; ; ; ; ; ; MOVT		R11,#0x0001

				; ; ; ; ; ; ; BL			DELAY
				


; ; ; ; ; ; ; ; This line is very important in your main program
; ; ; ; ; ; ; ; Initializes R11 to a 16-bit non-zero value and NOTHING else can write to R11 !!
				; ; ; ; ; ; ; MOV			R11, #0xABCD		; Init the random number generator with a non-zero number
; ; ; ; ; ; ; loop 			BL 			RandomNum 
				; ; ; ; ; ; ; B 			loop
				

; ; ; ; ; ; ; SIMPLECOUNTER	STMFD		R13!,{R3,R4,R14}
				; ; ; ; ; ; ; MOV			R3,#0x0

; ; ; ; ; ; ; DISPLAY_NMBR	BL			DISPLAYBITS
				; ; ; ; ; ; ; MOV			R4,#0x00000064
; ; ; ; ; ; ; DELAYLOOP		BL			DELAY
				; ; ; ; ; ; ; SUBS		R4,#1
				; ; ; ; ; ; ; BNE			DELAYLOOP
				
				
				; ; ; ; ; ; ; ADD			R3,#1
				; ; ; ; ; ; ; CMP			R3,#256
				; ; ; ; ; ; ; BEQ			EXITCOUNTER
				; ; ; ; ; ; ; BNE			DISPLAY_NMBR
				

; ; ; ; ; ; ; EXITCOUNTER		LDMFD		R13!,{R3,R4,R15}



; ; ; ; ; ; ; POLLING			STMFD		R13!,{R1,R10,R14}
				
; ; ; ; ; ; ; TESTING			LDR			R10, =FIO2PIN
				; ; ; ; ; ; ; LDR			R1,[R10]
				; ; ; ; ; ; ; AND			R1,R1,#0x00000400
				; ; ; ; ; ; ; B			TESTING
				
				
				; ; ; ; ; ; ; LDMFD		R13!,{R1,R10,R15}
				

				
; ; ; ; ; ; ; DISPLAYBITS		
				; ; ; ; ; ; ; STMFD		R13!,{R1,R2,R7,R8,R9,R14}
				; ; ; ; ; ; ; AND			R1,R3,#0x000000FF	;This will give us access to the first 8 bytes to display, which is 01100000
				; ; ; ; ; ; ; EOR			R1,#0x000000FF
				; ; ; ; ; ; ; LSL			R1,#24				;Left Shift to position 31,30,29 ... and so on
				; ; ; ; ; ; ; RBIT		R2,R1				;Reverse order and store in R2. 
				; ; ; ; ; ; ; LSL			R2,#29				;LSL by 29 puts the reversed bits back in positions
				; ; ; ; ; ; ; AND			R7, R2, #0x80000000	;This gives us access to the 31st bit by anding it with 1 in 31st bit and 0 in the rest
												; ; ; ; ; ; ; ;Now we want access to the 29 and 28th bit! but these two are in the 30th and 29th bit location! So LSR by 1!!
				; ; ; ; ; ; ; LSR			R8,R2,#1			;Now we can toggle these bits!First add them to the 31st bit!
				; ; ; ; ; ; ; ADD			R9,R8,R7			;Now you should have the pattern 0011 in the appropriate location from 31,30,29,28
				; ; ; ; ; ; ; STR			R9,[R10,#0x20]		;This should turn p1.31 ON... by writing 0 in the appropraite location--> Out Put: P31 on, other two off!

	

; ; ; ; ; ; ; ;NOW WE NEED TO SET THE APPROPRIATE BITS TO TOGGLE P@ LEDS PROPERLY
; ; ; ; ; ; ; ;R1 has the relevant bits in position 28-24.. we want them in position  2-6 reversed!
; ; ; ; ; ; ; ;First and so only these few bits are active!
; ; ; ; ; ; ; ;Then LSL till 28th bit is in 29th bit (by 1!)
; ; ; ; ; ; ; ;Then reverse the bits! and you should have your pattern! 
; ; ; ; ; ; ; ;Write this pattern to the LEDS!

				; ; ; ; ; ; ; AND			R4,R1,#0x1F000000	;AND So only the relvant bits are active
				; ; ; ; ; ; ; LSL			R4,R4,#1			;LSL by one in prep for Reversing all the bits!
				; ; ; ; ; ; ; RBIT		R4,R4				;R4 now has our PATTERN!
				; ; ; ; ; ; ; STR			R4,[R10,#0x40]		;Write the pattern in the port to trun appropriate LEDs off!... PORT 2 and 4 should be OFF! Rest ON!
				; ; ; ; ; ; ; LDMFD		R13!,{R1,R2,R7,R8,R9,R15}

; ; ; ; ; ; ; ;
; ; ; ; ; ; ; ; Display the number in R3 onto the 8 LEDs
; ; ; ; ; ; ; DISPLAY_NUM		STMFD		R13!,{R1, R2, R14}

; ; ; ; ; ; ; ; Usefull commaands:  RBIT (reverse bits), BFC (bit field clear), LSR & LSL to shift bits left and right, ORR & AND and EOR for bitwise operations

				; ; ; ; ; ; ; LDMFD		R13!,{R1, R2, R15}

; ; ; ; ; ; ; ;
; ; ; ; ; ; ; ; R11 holds a random number via a pseudo-random sequence as per the Linear feedback shift register (Fibonacci) on WikiPedia
; ; ; ; ; ; ; ;
; ; ; ; ; ; ; ; R11 MUST be initialized to a non-zero 16-bit value at the start of the program
; ; ; ; ; ; ; ; R11 can be read anywhere in the code but must only be written to by this subroutine
; ; ; ; ; ; ; RandomNum		STMFD		R13!,{R1, R2, R3, R14}

				; ; ; ; ; ; ; AND			R1, R11, #0x8000
				; ; ; ; ; ; ; AND			R2, R11, #0x2000
				; ; ; ; ; ; ; LSL			R2, #2
				; ; ; ; ; ; ; EOR			R3, R1, R2
				; ; ; ; ; ; ; AND			R1, R11, #0x1000
				; ; ; ; ; ; ; LSL			R2, #3
				; ; ; ; ; ; ; EOR			R3, R3, R1
				; ; ; ; ; ; ; AND			R1, R11, #0x0400
				; ; ; ; ; ; ; LSL			R1, #5
				; ; ; ; ; ; ; EOR			R3, R3, R1		; the new bit to go into the LSB is present
				; ; ; ; ; ; ; LSR			R3, #15
				; ; ; ; ; ; ; LSL			R11, #1
				; ; ; ; ; ; ; ORR			R11, R11, R3
				
				; ; ; ; ; ; ; LDMFD		R13!,{R1, R2, R3, R15}

; ; ; ; ; ; ; ;
; ; ; ; ; ; ; ;		Delay 0.1ms (100us) * R0 times
; ; ; ; ; ; ; ; 		aim for better than 10% accuracy
; ; ; ; ; ; ; DELAY			STMFD		R13!,{R2, R14}
				; ; ; ; ; ; ; MOV			R2, #0x000000FF		;counting down from 13 results in 100 microsecond delay
				; ; ; ; ; ; ; LSL			R2,#4
				
; ; ; ; ; ; ; DelayInnerLoop	SUBS		R2,R2,#1		;Subtract 1 from r2.
				; ; ; ; ; ; ; BEQ			exitDelay		;Exit if countdown is finished
				; ; ; ; ; ; ; BNE			DelayInnerLoop	;Minus one again if counter is NOT zero.
		
		; ; ; ; ; ; ; ;
		; ; ; ; ; ; ; ; code to generate a delay of 0.1mS * R0 times
		; ; ; ; ; ; ; ;
; ; ; ; ; ; ; exitDelay		LDMFD		R13!,{R2, R15}
				

; ; ; ; ; ; ; LED_BASE_ADR	EQU 	0x2009c000 		; Base address of the memory that controls the LEDs 
; ; ; ; ; ; ; PINSEL3			EQU 	0x4002c00c 		; Address of Pin Select Register 3 for P1[31:16]
; ; ; ; ; ; ; PINSEL4			EQU 	0x4002c010 		; Address of Pin Select Register 4 for P2[15:0]
; ; ; ; ; ; ; FIO2PIN 		EQU 	0x2009c054

; ; ; ; ; ; ; ;	Usefull GPIO Registers
; ; ; ; ; ; ; ;	FIODIR  - register to set individual pins as input or output
; ; ; ; ; ; ; ;	FIOPIN  - register to read and write pins
; ; ; ; ; ; ; ;	FIOSET  - register to set I/O pins to 1 by writing a 1
; ; ; ; ; ; ; ;	FIOCLR  - register to clr I/O pins to 0 by writing a 1

				; ; ; ; ; ; ; ALIGN 

				; ; ; ; ; ; ; END 




; ; ; ; ; ; ; ; ;*-------------------------------------------------------------------
; ; ; ; ; ; ; ; ;* Name:    	lab_4_program.s 
; ; ; ; ; ; ; ; ;* Purpose: 	A sample style for lab-4
; ; ; ; ; ; ; ; ;* Term:		Winter 2013
; ; ; ; ; ; ; ; ;*-------------------------------------------------------------------
				; ; ; ; ; ; ; ; THUMB 								; Declare THUMB instruction set 
				; ; ; ; ; ; ; ; AREA 	My_code, CODE, READONLY 	; 
				; ; ; ; ; ; ; ; EXPORT 		__MAIN 					; Label __MAIN is used externally 
				; ; ; ; ; ; ; ; ;EXPORT 		EINT3_IRQHandler 		; The ISR will be known to the startup_LPC17xx.s program 

				; ; ; ; ; ; ; ; ENTRY 

; ; ; ; ; ; ; ; __MAIN

; ; ; ; ; ; ; ; ; The following lines are similar to previous labs.
; ; ; ; ; ; ; ; ; They just turn off all LEDs 
				; ; ; ; ; ; ; ; LDR		R10, =LED_BASE_ADR		; R10 is a  pointer to the base address for the LEDs
				; ; ; ; ; ; ; ; MOV 		R3, #0xB0000000		; Turn off three LEDs on port 1  
				; ; ; ; ; ; ; ; STR 		R3, [r10, #0x20]
				; ; ; ; ; ; ; ; MOV 		R3, #0x0000007C
				; ; ; ; ; ; ; ; STR 		R3, [R10, #0x40] 	; Turn off five LEDs on port 2 
				

				; ; ; ; ; ; ; ; LDR			R12, =ISER0   	;ENABLING EINT3
				; ; ; ; ; ; ; ; MOV			R7, #0x00200000
				; ; ; ; ; ; ; ; STR			R7, [R12]
				; ; ; ; ; ; ; ; LDR			R12,=IO2IntEnf	;ENABLING FALLING EDGE
				; ; ; ; ; ; ; ; MOV			R7, #0x00000400
				; ; ; ; ; ; ; ; STR			R7, [R12]

; ; ; ; ; ; ; ; ; This line is very important in your main program
; ; ; ; ; ; ; ; ; Initializes R11 to a 16-bit non-zero value and NOTHING else can write to R11 !!
				; ; ; ; ; ; ; ; MOV			R11, #0x00000005		; Init the random number generator with a non-zero number
; ; ; ; ; ; ; ; LOOP 			BL 			RNG  
				; ; ; ; ; ; ; ; ;B 			LOOP
				
				; ; ; ; ; ; ; ; LDR			R10,=LED_BASE_ADR
				; ; ; ; ; ; ; ; MOV			R3, #0xB0000000		;initial storage value for Port 1 leds. (To turn them off) EOR to get the opposites
				; ; ; ; ; ; ; ; MOV			R4, #0x00000004		;initial storage value for Port 2 Leds
				
				
				
; ; ; ; ; ; ; ; REPEAT				STR			R3,[r10,#0x20]		;turn off port 1
				; ; ; ; ; ; ; ; STR			R4,[r10,#0x40] 		;turn on port 2
			
				; ; ; ; ; ; ; ; EOR			R3, R3, #0xB0000000	;attain value to turn on port 1 and turn off port 2
				; ; ; ; ; ; ; ; EOR			R4, R4, #0x0000007C
				; ; ; ; ; ; ; ; MOV			R5, #0xC350		;'multiplier' for Delay... causes a roughly 2 second delay

; ; ; ; ; ; ; ; twosecs				BL			DELAY			;two second delay sequence
				; ; ; ; ; ; ; ; SUBS			R5, #1
				; ; ; ; ; ; ; ; BNE			twosecs
				
				; ; ; ; ; ; ; ; SUBS			R11,#1			
				; ; ; ; ; ; ; ; BNE			REPEAT			;If R11 is not zero, keep flashing back and forth
				; ; ; ; ; ; ; ; BEQ			HIGHFLASH		;else start the high frequency flash



; ; ; ; ; ; ; ; HIGHFLASH			MOV			R6, #0x1		;Flag for checking if program entered ISR
				; ; ; ; ; ; ; ; MOV			R7, #0			;R7 will store PATTERN TO DISPLAY (counter)
				; ; ; ; ; ; ; ; LDR			R10, =LED_BASE_ADR	;R10 is a  pointer to the base address for the LEDs
				; ; ; ; ; ; ; ; MOV 			R3, #0xB0000000		; To Turn off three LEDs on port 1  
				; ; ; ; ; ; ; ; MOV 			R4, #0x0000007C		; To Turn off 5 LEDs on port 2
				
; ; ; ; ; ; ; ; REPEAT2				ADD			R7,#1			;increment count value to display
				; ; ; ; ; ; ; ; TEQ			R6, #0			;chec if program entered ISR
				; ; ; ; ; ; ; ; BEQ			DISPLAY			;if yes then start displaying the bits
				
				; ; ; ; ; ; ; ; STR 		R3, [r10, #0x20]		;if did not enter ISR then Turn on/off the leds

				; ; ; ; ; ; ; ; STR 		R4, [R10, #0x40] 	; Turn off five LEDs on port 2 		
				
				; ; ; ; ; ; ; ; MOV			R5, #0x09C4		;Delay multiplier
; ; ; ; ; ; ; ; HFQDELAY		BL			DELAY
				; ; ; ; ; ; ; ; SUBS		R5, #1
				; ; ; ; ; ; ; ; BNE			HFQDELAY


				; ; ; ; ; ; ; ; EOR			R3, R3, #0xB0000000	;EOR to get the opposite value so we can turn led on/off automatically
				; ; ; ; ; ; ; ; EOR			R4, R4, #0x0000007C

				; ; ; ; ; ; ; ; B			REPEAT2
				
				




; ; ; ; ; ; ; ; DISPLAY				MOV			R3, #0x0		;Display sequence from Lab 3
				; ; ; ; ; ; ; ; MOV			R3, R7
				; ; ; ; ; ; ; ; MOV			R5, #0

; ; ; ; ; ; ; ; DISPLAY_ALL		BL			DISPLAYBITS				;First display the first 8 bits of the value in R3
				; ; ; ; ; ; ; ; LSR			R3,#8					;Right shift by 8 to get access to the next 8 bits
				; ; ; ; ; ; ; ; BL			twosecdelay				;Cause a 2 second delay between displays
				; ; ; ; ; ; ; ; ADD			R5,R5,#1				;If this count reaches 4, we have displayed all 4 pattern sets in R3 
				; ; ; ; ; ; ; ; TEQ			R5,#4
				; ; ; ; ; ; ; ; BEQ			fivesecdelay			;Once all 4 have been displayed, cause a 5 second delay
				; ; ; ; ; ; ; ; BNE			DISPLAY_ALL				;If all 4 not displayed, delay the next set of bits
				
; ; ; ; ; ; ; ; fivesecdelay	;MOV			R3,#0x00000000			;First clear R3-->So we can store the original pattern in here again
				; ; ; ; ; ; ; ; MOV			R5,#0x00000000			;clear R5-->So when we display again we can count to 4 again

				; ; ; ; ; ; ; ; MOV			R3,R7					;Store original pattern back in R3
				; ; ; ; ; ; ; ; MOV			R8,#0x0000FDE8			;Calling the delay this many times causes a 5 second delay roughly (Mathematically it is C350 but that doesnt work so well practically)
				; ; ; ; ; ; ; ; LSL			R8,#1
; ; ; ; ; ; ; ; fivecheck		BL			DELAY
				; ; ; ; ; ; ; ; SUBS		R8,#1
				; ; ; ; ; ; ; ; BNE			fivecheck
				
				; ; ; ; ; ; ; ; BEQ			DISPLAY_ALL				;Once delay has been called enough times, go back and display everything again
				

				
				; ; ; ; ; ; ; ; LDMFD		R13!,{R2,R3,R5,R6,R7,R12,R15}
				
; ; ; ; ; ; ; ; twosecdelay		STMFD		R13!,{R7,R8,R14}

				; ; ; ; ; ; ; ; MOV			R8,#0x0000C350			;Calling the delay this many times causes a 2 second delay roughly
; ; ; ; ; ; ; ; mini			BL			DELAY
				; ; ; ; ; ; ; ; SUBS		R8,#1
				; ; ; ; ; ; ; ; BNE			mini
				
				; ; ; ; ; ; ; ; LDMFD		R13!,{R7,R9,R15}
		; ; ; ; ; ; ; ; ;
		; ; ; ; ; ; ; ; ; Your main program can appear here 
		; ; ; ; ; ; ; ; ;
				
				
				
; ; ; ; ; ; ; ; ;*------------------------------------------------------------------- 
; ; ; ; ; ; ; ; ; Subroutine RNG ... Generates a pseudo-Random Number in R11 
; ; ; ; ; ; ; ; ;*------------------------------------------------------------------- 
; ; ; ; ; ; ; ; ; R11 holds a random number as per the Linear feedback shift register (Fibonacci) on WikiPedia
; ; ; ; ; ; ; ; ; R11 MUST be initialized to a non-zero 16-bit value at the start of the program
; ; ; ; ; ; ; ; ; R11 can be read anywhere in the code but must only be written to by this subroutine
; ; ; ; ; ; ; ; RNG 			STMFD		R13!,{R1-R3, R14} 	; Random Number Generator 
				; ; ; ; ; ; ; ; AND			R1, R11, #0x8000
				; ; ; ; ; ; ; ; AND			R2, R11, #0x2000
				; ; ; ; ; ; ; ; LSL			R2, #2
				; ; ; ; ; ; ; ; EOR			R3, R1, R2
				; ; ; ; ; ; ; ; AND			R1, R11, #0x1000
				; ; ; ; ; ; ; ; LSL			R1, #3
				; ; ; ; ; ; ; ; EOR			R3, R3, R1
				; ; ; ; ; ; ; ; AND			R1, R11, #0x0400
				; ; ; ; ; ; ; ; LSL			R1, #5
				; ; ; ; ; ; ; ; EOR			R3, R3, R1			; The new bit to go into the LSB is present
				; ; ; ; ; ; ; ; LSR			R3, #15
				; ; ; ; ; ; ; ; LSL			R11, #1
				; ; ; ; ; ; ; ; ORR			R11, R11, R3
				; ; ; ; ; ; ; ; LDMFD		R13!,{R1-R3, R15}

; ; ; ; ; ; ; ; ;*------------------------------------------------------------------- 
; ; ; ; ; ; ; ; ; Subroutine DELAY ... Causes a delay of 1ms * R0 times
; ; ; ; ; ; ; ; ;*------------------------------------------------------------------- 
; ; ; ; ; ; ; ; ; 		aim for better than 10% accuracy
; ; ; ; ; ; ; ; DELAY			STMFD		R13!,{R2, R14}
				; ; ; ; ; ; ; ; MOV			R2, #0x0000000D
				; ; ; ; ; ; ; ; ;LSL			R2, #9

				
; ; ; ; ; ; ; ; INNER			SUBS		R2, #1
				; ; ; ; ; ; ; ; BEQ			exitDelay
				; ; ; ; ; ; ; ; BNE			INNER
				
		; ; ; ; ; ; ; ; ;
		; ; ; ; ; ; ; ; ; Code to generate a delay of 1mS * R0 times
		; ; ; ; ; ; ; ; ;
; ; ; ; ; ; ; ; exitDelay		LDMFD		R13!,{R2, R15}

; ; ; ; ; ; ; ; DISPLAYBITS		
				; ; ; ; ; ; ; ; STMFD		R13!,{R1,R2,R7,R8,R9,R14}
				; ; ; ; ; ; ; ; AND			R1,R3,#0x000000FF	;ANDing with this will give us access to the Least Significant 8 bits in R3, and store the result in R1
				; ; ; ; ; ; ; ; EOR			R1,#0x000000FF		;this operation will flip first 8 bits... this is done because we want to represent bit"1" as ON and bit "0" as off... without this we would get the oppoiste result
				; ; ; ; ; ; ; ; LSL			R1,#24				;Left Shift to position 31,30,29,28,27,26,25,24
				
				; ; ; ; ; ; ; ; ;--------------NOW LETS FIRST AFFECT PORT 1 LEDs--------------------
				; ; ; ; ; ; ; ; RBIT		R2,R1				;Reverse order of bits and store in R2.
												; ; ; ; ; ; ; ; ;the top 3 bits, which need to be mapped to position 31, 39,38, are right now in 0,1,2 respectively
				
				; ; ; ; ; ; ; ; LSL			R2,#29				;LSL by 29 puts then back in 31,30,29
				; ; ; ; ; ; ; ; AND			R7, R2, #0x80000000	;This gives us access to the 31st bit by anding it with 1 in 31st bit and 0 in the rest

; ; ; ; ; ; ; ; ;Now we want access to the 29 and 28th bit! BUT these two are in the 30th and 29th bit location respectively! So LSR by 1!
				; ; ; ; ; ; ; ; LSR			R8,R2,#1			
; ; ; ; ; ; ; ; ;Now we can toggle these bit patterns!First add them to the 31st bit!
				; ; ; ; ; ; ; ; ADD			R9,R8,R7			
; ; ; ; ; ; ; ; ;Now you should have the appropriate pattern in the appropriate location from 31,..,29,28
				
				; ; ; ; ; ; ; ; STR			R9,[R10,#0x20]		;This should turn p1.31 ON... by writing 0 in the appropraite location--> Out Put: P31 on, other two off!

; ; ; ; ; ; ; ; ;---------------NOW LETS WORK ON PORT 2 LEDs----------------------------------------
				; ; ; ; ; ; ; ; ;A very similar logic follows
				; ; ; ; ; ; ; ; AND			R4,R1,#0x1F000000	;AND So only the relvant bits are active
				; ; ; ; ; ; ; ; LSL			R4,R4,#1			;LSL by one in prep for Reversing all the bits!
				; ; ; ; ; ; ; ; RBIT		R4,R4				;R4 now has our PATTERN! (after reversing)
				; ; ; ; ; ; ; ; STR			R4,[R10,#0x40]		;Write the pattern in the port to turn appropriate LEDs off!
				; ; ; ; ; ; ; ; LDMFD		R13!,{R1,R2,R7,R8,R9,R15}


				

; ; ; ; ; ; ; ; ;*------------------------------------------------------------------- 
; ; ; ; ; ; ; ; ; Interrupt Service Routine (ISR) for EINT3_IRQHandler 
; ; ; ; ; ; ; ; ;*------------------------------------------------------------------- 
; ; ; ; ; ; ; ; ; This ISR handles the interrupt triggered when the INT0 push-button is pressed 
; ; ; ; ; ; ; ; ; with the assumption that the interrupt activation is done in the main program

			; ; ; ; ; ; ; ; ;IN ORDER TO USE THIS HANDLER COPY INTO THE LPC_17xx.s FILE UNDER THE HANDLER TITLED: EINT3_IRQHandler 

; ; ; ; ; ; ; ; ; EINT3_IRQHandler 	;PROC 
					; ; ; ; ; ; ; ; ;STMFD 		R13!,{R0,R10,R14}	; Use this command if you need it  
					
					; ; ; ; ; ; ; ; ;MOV			R6, #0x0  	;reset flag to 0 for when we chec in the main program
					; ; ; ; ; ; ; ; ;LDR			R10, =0x400280AC ;Address on which we need to clear the clear bit
					; ; ; ; ; ; ; ; ;MOV			R0, #0x00000400	; writing 1 on the 10th bit will clear  the interrupt bit
					; ; ; ; ; ; ; ; ;STR			R0, [R10]		
					
					
		; ; ; ; ; ; ; ; ;
		; ; ; ; ; ; ; ; ; Code that handles the interrupt 
		; ; ; ; ; ; ; ; ;
					; ; ; ; ; ; ; ; ;LDMFD 		R13!,{R0,R10,R15}	; Use this command if you used STMFD (otherwise use BX LR) 
					; ; ; ; ; ; ; ; ;ENDP

; ; ; ; ; ; ; ; ;*-------------------------------------------------------------------
; ; ; ; ; ; ; ; ; Below is a list of useful registers with their respective memory addresses.
; ; ; ; ; ; ; ; ;*------------------------------------------------------------------- 
; ; ; ; ; ; ; ; LED_BASE_ADR	EQU 	0x2009c000 		; Base address of the memory that controls the LEDs 
; ; ; ; ; ; ; ; PINSEL3			EQU 	0x4002C00C 		; Pin Select Register 3 for P1[31:16]
; ; ; ; ; ; ; ; PINSEL4			EQU 	0x4002C010 		; Pin Select Register 4 for P2[15:0]
; ; ; ; ; ; ; ; FIO1DIR			EQU		0x2009C020 		; Fast Input Output Direction Register for Port 1 
; ; ; ; ; ; ; ; FIO2DIR			EQU		0x2009C040 		; Fast Input Output Direction Register for Port 2 
; ; ; ; ; ; ; ; FIO1SET			EQU		0x2009C038 		; Fast Input Output Set Register for Port 1 
; ; ; ; ; ; ; ; FIO2SET			EQU		0x2009C058 		; Fast Input Output Set Register for Port 2 
; ; ; ; ; ; ; ; FIO1CLR			EQU		0x2009C03C 		; Fast Input Output Clear Register for Port 1 
; ; ; ; ; ; ; ; FIO2CLR			EQU		0x2009C05C 		; Fast Input Output Clear Register for Port 2 
; ; ; ; ; ; ; ; IO2IntEnf		EQU		0x400280B4		; GPIO Interrupt Enable for port 2 Falling Edge 
; ; ; ; ; ; ; ; ISER0			EQU		0xE000E100		; Interrupt Set-Enable Register 0
; ; ; ; ; ; ; ; CLEAR			EQU		0x400280AC

				; ; ; ; ; ; ; ; ALIGN 

				; ; ; ; ; ; ; ; END 



; ; ; ; ; ; ; ; ;*-------------------------------------------------------------------
; ; ; ; ; ; ; ; ;* Name:    	lab_4_program.s 
; ; ; ; ; ; ; ; ;* Purpose: 	A sample style for lab-4
; ; ; ; ; ; ; ; ;* Term:		Winter 2013
; ; ; ; ; ; ; ; ;*-------------------------------------------------------------------
				; ; ; ; ; ; ; ; THUMB 								; Declare THUMB instruction set 
				; ; ; ; ; ; ; ; AREA 	My_code, CODE, READONLY 	; 
				; ; ; ; ; ; ; ; EXPORT 		__MAIN 					; Label __MAIN is used externally 
				; ; ; ; ; ; ; ; ;EXPORT 		EINT3_IRQHandler 		; The ISR will be known to the startup_LPC17xx.s program 

				; ; ; ; ; ; ; ; ENTRY 

; ; ; ; ; ; ; ; __MAIN

; ; ; ; ; ; ; ; ; The following lines are similar to previous labs.
; ; ; ; ; ; ; ; ; They just turn off all LEDs 
				; ; ; ; ; ; ; ; LDR			R10, =LED_BASE_ADR		; R10 is a  pointer to the base address for the LEDs
				; ; ; ; ; ; ; ; MOV 		R3, #0xB0000000		; Turn off three LEDs on port 1  
				; ; ; ; ; ; ; ; STR 		R3, [r10, #0x20]
				; ; ; ; ; ; ; ; MOV 		R3, #0x0000007C
				; ; ; ; ; ; ; ; STR 		R3, [R10, #0x40] 	; Turn off five LEDs on port 2 
				; ; ; ; ; ; ; ; LDR			R12, =ISER0   	;ENABLING EINT3
				; ; ; ; ; ; ; ; MOV			R7, #0x00200000
				; ; ; ; ; ; ; ; STR			R7, [R12]
				; ; ; ; ; ; ; ; LDR			R12,=IO2IntEnf		;ENABLING FALLING EDGE
				; ; ; ; ; ; ; ; MOV			R7, #0x00000400
				; ; ; ; ; ; ; ; STR			R7, [R12]

; ; ; ; ; ; ; ; ; This line is very important in your main program
; ; ; ; ; ; ; ; ; Initializes R11 to a 16-bit non-zero value and NOTHING else can write to R11 !!
				; ; ; ; ; ; ; ; MOV			R11, #0x00000005		; Init the random number generator with a non-zero number
; ; ; ; ; ; ; ; LOOP 			BL 			RNG  
				; ; ; ; ; ; ; ; ;B 			LOOP
				
				; ; ; ; ; ; ; ; LDR			R10,=LED_BASE_ADR
				; ; ; ; ; ; ; ; MOV			R3, #0xB0000000		;initial storage value for Port 1 leds. EOR to get the opposites
				; ; ; ; ; ; ; ; MOV			R4, #0x00000004
				
				
				
; ; ; ; ; ; ; ; REPEAT			STR			R3,[r10,#0x20]
				; ; ; ; ; ; ; ; STR			R4,[r10,#0x40] 
				
				; ; ; ; ; ; ; ; EOR			R3, R3, #0xB0000000
				; ; ; ; ; ; ; ; EOR			R4, R4, #0x0000007C
				; ; ; ; ; ; ; ; MOV			R5, #0xC350

; ; ; ; ; ; ; ; twosecs			BL			DELAY
				; ; ; ; ; ; ; ; SUBS		R5, #1
				; ; ; ; ; ; ; ; BNE			twosecs
				
				; ; ; ; ; ; ; ; SUBS		R11,#1
				; ; ; ; ; ; ; ; BNE			REPEAT
				; ; ; ; ; ; ; ; BEQ			HIGHFLASH



; ; ; ; ; ; ; ; HIGHFLASH		MOV			R6, #0x1
				; ; ; ; ; ; ; ; MOV			R7, #0					;PATTERN TO DISPLAY
				; ; ; ; ; ; ; ; LDR			R10, =LED_BASE_ADR		; R10 is a  pointer to the base address for the LEDs
				; ; ; ; ; ; ; ; MOV 		R3, #0xB0000000		; Turn off three LEDs on port 1  
				; ; ; ; ; ; ; ; MOV 		R4, #0x0000007C
				
; ; ; ; ; ; ; ; REPEAT2			ADD			R7,#100
				; ; ; ; ; ; ; ; TEQ			R6, #0
				; ; ; ; ; ; ; ; BEQ			DISPLAY
				
				; ; ; ; ; ; ; ; STR 		R3, [r10, #0x20]

				; ; ; ; ; ; ; ; STR 		R4, [R10, #0x40] 	; Turn off five LEDs on port 2 		
				
				; ; ; ; ; ; ; ; MOV			R5, #0x09C4
; ; ; ; ; ; ; ; HFQDELAY		BL			DELAY
				; ; ; ; ; ; ; ; SUBS		R5, #1
				; ; ; ; ; ; ; ; BNE			HFQDELAY


				; ; ; ; ; ; ; ; EOR			R3, R3, #0xB0000000
				; ; ; ; ; ; ; ; EOR			R4, R4, #0x0000007C

				; ; ; ; ; ; ; ; B			REPEAT2
				
				




; ; ; ; ; ; ; ; DISPLAY			MOV			R3, #0x0
				; ; ; ; ; ; ; ; MOV			R3, R7
				; ; ; ; ; ; ; ; MOV			R5, #0

; ; ; ; ; ; ; ; DISPLAY_ALL		BL			DISPLAYBITS				;First display the first 8 bits of the value in R3
				; ; ; ; ; ; ; ; LSR			R3,#8					;Right shift by 8 to get access to the next 8 bits
				; ; ; ; ; ; ; ; BL			twosecdelay				;Cause a 2 second delay between displays
				; ; ; ; ; ; ; ; ADD			R5,R5,#1				;If this count reaches 4, we have displayed all 4 pattern sets in R3 
				; ; ; ; ; ; ; ; TEQ			R5,#4
				; ; ; ; ; ; ; ; BEQ			fivesecdelay			;Once all 4 have been displayed, cause a 5 second delay
				; ; ; ; ; ; ; ; BNE			DISPLAY_ALL				;If all 4 not displayed, delay the next set of bits
				
; ; ; ; ; ; ; ; fivesecdelay	;MOV			R3,#0x00000000			;First clear R3-->So we can store the original pattern in here again
				; ; ; ; ; ; ; ; MOV			R5,#0x00000000			;clear R5-->So when we display again we can count to 4 again

				; ; ; ; ; ; ; ; MOV			R3,R7					;Store original pattern back in R3
				; ; ; ; ; ; ; ; MOV			R8,#0x0000FDE8			;Calling the delay this many times causes a 5 second delay roughly (Mathematically it is C350 but that doesnt work so well practically)
				; ; ; ; ; ; ; ; LSL			R8,#1
; ; ; ; ; ; ; ; fivecheck		BL			DELAY
				; ; ; ; ; ; ; ; SUBS		R8,#1
				; ; ; ; ; ; ; ; BNE			fivecheck
				
				; ; ; ; ; ; ; ; BEQ			DISPLAY_ALL				;Once delay has been called enough times, go back and display everything again
				

				
				; ; ; ; ; ; ; ; LDMFD		R13!,{R2,R3,R5,R6,R7,R12,R15}
				
; ; ; ; ; ; ; ; twosecdelay		STMFD		R13!,{R7,R8,R14}

				; ; ; ; ; ; ; ; MOV			R8,#0x0000C350			;Calling the delay this many times causes a 2 second delay roughly
; ; ; ; ; ; ; ; mini			BL			DELAY
				; ; ; ; ; ; ; ; SUBS		R8,#1
				; ; ; ; ; ; ; ; BNE			mini
				
				; ; ; ; ; ; ; ; LDMFD		R13!,{R7,R9,R15}
		; ; ; ; ; ; ; ; ;
		; ; ; ; ; ; ; ; ; Your main program can appear here 
		; ; ; ; ; ; ; ; ;
				
				
				
; ; ; ; ; ; ; ; ;*------------------------------------------------------------------- 
; ; ; ; ; ; ; ; ; Subroutine RNG ... Generates a pseudo-Random Number in R11 
; ; ; ; ; ; ; ; ;*------------------------------------------------------------------- 
; ; ; ; ; ; ; ; ; R11 holds a random number as per the Linear feedback shift register (Fibonacci) on WikiPedia
; ; ; ; ; ; ; ; ; R11 MUST be initialized to a non-zero 16-bit value at the start of the program
; ; ; ; ; ; ; ; ; R11 can be read anywhere in the code but must only be written to by this subroutine
; ; ; ; ; ; ; ; RNG 			STMFD		R13!,{R1-R3, R14} 	; Random Number Generator 
				; ; ; ; ; ; ; ; AND			R1, R11, #0x8000
				; ; ; ; ; ; ; ; AND			R2, R11, #0x2000
				; ; ; ; ; ; ; ; LSL			R2, #2
				; ; ; ; ; ; ; ; EOR			R3, R1, R2
				; ; ; ; ; ; ; ; AND			R1, R11, #0x1000
				; ; ; ; ; ; ; ; LSL			R1, #3
				; ; ; ; ; ; ; ; EOR			R3, R3, R1
				; ; ; ; ; ; ; ; AND			R1, R11, #0x0400
				; ; ; ; ; ; ; ; LSL			R1, #5
				; ; ; ; ; ; ; ; EOR			R3, R3, R1			; The new bit to go into the LSB is present
				; ; ; ; ; ; ; ; LSR			R3, #15
				; ; ; ; ; ; ; ; LSL			R11, #1
				; ; ; ; ; ; ; ; ORR			R11, R11, R3
				; ; ; ; ; ; ; ; LDMFD		R13!,{R1-R3, R15}

; ; ; ; ; ; ; ; ;*------------------------------------------------------------------- 
; ; ; ; ; ; ; ; ; Subroutine DELAY ... Causes a delay of 1ms * R0 times
; ; ; ; ; ; ; ; ;*------------------------------------------------------------------- 
; ; ; ; ; ; ; ; ; 		aim for better than 10% accuracy
; ; ; ; ; ; ; ; DELAY			STMFD		R13!,{R2, R14}
				; ; ; ; ; ; ; ; MOV			R2, #0x000000FF
				; ; ; ; ; ; ; ; ;LSL			R2, #9

				
; ; ; ; ; ; ; ; INNER			SUBS		R2, #1
				; ; ; ; ; ; ; ; BEQ			exitDelay
				; ; ; ; ; ; ; ; BNE			INNER
				
		; ; ; ; ; ; ; ; ;
		; ; ; ; ; ; ; ; ; Code to generate a delay of 1mS * R0 times
		; ; ; ; ; ; ; ; ;
; ; ; ; ; ; ; ; exitDelay		LDMFD		R13!,{R2, R15}

; ; ; ; ; ; ; ; DISPLAYBITS		
				; ; ; ; ; ; ; ; STMFD		R13!,{R1,R2,R7,R8,R9,R14}
				; ; ; ; ; ; ; ; AND			R1,R3,#0x000000FF	;ANDing with this will give us access to the Least Significant 8 bits in R3, and store the result in R1
				; ; ; ; ; ; ; ; EOR			R1,#0x000000FF		;this operation will flip first 8 bits... this is done because we want to represent bit"1" as ON and bit "0" as off... without this we would get the oppoiste result
				; ; ; ; ; ; ; ; LSL			R1,#24				;Left Shift to position 31,30,29,28,27,26,25,24
				
				; ; ; ; ; ; ; ; ;--------------NOW LETS FIRST AFFECT PORT 1 LEDs--------------------
				; ; ; ; ; ; ; ; RBIT		R2,R1				;Reverse order of bits and store in R2.
												; ; ; ; ; ; ; ; ;the top 3 bits, which need to be mapped to position 31, 39,38, are right now in 0,1,2 respectively
				
				; ; ; ; ; ; ; ; LSL			R2,#29				;LSL by 29 puts then back in 31,30,29
				; ; ; ; ; ; ; ; AND			R7, R2, #0x80000000	;This gives us access to the 31st bit by anding it with 1 in 31st bit and 0 in the rest

; ; ; ; ; ; ; ; ;Now we want access to the 29 and 28th bit! BUT these two are in the 30th and 29th bit location respectively! So LSR by 1!
				; ; ; ; ; ; ; ; LSR			R8,R2,#1			
; ; ; ; ; ; ; ; ;Now we can toggle these bit patterns!First add them to the 31st bit!
				; ; ; ; ; ; ; ; ADD			R9,R8,R7			
; ; ; ; ; ; ; ; ;Now you should have the appropriate pattern in the appropriate location from 31,..,29,28
				
				; ; ; ; ; ; ; ; STR			R9,[R10,#0x20]		;This should turn p1.31 ON... by writing 0 in the appropraite location--> Out Put: P31 on, other two off!

; ; ; ; ; ; ; ; ;---------------NOW LETS WORK ON PORT 2 LEDs----------------------------------------
				; ; ; ; ; ; ; ; ;A very similar logic follows
				; ; ; ; ; ; ; ; AND			R4,R1,#0x1F000000	;AND So only the relvant bits are active
				; ; ; ; ; ; ; ; LSL			R4,R4,#1			;LSL by one in prep for Reversing all the bits!
				; ; ; ; ; ; ; ; RBIT		R4,R4				;R4 now has our PATTERN! (after reversing)
				; ; ; ; ; ; ; ; STR			R4,[R10,#0x40]		;Write the pattern in the port to turn appropriate LEDs off!
				; ; ; ; ; ; ; ; LDMFD		R13!,{R1,R2,R7,R8,R9,R15}


				

; ; ; ; ; ; ; ; ;*------------------------------------------------------------------- 
; ; ; ; ; ; ; ; ; Interrupt Service Routine (ISR) for EINT3_IRQHandler 
; ; ; ; ; ; ; ; ;*------------------------------------------------------------------- 
; ; ; ; ; ; ; ; ; This ISR handles the interrupt triggered when the INT0 push-button is pressed 
; ; ; ; ; ; ; ; ; with the assumption that the interrupt activation is done in the main program
; ; ; ; ; ; ; ; ; EINT3_IRQHandler 	PROC 
					; ; ; ; ; ; ; ; ; STMFD 		R13!,{R0,R10,R14}				; Use this command if you need it  
					
					; ; ; ; ; ; ; ; ; MOV			R6, #0x0
					; ; ; ; ; ; ; ; ; LDR			R10, =CLEAR
					; ; ; ; ; ; ; ; ; MOV			R0, #0x00000400
					; ; ; ; ; ; ; ; ; STR			R0, [R10]
					
					
		; ; ; ; ; ; ; ; ; ;
		; ; ; ; ; ; ; ; ; ; Code that handles the interrupt 
		; ; ; ; ; ; ; ; ; ;
					; ; ; ; ; ; ; ; ; LDMFD 		R13!,{R0,R10,R15}				; Use this command if you used STMFD (otherwise use BX LR) 
					; ; ; ; ; ; ; ; ; ENDP

; ; ; ; ; ; ; ; ;*-------------------------------------------------------------------
; ; ; ; ; ; ; ; ; Below is a list of useful registers with their respective memory addresses.
; ; ; ; ; ; ; ; ;*------------------------------------------------------------------- 
; ; ; ; ; ; ; ; LED_BASE_ADR	EQU 	0x2009c000 		; Base address of the memory that controls the LEDs 
; ; ; ; ; ; ; ; PINSEL3			EQU 	0x4002C00C 		; Pin Select Register 3 for P1[31:16]
; ; ; ; ; ; ; ; PINSEL4			EQU 	0x4002C010 		; Pin Select Register 4 for P2[15:0]
; ; ; ; ; ; ; ; FIO1DIR			EQU		0x2009C020 		; Fast Input Output Direction Register for Port 1 
; ; ; ; ; ; ; ; FIO2DIR			EQU		0x2009C040 		; Fast Input Output Direction Register for Port 2 
; ; ; ; ; ; ; ; FIO1SET			EQU		0x2009C038 		; Fast Input Output Set Register for Port 1 
; ; ; ; ; ; ; ; FIO2SET			EQU		0x2009C058 		; Fast Input Output Set Register for Port 2 
; ; ; ; ; ; ; ; FIO1CLR			EQU		0x2009C03C 		; Fast Input Output Clear Register for Port 1 
; ; ; ; ; ; ; ; FIO2CLR			EQU		0x2009C05C 		; Fast Input Output Clear Register for Port 2 
; ; ; ; ; ; ; ; IO2IntEnf		EQU		0x400280B4		; GPIO Interrupt Enable for port 2 Falling Edge 
; ; ; ; ; ; ; ; ISER0			EQU		0xE000E100		; Interrupt Set-Enable Register 0
; ; ; ; ; ; ; ; CLEAR			EQU		0x400280AC

				; ; ; ; ; ; ; ; ALIGN 

				; ; ; ; ; ; ; ; END 
