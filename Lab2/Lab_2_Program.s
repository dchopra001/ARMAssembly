;*----------------------------------------------------------------------------
;* Name:    Lab_1_program.s 
;* Purpose: This code flashes one LED at approximately 1 Hz frequency 
;* Author: Eric Praetzel and Rasoul Keshavarzi 
;*----------------------------------------------------------------------------*/
		THUMB 		; Declare THUMB instruction set 
                AREA 		My_code, CODE, READONLY 	; 
                EXPORT 		__MAIN 		; Label __MAIN is used externally q
		ENTRY 
__MAIN
; The following lines are similar to Lab-1 but use a defined address to make it easier.
; They just turn off all LEDs 
		MOV 		R2, #0xC000
		MOV 		R3, #0xB0000000	; Turn off three LEDs on port 1  
		MOV 		R4, #0x0
		MOVT 		R4, #0x2009
		ADD 		R4, R4, R2 		; 0x2009C000 
		STR 		R3, [r4, #0x20]
		MOV 		R3, #0x0000007C
		STR 		R3, [R4, #0x40] 	; Turn off five LEDs on port 2 

ResetLUT
		LDR         R5, =InputLUT            ; assign R5 to the address at label LUT

NextChar
		BL			LED_OFF
		MOV			R6,#3		;3 dots of delay before each character
		BL			MultipleDelay
        LDRB        R0, [R5]			; Read a character to convert to Morse... WILL START BY LOADING D,R,I,B
        ADD         R5, #1              ; point to next value for number of delays, jump by 1 byte
		TEQ         R0, #0              ; If we hit 0 (null at end of the string) then reset to the start of lookup table
		BNE			ProcessChar				; If we have a character process it
		
		;the section below just resets to the startt of the lookup table
		MOV		R6, #4		; delay 4 extra spaces (7 total) between words...the last letter will cause a 3 space delay?
		BL		MultipleDelay
		BEQ         ResetLUT  ;if line 28 is true, this branch will be executed

ProcessChar	

		BL		CHAR2MORSE	; convert ASCII to Morse pattern in R1		

;		Loop until we have a 1 bit to send 
;	But this is confusing as we're shifting left, but the data is in the lowest 16 bits, so test at bit 16 for 1 or 0
;
		MOV		R1,#0xF				;16th bit value set in R1--->COUNT
		MOV		R2, #0x0		;Flag with value set to 0
CHECKR1		
		CMP		R1,#0x0			;Check value in R1 to see if its zero
		BLT		NextChar		;If R1 is zero, the led has finished flashing... therefore, return to attain the next character
		BNE		CHECKR0
				
		;MOV		R6, #0x10000	; Init R6 with the value for the bit which we wish to test
		;LSL		R1, R1, #1	; shift R1 right by 1, store in R1
		;ANDS		R7, R1, R6	; R7 gets R1 AND R6, Zero bit gets set if it's zero
		;BEQ		; branch somewhere it's zero
		;BNE		; branch somewhere - it's not zero

		;....  lots of code
		;B 		;somewhere in your code! 	; This is the end of the main program 

CHECKR0

		MOV		R7,#0x8000					;this is how we will test the R1th bit of R0
		ANDS	R9,R7,R0				;AND R0 and R7 to see if 1 or 0
		;CMN		R1,#2
		LSL		R0,#1					;Left shift R0 by one for next round of anding (so u can AND another digit later!)
		BEQ		CHECKFLAG
		
		BLNE		LED_ON
		MOV			R6,#0x00000001
		BL			MultipleDelay
		SUB			R1,#1			;R1-1
		B			CHECKR1         ;Branch to check what R1 is again
		

CHECKFLAG	
		CMP		R2,#0x0					;Chck if Flag is 0 or 1
		BEQ		FLAGISZERO				;BRANCH TO FLAG IS ZERO SUBROUTINE
		BNE		FLAGISONE				;BRANCH TO FLAG IS ONE SUBROUTINE
		
		
FLAGISZERO
		;MOV		R6,#0x00000001			;SEt R6 to one so we can delay for one dot
		;BL		MultipleDelay			;Cause delay for 1 dot
		SUB		R1,#1					;R1-1 so we can compare it again
		B		CHECKR1					;Go back to check R1!
		
FLAGISONE	
		BL		LED_OFF					;Turn off LED
		MOV		R6,#1					;So delay is 1 dot
		BL		MultipleDelay
		SUB		R1,#1					;So we can check r1 again
		B		CHECKR1



; Subroutines
;
;			convert ASCII character to Morse pattern
;			pass ASCII character in R0, output in R1
;			index into MorseLuT must be by steps of 2 bytes
CHAR2MORSE	STMFD		R13!,{R14}	; push Link Register (return address) on stack
		;
		
		LDR		R11, =MorseLUT		;load the first mem. address of morse table in R11. 
		SUB		R0,R0,#0x41			;this will get the index number of the letter so we can look for it in the Morse table
		LSL		R0, #1				;Left shifting by 1 will multiply the  index in 2. This is done because each character takes 2 bytes in memory
		LDRH	R0, [R11,R0]		;R11 points to index 0 which is A..  adding R0 will get the index for the character in the table and then store the contents of that index in R12 
									;R12 HAS THE MORSE PATTERN!
		;MOV		R0,R12				;Mov into R0, the morse pattern (which is stored in R12 actually)
		;.. add code here to convert the ASCII to an index (subtract 41) and lookup the Morse patter in the Lookup Table
		;
		LDMFD		R13!,{R15}	; restore LR to R15 the Program Counter to return


; Turn the LED on, but deal with the stack in a simpler way
; NOTE: This method of returning from subroutine (BX  LR) does NOT work if subroutines are nested!!
;
LED_ON		STMFD		R13!,{R14}	; push Link Register (return address) on stack
			;push 		{r3-r4}		; preserve R3 and R4 on the R13 stack
			MOV			R2,#0x10000000		;Set flag to 1
			MOV			R3,#0xA0000000
			STR			R3,[R4,0x20] ;Store A  in 2009c020,turning the LED ON 	
					;... insert your code here

			LDMFD		R13!,{R15}	; restore LR to R15 the Program Counter to return		
			;pop 		{r3-r4}
			;BX 		LR		; branch to the address in the Link Register.  Ie return to the caller

LED_OFF	   	STMFD		R13!,{R3, R14}	; push R3 and Link Register (return address) on stack
			MOV			R3, #0xB0000000 ;Move the OFF Code in  R3
			STR			R3,[R4,0x20] ;Turn OFF the led by putting B in 2009c020
		;... insert your code here
			LDMFD		R13!,{R3, R15}	; restore R3 and LR to R15 the Program Counter to return

;	Delay 500ms * R0 times
;	Use the delay loop from Lab-1 but loop R0 times around
;
;DELAY		STMFD		R13!,{R14}	;Removed R2,R14
MultipleDelay		STMFD		R13!,{R14}	;Removed R2,R14
SECOND				CMP		R6, #0		; test R0 to see if it's 0 - set Zero flag so you can use BEQ, BNE
					SUB 	R6, #1		;R0 - 1
					BNE		DELAYSTART	;if not zero, run delay again
					BEQ		exitDelay	;if 0, exit delay method

					B		SECOND ;Branch to Multiple Delay to test again
					
DELAYSTART	MOV			R10, 0xFFFF   ;Move a large value in R10    DELAYSTART->NESTED SUBROUTINE TO AVOID CONTINUALLY UPDATING STACK 
			LSL 		R10, #2
loop		SUBS		R10, #1			;R9-1
			;CMP			R10, #0x0			;Compare to see if R9 is zero yet
			BNE			loop
			BEQ			SECOND
			
			;... insert your code here
exitDelay		LDMFD		R13!,{R15} ;Removed R2,R14

;
; Data used in the program
		ALIGN				; make sure things fall on word addresses
InputLUT	DCB		"THEP", 0	; strings must be stored, and read, as BYTES

		ALIGN				; make sure things fall on word addresses
MorseLUT 
		DCW 	0x17, 0x1D5, 0x75D, 0x75 	; A, B, C, D
		DCW 	0x1, 0x15D, 0x1DD, 0x55 	; E, F, G, H
		DCW 	0x5, 0x1777, 0x1D7, 0x175 	; I, J, K, L
		DCW 	0x77, 0x1D, 0x777, 0x5DD 	; M, N, O, P
		DCW 	0x1DD7, 0x5D, 0x15, 0x7 	; Q, R, S, T
		DCW 	0x57, 0x157, 0x177, 0x757 	; U, V, W, X
		DCW 	0x1D77, 0x775 			; Y, Z

; One can also define an address using the EQUate directive
;
LED_ADR		EQU 	0x2009c020 		; Address of the memory that controls the LED 

		END 
