;----------------------------------------------------------------------------
; Name    Lab_1_program.s 
; Purpose This code flashes one LED at approximately 1 Hz frequency 
; Author 	Rasoul Keshavarzi 
;----------------------------------------------------------------------------
				THUMB 		; Declare THUMB instruction set 
                AREA 		My_code, CODE, READONLY 	; 
                EXPORT 		__MAIN 		; Label __MAIN is used externally q
				ENTRY 
__MAIN
; The following operations can be done in simpler methods. They are done in this 
; way to practice different memory addressing methods. 
; MOV moves into the lower word (16 bits) and clears the upper word
; MOVT moves into the upper word
; show several ways to create an address � using a fixed offset and register as offset
;   and several examples are used below
; NOTE MOV can move ANY 16-bit, and only SOME 16-bit, constants into a register
		MOV 		R2, #0xC000		; move 0xC000 into R2
		MOV 		R4, #0x0		; init R4 register to 0 to build address
		MOVT 		R4, #0x2009		; assign 0x20090000 into R4
		ADD 		R4, R4, R2


		;LDR			R4, 0x2009C000
		MOV 		R3, #0x0000007C	; move initial value for port P2 into R3 
		STR 		R3, [R4, #0x40] 	; Turn off five LEDs on port 2 
		MOV 		R3, #0xB0000000	; move initial value for port P1 into R3
		STR 		R3, [R4, #0x20]	; Turn off three LEDs on Port 1 using an offset
		MOV 		R2, #0x20		; put Port 1 offset into R2
		MOV 		R0, #0xFFFF 		; Initialize R0 lower word for countdown

		
				
loop
		SUBS 		R0, #1 			; Decrement r0 and set N,Z,V,C status bits
		CMP			R0, #0x00000000 ;compare to see if R0 is 0 yet       
		BNE			loop			;if branch is not equal to zero, go back to loop and minus 1 again@!!!!!~!
		EOR			R3, #0x10000000		;exclusize or to get address A
		
		;
		; 	Approximately five lines of code
		; 	are required to complete the program 
		;
		
		STR 		R3, [R4, R2] 		; Toggle the bit 28 or port 1
		MOV			R0, #0xFFFFFF

		B 			loop		; This branch needs to be fixed!

 		END 
		