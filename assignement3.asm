	   title     "Name of program"
	   list      p=16f877a
	   include   "p16f877a.inc"

	__CONFIG _CP_OFF & _WDT_OFF & _BODEN_OFF & _PWRTE_ON & _HS_OSC & _WRT_OFF & _LVP_OFF & _CPD_OFF

; '__CONFIG' directive is used to embed configuration data within .asm file.
; The lables following the directive are located in the respective .inc file.
; See respective data sheet for additional information on configuration word.
; Remember there are TWO underscore characters before the word CONFIG.

;*******************************************************************
;
;  name  ::   Lightmeter
;  ===================================
;
; Description:  The program reads an analog voltage value on PORTA bit1 at every time
;			   when the user push the button connected on PORTB bit 1
;              it produces a PWM signal on the pin RC2 else
;   		   it sends a number between 0-9 on PORTD			  
;	
; Method
;    
; Version
;    Yassine Korachi    V1.0    December 2013
;
; History
;    /
;
;******************************************************************* 
;*******************************************************************    
;
; Constant declarations
; =====================
;
;
;******************************************************************* 
;*******************************************************************    
;
; Variable declarations  : User RAM area starts at location h'20' for PIC16F877a
; =====================
;
w_temp		equ	h'7D'		; variable used for context saving 
status_temp	equ	h'7E'		; variable used for context saving
pclath_temp	equ	h'7F'		; variable used for context saving

cnt1      	 		eq		h'21'
copyofadresh			equ		h'22'
copyofadresl			equ 		h'23'
delcnt				equ		h'24'
result1				equ 		h'25'
result0				equ		h'26'
resultl				equ		h'27'
resultl0			equ		h'28'
resulth				equ		h'29'
resulth0			equ		h'30'
final				equ		h'31'
delcnt1				equ		h'32'
cnt2				equ		h'33'
ccpr1lvalue			equ		h'33'
ccp1convalue			equ		h'34'
t2convalue			equ		h'35'
pr2value			equ		h'A1'

first				equ		h'36'
myvalue				equ     	h'37'
last				equ		h'38'
second				equ		h'39'
third				equ		h'40'
fourth				equ		h'41'
fifth				equ		h'42'
sixth				equ		h'43'
seventh				equ		h'44'
eighth				equ		h'45'
ninth				equ		h'46'
tempo				equ		h'47'

;
;*******************************************************************
;******************************************************************* 
; Initial system vector.
;   
	org     h'00'                   ; initialise system restart vector
	clrf 	STATUS
	clrf 	PCLATH			; needed for TinyBootloader functionality
	goto    start

;******************************************************************* 
;******************************************************************* 
; interrupt vector
;
	org	h'04'
	goto	int_routine

;******************************************************************* 
;******************************************************************* 
;
; System subroutines.
;  
	org     h'05'           ; start of program space
;
;******************************************************************* 
;******************************************************************* 
; System functions
;******************************************************************* 
;
;* Init : initialise I/O ports and variables
;  ====
; Notes
;  In this part we are going to use PORTC and PORTD as outputs and PORTB as inputs
; We are also going to set PORTA to act as an ADC and to use AN1 as an ADC input
Init    
	bsf     STATUS, RP0        ; enable page 1 register set
	bcf		STATUS, RP1	
	movlw	b'10001000'
	movwf	ADCON1		   ; set PORTA to be analog input VREF+ and vref- and right justified

	movlw   b'00000000'                 
	movwf   TRISB              ; set port B as input
	movlw   b'00000000'                 
	movwf   TRISC              ; set port C as outputs
	movlw   b'00000000'                 
	movwf   TRISD              ; set port D ad outputs	
	
	bcf     STATUS, RP0        ; back to page 0 register set
	movlw	b'10001001'
	movwf	ADCON0	           ;Set PORTA to allows AN1 as analog input and to power the DAC
	clrf PORTB
	clrf PORTC
	return
;******************************************************************* 
;
;  int_routine : routine to handle the single interrupt
;  ===========
; Notes
;      ............................
;
int_routine
	movwf   w_temp            ; save off current W register contents
	movf	STATUS,w          ; move status register into W register
	movwf	status_temp       ; save off contents of STATUS register
	movf	PCLATH,w	  ; move pclath register into w register
	movwf	pclath_temp	  ; save off contents of PCLATH register
;
; Your interrupt code goes HERE
;
	movf	pclath_temp,w	  ; retrieve copy of PCLATH register
	movwf	PCLATH		  	  ; restore pre-isr PCLATH register contents
	movf    status_temp,w     ; retrieve copy of STATUS register
	movwf	STATUS            ; restore pre-isr STATUS register contents
	swapf   w_temp,f
	swapf   w_temp,w          ; restore pre-isr W register contents
	retfie 
;*******************************************************************    
                       
;*******************************************************************
;* del100us : Provide a 100 microsecond delay.
; Memory used
;    delcnt
; Calls
;    none
del100us				; delay of 100us   call = 2uS     (2)
		movlw   d'23' 		;      + 1      	(3) move the value 23 to W register
    	movwf   delcnt			;      + 1      	(4) put W register in delcnt
loop1	nop				;      + 1     		(5) do nothing during 1us
     	decfsz  delcnt,d'1'		;      + 1    		(6) delcnt decrements by 1 each time and 
	    goto    loop1		;      + 1    		(7) return to label loop2. When it reach 0
     	return				;      + 2      	(98) it go to return
;******************************************************************* 
;* del500us : Provide a 500 microsecond delay.
; Memory used
;    delcnt1
; Calls
;    none
del500us				; delay of 500us   call = 2uS    (   2)
		movlw   d'99'		;      + 1      (   3) move the value 99 to W register
    	movwf   delcnt1			;      + 1      (   4) put W register in delcnt
loop2	nop				;      + 99     ( 103) do nothing during 1us
        nop				;      + 99     ( 202) do nothing during 1us
     	decfsz  delcnt1,d'1'		;      + 100    ( 302) delcnt decrements by 1 each time and 
	    goto    loop2		;      + 196    ( 498) return to label loop1. When it reach 0 
     	return				;      + 2      ( 500) it jumps the next instruction it go to return 
;*******************************************************************
;* del5ms : Provide a 5 milliseconds delay.
; Memory used
;    cnt2
; Calls
;    del500us
del5ms; delay of 5ms
		movlw d'101'		;move the value 101 to W register
		movwf cnt2		;put W register in cnt1
loop40	decfsz cnt2,d'1'		;cnt1 decrements by 1 
		goto loop30		;go to label loop6
		goto forexit		;go to label forexit2
loop30	call del500us			;call the subroutine del500us
		goto loop40		;go to label loop5
forexit return;
;*******************************************************************
;* addition16bits : Do an addition over 16 bits by using two registers.
; Memory used
;    resultl, copyofadresl, resultl0,resulth,copyofadresh,resulth0
; Calls
;    none
addition16bits
		movf copyofadresl,W
		movwf resultl
		movf copyofadresh,W
		movwf resulth
		movf resultl,W
		addwf resultl0
		btfsc STATUS,C
		incf resulth,F
		movf resulth,W
		addwf resulth0
		return
;****************************************************************** 
;* divideby256 : Do a division by 256 and provide the ADC converted value
;				range:(0-40)
; Memory used
;    resulth0,resultl0,final
; Calls
;    none
divideby256	
		btfsc resultl0,d'7'
		incf resulth0,d'1'
		movf resulth0,W
		movwf final
		return
;****************************************************************** 
;* getadcvalue : Provide a value from the ADC and store it in two registers
;				range:(0-1024)
; Memory used
;    copyofadresl,copyofadresh
; Calls
;    del100us
getadcvalue
		call del100us
		bsf ADCON0,GO			;start conversion
		call del100us			;call subroutine del100us
waitadc	btfsc ADCON0,NOT_DONE			;wait until the conversion is done
		goto waitadc			;go to label waitadc if still converting
		movf ADRESH,W			;move the content of adresh into w register
		movwf copyofadresh		;move w register into adcout+1
		bsf     STATUS, RP0     	; enable page 1 register set
		bcf		STATUS, RP1	
		movf ADRESL,W			;move the content of adresh into w register
		bcf     STATUS, RP0     	;come back on page 0 register set
		movwf copyofadresl		;move w register into adcout
		return
;****************************************************************** 
;* multiplyby10 : Multiply the ADC value by 10 doing 10 times an addition over 16 bits
;		  range:(0-10240)
; Memory used
;    cnt1
; Calls
;    addition16bits
multiplyby10
		movlw d'11'			;move the value 10 to W register ! do it 9 times
		movwf cnt1			;put W register in cnt1
loop4	decfsz cnt1,d'1'			;cnt1 decrements by 1
		goto loop5			;go to label loop8
		goto forexit3			;go to forexit3
loop5	call addition16bits			;call the subroutine del500us
		goto loop4			;go to label loop7
forexit3
		return
;****************************************************************** 
;* setpwm : Provide a PWM signal on RC2
; Memory used
;    pr2value,ccp1convalue,t2convalue,ccpr1lvalue
; Calls
;    none
setpwm
	bsf  STATUS, RP0        
	movf pr2value,W
	movwf PR2					;put this value in PR2
	bcf  STATUS, RP0        			;back to page 0 register set
	clrf CCPR1L					;clear register CCPR1L
	movf ccp1convalue,W
	movwf CCP1CON
	bsf     STATUS, RP0     			;enable page 1 register set
	bcf TRISC,d'2'					;clear PORTC bit 2
	bcf STATUS,RP0					;back to page0
	movf t2convalue,W
	movwf T2CON
	clrf TMR2
	bsf TMR2,TMR2ON
	movf ccpr1lvalue,W
    movwf   CCPR1L					;set duty cycle at 50%
	return
;****************************************************************** 
;* clrvalues : Clear all the parameters values of the PWM signal to switch it off
; Memory used
;    pr2value,ccp1convalue,t2convalue,ccpr1lvalue
; Calls
;    none
clrvalues
	bsf STATUS,RP0
	movlw d'0'
	movwf pr2value
	bcf STATUS,RP0
	movlw d'0'
	movwf ccp1convalue
	movlw	d'0'			
	movwf ccpr1lvalue
	movlw	d'0'			 
	movwf t2convalue
	return
;****************************************************************** 
;* checkvalue : From the ADC converted value (0-40) this subroutine decides  
;		which PWM period is set and which number is displayed on the 7 
;		segment display.	
; Memory used
;    first,second,third,fourth,fifth,sixth,seventh,eighth,ninth,last
;	 final, myvalue
; Calls
;    value0,value1,value2, value3, value4, value5 value6, value7,value8, 
;    value9
checkvalue
	movlw d'4'
	movwf first
	movlw d'8'
	movwf second
	movlw d'12'
	movwf third
	movlw d'16'
	movwf fourth
	movlw d'20'
	movwf fifth
	movlw d'24'
	movwf sixth
	movlw d'28'
	movwf seventh
	movlw d'32'
	movwf eighth
	movlw d'36'
	movwf ninth	
	movlw d'41'
	movwf last

	call getadcvalue
	call multiplyby10
	call divideby256

	movf final,W
	movwf myvalue
	movf myvalue,W
	subwf first
	btfsc STATUS,C
	goto label10
	goto label11
label10	call value0
		goto theexit	
label11	goto ifexit10	
ifexit10

	movf final,W
	movwf myvalue
	movf myvalue,W
	subwf second
	btfsc STATUS,C
	goto label30
	goto label31	
label30	call value1
		goto theexit
label31	goto ifexit12
ifexit12

	movf final,W
	movwf myvalue
	movf myvalue,W
	subwf third
	btfsc STATUS,C
	goto label40
	goto label41	
label40	call value2
		goto theexit
label41	goto ifexit13
ifexit13

	movf final,W
	movwf myvalue
	movf myvalue,W
	subwf fourth
	btfsc STATUS,C
	goto label50
	goto label51	
label50	call value3
		goto theexit
label51	goto ifexit14
ifexit14

	movf final,W
	movwf myvalue
	movf myvalue,W
	subwf fifth
	btfsc STATUS,C
	goto label60
	goto label61	
label60	call value4
		goto theexit
label61	goto ifexit15
ifexit15

	movf final,W
	movwf myvalue
	movf myvalue,W
	subwf sixth
	btfsc STATUS,C
	goto label70
	goto label71	
label70	call value5
		goto theexit
label71	goto ifexit16
ifexit16

	movf final,W
	movwf myvalue
	movf myvalue,W
	subwf seventh
	btfsc STATUS,C
	goto label80
	goto label81	
label80	call value6
		goto theexit
label81	goto ifexit17
ifexit17

	movf final,W
	movwf myvalue
	movf myvalue,W
	subwf eighth
	btfsc STATUS,C
	goto label90
	goto label91	
label90	call value7
		goto theexit
label91	goto ifexit18
ifexit18

	movf final,W
	movwf myvalue
	movf myvalue,W
	subwf ninth
	btfsc STATUS,C
	goto label100
	goto label101	
label100	call value8
			goto theexit
label101	goto ifexit19
ifexit19

	movf final,W
	movwf myvalue
	movf myvalue,W
	subwf last
	btfsc STATUS,C
	goto label20
	goto label21	
label20	call value9
		goto theexit
label21	goto ifexit11
ifexit11


theexit
	return
;****************************************************************** 
;****************************************************************** 
;* value0 : Set PWM parameter and 7 segment display for the first 
;	    range of adc value (0-3) value:0 , frequency:2700Hz
; Memory used
;    pr2value,ccp1convalue,t2convalue,ccpr1lvalue,tempo
; Calls
;    none
value0
	bsf STATUS,RP0
	movlw d'92'
	movwf pr2value
	bcf STATUS,RP0
	movlw d'28'
	movwf ccp1convalue
	movlw	d'46'			
	movwf ccpr1lvalue
	movlw	d'5'			 
	movwf t2convalue
	movlw b'0000'
	movwf tempo
	return
;****************************************************************** 
;****************************************************************** 
;* value1 : Set PWM parameter and 7 segment display for the first 
;	    range of adc value (4-7) value:1 , frequency:2250Hz
; Memory used
;    pr2value,ccp1convalue,t2convalue,ccpr1lvalue,tempo
; Calls
;    none
value1
	bsf STATUS,RP0
	movlw d'110'
	movwf pr2value
	bcf STATUS,RP0
	movlw d'28'
	movwf ccp1convalue
	movlw	d'55'			
	movwf ccpr1lvalue
	movlw	d'5'			 
	movwf t2convalue
	movlw b'0001'
	movwf tempo
	return
;******************************************************************
;****************************************************************** 
;* value2 : Set PWM parameter and 7 segment display for the first 
;	    range of adc value (8-11) value:2 , frequency:2000Hz
; Memory used
;    pr2value,ccp1convalue,t2convalue,ccpr1lvalue,tempo
; Calls
;    none 
value2
	bsf STATUS,RP0
	movlw d'124'
	movwf pr2value
	bcf STATUS,RP0
	movlw d'28'
	movwf ccp1convalue
	movlw	d'62'			
	movwf ccpr1lvalue
	movlw	d'5'			 
	movwf t2convalue
	movlw b'0010'
	movwf tempo
	return
;******************************************************************
;****************************************************************** 
;* value3 : Set PWM parameter and 7 segment display for the first 
;	    range of adc value (12-15) value:3 , frequency:1750Hz
; Memory used
;    pr2value,ccp1convalue,t2convalue,ccpr1lvalue,tempo
; Calls
;    none 
value3
	bsf STATUS,RP0
	movlw d'142'
	movwf pr2value
	bcf STATUS,RP0
	movlw d'28'
	movwf ccp1convalue
	movlw	d'71'			
	movwf ccpr1lvalue
	movlw	d'5'			 
	movwf t2convalue
	movlw b'0011'
	movwf tempo
	return
;****************************************************************** 
;****************************************************************** 
;* value4 : Set PWM parameter and 7 segment display for the first 
;	    range of adc value (16-19) value:4 , frequency:1500Hz
; Memory used
;    pr2value,ccp1convalue,t2convalue,ccpr1lvalue,tempo
; Calls
;    none 
value4
	bsf STATUS,RP0
	movlw d'166'
	movwf pr2value
	bcf STATUS,RP0
	movlw d'28'
	movwf ccp1convalue
	movlw	d'83'			
	movwf ccpr1lvalue
	movlw	d'5'			 
	movwf t2convalue
	movlw b'0100'
	movwf tempo
	return
;****************************************************************** 
;****************************************************************** 
;* value5 : Set PWM parameter and 7 segment display for the first 
;	     range of adc value (20-23) value:5 , frequency:1250Hz
; Memory used
;    pr2value,ccp1convalue,t2convalue,ccpr1lvalue,tempo
; Calls
;    none 
value5
	bsf STATUS,RP0
	movlw d'166'
	movwf pr2value
	bcf STATUS,RP0
	movlw d'28'
	movwf ccp1convalue
	movlw	d'83'			
	movwf ccpr1lvalue
	movlw	d'5'			 
	movwf t2convalue
	movlw b'0101'
	movwf tempo
	return
;****************************************************************** 
;****************************************************************** 
;* value6 : Set PWM parameter and 7 segment display for the first 
;	    range of adc value (24-27) value:6 , frequency:1000Hz
; Memory used
;    pr2value,ccp1convalue,t2convalue,ccpr1lvalue,tempo
; Calls
;    none 
value6
	bsf STATUS,RP0
	movlw d'49'
	movwf pr2value
	bcf STATUS,RP0
	movlw d'60'
	movwf ccp1convalue
	movlw	d'24'			
	movwf ccpr1lvalue
	movlw	d'7'			 
	movwf t2convalue
	movlw b'0110'
	movwf tempo
	return
;****************************************************************** 
;****************************************************************** 
;* value7 : Set PWM parameter and 7 segment display for the first 
;	    range of adc value (28-31) value:7 , frequency:750Hz
; Memory used
;    pr2value,ccp1convalue,t2convalue,ccpr1lvalue,tempo
; Calls
;    none 
value7
	bsf STATUS,RP0
	movlw d'249'
	movwf pr2value
	bcf STATUS,RP0
	movlw d'60'
	movwf ccp1convalue
	movlw	d'124'			
	movwf ccpr1lvalue
	movlw	d'5'			 
	movwf t2convalue
	movlw b'0111'
	movwf tempo
	return
;****************************************************************** 
;****************************************************************** 
;* value8 : Set PWM parameter and 7 segment display for the first 
;	    range of adc value (32-35) value:8 , frequency:500Hz
; Memory used
;    pr2value,ccp1convalue,t2convalue,ccpr1lvalue,tempo
; Calls
;    none 
value8
	bsf STATUS,RP0
	movlw d'82'
	movwf pr2value
	bcf STATUS,RP0
	movlw d'28'
	movwf ccp1convalue
	movlw	d'41'			
	movwf ccpr1lvalue
	movlw	d'7'			 
	movwf t2convalue
	movlw b'1000'
	movwf tempo
	return

;****************************************************************** 
;****************************************************************** 
;* value9 : Set PWM parameter and 7 segment display for the first 
;           range of adc value (36-40) value:9 , frequency:250Hz
; Memory used
;    pr2value,ccp1convalue,t2convalue,ccpr1lvalue,tempo
; Calls
;    none 
value9
	bsf STATUS,RP0
	movlw d'249'
	movwf pr2value
	bcf STATUS,RP0
	movlw d'60'
	movwf ccp1convalue
	movlw	d'124'			
	movwf ccpr1lvalue
	movlw	d'7'			 
	movwf t2convalue
	movlw b'1001'
	movwf tempo
	return
;****************************************************************** 
;******************************************************************
; MAIN program: When the switch on port B bit 1 a PWM signal is produced
;               on bit PORTC bit 2 otherwise a number is displayed on the
;		4 first bit(0,1,2,3) on PORTD.
; Calls
;    setpwm, del5ms, clrvalues, checkvalues
; ============
;
start   
    call    Init;			call init subroutine
mainloop
	call checkvalue
	btfsc PORTB,d'1'
	goto switchon
	goto switchoff

switchon	
			;call setvalues
			call setpwm
			call del5ms	
			clrf PORTD		
			goto loop
switchoff	
;*****disable PWM
			movf tempo,W
			movwf PORTD
			call clrvalues
			call setpwm
			call del5ms
			bcf TMR2,TMR2ON
;*************************************************************
			
loop	clrf resulth0
		clrf resultl0
		clrf final

goto	mainloop		;go to label mainloop
;
; Program complete.
;
	END


