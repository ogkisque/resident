.model tiny
.code
.286
org 100h

locals @@

Start:
		jmp Main

;----------------------------------------------------
; Writes a line to video mem
; Entry:	ah - color attribute
;		si - start position of style
;		cl - length of border
;		di - start position of line
; Assumes:	es - 0b800h
; Destr: 	al, cl, di, si

PrintLine	proc

		lodsb		; al = ds:[si++]
		stosw		; es:[di] = ax; di += 2
		sub cl, 2d
		lodsb
		rep stosw	; while (cx--) es:[di] = ax; di += 2
		lodsb
		stosw

		ret
		endp
;-------------------------------------------------
; Writes border to video mem
; Entry:	
; Assumes:	es - 0b800h
; Destr:	ax, bx, cx, si, di, ds

PrintBorder	proc
		mov si, offset StringStyle
		push cs
		pop ds
		mov di, 0

		xor ch, ch
		mov cl, 20d

		call PrintLine
		mov bl, 13d
		@@loop:
			mov cl, 20d
			add di, 160d	
			sub di, cx
			sub di, cx		; new line

			call PrintLine
			sub si, 3d
			dec bl
			cmp bl, 0
			jg @@loop

		add si, 3d
		mov cl, 20d
		add di, 160d
		sub di, cx
		sub di, cx
		call PrintLine

		mov di, 0

		ret
		endp	
;-------------------------------------------------
; Convert from value to ascii
; Entry:	
; Assumes:	
; Destr:	al

Convert	proc
		
		cmp al, 10d 
		jge @@letter
		add al, 48d
		jmp @@exit

		@@letter:
		add al, 55d

		@@exit:
		ret
		endp	
;-------------------------------------------------
; Writes values from stack to video mem
; Entry:	di - position 
; Assumes:	es - 0b800h, bx - value
; Destr:	ax, di

PrintReg	proc

		mov al, bh
		shr al, 4d
		call Convert
		mov es:[di], ax
		inc di
		inc di

		mov al, bh
		and al, 00001111b
		call Convert
		mov es:[di], ax
		inc di
		inc di

		mov al, bl
		shr al, 4d
		call Convert
		mov es:[di], ax
		inc di
		inc di

		mov al, bl
		and al, 00001111b
		call Convert
		mov es:[di], ax
		inc di
		inc di

		ret
		endp	
;-------------------------------------------------
; Writes values of registers to video mem
; Entry:	
; Assumes:	es - 0b800h
; Destr:	ax, bx, di, si, bp

PrintRegs	proc
		
        push bp
		mov bp, sp
		add bp, 6d

		mov cx, 13d
		mov di, 172d
		mov si, offset RegsNames
		@@loop1:
			lodsb
			stosw
			lodsb
			stosw
			inc di
			inc di

			mov bx, [bp]
			call PrintReg

			add di, 146d
			add bp, 2d 
			loop @@loop1

        pop bp
		ret
		endp	
;-------------------------------------------------
; Writes border with regs
; Entry:	
; Assumes:	es - 0b800h
; Destr:	

HotKey	proc

		push 0b800h
		pop es

		push cs
		pop ds

        mov ah, 4ah
        call PrintBorder
        call PrintRegs

		ret
		endp	
;-------------------------------------------------
; Handles the keystroke

MyInt09	proc

		push sp ss es ds bp si di dx cx bx ax

		in al, 60h
		cmp al, 58h			; f12
		jne @@continue

        call HotKey
		xor cs:NeedUpdate, 1d

		@@continue:

		in al, 61h
		or al, 80h			; first bit = 1
		out 61h, al
		and al, not 80h		; first bit = 0
		out 61h, al

		mov al, 20h
		out 20h, al

        pop ax bx cx dx di si bp ds es ss sp

		db 0eah
		OldOfs09 dw 0
		OldSeg09 dw 0		; do old handle 
		iret

		endp
;-------------------------------------------------
; Handles the timer

MyInt08	proc

		push sp ss es ds bp si di dx cx bx ax

		;call HotKey
		mov al, cs:NeedUpdate
		cmp al, 1d			
		jne @@continue

        call HotKey

		@@continue:

		mov al, 20h
		out 20h, al

        pop ax bx cx dx di si bp ds es ss sp

		db 0eah
		OldOfs08 dw 0
		OldSeg08 dw 0		; do old handle 
		iret

		endp
;-------------------------------------------------
; Exit of programm
; Entry:
; Assumes:
; Destr:	ax

ExitProg	proc
		mov ax,  4c00h
		int 21h
		ret
		endp
;-------------------------------------------------
; TSR exit of programm
; Entry:
; Assumes:
; Destr:	ax, dx

ExitProgTSR	proc

		mov ax, 3100h
		mov dx, offset EndProgramm
		shr dx, 4				; dx /= 4
		inc dx
		int 21h					; TSR
		ret

		endp
;-------------------------------------------------
Main:
		mov ax, 3509h
		int 21h
		mov OldOfs09, bx
		mov bx, es
		mov OldSeg09, bx		; save old handler of keyboard ints 

		push cs
		pop ds
		mov dx, offset MyInt09
		mov ax, 2509h
		int 21h					; overwrite handler of keyboard ints 

		mov ax, 3508h
		int 21h
		mov OldOfs08, bx
		mov bx, es
		mov OldSeg08, bx		; save old handler of timer ints 

		push cs
		pop ds
		mov dx, offset MyInt08
		mov ax, 2508h
		int 21h					; overwrite handler of timer ints 

		call ExitProgTSR

StringStyle	db 0c9h, 0cdh, 0bbh, 0bah, 020h, 0bah, 0c8h, 0cdh, 0bch
RegsNames	db 'axbxcxdxsidibpdsesssspipcs'
NeedUpdate	db 0

EndProgramm:

end    		Start
