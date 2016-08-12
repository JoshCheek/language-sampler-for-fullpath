%include	'system.theirs.inc'

%define	BUFSIZE	2048

section	.data
fd.in	dd	stdin
fd.out	dd	stdout
hex	db	'0123456789ABCDEF'

section .bss
ibuffer	resb	BUFSIZE
obuffer	resb	BUFSIZE

section	.text
align 4
err:
	push	dword 1		; return failure
	sys.exit

align 4
global	_start
_start:
	add	esp, byte 8	; discard argc and argv[0]

	pop	ecx
	jecxz	.init		; no more arguments

	; ECX contains the path to input file
	push	dword 0		; O_RDONLY
	push	ecx
	sys.open
	jc	err		; open failed

	add	esp, byte 8
	mov	[fd.in], eax

	pop	ecx
	jecxz	.init		; no more arguments

	; ECX contains the path to output file
	push	dword 420	; file mode (644 octal)
	push	dword 0200h | 0400h | 01h
	; O_CREAT | O_TRUNC | O_WRONLY
	push	ecx
	sys.open
	jc	err

	add	esp, byte 12
	mov	[fd.out], eax

.init:
	sub	eax, eax
	sub	ebx, ebx
	sub	ecx, ecx
	mov	edi, obuffer

.loop:
	; read a byte from input file or stdin
	call	getchar

	; convert it to hex
	mov	dl, al
	shr	al, 4
	mov	al, [hex+eax]
	call	putchar

	mov	al, dl
	and	al, 0Fh
	mov	al, [hex+eax]
	call	putchar

	mov	al, ' '
	cmp	dl, 0Ah
	jne	.put
	mov	al, dl

.put:
	call	putchar
	cmp	al, dl
	jne	.loop
	call	write
	jmp	short .loop

align 4
getchar:
	or	ebx, ebx
	jne	.fetch

	call	read

.fetch:
	lodsb
	dec	ebx
	ret

read:
	push	dword BUFSIZE
	mov	esi, ibuffer
	push	esi
	push	dword [fd.in]
	sys.read
	add	esp, byte 12
	mov	ebx, eax
	or	eax, eax
	je	.done
	sub	eax, eax
	ret

align 4
.done:
	call	write		; flush output buffer

	; close files
	push	dword [fd.in]
	sys.close

	push	dword [fd.out]
	sys.close

	; return success
	push	dword 0
	sys.exit

align 4
putchar:
	stosb
	inc	ecx
	cmp	ecx, BUFSIZE
	je	write
	ret

align 4
write:
	sub	edi, ecx	; start of buffer
	push	ecx
	push	edi
	push	dword [fd.out]
	sys.write
	add	esp, byte 12
	sub	eax, eax
	sub	ecx, ecx	; buffer is empty now
	ret
