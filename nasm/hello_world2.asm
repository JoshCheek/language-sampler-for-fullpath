; hello.asm - a "hello, world" program using NASM
section .text

global mystart                ; make the main function externally visible

; a procedure wrapping the system call to write
mywrite:
    mov eax, 0x4              ; system call write code
    int 0x80                  ; make system call
    ret

; a procedure wrapping the system call to exit
myexit:
    mov eax, 0x1              ; system call exit code
    int 0x80                  ; make system call
    ; no need to return

mystart:

; 1 print "hello, world"

    ; 1a prepare arguments
    push dword mylen           ; message length
    push dword mymsg           ; message to write
    push dword 1               ; file descriptor value
    ; 1b make call
    call mywrite
    ; 1c clean up stack
    add esp, 12

; 2 exit the program

    ; 2a prepare arguments
    push dword 0              ; exit code
    ; 2b make call
    call myexit
    ; 2c no need to clean up because no code here would executed...already exited!

section .data

  mymsg db "hello, world", 0xa  ; string with a carriage-return
  mylen equ $-mymsg             ; string length in bytes
