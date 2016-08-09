%include 'system.inc'

section .data
  hello   db  'Hello, World!', 0Ah
  hbytes  equ $-hello

section .text
global main

main:
  pop ecx ; argc
  pop eax ; filename
  dec ecx ; don't count the filename
  .pop_arg
    pop eax
    call printit
    dec ecx
    jnz .pop_arg
  sys.exit ecx

; pops and prints a null-terminated string from the stack
printit:
  sys.write hbytes, hello, stdout
