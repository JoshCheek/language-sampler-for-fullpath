%include 'system.inc'

section .data
  hello   db  'Hello, World!', 0Ah
  hbytes  equ $-hello

section .text
global main

main:
  pop ecx ; argc
  pop eax ; filename
  pop eax
  add ecx, -1
  .pop_arg
    pop eax
    sys.write hbytes, hello, stdout
    ; call printit
    add ecx, -1
  jecxz .done
  jmp .pop_arg
  .done
  sys.exit ecx

; prints a null-terminated string pointed to by eax
printit:
  mov ebx, [eax]
  ; sys.write 1
  ret
