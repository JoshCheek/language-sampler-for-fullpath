%include 'system.inc'

section .data
  newline       db  0Ah
  newline_size  equ $-newline

section .text
global main

main:
  pop ecx ; argc
  pop eax ; filename
  dec ecx ; don't count the filename
  .pop_arg
    call printit
    pop eax
    dec ecx
    jnz .pop_arg
  sys.exit ecx

; prints a null-terminated string from the stack
printit:
  mov eax, [esp+4]
  push ecx
  mov ecx, 0
  .count_chars
    cmp [eax+ecx], byte 0
    je .done_counting
    inc ecx
    jmp .count_chars
  .done_counting
  jecxz .done_printing
    sys.write ecx, eax, stdout
    sys.write newline_size, newline, stdout
  .done_printing
  pop ecx
  ret
