%include 'system.inc'

section .data
  newline        db  0Ah
  newline_size   equ $-newline
  print_help     dd  0
  copy_output    dd  0

  dash_h         db  '-h',     0
  dash_dash_help db  '--help', 0
  dash_c         db  '-c',     0
  dash_dash_copy db  '--copy', 0

  truestr        db  'true', 0Ah
  truestr_size   equ $-truestr

  falsestr       db  'false', 0Ah
  falsestr_size  equ $-falsestr

section .text
global main

main:
  pop ecx ; argc
  pop eax ; filename
  dec ecx ; don't count the filename
  .pop_arg
    call is_flag

    cmp eax, 1
    je .is_flag
      sys.write falsestr_size, falsestr, stdout
    jmp .end_is_flag
    .is_flag
      sys.write truestr_size, truestr, stdout
    .end_is_flag

    call print_arg
    pop eax
    dec ecx
    jnz .pop_arg
  sys.exit ecx

check_flag:
  push ecx
  mov ecx, 0
  mov eax, [esp+8]
  mov ebx, [esp+16]
  pop ecx
  ret


; sets eax to 1 if the string on the stack is a flag
is_flag:
  mov eax, [esp+4]
  cmp byte [eax], '-'
  je .is_flag
    mov eax, 0
  jmp .end_is_flag
  .is_flag
    mov eax, 1
  .end_is_flag
  ret


; prints a null-terminated string from the stack
print_arg:
  mov eax, [esp+4]
  push ecx
  mov ecx, 0
  .count_chars
    cmp [eax+ecx], byte 0
    je .end_counting
    inc ecx
    jmp .count_chars
  .end_counting
  jecxz .end_printing
    sys.write ecx, eax, stdout
    sys.write newline_size, newline, stdout
  .end_printing
  pop ecx
  ret
