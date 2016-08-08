%include 'system.inc'

section .data
  hello   db  'Hello, World!', 0Ah
  hbytes  equ $-hello

section .text
global main
main:
  call printit

  push dword 0
  sys.exit

printit:
  sys.write hbytes, hello, stdout
  ret
