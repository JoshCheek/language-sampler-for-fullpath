%include 'system.inc'

section .data
  hello db 'Hello, World!', 0Ah
  hbytes equ $-hello

section .text
global main
main:
  sys.write hbytes, hello, stdout

  push dword 0
  sys.exit
