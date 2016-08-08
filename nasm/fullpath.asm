%include 'system.inc'

section .data
  hello   db  'Hello, World!', 0Ah
  hbytes  equ $-hello

section .text
global main
main:
  call printit
  sys.exit 0

printit:
  sys.write hbytes, hello, stdout
  ret
