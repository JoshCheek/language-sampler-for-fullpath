; This code is taken/modified from:
;   http://www.freebsd.org/doc/en/books/developers-handbook/book.html#x86-portable-include
; This seems to be its license (fulfilled below):
;   http://www.freebsd.org/doc/en/books/developers-handbook/legalnotice.html

; Copyright
;
; Redistribution and use in source (XML DocBook) and 'compiled' forms (XML, HTML, PDF, PostScript, RTF and so forth)
; with or without modification, are permitted provided that the following conditions are met:
;
; 1. Redistributions of source code (XML DocBook) must retain the above copyright notice,
;    this list of conditions and the following disclaimer as the first lines of this file unmodified.
; 2. Redistributions in compiled form (transformed to other DTDs, converted to PDF, PostScript, RTF and other formats)
;    must reproduce the above copyright notice, this list of conditions and the following disclaimer in the
;    documentation and/or other materials provided with the distribution.
;
; Important: THIS DOCUMENTATION IS PROVIDED BY THE FREEBSD DOCUMENTATION PROJECT "AS IS"
; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
; THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
; IN NO EVENT SHALL THE FREEBSD DOCUMENTATION PROJECT BE LIABLE FOR ANY DIRECT, INDIRECT,
; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
; PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
; USE OF THIS DOCUMENTATION, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;
;
; ----------  End of stupid copywrite nonsense  ----------


; Turns out this info is already on my system, in the file sys/syscall.h
; I wrote a program to locate the file (included in this dir)
; and this script will reformat it for inclusion into this file
; $ ./included-from sys/syscall.h | xargs sed -ne 's/#/%/; /^%define/p' | column -t

%define	stdin      0
%define	stdout     1
%define	stderr     2

%define SYS_nosys  0
%define SYS_exit   1
%define SYS_fork   2
%define SYS_read   3
%define SYS_write  4

section .text
align 4
access.the.bsd.kernel:
  int 80h
  ret

%macro system 1
  mov  eax, %1
  call access.the.bsd.kernel
%endmacro


%macro sys.exit 0
  system SYS_exit
%endmacro

%macro sys.fork 0
  system SYS_fork
%endmacro

%macro sys.read 0
  system SYS_read
%endmacro

%macro sys.write 0
  system SYS_write
%endmacro
