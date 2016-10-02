#!/usr/bin/env csi -s

(import posix)
(use posix)

(length (command-line-arguments))
(if #t 1 2) ; 1
(if #f 1 2) ; 2

(if (= 1 (length (command-line-arguments)))
  (display (string-append (current-directory) "/" (car (command-line-arguments))))
  (for-each
    (lambda (arg) (print (current-directory) "/" arg))
    (command-line-arguments)))
