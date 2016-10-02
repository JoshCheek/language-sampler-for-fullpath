#!/usr/bin/env csi -s

(import posix)
(use posix)

(length (command-line-arguments))
(if #t 1 2) ; 1
(if #f 1 2) ; 2

(let ((dir (current-directory)))
  (if (= 1 (length (command-line-arguments)))
    (display (string-append dir "/" (car (command-line-arguments))))
    (for-each
      (lambda (arg) (print dir "/" arg))
      (command-line-arguments))))
