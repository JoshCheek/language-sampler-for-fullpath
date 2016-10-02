#!/usr/bin/env csi -s

(import posix)
(use posix)

(length (command-line-arguments))
(if #t 1 2) ; 1
(if #f 1 2) ; 2

(let ((dir  (current-directory))
      (argv (command-line-arguments)))
  (if (= 1 (length argv))
    (display (string-append dir "/" (car argv)))
    (for-each
      (lambda (arg) (print dir "/" arg))
      argv)))
