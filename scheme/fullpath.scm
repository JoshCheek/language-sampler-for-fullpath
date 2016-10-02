#!/usr/bin/env csi -s

(import posix)
(use posix)

(for-each
  (lambda (arg) (print (current-directory) "/" arg))
  (command-line-arguments))
