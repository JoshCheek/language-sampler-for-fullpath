#!/usr/bin/env csi -s

(import posix)
(use posix)
(use srfi-1)


; (print (read-line))
(define (any? fn lst)
  (if (null? lst)
    #f
    (or (fn (car lst))
        (any? fn (cdr lst)))))

(define (help-arg? arg)
  (or (string=? "-h"     arg)
      (string=? "--help" arg)))

(define (copy-output-arg? arg)
  (or (string=? "-c"      arg)
      (string=? "--copy" arg)))

(define (show-help? argv)
  (any? help-arg? argv))

(define (copy-output? argv)
  (any? copy-output-arg? argv))

(define (select-paths potentials)
  (filter (lambda (potential)
            (not (or (help-arg? potential)
                     (copy-output-arg? potential))))
          potentials))

(let* ((dir         (current-directory))
       (argv        (command-line-arguments))
       (show-help   (show-help? argv))
       (copy-output (copy-output? argv))
       (paths       (select-paths argv)))
    (if (= 1 (length paths))
      (display (string-append dir "/" (car paths)))
      (for-each (lambda (path) (print dir "/" path)) paths)))
