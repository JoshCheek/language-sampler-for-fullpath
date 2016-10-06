#!/usr/bin/env csi -s

(import posix)
(use posix)
(use srfi-1) ; filter

(define (help-screen)
  (string-append
    "usage: fullpath *[relative-paths] [-c]\n"
    "\n"
    "  Prints the fullpath of the paths\n"
    "  If no paths are given as args, it will read them from stdin\n"
    "\n"
    "  If there is only one path, the trailing newline is omitted\n"
    "\n"
    "  The -c flag will copy the results into your pasteboard\n"))

(define (any-match? fn lst)
  (if (null? lst)
    #f
    (or (fn (car lst))
        (any-match? fn (cdr lst)))))

(define (help-arg? arg)
  (or (string=? "-h"     arg)
      (string=? "--help" arg)))

(define (copy-output-arg? arg)
  (or (string=? "-c"      arg)
      (string=? "--copy" arg)))

(define (show-help? argv)
  (any-match? help-arg? argv))

(define (copy-output? argv)
  (any-match? copy-output-arg? argv))

(define (string-empty? string)
  (string=? "" string))

(define (select-paths potentials)
  (filter (lambda (potential)
            (not (or (help-arg?        potential)
                     (copy-output-arg? potential)
                     (string-empty?    potential))))
          potentials))

(define (prepend-lines-to lines)
  (let ((line (read-line)))
    (if (eof-object? line)
      lines
      (prepend-lines-to (cons line lines)))))

(define (read-lines)
    (reverse (prepend-lines-to '())))

(define (display-paths fileno dir paths)
  (if (= 1 (length paths))
      (file-write fileno (string-append dir "/" (car paths)))
      (for-each (lambda (path)
                  (file-write
                    fileno
                    (string-append dir "/" path "\n")))
                paths)))

(define (output-paths fileno dir paths copy-output)
  (display-paths fileno dir paths))

(let* ((dir         (current-directory))
       (argv        (command-line-arguments))
       (show-help   (show-help? argv))
       (copy-output (copy-output? argv))
       (paths       (select-paths argv)))
  (if show-help
      (display (help-screen))
      (let* ((paths (if (= 0 (length paths))
                        (select-paths (read-lines))
                        paths)))
        (output-paths fileno/stdout dir paths copy-output))))
