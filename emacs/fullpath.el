(require 'cl)
(require 'subr-x)

(defun expand (path &optional relative-to-dir)
  (directory-file-name (expand-file-name path relative-to-dir)))

(defun show-help ()
  (princ "usage: fullpath *[relative-paths] [-c]

  Prints the fullpath of the paths
  If no paths are given as args, it will read them from stdin

  If there is only one path, the trailing newline is omitted

  The -c flag will copy the results into your pasteboard
"))

(defun pbcopy (s)
  (with-temp-buffer
    (insert s)
    (call-process-region (point-min) (point-max) "pbcopy")))

(defun emit (paths copy-to-pasteboard)
  (let* ((unterminated (string-join paths "\n"))
         (out (if (> (length paths) 1)
                  (concat unterminated "\n")
                unterminated)))
    (princ out)
    (when copy-to-pasteboard (pbcopy out))))

(defun help? (args)
  (cl-find-if (lambda (v) (or (string-equal "-h" v)
                              (string-equal "--help" v)))
              args))

(defun consume-stdin ()
  (let (lines)
    (condition-case nil
        (while (setq line (read-from-minibuffer ""))
          (setq lines (cons line lines)))
      (error nil))
    (reverse lines)))

(defun main (args copy-to-pasteboard)
  (let ((paths (cl-remove-if 'string-blank-p args)))
    (emit (mapcar 'expand paths) copy-to-pasteboard)))

(let* ((argv (if (string-equal "--" (car command-line-args-left))
                 (cdr command-line-args-left)
               command-line-args-left))
       (copy-to-pasteboard (let ((final-arg (car (last argv))))
                             (or (string-equal final-arg "-c")
                                 (string-equal final-arg "--copy"))))
       (argv (if copy-to-pasteboard (butlast argv 1) argv)))
  (if (help? argv)
      (show-help)
    (main (or argv (consume-stdin)) copy-to-pasteboard)))
