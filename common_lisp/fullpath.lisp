(defun is-empty (list)
  (eq 0 (list-length list)))

(defun join (list delimiter)
  (if (is-empty list)
      ""
      (if (= 1 (list-length list))
          (first list)
          (format nil "~a~a~a"
                  (first list)
                  delimiter
                  (join (rest list) delimiter)))))

(defun help-screen ()
  (join '("usage: fullpath *[relative-paths] [-c]"
          ""
          "  Prints the fullpath of the paths"
          "  If no paths are given as args, it will read them from stdin"
          ""
          "  If there is only one path, the trailing newline is omitted"
          ""
          "  The -c flag will copy the results into your pasteboard"
          "")
        #\linefeed))

(defun does-include (needle haystack)
  (find needle haystack :test #'equal))

(defun has-help (args)
  (or (does-include "-h"     args)
      (does-include "--help" args)))

(defun has-copy (args)
  (or (does-include "-c"     args)
      (does-include "--copy" args)))

(defun readlines (instream)
  (let ((line (read-line instream nil nil)))
    (if line
        (cons line (readlines instream))
        nil)))

(defun is-arg (maybe-arg)
  (if (not (eq 0 (length maybe-arg)))
      (eq #\- (char maybe-arg 0))))

(defun is-blank (maybe-blank)
  (eq 0 (length maybe-blank)))

(defun remove-flags (args)
  (remove-if 'is-arg args))

(defun remove-blank (args)
  (remove-if 'is-blank args))

(defun absolute-pathname (relative-path)
  (namestring
    (merge-pathnames
      (make-pathname :name relative-path)
      (truename "."))))

(defun expand-paths (paths)
  (map 'list
       'absolute-pathname
       paths))

(defun format-paths (paths)
  (let ((formatted (join paths #\linefeed)))
    (if (< (list-length paths) 2)
        formatted
        (format nil "~a~%" formatted))))

(defun copy-to-pasteboard (string)
  (let* ((pbcopy   (sb-ext:run-program "/usr/bin/pbcopy" ; <-- bs to have to give it the abs path -.^
                                       '()
                                       :wait nil
                                       :input :stream
                                       :output nil))
         (copy-stream (sb-ext:process-input pbcopy)))
    (princ string copy-stream)
    (finish-output copy-stream)
    (sb-ext:process-close pbcopy)
    (sb-ext:process-exit-code pbcopy)))

(defun main ()
  (if (has-help *posix-argv*)
      (format t (help-screen))
      (let* ((do-copy  (has-copy *posix-argv*))
             (paths    (rest *posix-argv*))
             (paths    (remove-flags paths))
             (paths    (remove-blank paths))
             (paths    (if (is-empty paths)
                           (readlines *standard-input*)
                           paths))
             (paths    (remove-blank paths))
             (paths    (expand-paths paths))
             (to-print (format-paths paths)))
        (if do-copy (copy-to-pasteboard to-print))
        (princ to-print))))
