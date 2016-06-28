; Querying info
  ; Use completion to get lists of functions
  pathname<tab>

  ; Basic docs
  (describe 'function-name)

  ; list the SYMBOLs matching name.
  (apropos name)

  ; There's a bunch that have the word "stream" in them, might work here
  *standard-input*

  ; This thing is a normal list
  *args*

  ; Fuck this thing
  (argv)

; Useful
  (setq myvar someval)
  (eq 1 1) ; => T
  (eq 1 2) ; => NIL
  (not T)  ; => NIL
  (make-pathname :name "abc" :directory '(:absolute "lol" "wat")) ; => #P"/lol/wat/abc"
