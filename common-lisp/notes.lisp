; Querying info
  ; Use completion to get lists of functions
  pathname<tab>

  ; Basic docs
  (describe 'function-name)


; Useful
  (setq myvar someval)
  (eq 1 1) ; => T
  (eq 1 2) ; => NIL
  (not T)  ; => NIL
  (make-pathname :name "abc" :directory '(:absolute "lol" "wat"))
  ; => #P"/lol/wat/abc"
