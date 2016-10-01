PROGRAM Fullpath
  IMPLICIT NONE
  INTEGER, PARAMETER :: strlen=2048
  INTEGER :: i, j, status=0, ipath=1, num_paths=0

  ! Apparently I can't have a string of unknown length, so I'm
  ! just making it large enough to hold most things it could see
  CHARACTER(len=strlen) :: arg, path, dir
  CHARACTER, POINTER :: to_print

  ! Dynamically allocated arrays for holding the paths as we build them up
  CHARACTER(len=strlen), DIMENSION(:), POINTER :: paths, tmp

  LOGICAL :: print_help = .FALSE., copy_output = .FALSE.

  ! Analyze ARGV
  DO i = 1, iargc()
    CALL getarg(i, arg)
    IF (TRIM(arg) == "-h" .OR. TRIM(arg) == "--help") THEN
      print_help = .TRUE.
    ELSE IF (TRIM(arg) == "-c" .OR. TRIM(arg) == "--copy") THEN
      copy_output = .TRUE.
    ELSE IF (LEN_TRIM(arg) /= 0) THEN
      num_paths = num_paths + 1
    END IF
  END DO

  IF (print_help) THEN
    WRITE (*, '(a)') "usage: fullpath *[relative-paths] [-c]"
    WRITE (*, '(a)') ""
    WRITE (*, '(a)') "  Prints the fullpath of the paths"
    WRITE (*, '(a)') "  If no paths are given as args, it will read them from stdin"
    WRITE (*, '(a)') ""
    WRITE (*, '(a)') "  If there is only one path, the trailing newline is omitted"
    WRITE (*, '(a)') ""
    WRITE (*, '(a)') "  The -c flag will copy the results into your pasteboard"
    RETURN
  END IF

  ! Get the paths
  IF (num_paths /= 0) THEN
    ! Copy paths from argv
    allocate(paths(num_paths))
    ipath = 1
    DO i = 1, iargc()
      CALL getarg(i, arg)
      IF (LEN_TRIM(arg) /= 0) THEN
        paths(ipath) = arg
        ipath = ipath + 1
      END IF
    END DO
  ELSE
    ! Read paths from stdin
    num_paths = 0
    allocate(paths(0))
    DO WHILE (.TRUE.)
      ! Reset the path
      DO i=1, strlen
        path(i:i) = achar(32)
      END DO

      ! Read one path
      i=1
      DO WHILE(.TRUE.)
        call fget(path(i:i), status)
        ! break if we hit the end of the stream
        IF (status /= 0) THEN
          EXIT
        END IF
        ! break if we found a newline
        IF (path(i:i) == achar(10)) THEN
          path(i:i) = achar(32) ! remove the newline
          EXIT
        END IF
        i = i + 1
      END DO

      ! If we found a path
      IF (i /= 1) THEN
        ! allocate room for one the existing and new path
        allocate(tmp(num_paths+1))

        ! copy the paths over
        DO ipath=1, num_paths
          tmp(ipath) = paths(ipath)
        END DO

        ! add the path we just read in
        tmp(num_paths+1) = path

        ! deallocate the old paths
        deallocate(paths)

        ! set them to the correct variable
        paths => tmp
        NULLIFY(tmp)

        ! Increase counter
        num_paths = num_paths + 1
      END IF

      ! No more paths, we're done
      IF (status /= 0) THEN
        EXIT
      END IF
    END DO
  END IF


  ! Print the paths
  CALL getcwd(dir)
  IF (num_paths == 1) THEN
    path = paths(1)
    WRITE(*, '(3a)',advance="no") TRIM(dir), "/", TRIM(path)
    IF (copy_output) THEN
      call execute_command_line("printf %s '"//TRIM(dir)//"/"//TRIM(path)//"' | pbcopy")
    END IF
  ELSE
    DO ipath=1, num_paths
      path = paths(ipath)
      WRITE (*,'(3a)') TRIM(dir), "/", TRIM(path)
    END DO
  END IF

  ! Clean up memory
  deallocate(paths)
END PROGRAM
