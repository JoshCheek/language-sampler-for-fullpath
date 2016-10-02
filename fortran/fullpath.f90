PROGRAM Fullpath
  IMPLICIT NONE
  INTEGER, PARAMETER :: strlen=2048
  INTEGER :: i, j, print_len, status=0, ipath=1, num_paths=0

  ! Apparently I can't have a string of unknown length, so I'm
  ! just making it large enough to hold most things it could see
  CHARACTER(len=strlen) :: arg, path, dir
  CHARACTER, ALLOCATABLE, DIMENSION(:) :: to_print

  ! Dynamically allocated arrays for holding the paths as we build them up
  CHARACTER(len=strlen), DIMENSION(:), POINTER :: paths, tmp

  LOGICAL :: print_help = .FALSE., copy_output = .FALSE.

  ! Analyze ARGV
  DO i = 1, command_argument_count()
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
      CALL get_command_argument(i, arg)
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
    ! Print one path, copy it if we're supposed to
    path = paths(1)
    WRITE(*, '(3a)',advance="no") TRIM(dir), "/", TRIM(path)
    IF (copy_output) THEN
      ! Note that this doesn't work when input has a single-quote in it -.-
      call execute_command_line("printf %s '"//TRIM(dir)//"/"//TRIM(path)//"' | pbcopy")
    END IF
  ELSE
    ! Print all the paths, count how much we printed
    print_len = 0
    DO ipath=1, num_paths
      path = TRIM(dir)//"/"//TRIM(paths(ipath))
      WRITE (*,'(3a)') TRIM(path)
      print_len = LEN_TRIM(path) + 1 ! 1 for the newline
    END DO

    ! Copy the paths if we're supposed to
    ! FIXME: I cannot fucking figure out how to pull this off.
    ! Why doesn't execute_command_line allow me to pass stdin?
    ! (eg allowing me to pass a file descriptor for a pipe would be enough)
    IF (copy_output) THEN
      allocate(to_print(print_len))
      i = 0
      DO ipath=1, num_paths
        path = TRIM(dir)//"/"//TRIM(paths(ipath))//achar(10)
        DO j=1, LEN_TRIM(path)
          to_print(i+j:i+j) = path(j:j)
        END DO
        i = i + j
      END DO
      ! print_len = num_paths*(dirlen+2+strlen) ! dir, slash, path, newline
      ! call execute_command_line("ruby -e 'File.write %(tmp) ARGV[0]'"//to_print//"' | pbcopy")
      ! call execute_command_line("printf %s '"//to_print//"' | pbcopy")
      deallocate(to_print)
    END IF

  END IF

  ! Clean up memory
  deallocate(paths)
END PROGRAM
