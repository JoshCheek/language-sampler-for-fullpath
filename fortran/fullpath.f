      PROGRAM test_get_command_argument
        IMPLICIT NONE
        INTEGER :: i, ipath=0, num_paths=0
        ! Apparently I can't jave a string of unknown length, so I'm
        ! just making it large enough to hold most things it could see
        CHARACTER(len=2048) :: arg, path, dir

        ! A dynamically sized array
        CHARACTER(len=2048), ALLOCATABLE, DIMENSION(:):: paths

        ! Categorize arguments (path, flag, etc)
        DO i = 1, iargc()
          CALL getarg(i, arg)
          IF (LEN_TRIM(arg) /= 0) THEN
            num_paths = num_paths + 1
          END IF
        END DO

        ! Get the paths
        allocate(paths(num_paths))
        DO i = 1, iargc()
          CALL getarg(i, arg)
          IF (LEN_TRIM(arg) /= 0) THEN
            paths(ipath) = arg
            ipath = ipath + 1
          END IF
        END DO

        ! Print the paths
        CALL getcwd(dir)

        IF (num_paths == 1) THEN
          path = paths(0)
          WRITE(*, '(3a)',advance="no") TRIM(dir), "/", TRIM(path)
        ELSE
          DO ipath=0, num_paths-1
            path = paths(ipath)
            WRITE (*,'(3a)') TRIM(dir), "/", TRIM(path)
          END DO
        END IF

        ! Clean up memory
        deallocate(paths)
      END PROGRAM
