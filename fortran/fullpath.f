      PROGRAM test_get_command_argument
        IMPLICIT NONE
        INTEGER, PARAMETER :: strlen=2048
        INTEGER :: i, j, status=0, ipath=0, num_paths=0

        ! Apparently I can't have a string of unknown length, so I'm
        ! just making it large enough to hold most things it could see
        CHARACTER(len=strlen) :: arg, path, dir

        ! A dynamically sized array
        CHARACTER(len=strlen), ALLOCATABLE, DIMENSION(:):: paths

        ! Analyze argv
        DO i = 1, iargc()
          CALL getarg(i, arg)
          IF (LEN_TRIM(arg) /= 0) THEN
            num_paths = num_paths + 1
          END IF
        END DO

        ! Get the paths
        IF (num_paths /= 0) THEN
          ! Copy paths from argv
          allocate(paths(num_paths))
          DO i = 1, iargc()
            CALL getarg(i, arg)
            IF (LEN_TRIM(arg) /= 0) THEN
              paths(ipath) = arg
              ipath = ipath + 1
            END IF
          END DO
        ELSE
          ! Read paths from stdin
          ipath = 0
          num_paths = 0
          DO WHILE (.TRUE.)
            ! wipe path variable
            DO i=1, strlen
              path(i:i) = achar(0)
            END DO

            ! Read one path
            i=1
            DO WHILE(.TRUE.)
              call fget(path(i:i), status)
              IF (status /= 0) THEN
                EXIT
              END IF
              ! break if we found a newline
              IF (path(i:i) == achar(10)) THEN
                path(i:i) = achar(0) ! remove the newline
                EXIT
              END IF
              i = i + 1
            END DO

            ! Copy it into paths
            ! DO j = 1, strlen
              ! paths(ipath:j) = path(j:j)
            ! END DO
            ipath = ipath + 1
            num_paths = num_paths + 1

            ! No more paths
            IF (status /= 0) THEN
              EXIT
            END IF
          END DO

          num_paths = 1
          allocate(paths(num_paths))
          ! Need to read them from stdin,
          ! increasing the size of paths and copying them over as
          ! necessary
        END IF

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
