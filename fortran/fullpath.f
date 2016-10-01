      PROGRAM test_get_command_argument
        IMPLICIT NONE
        INTEGER :: i, num_paths
        ! Apparently I can't jave a string of unknown length, so I'm
        ! just making it large enough to hold most things it could see
        CHARACTER(len=2048) :: arg

        ! A dynamically sized array
        INTEGER, ALLOCATABLE, DIMENSION(:):: paths

        ! allocate(paths(num_paths))
        DO i = 1, iargc()
          CALL getarg(i, arg)
          IF (LEN_TRIM(arg) /= 0) THEN
            ! the (a) means its printing a string
            WRITE (*,'(a)') TRIM(arg)
            WRITE (*,*) LEN_TRIM(arg)
          END IF
        END DO

        ! deallocate(paths)
      END PROGRAM

      ! do i=1,elements
      !      read *,vector(i)
      ! end do

      ! do i=1,elements
      !      print *,vector(i)
      ! end do
