      PROGRAM test_get_command_argument
        INTEGER :: i
        ! Apparently I can't jave a string of unknown length, so I'm
        ! just making it large enough to hold most things it could see
        CHARACTER(len=2048) :: arg

        DO i = 1, iargc()
          CALL getarg(i, arg)
          ! the (a) means its printing a string
          WRITE (*,'(a)') TRIM(arg)
        END DO
      END PROGRAM
