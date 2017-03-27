!*******************************************************************************
!$RCSfile$
!$Revision$
!$Date$
!*******************************************************************************
!DEC$ IF DEFINED (DUALBUILD)
MODULE SCIPExceptionOMP
!DEC$ ELSE
MODULE SCIPException
!DEC$ ENDIF
  INTEGER, PARAMETER :: FOR_POINTER_LEN = INT_PTR_KIND()
  INTEGER, PARAMETER :: FOR_PTR_SIZE   = FOR_POINTER_LEN

  TYPE T_EXCEPTION_POINTERS
     SEQUENCE
     INTEGER(FOR_PTR_SIZE) SIGINFO_PTR
     INTEGER(FOR_PTR_SIZE) SIGCONTEXT_PTR
  END TYPE
!DEC$ IF DEFINED (DUALBUILD)
END MODULE SCIPExceptionOMP
!DEC$ ELSE
END MODULE SCIPException
!DEC$ ENDIF

!*******************************************************************************
!            Fortran Traceback from C/C++
!*******************************************************************************
INTEGER(4) FUNCTION SCIPTraceException ( ExceptionNumber, ExceptionInfo )
!DEC$ IF DEFINED (DUALBUILD)
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'SCIPTRACEEXCEPTIONOMP' :: SCIPTraceException
!DEC$ ELSE
!DEC$ ATTRIBUTES DLLEXPORT :: SCIPTraceException
!DEC$ ENDIF
!DEC$ ATTRIBUTES REFERENCE :: ExceptionInfo

USE SCIMgr_fd
!DEC$ IF DEFINED (DUALBUILD)
USE SCIPExceptionOMP
!DEC$ ELSE
USE SCIPException
!DEC$ ENDIF


INTEGER(4),                 INTENT(IN) ::  ExceptionNumber
TYPE(T_EXCEPTION_POINTERS), INTENT(IN) ::  ExceptionInfo

INTEGER(4)        exitCode, sts
CHARACTER(LEN=80) title             !Suggested maxiumum

INTEGER(LEN_ADDRESS), EXTERNAL :: ADDRESSOF

INTERFACE
  SUBROUTINE TRACEBACKQQ(STRING, USER_EXIT_CODE, STATUS, EPTR)
!DEC$ IF DEFINED (DUALBUILD)
    USE SCIPExceptionOMP
!DEC$ ELSE
    USE SCIPException
!DEC$ ENDIF
    !DEC$ ATTRIBUTES DEFAULT :: TRACEBACKQQ
    !DEC$ ATTRIBUTES DECORATE, ALIAS : 'TRACEBACKQQ' :: TRACEBACKQQ
    CHARACTER*(*), INTENT(IN ), OPTIONAL :: STRING
    INTEGER*4,     INTENT(IN ), OPTIONAL :: USER_EXIT_CODE
    INTEGER*4,     INTENT(OUT), OPTIONAL :: STATUS
    OPTIONAL EPTR
    TYPE(T_EXCEPTION_POINTERS) :: EBASE
    POINTER(EPTR,EBASE)
  END SUBROUTINE
END INTERFACE

SCIPTraceException = SCIPFailure

exitCode = -1

WRITE(title,'(A,Z10)')'SCIPtool Fortran Exception Traceback for exception ',ExceptionNumber

IF( exceptionNumber /= 999 )THEN
  WRITE(*,*)'Calling TraceBackQQ with '//TRIM(title)
  CALL TRACEBACKQQ( TRIM(title), exitCode, sts, ADDRESSOF(ExceptionInfo) )
ELSE
  WRITE(*,*)'Testing TraceBackQQ with '//TRIM(title)
END IF

SCIPTraceException = SCIPSuccess

RETURN
END
