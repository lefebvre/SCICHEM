!*******************************************************************************
!$RCSfile$
!$Revision$
!$Date$
!*******************************************************************************
MODULE SCIPException
  INTEGER, PARAMETER :: FOR_POINTER_LEN = 8
  INTEGER, PARAMETER :: FOR_PTR_SIZE    = FOR_POINTER_LEN

  TYPE T_EXCEPTION_POINTERS
     SEQUENCE
     INTEGER(FOR_PTR_SIZE) SIGINFO_PTR
     INTEGER(FOR_PTR_SIZE) SIGCONTEXT_PTR
  END TYPE
END MODULE SCIPException

!*******************************************************************************
!            Fortran Traceback from C/C++
!*******************************************************************************
INTEGER(4) FUNCTION SCIPTraceException ( ExceptionNumber, ExceptionInfo )
!DEC# ATTRIBUTES DLLEXPORT :: SCIPTraceException
!DEC# ATTRIBUTES REFERENCE :: ExceptionInfo

USE SCIMgr_fd
USE SCIPException


INTEGER(4),                 INTENT(IN) ::  ExceptionNumber
TYPE(T_EXCEPTION_POINTERS), INTENT(IN) ::  ExceptionInfo

INTEGER(4)        exitCode, sts
CHARACTER(LEN=80) title             !Suggested maxiumum

INTEGER(LEN_ADDRESS), EXTERNAL :: ADDRESSOF

SCIPTraceException = SCIPFailure

exitCode = -1

WRITE(title,'(A,Z10)')'SCIPtool Fortran Exception Traceback for exception ',ExceptionNumber

SCIPTraceException = SCIPSuccess

RETURN
END
