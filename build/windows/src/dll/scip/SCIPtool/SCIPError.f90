!*******************************************************************************
!$RCSfile$
!$Revision$
!$Date$
!*******************************************************************************
!*******************************************************************************
!            Get SCIP Tool Error
!*******************************************************************************
INTEGER FUNCTION SCIPGetLastError( error )

USE message_fd

IMPLICIT NONE

!DEC$ IF DEFINED (DUALBUILD)
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'SCIPGETLASTERROROMP' :: SCIPGetLastError
!DEC$ ELSE
!DEC$ ATTRIBUTES DLLEXPORT :: SCIPGetLastError
!DEC$ ENDIF

TYPE( messageT ), INTENT( OUT ) :: error

INTEGER, EXTERNAL :: ModuleGetLastError

SCIPGetLastError = ModuleGetLastError( error )

RETURN
END
!***********************************************************************
!               SCIPInitError
!***********************************************************************
SUBROUTINE SCIPInitError()

IMPLICIT NONE

!DEC$ IF DEFINED (DUALBUILD)
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'SCIPINITERROROMP' :: SCIPInitError
!DEC$ ELSE
!DEC$ ATTRIBUTES DLLEXPORT :: SCIPInitError
!DEC$ ENDIF

CALL ModuleInitError()

RETURN
END
