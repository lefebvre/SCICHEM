!*******************************************************************************
!$RCSfile$
!$Revision$
!$Date$
!*******************************************************************************
!*******************************************************************************
!            Exit SCIP Tool
!*******************************************************************************
INTEGER FUNCTION SCIPExitTool()

IMPLICIT NONE

!DEC$ IF DEFINED (DUALBUILD)
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'SCIPEXITTOOLOMP' :: SCIPExitTool
!DEC$ ELSE
!DEC$ ATTRIBUTES DLLEXPORT :: SCIPExitTool
!DEC$ ENDIF

INTEGER, EXTERNAL :: ExitTool

SCIPExitTool = ExitTool()

RETURN
END
