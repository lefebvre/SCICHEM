!*******************************************************************************
!$RCSfile$
!$Revision$
!$Date$
!*******************************************************************************
!*******************************************************************************
!            Run a Project
!*******************************************************************************
INTEGER FUNCTION SCIPRunProject( UserID,run )

USE timstruct_fd

IMPLICIT NONE

!DEC$ IF DEFINED (DUALBUILD)
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'SCIPRUNPROJECTOMP' :: SCIPRunProject
!DEC$ ELSE
!DEC$ ATTRIBUTES DLLEXPORT :: SCIPRunProject
!DEC$ ENDIF

INTEGER,       INTENT( IN  ) :: UserID !USER ID Tag
TYPE( pendT ), INTENT( IN  ) :: run    !Run input

INTEGER, EXTERNAL :: RunProject

SCIPRunProject = RunProject( UserID,run )

RETURN
END
!*******************************************************************************
!            Initialize a Project to run in single step mode
!*******************************************************************************
INTEGER FUNCTION SCIPInitProjectSS( UserID,run )

USE timstruct_fd

IMPLICIT NONE

!DEC$ IF DEFINED (DUALBUILD)
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'SCIPINITPROJECTSSOMP' :: SCIPInitProjectSS
!DEC$ ELSE
!DEC$ ATTRIBUTES DLLEXPORT :: SCIPInitProjectSS
!DEC$ ENDIF

INTEGER,       INTENT( IN  ) :: UserID !USER ID Tag
TYPE( pendT ), INTENT( IN  ) :: run    !Run input

INTEGER, EXTERNAL :: InitProjectSS

SCIPInitProjectSS = InitProjectSS( UserID,run )

RETURN
END
!*******************************************************************************
!            Run a Project for single step
!*******************************************************************************
INTEGER FUNCTION SCIPRunProjectSS( t )

IMPLICIT NONE

!DEC$ IF DEFINED (DUALBUILD)
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'SCIPRUNPROJECTSSOMP' :: SCIPRunProjectSS
!DEC$ ELSE
!DEC$ ATTRIBUTES DLLEXPORT :: SCIPRunProjectSS
!DEC$ ENDIF

REAL, INTENT( OUT ) :: t

INTEGER, EXTERNAL :: RunProjectSS

SCIPRunProjectSS = RunProjectSS( t )

RETURN
END
!*******************************************************************************
!            Exit Project from single step mode
!*******************************************************************************
INTEGER FUNCTION SCIPExitProjectSS()

IMPLICIT NONE

!DEC$ IF DEFINED (DUALBUILD)
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'SCIPEXITPROJECTSSOMP' :: SCIPExitProjectSS
!DEC$ ELSE
!DEC$ ATTRIBUTES DLLEXPORT :: SCIPExitProjectSS
!DEC$ ENDIF

INTEGER, EXTERNAL :: ExitProjectSS

SCIPExitProjectSS = ExitProjectSS()

RETURN
END

