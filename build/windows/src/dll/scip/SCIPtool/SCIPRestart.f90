!*******************************************************************************
!$RCSfile$
!$Revision$
!$Date$
!*******************************************************************************
!*******************************************************************************
!            Create a Project, Restart mode
!*******************************************************************************
INTEGER FUNCTION SCIPRestartProject( UserID,createRst )

USE structure_fd

IMPLICIT NONE

!DEC$ IF DEFINED (DUALBUILD)
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'SCIPRESTARTPROJECTOMP' :: SCIPRestartProject
!DEC$ ELSE
!DEC$ ATTRIBUTES DLLEXPORT :: SCIPRestartProject
!DEC$ ENDIF

INTEGER,                     INTENT( IN  ) :: UserID !USER ID Tag
TYPE( createRstT ),  TARGET, INTENT( IN  ) :: createRst !Project input

INTERFACE
  INTEGER FUNCTION RestartProject( CallerID,project )
    USE tooluser_fd
    INTEGER,                                     INTENT( IN  ) :: CallerID    !USER ID Tag
    TYPE( createRstT ),                  TARGET, INTENT( IN  ) :: project     !Project input
  END FUNCTION RestartProject
END INTERFACE

SCIPRestartProject = RestartProject( UserID,createRst )

RETURN
END
