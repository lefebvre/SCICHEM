!*******************************************************************************
!$RCSfile$
!$Revision$
!$Date$
!*******************************************************************************
!*******************************************************************************
!            Get Project Version
!*******************************************************************************
INTEGER FUNCTION SCIPGetProjectVersion( UserID,project )

USE prjstruct_fd

IMPLICIT NONE

!DEC$ IF DEFINED (DUALBUILD)
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'SCIPGETPROJECTVERSIONOMP' :: SCIPGetProjectVersion
!DEC$ ELSE
!DEC$ ATTRIBUTES DLLEXPORT :: SCIPGetProjectVersion
!DEC$ ENDIF

INTEGER,            INTENT( IN    ) :: UserID !USER ID Tag
TYPE( projectIDT ), INTENT( INOUT ) :: project !Project ID

INTEGER, EXTERNAL :: GetProjectVersion

SCIPGetProjectVersion = GetProjectVersion( UserID,project )

RETURN
END

!==============================================================================

INTEGER FUNCTION SCIPGetProjectAudit( UserID,audit )

USE inpstruct_fd

IMPLICIT NONE

!DEC$ IF DEFINED (DUALBUILD)
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'SCIPGETPROJECTAUDITOMP' :: SCIPGetProjectAudit
!DEC$ ELSE
!DEC$ ATTRIBUTES DLLEXPORT :: SCIPGetProjectAudit
!DEC$ ENDIF

INTEGER,         INTENT( IN    ) :: UserID !USER ID Tag
TYPE( pauditT ), INTENT( INOUT ) :: audit !Project ID

INTEGER, EXTERNAL :: GetProjectAudit

SCIPGetProjectAudit = GetProjectAudit( UserID,audit )

RETURN
END
