!*******************************************************************************
!$RCSfile$
!$Revision$
!$Date$
!*******************************************************************************
!*******************************************************************************
!            Initialize SCIP Tool
!*******************************************************************************
INTEGER FUNCTION SCIPInitTool( UserID,UserCall,request,limit,cstr_INIfile )

USE charT_fd
USE limitT_fd
USE basic_fd

IMPLICIT NONE

!DEC$ IF DEFINED (DUALBUILD)
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'SCIPINITTOOLOMP' :: SCIPInitTool
!DEC$ ELSE
!DEC$ ATTRIBUTES DLLEXPORT :: SCIPInitTool
!DEC$ ENDIF

INTEGER,              INTENT( IN    ) :: UserID
INTEGER(LEN_ADDRESS), INTENT( IN    ) :: UserCall
INTEGER,              INTENT( INOUT ) :: request
TYPE( limitT ),       INTENT( IN    ) :: limit
TYPE( fileNameT ),    INTENT( IN    ) :: cstr_INIfile

INTEGER, EXTERNAL :: InitTool

SCIPInitTool = InitTool( UserID,UserCall,request,limit,cstr_INIfile )

RETURN
END
