!*******************************************************************************
!$RCSfile$
!$Revision$
!$Date$
!*******************************************************************************
!*******************************************************************************
!            Get scipuff version number
!*******************************************************************************
INTEGER FUNCTION SCIPGetScipuffVersion()

IMPLICIT NONE

!DEC$ IF DEFINED (DUALBUILD)
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'SCIPGETSCIPUFFVERSIONOMP' :: SCIPGetScipuffVersion
!DEC$ ELSE
!DEC$ ATTRIBUTES DLLEXPORT :: SCIPGetScipuffVersion
!DEC$ ENDIF

INTEGER, EXTERNAL :: GetScipuffVersion

SCIPGetScipuffVersion = GetScipuffVersion()

RETURN
END
!*******************************************************************************
!            Get Tool API version number
!*******************************************************************************
INTEGER FUNCTION SCIPGetAPIVersion()

IMPLICIT NONE

!DEC$ IF DEFINED (DUALBUILD)
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'SCIPGETAPIVERSIONOMP' :: SCIPGetAPIVersion
!DEC$ ELSE
!DEC$ ATTRIBUTES DLLEXPORT :: SCIPGetAPIVersion
!DEC$ ENDIF

INTEGER, EXTERNAL :: GetAPIVersion

SCIPGetAPIVersion = GetAPIVersion()

RETURN
END
!*******************************************************************************
!            Get SCIP Tool version number
!*******************************************************************************
INTEGER FUNCTION SCIPGetVersion()

IMPLICIT NONE

!DEC$ IF DEFINED (DUALBUILD)
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'SCIPGETVERSIONOMP' :: SCIPGetVersion
!DEC$ ELSE
!DEC$ ATTRIBUTES DLLEXPORT :: SCIPGetVersion
!DEC$ ENDIF

INTEGER, EXTERNAL :: GetVersionValue

SCIPGetVersion = GetVersionValue()

RETURN
END
!*******************************************************************************
!            Get SCIP Tool version string
!*******************************************************************************
INTEGER FUNCTION SCIPGetVersionString( iflag,charString )

USE charT_fd

IMPLICIT NONE

!DEC$ IF DEFINED (DUALBUILD)
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'SCIPGETVERSIONSTRINGOMP' :: SCIPGetVersionString
!DEC$ ELSE
!DEC$ ATTRIBUTES DLLEXPORT :: SCIPGetVersionString
!DEC$ ENDIF

INTEGER         , INTENT(INOUT) :: iflag
TYPE( char128T ), INTENT(OUT  ) :: charString

INTEGER, EXTERNAL :: GetVersionString

SCIPGetVersionString = GetVersionString( iflag,charString )

RETURN
END
!*******************************************************************************
!            Get maximum length of filenames
!*******************************************************************************
INTEGER FUNCTION SCIPGetPathMaxLength()

IMPLICIT NONE

!DEC$ IF DEFINED (DUALBUILD)
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'SCIPGETPATHMAXLENGTHOMP' :: SCIPGetPathMaxLength
!DEC$ ELSE
!DEC$ ATTRIBUTES DLLEXPORT :: SCIPGetPathMaxLength
!DEC$ ENDIF

INTEGER, EXTERNAL :: GetPathMaxLength

SCIPGetPathMaxLength = GetPathMaxLength()

RETURN
END
