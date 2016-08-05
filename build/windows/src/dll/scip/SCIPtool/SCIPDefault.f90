!*******************************************************************************
!$RCSfile$
!$Revision$
!$Date$
!*******************************************************************************
!*******************************************************************************
!            Default Project Input
!*******************************************************************************
INTEGER FUNCTION SCIPDefaultInput( UserID,iInput,tInput0,tInput1 )

IMPLICIT NONE

INTEGER,               INTENT( IN    ) :: UserID
INTEGER,               INTENT( IN    ) :: iInput
INTEGER, DIMENSION(*), INTENT( INOUT ) :: tInput0
INTEGER, DIMENSION(*), INTENT( INOUT ) :: tInput1

!DEC$ IF DEFINED (DUALBUILD)
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'SCIPDEFAULTINPUTOMP' :: SCIPDefaultInput
!DEC$ ELSE
!DEC$ ATTRIBUTES DLLEXPORT :: SCIPDefaultInput
!DEC$ ENDIF

INTEGER, EXTERNAL :: DefaultInput

!==== Initialize

SCIPDefaultInput = DefaultInput( UserID,iInput,tInput0,tInput1 )

RETURN
END
!*******************************************************************************
!            Default Project Ctrl
!*******************************************************************************
INTEGER FUNCTION SCIPDefaultCtrlF( UserID,ctrlIO )

USE timstruct_fd

IMPLICIT NONE

!DEC$ IF DEFINED (DUALBUILD)
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'SCIPDEFAULTCTRLFOMP' :: SCIPDefaultCtrlF
!DEC$ ELSE
!DEC$ ATTRIBUTES DLLEXPORT :: SCIPDefaultCtrlF
!DEC$ ENDIF

INTEGER,        INTENT( IN    ) :: UserID
TYPE( pctrlT ), INTENT( INOUT ) :: ctrlIO

INTEGER, EXTERNAL :: DefaultCtrlF

SCIPDefaultCtrlF = DefaultCtrlF( UserID,ctrlIO )

RETURN
END
!*******************************************************************************
!            Default Project Start
!*******************************************************************************
INTEGER FUNCTION SCIPDefaultStartF( UserID,startIO )

USE timstruct_fd

IMPLICIT NONE

!DEC$ IF DEFINED (DUALBUILD)
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'SCIPDEFAULTSTARTFOMP' :: SCIPDefaultStartF
!DEC$ ELSE
!DEC$ ATTRIBUTES DLLEXPORT :: SCIPDefaultStartF
!DEC$ ENDIF

INTEGER,         INTENT( IN    ) :: UserID
TYPE( pstartT ), INTENT( INOUT ) :: startIO

INTEGER, EXTERNAL :: DefaultStartF

SCIPDefaultStartF = DefaultStartF( UserID,startIO )

RETURN
END
!*******************************************************************************
!            Default Project End
!*******************************************************************************
INTEGER FUNCTION SCIPDefaultEndF( UserID,endIO )

USE timstruct_fd

IMPLICIT NONE

!DEC$ IF DEFINED (DUALBUILD)
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'SCIPDEFAULTENDFOMP' :: SCIPDefaultEndF
!DEC$ ELSE
!DEC$ ATTRIBUTES DLLEXPORT :: SCIPDefaultEndF
!DEC$ ENDIF

INTEGER,       INTENT( IN    ) :: UserID
TYPE( pendT ), INTENT( INOUT ) :: endIO

INTEGER, EXTERNAL :: DefaultEndF

SCIPDefaultEndF = DefaultEndF( UserID,endIO )

RETURN
END
!*******************************************************************************
!            Default Project Flags
!*******************************************************************************
INTEGER FUNCTION SCIPDefaultFlagsF( UserID,flagsIO )

USE inpstruct_fd

IMPLICIT NONE

!DEC$ IF DEFINED (DUALBUILD)
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'SCIPDEFAULTFLAGSFOMP' :: SCIPDefaultFlagsF
!DEC$ ELSE
!DEC$ ATTRIBUTES DLLEXPORT :: SCIPDefaultFlagsF
!DEC$ ENDIF

INTEGER,         INTENT( IN    ) :: UserID
TYPE( pflagsT ), INTENT( INOUT ) :: flagsIO

INTEGER, EXTERNAL :: DefaultFlagsF

SCIPDefaultFlagsF = DefaultFlagsF( UserID,flagsIO )

RETURN
END
!*******************************************************************************
!            Default Project Domain
!*******************************************************************************
INTEGER FUNCTION SCIPDefaultDomainF( UserID,domainIO )

USE domstruct_fd

IMPLICIT NONE

!DEC$ IF DEFINED (DUALBUILD)
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'SCIPDEFAULTDOMAINFOMP' :: SCIPDefaultDomainF
!DEC$ ELSE
!DEC$ ATTRIBUTES DLLEXPORT :: SCIPDefaultDomainF
!DEC$ ENDIF

INTEGER,           INTENT( IN    ) :: UserID
TYPE( pspatialT ), INTENT( INOUT ) :: domainIO

INTEGER, EXTERNAL :: DefaultDomainF

SCIPDefaultDomainF = DefaultDomainF( UserID,domainIO )

RETURN
END
!*******************************************************************************
!            Default Project Options
!*******************************************************************************
INTEGER FUNCTION SCIPDefaultOptionsF( UserID,optionsIO )

USE inpstruct_fd

IMPLICIT NONE

!DEC$ IF DEFINED (DUALBUILD)
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'SCIPDEFAULTOPTIONSFOMP' :: SCIPDefaultOptionsF
!DEC$ ELSE
!DEC$ ATTRIBUTES DLLEXPORT :: SCIPDefaultOptionsF
!DEC$ ENDIF

INTEGER,           INTENT( IN    ) :: UserID
TYPE( poptionsT ), INTENT( INOUT ) :: optionsIO

INTEGER, EXTERNAL :: DefaultOptionsF

SCIPDefaultOptionsF = DefaultOptionsF( UserID,optionsIO )

RETURN
END
!*******************************************************************************
!            Default Project Material
!*******************************************************************************
INTEGER FUNCTION SCIPDefaultMaterialF( UserID,materialIO,mtlListIO )

USE mtlstruct_fd

IMPLICIT NONE

!DEC$ IF DEFINED (DUALBUILD)
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'SCIPDEFAULTMATERIALFOMP' :: SCIPDefaultMaterialF
!DEC$ ELSE
!DEC$ ATTRIBUTES DLLEXPORT :: SCIPDefaultMaterialF
!DEC$ ENDIF

INTEGER,                         INTENT( IN    ) :: UserID
TYPE( pmaterialT ),              INTENT( INOUT ) :: materialIO
TYPE( materialT ), DIMENSION(*), INTENT( INOUT ) :: mtlListIO

INTEGER, EXTERNAL :: DefaultMaterialF

SCIPDefaultMaterialF = DefaultMaterialF( UserID,materialIO,mtlListIO )

RETURN
END
!*******************************************************************************
!            Default Project Weather
!*******************************************************************************
INTEGER FUNCTION SCIPDefaultWeatherF( UserID,weatherIO )

USE metstruct_fd

IMPLICIT NONE

!DEC$ IF DEFINED (DUALBUILD)
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'SCIPDEFAULTWEATHERFOMP' :: SCIPDefaultWeatherF
!DEC$ ELSE
!DEC$ ATTRIBUTES DLLEXPORT :: SCIPDefaultWeatherF
!DEC$ ENDIF

INTEGER,           INTENT( IN    ) :: UserID
TYPE( pweatherT ), INTENT( INOUT ) :: weatherIO

INTEGER, EXTERNAL :: DefaultWeatherF

SCIPDefaultWeatherF = DefaultWeatherF( UserID,weatherIO )

RETURN
END
!*******************************************************************************
!            Default Project Time
!*******************************************************************************
INTEGER FUNCTION SCIPDefaultTimeF( UserID,timeIO )

USE timstruct_fd

IMPLICIT NONE

!DEC$ IF DEFINED (DUALBUILD)
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'SCIPDEFAULTTIMEFOMP' :: SCIPDefaultTimeF
!DEC$ ELSE
!DEC$ ATTRIBUTES DLLEXPORT :: SCIPDefaultTimeF
!DEC$ ENDIF

INTEGER,            INTENT( IN    ) :: UserID
TYPE( ptemporalT ), INTENT( INOUT ) :: timeIO

INTEGER, EXTERNAL :: DefaultTimeF

SCIPDefaultTimeF = DefaultTimeF( UserID,timeIO )

RETURN
END
!*******************************************************************************
!            Default Project Input
!*******************************************************************************
INTEGER FUNCTION SCIPDefaultInpF( UserID,inputIO,mtlListIO )

USE structure_fd

IMPLICIT NONE

!DEC$ IF DEFINED (DUALBUILD)
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'SCIPDEFAULTINPFOMP' :: SCIPDefaultInpF
!DEC$ ELSE
!DEC$ ATTRIBUTES DLLEXPORT :: SCIPDefaultInpF
!DEC$ ENDIF

INTEGER,                         INTENT( IN    ) :: UserID
TYPE( pinputT ),                 INTENT( INOUT ) :: inputIO
TYPE( materialT ), DIMENSION(*), INTENT( INOUT ) :: mtlListIO

INTEGER, EXTERNAL :: DefaultInpF

SCIPDefaultInpF = DefaultInpF( UserID,inputIO,mtlListIO )

RETURN
END

