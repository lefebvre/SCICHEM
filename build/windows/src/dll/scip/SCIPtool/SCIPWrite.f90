!*******************************************************************************
!$RCSfile$
!$Revision$
!$Date$
!*******************************************************************************
!*******************************************************************************
!            Write Project Ctrl
!*******************************************************************************
INTEGER FUNCTION SCIPWriteCtrlF( UserID,ctrlIO )

USE timstruct_fd

IMPLICIT NONE

!DEC$ IF DEFINED (DUALBUILD)
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'SCIPWRITECTRLFOMP' :: SCIPWriteCtrlF
!DEC$ ELSE
!DEC$ ATTRIBUTES DLLEXPORT :: SCIPWriteCtrlF
!DEC$ ENDIF

INTEGER,        INTENT( IN ) :: UserID
TYPE( pctrlT ), INTENT( IN ) :: ctrlIO

INTEGER, EXTERNAL :: WriteCtrlF

SCIPWriteCtrlF = WriteCtrlF( UserID,ctrlIO )

RETURN
END
!*******************************************************************************
!            Write Project Start
!*******************************************************************************
INTEGER FUNCTION SCIPWriteStartF( UserID,startIO )

USE timstruct_fd

IMPLICIT NONE

!DEC$ IF DEFINED (DUALBUILD)
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'SCIPWRITESTARTFOMP' :: SCIPWriteStartF
!DEC$ ELSE
!DEC$ ATTRIBUTES DLLEXPORT :: SCIPWriteStartF
!DEC$ ENDIF

INTEGER,         INTENT( IN ) :: UserID
TYPE( pstartT ), INTENT( IN ) :: startIO

INTEGER, EXTERNAL :: WriteStartF

SCIPWriteStartF = WriteStartF( UserID,startIO )

RETURN
END
!*******************************************************************************
!            Write Project End
!*******************************************************************************
INTEGER FUNCTION SCIPWriteEndF( UserID,endIO )

USE timstruct_fd

IMPLICIT NONE

!DEC$ IF DEFINED (DUALBUILD)
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'SCIPWRITEENDFOMP' :: SCIPWriteEndF
!DEC$ ELSE
!DEC$ ATTRIBUTES DLLEXPORT :: SCIPWriteEndF
!DEC$ ENDIF

INTEGER,       INTENT( IN ) :: UserID
TYPE( pendT ), INTENT( IN ) :: endIO

INTEGER, EXTERNAL :: WriteEndF

SCIPWriteEndF = WriteEndF( UserID,endIO )

RETURN
END
!*******************************************************************************
!            Write Project Flags
!*******************************************************************************
INTEGER FUNCTION SCIPWriteFlagsF( UserID,flagsIO )

USE inpstruct_fd

IMPLICIT NONE

!DEC$ IF DEFINED (DUALBUILD)
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'SCIPWRITEFLAGSFOMP' :: SCIPWriteFlagsF
!DEC$ ELSE
!DEC$ ATTRIBUTES DLLEXPORT :: SCIPWriteFlagsF
!DEC$ ENDIF

INTEGER,         INTENT( IN ) :: UserID
TYPE( pflagsT ), INTENT( IN ) :: flagsIO

INTEGER, EXTERNAL :: WriteFlagsF

SCIPWriteFlagsF = WriteFlagsF( UserID,flagsIO )

RETURN
END
!*******************************************************************************
!            Write Project Domain
!*******************************************************************************
INTEGER FUNCTION SCIPWriteDomainF( UserID,domainIO )

USE domstruct_fd

IMPLICIT NONE

!DEC$ IF DEFINED (DUALBUILD)
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'SCIPWRITEDOMAINFOMP' :: SCIPWriteDomainF
!DEC$ ELSE
!DEC$ ATTRIBUTES DLLEXPORT :: SCIPWriteDomainF
!DEC$ ENDIF

INTEGER,           INTENT( IN ) :: UserID
TYPE( pspatialT ), INTENT( IN ) :: domainIO

INTEGER, EXTERNAL :: WriteDomainF

SCIPWriteDomainF = WriteDomainF( UserID,domainIO )

RETURN
END
!*******************************************************************************
!            Write Project Options
!*******************************************************************************
INTEGER FUNCTION SCIPWriteOptionsF( UserID,optionsIO )

USE inpstruct_fd

IMPLICIT NONE

!DEC$ IF DEFINED (DUALBUILD)
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'SCIPWRITEOPTIONSFOMP' :: SCIPWriteOptionsF
!DEC$ ELSE
!DEC$ ATTRIBUTES DLLEXPORT :: SCIPWriteOptionsF
!DEC$ ENDIF

INTEGER,           INTENT( IN ) :: UserID
TYPE( poptionsT ), INTENT( IN ) :: optionsIO

INTEGER, EXTERNAL :: WriteOptionsF

SCIPWriteOptionsF = WriteOptionsF( UserID,optionsIO )

RETURN
END
!*******************************************************************************
!            Write Project Material
!*******************************************************************************
INTEGER FUNCTION SCIPWriteMaterialF( UserID,materialIO,mtlListIO )

USE mtlstruct_fd

IMPLICIT NONE

!DEC$ IF DEFINED (DUALBUILD)
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'SCIPWRITEMATERIALFOMP' :: SCIPWriteMaterialF
!DEC$ ELSE
!DEC$ ATTRIBUTES DLLEXPORT :: SCIPWriteMaterialF
!DEC$ ENDIF

INTEGER,                          INTENT( IN    ) :: UserID
TYPE( pmaterialT ),               INTENT( INOUT ) :: materialIO
TYPE( materialT  ), DIMENSION(*), INTENT( INOUT ) :: mtlListIO

INTEGER, EXTERNAL :: WriteMaterialF

SCIPWriteMaterialF = WriteMaterialF( UserID,materialIO,mtlListIO )

RETURN
END
!*******************************************************************************
!            Write Project Release
!*******************************************************************************
INTEGER FUNCTION SCIPWriteReleaseF( UserID,releaseIO,relListIO )

USE relstruct_fd

IMPLICIT NONE

!DEC$ IF DEFINED (DUALBUILD)
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'SCIPWRITERELEASEFOMP' :: SCIPWriteReleaseF
!DEC$ ELSE
!DEC$ ATTRIBUTES DLLEXPORT :: SCIPWriteReleaseF
!DEC$ ENDIF

INTEGER,                         INTENT( IN ) :: UserID
TYPE( preleaseT ),               INTENT( IN ) :: releaseIO
TYPE( releaseT  ), DIMENSION(*), INTENT( IN ) :: relListIO

INTEGER, EXTERNAL :: WriteReleaseF

SCIPWriteReleaseF = WriteReleaseF( UserID,releaseIO,relListIO )

RETURN
  END
!*******************************************************************************
!            Write Project ReleaseMC
!*******************************************************************************
INTEGER FUNCTION SCIPWriteReleaseMCF( UserID,releaseIO,relListIO,nMC,relMCList )

USE relstruct_fd

IMPLICIT NONE

!DEC$ IF DEFINED (DUALBUILD)
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'SCIPWRITERELEASEMCFOMP' :: SCIPWriteReleaseMCF
!DEC$ ELSE
!DEC$ ATTRIBUTES DLLEXPORT :: SCIPWriteReleaseMCF
!DEC$ ENDIF

INTEGER,                          INTENT( IN ) :: UserID
TYPE( preleaseT ),                INTENT( IN ) :: releaseIO
TYPE( releaseT  ), DIMENSION(*),  INTENT( IN ) :: relListIO
INTEGER,                          INTENT( IN ) :: nMC
TYPE( releaseMCT ), DIMENSION(*), INTENT( IN ) :: relMCList

INTEGER, EXTERNAL :: WriteReleaseMCF

SCIPWriteReleaseMCF = WriteReleaseMCF( UserID,releaseIO,relListIO,nMC,relMCList )

RETURN
END
!*******************************************************************************
!            Write Project Weather
!*******************************************************************************
INTEGER FUNCTION SCIPWriteWeatherF( UserID,weatherIO )

USE metstruct_fd

IMPLICIT NONE

!DEC$ IF DEFINED (DUALBUILD)
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'SCIPWRITEWEATHERFOMP' :: SCIPWriteWeatherF
!DEC$ ELSE
!DEC$ ATTRIBUTES DLLEXPORT :: SCIPWriteWeatherF
!DEC$ ENDIF

INTEGER,           INTENT( IN ) :: UserID
TYPE( pweatherT ), INTENT( IN ) :: weatherIO

INTEGER, EXTERNAL :: WriteWeatherF

SCIPWriteWeatherF = WriteWeatherF( UserID,weatherIO )

RETURN
END
!*******************************************************************************
!            Write Project Run
!*******************************************************************************
INTEGER FUNCTION SCIPWriteRunF( UserID,endIO )

USE timstruct_fd

IMPLICIT NONE

!DEC$ IF DEFINED (DUALBUILD)
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'SCIPWRITERUNFOMP' :: SCIPWriteRunF
!DEC$ ELSE
!DEC$ ATTRIBUTES DLLEXPORT :: SCIPWriteRunF
!DEC$ ENDIF

INTEGER,       INTENT( IN ) :: UserID
TYPE( pendT ), INTENT( IN ) :: endIO

INTEGER, EXTERNAL :: WriteRunF

SCIPWriteRunF = WriteRunF( UserID,endIO )

RETURN
END
!*******************************************************************************
!            Write Project Time
!*******************************************************************************
INTEGER FUNCTION SCIPWriteTimeF( UserID,timeIO )

USE timstruct_fd

IMPLICIT NONE

!DEC$ IF DEFINED (DUALBUILD)
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'SCIPWRITETIMEFOMP' :: SCIPWriteTimeF
!DEC$ ELSE
!DEC$ ATTRIBUTES DLLEXPORT :: SCIPWriteTimeF
!DEC$ ENDIF

INTEGER,            INTENT( IN ) :: UserID
TYPE( ptemporalT ), INTENT( IN ) :: timeIO

INTEGER, EXTERNAL :: WriteTimeF

SCIPWriteTimeF = WriteTimeF( UserID,timeIO )

RETURN
END
!*******************************************************************************
!            Write Project Input
!*******************************************************************************
INTEGER FUNCTION SCIPWriteInpF( UserID,inputIO,mtlListIO )

USE structure_fd

IMPLICIT NONE

!DEC$ IF DEFINED (DUALBUILD)
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'SCIPWRITEINPFOMP' :: SCIPWriteInpF
!DEC$ ELSE
!DEC$ ATTRIBUTES DLLEXPORT :: SCIPWriteInpF
!DEC$ ENDIF

INTEGER,                         INTENT( IN ) :: UserID
TYPE( pinputT ),                 INTENT( IN ) :: inputIO
TYPE( materialT ), DIMENSION(*), INTENT( IN ) :: mtlListIO

INTEGER, EXTERNAL :: WriteInpF

SCIPWriteInpF = WriteInpF( UserID,inputIO,mtlListIO )

RETURN
END

!*******************************************************************************
!            Write Project Input
!*******************************************************************************
INTEGER FUNCTION SCIPWriteRstF( UserID,inputIO )

USE structure_fd

IMPLICIT NONE

!DEC$ IF DEFINED (DUALBUILD)
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'SCIPWRITERSTFOMP' :: SCIPWriteRstF
!DEC$ ELSE
!DEC$ ATTRIBUTES DLLEXPORT :: SCIPWriteRstF
!DEC$ ENDIF

INTEGER,                         INTENT( IN ) :: UserID
TYPE( pinputT ),                 INTENT( IN ) :: inputIO

INTEGER, EXTERNAL :: WriteRst

SCIPWriteRstF = WriteRst( UserID,inputIO )

RETURN
END
!*******************************************************************************
!                SCIPWriteField
!*******************************************************************************
INTEGER FUNCTION SCIPWriteField( UserID,grdI,Field,PlotType,contourHead, &
                                 contourList,GUIWrite,nComment,Comment )

USE field_fd
USE contourlist_fd
USE charT_fd

IMPLICIT NONE

!DEC$ IF DEFINED (DUALBUILD)
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'SCIPWRITEFIELDOMP' :: SCIPWriteField
!DEC$ ELSE
!DEC$ ATTRIBUTES DLLEXPORT :: SCIPWriteField
!DEC$ ENDIF

!==============================================================================
! Function Arguments
!==============================================================================
INTEGER,                                                    INTENT( IN ) :: UserID      !User ID
INTEGER,                                                    INTENT( IN ) :: grdI        !SAG grid ID
TYPE( SCIPPlotFieldT ),                                     INTENT( IN ) :: Field       !Field definition
TYPE( SCIPPlotTypeT ),                                      INTENT( IN ) :: PlotType    !Plot definition
TYPE( SCIPContourHeaderT ),                                 INTENT( IN ) :: contourHead !Contour array header
TYPE( SCIPContourElementT ), DIMENSION(contourHead%number), TARGET, &
                                                         INTENT( INOUT ) :: contourList !Contour array
TYPE( ARAPWriteT ),                                         INTENT( IN ) :: GUIWrite    !Draw instructions
INTEGER,                                                    INTENT( IN ) :: nComment    !User supplied comments
TYPE( char128T ),            DIMENSION(nComment),           INTENT( IN ) :: Comment     !User supplied comments

INTERFACE
  INTEGER FUNCTION WriteFieldF( CallerID,grdID,Field,PlotType,contourHead,contourList, &
                                GUIWrite,nComment,Comment )
    USE tooluser_fd
    INTEGER,                                   INTENT( IN ) :: CallerID     !USER ID tag
    INTEGER,                                   INTENT( IN ) :: grdID        !SAG grid ID
    TYPE( SCIPPlotFieldT ),                    INTENT( IN ) :: Field        !Field descriptor
    TYPE( SCIPPlotTypeT ),                     INTENT( IN ) :: PlotType     !Plot definition
    TYPE( SCIPContourHeaderT ),                INTENT( IN ) :: contourHead  !Contour array header
    TYPE( SCIPContourElementT ), DIMENSION(contourHead%number), TARGET, &
                                            INTENT( INOUT ) :: contourList  !Contour array
    TYPE( ARAPWriteT ),                        INTENT( IN ) :: GUIWrite     !Draw instructions
    INTEGER,                                   INTENT( IN ) :: nComment     !Number of User supplied comments
    TYPE( char128T ),     DIMENSION(nComment), INTENT( IN ) :: Comment      !User supplied comments
  END FUNCTION WriteFieldF
END INTERFACE

SCIPWriteField = WriteFieldF( UserID,grdI,Field,PlotType,contourHead, &
                              contourList,GUIWrite,nComment,Comment )

RETURN
END
!*******************************************************************************
!                SCIPWriteSAGID
!*******************************************************************************
INTEGER FUNCTION SCIPWriteSAGID( UserID,grdI,file,append )

USE SCIPresults_fd
USE charT_fd

IMPLICIT NONE

!DEC$ IF DEFINED (DUALBUILD)
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'SCIPWRITESAGIDOMP' :: SCIPWriteSAGID
!DEC$ ELSE
!DEC$ ATTRIBUTES DLLEXPORT :: SCIPWriteSAGID
!DEC$ ENDIF

!==============================================================================
! Function Arguments
!==============================================================================
INTEGER,           INTENT( IN ) :: UserID      !User ID
INTEGER,           INTENT( IN ) :: grdI        !SAG grid ID
TYPE( fileNameT ), INTENT( IN ) :: file        !Filename
INTEGER,           INTENT( IN ) :: append      !Flag to append to existing file

INTEGER, EXTERNAL :: WriteSAGIDF

SCIPWriteSAGID = WriteSAGIDF( UserID,grdI,file,append )

RETURN
END
