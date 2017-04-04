!*******************************************************************************
!$RCSfile$
!$Revision$
!$Date$
!*******************************************************************************
!*******************************************************************************
!            Load Project Ctrl
!*******************************************************************************
INTEGER FUNCTION SCIPLoadCtrlF( UserID,ctrlIO )

USE timstruct_fd

IMPLICIT NONE

!DEC$ IF DEFINED (DUALBUILD)
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'SCIPLOADCTRLFOMP' :: SCIPLoadCtrlF
!DEC$ ELSE
!DEC$ ATTRIBUTES DLLEXPORT :: SCIPLoadCtrlF
!DEC$ ENDIF

INTEGER,        INTENT( IN    ) :: UserID
TYPE( pctrlT ), INTENT( INOUT ) :: ctrlIO

INTEGER, EXTERNAL :: LoadCtrlF

SCIPLoadCtrlF = LoadCtrlF( UserID,ctrlIO )

RETURN
END
!*******************************************************************************
!            Load Project Start
!*******************************************************************************
INTEGER FUNCTION SCIPLoadStartF( UserID,startIO )

USE timstruct_fd

IMPLICIT NONE

!DEC$ IF DEFINED (DUALBUILD)
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'SCIPLOADSTARTFOMP' :: SCIPLoadStartF
!DEC$ ELSE
!DEC$ ATTRIBUTES DLLEXPORT :: SCIPLoadStartF
!DEC$ ENDIF

INTEGER,         INTENT( IN    ) :: UserID
TYPE( pstartT ), INTENT( INOUT ) :: startIO

INTEGER, EXTERNAL :: LoadStartF

SCIPLoadStartF = LoadStartF( UserID,startIO )

RETURN
END
!*******************************************************************************
!            Load Project End
!*******************************************************************************
INTEGER FUNCTION SCIPLoadEndF( UserID,endIO )

USE timstruct_fd

IMPLICIT NONE

!DEC$ IF DEFINED (DUALBUILD)
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'SCIPLOADENDFOMP' :: SCIPLoadEndF
!DEC$ ELSE
!DEC$ ATTRIBUTES DLLEXPORT :: SCIPLoadEndF
!DEC$ ENDIF

INTEGER,       INTENT( IN    ) :: UserID
TYPE( pendT ), INTENT( INOUT ) :: endIO

INTEGER, EXTERNAL :: LoadEndF

SCIPLoadEndF = LoadEndF( UserID,endIO )

RETURN
END
!*******************************************************************************
!            Load Project End
!*******************************************************************************
INTEGER FUNCTION SCIPLoadEndXF( UserID,endIO,extFlag )

USE timstruct_fd

IMPLICIT NONE

!DEC$ IF DEFINED (DUALBUILD)
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'SCIPLOADENDXFOMP' :: SCIPLoadEndXF
!DEC$ ELSE
!DEC$ ATTRIBUTES DLLEXPORT :: SCIPLoadEndXF
!DEC$ ENDIF

INTEGER,       INTENT( IN    ) :: UserID
TYPE( pendT ), INTENT( INOUT ) :: endIO
INTEGER,       INTENT( IN    ) :: extFlag

INTEGER, EXTERNAL :: LoadEndXF

!==== Initialize

SCIPLoadEndXF = LoadEndXF( UserID,endIO,extFlag )

RETURN
END
!*******************************************************************************
!            Load Project Flags
!*******************************************************************************
INTEGER FUNCTION SCIPLoadFlagsF( UserID,flagsIO )

USE inpstruct_fd

IMPLICIT NONE

!DEC$ IF DEFINED (DUALBUILD)
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'SCIPLOADFLAGSFOMP' :: SCIPLoadFlagsF
!DEC$ ELSE
!DEC$ ATTRIBUTES DLLEXPORT :: SCIPLoadFlagsF
!DEC$ ENDIF

INTEGER,         INTENT( IN    ) :: UserID
TYPE( pflagsT ), INTENT( INOUT ) :: flagsIO

INTEGER, EXTERNAL :: LoadFlagsF

SCIPLoadFlagsF = LoadFlagsF( UserID,flagsIO )

RETURN
END
!*******************************************************************************
!            Load Project Domain
!*******************************************************************************
INTEGER FUNCTION SCIPLoadDomainF( UserID,domainIO )

USE domstruct_fd

IMPLICIT NONE

!DEC$ IF DEFINED (DUALBUILD)
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'SCIPLOADDOMAINFOMP' :: SCIPLoadDomainF
!DEC$ ELSE
!DEC$ ATTRIBUTES DLLEXPORT :: SCIPLoadDomainF
!DEC$ ENDIF

INTEGER,           INTENT( IN    ) :: UserID
TYPE( pspatialT ), INTENT( INOUT ) :: domainIO

INTEGER, EXTERNAL :: LoadDomainF

SCIPLoadDomainF = LoadDomainF( UserID,domainIO )

RETURN
END
!*******************************************************************************
!            Load Project Options
!*******************************************************************************
INTEGER FUNCTION SCIPLoadOptionsF( UserID,optionsIO )

USE inpstruct_fd

IMPLICIT NONE

!DEC$ IF DEFINED (DUALBUILD)
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'SCIPLOADOPTIONSFOMP' :: SCIPLoadOptionsF
!DEC$ ELSE
!DEC$ ATTRIBUTES DLLEXPORT :: SCIPLoadOptionsF
!DEC$ ENDIF

INTEGER,           INTENT( IN    ) :: UserID
TYPE( poptionsT ), INTENT( INOUT ) :: optionsIO

INTEGER, EXTERNAL :: LoadOptionsF

SCIPLoadOptionsF = LoadOptionsF( UserID,optionsIO )

RETURN
END
!*******************************************************************************
!            Load Project Material
!*******************************************************************************
INTEGER FUNCTION SCIPLoadMaterialF( UserID,materialIO,mtlListIO )

USE mtlstruct_fd

IMPLICIT NONE

!DEC$ IF DEFINED (DUALBUILD)
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'SCIPLOADMATERIALFOMP' :: SCIPLoadMaterialF
!DEC$ ELSE
!DEC$ ATTRIBUTES DLLEXPORT :: SCIPLoadMaterialF
!DEC$ ENDIF

INTEGER,                          INTENT( IN    ) :: UserID
TYPE( pmaterialT ),               INTENT( INOUT ) :: materialIO
TYPE( materialT  ), DIMENSION(*), INTENT( INOUT ) :: mtlListIO

INTEGER, EXTERNAL :: LoadMaterialF

SCIPLoadMaterialF = LoadMaterialF( UserID,materialIO,mtlListIO )

RETURN
END
!*******************************************************************************
!            Load Project Release
!*******************************************************************************
INTEGER FUNCTION SCIPLoadReleaseF( UserID,releaseIO,relListIO )

USE relstruct_fd

IMPLICIT NONE

!DEC$ IF DEFINED (DUALBUILD)
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'SCIPLOADRELEASEFOMP' :: SCIPLoadReleaseF
!DEC$ ELSE
!DEC$ ATTRIBUTES DLLEXPORT :: SCIPLoadReleaseF
!DEC$ ENDIF

INTEGER,                         INTENT( IN    ) :: UserID
TYPE( preleaseT ),               INTENT( INOUT ) :: releaseIO
TYPE( releaseT  ), DIMENSION(*), INTENT( INOUT ) :: relListIO

INTEGER, EXTERNAL :: LoadReleaseF

SCIPLoadReleaseF = LoadReleaseF( UserID,releaseIO,relListIO )

RETURN
END
!*******************************************************************************
!            Load Project Weather
!*******************************************************************************
INTEGER FUNCTION SCIPLoadWeatherF( UserID,weatherIO )

USE metstruct_fd

IMPLICIT NONE

!DEC$ IF DEFINED (DUALBUILD)
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'SCIPLOADWEATHERFOMP' :: SCIPLoadWeatherF
!DEC$ ELSE
!DEC$ ATTRIBUTES DLLEXPORT :: SCIPLoadWeatherF
!DEC$ ENDIF

INTEGER,           INTENT( IN    ) :: UserID
TYPE( pweatherT ), INTENT( INOUT ) :: weatherIO

INTEGER, EXTERNAL :: LoadWeatherF

SCIPLoadWeatherF = LoadWeatherF( UserID,weatherIO )

RETURN
END
!*******************************************************************************
!            Load Project Status
!*******************************************************************************
INTEGER FUNCTION SCIPLoadStatusF( UserID,statusIO,statListIO )

USE statstruct_fd

IMPLICIT NONE

!DEC$ IF DEFINED (DUALBUILD)
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'SCIPLOADSTATUSFOMP' :: SCIPLoadStatusF
!DEC$ ELSE
!DEC$ ATTRIBUTES DLLEXPORT :: SCIPLoadStatusF
!DEC$ ENDIF

INTEGER,               INTENT( IN    ) :: UserID
TYPE( pstatusT ),      INTENT( INOUT ) :: statusIO
INTEGER, DIMENSION(*), INTENT( INOUT ) :: statListIO

INTEGER, EXTERNAL :: LoadStatusF

!==== Initialize

SCIPLoadStatusF = LoadStatusF( UserID,statusIO,statListIO )

RETURN
END
!*******************************************************************************
!            Load Project Run
!*******************************************************************************
INTEGER FUNCTION SCIPLoadRunF( UserID,endIO )

USE timstruct_fd

IMPLICIT NONE

!DEC$ IF DEFINED (DUALBUILD)
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'SCIPLOADRUNFOMP' :: SCIPLoadRunF
!DEC$ ELSE
!DEC$ ATTRIBUTES DLLEXPORT :: SCIPLoadRunF
!DEC$ ENDIF

INTEGER,       INTENT( IN    ) :: UserID
TYPE( pendT ), INTENT( INOUT ) :: endIO

INTEGER, EXTERNAL :: LoadRunF

SCIPLoadRunF = LoadRunF( UserID,endIO )

RETURN
END
!*******************************************************************************
!            Load Project Time
!*******************************************************************************
INTEGER FUNCTION SCIPLoadTimeF( UserID,timeIO )

USE timstruct_fd

IMPLICIT NONE

!DEC$ IF DEFINED (DUALBUILD)
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'SCIPLOADTIMEFOMP' :: SCIPLoadTimeF
!DEC$ ELSE
!DEC$ ATTRIBUTES DLLEXPORT :: SCIPLoadTimeF
!DEC$ ENDIF

INTEGER,            INTENT( IN    ) :: UserID
TYPE( ptemporalT ), INTENT( INOUT ) :: timeIO

INTEGER, EXTERNAL :: LoadTimeF

SCIPLoadTimeF = LoadTimeF( UserID,timeIO )

RETURN
END
!*******************************************************************************
!            Load Project Input
!*******************************************************************************
INTEGER FUNCTION SCIPLoadInpF( UserID,inputIO,mtlListIO )

USE structure_fd

IMPLICIT NONE

!DEC$ IF DEFINED (DUALBUILD)
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'SCIPLOADINPFOMP' :: SCIPLoadInpF
!DEC$ ELSE
!DEC$ ATTRIBUTES DLLEXPORT :: SCIPLoadInpF
!DEC$ ENDIF

INTEGER,                          INTENT( IN    ) :: UserID
TYPE( pinputT ),                  INTENT( INOUT ) :: inputIO
TYPE( materialT  ), DIMENSION(*), INTENT( INOUT ) :: mtlListIO

INTEGER, EXTERNAL :: LoadInpF

SCIPLoadInpF = LoadInpF( UserID,inputIO,mtlListIO )

RETURN
END

!*******************************************************************************
!            Load Project Special Restart Input
!*******************************************************************************
INTEGER FUNCTION SCIPLoadRstF( UserID,inputIO )

USE structure_fd

IMPLICIT NONE

!DEC$ IF DEFINED (DUALBUILD)
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'SCIPLOADRSTFOMP' :: SCIPLoadRstF
!DEC$ ELSE
!DEC$ ATTRIBUTES DLLEXPORT :: SCIPLoadRstF
!DEC$ ENDIF

INTEGER,                          INTENT( IN    ) :: UserID
TYPE( pinputT ),                  INTENT( INOUT ) :: inputIO

INTEGER, EXTERNAL :: LoadRst

SCIPLoadRstF = LoadRst( UserID,inputIO )

RETURN
END
!*******************************************************************************
!            Load Met Output
!*******************************************************************************
INTEGER FUNCTION SCIPNumMetOutput( userID,Project,num2Dfields,num3Dfields, &
                                   numGrids,numTimes )

!------ Set met plot allocation requirements

USE prjstruct_fd

IMPLICIT NONE

!DEC$ IF DEFINED (DUALBUILD)
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'SCIPNUMMETOUTPUTOMP' :: SCIPNumMetOutput
!DEC$ ELSE
!DEC$ ATTRIBUTES DLLEXPORT :: SCIPNumMetOutput
!DEC$ ENDIF

INTEGER,            INTENT( IN  ) :: userID
TYPE( projectIDT ), INTENT( IN  ) :: Project
INTEGER,            INTENT( OUT ) :: num2Dfields, num3Dfields, numGrids, numTimes

INTEGER , EXTERNAL :: NumMetOutput

SCIPNumMetOutput = NumMetOutput( userID,Project,num2Dfields,num3Dfields, &
                                 numGrids,numTimes )

RETURN
END

!==============================================================================

INTEGER FUNCTION SCIPGetMetOutput( userID,project,n2D,n3D,name2D,units2D,name3D,units3D, &
                                   grid,time,field2Dlist,field3Dlist )

!------ Set met plot allocation requirements

USE prjstruct_fd
USE charT_fd
USE plotlist_fd
USE plotmet_fd

IMPLICIT NONE

!DEC$ IF DEFINED (DUALBUILD)
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'SCIPGETMETOUTPUTOMP' :: SCIPGetMetOutput
!DEC$ ELSE
!DEC$ ATTRIBUTES DLLEXPORT :: SCIPGetMetOutput
!DEC$ ENDIF

INTEGER,                                 INTENT( IN  ) :: userID
TYPE( projectIDT ),                      INTENT( IN  ) :: Project
INTEGER,                                 INTENT( IN  ) :: n2D, n3D
TYPE( char64T ),       DIMENSION(n2D),   INTENT( OUT ) :: name2D
TYPE( char64T ),       DIMENSION(n2D),   INTENT( OUT ) :: units2D
TYPE( char64T ),       DIMENSION(n3D),   INTENT( OUT ) :: name3D
TYPE( char64T ),       DIMENSION(n3D),   INTENT( OUT ) :: units3D
TYPE( metGridT ),      DIMENSION(*),     INTENT( OUT ) :: grid
TYPE( SCIPTimeT ),     DIMENSION(*),     INTENT( OUT ) :: time
TYPE( metGridFieldT ), DIMENSION(n2D,*), INTENT( OUT ) :: field2Dlist
TYPE( metGridFieldT ), DIMENSION(n3D,*), INTENT( OUT ) :: field3Dlist

INTEGER , EXTERNAL :: GetMetOutput

SCIPGetMetOutput = GetMetOutput( userID,project,n2D,n3D,name2D,units2D,name3D,units3D, &
                                   grid,time,field2Dlist,field3Dlist )

RETURN
END

!==============================================================================

INTEGER FUNCTION SCIPGetMetVertGrid( userID,Project,gridID,sigma )

!------ Set met plot allocation requirements

USE prjstruct_fd

IMPLICIT NONE

!DEC$ IF DEFINED (DUALBUILD)
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'SCIPGETMETVERTGRIDOMP' :: SCIPGetMetVertGrid
!DEC$ ELSE
!DEC$ ATTRIBUTES DLLEXPORT :: SCIPGetMetVertGrid
!DEC$ ENDIF

INTEGER,            INTENT( IN  ) :: userID
TYPE( projectIDT ), INTENT( IN  ) :: Project
INTEGER,            INTENT( IN  ) :: gridID
REAL, DIMENSION(*), INTENT( OUT ) :: sigma

INTEGER, EXTERNAL :: GetMetVertGrid

SCIPGetMetVertGrid = GetMetVertGrid( userID,Project,gridID,sigma )

RETURN
END

!==============================================================================

INTEGER FUNCTION SCIPGet2DMetData( userID,Project,gridID,timeID,numFields, &
                                   nameFields,nx,ny,data )

!------ Set met plot allocation requirements

USE prjstruct_fd
USE charT_fd

IMPLICIT NONE

!DEC$ IF DEFINED (DUALBUILD)
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'SCIPGET2DMETDATAOMP' :: SCIPGet2DMetData
!DEC$ ELSE
!DEC$ ATTRIBUTES DLLEXPORT :: SCIPGet2DMetData
!DEC$ ENDIF

INTEGER,                               INTENT( IN  ) :: userID
TYPE( projectIDT ),                    INTENT( IN  ) :: Project
INTEGER,                               INTENT( IN  ) :: gridID
INTEGER,                               INTENT( IN  ) :: TimeID
INTEGER,                               INTENT( IN  ) :: numFields
TYPE( char64T ), DIMENSION(numFields), INTENT( IN  ) :: nameFields
INTEGER,                               INTENT( IN  ) :: nx, ny
REAL,      DIMENSION(nx,ny,numFields), INTENT( OUT ) :: data

INTEGER, EXTERNAL :: Get2DMetData

SCIPGet2DMetData = Get2DMetData( userID,Project,gridID,timeID,numFields, &
                                 nameFields,nx,ny,data )

RETURN
END

!==============================================================================

INTEGER FUNCTION SCIPGet3DMetData( userID,Project,gridID,timeID,numFields, &
                                   nameFields,centerFlag,nx,ny,nz,data )

!------ Set met plot allocation requirements

USE prjstruct_fd
USE charT_fd

IMPLICIT NONE

!DEC$ IF DEFINED (DUALBUILD)
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'SCIPGET3DMETDATAOMP' :: SCIPGet3DMetData
!DEC$ ELSE
!DEC$ ATTRIBUTES DLLEXPORT :: SCIPGet3DMetData
!DEC$ ENDIF

INTEGER,                               INTENT( IN  ) :: userID
TYPE( projectIDT ),                    INTENT( IN  ) :: Project
INTEGER,                               INTENT( IN  ) :: gridID
INTEGER,                               INTENT( IN  ) :: TimeID
INTEGER,                               INTENT( IN  ) :: numFields
TYPE( char64T ), DIMENSION(numFields), INTENT( IN  ) :: nameFields
INTEGER,         DIMENSION(numFields), INTENT( IN  ) :: centerFlag
INTEGER,                               INTENT( IN  ) :: nx, ny, nz
REAL,   DIMENSION(nx,ny,nz,numFields), INTENT( OUT ) :: data

INTERFACE
  INTEGER FUNCTION Get3DMetData( userID,Project,gridID,timeID,numFields, &
                                 nameFields,centerFlag,nx,ny,nz,data )
    USE SCIMgr_fd
    INTEGER,                               INTENT( IN  ) :: userID
    TYPE( projectIDT ),                    INTENT( IN  ) :: Project
    INTEGER,                               INTENT( IN  ) :: gridID
    INTEGER,                               INTENT( IN  ) :: TimeID
    INTEGER,                               INTENT( IN  ) :: numFields
    TYPE( char64T ), DIMENSION(numFields), INTENT( IN  ) :: nameFields
    INTEGER,         DIMENSION(numFields), INTENT( IN  ) :: centerFlag
    INTEGER,                               INTENT( IN  ) :: nx, ny, nz
    REAL,   DIMENSION(nx,ny,nz,numFields), INTENT( OUT ),              &
                                       TARGET        :: data
  END FUNCTION Get3DMetData
END INTERFACE

SCIPGet3DMetData = Get3DMetData( userID,Project,gridID,timeID,numFields, &
                                 nameFields,centerFlag,nx,ny,nz,data )

RETURN
END
!*******************************************************************************
!            Load Puff Output
!*******************************************************************************
INTEGER FUNCTION SCIPGetProjectPuffHeader( UserID,puffHead,timeID )

USE spcstruct_fd

IMPLICIT NONE

!DEC$ IF DEFINED (DUALBUILD)
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'SCIPGETPROJECTPUFFHEADEROMP' :: SCIPGetProjectPuffHeader
!DEC$ ELSE
!DEC$ ATTRIBUTES DLLEXPORT :: SCIPGetProjectPuffHeader
!DEC$ ENDIF

INTEGER,            INTENT( IN    ) :: UserID   !USER ID Tag
TYPE( ppuffHeadT ), INTENT( INOUT ) :: puffHead !Project ID
INTEGER,            INTENT( IN    ) :: timeID   !Time ID for reading puffs

INTEGER, EXTERNAL :: GetProjectPuffHeader

SCIPGetProjectPuffHeader = GetProjectPuffHeader( UserID,puffHead,timeID )

RETURN
END

!==============================================================================

INTEGER FUNCTION SCIPGetProjectPuff( UserID,puffHead,timeID,transFlag,puffList,auxData,typeList,mcList )

USE spcstruct_fd
USE mcstruct_fd

IMPLICIT NONE

!DEC$ IF DEFINED (DUALBUILD)
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'SCIPGETPROJECTPUFFOMP' :: SCIPGetProjectPuff
!DEC$ ELSE
!DEC$ ATTRIBUTES DLLEXPORT :: SCIPGetProjectPuff
!DEC$ ENDIF

INTEGER,                           INTENT( IN    ) :: UserID      !USER ID Tag
TYPE( ppuffHeadT ),                INTENT( INOUT ) :: puffHead    !Project ID
INTEGER,                           INTENT( IN    ) :: timeID      !Time ID for reading puffs
INTEGER,                           INTENT( IN    ) :: transFlag   !SCIPtrue => transform to LLA
TYPE( puffT ),       DIMENSION(*), INTENT( OUT   ) :: puffList    !Puffs
REAL,                DIMENSION(*), INTENT( OUT   ) :: auxData     !Puff auxiliary data
TYPE( puffTypeT ),   DIMENSION(*), INTENT( OUT   ) :: typeList    !Puffs types
TYPE( material_MClist ),           INTENT( OUT   ) :: mcList      !multicomponent types

INTEGER, EXTERNAL :: GetProjectPuff

SCIPGetProjectPuff = GetProjectPuff( UserID,puffHead,timeID,transFlag,puffList,auxData,typeList,mcList )

RETURN
END
!*******************************************************************************
!            Load Project Input
!*******************************************************************************
INTEGER FUNCTION SCIPSizeProject( UserID,project,nMtl,nRel )

USE prjstruct_fd

IMPLICIT NONE

!DEC$ IF DEFINED (DUALBUILD)
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'SCIPSIZEPROJECTOMP' :: SCIPSizeProject
!DEC$ ELSE
!DEC$ ATTRIBUTES DLLEXPORT :: SCIPSizeProject
!DEC$ ENDIF

INTEGER,             INTENT( IN  ) :: UserID  !USER ID tag
TYPE ( projectIDT ), INTENT( IN  ) :: project !Project ID
INTEGER,             INTENT( OUT ) :: nMtl    !number of materials in file
INTEGER,             INTENT( OUT ) :: nRel    !number of releases in file

INTEGER nMCrel

INTEGER, EXTERNAL :: SizeProject

SCIPSizeProject = SizeProject( UserID,project,nMtl,nRel )

RETURN
END

!==============================================================================

INTEGER FUNCTION SCIPLoadProject( UserID,project,mtlList,relList )

USE structure_fd

IMPLICIT NONE

!DEC$ IF DEFINED (DUALBUILD)
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'SCIPLOADPROJECTOMP' :: SCIPLoadProject
!DEC$ ELSE
!DEC$ ATTRIBUTES DLLEXPORT :: SCIPLoadProject
!DEC$ ENDIF

INTEGER,                        INTENT( IN    ) :: UserID !USER ID Tag
TYPE( projectT ),               INTENT( INOUT ) :: project !Project ID
TYPE( materialT ),DIMENSION(*), INTENT( OUT   ) :: mtlList !Material list
TYPE( releaseT ), DIMENSION(*), INTENT( OUT   ) :: relList !Release list

INTEGER, EXTERNAL :: LoadProject

SCIPLoadProject = LoadProject( UserID,project,mtlList,relList )

RETURN
END
!*******************************************************************************
!            Get Project Puff Chem
!*******************************************************************************
INTEGER FUNCTION SCIPGetProjectPuffChem( UserID,project,chemOut,doData )

USE prjstruct_fd
USE multcomp_fd
USE SCIPresults_fd

IMPLICIT NONE

!DEC$ IF DEFINED (DUALBUILD)
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'SCIPGETPROJECTPUFFCHEMOMP' :: SCIPGetProjectPuffChem
!DEC$ ELSE
!DEC$ ATTRIBUTES DLLEXPORT :: SCIPGetProjectPuffChem
!DEC$ ENDIF

INTEGER,                             INTENT( IN    ) :: UserID     !USER ID Tag
TYPE( projectIDT ),                  INTENT( IN    ) :: project    !Project ID
TYPE( ChemMC_out ), DIMENSION(*),    INTENT( INOUT ) :: chemOut
INTEGER,                             INTENT( IN    ) :: doData

INTEGER, EXTERNAL :: GetProjectPuffChem

SCIPGetProjectPuffChem = GetProjectPuffChem( UserID,project,chemOut,(doData==SCIPtrue) )

RETURN
END



