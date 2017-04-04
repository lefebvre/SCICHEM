!*******************************************************************************
!$RCSfile$
!$Revision$
!$Date$
!*******************************************************************************
!DEC$ IF DEFINED (DUALBUILD)
MODULE SCIPtoolOMP
!DEC$ ELSE
MODULE SCIPtool
!DEC$ ENDIF

!Interface definitions for the SCIPtool exported functions

  INTERFACE

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPAdjointReleaseFilterOMP( CallerID,nRel,relList,nMat,matList,maxHit )
!DEC$ ELSE
    INTEGER FUNCTION SCIPAdjointReleaseFilter( CallerID,nRel,relList,nMat,matList,maxHit )
!DEC$ ENDIF
      USE tooluser_fd
      INTEGER,                                    INTENT( IN    ) :: CallerID   !USER ID tag
      INTEGER,                                    INTENT( INOUT ) :: nRel       !Number of releases
      TYPE( releaseT  ),DIMENSION(*),             INTENT( INOUT ) :: relList    !Release list
      INTEGER,                                    INTENT( INOUT ) :: nMat       !Number of materials
      TYPE( materialT ),DIMENSION(*),             INTENT( INOUT ) :: matList    !Material list
      INTEGER,                                    INTENT( IN    ) :: maxHit     !Max no. of hits
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPAdjointReleaseFilterOMP
!DEC$ ELSE
    END FUNCTION SCIPAdjointReleaseFilter
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPButtonOMP( CallerID,ButtonID )
!DEC$ ELSE
    INTEGER FUNCTION SCIPButton( CallerID,ButtonID )
!DEC$ ENDIF
      INTEGER,                                     INTENT( IN  ) :: CallerID    !USER ID Tag
      INTEGER,                                     INTENT( IN  ) :: ButtonID    !Progress Box Button ID
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPButtonOMP
!DEC$ ELSE
    END FUNCTION SCIPButton
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPCheckButtonsOMP( CallerID )
!DEC$ ELSE
    INTEGER FUNCTION SCIPCheckButtons( CallerID )
!DEC$ ENDIF
      INTEGER,                                     INTENT( IN  ) :: CallerID    !USER ID Tag
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPCheckButtonsOMP
!DEC$ ELSE
    END FUNCTION SCIPCheckButtons
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPCheckInputOMP( CallerID,iInput,tInput0,tInput1 )
!DEC$ ELSE
    INTEGER FUNCTION SCIPCheckInput( CallerID,iInput,tInput0,tInput1 )
!DEC$ ENDIF
      INTEGER,                                      INTENT( IN ) :: CallerID     !USER ID tag
      INTEGER,                                      INTENT( IN ) :: iInput       !Request type
      INTEGER, DIMENSION(*),                        INTENT( IN ) :: tInput0      !Input data
      INTEGER, DIMENSION(*),                        INTENT( IN ) :: tInput1      !Input data
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPCheckInputOMP
!DEC$ ELSE
    END FUNCTION SCIPCheckInput
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPContourCountOMP( CallerID,grdID,Field,PlotType,contourHead, &
                                       contourList,Mode,nLine,nPoint )
!DEC$ ELSE
    INTEGER FUNCTION SCIPContourCount( CallerID,grdID,Field,PlotType,contourHead, &
                                       contourList,Mode,nLine,nPoint )
!DEC$ ENDIF
      USE tooluser_fd
      INTEGER,                                     INTENT( IN  ) :: CallerID     !USER ID tag
      INTEGER,                                     INTENT( IN  ) :: grdID        !SAG grid ID
      TYPE( SCIPPlotFieldT ),                      INTENT( IN  ) :: Field        !Field descriptor
      TYPE( SCIPPlotTypeT ),                       INTENT( IN  ) :: PlotType     !Plot definition
      TYPE( SCIPContourHeaderT ),                  INTENT( IN  ) :: contourHead  !Contour array header
      TYPE( SCIPContourElementT ), DIMENSION(contourHead%number), TARGET, &
                                                   INTENT( IN  ) :: contourList  !Contour array
      INTEGER,                                     INTENT( IN  ) :: Mode         !Contour Mode
      INTEGER,                                     INTENT( OUT ) :: nLine        !Total number of lines
      INTEGER,                                     INTENT( OUT ) :: nPoint       !Total number of points
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPContourCountOMP
!DEC$ ELSE
    END FUNCTION SCIPContourCount
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPContourFieldOMP( CallerID,grdID,Field,PlotType,contourHead, &
                                       contourList,Mode,Line,Point )
!DEC$ ELSE
    INTEGER FUNCTION SCIPContourField( CallerID,grdID,Field,PlotType,contourHead, &
                                       contourList,Mode,Line,Point )
!DEC$ ENDIF
      USE tooluser_fd
      INTEGER,                                   INTENT( IN    ) :: CallerID     !USER ID tag
      INTEGER,                                   INTENT( IN    ) :: grdID        !SAG grid ID
      TYPE( SCIPPlotFieldT ),                    INTENT( IN    ) :: Field        !Field descriptor
      TYPE( SCIPPlotTypeT ),                     INTENT( IN    ) :: PlotType     !Plot definition
      TYPE( SCIPContourHeaderT ),                INTENT( IN    ) :: contourHead  !Contour array header
      TYPE( SCIPContourElementT ), DIMENSION(contourHead%number), TARGET, &
                                                 INTENT( INOUT ) :: contourList  !Contour array
      INTEGER,                                   INTENT( IN    ) :: Mode         !Contour Mode
      TYPE( SCIPLineT  ), DIMENSION(*),          INTENT( OUT   ) :: Line         !Lines output array
      TYPE( SCIPPointT ), DIMENSION(*),          INTENT( OUT   ) :: Point        !Points output array
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPContourFieldOMP
!DEC$ ELSE
    END FUNCTION SCIPContourField
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPCountMaterialOMP( CallerID,file,nMtl )
!DEC$ ELSE
    INTEGER FUNCTION SCIPCountMaterial( CallerID,file,nMtl )
!DEC$ ENDIF
      USE tooluser_fd
      INTEGER,                                     INTENT( IN  ) :: CallerID     !USER ID tag
      TYPE( fileNameT ),                           INTENT( IN  ) :: file         !filename
      INTEGER,                                     INTENT( OUT ) :: nMtl         !number of materials in file
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPCountMaterialOMP
!DEC$ ELSE
    END FUNCTION SCIPCountMaterial
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPCountReleaseMCOMP( CallerID,file,nRel,nMCrel )
!DEC$ ELSE
    INTEGER FUNCTION SCIPCountReleaseMC( CallerID,file,nRel,nMCrel )
!DEC$ ENDIF
      USE tooluser_fd
      INTEGER,                                     INTENT( IN  ) :: CallerID     !USER ID tag
      TYPE( fileNameT ),                           INTENT( IN  ) :: file         !filename
      INTEGER,                                     INTENT( OUT ) :: nRel         !number of releases in file
      INTEGER,                                     INTENT( OUT ) :: nMCrel       !number of MC records in file
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPCountReleaseMCOMP
!DEC$ ELSE
    END FUNCTION SCIPCountReleaseMC
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPCountReleaseOMP( CallerID,file,nRel )
!DEC$ ELSE
    INTEGER FUNCTION SCIPCountRelease( CallerID,file,nRel )
!DEC$ ENDIF
      USE tooluser_fd
      INTEGER,                                     INTENT( IN  ) :: CallerID     !USER ID tag
      TYPE( fileNameT ),                           INTENT( IN  ) :: file         !filename
      INTEGER,                                     INTENT( OUT ) :: nRel         !number of releases in file
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPCountReleaseOMP
!DEC$ ELSE
    END FUNCTION SCIPCountRelease
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPCountMCRelOMP( CallerID,file,nMCrel )
!DEC$ ELSE
    INTEGER FUNCTION SCIPCountMCRel( CallerID,file,nMCrel )
!DEC$ ENDIF
      USE tooluser_fd
      INTEGER,                                     INTENT( IN  ) :: CallerID     !USER ID tag
      TYPE( fileNameT ),                           INTENT( IN  ) :: file         !filename
      INTEGER,                                     INTENT( OUT ) :: nMCrel       !number of MC records in file
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPCountMCRelOMP
!DEC$ ELSE
    END FUNCTION SCIPCountMCRel
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPCreateFieldOMP( CallerID,Field,ClassData )
!DEC$ ELSE
    INTEGER FUNCTION SCIPCreateField( CallerID,Field,ClassData )
!DEC$ ENDIF
      USE tooluser_fd
      INTEGER,                                   INTENT( IN    ) :: CallerID     !USER ID Tag
      TYPE( SCIPPlotFieldT ),                    INTENT( INOUT ) :: Field        !Field descriptor
      REAL, DIMENSION(*),                        INTENT( IN    ) :: ClassData    !Additional Class data
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPCreateFieldOMP
!DEC$ ELSE
    END FUNCTION SCIPCreateField
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPDefaultInputOMP( CallerID,iInput,tInput0,tInput1 )
!DEC$ ELSE
    INTEGER FUNCTION SCIPDefaultInput( CallerID,iInput,tInput0,tInput1 )
!DEC$ ENDIF
      INTEGER,                                   INTENT( IN    ) :: CallerID     !USER ID tag
      INTEGER,                                   INTENT( IN    ) :: iInput       !Request type
      INTEGER, DIMENSION(*),                     INTENT( INOUT ) :: tInput0      !Results data
      INTEGER, DIMENSION(*),                     INTENT( INOUT ) :: tInput1      !Results data
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPDefaultInputOMP
!DEC$ ELSE
    END FUNCTION SCIPDefaultInput
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPDefaultCtrlFOMP( CallerID,ctrl )
!DEC$ ELSE
    INTEGER FUNCTION SCIPDefaultCtrlF( CallerID,ctrl )
!DEC$ ENDIF
      USE timstruct_fd
      INTEGER,                                   INTENT( IN    ) :: CallerID     !USER ID tag
      TYPE( pctrlT ),                            INTENT( INOUT ) :: ctrl
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPDefaultCtrlFOMP
!DEC$ ELSE
    END FUNCTION SCIPDefaultCtrlF
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPDefaultDomainFOMP( CallerID,domain )
!DEC$ ELSE
    INTEGER FUNCTION SCIPDefaultDomainF( CallerID,domain )
!DEC$ ENDIF
      USE domstruct_fd
      INTEGER,                                   INTENT( IN    ) :: CallerID     !USER ID tag
      TYPE( pspatialT ),                         INTENT( INOUT ) :: domain
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPDefaultDomainFOMP
!DEC$ ELSE
    END FUNCTION SCIPDefaultDomainF
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPDefaultEndFOMP( CallerID,end )
!DEC$ ELSE
    INTEGER FUNCTION SCIPDefaultEndF( CallerID,end )
!DEC$ ENDIF
      USE timstruct_fd
      INTEGER,                                   INTENT( IN    ) :: CallerID     !USER ID tag
      TYPE( pendT ),                             INTENT( INOUT ) :: end
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPDefaultEndFOMP
!DEC$ ELSE
    END FUNCTION SCIPDefaultEndF
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPDefaultFlagsFOMP( CallerID,flags )
!DEC$ ELSE
    INTEGER FUNCTION SCIPDefaultFlagsF( CallerID,flags )
!DEC$ ENDIF
      USE inpstruct_fd
      INTEGER,                                   INTENT( IN    ) :: CallerID     !USER ID tag
      TYPE( pflagsT ),                           INTENT( INOUT ) :: flags
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPDefaultFlagsFOMP
!DEC$ ELSE
    END FUNCTION SCIPDefaultFlagsF
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPDefaultInpFOMP( CallerID,input,mtlList )
!DEC$ ELSE
    INTEGER FUNCTION SCIPDefaultInpF( CallerID,input,mtlList )
!DEC$ ENDIF
      USE structure_fd
      USE mtlstruct_fd
      INTEGER,                                   INTENT( IN    ) :: CallerID     !USER ID tag
      TYPE( pinputT ),                           INTENT( INOUT ) :: input
      TYPE( materialT  ), DIMENSION(*),          INTENT( INOUT ) :: mtlList
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPDefaultInpFOMP
!DEC$ ELSE
    END FUNCTION SCIPDefaultInpF
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPDefaultMaterialFOMP( CallerID,material,mtlList )
!DEC$ ELSE
    INTEGER FUNCTION SCIPDefaultMaterialF( CallerID,material,mtlList )
!DEC$ ENDIF
      USE mtlstruct_fd
      INTEGER,                                   INTENT( IN    ) :: CallerID     !USER ID tag
      TYPE( pmaterialT ),                        INTENT( INOUT ) :: material
      TYPE( materialT  ), DIMENSION(*),          INTENT( INOUT ) :: mtlList
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPDefaultMaterialFOMP
!DEC$ ELSE
    END FUNCTION SCIPDefaultMaterialF
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPDefaultOptionsFOMP( CallerID,options )
!DEC$ ELSE
    INTEGER FUNCTION SCIPDefaultOptionsF( CallerID,options )
!DEC$ ENDIF
      USE inpstruct_fd
      INTEGER,                                   INTENT( IN    ) :: CallerID     !USER ID tag
      TYPE( poptionsT ),                         INTENT( INOUT ) :: options
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPDefaultOptionsFOMP
!DEC$ ELSE
    END FUNCTION SCIPDefaultOptionsF
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPDefaultStartFOMP( CallerID,start )
!DEC$ ELSE
    INTEGER FUNCTION SCIPDefaultStartF( CallerID,start )
!DEC$ ENDIF
      USE timstruct_fd
      INTEGER,                                   INTENT( IN    ) :: CallerID     !USER ID tag
      TYPE( pstartT ),                           INTENT( INOUT ) :: start
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPDefaultStartFOMP
!DEC$ ELSE
    END FUNCTION SCIPDefaultStartF
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPDefaultTimeFOMP( CallerID,time )
!DEC$ ELSE
    INTEGER FUNCTION SCIPDefaultTimeF( CallerID,time )
!DEC$ ENDIF
      USE timstruct_fd
      INTEGER,                                   INTENT( IN    ) :: CallerID     !USER ID tag
      TYPE( ptemporalT ),                        INTENT( INOUT ) :: time
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPDefaultTimeFOMP
!DEC$ ELSE
    END FUNCTION SCIPDefaultTimeF
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPDefaultWeatherFOMP( CallerID,weather )
!DEC$ ELSE
    INTEGER FUNCTION SCIPDefaultWeatherF( CallerID,weather )
!DEC$ ENDIF
      USE metstruct_fd
      INTEGER,                                   INTENT( IN    ) :: CallerID     !USER ID tag
      TYPE( pweatherT ),                         INTENT( INOUT ) :: weather
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPDefaultWeatherFOMP
!DEC$ ELSE
    END FUNCTION SCIPDefaultWeatherF
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPDeleteFieldOMP( CallerID,grdID )
!DEC$ ELSE
    INTEGER FUNCTION SCIPDeleteField( CallerID,grdID )
!DEC$ ENDIF
      USE tooluser_fd
      INTEGER,                                   INTENT( IN    ) :: CallerID     !USER ID tag
      INTEGER,                                   INTENT( INOUT ) :: grdID        !SAG grid ID
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPDeleteFieldOMP
!DEC$ ELSE
    END FUNCTION SCIPDeleteField
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPDeleteProjectOMP( CallerID,project,request )
!DEC$ ELSE
    INTEGER FUNCTION SCIPDeleteProject( CallerID,project,request )
!DEC$ ENDIF
      USE tooluser_fd
      INTEGER,                                     INTENT( IN  ) :: CallerID     !USER ID tag
      TYPE ( projectIDT ),                         INTENT( IN  ) :: project      !Project ID
      INTEGER,                                     INTENT( IN  ) :: request      !Delete instructions
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPDeleteProjectOMP
!DEC$ ELSE
    END FUNCTION SCIPDeleteProject
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPDrawFieldOMP( CallerID,grdID,Field,PlotType,contourHead, &
                                    contourList,GUIdraw,UserFill,UserDraw )
!DEC$ ELSE
    INTEGER FUNCTION SCIPDrawField( CallerID,grdID,Field,PlotType,contourHead, &
                                    contourList,GUIdraw,UserFill,UserDraw )
!DEC$ ENDIF
      USE tooluser_fd
      INTEGER,                                   INTENT( IN    ) :: CallerID     !USER ID tag
      INTEGER,                                   INTENT( IN    ) :: grdID        !SAG grid ID
      TYPE( SCIPPlotFieldT ),                    INTENT( IN    ) :: Field        !Field descriptor
      TYPE( SCIPPlotTypeT ),                     INTENT( IN    ) :: PlotType     !Plot definition
      TYPE( SCIPContourHeaderT ),                INTENT( IN    ) :: contourHead  !Contour array header
      TYPE( SCIPContourElementT ), DIMENSION(contourHead%number), TARGET, &
                                                 INTENT( INOUT ) :: contourList  !Contour array
      TYPE( ARAPDrawT ),                         INTENT( IN    ) :: GUIdraw      !Draw instructions
      INTEGER, EXTERNAL                                          :: UserFill     !Address of User supplied
                                                                                 !fill routine passed by value
      INTEGER, EXTERNAL                                          :: UserDraw     !Address of User supplied
                                                                                 !draw routine passed by value
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPDrawFieldOMP
!DEC$ ELSE
    END FUNCTION SCIPDrawField
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPDrawGridOMP( CallerID,grdID,UserDraw,mode )
!DEC$ ELSE
    INTEGER FUNCTION SCIPDrawGrid( CallerID,grdID,UserDraw,mode )
!DEC$ ENDIF
      INTEGER,                                     INTENT( IN  ) :: CallerID     !USER ID tag
      INTEGER,                                     INTENT( IN  ) :: grdID        !SAG grid ID
      INTEGER, EXTERNAL                                          :: UserDraw     !Address of User supplied
                                                                                 !draw routine passed by value
      INTEGER,                                     INTENT( IN  ) :: mode         !Draw instructions
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPDrawGridOMP
!DEC$ ELSE
    END FUNCTION SCIPDrawGrid
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPExitToolOMP()
!DEC$ ELSE
    INTEGER FUNCTION SCIPExitTool()
!DEC$ ENDIF
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPExitToolOMP
!DEC$ ELSE
    END FUNCTION SCIPExitTool
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPGetAPIVersionOMP()
!DEC$ ELSE
    INTEGER FUNCTION SCIPGetAPIVersion()
!DEC$ ENDIF
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPGetAPIVersionOMP
!DEC$ ELSE
    END FUNCTION SCIPGetAPIVersion
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPNumMetOutputOMP( userID,Project,num2Dfields,num3Dfields, &
                                          numGrids,numTimes )
!DEC$ ELSE
    INTEGER FUNCTION SCIPNumMetOutput( userID,Project,num2Dfields,num3Dfields, &
                                       numGrids,numTimes )
!DEC$ ENDIF
      USE prjstruct_fd
      INTEGER,            INTENT( IN  ) :: userID
      TYPE( projectIDT ), INTENT( IN  ) :: Project
      INTEGER,            INTENT( OUT ) :: num2Dfields, num3Dfields, numGrids, numTimes
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPNumMetOutputOMP
!DEC$ ELSE
    END FUNCTION SCIPNumMetOutput
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPGetMetOutputOMP( userID,project,n2D,n3D,name2D,units2D,name3D,units3D, &
                                          grid,time,field2Dlist,field3Dlist )
!DEC$ ELSE
    INTEGER FUNCTION SCIPGetMetOutput( userID,project,n2D,n3D,name2D,units2D,name3D,units3D, &
                                       grid,time,field2Dlist,field3Dlist )
!DEC$ ENDIF
      USE prjstruct_fd
      USE charT_fd
      USE plotlist_fd
      USE plotmet_fd
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
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPGetMetOutputOMP
!DEC$ ELSE
    END FUNCTION SCIPGetMetOutput
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPGetMetVertGridOMP( userID,Project,gridID,sigma )
!DEC$ ELSE
    INTEGER FUNCTION SCIPGetMetVertGrid( userID,Project,gridID,sigma )
!DEC$ ENDIF
      USE prjstruct_fd
      INTEGER,            INTENT( IN  ) :: userID
      TYPE( projectIDT ), INTENT( IN  ) :: Project
      INTEGER,            INTENT( IN  ) :: gridID
      REAL, DIMENSION(*), INTENT( OUT ) :: sigma
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPGetMetVertGridOMP
!DEC$ ELSE
    END FUNCTION SCIPGetMetVertGrid
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPGet2DMetDataOMP( userID,Project,gridID,timeID,numFields, &
                                          nameFields,nx,ny,data )
!DEC$ ELSE
    INTEGER FUNCTION SCIPGet2DMetData( userID,Project,gridID,timeID,numFields, &
                                        nameFields,nx,ny,data )
!DEC$ ENDIF
      USE prjstruct_fd
      USE charT_fd
      INTEGER,                               INTENT( IN  ) :: userID
      TYPE( projectIDT ),                    INTENT( IN  ) :: Project
      INTEGER,                               INTENT( IN  ) :: gridID
      INTEGER,                               INTENT( IN  ) :: TimeID
      INTEGER,                               INTENT( IN  ) :: numFields
      TYPE( char64T ), DIMENSION(numFields), INTENT( IN  ) :: nameFields
      INTEGER,                               INTENT( IN  ) :: nx, ny
      REAL,      DIMENSION(nx,ny,numFields), INTENT( OUT ) :: data
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPGet2DMetDataOMP
!DEC$ ELSE
    END FUNCTION SCIPGet2DMetData
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPGet3DMetDataOMP( userID,Project,gridID,timeID,numFields, &
                                          nameFields,centerFlag,nx,ny,nz,data )
!DEC$ ELSE
    INTEGER FUNCTION SCIPGet3DMetData( userID,Project,gridID,timeID,numFields, &
                                       nameFields,centerFlag,nx,ny,nz,data )
!DEC$ ENDIF
      USE prjstruct_fd
      USE charT_fd
      INTEGER,                               INTENT( IN  ) :: userID
      TYPE( projectIDT ),                    INTENT( IN  ) :: Project
      INTEGER,                               INTENT( IN  ) :: gridID
      INTEGER,                               INTENT( IN  ) :: TimeID
      INTEGER,                               INTENT( IN  ) :: numFields
      TYPE( char64T ), DIMENSION(numFields), INTENT( IN  ) :: nameFields
      INTEGER,         DIMENSION(numFields), INTENT( IN  ) :: centerFlag
      INTEGER,                               INTENT( IN  ) :: nx, ny, nz
      REAL,   DIMENSION(nx,ny,nz,numFields), INTENT( OUT ) :: data
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPGet3DMetDataOMP
!DEC$ ELSE
    END FUNCTION SCIPGet3DMetData
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPGetFieldAssocSizeOMP( CallerID,grdID,nPlots,nLines,nPoints )
!DEC$ ELSE
    INTEGER FUNCTION SCIPGetFieldAssocSize( CallerID,grdID,nPlots,nLines,nPoints )
!DEC$ ENDIF
      USE tooluser_fd
      INTEGER,                                     INTENT( IN  ) :: CallerID     !USER ID tag
      INTEGER,                                     INTENT( IN  ) :: grdID        !SAG grid ID
      INTEGER,                                     INTENT( OUT ) :: nPlots       !number of associated plots
      INTEGER,                                     INTENT( OUT ) :: nLines       !number of lines in associated plots
      INTEGER,                                     INTENT( OUT ) :: nPoints      !number of points in lines in associated plots
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPGetFieldAssocSizeOMP
!DEC$ ELSE
    END FUNCTION SCIPGetFieldAssocSize
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPGetFieldAssocDataOMP( CallerID,grdID,Line,Point,TitleStr,AxesStr,LineIDStr )
!DEC$ ELSE
    INTEGER FUNCTION SCIPGetFieldAssocData( CallerID,grdID,Line,Point,TitleStr,AxesStr,LineIDStr )
!DEC$ ENDIF
      USE tooluser_fd
      INTEGER,                                     INTENT( IN  ) :: CallerID     !USER ID tag
      INTEGER,                                     INTENT( IN  ) :: grdID        !SAG grid ID
      TYPE( SCIPLineT  ), DIMENSION(*),            INTENT( OUT ) :: Line         !Lines output array
      TYPE( SCIPPointT ), DIMENSION(*),            INTENT( OUT ) :: Point        !Points output array
      TYPE( char64T ),    DIMENSION(*),            INTENT( OUT ) :: TitleStr     !Title strings
      TYPE( char64T ),    DIMENSION(*),            INTENT( OUT ) :: AxesStr      !Axes strings
      TYPE( char64T ),    DIMENSION(*),            INTENT( OUT ) :: LineIDStr    !Line ID strings
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPGetFieldAssocDataOMP
!DEC$ ELSE
    END FUNCTION SCIPGetFieldAssocData
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPGetFieldOMP( CallerID,grdID,Field,PlotType,nodes,triangles )
!DEC$ ELSE
    INTEGER FUNCTION SCIPGetField( CallerID,grdID,Field,PlotType,nodes,triangles )
!DEC$ ENDIF
      USE tooluser_fd
      INTEGER,                                     INTENT( IN  ) :: CallerID     !USER ID tag
      INTEGER,                                     INTENT( IN  ) :: grdID        !SAG grid ID
      TYPE( SCIPPlotFieldT ),                      INTENT( IN  ) :: Field        !Field descriptor
      TYPE( SCIPPlotTypeT ),                       INTENT( IN  ) :: PlotType     !Plot definition
      TYPE( SCIPPlotFieldNodeT ),     DIMENSION(*),INTENT( OUT ) :: nodes        !Plot definition
      TYPE( SCIPPlotFieldTriangleT ), DIMENSION(*),INTENT( OUT ) :: triangles    !Plot definition
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPGetFieldOMP
!DEC$ ELSE
    END FUNCTION SCIPGetField
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPGetFieldDomainOMP( CallerID,grdID,nx0,ny0,x0,y0,dx0,dy0 )
!DEC$ ELSE
    INTEGER FUNCTION SCIPGetFieldDomain( CallerID,grdID,nx0,ny0,x0,y0,dx0,dy0 )
!DEC$ ENDIF
      INTEGER,                                     INTENT( IN  ) :: CallerID     !USER ID tag
      INTEGER,                                     INTENT( IN  ) :: grdID        !SAG grid ID
      INTEGER,                                     INTENT( OUT ) :: nx0          !Base grid number X direction
      INTEGER,                                     INTENT( OUT ) :: ny0          !Base grid number Y direction
      REAL,                                        INTENT( OUT ) :: x0           !Origin X direction
      REAL,                                        INTENT( OUT ) :: y0           !Origin Y direction
      REAL,                                        INTENT( OUT ) :: dx0          !Base grid size X direction
      REAL,                                        INTENT( OUT ) :: dy0          !Base grid size Y direction
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPGetFieldDomainOMP
!DEC$ ELSE
    END FUNCTION SCIPGetFieldDomain
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPGetFieldMinMaxOMP( CallerID,grdID,PlotType,mMin,mMax,vMin,vMax,fMin,fMax )
!DEC$ ELSE
    INTEGER FUNCTION SCIPGetFieldMinMax( CallerID,grdID,PlotType,mMin,mMax,vMin,vMax,fMin,fMax )
!DEC$ ENDIF
      USE tooluser_fd
      INTEGER,                                     INTENT( IN  ) :: CallerID     !USER ID tag
      INTEGER,                                     INTENT( IN  ) :: grdID        !SAG grid ID
      TYPE( SCIPPlotTypeT ),                       INTENT( IN  ) :: PlotType     !Plot definition
      REAL,                                        INTENT( OUT ) :: mMin         !Minimum value - mean field
      REAL,                                        INTENT( OUT ) :: mMax         !Maximum value - mean field
      REAL,                                        INTENT( OUT ) :: vMin         !Minimum value - variance field
      REAL,                                        INTENT( OUT ) :: vMax         !Maximum value - variance field
      REAL,                                        INTENT( OUT ) :: fMin         !Minimum value - plot field
      REAL,                                        INTENT( OUT ) :: fMax         !Maximum value - plot field
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPGetFieldMinMaxOMP
!DEC$ ELSE
    END FUNCTION SCIPGetFieldMinMax
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPGetFieldSizeOMP( CallerID,grdID,Field,PlotType,nNode,nTriangle )
!DEC$ ELSE
    INTEGER FUNCTION SCIPGetFieldSize( CallerID,grdID,Field,PlotType,nNode,nTriangle )
!DEC$ ENDIF
      USE tooluser_fd
      INTEGER,                                     INTENT( IN  ) :: CallerID     !USER ID tag
      INTEGER,                                     INTENT( IN  ) :: grdID        !SAG grid ID
      TYPE( SCIPPlotFieldT ),                      INTENT( IN  ) :: Field        !Field descriptor
      TYPE( SCIPPlotTypeT ),                       INTENT( IN  ) :: PlotType     !Plot definition
      INTEGER,                                     INTENT( OUT ) :: nNode        !Number of nodes in plot field
      INTEGER,                                     INTENT( OUT ) :: nTriangle    !Number of triangels in plot field
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPGetFieldSizeOMP
!DEC$ ELSE
    END FUNCTION SCIPGetFieldSize
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPGetFieldTableOMP( CallerID,Field,ClassData,TableTitle,ColTitle,RowTitle,Table )
!DEC$ ELSE
    INTEGER FUNCTION SCIPGetFieldTable( CallerID,Field,ClassData,TableTitle,ColTitle,RowTitle,Table )
!DEC$ ENDIF
      USE tooluser_fd
      INTEGER,                                   INTENT( IN    ) :: CallerID     !USER ID tag
      TYPE( SCIPPlotFieldT ),                    INTENT( INOUT ) :: Field        !Field descriptor
      REAL,            DIMENSION(*),             INTENT( IN    ) :: ClassData    !ClassDatatArray (Risk Level - Dual runs only
      TYPE( char32T ), DIMENSION(*),             INTENT( OUT   ) :: TableTitle   !Table titles
      TYPE( char32T ), DIMENSION(*),             INTENT( OUT   ) :: ColTitle     !Column headings
      TYPE( char32T ), DIMENSION(*),             INTENT( OUT   ) :: RowTitle     !Row headings
      INTEGER,         DIMENSION(*),             INTENT( OUT   ) :: Table        !Table data
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPGetFieldTableOMP
!DEC$ ELSE
    END FUNCTION SCIPGetFieldTable
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPGetFieldTableSizeOMP( CallerID,Field,ClassData,nTable,nCol,nRow )
!DEC$ ELSE
    INTEGER FUNCTION SCIPGetFieldTableSize( CallerID,Field,ClassData,nTable,nCol,nRow )
!DEC$ ENDIF
      USE tooluser_fd
      INTEGER,                                     INTENT( IN  ) :: CallerID     !USER ID tag
      TYPE( SCIPPlotFieldT ),                      INTENT( IN  ) :: Field        !Field descriptor
      REAL,     DIMENSION(*),                      INTENT( IN  ) :: ClassData    !ClassDatatArray (Risk Level - Dual runs only
      INTEGER,                                     INTENT( OUT ) :: nTable       !Number of tables
      INTEGER,                                     INTENT( OUT ) :: nCol         !Number of columns per table
      INTEGER,                                     INTENT( OUT ) :: nRow         !Number of rows per table
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPGetFieldTableSizeOMP
!DEC$ ELSE
    END FUNCTION SCIPGetFieldTableSize
!DEC$ ENDIF
!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPGetFieldValueOMP( CallerID,grdID,Field,PlotType,xPnt,yPnt,fPnt )
!DEC$ ELSE
    INTEGER FUNCTION SCIPGetFieldValue( CallerID,grdID,Field,PlotType,xPnt,yPnt,fPnt )
!DEC$ ENDIF
      USE tooluser_fd
      INTEGER,                                     INTENT( IN  ) :: CallerID     !USER ID tag
      INTEGER,                                     INTENT( IN  ) :: grdID        !SAG grid ID
      TYPE( SCIPPlotFieldT ),                      INTENT( IN  ) :: Field        !Field definition
      TYPE( SCIPPlotTypeT ),                       INTENT( IN  ) :: PlotType     !Plot definition
      REAL,                                        INTENT( IN  ) :: xPnt         !X location
      REAL,                                        INTENT( IN  ) :: yPnt         !Y location
      REAL,                                        INTENT( OUT ) :: fPnt         !Field value
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPGetFieldValueOMP
!DEC$ ELSE
    END FUNCTION SCIPGetFieldValue
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPGetFieldValuesOMP( CallerID,grdID,Field,PlotType,nPnt,xPnt,yPnt,fPnt )
!DEC$ ELSE
    INTEGER FUNCTION SCIPGetFieldValues( CallerID,grdID,Field,PlotType,nPnt,xPnt,yPnt,fPnt )
!DEC$ ENDIF
      USE tooluser_fd
      INTEGER,                                     INTENT( IN  ) :: CallerID     !USER ID tag
      INTEGER,                                     INTENT( IN  ) :: grdID        !SAG grid ID
      TYPE( SCIPPlotFieldT ),                      INTENT( IN  ) :: Field        !Field definition
      TYPE( SCIPPlotTypeT ),                       INTENT( IN  ) :: PlotType     !Plot definition
      INTEGER,                                     INTENT( IN  ) :: nPnt         !Number of locations
      REAL, DIMENSION(nPnt),                       INTENT( IN  ) :: xPnt         !X location
      REAL, DIMENSION(nPnt),                       INTENT( IN  ) :: yPnt         !Y location
      REAL, DIMENSION(nPnt),                       INTENT( OUT ) :: fPnt         !Field value
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPGetFieldValuesOMP
!DEC$ ELSE
    END FUNCTION SCIPGetFieldValues
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPGetLastErrorOMP( error )
!DEC$ ELSE
    INTEGER FUNCTION SCIPGetLastError( error )
!DEC$ ENDIF
      USE tooluser_fd
      TYPE( messageT ),                            INTENT( OUT ) :: error       !error message
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPGetLastErrorOMP
!DEC$ ELSE
    END FUNCTION SCIPGetLastError
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPGetPathMaxLengthOMP()
!DEC$ ELSE
    INTEGER FUNCTION SCIPGetPathMaxLength()
!DEC$ ENDIF
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPGetPathMaxLengthOMP
!DEC$ ELSE
    END FUNCTION SCIPGetPathMaxLength
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPGetPlotClassesOMP( CallerID,Project,ClassStr,ChoiceStr,KindStr, &
                                         CatClassArray,ClassChoiceArray,ProjectCoordinate )
!DEC$ ELSE
    INTEGER FUNCTION SCIPGetPlotClasses( CallerID,Project,ClassStr,ChoiceStr,KindStr, &
                                         CatClassArray,ClassChoiceArray,ProjectCoordinate )
!DEC$ ENDIF
      USE tooluser_fd
      INTEGER,                                            INTENT( IN  ) :: CallerID          !USER ID tag
      TYPE( projectIDT ),                                 INTENT( IN  ) :: Project           !Project ID
      TYPE( char64T ),             DIMENSION(*),          INTENT( OUT ) :: ClassStr          !Class strings
      TYPE( char64T ),             DIMENSION(*),          INTENT( OUT ) :: ChoiceStr         !Choice strings
      TYPE( char64T ),             DIMENSION(*),          INTENT( OUT ) :: KindStr           !Kind strings
      TYPE( SCIPCategoryClassT ),  DIMENSION(HP_NUMCAT,*),INTENT( OUT ) :: CatClassArray     !Class/Category use array
      TYPE( SCIPClassChoiceT ),    DIMENSION(*),          INTENT( OUT ) :: ClassChoiceArray  !Class/Choice use array
      TYPE( SCIPFieldCoordinateT ),                       INTENT( OUT ) :: ProjectCoordinate !Project coordinate descriptor
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPGetPlotClassesOMP
!DEC$ ELSE
    END FUNCTION SCIPGetPlotClasses
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPGetPlotTimesOMP( CallerID,Project,TimePuff,TimeSrf,TimeMet )
!DEC$ ELSE
    INTEGER FUNCTION SCIPGetPlotTimes( CallerID,Project,TimePuff,TimeSrf,TimeMet )
!DEC$ ENDIF
      USE tooluser_fd
      INTEGER,                                     INTENT( IN  ) :: CallerID     !USER ID tag
      TYPE( projectIDT ),                          INTENT( IN  ) :: Project      !Project ID
      TYPE(SCIPTimeT),   DIMENSION(*),             INTENT( OUT ) :: TimePuff     !Puff times
      TYPE(SCIPTimeT),   DIMENSION(*),             INTENT( OUT ) :: TimeSrf      !Surface grid times
      TYPE(SCIPTimeT),   DIMENSION(*),             INTENT( OUT ) :: TimeMet      !Met times
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPGetPlotTimesOMP
!DEC$ ELSE
    END FUNCTION SCIPGetPlotTimes
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPGetProjectAuditOMP( CallerID,audit )
!DEC$ ELSE
    INTEGER FUNCTION SCIPGetProjectAudit( CallerID,audit )
!DEC$ ENDIF
      USE tooluser_fd
      INTEGER,                                INTENT( IN    ) :: CallerID    !USER ID Tag
      TYPE( pauditT ),                        INTENT( INOUT ) :: audit     !Project ID
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPGetProjectAuditOMP
!DEC$ ELSE
    END FUNCTION SCIPGetProjectAudit
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPGetProjectTerrainOMP( CallerID,terrain,ths,tddx,tddy )
!DEC$ ELSE
    INTEGER FUNCTION SCIPGetProjectTerrain( CallerID,terrain,ths,tddx,tddy )
!DEC$ ENDIF
      USE tooluser_fd
      INTEGER,                                   INTENT( IN    ) :: CallerID    !USER ID Tag
      TYPE( pterrainHeadT ),                     INTENT( INOUT ) :: terrain     !Project ID
      REAL,                 DIMENSION(*),        INTENT( OUT   ) :: ths         !Terrain array
      REAL,                 DIMENSION(*),        INTENT( OUT   ) :: tddx        !Slope array
      REAL,                 DIMENSION(*),        INTENT( OUT   ) :: tddy        !Slope array
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPGetProjectTerrainOMP
!DEC$ ELSE
    END FUNCTION SCIPGetProjectTerrain
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPGetProjectTerrainHeaderOMP( UserID,terrain )
!DEC$ ELSE
    INTEGER FUNCTION SCIPGetProjectTerrainHeader( UserID,terrain )
!DEC$ ENDIF
      USE spcstruct_fd
      INTEGER,               INTENT( IN    ) :: UserID !USER ID Tag
      TYPE( pterrainHeadT ), INTENT( INOUT ) :: terrain !Project ID
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPGetProjectTerrainHeaderOMP
!DEC$ ELSE
    END FUNCTION SCIPGetProjectTerrainHeader
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPGetProjectTerrainInOMP( CallerID,ifld,terrain,ths,tddx,tddy )
!DEC$ ELSE
    INTEGER FUNCTION SCIPGetProjectTerrainIn( CallerID,ifld,terrain,ths,tddx,tddy )
!DEC$ ENDIF
      USE tooluser_fd
      INTEGER,                            INTENT( IN    ) :: CallerID !USER ID Tag
      INTEGER,                            INTENT( IN    ) :: ifld     !Met field ID
      TYPE( pterrainHeadT ),              INTENT( INOUT ) :: terrain  !Project ID
      REAL,                 DIMENSION(*), INTENT( OUT   ) :: ths      !Terrain array
      REAL,                 DIMENSION(*), INTENT( OUT   ) :: tddx     !Slope array
      REAL,                 DIMENSION(*), INTENT( OUT   ) :: tddy     !Slope array
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPGetProjectTerrainInOMP
!DEC$ ELSE
    END FUNCTION SCIPGetProjectTerrainIn
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPGetProjectTerrainHeaderInOMP( UserID,ifld,terrain )
!DEC$ ELSE
    INTEGER FUNCTION SCIPGetProjectTerrainHeaderIn( UserID,ifld,terrain )
!DEC$ ENDIF
      USE spcstruct_fd
      INTEGER,               INTENT( IN    ) :: UserID  !USER ID Tag
      INTEGER,               INTENT( IN    ) :: ifld    !Met field ID
      TYPE( pterrainHeadT ), INTENT( INOUT ) :: terrain !Project ID
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPGetProjectTerrainHeaderInOMP
!DEC$ ELSE
    END FUNCTION SCIPGetProjectTerrainHeaderIn
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPGetProjectPuffOMP( CallerID,puff,timeID,flag,puffList,auxData,typeList,mcList )
!DEC$ ELSE
    INTEGER FUNCTION SCIPGetProjectPuff( CallerID,puff,timeID,flag,puffList,auxData,typeList,mcList )
!DEC$ ENDIF
      USE tooluser_fd
      USE mcstruct_fd
      INTEGER,                                   INTENT( IN    ) :: CallerID    !USER ID Tag
      TYPE( ppuffHeadT ),                        INTENT( INOUT ) :: puff        !Project ID
      INTEGER,                                   INTENT( IN    ) :: timeID      !time ID
      INTEGER,                                   INTENT( IN    ) :: flag        !Bit to transform to LLA, MSL, terrain slope
      TYPE( puffT ),        DIMENSION(*),        INTENT( OUT   ) :: puffList    !puff data
      REAL,                 DIMENSION(*),        INTENT( OUT   ) :: auxData     !puff auxiliary data
      TYPE( puffTypeT ),    DIMENSION(*),        INTENT( OUT   ) :: typeList    !Puffs types
      TYPE( material_MClist ),                   INTENT( OUT   ) :: mcList      !multicomponent types
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPGetProjectPuffOMP
!DEC$ ELSE
    END FUNCTION SCIPGetProjectPuff
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPGetProjectPuffHeaderOMP( CallerID,puff,timeID )
!DEC$ ELSE
    INTEGER FUNCTION SCIPGetProjectPuffHeader( CallerID,puff,timeID )
!DEC$ ENDIF
      USE tooluser_fd
      INTEGER,                                   INTENT( IN    ) :: CallerID    !USER ID Tag
      TYPE( ppuffHeadT ),                        INTENT( INOUT ) :: puff        !Project ID
      INTEGER,                                   INTENT( IN    ) :: timeID      !time ID
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPGetProjectPuffHeaderOMP
!DEC$ ELSE
    END FUNCTION SCIPGetProjectPuffHeader
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPGetProjectPuffChemOMP( UserID,project,chemOut,doData )
!DEC$ ELSE
    INTEGER FUNCTION SCIPGetProjectPuffChem( UserID,project,chemOut,doData )
!DEC$ ENDIF
      USE prjstruct_fd
      USE multcomp_fd
      INTEGER,                             INTENT( IN    ) :: UserID     !USER ID Tag
      TYPE( projectIDT ),                  INTENT( IN    ) :: project    !Project ID
      TYPE( ChemMC_out ), DIMENSION(*),    INTENT( INOUT ) :: chemOut
      INTEGER,                             INTENT( IN    ) :: doData     !SCIPtrue => get arrays else get sizes
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPGetProjectPuffChemOMP
!DEC$ ELSE
    END FUNCTION SCIPGetProjectPuffChem
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPGetProjectVersionOMP( CallerID,project )
!DEC$ ELSE
    INTEGER FUNCTION SCIPGetProjectVersion( CallerID,project )
!DEC$ ENDIF
      USE tooluser_fd
      INTEGER,                                INTENT( IN    ) :: CallerID    !USER ID Tag
      TYPE( projectIDT ),                     INTENT( INOUT ) :: project     !Project ID
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPGetProjectVersionOMP
!DEC$ ELSE
    END FUNCTION SCIPGetProjectVersion
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPGetSCIPUFFVersionOMP()
!DEC$ ELSE
    INTEGER FUNCTION SCIPGetSCIPUFFVersion()
!DEC$ ENDIF
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPGetSCIPUFFVersionOMP
!DEC$ ELSE
    END FUNCTION SCIPGetSCIPUFFVersion
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPGetSubstratesOMP( mode, substrate )
!DEC$ ELSE
    INTEGER FUNCTION SCIPGetSubstrates( mode, substrate )
!DEC$ ENDIF
      USE tooluser_fd
      INTEGER,                       INTENT( IN  ) :: mode
      TYPE( char16T ), DIMENSION(*), INTENT( OUT ) :: substrate
    END FUNCTION

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPGetTerrainSliceOMP( UserID,Project,nPts,Location,Ht )
!DEC$ ELSE
    INTEGER FUNCTION SCIPGetTerrainSlice( UserID,Project,nPts,Location,Ht )
!DEC$ ENDIF
      USE prjstruct_fd
      USE field_fd
      INTEGER,                             INTENT( IN  ) :: UserID  !USER ID Tag
      TYPE( ProjectIDT ),                  INTENT( IN  ) :: Project !Project ID
      INTEGER,                             INTENT( IN  ) :: nPts    !No. of location points
      TYPE( SCIPpointT ), DIMENSION(nPts), INTENT( IN  ) :: Location    !location array
      REAL,DIMENSION(nPts),                INTENT( OUT ) :: Ht    !terrain height array
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPGetTerrainSliceOMP
!DEC$ ELSE
    END FUNCTION SCIPGetTerrainSlice
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPGetVersionOMP()
!DEC$ ELSE
    INTEGER FUNCTION SCIPGetVersion()
!DEC$ ENDIF
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPGetVersionOMP
!DEC$ ELSE
    END FUNCTION SCIPGetVersion
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPGetVersionStringOMP( iflag,string )
!DEC$ ELSE
    INTEGER FUNCTION SCIPGetVersionString( iflag,string )
!DEC$ ENDIF
      USE tooluser_fd
      INTEGER,                                    INTENT( IN  ) :: iflag         !request flag
      TYPE( char128T ),                           INTENT( OUT ) :: string        !version string
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPGetVersionStringOMP
!DEC$ ELSE
    END FUNCTION SCIPGetVersionString
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    SUBROUTINE SCIPInitErrorOMP()
    END SUBROUTINE SCIPInitErrorOMP
!DEC$ ELSE
    SUBROUTINE SCIPInitError()
    END SUBROUTINE SCIPInitError
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPInitToolOMP( CallerID,callback,request,limit,cINIfile )
!DEC$ ELSE
    INTEGER FUNCTION SCIPInitTool( CallerID,callback,request,limit,cINIfile )
!DEC$ ENDIF
      USE tooluser_fd
      USE basic_fd
      INTEGER,                                     INTENT( IN    ) :: CallerID     !USER ID tag
      INTEGER(LEN_ADDRESS),                        INTENT( IN    ) :: callback     !Address of user callback function
      INTEGER,                                     INTENT( INOUT ) :: request      !Initialization request
      TYPE( limitT ),                              INTENT( IN    ) :: limit        !Array size limits
      TYPE( fileNameT ),                           INTENT( IN    ) :: cINIfile     !INI file
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPInitToolOMP
!DEC$ ELSE
    END FUNCTION SCIPInitTool
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPLoadInputOMP( CallerID,iInput,tInput0,tInput1 )
!DEC$ ELSE
    INTEGER FUNCTION SCIPLoadInput( CallerID,iInput,tInput0,tInput1 )
!DEC$ ENDIF
      INTEGER,                                   INTENT( IN    ) :: CallerID     !USER ID tag
      INTEGER,                                   INTENT( IN    ) :: iInput       !Request type
      INTEGER, DIMENSION(*),                     INTENT( INOUT ) :: tInput0      !Results data
      INTEGER, DIMENSION(*),                     INTENT( INOUT ) :: tInput1      !Results data
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPLoadInputOMP
!DEC$ ELSE
    END FUNCTION SCIPLoadInput
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPLoadCtrlFOMP( CallerID,ctrl )
!DEC$ ELSE
    INTEGER FUNCTION SCIPLoadCtrlF( CallerID,ctrl )
!DEC$ ENDIF
      USE timstruct_fd
      INTEGER,                                   INTENT( IN    ) :: CallerID     !USER ID tag
      TYPE( pctrlT ),                            INTENT( INOUT ) :: ctrl
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPLoadCtrlFOMP
!DEC$ ELSE
    END FUNCTION SCIPLoadCtrlF
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPLoadDomainFOMP( CallerID,domain )
!DEC$ ELSE
    INTEGER FUNCTION SCIPLoadDomainF( CallerID,domain )
!DEC$ ENDIF
      USE domstruct_fd
      INTEGER,                                   INTENT( IN    ) :: CallerID     !USER ID tag
      TYPE( pspatialT ),                         INTENT( INOUT ) :: domain
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPLoadDomainFOMP
!DEC$ ELSE
    END FUNCTION SCIPLoadDomainF
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPLoadEndFOMP( CallerID,end )
!DEC$ ELSE
    INTEGER FUNCTION SCIPLoadEndF( CallerID,end )
!DEC$ ENDIF
      USE timstruct_fd
      INTEGER,                                   INTENT( IN    ) :: CallerID     !USER ID tag
      TYPE( pendT ),                             INTENT( INOUT ) :: end
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPLoadEndFOMP
!DEC$ ELSE
    END FUNCTION SCIPLoadEndF
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPLoadEndXFOMP( CallerID,end,extFlag )
!DEC$ ELSE
    INTEGER FUNCTION SCIPLoadEndXF( CallerID,end,extFlag )
!DEC$ ENDIF
      USE timstruct_fd
      INTEGER,                                   INTENT( IN    ) :: CallerID     !USER ID tag
      TYPE( pendT ),                             INTENT( INOUT ) :: end
      INTEGER,                                   INTENT( IN    ) :: extFlag      !extension flag FALSE=.INP TRUE=.RST
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPLoadEndXFOMP
!DEC$ ELSE
    END FUNCTION SCIPLoadEndXF
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPLoadFlagsFOMP( CallerID,flags )
!DEC$ ELSE
    INTEGER FUNCTION SCIPLoadFlagsF( CallerID,flags )
!DEC$ ENDIF
      USE inpstruct_fd
      INTEGER,                                   INTENT( IN    ) :: CallerID     !USER ID tag
      TYPE( pflagsT ),                           INTENT( INOUT ) :: flags
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPLoadFlagsFOMP
!DEC$ ELSE
    END FUNCTION SCIPLoadFlagsF
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPLoadInpFOMP( CallerID,input,mtlList )
!DEC$ ELSE
    INTEGER FUNCTION SCIPLoadInpF( CallerID,input,mtlList )
!DEC$ ENDIF
      USE structure_fd
      USE mtlstruct_fd
      INTEGER,                                   INTENT( IN    ) :: CallerID     !USER ID tag
      TYPE( pinputT ),                           INTENT( INOUT ) :: input
      TYPE( materialT  ), DIMENSION(*),          INTENT( INOUT ) :: mtlList
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPLoadInpFOMP
!DEC$ ELSE
    END FUNCTION SCIPLoadInpF
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPLoadRstFOMP( CallerID,input )
!DEC$ ELSE
    INTEGER FUNCTION SCIPLoadRstF( CallerID,input )
!DEC$ ENDIF
      USE structure_fd
      INTEGER,                                   INTENT( IN    ) :: CallerID     !USER ID tag
      TYPE( pinputT ),                           INTENT( INOUT ) :: input
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPLoadRstFOMP
!DEC$ ELSE
    END FUNCTION SCIPLoadRstF
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPLoadMaterialFOMP( CallerID,material,mtlList )
!DEC$ ELSE
    INTEGER FUNCTION SCIPLoadMaterialF( CallerID,material,mtlList )
!DEC$ ENDIF
      USE mtlstruct_fd
      INTEGER,                                   INTENT( IN    ) :: CallerID     !USER ID tag
      TYPE( pmaterialT ),                        INTENT( INOUT ) :: material
      TYPE( materialT  ), DIMENSION(*),          INTENT( INOUT ) :: mtlList
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPLoadMaterialFOMP
!DEC$ ELSE
    END FUNCTION SCIPLoadMaterialF
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPLoadOptionsFOMP( CallerID,options )
!DEC$ ELSE
    INTEGER FUNCTION SCIPLoadOptionsF( CallerID,options )
!DEC$ ENDIF
      USE inpstruct_fd
      INTEGER,                                   INTENT( IN    ) :: CallerID     !USER ID tag
      TYPE( poptionsT ),                         INTENT( INOUT ) :: options
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPLoadOptionsFOMP
!DEC$ ELSE
    END FUNCTION SCIPLoadOptionsF
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPLoadReleaseFOMP( CallerID,release,relList )
!DEC$ ELSE
    INTEGER FUNCTION SCIPLoadReleaseF( CallerID,release,relList )
!DEC$ ENDIF
      USE relstruct_fd
      INTEGER,                                   INTENT( IN    ) :: CallerID     !USER ID tag
      TYPE( preleaseT ),                         INTENT( INOUT ) :: release
      TYPE( releaseT  ), DIMENSION(*),           INTENT( INOUT ) :: relList
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPLoadReleaseFOMP
!DEC$ ELSE
    END FUNCTION SCIPLoadReleaseF
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPLoadReleaseMCFOMP( CallerID,release,relList,relMCList )
!DEC$ ELSE
    INTEGER FUNCTION SCIPLoadReleaseMCF( CallerID,release,relList,relMCList )
!DEC$ ENDIF
      USE relstruct_fd
      INTEGER,                                   INTENT( IN    ) :: CallerID     !USER ID tag
      TYPE( preleaseT ),                         INTENT( INOUT ) :: release
      TYPE( releaseT  ), DIMENSION(*),           INTENT( INOUT ) :: relList
      TYPE( releaseMCT), DIMENSION(*),           INTENT( INOUT ) :: relMCList
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPLoadReleaseMCFOMP
!DEC$ ELSE
    END FUNCTION SCIPLoadReleaseMCF
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPLoadRunFOMP( CallerID,end )
!DEC$ ELSE
    INTEGER FUNCTION SCIPLoadRunF( CallerID,end )
!DEC$ ENDIF
      USE timstruct_fd
      INTEGER,                                   INTENT( IN    ) :: CallerID     !USER ID tag
      TYPE( pendT ),                             INTENT( INOUT ) :: end
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPLoadRunFOMP
!DEC$ ELSE
    END FUNCTION SCIPLoadRunF
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPLoadStartFOMP( CallerID,start )
!DEC$ ELSE
    INTEGER FUNCTION SCIPLoadStartF( CallerID,start )
!DEC$ ENDIF
      USE timstruct_fd
      INTEGER,                                   INTENT( IN    ) :: CallerID     !USER ID tag
      TYPE( pstartT ),                           INTENT( INOUT ) :: start
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPLoadStartFOMP
!DEC$ ELSE
    END FUNCTION SCIPLoadStartF
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPLoadTimeFOMP( CallerID,time )
!DEC$ ELSE
    INTEGER FUNCTION SCIPLoadTimeF( CallerID,time )
!DEC$ ENDIF
      USE timstruct_fd
      INTEGER,                                   INTENT( IN    ) :: CallerID     !USER ID tag
      TYPE( ptemporalT ),                        INTENT( INOUT ) :: time
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPLoadTimeFOMP
!DEC$ ELSE
    END FUNCTION SCIPLoadTimeF
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPLoadWeatherFOMP( CallerID,weather )
!DEC$ ELSE
    INTEGER FUNCTION SCIPLoadWeatherF( CallerID,weather )
!DEC$ ENDIF
      USE metstruct_fd
      INTEGER,                                   INTENT( IN    ) :: CallerID     !USER ID tag
      TYPE( pweatherT ),                         INTENT( INOUT ) :: weather
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPLoadWeatherFOMP
!DEC$ ELSE
    END FUNCTION SCIPLoadWeatherF
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPLoadProjectOMP( CallerID,project,mtlList,relList )
!DEC$ ELSE
    INTEGER FUNCTION SCIPLoadProject( CallerID,project,mtlList,relList )
!DEC$ ENDIF
      USE tooluser_fd
      INTEGER,                                   INTENT( IN    ) :: CallerID    !USER ID Tag
      TYPE( projectT ),                          INTENT( INOUT ) :: project     !Project ID
      TYPE( materialT ),DIMENSION(*),            INTENT( OUT   ) :: mtlList     !Material list
      TYPE( releaseT ), DIMENSION(*),            INTENT( OUT   ) :: relList     !Release list
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPLoadProjectOMP
!DEC$ ELSE
    END FUNCTION SCIPLoadProject
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPLoadProjectMCOMP( CallerID,project,mtlList,relList,relMCList )
!DEC$ ELSE
    INTEGER FUNCTION SCIPLoadProjectMC( CallerID,project,mtlList,relList,relMCList )
!DEC$ ENDIF
      USE tooluser_fd
      INTEGER,                                   INTENT( IN    ) :: CallerID    !USER ID Tag
      TYPE( projectT ),                          INTENT( INOUT ) :: project     !Project ID
      TYPE( materialT ),DIMENSION(*),            INTENT( OUT   ) :: mtlList     !Material list
      TYPE( releaseT ), DIMENSION(*),            INTENT( OUT   ) :: relList     !Release list
      TYPE( releaseMCT ), DIMENSION(*),          INTENT( OUT   ) :: relMCList   !Release multicomponent list
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPLoadProjectMCOMP
!DEC$ ELSE
    END FUNCTION SCIPLoadProjectMC
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPNewProjectOMP( CallerID,project,mtlList,relList )
!DEC$ ELSE
    INTEGER FUNCTION SCIPNewProject( CallerID,project,mtlList,relList )
!DEC$ ENDIF
      USE tooluser_fd
      INTEGER,                                     INTENT( IN )    :: CallerID    !USER ID Tag
      TYPE( createNewT ),                          INTENT( INOUT ) :: project     !Project ID
      TYPE( materialT ),DIMENSION(*),              INTENT( INOUT ) :: mtlList     !Material list
      TYPE( releaseT ), DIMENSION(*),              INTENT( INOUT ) :: relList     !Release list
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPNewProjectOMP
!DEC$ ELSE
    END FUNCTION SCIPNewProject
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPNewProjectMCOMP( CallerID,project,mtlList,relList,nMC,relMCList )
!DEC$ ELSE
    INTEGER FUNCTION SCIPNewProjectMC( CallerID,project,mtlList,relList,nMC,relMCList )
!DEC$ ENDIF
      USE tooluser_fd
      INTEGER,                                     INTENT( IN    ) :: CallerID    !USER ID Tag
      TYPE( createNewT ),                          INTENT( INOUT ) :: project     !Project ID
      TYPE( materialT ),DIMENSION(*),              INTENT( INOUT ) :: mtlList     !Material list
      TYPE( releaseT ), DIMENSION(*),              INTENT( INOUT ) :: relList     !Release list
      INTEGER,                                     INTENT( IN    ) :: nMC         !Size of relMCList
      TYPE( releaseMCT ), DIMENSION(*),            INTENT( IN    ) :: relMCList   !Release multicomponent list
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPNewProjectMCOMP
!DEC$ ELSE
    END FUNCTION SCIPNewProjectMC
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION ProcessNewProjectOMP( CallerID,project,mtlList,relList )
!DEC$ ELSE
    INTEGER FUNCTION ProcessNewProject( CallerID,project,mtlList,relList )
!DEC$ ENDIF
      USE tooluser_fd
      INTEGER,                                     INTENT( IN    ) :: CallerID    !USER ID Tag
      TYPE( createNewT ),                          INTENT( INOUT ) :: project     !Project ID
      TYPE( materialT ),DIMENSION(*),              INTENT( INOUT ) :: mtlList     !Material list
      TYPE( releaseT ), DIMENSION(*),              INTENT( INOUT ) :: relList     !Release list
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION ProcessNewProjectOMP
!DEC$ ELSE
    END FUNCTION ProcessNewProject
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPNumPlotClassesOMP( CallerID,Project,nClass,nChoice,nKind )
!DEC$ ELSE
    INTEGER FUNCTION SCIPNumPlotClasses( CallerID,Project,nClass,nChoice,nKind )
!DEC$ ENDIF
      USE tooluser_fd
      INTEGER,                                     INTENT( IN  ) :: CallerID     !USER ID tag
      TYPE( projectIDT ),                          INTENT( IN  ) :: Project      !Project ID
      INTEGER,                                     INTENT( OUT ) :: nClass       !Number of Class strings
      INTEGER,                                     INTENT( OUT ) :: nChoice      !Number of Choice strings
      INTEGER,                                     INTENT( OUT ) :: nKind        !Number of Kind strings
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPNumPlotClassesOMP
!DEC$ ELSE
    END FUNCTION SCIPNumPlotClasses
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPNumPlotTimesOMP( CallerID,Project,nTimePuff,nTimeSrf,nTimeMet,nNotUsed )
!DEC$ ELSE
    INTEGER FUNCTION SCIPNumPlotTimes( CallerID,Project,nTimePuff,nTimeSrf,nTimeMet,nNotUsed )
!DEC$ ENDIF
      USE tooluser_fd
      INTEGER,                                     INTENT( IN  ) :: CallerID     !USER ID tag
      TYPE( projectIDT ),                          INTENT( IN  ) :: Project      !Project ID
      INTEGER,                                     INTENT( OUT ) :: nTimePuff    !Number of Puff times
      INTEGER,                                     INTENT( OUT ) :: nTimeSrf     !Number of Surface grid times
      INTEGER,                                     INTENT( OUT ) :: nTimeMet     !Number of Met times
      INTEGER,                                     INTENT( OUT ) :: nNotUsed     !Not used in this version
    END FUNCTION

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPNumSubstratesOMP( mode )
!DEC$ ELSE
    INTEGER FUNCTION SCIPNumSubstrates( mode )
!DEC$ ENDIF
      INTEGER, INTENT( IN  ) :: mode
    END FUNCTION

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPPopAreaFieldOMP( CallerID,grdID,Field,PlotType,contourHead,contourList )
!DEC$ ELSE
    INTEGER FUNCTION SCIPPopAreaField( CallerID,grdID,Field,PlotType,contourHead,contourList )
!DEC$ ENDIF
      USE tooluser_fd
      INTEGER,                                   INTENT( IN    ) :: CallerID     !USER ID tag
      INTEGER,                                   INTENT( IN    ) :: grdID        !SAG grid ID
      TYPE( SCIPPlotFieldT ),                    INTENT( IN    ) :: Field        !Field descriptor
      TYPE( SCIPPlotTypeT ),                     INTENT( IN    ) :: PlotType     !Plot definition
      TYPE( SCIPContourHeaderT ),                INTENT( IN    ) :: contourHead  !Contour array header
      TYPE( SCIPContourElementT ), DIMENSION(contourHead%number), TARGET, &
                                                 INTENT( INOUT ) :: contourList  !Contour array
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPPopAreaFieldOMP
!DEC$ ELSE
    END FUNCTION SCIPPopAreaField
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPRestartProjectOMP( CallerID,project )
!DEC$ ELSE
    INTEGER FUNCTION SCIPRestartProject( CallerID,project )
!DEC$ ENDIF
      USE tooluser_fd
      INTEGER,                                     INTENT( IN  ) :: CallerID    !USER ID Tag
      TYPE( createRstT ),                  TARGET, INTENT( IN  ) :: project     !Project input
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPRestartProjectOMP
!DEC$ ELSE
    END FUNCTION SCIPRestartProject
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPRunProjectOMP( CallerID,run )
!DEC$ ELSE
    INTEGER FUNCTION SCIPRunProject( CallerID,run )
!DEC$ ENDIF
      USE tooluser_fd
      INTEGER,                                     INTENT( IN  ) :: CallerID    !USER ID Tag
      TYPE( pendT ),                               INTENT( IN  ) :: run         !Run input
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPRunProjectOMP
!DEC$ ELSE
    END FUNCTION SCIPRunProject
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPInitProjectSSOMP( CallerID,run )
!DEC$ ELSE
    INTEGER FUNCTION SCIPInitProjectSS( CallerID,run )
!DEC$ ENDIF
      USE tooluser_fd
      INTEGER,                                     INTENT( IN  ) :: CallerID    !USER ID Tag
      TYPE( pendT ),                               INTENT( IN  ) :: run         !Run input
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPInitProjectSSOMP
!DEC$ ELSE
    END FUNCTION SCIPInitProjectSS
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPRunProjectSSOMP( t )
!DEC$ ELSE
    INTEGER FUNCTION SCIPRunProjectSS( t )
!DEC$ ENDIF
      REAL, INTENT( OUT ) :: t
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPRunProjectSSOMP
!DEC$ ELSE
    END FUNCTION SCIPRunProjectSS
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPExitProjectSSOMP()
!DEC$ ELSE
    INTEGER FUNCTION SCIPExitProjectSS()
!DEC$ ENDIF
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPExitProjectSSOMP
!DEC$ ELSE
    END FUNCTION SCIPExitProjectSS
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPSizeProjectOMP( CallerID,project,nMtl,nRel )
!DEC$ ELSE
    INTEGER FUNCTION SCIPSizeProject( CallerID,project,nMtl,nRel )
!DEC$ ENDIF
      USE tooluser_fd
      INTEGER,                                     INTENT( IN  ) :: CallerID     !USER ID tag
      TYPE ( projectIDT ),                         INTENT( IN  ) :: project      !Project ID
      INTEGER,                                     INTENT( OUT ) :: nMtl         !number of materials in file
      INTEGER,                                     INTENT( OUT ) :: nRel         !number of releases in file
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPSizeProjectOMP
!DEC$ ELSE
    END FUNCTION SCIPSizeProject
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPSizeProjectMCOMP( CallerID,project,nMtl,nRel,nMCrel )
!DEC$ ELSE
    INTEGER FUNCTION SCIPSizeProjectMC( CallerID,project,nMtl,nRel,nMCrel )
!DEC$ ENDIF
      USE tooluser_fd
      INTEGER,                                     INTENT( IN  ) :: CallerID     !USER ID tag
      TYPE ( projectIDT ),                         INTENT( IN  ) :: project      !Project ID
      INTEGER,                                     INTENT( OUT ) :: nMtl         !number of materials in file
      INTEGER,                                     INTENT( OUT ) :: nRel         !number of releases in file
      INTEGER,                                     INTENT( OUT ) :: nMCrel       !number of multicomponent records in file
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPSizeProjectMCOMP
!DEC$ ELSE
    END FUNCTION SCIPSizeProjectMC
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPTransformOMP( Cin,Cout,np,xp,yp )
!DEC$ ELSE
    INTEGER FUNCTION SCIPTransform( Cin,Cout,np,xp,yp )
!DEC$ ENDIF
      USE tooluser_fd
      TYPE( SCIPFieldCoordinateT ),              INTENT( IN    ) :: Cin          !Input Coordinate data
      TYPE( SCIPFieldCoordinateT ),              INTENT( IN    ) :: Cout         !Output Coordinate data
      INTEGER,                                   INTENT( IN    ) :: np           !number of points
      REAL,      DIMENSION(np),                  INTENT( INOUT ) :: xp           !x coordinate array
      REAL,      DIMENSION(np),                  INTENT( INOUT ) :: yp           !y coordinate array
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPTransformOMP
!DEC$ ELSE
    END FUNCTION SCIPTransform
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPTransformPtOMP( Cin,Cout,np,pt )
!DEC$ ELSE
    INTEGER FUNCTION SCIPTransformPt( Cin,Cout,np,pt )
!DEC$ ENDIF
      USE tooluser_fd
      TYPE( SCIPFieldCoordinateT ),              INTENT( IN    ) :: Cin          !Input Coordinate data
      TYPE( SCIPFieldCoordinateT ),              INTENT( IN    ) :: Cout         !Output Coordinate data
      INTEGER,                                   INTENT( IN    ) :: np           !number of points
      TYPE( SCIPPointT ),        DIMENSION(np),  INTENT( INOUT ) :: pt           !pt array
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPTransformPtOMP
!DEC$ ELSE
    END FUNCTION SCIPTransformPt
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPTransformXYOMP( Cin,Cout,xp,yp )
!DEC$ ELSE
    INTEGER FUNCTION SCIPTransformXY( Cin,Cout,xp,yp )
!DEC$ ENDIF
      USE tooluser_fd
      TYPE( SCIPFieldCoordinateT ),              INTENT( IN    ) :: Cin          !Input Coordinate data
      TYPE( SCIPFieldCoordinateT ),              INTENT( IN    ) :: Cout         !Output Coordinate data
      REAL,                                      INTENT( INOUT ) :: xp           !x coordinate
      REAL,                                      INTENT( INOUT ) :: yp           !y coordinate
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPTransformXYOMP
!DEC$ ELSE
    END FUNCTION SCIPTransformXY
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPWriteFieldOMP( CallerID,grdID,Field,PlotType,contourHead,contourList, &
                                     GUIWrite,nComment,Comment )
!DEC$ ELSE
    INTEGER FUNCTION SCIPWriteField( CallerID,grdID,Field,PlotType,contourHead,contourList, &
                                     GUIWrite,nComment,Comment )
!DEC$ ENDIF
      USE tooluser_fd
      INTEGER,                                   INTENT( IN ) :: CallerID     !USER ID tag
      INTEGER,                                   INTENT( IN ) :: grdID        !SAG grid ID
      TYPE( SCIPPlotFieldT ),                    INTENT( IN ) :: Field        !Field descriptor
      TYPE( SCIPPlotTypeT ),                     INTENT( IN ) :: PlotType     !Plot definition
      TYPE( SCIPContourHeaderT ),                INTENT( IN ) :: contourHead  !Contour array header
      TYPE( SCIPContourElementT ), DIMENSION(contourHead%number), TARGET, &
                                                 INTENT( IN ) :: contourList  !Contour array
      TYPE( ARAPWriteT ),                        INTENT( IN ) :: GUIWrite     !Draw instructions
      INTEGER,                                   INTENT( IN ) :: nComment     !Number of User supplied comments
      TYPE( char128T ),     DIMENSION(nComment), INTENT( IN ) :: Comment      !User supplied comments
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPWriteFieldOMP
!DEC$ ELSE
    END FUNCTION SCIPWriteField
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPWriteInputOMP( CallerID,iInput,tInput0,tInput1 )
!DEC$ ELSE
    INTEGER FUNCTION SCIPWriteInput( CallerID,iInput,tInput0,tInput1 )
!DEC$ ENDIF
      INTEGER,                                     INTENT( IN  ) :: CallerID     !USER ID tag
      INTEGER,                                     INTENT( IN  ) :: iInput       !Request type
      INTEGER, DIMENSION(*),                       INTENT( IN  ) :: tInput0      !Input data
      INTEGER, DIMENSION(*),                       INTENT( IN  ) :: tInput1      !Input data
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPWriteInputOMP
!DEC$ ELSE
    END FUNCTION SCIPWriteInput
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPWriteCtrlFOMP( CallerID,ctrl )
!DEC$ ELSE
    INTEGER FUNCTION SCIPWriteCtrlF( CallerID,ctrl )
!DEC$ ENDIF
      USE timstruct_fd
      INTEGER,                                   INTENT( IN ) :: CallerID     !USER ID tag
      TYPE( pctrlT ),                            INTENT( IN ) :: ctrl
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPWriteCtrlFOMP
!DEC$ ELSE
    END FUNCTION SCIPWriteCtrlF
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPWriteDomainFOMP( CallerID,domain )
!DEC$ ELSE
    INTEGER FUNCTION SCIPWriteDomainF( CallerID,domain )
!DEC$ ENDIF
      USE domstruct_fd
      INTEGER,                                   INTENT( IN ) :: CallerID     !USER ID tag
      TYPE( pspatialT ),                         INTENT( IN ) :: domain
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPWriteDomainFOMP
!DEC$ ELSE
    END FUNCTION SCIPWriteDomainF
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPWriteEndFOMP( CallerID,end )
!DEC$ ELSE
    INTEGER FUNCTION SCIPWriteEndF( CallerID,end )
!DEC$ ENDIF
      USE timstruct_fd
      INTEGER,                                   INTENT( IN ) :: CallerID     !USER ID tag
      TYPE( pendT ),                             INTENT( IN ) :: end
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPWriteEndFOMP
!DEC$ ELSE
    END FUNCTION SCIPWriteEndF
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPWriteFlagsFOMP( CallerID,flags )
!DEC$ ELSE
    INTEGER FUNCTION SCIPWriteFlagsF( CallerID,flags )
!DEC$ ENDIF
      USE inpstruct_fd
      INTEGER,                                   INTENT( IN ) :: CallerID     !USER ID tag
      TYPE( pflagsT ),                           INTENT( IN ) :: flags
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPWriteFlagsFOMP
!DEC$ ELSE
    END FUNCTION SCIPWriteFlagsF
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPWriteInpFOMP( CallerID,input,mtlList )
!DEC$ ELSE
    INTEGER FUNCTION SCIPWriteInpF( CallerID,input,mtlList )
!DEC$ ENDIF
      USE structure_fd
      USE mtlstruct_fd
      INTEGER,                                   INTENT( IN ) :: CallerID     !USER ID tag
      TYPE( pinputT ),                           INTENT( IN ) :: input
      TYPE( materialT  ), DIMENSION(*),          INTENT( IN ) :: mtlList
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPWriteInpFOMP
!DEC$ ELSE
    END FUNCTION SCIPWriteInpF
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPWriteRstFOMP( CallerID,input )
!DEC$ ELSE
    INTEGER FUNCTION SCIPWriteRstF( CallerID,input )
!DEC$ ENDIF
      USE structure_fd
      INTEGER,                                   INTENT( IN ) :: CallerID     !USER ID tag
      TYPE( pinputT ),                           INTENT( IN ) :: input
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPWriteRstFOMP
!DEC$ ELSE
    END FUNCTION SCIPWriteRstF
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPWriteMaterialFOMP( CallerID,material,mtlList )
!DEC$ ELSE
    INTEGER FUNCTION SCIPWriteMaterialF( CallerID,material,mtlList )
!DEC$ ENDIF
      USE mtlstruct_fd
      INTEGER,                                   INTENT( IN ) :: CallerID     !USER ID tag
      TYPE( pmaterialT ),                        INTENT( IN ) :: material
      TYPE( materialT  ), DIMENSION(*),          INTENT( IN ) :: mtlList
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPWriteMaterialFOMP
!DEC$ ELSE
    END FUNCTION SCIPWriteMaterialF
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPWriteOptionsFOMP( CallerID,options )
!DEC$ ELSE
    INTEGER FUNCTION SCIPWriteOptionsF( CallerID,options )
!DEC$ ENDIF
      USE inpstruct_fd
      INTEGER,                                   INTENT( IN ) :: CallerID     !USER ID tag
      TYPE( poptionsT ),                         INTENT( IN ) :: options
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPWriteOptionsFOMP
!DEC$ ELSE
    END FUNCTION SCIPWriteOptionsF
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPWriteReleaseFOMP( CallerID,release,relList )
!DEC$ ELSE
    INTEGER FUNCTION SCIPWriteReleaseF( CallerID,release,relList )
!DEC$ ENDIF
      USE relstruct_fd
      INTEGER,                                   INTENT( IN ) :: CallerID     !USER ID tag
      TYPE( preleaseT ),                         INTENT( IN ) :: release
      TYPE( releaseT  ), DIMENSION(*),           INTENT( IN ) :: relList
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPWriteReleaseFOMP
!DEC$ ELSE
    END FUNCTION SCIPWriteReleaseF
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPWriteReleaseMCFOMP( CallerID,release,relList,nMC,relMCList )
!DEC$ ELSE
    INTEGER FUNCTION SCIPWriteReleaseMCF( CallerID,release,relList,nMC,relMCList )
!DEC$ ENDIF
      USE relstruct_fd
      INTEGER,                                   INTENT( IN ) :: CallerID     !USER ID tag
      TYPE( preleaseT ),                         INTENT( IN ) :: release
      TYPE( releaseT  ), DIMENSION(*),           INTENT( IN ) :: relList
      INTEGER,                                   INTENT( IN ) :: nMC
      TYPE( releaseMCT ), DIMENSION(*),          INTENT( IN ) :: relMCList
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPWriteReleaseMCFOMP
!DEC$ ELSE
    END FUNCTION SCIPWriteReleaseMCF
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPWriteRunFOMP( CallerID,end )
!DEC$ ELSE
    INTEGER FUNCTION SCIPWriteRunF( CallerID,end )
!DEC$ ENDIF
      USE timstruct_fd
      INTEGER,                                   INTENT( IN ) :: CallerID     !USER ID tag
      TYPE( pendT ),                             INTENT( IN ) :: end
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPWriteRunFOMP
!DEC$ ELSE
    END FUNCTION SCIPWriteRunF
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPWriteStartFOMP( CallerID,start )
!DEC$ ELSE
    INTEGER FUNCTION SCIPWriteStartF( CallerID,start )
!DEC$ ENDIF
      USE timstruct_fd
      INTEGER,                                   INTENT( IN ) :: CallerID     !USER ID tag
      TYPE( pstartT ),                           INTENT( IN ) :: start
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPWriteStartFOMP
!DEC$ ELSE
    END FUNCTION SCIPWriteStartF
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPWriteTimeFOMP( CallerID,time )
!DEC$ ELSE
    INTEGER FUNCTION SCIPWriteTimeF( CallerID,time )
!DEC$ ENDIF
      USE timstruct_fd
      INTEGER,                                   INTENT( IN ) :: CallerID     !USER ID tag
      TYPE( ptemporalT ),                        INTENT( IN ) :: time
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPWriteTimeFOMP
!DEC$ ELSE
    END FUNCTION SCIPWriteTimeF
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPWriteWeatherFOMP( CallerID,weather )
!DEC$ ELSE
    INTEGER FUNCTION SCIPWriteWeatherF( CallerID,weather )
!DEC$ ENDIF
      USE metstruct_fd
      INTEGER,                                   INTENT( IN ) :: CallerID     !USER ID tag
      TYPE( pweatherT ),                         INTENT( IN ) :: weather
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPWriteWeatherFOMP
!DEC$ ELSE
    END FUNCTION SCIPWriteWeatherF
!DEC$ ENDIF

!DEC$ IF DEFINED (DUALBUILD)
    INTEGER FUNCTION SCIPWriteSAGIDOMP( UserID,grdI,file,append )
!DEC$ ELSE
    INTEGER FUNCTION SCIPWriteSAGID( UserID,grdI,file,append )
!DEC$ ENDIF
      USE charT_fd
      INTEGER,           INTENT( IN ) :: UserID      !User ID
      INTEGER,           INTENT( IN ) :: grdI        !SAG grid ID
      TYPE( fileNameT ), INTENT( IN ) :: file        !Filename
      INTEGER,           INTENT( IN ) :: append      !Flag to append to existing file
!DEC$ IF DEFINED (DUALBUILD)
    END FUNCTION SCIPWriteSAGIDOMP
!DEC$ ELSE
    END FUNCTION SCIPWriteSAGID
!DEC$ ENDIF

  END INTERFACE

!DEC$ IF DEFINED (DUALBUILD)
END MODULE SCIPtoolOMP
!DEC$ ELSE
END MODULE SCIPtool
!DEC$ ENDIF
