!*******************************************************************************
!$RCSfile$
!$Revision$
!$Date$
!*******************************************************************************
PROGRAM srf2Smp

USE DefSize_fd
USE Extract_fi
USE sagdef_fd
USE sagstr_fd
USE PtrGrdStrItf
USE scipuff_fi, ONLY: create,ntypm,material,mat_mc

IMPLICIT NONE

TYPE( SAGgrid_str ), POINTER     :: grd, agrd

CHARACTER (len=128)              :: string1
CHARACTER (len=PATH_MAXLENGTH)   :: srfFile, sraFile, outFile

INTEGER             :: irv, astat
INTEGER             :: i, j, ios, ft
INTEGER             :: iMult,iKind,iSp
INTEGER             :: grdI, grdA, lun
INTEGER             :: nfld, nRec
REAL                :: xPnt, yPnt

INTEGER, DIMENSION(:),      ALLOCATABLE :: ifld
REAL,    DIMENSION(:),      ALLOCATABLE :: fpnt, apnt, xRec, yRec
REAL,    DIMENSION(:,:,:),  ALLOCATABLE :: srfFldVal

CHARACTER (len=3), DIMENSION(2) :: fTypes

LOGICAL             :: lopen, lDos, lDep, lAmb, isMult, log_interp, lHead

INTEGER, EXTERNAL   :: SAG_InitList, SAG_ClearList, SAG_SetSpecialValue
INTEGER, EXTERNAL   :: SAG_NewGrdStr, SAG_RmvGrdStr
INTEGER, EXTERNAL   :: SAG_OpenID, SAG_CloseID, SAG_InitGridID, SAG_BottomValueID
INTEGER, EXTERNAL   :: SAG_ReadGridID, SAG_ReadMCDataID, SAG_ReadHeaderID

INTEGER, EXTERNAL   :: sysNumArgs
INTEGER, EXTERNAL   :: sysGetArg

CHARACTER(128), EXTERNAL :: AddExtension

!*************************************************************************
!--- BEGIN MAIN PROGRAM
!*************************************************************************

CALL init_error()

grdI = -1

grdA = -1

irv = SAG_SetSpecialValue( .TRUE.,HP_SPV )

irv = SAG_InitList()

WRITE(6,'("Project Name? : ")')
READ(5,*)Project%name
Project%path = ''

WRITE(6,'("Number of receptors? (Enter -1 to read from file):")')
READ(5,*)nRec
IF( nRec > 0 )THEN
  lun = 5
ELSE IF( nRec == -1 )THEN
  lun = 11
  WRITE(6,'("Receptor list file name? ")')
  READ(5,'(A)')string1
  OPEN(lun,FILE=TRIM(string1))
  nRec = 0
  DO
    READ(lun,'(A)',IOSTAT=ios)string1
    IF( ios < 0 )EXIT
    nRec = nRec + 1
  END DO
  REWIND(lun)
ELSE
  GO TO 9999
END IF

ALLOCATE(xRec(nRec),yRec(nRec),STAT=ios )
IF( ios /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'srf2Smp'
  eMessage = 'Error allocating surface grid arrays'
  GOTO 9999
END IF
IF( lun == 5 )WRITE(6,'("X,Y for receptor :")')
DO i = 1,nRec
  IF( lun == 5 )WRITE(*,'("  ",(I3),":")',ADVANCE='NO')i
  READ(lun,*,IOSTAT=ios)xRec(i),yRec(i)
  !READ(lun,'(A)',IOSTAT=ios)string1
  IF( ios /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'srf2Smp'
    WRITE(eMessage,'("Error reading xrec for i = ",I6)')i
    GOTO 9999
  END IF
END DO

IF( lun == 11 )CLOSE(lun)
!nRec = 1
!xRec(1) = -94.1
!yRec(1) = 31.85

CALL ReadProject( Project )

CALL init_srf_blocks( ntypm )

fTypes = (/'dos','dep'/)

DO ft = 1,2  

  !--- Surface File

  srfFile = TRIM( AddExtension( Project%name,fTypes(ft) ) )
 
  outFile = TRIM(Project%name)//'_'//fTypes(ft)//'.csv'
  
  OPEN(lun_out,FILE=outFile)

  IF( ft == 1)THEN
    lDos = .TRUE.
    lDep = .FALSE.
    sraFile = TRIM( AddExtension( Project%name,'ados' ) )
    INQUIRE(FILE=TRIM(sraFile),EXIST=lAmb)
  ELSE
    lDos = .FALSE.
    lDep = .TRUE.
  END IF

  !------ Get a new SAG structure

  irv = SAG_NewGrdStr( grdI )
  IF( lDos .AND. irv == SAG_OK .AND. lAmb )irv = SAG_NewGrdStr( grdA )
  IF( irv /= SAG_OK )THEN
    nError   = UK_ERROR
    eRoutine = 'SAG_NewGrdStr'
    eMessage = 'Error creating SAG surface grid'
    CALL ReportFileName( eInform,'File=',srfFile )
    GOTO 9999
  END IF

  !------ Initialize SAG structure

  lun_in = 100 + grdI
  irv = SAG_InitGridID( srfFile,lun_in,SAG_GRID_BOTH,0,0,0,grdI )
  IF( irv /= SAG_OK )THEN
    nError   = UK_ERROR
    eRoutine = 'InitGrid'
    eMessage = 'Error initializing SAG surface grid'
    CALL ReportFileName( eInform,'File=',srfFile )
    GOTO 9999
  END IF
  IF( lDos .AND. lAmb )THEN
    lun_in = 100 + grdA
    irv = SAG_InitGridID( sraFile,lun_in,SAG_GRID_BOTH,0,0,0,grdA )
    IF( irv /= SAG_OK )THEN
      nError   = UK_ERROR
      eRoutine = 'InitGrid'
      eMessage = 'Error initializing SAG surface grid'
      CALL ReportFileName( eInform,'File=',sraFile )
      GOTO 9999
    END IF
  END IF
  
  !------ Open surface file; read nvart for block version

  irv = SAG_OpenID( grdI )
  IF( lDos .AND. irv == SAG_OK .AND. lAmb )irv = SAG_OpenID( grdA )
  IF( irv /= SAG_OK )THEN
    nError   = UK_ERROR
    eRoutine = 'InitGrid'
    eMessage = 'Error opening SAG file'
    CALL ReportFileName( eInform,'File=',srfFile )
    GOTO 9999
  END IF
  
  lHead = .TRUE.

  DO  ! Time Loop

    grd => SAG_PtrGrdStr( grdI ) ! Associate "local" grid structure pointer
    IF( lDos )agrd => SAG_PtrGrdStr( grdA )

    irv = SAG_ReadHeaderID( grdI )
    IF( irv == SAG_OK .AND. lDos .AND. lAmb )irv = SAG_ReadHeaderID( grdA )
    IF( irv == SAG_EOF )EXIT
  
    IF( irv /= SAG_OK )THEN
      nError   = UK_ERROR
      eRoutine = 'InitGrid'
      eMessage = 'Error reading SAG file'
      CALL ReportFileName( eInform,'File=',grd%file )
      GOTO 9999
    END IF

    !------ Allocate grid and data fields for ncells
    grd%mxnam = grd%nvart
    grd%mxgrd = grd%ncells
    grd%mxfld = grd%nvart  ! All variables
    grd%naux  = 0          ! turn off any auxiliary fields
    
    WRITE(*,*)grd%time,grd%nvart,grd%ncells
    
    IF( lDos .AND. lAmb )THEN
      !------ Allocate grid and data fields for ncells
      agrd%mxnam = agrd%nvart
      agrd%mxgrd = agrd%ncells
      agrd%mxfld = agrd%nvart  ! All variables
      agrd%naux  = 0           ! turn off any auxiliary fields
        
      WRITE(*,*)grd%time,grd%nvart,grd%ncells      
    END IF
    
    ALLOCATE( grd%ipgrd(grd%mxgrd),STAT=ios )
    IF( ios == 0 .AND. lDos .AND. lAmb )ALLOCATE( agrd%ipgrd(agrd%mxgrd),STAT=ios )
    IF( ios /= 0 )THEN
      nError   = UK_ERROR
      eRoutine = 'srf2Smp'
      eMessage = 'Error allocating surface grid arrays'
      GOTO 9999
    END IF

    ALLOCATE( grd%ipdat(grd%mxgrd*grd%mxfld),STAT=ios )
    IF( ios == 0 .AND. lDos .AND. lAmb )ALLOCATE( agrd%ipdat(agrd%mxgrd*agrd%mxfld),STAT=ios )
    IF( ios /= 0 )THEN
      nError   = UK_ERROR
      eRoutine = 'srf2Smp'
      eMessage = 'Error allocating surface grid arrays'
      GOTO 9999
    END IF

    !------ Read grid

    irv = SAG_ReadGridID( grdI )
    IF( irv == SAG_OK .AND. lDos .AND. lAmb )irv = SAG_ReadGridID( grdA )
    IF( irv /= SAG_OK )THEN
      nError   = UK_ERROR
      eRoutine = 'SAG_ReadGridID'
      eMessage = 'Error reading surface grid'
      GOTO 9999
    END IF

    !------ Read specified fields

    irv = SAG_ReadMCDataID( grdI ) ! All fields
    IF( irv == SAG_OK .AND. lDos .AND. lAmb )irv = SAG_ReadMCDataID( grdA )
    IF( irv /= SAG_OK )THEN
      nError   = UK_ERROR
      eRoutine = 'SAG_ReadMCDataID'
      eMessage = 'Error reading surface grid'
      GOTO 9999
    END IF

    nfld = grd%mxfld
  
    IF( nfld > 3 )THEN
      isMult = .TRUE.
      IF( .NOT. lAmb )THEN
        nError   = UK_ERROR
        eRoutine = 'srf2smp'
        eMessage = 'Ambient dose file for multicomponent species not available'
        eInform  = 'File = '//TRIM(sraFile)
        GOTO 9999
      END IF
    ELSE
      isMult = .FALSE.
    END IF
  
    !IF( grd%ipnam(3) == 'Scl' )THEN
      nKind = grd%nvart - 3
    !ELSE
    !  nKind = grd%nvart - 2
    !END IF
    
    IF( lDep )THEN
      nKind = nKind/2
    END IF
    
    IF( lHead )THEN
      IF( isMult )THEN
        IF( lDep )THEN
          WRITE(lun_out,'("# Time, X, Y, C")',ADVANCE='NO')
          iMult = grd%nvart - 2*nKind      
          DO iKind = 1,nKind
            iSp = iMult + 2*iKind - 1 
            WRITE(lun_out,'(", ",A,", ",A)',ADVANCE='NO')TRIM(grd%ipnam(iSp)),TRIM(grd%ipnam(iSp+1))
          END DO
        ELSE
          WRITE(lun_out,'("# Time, X, Y, C, T(k), P(atm), H(g/g)")',ADVANCE='NO')
          iMult = grd%nvart - nKind      
          DO iKind = 1,nKind
            iSp = iMult + iKind 
            WRITE(lun_out,'(", ",A,", ",A)',ADVANCE='NO')TRIM(grd%ipnam(iSp))//'(Tot)',TRIM(agrd%ipnam(iSp))//'(Amb)'
          END DO
        END IF
      ELSE
        WRITE(lun_out,'("# Time, X, Y, C")',ADVANCE='NO')
      END IF
      WRITE(lun_out,'()',ADVANCE='YES')
      lHead = .FALSE.
    END IF
    
    ALLOCATE(ifld(nfld),fpnt(nfld),STAT=astat)
    IF( lDos )ALLOCATE(apnt(nfld),STAT=astat)

    ALLOCATE(srfFldVal(nRec,MAX(1,nKind),2),STAT=astat)

    DO i = 1,nfld
      ifld(i) = i
    END DO

    irv = SAG_BottomValueID( grdI,nfld,ifld )
    IF( irv == SAG_OK .AND. lDos )irv = SAG_BottomValueID( grdA,nfld,ifld )
  
    DO j = 1,nRec

      xpnt = xRec(j)
      ypnt = yRec(j)
    
      fpnt = 0.
      log_interp  = .TRUE.
  
      CALL GetPlotFieldMCVal( xPnt,yPnt,fpnt,grdI,log_interp )
      IF( nError /= NO_ERROR )THEN
        GOTO 9999
      END IF
      
      IF( lDos )THEN
        apnt = 0.
        CALL GetPlotFieldMCVal( xPnt,yPnt,apnt,grdA,log_interp )
        IF( nError /= NO_ERROR )THEN
          GOTO 9999
        END IF
      END IF
  
      IF( isMult )THEN
                         
        IF( lDep )THEN
      
          WRITE(lun_out,'(3(1pE13.5,", "),1pE13.5)',ADVANCE='NO')grd%time,xpnt,ypnt,fpnt(1)
          
          iMult = grd%nvart - 2*nKind
      
          DO iKind = 1,nKind
            iSp = iMult + 2*iKind - 1
            srfFldVal(j,iKind,1) = fpnt(iSp)     ! Dry
            srfFldVal(j,iKind,2) = fpnt(iSp+1)   ! Wet
            WRITE(lun_out,'(", ",1pE13.5,", ",1pE13.5)',ADVANCE='NO')fpnt(iSp),fpnt(iSp+1)
          END DO
        
        ELSE
          
          WRITE(lun_out,'(6(1pE13.5,", "),1pE13.5)',ADVANCE='NO')grd%time,xpnt,ypnt,fpnt(1),apnt(1),apnt(2),apnt(3)
      
          iMult = grd%nvart - nKind 

          DO iKind = 1,nKind
            iSp = iMult + iKind
            srfFldVal(j,iKind,1) = fpnt(iSp)     ! Total Conc
            srfFldVal(j,iKind,2) = apnt(iSp)     ! Ambient Conc
            WRITE(lun_out,'(", ",1pE13.5)',ADVANCE='NO')fpnt(iSp)
            WRITE(lun_out,'(", ",1pE13.5)',ADVANCE='NO')apnt(iSp)
          END DO 
               
        END IF
      
      ELSE
    
        WRITE(lun_out,'(3(1pE13.5,", "),1pE13.5)',ADVANCE='NO')grd%time,xpnt,ypnt,fpnt(1)
        srfFldVal(j,1,1) = fpnt(1)
    
      END IF
      
      WRITE(lun_out,'()',ADVANCE='YES')      
      
    END DO  ! Receptor loop

    IF( ALLOCATED(ifld)      )DEALLOCATE(ifld,fpnt,STAT=astat)
    IF( ALLOCATED(srfFldVal) )DEALLOCATE(srfFldVal,STAT=astat)

    IF( ASSOCIATED(grd%ipgrd) )THEN
      DEALLOCATE(grd%ipgrd,STAT=astat)
      NULLIFY(grd%ipgrd)
    END IF

    IF( ASSOCIATED(grd%ipdat) )THEN
      DEALLOCATE(grd%ipdat,STAT=astat)
      NULLIFY(grd%ipdat)
    END IF
    
    grd%mxgrd = 0
    
    IF( lDos .AND. lAmb )THEN
      
      IF( ALLOCATED(apnt) )DEALLOCATE(apnt,STAT=astat)
      
      IF( ASSOCIATED(agrd%ipgrd) )THEN
        DEALLOCATE(agrd%ipgrd,STAT=astat)
        NULLIFY(agrd%ipgrd)
      END IF

      IF( ASSOCIATED(agrd%ipdat) )THEN
        DEALLOCATE(agrd%ipdat,STAT=astat)
        NULLIFY(agrd%ipdat)
      END IF
    
      agrd%mxgrd = 0
      
    END IF

  END DO ! Time loop

  INQUIRE(UNIT=lun_out,OPENED=lopen,IOSTAT=ios)
  IF( lopen )CLOSE(lun_out)
  
END DO ! File Type loop

9999 CONTINUE

!------ Close file and deallocate grid structure

IF( nError /= NO_ERROR )THEN
  WRITE(*,*)TRIM(eMessage)
  WRITE(*,*)TRIM(eInform)
  WRITE(*,*)TRIM(eAction)
END IF

IF( grdI > 0 )THEN
  irv = SAG_CloseID( grdI )
  irv = SAG_RmvGrdStr( grdI )
END IF

IF( grdA > 0 )THEN
  irv = SAG_CloseID( grdA )
  irv = SAG_RmvGrdStr( grdA )
END IF

IF( ALLOCATED(xRec) )DEALLOCATE(xRec,yRec)

STOP
END