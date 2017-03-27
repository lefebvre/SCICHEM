!*******************************************************************************
!$RCSfile$
!$Revision$
!$Date$
!*******************************************************************************
!*******************************************************************************
!            UnloadRelease
!*******************************************************************************
SUBROUTINE UnloadRelease( release )

USE convert_fd
USE release_fd
USE relparam_fd
USE scipuff_fi

!     Load SCIPUFF commons from an SCIP Release structure

IMPLICIT NONE

TYPE( releaseT ), INTENT( IN ) :: release

INTEGER ios, n

TYPE( MCrelData ), POINTER :: rel

CHARACTER(192), EXTERNAL :: StripNull
REAL,           EXTERNAL :: ScaleReal

!==== Initialize check

IF( release%tRel == 1.0E+36 .OR. release%tRel == NOT_SET_R )THEN
  nError = IV_ERROR
  eRoutine = 'UnloadRelease'
  eMessage = 'Invalid release parameter : tRel'
  eInform  = 'Valid release not defined'
  GOTO 9999
END IF

!==== Unload
CALL InitRelease()

!==== Basic parameters

trel = ScaleReal( release%tRel,HCF_HOUR2SEC )

  xrel = release%xRel
  yrel = release%yRel
  zrel = release%zRel


!==== Release type dependent parameters

relName    = TRIM(StripNull(release%relName))
relDisplay = TRIM(StripNull(release%relDisplay))

  relmat    = TRIM(StripNull(release%material))
  relStatus = release%status

SELECT CASE( release%type )
  CASE( HR_INST )
    reltyp = 'I'
    CALL UnloadReleaseInst( release%relData )
    IF( nError /= NO_ERROR )GOTO 9999
  CASE( HR_XINST )
    reltyp = 'IX'
    CALL UnloadReleaseXInst( release%relData )
    IF( nError /= NO_ERROR )GOTO 9999
  CASE( HR_XINST3 )
    reltyp = 'IX3'
    CALL UnloadReleaseXInst3( release%relData )
    IF( nError /= NO_ERROR )GOTO 9999
  CASE( HR_FILE )
    reltyp = 'IF'
    CALL UnloadReleaseFile( release%relData )
    IF( nError /= NO_ERROR )GOTO 9999
  CASE( HR_PUFF )
    reltyp = 'IP'
    CALL UnloadReleasePuff( release%relData )
    IF( nError /= NO_ERROR )GOTO 9999
  CASE( HR_CONT )
    reltyp = 'C'
    CALL UnloadReleaseCont( release%relData )
    IF( nError /= NO_ERROR )GOTO 9999
  CASE( HR_MOVE )
    reltyp = 'CM'
    CALL UnloadReleaseMove( release%relData )
    IF( nError /= NO_ERROR )GOTO 9999
  CASE( HR_STACK )
    umom = DEF_VAL_R
    vmom = DEF_VAL_R
    reltyp = 'CS'
    CALL UnloadReleaseStack( release%relData )
    IF( nError /= NO_ERROR )GOTO 9999
  CASE( HR_STACK3 )
    reltyp = 'CS'
    CALL UnloadReleaseStack3( release%relData )
    IF( nError /= NO_ERROR )GOTO 9999
  CASE( HR_PRIME )
    umom = 0. !DEF_VAL_R
    vmom = 0. !DEF_VAL_R
    reltyp = 'CSP'
    CALL UnloadReleaseStack( release%relData )
    IF( nError /= NO_ERROR )GOTO 9999
  CASE( HR_POOL )
    reltyp(1:2) = 'CP'
    CALL UnloadReleasePool( release%relData )
    IF( nError /= NO_ERROR )GOTO 9999
  CASE DEFAULT
    nError   = IV_ERROR
    eRoutine = 'UnloadRelease'
    eMessage = 'Invalid release type'
    WRITE(eInform,'(A,I8)')'type =',release%type
    GOTO 9999
END SELECT

!------ Unload MC
IF( release%nMC > 0 )THEN
  ALLOCATE( RelMC%rel,STAT=ios )
  rel => RelMC%rel
  NULLIFY( rel%next )
  DO n = 1,release%nMC
    rel%MCname = TRIM(release%MCname(n))
    rel%MCmass = release%MCmass(n)
    IF( n < release%nMC )THEN
      ALLOCATE( rel%next,STAT=ios )
      IF( ios /= 0 )THEN
        nError = UK_ERROR
        eRoutine = 'UnloadRelease'
        eMessage = 'Error allocating muticomponent array'
        GOTO 9999
      END IF
      rel  => rel%next
      NULLIFY( rel%next )
    END IF
  END DO
  RelMC%nList = release%nMC
ELSE
  RelMC%nList = 0
END IF
9999 CONTINUE

RETURN
END
!*******************************************************************************
!            LoadRelease
!*******************************************************************************
SUBROUTINE LoadRelease( release )

USE convert_fd
USE release_fd
USE relparam_fd
USE scipuff_fi

!     Load an SCIP Release structure from SCIPUFF commons

IMPLICIT NONE

TYPE( releaseT ), INTENT( OUT ) :: release

INTEGER n

TYPE( MCrelData ), POINTER :: rel

REAL, EXTERNAL :: ScaleReal

!==== Load

IF( trel == 1.0E+36 .OR. trel == NOT_SET_R )THEN
  nError   = IV_ERROR
  eRoutine = 'LoadRelease'
  eMessage = 'Invalid release parameter : trel'
  eInform  = 'Valid release not defined'
  GOTO 9999
END IF

release%padding = NOT_SET_I ! For Lahey Global Check

!==== Basic parameters

release%tRel = ScaleReal( trel,HCF_SEC2HOUR )

IF( TRIM(reltyp(1:1)) /= 'X' )THEN

  release%xRel = xrel
  release%yRel = yrel
  release%zRel = zrel

  release%notUsedA = NOT_SET_R
  release%notUsedB = NOT_SET_R

  release%notUsed = NOT_SET_R

  release%relData%padding(1:HS_PADRELGEN) = NOT_SET_I

  release%MCname(1:MAX_MCR) = NOT_SET_C
  release%MCmass(1:MAX_MCR) = NOT_SET_R

END IF

!==== Release type dependent parameters

IF( TRIM(reltyp(1:1)) /= 'X' )THEN
  IF( tdur == DEF_VAL_R )THEN
    IF( reltyp(1:2) /= 'CP' )THEN
      nError = IV_ERROR
      eRoutine = 'LoadRelease'
      eMessage = 'Invalid release parameter : Duration'
      eInform  = 'Only Pool sources accept duration=DEFAULT'
      GOTO 9999
    END IF
  END IF

  release%material = relmat
  release%status   = relStatus

END IF

release%relName    = relName
release%relDisplay = relDisplay

SELECT CASE( TRIM(reltyp) )
  CASE( 'I' )
    IF( name_rel == ' ' )THEN
      release%type = HR_INST
      CALL LoadReleaseInst( release%relData )
      IF( nError /= NO_ERROR )GOTO 9999
    ELSE
      release%type = HR_FILE
      CALL LoadReleaseFile( release%relData )
      IF( nError /= NO_ERROR )GOTO 9999
    END IF
  CASE( 'IX' )
    release%type = HR_XINST
    CALL LoadReleaseXInst( release%relData )
    IF( nError /= NO_ERROR )GOTO 9999
  CASE( 'IX3' )
    release%type = HR_XINST3
    CALL LoadReleaseXInst3( release%relData )
    IF( nError /= NO_ERROR )GOTO 9999
  CASE( 'IF' )
    release%type = HR_FILE
    CALL LoadReleaseFile( release%relData )
    IF( nError /= NO_ERROR )GOTO 9999
  CASE( 'IP' )
    release%type = HR_PUFF
    CALL LoadReleasePuff( release%relData )
    IF( nError /= NO_ERROR )GOTO 9999
  CASE( 'C' )
    release%type = HR_CONT
    CALL LoadReleaseCont( release%relData )
    IF( nError /= NO_ERROR )GOTO 9999
  CASE( 'CM' )
    release%type = HR_MOVE
    CALL LoadReleaseMove( release%relData )
    IF( nError /= NO_ERROR )GOTO 9999
  CASE( 'CSP' )  ! PRIME
    release%type = HR_PRIME
    CALL LoadReleaseStack( release%relData )
    IF( nError /= NO_ERROR )GOTO 9999
  CASE( 'CS' ) ! Dynamics
    IF( umom == DEF_VAL_R .AND. vmom == DEF_VAL_R )THEN
      release%type = HR_STACK
      CALL LoadReleaseStack( release%relData )
    ELSE
      release%type = HR_STACK3
      CALL LoadReleaseStack3( release%relData )
    END IF
    IF( nError /= NO_ERROR )GOTO 9999
  CASE( 'CP' )
    release%type = HR_POOL
    CALL LoadReleasePool( release%relData )
    IF( nError /= NO_ERROR )GOTO 9999
  CASE DEFAULT
    nError   = IV_ERROR
    eRoutine = 'LoadRelease'
    eMessage = 'Invalid release type'
    eInform  = 'type ='// TRIM(reltyp)
    GOTO 9999
END SELECT

IF( RelMC%nList > 0 )THEN

  rel => RelMC%rel
  n = 0

  DO WHILE( ASSOCIATED(rel) )
    n = n + 1
    release%MCname(n) = rel%MCname
    release%MCmass(n) = rel%MCmass
    release%nMC       = n
    rel  => rel%next
  END DO

ELSE

  release%nMC = 0

END IF

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            UnloadReleaseMC
!*******************************************************************************
SUBROUTINE UnloadReleaseMC( release,nMC,releaseMC )

USE release_fd
USE scipuff_fi

!     Unload a Multicomponent SCIP Release structure into SCIPUFF commons

IMPLICIT NONE

TYPE( releaseT ),                 INTENT( IN ) :: release
INTEGER,                          INTENT( IN ) :: nMC
TYPE( releaseMCT ), DIMENSION(*), INTENT( IN ) :: releaseMC

INTEGER n, ios

TYPE( MCrelData ), POINTER :: rel

CALL UnloadRelease( release )
IF( nError /= NO_ERROR )GOTO 9999

ALLOCATE( RelMC%rel,STAT=ios )
rel => RelMC%rel
NULLIFY( rel%next )

DO n = 1,nMC
  rel%MCname = releaseMC(n)%MCname
  rel%MCmass = releaseMC(n)%MCmass
  IF( n < nMC )THEN
    ALLOCATE( rel%next,STAT=ios )
    IF( ios /= 0 )THEN
      nError = UK_ERROR
      eRoutine = 'UnloadReleaseMC'
      eMessage = 'Error allocating muticomponent array'
      GOTO 9999
    END IF
    rel  => rel%next
    NULLIFY( rel%next )
  END IF
END DO

relMC%nList = nMC

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            LoadReleaseMC
!*******************************************************************************
SUBROUTINE LoadReleaseMC( release,releaseMC )

USE release_fd
USE scipuff_fi

!     Load a Multicomponent SCIP Release structure from SCIPUFF commons

IMPLICIT NONE

TYPE( releaseT ),                 INTENT( OUT ) :: release
TYPE( releaseMCT ), DIMENSION(*), INTENT( OUT ) :: releaseMC

INTEGER n

TYPE( MCrelData ), POINTER :: rel

CALL LoadRelease( release )
IF( nError /= NO_ERROR )GOTO 9999

rel => RelMC%rel
n = 0

DO WHILE( ASSOCIATED(rel) )
  n = n + 1
  releaseMC(n)%MCname = rel%MCname
  releaseMC(n)%MCmass = rel%MCmass
  rel  => rel%next
END DO

9999 CONTINUE

RETURN
END
!*******************************************************************************
!        UnloadReleaseCont
!*******************************************************************************
SUBROUTINE UnloadReleaseCont( relDataIn )

USE convert_fd
USE release_fd
USE relparam_fd
USE scipuff_fi

!     Load SCIPUFF commons from an SCIP Release structure

IMPLICIT NONE

TYPE( relGenT ), INTENT( IN ) :: relDataIn

TYPE( relContT ) relData

REAL, EXTERNAL :: ScaleReal

!==== Unload

relData = TRANSFER(relDataIn,relData)

subgroup = relData%distribution

cmass = relData%rate
tdur  = ScaleReal( relData%duration,HCF_HOUR2SEC )

sigx     = 0.0
sigy     = relData%sigY
sigz     = relData%sigZ
size_rel = NOT_SET_R

umom = 0.0
vmom = 0.0
IF( relData%momentum /= NOT_SET_R )THEN
  wmom = relData%momentum
END IF

IF( relData%buoyancy /= NOT_SET_R )THEN
  buoy = relData%buoyancy
END IF

IF( relData%distribution == HD_LOGNORM )THEN
  rel_param(REL_MMD_INDX)   = relData%MMD
  rel_param(REL_SIGMA_INDX) = relData%sigma
END IF

rel_param(REL_WMFRAC_INDX) = relData%dryFrac
rel_param(REL_AFRAC_INDX)  = relData%activeFrac

RETURN
END
!*******************************************************************************
!        LoadReleaseCont
!*******************************************************************************
SUBROUTINE LoadReleaseCont( relDataOut )

USE convert_fd
USE release_fd
USE relparam_fd
USE scipuff_fi

!     Load an SCIP Release structure from SCIPUFF commons

IMPLICIT NONE

TYPE( relGenT ), INTENT( OUT ) :: relDataOut

TYPE( relContT ) relData

REAL, EXTERNAL :: ScaleReal

!==== Load

relData%distribution = subgroup

relData%rate     = cmass
relData%duration = ScaleReal( tdur,HCF_SEC2HOUR )

IF( size_rel > 0 )THEN
  relData%sigY = size_rel
  relData%sigZ = size_rel
ELSE
  relData%sigY = sigy
  relData%sigZ = sigz
END IF

relData%momentum = wmom
relData%buoyancy = buoy

IF( rel_param(REL_MMD_INDX)/= NOT_SET_R )THEN
  relData%MMD   = rel_param(REL_MMD_INDX)
  relData%sigma = rel_param(REL_SIGMA_INDX)
ELSE
  relData%MMD   = NOT_SET_R
  relData%sigma = NOT_SET_R
END IF

relData%activeFrac = rel_param(REL_AFRAC_INDX)
relData%dryFrac = rel_param(REL_WMFRAC_INDX)
relData%padding = NOT_SET_I

relDataOut = TRANSFER(relData,relDataOut)

RETURN
END
!*******************************************************************************
!        UnloadReleaseInst
!*******************************************************************************
SUBROUTINE UnloadReleaseInst( relDataIn )

USE release_fd
USE relparam_fd
USE scipuff_fi

!     Load SCIPUFF commons from an SCIP Release structure

IMPLICIT NONE

TYPE( relGenT ), INTENT( IN ) :: relDataIn

TYPE( relInstT ) relData

REAL, EXTERNAL :: Int2Real

!==== Unload

relData = TRANSFER(relDataIn,relData)

subgroup = relData%distribution

cmass = relData%mass

sigx     = relData%sigX
sigy     = relData%sigY
sigz     = relData%sigZ
size_rel = NOT_SET_R

umom = 0.0
vmom = 0.0
IF( relData%momentum /= NOT_SET_R )THEN
  wmom = relData%momentum
END IF

IF( relData%buoyancy /= NOT_SET_R )THEN
  buoy = relData%buoyancy
END IF

IF( relData%distribution == HD_LOGNORM )THEN
  rel_param(REL_MMD_INDX)   = relData%MMD
  rel_param(REL_SIGMA_INDX) = relData%sigma
END IF

IF( relData%nRandom > 0 )THEN
  rel_param(REL_RAND_INDX)   = Int2Real(relData%nRandom)
  rel_param(REL_SEED_INDX)   = Int2Real(relData%ranSeed)
  rel_param(REL_SPREAD_INDX) = relData%ranSpread
ELSE
  rel_param(REL_RAND_INDX)   = NOT_SET_R
  rel_param(REL_SEED_INDX)   = NOT_SET_R
  rel_param(REL_SPREAD_INDX) = NOT_SET_R
END IF

rel_param(REL_SPREADT_INDX) = NOT_SET_R
rel_param(REL_SPREADV_INDX) = NOT_SET_R
rel_param(REL_RANDIR_INDX)  = NOT_SET_R

rel_param(REL_WMFRAC_INDX) = relData%dryFrac
rel_param(REL_AFRAC_INDX)  = relData%activeFrac

RETURN
END
!*******************************************************************************
!        LoadReleaseInst
!*******************************************************************************
SUBROUTINE LoadReleaseInst( relDataOut )

USE release_fd
USE relparam_fd
USE scipuff_fi

!     Load an SCIP Release structure from SCIPUFF commons

IMPLICIT NONE

TYPE( relGenT ), INTENT( OUT ) :: relDataOut

TYPE( relInstT ) relData

INTEGER, EXTERNAL :: Real2Int

!==== Load

relData%distribution = subgroup

relData%mass = cmass

IF( size_rel > 0 )THEN
  relData%sigX = size_rel
  relData%sigY = size_rel
  relData%sigZ = size_rel
ELSE
  relData%sigX = sigx
  relData%sigY = sigy
  relData%sigZ = sigz
END IF

relData%momentum = wmom
relData%buoyancy = buoy

IF( rel_param(REL_MMD_INDX)/= NOT_SET_R )THEN
  relData%MMD   = rel_param(REL_MMD_INDX)
  relData%sigma = rel_param(REL_SIGMA_INDX)
ELSE
  relData%MMD   = NOT_SET_R
  relData%sigma = NOT_SET_R
END IF

IF( rel_param(REL_RAND_INDX) > 0 )THEN
  relData%nRandom   = Real2Int(rel_param(REL_RAND_INDX))
  relData%ranSeed   = Real2Int(rel_param(REL_SEED_INDX))
  relData%ranSpread = rel_param(REL_SPREAD_INDX)
ELSE
  relData%nRandom   = NOT_SET_I
  relData%ranSeed   = NOT_SET_I
  relData%ranSpread = NOT_SET_R
END IF

relData%activeFrac = rel_param(REL_AFRAC_INDX)
relData%dryFrac = rel_param(REL_WMFRAC_INDX)
relData%padding = NOT_SET_I

relDataOut = TRANSFER(relData,relDataOut)

RETURN
END
!*******************************************************************************
!        UnloadReleaseXInst
!*******************************************************************************
SUBROUTINE UnloadReleaseXInst( relDataIn )

USE release_fd
USE relparam_fd
USE scipuff_fi

!     Load SCIPUFF commons from an SCIP Release structure

IMPLICIT NONE

TYPE( relGenT ), INTENT( IN ) :: relDataIn

TYPE( relXInstT ) relData

REAL, EXTERNAL :: Int2Real

!==== Unload

relData = TRANSFER(relDataIn,relData)

subgroup = relData%distribution

cmass = relData%mass

sigx     = relData%sigX
sigy     = relData%sigY
sigz     = relData%sigZ
sigRxy   = relData%sigRxy
sigRxz   = relData%sigRxz
sigRyz   = relData%sigRyz
size_rel = NOT_SET_R

umom = 0.0
vmom = 0.0
IF( relData%momentum /= NOT_SET_R )THEN
  wmom = relData%momentum
END IF

IF( relData%buoyancy /= NOT_SET_R )THEN
  buoy = relData%buoyancy
END IF

IF( relData%distribution == HD_LOGNORM )THEN
  rel_param(REL_MMD_INDX)   = relData%MMD
  rel_param(REL_SIGMA_INDX) = relData%sigma
END IF

IF( relData%nRandom > 0 )THEN
  rel_param(REL_RAND_INDX)   = Int2Real(relData%nRandom)
  rel_param(REL_SEED_INDX)   = Int2Real(relData%ranSeed)
  rel_param(REL_SPREAD_INDX) = relData%ranSpread
ELSE
  rel_param(REL_RAND_INDX)   = NOT_SET_R
  rel_param(REL_SEED_INDX)   = NOT_SET_R
  rel_param(REL_SPREAD_INDX) = NOT_SET_R
END IF

rel_param(REL_SPREADT_INDX) = NOT_SET_R
rel_param(REL_SPREADV_INDX) = NOT_SET_R
rel_param(REL_RANDIR_INDX)  = NOT_SET_R

rel_param(REL_WMFRAC_INDX) = relData%dryFrac
rel_param(REL_AFRAC_INDX)  = relData%activeFrac

RETURN
END
!*******************************************************************************
!        LoadReleaseXInst
!*******************************************************************************
SUBROUTINE LoadReleaseXInst( relDataOut )

USE release_fd
USE relparam_fd
USE scipuff_fi

!     Load an SCIP Release structure from SCIPUFF commons

IMPLICIT NONE

TYPE( relGenT ), INTENT( OUT ) :: relDataOut

TYPE( relXInstT ) relData

INTEGER, EXTERNAL :: Real2Int

!==== Load

relData%distribution = subgroup

relData%mass = cmass

IF( size_rel > 0 )THEN
  relData%sigX = size_rel
  relData%sigY = size_rel
  relData%sigZ = size_rel
ELSE
  relData%sigX = sigx
  relData%sigY = sigy
  relData%sigZ = sigz
END IF

relData%sigRxy = sigRxy
relData%sigRxz = sigRxz
relData%sigRyz = sigRyz

relData%momentum = wmom
relData%buoyancy = buoy

IF( rel_param(REL_MMD_INDX)/= NOT_SET_R )THEN
  relData%MMD   = rel_param(REL_MMD_INDX)
  relData%sigma = rel_param(REL_SIGMA_INDX)
ELSE
  relData%MMD   = NOT_SET_R
  relData%sigma = NOT_SET_R
END IF

IF( rel_param(REL_RAND_INDX) > 0 )THEN
  relData%nRandom   = Real2Int(rel_param(REL_RAND_INDX))
  relData%ranSeed   = Real2Int(rel_param(REL_SEED_INDX))
  relData%ranSpread = rel_param(REL_SPREAD_INDX)
ELSE
  relData%nRandom   = NOT_SET_I
  relData%ranSeed   = NOT_SET_I
  relData%ranSpread = NOT_SET_R
END IF

relData%activeFrac = rel_param(REL_AFRAC_INDX)
relData%dryFrac = rel_param(REL_WMFRAC_INDX)
relData%padding = NOT_SET_I

relDataOut = TRANSFER(relData,relDataOut)

RETURN
END
!*******************************************************************************
!        UnloadReleaseXInst
!*******************************************************************************
SUBROUTINE UnloadReleaseXInst3( relDataIn )

USE release_fd
USE relparam_fd
USE scipuff_fi

!     Load SCIPUFF commons from an SCIP Release structure

IMPLICIT NONE

TYPE( relGenT ), INTENT( IN ) :: relDataIn

TYPE( relXInst3T ) relData

REAL, EXTERNAL :: Int2Real

!==== Unload

relData = TRANSFER(relDataIn,relData)

subgroup = relData%distribution

cmass = relData%mass

sigx     = relData%sigX
sigy     = relData%sigY
sigz     = relData%sigZ
sigRxy   = relData%sigRxy
sigRxz   = relData%sigRxz
sigRyz   = relData%sigRyz
size_rel = NOT_SET_R

umom = 0.0
vmom = 0.0
IF( relData%momentum /= NOT_SET_R )THEN
  wmom = relData%momentum
END IF

IF( relData%buoyancy /= NOT_SET_R )THEN
  buoy = relData%buoyancy
END IF

IF( relData%distribution == HD_LOGNORM )THEN
  rel_param(REL_MMD_INDX)   = relData%MMD
  rel_param(REL_SIGMA_INDX) = relData%sigma
END IF

IF( relData%nRandom > 0 )THEN
  rel_param(REL_RAND_INDX)    = Int2Real(relData%nRandom)
  rel_param(REL_SEED_INDX)    = Int2Real(relData%ranSeed)
  rel_param(REL_SPREAD_INDX)  = relData%ranSpreadA
  rel_param(REL_SPREADT_INDX) = relData%ranSpreadT
  rel_param(REL_SPREADV_INDX) = relData%ranSpreadV
  rel_param(REL_RANDIR_INDX)  = relData%ranDir
ELSE
  rel_param(REL_RAND_INDX)    = NOT_SET_R
  rel_param(REL_SEED_INDX)    = NOT_SET_R
  rel_param(REL_SPREAD_INDX)  = NOT_SET_R
  rel_param(REL_SPREADT_INDX) = NOT_SET_R
  rel_param(REL_SPREADV_INDX) = NOT_SET_R
  rel_param(REL_RANDIR_INDX)  = NOT_SET_R
END IF

rel_param(REL_WMFRAC_INDX) = relData%dryFrac
rel_param(REL_AFRAC_INDX)  = relData%activeFrac

RETURN
END
!*******************************************************************************
!        LoadReleaseXInst
!*******************************************************************************
SUBROUTINE LoadReleaseXInst3( relDataOut )

USE release_fd
USE relparam_fd
USE scipuff_fi

!     Load an SCIP Release structure from SCIPUFF commons

IMPLICIT NONE

TYPE( relGenT ), INTENT( OUT ) :: relDataOut

TYPE( relXInst3T ) relData

INTEGER, EXTERNAL :: Real2Int

!==== Load

relData%distribution = subgroup

relData%mass = cmass

IF( size_rel > 0 )THEN
  relData%sigX = size_rel
  relData%sigY = size_rel
  relData%sigZ = size_rel
ELSE
  relData%sigX = sigx
  relData%sigY = sigy
  relData%sigZ = sigz
END IF

relData%sigRxy = sigRxy
relData%sigRxz = sigRxz
relData%sigRyz = sigRyz

relData%momentum = wmom
relData%buoyancy = buoy

IF( rel_param(REL_MMD_INDX)/= NOT_SET_R )THEN
  relData%MMD   = rel_param(REL_MMD_INDX)
  relData%sigma = rel_param(REL_SIGMA_INDX)
ELSE
  relData%MMD   = NOT_SET_R
  relData%sigma = NOT_SET_R
END IF

IF( rel_param(REL_RAND_INDX) > 0 )THEN
  relData%nRandom    = Real2Int(rel_param(REL_RAND_INDX))
  relData%ranSeed    = Real2Int(rel_param(REL_SEED_INDX))
  relData%ranSpreadA = rel_param(REL_SPREAD_INDX)
  relData%ranSpreadT = rel_param(REL_SPREADT_INDX)
  relData%ranSpreadV = rel_param(REL_SPREADV_INDX)
  relData%ranDir     = rel_param(REL_RANDIR_INDX)
ELSE
  relData%nRandom    = NOT_SET_I
  relData%ranSeed    = NOT_SET_I
  relData%ranSpreadA = NOT_SET_R
  relData%ranSpreadT = NOT_SET_R
  relData%ranSpreadV = NOT_SET_R
  relData%ranDir     = NOT_SET_R
END IF

relData%activeFrac = rel_param(REL_AFRAC_INDX)
relData%dryFrac = rel_param(REL_WMFRAC_INDX)
relData%padding = NOT_SET_I

relDataOut = TRANSFER(relData,relDataOut)

RETURN
END
!*******************************************************************************
!        UnloadReleaseMove
!*******************************************************************************
SUBROUTINE UnloadReleaseMove( relDataIn )

USE convert_fd
USE release_fd
USE relparam_fd
USE scipuff_fi

!     Load SCIPUFF commons from an SCIP Release structure

IMPLICIT NONE

TYPE( relGenT ), INTENT ( IN ) :: relDataIn

TYPE( relMoveT ) relData

REAL, EXTERNAL :: ScaleReal

!==== Unload

relData = TRANSFER(relDataIn,relData)

subgroup = relData%distribution

cmass = relData%rate
tdur  = ScaleReal(relData%duration,HCF_HOUR2SEC)

sigx     = 0.0
sigy     = relData%sigY
sigz     = relData%sigZ
size_rel = NOT_SET_R

urel  = relData%velX
vrel  = relData%velY
wrel  = relData%velZ

umom = 0.0
vmom = 0.0
IF( relData%momentum /= NOT_SET_R )wmom = relData%momentum
IF( relData%buoyancy /= NOT_SET_R )buoy = relData%buoyancy

IF( relData%distribution == HD_LOGNORM )THEN
  rel_param(REL_MMD_INDX)   = relData%MMD
  rel_param(REL_SIGMA_INDX) = relData%sigma
END IF

rel_param(REL_WMFRAC_INDX) = relData%dryFrac
rel_param(REL_AFRAC_INDX)  = relData%activeFrac

RETURN
END
!*******************************************************************************
!        LoadReleaseMove
!*******************************************************************************
SUBROUTINE LoadReleaseMove( relDataOut )

USE convert_fd
USE release_fd
USE relparam_fd
USE scipuff_fi

!     Load an SCIP Release structure from SCIPUFF commons

IMPLICIT NONE

TYPE( relGenT ), INTENT( OUT ) :: relDataOut

TYPE( relMoveT ) relData

REAL, EXTERNAL :: ScaleReal

!==== Load

relData%distribution = subgroup

relData%rate     = cmass
relData%duration = ScaleReal( tdur,HCF_SEC2HOUR )

IF( size_rel > 0 )THEN
  relData%sigY = size_rel
  relData%sigZ = size_rel
ELSE
  relData%sigY = sigy
  relData%sigZ = sigz
END IF

relData%velX = urel
relData%velY = vrel
relData%velZ = wrel

relData%momentum = wmom
relData%buoyancy = buoy

IF( rel_param(REL_MMD_INDX)/= NOT_SET_R )THEN
  relData%MMD   = rel_param(REL_MMD_INDX)
  relData%sigma = rel_param(REL_SIGMA_INDX)
ELSE
  relData%MMD   = NOT_SET_R
  relData%sigma = NOT_SET_R
END IF

relData%activeFrac = rel_param(REL_AFRAC_INDX)
relData%dryFrac = rel_param(REL_WMFRAC_INDX)
relData%padding = NOT_SET_I

relDataOut = TRANSFER(relData,relDataOut)

RETURN
END
!*******************************************************************************
!        UnloadReleaseStack
!*******************************************************************************
SUBROUTINE UnloadReleaseStack( relDataIn )

USE convert_fd
USE release_fd
USE relparam_fd
USE scipuff_fi

!     Load SCIPUFF commons from an SCIP Release structure

IMPLICIT NONE

TYPE( relGenT ), INTENT ( IN ) :: relDataIn

TYPE( relStackT ) relData

REAL, EXTERNAL :: ScaleReal

!==== Unload

relData = TRANSFER(relDataIn,relData)

subgroup = relData%distribution

cmass = relData%rate
tdur  = ScaleReal( relData%duration,HCF_HOUR2SEC )

sigx     = NOT_SET_R
sigy     = NOT_SET_R
sigz     = NOT_SET_R
size_rel = relData%diameter

umom = DEF_VAL_R
vmom = DEF_VAL_R
IF( relData%exitVel  /= NOT_SET_R )wmom = relData%exitVel

IF( relData%exitTemp /= NOT_SET_R )buoy = relData%exitTemp

IF( relData%distribution == HD_LOGNORM )THEN
  rel_param(REL_MMD_INDX)   = relData%MMD
  rel_param(REL_SIGMA_INDX) = relData%sigma
END IF

rel_param(REL_WMFRAC_INDX) = relData%dryFrac
rel_param(REL_AFRAC_INDX)  = relData%activeFrac

RETURN
END
!*******************************************************************************
!        UnloadReleaseStack3
!*******************************************************************************
SUBROUTINE UnloadReleaseStack3( relDataIn )

USE convert_fd
USE release_fd
USE relparam_fd
USE scipuff_fi

!     Load SCIPUFF commons from an SCIP Release structure

IMPLICIT NONE

TYPE( relGenT ), INTENT ( IN ) :: relDataIn

TYPE( relStack3T ) relData

REAL, EXTERNAL :: ScaleReal

!==== Unload

relData = TRANSFER(relDataIn,relData)

subgroup = relData%distribution

cmass = relData%rate
tdur  = ScaleReal( relData%duration,HCF_HOUR2SEC )

sigx     = NOT_SET_R
sigy     = NOT_SET_R
sigz     = NOT_SET_R
size_rel = relData%diameter

IF( relData%exitVel(1) /= NOT_SET_R )umom = relData%exitVel(1)
IF( relData%exitVel(2) /= NOT_SET_R )vmom = relData%exitVel(2)
IF( relData%exitVel(3) /= NOT_SET_R )wmom = relData%exitVel(3)

IF( relData%exitTemp /= NOT_SET_R )buoy = relData%exitTemp

IF( relData%distribution == HD_LOGNORM )THEN
  rel_param(REL_MMD_INDX)   = relData%MMD
  rel_param(REL_SIGMA_INDX) = relData%sigma
END IF

rel_param(REL_WMFRAC_INDX) = relData%dryFrac
rel_param(REL_AFRAC_INDX)  = relData%activeFrac

RETURN
END
!*******************************************************************************
!        LoadReleaseStack
!*******************************************************************************
SUBROUTINE LoadReleaseStack( relDataOut )

USE convert_fd
USE release_fd
USE relparam_fd
USE scipuff_fi

!     Load an SCIP Release structure from SCIPUFF commons

IMPLICIT NONE

TYPE( relGenT ), INTENT( OUT ) :: relDataOut

TYPE( relStackT ) relData

REAL, EXTERNAL :: ScaleReal

!==== Load

relData%distribution = subgroup

relData%rate     = cmass
relData%duration = ScaleReal( tdur,HCF_SEC2HOUR )

relData%diameter = size_rel

relData%exitVel  = wmom

relData%exitTemp = buoy

IF( rel_param(REL_MMD_INDX)/= NOT_SET_R )THEN
  relData%MMD   = rel_param(REL_MMD_INDX)
  relData%sigma = rel_param(REL_SIGMA_INDX)
ELSE
  relData%MMD   = NOT_SET_R
  relData%sigma = NOT_SET_R
END IF

relData%activeFrac = rel_param(REL_AFRAC_INDX)
relData%dryFrac = rel_param(REL_WMFRAC_INDX)
relData%padding = NOT_SET_I

relDataOut = TRANSFER(relData,relDataOut)

RETURN
END
!*******************************************************************************
!        LoadReleaseStack3
!*******************************************************************************
SUBROUTINE LoadReleaseStack3( relDataOut )

USE convert_fd
USE release_fd
USE relparam_fd
USE scipuff_fi

!     Load an SCIP Release structure from SCIPUFF commons

IMPLICIT NONE

TYPE( relGenT ), INTENT( OUT ) :: relDataOut

TYPE( relStack3T ) relData

REAL, EXTERNAL :: ScaleReal

!==== Load

relData%distribution = subgroup

relData%rate     = cmass
relData%duration = ScaleReal( tdur,HCF_SEC2HOUR )

relData%diameter = size_rel

relData%exitVel(1) = umom
relData%exitVel(2) = vmom
relData%exitVel(3) = wmom

relData%exitTemp = buoy

IF( rel_param(REL_MMD_INDX)/= NOT_SET_R )THEN
  relData%MMD   = rel_param(REL_MMD_INDX)
  relData%sigma = rel_param(REL_SIGMA_INDX)
ELSE
  relData%MMD   = NOT_SET_R
  relData%sigma = NOT_SET_R
END IF

relData%activeFrac = rel_param(REL_AFRAC_INDX)
relData%dryFrac = rel_param(REL_WMFRAC_INDX)
relData%padding = NOT_SET_I

relDataOut = TRANSFER(relData,relDataOut)

RETURN
END
!*******************************************************************************
!        UnloadReleasePool
!*******************************************************************************
SUBROUTINE UnloadReleasePool( relDataIn )

USE release_fd
USE scipuff_fi

!     Load SCIPUFF commons from an SCIP Release structure

IMPLICIT NONE

TYPE( relGenT ), INTENT( IN ) :: relDataIn

TYPE( relPoolT ) relData

!==== Unload

relData = TRANSFER(relDataIn,relData)

cmass = relData%mass
tdur  = DEF_VAL_R          !SCIPUFF expects this
sigx  = relData%sizeX
sigy  = relData%sizeY
sigz  = 0.0                !SCIPUFF expects this
size_rel = NOT_SET_R

RETURN
END
!*******************************************************************************
!        LoadReleasePool
!*******************************************************************************
SUBROUTINE LoadReleasePool( relDataOut )

USE release_fd
USE scipuff_fi

!     Load an SCIP Release structure from SCIPUFF commons

IMPLICIT NONE

TYPE( relGenT ), INTENT( OUT ) :: relDataOut

TYPE( relPoolT ) relData

!==== Load

relData%mass  = cmass

IF( size_rel > 0 )THEN
  relData%sizeX = size_rel
  relData%sizeY = size_rel
ELSE
  relData%sizeX = sigx
  relData%sizeY = sigy
END IF

relData%padding = NOT_SET_I

relDataOut = TRANSFER(relData,relDataOut)

RETURN
END
!*******************************************************************************
!        UnloadReleaseFile
!*******************************************************************************
SUBROUTINE UnloadReleaseFile( relDataIn )

USE release_fd
USE relparam_fd
USE scipuff_fi

!     Load SCIPUFF commons from an SCIP Release structure

IMPLICIT NONE

TYPE( relGenT ), INTENT( IN) :: relDataIn

TYPE( relFileT ) relData

REAL,                      EXTERNAL :: Int2Real
CHARACTER(PATH_MAXLENGTH), EXTERNAL :: StripNull

!==== Unload

relData = TRANSFER(relDataIn,relData)

IF( relData%nRandom > 0 )THEN
  rel_param(REL_RAND_INDX)   = Int2Real( relData%nRandom )
  rel_param(REL_SEED_INDX)   = Int2Real( relData%ranSeed )
  rel_param(REL_SPREAD_INDX) = relData%ranSpread
ELSE
  rel_param(REL_RAND_INDX)   = NOT_SET_R
  rel_param(REL_SEED_INDX)   = NOT_SET_R
  rel_param(REL_SPREAD_INDX) = NOT_SET_R
END IF

rel_param(REL_SPREADT_INDX) = NOT_SET_R
rel_param(REL_SPREADV_INDX) = NOT_SET_R
rel_param(REL_RANDIR_INDX)  = NOT_SET_R

name_rel = TRIM(StripNull(relData%relFile))

RETURN
END
!*******************************************************************************
!        LoadReleaseFile
!*******************************************************************************
SUBROUTINE LoadReleaseFile( relDataOut )

USE release_fd
USE relparam_fd
USE scipuff_fi

!     Load an SCIP Release structure from SCIPUFF commons

IMPLICIT NONE

TYPE( relGenT ), INTENT( OUT ) :: relDataOut

TYPE( relFileT ) relData

INTEGER, EXTERNAL :: Real2Int

!==== Load

IF( rel_param(REL_RAND_INDX) > 0 )THEN
  relData%nRandom   = Real2Int( rel_param(REL_RAND_INDX) )
  relData%ranSeed   = Real2Int( rel_param(REL_SEED_INDX) )
  relData%ranSpread = rel_param(REL_SPREAD_INDX)
ELSE
  relData%nRandom   = NOT_SET_I
  relData%ranSeed   = NOT_SET_I
  relData%ranSpread = NOT_SET_R
END IF

relData%padding = NOT_SET_I

relData%relFile = TRIM(name_rel)

relDataOut = TRANSFER(relData,relDataOut)

RETURN
END
!*******************************************************************************
!        UnloadReleasePuff
!*******************************************************************************
SUBROUTINE UnloadReleasePuff( relDataIn )

USE release_fd
USE scipuff_fi
USE UtilMtlAux

!     Load SCIPUFF commons from an SCIP Release structure

IMPLICIT NONE

TYPE( relGenT ), INTENT( IN ) :: relDataIn

TYPE( relPuffT ) relData

INTEGER ityp
INTEGER imat

TYPE( puff_material )   matp
TYPE( liquid_material ) matl

LOGICAL, EXTERNAL :: IsGas
LOGICAL, EXTERNAL :: IsLiquid
LOGICAL, EXTERNAL :: IsWetParticle

!==== Unload

relData = TRANSFER(relDataIn,relData)

rel_param = NOT_SET_R

CALL zero_puff( puffRelease )

puffRelease%xbar = SNGL(xrel)
puffRelease%ybar = SNGL(yrel)
puffRelease%zbar = zrel

puffRelease%sxx = relData%sxx
puffRelease%sxy = relData%sxy
puffRelease%sxz = relData%sxz
puffRelease%syy = relData%syy
puffRelease%syz = relData%syz
puffRelease%szz = relData%szz

puffRelease%c  = relData%mass
puffRelease%cc = 1.0 + relData%sigRatio**2

puffRelease%xuc = relData%mass*relData%difhLSVxx
puffRelease%xvc = relData%mass*relData%difhLSVxy
puffRelease%yvc = relData%mass*relData%difhLSVyy

puffRelease%yvsc = relData%mass*relData%difhShear
puffRelease%yvbc = relData%mass*relData%difhBuoy

puffRelease%zwc = relData%mass*relData%difVert

puffRelease%si  = relData%scaleLateral
puffRelease%si2 = relData%scaleStream
puffRelease%sv  = relData%scaleVert

puffRelease%cfo = relData%activeFrac

CALL set_rel_type( relmat,relData%subgroup,ityp,rel_dist )
IF( nError /= NO_ERROR )GOTO 9999

puffRelease%ityp = ityp

IF( rel_dist > 0 )THEN
  nError = IV_ERROR
  eRoutine = 'UnloadReleasePuff'
  eMessage = 'Invalid release distribution'
  eInform  = 'Not allowed for PUFF releases'
  GOTO 9999
END IF

umom = 0.0
vmom = 0.0
wmom = 0.0
buoy = 0.0

IF( dynamic )THEN
  IF( IsGas(typeID(ityp)%icls) )THEN
    CALL siginv( puffRelease )
    wmom = relData%wDynamic*PI3*SQRT(puffRelease%det)
    buoy = relData%tDynamic*PI3*SQRT(puffRelease%det)
  END IF
END IF

IF( IsLiquid(typeID(ityp)%icls) .OR. IsWetParticle(typeID(ityp)%icls) )THEN
  IF( relData%dropDiam == NOT_SET_R )THEN

    puffRelLiquid%d     = NOT_SET_R
    puffRelLiquid%sigd  = NOT_SET_R
    puffRelLiquid%t     = NOT_SET_R
    puffRelLiquid%ccs   = NOT_SET_R
    puffRelLiquid%tevap = NOT_SET_R

  ELSE

    puffRelLiquid%d     = relData%dropDiam
    puffRelLiquid%sigd  = relData%dropSigD
    puffRelLiquid%t     = relData%dropTemp
    puffRelLiquid%ccs   = puffRelease%c
    puffRelLiquid%tevap = 0.0

    IF( puffRelLiquid%d <= 0.0 )THEN
      nError = IV_ERROR
      eRoutine = 'UnloadReleasePuff'
      eMessage = 'Invalid liquid release droplet size'
      eInform  = 'Must be greater than zero'
      GOTO 9999
    END IF

!-----  Set correct size bin

    imat = typeID(ityp)%imat
    ityp = material(imat)%ioffp + GetSubgroups( material(imat),mat_aux ) + 1
    CALL get_puff_material( ityp,matp )
    matl = TRANSFER(matp,matl)
    DO WHILE( puffRelLiquid%d < matl%dmin )
      ityp = ityp - 1
      CALL get_puff_material( ityp,matp )
      matl = TRANSFER(matp,matl)
    END DO

    puffRelease%ityp = ityp

  END IF

END IF

9999 CONTINUE

RETURN
END
!*******************************************************************************
!        LoadReleasePuff
!*******************************************************************************
SUBROUTINE LoadReleasePuff( relDataOut )

USE release_fd
USE scipuff_fi
USE UtilMtlAux

!     Load SCIPUFF commons from an SCIP Release structure

IMPLICIT NONE

TYPE( relGenT ), INTENT( OUT ) :: relDataOut

TYPE( relPuffT ) relData

INTEGER icls

LOGICAL, EXTERNAL :: IsGas
LOGICAL, EXTERNAL :: IsLiquid
LOGICAL, EXTERNAL :: IsWetParticle

!==== Load

relData%sxx = puffRelease%sxx
relData%sxy = puffRelease%sxy
relData%sxz = puffRelease%sxz
relData%syy = puffRelease%syy
relData%syz = puffRelease%syz
relData%szz = puffRelease%szz

relData%mass = puffRelease%c
relData%sigRatio = SQRT(puffRelease%cc - 1.0)

relData%difhLSVxx = puffRelease%xuc/relData%mass
relData%difhLSVxy = puffRelease%xvc/relData%mass
relData%difhLSVyy = puffRelease%yvc/relData%mass

relData%difhShear = puffRelease%yvsc/relData%mass
relData%difhBuoy  = puffRelease%yvbc/relData%mass

relData%difVert = puffRelease%zwc/relData%mass

relData%scaleLateral = puffRelease%si
relData%scaleStream  = puffRelease%si2
relData%scaleVert    = puffRelease%sv

relData%activeFrac = puffRelease%cfo

relData%subgroup = typeID(puffRelease%ityp)%igrp

icls = typeID(puffRelease%ityp)%icls

relData%wDynamic = 0.0
relData%tDynamic = 0.0

IF( dynamic )THEN
  IF( IsGas(icls) )THEN
    CALL siginv( puffRelease )
    relData%wDynamic = wmom/(PI3*SQRT(puffRelease%det))
    relData%tDynamic = buoy/(PI3*SQRT(puffRelease%det))
  END IF
END IF

IF( IsLiquid(icls) .OR. IsWetParticle(icls) )THEN
  IF( puffRelLiquid%d == NOT_SET_R )THEN

    relData%dropDiam = NOT_SET_R
    relData%dropSigD = NOT_SET_R
    relData%dropTemp = NOT_SET_R

  ELSE

    relData%dropDiam = puffRelLiquid%d
    relData%dropSigD = puffRelLiquid%sigd
    relData%dropTemp = puffRelLiquid%t

  END IF

END IF

relDataOut = TRANSFER(relData,relDataOut)

RETURN
END
