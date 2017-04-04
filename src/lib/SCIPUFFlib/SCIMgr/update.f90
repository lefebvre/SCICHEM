!***********************************************************************
!               UpdateRelease
!***********************************************************************
FUNCTION UpdateRelease( oldSpec, tstep ) RESULT( newSpec )

USE release_fd
USE SciMgr_fd
USE files_fi
USE error_fi
USE SciMgr_fi
USE time_fi
USE SCIMgrState

IMPLICIT NONE

REAL, PARAMETER :: EPS_TIME = 0.0001   ! ~0.5 second in hours

TYPE( releaseSpecT ), INTENT( IN ) :: oldSpec
REAL,                 INTENT( IN ) :: tstep      ! end time for current large timestep

TYPE( releaseT )               :: newRelease
TYPE( releaseSpecT )           ::newSpec

INTEGER currentState
INTEGER waitState
INTEGER irv

TYPE( updateRelT ) update
TYPE( releaseT     ) oldRelease
TYPE( updateRelMCT ) updateMC

INTEGER, EXTERNAL :: Write_Release
INTEGER, EXTERNAL :: PostUpdateMessage
INTEGER, EXTERNAL :: PostUpdateMCMessage
LOGICAL, EXTERNAL :: IsValidData
LOGICAL, EXTERNAL :: IsFastMode
LOGICAL, EXTERNAL :: IsReverseMode
LOGICAL, EXTERNAL :: ReleaseCheck
LOGICAL, EXTERNAL :: SkipRelease
LOGICAL, EXTERNAL :: IsMultiUpdate
REAL   , EXTERNAL :: GetSCIPUFFtime

CHARACTER(128),EXTERNAL :: StripNull

CALL InitReleaseSpec(newSpec)

oldRelease = oldSpec%release
newRelease = oldRelease

IF( IsValidData(newRelease%status) )GOTO 9999

!------------------------------------------------------------------------------
! Set the update release
!------------------------------------------------------------------------------
update%release = newRelease

!------------------------------------------------------------------------------
! Set the update time relative to release time
!------------------------------------------------------------------------------
update%currentTime = t/3600.     - update%release%tRel
update%nextUpdate  = tstep/3600. - update%release%tRel

!------------------------------------------------------------------------------
! Get the Met
!------------------------------------------------------------------------------
CALL SetEnvironment( update%environment,SNGL(update%release%xRel),SNGL(update%release%yRel) )
IF( nError /= NO_ERROR )GOTO 9999

!------------------------------------------------------------------------------
! Set the update mode
!------------------------------------------------------------------------------
update%mode = 0
update%mode = IBSET(update%mode,HUB_ENVIRONMENT)
IF( IsFastMode() )update%mode = IBSET(update%mode,HFB_FAST)
IF( IsReverseMode() )update%mode = IBSET(update%mode,HFB_REVERSE)

!------------------------------------------------------------------------------
!Call the SCIPUFFServer function
!------------------------------------------------------------------------------
!==== Set tool state to WAIT

waitState    = IBSET( toolState,HSB_WAIT )
currentState = SCIMgrSetState( waitState )

!==== Make Callback

IF( IsMultiUpdate( oldSpec ) )THEN
  updateMC = TRANSFER( update,updateMC )
  irv = PostUpdateMCMessage( updateMC )
  CALL readUpdateFile( updateMC%updateSCN, newSpec )
  newRelease = newSpec%release
ELSE
  irv = PostUpdateMessage( update )
  newRelease = update%release
END IF

!==== Reset Tool state

waitState = SCIMgrSetState( currentState )

!==== Check Error

IF( irv /= SCIPsuccess )THEN
  SELECT CASE( newRelease%type )
    CASE( HR_CONTF,HR_STACKF,HR_STACK3F )
      eRoutine = 'UpdateContRel'
      nError   = IV_ERROR
      eMessage = 'Release '//TRIM(newRelease%relName)//' not VALID'
      eInform  = TRIM(update%release%relDisplay)
    CASE DEFAULT
  CALL SetMessageHandlerError( 'UpdateRelease' )
  END SELECT
  GOTO 9999
END IF

!------------------------------------------------------------------------------
! Remove null characters from release material
!------------------------------------------------------------------------------
newRelease%material = TRIM(StripNull(TRIM(newRelease%material)))
CALL cupper( newRelease%material )

!------------------------------------------------------------------------------
! Check to see if release was properly updated
!------------------------------------------------------------------------------
IF( .NOT.IsValidData(newRelease%status) )THEN
  nError   = IV_ERROR
  eRoutine = 'UpdateRelease'
  eMessage = 'Release status not VALID'
  eInform  = 'Updated release failed to fully validate the release data'
  GOTO 9999
END IF

IF( ABS(newRelease%trel - oldRelease%trel) > EPS_TIME )THEN
  nError   = IV_ERROR
  eRoutine = 'UpdateRelease'
  eMessage = 'Release time changed'
  eInform  = 'Updated release must be at the time originally specified'
  GOTO 9999
ELSE
  newRelease%trel = oldRelease%trel
END IF

IF( newRelease%type /= oldRelease%type )THEN
  nError   = IV_ERROR
  eRoutine = 'UpdateRelease'
  eMessage = 'Release type changed'
  eInform  = 'Updated release must of the same type as originally specified'
  GOTO 9999
END IF

IF( newRelease%material /= oldRelease%material )THEN
  nError   = IV_ERROR
  eRoutine = 'UpdateRelease'
  eMessage = 'Release material changed'
  eInform  = 'Updated release must specify the same material as originally specified'
  GOTO 9999
END IF

!------------------------------------------------------------------------------
! Check to see if release is set to be skipped
!------------------------------------------------------------------------------
IF( SkipRelease(newRelease) .OR. nError /= NO_ERROR )GOTO 9999

!------------------------------------------------------------------------------
! Check to see if release is properly defined
!------------------------------------------------------------------------------
IF( .NOT.ReleaseCheck(newRelease) )THEN
  eRoutine = 'UpdateRelease'
  GOTO 9999
END IF

!------------------------------------------------------------------------------
!Copy updated results back out
!------------------------------------------------------------------------------
CALL AdjustNextUpdtTime(newRelease,t/3600.,tstep/3600.0)
IF( .NOT.ReleaseCheck(newRelease) )THEN
  eRoutine = 'UpdateRelease'
  GOTO 9999
END IF

!------------------------------------------------------------------------------
!Rest the status switch
!------------------------------------------------------------------------------
newRelease%status = oldRelease%status
newSpec%release = newRelease

9999 CONTINUE

RETURN
END

!*******************************************************************************
! AdjustNextUpdtTime
!*******************************************************************************
SUBROUTINE AdjustNextUpdtTime( release,time,tstep )
USE default_fd
USE release_fd

!  Adjust nextUpdtTime from relative to Release time to runtime

IMPLICIT NONE

TYPE( releaseT ), INTENT( INOUT ) :: release
REAL,             INTENT( IN    ) :: time
REAL,             INTENT( IN    ) :: tstep

TYPE( relContT )   cData
TYPE( relMoveT )   mData
TYPE( relStackT )  sData
TYPE( relStack3T ) jData

SELECT CASE( release%type )
CASE( HR_CONT )
  cData = TRANSFER(release%relData,cData)
  IF( cData%nextUpdtTime /= DEF_VAL_R )THEN
    cData%nextUpdtTime = MIN(tstep,cData%nextUpdtTime + release%tRel)
    IF( (tstep - cData%nextUpdtTime)/(cData%nextUpdtTime - time) < 0.1 ) cData%nextUpdtTime = tstep
    release%relData = TRANSFER(cData,release%relData)
  END IF
CASE( HR_MOVE )
  mData = TRANSFER(release%relData,mData)
  IF( mData%nextUpdtTime /= DEF_VAL_R )THEN
    mData%nextUpdtTime = MIN(tstep,mData%nextUpdtTime + release%tRel)
    IF( (tstep - mData%nextUpdtTime)/(mData%nextUpdtTime - time) < 0.1 ) mData%nextUpdtTime = tstep
    release%relData = TRANSFER(mData,release%relData)
  END IF
CASE( HR_STACK )
  sData = TRANSFER(release%relData,sData)
  IF( sData%nextUpdtTime /= DEF_VAL_R )THEN
    sData%nextUpdtTime = MIN(tstep,sData%nextUpdtTime + release%tRel)
    IF( (tstep - sData%nextUpdtTime)/(sData%nextUpdtTime - time) < 0.1 ) sData%nextUpdtTime = tstep
    release%relData = TRANSFER(sData,release%relData)
  END IF
CASE( HR_STACK3 )
  jData = TRANSFER(release%relData,jData)
  IF( jData%nextUpdtTime /= DEF_VAL_R )THEN
    jData%nextUpdtTime = MIN(tstep,jData%nextUpdtTime + release%tRel)
    IF( (tstep - jData%nextUpdtTime)/(jData%nextUpdtTime - time) < 0.1 ) jData%nextUpdtTime = tstep
    release%relData = TRANSFER(jData,release%relData)
  END IF
CASE DEFAULT
END SELECT

RETURN
END

!*******************************************************************************
! GetNextUpdtTime
!*******************************************************************************
REAL FUNCTION GetNextUpdtTime( def ) RESULT( nextUpdtTime )
USE default_fd
USE release_fd
USE cont_rel_fd

!  get nextUpdtTime for this definition

IMPLICIT NONE

TYPE( cont_release_def ), INTENT( INOUT ) :: def

TYPE( relContT )   cData
TYPE( relMoveT )   mData
TYPE( relStackT )  sData
TYPE( relStack3T ) jData

nextUpdtTime = DEF_VAL_R

!IF( ASSOCIATED(def%relSpec) )THEN
  SELECT CASE( def%relSpec%release%type )
  CASE( HR_CONT )
    cData = TRANSFER(def%relSpec%release%relData,cData)
    nextUpdtTime = cData%nextUpdtTime
  CASE( HR_MOVE )
    mData = TRANSFER(def%relSpec%release%relData,mData)
    nextUpdtTime = mData%nextUpdtTime
  CASE( HR_STACK )
    sData = TRANSFER(def%relSpec%release%relData,sData)
    nextUpdtTime = sData%nextUpdtTime
  CASE( HR_STACK3 )
    jData = TRANSFER(def%relSpec%release%relData,jData)
    nextUpdtTime = jData%nextUpdtTime
  CASE DEFAULT
  END SELECT
!END IF

RETURN
END

!*******************************************************************************
! GetNextUpdtTimeLevel
!*******************************************************************************
INTEGER FUNCTION GetNextUpdtTimeLevel( relSpec, time ) RESULT( ilev )
USE default_fd
USE release_fd
USE error_fi
USE param_fd
USE cont_rel_fd
USE files_fi

!  get a timnelvel based on nextUpdtTime for this definition

IMPLICIT NONE

TYPE( releaseSpecT ), INTENT( IN ) :: relSpec
REAL,                 INTENT( IN ) :: time

TYPE( relContT )   cData
TYPE( relMoveT )   mData
TYPE( relStackT )  sData
TYPE( relStack3T ) jData

REAL nextUpdtTime, delta

INTEGER, EXTERNAL :: time_level

ilev = 0

nextUpdtTime = DEF_VAL_R

SELECT CASE( relSpec%release%type )
CASE( HR_CONT )
  cData = TRANSFER(relSpec%release%relData,cData)
  nextUpdtTime = cData%nextUpdtTime
CASE( HR_MOVE )
  mData = TRANSFER(relSpec%release%relData,mData)
  nextUpdtTime = mData%nextUpdtTime
CASE( HR_STACK )
  sData = TRANSFER(relSpec%release%relData,sData)
  nextUpdtTime = sData%nextUpdtTime
CASE( HR_STACK3 )
  jData = TRANSFER(relSpec%release%relData,jData)
  nextUpdtTime = jData%nextUpdtTime
CASE DEFAULT
END SELECT

IF( nextUpdtTime /= DEF_VAL_R )THEN
  delta = 3600.*(nextUpdtTime - MAX(time,relSpec%release%tRel))
  ilev = MIN(MAXTLV,time_level(delta))
  IF( nError /= NO_ERROR )THEN
    eRoutine = 'GetNextUpdtTimeLevel:'//TRIM(eRoutine)
    WRITE(lun_log,*,ERR=9999)'********* GetNextUpdtTimeLevel error ***********'
    WRITE(lun_log,*,ERR=9999)'type=',relSpec%release%type
    WRITE(lun_log,*,ERR=9999)'release=',relSpec%release%relName
    WRITE(lun_log,*,ERR=9999)'nextUpdtTime=',nextUpdtTime
    WRITE(lun_log,*,ERR=9999)'tRel=',relSpec%release%tRel
    WRITE(lun_log,*,ERR=9999)'time=',time
    WRITE(lun_log,*,ERR=9999)'delta=',delta
    GOTO 9999
  END IF
END IF

9999 CONTINUE

RETURN
END

!*******************************************************************************
! extraUpdateDefinition
!*******************************************************************************
INTEGER FUNCTION extraUpdateDefinition() RESULT( num )

USE cont_rel_fi
USE error_fi
USE default_fd

IMPLICIT NONE

INTEGER idef

REAL, EXTERNAL :: GetNextUpdtTime

num = 0
DO idef = 1,numDefinition
  IF( (cDefinition(idef)%state /= CR_EMPTY) .AND. (GetNextUpdtTime(cDefinition(idef)) /= DEF_VAL_R) )THEN  !Not completed and non-Standard update
    num = num + 1
  END IF
END DO

RETURN
END FUNCTION

LOGICAL FUNCTION IsMultiUpdate( spec ) RESULT( answer )

USE scipuff_fi

IMPLICIT NONE

TYPE( releaseSpecT ), INTENT( IN ) :: spec

INTEGER i, imat, icls
CHARACTER(16) mat

LOGICAL, EXTERNAL :: IsMulti

answer = .FALSE.

icls = NOT_SET_I
IF( spec%ityp == NOT_SET_I )THEN
  imat  = 0

  mat = TRIM(spec%release%material)
  CALL cupper( mat )

  DO i = 1,ntypm
    IF( mat == material(i)%cmat )imat = i
  END DO

  IF( imat == 0 )THEN
    nError   = IV_ERROR
    eMessage = 'Unknown release material'
    eRoutine = 'IsMultiUpdate'
    GOTO 9999
  END IF

  icls = material(imat)%icls
ELSE
  icls = typeID(spec%ityp)%icls
END IF

IF( IsMulti(icls) )THEN
  answer = .NOT.(BTEST(spec%release%type,HRB_POOL) .OR. BTEST(spec%release%type,HRB_FILE))
END IF

9999 CONTINUE

RETURN
END

SUBROUTINE readUpdateFile( file, relSpec )

USE scipuff_fi
USE files_fi

IMPLICIT NONE

CHARACTER(*)        , INTENT( IN    ) :: file
TYPE( releaseSpecT ), INTENT( INOUT ) :: relSpec

INTEGER ios

OPEN(UNIT=lun_tmp,FILE=file,STATUS='OLD',ACTION="READ",IOSTAT=ios)
IF( ios /= 0 )THEN
  nError   = OP_ERROR
  eRoutine = 'readUpdateFile'
  eMessage = 'Error opening SCIPUFF release scenario update file'
  CALL ReportFileName( eInform,'File=',file )
  eAction  = 'Make sure file exists'
  GOTO 9999
END IF

relSpec%ityp = NOT_SET_I
relSpec%distrib = NOT_SET_I
relSpec%release%trel = NOT_SET_R
CALL ClearMCrelList(relSpec%MClist)

CALL ReadNamelistScn( lun_tmp, relSpec )
IF( nError /= NO_ERROR )GOTO 9999

9999 CONTINUE

CLOSE(UNIT=lun_tmp,IOSTAT=ios)

RETURN
END
