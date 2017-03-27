!***********************************************************************
!               UpdateRelease
!***********************************************************************
FUNCTION UpdateRelease( oldRelease ) RESULT( newRelease )

USE release_fd
USE SciMgr_fd
USE files_fi
USE error_fi
USE SciMgr_fi
USE time_fi
USE SCIMgrState

IMPLICIT NONE

REAL, PARAMETER :: EPS_TIME = 0.0001   ! ~0.5 second in hours

TYPE( releaseT ), INTENT( IN ) :: oldRelease
TYPE( releaseT )               :: newRelease

INTEGER currentState
INTEGER waitState
INTEGER irv

TYPE( updateRelT ) update

INTEGER, EXTERNAL :: Write_Release
INTEGER, EXTERNAL :: PostUpdateMessage
LOGICAL, EXTERNAL :: IsValidData
LOGICAL, EXTERNAL :: IsFastMode
LOGICAL, EXTERNAL :: IsReverseMode
LOGICAL, EXTERNAL :: ReleaseCheck
LOGICAL, EXTERNAL :: SkipRelease
REAL   , EXTERNAL :: GetSCIPUFFtime

CHARACTER(128),EXTERNAL :: StripNull

newRelease = oldRelease

IF( IsValidData(newRelease%status) )GOTO 9999

!------------------------------------------------------------------------------
! Set the update release
!------------------------------------------------------------------------------
update%release = newRelease

!------------------------------------------------------------------------------
! Set the update time relative to release time
!------------------------------------------------------------------------------
update%currentTime = t/3600.         - update%release%tRel
update%nextUpdate  = (t+delt)/3600.  - update%release%tRel

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

irv = PostUpdateMessage( update )

!==== Reset Tool state

waitState = SCIMgrSetState( currentState )

!==== Check Error

IF( irv /= SCIPsuccess )THEN
  CALL SetMessageHandlerError( 'UpdateRelease' )
  GOTO 9999
END IF

!------------------------------------------------------------------------------
! Remove null characters from release material
!------------------------------------------------------------------------------
update%release%material = TRIM(StripNull(TRIM(update%release%material)))
CALL cupper( update%release%material )

!------------------------------------------------------------------------------
! Check to see if release was properly updated
!------------------------------------------------------------------------------
IF( .NOT.IsValidData(update%release%status) )THEN
  nError   = IV_ERROR
  eRoutine = 'UpdateRelease'
  eMessage = 'Release status not VALID'
  eInform  = 'Updated release failed to fully validate the release data'
  GOTO 9999
END IF

IF( ABS(update%release%trel - oldRelease%trel) > EPS_TIME )THEN
  nError   = IV_ERROR
  eRoutine = 'UpdateRelease'
  eMessage = 'Release time changed'
  eInform  = 'Updated release must be at the time originally specified'
  GOTO 9999
ELSE
  update%release%trel = oldRelease%trel
END IF

IF( update%release%type /= oldRelease%type )THEN
  nError   = IV_ERROR
  eRoutine = 'UpdateRelease'
  eMessage = 'Release type changed'
  eInform  = 'Updated release must of the same type as originally specified'
  GOTO 9999
END IF

IF( update%release%material /= oldRelease%material )THEN
  nError   = IV_ERROR
  eRoutine = 'UpdateRelease'
  eMessage = 'Release material changed'
  eInform  = 'Updated release must specify the same material as originally specified'
  GOTO 9999
END IF

!------------------------------------------------------------------------------
! Check to see if release is set to be skipped
!------------------------------------------------------------------------------
IF( SkipRelease(update%release) .OR. nError /= NO_ERROR )GOTO 9999

!------------------------------------------------------------------------------
! Check to see if release is properly defined
!------------------------------------------------------------------------------
IF( .NOT.ReleaseCheck(update%release) )THEN
  eRoutine = 'UpdateRelease'
  GOTO 9999
END IF

!------------------------------------------------------------------------------
!Copy updated results back out
!------------------------------------------------------------------------------
newRelease = update%release
IF( .NOT.ReleaseCheck(newRelease) )THEN
  eRoutine = 'UpdateRelease'
  GOTO 9999
END IF

!------------------------------------------------------------------------------
!Rest the status switch
!------------------------------------------------------------------------------
newRelease%status = oldRelease%status

9999 CONTINUE

RETURN
END
