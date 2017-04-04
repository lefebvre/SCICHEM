!*******************************************************************************
!$RCSfile$
!$Revision$
!$Date$
!*******************************************************************************
INTEGER FUNCTION SWIMinitObsAssim( fld )

!------ Setup obs structures for use in assimilation into existing met field

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

TYPE( MetField ), INTENT( INOUT ) :: fld

INTEGER irv, i, jObs, nxy, nxyz
LOGICAL lRifl

INTEGER, EXTERNAL :: SWIMallocObsWt, SWIMalloc3dField, SWIMallocBLParam

SWIMinitObsAssim = SWIMfailure

!------ Check for consistency of time reference

DO i = 1,fld%nObsSource
  jObs = fld%iObsSource(i)
  IF( ObsSrc(jObs)%local .NEQV. Prj%localMet )THEN
    error%Number  = IV_ERROR
    error%Routine = 'SWIMinitObsAssim'
    error%Message = 'Inconsistent time references (Local or UTC) in obs files'
    GOTO 9999
  END IF
END DO

!------ Check for use of influence radius

lRifl = .TRUE.
DO i = 1,fld%nObsSource
  jObs = fld%iObsSource(i)
  IF( .NOT.BTEST(ObsSrc(jObs)%type,OTB_RIFL) )lRifl = .FALSE.
END DO

IF(.NOT.lRifl )THEN
  DO i = 1,fld%nObsSource
    jObs = fld%iObsSource(i)
    ObsSrc(jObs)%type = IBCLR(ObsSrc(jObs)%type,OTB_RIFL)
  END DO
END IF

!------ Allocate gridded fields that will be saved for temporal interpolation

nxy = fld%grid%nXY

IF( BTEST(fld%grid%type,GTB_STAGGERZ) )THEN
  nxyz = nxy*(fld%grid%nZ+1)
ELSE
  nxyz = nxy*fld%grid%nZ
END IF

irv = SWIMalloc3dField( fld%type,nxyz,fld%Field1 )
IF( irv /= SWIMsuccess )GOTO 9999

irv = SWIMalloc3dField( fld%type,nxyz,fld%Field2 )
IF( irv /= SWIMsuccess )GOTO 9999

irv = SWIMallocBLParam( fld%type,nxy,fld%BL1,fld%BL2 )
IF( irv /= SWIMsuccess )GOTO 9999

!----- Allocate for obs weights
!      N.B. this is based on fld%type, not necessarily obs

irv = SWIMallocObsWt( fld%type,nxy,nxyz,fld%obsWt )
IF( irv /= SWIMsuccess )GOTO 9999

!----- Set 'first' status

fld%status = IBSET(fld%status,FSB_FIRSTASSM)

SWIMinitObsAssim = SWIMresult

9999 CONTINUE

RETURN
END


