!*******************************************************************************
!$RCSfile$
!$Revision$
!$Date$
!*******************************************************************************
INTEGER FUNCTION SWIMnextField( t,fld )

USE SWIM_fi
USE SWIMparam_fd
USE message_fd

IMPLICIT NONE

REAL, INTENT( IN )                :: t
TYPE( MetField ), INTENT( INOUT ) :: fld

INTEGER irv, iflag
INTEGER hour, min, sec, year, month, day
LOGICAL lymd

CHARACTER(128) string

TYPE ( messageT ) caution

INTERFACE
  INTEGER FUNCTION SWIMupdateSmoothField( fldi,iflag )
    USE SWIMmetField_fd
    TYPE( MetField ), TARGET, INTENT( INOUT ) :: fldi
    INTEGER,                  INTENT( IN    ) :: iflag
  END FUNCTION SWIMupdateSmoothField
  INTEGER FUNCTION SWIMMcWIF( grid,meanField )
    USE SWIMmetField_fd
    TYPE( MetGrid   ), TARGET, INTENT( INOUT ) :: grid
    TYPE( MetMean3D ),         INTENT( INOUT ) :: meanField
  END FUNCTION SWIMMcWIF
END INTERFACE

INTEGER, EXTERNAL :: SWIMupdateBL, SWIMinterpTime
INTEGER, EXTERNAL :: SWIMupdateObsField, SWIMupdateGriddedMet
INTEGER, EXTERNAL :: SWIMupdatePolarField, SWIMshearGrad
INTEGER, EXTERNAL :: SWIMinterpNest, CheckMet
INTEGER, EXTERNAL :: SWIMinterpTimeAssm
INTEGER, EXTERNAL :: PostCautionMessage

SWIMnextField = SWIMfailure

!------ Make sure calculated BL is set for current fields before next
!       fields are moved (unless initializing)

IF( fld%BLtype == BLP_CALC .AND. .NOT.BTEST(fld%status,FSB_DOINITBL) )THEN
  irv = SWIMupdateBL( fld%tNext,fld )
  IF( irv /= SWIMsuccess )GOTO 9999
END IF

!------ Move fields at next time to current time (unless initializing)

IF( .NOT.BTEST(fld%status,FSB_DOINIT) )THEN
  IF( fld%tNext < fld%t )THEN
    error%Number  = IV_ERROR
    error%Routine = 'SWIMnextField'
    error%Message = 'Time on meteorological input is non-increasing'
    GOTO 9999
  END IF
  fld%t = fld%tNext
  irv = SWIMinterpTime( 0.,fld )
  fld%status = IBCLR(fld%status,FSB_FIRST)
END IF

!------ Get next field

IF( BTEST(fld%type,FTB_NEST) )THEN        !Interpolated onto nested domain

  irv = SWIMinterpNest( fld )
  IF( irv /= SWIMsuccess )GOTO 9999

ELSE IF( BTEST(fld%type,FTB_OBS) )THEN    !Field based on observations

  irv = SWIMupdateObsField( t,fld )
  IF( irv /= SWIMsuccess )GOTO 9999

ELSE IF( BTEST(fld%type,FTB_SMOOTH) )THEN  !Averaged onto coarse grid

  iflag = 2
  irv = SWIMupdateSmoothField( fld,iflag )
  IF( irv /= SWIMsuccess )GOTO 9999

ELSE IF( BTEST(fld%type,FTB_NPOLE) .OR. &  !Averaged from North/South boundaries
         BTEST(fld%type,FTB_SPOLE) )THEN   !of global fields

  irv = SWIMupdatePolarField( fld )
  IF( irv /= SWIMsuccess )GOTO 9999

ELSE                                       !Gridded met

  irv = SWIMupdateGriddedMet( t,fld )
  IF( irv /= SWIMsuccess )GOTO 9999

END IF

!------ Obs assimilation

IF( fld%nObsSource > 0 .AND. .NOT.BTEST(fld%type,FTB_OBS) )THEN

  IF( BTEST(fld%status,FSB_DOINIT) )THEN
    irv = SWIMinterpTimeAssm( 0.,fld )
    fld%t1 = fld%t2
  END IF

  irv = SWIMupdateObsField( t,fld )
  IF( irv /= SWIMsuccess )GOTO 9999

END IF

!------- Mass-consistent adjustment if requested

IF( BTEST(fld%status,FSB_UPDATE) )THEN

  IF( BTEST(fld%type,FTB_MCWIF) )THEN

    irv = SWIMMcWIF( fld%grid,fld%NextField )
    IF( irv /= SWIMsuccess )GOTO 9999

  END IF

END IF

!------ Check met for unrealistic temperature & velocity

irv = CheckMet( fld%NextField,fld%grid )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Compute measure of velocity gradient inhomogeneity

IF( BTEST(fld%type,FTB_DU2) )THEN
  IF( BTEST(fld%status,FSB_UPDATE) )THEN
    irv = SWIMshearGrad( fld%NextField,fld%grid )
    IF( irv /= SWIMsuccess )GOTO 9999
  END IF
END IF

!------ Move 'next' field into 'current' field for initialization

IF( BTEST(fld%status,FSB_DOINIT) )THEN

  IF( fld%tNext > t + 900. )THEN
    caution%iParm = 0; caution%jParm = 0; caution%routine = 'SWIMnextField'
    caution%aString = 'First meteorology time is after calculation start'
    lymd = .NOT.( Prj%julStart == 0 .OR. Prj%julStart == NOT_SET_I )
    CALL TimeConvert( fld%tNext,Prj%local,lymd,hour,min,sec,year,month,day,string )
    caution%bString = 'Met   time: '//string
    CALL TimeConvert( t,Prj%local,lymd,hour,min,sec,year,month,day,string )
    caution%cString = 'Start time: '//string
    irv = PostCautionMessage( caution )
  END IF

  irv = SWIMinterpTime( 0.,fld )

  fld%t      = fld%tNext
  fld%status = IBCLR(fld%status,FSB_DOINIT)

  IF( fld%tNext > t )fld%status = IBSET(fld%status,FSB_FIRST)
  fld%status = IBSET(fld%status,FSB_SECOND)

ELSE

  IF( BTEST(fld%status,FSB_SECOND) )THEN
    IF( fld%tNext < t - 900. .AND. .NOT.BTEST(fld%status,FSB_UPDATE) )THEN
      caution%iParm = 0; caution%jParm = 0; caution%routine = 'SWIMnextField'
      caution%aString = 'Last meteorology time is before calculation start'
      lymd = .NOT.( Prj%julStart == 0 .OR. Prj%julStart == NOT_SET_I )
      CALL TimeConvert(  fld%tNext,Prj%local,lymd,hour,min,sec,year,month,day,string )
      caution%bString = 'Met   time: '//string
      CALL TimeConvert( t,Prj%local,lymd,hour,min,sec,year,month,day,string )
      caution%cString = 'Start time: '//string
      irv = PostCautionMessage( caution )
    END IF
    fld%status = IBCLR(fld%status,FSB_SECOND)
  END IF

END IF

SWIMnextField = SWIMresult

9999 CONTINUE

RETURN
END
