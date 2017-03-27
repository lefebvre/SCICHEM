!*******************************************************************************
!$RCSfile$
!$Revision$
!$Date$
!*******************************************************************************
INTEGER FUNCTION UpdateStackEmission( update )

USE SCIPUFFdriver_fi
USE update_fd
USE constants_fd

IMPLICIT NONE

TYPE( updateRelT ), INTENT( INOUT ) :: update

TYPE( relStackT ) :: relData

INTEGER irv, i
REAL    t, rate, ratm, r1, r2, r3, tdur

CHARACTER(32) relName

REAL, DIMENSION(:), ALLOCATABLE :: rMC

INTEGER, EXTERNAL :: ReadNextEmission, FindRelName

UpdateStackEmission = FAILURE

IF( .NOT.lReadEmission )THEN
  UpdateStackEmission = SUCCESS
  GOTO 9999
END IF

i = INDEX(update%release%relName,':')
IF( i == 0 )THEN
  relName = TRIM(update%release%relName)
ELSE
  relName = TRIM(update%release%relName(1:i-1))
END IF
i = FindRelName( relName )

IF( i == 0 )GOTO 9999

IF( nMC > 0 )THEN
  ALLOCATE( rMC(nMC),STAT=irv )
  IF( irv /= 0 )GOTO 9999
END IF

IF( lIntrpEmission )THEN

  t = 0.5*(update%currentTime+update%nextUpdate)

  IF( t > tNextEmi )THEN
    irv = ReadNextEmission( t )
    IF( irv /= SUCCESS )GOTO 9999
  END IF

  IF( lReadEmission )THEN
    rate = (t-tEmi)/(tNextEmi-tEmi)
    rate = MAX(MIN(rate,1.),0.)
    ratm = 1.-rate
    r1 = ratm*emiRate1(i) + rate*emiRate2(i)
    r2 = ratm*emiTemp1(i) + rate*emiTemp2(i)
    r3 = ratm*emiVel1(i)  + rate*emiVel2(i)
    IF( nMC > 0 )THEN
      rMC = ratm*emiMC1(:,i) + rate*emiMC2(:,i)
    END IF
  ELSE
    r1 = emiRate2(i)
    r2 = emiTemp2(i)
    r3 = emiVel2(i)
    IF( nMC > 0 )THEN
      rMC = emiMC2(:,i)
    END IF
  END IF

ELSE

  t = update%currentTime

  IF( t >= tNextEmi )THEN
    irv = ReadNextEmission( t )
    IF( irv /= SUCCESS )GOTO 9999
  END IF

  IF( .NOT.lReadEmission )THEN
    relData = TRANSFER(update%release%relData,relData)
    tdur    = relData%duration
  ELSE
    tdur = 1.   !Emission rates last at least 1 hour
  END IF

  IF( t < tEmi + tdur )THEN
    r1 = emiRate1(i)
    r2 = emiTemp1(i)
    r3 = emiVel1(i)
    IF( nMC > 0 )THEN
      rMC = emiMC1(:,i)
    END IF
  ELSE
    r1 = 0.
    r2 = 300.  !Set small but physical velocity and temperature
    r3 = 0.01
    IF( nMC > 0 )THEN
      rMC = 0.
    END IF
  END IF

END IF

SELECT CASE( update%release%type )
  CASE( HR_STACK,HR_PRIME )
    relStackData = TRANSFER(update%release%relData,relStackData)
    relStackData%rate    = r1
    relStackData%exitVel = r3
    IF( iAreaSrc(i,1) > 0 )THEN
      relStackData%exitTemp = DEF_VAL_R
    ELSE
      relStackData%exitTemp = r2 + ABSZERO
    END IF
    update%release%relData = TRANSFER(relStackData,update%release%relData)
  CASE( HR_CONT )
    relContData  = TRANSFER(update%release%relData,relContData)
    relContData%rate     = r1
    relContData%buoyancy = r2
    relContData%momentum = r3
    update%release%relData = TRANSFER(relContData,update%release%relData)
END SELECT

  IF( update%release%nMC > 0 )THEN
    DO i = 1,update%release%nMC
      update%release%MCmass(i) = rMC(i)
    END DO
  END IF

UpdateStackEmission = SUCCESS

9999 CONTINUE

IF( ALLOCATED(rMC) )DEALLOCATE( rMC,STAT=irv )

RETURN
END

