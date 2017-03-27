!*******************************************************************************
!$RCSfile$
!$Revision$
!$Date$
!*******************************************************************************
!===============================================================================
!     ReadStatus
!===============================================================================
SUBROUTINE ReadStatus( file,lunit,statusID,statHead,statList )

USE scipuff_fi
USE list_fd
USE status_fd

!     Reads the SCIPUFF TIME1 namelist from the input file (*.INP)

IMPLICIT NONE

CHARACTER(*),          INTENT( IN    ) :: file
INTEGER,               INTENT( IN    ) :: lunit
INTEGER,               INTENT( IN    ) :: statusID
TYPE( listHeadT ),     INTENT( INOUT ) :: statHead
INTEGER, DIMENSION(*), INTENT( OUT   ) :: statList

INTEGER ios,i,searchID
LOGICAL found

!==== Initialize

CALL init_error()

!==== Open the file

OPEN( UNIT=lunit,FILE=file,STATUS='OLD',ACTION="READ",IOSTAT=ios )
IF( ios /= 0 )THEN
  nError   = OP_ERROR
  eRoutine = 'ReadStatus'
  eMessage = 'Error opening SCIPUFF input file'
  GOTO 9998
END IF

!==== Read the namelist

SELECT CASE( statusID )

  CASE( 1 )
    tzone = 0.
    CALL ReadNamelistTime1( lunit )
    IF( nError /= NO_ERROR )GOTO 9998
    IF( statHead%number + TIME_STATUS_ARRAY_SIZE <= statHead%max )THEN
      DO i = 1,TIME_STATUS_ARRAY_SIZE
        statList(statHead%number+i) = time_status(i)
      END DO
      statHead%number = statHead%number + TIME_STATUS_ARRAY_SIZE
    ELSE
      nError   = SZ_ERROR
      eRoutine = 'ReadStatus'
      eMessage = 'Insufficient space to load time status array'
      goto 9998
    END IF

  CASE( 2 )
    CALL ReadNamelistDomain( lunit )
    IF( nError /= NO_ERROR )GOTO 9998
    IF( statHead%number + DOMAIN_STATUS_ARRAY_SIZE <= statHead%max )THEN
      DO i = 1,DOMAIN_STATUS_ARRAY_SIZE
        statList(statHead%number+i) = domain_status(i)
      END DO
      statHead%number = statHead%number + DOMAIN_STATUS_ARRAY_SIZE
    ELSE
      nError   = SZ_ERROR
      eRoutine = 'ReadStatus'
      eMessage = 'Insufficient space to load domain status array'
      GOTO 9998
    END IF

  CASE( 3 )
    found = .FALSE.
    DO WHILE( nError == NO_ERROR )
      CALL InitRelease()
      CALL ReadNamelistScn( lunit )
      IF( nError == NO_ERROR )THEN
        found = .TRUE.
        IF( statHead%number + SCIPUFF_STATUS_ARRAY_SIZE <= statHead%max )THEN
          IF( opid == NOT_SET_I )THEN
            DO i = 1,SCIPUFF_STATUS_ARRAY_SIZE
              statList(statHead%number+i) = STATUS_VALID
            END DO
          ELSE
            DO i = 1,SCIPUFF_STATUS_ARRAY_SIZE
              statList(statHead%number+i) = opmod(i)
            END DO
          END IF
          statList(statHead%number+SCIP32_OPID_HOLDER) = opid
          statHead%number = statHead%number + SCIPUFF_STATUS_ARRAY_SIZE
        ELSE
          nError   = SZ_ERROR
          eRoutine = 'ReadStatus'
          eMessage = 'Insufficient space to load release status array'
          GOTO 9998
        END IF
      END IF
    END DO
    IF( nError == EOF_ERROR .AND. found )CALL init_error()

  CASE DEFAULT
    found = .FALSE.
    searchID = ABS(statusID)
    DO WHILE( nError == NO_ERROR .AND. .NOT.found )
      CALL InitRelease()
      CALL ReadNamelistScn( lunit )
      IF( nError == NO_ERROR )THEN
        IF( opid == searchID )THEN
          found = .TRUE.
          IF( statHead%number + SCIPUFF_STATUS_ARRAY_SIZE <= statHead%max )THEN
            DO i = 1,SCIPUFF_STATUS_ARRAY_SIZE
              statList(statHead%number+i) = opmod(i)
            END DO
            statHead%number = statHead%number + SCIPUFF_STATUS_ARRAY_SIZE
          ELSE
            nError   = SZ_ERROR
            eRoutine = 'ReadStatus'
            eMessage = 'Insufficient space to load release status array'
            GOTO 9998
          END IF
        END IF
      END IF
    END DO

END SELECT

!==== Close the file and return

9999 CONTINUE

CLOSE( UNIT=lunit,IOSTAT=ios )

RETURN

9998 CONTINUE

CALL ReportFileName( eInform,'File=',file )
GOTO 9999

END
!===============================================================================
!     WriteStatus
!===============================================================================
SUBROUTINE WriteStatus( file,lunit,statHead,statList )

USE scipuff_fi
USE list_fd
USE status_fd

!     Reads the SCIPUFF TIME1 namelist to the input file (*.INP)

IMPLICIT NONE

CHARACTER(*),          INTENT( IN ) :: file
INTEGER,               INTENT( IN ) :: lunit
TYPE( listHeadT ),     INTENT( IN ) :: statHead
INTEGER, DIMENSION(*), INTENT( IN ) :: statList

INTEGER ios,i,ioff

!==== Open the file

OPEN( UNIT=lunit,FILE=file,STATUS='UNKNOWN',POSITION='APPEND',IOSTAT=ios )
IF( ios /= 0 )THEN
  nError   = OP_ERROR
  eRoutine = 'WriteStart'
  eMessage = 'Error opening SCIPUFF input file'
  GOTO 9998
END IF

!==== write the namelist

DO i = 1,TIME_STATUS_ARRAY_SIZE
  time_status(i) = statList(i)
END DO
CALL WriteNamelistTimeStatus( lunit )
IF( nError /= NO_ERROR )GOTO 9998

DO i = 1,DOMAIN_STATUS_ARRAY_SIZE
  domain_status(i) = statList(i+TIME_STATUS_ARRAY_SIZE)
END DO
CALL WriteNamelistDomainStatus( lunit )
IF( nError /= NO_ERROR )GOTO 9998

ioff = TIME_STATUS_ARRAY_SIZE + DOMAIN_STATUS_ARRAY_SIZE
DO WHILE( ioff+SCIPUFF_STATUS_ARRAY_SIZE <= statHead%number )
  DO i = 1,SCIPUFF_STATUS_ARRAY_SIZE-1
    opmod(i) = statList(i+ioff)
  END DO
  opmod(SCIP32_OPID_HOLDER) = 0
  opid                      = statList(SCIP32_OPID_HOLDER+ioff)
  CALL WriteNamelistScnStatus( lunit )
  IF( nError /= NO_ERROR )GOTO 9998
  ioff = ioff + SCIPUFF_STATUS_ARRAY_SIZE
END DO

!==== Close the file and return

9999 CONTINUE

CLOSE( UNIT=lunit,IOSTAT=ios )

RETURN

9998 CONTINUE
CALL ReportFileName( eInform,'File=',file )
GOTO 9999

END
