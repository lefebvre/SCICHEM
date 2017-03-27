!*******************************************************************************
!$RCSfile$
!$Revision$
!$Date$
!*******************************************************************************
!===============================================================================
!     ReadRelease
!===============================================================================
SUBROUTINE ReadRelease( file,lunit,searchID,scnHead,relList )

USE list_fd
USE release_fd
USE scipuff_fi

!     Reads the SCIPUFF SCN namelists from the input file (*.SCN)

IMPLICIT NONE

CHARACTER(*),                   INTENT( IN  )    :: file
INTEGER,                        INTENT( IN  )    :: lunit
CHARACTER(*),                   INTENT( IN  )    :: searchID
TYPE( listHeadT ),              INTENT( INOUT  ) :: scnHead
TYPE( releaseT ), DIMENSION(*), INTENT( OUT )    :: relList

CHARACTER(32) testID
CHARACTER(32) incID

INTEGER ios, lun
INTEGER i
INTEGER nrelStart, iReplace
LOGICAL lseek, lopen
INTEGER n, nmc

TYPE( releaseMCT ), DIMENSION(:), ALLOCATABLE :: releaseMC

CHARACTER(128), EXTERNAL :: StripNull
LOGICAL,        EXTERNAL :: IncrementList

!==== Initialize

lseek = LEN_TRIM(searchID) > 0

!==== Check to see if file is already opened

INQUIRE( FILE=file,OPENED=lopen,IOSTAT=ios )
IF( ios /= 0 )THEN
  nError   = OP_ERROR
  eRoutine = 'ReadRelease'
  eMessage = 'Error checking open status of SCIPUFF input file'
  CALL ReportFileName( eInform,'File=',file )
  GOTO 9999
END IF

!==== If Open - determine unit number

IF( lopen )THEN
  INQUIRE( FILE=file,NUMBER=lun,IOSTAT=ios )
  IF( ios /= 0 )THEN
    nError   = OP_ERROR
    eRoutine = 'ReadRelease'
    eMessage = 'Error determining logical unit number of SCIPUFF input file'
    CALL ReportFileName( eInform,'File=',file )
    GOTO 9999
  END IF

!==== If not - Open the file

ELSE
  lun = lunit
  OPEN( UNIT=lunit,FILE=file,STATUS='OLD',ACTION="READ",IOSTAT=ios,DELIM='APOSTROPHE' )
  IF( ios /= 0 )THEN
    nError   = OP_ERROR
    eRoutine = 'ReadRelease'
    eMessage = 'Error opening SCIPUFF input file'
    GOTO 9998
  END IF
END IF

!==== If searching for specific name - check to see if already have it

IF( lseek )THEN
  iReplace = 0
  IF( searchID(1:1) /= '!' )THEN
    testID = searchID
    DO i = 1,scnHead%number
      incID = TRIM(StripNull( relList(i)%relName ))
      IF( TRIM(incID) == TRIM(testID) )GOTO 1000
    END DO
  ELSE
    testID = searchID(2:)
    DO i = 1,scnHead%number
      incID = TRIM(StripNull( relList(i)%relName ))
      IF( TRIM(incID) == TRIM(testID) )iReplace = i
    END DO
  END IF
ELSE
  testID   = ' '
  iReplace = 0
END IF

!==== Read the namelists
!       if lseek - read until find desired material
!       else     - read until encounter end-of-file

nrelStart = scnHead%number

DO WHILE( nError == NO_ERROR )
  CALL InitRelease()
  CALL ReadNamelistScn( lun )
  IF( nError == NO_ERROR )THEN
    IF( lseek )THEN
      IF( TRIM(relName) == TRIM(testID) )THEN
        IF( iReplace == 0 )THEN
          IF( .NOT.IncrementList(scnHead) )GOTO 9997
          CALL LoadRelease( relList(scnHead%number) )
        ELSE
          CALL LoadRelease( relList(iReplace) )
        END IF
        IF( nError /= NO_ERROR )GOTO 9999
        GOTO 1000
      END IF
    ELSE
      IF( .NOT.IncrementList(scnHead) )GOTO 9997
      CALL LoadRelease( relList(scnHead%number) )
      IF( nError /= NO_ERROR )GOTO 9999
    END IF
  END IF
END DO

!==== Do some error checking

IF( nError == EOF_ERROR )THEN
  IF( nrelStart == scnHead%number )THEN
    eRoutine = 'ReadRelease'
    eMessage = 'EOF encountered reading SCN namelists'
    IF( lseek )THEN
      WRITE(eInform,'(A,A)')'Requested release not found : ',TRIM(searchID)
    ELSE
      eInform = 'No valid release definitions found'
    END IF
  ELSE
    CALL init_error()
  END IF
END IF

IF( nError /= NO_ERROR )GOTO 9999

1000 CONTINUE

!==== Close the file and return

9999 CONTINUE

IF( .NOT.lopen )CLOSE(UNIT=lun,IOSTAT=ios)
RETURN

9997 CONTINUE
eMessage  = TRIM(eMessage)//' : Release List'
GOTO 9999

9998 CONTINUE
CALL ReportFileName( eInform,'File=',file )
GOTO 9999

END
!===============================================================================
!     WriteRelease
!===============================================================================
SUBROUTINE WriteRelease( file,lunit,searchID,scnHead,relList )

USE list_fd
USE release_fd
USE scipuff_fi

!     Writes the SCIPUFF SCN namelists to the input file (*.SCN)

IMPLICIT NONE


CHARACTER(*),                   INTENT( IN ) :: file
INTEGER,                        INTENT( IN ) :: lunit
CHARACTER(*),                   INTENT( IN ) :: searchID
TYPE( listHeadT ),              INTENT( IN ) :: scnHead
TYPE( releaseT ), DIMENSION(*), INTENT( IN ) :: relList

CHARACTER(32)  incID

INTEGER lun, ios, i
INTEGER n, n0, nn
LOGICAL lseek, lopen

CHARACTER(128), EXTERNAL :: StripNull

!==== Initialize

lseek = LEN_TRIM(searchID) > 0

!==== Check to see if file is already opened

INQUIRE( FILE=file,OPENED=lopen,IOSTAT=ios )
IF( ios /= 0 )THEN
  nError   = OP_ERROR
  eRoutine = 'WriteRelease'
  eMessage = 'Error checking open status of SCIPUFF input file'
  GOTO 9998
END IF

!==== If Open - determine unit number

IF( lopen )THEN
  INQUIRE( FILE=file,NUMBER=lun,IOSTAT=ios )
  IF( ios /= 0 )THEN
    nError   = OP_ERROR
    eRoutine = 'WriteRelease'
    eMessage = 'Error determining logical unit number of SCIPUFF input file'
    GOTO 9998
  END IF

!==== If not - Open the file

ELSE
  lun = lunit
  OPEN( UNIT=lun,FILE=file,STATUS='UNKNOWN',POSITION='APPEND',IOSTAT=ios,DELIM='APOSTROPHE' )
  IF( ios /= 0 )THEN
    nError   = OP_ERROR
    eRoutine = 'WriteRelease'
    eMessage = 'Error opening SCIPUFF input file'
    GOTO 9998
  END IF
END IF

!==== write the namelist

DO i = 1,scnHead%number
  IF( lseek )THEN
    incID = TRIM(StripNull( relList(i)%relName ))
    IF( TRIM(incID) == TRIM(searchID) )THEN
      CALL UnloadRelease( relList(i) )
      IF( nError /= NO_ERROR )GOTO 9999
      CALL WriteNamelistScn( lun )
      IF( nError /= NO_ERROR )GOTO 9999
      EXIT
    END IF
  ELSE
    CALL UnloadRelease( relList(i) )
    IF( nError /= NO_ERROR )GOTO 9999
    CALL WriteNamelistScn( lun )
    IF( nError /= NO_ERROR )GOTO 9999
  END IF
END DO

!==== Close the file and return

9999 CONTINUE

IF( .NOT.lopen )CLOSE( UNIT=lun,IOSTAT=ios )

RETURN

9998 CONTINUE
CALL ReportFileName( eInform,'File=',file )
GOTO 9999

END
