PROGRAM met2sci

!------ Generate SCIPUFF weather observation files
!       Based on AERMET (dated 12345)

!       Written by D. Henn, Sage Management, Princeton, NJ

USE met2sci_fi

IMPLICIT NONE

INTEGER irv
INTEGER iPathway

INTEGER, EXTERNAL :: ParseJobInput, ParseUpperAirInput, ParseSurfaceInput
INTEGER, EXTERNAL :: ExtractUAdata, ExtractSFCdata

!------ Initialization

lun        =  1      !Basic input/control file
lunUA      =  2
lunSfc     =  3
lunOutSFC  = 30
lunOutUA   = 31
lunTmp     = 99
lunScratch = 98
lunErr     = 97

iPathway  = NoPathway
lJob      = .FALSE.
lUpperAir = .FALSE.
lSurface  = .FALSE.
lUAQ      = .TRUE.
lSFQ      = .TRUE.

UAlist%nFile  = 0; NULLIFY( UAlist%first )
SFClist%nFile = 0; NULLIFY( SFClist%first )

UAfile = ''; SFCfile = ''
reportFile = '';  messageFile = ''
!QAfileUA = ''; QAfileSFC = ''

error%Number  = NO_ERROR
error%Routine = ''
error%aString = ''
error%bString = ''
error%cString = ''

!------ Define some AERMET data

CALL SetAERMETdata()

!------ Open file name from command line

CALL getarg(1,inputFile)

IF( LEN_TRIM(inputFile) == 0 )THEN
  error%Number  = IV_ERROR
  error%Routine = 'met2sci'
  error%aString = 'No input file specifed on command line'
  GOTO 9999
END IF

OPEN(UNIT=lun,FILE=TRIM(inputFile),STATUS='OLD',ACTION='READ',IOSTAT=ios)
IF( ios /= 0 )THEN
  error%Number  = OP_ERROR
  error%Routine = 'met2sci'
  error%aString = 'Error opening input file '//TRIM(inputFile)
  GOTO 9999
END IF

!----- Read down file looking for pathway sections and data
!      N.B. blank lines are skipped and all strings are
!      converted to upper case

DO

  line = ' '
  CALL get_next_data( lun,line,nch,kwrd,narg,carg,MAXN,lerr )
  IF( lerr )EXIT !Assumed to be EOF error

  CALL cupper( carg(1) )

!----- Skip comments

  IF( carg(1)(1:1) == '*' )CYCLE

!----- Check for one of 3 pathways
!      JOB, UPPERAIR, or SURFACE

  SELECT CASE( TRIM(carg(1)) )

    CASE( 'JOB' )
      iPathway = JOB;
      lJob     = .TRUE.
      CYCLE

    CASE( 'UPPERAIR' )
      iPathway  = UPPERAIR
      lUpperAir = .TRUE.
      NULLIFY( currentFile )
      CYCLE

    CASE( 'SURFACE' )
      iPathway = SURFACE
      lSurface = .TRUE.
      NULLIFY( currentFile )
      CYCLE

  END SELECT

!------ Parse input for current pathway

  SELECT CASE( iPathway )

    CASE( JOB )
      irv = ParseJobInput()
      IF( irv /= SUCCESS )GOTO 9999

    CASE( UPPERAIR )
      irv = ParseUpperAirInput()
      IF( irv /= SUCCESS )GOTO 9999

    CASE( SURFACE )
      irv = ParseSurfaceInput()
      IF( irv /= SUCCESS )GOTO 9999

  END SELECT

END DO

!------ Open report and message files
!       for AERMET routines 

CALL SetDeviceUnits( lunRpt,lunMsg )

IF( LEN_TRIM(reportFile) > 1 )THEN
  OPEN(UNIT=lunRpt,FILE=TRIM(reportFile),STATUS='UNKNOWN',ACTION='READWRITE',IOSTAT=ios)
  IF( ios /= 0 )THEN
    error%Number  = OP_ERROR
    error%Routine = 'met2sci'
    error%aString = 'Error opening report file'
    error%bString = 'File='//TRIM(reportFile)
    GOTO 9999
  END IF
END IF

IF( LEN_TRIM(messageFile) > 1 )THEN
  OPEN(UNIT=lunMsg,FILE=TRIM(messageFile),STATUS='UNKNOWN',ACTION='READWRITE',IOSTAT=ios)
  IF( ios /= 0 )THEN
    error%Number  = OP_ERROR
    error%Routine = 'met2sci'
    error%aString = 'Error opening message file'
    error%bString = 'File='//TRIM(messageFile)
    GOTO 9999
  END IF
END IF

!------ Read UA and SFC files and create corresponding SCICHEM files
!       N.B. Call AREMET subroutines

IF( UAlist%nFile > 0 )THEN
  irv = ExtractUAdata()
  IF( irv /= SUCCESS )GOTO 9999
END IF

IF( SFClist%nFile > 0 )THEN
  irv = ExtractSFCdata()
  IF( irv /= SUCCESS )GOTO 9999
END IF

9999 CONTINUE

!------ Write error messages to met2sci.err

IF( error%Number /= NO_ERROR )THEN
  CALL ReportError()
END IF

!------ Create (partial) Stage 1 AERMET report

CALL SUMRY2( 1 )

CLOSE(lun,   IOSTAT=ios)
CLOSE(lunMsg,IOSTAT=ios)
CLOSE(lunRpt,IOSTAT=ios)

STOP

END PROGRAM met2sci

