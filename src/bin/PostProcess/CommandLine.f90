!*******************************************************************************
!$RCSfile$
!$Revision$
!$Date$
!*******************************************************************************
!==============================================================================
! readCommandLine
!==============================================================================
INTEGER FUNCTION readCommandLine()

USE Extract_fi

IMPLICIT NONE

INTEGER    numCommand
INTEGER    ios2
CHARACTER(128) Command
CHARACTER(128) Value
CHARACTER( 2) Switch

INTEGER i, irv, ios
INTEGER nch, cNo

INTEGER, EXTERNAL :: SYSCheckFile
INTEGER, EXTERNAL :: sysNumArgs
INTEGER, EXTERNAL :: sysGetArg

INTEGER iArg
LOGICAL lExist, isChar

readCommandLine = 0

numCommand = sysNumArgs()


DO iArg = 1,numCommand

  ios2 = sysGetArg( iArg,Command )

  IF( ios2 > 0 )THEN

!------ Check for valid Command format
    IF( Command(1:1) /= '-' )THEN
      WRITE(6,*)'Invalid command line parameter'
      WRITE(6,*)'Parameter='//TRIM(Command)
      WRITE(6,'(" Valid parameter format is -SWITCH:Value ")')
      readCommandLine = -2
      GOTO 9999
    END IF

!------ Get Command value

    nch = LEN_TRIM(Command)
    i   = INDEX(Command,':')
    IF( i <= 0 )THEN
      Value = ' '
    ELSE IF( i >= nch )THEN
      Value = ' '
    ELSE IF( i < 3 )THEN
      WRITE(6,*)'Invalid command line parameter'
      WRITE(6,*)'Parameter='//TRIM(Command)
      WRITE(6,'(" Valid parameter format is -SWITCH:Value ")')
      readCommandLine = -3
      GOTO 9999
    ELSE
      Value = Command(i+1:nch)
    END IF

!-----  Determine Switch

    Switch = Command(2:2)

    ! Make option case insensitive
    CALL cupper( Switch )

    SELECT CASE( Switch )
      CASE( 'I' )
        IF( LEN_TRIM(Value) > 0 )THEN
          INQUIRE( FILE=Value,EXIST=lExist )
          IF( .NOT.lExist )THEN
            WRITE(6,*)'Specified INI file not found'
            WRITE(6,*)'File='//TRIM(Value)
            WRITE(6,*)'Check Path/File name'
            readCommandLine = -4
            GOTO 9999
          END IF
          ini_file = TRIM(Value)
        ELSE
          ini_file = "DEFAULT"
        END IF
      CASE( 'P' )
        IF( LEN_TRIM(Value) > 0 )THEN
          PrjName = TRIM(Value)
        ELSE
          WRITE(6,*)'Project name not specified'
          WRITE(6,*)'Please specify the name of the project'
          GOTO 9999
        END IF
      CASE( 'M' )
        IF( LEN_TRIM(Value) > 0 )THEN
          READ(Value,*,IOSTAT=ios)i
          IF( ios /= 0 .OR. i <= 0 )THEN
            i = maxGrd
          END IF
          maxGrd = i
        END IF
      CASE( 'O' )
        IF( LEN_TRIM(Value) > 0 )THEN
          CALL cupper( Value )
          SELECT CASE( Value(1:1) )
            CASE ('Y')
              overWrite = 1
            CASE ('N')
              overWrite = -1
            CASE ('P')
              overWrite = 0
            CASE DEFAULT
              WRITE(6,*)'Invalid /O command value : '//TRIM(Value)
              WRITE(6,*)'Valid values are Y(es), N(o), P(rompt)'
              WRITE(6,*)'Using default value : P'
          END SELECT
        END IF
      CASE( 'B' )
        IF( LEN_TRIM(Value) > 0 )THEN
          script_file = ADJUSTL(Value)
          irv = sysCheckFile( script_file )
          IF( irv /= SCIPsuccess )THEN
            WRITE(6,*)'Script file not found'
            WRITE(6,*)'File='//TRIM(script_file)
            WRITE(6,*)'Check Path/File name'
            GOTO 9999
          END IF
        ELSE
          WRITE(6,*)'Script file not specified'
          WRITE(6,*)'Please specify the name of the desired script file'
          GOTO 9999
        END IF
      CASE( 'R' )
        IF( LEN_TRIM(Value) > 0 )THEN
          CALL CUPPER( Value )
          RunMode = ADJUSTL(Value)
        ELSE
          WRITE(6,*)'Run Mode not specified'
          WRITE(6,*)'Please specify the run mode'
          GOTO 9999
        END IF
      CASE DEFAULT
        WRITE(6,*)'Invalid command line switch : '//TRIM(Switch)
        WRITE(6,'(" Valid switches for Ini file, Max grid, Overwrite ")',ADVANCE='NO')
        WRITE(6,'(" Batch file and Run mode are I,M,O, B and R respt. ")')
        WRITE(6,'()')
        readCommandLine = -5
        GOTO 9999
    END SELECT

  ELSE
    WRITE(6,*)'Error reading command line'
    WRITE(6,*)'Processing command number ',iArg
    readCommandLine = -1
    GOTO 9999
  END IF

END DO

9999 CONTINUE

RETURN
END

!==============================================================================
! CurrentTime
!==============================================================================
REAL FUNCTION CurrentTime()

IMPLICIT NONE

INTEGER, DIMENSION(8) :: values
INTEGER, EXTERNAL :: julian_day

INTEGER jday

CALL DATE_AND_TIME(VALUES = values)

!Value 1: 4 digit yr, 2: month, 3: day, local -> 5: hr, 6: min, 7: sec, 8: millisec

jday = julian_day( values(2),values(3),values(1) )

CurrentTime = ((jday*24. + values(5))*60. + values(6))*60. + values(7)

RETURN
END
