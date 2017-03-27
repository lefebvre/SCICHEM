!*******************************************************************************
!$RCSfile$
!$Revision$
!$Date$
!*******************************************************************************
!==============================================================================
! GetCurrentDirectory
!==============================================================================
INTEGER FUNCTION GetCurrentDirectory ( nBufferLength ,lpBuffer )

USE winAPI_fd
IMPLICIT NONE

INTEGER, INTENT(IN)                   :: nBufferLength
CHARACTER(nBufferLength), INTENT(OUT) :: lpBuffer

INTEGER(4)     irv, getcwd, nch
CHARACTER(256) DirName

irv = -1
GetCurrentDirectory = NULL
lpBuffer = ''

irv = getcwd(DirName)
IF( irv == 0 )THEN
  nch = LEN_TRIM(DirName)
  IF( nch < nBufferLength )THEN
    lpBuffer = DirName(1:nch)
    GetCurrentDirectory = nch
  ELSE
    WRITE(*,*)' Error in GetCurrentDirectory nch > MAXPATHC',nch,nBufferLength
  END IF
ELSE
  WRITE(*,*)' Error in GetCurrentDirectory for getcwd'
END IF

RETURN
END

!==============================================================================
! SetCurrentDirectory
!==============================================================================
INTEGER FUNCTION SetCurrentDirectory( path_tmp )

USE winAPI_fd
IMPLICIT NONE

CHARACTER*(*)  path_tmp

CHARACTER*128 path

INTEGER  irv, chdir

CHARACTER*128, EXTERNAL :: STRIPNULL

path = STRIPNULL(path_tmp)

irv = -1
irv = chdir(TRIM(path))
IF (irv == 0) THEN
  SetCurrentDirectory  = TRUE
ELSE
  WRITE(*,100)TRIM(path)
  SetCurrentDirectory  = FALSE
END IF

100 FORMAT('Error in SetCurrentDirectory for "',(A),'"')

RETURN
END
!==============================================================================
! RemoveDirectory
!==============================================================================
INTEGER FUNCTION CreateDirectory (path_tmp,sa)

USE winAPI_fd
IMPLICIT NONE

CHARACTER*(*)  path_tmp

CHARACTER*128 path,cmd

INTEGER  ios, SYSTEM

TYPE ( T_SECURITY_ATTRIBUTES ) sa

CHARACTER*128, EXTERNAL :: STRIPNULL

path = STRIPNULL(path_tmp)
cmd = 'mkdir  '//TRIM(path)
ios = SYSTEM(TRIM(cmd))
IF (ios == 0) THEN
  CreateDirectory = TRUE
ELSE
  CreateDirectory = FALSE
  WRITE(*,*)' Error in CreateDirectory for ',TRIM(path)
END IF

RETURN
END

!==============================================================================
! RemoveDirectory
!==============================================================================
INTEGER FUNCTION RemoveDirectory (path_tmp)

USE winAPI_fd
IMPLICIT NONE

CHARACTER*(*) path_tmp
CHARACTER*128 path,cmd

INTEGER  ios, SYSTEM

CHARACTER*128, EXTERNAL :: STRIPNULL

path = STRIPNULL(path_tmp)

cmd = 'rmdir  '//TRIM(path)
ios = SYSTEM(TRIM(cmd))
IF (ios == 0) THEN
  RemoveDirectory = TRUE
ELSE
  RemoveDirectory = FALSE
  WRITE(*,*)' Error in RemoveDirectory for ',TRIM(path)
END IF

RETURN
END

!==============================================================================
! GetDriveType
!==============================================================================
INTEGER FUNCTION GetDriveType ( lpRootPathName )

USE search_fd
USE winAPI_fd
IMPLICIT NONE

CHARACTER*(*)   lpRootPathName

GetDriveType = LOCAL_DRIVE    ! Dummy function to always return local drive

RETURN
END

!==============================================================================
! GetLastError
!==============================================================================
INTEGER FUNCTION GetLastError ()
 GetLastError = 1
END

!==============================================================================
! LoadLibrary
!==============================================================================
FUNCTION LoadLibrary (lpLibFileName   )
USE winAPI_fd
IMPLICIT NONE

INTEGER(HANDLE) :: LoadLibrary
CHARACTER*(*)   lpLibFileName

!INTEGER, PARAMETER :: RTLD_LAZY   =  #0001   ! Lazy function call binding.
!INTEGER, PARAMETER :: RTLD_NOW    =  #0002   ! Immediate function call binding.

INTEGER(HANDLE)     dlopen

LoadLibrary = 1 !dlopen(lpLibFileName, RTLD_NOW)

RETURN
END

!==============================================================================
! FreeLibrary
!==============================================================================
FUNCTION FreeLibrary( hLibModule )
USE winAPI_fd
!USE SCIPresults_fd
IMPLICIT NONE

INTEGER(BOOL)   :: FreeLibrary
INTEGER(HANDLE)    hLibModule

INTEGER irv

INTEGER(BOOL)     dlclose

irv = 1 !dlclose( hLibModule )

IF( irv == FALSE )THEN
  FreeLibrary = FALSE
ELSE
  FreeLibrary = TRUE
END IF

RETURN
END

!==============================================================================
! GetProcAddress
!==============================================================================
FUNCTION GetProcAddress (hModule ,lpProcName)
USE winAPI_fd
IMPLICIT NONE

INTEGER(HANDLE) :: GetProcAddress
INTEGER(HANDLE)    hModule
CHARACTER*(*)      lpProcName

INTEGER(HANDLE)     dlsym

GetProcAddress = 1 !dlsym( hModule,lpProcName )

RETURN
END

!==============================================================================
! DeleteFile
!==============================================================================
INTEGER FUNCTION DeleteFile ( lpFileName )

USE winAPI_fd
IMPLICIT NONE

CHARACTER*(*)   lpFileName

CHARACTER*256   FileName
CHARACTER*128   cmd

INTEGER  ios, SYSTEM

CHARACTER*128, EXTERNAL :: STRIPNULL

FileName = STRIPNULL(lpFileName)

cmd = 'rm -f '//TRIM(FileName)
ios = SYSTEM(TRIM(cmd))
IF (ios == 0) THEN
  DeleteFile = TRUE
ELSE
  DeleteFile = FALSE
  WRITE(*,*)'Error in DeleteFile for ',TRIM(FileName)
END IF

RETURN
END

!==============================================================================
! CopyFile
!==============================================================================
INTEGER FUNCTION CopyFile (lpExistingFileName ,lpNewFileName ,bFailIfExists )

USE winAPI_fd
IMPLICIT NONE

CHARACTER(*)      lpExistingFileName
CHARACTER(*)      lpNewFileName
INTEGER           bFailIfExists

CHARACTER*256     ExistingFileName,NewFileName
CHARACTER(128)    cmd
INTEGER           ios, SYSTEM

CHARACTER*128, EXTERNAL :: STRIPNULL

ExistingFileName = STRIPNULL(lpExistingFileName)
NewFileName      = STRIPNULL(lpNewFileName)

cmd = 'cp -p '//TRIM(ExistingFileName)//' '//TRIM(NewFileName)
ios = SYSTEM(TRIM(cmd))
IF (ios == 0) THEN
  CopyFile = TRUE
ELSE
  CopyFile = FALSE
  WRITE(*,*)'Error in CopyFile for ',TRIM(ExistingFileName)
END IF

RETURN
END

!==============================================================================
! GetLocalTime
!==============================================================================
SUBROUTINE GetLocalTime (lpSystemTime)

USE winAPI_fd
IMPLICIT NONE

TYPE(T_SYSTEMTIME)    lpSystemTime


CHARACTER*12 :: sysGetTime
CHARACTER*10 :: date,clock,time,zone
INTEGER      :: values(8)

CALL date_and_time(date,clock,zone,values)

!values (1)  The 4-digit year
!values (2)  The month of the year
!values (3)  The day of the month
!values (4)  The time difference with respect to Coordinated Universal Time (UTC) in minutes
!values (5)  The hour of the day (range 0 to 23) - local time
!values (6)  The minutes of the hour (range 0 to 59) - local time
!values (7)  The seconds of the minute (range 0 to 59) - local time
!values (8)  The milliseconds of the second (range 0 to 999) - local time

lpSystemTime%wYear    = values(1)
lpSystemTime%wMonth   = values(2)
lpSystemTime%wDayOfWeek     = 1
lpSystemTime%wDay     = values(3)
lpSystemTime%wHour    = values(5)
lpSystemTime%wMinute  = values(6)
lpSystemTime%wSecond  = values(7)
lpSystemTime%wMilliseconds  = values(8)

RETURN
END

!==============================================================================
! GetTickCount
!==============================================================================
INTEGER FUNCTION GetTickCount ()
 GetTickCount = 0
RETURN
END

!==============================================================================
! Sleep
!==============================================================================
SUBROUTINE Sleep (Msec)

IMPLICIT NONE

INTEGER           Msec
CHARACTER(128) :: cmd
INTEGER :: ios, SYSTEM

WRITE(cmd,*)'sleep ',Msec*1.e-3
ios = SYSTEM(TRIM(cmd))
IF (ios /= 0) THEN
  WRITE(*,*)'Error in Sleep '
END IF

RETURN
END

!==============================================================================
! get_private_profile_string
!==============================================================================
INTEGER FUNCTION get_private_profile_string ( lpAppName ,lpKeyName ,lpDefault ,&
                                             lpReturnedString ,nSize ,lpFileName )
USE winAPI_fd
IMPLICIT NONE

CHARACTER(*)            :: lpAppName,lpKeyName,lpDefault
INTEGER                 :: nSize
CHARACTER(*)            :: lpReturnedString,lpFileName

INTEGER, PARAMETER      :: LUIN = 77
CHARACTER(128)          :: AppName, KeyName, FileName
CHARACTER(128)          :: Line,Buffer,SectionName
INTEGER                 :: ios,nstart,nend,ncomment
LOGICAL                 :: InSection,lrv
LOGICAL,EXTERNAL        :: IS_Section

CHARACTER*128, EXTERNAL :: ADDNULL, STRIPNULL


get_private_profile_string = nSize-1
lpReturnedString = lpDefault


FileName = STRIPNULL(lpFileName)
AppName  = STRIPNULL(lpAppName)
KeyName  = STRIPNULL(lpKeyName)

CALL cupper( AppName )
CALL cupper( KeyName )

OPEN(UNIT=LUIN,FILE=TRIM(FileName),STATUS='OLD',ACTION='READ',IOSTAT=ios)
IF( ios /= 0 )THEN
  WRITE(6,*)'Error opening '//TRIM(FileName)
  RETURN
END IF

ios         = 0
InSection   = .false.
SectionName = ' '
DO WHILE ( ios == 0 )
  !==============================================================================
  ! Read a line
  !==============================================================================
  READ(UNIT=LUIN,IOSTAT=ios,FMT='(A)')Line
  IF( ios /= 0 )EXIT
  Buffer = Line
  CALL cupper( Buffer )
  lrv = Is_Section(Buffer, SectionName)
  IF (lrv) THEN
    IF (TRIM(SectionName) == TRIM(AppName)) THEN
      InSection = .true.
    ELSE
      InSection = .false.
    END IF
  END IF
  IF (InSection) THEN
    ncomment = INDEX(Buffer,';')
    IF (ncomment /=1) THEN
      nstart = INDEX(Buffer,'=')
      IF ((nstart > 1).and.(nstart>ncomment)) THEN
        IF (Buffer(1:nstart-1) == TRIM(KeyName)) THEN
          nend = LEN_TRIM(Buffer)
          IF ( nend > nstart) THEN
            lpReturnedString = ADDNULL(TRIM(Line(nstart+1:nend)))
            get_private_profile_string = LEN_TRIM(lpReturnedString)
            GO TO 999
          END IF
        END IF
      END IF
    END IF ! ncomment
  END IF ! InSection
END DO

lpReturnedString           = lpDefault
get_private_profile_string = LEN_TRIM(lpDefault)

999   CLOSE(LUIN)

RETURN
END

!==============================================================================
LOGICAL FUNCTION Is_Section (Buffer, SectionName)

CHARACTER(128)    :: Buffer, SectionName
INTEGER           :: nstart, nend, ncomment

Is_Section = .false.
ncomment = INDEX(Buffer,';')
IF (ncomment == 1) RETURN
nstart = INDEX(Buffer,'[')
IF ((nstart > 0).and.(nstart>ncomment)) THEN
  nend = INDEX(Buffer,']')
  IF (nend > nstart+1) THEN
    SectionName = TRIM(Buffer(nstart+1:nend-1))
    Is_Section  = .true.
  END IF
END IF

END

!==============================================================================
! get_private_profile_int
!==============================================================================
INTEGER FUNCTION get_private_profile_int ( lpAppName ,lpKeyName ,nDefault ,lpFileName )

USE winAPI_fd
IMPLICIT NONE

CHARACTER(*)      :: lpAppName
CHARACTER(*)      :: lpKeyName
INTEGER           :: nDefault
CHARACTER(*)      :: lpFileName
CHARACTER*(128)   :: lpDefault
CHARACTER*(128)   :: lpReturnedString
INTEGER           :: nSize
INTEGER           :: i
INTEGER, EXTERNAL :: get_private_profile_string
CHARACTER(128)    :: StripNull, AddNull
EXTERNAL          :: StripNull, AddNull

WRITE(lpDefault,*) nDefault
lpDefault = AddNull( TRIM(lpDefault) )
nSize     = LEN(lpDefault)
i = get_private_profile_string (lpAppName ,lpKeyName ,lpDefault ,&
                                lpReturnedString ,nSize ,lpFileName  )
lpReturnedString = ADJUSTL(StripNull(lpReturnedString))

get_private_profile_int = 0
DO i = 1,LEN_TRIM(lpReturnedString)
  IF( lpReturnedString(i:i) < '0' )EXIT
  IF( lpReturnedString(i:i) > '9' )EXIT
  get_private_profile_int = get_private_profile_int*10 &
                            + ICHAR(lpReturnedString(i:i))-48
END DO

RETURN
END

!==============================================================================
! write_private_profile_string
!==============================================================================
INTEGER FUNCTION write_private_profile_string ( lpAppName ,lpKeyName ,lpString ,lpFileName )

USE winAPI_fd
IMPLICIT NONE

CHARACTER(*)   lpAppName
CHARACTER(*)   lpKeyName
CHARACTER(*)   lpString
CHARACTER(*)   lpFileName

write_private_profile_string = TRUE

END

!==============================================================================
! delete_private_profile_string
!==============================================================================
INTEGER FUNCTION delete_private_profile_string (lpAppName ,lpKeyName ,lpString ,lpFileName   )

USE winAPI_fd
IMPLICIT NONE

CHARACTER(*)   lpAppName
CHARACTER(*)   lpKeyName
INTEGER        lpString
CHARACTER(*)   lpFileName

delete_private_profile_string = TRUE

END

!==============================================================================
! delete_private_profile_section
!==============================================================================
INTEGER FUNCTION delete_private_profile_section (lpAppName ,lpKeyName ,lpString ,lpFileName   )

USE winAPI_fd
IMPLICIT NONE

CHARACTER(*)   lpAppName
INTEGER        lpKeyName
INTEGER        lpString
CHARACTER(*)   lpFileName

delete_private_profile_section = TRUE

END
