!*******************************************************************************
!$RCSfile$
!$Revision$
!$Date$
!*******************************************************************************
MODULE winAPI_fd
  USE basic_fd, ONLY: TRUE, FALSE, NULL

!==============================================================================
! WINDOWS API parameter and structure definitions
!==============================================================================
!
! PARAMETERS
!
  INTEGER, PARAMETER :: POINTER_LEN = 8
  INTEGER, PARAMETER :: LONG_PTR = POINTER_LEN
  INTEGER, PARAMETER :: UINT_PTR = POINTER_LEN
  INTEGER, PARAMETER :: ULONG_PTR= POINTER_LEN
  INTEGER, PARAMETER :: LPVOID   = POINTER_LEN
  INTEGER, PARAMETER :: PVOID    = POINTER_LEN
  INTEGER, PARAMETER :: LPCSTR   = POINTER_LEN
  INTEGER, PARAMETER :: LPSTR    = POINTER_LEN
  INTEGER, PARAMETER :: LPBYTE   = POINTER_LEN
  INTEGER, PARAMETER :: LPDWORD  = POINTER_LEN
  INTEGER, PARAMETER :: HANDLE   = POINTER_LEN

!  INTEGER, PARAMETER :: DOUBLE = 8          ! REAL(8)
  INTEGER, PARAMETER :: HFILESZ  = 4         ! REAL(4)  WHY?
  INTEGER, PARAMETER :: SFLOAT  = 4          ! REAL(4)
  INTEGER, PARAMETER :: DWORD  = 4           ! INTEGER(4)
  INTEGER, PARAMETER :: ULONG  = 4           ! INTEGER(4)
  INTEGER, PARAMETER :: LONG   = 4           ! INTEGER(4)
  INTEGER, PARAMETER :: UINT   = 4           ! INTEGER(4)
  INTEGER, PARAMETER :: SINT   = 4           ! INTEGER(4)
!  INTEGER, PARAMETER :: ENUM   = 4          ! INTEGER(4)
  INTEGER, PARAMETER :: BOOL   = 4           ! INTEGER(4)
  INTEGER, PARAMETER :: WORD   = 2           ! INTEGER(2)
!  INTEGER, PARAMETER :: USHORT = 2          ! INTEGER(2)
  INTEGER, PARAMETER :: SHORT  = 2           ! INTEGER(2)
  INTEGER, PARAMETER :: BYTE   = 1           ! INTEGER(1)
!  INTEGER, PARAMETER :: UCHAR  = 1          ! INTEGER(1)
!  INTEGER, PARAMETER :: SCHAR  = 1           ! INTEGER(1)
!  INTEGER, PARAMETER :: WCHAR  = 2          ! INTEGER(2)

  INTEGER, PARAMETER :: FLPARAM  = LONG_PTR
  INTEGER, PARAMETER :: FWPARAM  = UINT_PTR

!==============================================================================
! Basic parameters
!==============================================================================
  !INTEGER, PARAMETER :: NULL  = 0
  !INTEGER, PARAMETER :: FALSE = 0
  !INTEGER, PARAMETER :: TRUE  = 1
  INTEGER, PARAMETER :: IDOK  = 1


!==============================================================================
! Drive parameters
!==============================================================================
  INTEGER, PARAMETER :: DRIVE_REMOVABLE = 3
  INTEGER, PARAMETER :: DRIVE_REMOTE    = 4
  INTEGER, PARAMETER :: DRIVE_CDROM     = 5
!==============================================================================
! SECURITY_ATTRIBUTES
!==============================================================================
  TYPE  T_SECURITY_ATTRIBUTES
    SEQUENCE
    INTEGER(DWORD)    :: nLength
    INTEGER(LPVOID)   :: lpSecurityDescriptor
    INTEGER(BOOL)     :: bInheritHandle
  END TYPE  T_SECURITY_ATTRIBUTES

!==============================================================================
! SYSTEMTIME
!==============================================================================
!DEC# PACK:1
  TYPE  T_SYSTEMTIME
    SEQUENCE
    INTEGER(WORD)     :: wYear
    INTEGER(WORD)     :: wMonth
    INTEGER(WORD)     :: wDayOfWeek
    INTEGER(WORD)     :: wDay
    INTEGER(WORD)     :: wHour
    INTEGER(WORD)     :: wMinute
    INTEGER(WORD)     :: wSecond
    INTEGER(WORD)     :: wMilliseconds
  END TYPE  T_SYSTEMTIME
!DEC# PACK:

END MODULE winAPI_fd
!==============================================================================
!==============================================================================
!==============================================================================
!==============================================================================
MODULE winAPI
  USE winAPI_fd
!==============================================================================
! WINDOWS API function interfaces
!==============================================================================

  INTERFACE

!==============================================================================
! CopyFile
!==============================================================================
    FUNCTION CopyFile( lpExistingFileName,lpNewFileName,bFailIfExists )
!DEC# ATTRIBUTES DEFAULT                                :: CopyFile
!DEC# ATTRIBUTES STDCALL, DECORATE, ALIAS : 'CopyFileA' :: CopyFile
!DEC# ATTRIBUTES REFERENCE                              :: lpExistingFileName
!DEC# ATTRIBUTES REFERENCE                              :: lpNewFileName
      USE winAPI_fd
      INTEGER(BOOL) :: CopyFile
      CHARACTER(*)     lpExistingFileName
      CHARACTER(*)     lpNewFileName
      INTEGER(BOOL)    bFailIfExists
    END FUNCTION CopyFile

!==============================================================================
! CreateDirectory
!==============================================================================
    FUNCTION CreateDirectory( lpPathName,lpSecurityAttributes )
!DEC# ATTRIBUTES DEFAULT                                       :: CreateDirectory
!DEC# ATTRIBUTES STDCALL, DECORATE, ALIAS : 'CreateDirectoryA' :: CreateDirectory
!DEC# ATTRIBUTES REFERENCE                                     :: lpPathName
!DEC# ATTRIBUTES REFERENCE                                     :: lpSecurityAttributes
      USE winAPI_fd
      INTEGER(BOOL)            :: CreateDirectory
      CHARACTER(*)                lpPathName
      TYPE(T_SECURITY_ATTRIBUTES) lpSecurityAttributes
    END FUNCTION CreateDirectory

!==============================================================================
! DeleteFile
!==============================================================================
    FUNCTION DeleteFile( lpFileName )
!DEC# ATTRIBUTES DEFAULT                                  :: DeleteFile
!DEC# ATTRIBUTES STDCALL, DECORATE, ALIAS : 'DeleteFileA' :: DeleteFile
!DEC# ATTRIBUTES REFERENCE                                :: lpFileName
      USE winAPI_fd
      INTEGER(BOOL) :: DeleteFile
      CHARACTER(*)     lpFileName
    END FUNCTION DeleteFile

!==============================================================================
! FreeLibrary
!==============================================================================
    FUNCTION FreeLibrary( hLibModule )
!DEC# ATTRIBUTES DEFAULT                                  :: FreeLibrary
!DEC# ATTRIBUTES STDCALL, DECORATE, ALIAS : 'FreeLibrary' :: FreeLibrary
      USE winAPI_fd
      INTEGER(BOOL)   :: FreeLibrary
      INTEGER(HANDLE)    hLibModule
    END FUNCTION FreeLibrary

!==============================================================================
! GetCurrentDirectory
!==============================================================================
   FUNCTION GetCurrentDirectory( nBufferLength,lpBuffer )
!DEC# ATTRIBUTES DEFAULT                                           :: GetCurrentDirectory
!DEC# ATTRIBUTES STDCALL, DECORATE, ALIAS : 'GetCurrentDirectoryA' :: GetCurrentDirectory
!DEC# ATTRIBUTES REFERENCE                                         ::  lpBuffer
      USE winAPI_fd
      INTEGER(DWORD) :: GetCurrentDirectory
      INTEGER(DWORD)    nBufferLength
      CHARACTER(*)      lpBuffer
    END FUNCTION GetCurrentDirectory

!==============================================================================
! GetDriveType
!==============================================================================
    FUNCTION GetDriveType( lpRootPathName )
!DEC# ATTRIBUTES DEFAULT                                    :: GetDriveType
!DEC# ATTRIBUTES STDCALL, DECORATE, ALIAS : 'GetDriveTypeA' :: GetDriveType
!DEC# ATTRIBUTES REFERENCE                                  :: lpRootPathName
      USE winAPI_fd
      INTEGER(DWORD) :: GetDriveType
      CHARACTER(*)      lpRootPathName
    END FUNCTION GetDriveType

!==============================================================================
! GetLastError
!==============================================================================
    FUNCTION GetLastError()
!DEC# ATTRIBUTES DEFAULT                                   :: GetLastError
!DEC# ATTRIBUTES STDCALL, DECORATE, ALIAS : 'GetLastError' :: GetLastError
      USE winAPI_fd
      INTEGER(DWORD) :: GetLastError
    END FUNCTION GetLastError

!==============================================================================
! GetLocalTime
!==============================================================================
    SUBROUTINE GetLocalTime( lpSystemTime )
!DEC# ATTRIBUTES DEFAULT                                   :: GetLocalTime
!DEC# ATTRIBUTES STDCALL, DECORATE, ALIAS : 'GetLocalTime' :: GetLocalTime
!DEC# ATTRIBUTES REFERENCE                                 :: lpSystemTime
      USE winAPI_fd
      TYPE(T_SYSTEMTIME) lpSystemTime
    END SUBROUTINE GetLocalTime

!==============================================================================
! GetPrivateProfileInt
!==============================================================================
    FUNCTION GetPrivateProfileInt( lpAppName,lpKeyName,nDefault,lpFileName )
!DEC# ATTRIBUTES DEFAULT                                            :: GetPrivateProfileInt
!DEC# ATTRIBUTES STDCALL, DECORATE, ALIAS : 'GetPrivateProfileIntA' :: GetPrivateProfileInt
!DEC# ATTRIBUTES REFERENCE                                          :: lpAppName
!DEC# ATTRIBUTES REFERENCE                                          :: lpKeyName
!DEC# ATTRIBUTES REFERENCE                                          :: lpFileName
      USE winAPI_fd
      INTEGER(UINT) :: GetPrivateProfileInt
      CHARACTER(*)     lpAppName
      CHARACTER(*)     lpKeyName
      INTEGER(SINT)     nDefault
      CHARACTER(*)     lpFileName
    END FUNCTION GetPrivateProfileInt

!==============================================================================
! GetPrivateProfileString
!==============================================================================
    FUNCTION GetPrivateProfileString( lpAppName,lpKeyName,lpDefault,lpReturnedString,nSize,lpFileName )
!DEC# ATTRIBUTES DEFAULT                                               :: GetPrivateProfileString
!DEC# ATTRIBUTES STDCALL, DECORATE, ALIAS : 'GetPrivateProfileStringA' :: GetPrivateProfileString
!DEC# ATTRIBUTES REFERENCE                                             :: lpAppName
!DEC# ATTRIBUTES REFERENCE                                             :: lpKeyName
!DEC# ATTRIBUTES REFERENCE                                             :: lpDefault
!DEC# ATTRIBUTES REFERENCE                                             :: lpReturnedString
!DEC# ATTRIBUTES REFERENCE                                             :: lpFileName
      USE winAPI_fd
      INTEGER(BOOL) :: GetPrivateProfileString
      CHARACTER(*)     lpAppName
      CHARACTER(*)     lpKeyName
      CHARACTER(*)     lpDefault
      CHARACTER(*)     lpReturnedString
      INTEGER(DWORD)   nSize
      CHARACTER(*)     lpFileName
    END FUNCTION GetPrivateProfileString

!==============================================================================
! GetProcAddress
!==============================================================================
    FUNCTION GetProcAddress( hModule,lpProcName )
!DEC# ATTRIBUTES DEFAULT                                     :: GetProcAddress
!DEC# ATTRIBUTES STDCALL, DECORATE, ALIAS : 'GetProcAddress' :: GetProcAddress
!DEC# ATTRIBUTES REFERENCE                                   :: lpProcName
      USE winAPI_fd
      INTEGER(HANDLE) :: GetProcAddress
      INTEGER(HANDLE)    hModule
      CHARACTER*(*)      lpProcName
    END FUNCTION GetProcAddress

!==============================================================================
! GetTickCount
!==============================================================================
    FUNCTION GetTickCount()
!DEC# ATTRIBUTES DEFAULT                                   :: GetTickCount
!DEC# ATTRIBUTES STDCALL, DECORATE, ALIAS : 'GetTickCount' :: GetTickCount
      USE winAPI_fd
      INTEGER(DWORD) :: GetTickCount
    END FUNCTION GetTickCount

!==============================================================================
! LoadLibrary
!==============================================================================
    FUNCTION LoadLibrary( lpLibFileName )
!DEC# ATTRIBUTES DEFAULT                                   :: LoadLibrary
!DEC# ATTRIBUTES STDCALL, DECORATE, ALIAS : 'LoadLibraryA' :: LoadLibrary
!DEC# ATTRIBUTES REFERENCE :: lpLibFileName
      USE winAPI_fd
      INTEGER(HANDLE) :: LoadLibrary
      CHARACTER*(*)      lpLibFileName
    END FUNCTION LoadLibrary

!==============================================================================
! MoveFileEx
!==============================================================================
    FUNCTION MoveFileEx( lpExistingFileName,lpNewFileName,dwFlags )
!DEC# ATTRIBUTES DEFAULT                                  :: MoveFileEx
!DEC# ATTRIBUTES STDCALL, DECORATE, ALIAS : 'MoveFileExA' :: MoveFileEx
!DEC# ATTRIBUTES REFERENCE                                :: lpExistingFileName
!DEC# ATTRIBUTES REFERENCE                                :: lpNewFileName
      USE winAPI_fd
      INTEGER(BOOL) :: MoveFileEx
      CHARACTER(*)     lpExistingFileName
      CHARACTER(*)     lpNewFileName
      INTEGER(DWORD)   dwFlags
    END FUNCTION MoveFileEx

!==============================================================================
! RemoveDirectory
!==============================================================================
    FUNCTION RemoveDirectory( lpPathName )
!DEC# ATTRIBUTES DEFAULT                                       :: RemoveDirectory
!DEC# ATTRIBUTES STDCALL, DECORATE, ALIAS : 'RemoveDirectoryA' :: RemoveDirectory
!DEC# ATTRIBUTES REFERENCE                                     :: lpPathName
      USE winAPI_fd
      INTEGER(BOOL) :: RemoveDirectory
      CHARACTER(*)     lpPathName
    END FUNCTION RemoveDirectory

!==============================================================================
! SetCurrentDirectory
!==============================================================================
    FUNCTION SetCurrentDirectory( lpPathName )
!DEC# ATTRIBUTES DEFAULT                                           :: SetCurrentDirectory
!DEC# ATTRIBUTES STDCALL, DECORATE, ALIAS : 'SetCurrentDirectoryA' :: SetCurrentDirectory
!DEC# ATTRIBUTES REFERENCE                                         :: lpPathName
      USE winAPI_fd
      INTEGER(BOOL) :: SetCurrentDirectory
      CHARACTER(*)     lpPathName
    END FUNCTION SetCurrentDirectory

!==============================================================================
! WritePrivateProfileString
!==============================================================================
    FUNCTION WritePrivateProfileString( lpAppName,lpKeyName,lpString,lpFileName )
!DEC# ATTRIBUTES DEFAULT                                                 :: WritePrivateProfileString
!DEC# ATTRIBUTES STDCALL, DECORATE, ALIAS : 'WritePrivateProfileStringA' :: WritePrivateProfileString
!DEC# ATTRIBUTES REFERENCE                                               :: lpAppName
!DEC# ATTRIBUTES REFERENCE                                               :: lpKeyName
!DEC# ATTRIBUTES REFERENCE                                               :: lpString
!DEC# ATTRIBUTES REFERENCE                                               :: lpFileName
      USE winAPI_fd
      INTEGER(BOOL) :: WritePrivateProfileString
      CHARACTER(*)     lpAppName
      CHARACTER(*)     lpKeyName
      CHARACTER(*)     lpString
      CHARACTER(*)     lpFileName
    END FUNCTION WritePrivateProfileString

    FUNCTION DeletePrivateProfileString( lpAppName,lpKeyName,lpString,lpFileName )
!DEC# ATTRIBUTES DEFAULT                                                 :: DeletePrivateProfileString
!DEC# ATTRIBUTES STDCALL, DECORATE, ALIAS : 'WritePrivateProfileStringA' :: DeletePrivateProfileString
!DEC# ATTRIBUTES REFERENCE                                               :: lpAppName
!DEC# ATTRIBUTES REFERENCE                                               :: lpKeyName
!DEC# ATTRIBUTES VALUE                                                   :: lpString
!DEC# ATTRIBUTES REFERENCE                                               :: lpFileName
      USE winAPI_fd
      INTEGER(BOOL) :: DeletePrivateProfileString
      CHARACTER(*)     lpAppName
      CHARACTER(*)     lpKeyName
      INTEGER(LPSTR)   lpString
      CHARACTER(*)     lpFileName
    END FUNCTION DeletePrivateProfileString

    FUNCTION DeletePrivateProfileSection( lpAppName,lpKeyName,lpString,lpFileName )
!DEC# ATTRIBUTES DEFAULT                                                 :: DeletePrivateProfileSection
!DEC# ATTRIBUTES STDCALL, DECORATE, ALIAS : 'WritePrivateProfileStringA' :: DeletePrivateProfileSection
!DEC# ATTRIBUTES REFERENCE                                               :: lpAppName
!DEC# ATTRIBUTES VALUE                                                   :: lpKeyName
!DEC# ATTRIBUTES VALUE                                                   :: lpString
!DEC# ATTRIBUTES REFERENCE                                               :: lpFileName
      USE winAPI_fd
      INTEGER(BOOL) :: DeletePrivateProfileSection
      CHARACTER(*)     lpAppName
      INTEGER(LPSTR)   lpKeyName
      INTEGER(LPSTR)   lpString
      CHARACTER(*)     lpFileName
    END FUNCTION DeletePrivateProfileSection



!==============================================================================
! IARGC
!==============================================================================
    FUNCTION IARGC()
!DEC# ATTRIBUTES DEFAULT :: iargc
      USE winAPI_fd
      INTEGER(SINT) :: IARGC
    END FUNCTION

	  SUBROUTINE GETARG(N, BUFFER, STATUS)
!DEC# ATTRIBUTES DEFAULT :: GETARG
	    INTEGER(2) N
	    CHARACTER*(*) BUFFER
	    INTEGER(2), OPTIONAL :: STATUS
	  END SUBROUTINE

  END INTERFACE

!==============================================================================
! LoadIcon
!==============================================================================
  INTERFACE LoadIcon
    FUNCTION LoadIcon_G1( hInstance,lpIconName )
!DEC# ATTRIBUTES DEFAULT, STDCALL, DECORATE, ALIAS : 'LoadIconA' :: LoadIcon_G1
!DEC# ATTRIBUTES REFERENCE, ALLOW_NULL                           :: lpIconName
      USE winAPI_fd
      INTEGER(HANDLE) :: LoadIcon_G1
      INTEGER(HANDLE)    hInstance
      CHARACTER(*)       lpIconName
    END FUNCTION LoadIcon_G1

    FUNCTION LoadIcon_G2( hInstance,lpIconName )
!DEC# ATTRIBUTES DEFAULT, STDCALL, DECORATE, ALIAS : 'LoadIconA' :: LoadIcon_G2
      USE winAPI_fd
      INTEGER(HANDLE) :: LoadIcon_G2
      INTEGER(HANDLE)    hInstance
      INTEGER(HANDLE)    lpIconName
    END FUNCTION LoadIcon_G2
  END INTERFACE

!==============================================================================
! LoadCursor
!==============================================================================
  INTERFACE LoadCursor
    FUNCTION LoadCursor_G1( hInstance,lpCursorName )
!DEC# ATTRIBUTES DEFAULT, STDCALL, DECORATE, ALIAS : 'LoadCursorA' :: LoadCursor_G1
!DEC# ATTRIBUTES REFERENCE, ALLOW_NULL                             :: lpCursorName
      USE winAPI_fd
      INTEGER(HANDLE) :: LoadCursor_G1
      INTEGER(HANDLE)    hInstance
      CHARACTER(*)       lpCursorName
    END FUNCTION LoadCursor_G1

    FUNCTION LoadCursor_G2( hInstance,lpCursorName )
!DEC# ATTRIBUTES DEFAULT, STDCALL, DECORATE, ALIAS : 'LoadCursorA' :: LoadCursor_G2
      USE winAPI_fd
      INTEGER(HANDLE) :: LoadCursor_G2
      INTEGER(HANDLE)    hInstance
      INTEGER(HANDLE)    lpCursorName
    END FUNCTION LoadCursor_G2
  END INTERFACE

END MODULE winAPI
