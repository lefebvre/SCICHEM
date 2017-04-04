!*******************************************************************************
!$RCSfile$
!$Revision$
!$Date$
!*******************************************************************************
MODULE datums

IMPLICIT NONE

INTERFACE

!==============================================================================
! DtmsExtendedZone
!==============================================================================
  INTEGER FUNCTION DtmsExtendedZone(rlamda,rphi,izone,xdecal,xeast,xnorth,idir,ierr,lipsoid)
!DEC$ ATTRIBUTES STDCALL, DECORATE, ALIAS : 'DtmsExtendedZone' :: DtmsExtendedZone
!DEC$ ATTRIBUTES REFERENCE :: rlamda
!DEC$ ATTRIBUTES REFERENCE :: rphi
!DEC$ ATTRIBUTES REFERENCE :: izone
!DEC$ ATTRIBUTES REFERENCE :: xdecal
!DEC$ ATTRIBUTES REFERENCE :: xeast
!DEC$ ATTRIBUTES REFERENCE :: xnorth
!DEC$ ATTRIBUTES REFERENCE :: idir
!DEC$ ATTRIBUTES REFERENCE :: ierr
!DEC$ ATTRIBUTES REFERENCE :: lipsoid
    REAL    rlamda
    REAL    rphi
    INTEGER izone
    REAL    xdecal
    REAL    xeast
    REAL    xnorth
    INTEGER idir
    INTEGER ierr
    INTEGER lipsoid
  END FUNCTION DtmsExtendedZone

!==============================================================================
! DtmsUTMfromLLAdomain
!==============================================================================
  INTEGER FUNCTION DtmsUTMfromLLAdomain(Wlla,Ella,Nlla,Slla,Zone,Wutm,Eutm,Nutm,Sutm,Rcode)
!DEC$ ATTRIBUTES STDCALL, DECORATE, ALIAS : 'DtmsUTMfromLLAdomain' :: DtmsUTMfromLLAdomain
!DEC$ ATTRIBUTES REFERENCE :: Wlla
!DEC$ ATTRIBUTES REFERENCE :: Ella
!DEC$ ATTRIBUTES REFERENCE :: Nlla
!DEC$ ATTRIBUTES REFERENCE :: Slla
!DEC$ ATTRIBUTES REFERENCE :: Wutm
!DEC$ ATTRIBUTES REFERENCE :: Eutm
!DEC$ ATTRIBUTES REFERENCE :: Nutm
!DEC$ ATTRIBUTES REFERENCE :: Sutm
!DEC$ ATTRIBUTES REFERENCE :: Zone
!DEC$ ATTRIBUTES REFERENCE :: Rcode
    REAL    Wlla
    REAL    Ella
    REAL    Nlla
    REAL    Slla
    REAL    Wutm
    REAL    Eutm
    REAL    Nutm
    REAL    Sutm
    INTEGER Zone
    INTEGER Rcode
  END FUNCTION DtmsUTMfromLLAdomain

!==============================================================================
! DtmsInitDatums
!==============================================================================
  INTEGER FUNCTION DtmsInitDatums( pFile )  RESULT( irv )
!DEC$ ATTRIBUTES STDCALL, DECORATE, ALIAS : 'DtmsInitDatums' :: DtmsInitDatums
!DEC$ ATTRIBUTES REFERENCE :: pFile
    USE DefSize_fd
    CHARACTER(PATH_MAXLENGTH) pFile
  END FUNCTION DtmsInitDatums

END INTERFACE

!==============================================================================

CONTAINS

!===============================================================================

LOGICAL FUNCTION InitUTM( INIfile )

USE DefSize_fd

!------ Checks to see if DATUMS dll is installed OK

CHARACTER(*), INTENT( IN ) :: INIfile

CHARACTER(PATH_MAXLENGTH) string

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: AddNull

string = AddNull( INIfile )

InitUTM = DtmsInitDatums( string ) == 1

RETURN
END FUNCTION InitUTM

!==============================================================================

INTEGER FUNCTION UTM2LL( zone,x,y,lat,lon ) RESULT( ierr )

!------ Convert from UTM to geodetic location

INTEGER, INTENT( IN  ) :: zone
REAL,    INTENT( IN  ) :: x,y
REAL,    INTENT( OUT ) :: lat,lon

INTEGER irv, izone, idir, lipsoid
REAL    xdecal, xeast, xnorth, ya

idir    = -1
xdecal  = 0.
ierr    = 0
lipsoid = 0

xeast  = x
xnorth = y

IF( xnorth < 0 )THEN
  xnorth = 10000. + xnorth
  izone  = -zone
ELSE
  izone = zone
END IF

irv = DtmsExtendedZone( lon,lat,izone,xdecal,xeast,xnorth, &
                        idir,ierr,lipsoid )
!------ If error, limit (x,y) to valid locations

IF( ierr /= 0 )THEN

  ya = MIN(10000.,ABS(y))
  IF( ABS(ya) <= 2230. )THEN
    xeast = MIN(MAX(x,-524.),1524.)
  ELSE IF( ABS(ya) <= 4460. )THEN
    xeast = MIN(MAX(x,-462.),1462.)
  ELSE IF( ABS(ya) <= 6680. )THEN
    xeast = MIN(MAX(x,-283.),1283.)
  ELSE IF( ABS(ya) <= 8890. )THEN
    xeast = MIN(MAX(x, -10.),1010.)
  ELSE
    xeast = MIN(MAX(x,323.),677.)
  END IF
  xnorth = SIGN(ya,y)

  IF( xnorth < 0 )THEN
    xnorth = 10000. + xnorth
  END IF

  irv = DtmsExtendedZone( lon,lat,izone,xdecal,xeast,xnorth, &
                          idir,ierr,lipsoid )

END IF

RETURN
END FUNCTION UTM2LL

!==============================================================================

INTEGER FUNCTION LL2UTM( lat,lon,zone,x,y ) RESULT( ierr )

!------ Convert from geodetic to UTM location

INTEGER, INTENT( INOUT ) :: zone
REAL,    INTENT( OUT   ) :: x,y
REAL,    INTENT( IN    ) :: lat,lon

INTEGER irv, izone, idir, lipsoid, izone0
REAL    rlamda, rphi, xdecal

idir    = 1
xdecal  = 0.
ierr    = 0
lipsoid = 0

rlamda = lon
rphi   = lat
izone  = zone

IF( izone == 0 )THEN
  IF( rlamda < -180. )THEN
    rlamda = rlamda + 360.
  ELSE IF( rlamda > 180. )THEN
    rlamda = rlamda - 360.
  END IF
!------ Handle longitude outside -/+180
!       Move to nearby zone at same distance from central meridian
ELSE IF( lon < -180. )THEN
  izone0 = izone
  DO izone = izone0+1,60
    rlamda = rlamda + 6.
    IF( rlamda >= -180. )EXIT
  END DO
ELSE IF( lon > 180. )THEN
  izone0 = izone
  DO izone = izone0-1,1,-1
    rlamda = rlamda - 6.
    IF( rlamda <= 180. )EXIT
  END DO
END IF

irv = DtmsExtendedZone( rlamda,rphi,izone,xdecal,x,y, &
                        idir,ierr,lipsoid )

!------ If error, limit to +/-9 deg. of center meridian

IF( ierr /= 0 )THEN
  rphi   = MIN(MAX(lat,-89.),89.)
  rlamda = -183. + izone*6.
  rlamda = MIN(MAX(lon,rlamda-9.),rlamda+9.)
  irv = DtmsExtendedZone( rlamda,rphi,izone,xdecal,x,y, &
                          idir,ierr,lipsoid )
END IF

IF( rphi < 0. )THEN
  y = y - 10000.
  izone = -izone
END IF
IF( zone == 0 )zone = izone

RETURN
END FUNCTION LL2UTM

!==============================================================================

INTEGER FUNCTION UTMdomain( Wlla,Ella,Nlla,Slla,Wutm,Eutm,Nutm,Sutm )

!------ Get bounding UTM domain for rectangular LL domain

REAL, INTENT( IN  ) :: Wlla,Ella,Nlla,Slla
REAL, INTENT( OUT ) :: Wutm,Eutm,Nutm,Sutm

LOGICAL lok
INTEGER irv, zone, rcode

irv = DtmsUTMfromLLAdomain( Wlla,Ella,Nlla,Slla,zone,Wutm,Eutm,Nutm,Sutm,rcode )
lok = (irv == 1) .AND. (rcode == 1)

IF( .NOT.lok )THEN
  UTMdomain = rcode
ELSE
  UTMdomain = 0
END IF

RETURN
END FUNCTION UTMdomain

END MODULE datums
