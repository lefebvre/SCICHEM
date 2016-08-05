!*******************************************************************************
!$RCSfile$
!$Revision$
!$Date$
!*******************************************************************************
SUBROUTINE PufMom( ToolUserID )

USE scipuff_fi
USE SCIPtool
USE localpuf

IMPLICIT NONE

!  this program prints the contents of a puff plot file
!  for a specified (or all) variable(s).

INTEGER                          :: ToolUserID

INTEGER, PARAMETER               :: MAXN=20

CHARACTER(80)                    :: ifile, ofile, answ
CHARACTER(4),DIMENSION(MAXN)     :: namex

REAL, DIMENSION(3)               :: xbar, ubar, xxrel, xfac
REAL, DIMENSION(6)               :: xmom

REAL                             :: sumc, zrfl, xmap, ymap

INTEGER                          :: nvar, ncht, nchi, ncho, itxt, ios
INTEGER                          :: i, it, j, k, m, irv, nt
LOGICAL                          :: lerror, lexist

REAL, DIMENSION(:,:) , ALLOCATABLE           :: rdata
TYPE( ppuffHeadT )                           :: puffHead

INTEGER, EXTERNAL                :: GetProjectPuffs
INTEGER, EXTERNAL                :: GetProjectPlotTimes

ToolUserID = 8642

itxt = -1
NPUF = 1

nvar = 13
nt   = 0
namex(1)  = 'time'
namex(2)  = 'x'
namex(3)  = 'y'
namex(4)  = 'z'
namex(5)  = 'sxx'
namex(6)  = 'sxy'
namex(7)  = 'sxz'
namex(8)  = 'syy'
namex(9)  = 'syz'
namex(10) = 'szz'
namex(11) = 'u'
namex(12) = 'v'
namex(13) = 'w'

title = 'Puff Moments'
ncht  = LEN_TRIM(title)

WRITE(6,60)
60 FORMAT('Enter offsets for x, y, and z : ',$)
READ(5,*) xxrel(1),xxrel(2),xxrel(3)

WRITE(6,61)
61 FORMAT('Enter xmap and ymap for project (prj/meters) : ',$)
READ(5,*) xmap, ymap

xfac(1) = 1./xmap
xfac(2) = 1./ymap
xfac(3) = 1.

DO
  CALL get_file( 'Puff ',ifile,nchi )
  IF( nchi == 0 )GOTO 9999

  ifile = ADJUSTL(ifile)
  nchi  = LEN_TRIM(ifile)
  INQUIRE( file=TRIM(ifile),EXIST=lexist )
  IF( lexist )THEN
    EXIT
  ELSE
    WRITE(6,*)'Cannot find ',TRIM(ifile)
    CYCLE
  END IF

END DO

DO
  WRITE(6,'(/,"Use TXT:filename for text output",/)')
  CALL get_file( 'Output ',ofile,ncho )
  IF( ncho == 0 )GOTO 9999
  ofile = ADJUSTL(ofile)
  IF (ofile(1:4) == 'TXT:') THEN
    itxt = 1
    ofile = ofile(5:LEN_TRIM(ofile))
  ELSE
    itxt = 0
  END IF
  INQUIRE( file=TRIM(ofile),EXIST=lexist )
  IF(lexist)THEN
    WRITE(6,'(" ",A," already exists",/," Delete file ? ",$)')TRIM(ofile)
    READ(5,'(A)',err=9999,END=9999) answ
    CALL cupper( answ )
    IF( answ(1:1) == 'Y' )THEN
      OPEN( 1,FILE=TRIM(ofile) )
      CLOSE( 1,STATUS='DELETE' )
      INQUIRE( file=TRIM(ofile),exist=lerror )
    ELSE
      CYCLE
    END IF
  ELSE
    lerror = .FALSE.
  END IF
  IF( lerror )THEN
    WRITE(6,*)'Cannot delete ',TRIM(ofile)
    CYCLE
  ELSE
    EXIT
  END IF

END DO

puffHead%project%name    = ifile(1:nchi-4)
puffHead%project%path    = ''
puffHead%Project%ID      = 0
puffHead%Project%version = 0

irv = GetProjectPlotTimes( puffHead%project )
IF( irv /= SCIPSuccess )THEN
  WRITE(6,*)' Error reading puff times'
  GOTO 9999
ELSEIF( nTimePuff == 0 )THEN
  WRITE(6,*)' Cannot find any valid puff times'
  GOTO 9999
END IF

ALLOCATE( rdata(MAXN,nTimePuff),STAT=ios )

DO it = 1,nTimePuff

  irv = GetProjectPuffs( ToolUserID,puffHead,it,.TRUE.,.FALSE. )
  IF( irv /= SCIPSuccess )THEN
    WRITE(6,*)' Error reading puff at time break ',it
    GOTO 9999
  END IF

  IF( npuf > 0 )THEN

    sumc = 0.
    DO i = 1,3
      xbar(i) = 0.
      ubar(i) = 0.
    END DO
    DO i = 1,6
      xmom(i)  = 0.
    END DO
    DO i = 1,npuf
      CALL subr_xx( puff(i),xxrel )
    END DO
    DO i = 1,npuf
      CALL getz_refl( puff(i)%zbar,puff(i)%zc,puff(i)%szz,zrfl )
      puff(i)%zbar = zrfl
      sumc = sumc + puff(i)%c
      CALL get_puff_mom( puff(i),puff(i)%c,xbar,xmom,xfac )
      ubar(1) = ubar(1) + puff(i)%c*puff(i)%uo
      ubar(2) = ubar(2) + puff(i)%c*puff(i)%vo
      ubar(3) = ubar(3) + puff(i)%c*puff(i)%wo
    END DO
    DO j = 1,3
      xbar(j) = xbar(j)/sumc
      ubar(j) = ubar(j)/sumc
    END DO
    m = 0
    DO j = 1,3
      DO k = j,3
        m = m + 1
        xmom(m) = xmom(m)/sumc - xbar(j)*xfac(j)*xbar(k)*xfac(k)
      END DO
    END DO
    nt = nt + 1
    rdata(1,nt) = t
    DO i = 1,3
      rdata(i+1,nt) = xbar(i)
    END DO
    DO i = 1,6
      rdata(i+4,nt) = xmom(i)
    END DO
    DO i = 1,3
      rdata(i+10,nt) = ubar(i)
    END DO

  END IF
END DO

IF( itxt == 1 )THEN
  !!!!!!!!!!!!!!!!! for ASCII output !!!!!!!!!!!!!!!!!!!!
  CALL write_ascii( 3,TRIM(ofile),MAXN,nvar,nt,namex,title,rdata,0 )
ELSE
  CALL write_xpp( 3,TRIM(ofile),MAXN,nvar,nt,namex,title,rdata,0 )
ENDIF

9999  CONTINUE

CALL ClearProjectPuffs()

RETURN
END

!----------------------------------------------------------------------

SUBROUTINE get_puff_mom( p,mass,xbar,xmom,xfac )

USE struct_fd

IMPLICIT NONE

TYPE (puff_str) :: p
REAL :: mass
REAL, DIMENSION(3) :: xbar, xfac, xcent
REAL, DIMENSION(6) :: xmom, sig
INTEGER :: j, k, m

xcent(1) = p%xbar
xcent(2) = p%ybar
xcent(3) = p%zbar
sig(1)   = p%sxx
sig(2)   = p%sxy
sig(3)   = p%sxz
sig(4)   = p%syy
sig(5)   = p%syz
sig(6)   = p%szz

m = 0
DO j = 1,3
  xbar(j) = xbar(j) + mass*xcent(j)
  DO k = j,3
    m = m + 1
    xmom(m) = xmom(m) + mass * &
              ( xcent(j)*xfac(j)*xcent(k)*xfac(k)+ sig(m) )
  END DO
END DO

RETURN
END

!----------------------------------------------------------------------

SUBROUTINE subr_xx( p,xxrel )

USE struct_fd

IMPLICIT NONE
TYPE (puff_str) ::  p
REAL, DIMENSION(3) ::  xxrel

p%xbar = p%xbar - xxrel(1)
p%ybar = p%ybar - xxrel(2)
p%zbar = p%zbar - xxrel(3)

RETURN
END

! -----------------------------------------------------------------------

SUBROUTINE getz_refl( zc,zinv,szz,zrfl )

IMPLICIT NONE

REAL :: zc, zinv, szz, zrfl, sz
REAL :: rt2pi, arg1, arg2, arg3, arg4
REAL, EXTERNAL :: erfc

sz = SQRT(szz)
if (zinv == 0.) zinv = zc+10.*sz
arg1 = zc*zc/(2.*szz)
arg2 = (zinv - zc)*(zinv - zc)/(2.*szz)
arg3 = SQRT(arg1)
arg4 = SQRT(arg2)
rt2pi = SQRT(2.*3.14159265)

zrfl = 2.*szz*(EXP(-arg1) - EXP(-arg2)) &
             + zc*rt2pi*sz*(1. - erfc(arg4) - erfc(arg3)) &
             + zinv*rt2pi*sz*erfc(arg4)

zrfl = zrfl/(rt2pi*sz)

RETURN
END
