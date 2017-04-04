SUBROUTINE read_puff_util(pfile,time,tNearest)
USE scipuff_fi
USE files_fi
USE error_fi

IMPLICIT NONE

REAL         , INTENT( IN  ) :: time
CHARACTER*(*), INTENT( IN  ) :: pfile
REAL         , INTENT( OUT ) :: tNearest

INTEGER timeID

CALL deallocatePuffs()

file_puf = TRIM(pfile)
lun_puf  = 1

CALL NearestPuffTime( lun_puf,file_puf,time,timeID,tNearest )
IF( nError /= 0 )GOTO 9999

CALL ReadPuffsID( timeID )
IF( nError /= 0 )GOTO 9999

tNearest = t/3600.

WRITE(6,*)'Read puff file at t(hr) =',tNearest
WRITE(6,*)'Read',npuf,' puffs'

9999 CONTINUE

RETURN
END

!===============================================================================

SUBROUTINE accum_cell(icell,lgrid,cgrid,nv,ncell,maxn)
IMPLICIT NONE

!  Refine cell number ICELL of the adaptive grid pointer list LGRID
!  NCELL is the total number of cells in the grid

INTEGER, DIMENSION(*) :: lgrid
INTEGER               :: icell, ii, iv, ncell, maxn, nv
REAL, DIMENSION(maxn,*):: cgrid

lgrid(icell) = ncell + 1

DO ii = 1,2
  lgrid(ncell+ii) = 0
  DO iv = 1,nv
    cgrid(ncell+ii,iv) = 0.0
  END DO
END DO

ncell = ncell + 2

RETURN
END
!===============================================================================

SUBROUTINE set_slice(sl,nres)
USE scipuff_fi
USE surface_fi
IMPLICIT NONE

INTEGER, DIMENSION(2) :: nres
REAL, DIMENSION(2,2)    :: sl
REAL                  :: x1, y1, x2, y2

!------ Input sl = 2 endpoints

x1 = sl(1,1)
y1 = sl(2,1)

x2 = sl(1,2)
y2 = sl(2,2)

xmin = x1
dxg = (x2-x1)/FLOAT(nres(1))

ymin = y1
dyg = (y2-y1)/FLOAT(nres(2))

RETURN

END

!********************************************************!
SUBROUTINE translate_file(ctype,input,output,maxc,nch,lerror)

CHARACTER*(*) input,output,ctype
CHARACTER*80 temp
LOGICAL  lerror

INTEGER  SYSTEM
LOGICAL  lexist
CHARACTER*1   ctyp,answ
CHARACTER*4   cversion
CHARACTER*200  line

!==== This subroutine attempts to translate a file name that has an
!     enviroment variable at the start of its path name. It than checks
!     the existence of the file.
!     ctype = 'N' -> New file lerror=.true. If it already exists
!           = 'O' -> Old file lerror=.true. If it doesn't exist
!           = 'U' -> Unknown file lerror=.false.
!           = 'P' -> Prompt file - IF it exists then ask if file is to be
!                                  delete. If yes then delete the file.
!                                  lerror=.true. If can't delete the file
!           = 'B' -> Backup file - If it exists then move file to backup

!==== Translate the file name

nchi = LEN(input)
i = INDEX(input(1:nchi),':')
IF(input(1:1) == '$')THEN
  j = INDEX(input,'/')
  IF(j > 0)THEN
    CALL getenv(input(2:j-1),output)
    k = LEN_TRIM(output)
    output = output(1:k)//input(j:nchi)
    nch = MIN( k + (nchi-j+1) , maxc )
  END IF
ELSE IF(i > 0)THEN
  CALL getenv(input(1:i-1),output)
  k = LEN_TRIM(output)
  output = output(1:k)//'/'//input(i+1:nchi)
  nch = MIN( k + (nchi-i) + 1 , maxc )
ELSE
  output = input
  nch = MIN( nchi , maxc )
END IF

!==== Check for existence

ctyp = ctype(1:1)
CALL cupper(ctyp)

INQUIRE(FILE=output(1:nch),EXIST=lexist)

!==== New file
IF(ctyp == 'N')THEN
  IF(lexist)THEN
    lerror=.TRUE.
  ELSE
    lerror=.TRUE.
  END IF
!==== Old file
ELSE IF(ctyp == 'O')THEN
  IF(lexist)THEN
    lerror=.FALSE.
  ELSE
    lerror=.TRUE.
  END IF
!==== Prompt/delete file
ELSE IF(ctyp == 'P')THEN
  IF(lexist)THEN
    WRITE(6,200)output(1:nch)
  200  FORMAT(' ',a,' already exists',/,' Delete file ? ',$)
    READ(5,'(A)',ERR=8000,END=8000) answ
    CALL cupper(answ)
    IF( answ(1:1) == 'Y')THEN
      temp(1:nch) = output(1:nch)
      kstat = SYSTEM('rm -f '//temp(1:nch))
      INQUIRE(FILE=output(1:nch),EXIST=lerror)
    ELSE
      lerror=.TRUE.
    END IF
  ELSE
    lerror=.FALSE.
  END IF
!==== Backup file
ELSE IF(ctyp == 'B')THEN
  IF(lexist)THEN
    iversion = 1
10   WRITE(cversion,5000) iversion
5000 FORMAT('_',i3.3)
    INQUIRE(file=output(1:nch)//cversion,exist=lexist)
    IF (lexist .and. iversion<999) THEN
      iversion = iversion + 1
      GOTO 10
    ELSE
      line = 'mv '//output(1:nch)//' '//output(1:nch)//cversion
      kstat = SYSTEM(TRIM(line))
      WRITE(*,*)'Kstat = ',kstat
      WRITE(*,*)'Backed up ',output(1:nch),' TO ',output(1:nch),cversion
    END IF
  END IF
  INQUIRE(file=output(1:nch),exist=lexist)
  IF (lexist) THEN
    lerror = .TRUE.
  ELSE
    lerror = .FALSE.
  END IF
!==== Unknown TYPE
ELSE
  lerror=.FALSE.
END IF

RETURN

!==== Error getting prompt

8000  lerror = .TRUE.

RETURN
END
!==============================================================================

! Dummy call to LL2UTM and UTM2LL required for static lahey build

SUBROUTINE Callutm2ll
USE datums

INTEGER :: zone, irv
REAL    :: x,y
REAL    :: lat,lon

irv = UTM2LL( zone,x,y,lat,lon )
irv = LL2UTM( lat,lon,zone,x,y )

RETURN
END
