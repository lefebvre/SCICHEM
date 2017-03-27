!*******************************************************************************
!$RCSfile$
!$Revision$
!$Date$
!*******************************************************************************
MODULE accumsrf

CONTAINS

SUBROUTINE accum_surf( grd,x,y,mlev,ig,ng,ccell,icell,iout )

!------ Accumulate field on SAG grid

USE error_fi
USE sagstr_fd

IMPLICIT NONE

INTEGER,                       INTENT( IN    ) :: mlev  !Grid level for accumulation
INTEGER,                       INTENT( IN    ) :: ng    !No. of data fields to increment
INTEGER, DIMENSION(MAX(ng,1)), INTENT( IN    ) :: ig    !List of pointers into data fields
REAL,                          INTENT( IN    ) :: x, y  !(x,y) location
REAL, DIMENSION(MAX(ng,1)),    INTENT( IN    ) :: ccell !Field increments
TYPE( SAGgrid_str ),           POINTER         :: grd   !SAG grid structure
INTEGER,                       INTENT( INOUT ) :: icell !Next cell number if >0
INTEGER, OPTIONAL,             INTENT( OUT   ) :: iout  !Cell number

REAL, PARAMETER :: EPS = 1.E-6

INTEGER, POINTER, DIMENSION(:) :: igrd
REAL,    POINTER, DIMENSION(:) :: dat

REAL, SAVE :: c0

REAL xc, yc, ctst

INTEGER i, ix, iy, igx, ilev, iv, jcell, mx, nv

igrd => grd%ipgrd
dat  => grd%ipdat
mx   =  grd%mxgrd
nv   =  grd%nvart

IF( PRESENT(iout) )iout = 0

! ----- Find cell number if not given

IF( icell == 0 )THEN

  xc = x
  yc = y

  ix = INT( xc )
  iy = INT( yc )
  jcell = iy*grd%nx + ix + 1

! ----- Move down to required level

  DO ilev = 1,mlev

! ----- Build grid if it doesn't exist

    IF( igrd(jcell) == 0 )THEN
      IF( grd%ncells+4 > mx )THEN
        nError = SZ_ERROR
        eRoutine = 'accum_surf'
        eMessage = 'Unable to build SAG grid'
        eInform  = 'Maximum grid size encountered'
        GOTO 9999
      END IF
      CALL accum_cell( jcell,igrd,dat,nv,grd%ncells,mx )
    END IF
    xc = xc - FLOAT(ix)
    yc = yc - FLOAT(iy)

    xc = xc + xc
    yc = yc + yc

    ix = INT(xc)
    iy = INT(yc)

    jcell = igrd(jcell) + iy + iy + ix

  END DO

  IF( ix == 0 )icell = jcell

ELSE

! ---- If icell > 0 want neighboring cell, i.e., icell+1

  jcell = icell + 1
  icell = 0

END IF

! ----- Accumulate into data fields

DO iv = 1,ng
  i = (ig(iv)-1)*mx+jcell
  dat(i) = dat(i) + ccell(iv)
END DO

IF( PRESENT(iout) )iout = jcell

9999 CONTINUE

RETURN
END SUBROUTINE accum_surf

!===============================================================================

SUBROUTINE accum_cell( icell,lgrid,cgrd,nv,ncell,MAX )

!  Refine cell number ICELL of the adaptive grid pointer list LGRID
!  NCELL is the total number of cells in the grid

IMPLICIT NONE

INTEGER, INTENT( IN    ) :: icell      !Cell to be refined
INTEGER, INTENT( IN    ) :: nv         !No. of data fields
INTEGER, INTENT( INOUT ) :: ncell      !Current no. of cells
INTEGER, INTENT( IN    ) :: MAX        !Grid size

INTEGER, DIMENSION(:), POINTER :: lgrid  !SAG grid pointers
REAL,    DIMENSION(:), POINTER :: cgrd   !SAG grid data

INTEGER  ii, iv

! ----- Zero the 4 new grid cell data fields

DO ii = 1,4
  lgrid(ncell+ii) = 0
  DO iv = 1,nv
    cgrd(ncell+ii+(iv-1)*MAX) = 0.0
  END DO
END DO

! ----- Set grid pointer to first new cell

lgrid(icell) = ncell + 1

! ----- Increment ncell

ncell = ncell + 4

RETURN
END SUBROUTINE accum_cell

!===============================================================================

SUBROUTINE accum_surfv( grd,x,y,mlev,ig,ng,ccell,icell )

USE error_fi
USE sagstr_fd

!------ Accumulate field on SAG grid with no vertical refinement

IMPLICIT NONE

TYPE ( SAGgrid_str ), POINTER         :: grd   !SAG grid structure
REAL,                 INTENT( IN    ) :: x     !x-location
REAL,                 INTENT( IN    ) :: y     !y-location
INTEGER,              INTENT( IN    ) :: mlev  !Grid level for accumulation
INTEGER,              INTENT( IN    ) :: ng    !No. of data fields to increment
INTEGER,DIMENSION(ng),INTENT( IN    ) :: ig    !Pointer list for data fields
REAL,   DIMENSION(ng),INTENT( IN    ) :: ccell !Cell data values
INTEGER,              INTENT( INOUT ) :: icell !Cell number if >0

REAL, PARAMETER :: EPS = 1.E-6

INTEGER, POINTER, DIMENSION(:) :: igrd
REAL,    POINTER, DIMENSION(:) :: dat

REAL, SAVE :: c0

REAL xc, yc, ctst

INTEGER i, ix, iy, igx, ilev, iv, jcell, mx, nv

igrd => grd%ipgrd
dat  => grd%ipdat
mx   =  grd%mxgrd
nv   =  grd%nvart

ctst = ABS(ccell(1))
igx  = (ig(1)-1)*mx

! ----- Find cell number if not given

IF( icell == 0 )THEN

  xc = x
  yc = y

  ix = INT(xc)
  iy = INT(yc)
  jcell = iy*grd%nx + ix + 1

  c0  = dat(igx+jcell)
  IF( ctst < EPS*ABS(c0) )GOTO 9999

! ----- Move down to required level

  DO ilev = 1,mlev

! ----- Build grid if it doesn't exist

    IF( igrd(jcell) == 0 )THEN
      IF( grd%ncells+2 > mx )THEN
        nError = SZ_ERROR
        eRoutine = 'accum_surf'
        GOTO 9999
      END IF
      CALL accum_cellv( jcell,igrd,dat,nv,grd%ncells,mx )
    END IF

    xc = xc - FLOAT(ix)

    xc = xc + xc

    ix = INT(xc)

    jcell = igrd(jcell) + ix

    c0 = c0 + dat(igx+jcell)
    IF( ctst < EPS*ABS(c0) )GOTO 9999

  END DO

  IF( ix == 0 )icell = jcell

  c0 = c0 - dat(igx+jcell)

ELSE

! ---- If icell > 0 want neighboring cell, i.e., icell+1

  jcell = icell + 1
  icell = 0

  c0 = c0 + dat(igx+jcell)
  IF( ABS(ccell(1)) < EPS*ABS(c0) )GOTO 9999

END IF

! ----- Accumulate into data fields

DO iv = 1,ng
  i = (ig(iv)-1)*mx+jcell
  dat(i) = dat(i) + ccell(iv)
END DO

9999 CONTINUE

RETURN
END SUBROUTINE accum_surfv

!===============================================================================

SUBROUTINE accum_cellv( icell,lgrid,cgrd,nv,ncell,MAX )

!  Refine cell number ICELL of the adaptive grid pointer list LGRID
!  NCELL is the total number of cells in the grid

IMPLICIT NONE

INTEGER, INTENT( IN    ) :: icell      !Cell to be refined
INTEGER, INTENT( IN    ) :: nv         !No. of data fields
INTEGER, INTENT( INOUT ) :: ncell      !Current no. of cells
INTEGER, INTENT( IN    ) :: MAX        !Grid size

INTEGER, DIMENSION(:), POINTER :: lgrid  !SAG grid pointers
REAL,    DIMENSION(:), POINTER :: cgrd   !SAG grid data

INTEGER  ii, iv

! ----- Set grid pointer to first new cell

lgrid(icell) = ncell + 1

! ----- Zero the 2 new grid cell data fields

DO ii = 1,2
  lgrid(ncell+ii) = 0
  DO iv = 1,nv
    cgrd(ncell+ii+(iv-1)*MAX) = 0.0
  END DO
END DO

! ----- Increment ncell

ncell = ncell + 2

RETURN
END SUBROUTINE accum_cellv

END MODULE accumsrf

