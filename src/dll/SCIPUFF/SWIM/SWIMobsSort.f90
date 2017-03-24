!*******************************************************************************
!$RCSfile$
!$Revision$
!$Date$
!*******************************************************************************
MODULE SWIMObsSort

  USE SWIMobs_fd
  USE SWIMparam_fd
  USE SWIM_fi

  IMPLICIT NONE

  TYPE SortNode
    REAL                     :: r2, wt
    TYPE( ObsMet  ), POINTER :: Obs
    TYPE( SortNode), POINTER :: Greater, Lesser
  END TYPE SortNode

  CONTAINS

!------------------------------------------------------------------------------

  RECURSIVE INTEGER FUNCTION obs_tree( ObsNode,Obs,r2,wt ) RESULT( FuncVal )

  TYPE( SortNode ), POINTER :: ObsNode
  TYPE( ObsMet   ), POINTER :: Obs
  REAL,        INTENT( IN ) :: r2, wt

  INTEGER irv

  FuncVal = SWIMfailure

  IF( .NOT.ASSOCIATED(ObsNode) )THEN

    ALLOCATE( ObsNode,STAT=irv )
    IF( irv /= 0 )GOTO 9999

    NULLIFY( ObsNode%Lesser,ObsNode%Greater )
    ObsNode%Obs => Obs
    ObsNode%r2  =  r2
    ObsNode%wt  =  wt

  ELSE IF( r2 < ObsNode%r2 )THEN

    irv = obs_tree( ObsNode%Lesser,Obs,r2,wt )
    IF( irv /= SWIMsuccess )GOTO 9999

  ELSE

    irv = obs_tree( ObsNode%Greater,Obs,r2,wt )
    IF( irv /= SWIMsuccess )GOTO 9999

  END IF

  FuncVal = SWIMsuccess

  9999 CONTINUE

  RETURN

  END FUNCTION obs_tree

!------------------------------------------------------------------------------

  RECURSIVE INTEGER FUNCTION FindNearestList( xi,yi,xfac,yfac,a2,FirstObs, &
                                              NearObs,nobs,lsort )

!------ Find nearest obs based on single (master) linked-list

  INTEGER,            INTENT( IN    ) :: nobs
  REAL,               INTENT( IN    ) :: xi, yi, xfac, yfac, a2
  LOGICAL,            INTENT( IN    ) :: lsort
  TYPE( NearestObs ), INTENT( INOUT ) :: NearObs
  TYPE( ObsMet     ), POINTER         :: FirstObs

  TYPE( SortNode ), POINTER :: Root
  TYPE( ObsMet   ), POINTER :: Obs

  INTEGER n, irv
  REAL    r2, wt
  REAL    fx, fy

  REAL, EXTERNAL :: dist2

  FindNearestList = SWIMfailure

!------ Initial linked lists

  Obs => FirstObs
  NULLIFY( Root )

!------ If sorting, build binary tree; otherwise, just put
!       obs in Nearest array

  n = 0
  DO WHILE( ASSOCIATED(Obs) )
    fx = (xfac+Obs%xfac)/2.; fy = (yfac+Obs%yfac)/2.;
    r2 = dist2( Obs%x,Obs%y,xi,yi,fx,fy )
    IF( a2 /= 0. )THEN
      wt = 1.0/(1.0+r2*a2)
    ELSE
      wt = 1.0/(1.0+r2*Obs%a2)
    END IF
    n  = n + 1
    IF( lsort )THEN
      irv = obs_tree( Root,Obs,r2,wt )
      IF( irv /= SWIMsuccess )GOTO 9999
    ELSE
      NearObs%obs(n)%r2 = r2
      NearObs%obs(n)%wt = wt
      NearObs%obs(n)%Obs => Obs
    END IF
    Obs => Obs%NextObs
  END DO

  IF( lsort )THEN

!------ Build list of ordered obs

    n = 0
    CALL sort_obs( Root,NearObs%obs,n,nobs )

  END IF

  NearObs%numObs = nobs

  FindNearestList = SWIMsuccess

  9999 CONTINUE

  RETURN

  END FUNCTION FindNearestList

!------------------------------------------------------------------------------

  RECURSIVE INTEGER FUNCTION FindNearestGridList( xi,yi,xfac,yfac,a2,First,NearObs,nobs )

!------ Find nearest obs from grid cell-based linked-lists

  INTEGER,            INTENT( IN    ) :: nobs
  REAL,               INTENT( IN    ) :: xi, yi, xfac, yfac, a2
  TYPE( NearestObs ), INTENT( INOUT ) :: NearObs
  TYPE( FirstObsGridList ), POINTER   :: First

  TYPE( SortNode    ), POINTER :: Root
  TYPE( ObsMet      ), POINTER :: Obs
  TYPE( ObsGridList ), POINTER :: GridList

  INTEGER n, irv, is, js, ie, je, i, j, ip, iInc, jInc
  INTEGER isn, jsn, ien, jen
  REAL    r2, wt, fx, fy

  LOGICAL, DIMENSION(:), ALLOCATABLE :: DoneCell

  INTEGER, EXTERNAL :: SWIMlimit
  REAL,    EXTERNAL :: dist2

  FindNearestGridList = SWIMfailure

!------ Allocate array that checks if cell has already been used

  ALLOCATE( DoneCell(First%Nx*First%Ny),STAT=irv )
  IF( irv /= 0 )THEN
    error%Number  = UK_ERROR
    error%Routine = 'FindNearestGridList'
    error%Message = 'Error arrays for checking linked-list cells'
    GOTO 9999
  END IF

  DoneCell = .FALSE.

  CALL ZeroNumObsList( First )

!------ Find first cell for sweep

  fx = (xi-First%Xmin)/(First%Xmax-First%Xmin) * FLOAT(First%Nx)
  fy = (yi-First%Ymin)/(First%Ymax-First%Ymin) * FLOAT(First%Ny)
  is = SWIMlimit( INT(fx+0.5),1,First%Nx )
  js = SWIMlimit( INT(fy+0.5),1,First%Ny )
  ie = MIN(is+1,First%Nx)
  je = MIN(js+1,First%Ny)

!------ Sweep over cells clockwise

  i = is; j = js
  iInc = 0; jInc = 1
  n = 0
  NULLIFY( Root )

  DO

!------ Cell index; check if linked-list exists for this cell

    ip = (j-1)*First%Nx + i

    IF( ASSOCIATED(First%GridList(ip)%Obs) .AND. .NOT.DoneCell(ip) )THEN

!------ Initial obs

      GridList => First%GridList(ip)
      Obs      => GridList%Obs

!------ Go down linked-list and build binary tree

      DO
        fx = (xfac+Obs%xfac)/2.; fy = (yfac+Obs%yfac)/2.;
        r2 = dist2( Obs%x,Obs%y,xi,yi,fx,fy )
        IF( a2 /= 0. )THEN
          wt =  1.0/(1.0+r2*a2)
        ELSE
          wt = 1.0/(1.0+r2*Obs%a2)
        END IF
        wt =  1.0/(1.0+r2*a2)
        CALL SetNumObsList( First,Obs )
        irv = obs_tree( Root,Obs,r2,wt )
        IF( irv /= SWIMsuccess )GOTO 9999
        IF( .NOT.ASSOCIATED(GridList%Next) )EXIT
        GridList => GridList%Next
        Obs      => GridList%Obs
      END DO

    END IF

    DoneCell(ip) = .TRUE.

!------ Next cell; change increments when sweep limits are hit

    i = i + iInc; j = j + jInc
    IF( j > je )THEN
      iInc = 1; jInc = 0
      i = MIN(i+iInc,First%Nx); j = je
    ELSE IF( i > ie )THEN
      iInc = 0; jInc = -1
      i = ie; j = MAX(j+jInc,1)
    ELSE IF( j < js )THEN
      iInc = -1; jInc = 0
      i = MAX(i+iInc,1); j = js
    END IF

!------ Check for terminating sweep when back to starting cell:
!       terminate if enough obs found; otherwise sweep over "outer" cells

    IF( i == is .AND. j == js )THEN
      IF( CheckNumObsList(First,NearObs%numInterp) )EXIT !Found minimum no.of obs
      isn = MAX(is-1,1); ien = MIN(ie+1,First%Nx)
      jsn = MAX(js-1,1); jen = MIN(je+1,First%NY)
      IF( isn == is .AND. ien == ie .AND. &
          jsn == js .AND. jen == je )EXIT         !Exit if limits don't change
      is = isn; ie = ien; iInc = 0
      js = jsn; je = jen; jInc = 1
      i = is; j = js
    END IF

  END DO

!------ Build list of ordered obs; first find nearest

  n = 0
  CALL sort_obs( Root,NearObs%obs,n,nobs )

  NearObs%numObs = n

  FindNearestGridList = SWIMsuccess

  9999 CONTINUE

  IF( ALLOCATED(DoneCell) )DEALLOCATE( DoneCell,STAT=irv )

  RETURN

  END FUNCTION FindNearestGridList

!------------------------------------------------------------------------------

  RECURSIVE SUBROUTINE sort_obs( ObsNode,ObsPtr,n,nobs )

  INTEGER,          INTENT( IN    ) :: nobs
  INTEGER,          INTENT( INOUT ) :: n
  TYPE( SortNode ),         POINTER :: ObsNode
  TYPE( ObsList  ), DIMENSION(nobs) :: ObsPtr

  IF( ASSOCIATED(ObsNode) )THEN

    CALL sort_obs( ObsNode%Lesser,ObsPtr,n,nobs )

    n = n + 1
    ObsPtr(n)%Obs => ObsNode%Obs
    ObsPtr(n)%r2  =  ObsNode%r2
    ObsPtr(n)%wt  =  ObsNode%wt

    CALL sort_obs( ObsNode%Greater,ObsPtr,n,nobs )

    DEALLOCATE( ObsNode ); NULLIFY( ObsNode )

  END IF

  RETURN

  END SUBROUTINE sort_obs

!------------------------------------------------------------------------------

  SUBROUTINE ZeroNumObsList( First )

    TYPE( FirstObsGridList ), POINTER   :: First

    First%nVel = 0
    First%nT   = 0
    First%nP   = 0
    First%nH   = 0
    First%nQc  = 0
    First%nZi  = 0
    First%nHf  = 0
    First%nUs  = 0
    First%nL   = 0
    First%nCC  = 0
    First%nPr  = 0

    RETURN

  END SUBROUTINE ZeroNumObsList

!------------------------------------------------------------------------------

  SUBROUTINE SetNumObsList( First,Obs )

    TYPE( FirstObsGridList ), POINTER   :: First
    TYPE( ObsMet           ), POINTER :: Obs

    IF( First%lInterpVel )THEN
      IF( Obs%Vel%nZ > 0 )First%nVel = First%nVel + 1
    END IF
    IF( First%lInterpT )THEN
      IF( Obs%Tpot%nZ > 0 )First%nT = First%nT + 1
    END IF
    IF( First%lInterpP )THEN
      IF( Obs%Press%nZ > 0 )First%nP = First%nP + 1
    END IF
    IF( First%lInterpH )THEN
      IF( Obs%Humid%nZ > 0 )First%nH = First%nH + 1
    END IF
    IF( First%lInterpQcld )THEN
      IF( Obs%Qcloud%nZ > 0 )First%nQc = First%nQc + 1
    END IF

    IF( First%lInterpZi )THEN
      IF( Obs%varSrf%zi /= OBP_NODATA )First%nZi = First%nZi + 1
    END IF
    IF( First%lInterpHf )THEN
      IF( Obs%varSrf%hflux /= OBP_NODATA )First%nHf = First%nHf + 1
    END IF
    IF( First%lInterpUs )THEN
      IF( Obs%varSrf%ustr /= OBP_NODATA )First%nUs = First%nUs + 1
    END IF
    IF( First%lInterpL )THEN
      IF( Obs%varSrf%invL /= OBP_NODATA )First%nL = First%nL + 1
    END IF
    IF( First%lInterpCC )THEN
      IF( Obs%varSrf%cloudcover /= OBP_NODATA )First%nCC = First%nCC + 1
    END IF
    IF( First%lInterpPr )THEN
      IF( Obs%varSrf%prcp /= OBP_NODATA )First%nPr = First%nPr + 1
    END IF

    RETURN

  END SUBROUTINE SetNumObsList

!------------------------------------------------------------------------------

  LOGICAL FUNCTION CheckNumObsList( First,numInterp ) RESULT( lDone )

    TYPE( FirstObsGridList ), POINTER :: First
    INTEGER,            INTENT( IN ) :: numInterp

    INTEGER numInterpT

    lDone = .TRUE.

    numInterpT = MAX(numInterp/2,2)

    IF( First%lInterpVel  )lDone = lDone .AND. First%nVel >= numInterp
    IF( First%lInterpT    )lDone = lDone .AND. First%nT   >= numInterpT
    IF( First%lInterpP    )lDone = lDone .AND. First%nP   >= numInterpT
    IF( First%lInterpH    )lDone = lDone .AND. First%nH   >= numInterpT
    IF( First%lInterpQcld )lDone = lDone .AND. First%nQc  >= numInterpT
    IF( First%lInterpZi   )lDone = lDone .AND. First%nZi  >= numInterpT
    IF( First%lInterpHf   )lDone = lDone .AND. First%nHf  >= numInterpT
    IF( First%lInterpUs   )lDone = lDone .AND. First%nUs  >= numInterpT
    IF( First%lInterpL    )lDone = lDone .AND. First%nL   >= numInterpT
    IF( First%lInterpCC   )lDone = lDone .AND. First%nCC  >= numInterpT
    IF( First%lInterpPr   )lDone = lDone .AND. First%nPr  >= numInterpT

    RETURN

  END FUNCTION CheckNumObsList

END MODULE SWIMObsSort

