!*******************************************************************************
!$RCSfile$
!$Revision$
!$Date$
!*******************************************************************************
SUBROUTINE puff_los( lsg,los,pIn,cl,ccl,cID,comp )

USE scipuff_fi
USE los_fd

IMPLICIT NONE

LOGICAL,           INTENT( IN    ) :: lsg     !Single group flag
TYPE( los_str  ),  INTENT( IN    ) :: los     !Line-of-Sight Structure
TYPE( puff_str ),  INTENT( IN    ) :: pIn     !Puff Structure
REAL,              INTENT( INOUT ) :: cl, ccl !Integrated values (added to input values)
INTEGER, DIMENSION(:), OPTIONAL, POINTER         :: cID     !Multicomponent ID
REAL,    DIMENSION(*), OPTIONAL, INTENT( INOUT ) :: comp    !Multicomponent data

LOGICAL lcap

REAL,    DIMENSION(2) :: cfac
REAL,    DIMENSION(3) :: xr, xnrm
REAL(8), DIMENSION(7) :: asig

INTEGER ifld
REAL    vol, xmap, ymap, xbar, ybar, zbar, zc, dels
REAL    deth, hz, hx, hy, lx, ly, lz
REAL    znrm, xp, yp, zp, sl
REAL    xsav, ysav, zsav, dels_rfl, dot
INTEGER mcID, alloc_stat, i
REAL    cl0, ccl0
REAL, DIMENSION(:), ALLOCATABLE :: compMass

TYPE( puff_str )     p
TYPE( puff_totalcc ) pt

INTEGER, EXTERNAL :: getPuffifld

INTERFACE
  SUBROUTINE GetChemMassMC( p,cID,comp )
    USE chem_fi
    USE scipuff_fi
    TYPE( puff_str ),   INTENT( IN  )   :: p        !puff structure
    INTEGER, DIMENSION(:), POINTER      :: cID      !Multicomponent ID
    REAL, DIMENSION(*), INTENT( INOUT ) :: comp     !puff concentration at sensor, mean, variance and scale
  END SUBROUTINE GetChemMassMC
END INTERFACE

!---- Calculate puff integral along line-of-sight

p = pIn

!---- Set decay factor to unity for multicomponent

IF( PRESENT(cID) )THEN
  p%cfo = 1.0
  cl0   = cl
  ccl0  = ccl
  ALLOCATE( compMass(SIZE(cID)),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    nError = UK_ERROR
    eRoutine = 'puff_los'
    eMessage = 'Error allocating multicomponent array'
    GOTO 9999
  END IF
  DO i = 1,SIZE(cID)
    compMass(i) = 0.0
  END DO
END IF

vol     = PI3*SQRT(p%det)
cfac(1) = p%cfo*p%c/vol

IF( cfac(1) > SMALL )THEN

  lx = los%lx; ly = los%ly; lz = los%lz

  IF( lsg )THEN
    cfac(2) = (p%cfo**2)*(p%cc-p%ccb)/vol
  ELSE
    CALL get_totalcc( p,pt )
    cfac(2) = (p%cfo**2)*(pt%cct-pt%cctb)/vol
  END IF
  sl = (lx*lx+ly*ly)*p%si + lz*lz*p%sv
  IF( los%r > 0. )sl = MIN(sl,los%r)
  cfac(2) = sl*MAX(cfac(2),0.)

  CALL mapfac( p%xbar,p%ybar,xmap,ymap )

!---- Set terrain parameters

  IF( lter )THEN
    ifld = getPuffifld(p)
    CALL get_topogIn( p%xbar,p%ybar,hz,hx,hy,ifld )
  ELSE
    hx = 0.; hy = 0.; hz = 0.
  END IF

!---- Set puff centroid as origin

  xbar = p%xbar
  ybar = p%ybar
  zbar = p%zbar
  zc   = p%zc - zbar
  lcap = zc >= 0.

!---- Set sensor location relative to puff centroid

  xp   = (los%x - xbar)/xmap
  yp   = (los%y - ybar)/ymap
  zp   = los%z - zbar

!---- Set integral limits depending on LOS orientation

  IF( lz > 0. )THEN

    IF( lcap )THEN
      dels = (zc - zp)/lz !distance to cap (may be negative)
    ELSE
      dels = 1.e+36
    END IF

    dels = MIN(dels,los%r)

  ELSE IF( lz < 0. )THEN

    IF( lcap )THEN
      IF( zp > zc )THEN !move lower limit to zc
        dels = (zc - zp)/lz !this is a positive quantity
        xp   = xp + dels*lx
        yp   = yp + dels*ly
        zp   = zc
        dels = los%r - dels
      ELSE
        dels = los%r
      END IF
    ELSE
      dels = los%r
    END IF

  ELSE !if (lz == 0.) then

    dels = los%r

  END IF

!---- Return without calculation if dels <= 0.

  IF( dels <= 0. )GOTO 9999

  CALL point_int( p,xp,yp,zp,los%lx,los%ly,los%lz,dels,cfac,cl,ccl )

!---- Save sensor location

  xsav = xp; ysav = yp; zsav = zp

!---- Do ground reflection

  zbar = zbar - hz
  CALL get_asig( p,asig )
  CALL grnd_reflect( zbar,asig,hx,hy,xr,xnrm,deth,znrm )

!---- Set sensor location relative to ground reflected puff centroid

  xp = xsav - 2.*xr(1)
  yp = ysav - 2.*xr(2)
  zp = zsav - 2.*xr(3)

!---- Adjust LOS distance for intersection with ground plane [through (p%xbar,p%ybar,hz) with normal (-hx,-hy,1)]

  IF( lter )THEN

    dot = -hx*los%lx - hy*los%ly + los%lz

    IF( ABS(dot) > TINY(dot) )THEN

      dels_rfl = MAX( (hx*xsav + hy*ysav + (hz-los%z)) / dot, 0. )
      dels_rfl = MIN( dels_rfl,dels )

    ELSE

      dels_rfl = dels

    END IF

  ELSE

    dels_rfl = dels

  END IF

!---- Integrate through reflected puff from sensor location to dels

  CALL point_int( p,xp,yp,zp,los%lx,los%ly,los%lz,dels_rfl,cfac,cl,ccl )

!---- Do reflection if puff and sensor location are below cap
!     Create a reflected puff above the cap using grnd_reflect algorithm

  IF( lcap )THEN

    hx = 0.
    hy = 0.
    hz = 0.
    zbar = -zc
    CALL get_asig( p,asig )
    CALL grnd_reflect( zbar,asig,hx,hy,xr,xnrm,deth,znrm )

!---- Set sensor location relative to reflected inversion puff centroid

    xp = xsav - 2.*xr(1)
    yp = ysav - 2.*xr(2)
    zp = zsav - 2.*xr(3)

!---- Integrate through reflected puff from sensor location to dels

    CALL point_int( p,xp,yp,zp,los%lx,los%ly,los%lz,dels,cfac,cl,ccl )

  END IF

!---- Rescale for multicomponent

  IF( PRESENT(cID) )THEN

    mcID = typeID(p%ityp)%mcID

    SELECT CASE( mat_mc%type(mcID) )
      CASE( MC_CHEM )
        CALL GetChemMassMC( p,cID,compMass )
      CASE DEFAULT
        nError   = UK_ERROR
        eRoutine = 'puff_los'
        eMessage = 'Multicomponent error'
        WRITE(eInform,'(A,I6)') 'Unknown multicomponent type :',mat_mc%type(mcID)
        GOTO 9999
    END SELECT

    DO i = 1,SIZE(cID)
      comp(i) = comp(i) + (cl-cl0)*compMass(i)/p%c
    END DO

    cl  = cl0  + (cl -cl0 ) * pIn%cfo
    ccl = ccl0 + (ccl-ccl0) * pIn%cfo**2

  END IF

END IF

9999 CONTINUE

RETURN
END

!===============================================================================

SUBROUTINE point_int( p,xp,yp,zp,lx,ly,lz,dels,cfac,cl,ccl )

USE struct_fd
USE constants_fd

IMPLICIT NONE

REAL, PARAMETER :: ARGMAX = 20.0

TYPE( puff_str ),   INTENT( IN    ) :: p        !Puff Structure
REAL,               INTENT( IN    ) :: xp,yp,zp !Point location
REAL,               INTENT( IN    ) :: lx,ly,lz !Unit Vector
REAL,               INTENT( IN    ) :: dels     !Integral limit
REAL, DIMENSION(2), INTENT( IN    ) :: cfac     !Puff factor
REAL,               INTENT( INOUT ) :: cl, ccl  !Integrated values

LOGICAL lgrd

REAL xs, ys, zs, arg, fac
REAL s0, arg1, arg2, arg3
REAL bmin, bmax, emin, emax, del1, del2, del, f1, f2
REAL afac, bfac

REAL, EXTERNAL :: erfc

!-----  Calculate arguments for integral along the line-of-sight

arg1 =  (p%axx*lx*lx + 2.*p%axy*lx*ly + 2.*p%axz*lx*lz + &
         p%ayy*ly*ly + 2.*p%ayz*ly*lz + p%azz*lz*lz)

arg2 =  2.*(p%axx*lx*xp + p%axy*lx*yp + p%axz*lx*zp + &
            p%ayy*ly*yp + p%axy*ly*xp + p%ayz*ly*zp + &
            p%azz*lz*zp + p%axz*lz*xp + p%ayz*lz*yp)

!-----  Calculate distance to maximum concentration

s0 = -0.5*arg2/arg1

!-----  Check exponential argument at point location if pointing away from puff

IF( s0 < 0. )THEN
  arg = p%axx*xp*xp + 2.*p%axy*xp*yp + 2.*p%axz*xp*zp &
      + p%ayy*yp*yp + 2.*p%ayz*yp*zp + p%azz*zp*zp
  IF( arg > ARGMAX )RETURN
END IF

!-----  Compute exponential argument at s0

xs = xp + s0*lx
ys = yp + s0*ly
zs = zp + s0*lz

arg3 = p%axx*xs*xs + 2.*p%axy*xs*ys + 2.*p%axz*xs*zs &
     + p%ayy*ys*ys + 2.*p%ayz*ys*zs + p%azz*zs*zs

lgrd = arg3 < ARGMAX !check if puff makes a "significant" contribution

!------ Calculate integral from sensor smin to dels

IF( lgrd )THEN

  arg2 =  2.*(p%axx*lx*xs + p%axy*lx*ys + p%axz*lx*zs &
            + p%ayy*ly*ys + p%axy*ly*xs + p%ayz*ly*zs &
            + p%azz*lz*zs + p%axz*lz*xs + p%ayz*lz*ys)
  afac = SQRT(arg1)
  bfac = 0.5*arg2/arg1
  arg  = afac*bfac
  bmin = afac*(bfac - s0)
  bmax = bmin + afac*dels

  emin = erfc(ABS(bmin)); emax = erfc(ABS(bmax))
  IF( bmin < 0. )THEN
    del1 = 2.
    f1   = -1.
  ELSE
    del1 = 0.
    f1   = 1.
  END IF
  IF( bmax < 0. )THEN
    del2 = 2.
    f2   = -1.
  ELSE
    del2 = 0.
    f2   = 1.
  END IF

  del  = del1 - del2
  fac  = SQRTPI*(f1*emin - f2*emax + del)
  fac  = 0.5*fac*EXP(-arg3+arg*arg)/afac
  cl   = cl  + fac*cfac(1)
  ccl  = ccl + fac*cfac(2)

END IF

RETURN
END

!===============================================================================

SUBROUTINE grnd_intersect( los,dels )

! Find intersection of LOS with terrain

USE met_fi
USE los_fd

IMPLICIT NONE

TYPE( los_str ), INTENT( IN    ) :: los  !LOS structure
REAL,            INTENT( INOUT ) :: dels !Distance along LOS to intersection with ground

IF( .NOT.MetGrid(1)%lter )THEN !Simple calculation with no terrain

  IF( los%lz < 0. )THEN
    dels = los%z / ABS(los%lz)
  ELSE
    dels = 1.e+36
  END IF

ELSE !Call search algorithm with terrain

  CALL find_intersect( los,dels )

END IF

RETURN
END

!===============================================================================

SUBROUTINE find_intersect( los,s )

USE met_fi
USE los_fd

IMPLICIT NONE

TYPE( los_str ), INTENT( IN  ) :: los !LOS structure
REAL,            INTENT( OUT ) :: s   !Distance along LOS to intersection with ground

REAL    xmap, ymap
REAL    x1, y1, z1, x2, y2
REAL    lx, ly, lz, fx, fy
REAL    sx, sy, s0, ds
REAL    xb1, xb2, yb1, yb2
REAL    xp, yp, xb, yb
REAL    xmin1, ymin1, dxb, dyb
LOGICAL lcnv
INTEGER irv
INTEGER nxb, nyb
INTEGER i1, j1, i2, j2, ip, jp, inc

REAL, DIMENSION(4) :: h

LOGICAL, EXTERNAL :: intersect_los
INTEGER, EXTERNAL :: SWIMcnvCoord

!----- Check if LOS origin is within met domain

IF( los%x < MetGrid(1)%xminPrj .OR. los%x > MetGrid(1)%xmaxPrj .OR. &
    los%y < MetGrid(1)%yminPrj .OR. los%y > MetGrid(1)%ymaxPrj )THEN
  s = 0.; RETURN
END IF

!----- Define terrain grid locals

xmin1 = MetGrid(1)%xmin; nxb = MetGrid(1)%nx; dxb = MetGrid(1)%dx
ymin1 = MetGrid(1)%ymin; nyb = MetGrid(1)%ny; dyb = MetGrid(1)%dy

lcnv = MetGrid(1)%coord%type /= PrjCoord%type

!----- Locate origin on terrain grid

x1 = los%x ;  y1 = los%y ; z1 = los%z

IF( lcnv )THEN
  irv = SWIMcnvCoord( x1,y1,PrjCoord,xp,yp,MetGrid(1)%coord )
  CALL SWIMmapfac( MetGrid(1)%coord,xp,yp,xmap,ymap )
ELSE
  xp = x1; yp = y1
  CALL mapfac( x1,y1,xmap,ymap )
END IF

i1 = INT((xp-xmin1)/dxb) + 1 ; j1 = INT((yp-ymin1)/dyb) + 1

!----- Point to next terrain grid location

lx = SIGN(MAX(ABS(los%lx),1.e-10),los%lx); ip = NINT(SIGN(1.,lx))
ly = SIGN(MAX(ABS(los%ly),1.e-10),los%ly); jp = NINT(SIGN(1.,ly))
lz = los%lz

!----- Set grid indices for stepping along LOS

IF( ip == -1 )i1 = i1 + 1
IF( jp == -1 )j1 = j1 + 1

i2 = i1 + ip; j2 = j1 + jp

xb2 = xmin1 + FLOAT(i2-1)*dxb
yb2 = ymin1 + FLOAT(j2-1)*dyb

IF( lcnv )THEN
  irv = SWIMcnvCoord( xb2,yb2,MetGrid(1)%coord,xp,yp,PrjCoord )
  xb2 = xp; yb2 = yp
END IF

!----- Check for special case where x or y lie exactly on xb(i2) or yb(j2)

IF( x1 == xb2 )THEN
  i1 = i2; i2 = i1 + ip
END IF

IF( y1 == yb2 )THEN
  j1 = j2; j2 = j1 + jp
END IF

!----- Step along LOS

s = 0.

DO

  xb1 = xmin1 + FLOAT(i1-1)*dxb
  xb2 = xmin1 + FLOAT(i2-1)*dxb
  yb1 = ymin1 + FLOAT(j1-1)*dyb
  yb2 = ymin1 + FLOAT(j2-1)*dyb

  IF( lcnv )THEN
    xb = xb1 + 0.5*dxb; yb = yb1 + 0.5*dyb
    irv = SWIMcnvCoord( xb1,yb,MetGrid(1)%coord,xp,yp,PrjCoord )
    xb1 = xp
    irv = SWIMcnvCoord( xb2,yb,MetGrid(1)%coord,xp,yp,PrjCoord )
    xb2 = xp
    irv = SWIMcnvCoord( xb,yb1,MetGrid(1)%coord,xp,yp,PrjCoord )
    yb1 = yp
    irv = SWIMcnvCoord( xb,yb2,MetGrid(1)%coord,xp,yp,PrjCoord )
    yb2 = yp
  END IF

!----- Terminate if outside terrain domain

  IF( i2 < 1 .OR. i2 > nxb .OR. j2 < 1 .OR. j2 > nyb )THEN
    s = 1.e+36
    EXIT
  END IF

  CALL mapfac( x1,y1,xmap,ymap )

!----- Find distance along LOS to intersection with next x- and y-grid locations

  sx = (xb2-x1)/(lx*xmap); sy = (yb2-y1)/(ly*ymap)

!----- Use closest

  IF( sx < sy )THEN
    s0 = sx
    x2 = xb2; y2 = y1 + ly*ymap*s0
    inc = 1
  ELSE IF( sy < sx )THEN
    s0 = sy
    y2 = yb2; x2 = x1 + lx*xmap*s0
    inc = 2
  ELSE ! sx=sy
    s0 = sx
    x2 = xb2; y2 = yb2
    inc = 0
  END IF

!----- Setup input for finding intersection

!----- Terrain elevations of bounding cell

  h(1) = MetGrid(1)%H((j1-1)*nxb+i1)
  h(2) = MetGrid(1)%H((j1-1)*nxb+i2)
  h(3) = MetGrid(1)%H((j2-1)*nxb+i2)
  h(4) = MetGrid(1)%H((j2-1)*nxb+i1)

!----- Factors for converting x,y to distance along LOS

  IF( lcnv )CALL SWIMmapfac( MetGrid(1)%coord,xb,yb,xmap,ymap )
  fx = FLOAT(ip)*lx*xmap/dxb; fy = FLOAT(jp)*ly*ymap/dyb

!----- Look for intersection

  IF( intersect_los(x1,y1,z1,s0,xb1,xb2,yb1,yb2,fx,fy,lz,h,ds) )THEN
    s = s + ds; EXIT
  END IF

!----- None found; increment grid indices

  IF( inc == 1 )THEN
    i1 = i2; i2 = i2 + ip
  ELSE IF( inc == 2 )THEN
    j1 = j2; j2 = j2 + jp
  ELSE
    i1 = i2; i2 = i2 + ip
    j1 = j2; j2 = j2 + jp
  END IF

!----- Increment location along LOS

  x1 = x2; y1 = y2; z1 = z1 + lz*s0; s = s + s0

END DO

RETURN
END

!===============================================================================

LOGICAL FUNCTION intersect_los( x1,y1,z1,ds,xb1,xb2,yb1,yb2,fx,fy,fz,h,s )

!------ Find intersection of LOS through (x1,y1,z1),
!       with direction determined by fx,fy & fz

USE constants_fd

IMPLICIT NONE

REAL,               INTENT( IN  ) :: x1,y1,z1 !Starting point
REAL,               INTENT( IN  ) :: ds       !Maximum length along LOS
REAL,               INTENT( IN  ) :: xb1,xb2  !x-coordinates of bounding grid cell
REAL,               INTENT( IN  ) :: yb1,yb2  !y-coordinates of bounding grid cell
REAL,               INTENT( IN  ) :: fx,fy,fz !Factors for converting distance to project coordinates
REAL, DIMENSION(4), INTENT( IN  ) :: h        !Array of elevation at corners of bounding grid cell
REAL,               INTENT( OUT ) :: s        !Distance from (x1,y1,z1) to terrain intersection

REAL h1, h2, h3, h4
REAL dx, dy
REAL x0, y0
REAL dh, aq, bq, c, d, f, sqrtd
REAL s1, s2

!------ Initialize to false

intersect_los = .FALSE.; s = 1.e+36

!------ Setup for solving quadratic equation, i.e,
!       LOS elevation minus terrain as function of distance along LOS

h1 = h(1)
h2 = h(2) - h1; h3 = h(3) - h1; h4 = h(4) - h1
dh = h3 - h2 - h4

dx = xb2-xb1;     dy = yb2-yb1
x0 = (x1-xb1)/dx; y0 = (y1-yb1)/dy

aq = -fx*fy*dh
bq = fz - (fx*h2 + fy*h4 + (x0*fy+y0*fx)*dh)
c  = z1 - (h1 + x0*h2 + y0*h4 + x0*y0*dh)

!------ Check if intersection is possible

f = aq*ds*ds + bq*ds + c      !Function at endpoint (assume always > 0 at start pt.)

IF( f > 0. )THEN              !No intersection possible unless a
                              !minimum exists along the LOS
  IF(  aq <= 0.       )RETURN !No minimum possible
  IF(  bq >= 0.       )RETURN !No positive s intersection possible
  IF( -bq >= 2.*aq*ds )RETURN !Minimum outside range

END IF

!------ Solve quadratic equation

IF( ABS(aq*c) > SPACING(bq*bq) )THEN

  d = bq*bq - 4.*aq*c
  IF( d >= 0. )THEN
    sqrtd = SQRT(d)
    s1 = (-bq - sqrtd)/(2.*aq); s2 = (-bq + sqrtd)/(2.*aq)
    IF( s1 > 0. .AND. s2 > 0. )THEN
      s = MIN( s1,s2 )
      IF( s > ds )THEN
        s = 1.e+36
        RETURN
      END IF
    ELSE IF( s1 >= 0. .AND. s1 <= ds )THEN
      s = s1
    ELSE IF( s2 >= 0. .AND. s2 <= ds )THEN
      s = s2
    ELSE
      RETURN
    END IF
  ELSE
    RETURN
  END IF

!------ Solve linear equation

ELSE IF( ABS(bq) > SMALL )THEN

  s1 = -c/bq
  IF( s1 > 0. .AND. s1 <= ds )THEN
    s = s1
  ELSE
    RETURN
  END IF

ELSE

  RETURN

END IF

intersect_los = .TRUE.

RETURN
END

