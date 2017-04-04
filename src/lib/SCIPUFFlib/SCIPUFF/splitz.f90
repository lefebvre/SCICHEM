!*******************************************************************************
!$RCSfile$
!$Revision$
!$Date$
!*******************************************************************************
SUBROUTINE split_zi( p,zinv )

USE scipuff_fi
USE step_p_fi

IMPLICIT NONE

REAL, PARAMETER :: ZFAC = 3.0000000 ! set to match value in step_p
REAL, PARAMETER :: R2   = 0.7071068

TYPE( puff_str ), INTENT( INOUT ) :: p
REAL,             INTENT( IN    ) :: zinv


REAL    dzz, tfac, zrfl
REAL    temz, szz
LOGICAL lrfl_top, lrfl_bot

!------ check space for new puff

CALL check_newpuff()
IF( nError /= NO_ERROR )GOTO 9999

!------ determine vertical range for newly created puffs

zbar = p%zbar
dzz  = ZFAC*sz
tfac = R2/sz
zrfl = p%zc

!------ top-most extent

IF( zrfl >= zbar + dzz .OR. zrfl == 0. )THEN
  ztop = zbar + dzz
  lrfl_top = .FALSE.
ELSE
  ztop = zrfl
  lrfl_top = .TRUE.
END IF

!------ bottom reflection condition

lrfl_bot = (zbar - hp) < dzz

!------ create new puff above zi with remainder below (in original puff)

CALL chop_puff_zi( p,sz,tfac,zinv,ztop,zrfl,hp,lrfl_top,lrfl_bot )

!----- limit capped puff to well-mixed value

temz = WMX*(zinv-hp)
szz  = MIN(p%szz,temz*temz)

temz  = SQRT(szz/p%szz)
p%sxz = p%sxz*temz
p%syz = p%syz*temz
p%szz = szz
sz    = sz*temz

9999 CONTINUE

RETURN
END

!===============================================================================

SUBROUTINE chop_puff_zi( p,sz,tfac,zinv,ztop,zrfl,hp,lrfl_top,lrfl_bot )

USE scipuff_fi

USE files_fi

IMPLICIT NONE

TYPE( puff_str ), INTENT( INOUT ) :: p
REAL,             INTENT( IN    ) :: sz
REAL,             INTENT( IN    ) :: tfac
REAL,             INTENT( IN    ) :: zinv
REAL,             INTENT( IN    ) :: ztop
REAL,             INTENT( IN    ) :: zrfl
REAL,             INTENT( IN    ) :: hp
LOGICAL,          INTENT( IN    ) :: lrfl_top, lrfl_bot

INTEGER next
REAL    zlow, frac
REAL    szz, delz, temz, sxz, syz
REAL    aspect, fracFrac, minFrac

REAL, PARAMETER :: MIN_FRAC = 0.05
REAL, PARAMETER :: MAX_FRAC = 0.5 - MIN_FRAC
REAL, PARAMETER :: MIN_ASPECT = 1000.0             !Aspect**2 => Aspect=31.6
REAL, PARAMETER :: MAX_ASPECT = 100.0*MIN_ASPECT   !Aspect**2 => Aspect=316.2

INTEGER, EXTERNAL :: next_puff

!----- set puff above zi

aspect = MAX(p%sxx,p%syy)/p%szz                    !Aspect**2
IF( aspect > 10.0*MAX_ASPECT )GOTO 9999            !Aspect**2 => Aspect = 1000.0

zlow = zinv
CALL chop_puff( p,sz,tfac,zlow,ztop,zrfl,hp,lrfl_top,lrfl_bot,frac,delz,szz )

fracFrac = 0.5*LOG10(MAX(MIN(aspect,MAX_ASPECT),MIN_ASPECT)/MIN_ASPECT)
minFrac = min_FRAC + fracFrac*MAX_FRAC

IF( frac <= minFrac )GOTO 9999

next = next_puff()

CALL zero_puff( puff(next) )

puff(next)%sxx = p%sxx
puff(next)%sxy = p%sxy
puff(next)%syy = p%syy

CALL set_puff_xc( puff(next),p,frac )

puff(next)%zc = p%zc
puff(next)%zi = p%zi

puff(next)%xbar = p%xbar
puff(next)%ybar = p%ybar
puff(next)%zbar = p%zbar + delz

temz = SQRT(szz/p%szz)
sxz  = p%sxz*temz
syz  = p%syz*temz

puff(next)%sxz  = sxz
puff(next)%syz  = syz
puff(next)%szz  = szz

puff(next)%ityp = p%ityp
puff(next)%inxt = 0
puff(next)%iprv = 0
puff(next)%idtl = p%idtl
puff(next)%idtn = p%idtn
puff(next)%ipgd = p%ipgd

!----- set puff below zi (don't move centroid or change second-moments)

frac = 1. - frac
CALL scale_puff( p,frac )

p%zc = zinv

9999 CONTINUE

RETURN
END

!===============================================================================

SUBROUTINE chop_puff( p,sz,tfac,zlow,ztop,zrfl,hp, &
                      lrfl_top,lrfl_bot,frac,delz,szz )

USE scipuff_fi

IMPLICIT NONE

TYPE( puff_str ), INTENT( INOUT ) :: p
REAL,             INTENT( IN    ) :: sz
REAL,             INTENT( IN    ) :: tfac
REAL,             INTENT( IN    ) :: zlow
REAL,             INTENT( IN    ) :: ztop
REAL,             INTENT( IN    ) :: zrfl
REAL,             INTENT( IN    ) :: hp
LOGICAL,          INTENT( IN    ) :: lrfl_top, lrfl_bot
REAL,             INTENT( OUT   ) :: frac
REAL,             INTENT( OUT   ) :: delz
REAL,             INTENT( OUT   ) :: szz

REAL zbar_top, zbar_bot, zbar

!------ compute section mass fraction and vertical moments

CALL gauss_int( p%zbar,zlow,ztop,sz,p%szz,tfac,frac,delz,szz )

IF( frac == 0. )RETURN

!------ reflections if necessary

IF( lrfl_top )THEN

  zbar_top = 2.*zrfl - p%zbar
  CALL refl_int( zbar_top,p%zbar,zlow,ztop,sz,szz,delz,frac,tfac )

END IF

IF( lrfl_bot )THEN

  zbar_bot = 2.*hp - p%zbar
  CALL refl_int( zbar_bot,p%zbar,zlow,ztop,sz,szz,delz,frac,tfac )

  IF( lrfl_top )THEN

    zbar = 2.*zrfl - zbar_bot
    CALL refl_int( zbar,p%zbar,zlow,ztop,sz,szz,delz,frac,tfac )

    zbar = 2.*hp - zbar_top
    CALL refl_int( zbar,p%zbar,zlow,ztop,sz,szz,delz,frac,tfac )

  END IF

END IF

RETURN
END

!===============================================================================

SUBROUTINE set_puff_xc( p1,p2,frac )

USE scipuff_fi

IMPLICIT NONE

TYPE( puff_str), INTENT( INOUT ) :: p1
TYPE( puff_str), INTENT( IN    ) :: p2
REAL,            INTENT( IN    ) :: frac

TYPE( puff_str_xc) p1_xc, p2_xc

p1_xc = TRANSFER(p1,p1_xc)
p2_xc = TRANSFER(p2,p2_xc)

p1_xc%psum = frac*p2_xc%psum

p1_xc%prat = p2_xc%prat

p1 = TRANSFER(p1_xc,p1)

CALL copy_scale_paux( p1,p2,frac )

RETURN
END

!===============================================================================

SUBROUTINE gauss_int( zbar,zlow,ztop,sz,szz,tfac,q0,delz,szzo )

USE constants_fd

!------ compute section mass fraction and vertical moments

IMPLICIT NONE

REAL, PARAMETER :: EPS  = 1.E-2

REAL, INTENT( IN  ) :: zbar
REAL, INTENT( IN  ) :: zlow
REAL, INTENT( IN  ) :: ztop
REAL, INTENT( IN  ) :: sz
REAL, INTENT( IN  ) :: szz
REAL, INTENT( IN  ) :: tfac
REAL, INTENT( OUT ) :: q0
REAL, INTENT( OUT ) :: delz
REAL, INTENT( OUT ) :: szzo

REAL t1, t2, e1, e2, q1, q2

REAL, EXTERNAL :: erfc

t1   = (zlow-zbar)*tfac
t2   = (ztop-zbar)*tfac
e1   = EXP(-t1**2)
e2   = EXP(-t2**2)
q0   = 0.5*(erfc( t1 ) - erfc( t2 ))

IF( q0 < EPS )THEN
  q0   = 0.
  delz = 0.
  szzo = 1.E+10
  GOTO 9999
END IF

q1   = (e1 - e2) / (SQRT2*SQRTPI)
q2   = (t1*e1 - t2*e2) / SQRTPI

delz = sz*q1/q0
delz = MAX(MIN(delz,ztop-zbar),zlow-zbar)
szzo = szz*(1.+q2/q0-(q1/q0)**2)
IF( szzo <= 0. )szzo = 0.0833333*(ztop-zlow)**2

9999 CONTINUE

RETURN
END

!===============================================================================

SUBROUTINE refl_int( zbar,zbari,zlow,ztop,sz,szz,delz,frac,tfac )

!------ integrate reflected puff at z=zbar and combine with existing puff

IMPLICIT NONE

REAL, INTENT( IN    ) :: zbar
REAL, INTENT( IN    ) :: zbari
REAL, INTENT( IN    ) :: zlow, ztop
REAL, INTENT( IN    ) :: sz
REAL, INTENT( INOUT ) :: szz
REAL, INTENT( INOUT ) :: delz
REAL, INTENT( INOUT ) :: frac
REAL, INTENT( IN    ) :: tfac

REAL szzi, fracr, delr, szzr, ftot, temz

szzi = sz*sz

CALL gauss_int( zbar,zlow,ztop,sz,szzi,tfac,fracr,delr,szzr )

IF( fracr > 0. )THEN

  ftot = frac + fracr
  delr = delr + zbar - zbari
  temz = (frac*delz + fracr*delr)/ftot
  szz  = (frac*(delz*delz+szz) + fracr*(delr*delr+szzr))/ftot - temz**2
  delz = temz

  frac = ftot

END IF

RETURN
END

