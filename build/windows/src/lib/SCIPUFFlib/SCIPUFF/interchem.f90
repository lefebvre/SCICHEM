!*******************************************************************************
!$RCSfile$
!$Revision$
!$Date$
!*******************************************************************************
SUBROUTINE InterChem( ipuf,jpuf,puffi,puffj )

USE scipuff_fi
USE chem_fi
USE inter_fi

IMPLICIT NONE

INTEGER,          INTENT( IN ) :: ipuf, jpuf          ! -- puff numbers
TYPE( puff_str ), INTENT( IN ) :: puffi, puffj

INTEGER is, imax
REAL    fmaxi, fmaxj, pvol

IF( chem%nStar <= 0 )RETURN

!====   Loop over multicomponents species and set correlations

pvol = PI3*SQRT(puffj%det)
IF( pvol ==  0. )THEN
  fmaxj = 0.0
ELSE
  CALL get_fmax( puffi,puffj,fmaxj )
  fmaxj = fmaxj/pvol
END IF

pvol = PI3*SQRT(puffi%det)
IF( pvol ==  0. )THEN
  fmaxi = 0.0
ELSE
  CALL get_fmax( puffj,puffi,fmaxi )
  fmaxi = fmaxi/pvol
END IF

imax = chem%imax

DO is = 1,chem%nStar
  chem%star(is)%s%conc = chem%star(is)%s%conc + fac*chem%star(is)%s%mass2
  chem%cmax(ipuf-imax,is) = chem%cmax(ipuf-imax,is) + fmaxj*chem%star(is)%s%mass2
  IF( .NOT.lstatic )THEN
    chem%star(is)%s%conc2 = chem%star(is)%s%conc2 + fac*chem%star(is)%s%mass
    chem%cmax(jpuf-imax,is) = chem%cmax(jpuf-imax,is) + fmaxi*chem%star(is)%s%mass
  END IF
END DO

!===    Save inverse volume

vol = vol + fac
IF( .NOT.lstatic )vol2 = vol2 + fac

IF( ipuf == jpuf )vself = fac

RETURN
END

!------------------------------------------------------------------------------

SUBROUTINE GetChemInter1( ID,p )

USE scipuff_fi
USE chem_fi

IMPLICIT NONE

INTEGER,          INTENT( IN ) :: ID
TYPE( puff_str ), INTENT( IN ) :: p

INTEGER i, is, ipaux, nmc

chem => chemMC(ID)
IF( chem%nStar <= 0 )RETURN

species => chem%species

ipaux = typeID(p%ityp)%ipmc - 1

nmc = chem%nFast + chem%nSlow + chem%nParticle + chem%nEquilibrium

DO i = 1,chem%nStar
  is = chem%star(i)%s%ID
  chem%star(i)%s%mass = p%aux(ipaux+is)
  chem%star(i)%s%conc = p%aux(ipaux+i+nmc)/p%c
END DO

vol   = p%aux(ipaux+nmc+chem%nStar+1)/p%c
vself = p%aux(ipaux+nmc+chem%nStar+4)/p%c

RETURN
END

!------------------------------------------------------------------------------

SUBROUTINE GetChemInter2( p )

USE scipuff_fi
USE chem_fi

IMPLICIT NONE

TYPE( puff_str ), INTENT( IN ) :: p

INTEGER i, is, ipaux, nmc

IF( chem%nStar <= 0 )RETURN

ipaux = typeID(p%ityp)%ipmc - 1

nmc = chem%nFast + chem%nSlow + chem%nParticle + chem%nEquilibrium

DO i = 1,chem%nStar
  is = chem%star(i)%s%ID
  chem%star(i)%s%mass2 = p%aux(ipaux+is)
  chem%star(i)%s%conc2 = p%aux(ipaux+i+nmc)/p%c
END DO

vol2 = p%aux(ipaux+nmc+chem%nStar+1)/p%c

RETURN
END

!------------------------------------------------------------------------------

SUBROUTINE PutChemInter1( p )

USE scipuff_fi
USE chem_fi

IMPLICIT NONE

TYPE( puff_str ), INTENT( INOUT ) :: p

INTEGER i, is, ipaux, nmc

IF( chem%nStar <= 0 )RETURN

ipaux = typeID(p%ityp)%ipmc - 1

nmc = chem%nFast + chem%nSlow + chem%nParticle + chem%nEquilibrium

DO i = 1,chem%nStar
  is = chem%star(i)%s%ID
  p%aux(ipaux+is)    = chem%star(i)%s%mass
  p%aux(ipaux+i+nmc) = chem%star(i)%s%conc*p%c
END DO

p%aux(ipaux+nmc+chem%nStar+1) = vol*p%c
p%aux(ipaux+nmc+chem%nStar+4) = vself*p%c

RETURN
END

!------------------------------------------------------------------------------

SUBROUTINE PutChemInter2( p )

USE scipuff_fi
USE chem_fi

IMPLICIT NONE

TYPE( puff_str ), INTENT( INOUT ) :: p

INTEGER i, is, ipaux, nmc

IF( chem%nStar <= 0 )RETURN

ipaux = typeID(p%ityp)%ipmc - 1

nmc = chem%nFast + chem%nSlow + chem%nParticle + chem%nEquilibrium

DO i = 1,chem%nStar
  is = chem%star(i)%s%ID
  p%aux(ipaux+is)    = chem%star(i)%s%mass2
  p%aux(ipaux+i+nmc) = chem%star(i)%s%conc2*p%c
END DO

p%aux(ipaux+nmc+chem%nStar+1) = vol2*p%c

RETURN
END

!------------------------------------------------------------------------------

SUBROUTINE InitChemCont1( p )

USE scipuff_fi
USE chem_fi

IMPLICIT NONE

TYPE( puff_str ), INTENT( INOUT ) :: p

INTEGER i, is, ipaux, nmc

ipaux = typeID(p%ityp)%ipmc - 1

nmc = chem%nFast + chem%nSlow + chem%nParticle + chem%nEquilibrium

DO i = 1,chem%nStar
  is = chem%star(i)%s%ID
  p%aux(ipaux+is)    = chem%star(i)%s%mass
  p%aux(ipaux+i+nmc) = chem%star(i)%s%conc*p%c
END DO

p%aux(ipaux+nmc+chem%nStar+1) = 2.0*vol*p%c
p%aux(ipaux+nmc+chem%nStar+4) = 2.0*vself*p%c

RETURN
END

!------------------------------------------------------------------------------

SUBROUTINE get_fmax( puffi,puffj,fmax )

USE scipuff_fi

IMPLICIT NONE

TYPE( puff_str ), INTENT( IN  ) :: puffi, puffj
REAL,             INTENT( OUT ) :: fmax

REAL, PARAMETER :: ARGMAX = 20.0

REAL    xmap, ymap, xbar, ybar, vbar
REAL    deth, rat, hz, hx, hy, zp, znrm, zfac, xs, ys, vs
REAL    xp, yp, faci, arg, zr, facs
LOGICAL lset

REAL,    DIMENSION(3) :: xr, xnrm
REAL(8), DIMENSION(7) :: asig

INTEGER, EXTERNAL :: getPuffifld

!------ calculate jpuf-Gaussian at ipuf-Centroid location

fmax = 0.0

CALL mapfac( puffj%xbar,puffj%ybar,xmap,ymap )

xbar = puffj%xbar
ybar = puffj%ybar
vbar = puffj%zbar

deth = puffj%axx*puffj%ayy - puffj%axy**2
rat  = 0.5/(puffj%det*deth)

xs = puffi%xbar
ys = puffi%ybar
vs = puffi%zbar
xp = (xs-xbar)/xmap
yp = (ys-ybar)/ymap
zp = (vs-vbar)

CALL zi_reflect( vbar,puffj%zc,puffj%zc,vs,rat,faci )

IF( lter )THEN
  CALL get_topogIn( puffj%xbar,puffj%ybar,hz,hx,hy,getPuffifld(puffj) )
  CALL get_asig( puffj,asig )
  CALL grnd_reflect( vbar-hz,asig,hx,hy,xr,xnrm,deth,znrm )
  zfac = 0.5*znrm/(puffj%det*deth)
  CALL get_topogIn( xs,ys,hz,hx,hy,getPuffifld(puffj) )
  lset =  hz <= vs
ELSE
  lset = .TRUE.
END IF

IF( lset )THEN
  arg = puffj%axx*xp*xp+2.*puffj%axy*xp*yp+2.*puffj%axz*xp*zp &
      + puffj%ayy*yp*yp+2.*puffj%ayz*yp*zp+puffj%azz*zp*zp
  IF( arg < ARGMAX )THEN
    IF( lter )THEN
      zr   = xnrm(1)*(xp-xr(1)) + xnrm(2)*(yp-xr(2)) + xnrm(3)*(zp-xr(3))
      zr   = MAX(zr,0.)
      facs = EXP(zfac*zr)
    ELSE
      facs = EXP(-vs*vbar*rat)
    END IF
    fmax  = EXP(-arg)*(1.+facs)*(1.+faci)
  END IF
END IF

RETURN
END

!------------------------------------------------------------------------------

SUBROUTINE InitChemCmax( ID,nn,ioffset )

!--- Initialize cmax array for chem structures, nn puffs starting at ioffset

USE chem_fi
USE error_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: ID
INTEGER, INTENT( IN ) :: nn
INTEGER, INTENT( IN ) :: ioffset

INTEGER alloc_stat, i, j

IF( chemMC(ID)%nStar <= 0 )RETURN

IF( ASSOCIATED(chemMC(ID)%cmax) )DEALLOCATE( chemMC(ID)%cmax,STAT=alloc_stat )

ALLOCATE( chemMC(ID)%cmax(nn,chemMC(ID)%nStar),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  nError   = IV_ERROR
  eRoutine = 'InitChemCmax'
  eMessage = 'Error allocating work arrays'
  GOTO 9999
END IF

DO i = 1,nn
  DO j = 1,chemMC(ID)%nStar
    chemMC(ID)%cmax(i,j) = 0.0
  END DO
END DO

chemMC(ID)%imax = ioffset - 1

9999 CONTINUE

RETURN
END

!------------------------------------------------------------------------------

SUBROUTINE ClearChemCmax( ID )

!--- Clear cmax array for chem structures

USE chem_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: ID

INTEGER alloc_stat

IF( ASSOCIATED(chemMC(ID)%cmax) )DEALLOCATE( chemMC(ID)%cmax,STAT=alloc_stat )

chemMC(ID)%imax = 0

RETURN
END

!------------------------------------------------------------------------------

SUBROUTINE ResetCstar( ID,ipuf,puffi )

USE scipuff_fi
USE chem_fi

IMPLICIT NONE

INTEGER,          INTENT( IN    ) :: ID
INTEGER,          INTENT( IN    ) :: ipuf
TYPE( puff_str) , INTENT( INOUT ) :: puffi

INTEGER  is, ispuf

REAL, PARAMETER :: TOP2GAUSS = 0.8, R0=0.75

REAL rat, amb, hp, hx, hy

INTEGER, EXTERNAL :: getPuffifld

INTERFACE
  SUBROUTINE SetChemAmbient( xIn,yIn,zIn,t,leq,lmet,lplot )
    REAL,    INTENT( IN ) :: xIn, yIn, zIn, t
    LOGICAL, INTENT( IN ) :: leq
    LOGICAL, INTENT( IN ) :: lmet
    LOGICAL, OPTIONAL, INTENT( IN ) :: lplot
  END SUBROUTINE
END INTERFACE


! Use 3 Gaussians for sampler and skip ResetCstar for now. -BNC
RETURN

IF( chemMC(ID)%nStar <= 0 )RETURN

CALL GetChemInter1( ID,puffi )

IF( lter )THEN
  CALL get_topogIn( puffi%xbar,puffi%ybar,hp,hx,hy,getPuffifld(puffi) )
ELSE
  hp = 0.0
END IF
CALL SetChemAmbient( puffi%xbar,puffi%ybar,puffi%zbar-hp,t,.FALSE.,.TRUE. )
IF( nError /= NO_ERROR )GOTO 9999

ispuf = ipuf - chem%imax

DO is = 1,chem%nStar
  amb = chem%star(is)%s%amb
  IF( amb /= 0.0 )THEN
    rat = -TOP2GAUSS*chem%cmax(ispuf,is)
    IF( rat > R0*amb )THEN
      IF( rat < amb )THEN
        rat = rat/amb
        chem%star(is)%s%conc = chem%star(is)%s%conc &
                             + (R0 - rat)*(chem%star(is)%s%conc + amb)/(1. - R0)
      ELSE
        chem%star(is)%s%conc = -rat
      END IF
    END IF
  ELSE IF( chem%cmax(ispuf,is) < 0.0 )THEN
    chem%star(is)%s%conc = chem%cmax(ispuf,is)
  END IF
END DO

CALL PutChemInter1( puffi )

9999 CONTINUE

RETURN
END

!------------------------------------------------------------------------------

SUBROUTINE GetChemComp( ID,compName,ss )

USE sampler_fd
USE chem_fi
USE error_fi
USE chem_aqaer_fi

IMPLICIT NONE

INTEGER,                    INTENT( IN    ) :: ID
CHARACTER(*), DIMENSION(*), INTENT( IN    ) :: compName
TYPE( sensor ),             INTENT( INOUT ) :: ss

INTEGER i, j, nmc, nComp, alloc_stat

nmc   = ss%nmc
nComp = chemMC(ID)%nFast + chemMC(ID)%nSlow + chemMC(ID)%nParticle + chemMC(ID)%nEquilibrium

IF( nmc == -1 )ss%nmc = nComp

ALLOCATE( ss%mcID(ss%nmc),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'GetChemComp'
  eMessage = 'Error allocating multicomponent array'
  GOTO 9999
END IF

IF( nmc == -1 )THEN

  DO i = 1,nComp
    ss%mcID(i) = i
  END DO

ELSE

  DO j = 1,ss%nmc

    ss%mcID(j) = 0

    DO i = 1,nComp
      IF( TRIM(chemMC(ID)%species(i)%name) == TRIM(compName(j)) )THEN
        ss%mcID(j) = i
        EXIT
      END IF
    END DO

    IF( ss%mcID(j) == 0 )THEN
      nError   = UK_ERROR
      eRoutine = 'GetChemComp'
      eMessage = 'Multicomponent name not found'
      eInform  = TRIM(compName(j))
      GOTO 9999
    END IF

  END DO

END IF

IF (laerosol) THEN

  DO j = 1,ss%nmc

    SELECT CASE( TRIM(compName(j)) )
      CASE ('ASO4J')
        IDSO4J = j
      CASE ('ASO4I')
        IDSO4I = j
      CASE ('ANO3J')
        IDNO3J = j
      CASE ('ANO3I')
        IDNO3I = j
      CASE ('ANH4J')
        IDNH4J = j
      CASE ('ANH4I')
        IDNH4I = j
      CASE ('ANAJ')
        IDNAJ = j
      CASE ('ANAI')
        IDNAI = j
      CASE ('ACLJ')
        IDCLJ = j
      CASE ('ACLI')
        IDCLI = j
      CASE ('HNO3')
        IDHNO3 = j
      CASE ('NH3')
        IDNH3 = j
      CASE ('HCL')
        IDHCL = j
    END SELECT

  END DO

  IF ( IDSO4J == -1 .OR. IDSO4I == -1 .OR. IDNO3J == -1 .OR. IDNO3I == -1 .OR. &
       IDNH4J == -1 .OR. IDNH4I == -1 .OR. IDHNO3 == -1 .OR. IDNH3  == -1  ) THEN
    nError   = UK_ERROR
    eRoutine = 'GetChemComp'
    eMessage = 'Inorganic aerosol species not found'
    eInform  = 'Check sam file for ISOROPPIA species ASO4J,ASO4I,ANO3J,ANO3I,ANH4J,ANH4I,HNO3 or NH3'
    GOTO 9999
  END IF

END IF

9999 CONTINUE

RETURN
END

!------------------------------------------------------------------------------

SUBROUTINE GetChemCompName( ID,ispecie,compName,unitName )

USE chem_fi
USE error_fi

IMPLICIT NONE

INTEGER,      INTENT( IN    ) :: ID
INTEGER,      INTENT( IN    ) :: ispecie
CHARACTER(*), INTENT( OUT   ) :: compName
CHARACTER(*), INTENT( OUT   ) :: unitName

INTEGER nComp

compName = 'Unknown'

!------ Check indices

IF( ID > SIZE(chemMC) )GOTO 9999

nComp = chemMC(ID)%nFast + chemMC(ID)%nSlow + chemMC(ID)%nParticle + chemMC(ID)%nEquilibrium

IF( ispecie > nComp )GOTO 9999

!------ Set component name

compName = TRIM(chemMC(ID)%species(ispecie)%name)

!------ Set units
SELECT CASE( chemMC(ID)%oUnits )
  CASE( UNIT_UGM3 )
    unitName = 'ug/m3'
  CASE( UNIT_PPM )
    unitName = 'ppm'
  CASE DEFAULT
    unitName = ' '
END SELECT

9999 CONTINUE

RETURN
END

