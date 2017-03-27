!*******************************************************************************
!$RCSfile$
!$Revision$
!$Date$
!*******************************************************************************
SUBROUTINE puff_release()

USE scipuff_fi
USE files_fi
USE cont_rel_fi
USE cont_rel_functions

!   Releases all puffs at time t

IMPLICIT NONE

INTEGER mpuf, mcrel, crmode
INTEGER n1, n2

CHARACTER(128) cmsg,cmsg2,cmsg3
CHARACTER(8)   ctmp

CALL start_clock()

mpuf  = npuf
mcrel = count_nrel()

DO WHILE( trel < t+delt .AND. ActiveSource )

  init_source = init_source + 1
  cmsg  = CHAR(0)
  cmsg2 = CHAR(0)
  IF( LEN_TRIM(relDisplay) > 0 )THEN
    WRITE(cmsg3,'(A,I5,''('',A,'')'')')'Preparing source',init_source,TRIM(relDisplay)
  ELSE
    WRITE(cmsg3,'(A,I5)')'Preparing source',init_source
  END IF
  CALL write_progress( cmsg,cmsg2,cmsg3 )

!------ process scn

  crmode = CRMODE_START

  CALL process_scn( crmode )
  IF( nError /= NO_ERROR )GOTO 9999

  t_old_r = trel

  CALL get_scn( lun_scn,file_scn )
  IF( nError /= NO_ERROR )GOTO 9999

END DO

!------ Set interactions for new releases

IF( npuf > mpuf )THEN

  WRITE(ctmp,'(I8)')npuf-mpuf
  ctmp  = ADJUSTL(ctmp)
  cmsg  = CHAR(0)
  cmsg2 = CHAR(0)
  WRITE(cmsg3,'(A)')'Initializing '//TRIM(ctmp)//' new puffs'
  CALL write_progress( cmsg,cmsg2,cmsg3 )

  n1 = mpuf + 1
  n2 = npuf
  CALL set_rel( n1,n2,0 )
  IF( nError /= NO_ERROR )GOTO 9999

END IF

IF( count_nrel() > mcrel )THEN

  WRITE(ctmp,'(I8)')count_nrel()-mcrel
  ctmp  = ADJUSTL(ctmp)
  cmsg  = CHAR(0)
  cmsg2 = CHAR(0)
  WRITE(cmsg3,'(A)')'Initializing '//TRIM(ctmp)//' new releases'
  CALL write_progress( cmsg,cmsg2,cmsg3 )

  CALL InteractContinuousReleases( .TRUE. )
  IF( nError /= NO_ERROR )GOTO 9999

END IF

9999 CONTINUE

CALL clear_random_loc()

CALL stop_clock()

RETURN
END

!===============================================================================

SUBROUTINE get_scn( lun,file )

USE scipuff_fi

IMPLICIT NONE

INTEGER,      INTENT( IN ) :: lun
CHARACTER(*), INTENT( IN ) :: file

INTEGER i

!------ defaults

xrel     = DEF_VAL_D
yrel     = DEF_VAL_D
zrel     = 0.
trel     = 0.
urel     = 0.
vrel     = 0.
wrel     = 0.
tdur     = 0.
cmass    = 0.
umom     = DEF_VAL_R
vmom     = DEF_VAL_R
wmom     = 0.
buoy     = 0.
size_rel = 0.
sigx     = 0.
sigy     = 0.
sigz     = 0.
subgroup = 1

rel_dist = NOT_SET_I
DO i = 1,SCIPUFF_MAXRELPARAM
  rel_param(i) = NOT_SET_R
END DO

reltyp   = ' '
relmat   = 'UNKNOWN'
name_rel = ' '

relName    = '<empty>'
relDisplay = '<empty>'

opid = 0
DO i = 1,SCIPUFF_STATUS_ARRAY_SIZE
  opmod(i) = 0
END DO

relStatus  = NOT_SET_I

RelLoop : DO

!------ read and check input

  CALL ReadNamelistScn( lun )

  IF( nError == EOF_ERROR )THEN

    CALL init_error()
    trel   = 1.E+36
    tdur   = 1.E+36
    ActiveSource = .FALSE.

  ELSE IF( nError /= NO_ERROR )THEN

    eRoutine = 'get_scn'
    CALL ReportFileName( eInform,'File=',file )
    GOTO 9999

  ELSE IF( reltyp(1:1) =='C' .OR. reltyp(1:1) =='I' )THEN

    IF( lmap == I_LATLON )THEN
      CALL check_lonD( xrel,xmin,xmax )
      IF( nError /= NO_ERROR )THEN
        eInform = 'Setting source location'
        GOTO 9999
      END IF
    END IF

    CALL LoadRelease( currentRelease )
    IF( nError /= NO_ERROR )THEN
      nError   = RD_ERROR
      eRoutine = 'get_scn'
      eMessage = 'Error loading source data into release structure'
      CALL ReportFileName( eInform,'File=',file )
      GOTO 9999
    END IF

  ELSE

    nError   = RD_ERROR
    eRoutine = 'get_scn'
    eMessage = 'Release type must be I or C in SCIPUFF scenario'
    CALL ReportFileName( eInform,'File=',file )
    GOTO 9999

  END IF

  EXIT RelLoop

END DO RelLoop

9999 CONTINUE

RETURN
END

!===============================================================================

SUBROUTINE copy_scn()

USE scipuff_fi

IMPLICIT NONE

INTEGER i

!------ copy standard (non-interactive) source back into common

xrel     = xrel_scn
yrel     = yrel_scn
zrel     = zrel_scn
trel     = trel_scn
urel     = urel_scn
vrel     = vrel_scn
wrel     = wrel_scn
tdur     = tdur_scn
cmass    = cmass_scn
umom     = umom_scn
vmom     = vmom_scn
wmom     = wmom_scn
buoy     = buoy_scn
size_rel = size_scn
sigx     = sigx_scn
sigy     = sigy_scn
sigz     = sigz_scn
sigRxy   = sigRxy_scn
sigRxz   = sigRxz_scn
sigRyz   = sigRyz_scn
subgroup = subgroup_scn

rel_dist = rel_dist_scn
DO i = 1,SCIPUFF_MAXRELPARAM
  rel_param(i) = rel_param_scn(i)
END DO

reltyp   = reltyp_scn
relmat   = relmat_scn
name_rel = name_rel_scn

opid = opid_scn
DO i = 1,SCIPUFF_STATUS_ARRAY_SIZE
  opmod(i) = opmod_scn(i)
END DO

relName    = relName_scn
relDisplay = relDisplay_scn
relStatus  = relStatus_scn

RETURN
END

!===============================================================================

SUBROUTINE save_scn()

USE scipuff_fi

IMPLICIT NONE

INTEGER i

!------ save standard (non-interactive) source paramtersn

xrel_scn     = xrel
yrel_scn     = yrel
zrel_scn     = zrel
trel_scn     = trel
urel_scn     = urel
vrel_scn     = vrel
wrel_scn     = wrel
tdur_scn     = tdur
cmass_scn    = cmass
umom_scn     = umom
vmom_scn     = vmom
wmom_scn     = wmom
buoy_scn     = buoy
size_scn     = size_rel
sigx_scn     = sigx
sigy_scn     = sigy
sigz_scn     = sigz
sigRxy_scn   = sigRxy
sigRxz_scn   = sigRxz
sigRyz_scn   = sigRyz
subgroup_scn = subgroup

rel_dist_scn = rel_dist
DO i = 1,SCIPUFF_MAXRELPARAM
  rel_param_scn(i) = rel_param(i)
END DO

reltyp_scn   = reltyp
relmat_scn   = relmat
name_rel_scn = name_rel

opid_scn = opid
DO i = 1,SCIPUFF_STATUS_ARRAY_SIZE
  opmod_scn(i) = opmod(i)
END DO

relName_scn    = relName
relDisplay_scn = relDisplay
relStatus_scn  = relStatus

RETURN
END

!============================================================================

SUBROUTINE init_random_loc()

USE scipuff_fi
USE relparam_fd
USE constants_fd

IMPLICIT NONE

INTEGER ireal, i, alloc_stat
REAL    spread, xmap, ymap, ranx, rad, ang
REAL    spreadT, spreadV, cs, sn, rx, ry

INTEGER, DIMENSION(:), ALLOCATABLE :: iseed

REAL, EXTERNAL :: sind, cosd, erfcinv

IF( reltyp(1:1) == 'I' .AND. rel_param(REL_RAND_INDX) /= NOT_SET_R )THEN

  nRandom = NINT(rel_param(REL_RAND_INDX))

!---- Do Nothing if only 1 realization

  IF( nRandom > 1 )THEN

    ALLOCATE( xRandom(nRandom),yRandom(nRandom),zRandom(nRandom),STAT=alloc_stat )
    IF( alloc_stat /= 0 )THEN
      nError = IV_ERROR
      eRoutine = 'init_random_loc'
      eMessage = 'Uanble to allocate work arrays'
      WRITE(eInform,*)'Size =',nRandom
      GOTO 9999
    END IF

    spread = rel_param(REL_SPREAD_INDX)
    IF( rel_param(REL_SEED_INDX) <= 0.0 .OR. &
        rel_param(REL_SEED_INDX) == DEF_VAL_R .OR. &
        rel_param(REL_SEED_INDX) == NOT_SET_R .OR. &
        rel_param(REL_SEED_INDX) == DEFERRED_R )THEN
      nError = IV_ERROR
      eRoutine = 'init_random_loc'
      eMessage = 'Invalid random seed'
      WRITE(eInform,*)'Seed =',rel_param(REL_SEED_INDX)
      GOTO 9999
    END IF

    CALL RANDOM_SEED( SIZE=i )  ! I is set to the size of  the seed array
    ALLOCATE( iseed(i),STAT=alloc_stat )
    IF( alloc_stat /= 0 )THEN
      nError = IV_ERROR
      eRoutine = 'init_random_loc'
      eMessage = 'Error allocating work arrays (iseed)'
      GOTO 9999
    END IF
    iseed(1) = NINT(rel_param(REL_SEED_INDX))
    DO i = 2,SIZE(iseed)
      iseed(i) = 2*iseed(i-1) + 1
    END DO
    CALL RANDOM_SEED( PUT=iseed )

!-----  Generate random locations

    IF( rel_param(REL_SPREADT_INDX) == NOT_SET_R )THEN

      CALL mapfac( SNGL(xrel),SNGL(yrel),xmap,ymap )

      DO ireal = 1,nRandom
        CALL RANDOM_NUMBER( HARVEST=ranx )
        rad  = spread*SQRT(ranx) !circle
        CALL RANDOM_NUMBER( HARVEST=ranx )
        ang  = 360.*ranx !circle
        xRandom(ireal) = DBLE(rad*xmap*cosd( ang )) !circle
        yRandom(ireal) = DBLE(rad*ymap*sind( ang )) !circle
        zRandom(ireal) = 0.0
      END DO

    ELSE

      CALL mapfac( SNGL(xrel),SNGL(yrel),xmap,ymap )

!---- Convert input values to Gaussian std deviations

      spread  = spread/0.675
      spreadT = rel_param(REL_SPREADT_INDX)/0.675
      spreadV = rel_param(REL_SPREADV_INDX)/0.675
      cs      = cosd(rel_param(REL_RANDIR_INDX))
      sn      = sind(rel_param(REL_RANDIR_INDX))

      DO ireal = 1,nRandom
        CALL RANDOM_NUMBER( HARVEST=ranx )
        rx = SQRT2*spread*erfcinv( 2.*ranx )
        CALL RANDOM_NUMBER( HARVEST=ranx )
        ry = SQRT2*spreadT*erfcinv( 2.*ranx )
        CALL RANDOM_NUMBER( HARVEST=ranx )
        zRandom(ireal) = SQRT2*spreadV*erfcinv( 2.*ranx )
        xRandom(ireal) = DBLE(xmap*(rx*sn - ry*cs ))
        yRandom(ireal) = DBLE(ymap*(ry*sn + rx*cs ))
      END DO

    END IF

  ELSE

    nRandom = 0

  END IF

ELSE

  nRandom = 0

END IF

9999 CONTINUE

DEALLOCATE( iseed,STAT=alloc_stat )

RETURN
END

!============================================================================

SUBROUTINE clear_random_loc()

USE scipuff_fi

IMPLICIT NONE

INTEGER alloc_stat

nRandom = 0

IF( ALLOCATED(xRandom) )DEALLOCATE( xRandom,STAT=alloc_stat )
IF( ALLOCATED(yRandom) )DEALLOCATE( yRandom,STAT=alloc_stat )
IF( ALLOCATED(zRandom) )DEALLOCATE( zRandom,STAT=alloc_stat )

RETURN
END

!===============================================================================

SUBROUTINE init_tlev( p,pd,ilev,xmap,ymap )

USE scipuff_fi
USE met_fi
USE surface_fi
USE files_fi
USE PtrGrdStrItf
USE sagdef_fd

IMPLICIT NONE

TYPE( puff_str ),      INTENT( IN )  :: p
TYPE( puff_dynamics ), INTENT( IN )  :: pd
INTEGER,               INTENT( OUT ) :: ilev
REAL,                  INTENT( IN )  :: xmap, ymap

TYPE( puff_material )  pmat
TYPE( part_material )  pmatpart
TYPE( liquid_material )pmatliq

LOGICAL lzinv, lsrf, ldos, lscale, ltest
INTEGER jtyp, icls, ifld
INTEGER imat
REAL    hp, hx, hy, ztest, dels
REAL    si, sv, sx, qosi_cc, dtcc, rhoa
REAL    drat, dtw, vel2, sigu, dta, dd, dtk, dtp, dtb
REAL    delg, xlam
REAL    wdyn, bdyn, wp2, vp2, qqvrtx, qpi, qosi_p, qqbi, dsrf
REAL    rhod, dbar, aspect
REAL    udyn, vdyn

LOGICAL, EXTERNAL :: IsParticle
LOGICAL, EXTERNAL :: IsWetParticle,IsLiquid
REAL,    EXTERNAL :: ufall, fun_rhoa
INTEGER, EXTERNAL :: time_level, getPuffifld

jtyp = p%ityp
icls = typeID(jtyp)%icls
ifld = getPuffifld( p )

CALL get_met( p%xbar,p%ybar,p%zbar,p%szz,p%zc,0,inField=ifld )

IF( lter )THEN
  CALL get_topogIn( p%xbar,p%ybar,hp,hx,hy,ifld )
ELSE
  hp = 0.0
END if

lzinv = (p%zbar <= zinv) .AND. lbl

vel2 = ub*ub + vb*vb
si   = p%si
sv   = MAX(zruf,p%sv)

IF( lsv_oper )CALL reset_lsv( si )

IF( t_avg /= DEF_VAL_R )THEN
  CALL turb_rescale( si,sv,vel2 )
END IF

qqb  = uub + vvb + 2.*(uubl + vvbl) + wwbh
qqbi = (     uub  + vvb )*MIN(1.0,(si/sby )**TwoThirds) &
       + (2.*uubl + wwbh)*MIN(1.0,(si/sbls)**TwoThirds) &
       + (2.*vvbl       )*MIN(1.0,(si/sbl )**TwoThirds)

IF( p%axx > SMALL )THEN
  xlam = 1.0/(p%axx + p%ayy + SQRT((p%axx-p%ayy)**2+4.*p%axy**2))
ELSE
  xlam = p%syy
END IF
sx = MAX(si,SQRT(xlam))

qosi_cc = MAX(0.5*SQRT(qqb)/sx,0.25*SQRT(qqbi)/si)

!------ account for dynamics

IF( dynamic )THEN
  udyn = pd%ucp/p%c
  vdyn = pd%vcp/p%c
  wdyn = pd%wcp/p%c
  bdyn = g*MIN(ABS((pd%ctp/thb - pd%bcp)/p%c),1.)
  IF( bdyn > SMALL )THEN
    dtb = 0.5*SQRT(sv/bdyn)
  ELSE
    dtb = delt
  END IF
ELSE
  udyn = 0.
  vdyn = 0.
  wdyn = 0.
  dtb  = delt
END IF
vp2    = vel2 + uub + vvb + SMALL
wp2    = udyn*udyn + vdyn*vdyn + wdyn*wdyn
qqvrtx = CVRTX*(vp2*wp2+(ub*vdyn-vb*udyn)**2)/(vp2+wp2)

qpi    = SQRT(CQB*wp2 + qqvrtx)
qosi_p = 0.5*qpi/si

qosi_cc = qosi_cc + qosi_p

IF( qosi_cc > SMALL )THEN
  dtcc = 0.25/qosi_cc
ELSE
  dtcc = delt
END IF

IF( IsParticle(icls) .OR. IsWetParticle(icls) )THEN

  CALL get_puff_material( jtyp,pmat )
  pmatpart = TRANSFER(pmat,pmatpart)
  rhoa = fun_rhoa( tb,pb ) !rhoair*(pb/PSURF)*(283.2/tb) !*(pb**KAPPAC)*273.2/tb
  drat = ufall( rhoa,pmatpart%rho,rmuair,pmatpart%dbar )

ELSE IF( IsLiquid(icls) )THEN

  CALL get_puff_material( jtyp,pmat )
  pmatliq = TRANSFER(pmat,pmatliq)
  rhoa = fun_rhoa( tb,pb ) !rhoair*(pb/PSURF)*(283.2/tb) !*(pb**KAPPAC)*273.2/tb
  rhod = pmatliq%rho
  dbar = pmatliq%dbar
  CALL drop_deform( rhod,rhoa,dbar,pmatliq%st,aspect )
  rhod = rhod/(aspect*aspect)
  drat = ufall( rhoa,rhod,rmuair,dbar )

ELSE

  drat = 0.

END IF

IF( drat > SMALL )THEN
  dtw = dzg/drat
ELSE
  dtw = delt
END IF

IF( vel2 > SMALL )THEN
  lsrf   = srf_puff(jtyp,1)%nblocks > 0
  ldos   = srf_puff(jtyp,2)%nblocks > 0
  lscale = lsrf .OR. ldos .OR. lsmp
  delg   = MIN(MetGrid(ifld)%dx/xmap,MetGrid(ifld)%dy/ymap)
  IF( lscale )THEN
    IF( lsmp )THEN
      ltest = .TRUE.
      dsrf  = 0.
    ELSE
      IF( lter )THEN
        dels = fac_rfl*(ABS(hx)*SQRT(p%sxx) + ABS(hy)*SQRT(p%syy))
      ELSE
        dels = 0.0
      END IF
      IF( lsrf .OR. z_dosage == 0. )THEN
        ztest = p%zbar - hp - dels
      ELSE
        ztest = HUGE(0.)
      END IF
      IF( ldos .AND. z_dosage > 0. )THEN
        ztest = MIN(ztest,ABS(p%zbar-z_dosage-hp)-dels)
      END IF
      ztest = ztest - fac_rfl*SQRT(p%szz)
      ltest = (ztest + MIN(wdyn,0.)*delt) < 0.
      IF( ltest )THEN
        IF( lsrf )THEN
          dsrf = Psrfdep%delmin
        ELSE
          dsrf = 0.
        END IF
        IF( ldos )THEN
          IF( BTEST(run_mode,REVERSE_MODE) )THEN
            imat = typeID(p%ityp)%imat
            Psrfdos => SAG_PtrGrdStr( srfdosAdj(imat) ) ! Associate "local" grid structure pointer
          END IF
          dsrf = MAX(Psrfdos%delmin,dsrf)
        END IF
      END IF
    END IF
    IF( ltest )THEN
      IF( p%axx > SMALL )THEN
        sigu = ub*ub*p%axx + 2.*ub*vb*p%axy + vb*vb*p%ayy
      ELSE
        sigu = vel2/p%syy
      END IF
      dta  = SQRT(MAX(1./sigu,dsrf*dsrf/vel2))
    ELSE
      dta = delg/SQRT(vel2)
    END IF
  ELSE
    dta = delg/SQRT(vel2)
  END IF
ELSE
  dta = delt
END IF

dd = delz2
IF( lzinv )dd = MIN(dd,0.64*(zinv-hp)**2)
IF( p%zbar-hp < sv )sv = 2.5*sv-1.5*(p%zbar-hp)  !Increase timestep for near-surface puffs (pools)
dtk = MIN(dd/(2.*MAX(difb,1.E-6)),sv/SQRT(MAX(wwbl,1.E-6)))

dtp = MIN( dtcc,dtw,dta,dtk,dtb )

ilev = time_level( dtp )
IF( nError /= NO_ERROR )THEN
  eRoutine = 'init_tlev'
  WRITE(lun_log,*,ERR=9999)'******* TIME LEVEL ERROR ********'
  WRITE(lun_log,*,ERR=9999)TRIM(eRoutine)
  WRITE(lun_log,*,ERR=9999)TRIM(eInform)
  WRITE(lun_log,*,ERR=9999)'DT(cc,w,a,k,b)=',dtcc,dtw,dta,dtk,dtb
  WRITE(lun_log,*,ERR=9999)'DELT=',delt
  WRITE(lun_log,*,ERR=9999)'LEVEL=',ilev,MAXTLV
  WRITE(lun_log,*,ERR=9999)'DZG,DRAT,DIFB,VEL2,QOSI=', &
                                           dzg,drat,difb,vel2,qosi_cc
  CALL dump_puff( 0,p )
  GOTO 9999
END IF

9999 CONTINUE

RETURN
END

!===============================================================================

SUBROUTINE set_cc( n1,n2 )

USE scipuff_fi
USE met_fi
USE inter_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: n1, n2

INTEGER ityp, ilev, ipuf
INTEGER i, icls, alloc_stat
REAL    dum, rat

LOGICAL, EXTERNAL :: IsMulti
LOGICAL, EXTERNAL :: IsLiquid, IsWetParticle
LOGICAL, EXTERNAL :: IsAerosol
INTEGER, EXTERNAL :: getPuffifld

TYPE( puff_dynamics ) pdi
TYPE( puff_totalcc  ) pti
TYPE( puff_liquid   ) pqi
TYPE( puff_material ) pmatl

CALL mapfac( puff(n1)%xbar,puff(n1)%ybar,xmap_i,ymap_i )

!----   Prepare to compute interactions

ALLOCATE( ptmp(n2),kl(n2),ku(n2),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  nError = UK_ERROR
  eRoutine = 'set_cc'
  eMessage = 'Error allocating interaction arrays'
  WRITE(eInform,'(A,I10)') 'Requested size =',n2
  GOTO 9999
END IF

kl   = -1  ! Initialize to negative number for check in loverlap
ptmp = 0.

DO i = n1,n2
  CALL siginv( puff(i) )
  CALL clear_inter( puff(i),dum )
  CALL step_clock()
END DO

IF( multicomp )THEN
  CALL InitInterMC( n1,n2 )
  IF( nError /= NO_ERROR )GOTO 9999
END IF

!----   Compute interactions for new puffs

ipufMin = n1       !Only computing interactions for new puffs

DO i = n1,n2
  ipuf = i
  CALL inter_puff( ipuf,-1,-1 )
  IF( nError /= NO_ERROR )GOTO 9999
  CALL step_clock()
END DO

!---- Loop over new puffs to initialize

DO i = n1,n2

  ityp = puff(i)%ityp
  icls = typeID(ityp)%icls

!-----    Initialize source location uncertainty first

  CALL init_loc_uncertainty( puff(i) )

!-----    Re-calculate inverse sigmas

  CALL siginv( puff(i) )

!-----    Initialize <cc>

  IF( puff(i)%cc <= 0.0 )THEN
    rat = 1.0
  ELSE
    rat = puff(i)%cc
  END IF

  puff(i)%cc = rat*puff(i)%ccb

!-----    Initialize dynamic correlations

  IF( dynamic )THEN
    CALL get_dynamics( puff(i),pdi )
    pdi%wcp = rat*pdi%wcb
    pdi%ctp = rat*pdi%ctb
    pdi%bcp = rat*pdi%bcb
    CALL put_dynamics( puff(i),pdi )
  END IF

!-----    Initialize total variance correlations

  IF( typeID(ityp)%ltot )THEN
    CALL get_totalcc( puff(i),pti )
    pti%cct = rat*pti%cctb
    CALL put_totalcc( puff(i),pti )
  END IF

!-----    Initialize met variables

  CALL init_tlev( puff(i),pdi,ilev,xmap_i,ymap_i )
  IF( nError /= NO_ERROR )GOTO 9999

  puff(i)%idtl = ilev
  puff(i)%uo   = ub
  puff(i)%vo   = vb
  puff(i)%wo   = wb
  puff(i)%zi   = zinv
  CALL set_zcap_rel( getPuffifld(puff(i)),puff(i)%xbar,puff(i)%ybar,puff(i)%zbar, &
                                          puff(i)%szz,zinv,dtdzs,puff(i)%zc,lbl )

  mxtlev = MAX(mxtlev,puff(i)%idtl)

!-----    Initialize liquid/aerosol variables

  IF( IsLiquid(icls) .OR. IsWetParticle(icls) )THEN
    CALL get_liquid( puff(i),pqi )
    pqi%tevap = 0.0
    pqi%t = tb !*(pb**KAPPA)
    CALL put_liquid( puff(i),pqi )
  END IF

  IF( IsAerosol(icls) )THEN
    CALL get_puff_material( ityp,pmatl )
    CALL init_aerosol_equilibrium( puff(i),pdi,pmatl )
  END IF

  IF( IsMulti(icls) )THEN
    CALL ResetInterMC( i,puff(i) )
    IF( nError /= NO_ERROR )GOTO 9999
  END IF
  CALL step_clock()

END DO

9999 CONTINUE

DEALLOCATE( ptmp,kl,ku,STAT=alloc_stat )

IF( multicomp )CALL ExitInterMC()

RETURN
END

!===============================================================================

SUBROUTINE init_loc_uncertainty( p )

USE scipuff_fi

IMPLICIT NONE

TYPE( puff_str ), INTENT( INOUT ) :: p

REAL h_unc, v_unc

h_unc = p%zi**2
v_unc = p%sr**2

p%sxx = p%sxx + h_unc
p%syy = p%syy + h_unc
p%szz = p%szz + v_unc

p%zi = 0.0
p%sr = 0.0

RETURN
END

!===============================================================================

SUBROUTINE set_zcap_rel( ifld,xbar,ybar,zbar,szz,zi,tzs,zc,bl_flag )

IMPLICIT NONE

INTEGER, INTENT( IN    ) :: ifld
REAL,    INTENT( IN    ) :: xbar, ybar, zbar, szz, tzs, zi
REAL,    INTENT( INOUT ) :: zc
LOGICAL, INTENT( IN    ) :: bl_flag

REAL, EXTERNAL :: SWIMgetGamma

IF( zbar < zi .AND. tzs <= 0. .AND. bl_flag )THEN
  IF( SWIMgetGamma(ifld,xbar,ybar,zi) > 0. )THEN
    IF( zc > 0.0 .AND. zbar <= zc )THEN
      zc = zi
    ELSE
      zc = MAX(zi,zbar+2.*SQRT(szz))
    END IF
  END IF
ELSE
  zc = 0.
END IF

RETURN
END

!===============================================================================

SUBROUTINE set_rel( m,n,lev )

USE scipuff_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: m, n
INTEGER, INTENT( IN ) :: lev      !Time level of release time, min puff level

INTEGER i

CHARACTER(128) cmsg,cmsg2,cmsg3
CHARACTER(6)   ctmp

cmsg  = CHAR(0)
cmsg2 = CHAR(0)
WRITE(ctmp,'(I6)') n-m+1
cmsg3 = ' Initializing '//TRIM(ADJUSTL(ctmp))//' new puffs'
CALL write_progress( cmsg,cmsg2,cmsg3 )
IF( nError /= NO_ERROR )GOTO 9999

CALL set_ip( m,n )
IF( nError /= NO_ERROR )GOTO 9999

CALL set_cc( m,n )
IF( nError /= NO_ERROR )GOTO 9999

DO i = m,n
  puff(i)%idtl = MAX(lev,puff(i)%idtl)
END DO

CALL add_tlev( m,n )

CALL CheckSkewPuffs( m,n )
IF( nError /= NO_ERROR )GOTO 9999

IF( npuf > n )THEN
  CALL set_ip( n+1,npuf )
  IF( nError /= NO_ERROR )GOTO 9999

  CALL add_tlev( n+1,npuf )
END IF

9999 CONTINUE

RETURN
END

!============================================================================

SUBROUTINE CheckSkewPuffs( m,n )

USE scipuff_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: m, n

INTEGER i

DO i = m,n

  CALL SplitSkewPuff( puff(i),.FALSE. )
  IF( nError /= NO_ERROR )GOTO 9999

END DO

9999 CONTINUE

RETURN
END

!============================================================================

SUBROUTINE set_puff_rel( p,xmass,zbar,ityp,naux,ifld )

USE nextRel_fi
USE default_fd
USE relparam_fd

IMPLICIT NONE

TYPE( puff_str ), INTENT( INOUT ) :: p          !New puff structure
REAL,             INTENT( IN    ) :: xmass      !Puff mass
REAL,             INTENT( IN    ) :: zbar       !Puff centroid height
INTEGER,          INTENT( IN    ) :: ityp       !Puff type ID
INTEGER,          INTENT( IN    ) :: naux       !No. of auxiliary variables
INTEGER,          INTENT( IN    ) :: ifld       !Met field index

CALL zero_puff( p )

p%c    = xmass
p%xbar = SNGL(xrel)
p%ybar = SNGL(yrel)
p%zbar = zbar
IF( sigx /= DEF_VAL_R .AND. sigx /= NOT_SET_R )THEN
  p%sxx = sigx*sigx
  p%syy = sigy*sigy
  p%szz = sigz*sigz
  p%si  = MIN(sigx,sigy)
  p%si2 = MAX(sigx,sigy)
  p%sv  = sigz
  IF( reltyp(2:2) == 'X' )THEN
    p%sxy = sigRxy*sigx*sigy
    p%syz = sigRxz*sigx*sigz
    p%syz = sigRyz*sigy*sigz
  END IF
END IF
IF( wake )THEN
  p%yvsc = xmass*kyprm
  p%zwc  = xmass*kzprm
END IF
p%cfo = rel_param(REL_AFRAC_INDX)

p%ityp = ityp

CALL setPuffifld( p,ifld )
CALL setPuffirel( p,0 )

CALL set_aux_rel( p,naux )

RETURN
END

!=============================================================================

SUBROUTINE set_aux_rel( p,naux )

! ----- Set auxiliary space for new puff

USE scipuff_fi
USE relparam_fd
USE UtilMtlAux
USE met_fi

IMPLICIT NONE

TYPE( puff_str ) p

INTEGER, INTENT( IN ) :: naux

TYPE( puff_liquid   ) pq
TYPE( puff_aerosol  ) pa
TYPE( puff_dynamics ) pd

TYPE( puff_material   ) pmatl
TYPE( part_material   ) pmatpart
TYPE( liquid_material ) pmatliq

INTEGER i, ityp, icls, imat, ios
REAL    rat, temp, rhov, rhoi, rhoa

LOGICAL, EXTERNAL :: IsMulti
LOGICAL, EXTERNAL :: IsLiquid, IsWetParticle
LOGICAL, EXTERNAL :: IsAerosol
REAL,    EXTERNAL :: fun_rhoa
INTEGER, EXTERNAL :: allocatePuffAux

ityp = p%ityp
icls = typeID(ityp)%icls

p%naux = naux
ios = allocatePuffAux( p )

IF( dynamic )THEN
  CALL get_dynamics( p,pd )
  pd%w = wmom
  pd%t = buoy
  pd%un = umom
  pd%vn = vmom
  CALL put_dynamics( p,pd )
END IF

IF( IsLiquid(icls) )THEN

  CALL get_liquid( p,pq )
  CALL get_puff_material( ityp,pmatl )
  pmatliq  = TRANSFER(pmatl,pmatliq)
  pq%d     = pmatliq%dbar
  pq%sigd  = (pmatliq%dmax-pmatliq%dmin)/(2.*SQRT3)
  pq%tevap = 0.0
  pq%t     = 0.0
  pq%ccs   = p%c
  CALL put_liquid( p,pq )

ELSE IF( IsWetParticle(icls) )THEN

  CALL get_liquid( p,pq )
  i = ityp - GetSubgroups( material(typeID(ityp)%imat),mat_aux )
  CALL get_puff_material( i,pmatl )
  pmatpart = TRANSFER(pmatl,pmatpart)
  imat = typeID(ityp)%imat
  IF( material(imat)%AuxMatID /= NOT_SET_I )THEN
    rat = pmatpart%rho/WetPartLiquidMat(material(imat)%AuxMatID)%rho
  ELSE
    rat = pmatpart%rho/RHO_WATER
  END IF
  rat = (1.0+rat*(1.0/rel_param(REL_WMFRAC_INDX)-1.0))**0.3333333
  pq%d     = pmatpart%dbar * rat
  pq%sigd  = (pmatpart%dmax-pmatpart%dmin)/(2.*SQRT3) * rat
  pq%tevap = 0.0
  pq%t     = 0.0
  pq%ccs   = p%c
  CALL put_liquid( p,pq )

END IF

IF( IsAerosol(icls) )THEN
  imat = typeID(ityp)%imat
  CALL get_puff_material( ityp,pmatl )
    pmatliq = TRANSFER(pmatl,pmatliq)
    pa%co = p%c/(PI3*SQRT(8.0*p%sxx*p%syy*p%szz))
    temp = tb
    IF( dynamic )temp = temp + pd%ctp/p%c
    rhoa = fun_rhoa( temp,pb )
    rhov = rhoa*pmatliq%w/MWAIR
    rhoi = (1.0-rel_param(REL_WMFRAC_INDX))/rhov + rel_param(REL_WMFRAC_INDX)/pmatliq%rho
    pa%tevap = 0.0
    pa%fl    = rel_param(REL_WMFRAC_INDX)
    pa%fw    = 1.0E-10
    CALL put_aerosol( p,pa )
END IF

IF( IsMulti(icls) )THEN
  CALL InitMCinst( p )
  IF( nError /= NO_ERROR )GOTO 9999
END IF

9999 CONTINUE

RETURN
END

!===============================================================================

SUBROUTINE InitMCinst( p )

USE scipuff_fi
USE error_fi

IMPLICIT NONE

TYPE( puff_str ), INTENT( INOUT ) :: p

INTEGER mcID

mcID = typeID(p%ityp)%mcID

SELECT CASE( mat_mc%type(mcID) )
  CASE( MC_CHEM )
    CALL InitChemInst( p,mat_mc%ID(mcID) )
  CASE DEFAULT
    nError   = UK_ERROR
    eRoutine = 'InitMCinst'
    eMessage = 'Multicomponent error'
    WRITE(eInform,'(A,I6)') 'Unknown multicomponent type :',mat_mc%type(mcID)
    GOTO 9999
END SELECT

9999 CONTINUE

RETURN
END

!===============================================================================

SUBROUTINE InitMCvapor( p )

USE scipuff_fi
USE error_fi

IMPLICIT NONE

TYPE( puff_str ), INTENT( INOUT ) :: p

INTEGER mcID

mcID = typeID(p%ityp)%mcID

SELECT CASE( mat_mc%type(mcID) )
  CASE DEFAULT
    nError   = UK_ERROR
    eRoutine = 'InitMCvapor'
    eMessage = 'Multicomponent error'
    WRITE(eInform,'(A,I6)') 'Unknown multicomponent type :',mat_mc%type(mcID)
    GOTO 9999
END SELECT

9999 CONTINUE

RETURN
END

!===============================================================================

SUBROUTINE InitMCcont( p,dt,velp )

USE scipuff_fi
USE error_fi

IMPLICIT NONE

TYPE( puff_str ), INTENT( INOUT ) :: p
REAL,             INTENT( IN    ) :: dt
REAL,             INTENT( IN    ) :: velp

INTEGER mcID

mcID = typeID(p%ityp)%mcID

SELECT CASE( mat_mc%type(mcID) )
  CASE( MC_CHEM )
    CALL InitChemCont( p,mat_mc%ID(mcID),dt,velp )
  CASE DEFAULT
    nError   = UK_ERROR
    eRoutine = 'InitMCcont'
    eMessage = 'Multicomponent error'
    WRITE(eInform,'(A,I6)') 'Unknown multicomponent type :',mat_mc%type(mcID)
  GOTO 9999
END SELECT

9999 CONTINUE

RETURN
END

!===============================================================================

SUBROUTINE ScaleStaticMC( p,fac )

USE scipuff_fi
USE error_fi

IMPLICIT NONE

TYPE( puff_str ), INTENT( INOUT ) :: p
REAL,             INTENT( IN    ) :: fac

INTEGER mcID

mcID = typeID(p%ityp)%mcID

SELECT CASE( mat_mc%type(mcID) )
  CASE( MC_CHEM )
    CALL ScaleStaticChem( p,mat_mc%ID(mcID),fac )
  CASE DEFAULT
    nError   = UK_ERROR
    eRoutine = 'ScaleStaticMC'
    eMessage = 'Multicomponent error'
    WRITE(eInform,'(A,I6)') 'Unknown multicomponent type :',mat_mc%type(mcID)
    GOTO 9999
END SELECT

9999 CONTINUE

RETURN
END

!===============================================================================

SUBROUTINE update_scn()

USE scipuff_fi
USE files_fi

IMPLICIT NONE

TYPE( releaseT ) newRelease

LOGICAL, EXTERNAL :: IsReady
TYPE( releaseT ), EXTERNAL :: UpdateRelease

IF( .NOT.IsReady(relName,relStatus) )THEN

  newRelease = UpdateRelease( currentRelease )
  IF( nError /= NO_ERROR )GOTO 9999

  CALL UnloadRelease( newRelease )
  IF( nError /= NO_ERROR )THEN
    nError   = IV_ERROR
    eRoutine = 'update_scn'
    eMessage = 'Unable to unload updated release structure'
    eInform  = 'UnloadRelease failure'
    GOTO 9999
  END IF

  currentRelease = newRelease

  IF( lmap == I_LATLON )THEN
    CALL check_lonD( xrel,xmin,xmax )
    IF( nError /= NO_ERROR )THEN
      eInform = 'Setting source location'
      GOTO 9999
    END IF
  END IF

END IF

9999 CONTINUE

RETURN

END

!===============================================================================

SUBROUTINE valid_scn()

USE scipuff_fi
USE met_fi
USE files_fi
USE adjoint_fi

IMPLICIT NONE

INTEGER ityp
INTEGER imat
REAL    zbar, h, hx, hy

WRITE(eAction,'(A,F8.2)')'Release time TREL=',trel

IF( xrel >= DEF_VAL_D )THEN
  eMessage = 'Must set XREL in scenario input'
  GOTO 9998
END IF

IF( yrel >= DEF_VAL_D )THEN
  eMessage = 'Must set YREL in scenario input'
  GOTO 9998
END IF

IF( zrel >= DEF_VAL_R )THEN
  eMessage = 'Must set ZREL in scenario input'
  GOTO 9998
END IF

!------ Make sure xrel inside domain if global longitude

IF( global_lon )CALL SetGlobalLonD( xrel )

!------ Check continuous release parameters

IF( reltyp(1:1) == 'C' )THEN

  IF( tdur == DEF_VAL_R )subgroup = 0 !set vapor group for pools

  IF( tdur <= 0. )THEN
    eMessage = 'Must set TDUR for RELTYP=C in scenario input'
    GOTO 9998
  END IF

  IF( tdur == DEF_VAL_R .AND. .NOT.lbl )THEN
    eMessage = 'Boundary layer must be set for liquid pool sources'
    GOTO 9998
  END IF

  IF( wrel < 0.0 .AND. tdur*wrel < -zrel )THEN
    eMessage = 'End point of moving source trajectory below ground'
    GOTO 9998
  END IF

END IF

IF( name_rel == ' ' )THEN

  IF( zrel < 0. .AND. tdur /= DEF_VAL_R )THEN
    eMessage = 'Must set ZREL >= 0 in scenario input'
    GOTO 9998
  END IF

  IF( size_rel <= 0. )THEN
    IF( reltyp(1:1) == 'I' )THEN
      IF( reltyp(2:2) /= 'A' )THEN
        IF( sigx <= 0. .OR. sigy <= 0. .OR. sigz <= 0. )THEN
          eMessage = 'Must set SIZE or SIGX, SIGY, and SIGZ in'// &
                     ' scenario input'
          GOTO 9998
        END IF
        IF( (sigx >= DEF_VAL_R .OR. sigy >= DEF_VAL_R .OR. &
                sigz >= DEF_VAL_R) )THEN
          eMessage = 'Must set SIZE or SIGX, SIGY, and SIGZ in'// &
                     ' scenario input'
          GOTO 9998
        END IF
      END IF
    ELSE
      IF( tdur /= DEF_VAL_R )THEN
        IF( sigy <= 0. )THEN
          eMessage = 'Must set SIZE or SIGY for continuous release'
          GOTO 9998
        END IF
        IF( (sigy >= DEF_VAL_R .OR. sigz >= DEF_VAL_R) )THEN
          eMessage = 'Must set SIZE or SIGY and SIGZ in scenario'// &
                     ' input'
          GOTO 9998
        END IF
      ELSE
        IF( sigz /= 0. )THEN
          eMessage = 'Must set SIGZ=0 for liquid pool release in'// &
                    ' scenario input'
          GOTO 9998
        END IF
        IF( sigx <= 0. .OR. sigy <= 0. )THEN
          eMessage = 'Must set SIZE or SIGX and SIGY in scenario'// &
                     ' input'
          GOTO 9998
        END IF
        IF( (sigx >= DEF_VAL_R .OR. sigy >= DEF_VAL_R)  )THEN
          eMessage = 'Must set SIZE or SIGX and SIGY in scenario'// &
                     ' input'
          GOTO 9998
        END IF
      END IF
    END IF
  ELSE
    IF( sigx > 0. .OR. sigy > 0. .OR. sigz > 0. )THEN
      eMessage = 'Cannot set SIZE and SIGX, SIGY or SIGZ in'// &
                 ' scenario input'
      GOTO 9998
    END IF
    sigx = size_rel
    sigy = size_rel
    sigz = size_rel
  END IF

  CALL set_rel_type( relmat,subgroup,ityp,rel_dist )
  IF( nError /= NO_ERROR )GOTO 9998

  rel_ityp = ityp

  IF( BTEST(run_mode,REVERSE_MODE) )THEN
    imat = typeID(ityp)%imat
    IF( AdjMat(imat)%umet == NOT_SET_R )THEN

      CALL get_topog( SNGL(xrel),SNGL(yrel),h,hx,hy )
      IF( nError /= NO_ERROR )GOTO 9999

      zbar = MAX(zrel,10.0) + h

      CALL get_met( SNGL(xrel),SNGL(yrel),zbar,0.0,0.,0 )

      AdjMat(imat)%umet = ub
      AdjMat(imat)%vmet = vb

    END IF
  END IF

END IF

eAction = CHAR(0)

9999 CONTINUE

RETURN

!------ set read errors and goto return

9998 CONTINUE

nError   = RD_ERROR
eRoutine = 'valid_scn'
CALL ReportFileName( eInform,'File=',file_scn )
GOTO 9999

END

!==============================================================================

SUBROUTINE check_liquid_reltyp( ityp,zbar,l2phase )

! Checks for wet particle release

USE scipuff_fi
USE relparam_fd
USE UtilMtlAux
USE met_fi

IMPLICIT NONE

INTEGER, INTENT( INOUT ) :: ityp
REAL,    INTENT( IN    ) :: zbar
LOGICAL, INTENT(   OUT ) :: l2phase

INTEGER imat, icls, ios
REAL    wfrac, tboil
LOGICAL checkHE

TYPE( puff_material   ) pmatl
TYPE( liquid_material ) matl

LOGICAL, EXTERNAL :: IsLiquid

l2phase = .FALSE.

imat = typeID(ityp)%imat
icls = material(imat)%icls

wfrac = rel_param(REL_WMFRAC_INDX)

IF( IsLiquid(icls) )THEN

!--- check for 2-phase release

  IF( wfrac /= 1.0 .AND. wfrac /= NOT_SET_R )THEN
    IF( wfrac <= 0.0 .OR. wfrac > 1.0 )THEN
      nError   = IV_ERROR
      eRoutine = 'check_liquid_reltyp'
      eMessage = 'Invalid release liquid fraction'
      eInform  = 'Liquid fraction must be between 0.0 and 1.0'
      WRITE(eAction,'(A,ES10.2)',IOSTAT=ios) &
                  'Release value =',rel_param(REL_WMFRAC_INDX)
      GOTO 9999
    END IF

    l2phase = .TRUE.

!--- check for Homogeneous Equilibrium conditions

    CALL get_puff_material( ityp,pmatl )
    matl = TRANSFER(pmatl,matl)

    IF( rel_dist <= 0 )THEN
      checkHE = matl%dbar < 100.0E-6
    ELSE
      checkHE = rel_param(REL_MMD_INDX) < 100.0E-6
    END IF

    IF( checkHE )THEN
      CALL get_met( SNGL(xrel),SNGL(yrel),zbar,0.0,0.0,0 )
      tboil = matl%b/(matl%a - LOG10(pb*0.7500616)) - matl%c - ABSZERO
      IF( tb > tboil + 5.0 )THEN
        ityp = material(imat)%ioffp + &
               GetSubgroups( material(imat),mat_aux ) + 2
        rel_dist = 0
        subgroup = 0
        l2phase  = .FALSE.
      END IF
    END IF

  END IF

END IF

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE check_wet_reltyp( ityp )

! Checks for wet particle release

USE scipuff_fi
USE relparam_fd
USE UtilMtlAux

IMPLICIT NONE

INTEGER, INTENT( INOUT ) :: ityp

INTEGER imat, icls, ios
REAL    wfrac

LOGICAL, EXTERNAL :: IsWetParticle, IsParticle

imat = typeID(ityp)%imat
icls = material(imat)%icls

wfrac = rel_param(REL_WMFRAC_INDX)

IF( IsWetParticle(icls) )THEN
  IF( wfrac /= 1.0 .AND. wfrac /= NOT_SET_R )THEN
    IF( wfrac <= 0.0 .OR. wfrac > 1.0 )THEN
      nError   = IV_ERROR
      eRoutine = 'check_wet_reltyp'
      eMessage = 'Invalid release mass fraction'
      eInform  = 'Wet particle agent fraction must be between 0.0 and 1.0'
      WRITE(eAction,'(A,ES10.2)',IOSTAT=ios) &
                  'Release value =',rel_param(REL_WMFRAC_INDX)
      GOTO 9999
    END IF
    ityp = ityp + GetSubgroups( material(imat),mat_aux )
  END IF
ELSE IF( IsParticle(icls) )THEN
  IF( wfrac /= 1.0 .AND. wfrac /= NOT_SET_R )THEN
    nError   = IV_ERROR
    eRoutine = 'check_wet_reltyp'
    eMessage = 'Invalid release dry mass fraction'
    eInform  = 'Dry mass fraction only allowed for wet particle materials'
    WRITE(eAction,'(A,ES10.2)',IOSTAT=ios) &
                'Release value =',rel_param(REL_WMFRAC_INDX)
    GOTO 9999
  END IF
END IF

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE set_wetbin( pbounds,nsg,imat,wmfrac )

!  Scale particle bins to get droplet bins for wet particle release

USE scipuff_fi

IMPLICIT NONE

REAL, DIMENSION(*), INTENT( INOUT ) :: pbounds    ! Bin sizes
INTEGER,            INTENT( IN )    :: nsg        ! No. of bins
INTEGER,            INTENT( IN )    :: imat       ! Material
REAL,               INTENT( IN )    :: wmfrac     ! Mass fraction

INTEGER i
REAL    rat

TYPE( puff_material )pmatl
TYPE( part_material )pmatpart

CALL get_puff_material( imat,pmatl )

pmatpart = TRANSFER(pmatl,pmatpart)

IF( material(imat)%AuxMatID /= NOT_SET_I )THEN
  rat = pmatpart%rho/WetPartLiquidMat(material(imat)%AuxMatID)%rho
ELSE
  rat = pmatpart%rho/RHO_WATER
END IF
rat = (1.0+rat*(1.0/wmfrac-1.0))**0.3333333

DO i = 1,nsg+1
  pbounds(i) = rat*pbounds(i)
END DO

RETURN
END

