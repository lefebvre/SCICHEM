!*******************************************************************************
!$RCSfile$
!$Revision$
!$Date$
!*******************************************************************************
SUBROUTINE step()

USE scipuff_fi
USE surface_fi
USE files_fi
USE sampler_fi
USE met_fi
USE cont_rel_fi
USE sciprime_fi
USE srfparam_fd
USE step_p_fi, ONLY: splitf, isplit
USE chem_fi, ONLY: chemMC
USE mpi_fi, ONLY: useMPI, pufNo
USE StepMC_mpi_fi, ONLY: puffNos,iStepMC
USE cont_rel_functions

IMPLICIT NONE

INTEGER mstep, istep, lev, lev2, npufo
INTEGER ipuf, ilev, nch
INTEGER mchg, nxtlev, ntpuf, nspuf
INTEGER ntem
INTEGER i, id, imat, ifld
INTEGER SmpOutType
REAL    dts, dt, t0_step, t_step, t1_step, dt_dose, fac_srf
LOGICAL compress, lsplit, lUpdateSmp

CHARACTER(80) cmsg,cmsg2,cmsg3
CHARACTER(32) ctem

INTEGER, EXTERNAL :: set_lev

CALL start_clock()

IF( ALLOCATED(concPrime) )concPrime = 0.

IF( npuf == 0 .AND. countDefinitions() == 0 .AND. &
    InstReleaseList%release%trel == NOT_SET_R )THEN

  t = t + delt

  IF( depint )CALL grdose( t,delt )

  cmsg = TRIM(name)//' : Calculating '
  CALL time_message( cmsg2,t )
  IF( nError /= NO_ERROR )GOTO 9999
  CALL c_format( delt,nch,ctem )
  WRITE(cmsg3,'(I5,A)')npuf,' Puffs  '//ctem(1:nch)//'s timestep'
  CALL write_progress( cmsg,cmsg2,cmsg3 )
  IF( nError /= NO_ERROR )GOTO 9999

  ntem = 0
  CALL write_progress_bar( ntem )

  IF( lsmp )THEN
    SmpOutType = 0
    SmpOutType = IBSET(SmpOutType,SOB_NOPUFF)
    SmpOutType = IBSET(SmpOutType,SOB_LARGEDELT)
    lUpdateSmp = .FALSE.
    IF( lSmpOut )THEN
      IF( t >= tSmpOut-tolSmpOut )THEN
        lUpdateSmp = .TRUE.
      ELSE IF( int_sensor )THEN
        SmpOutType = IBSET(SmpOutType,SOB_INTONLY)
        lUpdateSmp = .TRUE.
      END IF
    ELSE
      lUpdateSmp = .TRUE.
    END IF
    IF( lUpdateSmp )THEN
      CALL update_smp( SmpOutType )
      IF( nError /= NO_ERROR )GOTO 9999
    END IF
  END IF

  GOTO 9999

END IF

CALL step_clock()

!------ Update multicomponent chemistry background

IF( multicomp )THEN
  CALL updateRunMC()
  IF( nError /= NO_ERROR )GOTO 9999
END IF

!------ initialize for large timestep

ntpuf    = npuf
nxtlev   = mxtlev
compress = .FALSE.
lsplit   = .TRUE.

!------ set time stepping for surface evaporation

IF( evaporation )THEN
  CALL set_evap_step()
  IF( nError /= NO_ERROR )GOTO 9999
  nxtlev = MAX(nxtlev,mxtlev_evap)
  CALL step_clock()
END IF

mstep = 2**nxtlev
dts   = delt/FLOAT(mstep)

!------ report run progress

cmsg = TRIM(name)//' : Calculating '
CALL time_message( cmsg2,t )
IF( nError /= NO_ERROR )GOTO 9999
CALL c_format( dts,nch,ctem )
WRITE(cmsg3,'(I7,A)')npuf,' Puffs  '//ctem(1:nch)//'s timestep'
CALL write_progress( cmsg,cmsg2,cmsg3 )
IF( nError /= NO_ERROR )GOTO 9999

ntem = 0
CALL write_progress_bar( ntem )

!-----  Loop over small timesteps

t0_step = t
t1_step = t + delt
t_step  = 0.
istep   = 0
dt_dose = 0.

DO WHILE( istep < mstep )

  istep = istep + 1
  lev   = nxtlev - set_lev( istep )
  lev2  = nxtlev

!-------- always try to split level-0 puffs

  IF( istep == mstep )lsplit = .TRUE.

!-------- continuous releases

  CALL c_release( dts,t1_step,lev,lev2 )
  IF( nError /= NO_ERROR )GOTO 9999

!-------- instantaneous releases

  CALL CheckInstRelease( lev,lev2 )
  IF( nError /= NO_ERROR )GOTO 9999

  CALL step_clock()

!-------- adjust minimum step if necessary after c_release

  IF( lev2 > nxtlev )THEN
    mchg   = 2**(lev2-nxtlev)
    mstep  = mstep*mchg
    istep  = (istep-1)*mchg + 1
    dts    = delt/FLOAT(mstep)
    nxtlev = lev2
    lev    = nxtlev - set_lev( istep )
  END IF

  npufo = npuf

!-------- Check if any puffs are being stepped

  nspuf = 0
  DO ilev = lev,nxtlev
    nspuf = nspuf + ntlev(ilev)
  END DO

!-------- Loop over time levels

  IF( nspuf > 0 )THEN

    IF( useMPI )THEN
      CALL AllocStepMCDat( nspuf )
      IF( nError /= NO_ERROR )GOTO 9999
    ELSE

    END IF

    CALL step_clock()

    DO ilev = lev,nxtlev

      dt = dts*FLOAT(2**(nxtlev-ilev))

      ipuf = itfrst(ilev)

      DO WHILE( ipuf > 0 )

!-----------  integrate

        fac_srf = NOT_SET_R
        pufNo   = ipuf

        CALL step_p( dt,puff(ipuf),ipuf,lev,lev2,fac_srf )
        IF( nError /= NO_ERROR )GOTO 9999

        IF( .NOT.useMPI )THEN

        IF( lsplit )THEN
          CALL split( ipuf,lsplit )
          IF( nError /= NO_ERROR )GOTO 9999
        END IF

        ELSE

          puffNos(iStepMC)%ipuf = ipuf
          puffNos(iStepMC)%npuf = isplit
          puffNos(iStepMC)%frac = splitf

        END IF

        CALL step_clock()

        ipuf = puff(ipuf)%idtn

      END DO
      CALL check_progress()
      IF( nError /= NO_ERROR )GOTO 9999

    END DO

  ELSE

!-----------  no puffs, so increase timestep if no evaporation

    IF( nxtlev > 0 .AND. MOD(istep,2) /= 0 )THEN
      IF( (InstReleaseList%release%trel == NOT_SET_R .AND. readyDefinition() == 0 ) .OR. nxtlev > 7 )THEN
        IF( mxlev_evap < nxtlev .AND. tlevDefinition() < nxtlev )THEN
          mstep  = mstep/2
          istep  = istep/2
          dts    = dts + dts
          nxtlev = nxtlev - 1
          IF( istep < mstep )CYCLE
        END IF
      END IF
    END IF

  END IF

!-------- Call splitting outside step_p loop for MPI calls

  IF( nspuf > 0 )THEN

    IF( useMPI )THEN

      CALL StepChemPuffMPI( nspuf )
      IF( nError /= NO_ERROR )GO TO 9999

      DO ilev = lev,nxtlev

        dt = dts*FLOAT(2**(nxtlev-ilev))

        ipuf = itfrst(ilev)

        DO WHILE( ipuf > 0 )

  !-----------  integrate

          fac_srf = NOT_SET_R

          IF( lsplit )THEN

            CALL split( ipuf,lsplit )
            IF( nError /= NO_ERROR )GOTO 9999

          END IF

          ipuf = puff(ipuf)%idtn

        END DO

      END DO

    END IF

  END IF

!-------- Modified accumulation of time to eliminate deactivation/static source problems
!         due to round-off errors when making a long run with a small time step.  (SFP 6/3/98)

  t_step = t_step + dts
  t      = t0_step + t_step

!------ Update multicomponent chemistry background

  IF( multicomp )THEN
    CALL updateRunMC()
    IF( nError /= NO_ERROR )GOTO 9999
  END IF

  ntem = NINT(64.*t_step/delt)
  CALL write_progress_bar( ntem )

  dt_dose = dt_dose + dts

  IF( nspuf > 0 )THEN

!----- Set compress flag if too many puffs for splitting
    IF( .NOT.lsplit .AND. (4*npuf > 3*MAXPUF) )compress = .TRUE.

!----- Update surface radiation dose integrals
    IF( depint )CALL grdose( t,dt_dose )
    dt_dose = 0.0

  END IF

  CALL step_clock()

!-------- calculate surface evaporation

  IF( mxlev_evap >= 0 )THEN
    CALL step_srf_evap( lev,lev2 )
    IF( nError /= NO_ERROR )GOTO 9999
    CALL step_clock()
  END IF

!-------  add new puffs into time lists

  IF( npuf > npufo )CALL add_tlev( npufo+1,npuf )

!-------  check for merge and puffs off grid

  CALL merge( lev,lev2 )
  IF( nError /= NO_ERROR )GOTO 9999

!-------  check for compress and reset time lists

  IF( istep == mstep )THEN

    CALL compress_puff_list()
    CALL find_mxgrd( nz )
    CALL set_tlev(.FALSE.)

  ELSE

    IF( compress )CALL compress_puff_list()
    CALL reset_tlev()

  END IF

  CALL step_clock()

!-------  compute interaction terms

  CALL inter( lev,lev2 )
  IF( nError /= NO_ERROR )GOTO 9999

  CALL step_clock()

!-------  check for puff cleanup

  IF( istep /= mstep )THEN

    ntpuf = 0
    DO ilev = 0,lev2
      ntpuf = ntpuf + ntlev(ilev)
    END DO
    compress = (npuf > MAXPUF/3) .AND. ((npuf-ntpuf) > npuf/4)
  ELSE

    compress = .FALSE.

  END IF

  CALL step_clock()

!-------- adjust minimum step if necessary

  IF( evaporation )lev2 = MAX( lev2,mxtlev_evap )

  IF( lev2 > nxtlev )THEN
    mchg   = 2**(lev2-nxtlev)
    mstep  = mstep*mchg
    istep  = istep*mchg
    dts    = delt/FLOAT(mstep)
    nxtlev = lev2
  END IF

!------- Compute sampler output

  IF( lsmp )THEN

    istepP = istep  !Save for "advecting" puffs in sampler (GetPuffVal)
    dtsP   = dts
    mstepP = mstep

    SmpOutType = 0
    IF( istep == mstep )SmpOutType = IBSET(SmpOutType,SOB_LARGEDELT)

    lUpdateSmp = .FALSE.
    IF( lSmpOut )THEN
      IF( t >= tSmpOut-tolSmpOut )THEN
        SmpOutType = IBCLR(SmpOutType,SOB_INTONLY); lUpdateSmp = .TRUE.
      ELSE IF( int_sensor )THEN
        SmpOutType = IBSET(SmpOutType,SOB_INTONLY); lUpdateSmp = .TRUE.
      END IF
    ELSE IF( lev <= mxlev_smp )THEN
      SmpOutType = IBCLR(SmpOutType,SOB_INTONLY);   lUpdateSmp = .TRUE.
    ELSE IF( int_sensor )THEN
      SmpOutType = IBSET(SmpOutType,SOB_INTONLY);   lUpdateSmp = .TRUE.
    END IF
    IF( lUpdateSmp )THEN
      CALL update_smp( SmpOutType )
      IF( nError /= NO_ERROR )GOTO 9999
    END IF

  END IF

  CALL step_clock()

  CALL check_progress()
  IF( nError /= NO_ERROR )GOTO 9999

END DO

t = t1_step          !Reset t to avoid accumulation errors

runUpdates = .TRUE.  !Run updates at start of next step

!------ Add ambient concentration contribution to multicomponent doses

IF( dose )THEN
  DO i = ndep_blocks+1,nsrf_blocks
    IF( srf_block(i)%type == SBLK_MULTI_DOS )THEN
      id   = srf_block(i)%id
      imat = id - (id/65536)*65536
      ifld = srf_block(i)%Field + 2 !Offset accounting for mean and var
      IF( chemMC(mat_mc%ID( material(imat)%mcID))%lAddAmb )THEN
        CALL SetChemAmbDos( imat,ifld,delt )
        IF( nError /= NO_ERROR )GOTO 9999
      END IF
    END IF
  END DO
END IF

CALL check_progress()
IF( nError /= NO_ERROR )GOTO 9999

9999 CONTINUE

CALL stop_clock()

RETURN

!------ set log write error and goto return

9998 CONTINUE

nError   = WR_ERROR
eRoutine = 'step'
eMessage = 'Error writing SCIPUFF log file'
CALL ReportFileName( eInform,'File=',file_log )
GOTO 9999

END

!===============================================================================

SUBROUTINE updateRunMC()

USE error_fi
USE files_fi
USE matl_fi
USE chem_fi

IMPLICIT NONE

INTEGER ii, n

! -- Initialize background (if required) for any chemistry multicomponents

DO ii = 1,mat_mc%nMCtype

  IF( mat_mc%type(ii) == MC_CHEM )THEN

    n = mat_mc%ID(ii)

    chem => chemMC(n)

    IF( chem%lStepAmb )THEN
      CALL StepChemAmb()
      IF( nError /= NO_ERROR )GOTO 9999
    ELSE IF( chem%lAmbFile )THEN
      CALL UpdateAmbFields()
      IF( nError /= NO_ERROR )GOTO 9999
    END IF

  END IF

END DO

9999 CONTINUE

RETURN
END
