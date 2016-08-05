!*******************************************************************************
!$RCSfile$
!$Revision$
!$Date$
!*******************************************************************************
SUBROUTINE process_scn_cont( crmode,defIN )

USE scipuff_fi
USE cont_rel_fi
USE cont_rel_functions

IMPLICIT NONE

INTEGER,           INTENT( IN ) :: crmode      !CRMODE_START, CRMODE_RESTART or CRMODE_UPDATE
INTEGER, OPTIONAL, INTENT( IN ) :: defIN       !Release ID (for update only)

INTEGER defID
LOGICAL, EXTERNAL :: still_active_rel
LOGICAL, EXTERNAL :: in_domain_rel

!Stack sources input a stack diameter. Need to convert this to sigmas for
! - Domain check (now)
! - Initializing plen (now)
! - Convert stack exit speed and temperature to momentum and buoyancy (later)
IF( reltyp(2:2) == 'S' )CALL set_stack_sig()

SELECT CASE( crmode )
  CASE( CRMODE_START )
    IF( cmass < 0.0          )GOTO 9999
    IF( .NOT.in_domain_rel() )GOTO 9999
    defID = nextDefinition()
  CASE( CRMODE_RESTART )
    IF( cmass <  0.0            )GOTO 9999
    IF( .NOT.still_active_rel() )GOTO 9999
    IF( .NOT.in_domain_rel()    )GOTO 9999
    IF( reltyp(2:2) == 'M' )CALL restart_moving_cont() !Reset moving release start position on a restart
    defID = nextDefinition()
  CASE( CRMODE_UPDATE )
    IF( PRESENT(defIN) )THEN
      defID = defIN
      IF( .NOT.validDefID(defID) )THEN
        nError   = UK_ERROR
        eRoutine = 'process_scn_cont'
        eMessage = 'Request to update continuous release failed'
        WRITE(eInform,'(A,I0,A,I0)')'Invalid defID. ID=',defID,' : num def=',numDefinition
      END IF
    ELSE
      nError   = IV_ERROR
      eRoutine = 'process_scn_cont'
      eMessage = 'Request to update continuous release failed'
      eInform  = 'No continuous release definition specified'
    END IF
  CASE DEFAULT
    nError   = IV_ERROR
    eRoutine = 'process_scn_cont'
    eMessage = 'Unrecognized continuous release mode'
    WRITE(eInform,'(A,I0)')'mode =',crmode
END SELECT
IF( nError /= NO_ERROR )GOTO 9999

CALL create_Definition( defID,crmode )

9999 CONTINUE

RETURN
END
!*******************************************************************************
! create_Definition
!*******************************************************************************
SUBROUTINE create_Definition( defIN,crmode )

USE scipuff_fi
USE cont_rel_fi
USE relparam_fd
USE cont_rel_functions
USE default_fd
USE files_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: defIN       !cDefinition structure ID
INTEGER, INTENT( IN ) :: crmode      !CRMODE_START, CRMODE_RESTART or CRMODE_UPDATE

INTEGER ios
REAL    h0, hx, hy

REAL,    EXTERNAL :: set_moving_step
LOGICAL, EXTERNAL :: IsReady

!---- Set release definition parameters used in the colocation check for interactions

cDefinition(defIN)%loc%x = xrel
cDefinition(defIN)%loc%y = yrel

IF( reltyp(2:2) == 'M' )THEN
  cDefinition(defIN)%isMoving = .TRUE.
  cDefinition(defIN)%vel%x = urel
  cDefinition(defIN)%vel%y = vrel
  cDefinition(defIN)%vel%z = wrel
END IF

cDefinition(defIN)%mID = typeID(rel_ityp)%imat

!---- Set release defintion parameters

cDefinition(defIN)%isPool    = reltyp(2:2) == 'P'  !Used to check tdur == NOT_SET_R but IMHO this is more consistent
cDefinition(defIN)%time   = MAX(trel,t)
cDefinition(defIN)%dur    = tdur

cDefinition(defIN)%update = ( .NOT.IsReady(relName,relStatus) ) .AND. .NOT.cDefinition(defIN)%isPool !Pools get updated only initially
IF( cDefinition(defIN)%update .AND. crmode /= CRMODE_UPDATE )THEN
  ALLOCATE( cDefinition(defIN)%release,STAT=ios )
  IF( ios /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'create_Definition'
    eMessage = 'Error allocating scipuff release structure'
    WRITE(eInform,'(A,I0)')'IOS = ',ios
    GOTO 9999
  END IF
  cDefinition(defIN)%release = currentRelease
END IF

wake  = .FALSE.
prise = .FALSE.

IF( cDefinition(defIN)%isPool )THEN
  cDefinition(defIN)%loc%z = 0.0D0
  cDefinition(defIN)%end   = DEF_VAL_R
  CALL build_contRel_pool( cDefinition(defIN),crmode )
ELSE
  CALL get_topog( SNGL(xrel),SNGL(yrel),h0,hx,hy )
  cDefinition(defIN)%loc%z = DBLE(zrel+h0)
  cDefinition(defIN)%end   = trel + tdur
  IF( cDefinition(defIN)%isMoving )THEN
    cDefinition(defIN)%dtr  = set_moving_step()
  ELSE
    cDefinition(defIN)%dtr = tdur
  END IF
  CALL build_contRel( cDefinition(defIN),crmode )
END IF

IF( cDefinition(defIN)%time > t )THEN
  cDefinition(defIN)%state = CR_READY
ELSE
  cDefinition(defIN)%state = CR_ACTIVE
END IF

IF( crmode /= CRMODE_UPDATE )THEN
  CALL AddToCollection( cDefinition(defIN) )
  IF( crmode == CRMODE_START )THEN
    WRITE(lun_log,'(A,F7.2,A,I4)',IOSTAT=ios) &
          'C-release activated at t =',t/3600.,'hrs. with ncrel  = ',count_nrel()
  ELSE
    WRITE(lun_log,'(A,F7.2,A,I4)',IOSTAT=ios) &
          'C-release reactivated at t =',t/3600.,'hrs. with ncrel  = ',count_nrel()
  END IF
  IF( ios /= 0 )THEN
    nError   = WR_ERROR
    eRoutine = 'create_Definition'
    eMessage = 'Error writing SCIPUFF log file'
    CALL ReportFileName( eInform,'File=',file_log )
    GOTO 9999
  END IF
END IF

9999 CONTINUE

RETURN
END
!*******************************************************************************
! build_contRel
!*******************************************************************************
SUBROUTINE build_contRel( def,crmode )

USE scipuff_fi
USE cont_rel_fi
USE cont_rel_functions

IMPLICIT NONE

TYPE( cont_release_def ), INTENT( INOUT ) :: def         !cDefinition structure
INTEGER,                  INTENT( IN    ) :: crmode      !CRMODE_START, CRMODE_RESTART or CRMODE_UPDATE

INTEGER irel, nrels
INTEGER num_puff_rel
LOGICAL dyn_src
INTEGER ios
REAL plen
REAL massfrac
TYPE( cont_release_mass ) :: rmass
TYPE( cont_release_met )  :: contMet     !Extra save values from call get_met

REAL,                      EXTERNAL :: init_plen
TYPE( cont_release_mass ), EXTERNAL :: computeReleaseWeights
LOGICAL,                   EXTERNAL :: IsGas
INTEGER,                   EXTERNAL :: deallocatePuffAux

!==== get met required for initializing basePuffs

contMet%init  = .TRUE.
contMet%doSrf = .FALSE.
CALL get_contMet( contMet )

!Stack sources input stack exit speed and temperature. Need to convert this to momentum and buoyancy
!Conversion of stack diameter to sigmas should have been done earlier.

num_puff_rel = 1

IF( reltyp(2:2) == 'S' )THEN
  IF( reltyp(3:3) == 'P' )THEN
    IF( INDEX(relDisplay,'.pri') > 0 )THEN
      IF( cmass > 0. )THEN
        CALL set_stack_rel_prime( massfrac )
      ELSE
        CALL set_stack_rel_prime_null()
      END IF
      IF( nError /= NO_ERROR )GOTO 9999
      num_puff_rel = 2
      IF( .NOT.wake )THEN
        prise = .TRUE.
        CALL set_stack_rel_prise()  !use plume rise formula
        IF( nError /= NO_ERROR )GOTO 9999
      END IF
    ELSE
      prise = .TRUE.
      CALL set_stack_rel_prise()  !use plume rise formula
      IF( nError /= NO_ERROR )GOTO 9999
    END IF
  ELSE
    CALL set_stack_params()
    IF( nError /= NO_ERROR )GOTO 9999
  END IF
END IF

!==== Set the number of releases

rmass = computeReleaseWeights( contMet )
IF( rmass%num == 0 )THEN
  nError   = IV_ERROR
  eRoutine = 'build_contRel'
  eMessage = 'computeReleaseWeights returned 0 groups with mass > 0.0'
END IF

nrels = rmass%num
IF( wake )THEN
  nrels = 2*nrels

END IF

SELECT CASE( crmode )
  CASE( CRMODE_START )
    plen = init_plen()
    IF( nrels > 0 )THEN
      def%rSet%nrel = nrels
      CALL allocate_contSet_rel( def%rSet )
      DO irel = 1,nrels
        def%rSet%rels(irel)%plen = plen
      END DO
    END IF
  CASE( CRMODE_RESTART )
    IF( nrels /= def%rSet%nrel )THEN
      nError   = IV_ERROR
      eRoutine = 'build_contRel'
      eMessage = 'standard nrel inconsistent with restart data'
      WRITE(eInform,'(A,I0,A,I0)')'def%rSet%nrel=',def%rSet%nrel,' : nrels=',nrels
    END IF
  CASE( CRMODE_UPDATE )
    IF( nrels /= def%rSet%nrel )THEN
      plen = 0.0
      DO irel = 1,def%rSet%nrel
        plen = plen + def%rSet%rels(irel)%plen
      END DO
      plen = plen/FLOAT(MAX(def%rSet%nrel,1))
      ios = deallocate_Definition_rel( def )
      IF( ios /= 0 )THEN
        nError   = UK_ERROR
        eRoutine = 'build_contRel'
        eMessage = 'Error deallocating scipuff release specification'
        WRITE(eInform,'(A,I0)')'IOS = ',ios
        GOTO 9999
      END IF
      def%rSet%nrel = nrels
      CALL allocate_contSet_rel( def%rSet )
      DO irel = 1,def%rSet%nrel
        def%rSet%rels(irel)%plen = plen
      END DO
    ELSE
      DO irel = 1,def%rSet%nrel
        ios = deallocate_contRel_puff( def%rSet%rels(irel) )
        IF( ios /= 0 )GOTO 9999
        ios = deallocatePuffAux( def%rSet%rels(irel)%basePuff )
        IF( ios /= 0 )THEN
          nError   = UK_ERROR
          eRoutine = 'build_contRel'
          eMessage = 'Error deallocating standard scipuff base puff'
          WRITE(eInform,'(A,I0)')'IOS = ',ios
          GOTO 9999
        END IF
      END DO
    END IF
  CASE DEFAULT
    nError   = IV_ERROR
    eRoutine = 'build_contRel'
    eMessage = 'Unrecognized continuous release mode'
    WRITE(eInform,'(A,I0)')'mode =',crmode
END SELECT
IF( nError /= NO_ERROR )GOTO 9999

!==== Determine if this is a dynamic definition

dyn_src = (umom /= 0.0 .OR. vmom /= 0.0 .OR. wmom /= 0.0 .OR. buoy /= 0)
def%isDynamic = .FALSE.
IF( dynamic ) THEN
  DO irel = 1,rmass%num
    def%isDynamic = def%isDynamic .OR. (IsGas(typeID(rmass%ityp(irel))%icls) &
                    .AND. (dyn_src .OR. buoy_fac(rmass%ityp(irel)) /= 0.0) )
  END DO
END IF

!==== initialize basePuffs

IF( nrels > 0 )THEN
  CALL build_contRel_base( def,def%rSet,rmass,contMet )
  IF( nError /= NO_ERROR )GOTO 9999
END IF

9999 CONTINUE

RETURN
END
!*******************************************************************************
! build_contRel_base
!*******************************************************************************
SUBROUTINE build_contRel_base( def,set,rmass,contMet )

USE scipuff_fi
USE cont_rel_fi
USE error_fi
USE cont_rel_functions

IMPLICIT NONE

TYPE( cont_release_def  ), INTENT( IN    ) :: def         !cDefinition structure
TYPE( cont_release_set  ), INTENT( INOUT ) :: set         !cDefinition base structure
TYPE( cont_release_mass ), INTENT( IN    ) :: rmass       !mass/iytp for each base
TYPE( cont_release_met  ), INTENT( IN    ) :: contMet     !extra met data

INTEGER irel, ios
INTEGER iwake, jrel

INTEGER, EXTERNAL :: allocatePuffAux

IF( wake .OR. prise )THEN
  IF( wake )THEN
    DO iwake = 1,2
      CALL load_prime_rel( iwake,contMet%zbar )
      DO jrel = 1,set%nrel,2
        irel = jrel + (iwake - 1)
        set%rels(irel)%time = def%time
        set%rels(irel)%loc = def%loc
        set%rels(irel)%basePuff%naux = typeID(rmass%ityp(jrel))%npaux
        ios = allocatePuffAux( set%rels(irel)%basePuff )
        IF( ios /= 0 )THEN
          nError   = UK_ERROR
          eRoutine = 'build_contRel_base'
          eMessage = 'Error allocating scipuff puff auxiliary array'
          WRITE(eInform,'(A,I0,A,I0)')'Request=',set%rels(irel)%basePuff%naux,'  : Error = ',ios
          GOTO 9999
        END IF
        CALL set_basePuff( set%rels(irel)%basePuff,cmass,rmass%ityp(jrel),contMet )
      END DO
    END DO
  ELSE
    CALL load_prise_rel( 1,contMet%zbar )
    set%rels(1)%time = def%time
    set%rels(1)%loc = def%loc
    set%rels(1)%basePuff%naux = typeID(rmass%ityp(1))%npaux
    ios = allocatePuffAux( set%rels(1)%basePuff )
    IF( ios /= 0 )THEN
      nError   = UK_ERROR
      eRoutine = 'build_contRel_base'
      eMessage = 'Error allocating scipuff puff auxiliary array'
      WRITE(eInform,'(A,I0,A,I0)')'Request=',set%rels(1)%basePuff%naux,'  : Error = ',ios
      GOTO 9999
    END IF
    CALL set_basePuff( set%rels(1)%basePuff,cmass,rmass%ityp(1),contMet )
  END IF
  GOTO 9999
END IF

DO irel = 1,set%nrel
  set%rels(irel)%time = def%time
  set%rels(irel)%loc = def%loc
  set%rels(irel)%basePuff%naux = typeID(rmass%ityp(irel))%npaux
  ios = allocatePuffAux( set%rels(irel)%basePuff )
  IF( ios /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'build_contRel_base'
    eMessage = 'Error allocating scipuff puff auxiliary array'
    WRITE(eInform,'(A,I0,A,I0)')'Request=',set%rels(irel)%basePuff%naux,'  : Error = ',ios
    GOTO 9999
  END IF
  CALL set_basePuff( set%rels(irel)%basePuff,rmass%mass(irel),rmass%ityp(irel),contMet )
END DO

9999 CONTINUE

RETURN
END
!*******************************************************************************
! computeReleaseWeights
!*******************************************************************************
FUNCTION computeReleaseWeights( contMet ) RESULT( relMass )

USE cont_rel_fd
USE scipuff_fi
USE error_fi
USE param_fd
USE relparam_fd

IMPLICIT NONE

TYPE( cont_release_met  ), INTENT( IN ) :: contMet     !Extra save values from call get_met
TYPE( cont_release_mass )               :: relMass

INTEGER ityp, jtyp, i, imat, nsg
LOGICAL l2phase
REAL    rxx, cmass_liq
REAL    pbounds(MAXSGP+1)
REAL    weight(MAXSGP)

LOGICAL, EXTERNAL :: IsWetParticle

relMass%num    = 0
relMass%mass = 0.0
relMass%ityp   = 0

!-----  Set parameters for simple continuous release

ityp = rel_ityp
imat = typeID(ityp)%imat

!-----  Check 2-phase liquid release

CALL check_liquid_reltyp( ityp,contMet%zbar,l2phase )
IF( nError /= NO_ERROR )GOTO 9999

!-----  Check for wet particle release

CALL check_wet_reltyp( ityp )
IF( nError /= NO_ERROR )GOTO 9999

!-----  Check distribution dist < 0 implies single bin

IF( rel_dist <= 0 )THEN

!---------  Set single puff release

  IF( l2phase )THEN

    cmass_liq = cmass*rel_param(REL_WMFRAC_INDX)
    relMass%num = relMass%num + 1
    relMass%mass(relMass%num) = cmass_liq                  !liquid
    relMass%ityp(relMass%num) = ityp

    relMass%num = relMass%num + 1
    relMass%mass(relMass%num) = cmass - cmass_liq          !vapor
    relMass%ityp(relMass%num) = material(imat)%ioffp + 1

  ELSE

    relMass%num = relMass%num + 1
    relMass%mass(relMass%num) = cmass
    relMass%ityp(relMass%num) = ityp

  END IF

ELSE

!---------  Set lognormal distribution for release

  CALL get_bounds( material(imat),nsg,pbounds )
  IF( nsg < 2 )THEN
    eMessage = 'Request for a lognormal distribution of release mass but'
    eInform  = TRIM(ADJUSTL(material(imat)%cmat))//' has only a single bin'
    eAction  = 'It is more efficient to use a single bin release'
    CALL CautionMessage()
  END IF

  IF( IsWetParticle(typeID(ityp)%icls) )CALL set_wetbin( pbounds,nsg,imat,rel_param(REL_WMFRAC_INDX) )
  CALL check_logn( pbounds(1),pbounds(nsg+1),rel_param(REL_MMD_INDX),rel_param(REL_SIGMA_INDX),rxx )
  IF( rxx > 0.05 )THEN
    nError   = WN_ERROR
    eRoutine = 'computeReleaseWeights'
    WRITE(eMessage,*) NINT(rxx*100.0)
    eMessage = TRIM(ADJUSTL(eMessage))//'% of the mass is outside the bin range'
    eInform  = 'This mass will be lumped into the first and last bins'
    CALL WarningMessage( .TRUE. )
    IF( nError /= NO_ERROR )THEN
      eMessage = 'More than 5% of the mass is outside the bin range'
      eInform  = 'Please adjust the MMD and/or sigma'
      eAction  = '(or redefine the material size bins)'
      GOTO 9999
    END IF
  END IF

  CALL logn_bin( pbounds,nsg,rel_param(REL_MMD_INDX),rel_param(REL_SIGMA_INDX),weight )

!---- Check for 2-phase liquid release; reset weights and release vapor puff

  IF( l2phase )THEN

    relMass%num = relMass%num + 1
    relMass%mass(relMass%num) = cmass*(1.0-rel_param(REL_WMFRAC_INDX))  !vapor
    relMass%ityp(relMass%num) = material(imat)%ioffp + 1

    DO i = 1,nsg
      weight(i) = weight(i)*rel_param(REL_WMFRAC_INDX)                  !liquid
    END DO

  END IF

  DO i = 1,nsg
    jtyp = ityp + i - 1
    IF( cmass*weight(i) > 0.0 )THEN
      relMass%num = relMass%num + 1
      relMass%mass(relMass%num) = cmass*weight(i)
      relMass%ityp(relMass%num) = jtyp
      opid = -ABS(opid)
    END IF
  END DO
  opid = IABS(opid)
END IF

9999 CONTINUE

RETURN
END
!*******************************************************************************
! build_contRel_pool
!*******************************************************************************
SUBROUTINE build_contRel_pool( def,crmode )

USE scipuff_fi
USE cont_rel_fi
USE error_fi
USE cont_rel_functions

IMPLICIT NONE

TYPE( cont_release_def ), INTENT( INOUT ) :: def         !cDefinition structure
INTEGER,                  INTENT( IN    ) :: crmode      !CRMODE_START, CRMODE_RESTART or CRMODE_UPDATE

INTEGER nrels, irel
REAL plen
TYPE( cont_release_met )  :: contMet     !Extra save values from call get_met
TYPE( cont_release_mass ) :: rmass

REAL,           EXTERNAL :: init_plen
INTEGER,        EXTERNAL :: allocatePuffAux

!==== get met required for initializing basePuffs

contMet%init  = .TRUE.
contMet%doSrf = .TRUE.
CALL get_contMet( contMet )

!==== Set the number of releases
!     For pool releases
nrels = 1

rmass%num     = 1
rmass%mass(1) = cmass
rmass%ityp(1) = rel_ityp

SELECT CASE( crmode )
  CASE( CRMODE_START )
    plen = init_plen()
    IF( nrels > 0 )THEN
      def%rSet%nrel = nrels
      CALL allocate_contSet_rel( def%rSet )
      DO irel = 1,nrels
        def%rSet%rels(irel)%plen = plen
          CALL init_poolAux( def,def%rSet%rels(irel) )
      END DO
    END IF
  CASE( CRMODE_RESTART )
    IF( nrels /= def%rSet%nrel )THEN
      nError   = IV_ERROR
      eRoutine = 'build_contRel_pool'
      eMessage = 'standard nrel inconsistent with restart data'
      WRITE(eInform,'(A,I0,A,I0)')'def%rSet%nrel=',def%rSet%nrel,' : nrels=',nrels
      GOTO 9999
    END IF
    CALL check_poolAux( def )
    IF( def%time == DEF_VAL_R )GOTO 9999   !This pool has completed.
  CASE( CRMODE_UPDATE )
    nError   = IV_ERROR
    eRoutine = 'build_contRel_pool'
    eMessage = 'Request to update pool release failed'
    eInform  = 'Pool updates after release has started not yet enabled'
  CASE DEFAULT
    nError   = IV_ERROR
    eRoutine = 'build_contRel_pool'
    eMessage = 'Unrecognized continuous release mode'
    WRITE(eInform,'(A,I0)')'mode =',crmode
END SELECT
IF( nError /= NO_ERROR )GOTO 9999

def%isDynamic = .FALSE.

!==== Set the basePuff

IF( nrels > 0 )THEN
  CALL build_contRel_base( def,def%rSet,rmass,contMet )
  IF( nError /= NO_ERROR )GOTO 9999
  def%rSet%tlev = 0
  DO irel = 1,nrels
      CALL init_tlev_pool( def%rSet%rels(irel)%basePuff )
    IF( nError /= NO_ERROR )GOTO 9999
    def%rSet%tlev = MAX(def%rSet%tlev,def%rSet%rels(irel)%basePuff%idtl)
  END DO
END IF

9999 CONTINUE

RETURN
END
!*******************************************************************************
! init_tlev_pool
!*******************************************************************************
SUBROUTINE init_tlev_pool( p )

USE scipuff_fi
USE met_fi
USE error_fi
USE srfevap_fi
USE files_fi

IMPLICIT NONE

TYPE( puff_str ), INTENT( INOUT ) :: p

REAL del, u2, v2
INTEGER ilev

INTEGER, EXTERNAL :: time_level

!-----  Set time-level for the pool release based on pool size
!       and local surface wind speed (get_met and get_srf_met should have already been called)

u2  = ubsrf*ubsrf + uub + uubl + vvbl
v2  = vbsrf*vbsrf + vvb + uubl + vvbl
del = MAX(SQRT(u2/p%sxx),SQRT(v2/p%syy),1.0/delt)
del = 2.0/del

ilev = time_level( del )
IF( nError /= NO_ERROR )THEN
  eRoutine = 'init_tlev_pool'
  WRITE(lun_log,*,ERR=9998)'******* TIME LEVEL ERROR ********'
  WRITE(lun_log,*,ERR=9998)TRIM(eRoutine)
  WRITE(lun_log,*,ERR=9998)TRIM(eInform)
  WRITE(lun_log,*,ERR=9998)'DT=',del
  WRITE(lun_log,*,ERR=9998)'DELT=',delt
  WRITE(lun_log,*,ERR=9998)'LEVEL=',ilev,MAXTLV
  WRITE(lun_log,*,ERR=9998)'U,V=',ubsrf,vbsrf
  CALL dump_puff( 0,p )
  GOTO 9999
END IF

p%idtl = ilev
mxtlev = MAX(mxtlev,ilev)

9999 CONTINUE

RETURN

9998 CONTINUE
nError   = UK_ERROR
eMessage = 'Error writing to log file'
GOTO 9999

END
!*******************************************************************************
! set_basePuff
!*******************************************************************************
SUBROUTINE set_basePuff( p,rmass,ityp,contMet )

USE scipuff_fi
USE cont_rel_fi
USE relparam_fd

IMPLICIT NONE

TYPE( puff_str ),         INTENT( INOUT ) :: p
REAL,                     INTENT( IN    ) :: rmass
INTEGER,                  INTENT( IN    ) :: ityp
TYPE( cont_release_met ), INTENT( IN    ) :: contMet     !Extra save values

p%c    = rmass
p%xbar = SNGL(xrel)
p%ybar = SNGL(yrel)
p%zbar = contMet%zbar
IF( sigx /= DEF_VAL_R .AND. sigx /= NOT_SET_R )THEN
  p%sxx  = sigx*sigx
  p%syy  = sigy*sigy
  p%szz  = sigz*sigz
  IF( sigx == 0.0 )THEN
    p%si  = sigy
    p%si2 = sigy
  ELSE
    p%si  = MIN(sigx,sigy)
    p%si2 = MAX(sigx,sigy)
  END IF
  IF( sigz == 0.0 )THEN
    p%sv = MIN(sigx,sigy)
  ELSE
    p%sv = sigz
  END IF
END IF
IF( wake )THEN
  p%yvsc = rmass*kyprm
  p%zwc  = rmass*kzprm
END IF
p%cfo = rel_param(REL_AFRAC_INDX)

p%ityp = ityp

CALL setPuffifld( p,contMet%ifld )
CALL setPuffirel( p,0 )

p%uo = urel
p%vo = vrel
p%wo = wrel

CALL set_baseAux( p )

9999 CONTINUE

RETURN
END
!*******************************************************************************
! set_baseAux
!*******************************************************************************
SUBROUTINE set_baseAux( p )

USE scipuff_fi
USE relparam_fd
USE UtilMtlAux

IMPLICIT NONE

TYPE( puff_str ), INTENT( INOUT ) :: p

TYPE( puff_liquid   ) pq
TYPE( puff_aerosol  ) pa
TYPE( puff_dynamics ) pd

TYPE( puff_material   ) pmatl
TYPE( part_material   ) pmatpart
TYPE( liquid_material ) pmatliq

INTEGER i, ityp, icls, imat
REAL    rat

LOGICAL, EXTERNAL :: IsMulti
LOGICAL, EXTERNAL :: IsLiquid, IsWetParticle
LOGICAL, EXTERNAL :: IsAerosol
INTEGER, EXTERNAL :: allocatePuffAux

!---- Initial checks and setup

IF( p%naux == 0 )GOTO 9999

IF( .NOT.ASSOCIATED(p%aux) )THEN
  nError   = UK_ERROR
  eRoutine = 'set_basePuff'
  eMessage = 'Error setting puff auxiliary array'
  eInform  = 'Puff auxiliary array not allocated'
  GOTO 9999
END IF

ityp = p%ityp
icls = typeID(ityp)%icls

!---- dynamic aux

IF( dynamic )THEN
  CALL get_dynamics( p,pd )
  pd%w = wmom
  pd%t = buoy
  pd%un = umom
  pd%vn = vmom
  CALL put_dynamics( p,pd )
END IF

!---- Liquid material specific aux

IF( IsLiquid(icls) )THEN

  CALL get_liquid( p,pq )
  CALL get_puff_material( ityp,pmatl )
  pmatliq = TRANSFER(pmatl,pmatliq)
  pq%d    = pmatliq%dbar
  pq%sigd = (pmatliq%dmax-pmatliq%dmin)/(2.*SQRT3)
  pq%tevap = 0.0
  pq%t    = 0.0
  pq%ccs  = p%c
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
  pq%d    = pmatpart%dbar * rat
  pq%sigd = (pmatpart%dmax-pmatpart%dmin)/(2.*SQRT3) * rat
  pq%tevap = 0.0
  pq%t     = 0.0
  pq%ccs   = p%c
  CALL put_liquid( p,pq )

END IF

IF( IsAerosol(icls) )THEN
  pa%fl    = rel_param(REL_WMFRAC_INDX)
  pa%fw    = 1.0E-10
  pa%tevap = 0.0
  pa%co    = 0.0
  CALL put_aerosol( p,pa )
END IF

!---- multicomponent aux

IF( IsMulti(icls) )THEN
  CALL InitMCinst( p )
  IF( nError /= NO_ERROR )GOTO 9999
END IF

9999 CONTINUE

RETURN
END
!*******************************************************************************
! init_poolAux
!*******************************************************************************
SUBROUTINE init_poolAux( def,rel )

USE scipuff_fi
USE cont_rel_fi
USE error_fi
USE cont_rel_functions

IMPLICIT NONE

TYPE( cont_release_def ), INTENT( INOUT ) :: def         !cDefinition structure
TYPE( cont_release_rel ), INTENT( INOUT ) :: rel         !contRel structure

REAL age, offset

!---- Call InitPoolAux

age    = 0.0     !Initial pool age
offset = 0.0     !Initial pool tempreature offset

CALL InitPoolAux( def%mID,cmass,age,offset,rel )

9999 CONTINUE

RETURN
END
!*******************************************************************************
! check_poolAux
!*******************************************************************************
SUBROUTINE check_poolAux( def )

USE scipuff_fi
USE cont_rel_fi
USE error_fi
USE cont_rel_functions

IMPLICIT NONE

TYPE( cont_release_def ), INTENT( INOUT ) :: def         !cDefinition structure

INTEGER i, naux
LOGICAL inactive

inactive = .TRUE.
DO i = 1,def%rSet%nrel
  IF( .NOT.ASSOCIATED(def%rSet%rels(i)%saux) )THEN
    nError   = IV_ERROR
    eMessage = 'Error restarting auxiliary source data'
    eInform  = 'Standard Pool release with unassociated saux'
    eRoutine = 'check_poolAux'
    GOTO 9999
  END IF
  naux = SIZE(def%rSet%rels(i)%saux)
  IF(  naux /= def%rSet%rels(i)%naux )THEN
    nError   = IV_ERROR
    eMessage = 'Error restarting auxiliary source data'
    eInform  = 'Standard Pool release with wrong size saux'
    eRoutine = 'check_poolAux'
    GOTO 9999
  END IF
  IF(  naux /= SCIPUFF_POOLSRCPARAM .AND. naux /= 1 )THEN
    nError   = IV_ERROR
    eMessage = 'Error restarting auxiliary source data'
    eInform  = 'Standard Pool release with invalid size saux'
    eRoutine = 'check_poolAux'
    GOTO 9999
  END IF
  inactive = inactive .AND. (naux == 1)
END DO

IF( inactive )THEN
  def%time = DEF_VAL_R  !This pool is complete. Set so it will not restart.
  GOTO 9999
END IF

9999 CONTINUE

RETURN
END
!*******************************************************************************
! get_contMet
!*******************************************************************************
SUBROUTINE get_contMet( contMet )

USE scipuff_fi
USE error_fi
USE cont_rel_fi
USE met_fi        !For interface to get_met

IMPLICIT NONE

TYPE( cont_release_met ), INTENT( INOUT ) :: contMet

REAL h, hx, hy, x, y

x = SNGL(xrel)
y = SNGL(yrel)

IF( contMet%init )THEN
  contMet%shh = 0.5*(sigx**2+sigy**2)

  CALL get_topogOut( x,y,h,hx,hy,contMet%shh,contMet%ifld )

  IF( reltyp(2:2) == 'P' )THEN !Liquid Pool source
    contMet%zbar = h
  ELSE
    contMet%zbar = zrel + h
  END IF

  IF( reltyp(2:2) == 'S' .OR. reltyp(2:2) == 'P' )THEN
    contMet%szz = (MAX(0.01*sigy,0.01*zrel))**2
  ELSE
    contMet%szz = sigz*sigz
  END IF

  contMet%init = .FALSE.
END IF

CALL get_met( x,y,contMet%zbar,contMet%szz,0.,0,inField=contMet%ifld,Shh=contMet%shh )
IF( nError /= NO_ERROR )GOTO 9999

IF( contMet%doSrf )THEN
  CALL get_srf_met( x,y )
  IF( nError /= NO_ERROR )GOTO 9999
END IF

9999 CONTINUE

RETURN
END
!*******************************************************************************
! still_active_rel
!*******************************************************************************
LOGICAL FUNCTION still_active_rel() RESULT( yes )

USE scipuff_fi
USE error_fi
USE default_fd

IMPLICIT NONE

!Pool releases are always "active" becuase they do not have a predefined end time
!Pool releases get checked later to see if they still have mass to release

yes = reltyp(2:2) == 'P'
IF( .NOT.yes )yes = (trel+tdur > t)

RETURN
END
!*******************************************************************************
! in_domain_rel
!*******************************************************************************
LOGICAL FUNCTION in_domain_rel() RESULT( yes )

USE scipuff_fi
USE error_fi

IMPLICIT NONE

REAL    xmap, ymap, x, y
REAL    xx, yy, rxx, ryy
REAL    dtm
INTEGER i, n

LOGICAL, EXTERNAL :: chkgrd

IF( reltyp(2:2) == 'M' )THEN
  n = 11
  dtm = 0.1*tdur
ELSE
  n = 1
  dtm = 0.
END IF

x = SNGL(xrel)
y = SNGL(yrel)

DO i = 1,n

  CALL mapfac( x,y,xmap,ymap )

  xx   = (x-xmin)/dxg
  yy   = (y-ymin)/dyg
  rxx  = (sigy*xmap/dxg)**2
  ryy  = (sigy*ymap/dyg)**2

  yes = chkgrd(xx,yy,rxx,ryy)

  IF( yes )EXIT

  x = x + dtm*urel*xmap
  y = y + dtm*vrel*ymap

END DO

RETURN
END
!*******************************************************************************
! init_plen
!*******************************************************************************
REAL FUNCTION init_plen() RESULT( plen )

USE scipuff_fi
USE cont_rel_fi

IMPLICIT NONE

IF( sigx == 0.0 )THEN
  plen = MIN(sigz,sigy)  !Standard continuous release
ELSE
  plen = MIN(sigx,sigy)  !Pool, stack releases
END IF

RETURN
END
!*******************************************************************************
! restart_moving_cont
!*******************************************************************************
SUBROUTINE restart_moving_cont()

USE scipuff_fi

IMPLICIT NONE

REAL xmap, ymap, tmov

IF( t > trel )THEN
  CALL mapfac( SNGL(xrel),SNGL(yrel),xmap,ymap )
  tmov = t - trel
  xrel = xrel + tmov*urel*xmap
  yrel = yrel + tmov*vrel*ymap
  zrel = zrel + tmov*wrel
END IF

RETURN
END
!*******************************************************************************
! set_moving_step
!*******************************************************************************
REAL FUNCTION set_moving_step() RESULT( dtr )

USE scipuff_fi

IMPLICIT NONE

INTEGER, PARAMETER :: NMOVP = 10

REAL    vel
INTEGER nstp

vel = SQRT(urel*urel+vrel*vrel+wrel*wrel)

IF( sigx == 0.0 )THEN
  vel = vel/MIN(sigy,sigz)
ELSE IF( sigz == 0.0 )THEN
  vel = vel/MIN(sigy,sigx)
ELSE
  vel = vel/MIN(sigx,sigy,sigz)
END IF

nstp = MIN(NMOVP,INT(vel*tdur)+1)
nstp = MAX(nstp,INT(0.003*vel*tdur)+1)

dtr = tdur/FLOAT(nstp)

RETURN
END
!*******************************************************************************
! set_stack_sig
!*******************************************************************************
SUBROUTINE set_stack_sig()

USE scipuff_fi

IMPLICIT NONE

!  Set true release sigmas for STACK release
!  STACK release contains EXIT DIAMETER (m) in size

REAL rstack

rstack = 0.5*size_rel
sigx   = rstack
sigy   = sigx
IF( umom == DEF_VAL_R )THEN
  sigz = 0.0
ELSE
  sigz = sigx
END IF
size_rel = 0.0

RETURN
END
!*******************************************************************************
! set_stack_params
!*******************************************************************************
SUBROUTINE set_stack_params()

USE scipuff_fi
USE met_fi

IMPLICIT NONE

!  Set true release parameters for STACK release. Requires met.
!  STACK release contains EXIT TEMPERATURE (C) in buoy
!                         EXIT VELOCITY in wmom
!                         EXIT DIAMETER (m) converted to sigmas in set_stack_sig

REAL tstack, wstack, astack, bfac
REAL rstack, ustack, vstack, vel, mom

IF( buoy == DEF_VAL_R )THEN
  tstack = tb
ELSE
  tstack = buoy - ABSZERO
END IF

rstack = 0.5*size_rel
astack = PI*sigx*sigy
bfac   = tb/MAX(tstack,tb)
IF( umom == DEF_VAL_R )THEN
  wstack = wmom
  buoy   = astack*wstack*(tstack-tb)*bfac
  wmom   = astack*wstack*ABS(wstack)*bfac
  umom   = 0.0
  vmom   = 0.0
ELSE
  wstack = wmom
  vstack = vmom - vb
  ustack = umom - ub
  vel    = SQRT( ustack*ustack + vstack*vstack + wstack*wstack)
  IF( vel == 0.0 )THEN
    buoy = 0.0
    wmom = 0.0
    umom = 0.0
    vmom = 0.0
  ELSE
    buoy = astack*vel*(tstack-tb)*bfac
    mom  = astack*vel*bfac
    umom = mom*ustack
    vmom = mom*vstack
    wmom = mom*wstack
  END IF
END IF
size_rel = 0.0

9999 CONTINUE

RETURN
END
