!*******************************************************************************
!$RCSfile$
!$Revision$
!$Date$
!*******************************************************************************
SUBROUTINE i_release( LogFlag )

USE scipuff_fi
USE files_fi
USE relparam_fd

IMPLICIT NONE

LOGICAL, INTENT( IN ) :: LogFlag    ! Write to log file

REAL    cmass_liq, cmass_vap
INTEGER ityp_vap, naux_vap
INTEGER ios, nun, nch, iflg, mpuf, imat, ityp
INTEGER i, nsg, nb_rel, naux, jtyp, ifld
LOGICAL l2phase
LOGICAL lGroundRel
INTEGER jfld

REAL xbar, ybar, zbar, h, xmap, ymap, hx, hy, xx, yy, rxx, ryy, shh

REAL, DIMENSION(MAXSGP)   :: weight
REAL, DIMENSION(MAXSGP+1) :: pbounds

LOGICAL, EXTERNAL :: chkgrd
LOGICAL, EXTERNAL :: IsWetParticle, IsLiquid, IsAerosol
INTEGER, EXTERNAL :: num_puff_types

!-----  Check release time, and save if necessary

IF( trel > t + 0.01*delt )THEN
  CALL StoreInstRel()
  GOTO 9999
END IF

!---- Initialize eRoutine for check_newpuff calls

eRoutine = 'i_release'

!-----  releases puffs from NAME_REL[.rel] file

mpuf = npuf   !Save old number of puffs

lGroundRel = .FALSE.

IF( name_rel /= ' ' )THEN

  nch = LEN_TRIM(name_rel)
  nun = lun_tmp
  CALL add_ext( file_tmp,name_rel(1:nch),'rel' )

  OPEN(UNIT=nun,FILE=file_tmp,STATUS='OLD',ACTION='READ',IOSTAT=ios)
  IF( ios /= 0 )THEN
    nError   = OP_ERROR
    eMessage = 'Error opening source release input file'
    CALL ReportFileName( eInform,'File=',file_tmp )
    eAction  = 'Check scenario file for proper specification'
    GOTO 9999
  END IF

  iflg  = 0

  CALL input_puff( nun,iflg,nb_rel )
  IF( nError /= NO_ERROR )GOTO 9999

  IF( nun == lun_tmp )CLOSE(lun_tmp,IOSTAT=ios)

  IF( nb_rel == 0 )GOTO 9999

  CALL mapfac( SNGL(xrel),SNGL(yrel),xmap,ymap )

  npuf = mpuf

  DO i = mpuf+1,mpuf+nb_rel

    xbar = SNGL(xrel) + puff(i)%xbar*xmap
    ybar = SNGL(yrel) + puff(i)%ybar*ymap

!------ Make sure xrel inside domain if global longitude

    IF( global_lon )CALL SetGlobalLon( xbar )

    xx   = (xbar-xmin)/dxg
    yy   = (ybar-ymin)/dyg
    rxx  = puff(i)%sxx*xmap*xmap/(dxg*dxg)
    ryy  = puff(i)%syy*ymap*ymap/(dyg*dyg)

    IF( chkgrd(xx,yy,rxx,ryy) )THEN

      shh = 0.5*(puff(i)%sxx+puff(i)%syy)

      CALL get_topogOut( xbar,ybar,h,hx,hy,shh,ifld )

      CALL setPuffifld( puff(i),ifld )
      CALL setPuffirel( puff(i),0 )

      zbar = zrel + puff(i)%zbar + h
      npuf = npuf + 1

      IF( i /= npuf )THEN
        CALL copy_puff( puff(i),puff(npuf) )
        IF( nError /= NO_ERROR )GOTO 9999
      END IF

      puff(npuf)%xbar = xbar
      puff(npuf)%ybar = ybar
      puff(npuf)%zbar = zbar

    END IF

  END DO

!------ set puff for simple puff release (usually from nested model)

ELSE IF( reltyp(2:2) == 'P' )THEN

  ityp = puffRelease%ityp

! --- Check for wet particle release

  CALL check_wet_reltyp( ityp )
  IF( nError /= NO_ERROR )GOTO 9999

  naux = typeID(ityp)%npaux
  CALL check_newpuff()
  IF( nError /= NO_ERROR )GOTO 9999

  npuf = npuf + 1

  puff(npuf) = puffRelease                 !puffRelease has no aux array

  shh = 0.5*(puff(npuf)%sxx+puff(npuf)%syy)

  CALL get_topogOut( puffRelease%xbar,puffRelease%ybar,h,hx,hy,shh,ifld )

  puff(npuf)%zbar = puff(npuf)%zbar + h

  CALL setPuffifld( puff(npuf),ifld )
  CALL setPuffirel( puff(npuf),0 )

  CALL set_aux_rel( puff(npuf),naux )

!------ Override default with liquid release specification for a liquid puff, if set

  IF( IsLiquid(typeID(ityp)%icls) .OR. IsWetParticle(typeID(ityp)%icls) )THEN
    IF( puffRelLiquid%d /= NOT_SET_R )CALL put_liquid( puff(npuf),puffRelLiquid )
  END IF

!------ Set puff for simple instantaneous release

ELSE

  lGroundRel = lter .AND. zrel==0.0
  IF( reltyp(2:2) == 'X' .AND. lGroundRel )lGroundRel = sigRxz==0.0 .AND. sigRyz==0.0

  CALL mapfac( SNGL(xrel),SNGL(yrel),xmap,ymap )

  xbar = SNGL(xrel)
  ybar = SNGL(yrel)
  xx   = (xbar-xmin)/dxg
  yy   = (ybar-ymin)/dyg
  rxx  = (sigx*xmap/dxg)**2
  ryy  = (sigy*ymap/dyg)**2

  IF( chkgrd(xx,yy,rxx,ryy) )THEN

    shh = 0.5*(sigx**2+sigy**2)

    CALL get_topogOut( SNGL(xrel),SNGL(yrel),h,hx,hy,shh,ifld )

    zbar = zrel + h

    ityp = rel_ityp

! --- Check 2-phase liquid release

    CALL check_liquid_reltyp( ityp,zbar,l2phase )
    IF( nError /= NO_ERROR )GOTO 9999

! --- Check for wet particle release

    CALL check_wet_reltyp( ityp )
    IF( nError /= NO_ERROR )GOTO 9999

    naux = typeID(ityp)%npaux
    imat = typeID(ityp)%imat

    IF( rel_dist <= 0 )THEN   !--- Single size bin release

      IF( l2phase )THEN

        cmass_liq = cmass*rel_param(REL_WMFRAC_INDX)

        CALL set_puff_inst( zbar,cmass_liq,ityp,naux,ifld )
        IF( nError /= NO_ERROR )GOTO 9999

        ityp_vap  = material(imat)%ioffp + 1
        cmass_vap = cmass - cmass_liq
        naux_vap  = typeID(ityp_vap)%npaux

        CALL set_puff_inst( zbar,cmass_vap,ityp_vap,naux_vap,ifld )
        IF( nError /= NO_ERROR )GOTO 9999

      ELSE

        CALL set_puff_inst( zbar,cmass,ityp,naux,ifld )
        IF( nError /= NO_ERROR )GOTO 9999

      END IF

    ELSE     !--- Lognormal distribution

      CALL get_bounds( material(imat),nsg,pbounds )
      IF( nsg < 2 )THEN
        eMessage = 'Request for a lognormal distribution of release mass but'
        eInform  = TRIM(ADJUSTL(material(imat)%cmat))//' has only a single bin'
        eAction  = 'It is more efficient to use a single bin release'
        CALL CautionMessage()
      END IF
      IF( IsWetParticle(typeID(ityp)%icls) )CALL set_wetbin( pbounds,nsg,imat,rel_param(REL_WMFRAC_INDX) )

		  CALL check_logn( pbounds(1),pbounds(nsg+1),rel_param(REL_MMD_INDX), &
                                                 rel_param(REL_SIGMA_INDX),rxx )
      IF( rxx > 0.05 )THEN
        nError = WN_ERROR
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

!---- Set relative weights for size bins

      CALL logn_bin( pbounds,nsg,rel_param(REL_MMD_INDX),rel_param(REL_SIGMA_INDX),weight )

!---- Check for 2-phase liquid release; reset weights and release vapor puff

      IF( l2phase )THEN

        DO i = 1,nsg
          weight(i) = weight(i)*rel_param(REL_WMFRAC_INDX)
        END DO

        cmass_vap = cmass*(1.0-rel_param(REL_WMFRAC_INDX))
        ityp_vap  = material(imat)%ioffp + 1
        naux_vap  = typeID(ityp_vap)%npaux

        CALL set_puff_inst( zbar,cmass_vap,ityp_vap,naux_vap,ifld )
        IF( nError /= NO_ERROR )GOTO 9999

      END IF

!---- Release a puff in each size bin

      DO i = 1,nsg
        jtyp = ityp + i - 1
        CALL set_puff_inst( zbar,cmass*weight(i),jtyp,naux,ifld )
        IF( nError /= NO_ERROR )GOTO 9999
      END DO

    END IF

  END IF

END IF

IF( npuf > mpuf )THEN

! --- Write to log file

  IF( LogFlag ) THEN

    IF( name_rel /= ' ' )THEN
      WRITE(lun_log,114,IOSTAT=ios)'Instantaneous release  at t =', &
                          t/3600.,'hrs. from file ',TRIM(name_rel), &
                          ' with NPUFF = ',npuf
    ELSE
      WRITE(lun_log,115,IOSTAT=ios)'Instantaneous release  at t =', &
               t/3600.,'hrs. from input scenario file with NPUFF = ',npuf
    END IF
114 FORMAT(A,F7.2,A,A,A,I5)
115 FORMAT(A,F7.2,A,I5)

    IF( ios /= 0 )THEN
      nError   = WR_ERROR
      eMessage = 'Error writing SCIPUFF log file'
      CALL ReportFileName( eInform,'File=',file_log )
      GOTO 9999
    END IF

  END IF

!-------- Generate random realizations, if required

  IF( nRandom > 1 )CALL random_puff_loc( mpuf )

!-------- Rotate ground releases

  IF( lGroundRel )THEN
    WRITE(lun_log,'(A)')'  Rotating ground puffs from release '//TRIM(relDisplay)//' of '//TRIM(relmat)
    DO i = mpuf+1,npuf
      WRITE(lun_log,'(A,1P6G13.6)')'  Before rotation',puff(i)%sxx,puff(i)%sxy,puff(i)%sxz,puff(i)%syy,puff(i)%syz,puff(i)%szz
      shh = 0.5*(puff(i)%sxx+puff(i)%syy)
      jfld = 0
      ifld = 1
      DO WHILE( jfld < ifld )
        jfld = ifld
        CALL get_metFieldID( puff(i)%xbar,puff(i)%ybar,0.0,shh,ifld )
        IF( nError /= NO_ERROR )GOTO 9999
      END DO
      CALL get_topogIn( puff(i)%xbar,puff(i)%ybar,h,hx,hy,ifld )
      CALL dense_rot_norm( 0.,0.,hx,hy,puff(i) )
      WRITE(lun_log,'(A,1P6G13.6)')'  After  rotation',puff(i)%sxx,puff(i)%sxy,puff(i)%sxz,puff(i)%syy,puff(i)%syz,puff(i)%szz
    END DO
  END IF

ELSE

  IF( LogFlag ) THEN
    WRITE(lun_log,115,IOSTAT=ios)'Instantaneous release  at t =', &
                                t/3600.,'hrs. outside domain - ignored'
    IF( ios /= 0 )THEN
      nError   = WR_ERROR
      eMessage = 'Error writing SCIPUFF log file'
      CALL ReportFileName( eInform,'File=',file_log )
      GOTO 9999
    END IF
  END IF

  nError = WN_ERROR

  WRITE(eMessage,*)'Release at t =',t/3600.,'hrs. is outside domain'
  eInform = 'Release will be ignored'

  CALL WarningMessage( .TRUE. )
  IF( nError /= NO_ERROR )GOTO 9999
  nRelOutsideDomain = nRelOutsideDomain + 1

END IF

!---- Reset eRoutine if no errors

eRoutine = CHAR(0)

9999 CONTINUE

RETURN
END

!===============================================================================

SUBROUTINE StoreInstRel()

USE scipuff_fi

IMPLICIT NONE

INTEGER alloc_stat

TYPE( StoreReleaseT ), POINTER :: lastRel, nextRel

!--- Save instantaneous release definition for release during large timestep

IF( InstReleaseList%release%trel == NOT_SET_R )THEN

  CALL LoadRelease( InstReleaseList%release )

  mxtlev = MAX(mxtlev,7)

ELSE

  lastRel => InstReleaseList
  nextRel => lastRel%NextRelease

  DO WHILE( ASSOCIATED(nextRel) )
    lastRel => nextRel
    nextRel => lastRel%NextRelease
  END DO

  ALLOCATE( nextRel,STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    nError   = SZ_ERROR
    eMessage = 'Error allocating instantaneous release list'
    GOTO 9999
  END IF

  lastRel%NextRelease => nextRel

  CALL LoadRelease( nextRel%release )
  NULLIFY( nextRel%NextRelease )

END IF

lastRel => InstReleaseList
nextRel => lastRel%NextRelease

9999 CONTINUE

RETURN
END

!===============================================================================

SUBROUTINE CheckInstRelease( lev1,lev2 )

USE scipuff_fi

IMPLICIT NONE

INTEGER, INTENT( IN    ) :: lev1       ! current time level
INTEGER, INTENT( INOUT ) :: lev2       ! max time level

REAL    dts, trel_sav
INTEGER alloc_stat, mpuf, i
INTEGER n1, n2

CHARACTER(128) cmsg,cmsg2,cmsg3
CHARACTER(8)   ctmp

TYPE( releaseT )rel_save

TYPE( StoreReleaseT ), POINTER :: nextRel

IF( InstReleaseList%release%trel == NOT_SET_R )RETURN

lev2 = MAX(lev2,7)
dts  = delt * (0.5**lev2)

IF( 3600.*InstReleaseList%release%trel < t+dts )THEN

  mpuf = npuf

!----- Save current release (if there is one)

  trel_sav = trel
  IF( trel /= 1.0E+36 .AND. trel /= NOT_SET_R )THEN
    CALL LoadRelease( rel_save )
    IF( nError /= NO_ERROR )THEN
      nError   = IV_ERROR
      eRoutine = 'CheckInstRelease'
      eMessage = 'Unable to save release structure'
      eInform  = 'LoadRelease failure'
      GOTO 9999
    END IF
  END IF

  DO WHILE( 3600.*InstReleaseList%release%trel < t+dts )

    CALL UnloadRelease( InstReleaseList%release )

    nextRel => InstReleaseList%NextRelease

    IF( ASSOCIATED(nextRel) )THEN
      InstReleaseList%release = nextRel%release
      IF( ASSOCIATED(nextRel%NextRelease) )THEN
        InstReleaseList%NextRelease => nextRel%NextRelease
      ELSE
        NULLIFY( InstReleaseList%NextRelease )
      END IF
      DEALLOCATE( nextRel,STAT=alloc_stat )
    ELSE
      InstReleaseList%release%trel = NOT_SET_R
    END IF

    CALL valid_scn()
    IF( nError /= NO_ERROR )GOTO 9999

      CALL i_release( .TRUE. )
      IF( nError /= NO_ERROR )GOTO 9999

    IF( InstReleaseList%release%trel == NOT_SET_R )EXIT

  END DO

!----- Restore SCIPUFF release if necessary

  trel = trel_sav
  IF( trel /= 1.0E+36 .AND. trel /= NOT_SET_R )THEN
    CALL UnloadRelease( rel_save )
    IF( nError /= NO_ERROR )THEN
      nError   = IV_ERROR
      eRoutine = 'CheckInstRelease'
      eMessage = 'Unable to unload saved release structure'
      eInform  = 'UnloadRelease failure'
      GOTO 9999
    END IF
  END IF

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
    CALL set_rel( n1,n2,lev1 )
    IF( nError /= NO_ERROR )GOTO 9999

    DO i = mpuf+1,npuf
      lev2 = MAX(lev2,puff(i)%idtl)
    END DO

  END IF

END IF

9999 CONTINUE

RETURN
END

!===============================================================================

SUBROUTINE add_ext( file,name,ext )

USE DefSize_fd

IMPLICIT NONE

CHARACTER(*), INTENT( OUT ) :: file
CHARACTER(*), INTENT( IN  ) :: name, ext

CHARACTER(PATH_MAXLENGTH) ftmp, path

INTEGER i

CALL SplitName( name,ftmp,path )

i = INDEX(ftmp,'.')

IF( i == 0 )THEN
  file = name//'.'//ext
ELSE
  file = name
END IF

RETURN
END

!============================================================================

SUBROUTINE set_puff_inst( zbar,xmass,ityp,naux,ifld )

USE scipuff_fi

IMPLICIT NONE

REAL,    INTENT( IN ) :: xmass, zbar
INTEGER, INTENT( IN ) :: ityp, naux, ifld

IF( xmass <= 0.0 )RETURN

CALL check_newpuff()
IF( nError /= NO_ERROR )GOTO 9999

npuf = npuf + 1

CALL set_puff_rel( puff(npuf),xmass,zbar,ityp,naux,ifld )

RETURN

9999 CONTINUE

eRoutine = 'set_puff_inst'

RETURN
END

!============================================================================

SUBROUTINE random_puff_loc( mpuf )

USE scipuff_fi
USE relparam_fd

IMPLICIT NONE

INTEGER, INTENT( IN ) :: mpuf

INTEGER ireal, npufo, nb_rel, i, ipuf, naux, ifld
REAL    shh, zz, h, hx, hy

nb_rel = npuf - mpuf
npufo  = npuf

!---- Set release data structure

CALL SetDataRelID( nRandom,rel_param(REL_SPREAD_INDX),puff(npuf) )

!-------- Generate nRandom copies

DO ireal = 2,nRandom
  DO i = mpuf+1,npufo
    naux = typeID(puff(i)%ityp)%npaux
    CALL check_newpuff()
    IF( nError /= NO_ERROR )THEN
      eRoutine = 'random_puff_loc'
      GOTO 9999
    END IF

    npuf = npuf + 1

    CALL copy_puff( puff(i),puff(npuf) )
    IF( nError /= NO_ERROR )GOTO 9999

  END DO
END DO

!------ Determine height AGL

IF( lter )THEN
  shh = 0.5*(puff(npuf)%sxx+puff(npuf)%syy)
  CALL get_topogOut( puff(npuf)%xbar,puff(npuf)%ybar,h,hx,hy,shh,ifld )
  zz = MAX(puff(npuf)%zbar-h,0.01*SQRT(puff(npuf)%szz))
END IF

!-----  Randomize puff locations

DO ireal = 1,nRandom
  DO i = mpuf+1,npufo
    ipuf = (ireal-1)*nb_rel + i
    puff(ipuf)%xbar = puff(ipuf)%xbar + xRandom(ireal)
    puff(ipuf)%ybar = puff(ipuf)%ybar + yRandom(ireal)
    IF( lter )THEN
      CALL get_topogIn( puff(ipuf)%xbar,puff(ipuf)%ybar,h,hx,hy,ifld )
      puff(ipuf)%zbar = h + MAX(0.,zz + zRandom(ireal))   !Set to constant height AGL + random displacement
    ELSE
      puff(ipuf)%zbar = MAX(0.,puff(ipuf)%zbar + zRandom(ireal))   !Set to constant height AGL + random displacement
    END IF
  END DO
END DO

DO i = mpuf+1,npuf
  CALL setPuffirel( puff(i),numRelID )
END DO

9999 CONTINUE

RETURN
END
