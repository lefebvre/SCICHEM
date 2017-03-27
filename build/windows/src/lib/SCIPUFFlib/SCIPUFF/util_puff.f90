!*******************************************************************************
!$RCSfile$
!$Revision$
!$Date$
!*******************************************************************************

SUBROUTINE check_newpuff()

USE scipuff_fi

IMPLICIT NONE

IF( npuf >= MAXPUF )THEN
  nError   = SZ_ERROR
  eMessage = 'Too many puffs'
  WRITE(eInform,'(A,I8)')'Maximum number is ',MAXPUF
END IF

RETURN
END

!===============================================================================

SUBROUTINE check_splitpuff()

!----- Checks for SPLIT routine, allowing space for new releases.
!      N.B. only sets nError, since SPLIT resets after turning off splitting flag

USE scipuff_fi

IMPLICIT NONE

IF( npuf >= NINT(0.99*FLOAT(MAXPUF)) )THEN
  nError = SZ_ERROR
END IF

RETURN
END

!===============================================================================

RECURSIVE INTEGER FUNCTION next_puff()

USE scipuff_fi, ONLY : npuf

IMPLICIT NONE

npuf = npuf + 1
next_puff = npuf

RETURN
END

!===============================================================================

SUBROUTINE set_auxskp()

!  Set skip size for dynamic puffs

USE scipuff_fi

IMPLICIT NONE

IF( dynamic )THEN
  nskp_dyn_gas = NAUX_DYNAMICS_GAS
  nskp_dyn     = NAUX_DYNAMICS_PART
  IF( dense_gas )THEN
     nskp_dyn_gas = nskp_dyn_gas + NAUX_DENSE_GAS
     nskp_dyn     = nskp_dyn     + NAUX_DENSE_PART
  END IF
  IF( buoy_gas )THEN
    nskp_dyn_gas = nskp_dyn_gas + NAUX_BUOY
    nskp_dyn     = nskp_dyn     + NAUX_BUOY
  END IF
ELSE
  nskp_dyn_gas = 0
  nskp_dyn     = 0
END IF

RETURN
END

!===============================================================================

SUBROUTINE get_dynamics( p,pd )

USE scipuff_fi

IMPLICIT NONE

TYPE( puff_str ),      INTENT( IN  ) :: p
TYPE( puff_dynamics ), INTENT( OUT ) :: pd

INTEGER ipaux

LOGICAL, EXTERNAL :: IsGas

ipaux  = 1
pd%wcb = p%aux(ipaux)
ipaux  = ipaux + 1
pd%ctb = p%aux(ipaux)
ipaux  = ipaux + 1
pd%wcp = p%aux(ipaux)
ipaux  = ipaux + 1
pd%ctp = p%aux(ipaux)

ipaux  = ipaux + 1
pd%ucb = p%aux(ipaux)
ipaux  = ipaux + 1
pd%vcb = p%aux(ipaux)
ipaux  = ipaux + 1
pd%ucp = p%aux(ipaux)
ipaux  = ipaux + 1
pd%vcp = p%aux(ipaux)

IF( IsGas(typeID(p%ityp)%icls) )THEN
  ipaux = ipaux + 1
  pd%un = p%aux(ipaux)
  ipaux = ipaux + 1
  pd%vn = p%aux(ipaux)
  ipaux = ipaux + 1
  pd%w  = p%aux(ipaux)
  ipaux = ipaux + 1
  pd%t  = p%aux(ipaux)
ELSE
  pd%w  = 0.0
  pd%t  = 0.0
  pd%un = 0.0
  pd%vn = 0.0
END IF

IF( buoy_gas )THEN
  ipaux  = ipaux + 1
  pd%bcb = p%aux(ipaux)
  ipaux  = ipaux + 1
  pd%bcp = p%aux(ipaux)
ELSE
  pd%bcb = 0.0
  pd%bcp = 0.0
END IF

IF( dense_gas .AND. p%c > SMALL )THEN
  ipaux = ipaux + 1
  pd%u  = p%aux(ipaux) / p%c
  ipaux = ipaux + 1
  pd%v  = p%aux(ipaux) / p%c
  ipaux   = ipaux + 1
  pd%dudx = p%aux(ipaux) / p%c
  ipaux   = ipaux + 1
  pd%dudy = p%aux(ipaux) / p%c
  ipaux   = ipaux + 1
  pd%dvdx = p%aux(ipaux) / p%c
  ipaux   = ipaux + 1
  pd%dvdy = p%aux(ipaux) / p%c

  IF( IsGas(typeID(p%ityp)%icls) )THEN
    ipaux = ipaux + 1
    pd%u0 = p%aux(ipaux) / p%c
    ipaux = ipaux + 1
    pd%X  = p%aux(ipaux) / p%c
    ipaux = ipaux + 1
    pd%Y  = p%aux(ipaux) / p%c
    ipaux = ipaux + 1
    pd%sn = p%aux(ipaux) / p%c
    ipaux = ipaux + 1
    pd%cs = p%aux(ipaux) / p%c
  ELSE
    pd%u0 = 0.0
    pd%X  = 0.0
    pd%Y  = 0.0
    pd%sn = 0.0
    pd%cs = 0.0
  END IF

ELSE

  pd%u    = 0.0
  pd%v    = 0.0
  pd%dudx = 0.0
  pd%dudy = 0.0
  pd%dvdx = 0.0
  pd%dvdy = 0.0
  pd%u0   = 0.0
  pd%X    = 0.0
  pd%Y    = 0.0
  pd%sn   = 0.0
  pd%cs   = 0.0

END IF

RETURN
END

!===============================================================================

SUBROUTINE put_dynamics( p,pd )

USE scipuff_fi

IMPLICIT NONE

TYPE( puff_str ),      INTENT( INOUT ) :: p
TYPE( puff_dynamics ), INTENT( IN    ) :: pd

INTEGER ipaux

LOGICAL, EXTERNAL :: IsGas

ipaux = 1
p%aux(ipaux) = pd%wcb
ipaux = ipaux + 1
p%aux(ipaux) = pd%ctb
ipaux = ipaux + 1
p%aux(ipaux) = pd%wcp
ipaux = ipaux + 1
p%aux(ipaux) = pd%ctp

ipaux = ipaux + 1
p%aux(ipaux) = pd%ucb
ipaux = ipaux + 1
p%aux(ipaux) = pd%vcb
ipaux = ipaux + 1
p%aux(ipaux) = pd%ucp
ipaux = ipaux + 1
p%aux(ipaux) = pd%vcp

IF( IsGas(typeID(p%ityp)%icls) )THEN
  ipaux = ipaux + 1
  p%aux(ipaux) = pd%un
  ipaux = ipaux + 1
  p%aux(ipaux) = pd%vn
  ipaux = ipaux + 1
  p%aux(ipaux) = pd%w
  ipaux = ipaux + 1
  p%aux(ipaux) = pd%t
END IF

IF( buoy_gas )THEN
  ipaux = ipaux + 1
  p%aux(ipaux) = pd%bcb
  ipaux = ipaux + 1
  p%aux(ipaux) = pd%bcp
END IF

IF( dense_gas )THEN
  ipaux = ipaux + 1
  p%aux(ipaux) = pd%u * p%c
  ipaux = ipaux + 1
  p%aux(ipaux) = pd%v * p%c
  ipaux = ipaux + 1
  p%aux(ipaux) = pd%dudx * p%c
  ipaux = ipaux + 1
  p%aux(ipaux) = pd%dudy * p%c
  ipaux = ipaux + 1
  p%aux(ipaux) = pd%dvdx * p%c
  ipaux = ipaux + 1
  p%aux(ipaux) = pd%dvdy * p%c

  IF( IsGas(typeID(p%ityp)%icls) )THEN
    ipaux = ipaux + 1
    p%aux(ipaux) = pd%u0 * p%c
    ipaux = ipaux + 1
    p%aux(ipaux) = pd%X * p%c
    ipaux = ipaux + 1
    p%aux(ipaux) = pd%Y * p%c
    ipaux = ipaux + 1
    p%aux(ipaux) = pd%sn * p%c
    ipaux = ipaux + 1
    p%aux(ipaux) = pd%cs * p%c
  END IF

END IF

RETURN
END

!===============================================================================

SUBROUTINE get_totalcc( p,pt )

USE scipuff_fi

IMPLICIT NONE

TYPE( puff_str )    , INTENT( IN  ) :: p
TYPE( puff_totalcc ), INTENT( OUT ) :: pt

INTEGER nskp

LOGICAL, EXTERNAL :: IsGas

IF( IsGas(typeID(p%ityp)%icls) )THEN
  nskp = nskp_dyn_gas
ELSE
  nskp = nskp_dyn
END IF

pt%cctb = p%aux(nskp+1)
pt%cct  = p%aux(nskp+2)

RETURN
END

!===============================================================================

SUBROUTINE put_totalcc( p,pt )

USE scipuff_fi

IMPLICIT NONE

TYPE( puff_str )    , INTENT( INOUT ) :: p
TYPE( puff_totalcc ), INTENT( IN    ) :: pt

INTEGER nskp

LOGICAL, EXTERNAL :: IsGas

IF( IsGas(typeID(p%ityp)%icls) )THEN
  nskp = nskp_dyn_gas
ELSE
  nskp = nskp_dyn
END IF

p%aux(nskp+1) = pt%cctb
p%aux(nskp+2) = pt%cct

RETURN
END

!===============================================================================

SUBROUTINE get_liquid( p,pq )

USE scipuff_fi
USE UtilMtlAux

IMPLICIT NONE

TYPE( puff_str )   , INTENT( IN  ) :: p
TYPE( puff_liquid ), INTENT( OUT ) :: pq

INTEGER nskp

TYPE( part_material   ) :: partmatl

LOGICAL, EXTERNAL :: IsWetParticle

nskp = nskp_dyn

IF( typeID(p%ityp)%ltot )nskp = nskp + NAUX_TOTALCC

IF( p%c > SMALL )THEN

  pq%d    = p%aux(nskp+1)/p%c
  pq%sigd = SQRT(MAX(0.0,p%aux(nskp+2)/p%c - pq%d*pq%d))
  pq%t    = p%aux(nskp+3)/p%c

ELSE

  IF( IsWetParticle(typeID(p%ityp)%icls) )THEN
    CALL GetParticleParam( partmatl,material(typeID(p%ityp)%imat)%iaux,typeID(p%ityp)%igrp,mat_aux )
    pq%d    = 2.*partmatl%dbar  !Set wet droplet size to twice particle size
    pq%sigd = 0.
    pq%t    = 300.
  ELSE
    pq%d    = 1.0E-7      !Set to 0.1micron for small mass
    pq%sigd = 0.
    pq%t    = 0.
  END IF

END IF

pq%ccs   = p%aux(nskp+4)
pq%tevap = p%aux(nskp+5)

RETURN
END

!===============================================================================

SUBROUTINE put_liquid( p,pq )

USE scipuff_fi

IMPLICIT NONE

TYPE( puff_str )   , INTENT( INOUT ) :: p
TYPE( puff_liquid ), INTENT( IN    ) :: pq

INTEGER nskp

nskp = nskp_dyn

IF( typeID(p%ityp)%ltot )nskp = nskp + NAUX_TOTALCC

p%aux(nskp+1) = pq%d*p%c
p%aux(nskp+2) = (pq%sigd*pq%sigd+pq%d*pq%d)*p%c
p%aux(nskp+3) = pq%t*p%c
p%aux(nskp+4) = pq%ccs
p%aux(nskp+5) = pq%tevap

RETURN
END

!===============================================================================

SUBROUTINE get_aerosol( p,pa )

USE scipuff_fi

IMPLICIT NONE

TYPE( puff_str )    , INTENT( IN  ) :: p
TYPE( puff_aerosol ), INTENT( OUT ) :: pa

INTEGER nskp

nskp = nskp_dyn_gas

IF( typeID(p%ityp)%ltot )nskp = nskp + NAUX_TOTALCC

IF( p%c > SMALL )THEN
  pa%fl    = p%aux(nskp+1)/p%c
  pa%co    = p%aux(nskp+2)/p%c
  pa%fw    = p%aux(nskp+3)/p%c
  pa%tevap = p%aux(nskp+4)
ELSE
  pa%fl    = 0.
  pa%co    = 0.
  pa%fw    = 0.
  pa%tevap = 0.
END IF

RETURN
END

!===============================================================================

SUBROUTINE put_aerosol( p,pa )

USE scipuff_fi

IMPLICIT NONE

TYPE( puff_str )    , INTENT( INOUT ) :: p
TYPE( puff_aerosol ), INTENT( IN    ) :: pa

INTEGER nskp

nskp = nskp_dyn_gas

IF( typeID(p%ityp)%ltot )nskp = nskp + NAUX_TOTALCC

p%aux(nskp+1) = pa%fl*p%c
p%aux(nskp+2) = pa%co*p%c
p%aux(nskp+3) = pa%fw*p%c
p%aux(nskp+4) = pa%tevap

RETURN
END

!===============================================================================

SUBROUTINE get_static( p,ps )

USE scipuff_fi

IMPLICIT NONE

TYPE( puff_str )   , INTENT( IN  ) :: p
TYPE( puff_static ), INTENT( OUT ) :: ps

INTEGER nskp

nskp = typeID(p%ityp)%npaux

ps%sr    = p%aux(nskp+1)
ps%isnxt = TRANSFER(p%aux(nskp+2),ps%isnxt)
ps%isprv = TRANSFER(p%aux(nskp+3),ps%isprv)

RETURN
END

!===============================================================================

SUBROUTINE put_static( p,ps )

USE scipuff_fi

IMPLICIT NONE

TYPE( puff_str )   , INTENT( INOUT ) :: p
TYPE( puff_static ), INTENT( IN    ) :: ps

INTEGER nskp

nskp = typeID(p%ityp)%npaux

p%aux(nskp+1) = ps%sr
p%aux(nskp+2) = TRANSFER(ps%isnxt,p%aux(nskp+2))
p%aux(nskp+3) = TRANSFER(ps%isprv,p%aux(nskp+3))

RETURN
END

!===============================================================================

SUBROUTINE copy_puff( pold,pnew )

USE scipuff_fi

IMPLICIT NONE

TYPE( puff_str ), INTENT( IN    ) :: pold
TYPE( puff_str ), INTENT( OUT   ) :: pnew

INTEGER ios
INTEGER, EXTERNAL :: deallocatePuffAux

IF( ASSOCIATED(pnew%aux) )THEN
  ios = deallocatePuffAux( pnew )
  IF( ios /= 0 )THEN
    nError = UK_ERROR
    eRoutine = 'copy_puff'
    eMessage = 'Error deallocating existing puff aux array'
    WRITE(eInform,'(A,I0)')'IOS =',ios
    GOTO 9999
  END IF
END IF

pnew = pold

NULLIFY(pnew%aux)  !Since it now points to pold%aux

CALL copyPuffAux( pold,pnew )

9999 CONTINUE

RETURN
END

!===============================================================================

SUBROUTINE copy_puff_noaux( pold,pnew )

USE scipuff_fi

IMPLICIT NONE

TYPE( puff_str ), INTENT( IN    ) :: pold
TYPE( puff_str ), INTENT( OUT   ) :: pnew

INTEGER naux
REAL, DIMENSION(:), POINTER :: aux

NULLIFY(aux)

IF( ASSOCIATED(pnew%aux) )THEN
  naux = pnew%naux
  aux => pnew%aux
END IF

pnew = pold

IF( ASSOCIATED(aux) )THEN
  pnew%naux = naux
  pnew%aux => aux
  NULLIFY(aux)
ELSE
  pnew%naux = 0
  NULLIFY(pnew%aux)
END IF

9999 CONTINUE

RETURN
END

!===============================================================================

SUBROUTINE move_puff( pold,pnew )

USE scipuff_fi

IMPLICIT NONE

TYPE( puff_str ), INTENT( INOUT ) :: pold
TYPE( puff_str ), INTENT(   OUT ) :: pnew

pnew = pold

NULLIFY(pold%aux)

9999 CONTINUE

RETURN
END

!===============================================================================

SUBROUTINE copyPuffAux( pold,pnew )

USE puffstruct_fd
USE error_fi

IMPLICIT NONE

TYPE( puff_str ), INTENT( IN    ) :: pold
TYPE( puff_str ), INTENT( OUT   ) :: pnew

INTEGER i,ios

INTEGER, EXTERNAl :: allocatePuffAux

IF( pold%naux > 0 )THEN
  IF( .NOT.ASSOCIATED(pnew%aux) )THEN
    pnew%naux = pold%naux
    ios = allocatePuffAux( pnew )
    IF( ios /= 0 )THEN
      nError = SZ_ERROR
      eRoutine = 'copyPuffAux'
      eMessage = 'Error allocating puff aux array'
      WRITE(eInform,'(A,I0,A,I0)')'IOS =',ios,' :size =',pnew%naux
      GOTO 9999
    END IF
  ELSE IF( pnew%naux < pold%naux )THEN
    nError = SZ_ERROR
    eRoutine = 'copyPuffAux'
    eMessage = 'Insufficient space to copy puff aux array'
    WRITE(eInform,'(A,I0,A,I0)')'Old size =',pold%naux,' : New size =',pnew%naux
    GOTO 9999
  END IF
  DO i = 1,pold%naux
    pnew%aux(i) = pold%aux(i)
  END DO
ELSE
 pnew%naux = 0
 NULLIFY( pnew%aux )
END IF

9999 CONTINUE

RETURN
END

!===============================================================================

SUBROUTINE reset_static_puffs()

USE scipuff_fi, ONLY: static
USE cont_rel_fi

IMPLICIT NONE

INTEGER icol

!==== Remove all static puffs (Pool definitions don't have statics)

DO icol = 1,numCollection
  IF( .NOT.cCollection(icol)%isActive )CYCLE
  IF( cCollection(icol)%isPool )CYCLE
  CALL remove_static_puffs( cCollection(icol) )
END DO

RETURN
END

!===============================================================================

SUBROUTINE remove_static_puffs( col )

USE scipuff_fi,  ONLY: static
USE cont_rel_fi, ONLY: initStatics
USE cont_rel_fd
USE error_fi

!  Remove all static puffs associated with release definition

IMPLICIT NONE

TYPE( cont_release_col), INTENT( INOUT ) :: col

INTEGER clev

TYPE( cont_release_def), POINTER :: def

INTEGER, EXTERNAL :: remove_static_puffs_set

NULLIFY(def)

IF( col%isPool )GOTO 9999      !This may not be necessary as it should never get called for a pool collection

IF( col%rStat%hasStatics )THEN
  col%rstat%tlev = 0
  def => col%firstDef
  DO WHILE( ASSOCIATED(def) )
    IF( col%rStat%hasStatics )THEN
      clev = remove_static_puffs_set( def%rSet )
      IF( nError /= NO_ERROR )GOTO 9999
      col%rstat%tlev = MAX( col%rstat%tlev,clev )
    END IF
    def => def%nextDef
  END DO
  NULLIFY(def)
  col%rStat%hasStatics = .FALSE.
END IF

IF( col%firstDef%isMoving )initStatics = static
col%rStat%doStatics = static

9999 CONTINUE

NULLIFY(def)

RETURN
END

!===============================================================================

INTEGER FUNCTION remove_static_puffs_set( set ) RESULT(clev)

USE scipuff_fi
USE error_fi
USE cont_rel_fd
USE cont_rel_functions

!  Remove all static puffs associated with release definition

IMPLICIT NONE

TYPE( cont_release_set), INTENT( INOUT ) :: set

INTEGER i, irel, ipuf, ios

TYPE( puff_static ) ps

clev = 0

IF( set%nrel > 0 )THEN
  DO irel = 1,set%nrel
    IF( set%rels(irel)%isStatic )THEN
      clev = MAX( clev,set%rels(irel)%basePuff%idtl )
      DO ipuf = 1,set%rels(irel)%npuff
        i = set%rels(irel)%relPuff(ipuf)%inxt
        DO WHILE( i > 0 )
          puff(i)%idtl = I_REMOVE
          CALL get_static( puff(i),ps )
          CALL remove_ipgrd( i )
          i = ps%isnxt
        END DO
      END DO
      ios = deallocate_contRel_puff( set%rels(irel) )
      IF( ios /= 0 )GOTO 9999
      IF( ASSOCIATED(set%rels(irel)%vapPuff) )THEN
        DO ipuf = 1,set%rels(irel)%npuff
          i = set%rels(irel)%vapPuff(ipuf)%inxt
          DO WHILE( i > 0 )
            puff(i)%idtl = I_REMOVE
            CALL get_static( puff(i),ps )
            CALL remove_ipgrd( i )
            i = ps%isnxt
          END DO
        END DO
        ios = deallocate_contRel_vapor( set%rels(irel) )
        IF( ios /= 0 )THEN
          nError   = UK_ERROR
          eRoutine = 'remove_static_puffs'
          eMessage = 'Error deallocating scipuff vapor release puffs'
          WRITE(eInform,'(A,I0)')'IOS = ',ios
          GOTO 9999
        END IF
      END IF
      set%rels(irel)%npuff = 0
      set%rels(irel)%isStatic = .FALSE.
    END IF
  END DO
END IF

9999 CONTINUE

RETURN
END

!===============================================================================

SUBROUTINE restart_static_puffs()

USE scipuff_fi

!  Remove ALL static puffs for restarting

IMPLICIT NONE

INTEGER i, iout, ios
INTEGER, EXTERNAL :: deallocatePuffAux

iout = 0

DO i = 1,npuf
  IF( puff(i)%idtl >= 0 )THEN
    iout = iout + 1
    IF( i /= iout )CALL move_puff( puff(i),puff(iout) )
  ELSE
    ios = deallocatePuffAux( puff(i) )
  END IF
END DO

npuf = iout

RETURN
END

!===============================================================================

SUBROUTINE scale_puff( p,scale )

USE scipuff_fi

IMPLICIT NONE

TYPE( puff_str ), INTENT( INOUT ) :: p
REAL,             INTENT( IN    ) :: scale

CALL scale_psum( p,scale )
CALL scale_paux( p,scale )

RETURN
END

!===============================================================================

SUBROUTINE scale_paux( p,scale )

USE scipuff_fi

IMPLICIT NONE

TYPE( puff_str ), INTENT( INOUT ) :: p
REAL,             INTENT( IN    ) :: scale

INTEGER i

IF( p%naux > 0 )THEN
  DO i = 1,p%naux
    IF( p%aux(i) /= NOT_SET_R )THEN
      p%aux(i) = scale*p%aux(i)
    END IF
  END DO
END IF

RETURN
END

!===============================================================================

SUBROUTINE copy_scale_paux( pnew,pold,scale )

USE scipuff_fi

IMPLICIT NONE

TYPE( puff_str ), INTENT( IN    ) :: pold
TYPE( puff_str ), INTENT( OUT   ) :: pnew
REAL,             INTENT( IN    ) :: scale

INTEGER i,ios

INTEGER, EXTERNAL :: allocatePuffAux

IF( pold%naux > 0 )THEN
  IF( .NOT.ASSOCIATED(pnew%aux) )THEN
    pnew%naux = pold%naux
    ios = allocatePuffAux( pnew )
    IF( ios /= 0 )THEN
      nError   = SZ_ERROR
      eRoutine = 'copy_scale_paux'
      eMessage = 'Error allocating puff aux array'
      WRITE(eInform,'(A,I0,A,I0)')'IOS =',ios,' :size =',pnew%naux
      GOTO 9999
    END IF
  ELSE IF( pnew%naux < pold%naux )THEN
    nError   = SZ_ERROR
    eRoutine = 'copy_scale_paux'
    eMessage = 'Insufficient space to copy aux array'
    WRITE(eInform,'(A,I0,A,I0)')'Old size =',pold%naux,' : new size =',pnew%naux
    GOTO 9999
  END IF
  DO i = 1,pold%naux
    pnew%aux(i) = scale*pold%aux(i)
  END DO
ELSE
 pnew%naux = 0
 NULLIFY( pnew%aux )
END IF

9999 CONTINUE

RETURN
END

!===============================================================================

SUBROUTINE sum_paux( p1,p2 )

USE scipuff_fi

IMPLICIT NONE

TYPE( puff_str ), INTENT( IN    ) :: p1
TYPE( puff_str ), INTENT( INOUT ) :: p2

INTEGER i

IF( p2%naux > 0 )THEN
  DO i = 1,typeID(p2%ityp)%npaux            !Can't use p2%naux because of NWPN PA stuff should not be included here
    IF( p2%aux(i) == NOT_SET_R )THEN
      IF(p1%aux(i) == NOT_SET_R )THEN
        p2%aux(i) = NOT_SET_R
      ELSE
        p2%aux(i) = p1%aux(i)
      END IF
    ELSE
      IF( p1%aux(i) /= NOT_SET_R )THEN
        p2%aux(i) = p2%aux(i) + p1%aux(i)
      END IF
    END IF
  END DO
END IF

RETURN
END

!===============================================================================

SUBROUTINE scale_psum( p,scale )

USE struct_fd

IMPLICIT NONE

REAL,             INTENT( IN    ) :: scale
TYPE( puff_str ), INTENT( INOUT ) :: p

TYPE( puff_str_xc ) p_xc

p_xc = TRANSFER(p,p_xc)

p_xc%psum = scale * p_xc%psum

p = TRANSFER(p_xc,p)

RETURN
END

!===============================================================================

SUBROUTINE zero_puff_NOaux( p )

USE struct_fd

IMPLICIT NONE

TYPE( puff_str_ri_NOaux ), INTENT( OUT ) :: p

TYPE( puff_str_ri_NOaux ) p_ri

p_ri%p_real = 0.0
p_ri%p_int  = 0

p = TRANSFER(p_ri,p)

RETURN
END

!===============================================================================

SUBROUTINE scale_dynamics( pd,rat )

USE scipuff_fi

IMPLICIT NONE

REAL,                  INTENT( IN    ) :: rat
TYPE( puff_dynamics ), INTENT( INOUT ) :: pd

TYPE( puff_dynamics_data ) pdx

INTEGER i

pdx = TRANSFER(pd,pdx)

DO i = 1,NAUX_DYNAMICS
  pdx%data(i) = rat*pdx%data(i)
END DO

IF( buoy_gas )THEN
  DO i = 1,NAUX_BUOY
    pdx%data_buoy(i) = rat*pdx%data_buoy(i)
  END DO
END IF

pd = TRANSFER(pdx,pd)

RETURN
END

!===============================================================================-

SUBROUTINE update_static_pointers( iout )

USE scipuff_fi
USE cont_rel_fi
USE cont_rel_functions

IMPLICIT NONE

INTEGER, INTENT( IN ) :: iout

INTEGER iprv, inxt

TYPE( puff_static )    ps
TYPE( cont_release_id) triad

CALL get_static( puff(iout),ps )

iprv = ps%isprv
inxt = ps%isnxt

IF( iprv < 0 )THEN
  triad = relPuffID(-iprv)
  IF( triad%ipuf > cDefinition(triad%idef)%rSet%rels(triad%irel)%npuff )THEN
    triad%ipuf = triad%ipuf - cDefinition(triad%idef)%rSet%rels(triad%irel)%npuff
    cDefinition(triad%idef)%rSet%rels(triad%irel)%vapPuff(triad%ipuf)%inxt = iout
  ELSE
    cDefinition(triad%idef)%rSet%rels(triad%irel)%relPuff(triad%ipuf)%inxt = iout
  END IF
ELSE
  CALL get_static( puff(iprv),ps )
  ps%isnxt = iout
  CALL put_static( puff(iprv),ps )
END IF

IF( inxt < 0 )THEN
  triad = relPuffID(-inxt)
  IF( triad%ipuf > cDefinition(triad%idef)%rSet%rels(triad%irel)%npuff )THEN
    triad%ipuf = triad%ipuf - cDefinition(triad%idef)%rSet%rels(triad%irel)%npuff
    cDefinition(triad%idef)%rSet%rels(triad%irel)%vapPuff(triad%ipuf)%iprv = iout
  ELSE
    cDefinition(triad%idef)%rSet%rels(triad%irel)%relPuff(triad%ipuf)%iprv = iout
  END IF
ELSE
  CALL get_static( puff(inxt),ps )
  ps%isprv = iout
  CALL put_static( puff(inxt),ps )
END IF

RETURN
END

!===============================================================================

SUBROUTINE update_static_rel_pointers( def )

USE scipuff_fi
USE cont_rel_fi
USE cont_rel_functions

IMPLICIT NONE

TYPE( cont_release_def ), INTENT( INOUT ) :: def

TYPE( puff_static ) ps

INTEGER is, irel, ipuf, sid

TYPE( cont_release_id ):: id

IF( static )THEN
  id%idef = def%ID
  IF( def%rSet%nrel > 0 )THEN
    DO irel = 1,def%rSet%nrel
      id%irel = irel
      DO ipuf = 1,def%rSet%rels(irel)%npuff
        id%ipuf = ipuf
        sid = staticID(id)
        IF( def%rSet%rels(irel)%relPuff(ipuf)%inxt > 0 )THEN
          is = def%rSet%rels(irel)%relPuff(ipuf)%inxt
          CALL get_static( puff(is),ps )
          ps%isprv = -sid
          CALL put_static( puff(is),ps )
        END IF
        IF( def%rSet%rels(irel)%relPuff(ipuf)%iprv > 0 )THEN
          is = def%rSet%rels(irel)%relPuff(ipuf)%iprv
          CALL get_static( puff(is),ps )
          ps%isnxt = -sid
          CALL put_static( puff(is),ps )
        END IF
      END DO
      IF( ASSOCIATED( def%rSet%rels(irel)%vapPuff ) )THEN
        DO ipuf = 1,def%rSet%rels(irel)%npuff
          IF( def%rSet%rels(irel)%vapPuff(ipuf)%inxt > 0 )THEN
            id%ipuf = ipuf + def%rSet%rels(irel)%npuff
            sid = staticID(id)
            is = def%rSet%rels(irel)%vapPuff(ipuf)%inxt
            CALL get_static( puff(is),ps )
            ps%isprv = -sid
            CALL put_static( puff(is),ps )
            is = def%rSet%rels(irel)%vapPuff(ipuf)%iprv
            CALL get_static( puff(is),ps )
            ps%isnxt = -sid
            CALL put_static( puff(is),ps )
          END IF
        END DO
      END IF
    END DO
  END IF
END IF

RETURN
END

!===============================================================================

LOGICAL FUNCTION check_slope( hx,hy )

IMPLICIT NONE

REAL, INTENT( IN ) :: hx, hy

REAL, PARAMETER :: HSMIN = 1.E-4

check_slope = (ABS(hx) > HSMIN) .OR. (ABS(hy) > HSMIN)

RETURN
END

!===============================================================================

SUBROUTINE dump_puff( ipuf,p )

USE scipuff_fi
USE files_fi

IMPLICIT NONE

INTEGER,          INTENT( IN ) :: ipuf
TYPE( puff_str ), INTENT( IN ) :: p

INTEGER i, ios
REAL    h, hx, hy

INTEGER, EXTERNAL :: getPuffipgrd,getPuffifld

WRITE(lun_log,'(A)',IOSTAT=ios)'******** PUFF DUMP ************'
WRITE(lun_log,*,IOSTAT=ios)'Time = ',t,'( ',t/3600.,' )'
WRITE(lun_log,*,IOSTAT=ios)'Npuf = ',npuf
WRITE(lun_log,*,IOSTAT=ios)'Puff = ',ipuf
WRITE(lun_log,*,IOSTAT=ios)'Xbar = ',p%xbar, p%ybar, p%zbar
WRITE(lun_log,*,IOSTAT=ios)'Sig  = ',p%sxx,p%sxy,p%sxz,p%syy, &
                                            p%syz,p%szz
WRITE(lun_log,*,IOSTAT=ios)'Asig = ',p%axx,p%axy,p%axz,p%ayy, &
                                     p%ayz,p%azz,p%det
WRITE(lun_log,*,IOSTAT=ios)'Mass = ',p%c,p%cc,p%ccb,p%cfo
WRITE(lun_log,*,IOSTAT=ios)'hDiff= ',p%xuc,p%xvc,p%yvc,p%yvsc, &
                                                    p%yvbc
WRITE(lun_log,*,IOSTAT=ios)'vDiff= ',p%zwc,p%wc
WRITE(lun_log,*,IOSTAT=ios)'Scale= ',p%si,p%si2,p%sv,p%sr
WRITE(lun_log,*,IOSTAT=ios)'Zi   = ',p%zi,p%zc
WRITE(lun_log,*,IOSTAT=ios)'Vel  = ',p%uo,p%vo,p%wo
WRITE(lun_log,*,IOSTAT=ios)'type = ',p%ityp
WRITE(lun_log,*,IOSTAT=ios)'lev  = ',p%idtl,getPuffipgrd( p ),getPuffifld( p )
WRITE(lun_log,*,IOSTAT=ios)'Point= ',p%inxt,p%iprv,p%idtn,p%naux
IF( p%naux > 0 )THEN
  WRITE(lun_log,*,IOSTAT=ios)'nAux = ',p%naux
  WRITE(lun_log,*,IOSTAT=ios)'Aux  = ',(p%aux(i),i=1,p%naux)
END IF
CALL get_topogIn( p%xbar,p%ybar,h,hx,hy,getPuffifld(p) )
WRITE(lun_log,*,IOSTAT=ios)'Topo = ',h,hx,hy
WRITE(lun_log,'(A)',IOSTAT=ios)'*******************************'

RETURN
END

!===============================================================================

INTEGER FUNCTION set_lev( istep )

IMPLICIT NONE

INTEGER, INTENT( IN ) :: istep

INTEGER lev,itst

lev  = 0
itst = istep
DO WHILE( .NOT.BTEST(itst,0) )
  lev = lev + 1
  itst = ISHFT(itst,-1)
END DO

set_lev = lev

RETURN
END

!=======================================================================

SUBROUTINE check_tlist( lev1,lev2,ldbg )

USE scipuff_fi
USE files_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: lev1, lev2
LOGICAL, INTENT( IN ) :: ldbg

LOGICAL InList(MAXPUF)

INTEGER i,lev,ios
LOGICAL lfirst

!==== Initialize check array

DO i = 1,npuf
  InList(i) = .FALSE.
END DO

lfirst = .TRUE.

!==== Check Lists

DO lev = 0,mxtlev
  i = itfrst(lev)

  DO WHILE( i > 0 )

    IF( puff(i)%idtl /= lev )THEN
      IF( ldbg )THEN
        IF( (puff(i)%idtl /= I_REMOVE) .AND. &
               ((puff(i)%idtl<lev1) .OR. (puff(i)%idtl>lev2)) )THEN
          IF( lfirst )THEN
            WRITE(lun_log,*,IOSTAT=ios)'****** TLIST ERROR *******'
            WRITE(lun_log,*,IOSTAT=ios)'Time  =',t
            WRITE(lun_log,*,IOSTAT=ios)'Levels=',lev1,lev2
            lfirst = .FALSE.
          END IF
          WRITE(lun_log,*,IOSTAT=ios)'  Puff in wrong list'
          WRITE(lun_log,*,IOSTAT=ios)'    Puff =',i
          WRITE(lun_log,*,IOSTAT=ios)'    idtl =',puff(i)%idtl
          WRITE(lun_log,*,IOSTAT=ios)'    lev  =',lev
        END IF
      ELSE
        IF( lfirst )THEN
          WRITE(lun_log,*,IOSTAT=ios)'****** TLIST ERROR *******'
          WRITE(lun_log,*,IOSTAT=ios)'Time  =',t
          WRITE(lun_log,*,IOSTAT=ios)'Levels=',lev1,lev2
          lfirst = .FALSE.
        END IF
        WRITE(lun_log,*,IOSTAT=ios)'  Puff in wrong list'
        WRITE(lun_log,*,IOSTAT=ios)'    Puff =',i
        WRITE(lun_log,*,IOSTAT=ios)'    idtl =',puff(i)%idtl
        WRITE(lun_log,*,IOSTAT=ios)'    lev  =',lev
      END IF
    END IF

    InList(i) = .TRUE.
    i = puff(i)%idtn

  END DO

END DO

!==== Check puffs

DO i = 1,npuf
  IF( .NOT.InList(i) )THEN
    IF( puff(i)%idtl /= I_REMOVE )THEN
      IF( lfirst )THEN
        WRITE(lun_log,*,IOSTAT=ios)'****** TLIST ERROR *******'
        WRITE(lun_log,*,IOSTAT=ios)'Time  =',t
        WRITE(lun_log,*,IOSTAT=ios)'Levels=',lev1,lev2
        lfirst = .FALSE.
      END IF
      WRITE(lun_log,*,IOSTAT=ios)'  Puff not in list'
      WRITE(lun_log,*,IOSTAT=ios)'    Puff =',i
      WRITE(lun_log,*,IOSTAT=ios)'    idtl =',puff(i)%idtl
    END IF
  END IF
END DO

RETURN
END
