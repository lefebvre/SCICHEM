!*******************************************************************************
!$RCSfile$
!$Revision$
!$Date$
!*******************************************************************************
SUBROUTINE DezoneCell( cgrid,maxg,icell0,i0 )

USE dezone_fi
USE srfparam_fd

IMPLICIT NONE

INTEGER, INTENT( IN )          :: maxg    !Grid size
INTEGER, INTENT( IN )          :: icell0  !Parent cell
INTEGER, INTENT( IN )          :: i0      !Base subcell
REAL,    POINTER, DIMENSION(:) :: cgrid   !Grid data

INTEGER ic, icell, i, ip
REAL    sum, cbar, vsum

!------ Add the 4 subcells into the parent cell

DO ic = 1,dezone_nvar

  ip    = (ic-1)*maxg
  icell = i0 + ip

  SELECT CASE( dezone_type(ic) )
    CASE( DEZONE_MEAN,DEZONE_VAR,DEZONE_SCALE,DEZONE_AVG )
      sum = 0.
      DO i = 0,3
        sum = sum + cgrid(icell+i)
      END DO
      cgrid(icell0+ip) = cgrid(icell0+ip) + 0.25*sum

    CASE DEFAULT
  END SELECT

  SELECT CASE( dezone_type(ic) )
    CASE( DEZONE_MEAN )
      cbar = 0.25*sum
      vsum = 0.
      DO i = 0,3
        vsum = vsum + (cgrid(icell+i)-cbar)**2
      END DO

    CASE( DEZONE_VAR )
      cgrid(icell0+ip) = cgrid(icell0+ip) + 0.25*vsum
      IF( sum <= 1.E-30 )THEN
        vsum = 0.
      ELSE
        vsum = vsum/sum
      END IF

    CASE( DEZONE_SCALE )
      cgrid(icell0+ip) = cgrid(icell0+ip) + 0.25*vsum*sum

  END SELECT

END DO

RETURN
END

!==============================================================================

SUBROUTINE init_dezone( nv,stype )

USE error_fi
USE dezone_fi

IMPLICIT NONE

INTEGER,               INTENT( IN ) :: nv
INTEGER, DIMENSION(*), INTENT( IN ) :: stype

INTEGER ios, i

dezone_nvar = nv

ALLOCATE( dezone_type(dezone_nvar),STAT=ios )
IF( ios == 0 )ALLOCATE( dezone_auxID(nv),STAT=ios )
IF( ios /= 0 )THEN
  nError = SZ_ERROR
  eRoutine = 'init_dezone'
  eMessage = 'Error allocating dezone types'
  GOTO 9999
END IF

DO i = 1,dezone_nvar

  dezone_type(i)  = MOD(stype(i),256)
  dezone_auxID(i) = stype(i)/256

END DO

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE exit_dezone()

USE dezone_fi

IMPLICIT NONE

INTEGER irv

DEALLOCATE( dezone_type,STAT=irv )
DEALLOCATE( dezone_auxID,STAT=irv )

RETURN
END
