!*******************************************************************************
!$RCSfile$
!$Revision$
!$Date$
!*******************************************************************************
SUBROUTINE get_puff_material( ityp,pmatl )

USE scipuff_fi

IMPLICIT NONE

INTEGER,               INTENT( IN  ) :: ityp
TYPE( puff_material ), INTENT( OUT ) :: pmatl

INTEGER imat,igrp,iaux,icls

LOGICAL, EXTERNAL :: IsGas,IsParticle
LOGICAL, EXTERNAL :: IsLiquid,IsWetParticle
LOGICAL, EXTERNAL :: IsAerosol

imat = typeID(ityp)%imat
igrp = typeID(ityp)%igrp

iaux = material(imat)%iaux

icls = typeID(ityp)%icls
IF( IsAerosol(icls) )icls = material(imat)%icls

IF( IsGas(icls) )THEN

  CALL get_puff_material_gas( pmatl,iaux )

ELSE IF( IsParticle(icls) )THEN

  CALL get_puff_material_part( pmatl,iaux,igrp )

ELSE IF( IsWetParticle(icls) )THEN

  CALL get_puff_material_part( pmatl,iaux,igrp )

ELSE IF( IsLiquid(icls) )THEN

  CALL get_puff_material_liquid( pmatl,iaux,igrp )

ELSE

  nError   = IV_ERROR
  eMessage = 'Invalid Material Class'
  WRITE(eInform,*)'Class =',icls
  eRoutine = 'GetPuffMaterial'

END IF

RETURN
END

!------------------------------------------------------------------------------

SUBROUTINE put_puff_material( ityp,pmatl )

USE scipuff_fi

IMPLICIT NONE

INTEGER,               INTENT( IN ) :: ityp
TYPE( puff_material ), INTENT (IN ) :: pmatl

INTEGER imat, igrp, iaux, icls, ios

LOGICAL, EXTERNAL :: IsGas,IsParticle
LOGICAL, EXTERNAL :: IsLiquid,IsWetParticle

imat = typeID(ityp)%imat
igrp = typeID(ityp)%igrp

iaux = material(imat)%iaux

icls = typeID(ityp)%icls

IF( IsGas(icls) )THEN

  CALL put_puff_material_gas( pmatl,iaux )

ELSE IF( IsParticle(icls) )THEN

  CALL put_puff_material_part( pmatl,iaux,igrp )

ELSE IF( IsWetParticle(icls) )THEN

  CALL put_puff_material_part( pmatl,iaux,igrp )

ELSE IF( IsLiquid(icls) )THEN

  CALL put_puff_material_liquid( pmatl,iaux,igrp )

ELSE

  nError   = IV_ERROR
  eMessage = 'Invalid Material Class'
  WRITE(eInform,*,IOSTAT=ios)'Class =',icls
  eRoutine = 'PutPuffMaterial'

END IF

RETURN
END

!===============================================================================

INTEGER FUNCTION output_groups( mat )

USE scipuff_fi

IMPLICIT NONE

TYPE( material_str ), INTENT( IN ) :: mat

LOGICAL, EXTERNAL :: IsGas, IsParticle
LOGICAL, EXTERNAL :: IsLiquid, IsWetParticle

IF( IsGas(mat%icls) )THEN
  output_groups = 1
ELSE IF( IsParticle(mat%icls) )THEN
  output_groups = NINT(mat_aux(mat%iaux))
ELSE IF( IsParticle(mat%icls) .OR. IsWetParticle(mat%icls) )THEN
  output_groups = NINT(mat_aux(mat%iaux))
ELSE IF( IsLiquid(mat%icls) )THEN
  output_groups = 2
END IF

RETURN
END

!===============================================================================

REAL FUNCTION material_density( mat )

USE scipuff_fi

IMPLICIT NONE

TYPE( material_str ), INTENT( IN ) :: mat

LOGICAL, EXTERNAL :: IsGas, IsParticle
LOGICAL, EXTERNAL :: IsLiquid, IsWetParticle

IF( IsGas(mat%icls) )THEN
  material_density = mat_aux(mat%iaux)
ELSE IF( IsParticle(mat%icls) .OR. IsWetParticle(mat%icls) )THEN
  material_density = mat_aux(mat%iaux+1)
ELSE IF( IsLiquid(mat%icls) )THEN
  material_density = mat_aux(mat%iaux+MAXGMAUX+MAXLMAUXP+1)
END IF

RETURN
END

!===============================================================================

INTEGER FUNCTION num_puff_types( mat )

USE scipuff_fi

IMPLICIT NONE

TYPE( material_str ), INTENT( IN ) :: mat

LOGICAL, EXTERNAL :: IsGas, IsParticle
LOGICAL, EXTERNAL :: IsLiquid, IsWetParticle

IF( IsGas(mat%icls) )THEN
  num_puff_types = 1
ELSE IF( IsParticle(mat%icls) )THEN
  num_puff_types = NINT(mat_aux(mat%iaux))
ELSE IF( IsWetParticle(mat%icls) )THEN
  num_puff_types = 2*NINT(mat_aux(mat%iaux))
ELSE IF( IsLiquid(mat%icls) )THEN
  num_puff_types = NINT(mat_aux(mat%iaux+MAXGMAUX+MAXLMAUXP))+2
END IF

RETURN
END

!===============================================================================

SUBROUTINE get_bounds( mat,nsg,pbounds )

USE scipuff_fi

IMPLICIT NONE

TYPE( material_str ), INTENT( IN  ) :: mat
INTEGER,              INTENT( OUT ) :: nsg
REAL, DIMENSION(*),   INTENT( OUT ) :: pbounds

INTEGER k,j

LOGICAL, EXTERNAL :: IsParticle
LOGICAL, EXTERNAL :: IsLiquid, IsWetParticle

IF( IsParticle(mat%icls) .OR. IsWetParticle(mat%icls) )THEN
  nsg = NINT(mat_aux(mat%iaux))
  DO k = 1,nsg+1
    j = mat%iaux + (k-1)*MAXPMAUX + PMAUX_BOUNDS
    pbounds(k) = mat_aux(j)
  END DO
ELSE IF( IsLiquid(mat%icls) )THEN
  nsg = NINT(mat_aux(mat%iaux+MAXGMAUX+MAXLMAUXP))
  DO k = 1,nsg+1
    j = mat%iaux + (k-1)*MAXLMAUX + MAXLMAUXP + &
                              LMAUX_BOUNDS + MAXGMAUX
    pbounds(k) = mat_aux(j)
  END DO
ELSE
  nsg = 0
END IF

RETURN
END

!==============================================================================

SUBROUTINE get_puff_material_gas( pmatlIn,iaux )

USE scipuff_fi
USE UtilMtlAux

IMPLICIT NONE

INTEGER,               INTENT( IN  ) :: iaux
TYPE( puff_material ), INTENT( OUT ) :: pmatlIn

TYPE( gas_material ) pmatl
INTEGER i

CALL GetGasParam( pmatl,iaux,mat_aux )

pmatlIn = TRANSFER(pmatl,pmatlIn)

! Initialise the pmatl array
DO i = 3,SIZE(pmatlIn%param)
  pmatlIn%param(i) = NOT_SET_R
END DO

RETURN
END

!==============================================================================

SUBROUTINE get_puff_material_liquid( pmatlIn,iaux,igrp )

USE scipuff_fi
USE UtilMtlAux

IMPLICIT NONE

TYPE( puff_material ), INTENT( OUT ) :: pmatlIn
INTEGER,               INTENT( IN  ) :: iaux
INTEGER,               INTENT( IN  ) :: igrp

TYPE( liquid_material ) pmatl
INTEGER i

CALL GetLiquidParam( pmatl,iaux,igrp,mat_aux )

pmatlIn = TRANSFER(pmatl,pmatlIn)

! Initialise the pmatl array
DO i = MAXLMAUXP+MAXLMAUX+4,SIZE(pmatlIn%param)
  pmatlIn%param(i) = NOT_SET_R
END DO

RETURN
END

!=============================================================================

SUBROUTINE get_puff_material_part( pmatlIn,iaux,igrp )

USE scipuff_fi
USE UtilMtlAux

IMPLICIT NONE

TYPE( puff_material ), INTENT( OUT ) :: pmatlIn
INTEGER,               INTENT( IN  ) :: iaux
INTEGER,               INTENT( IN  ) :: igrp

TYPE ( part_material ) pmatl
INTEGER i

CALL GetParticleParam( pmatl,iaux,igrp,mat_aux )

pmatlIn = TRANSFER(pmatl,pmatlIn)

! Initialise the pmatl array
DO i = MAXPMAUX+4,SIZE(pmatlIn%param)
  pmatlIn%param(i) = NOT_SET_R
END DO

RETURN
END

!==============================================================================

SUBROUTINE put_puff_material_gas( pmatlIn,iaux )

USE scipuff_fi
USE UtilMtlAux

IMPLICIT NONE

TYPE( puff_material ), INTENT( IN ) :: pmatlIn
INTEGER,               INTENT( IN ) :: iaux

TYPE( gas_material ) pmatl

pmatl = TRANSFER(pmatlIn,pmatl)

CALL PutGasParam( pmatl,iaux,mat_aux )

RETURN
END

!==============================================================================

SUBROUTINE put_puff_material_liquid( pmatlIn,iaux,igrp )

USE scipuff_fi
USE UtilMtlAux

IMPLICIT NONE

TYPE( puff_material ), INTENT( IN ) :: pmatlIn
INTEGER,               INTENT( IN ) :: iaux
INTEGER,               INTENT( IN ) :: igrp

TYPE( liquid_material ) pmatl

pmatl = TRANSFER(pmatlIn,pmatl)

CALL PutLiquidParam( pmatl,iaux,igrp,mat_aux )

RETURN
END

!=============================================================================

SUBROUTINE put_puff_material_part( pmatlIn,iaux,igrp )

USE scipuff_fi
USE UtilMtlAux

IMPLICIT NONE

TYPE( puff_material ), INTENT( IN ) :: pmatlIn
INTEGER,               INTENT( IN ) :: iaux
INTEGER,               INTENT( IN ) :: igrp

TYPE( part_material ) pmatl

pmatl = TRANSFER(pmatlIn,pmatl)

CALL PutParticleParam( pmatl,iaux,igrp,mat_aux )

RETURN
END
