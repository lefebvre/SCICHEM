!==============================================================================
!==============================================================================
!==============================================================================
INTEGER FUNCTION SCIPAdjointReleaseFilter( CallerID,nRel,relList,nMat,matList,maxHit )

!--- Filter adjoint releases - discarded releases have zero mass

USE release_fd
USE material_fd
USE AdjointFilter_fi

IMPLICIT NONE

!DEC# ATTRIBUTES DLLEXPORT :: SCIPAdjointReleaseFilter

INTEGER,                        INTENT( IN    ) :: CallerID
INTEGER,                        INTENT( INOUT ) :: nRel
TYPE( releaseT  ),DIMENSION(*), INTENT( INOUT ) :: relList
INTEGER,                        INTENT( INOUT ) :: nMat
TYPE( materialT ),DIMENSION(*), INTENT( INOUT ) :: matList
INTEGER,                        INTENT( IN    ) :: maxHit

INTEGER, EXTERNAL  :: AdjointReleaseFilterF

nhit_target = maxHit

SCIPAdjointReleaseFilter = AdjointReleaseFilterF( CallerID,nRel,relList,nMat,matList )

RETURN
END

