!*******************************************************************************
!$RCSfile$
!$Revision$
!$Date$
!*******************************************************************************
!==============================================================================
! SCIPNumSubstrates
!==============================================================================
INTEGER FUNCTION SCIPNumSubstrates( mode )

IMPLICIT NONE

!DEC$ IF DEFINED (DUALBUILD)
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'SCIPNUMSUBSTRATESOMP' :: SCIPNumSubstrates
!DEC$ ELSE
!DEC$ ATTRIBUTES DLLEXPORT :: SCIPNumSubstrates
!DEC$ ENDIF

INTEGER, INTENT( IN ) :: mode

INTEGER, EXTERNAL :: SizeSubstrateList

SCIPNumSubstrates = SizeSubstrateList( mode )

RETURN
END
!==============================================================================
! SCIPGetSubstrates
!==============================================================================
INTEGER FUNCTION SCIPGetSubstrates( mode,cSubstrate )

USE charT_fd

IMPLICIT NONE

!DEC$ IF DEFINED (DUALBUILD)
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'SCIPGETSUBSTRATESOMP' :: SCIPGetSubstrates
!DEC$ ELSE
!DEC$ ATTRIBUTES DLLEXPORT :: SCIPGetSubstrates
!DEC$ ENDIF

INTEGER,                       INTENT( IN  ) :: mode
TYPE( char16T ), DIMENSION(*), INTENT( OUT ) :: cSubstrate

INTEGER, EXTERNAL :: GetSubstrateList

SCIPGetSubstrates = GetSubstrateList( mode,cSubstrate )

RETURN
END

