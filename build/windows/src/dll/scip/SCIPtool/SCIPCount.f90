!===============================================================================
!     SCIPCountRelease
!===============================================================================
INTEGER FUNCTION SCIPCountRelease( UserID,file,nRel )

USE charT_fd

!Counts the SCIPUFF SCN namelists from the input file

IMPLICIT NONE

!DEC$ IF DEFINED (DUALBUILD)
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'SCIPCOUNTRELEASEOMP' :: SCIPCountRelease
!DEC$ ELSE
!DEC$ ATTRIBUTES DLLEXPORT :: SCIPCountRelease
!DEC$ ENDIF

INTEGER,           INTENT( IN  ) :: userID !USER ID tag
TYPE( fileNameT ), INTENT( IN  ) :: file   !filename
INTEGER,           INTENT( OUT ) :: nRel   !number of releases in file

INTEGER nMCrel

INTEGER, EXTERNAL :: CountRelease

SCIPCountRelease = CountRelease( UserID,file,nRel,nMCrel )

RETURN
END
!===============================================================================
!     SCIPCountMaterial
!===============================================================================
INTEGER FUNCTION SCIPCountMaterial( UserID,file,nMtl )

USE charT_fd

!Counts the SCIPUFF MATDEF namelists from the input file

IMPLICIT NONE

!DEC$ IF DEFINED (DUALBUILD)
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'SCIPCOUNTMATERIALOMP' :: SCIPCountMaterial
!DEC$ ELSE
!DEC$ ATTRIBUTES DLLEXPORT :: SCIPCountMaterial
!DEC$ ENDIF

INTEGER,           INTENT( IN  ) :: userID !USER ID tag
TYPE( fileNameT ), INTENT( IN  ) :: file !filename
INTEGER,           INTENT( OUT ) :: nMtl !number of materials in file

INTEGER, EXTERNAL :: CountMaterial

!==== Initialize

SCIPCountMaterial = CountMaterial( UserID,file,nMtl )

RETURN
END
!===============================================================================
!     SCIPCountReleaseMC
!===============================================================================
INTEGER FUNCTION SCIPCountReleaseMC( UserID,file,nRel,nMCrel )

USE charT_fd

!Counts the SCIPUFF SCN namelists from the input file

IMPLICIT NONE

!DEC$ IF DEFINED (DUALBUILD)
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'SCIPCOUNTRELEASEMCOMP' :: SCIPCountReleaseMC
!DEC$ ELSE
!DEC$ ATTRIBUTES DLLEXPORT :: SCIPCountReleaseMC
!DEC$ ENDIF

INTEGER,           INTENT( IN  ) :: userID !USER ID tag
TYPE( fileNameT ), INTENT( IN  ) :: file   !filename
INTEGER,           INTENT( OUT ) :: nRel   !number of releases in file
INTEGER,           INTENT( OUT ) :: nMCrel !number of multicomponent records in file

INTEGER, EXTERNAL :: CountRelease

SCIPCountReleaseMC = CountRelease( UserID,file,nRel,nMCrel )

RETURN
END
!===============================================================================
!     SCIPCountMCRel
!===============================================================================
INTEGER FUNCTION SCIPCountMCRel( UserID,file,nMCrel )

USE charT_fd

!Counts the SCIPUFF SCN namelists from the input file

IMPLICIT NONE

!DEC$ IF DEFINED (DUALBUILD)
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'SCIPCOUNTMCRELOMP' :: SCIPCountMCRel
!DEC$ ELSE
!DEC$ ATTRIBUTES DLLEXPORT :: SCIPCountMCRel
!DEC$ ENDIF

INTEGER,          INTENT( IN  ) :: userID !USER ID tag
TYPE( fileNameT ), INTENT( IN  ) :: file   !filename
INTEGER,          INTENT( OUT ) :: nMCrel !number of multicomponent records in file

INTEGER nRel

INTEGER, EXTERNAL :: CountRelease

SCIPCountMCRel = CountRelease( UserID,file,nRel,nMCrel )

RETURN
END
