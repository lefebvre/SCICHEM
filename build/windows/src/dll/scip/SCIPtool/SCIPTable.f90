!*******************************************************************************
!$RCSfile$
!$Revision$
!$Date$
!*******************************************************************************
INTEGER FUNCTION SCIPGetFieldTableSize( UserID,FieldX,ClassData,mTable,mCol,mRow )

USE field_fd

IMPLICIT NONE

!DEC$ IF DEFINED (DUALBUILD)
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'SCIPGETFIELDTABLESIZEOMP' :: SCIPGetFieldTableSize
!DEC$ ELSE
!DEC$ ATTRIBUTES DLLEXPORT :: SCIPGetFieldTableSize
!DEC$ ENDIF

INTEGER,                INTENT( IN  ) :: UserID
TYPE( SCIPPlotFieldT ), INTENT( IN  ) :: FieldX
REAL, DIMENSION(*),     INTENT( IN  ) :: ClassData
INTEGER,                INTENT( OUT ) :: mTable,mCol,mRow

INTEGER, EXTERNAL :: GetFieldTableSizeF

SCIPGetFieldTableSize = GetFieldTableSizeF( UserID,FieldX,ClassData,mTable,mCol,mRow )

RETURN
END

!==============================================================================

INTEGER FUNCTION SCIPGetFieldTable( UserID,FieldX,ClassData,TableTitle,ColTitle,RowTitle,Table )

USE field_fd
USE charT_fd

IMPLICIT NONE

!DEC$ IF DEFINED (DUALBUILD)
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'SCIPGETFIELDTABLEOMP' :: SCIPGetFieldTable
!DEC$ ELSE
!DEC$ ATTRIBUTES DLLEXPORT :: SCIPGetFieldTable
!DEC$ ENDIF

INTEGER,                              INTENT( IN )    :: UserID
TYPE( SCIPPlotFieldT ),               INTENT( INOUT ) :: FieldX
REAL,                   DIMENSION(*), INTENT( IN    ) :: ClassData
TYPE( char32T ),        DIMENSION(*), INTENT(   OUT ) :: TableTitle
TYPE( char32T ),        DIMENSION(*), INTENT(   OUT ) :: ColTitle
TYPE( char32T ),        DIMENSION(*), INTENT(   OUT ) :: RowTitle
INTEGER,                DIMENSION(*), INTENT(   OUT ) :: Table

INTEGER, EXTERNAL :: GetFieldTableF

SCIPGetFieldTable = GetFieldTableF( UserID,FieldX,ClassData,TableTitle,ColTitle,RowTitle,Table )

RETURN
END
