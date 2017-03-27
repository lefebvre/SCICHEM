!*******************************************************************************
!$RCSfile$
!$Revision$
!$Date$
!*******************************************************************************
!*******************************************************************************
!            Load Project Release Input
!*******************************************************************************
INTEGER FUNCTION Load_Release( tool,relList )

USE SCIMgr_fd
USE files_fi
USE error_fi

IMPLICIT NONE

TYPE( preleaseT ),              INTENT( INOUT ) :: tool
TYPE( releaseT ), DIMENSION(*), INTENT( OUT   ) :: relList

CHARACTER(PATH_MAXLENGTH) filename
CHARACTER(128)            searchID

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: AddExtension

!==== Initialize

Load_Release = SCIPfailure

!==== Set namelist default values (SCIPUFF memory) from input data
!     NOTE - For Releases this must be done as part of the loop in ReadRelease

!==== Read namelist

IF( BTEST(tool%control%mode,HCB_SEARCH) )THEN
  searchID = tool%control%searchID
  IF( BTEST(tool%control%mode,HCB_REPLACE) )searchID = '!'//TRIM(searchID)
ELSE
  searchID = ' '
  IF( .NOT.BTEST(tool%control%mode,HCB_APPEND) )tool%scnHead%number = 0
END IF

IF( BTEST(tool%control%mode,HCB_FILE) )THEN
  filename = TRIM(AddExtension( tool%project%name,tool%control%fileExtension ))
ELSE
  filename = TRIM(AddExtension( tool%project%name,'scn' ))
END IF
CALL AddPath( filename,tool%project%path )

CALL ReadRelease( filename,lun_tmp,searchID,tool%scnHead,relList ) !Release list
IF( nError /= NO_ERROR )GOTO 9999

!==== Return

Load_Release = SCIPsuccess

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            Write Project Release Input
!*******************************************************************************
INTEGER FUNCTION Write_Release( tool,relList )

USE SCIMgr_fd
USE files_fi
USE error_fi

IMPLICIT NONE

TYPE( preleaseT ),              INTENT( IN ) :: tool
TYPE( releaseT ), DIMENSION(*), INTENT( IN ) :: relList

CHARACTER(PATH_MAXLENGTH) filename
CHARACTER(20)             searchID

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: AddExtension

!==== Initialize

Write_Release = SCIPfailure

!==== Set namelist values (SCIPUFF memory) from input data
!     NOTE - For Releases this must be done as part of the loop in WriteRelease

!==== Write namelist

IF( BTEST(tool%control%mode,HCB_SEARCH) )THEN
  searchID = TRIM(tool%control%searchID)
ELSE
  searchID = ' '
END IF

IF( BTEST(tool%control%mode,HCB_FILE) )THEN
  filename = TRIM(AddExtension( tool%project%name,tool%control%fileExtension ))
ELSE
  filename = TRIM(AddExtension( tool%project%name,'scn' ))
END IF
CALL AddPath( filename,tool%project%path )

CALL WriteRelease( filename,lun_tmp,searchID,tool%scnHead,relList ) !Release list
IF( nError /= NO_ERROR )GOTO 9999

!==== Return

Write_Release = SCIPsuccess

9999 CONTINUE

RETURN
END

