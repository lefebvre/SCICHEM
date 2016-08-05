!*******************************************************************************
!$RCSfile$
!$Revision$
!$Date$
!*******************************************************************************
!*******************************************************************************
!            Load Project Status Input
!*******************************************************************************
INTEGER FUNCTION Load_Status( tool,statList )

USE SCIMgr_fd
USE files_fi
USE error_fi
USE statstruct_fd

IMPLICIT NONE

TYPE( pstatusT ),      INTENT( INOUT ) :: tool
INTEGER, DIMENSION(*), INTENT( INOUT ) :: statList

CHARACTER(PATH_MAXLENGTH) filename

INTEGER mxs,nTot,istart
LOGICAL SCIP32_status

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: AddExtension

!==== Initialize

Load_Status = SCIPfailure

!==== Set namelist default values (SCIPUFF memory) from input data
!     NOTE - For Status this must be done as part of the loop in ReadStatus

SCIP32_status = .FALSE.

mxs  = tool%statHead%max
nTot = tool%statHead%number

filename = TRIM(AddExtension( tool%project%name,'icd' ))
CALL AddPath( filename,tool%project%path )

IF( tool%ID == HS_READTIME .OR. tool%ID == HS_READALL )THEN
  tool%statHead%max    = mxs - nTot
  tool%statHead%number = 0
  istart               = nTot + 1
  CALL ReadStatus( filename,lun_tmp,1,tool%statHead,statList(istart) ) !Status list
  IF( nError /= NO_ERROR )THEN
    SCIP32_status = .TRUE.
    filename = TRIM(AddExtension( tool%project%name,'inp' ))
    CALL AddPath( filename,tool%project%path )
    CALL init_error()
    CALL ReadStatus( filename,lun_tmp,1,tool%statHead,statList(istart) ) !Status list
    IF( nError /= NO_ERROR )GOTO 9999
  END IF
  nTot = nTot + tool%statHead%number
END IF

IF( tool%ID == HS_READDOMAIN .OR. tool%ID == HS_READALL )THEN
  tool%statHead%max    = mxs - nTot
  tool%statHead%number = 0
  istart               = nTot + 1
  CALL ReadStatus( filename,lun_tmp,2,tool%statHead,statList(istart) ) !Status list
  IF( nError /= NO_ERROR )THEN
    IF( .NOT.SCIP32_status )THEN
      SCIP32_status = .TRUE.
      filename = TRIM(AddExtension( tool%project%name,'inp' ))
      CALL AddPath( filename,tool%project%path )
      CALL init_error()
      CALL ReadStatus( filename,lun_tmp,2,tool%statHead,statList(istart) ) !Status list
      IF( nError /= NO_ERROR )GOTO 9999
    ELSE
      GOTO 9999
    END IF
  END IF
  nTot = nTot + tool%statHead%number
END IF

IF( SCIP32_status )THEN
  filename = TRIM(AddExtension(tool%project%name,'scn'))
  CALL AddPath( filename,tool%project%path )
END IF

IF( tool%ID == HS_READSCN .OR. tool%ID == HS_READALL )THEN
  tool%statHead%max    = mxs - nTot
  tool%statHead%number = 0
  istart               = nTot + 1
  CALL ReadStatus( filename,lun_tmp,3,tool%statHead,statList(istart) ) !Status list
  IF( nError /= NO_ERROR )THEN
    IF( .NOT.SCIP32_status )THEN
      SCIP32_status = .TRUE.
      filename = TRIM(AddExtension( tool%project%name,'scn' ))
      CALL AddPath( filename,tool%project%path )
      CALL init_error()
      CALL ReadStatus( filename,lun_tmp,3,tool%statHead,statList(istart) ) !Status list
      IF( nError /= NO_ERROR )GOTO 9999
    ELSE
      GOTO 9999
    END IF
  END IF
  nTot = nTot + tool%statHead%number
ELSE IF( tool%ID /= HS_READDOMAIN .AND. tool%ID /= HS_READTIME )THEN
  tool%statHead%max    = mxs - nTot
  tool%statHead%number = 0
  istart               = nTot + 1
  CALL ReadStatus( filename,lun_tmp,-tool%ID,tool%statHead,statList(istart) ) !Status list
  IF( nError /= NO_ERROR )THEN
    IF( .NOT.SCIP32_status )THEN
      SCIP32_status = .TRUE.
      filename = TRIM(AddExtension( tool%project%name,'scn' ))
      CALL AddPath( filename,tool%project%path )
      CALL init_error()
      CALL ReadStatus( filename,lun_tmp,-tool%ID,tool%statHead,statList(istart) ) !Status list
      IF( nError /= NO_ERROR )GOTO 9999
    ELSE
      GOTO 9999
    END IF
  END IF
  nTot = nTot + tool%statHead%number
END IF

!==== Return

Load_Status = SCIPsuccess

9999  CONTINUE
tool%statHead%max    = mxs
tool%statHead%number = nTot

RETURN
END
!*******************************************************************************
!            Write Project Status Input
!*******************************************************************************
INTEGER FUNCTION Write_Status( tool,statList )

USE SCIMgr_fd
USE files_fi
USE statstruct_fd
USE error_fi

IMPLICIT NONE

TYPE( pstatusT ), INTENT( IN ) :: tool

INTEGER,DIMENSION(*) :: statList

CHARACTER(PATH_MAXLENGTH) filename

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: AddExtension

!==== Initialize

Write_Status = SCIPfailure

!==== Set namelist values (SCIPUFF memory) from input data
!     NOTE - For Statuss this must be done as part of the loop in WriteStatus

!==== Write namelist

filename = TRIM(AddExtension( tool%project%name,'icd' ))
CALL AddPath( filename,tool%project%path )

CALL WriteStatus( filename,lun_tmp,tool%statHead,statList ) !Status list
IF( nError /= NO_ERROR )GOTO 9999

!==== Return

Write_Status = SCIPsuccess

9999 CONTINUE

RETURN
END
