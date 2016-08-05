!*******************************************************************************
!$RCSfile$
!$Revision$
!$Date$
!*******************************************************************************
!*******************************************************************************
!                SetFileNamesT
!*******************************************************************************
SUBROUTINE SetFileNamesT( input )

USE files_fi

IMPLICIT NONE

CHARACTER(*), INTENT( IN ) :: input !PATH\BASE file name

CHARACTER(PATH_MAXLENGTH) project

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: AddExtension, StripExtension

!---- Set base project name

project = TRIM(input)
project = StripExtension( project )

!---- Set Project File names

file_dep(1:) = TRIM(AddExtension(project,'dep'))
file_amr(1:)  = TRIM(AddExtension(project,'amr'))
file_dgn(1:)  = TRIM(AddExtension(project,'dgn'))
file_ados(1:) = TRIM(AddExtension(project,'ados'))
file_asmp(1:) = TRIM(AddExtension(project,'asmp'))
file_dmp(1:) = TRIM(AddExtension(project,'dmp'))
file_dos(1:) = TRIM(AddExtension(project,'dos'))
file_err(1:) = TRIM(AddExtension(project,'err'))
file_inp(1:) = TRIM(input)
file_msc(1:) = TRIM(AddExtension(project,'msc'))
file_prj(1:) = TRIM(AddExtension(project,'prj'))
file_puf(1:) = TRIM(AddExtension(project,'puf'))
file_scn(1:) = TRIM(AddExtension(project,'scn'))
file_sfc(1:) = TRIM(AddExtension(project,'sfo'))
file_log(1:) = TRIM(AddExtension(project,'log'))
file_mcw(1:) = TRIM(AddExtension(project,'mcw'))
file_smp(1:) = TRIM(AddExtension(project,'smp'))
file_sps(1:) = TRIM(AddExtension(project,'sps'))
file_clog(1:) = TRIM(AddExtension(project,'clog'))

RETURN
END
!*******************************************************************************
!                SetFileUnitsT
!*******************************************************************************
SUBROUTINE SetFileUnitsT()

USE files_fi

IMPLICIT NONE

!     Set SCIPUFF Logical units

lun_prj = 10
lun_dbg = 11
lun_err = 12
lun_inp = 13
lun_msc = 14
lun_scn = 15
lun_pal = 16
lun_puf = 17
lun_dep = 18
lun_dos = 19
lun_tmp = 21
lun_sfc = 22
lun_met = 23
lun_log = 24
lun_mcw = 25
lun_ter = 26
lun_smp = 27
lun_sps = 28
lun_dmp = 33
lun_clog =34
lun_amr = 36
lun_dgn = 37
lun_asmp = 38
lun_ados = 39

pufffile_version = -1  !(not opened)

RETURN
END
!*******************************************************************************
!            SetupStopFiles
!*******************************************************************************
SUBROUTINE SetupStopFiles( )

USE SCIMgr_fd
USE files_fi
USE error_fi

IMPLICIT NONE

CHARACTER(PATH_MAXLENGTH) filename, filepath

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: buildStopFile

CALL SplitName( file_log,filename,filepath )

file_abort = buildStopFile( 'scipuff.abort',filepath )
IF( nError /= 0 )GOTO 9999

file_halt = buildStopFile( 'scipuff.halt',filepath )
IF( nError /= 0 )GOTO 9999

file_stop = buildStopFile( 'scipuff.stop',filepath )
IF( nError /= 0)GOTO 9999

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            Adds the path to the name and deletes existing file
!*******************************************************************************
CHARACTER(*) FUNCTION buildStopFile( filename,filepath )

USE SCIMgr_fd
USE error_fi

IMPLICIT NONE

CHARACTER(*) filename, filepath

INTEGER irv
LOGICAL lexist
INTEGER, EXTERNAL :: sysDeleteFile, sysGetLastError

buildStopFile = filename

CALL AddPath( buildStopFile,TRIM(filepath) )
INQUIRE( FILE=buildStopFile,EXIST=lexist )
IF( lexist )THEN
  irv = sysDeleteFile( buildStopFile )
  IF( irv == SCIPfailure )THEN
    nError   = UK_ERROR
    eRoutine = 'buildStopFile'
    WRITE(eMessage,'(A,I4)')'sysDeleteFile error =',sysGetLastError()
    CALL ReportFileName( eInform,'File=',buildStopFile )
  END IF
END IF

RETURN
END
!*******************************************************************************
!            SetupFileNames
!*******************************************************************************
SUBROUTINE SetupFileNames( project )

USE prjstruct_fd

TYPE( projectIDT ), INTENT( IN ) :: project

CHARACTER(PATH_MAXLENGTH) string

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: AddExtension

string = TRIM(project%name)
CALL AddPath( string,TRIM(project%path) )
string = TRIM(AddExtension(string,'inp'))
CALL SetFileNamesT( string )
CALL SetFileUnitsT()

CALL SetupStopFiles()

RETURN
END
