MODULE met2sci_fi

  USE MetSCIparam_fd
  USE MetSCIerrorParam_fd  
  USE MaxChar_fd
  USE error_fd
  USE fileList_fd

  TYPE( error_str ) :: error

  CHARACTER(MAXC) inputFile, SFCfile, UAfile
  CHARACTER(MAXC) reportFile, messageFile
!  CHARACTER(MAXC) QAfileUA, QAfileSFC  ** use scratch files **

  INTEGER lun, lunSfc, lunUA, lunOutSFC, lunOutUA
  INTEGER lunRpt, lunMsg, lunErr
  INTEGER lunTmp, lunScratch
  INTEGER lunSFQ, lunUAQ
  INTEGER ios, alloc_stat

  LOGICAL lJob, lUpperAir, lSurface
  LOGICAL lUAQ, lSFQ
  LOGICAL lexist

  TYPE( firstFile ) :: UAlist
  TYPE( firstFile ) :: SFClist

  TYPE( fileList ), POINTER :: currentFile

  INTEGER yearStartUA,  yearEndUA,  yearStartSFC,  yearEndSFC
  INTEGER monthStartUA, monthEndUA, monthStartSFC, monthEndSFC
  INTEGER dayStartUA,   dayEndUA,   dayStartSFC,   dayEndSFC

!------ get_next_data variables

  INTEGER, PARAMETER :: MAXN = 40     !Max. no. of arguments on a line

  INTEGER nch, narg, ikwrd
  LOGICAL lerr

  CHARACTER(16)   kwrd
  CHARACTER(1000) line

  CHARACTER(128), DIMENSION(MAXN) :: carg

END MODULE met2sci_fi
