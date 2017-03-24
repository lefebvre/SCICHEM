!*******************************************************************************
!$RCSfile$
!$Revision$
!$Date$
!*******************************************************************************
!=======================================================================
!    ERROR common
!=======================================================================
MODULE error_fi

  USE errorParam_fd

  SAVE

  CHARACTER(80)  eRoutine
  CHARACTER(128) eMessage
  CHARACTER(128) eInform
  CHARACTER(128) eAction

  INTEGER nError

  ! Caution counters

  INTEGER nRelOutsideDomain, nCRelOutsideDomain
  INTEGER nRelBeforeStart
  INTEGER nStopSplit
  INTEGER nDezoneSrf
  INTEGER nPuffAboveTop
  INTEGER nPuffReflect

END MODULE error_fi
