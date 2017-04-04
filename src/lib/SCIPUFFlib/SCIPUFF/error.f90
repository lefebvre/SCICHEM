!*******************************************************************************
!$RCSfile$
!$Revision$
!$Date$
!*******************************************************************************
!***********************************************************************
!               InitError
!***********************************************************************
SUBROUTINE init_error()

USE error_fi

IMPLICIT NONE


nError = NO_ERROR

eMessage = CHAR(0)
eInform  = CHAR(0)
eAction  = CHAR(0)
eRoutine = CHAR(0)


RETURN
END
!***********************************************************************
!               InitCaution
!***********************************************************************
SUBROUTINE initCaution()

USE error_fi

IMPLICIT NONE

nRelOutsideDomain = 0
nRelBeforeStart   = 0
nStopSplit        = 0
nDezoneSrf        = 0
nPuffAboveTop     = 0
nPuffReflect      = 0

RETURN
  END

