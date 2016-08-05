!*******************************************************************************
!$RCSfile$
!$Revision$
!$Date$
!*******************************************************************************
MODULE poparea_fd

  INTEGER, PARAMETER :: POP_BON     = 0
  INTEGER, PARAMETER :: POP_BAREA   = 1
  INTEGER, PARAMETER :: POP_BEXPECT = 2
  INTEGER, PARAMETER :: POP_OFF        = 0
  INTEGER, PARAMETER :: POP_ON         = 2**POP_BON      ! (1)
  INTEGER, PARAMETER :: POP_AREA       = 2**POP_BAREA    ! (2)
  INTEGER, PARAMETER :: POP_EXPECT     = 2**POP_BEXPECT  ! (4)

END MODULE poparea_fd

