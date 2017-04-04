!*******************************************************************************
!$RCSfile$
!$Revision$
!$Date$
!*******************************************************************************
MODULE poparea_fi

  USE poparea_fd
  USE field_fd
  USE param_fd

  SAVE

  INTEGER nlev
  LOGICAL DoPop
  REAL grdX0
  REAL grdY0
  REAL grdDx
  REAL grdDy
  TYPE( SCIPFieldCoordinateT )  Coordinate
  REAL(8), DIMENSION(:), ALLOCATABLE :: PopArea
  REAL,    DIMENSION(:), ALLOCATABLE :: level
  REAL popScale

END MODULE poparea_fi


