!*******************************************************************************
!$RCSfile$
!$Revision$
!$Date$
!*******************************************************************************
!==============================================================================
!  Module for SWIM met field interpolation
!==============================================================================

MODULE SWIMcurrentMet_fi

  USE SWIMinterp_fd

  SAVE

  REAL hx, hy, hp, dp, gxp, gyp  !Local terrain parameters
  REAL zRef                      !Height above reference level (Hmin)
  REAL zh                        !Height AGL
  REAL zm                        !Terrain-following coordinate
  REAL zsl                       !Surface-layer depth

  REAL sigh                      !Horizontal puff scale
  REAL sigv                      !Vertical puff scale

  REAL xbar                      !Puff centroid
  REAL ybar                      !Puff latitude (perhaps)

  LOGICAL lzinv, lsl, lter, lstagger

  TYPE( met1dh ) mx, mxu, my, myv
  TYPE( meth   ) mxy
  TYPE( metv   ) mz, mzw

  REAL, DIMENSION(:), POINTER :: zb, zbw

  INTEGER klev0, klev0w

  LOGICAL :: lSaveCoord

  REAL, DIMENSION(:), ALLOCATABLE :: xFld, yFld

END MODULE SWIMcurrentMet_fi
