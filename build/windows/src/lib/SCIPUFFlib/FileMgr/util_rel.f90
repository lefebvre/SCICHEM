!*******************************************************************************
!$RCSfile$
!$Revision$
!$Date$
!*******************************************************************************
!===============================================================================
!     InitRelease
!===============================================================================
SUBROUTINE InitRelease()

USE nextRel_fi
USE default_fd

IMPLICIT NONE

INTEGER i

!==== Integers

DO i = 1,SCIPUFF_STATUS_ARRAY_SIZE
  opmod(i) = 0
END DO

opid     = NOT_SET_I

relStatus  = NOT_SET_I

relName    = '<empty>'
relDisplay = '<empty>'

subgroup = 1

!==== Logicals

!==== Reals

trel = NOT_SET_R
xrel = NOT_SET_D
yrel = NOT_SET_D
zrel = NOT_SET_R

sigx = NOT_SET_R
sigy = NOT_SET_R
sigz = NOT_SET_R
size_rel = NOT_SET_R

sigRxy = NOT_SET_R
sigRxz = NOT_SET_R
sigRyz = NOT_SET_R

urel = 0.0
vrel = 0.0
wrel = 0.0

umom = DEF_VAL_R
vmom = DEF_VAL_R
wmom = 0.0
buoy = 0.0

cmass = NOT_SET_R
tdur  = NOT_SET_R

DO i = 1,SCIPUFF_MAXRELPARAM
  rel_param(i) = NOT_SET_R
END DO

!==== Character strings

reltyp   = ' '
relmat   = ' '
name_rel = ' '

RETURN
END
