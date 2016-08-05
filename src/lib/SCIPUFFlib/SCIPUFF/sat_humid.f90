!*******************************************************************************
!$RCSfile$
!$Revision$
!$Date$
!*******************************************************************************
RECURSIVE SUBROUTINE sat_humid( tabs,p,hs )

!------ From fits to Smithsonian Meteorological Tables 89 and 94 given in
!       Appendix A4.2 of A. Gill (1982), Atmosphere-Ocean Dynamics, Academic Press, NY

!------ Input: tabs = absolute temp. (K)
!              p    = pressure (mb)

!------ Output: hs = staturation mixing ratio (g/g)

USE constants_fd

IMPLICIT NONE

REAL, INTENT( IN  ) ::  tabs
REAL, INTENT( IN  ) ::  p
REAL, INTENT( OUT ) ::  hs

REAL, PARAMETER :: EMAX = 1./(1.+MR)

REAL t, e, f

!------ Temperature in Celsius

t = tabs + ABSZERO
t = MIN(MAX(t,-100.),100.)  !Limit range to [-100C,100C]

!------ Saturation vapor pressure

e = (0.7859 + 0.03477*t) / (1. + 0.00412*t)
IF( t <= 0. )THEN
  e = e + 0.00422*t
END IF
f = 1. + (4.5 + 0.0006*t*t)*p*1.E-6
e = f*10.**e

!------ Saturation mixing ratio (from equ. A4.3)

e  = MIN( e/p,EMAX )  !Limit vapor pressure ratio so max(hs) = 1. (Arbitrary)
hs = MR*e/(1.-e)

RETURN
END
