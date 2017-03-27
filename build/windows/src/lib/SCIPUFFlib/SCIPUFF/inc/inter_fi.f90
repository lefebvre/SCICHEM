!*******************************************************************************
!$RCSfile$
!$Revision$
!$Date$
!*******************************************************************************
MODULE inter_fi

  SAVE

  REAL det, d33, x1, y1, z1
  REAL dgrd, xp, yp, zp, h, hx, hy
  REAL xx, yy, delx, dely, delz
  REAL fac, facc
  REAL xmap_i, ymap_i, zp1, r_ipuf
  REAL hp, hxp, hyp
  REAL zp2, r_jpuf
  REAL(8) betx, bety, betz
  REAL g_ipuf, g_jpuf, g_kpuf, z_ipuf, z_jpuf, z_kpuf
  REAL r_kpuf, facr, den
  REAL facp, facn, facw, facwp, faccp, ctot, ctotp
  REAL facu, facup, facv, facvp

  REAL, DIMENSION(3) :: xr
  REAL(8), DIMENSION(7) :: asig, bsig

  INTEGER iovlp, iprv, irfrst, irlast, nrlist
  INTEGER ip, ipufMin

  INTEGER, DIMENSION(:), ALLOCATABLE :: ku, kl
  REAL,    DIMENSION(:), ALLOCATABLE :: ptmp

  LOGICAL lprocess, lsame, lstatic, lrfl_ipuf, lrfl_jpuf
  LOGICAL ltyp, lmat, ltot, ldyni, ldynj
  LOGICAL lliqi, lliqj, lliqm, laeri
  LOGICAL lProcessAll
  LOGICAL lmc
  INTEGER mcID, ID

END MODULE inter_fi
