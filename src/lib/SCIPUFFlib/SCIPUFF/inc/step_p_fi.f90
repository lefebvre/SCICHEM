!*******************************************************************************
!$RCSfile$
!$Revision$
!$Date$
!*******************************************************************************
MODULE step_p_fi

  USE puffstruct_fd

  SAVE

! --- Local variables for STEP_P routines

  REAL, PARAMETER :: HSCALE  = 8000.     ! Atmospheric scale height
  REAL, PARAMETER :: WMX     = 0.8       ! Well-mixed criterion sz/zinv
  REAL, PARAMETER :: ZIFAC   = 1.1       ! Zi-split criterion (delta Zi)
  REAL, PARAMETER :: SZFAC   = 3.0       ! Zi-split criterion (height)
  REAL, PARAMETER :: ZIDEL   = 10.0      ! Zi-split criterion (height)

  REAL(8) xsav, ysav, lonsav

  REAL xmap, ymap, hp, hx, hy, area_fac, sz, zbar, ztop, zlim
  REAL cnew, csav, zsav, hsav, hxsav, hysav, zisav
  REAL uubt, vvbt, uvbt
  REAL si, si2, sv, qosi_cc, qi, qvi, qtot, fac_diss
  REAL rhod, vfall, sigvd, vdry, vdtot, cmin2
  REAL xuct, xvct, yvct, zwct, tauc, taur, fwash
  REAL siq, svq, aqoszt, qs
  REAL aqsosyt, aqbosyt, aqlosyt
  REAL wpuff, vel2
  REAL aspect, ztest, zexp_dos
  REAL dts, stest
  REAL udyn, vdyn, wdyn, tdyn, bdyn, ddyn, fdyn, zdyn
  REAL tauw, difp, gt0, bv, gamma, ti
  REAL tscale
  REAL rhogas, rhopuff
  REAL splitf

  REAL dtstep

  INTEGER imat
  REAL iSplit
  INTEGER iSkew
  INTEGER ityppr
  REAL    skewness

  LOGICAL lzinv, lsrf, ldos, ltot, lcap, lblcap
  LOGICAL lscale
  LOGICAL pole_transition
  LOGICAL ldense

  TYPE( puff_str ), POINTER :: washPuff

END MODULE step_p_fi

MODULE grdlock_fi
  INTEGER, PARAMETER :: OMPONLYMODULE = 0
END MODULE grdlock_fi
