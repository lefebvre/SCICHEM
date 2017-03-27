!*******************************************************************************
!$RCSfile$
!$Revision$
!$Date$
!*******************************************************************************
!=======================================================================
!    SCIPUFF commons
!=======================================================================

MODULE tlev_fi

  USE scipuff_fd

  SAVE

  INTEGER, DIMENSION(0:MAXTLV) :: itfrst, itlast, ntlev

  INTEGER mxtlev
  INTEGER mxtlev_evap

END MODULE tlev_fi

MODULE matl_fi

  USE scipuff_fd
  USE matlstruct_fd
  USE mcstruct_fd
  USE mauxstruct_fd

  SAVE

  INTEGER ntypm
  TYPE( material_str ), DIMENSION(:), ALLOCATABLE :: material

  INTEGER nmaux
  REAL, DIMENSION(:), POINTER :: mat_aux

  TYPE( material_MClist ) mat_mc

  INTEGER nWetPartLiq
  TYPE( liquid_material ), DIMENSION(:), ALLOCATABLE :: WetPartLiquidMat

END MODULE matl_fi

MODULE basic_fi

  USE obsolete_fd
  USE puff_fd

  SAVE

  REAL, PARAMETER :: ASPLT_ZFAC = 0.8666

  REAL  g       ,gt     ,f0     ,rhoair ,rmuair ,rnu
  REAL  wwtrop  ,sltrop ,epstrop,sle_fac
  REAL  rrmrge  ,simrge ,cmin   ,delmin ,asplt  ,asplt2, z_dosage
  REAL  aspltc  ,dxsplt ,dzsplt ,delx2  ,delz2  ,fac_rfl

  CHARACTER(4), DIMENSION(NP_ALL) :: names
  CHARACTER(4), DIMENSION(MAXCLS) :: namec

END MODULE basic_fi

MODULE flags_fi

  SAVE

  INTEGER prjEffects
  INTEGER run_mode
  INTEGER flag_start

  LOGICAL restart, surface, dose, lter, lymd
  LOGICAL create, ldecay, local, lsplitz
  LOGICAL dynamic,buoy_gas,lsmp,static
  LOGICAL evaporation
  LOGICAL multicomp
  LOGICAL sampamb
  LOGICAL dense_gas
  LOGICAL depint
  LOGICAL lwash
  LOGICAL lsplit_report
  LOGICAL int_sensor
  LOGICAL global_lon
  LOGICAL polarcap_n
  LOGICAL polarcap_s

END MODULE flags_fi

MODULE project_fi

  USE DefSize_fd

  SAVE

  INTEGER iversion
  INTEGER iversion_code
  INTEGER istop

  LOGICAL SCIPUFFinProgress

  CHARACTER(64)  name
  CHARACTER(80)  title
  CHARACTER(32)  audit_class
  CHARACTER(32)  audit_analyst
  CHARACTER(32)  audit_date
  CHARACTER(32)  audit_version
  CHARACTER(320) audit_space

  CHARACTER(PATH_MAXLENGTH) smpfile

END MODULE project_fi

MODULE nextRel_fi

  USE opRel_fd
  USE multcomp_fd
  USE scipuff_fd
  USE puffstruct_fd
  USE release_fd
  USE pauxstruct_fd

  SAVE

  INTEGER, DIMENSION(SCIPUFF_STATUS_ARRAY_SIZE) :: opmod
  INTEGER, DIMENSION(TIME_STATUS_ARRAY_SIZE)    :: time_status
  INTEGER, DIMENSION(DOMAIN_STATUS_ARRAY_SIZE)  :: domain_status

  INTEGER opid
  INTEGER relStatus
  INTEGER subgroup ,rel_dist, rel_ityp

  INTEGER init_source
  INTEGER nsrc_prime

  REAL     trel, zrel, cmass, size_rel, tdur
  REAL(8)  xrel, yrel
  REAL  sigx, sigy, sigz, urel, vrel, wrel, wmom, buoy
  REAL  sigRxy, sigRxz, sigRyz
  REAL umom,vmom
  REAL kyprm, kzprm

  REAL, DIMENSION(SCIPUFF_MAXRELPARAM) :: rel_param

  INTEGER nRandom
  REAL(8), DIMENSION(:), ALLOCATABLE :: xRandom, yRandom
  REAL,    DIMENSION(:), ALLOCATABLE :: zRandom

  CHARACTER(4)   reltyp
  CHARACTER(16)  relmat
  CHARACTER(PATH_MAXLENGTH) name_rel
  CHARACTER(32)  relName
  CHARACTER(192) relDisplay

  LOGICAL ActiveSource
  LOGICAL wake
  LOGICAL prise

  TYPE( FirstMCrel ) :: RelMC
  TYPE( releaseMCT ), DIMENSION(:), ALLOCATABLE :: CurrentRelMC
  TYPE( releaseT ) currentRelease
  TYPE( puff_str ) puffRelease
  TYPE( puff_liquid ) puffRelLiquid
  TYPE( StoreReleaseT ), TARGET :: InstReleaseList

END MODULE nextRel_fi

MODULE scnRel_fi

  USE opRel_fd
  USE scipuff_fd
  USE DefSize_fd
  USE releaseID_fd

  SAVE

  INTEGER, DIMENSION(SCIPUFF_STATUS_ARRAY_SIZE) :: opmod_scn

  INTEGER opid_scn
  INTEGER relStatus_scn
  INTEGER subgroup_scn ,rel_dist_scn

  REAL, DIMENSION(SCIPUFF_MAXRELPARAM) :: rel_param_scn

  INTEGER numRelID
  TYPE( Data_relID ), DIMENSION(:), ALLOCATABLE :: releaseID

  REAL     trel_scn, zrel_scn, cmass_scn ,size_scn, tdur_scn
  REAL(8)  xrel_scn, yrel_scn
  REAL  sigx_scn, sigy_scn, sigz_scn, urel_scn, vrel_scn, wrel_scn, wmom_scn, buoy_scn
  REAL  sigRxy_scn, sigRxz_scn, sigRyz_scn
  REAL umom_scn,vmom_scn

  CHARACTER(4)   reltyp_scn
  CHARACTER(PATH_MAXLENGTH) name_rel_scn
  CHARACTER(16)  relmat_scn
  CHARACTER(32)  relName_scn
  CHARACTER(192) relDisplay_scn

END MODULE scnRel_fi

MODULE grid_fi

  SAVE

  INTEGER mgrd    ,nx     ,ny     ,nz
  INTEGER utm_zone
  INTEGER mxlev_evap, mxlev_smp
  INTEGER lmap
  REAL    xmin    ,xmax   ,ymin   ,ymax   ,zmax   ,dxg    ,dyg    ,dzg
  REAL    hres    ,vres   ,lon0   ,lat0   ,xref   ,yref   ,lon_ref,lat_ref

  INTEGER polefld_n
  INTEGER polefld_s

END MODULE grid_fi

MODULE puff_fi

  USE puffstruct_fd

  SAVE

  INTEGER mxsgp

  INTEGER npuf, MAXPUF
  TYPE( puff_str ), DIMENSION(:), ALLOCATABLE :: puff
  TYPE (puff_str ), DIMENSION(:), ALLOCATABLE :: puffSav
END MODULE puff_fi

MODULE type_fi

  USE puff_fd
  USE precip_fd
  USE typestruct_fd

  SAVE

  INTEGER nclass, ntypp

  TYPE( type_str ), DIMENSION(:),   ALLOCATABLE :: typeID
  LOGICAL,          DIMENSION(:),   ALLOCATABLE :: buoy_flag
  REAL,             DIMENSION(:),   ALLOCATABLE :: buoy_fac
  REAL,             DIMENSION(:),   ALLOCATABLE :: vwash
  REAL,             DIMENSION(:,:), ALLOCATABLE :: twash

  INTEGER ntyps,    ntypd
  INTEGER nskp_dyn, nskp_dyn_gas

  INTEGER substrate_type

END MODULE type_fi

MODULE time_fi

  SAVE

  INTEGER jul_start
  INTEGER year_start, month_start, day_start
  INTEGER year_end,   month_end,   day_end

  REAL  tstart, tend, tend_hr, tend_r, time_rst, tzone
  REAL  t, delt, t_avg
  REAL  t_old_r, t_save, dt_save, dt_smp
  REAL  tLastDep, tLastDos

END MODULE time_fi

MODULE scipuff_fi
  USE error_fi
  USE constants_fd
  USE param_fd
  USE default_fd
  USE struct_fd
  USE tlev_fi
  USE flags_fi
  USE matl_fi
  USE basic_fi
  USE project_fi
  USE grid_fi
  USE nextRel_fi
  USE scnRel_fi
  USE puff_fi
  USE type_fi
  USE time_fi
END MODULE scipuff_fi
