!*******************************************************************************
!$RCSfile$
!$Revision$
!$Date$
!*******************************************************************************
MODULE surface_fi

  USE surface_fd
  USE sagstr_fd

  SAVE

  INTEGER                      :: srfdep,  srfdos
  TYPE( SAGgrid_str ), POINTER :: Psrfdep, Psrfdos

  INTEGER, DIMENSION(:), ALLOCATABLE :: srfdosAdj

  INTEGER                                         :: srfados
  TYPE( SAGgrid_str ),  POINTER                   :: Psrfados
  TYPE( SAGfield_aux ), DIMENSION(:), POINTER     :: Pauxados

  INTEGER nsrf_blocks, ndep_blocks, n_effects, nsrf_aux, MAXSG

  CHARACTER(4),          DIMENSION(:), ALLOCATABLE :: srfnam
  INTEGER,               DIMENSION(:), ALLOCATABLE :: srftyp
  TYPE( sfield_block  ), DIMENSION(:), ALLOCATABLE :: srf_block
  TYPE( sfield_effect ), DIMENSION(:), ALLOCATABLE :: srf_effect

  TYPE( sfield_puff ), DIMENSION(:,:), ALLOCATABLE :: srf_puff

  TYPE( SAGfield_aux ), DIMENSION(:), POINTER :: Pauxdep, Pauxdos

  REAL scaleEvap

END MODULE surface_fi
