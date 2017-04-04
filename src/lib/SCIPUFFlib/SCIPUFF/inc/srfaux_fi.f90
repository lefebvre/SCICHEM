!*******************************************************************************
!$RCSfile$
!$Revision$
!$Date$
!*******************************************************************************
MODULE srfaux_fi

  USE struct_fd

  SAVE

  INTEGER nbaux_srf  !No. of blocks needing auxiliary processing

  INTEGER, DIMENSION(:), ALLOCATABLE :: ibaux_srf  !Pointer to blocks
  REAL,    DIMENSION(:), ALLOCATABLE :: cmaux_srf  !Puff mean contribution
  REAL,    DIMENSION(:), ALLOCATABLE :: cvaux_srf  !Puff variance contribution

  TYPE( puff_liquid  )  pq

END MODULE srfaux_fi
