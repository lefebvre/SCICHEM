MODULE grdstr

  SAVE
  TYPE  grid_str
  INTEGER     nunit    !Unit number
  INTEGER     nvart    !Total number of variables
  INTEGER     nx    !Primary grid
  INTEGER     ny    !Primary grid
  INTEGER     nvar    !No. data values per variable
  INTEGER     record    !Record for writing
  INTEGER     ncells    !No. cells
  INTEGER     ipgrd    !Pointer to grid data
  INTEGER     ipdat    !Pointer to surface data
  INTEGER     ipnam    !Offset to first name in list
  INTEGER     maxlev    !Max refinement level
  REAL        xmin    !grid origin
  REAL        ymin    !grid origin
  REAL        dx    !grid size
  REAL        dy    !grid size
  REAL        delmin    !minimum grid size
  END TYPE

  TYPE (grid_str ) srfdepxxx
  INTEGER     srfgrd(25000,2)
  REAL        srfdat(25000,100)

END MODULE grdstr

MODULE vert_mod

  SAVE

  INTEGER nvg, nhg
  REAL    vmin, hmin, dvg, dhg, zlev

END MODULE vert_mod

