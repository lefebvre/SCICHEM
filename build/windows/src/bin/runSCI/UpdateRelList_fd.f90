MODULE UpdateRelList_fd

  USE DefSize_fd

  TYPE UpdateRelList
    CHARACTER(64) ID
    INTEGER       nData
    REAL          rate
    REAL          buoy
    REAL          wmom, umom, vmom
    REAL          rateScale
    REAL          buoyScale
    REAL          wmomScale, umomScale, vmomScale
    REAL          tNext
    INTEGER       fileUnit
    CHARACTER(PATH_MAXLENGTH) :: name
    TYPE( UpdateRelList ), POINTER :: next
  END TYPE UpdateRelList

  TYPE FirstRel
    TYPE( UpdateRelList), POINTER :: Rel
  END TYPE FirstRel

END MODULE UpdateRelList_fd
