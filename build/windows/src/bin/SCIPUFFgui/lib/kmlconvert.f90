!*******************************************************************************
! JavaConvert - converts OVL files to GoogleEarth KML files or ESRI Shapefiles
!*******************************************************************************
!*******************************************************************************
! JavaConvert_winDef - Additional Window definitions
!*******************************************************************************
MODULE JavaConvert_winDef
  INTEGER winDef_NotUsed
END MODULE JavaConvert_winDef

MODULE JavaConvert_winAPI
  INTEGER winAPI_NotUsed
END MODULE JavaConvert_winAPI

MODULE JavaConvert
  USE JavaConvert_winDef
  USE JavaConvert_winAPI
  INTEGER JavaConvert_NotUsed
END MODULE JavaConvert

SUBROUTINE runJavaConvert()
USE JavaConvert
USE error_fi

nError = UK_ERROR
eRoutine = 'JavaConvert'
eMessage = 'JavaConvert not enabled'

RETURN
END
