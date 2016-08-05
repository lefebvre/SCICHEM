!***********************************************************************
!               CODAinit
!***********************************************************************
SUBROUTINE CODAinit( INIfile )


!     This routine inits the coda DLL

IMPLICIT NONE

CHARACTER(*)   INIfile

END
!***********************************************************************
!               CODAexit
!***********************************************************************
SUBROUTINE CODAexit

RETURN

END
!***********************************************************************
!               IsCODA
!***********************************************************************
LOGICAL FUNCTION IsCODA(agent)
USE resource_fd
USE coda32_fi
USE pcscipuf_fi

IMPLICIT NONE

CHARACTER(*) agent

INTEGER i

IsCODA = .FALSE.
i = 0
DO WHILE( i < nCODAagents .AND. .NOT.IsCODA)
  i = i + 1
  IsCODA = TRIM(agent) == TRIM(CODAagents(i))
END DO

RETURN
END
!***********************************************************************
!               IsCODAoption
!***********************************************************************
LOGICAL FUNCTION IsCODAoption(option)
USE resource_fd
USE coda32_fi
USE pcscipuf_fi

IMPLICIT NONE

CHARACTER(*) option

INTEGER i

IsCODAoption = .FALSE.
i = 0
DO WHILE( i < nCODAoptions .AND. .NOT.IsCODAoption)
  i = i + 1
  IsCODAoption = TRIM(option) == TRIM(CODAoptions(i))
END DO

RETURN
END
!***********************************************************************
!               CODADialog
!***********************************************************************
SUBROUTINE CODADialog(iwnd,id_level)

USE resource_fd
USE errorParam_fd
USE coda32_fi
USE pcscipuf_fi
USE files_fi
USE dialog_fi
USE CODA32

!     This routine runs the coda DLL DialogBox

IMPLICIT NONE

INTEGER iwnd
INTEGER id_level


9999  CONTINUE

RETURN
END

!***********************************************************************
!               setCODAtitle
!***********************************************************************
SUBROUTINE setCODAtitle(title)

USE resource_fd
USE coda32_fi
USE pcscipuf_fi
USE files_fi
USE dialog_fi
USE CODA32

!     This set the coda agent

IMPLICIT NONE

CHARACTER(*)  title

title ='Unknown'

9999  RETURN
END

!***********************************************************************
!               guiCODAagent
!***********************************************************************
SUBROUTINE guiCODAagent(eAgent)

USE resource_fd
USE coda32_fi
USE pcscipuf_fi
USE files_fi
USE dialog_fi
USE CODA32

!     This set the coda agent

IMPLICIT NONE

CHARACTER(*)  eAgent


9999  RETURN
END

!***********************************************************************
!               guiCODAoption
!***********************************************************************
SUBROUTINE guiCODAoption(eOption)

USE resource_fd
USE resource_fd
USE coda32_fi
USE pcscipuf_fi
USE files_fi
USE dialog_fi

!     This set the coda agent

IMPLICIT NONE

CHARACTER(*)  eOption


9999  RETURN
END

!***********************************************************************
!               setCODAtimes
!***********************************************************************
SUBROUTINE setCODAtimes()

USE resource_fd
USE coda32_fi
USE pcscipuf_fi
USE errorParam_fd
USE files_fi
USE dialog_fi
USE default_fd
USE pltchoice_fi
USE CODA32

!     This routine set the coda DLL list of available times

IMPLICIT NONE


RETURN
END
!***********************************************************************
!               NearestTime
!***********************************************************************
INTEGER FUNCTION NearestTime(tList,nList,time)

USE resource_fd
USE default_fd

IMPLICIT NONE

REAL tList(0:1),time

INTEGER nList

REAL dt,dtx

INTEGER i

NearestTime = NOT_SET_I

IF( time /= NOT_SET_R )THEN

  IF( time <= tList(0) )THEN
    NearestTime = 0
  ELSE IF( time >= tList(nList) )THEN
    NearestTime = nList
  ELSE
    dtx = DEF_VAL_R
    DO i = 0,nList
      dt = ABS(time - tList(i))
      IF( dt < dtx )THEN
        dtx = dt
        NearestTime = i
      END IF
    END DO
  END IF

ELSE

  NearestTime = nList

END IF

RETURN
END
