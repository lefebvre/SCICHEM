!*******************************************************************************
!$RCSfile$
!$Revision$
!$Date$
!*******************************************************************************
MODULE BotCells_fi

  INTEGER iBot, nBot

  INTEGER, DIMENSION(:), ALLOCATABLE :: idBot
  REAL,    DIMENSION(:), ALLOCATABLE :: xBot, yBot, dxBot, dyBot

END MODULE BotCells_fi

!==============================================================================

INTEGER FUNCTION SetBotCells( dat,mxgrd,p )

!------ Fill bottom cell arrays for sampler concentration file
!       Called as a SAG bottom function

USE BotCells_fi
USE sagstr_fd
USE sagdef_fd
USE sagcel_fd

IMPLICIT NONE

REAL, POINTER, DIMENSION(:)       :: dat
INTEGER,             INTENT( IN ) :: mxgrd
TYPE( SAGcell_str ), INTENT( IN ) :: p

iBot = iBot + 1

idBot(iBot) = p%id
xBot(iBot)  = p%x
yBot(iBot)  = p%y
dxBot(iBot) = p%hx
dyBot(iBot) = p%hy

SetBotCells = SAG_OK

RETURN
END

