!***********************************************************************
!                INITPROC
!***********************************************************************
RECURSIVE INTEGER FUNCTION INITPROC(HDLG, MESSAGE, WPARAM, LPARAM)
!DEC$ ATTRIBUTES STDCALL, ALIAS : '_INITPROC@16' :: INITPROC
USE resource_fd
USE pcscipuf_fi
USE winAPI

!     This is the message handler for the CAUTION dialog boxes.

IMPLICIT NONE
INTEGER   HDLG,MESSAGE,WPARAM,LPARAM

INTEGER irv,istat
INTEGER ICOMM
INTEGER IFOCUS
INTEGER id_dialog
INTEGER id_level

SELECT CASE (MESSAGE)
  CASE (WM_INITDIALOG) !INITIALIZE
    id_dialog = IDB_CAUTION !  Set Dialog ID
    CALL SetHwndList(HDLG,id_dialog,id_level) !  Set Dialog ID into List
    CALL position_init(HDLG)
    INITPROC = TRUE
  CASE (WM_CTLCOLORDLG) !STATIC COLOR
    INITPROC = dlgBkBrush
  CASE (WM_CTLCOLORSTATIC) !STATIC COLOR
    CALL GetStaticID(HDLG,LPARAM,istat)
    SELECT CASE (istat)
      CASE (IDB_STATIC07)
        irv = SetTextColor(WPARAM,dlgCTxColor)
      CASE DEFAULT
        irv = SetTextColor(WPARAM,dlgATxColor)
    END SELECT
    irv = SetBkColor(WPARAM,dlgBkColor)
    INITPROC = dlgBkBrush
  CASE (WM_DESTROY) !DESTROY
    CALL DeleteHwndList(HDLG) !  Remove Dialog from List
  CASE (WM_COMMAND) !COMMANDS
    CALL split_param(WPARAM,ICOMM,IFOCUS) !  Get Command and Focus
    IF( ICOMM == IDB_BUTTON1 )THEN
      CALL CallButton(hwnd_tb,IDB_HELP,irv) !Call HELP Button
    ELSE
    END IF
!          call DeleteControlImage(HDLG,IDB_STATIC99)
    irv = EndDialog(HDLG,TRUE)
    INITPROC = TRUE
  CASE DEFAULT
    INITPROC = FALSE
END SELECT
RETURN
END

!***********************************************************************
!                Position Caution Box
!***********************************************************************
SUBROUTINE position_init(HWND)

USE resource_fd
USE pcscipuf_fi
USE winAPI

!     This routine positions the CAUTION box in the center of the screen
!     and sets Version Number in the Message

IMPLICIT NONE

INTEGER HWND

INTEGER irv !,ibmp

CALL center_window(HWND)

irv = LoadString(MyAppInst, &
           IDS_SCIPUFF, &
           string2, &
           LEN(string2))
IF( irv == 0 )THEN
  string1 = 'Load String Error'
ELSE
  string1 = TRIM(string2(1:irv))//TRIM(ToolVersionString)
END IF

CALL SetControlText(HWND,IDB_STATIC02,string1)

irv = LoadString(MyAppInst, &
           IDS_HASCAL, &
           string2, &
           LEN(string2))
IF( irv == 0 )THEN
  string1 = 'Load String Error'
ELSE
  string1 = TRIM(string2(1:irv))//TRIM(GUIversionString)
END IF

CALL SetControlText(HWND,IDB_STATIC01,string1)

irv = LoadString(MyAppInst, &
           IDS_SCIP, &
           string2, &
           LEN(string2))
IF( irv == 0 )THEN
  string1 = 'Load String Error'
ELSE
  string1 = TRIM(string2(1:irv))//TRIM(SCIPversionString)
END IF

CALL SetControlText(HWND,IDB_STATIC06,string1)

string1 = 'SCIPUFF Version'//cnull
irv = SetWindowText(HWND,string1)

RETURN

END

