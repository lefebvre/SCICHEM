!*******************************************************************************
!$RCSfile$
!$Revision$
!$Date$
!*******************************************************************************
MODULE winAPI_fd
  USE basic_fd, ONLY: TRUE, FALSE, NULL
  USE ifwinty, HIDE_TRUE => TRUE, HIDE_FALSE => FALSE, HIDE_NULL=>NULL, HIDE_NO_ERROR => NO_ERROR &
             , HIDEX_ERROR => ERROR
  INTEGER(POINTER_LEN), EXTERNAL :: ADDRESSOF

!==============================================================================
! WINDOWS API parameter and structure definitions
!==============================================================================
!
! PARAMETERS
!
END MODULE winAPI_fd
!==============================================================================
!==============================================================================
!==============================================================================
!==============================================================================
MODULE winAPI
  USE winAPI_fd
  USE IFWINA, HIDEX_TRUE => TRUE, HIDEX_FALSE => FALSE, HIDEX_NULL=>NULL, HIDEX_NO_ERROR => NO_ERROR &
            , HIDEX_ERROR => ERROR
!==============================================================================
! WINDOWS API function interfaces
!==============================================================================


END MODULE winAPI
