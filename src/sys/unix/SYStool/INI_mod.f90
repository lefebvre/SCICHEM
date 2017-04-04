!*******************************************************************************
!$RCSfile$
!$Revision$
!$Date$
!*******************************************************************************
MODULE INI
  INTERFACE

!==============================================================================
! get_private_profile_string
!==============================================================================
    INTEGER FUNCTION  get_private_profile_string (lpAppName ,lpKeyName ,lpDefault ,lpReturnedString ,nSize ,lpFileName   )
!DEC# ATTRIBUTES DEFAULT                                         :: get_private_profile_string
!DEC# ATTRIBUTES C, ALIAS : '_get_private_profile_string'  :: get_private_profile_string
!DEC# ATTRIBUTES REFERENCE                                       :: lpAppName
!DEC# ATTRIBUTES REFERENCE                                       :: lpKeyName
!DEC# ATTRIBUTES REFERENCE                                       :: lpDefault
!DEC# ATTRIBUTES REFERENCE                                       :: lpReturnedString
!DEC# ATTRIBUTES REFERENCE                                       :: lpFileName
      CHARACTER(*)   lpAppName
      CHARACTER(*)   lpKeyName
      CHARACTER(*)   lpDefault
      CHARACTER(*)   lpReturnedString
      INTEGER        nSize
      CHARACTER(*)   lpFileName
    END FUNCTION get_private_profile_string

!==============================================================================
! write_private_profile_string
!==============================================================================
    INTEGER FUNCTION  write_private_profile_string (lpAppName ,lpKeyName ,lpString ,lpFileName   )
!DEC# ATTRIBUTES DEFAULT                                           :: write_private_profile_string
!DEC# ATTRIBUTES C, ALIAS : '_write_private_profile_string'  :: write_private_profile_string
!DEC# ATTRIBUTES REFERENCE                                         :: lpAppName
!DEC# ATTRIBUTES REFERENCE                                         :: lpKeyName
!DEC# ATTRIBUTES REFERENCE                                         :: lpString
!DEC# ATTRIBUTES REFERENCE                                         :: lpFileName
      CHARACTER(*)   lpAppName
      CHARACTER(*)   lpKeyName
      CHARACTER(*)   lpString
      CHARACTER(*)   lpFileName
    END FUNCTION write_private_profile_string

    INTEGER FUNCTION  delete_private_profile_string (lpAppName ,lpKeyName ,lpString ,lpFileName   )
!DEC# ATTRIBUTES DEFAULT                                           :: delete_private_profile_string
!DEC# ATTRIBUTES C, ALIAS : '_write_private_profile_string'  :: delete_private_profile_string
!DEC# ATTRIBUTES REFERENCE                                         :: lpAppName
!DEC# ATTRIBUTES REFERENCE                                         :: lpKeyName
!DEC# ATTRIBUTES VALUE                                             :: lpString
!DEC# ATTRIBUTES REFERENCE                                         :: lpFileName
      CHARACTER(*)   lpAppName
      CHARACTER(*)   lpKeyName
      INTEGER        lpString
      CHARACTER(*)   lpFileName
    END FUNCTION delete_private_profile_string

    INTEGER FUNCTION  delete_private_profile_section (lpAppName ,lpKeyName ,lpString ,lpFileName   )
!DEC# ATTRIBUTES DEFAULT                                           :: delete_private_profile_section
!DEC# ATTRIBUTES C, ALIAS : '_write_private_profile_string'  :: delete_private_profile_section
!DEC# ATTRIBUTES REFERENCE                                         :: lpAppName
!DEC# ATTRIBUTES VALUE                                             :: lpKeyName
!DEC# ATTRIBUTES VALUE                                             :: lpString
!DEC# ATTRIBUTES REFERENCE                                         :: lpFileName
      CHARACTER(*)   lpAppName
      INTEGER        lpKeyName
      INTEGER        lpString
      CHARACTER(*)   lpFileName
    END FUNCTION delete_private_profile_section
!==============================================================================
! get_private_profile_int
!==============================================================================
    INTEGER FUNCTION  get_private_profile_int (lpAppName ,lpKeyName ,nDefault ,lpFileName   )
!DEC# ATTRIBUTES DEFAULT                                      :: get_private_profile_int
!DEC# ATTRIBUTES C, ALIAS : '_get_private_profile_int'  :: get_private_profile_int
!DEC# ATTRIBUTES REFERENCE                                    :: lpAppName
!DEC# ATTRIBUTES REFERENCE                                    :: lpKeyName
!DEC# ATTRIBUTES REFERENCE                                    :: lpFileName
      CHARACTER(*)   lpAppName
      CHARACTER(*)   lpKeyName
      INTEGER        nDefault
      CHARACTER(*)   lpFileName
    END FUNCTION get_private_profile_int

  END INTERFACE
END MODULE INI
