!*******************************************************************************
!$RCSfile$
!$Revision$
!$Date$
!*******************************************************************************
MODULE netcdf_fd

!------ Interface block for netCDF calls

USE netcdf
USE DefSize_fd

IMPLICIT NONE

INCLUDE 'netcdf.inc'

CONTAINS

  FUNCTION nc_inq_libvers()   !C character function
    USE basic_fd
!DEC# ATTRIBUTES C, DECORATE, ALIAS : 'nc_inq_libvers' :: nc_inq_libvers
    INTEGER nc_inq_libvers
    INTEGER, EXTERNAL :: ADDRESSOF
    CHARACTER*128 libvers
    libvers        = nf90_inq_libvers()
    nc_inq_libvers = ADDRESSOF(libvers)
  END FUNCTION nc_inq_libvers

  FUNCTION nc_strerror( ios )   !C character function
!DEC# ATTRIBUTES C, DECORATE, ALIAS:'nc_strerror' :: nc_strerror
!DEC# ATTRIBUTES VALUE :: ios
    INTEGER :: nc_strerror
    INTEGER, INTENT( IN )  :: ios
    INTEGER, EXTERNAL :: ADDRESSOF
    CHARACTER(PATH_MAXLENGTH) errString

    errString   = nf90_strerror(ios)
    nc_strerror = ADDRESSOF(errString)
  END FUNCTION nc_strerror

  INTEGER FUNCTION nc_open( fname,omode,fid )
!DEC# ATTRIBUTES C, DECORATE, ALIAS:'nc_open' :: nc_open
!DEC# ATTRIBUTES REFERENCE :: fname
!DEC# ATTRIBUTES VALUE     :: omode
!DEC# ATTRIBUTES REFERENCE :: fid
    CHARACTER(*), INTENT( IN )  :: fname
    INTEGER,      INTENT( IN )  :: omode
    INTEGER,      INTENT( OUT ) :: fid
    nc_open = nf90_open(TRIM(fName), omode, fid)
  END FUNCTION nc_open

  INTEGER FUNCTION nc_close( fid )
!DEC# ATTRIBUTES C, DECORATE, ALIAS:'nc_close' :: nc_close
!DEC# ATTRIBUTES VALUE :: fid
    INTEGER, INTENT( IN ) :: fid
    nc_close = nf90_close( fid )
  END FUNCTION nc_close

  INTEGER FUNCTION nc_inq( fid,ndims,nvars,natts,unlimdimID )
!DEC# ATTRIBUTES C, DECORATE, ALIAS:'nc_inq' :: nc_inq
!DEC# ATTRIBUTES VALUE     :: fid
!DEC# ATTRIBUTES REFERENCE :: ndims, nvars, natts, unlimdimID
    INTEGER, INTENT( IN )  :: fid
    INTEGER, INTENT( OUT ) :: ndims, nvars, natts, unlimdimID
    INTEGER formatNum
    nc_inq = nf90_inquire(fid,ndims,nvars,natts,unlimdimID,formatNum)
  END FUNCTION nc_inq

  INTEGER FUNCTION nc_inq_varid( fid,varnam,varid )
!DEC# ATTRIBUTES C, DECORATE, ALIAS:'nc_inq_varid' :: nc_inq_varid
!DEC# ATTRIBUTES VALUE     :: fid
!DEC# ATTRIBUTES REFERENCE :: varnam
!DEC# ATTRIBUTES REFERENCE :: varid
    INTEGER,      INTENT( IN  ) :: fid
    CHARACTER(*), INTENT( IN  ) :: varnam
    INTEGER,      INTENT( OUT ) :: varid
    INTEGER                     :: varidp1
    nc_inq_varid = nf90_inq_varid( fid,varnam,varidp1 )
    varid = varidp1 - 1
  END FUNCTION nc_inq_varid

  INTEGER FUNCTION nc_inq_vartype( fid,varid,ivtype )
!DEC# ATTRIBUTES C, DECORATE, ALIAS:'nc_inq_vartype' :: nc_inq_vartype
!DEC# ATTRIBUTES VALUE     :: fid, varid
!DEC# ATTRIBUTES REFERENCE :: ivtype
    INTEGER,               INTENT( IN  ) :: fid, varid
    INTEGER,               INTENT( OUT ) :: ivtype
    INTEGER                              :: varidp1
    varidp1 = varid + 1
    nc_inq_vartype = nf90_inquire_variable( fid,varidp1,xtype=ivtype )
  END FUNCTION nc_inq_vartype

  INTEGER FUNCTION nc_inq_attname( fid,iv,attnum,attname )
!DEC# ATTRIBUTES C, DECORATE, ALIAS:'nc_inq_attname' :: nc_inq_attname
!DEC# ATTRIBUTES VALUE     :: fid, iv, attnum
!DEC# ATTRIBUTES REFERENCE :: attname
    INTEGER,      INTENT( IN  ) :: fid, iv, attnum
    CHARACTER(*), INTENT( OUT ) :: attname
    INTEGER                     :: ivp1, attnp1
    ivp1   = iv + 1
    attnp1 = attnum + 1
    nc_inq_attname = nf90_inq_attname( fid,ivp1,attnp1,attname )
  END FUNCTION nc_inq_attname

  INTEGER FUNCTION nc_inq_atttype( fid,varid,attname,atttype )
!DEC# ATTRIBUTES C, DECORATE, ALIAS:'nc_inq_atttype' :: nc_inq_atttype
!DEC# ATTRIBUTES VALUE     :: fid, varid
!DEC# ATTRIBUTES REFERENCE :: attname
!DEC# ATTRIBUTES REFERENCE :: atttype
    INTEGER,      INTENT( IN  ) :: fid, varid
    CHARACTER(*), INTENT( IN  ) :: attname
    INTEGER,      INTENT( OUT ) :: atttype
    INTEGER                     :: varidp1
    varidp1 = varid + 1
    nc_inq_atttype = nf90_inquire_attribute( fid, varidp1, attname, xtype=atttype )
  END FUNCTION nc_inq_atttype

  INTEGER FUNCTION nc_get_att_text( fid,varid,attname,string )
!DEC# ATTRIBUTES C, DECORATE, ALIAS:'nc_get_att_text' :: nc_get_att_text
!DEC# ATTRIBUTES VALUE     :: fid, varid
!DEC# ATTRIBUTES REFERENCE :: attname
!DEC# ATTRIBUTES REFERENCE :: string
    INTEGER,      INTENT( IN  ) :: fid, varid
    CHARACTER(*), INTENT( IN  ) :: attname
    CHARACTER(*), INTENT( OUT ) :: string
    INTEGER                     :: varidp1
    varidp1 = varid + 1
    nc_get_att_text = nf90_get_att( fid,varidp1,attname,string )
  END FUNCTION nc_get_att_text

  INTEGER FUNCTION nc_get_att_float( fid,varid,attname,attval )
!DEC# ATTRIBUTES C, DECORATE, ALIAS:'nc_get_att_float' :: nc_get_att_float
!DEC# ATTRIBUTES VALUE     :: fid, varid
!DEC# ATTRIBUTES REFERENCE :: attname
!DEC# ATTRIBUTES REFERENCE :: attval
    INTEGER,      INTENT( IN  ) :: fid, varid
    CHARACTER(*), INTENT( IN  ) :: attname
    REAL,         INTENT( OUT ) :: attval
    INTEGER                       :: varidp1
    varidp1 = varid + 1
    nc_get_att_float = nf90_get_att( fid,varidp1,attname,attval )
  END FUNCTION nc_get_att_float

  INTEGER FUNCTION nc_get_att_short( fid,varid,attname,attval )
!DEC# ATTRIBUTES C, DECORATE, ALIAS:'nc_get_att_short' :: nc_get_att_short
!DEC# ATTRIBUTES VALUE     :: fid, varid
!DEC# ATTRIBUTES REFERENCE :: attname
!DEC# ATTRIBUTES REFERENCE :: attval
    INTEGER,      INTENT( IN  ) :: fid, varid
    CHARACTER(*), INTENT( IN  ) :: attname
    INTEGER(2),   INTENT( OUT ) :: attval
    INTEGER                     :: varidp1
    varidp1 = varid + 1
    nc_get_att_short = nf90_get_att( fid,varidp1,attname,attval )
  END FUNCTION nc_get_att_short

  INTEGER FUNCTION nc_get_att_int( fid,varid,attname,attval )
!DEC# ATTRIBUTES C, DECORATE, ALIAS:'nc_get_att_int' :: nc_get_att_int
!DEC# ATTRIBUTES VALUE     :: fid, varid
!DEC# ATTRIBUTES REFERENCE :: attname
!DEC# ATTRIBUTES REFERENCE :: attval
    INTEGER,      INTENT( IN  ) :: fid, varid
    CHARACTER(*), INTENT( IN  ) :: attname
    INTEGER,      INTENT( OUT ) :: attval
    INTEGER                     :: varidp1
    varidp1 = varid + 1
    nc_get_att_int = nf90_get_att( fid,varidp1,attname,attval )
  END FUNCTION nc_get_att_int

  INTEGER FUNCTION nc_inq_dim( fid,dimid,dimname,dimlen )
!DEC# ATTRIBUTES C, DECORATE, ALIAS:'nc_inq_dim' :: nc_inq_dim
!DEC# ATTRIBUTES VALUE     :: fid, dimid
!DEC# ATTRIBUTES REFERENCE :: dimname
!DEC# ATTRIBUTES REFERENCE :: dimlen
    INTEGER,               INTENT( IN  ) :: fid, dimid
    CHARACTER(*),          INTENT( OUT ) :: dimname
    INTEGER,               INTENT( OUT ) :: dimlen
    nc_inq_dim = nf90_inquire_dimension( fid,dimid+1,dimname,dimlen )
  END FUNCTION nc_inq_dim

  INTEGER FUNCTION nc_inq_dimid( fid,dimname,dimid )
!DEC# ATTRIBUTES C, DECORATE, ALIAS:'nc_inq_dimid' :: nc_inq_dimid
!DEC# ATTRIBUTES VALUE     :: fid
!DEC# ATTRIBUTES REFERENCE :: dimname
!DEC# ATTRIBUTES REFERENCE :: dimid
    INTEGER,               INTENT( IN  ) :: fid
    CHARACTER(*),          INTENT( IN  ) :: dimname
    INTEGER,               INTENT( OUT ) :: dimid
    nc_inq_dimid = nf90_inq_dimid( fid,dimname,dimid )
  END FUNCTION nc_inq_dimid

END MODULE netcdf_fd
