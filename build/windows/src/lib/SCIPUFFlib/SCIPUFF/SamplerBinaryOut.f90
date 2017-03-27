!*******************************************************************************
!$RCSfile$
!$Revision$
!$Date$
!*******************************************************************************
SUBROUTINE out_smpBinaryInit()

USE scipuff_fi
USE met_fi
USE sampler_fi
USE files_fi
USE error_fi

IMPLICIT NONE

INTEGER ios, is
INTEGER thisYear, thisMonth, thisDay
REAL    thisTimeHours

INTEGER, EXTERNAL :: WriteBinaryInt, WriteBinaryInt8

CALL init_binary_sampler()
IF( nError /= NO_ERROR )GOTO 9999

nError = WR_ERROR
eRoutine = 'out_smp'
eMessage = 'Error writing output time to binary sampler file'

CALL year_month_day( t/3600.+tstart,year_start,month_start,day_start, &
                     thisTimeHours,thisYear,thisMonth,thisDay )
ios = WriteBinaryInt( lun_sps,isps,thisYear  ); IF( ios /= 0 )GOTO 9999
ios = WriteBinaryInt( lun_sps,isps,thisMonth ); IF( ios /= 0 )GOTO 9999
ios = WriteBinaryInt( lun_sps,isps,thisDay   ); IF( ios /= 0 )GOTO 9999
is = INT(thisTimeHours)
ios = WriteBinaryInt( lun_sps,isps,is ); IF( ios /= 0 )GOTO 9999
is = INT(MOD(INT(thisTimeHours * 60), 60))
ios = WriteBinaryInt( lun_sps,isps,is ); IF( ios /= 0 )GOTO 9999
is = INT(MOD(INT(thisTimeHours * 3600), 60))
ios = WriteBinaryInt( lun_sps,isps,is ); IF( ios /= 0 )GOTO 9999

intSPSDataLength = nsmpT * nbinOut * 4
ios = WriteBinaryInt8( lun_sps,isps,intSPSDataLength ); IF( ios /= 0 )GOTO 9999

CALL init_error()

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE init_binary_sampler()

USE scipuff_fi
USE met_fi
USE sampler_fi
USE files_fi
USE SamplerGridOuput

IMPLICIT NONE

INTEGER ios, alloc_stat
LOGICAL lSPS
INTEGER i, j
INTEGER ni, nj
INTEGER mcID, iv

TYPE( SampClassT ), POINTER :: SampClass

INTEGER(KIND=4) :: is, intSPS
REAL(KIND=4)    :: sngSPS

LOGICAL :: lKmToM, lSouthern

CHARACTER(256) strSPSString, units
CHARACTER(256) class, strSave
CHARACTER(256) specieName
CHARACTER(256), DIMENSION(:), ALLOCATABLE :: unitsStr, classStr

INTEGER, EXTERNAL :: WriteBinaryString, WriteBinaryInt, WriteBinaryInt8, WriteBinaryReal

!------ If binary sampler file not begun, create header here

IF( .NOT.lIsSPSOpened )THEN

  nError   = WR_ERROR
  eRoutine = 'init_binary_sampler'
  eMessage = 'Error writing binary sampler file'

  lIsSPSOpened = .TRUE.

! **************************************
! BINARY SAMPLER OUTPUT FILE MAIN HEADER
! **************************************

  isps = 1  !Record count

  strSPSString = "SCIPUFF Samplers"
  ios = WriteBinaryString( lun_sps,isps,strSPSString ); IF( ios /= 0 )GOTO 9999

  sngSPS = 1.4     !SPS file version
  ios = WriteBinaryReal( lun_sps,isps,sngSPS ); IF( ios /= 0 )GOTO 9999

  intSPSTimeCountFilePosition = HUGE( intSPSTimeCountFilePosition )  !Dummy integer(8) value; will be position of time-varying output
  ios = WriteBinaryInt8( lun_sps,isps,intSPSTimeCountFilePosition ); IF( ios /= 0 )GOTO 9999  !
  intSPSDataLength = isps-2                                     !Save record for write below

!------ Coordinate system specs

  strSPSString = "LATLON"
  SELECT CASE( lmap )
    CASE( I_UTM )
      strSPSString = "UTM"
    CASE( I_CARTESIAN )
      strSPSString = "CARTESIAN"
    CASE( I_METERS )
      strSPSString = "METERS"
  END SELECT
  ios = WriteBinaryString( lun_sps,isps,strSPSString ); IF( ios /= 0 )GOTO 9999

  strSPSString = "WGS84"
  ios = WriteBinaryString( lun_sps,isps,strSPSString ); IF( ios /= 0 )GOTO 9999  !Datum

  lKmToM = .FALSE.
  strSPSString = "DEGREES"
  SELECT CASE( lmap )
    CASE( I_UTM )
      strSPSString = "METERS"
      lKmToM = .TRUE.
    CASE( I_CARTESIAN )
      strSPSString = "METERS"
      lKmToM = .TRUE.
    CASE( I_METERS )
      strSPSString = "METERS"
  END SELECT
  ios = WriteBinaryString( lun_sps,isps,strSPSString ); IF( ios /= 0 )GOTO 9999  !Horiz units

  strSPSString = "METERS"
  ios = WriteBinaryString( lun_sps,isps,strSPSString ); IF( ios /= 0 )GOTO 9999  !Vert units

  ios = WriteBinaryInt( lun_sps,isps,utm_zone ); IF( ios /= 0 )GOTO 9999  !UTM zone

  strSPSString = "N"
  lSouthern = .FALSE.
  IF( lmap == I_UTM )CALL is_southern_hemisphere( lSouthern )
  IF( lSouthern )strSPSString = "S"
  ios = WriteBinaryString( lun_sps,isps,strSPSString ); IF( ios /= 0 )GOTO 9999  !Hemisphere

  ios = WriteBinaryReal( lun_sps,isps,lon0 ); IF( ios /= 0 )GOTO 9999  !Origin lon
  ios = WriteBinaryReal( lun_sps,isps,lat0 ); IF( ios /= 0 )GOTO 9999  !Origin lon

  sngSPS = 0.0
  ios = WriteBinaryReal( lun_sps,isps,sngSPS ); IF( ios /= 0 )GOTO 9999  !Std parallel 1
  ios = WriteBinaryReal( lun_sps,isps,sngSPS ); IF( ios /= 0 )GOTO 9999  !Std parallel 1
  ios = WriteBinaryReal( lun_sps,isps,sngSPS ); IF( ios /= 0 )GOTO 9999  !Azimuth

  sngSPS = 1.0
  ios = WriteBinaryReal( lun_sps,isps,sngSPS ); IF( ios /= 0 )GOTO 9999  !Scale factor

  sngSPS = 0.0
  ios = WriteBinaryReal( lun_sps,isps,sngSPS ); IF( ios /= 0 )GOTO 9999  !False east
  ios = WriteBinaryReal( lun_sps,isps,sngSPS ); IF( ios /= 0 )GOTO 9999  !False north

!------ Time convention

  intSPS = INT(tzone)
  ios = WriteBinaryInt( lun_sps,isps,intSPS ); IF( ios /= 0 )GOTO 9999  !Time zone

  intSPS = 0; IF( local )intSPS = 1
  ios = WriteBinaryInt( lun_sps,isps,intSPS ); IF( ios /= 0 )GOTO 9999  !Local time flag

! **************************************
! SAMPLER DEFINITIONS
! **************************************

! get the number of unique materials for these samplers
  intSPSMaterialCount = nSampClass  !"Material" is legacy of original EPA code

  intSPS = 0        !Get number of locations per class; must check for fixed grids
  DO is = 1,nsmp,nSampClass
    IF( BTEST(smp(is)%stype,STB_FXGRID) )THEN
      intSPS = intSPS + smp(is)%nx*smp(is)%ny
    ELSE
      intSPS = intSPS + 1
    END IF
  END DO

  ios = WriteBinaryInt( lun_sps,isps,intSPS ); IF( ios /= 0 )GOTO 9999

  nsmpT = intSPS

  DO is = 1,nsmp,nSampClass

    ni = smp(is)%nx
    nj = smp(is)%ny

    DO j = 1,nj
      DO i = 1,ni

        IF( BTEST(smp(is)%stype,STB_FXGRID) )THEN
          sngSPS = smp(is)%x + FLOAT(i-1)*smp(is)%dxs
        ELSE
          sngSPS = smp(is)%x
        END IF
        IF( xref /= NOT_SET_R )sngSPS = sngSPS + xref
        IF( lKmToM )sngSPS = sngSPS * 1000.
        ios = WriteBinaryReal( lun_sps,isps,sngSPS ); IF( ios /= 0 )GOTO 9999

        IF( BTEST(smp(is)%stype,STB_FXGRID) )THEN
          sngSPS = smp(is)%y + FLOAT(j-1)*smp(is)%dys
        ELSE
          sngSPS = smp(is)%y
        END IF
        IF( yref /= NOT_SET_R )sngSPS = sngSPS + yref
        IF( lSouthern )sngSPS = sngSPS + 10000  !Only for UTM - assumed in KM
        IF( lKmToM )sngSPS = sngSPS * 1000.
        ios = WriteBinaryReal( lun_sps,isps,sngSPS ); IF( ios /= 0 )GOTO 9999
        sngSPS = smp(is)%z + hmin
        ios = WriteBinaryReal( lun_sps,isps,sngSPS ); IF( ios /= 0 )GOTO 9999

      END DO
    END  DO
  END DO

  lKgToUg = .FALSE.

  ALLOCATE( unitsStr(nSampClass), classStr(nSampClass),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'init_binary_sampler'
    eMessage = 'Error allocating units and class string arrays'
    GOTO 9999
  END IF

  DO is = 1,nSampClass     !Set units based on first nSampClass samplers (at first location); set class string too
    SELECT CASE( smp(is)%type )
      CASE DEFAULT  !Assumes material-based (should be ok based on previous checks)
        intSPSMaterial = typeID(smp(is)%is)%imat
        units = TRIM(material(intSPSMaterial)%unit) !Mass units
        class = TRIM(material(intSPSMaterial)%ccls) !Material class
    END SELECT
    IF( BTEST(smp(is)%stype,STB_DEP) )THEN
      units = TRIM(units)//'/m^2'
    ELSE
      units = TRIM(units)//'/m^3'
    END IF
    IF( BTEST(smp(is)%stype,STB_INTEGRATE) )units = TRIM(units)//'-sec'
    unitsStr(is) = TRIM(units)
    classStr(is) = TRIM(class)
  END DO

!------ Write the "materials"
!       N.B. Assumes only certain sensors are defined, e.g., CONC, DEP, ...

  intSPS = 0
  DO is = 1,nSampClass
    intSPS = intSPS + 1
    IF( lOutputVariance )intSPS = intSPS + 1
  END DO
  ios = WriteBinaryInt( lun_sps,isps,intSPS ); IF( ios /= 0 )GOTO 9999

  nbinOut = intSPS
  iv = 0

  DO intSPSLoop = 1,intSPSMaterialCount
    IF( intSPSLoop == 1 )THEN
      SampClass => FirstSampClass
    ELSE
      SampClass => SampClass%next
    END IF
    strSPSString = TRIM(SampClass%type)//' '//TRIM(SampClass%var)
    IF( BTEST(smp(intSPSLoop)%stype,STB_MULT) )THEN
      i = INDEX(strSPSString,':',BACK=.TRUE.)
      IF( i > 1 )strSPSString = TRIM(strSPSString(1:i-1))
    END IF
    units = TRIM(unitsStr(intSPSLoop))
    ios = WriteBinaryString( lun_sps,isps,strSPSString ); IF( ios /= 0 )GOTO 9999
    strSave = strSPSString
    strSPSString = TRIM(classStr(intSPSLoop))
    ios = WriteBinaryString( lun_sps,isps,strSPSString ); IF( ios /= 0 )GOTO 9999
    ios = WriteBinaryString( lun_sps,isps,units ); IF( ios /= 0 )GOTO 9999
    IF( lOutputVariance )THEN
      strSPSString = TRIM(strSave)//" variance"
      ios = WriteBinaryString( lun_sps,isps,strSPSString ); IF( ios /= 0 )GOTO 9999
      strSPSString = TRIM(classStr(intSPSLoop))
      ios = WriteBinaryString( lun_sps,isps,strSPSString ); IF( ios /= 0 )GOTO 9999
      units = '('//TRIM(units)//')^2'
      ios = WriteBinaryString( lun_sps,isps,units ); IF( ios /= 0 )GOTO 9999
    END IF
  END DO

!------ Save current file position so we can come back to write number of times in the file

  intSPSTimeCountFilePosition = (isps-1)*4           !Save current record (zero-based bytes for post-processor)
  isps = intSPSDataLength                            !Prepare to overwrite (record saved above)

  ios = WriteBinaryInt8( lun_sps,isps,intSPSTimeCountFilePosition ); IF( ios /= 0 )GOTO 9999  !

  isps = intSPSTimeCountFilePosition/4 + 1           !Put back current record

  intSPSDumpCount = 123456                           !Write unique number for check when finalizing
  ios = WriteBinaryInt( lun_sps,isps,intSPSDumpCount ); IF( ios /= 0 )GOTO 9999

  intSPSDumpCount = 0                                !Zero count

  CALL init_error()

END IF

9999 CONTINUE

IF( ALLOCATED(unitsStr) )DEALLOCATE( unitsStr,STAT=alloc_stat)
IF( ALLOCATED(classStr) )DEALLOCATE( classStr,STAT=alloc_stat)

RETURN
END

!==============================================================================

INTEGER FUNCTION WriteBinaryString( lun,irec,string ) RESULT( ios )

IMPLICIT NONE

INTEGER,      INTENT( IN    ) :: lun
INTEGER(8),   INTENT( INOUT ) :: irec
CHARACTER(*), INTENT( IN    ) :: string

INTEGER i, j, ilen

CHARACTER(3) :: pad = '   '

ilen = LEN_TRIM(string)
i    = (ilen + 3)/4

WRITE(lun,REC=irec,IOSTAT=ios) i*4
IF( ios /= 0 )RETURN
irec = irec + 1

DO i = 1,ilen,4
  j = i+3
  IF( j > ilen )THEN
    WRITE(lun,REC=irec,IOSTAT=ios) string(i:ilen)//pad(1:j-ilen)
  ELSE
    WRITE(lun,REC=irec,IOSTAT=ios) string(i:j)
  END IF
  IF( ios /= 0 )EXIT
  irec = irec + 1
END DO

RETURN
END

!==============================================================================

INTEGER FUNCTION WriteBinaryInt( lun,irec,i4 ) RESULT( ios )

IMPLICIT NONE

INTEGER,    INTENT( IN    ) :: lun
INTEGER(8), INTENT( INOUT ) :: irec
INTEGER,    INTENT( IN    ) :: i4

WRITE(lun,REC=irec,IOSTAT=ios) i4
IF( ios == 0 )irec = irec + 1

RETURN
END

!==============================================================================

INTEGER FUNCTION WriteBinaryReal( lun,irec,r4 ) RESULT( ios )

IMPLICIT NONE

INTEGER,    INTENT( IN    ) :: lun
INTEGER(8), INTENT( INOUT ) :: irec
REAL,       INTENT( IN    ) :: r4

WRITE(lun,REC=irec,IOSTAT=ios) r4
IF( ios == 0 )irec = irec + 1

RETURN
END

!==============================================================================

INTEGER FUNCTION WriteBinaryInt8( lun,irec,i8 ) RESULT( ios )

IMPLICIT NONE

INTEGER,    INTENT( IN    ) :: lun
INTEGER(8), INTENT( INOUT ) :: irec
INTEGER(8), INTENT( IN    ) :: i8

INTEGER i

INTEGER(4), DIMENSION(2) :: intT

intT = TRANSFER(i8,intT)

DO i = 1,2
  WRITE(lun,rec=irec,IOSTAT=ios) intT(i)
  IF( ios /= 0 )EXIT
  irec = irec + 1
END DO

RETURN
END

!==============================================================================

SUBROUTINE is_southern_hemisphere( lSouthern )

USE sampler_fi

IMPLICIT NONE

LOGICAL, INTENT( OUT ) :: lSouthern

INTEGER ismp
REAL    ycoord

lSouthern = .TRUE.
DO ismp = 1,nsmp
  ycoord = smp(ismp)%y
  IF( ycoord >= 0.0 )THEN
    lSouthern = .FALSE.
    EXIT
  END IF
END DO

RETURN
END

!==============================================================================

SUBROUTINE out_smpBinarySingle()

USE scipuff_fi
USE met_fi
USE sampler_fi
USE files_fi
USE error_fi

IMPLICIT NONE

INTEGER ios, iFirstSmp, is
REAL    sngSPS, fac

INTEGER, EXTERNAL :: WriteBinaryReal

nError   = WR_ERROR
eRoutine = 'out_smp'
eMessage = 'Error writing to binary sampler file'

IF( lKgToUg )THEN
  fac = 1.0E9
ELSE
  fac = 1.
END IF

iFirstSmp = 0

DO intSPSLoop = 1,intSPSMaterialCount
  iFirstSmp = iFirstSmp + 1

  DO is = iFirstSmp,nsmp,intSPSMaterialCount             !First variable (mean)
    sngSPS = smp(is)%dsmp(1)*fac
    ios = WriteBinaryReal( lun_sps,isps,sngSPS ); IF( ios /= 0 )GOTO 9999
  END DO

  IF( lOutputVariance )THEN                              !Second variable (variance)
    DO is = iFirstSmp,nsmp,intSPSMaterialCount
      sngSPS = smp(is)%dsmp(2)*fac**2
      ios = WriteBinaryReal( lun_sps,isps,sngSPS ); IF( ios /= 0 )GOTO 9999
    END DO
  END IF

END DO

CALL init_error()

intSPSDumpCount = intSPSDumpCount + 1

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE finalize_binary_sampler()

!------ Write final number of output times to binary sampler file

USE scipuff_fi
USE files_fi
USE sampler_fi

IMPLICIT NONE

INTEGER    ios, idum
INTEGER(8) irec

INTEGER, EXTERNAL :: WriteBinaryInt

IF( lIsSPSOpened )THEN

  eRoutine = 'finalize_binary_sampler'

  irec = intSPSTimeCountFilePosition/4 + 1
  READ(lun_sps,REC=irec,IOSTAT=ios) idum
  IF( ios /= 0 )THEN
    nError  = RD_ERROR
    eInform = 'Error reading number of times on binary sampler file'
    GOTO 9999
  END IF
  IF( idum /= 123456 )THEN
    nError  = IV_ERROR
    eInform = 'Invalid number of times on binary sampler file'
    GOTO 9999
  END IF

  nError  = WR_ERROR
  eInform = 'Error writing number of times on binary sampler file'

  ios = WriteBinaryInt( lun_sps,irec,intSPSDumpCount ); IF( ios /= 0 )GOTO 9999

END IF

CALL init_error()

9999 CONTINUE

CLOSE(lun_sps,IOSTAT=ios)

RETURN
END

