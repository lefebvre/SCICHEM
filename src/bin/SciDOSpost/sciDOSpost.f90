!
!*************************************************************************
!
PROGRAM runSciDosPost

! SCIDOSPOST version 1.0, 2015-05-15

USE DefSize_fd
USE Extract_fi
USE tooluser_fd
USE SCIPtool
USE GetTimes_fi
USE domainCoord_fd
USE sagdef_fd
USE sagstr_fd
USE PtrGrdStrItf
USE scipuff_fi, ONLY: create,ntypm,material,mat_mc
USE MPI_fi, ONLY: useMPI,myid
USE sciDOSpostTools
USE met_fi, ONLY:PrjCoord

IMPLICIT NONE

TYPE( SAGgrid_str ), POINTER     :: grdC, grdA, grdD

CHARACTER (len=8)                :: maxPath
CHARACTER (len=20)               :: units
CHARACTER (len=PATH_MAXLENGTH)   :: dosFile, depFile, adosFile
CHARACTER (len=256)              :: Command
character (len=8)                :: datestr  ! current date
character (len=10)               :: timestr  ! current time
character (len=6)                :: AVEstr   ! AERMOD averaging time eg "  1-HR"

INTEGER             :: nOuts            ! number of Output values to find
INTEGER             :: iOut             ! index for Output
INTEGER             :: nRsets           ! number of Receptor Sets to process
INTEGER             :: iRset            ! index for Receptor Set
INTEGER             :: nRecs            ! number of receptors in a Receptor Set
INTEGER             :: iRec             ! index for receptor within Receptor Set
INTEGER             :: MaxRec           ! max( nRec(:) )
INTEGER             :: MaxRank          ! max( output(:)%rank )
INTEGER             :: avg_time         ! used to convert units (s)
INTEGER             :: astat            ! short for alloc_stat
INTEGER             :: irv              ! return value for SCIP calls
INTEGER             :: ij               ! index of lat,lon where answer is found
INTEGER             :: iy,im,id,ih,iymdh! year, month, day, hour, YYYYMMDDHH
INTEGER             :: iRun_Hours       ! Length of this run, in hours
INTEGER             :: iy1,im1,id1,ih1, iy2,im2,id2,ih2 ! used to calc iRun_Hours 
INTEGER             :: iClass, iChoice, iKind, iDOS, iDEP
INTEGER             :: i, j, iTime, ios, iArg, ir
INTEGER             :: lun_dos, lun_ados, lun_dep,lun
INTEGER             :: numCommand
INTEGER(2)          :: ios2

INTEGER, DIMENSION(:), ALLOCATABLE :: cGrdI, aGrdI, dGrdI

LOGICAL             :: lexist, lopen, lAmb    ! does a file exist, open ?
LOGICAL             :: isMULT = .false. ! multi-component or tracer run?
LOGICAL             :: first  = .true.  ! first of the projName files, do init
LOGICAL             :: skip             ! either read or skip next timestamp

REAL                :: con_units_conv, dep_units_conv ! units conversion factors
REAL                :: answer           ! what we seek

REAL                :: Tk,Pa,Hg,Hsx,rh  ! Temp., pressure, humidity, saturation, RH
REAL                :: tot_SO4,tot_NO3,tot_NH4,tot_HNO3,tot_NH3
REAL                :: amb_SO4,amb_NO3,amb_NH4,amb_HNO3,amb_NH3
REAL                :: PNO3_old,PNH4_old

INTEGER, EXTERNAL   :: SCIPGetPrjCoord
INTEGER, EXTERNAL   :: SAG_InitList, SAG_ClearList, SAG_SetSpecialValue
INTEGER, EXTERNAL   :: SAG_OpenID, SAG_CloseID, SAG_RmvGrdStr
INTEGER, EXTERNAL   :: SAG_NewGrdStr, SAG_InitGridID, SAG_ReadHeaderID
INTEGER, EXTERNAL   :: sysNumArgs
INTEGER, EXTERNAL   :: sysGetArg

CHARACTER(128), EXTERNAL :: AddExtension

!*************************************************************************
!--- BEGIN MAIN PROGRAM
!*************************************************************************

useMPI = .FALSE.
myid   = 0
create = .FALSE.
lAmb   = .FALSE.
call date_and_time(datestr,timestr) ! current time, for the POSTFILE header

!--- Start up : Initialize common

CALL InitSciDosPost()

INQUIRE(UNIT=24,OPENED=lopen,IOSTAT=ios)
IF( .NOT. lopen )THEN
  OPEN(24,FILE='sciDOSpost.log',position='append')
ELSE
  OPEN(24,FILE='sciDOSpost.log')
END IF

!--- Get the input file from the command line

controlFile = 'sciDOSpost.inp' ! default
numCommand  = sysNumArgs()
iArg = 0
do while (iArg < numCommand)
   iArg = iArg + 1
   ios2 = sysGetArg(iArg,Command)
   if (Command(1:3) == '-I:') then
      read(Command(4:len_trim(Command)),'(a256)') ini_file
   elseif (trim(Command) == '-i') then
      iArg = iArg + 1
      ios2 = sysGetArg(iArg,controlFile)
   else if (trim(Command) == '-h' .or. trim(Command) == '--help') then
      call usage
   else if (trim(Command) == '--sample') then
      call sample_control_file
   else if (trim(Command) == '--version') then
      write(*,*) "SciDOSpost version 1.1, 2017-02-22"
      stop
   else if (trim(Command) == '--debug') then
      debug = .true.
   else
      write(*,*) "Error: unknown switch ",trim(Command)
      call usage
   end if
end do

!--- Start up : Write Screen Header

WRITE(maxPath,'(I0)') PATH_MAXLENGTH

WRITE(6,'(/,A)')'Program : SciDosPost v'//TRIM(CODE_VERSION)//' &
     &(maxPath='//ADJUSTL(TRIM(maxPath))//')'

irv = SCIPGetVersionString(USER_ID,toolString)
IF( irv == SCIPsuccess )THEN
  irv = SCIPGetPathMaxLength()
  IF( irv == PATH_MAXLENGTH )THEN
    WRITE(6,'(A)')'   uses : SCIPtool v'//TRIM(toolString%string)
  ELSE
    nError = UK_ERROR
    eMessage = 'Error:SCIPtool PATH_MAXLENGTH mismatch'
    eInform ='SCIPUFFPostProcess='//ADJUSTL(TRIM(maxPath))//' : SCIPtool='
    WRITE(maxPath,'(I0)')irv
    eInform =TRIM(eInform)//ADJUSTL(TRIM(maxPath))
    GO TO 9999
  END IF
ELSE
  nError = UK_ERROR
  eMessage = 'Error checking SCIPtool version'
  GO TO 9999
END IF

!--- Start up : Initialize SCIPtool library

CALL InitToolSciDosPost()
IF ( nError /= NO_ERROR ) GOTO 9999

!--- Count the number of occurences of a few keywords

call control_counts(nRsets,nOuts)

!--- Allocate the arrays, based on the counts

allocate( output(nOuts),STAT=astat )
IF( astat > 0 )THEN
  nError = UK_ERROR
  WRITE(eMessage,*) 'Error allocating outputs(nOuts)'
  WRITE(eInform,*)  'nOuts = ',nOuts
  GO TO 9999
END IF
output(1:nOuts)%exceeds = HUGE(0.)  ! largest number possible
output(1:nOuts)%rank    = 1         ! default to output rank 1 (H1H)
output(1:nOUts)%lun     = 0         ! default logical unit number

allocate( Rset(nRsets),STAT=astat )
IF( astat > 0 )THEN
  nError = UK_ERROR
  WRITE(eMessage,*) 'Error allocating Rset(nRsets)'
  GO TO 9999
END IF

allocate( ProjName(nProj),STAT=astat )
IF( astat > 0 )THEN
  nError = UK_ERROR
  WRITE(eMessage,*) 'Error allocating ProjName(nProj)'
  GO TO 9999
END IF

!--- Parse the control file

call read_control
IF( nError /= NO_ERROR ) goto 9999

!--- Read in the lat-lon or UTM receptors, after allocating lat and lon

call read_rec_set( nRsets, MaxRec )
IF( nError /= NO_ERROR ) goto 9999
if (debug) then
   print*,"nRsets = ",nRsets
   do i = 1,nRsets
      print*,"iRset,nRec(i)  = ",i,nRec(i)
   end do
   print*,"size,shape of lon ",size(lon),shape(lon)
   print*,"size,shape of lat ",size(lat),shape(lat)
   print*,"size,shape of xrec ",size(xrec),shape(xrec)
   print*,"size,shape of yrec ",size(yrec),shape(yrec)
   call flush(6)
end if

!--- Initialize the visibility calcuations, if we'll need them

if ( any(output(:)%ConDepVis == "VIS") ) then
   call init_visibility(nRsets)
   IF( nError /= NO_ERROR ) goto 9999
end if

!--- First delete all the output files, so we can (possibly) append to them

do iOut = 1, nOuts
   open (100, file=output(iOut)%outFile)
   close(100, status='delete')
end do

!--- Open the output files, and write headers

lun = 500 ! start number output logical unit numbers at 300 to avoid conflicts
do iOut = 1, nOuts

   inquire(file=output(iOut)%outFile, exist=lexist)
   if (.not. lexist) then ! not found, must be new, write a header
      lun = lun + 1
      output(iOut)%lun = lun
      open(lun, file=output(iOut)%outFile, status="new",iostat=ios)
      write(*,'(1x,2(a,i4),2a)') "Output ",iOut," created new file ",lun-500," ", &
           trim(output(iOut)%outFile)
      if (ios /= 0) then
         nError   = UK_ERROR
         eRoutine = 'SciDosPost'
         write(eMessage,*) 'Error ',ios,'opening output file.'
         WRITE(eInform,*)  'File = ',TRIM(output(iOut)%outFile)
         goto 9999
      end if

      if (output(iOut)%outType == "CSV") then

         if (output(iOut)%ConDepVis == "VIS" .and. &
              output(iOut)%ChemSpecies == "CONT") then
            write(lun,'(a)') &
                 "RecSet, Species, X, Y, Time, Period(hr), Frequency(hr), Rank, dBext(%), Bext_Model, Bext_Backgroundl, bNO3, bSO4, bOCM, bSOIL, bPEC, bNO2, bCORS"
         else
            write(lun,'(a)') &
                 "RecSet, Species, X, Y, Time, Period(hr), Frequency(hr), Rank, Value"
         end if

      else if (output(iOut)%outType == "XYZ") then

         write(lun,*) "# X      Y      Value"
         
      else if (output(iOut)%outType == "POST" .or. &
               output(iOut)%outType == "PLOT" ) then
!
!---- write a header to each POSTFILE
!
10       format(8a)         ! the formats used in the POSTFILE header
11       format(a,75x,6a)
12       format(a,i6,a)

         write(lun, 10) "* sciDOSpost (2015-05-06): ", ProjName(1)(1:73), &
             datestr(3:4),"/",datestr(5:6),"/",datestr(7:8)
         write(lun, 11) "* MODELING OPTIONS USED: ",                &
             timestr(1:2),":",timestr(3:4),":",timestr(5:6)
        ! I believe this next line is enough to keep AERMOD happy:
        write(lun, 10) "* NonDFAULT CONC                                              ELEV"
        write(AVEstr,'(i3,"-HR")') output(iOut)%avgPeriods
        write(lun, 10) "*         POST/PLOT FILE OF CONCURRENT ",  &
             AVEstr, " VALUES FOR SOURCE GROUP: ","ALL"
        write(lun, 12) "*         FOR A TOTAL OF",nRec(1)," RECEPTORS."
        write(lun, 10) "*         FORMAT: (3(1X,F13.5),3(1X,F8.2),2X,",&
             "A6,2X,A8,2X,I8.8,2X,A8)"
        write(lun, 10) "*        X             Y      AVERAGE CONC", &
             "    ZELEV    ZHILL    ZFLAG    AVE     GRP       DATE     NET ID"
        write(lun, 10) "* ____________  ____________  ____________", &
             "   ______   ______   ______  ______  ________  ________  ________"

     end if

   else ! we've already created it, we'll append to it by assigning the same LUN

      do i = iOut, 1, -1
         if (output(iOut)%outFile == output(i)%outFile) &
              output(iOut)%lun = output(i)%lun
      end do
      write(*,'(1x,2(a,i4),2a)') "Output ",iOut," appended to file ",  & 
           output(iOut)%lun-500," ", trim(output(iOut)%outFile)

   end if

end do
write(*,*)      ! make screen output look nicer

ALLOCATE(cGrdI(nProj),aGrdI(nProj),dGrdI(nProj),STAT=irv )
IF( irv /= 0 )THEN
  nError = UK_ERROR
  eRoutine = 'SciDosPost'
  eMessage = 'Error allocating cGrdI'
  WRITE(eInform,'(A,I0,A,I0)')'Request =',nProj,' : Error=',irv
  GO TO 9999
END IF

!******** MAIN LOOP OVER PROJECT (Basename) FILES ********

do iProj = 1, nProj

! -- Get Plot Times

   WRITE(24,*)'Reading project files for ',TRIM(ProjName(iProj))
   CALL SplitName( ProjName(iProj),Project%name,Project%path ) !project name, path
   Project%ID      = 0
   Project%version = 0

!==== Action : Retrieve output times

   irv = SCIPNumPlotTimes( CallerID,Project,nTimePuff,nTimeSrf,nTimeMet,nNotUsed )
   IF( irv /= SCIPsuccess )THEN
      CALL toolError( 'Failed retrieving number of project plot times' )
      GOTO 9999
   END IF
   
   CALL allocatePlotTimes( )

   irv = SCIPGetPlotTimes( callerID,Project,TimePuff,TimeSrf,TimeMet )
   IF( irv /= SCIPsuccess )THEN
      CALL toolError( 'Failed retrieving project plot times' )
      GOTO 9999
   END IF

   TimeOut  => TimeSrf
   nTimeOut = nTimeSrf

!==== Action : Set Plot Field

   Field%maxCells = limit%surfaceGrid
   Field%project  = TRIM(Project%name)
   Field%path     = TRIM(Project%path)
   
   Field%class    = -1
   Field%choice   = -1
   Field%category = HP_SURF
   PlotType%type  = HP_MEAN

   if (first) then ! first of the ProjName files, do some initialization stuff

      irv = SCIPNumPlotClasses( callerID,Project,nClass,nChoice,nKind )
      IF( irv /= SCIPsuccess )THEN
         CALL toolError( 'Failed retrieving number of project plot classes' )
         GOTO 9999
      END IF

      CALL allocatePlotLists( )

      irv = SCIPGetPlotClasses( callerID,Project,ClassStr,ChoiceStr,KindStr, &
           CatClassArray,ClassChoiceArray,ProjectCoordinate )
      IF( irv /= SCIPsuccess )THEN
         CALL toolError( 'Failed retrieving project plot classes' )
         GOTO 9999
      END IF

! Convert between LATLON and UTM. xRec,yRec need to be in ProjectCoordinate%mode
! format

      if (debug) then
         write(*,'(a,$)') "Project coords = "
         if (ProjectCoordinate%mode == HD_LATLON)    print*,"LATLON"
         if (ProjectCoordinate%mode == HD_CARTESIAN) print*,"CARTESIAN"
         if (ProjectCoordinate%mode == HD_UTM)       print*,"UTM"
         if (ProjectCoordinate%mode == HD_METERS)    print*,"METERS"
      end if
   

      if (ProjectCoordinate%mode == HD_LATLON) then
         DO iRset = 1,nRsets
            if (Rset(iRset)%CoordType == "LATLON") then
               xRec(iRset,1:nRec(iRset)) = lon(iRset,1:nRec(iRset)) ! read into lat,lon
               yRec(iRset,1:nRec(iRset)) = lat(iRset,1:nRec(iRset)) ! in read_rec_set()
!      else if (Rset(iRset)%CoordType == "UTM") then 
         ! FIXME: USE datums, call UTM2LL to support user giving UTM 
            else
               write(*,*) "ERROR: Receptor Set ",iRset," is not LATLON, yet the project"
               write(*,*) "       is in LATLON. Stopping."
               stop
            end if
         END DO
      else ! not LATLON, one of CARTESIAN, UTM, etc.
         DO iRset = 1,nRsets
            if (Rset(iRset)%CoordType == "LATLON") then ! convert LON,LAT to xRec,yRec
               irv = SCIPGetPrjCoord( callerID, nRec(iRset),                 &
                    lon(iRset,1:nRec(iRset)),   lat(iRset,1:nRec(iRset)),    &
                    xRec(iRset,1:nRec(iRset)),  yRec(iRset,1:nRec(iRset)) )
               IF( irv /= SCIPsuccess )THEN
                  CALL toolError( 'Failed converting to project coordinates' )
                  GOTO 9999
               END IF
            else
               xRec(iRset,1:nRec(iRset)) = lon(iRset,1:nRec(iRset)) ! read into lat,lon
               yRec(iRset,1:nRec(iRset)) = lat(iRset,1:nRec(iRset)) ! in read_rec_set()
            end if
         END DO
      end if

!--- Find the iClass for the DOSAGE (con) file, call it iDOS

      iDOS = -1
      DO i = 1,nClass
         string1 = ClassStr(i)%string
         CALL CUPPER( string1 )
         IF( INDEX(string1,'SURFACE DOSAGE') > 0 )THEN
            iDOS = i
            EXIT
         END IF
      END DO
      IF( iDOS == -1 )THEN
         WRITE(*,*)'SURFACE DOSAGE not available'
         GOTO 9999
      END IF
      Field%class = iDOS

!--- Find the Choice, which determines multi-component vs. tracer runs

      iChoice = -1
      DO i = 1,nChoice
         string1 = ChoiceStr(i)%string
         CALL CUPPER( string1 )
         IF( INDEX(string1,':COMPONENTS') > 0 )THEN
            iChoice = i
            EXIT
         END IF
      END DO

      isMult = .FALSE.
      IF( iChoice == -1 )THEN
         WRITE(*,*)'This is a Single-Component run.'
         iChoice = 1 ! no ambient ados or asmp files for single-component runs
         nKind   = 1
      ELSE
         WRITE(*,*)'This is a Multi-Component run.'
         isMult = .TRUE.
         if (debug) print*,"iDos,iChoice = ",iDOS,iChoice
         nKind  = ClassChoiceArray(iDOS,iChoice)%nkind
      END IF
      if (debug) print*,"nKind (number of chemical species) =",nKind
      write(*,*) ! blank line after multi-single component message

      Field%choice = iChoice
      CALL setPlotClassData(.FALSE.,.FALSE.)

!--- Find the index of each chemical species we need to use later

      allocate(iCon(nOuts)) ! This triggers whether or not to process in calc_con
      iCon = 0              ! Fill with zeroes, non-zero means 'process this one'

!--- Initialize to -1 so we can detect if they are not found

      iPSO4  = -1 ; iASO4K = -1 ; iPNO3  = -1 ; iANO3K = -1 ; iPSOA  = -1
      iPOC   = -1 ; iPEC   = -1 ; iASOIL = -1 ; iACORS = -1 ; iNO2   = -1
      iNO    = -1 ; iNO3   = -1 ; iN2O5  = -1 ; iHNO3  = -1 ; iHONO  = -1
      iPNA   = -1 ; iPAN   = -1 ; iPANX  = -1 ; iNTR   = -1 ; iNH3   = -1
      iPNH4  = -1 ; iANH4K = -1 ; iSO2   = -1 ; iSULF  = -1 
      iO3    = -1 ; iPM25  = -1 ; iPM10  = -1

      do iKind = 1, nKind

         if (isMULT) then                        ! multi-species output
            Field%kind = iKind + ClassChoiceArray(iDos,iChoice)%ikind - 1
         else                                    ! Only Tracer output
            Field%kind = 0
         endif

         if (isMult) then                        ! multi-species output
            string1 = KindStr(Field%kind)%string
         else                                    ! Only Tracer output
            string1 = ChoiceStr(iChoice)%string
         endif
         CALL CUPPER( string1 )

         print*,"Found chemical/particulate species: ",trim(string1)

! These concentrations are to be averaged/ranked and output

         do iOut = 1, nOuts
            if (TRIM(string1) == output(iOut)%chemSpecies) iCon(iOut) = iKind
         end do

! Treat these specially
         
         if (TRIM(string1) == 'O3'   ) iO3    = iKind ! ozone
         if (TRIM(string1) == 'PM25' ) iPM25  = iKind ! PM 2.5 microns or less
         if (TRIM(string1) == 'PM10' ) iPM10  = iKind ! PM  10 microns or less

! These species are needed for visilbity

         if (TRIM(string1) == 'PSO4' ) iPSO4  = iKind ! small sulfate
         if (TRIM(string1) == 'ASO4K') iASO4K = iKind ! large sulfate
         if (TRIM(string1) == 'PNO3' ) iPNO3  = iKind ! small nitrate
         if (TRIM(string1) == 'ANO3K') iANO3K = iKind ! large nitrate
         if (TRIM(string1) == 'PSOA' ) iPSOA  = iKind ! small organic mass comp
         if (TRIM(string1) == 'POC'  ) iPOC   = iKind ! small organic mass comp
         if (TRIM(string1) == 'PEC'  ) iPEC   = iKind ! elemental carbon
         if (TRIM(string1) == 'ASOIL') iASOIL = iKind ! soil
         if (TRIM(string1) == 'ACORS') iACORS = iKind ! coarse mass
         if (TRIM(string1) == 'NO2'  ) iNO2   = iKind ! nitrogen dioxide

! These additional species are needed for deposition
! Note: NO2, PNO3, ANO3K, PSO4, ASO4K, already found above

         if (TRIM(string1) == 'NO'   ) iNO    = iKind
         if (TRIM(string1) == 'NO3'  ) iNO3   = iKind
         if (TRIM(string1) == 'N2O5' ) iN2O5  = iKind
         if (TRIM(string1) == 'HNO3' ) iHNO3  = iKind
         if (TRIM(string1) == 'HONO' ) iHONO  = iKind
         if (TRIM(string1) == 'PNA'  ) iPNA   = iKind
         if (TRIM(string1) == 'PAN'  ) iPAN   = iKind
         if (TRIM(string1) == 'PANX' ) iPANX  = iKind
         if (TRIM(string1) == 'NTR'  ) iNTR   = iKind
         if (TRIM(string1) == 'NH3'  ) iNH3   = iKind
         if (TRIM(string1) == 'PNH4' ) iPNH4  = iKind
         if (TRIM(string1) == 'ANH4K') iANH4K = iKind
         if (TRIM(string1) == 'SO2'  ) iSO2   = iKind
         if (TRIM(string1) == 'SULF' ) iSULF  = iKind

      end do  ! do i = 1, nKind

!--- Warn the user about missing chemical species.  We'll just skip their
!    contributions.

      if (isMult) then ! Don't warn if we don't expect them
         if ( iO3     == -1) write(*,*) "Warning! Did NOT find chemical/particulate species: O3   "
         if ( iPM25   == -1) write(*,*) "Warning! Did NOT find chemical/particulate species: PM25 "
         if ( iPM10   == -1) write(*,*) "Warning! Did NOT find chemical/particulate species: PM10 "
         if ( iPSO4   == -1) write(*,*) "Warning! Did NOT find chemical/particulate species: PSO4 "
         if ( iASO4K  == -1) write(*,*) "Warning! Did NOT find chemical/particulate species: ASO4K"
         if ( iPNO3   == -1) write(*,*) "Warning! Did NOT find chemical/particulate species: PNO3 "
         if ( iANO3K  == -1) write(*,*) "Warning! Did NOT find chemical/particulate species: ANO3K"
         if ( iPSOA   == -1) write(*,*) "Warning! Did NOT find chemical/particulate species: PSOA "
         if ( iPOC    == -1) write(*,*) "Warning! Did NOT find chemical/particulate species: POC  "
         if ( iPEC    == -1) write(*,*) "Warning! Did NOT find chemical/particulate species: PEC  "
         if ( iASOIL  == -1) write(*,*) "Warning! Did NOT find chemical/particulate species: ASOIL"
         if ( iACORS  == -1) write(*,*) "Warning! Did NOT find chemical/particulate species: ACORS"
         if ( iNO2    == -1) write(*,*) "Warning! Did NOT find chemical/particulate species: NO2  "
         if ( iNO     == -1) write(*,*) "Warning! Did NOT find chemical/particulate species: NO   "
         if ( iNO3    == -1) write(*,*) "Warning! Did NOT find chemical/particulate species: NO3  "
         if ( iN2O5   == -1) write(*,*) "Warning! Did NOT find chemical/particulate species: N2O5 "
         if ( iHNO3   == -1) write(*,*) "Warning! Did NOT find chemical/particulate species: HNO3 "
         if ( iHONO   == -1) write(*,*) "Warning! Did NOT find chemical/particulate species: HONO "
         if ( iPNA    == -1) write(*,*) "Warning! Did NOT find chemical/particulate species: PNA  "
         if ( iPAN    == -1) write(*,*) "Warning! Did NOT find chemical/particulate species: PAN  "
         if ( iPANX   == -1) write(*,*) "Warning! Did NOT find chemical/particulate species: PANX "
         if ( iNTR    == -1) write(*,*) "Warning! Did NOT find chemical/particulate species: NTR  "
         if ( iNH3    == -1) write(*,*) "Warning! Did NOT find chemical/particulate species: NH3  "
         if ( iPNH4   == -1) write(*,*) "Warning! Did NOT find chemical/particulate species: PNH4 "
         if ( iANH4K  == -1) write(*,*) "Warning! Did NOT find chemical/particulate species: ANH4K"
         if ( iSO2    == -1) write(*,*) "Warning! Did NOT find chemical/particulate species: SO2  "
         if ( iSULF   == -1) write(*,*) "Warning! Did NOT find chemical/particulate species: SULF "
      end if     ! if (isMult) then
      write(*,*) ! just for looks

!--- Find the units of this DOSAGE file, and get the conversion factor

      Field%timeID   = 1
      Field%userTime = TimeOut(1)%time%runTime        ! integration time in hours
      avg_time       = TimeOut(1)%time%runTime * 3600 ! integration time in sec

      units = TRIM(ClassChoiceArray(iDOS,iChoice)%units)
      call CLOWER( units )
      call find_units_conv(units,"ug/m3", avg_time, con_units_conv)
      if (debug) print*,"units, con_units_conv = ",trim(units),con_units_conv

!--- Find the iClass for the DEPOSITION (drydep+wetdep) file, call it iDEP

      iDEP = -1
      DO i = 1,nClass
         string1 = ClassStr(i)%string
         CALL CUPPER( string1 )
         IF( INDEX(string1,'SURFACE DEPOSITION') > 0 )THEN
            iDEP = i
            EXIT
         END IF
      END DO
      IF( iDEP == -1 )THEN
         WRITE(*,*)'SURFACE DOSAGE not available'
         GOTO 9999
      END IF

!--- Find the units of this DEPOSITION file, and get the conversion factor

      Field%class = iDEP
      units = TRIM(ClassChoiceArray(iDep,iChoice)%units)

      call CLOWER( units )
      call find_units_conv( units, "kg/ha", avg_time, dep_units_conv )
      if (debug) print*,"units, dep_units_conv = ",trim(units),dep_units_conv

!--- Allocate vars to hold the concentration and deposition, at each Receptor
!    set, for nKind chemical species.  Use MaxRec = max(nRec(:))

      if (debug) then
         print*,"Allocation con,TotCon,AmbCon, using size"
         print*,"     nRsets,MaxRec,nKind =",nRsets,MaxRec,nKind
      end if
      
      ALLOCATE( con(nRsets,MaxRec,nKind), STAT=irv )
      IF( irv /= 0 )THEN
         nError = UK_ERROR
         eRoutine = 'SciDosPost'
         eMessage = 'Error allocating con'
         WRITE(eInform,'(A,I0,A,I0)')'Request =',MaxRec,' : Error=',irv
         GO TO 9999
      END IF
      con = 0.

      ALLOCATE( TotCon(nRsets,MaxRec,nKind), STAT=irv )
      IF( irv /= 0 )THEN
         nError = UK_ERROR
         eRoutine = 'SciDosPost'
         eMessage = 'Error allocating TotCon'
         WRITE(eInform,'(A,I0,A,I0)')'Request =',MaxRec,' : Error=',irv
         GO TO 9999
      END IF
      TotCon = 0.

      ALLOCATE( AmbCon(nRsets,MaxRec,nKind), STAT=irv )
      IF( irv /= 0 )THEN
         nError = UK_ERROR
         eRoutine = 'SciDosPost'
         eMessage = 'Error allocating AmbCon'
         WRITE(eInform,'(A,I0,A,I0)')'Request =',MaxRec,' : Error=',irv
         GO TO 9999
      END IF
      AmbCon = 0.

      ALLOCATE( dep(nRsets,MaxRec,nKind), STAT=irv )
      IF( irv /= 0 )THEN
         nError = UK_ERROR
         eRoutine = 'SciDosPost'
         eMessage = 'Error allocating dep'
         WRITE(eInform,'(A,I0,A,I0)')'Request =',MaxRec,' : Error=',irv
         GO TO 9999
      END IF
      dep = 0.

      ALLOCATE( drydep(nRsets,MaxRec,nKind), STAT=irv )
      IF( irv /= 0 )THEN
         nError = UK_ERROR
         eRoutine = 'SciDosPost'
         eMessage = 'Error allocating drydep'
         WRITE(eInform,'(A,I0,A,I0)')'Request =',MaxRec,' : Error=',irv
         GO TO 9999
      END IF
      drydep = 0.

      ALLOCATE( wetdep(nRsets,MaxRec,nKind), STAT=irv )
      IF( irv /= 0 )THEN
         nError = UK_ERROR
         eRoutine = 'SciDosPost'
         eMessage = 'Error allocating wetdep'
         WRITE(eInform,'(A,I0,A,I0)')'Request =',MaxRec,' : Error=',irv
         GO TO 9999
      END IF
      wetdep = 0.

      if (isMult) then ! also has Temperature, Pressure, Humidity
         ALLOCATE( srfFldVal(nRsets,MaxRec,nKind+3,2), STAT=irv ) ! temporary
      else
         ALLOCATE( srfFldVal(nRsets,MaxRec,nKind,  2), STAT=irv ) ! temporary
      end if
      IF( irv /= 0 )THEN
         nError = UK_ERROR
         eRoutine = 'SciDosPost'
         eMessage = 'Error allocating srfFldVal'
         WRITE(eInform,'(A,I0,A,I0)')'Request =',MaxRec,' : Error=',irv
         GO TO 9999
      END IF

      MaxRank = maxval( output(:)%rank )

      ALLOCATE( sum_val(nRsets,MaxRec,nOuts), STAT=irv )
      IF( irv /= 0 )THEN
         nError = UK_ERROR
         eRoutine = 'SciDosPost'
         eMessage = 'Error allocating sum_val'
         WRITE(eInform,'(A,I0,A,I0)')'Request =',nRsets*MaxRec*nOuts,' : Error=',irv
         GO TO 9999
      END IF
      sum_val = 0.

      ALLOCATE( max_val(nRsets,MaxRec,nOuts,MaxRank), STAT=irv )
      IF( irv /= 0 )THEN
         nError = UK_ERROR
         eRoutine = 'SciDosPost'
         eMessage = 'Error allocating max_val'
         WRITE(eInform,'(A,I0,A,I0)')'Request =',nRsets*MaxRec*nOuts*MaxRank, &
              ' : Error=',irv
         GO TO 9999
      END IF
      max_val = -HUGE(0.) ! most negative number

      ALLOCATE( max_time(nRsets,MaxRec,nOuts,MaxRank), STAT=irv )
      IF( irv /= 0 )THEN
         nError = UK_ERROR
         eRoutine = 'SciDosPost'
         eMessage = 'Error allocating max_time'
         WRITE(eInform,'(A,I0,A,I0)')'Request =',nRsets*MaxRec*nOuts*MaxRank, &
              ' : Error=',irv
         GO TO 9999
      END IF
      max_time = 0

      ALLOCATE( iHours(nRsets,nOuts), STAT=irv )
      IF( irv /= 0 )THEN
         nError = UK_ERROR
         eRoutine = 'SciDosPost'
         eMessage = 'Error allocating iHours'
         WRITE(eInform,'(A,I0,A,I0)')'Request =',nRsets*nOuts, &
              ' : Error=',irv
         GO TO 9999
      END IF
      iHours = 0
      
      ALLOCATE( iymdh_max(nRsets,MaxRec,nOuts), STAT=irv )
      IF( irv /= 0 )THEN
         nError = UK_ERROR
         eRoutine = 'SciDosPost'
         eMessage = 'Error allocating iymdh_max'
         WRITE(eInform,'(A,I0,A,I0)')'Request =',nOuts, &
              ' : Error=',irv
         GO TO 9999
      END IF
      iymdh_max = 0
      
      ALLOCATE( dBc_sum(nRsets,MaxRec,nOuts,nCont), STAT=irv )
      IF( irv /= 0 )THEN
         nError = UK_ERROR
         eRoutine = 'SciDosPost'
         eMessage = 'Error allocating dBc_sum'
         WRITE(eInform,'(A,I0,A,I0)')'Request =',nRsets*MaxRec*nOuts*nCont,' : Error=',irv
         GO TO 9999
      END IF
      dBc_sum = 0.
      
      ALLOCATE( dBc_max(nRsets,MaxRec,nOuts,MaxRank,nCont), STAT=irv )
      IF( irv /= 0 )THEN
         nError = UK_ERROR
         eRoutine = 'SciDosPost'
         eMessage = 'Error allocating dBc_sum'
         WRITE(eInform,'(A,I0,A,I0)')'Request =',nRsets*MaxRec*nOuts*nCont,' : Error=',irv
         GO TO 9999
      END IF
      dBc_max = -HUGE(0.) ! most negative number

      CALL init_error()

      irv = SAG_SetSpecialValue( .TRUE.,HP_SPV )

      irv = SAG_InitList()

      first = .false.

   end if ! if (first) then

   Field%project  = TRIM(Project%name)
   Field%path     = TRIM(Project%path)

   CALL ReadProject( Project )

   CALL init_srf_blocks( ntypm )

!--- Dos File

   dosFile  = TRIM( AddExtension( Project%name,'dos' ) )
   adosFile = TRIM( AddExtension( Project%name,'ados' ) )
   INQUIRE(FILE=TRIM(adosFile),EXIST=lAmb)

!------ Get a new SAG structure

   irv = SAG_NewGrdStr( cgrdI(iProj) )
   IF( irv /= SAG_OK )THEN
      nError   = UK_ERROR
      eRoutine = 'InitGrid'
      eMessage = 'Error creating SAG surface grid'
      CALL ReportFileName( eInform,'File=',dosFile )
      GOTO 9999
   END IF

!------ Initialize SAG structure

   lun_dos = 100 + cgrdI(iProj)
   irv = SAG_InitGridID( dosFile,lun_dos,SAG_GRID_BOTH,0,0,0,cgrdI(iProj) )
   IF( irv /= SAG_OK )THEN
      nError   = UK_ERROR
      eRoutine = 'InitGrid'
      eMessage = 'Error initializing SAG surface grid'
      CALL ReportFileName( eInform,'File=',dosFile )
      GOTO 9999
   END IF

!------ Open surface file; read nvart for block version

   irv = SAG_OpenID( cgrdI(iProj) )
   IF( irv /= SAG_OK )THEN
      nError   = UK_ERROR
      eRoutine = 'InitGrid'
      eMessage = 'Error opening SAG file'
      CALL ReportFileName( eInform,'File=',dosFile )
      GOTO 9999
   END IF

   grdC => SAG_PtrGrdStr( cgrdI(iProj) ) ! Associate "local" grid structure pointer

   grdC%mxnam = grdC%nvart

   IF( lAmb )THEN
!------ Get a new SAG structure
      irv = SAG_NewGrdStr( agrdI(iProj) )
      IF( irv /= SAG_OK )THEN
         nError   = UK_ERROR
         eRoutine = 'InitGrid'
         eMessage = 'Error creating SAG surface grid'
         CALL ReportFileName( eInform,'File=',adosFile )
         GOTO 9999
      END IF

!------ Initialize SAG structure
      lun_ados = 100 + agrdI(iProj)
      irv = SAG_InitGridID( adosFile,lun_ados,SAG_GRID_BOTH,0,0,0,agrdI(iProj) )
      IF( irv /= SAG_OK )THEN
         nError   = UK_ERROR
         eRoutine = 'InitGrid'
         eMessage = 'Error initializing SAG surface grid'
         CALL ReportFileName( eInform,'File=',adosFile )
         GOTO 9999
      END IF

!------ Open surface file; read nvart for block version
      irv = SAG_OpenID( agrdI(iProj) )
      IF( irv /= SAG_OK )THEN
         nError   = UK_ERROR
         eRoutine = 'InitGrid'
         eMessage = 'Error opening SAG file'
         CALL ReportFileName( eInform,'File=',adosFile )
         GOTO 9999
      END IF
      
      grdA => SAG_PtrGrdStr( agrdI(iProj) ) ! Associate "local" grid structure pointer
      grdA%mxnam = grdA%nvart
  
   ELSE
  
      agrdI(iProj) = -1
  
   END IF
  
! Deposition file

   depFile = TRIM( AddExtension( Project%name,'dep' ) )

!------ Get a new SAG structure

   irv = SAG_NewGrdStr( dgrdI(iProj) )
   IF( irv /= SAG_OK )THEN
      nError   = UK_ERROR
      eRoutine = 'InitGrid'
      eMessage = 'Error creating SAG surface grid'
      CALL ReportFileName( eInform,'File=',depFile )
      GOTO 9999
   END IF
   
!------ Initialize SAG structure

   lun_dep = 100 + dgrdI(iProj)
   irv = SAG_InitGridID( depFile,lun_dep,SAG_GRID_BOTH,0,0,0,dgrdI(iProj) )
   IF( irv /= SAG_OK )THEN
      nError   = UK_ERROR
      eRoutine = 'InitGrid'
      eMessage = 'Error initializing SAG surface grid'
      CALL ReportFileName( eInform,'File=',depFile )
      GOTO 9999
   END IF

!------ Open surface file; read nvart for block version

   irv = SAG_OpenID( dgrdI(iProj) )
   IF( irv /= SAG_OK )THEN
      nError   = UK_ERROR
      eRoutine = 'InitGrid'
      eMessage = 'Error opening SAG file'
      CALL ReportFileName( eInform,'File=',depFile )
      GOTO 9999
   END IF

   grdD => SAG_PtrGrdStr( dgrdI(iProj) ) ! Associate "local" grid structure pointer
   
   grdD%mxnam = grdD%nvart

! !--- Do some sanity checks 
!
! 2016-10-21 FIXME: need to get iy2... from last of the ProjName files...
!
!    if (iymdh_beg == 0) then ! flag to process all time found in file
!
!       iTime = 1
!       Field%timeID   = iTime
!       Field%userTime = TimeOut(iTime)%time%runTime  ! integration time in hours
!       iy1 = TimeOut(iTime)%time%year  ! this year
!       im1 = TimeOut(iTime)%time%month ! this month
!       id1 = TimeOut(iTime)%time%day   ! this month
!       ih1 = TimeOut(iTime)%time%hour  ! this month
!
!       iTime = nTimeOut
!       Field%timeID   = iTime
!       Field%userTime = TimeOut(iTime)%time%runTime  ! integration time in hours
!       iy2 = TimeOut(iTime)%time%year  ! this year
!       im2 = TimeOut(iTime)%time%month ! this month
!       id2 = TimeOut(iTime)%time%day   ! this month
!       ih2 = TimeOut(iTime)%time%hour  ! this month
!     
!       call TimeDiff(iy1,im1,id1,ih1, iy2,im2,id2,ih2, iRun_Hours)
!    else
!
!       call idate2ymdh(iymdh_beg ,iy1,im1,id1,ih1)
!       call idate2ymdh(iymdh_end ,iy2,im2,id2,ih2)
!       call TimeDiff(iy1,im1,id1,ih1, iy2,im2,id2,ih2, iRun_Hours)
!     
!    end if
!
!    do iOut = 1, nOuts
!       if ( iRun_Hours < output(iOut)%frequency ) then
! 1234     format(a,i6,a,i6,a)
!          write(*,1234) &
!         "***** WARNING: Run Length is less than Frequency for output number ", &
!               iOut
!     write(*,1234) "      Run Length is ",iRun_Hours," hours, Frequency is ",  &
!               output(iOut)%frequency," hours."
!       end if
!    end do

   if (debug) print*,"Working on dates from ",iymdh_cur," to ",iymdh_end
   INQUIRE(UNIT=24,OPENED=lopen,IOSTAT=ios)
   IF( .NOT. lopen )OPEN(24,FILE='sciDOSpost.log',position='append')
   write(24,*)"Working on dates from ",iymdh_cur," to ",iymdh_end

!*****************************************************************
!   LOOP OVER TIME STAMPS IN THIS PROJECT FILE
!*****************************************************************

   do iTime = 1, nTimeOut

! ---Get the concentrations and deposition (wet & dry) at all rececptors,
!    for this time-stamp

      Field%timeID   = iTime
      Field%userTime = TimeOut(iTime)%time%runTime  ! integration time in hours

      iy = TimeOut(iTime)%time%year  ! this year
      im = TimeOut(iTime)%time%month ! this month
      id = TimeOut(iTime)%time%day   ! this month
      ih = TimeOut(iTime)%time%hour  ! this month
      CALL legal_timestamp(iy,im,id,ih,23)
      iymdh = iymdh2idate(iy,im,id,ih)
      if( iymdh_cur == 0 )iymdh_cur = iymdh  ! Use the first available time 

      if (debug) then
        print*,"Current time-stamp from file:",iymdh
        write(24,*)"Current time-stamp from file:",iymdh,iymdh_cur,iymdh_end
      end if

!--- Retrieve or skip the concentration for this time-stamp

      if (iymdh_cur <= iymdh .and. iymdh <= iymdh_end) then
         skip = .false.
      else
         skip = .true.
      end if

      srfFldVal = 0. 
      CALL GetAllFieldValues( cgrdI(iProj),agrdI(iProj),nRsets,maxRec,nRec,.FALSE.,isMult,skip )
      IF( nError /= NO_ERROR )GOTO 9999
      

      if (skip) then
        !--- Skip the deposition for this time-stamp
        srfFldVal = 0.
        CALL GetAllFieldValues( dgrdI(iProj),agrdI(iProj),nRsets,maxRec,nRec,.TRUE.,isMult,skip )
        IF( nError /= NO_ERROR )GOTO 9999
        
        cycle ! skip the rest of the processing for this timestep
      end if

!--- Write some progress to the screen, but not too much

      if ((id == 1 .and. ih == 1) .or. iTime == 1) then
         write(*,*) ! CR
         write(*,'(a,i4,"-",i2.2,a)') "Processing YEAR-MO: ",iy,im,", Day:"
         write(24,'(a,i4,"-",i2.2,a)') "Processing YEAR-MO: ",iy,im,", Day:"
      endif
      if (ih == 1) then
         write(string1,*) id
         write(*,'(a," ",$)') trim(adjustl(string1))
      end if
      
!-- Increment the timestamp, so iymdh_cur is now the next desired timestamp

     call idate2ymdh(iymdh_cur,iy,im,id,ih)
     ih = ih + 1
     call legal_timestamp(iy,im,id,ih,23)
     iymdh_cur = iymdh2idate(iy,im,id,ih)      
      
      if (debug) then
         do i = 1,nRsets
            print*
            print*,"Receptor set #",i
            print*,"max con in srfFldVal =",maxval(srfFldVal(i,1:nRec(i),1:nKind,1))
            print*,"min con in srfFldVal =",minval(srfFldVal(i,1:nRec(i),1:nKind,1))
            if (isMult) then
               print*,"max T   = ",maxval(srfFldVal(i,1:nRec(i),nKind+1,2))
               print*,"min T   = ",minval(srfFldVal(i,1:nRec(i),nKind+1,2))
            end if
         end do
      end if
      
      where (srfFldVal == 1.e-30) srfFldVal = 0.

      if (.not. isMult) then

         con = srfFldVal(:,:,:,1) * con_units_conv

      else

         ! TotCon = Total Concentration = plume + background
         TotCon(:,:,:) = srfFldVal(:,:,1:nKind,1) * con_units_conv 
         ! AmbCon = Ambient Concentration = background
         AmbCon(:,:,:) = srfFldVal(:,:,1:nKind,2) * con_units_conv 

         if ( calc_total_con ) then
            ! chem species:
            con(:,:,:) = TotCon(:,:,1:nKind) 
         else
            ! chem species:
            if (debug .and. .false.) then ! FIXME: turn .false. into verbose?
               do i = 1,nRec(1)
                  print*,"i,TotCon, AmbCon:", &
                       i, TotCon(1,i,max(iCon(1),1)), AmbCon(1,i,max(iCon(1),1))
               end do
            end if
            con(:,:,:) = TotCon(:,:,1:nKind) - AmbCon(:,:,1:nKind)

            where (con >= -1.e-10 .and. con < 0.) con = 0. ! prevent small neg.

         end if

!
!---- Re-calculate the SO4/NO3/NH4/HNO3/NH3 partitioning, if possible
!
         if (iPSO4 > 0 .and. iPNO3 > 0 .and. iPNH4 > 0 .and. iHNO3 > 0 .and. &
              iNH3 > 0) then
            do iRset = 1, nRsets
               do iRec = 1, nRec(iRset)

                  Tk = srfFldVal(iRset,iRec,nKind+1,2) ! temperature, K
                  Pa = srfFldVal(iRset,iRec,nKind+2,2) ! pressure, atm
                  Hg = srfFldVal(iRset,iRec,nKind+3,2) ! absolute humidity, g/g
                  
                  call sat_humid(Tk,Pa,Hsx)
                  rh = max( min( Hg/Hsx, 1.), 0.01) ! enforce sane limits
                  
                  tot_SO4  = TotCon(iRset,iRec,iPSO4)
                  tot_NO3  = TotCon(iRset,iRec,iPNO3)
                  tot_NH4  = TotCon(iRset,iRec,iPNH4)
                  tot_HNO3 = TotCon(iRset,iRec,iHNO3)
                  tot_NH3  = TotCon(iRset,iRec,iNH3 )
                  
                  call inorg_aero(tot_SO4,tot_NO3,tot_NH4,tot_HNO3,tot_NH3,Tk,rh)
                  
                  amb_SO4  = AmbCon(iRset,iRec,iPSO4)
                  amb_NO3  = AmbCon(iRset,iRec,iPNO3)
                  amb_NH4  = AmbCon(iRset,iRec,iPNH4)
                  amb_HNO3 = AmbCon(iRset,iRec,iHNO3)
                  amb_NH3  = AmbCon(iRset,iRec,iNH3 )

                  call inorg_aero(amb_SO4,amb_NO3,amb_NH4,amb_HNO3,amb_NH3,Tk,rh)

                  amb_SO4  = min( tot_SO4 , amb_SO4 ) ! make sure amb < tot
                  amb_NO3  = min( tot_NO3 , amb_NO3 )
                  amb_NH4  = min( tot_NH4 , amb_NH4 )
                  amb_HNO3 = min( tot_HNO3, amb_HNO3)
                  amb_NH3  = min( tot_NH3 , amb_NH3 )

                  PNO3_old = con(iRset,iRec,iPNO3)
                  PNH4_old = con(iRset,iRec,iPNH4)

                  if ( calc_total_con ) then          ! plume + background = total
                     con(iRset,iRec,iPSO4)  = tot_SO4
                     con(iRset,iRec,iPNO3)  = tot_NO3 
                     con(iRset,iRec,iPNH4)  = tot_NH4 
                     con(iRset,iRec,iHNO3)  = tot_HNO3
                     con(iRset,iRec,iNH3 )  = tot_NH3 
                  else                                ! just plume
                     con(iRset,iRec,iPSO4)  = tot_SO4  - amb_SO4
                     con(iRset,iRec,iPNO3)  = tot_NO3  - amb_NO3 
                     con(iRset,iRec,iPNH4)  = tot_NH4  - amb_NH4 
                     con(iRset,iRec,iHNO3)  = tot_HNO3 - amb_HNO3
                     con(iRset,iRec,iNH3 )  = tot_NH3  - amb_NH3 
                  end if
                  
                  ! Adjust PM25 and PM10 for the newly-created vals

                  con(iRset,iRec,iPM25) = con(iRset,iRec,iPM25) & ! old PM2.5
                       + con(iRset,iRec,iPNO3) - PNO3_old & ! new - old PNO3 
                       + con(iRset,iRec,iPNH4) - PNH4_old   ! new - old PNH4
                  
                  con(iRset,iRec,iPM10) = con(iRset,iRec,iPM10) & ! old PM10
                       + con(iRset,iRec,iPNO3) - PNO3_old & ! new - old PNO3 
                       + con(iRset,iRec,iPNH4) - PNH4_old   ! new - old PNH4

               end do
            end do
         end if
      end if ! if (.not. isMult) then

!--- Retrieve the deposition for this time-stamp

      srfFldVal = 0.
      CALL GetAllFieldValues( dgrdI(iProj),agrdI(iProj),nRsets,maxRec,nRec,.TRUE.,isMult,skip )
      IF( nError /= NO_ERROR )GOTO 9999

      if (debug) then
         do i = 1,nRsets
            print*
            print*,"Receptor set #",i
            print*,"max dep = ",maxval(srfFldVal(i,1:nRec(i),1:nKind,1))
            print*,"min dep = ",minval(srfFldVal(i,1:nRec(i),1:nKind,1))
         end do
         print*
      end if
      
      where (srfFldVal == 1.e-30) srfFldVal = 0.
      dep(:,:,:) = (srfFldVal(:,:,:,1) + srfFldVal(:,:,:,2)) * dep_units_conv
      drydep(:,:,:) = srfFldVal(:,:,:,1) * dep_units_conv
      wetdep(:,:,:) = srfFldVal(:,:,:,2) * dep_units_conv

!*****************************************************************
!   LOOP OVER RECEPTOR SETS
!*****************************************************************

      do iOut = 1, nOuts
         do iRset = 1, nRsets

            if (output(iOut)%avgPeriods == 8760 .and. iHours(iRset,iOut) == 0 &
                 .and. leap_yr(iy)) output(iOut)%avgPeriods = 8784

            if (output(iOut)%ConDepVis == "CON") then ! iCon is like iKind here
               
               CALL calc_con( iRset, iCon(iOut), iOut, iymdh)
               IF( nError /= NO_ERROR ) goto 9999
               
            else if (output(iOut)%ConDepVis == "DEP") then
               
               CALL calc_dep( iRset, iCon(iout), iOut,iymdh) ! iCon == iDep
               IF( nError /= NO_ERROR ) goto 9999
               
            else if (output(iOut)%ConDepVis == "DRY") then
               
               CALL calc_dry_dep( iRset, iCon(iout), iOut,iymdh) ! iCon == iDep
               IF( nError /= NO_ERROR ) goto 9999
               
            else if (output(iOut)%ConDepVis == "WET") then
               
               CALL calc_wet_dep( iRset, iCon(iout), iOut,iymdh) ! iCon == iDep
               IF( nError /= NO_ERROR ) goto 9999
               
            else if (output(iOut)%ConDepVis == "VIS" .and. Rset(iRset)%class == 1)&
                 then
               
               CALL calc_vis( iRset, output(iOut)%ChemSpecies, iOut, im, iymdh)
               IF( nError /= NO_ERROR ) goto 9999
               
            end if
            
         end do ! do iRset = 1, nRsets
      end do    ! do iOut  = 1, nOuts
   end do       ! do iTime = 1, nTimeOut 
   
end do          ! do iProj = 1, nProj

write(*,*)      ! CR on last write(*,'(1x,i2.2,$)') TimeOut(iTime)%time%day
write(*,*)      ! an extra, just for looks

! Done with calculations, now write out the final answers

do iRset = 1, nRsets
   do iOut = 1, nOuts

      if (.not. (output(iOut)%ConDepVis == "VIS" .and. Rset(iRset)%class == 2)) &
           then 
      write(*,*) "Writing output ",iOut," ",trim(output(iOut)%chemSpecies), " ", &
           trim(output(iOut)%ConDepVis), " for ",Rset(iRset)%Name

!--- If this output's frequency hasn't yet been reached, calculate the 
!    average/max anyway, but set the hours to what we actually got.

      if (iHours(iRset,iOut) > 0 .and. &
           iHours(iRset,iOut) < output(iOut)%frequency) then
         do iRec = 1, nRec(iRset)

            output(iOut)%avgPeriods = iHours(iRset,iOut)

            if ( output(iOut)%stat == "A" ) then ! average by div sum/hours
               sum_val(iRset,iRec,iOut) = &
                    sum_val(iRset,iRec,iOut)/iHours(iRset,iOut)
            end if ! if output(iOut)%stat == "M", we already have the maximum
            
            ir = find_rank( sum_val(iRset,iRec,iOut), iRset, iRec, iOut, &
                 output(iOut)%rank)
            ! inserts into max_val at right rank
            call insert( sum_val(iRset,iRec,iOut), iRset, iRec, iOut,   &
                 output(iOut)%rank, ir, iymdh_max(iRset,iRec,iOut) ) 

            if (output(iOut)%chemSpecies == "CONT") then
               do i = 1, nCont ! dBc is the dBext contributions
                  ! inserts into dBc_max
                  call insert_dBc( dBc_sum(iRset,iRec,iOut,i),iRset,iRec,iOut,i, &
                       output(iOut)%rank, ir, iymdh_max(iRset,iRec,iOut) ) 
               end do
            end if

            if ( sum_val(iRset,iRec,iOut) >= output(iOut)%exceeds ) then
               
               write(string1,*) ">=",output(iOut)%exceeds
               if (output(iOut)%chemSpecies == "CONT") then
                  tmp7(:) = dBc_sum(iRset,iRec,iOut,:)
                  call write_vis( iOut, iRset, iRec, iymdh_max(iRset,iRec,iOut), &
                       string1, sum_val(iRset,iRec,iOut), tmp7, nCont )
               else
                  call write_output(iOut,iRset,iRec,iymdh_max(iRset,iRec,iOut), &
                       string1, sum_val(iRset,iRec,iOut) )
               end if
               
            end if
            
         end do ! do iRec = 1, nRec(iRset)
      end if    ! if (iHours(iRset,iOut) < output(iOut)%frequency) then

!--- Skip the exceedence outputs: we wrote those in calc_con() etc. Also 
!    skip VIS output for Class II receptors.

      if ( output(iOut)%exceeds == HUGE(0.) ) then ! flag for write max

         write(string1,*) output(iOut)%rank ! so H1H and H12H both work
         string1 = "H" // trim(adjustl(string1)) // "H"

         if (output(iOut)%sel == "MAX") then       ! write the single-point MAX

!--- Find the maximum (over receptors) of the right rank

            ij = -1            ! detect not found
            answer = -HUGE(0.) ! most negative number
            do iRec = 1, nRec(iRset)
               if ( max_val(iRset,iRec,iOut,output(iOut)%rank) > answer) then
                  answer = max_val(iRset,iRec,iOut,output(iOut)%rank)
                  iymdh  = max_time(iRset,iRec,iOut,output(iOut)%rank)
                  ij = iRec
               end if
            end do

!--- Write out the single-point "answer"

            if (output(iOut)%chemSpecies == "CONT") then
               tmp7(:) = dBc_max(iRset,ij,iOut,output(iOut)%rank,:)
               call write_vis( iOut, iRset, ij, iymdh, string1, &
                    answer, tmp7, nCont )
            else
               call write_output( iOut, iRset, ij, iymdh, string1, answer )
            end if

         else                                      ! write the MAX at ALL points

!--- Write out the "answer" at all points

            do iRec = 1, nRec(iRset)

               if (output(iOut)%chemSpecies == "CONT") then
                  tmp7(:) = dBc_max(iRset,iRec,iOut,output(iOut)%rank,:)
                  call write_vis( iOut, iRset, iRec,                         &
                       max_time(iRset,iRec,iOut,output(iOut)%rank), string1, &
                       max_val(iRset,iRec,iOut,output(iOut)%rank),           &
                       tmp7, nCont )
               else
                  call write_output( iOut, iRset, iRec,                      &
                       max_time(iRset,iRec,iOut,output(iOut)%rank), string1, &
                       max_val(iRset,iRec,iOut,output(iOut)%rank) )
               end if

            end do

         end if ! if (output(iOut)%sel == "MAX") then 

      end if ! if ( output(iOut)%exceeds == HUGE(0.) ) then
      end if ! .not. (output(iOut)%ConDepVis == "VIS" .and. Rset(iRset)%class == 2) 
   end do    ! do iOut  = 1, nOuts
end do       ! do iRset = 1, nRsets

!--- Close the output files

do iOut = 1, nOuts
   inquire( output(iOut)%lun, opened=lexist)
   if ( lexist ) close( output(iOut)%lun )
end do ! do iOut  = 1, nOuts

!--- WE'RE DONE!!!

9999 CONTINUE

IF( nError /= NO_ERROR )THEN
  WRITE(*,*)TRIM(eMessage)
  WRITE(*,*)TRIM(eInform)
  WRITE(*,*)TRIM(eAction)
END IF

IF( ALLOCATED(output)         ) DEALLOCATE(output)
IF( ALLOCATED(TotCon)         ) DEALLOCATE(TotCon)
IF( ALLOCATED(AmbCon)         ) DEALLOCATE(AmbCon)
IF( ALLOCATED(con)            ) DEALLOCATE(con)
IF( ALLOCATED(dep)            ) DEALLOCATE(dep)
IF( ALLOCATED(drydep)         ) DEALLOCATE(drydep)
IF( ALLOCATED(wetdep)         ) DEALLOCATE(wetdep)
IF( ALLOCATED(Rset)           ) DEALLOCATE(Rset)
IF( ALLOCATED(srfFldVal)      ) DEALLOCATE(srfFldVal)
IF( ALLOCATED(lat)            ) DEALLOCATE(lat,lon)
IF( ALLOCATED(xRec)           ) DEALLOCATE(xRec,yRec)
IF( ALLOCATED(nRec)           ) DEALLOCATE(nRec)
IF( ALLOCATED(sum_val)        ) DEALLOCATE(sum_val)
IF( ALLOCATED(max_val)        ) DEALLOCATE(max_val)
IF( ALLOCATED(max_time)       ) DEALLOCATE(max_time)
IF( ALLOCATED(dBc_sum)        ) DEALLOCATE(dBc_sum)
IF( ALLOCATED(dBc_max)        ) DEALLOCATE(dBc_max)
IF( ALLOCATED(iHours)         ) DEALLOCATE(iHours)
IF( ALLOCATED(NatCond)        ) DEALLOCATE(NatCond)
IF( ALLOCATED(LargeRH)        ) DEALLOCATE(LargeRH)
IF( ALLOCATED(SmallRH)        ) DEALLOCATE(SmallRH)
IF( ALLOCATED(SeaSalt)        ) DEALLOCATE(SeaSalt)
IF( ALLOCATED(Bext_nat)       ) DEALLOCATE(Bext_nat)
IF( ALLOCATED(iCon)           ) DEALLOCATE(iCon)

!------ Close file and deallocate grid structure

INQUIRE(UNIT=24,OPENED=lopen,IOSTAT=ios)
IF( lopen )CLOSE(24)

DO iProj = 1, nProj
  irv = SAG_CloseID( cgrdI(iProj) )
  irv = SAG_RmvGrdStr( cgrdI(iProj) )
  IF( lAmb )THEN
    irv = SAG_CloseID( agrdI(iProj) )
    irv = SAG_RmvGrdStr( agrdI(iProj) )
  END IF
  irv = SAG_CloseID( dgrdI(iProj) )
  irv = SAG_RmvGrdStr( dgrdI(iProj) )
END DO

!-- Release memory
CALL ClearMemory()

!-- Clean up : Exit SCIPtool
IF ( lInit ) irv = SCIPExitTool()

write(*,*) "SciDOSpost finished normally."

END PROGRAM runSciDosPost
