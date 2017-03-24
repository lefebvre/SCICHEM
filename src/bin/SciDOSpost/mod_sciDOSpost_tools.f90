!*******************************************************************************
!$RCSfile$
!$Revision$
!$Date$
!*******************************************************************************

MODULE SciDosPostTools

  USE tooluser_fd
  USE Extract_fi
  USE SCIPtool
  USE GetTimes_fi
  USE scipuff_fi, ONLY: create
  USE MPI_fi, ONLY: useMPI,myid
  IMPLICIT NONE

  TYPE an_output
     character (len=3)              :: ConDepVis  ! output type: con, dep, vis
     character (len=128)            :: chemSpecies! chemical species name
     real                           :: exceeds    ! print all vals over this
     character (len=3)              :: sel        ! MAX or ALL points output
     integer                        :: rank       ! 1st or 4th or 8th high
     integer                        :: avgPeriods ! The number of hours to... 
     character (len=1)              :: stat       ! take the avg or max over...
     integer                        :: frequency  ! this number of hours.
     character (len=4)              :: outType    ! CSV POST PLOT XYZ XYXT
     character (len=PATH_MAXLENGTH) :: outFile    ! output filename
     integer                        :: lun        ! unit to write to
  end TYPE an_output

  TYPE(an_output), allocatable, dimension(:)  :: output ! index: iOut

  TYPE a_RecSet ! FIXME: add xRec,yRec,Lon,Lat to this type? what about con?
     integer                        :: class 
     character (len=20)             :: Name
     character (len=20)             :: CoordType
     character (len=PATH_MAXLENGTH) :: Filename
  end type a_RecSet

  TYPE(a_RecSet), allocatable, dimension(:)   :: Rset

! iRset nRset refers to the receptor set, in one file, either Class I or II
! iRec  nRec  refers to a receptor location within a receptor set
! iKind nKind refers to the chemical/aerosol species
! iRank       is the 1st highest high, 2nd highest high, etc.

! TotCon is the total concentration, AmbCon is the ambient concentration
! We'll store the difference (attributable to the source) in con().

  REAL, DIMENSION(:,:,:),  ALLOCATABLE :: TotCon    !index: iRset,iRec,ikind
  REAL, DIMENSION(:,:,:),  ALLOCATABLE :: AmbCon    !index: iRset,iRec,ikind
  REAL, DIMENSION(:,:,:),  ALLOCATABLE :: con       !index: iRset,iRec,ikind
  REAL, DIMENSION(:,:,:),  ALLOCATABLE :: dep       !index: iRset,iRec,ikind
  REAL, DIMENSION(:,:,:),  ALLOCATABLE :: wetdep    !index: iRset,iRec,ikind
  REAL, DIMENSION(:,:,:),  ALLOCATABLE :: drydep    !index: iRset,iRec,ikind
  REAL, DIMENSION(:,:,:,:),ALLOCATABLE :: srfFldVal !index: iRset,iRec,ikind,2
  REAL, DIMENSION(:,:),    ALLOCATABLE :: lat,lon   !index: iRset,iRec
  REAL, DIMENSION(:,:),    ALLOCATABLE :: xRec,yRec !index: iRset,iRec
  REAL, DIMENSION(:,:),    ALLOCATABLE :: Bext_nat  !index: iRset, Month
  REAL, DIMENSION(:,:),    ALLOCATABLE :: NatCond, LargeRH, SmallRH, SeaSalt
  REAL, DIMENSION(:,:,:,:),ALLOCATABLE :: max_val   !index: iRset,iRec,iOut,iRank
  REAL, DIMENSION(:,:,:),  ALLOCATABLE :: sum_val   !index: iRset,iRec,iOut
  REAL, DIMENSION(:,:,:,:,:),ALLOCATABLE::dBc_max   !index: iRset,iRec,iOut,iRank,i
  REAL, DIMENSION(:,:,:,:),ALLOCATABLE :: dBc_sum   !index: iRset,iRec,iOut,i
  INTEGER, PARAMETER                   :: nCont = 9 !number of VIS components
  REAL, DIMENSION(nCont)               :: tmp7      !used for passing dBc_*

  INTEGER,DIMENSION(:,:,:,:),ALLOCATABLE :: max_time!index: same as max_val
  INTEGER,DIMENSION(:,:,:),ALLOCATABLE :: iymdh_max ! index: iRset,iRec,iOut
  INTEGER,DIMENSION(:,:),  ALLOCATABLE :: iHours    ! index: iRset,iOut
  INTEGER,DIMENSION(:),    ALLOCATABLE :: nRec      ! index: iRset
  INTEGER                              :: iymdh_beg = 0       ! begin output
  INTEGER                              :: iymdh_cur = 0       ! next (current) out
  INTEGER                              :: iymdh_end = HUGE(0) ! end output

! The iKind index of the species we need to calc max concentrations

  INTEGER, DIMENSION(:),   ALLOCATABLE :: iCon
  INTEGER  iO3, iPM25, iPM10  ! these get some special treatment

! The iKind index of the species we need to calc total S and N deposition

  INTEGER :: iNO, iNO2, iNO3, iN2O5, iHNO3, iHONO, iPNA, iPAN, iPANX, iNTR
  INTEGER :: iPNO3, iANO3K, iPNH4, iANH4K, iNH3
  INTEGER :: iSO2,  iSULF,  iPSO4, iASO4K

! The iKind index of the species we need to calculate visibility

  ! Already declared iPSO4, iASO4K, iPNO3, iANO3K above
  INTEGER :: iPSOA, iPOC, iPEC, iASOIL, iACORS

! The iKind index of the things we need to do the partitioning

  INTEGER :: iTemperature=1, iPressure=2, iHumidity=3

  INTEGER                           :: ProjCoord

! Other path/filenames we need

  INTEGER             :: nProj  ! number of projName (basename) filenames given
  INTEGER             :: iProj  ! index for projName 
  CHARACTER (len=PATH_MAXLENGTH), dimension(:), allocatable :: projName

  CHARACTER (len=PATH_MAXLENGTH)    :: controlFile, flagDir
  CHARACTER (len=PATH_MAXLENGTH)    :: NatCondFile, LargeRHFile
  CHARACTER (len=PATH_MAXLENGTH)    :: SmallRHFile, SeaSaltFile
  CHARACTER (len=128)               :: string1  ! tmp variable

  CHARACTER (len=1),   dimension(3) :: comment   = (/ "#", ";", "!" /)
  CHARACTER (len=1),   dimension(5) :: delim     = (/ " ", ",", "#", ";", "!" /)
  CHARACTER (len=1),   dimension(3) :: datedelim = (/ "-", "_", ":" /)

  LOGICAL                           :: calc_total_con = .false. ! default=plume
  LOGICAL                           :: debug = .false. ! print lots of debug info

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CONTAINS
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE read_control

    implicit none

    integer, parameter   :: lun = 100
    integer              :: iRset, iOut
    integer              :: ios, i, k
    integer              :: num_words
    integer              :: ibyr,ibmo,ibdy,ibhr, ieyr,iemo,iedy,iehr
    character (len=999)  :: line, word, word2

    iRset = 0 ! initialize
    iOut  = 0
    ibyr  = -9999 ! invalid default, so we can detect missing START keyword
    ieyr  = -9999 ! invalid default, so we can detect missing STOP  keyword

99  format(a999)

    write(*,*)
    write(*,'(3a,$)') " Parsing ",trim(controlfile),", "
    call flush(6)
    open(lun,file=controlFile,status='old',err=999)

    read(lun,99,iostat=ios) line
    call count_words(line,num_words)    ! read to first non-comment non-blank
    do while ((any(line(1:1) == comment) .or. num_words == 0) .and. ios == 0)
       read(lun,99,iostat=ios) line
       call count_words(line,num_words)
    end do

    do while (ios == 0)                 ! loop over lines in control file

       call get_word(line,1,word)
       call CUPPER( word )

       if ( word(1:4) == "PROJ" ) then
          do i = 1, nProj
             call get_word(line,i+1,projName(i))
          end do
       end if
       
       if ( word(1:4) == "CALC" ) then
          call get_word( line, 2, word2 )
          call CUPPER( word2 )
          if ( word2 == "PLUME") calc_total_con = .false.
          if ( word2 == "TOTAL") calc_total_con = .true.
       end if

       if (word == "START" .or. word == "BEGIN" .or. &
           word == "STOP"  .or. word == "END" ) then
!
!---- Special processing for dates to turn 2013-09-30_12 into 2013 09 30 12.
!     This allow WRF-style time-stamps (e.g. 2011-07-02_00:00:00).
!
          i = 1
          k = len_trim(line) ! like this_len
          do while (i < k)
             if (any(line(i:i) == comment)) k = i - 1
             if (any(line(i:i) == datedelim)) line(i:i) = " "
             i = i + 1
          end do
          call count_words(line,num_words) ! re-count them
       end if
!
!---- Start and Stop times
!
       if (word == "START" .or. word == "BEGIN") then
          if (num_words >= 5) then      ! like 'start 2008 07 04 01'
             call get_word(line,2,word2) ; read(word2,*,err=81) ibyr
             call get_word(line,3,word2) ; read(word2,*,err=81) ibmo
             call get_word(line,4,word2) ; read(word2,*,err=81) ibdy
             call get_word(line,5,word2) ; read(word2,*,err=81) ibhr
          else if (num_words == 2) then ! like 'start 2008070401'
             call get_word(line,2,word2)
             read(word2,'(i4,3i2)',err=81) ibyr,ibmo,ibdy,ibhr
          else
             write(*,*) "Error reading starting time-stamp: ",trim(line)
             stop
          end if
          call legal_timestamp(ibyr,ibmo,ibdy,ibhr,24)
          iymdh_beg = iymdh2idate(ibyr,ibmo,ibdy,ibhr)
          iymdh_cur = iymdh_beg ! set the current time to the start time
       end if

       if (word == "STOP" .or. word == "END") then
          if (num_words >= 5) then      ! like 'stop 2008 07 04 01'
             call get_word(line,2,word2) ; read(word2,*,err=82) ieyr
             call get_word(line,3,word2) ; read(word2,*,err=82) iemo
             call get_word(line,4,word2) ; read(word2,*,err=82) iedy
             call get_word(line,5,word2) ; read(word2,*,err=82) iehr
          else if (num_words == 2) then ! like 'stop 2008070401'
             call get_word(line,2,word2)
             read(word2,'(i4,3i2)',err=82) ieyr,iemo,iedy,iehr
          else
             write(*,*) "Error reading ending time-stamp: ",trim(line)
             stop
          end if
          call legal_timestamp(ieyr,iemo,iedy,iehr,23)
          iymdh_end = iymdh2idate(ieyr,iemo,iedy,iehr)
       end if
!
!---- Receptor Sets
!
       if ( word(1:3) == "REC" ) then
          iRset = iRset + 1
          call get_word( line, 2, word2 )
          if (word2(1:2) == "I ") then
             Rset(iRset)%class = 1
          elseif (word2(1:2) == "II") then
             Rset(iRset)%class = 2
          else
             read(word2,*) Rset(iRset)%class ! FIXME: handle read errors
          end if
          call get_word( line, 3, Rset(iRset)%Name )
          call CLOWER( Rset(iRset)%Name ) ! the lookup tables are in lower case
          call get_word( line, 4, Rset(iRset)%CoordType )
          call CUPPER( Rset(iRset)%CoordType )
          if ( Rset(iRset)%CoordType == "LL" ) Rset(iRset)%CoordType = "LATLON"
          call get_word( line, 5, Rset(iRset)%Filename )
!          call ADDPATH( class1Filename(iRset),class1AreaDir) ! not relevant
       end if
!
!---- Outputs
!
       if (word(1:3) == "CON" .or. word(1:3) == "DEP" .or.    &
           word(1:3) == "DRY" .or. word(1:3) == "WET" .or.    &
           word(1:3) == "VIS") then
          iOut = iOut + 1

          output(iOut)%ConDepVis = word(1:3)

          call get_word( line, 2, output(iOut)%chemSpecies ) ! 2nd word = species
          call CUPPER(output(iOut)%chemSpecies)          ! must be uppercase
          if (output(iOut)%chemSpecies(1:4) == "CONT")  &
               output(iOut)%chemSpecies = "CONT"         ! trim any other chars
          if (output(iOut)%chemSpecies(1:5) == "DBEXT") &
               output(iOut)%chemSpecies = "DBEXT"        ! trim any other chars

          call get_word( line, 3, word2 )                ! 3rd word: max,all,>5
          call CUPPER( word2 )
          if (word2(1:1) == "A") then            ! The Max values at ALL points
             output(iOut)%sel = "ALL"
             output(iOut)%exceeds = HUGE(0.)     ! flag for write maximum
          else if (word2(1:1) == "M") then       ! The MAX value at MAX val point
             output(iOut)%sel = "MAX"
             output(iOut)%exceeds = HUGE(0.)     ! flag for write maximum
          else if (word2(1:1) == ">") then       ! greater than, which is the
             read(word2(2:),*) output(iOut)%exceeds ! same as >= val - tiny()
             output(iOut)%exceeds = output(iOut)%exceeds - TINY(0.0)
          else if (word2(1:2) == ">=") then      ! greater than or equal to
             read(word2(3:),*) output(iOut)%exceeds
          else
             write(*,*) "Error parsing 3rd word of the following line:"
             write(*,*) trim(line)
             write(*,*) "3rd word must be max, all (same as >0), or >5, etc."
             stop
          end if

          call get_word( line, 4, word2 )                ! 4th word is rank
          read(word2(1:1),*) output(iOut)%rank

          call get_word( line, 5, word2 )                ! 5th word is period
          read(word2,*) output(iOut)%avgPeriods
          call get_word( line, 6, word2 )                ! 6th word is hr,mo,yr
          call CUPPER( word2 )
          if ( word2 == 'YR' ) then
             if ( leap_yr(ibyr) ) then                 ! number of hours per yr
                output(iOut)%avgPeriods = output(iOut)%avgPeriods * 8784
             else
                output(iOut)%avgPeriods = output(iOut)%avgPeriods * 8760
             end if
          end if

          call get_word( line, 7, word2 )                ! 7th word is AVG or MAX
          call CUPPER( word2 )
          output(iOut)%stat = word2(1:1)                ! really just A or M

          call get_word( line, 8, word2 )                ! 8th word is frequency
          read(word2,*) output(iOut)%frequency
          call get_word( line, 9, word2 )                ! 9th word is hr,mo,yr
          call CUPPER( word2 )
          if (word2 == 'YR') then
             if ( leap_yr(ibyr) ) then                 ! number of hours per yr
                output(iOut)%frequency = output(iOut)%frequency * 8784
             else
                output(iOut)%frequency = output(iOut)%frequency * 8760
             end if
          end if

          call get_word( line,10, output(iOut)%outType )! 11th is CSV, POST, etc

          call get_word( line,11, output(iOut)%outFile )! 10th word is filename
       end if

       read(lun,99,iostat=ios) line
       call count_words(line,num_words)     ! non-blank line
       do while (num_words == 0 .and. ios == 0)
          read(lun,99,iostat=ios) line
          call count_words(line,num_words)
       end do

    end do ! do while (ios == 0) ! loop over lines in control file

    write(*,'(i3,a)') iOut," outputs to be written."
    write(*,*) ! blank line to make output look nice

    close(lun)
    return

81  nError = UK_ERROR
    write(eMessage,*) "Error reading START time in control file "//trim(controlFile)

82  nError = UK_ERROR
    write(eMessage,*) "Error reading END time in control file "//trim(controlFile)


999 nError = UK_ERROR
    write(eMessage,*) "Error opening control file "//trim(controlFile)

  END SUBROUTINE read_control
!
!*************************************************************************
!
  SUBROUTINE control_counts(nRsets,nOuts)

    implicit none

    character (len=999) :: line, word
    integer             :: nRsets, nOuts, ios, num_words
    integer, parameter  :: lun = 100

    open(lun,file=controlFile,status='old',err=999)

    nRsets  = 0 ! initialize
    nOuts   = 0
    nProj   = 0

99  format(a999)

    read(lun,99,iostat=ios) line
    call count_words(line,num_words)    ! read to first non-comment non-blank
    do while ((any(line(1:1) == comment) .or. num_words == 0) .and. ios == 0)
       read(lun,99,iostat=ios) line
       call count_words(line,num_words)
    end do

    do while (ios == 0)

       call get_word(line,1,word)
       call CUPPER( word )

       if (word == "REC") nRsets = nRsets + 1
       if (word == "CON" .or. word == "DEP" .or. word == "DRY" .or. &
           word == "WET" .or. word == "VIS") &
            nOuts = nOuts + 1
       if (word(1:4) == "PROJ") then
         call count_words(line,num_words)     ! non-blank line
         nProj = num_words - 1
       end if
         
       read(lun,99,iostat=ios) line ! read to the next non-comment,
       call count_words(line,num_words)     ! non-blank line
       do while (num_words == 0 .and. ios == 0)
          read(lun,99,iostat=ios) line
          call count_words(line,num_words)
       end do

    end do

    close(lun)
    return

999 nError = UK_ERROR
    write(eMessage,*) "Error opening control file "//trim(controlFile)

  END SUBROUTINE control_counts
!
!*************************************************************************
!
  SUBROUTINE get_word(line,which_word,word)

    IMPLICIT NONE

    character (len=*) :: line       ! input string
    character (len=*) :: word       ! output string
    integer           :: which_word ! which word to return

    integer           :: i,j, this_word, this_len  ! local variables
!
!-----Entry point
!
    i = 1
    this_len = len_trim(line)
    do while (i < this_len)
       if (any(line(i:i) == comment)) this_len = i - 1 ! ignore commented parts
       i = i + 1
    end do

    this_word = 0
    i = 1

    do while (this_word < which_word)
       if (any(line(i:i) == delim)) then ! advance to next non-space
          do while (any(line(i:i) == delim) .and. i < this_len)
             i = i + 1
          end do
       end if                           ! line(i:i) now the beginnig of next word
       if (i < this_len) then
          j = i + 1                     ! next character
       else
          j = i                         ! this character
       end if

       if (line(i:i) == '"') then       ! advance to matching double quote
          do while (line(j:j) /= '"' .and. j < this_len)
             j = j + 1                  ! advance to end of this word (next ")
          end do
       else if (line(i:i) == "'") then  ! advance to matching single quote
          do while (line(j:j) /= "'" .and. j < this_len)
             j = j + 1                  ! advance to end of this word (next ')
          end do
       else                             ! word doesn't start with a quote
          do while ((.not. any(line(j:j) == delim)) .and. j < this_len)
             j = j + 1                  ! advance to end of this word
          end do
          if (j > i .and. any(line(j:j) == delim)) j = j - 1
       end if
       this_word = this_word + 1
       if (this_word == which_word) then
          word = trim(line(i:j))
          if (word(1:1) == '"' .or. word(1:1) == "'") then
             word = word(2:(len_trim(word)-1)) ! remove the quotes
          end if
          return                        ! done, got the right word, exit
       else if (j == this_len) then
          write(*,*) "*** Error: Asked for word ",which_word,", but only ", &
               this_word," words found."
          write(*,*) "           Problematic line is:"
          write(*,*) trim(line)
          nError = UK_ERROR
          WRITE(eMessage,*)"*** Error: Asked for word ",which_word,", but only ", &
               this_word," words found."
          WRITE(eInform,*) "           Problematic line is:"//TRIM(line)

          RETURN
       end if
       i = j + 1                        ! start at next character
    end do

  END SUBROUTINE get_word
!
!*************************************************************************
!
  SUBROUTINE get_csv( line, which_word, word )

    implicit none
    character (len=*) :: line        ! input string
    character (len=*) :: word        ! output string
    character (len=1) :: comma = "," ! delimiter between words
    integer           :: which_word  ! which word to return
    integer           :: i,j, this_word, this_len  ! local variables
!
!-----Entry point
!
    this_len = len_trim(line)
    this_word = 0
    i = 1

    do while (this_word < which_word)
       if (line(i:i) == comma) then     ! advance to next non-comma
          do while (line(i:i) == comma .and. i < this_len)
             i = i + 1
          end do
       end if                           ! line(i:i) now the beginnig of next word
       if (i < this_len) then
          j = i + 1                     ! next character
       else
          j = i                         ! this character
       end if

       if (line(i:i) == '"') then       ! advance to matching double quote
          do while (line(j:j) /= '"' .and. j < this_len)
             j = j + 1                  ! advance to end of this word (next ")
          end do
       else if (line(i:i) == "'") then  ! advance to matching single quote
          do while (line(j:j) /= "'" .and. j < this_len)
             j = j + 1                  ! advance to end of this word (next ')
          end do
       else                             ! word doesn't start with a quote
          do while ((.not. line(j:j) == comma) .and. j < this_len)
             j = j + 1                  ! advance to end of this word
          end do
          if (j > i .and. line(j:j) == comma) j = j - 1
       end if
       this_word = this_word + 1
       if (this_word == which_word) then
          word = trim(line(i:j))
          if (word(1:1) == '"' .or. word(1:1) == "'") then
             word = word(2:(len_trim(word)-1)) ! remove the quotes
          end if
          return                        ! done, got the right word, exit
       else if (j == this_len) then
          write(*,*) "*** Error: Asked for word ",which_word,", but only ", &
               this_word," words found."
          write(*,*) "           Problematic line is:"
          write(*,*) trim(line)
          stop
       end if
       i = j + 1                        ! start at next character
    end do

  END SUBROUTINE get_csv
!
!*************************************************************************
!
  SUBROUTINE count_words(line,num_words)
!
!------------------------------------------------------------------------------
!     MESOSCALE MODEL INTERFACE PROGRAM (MMIF)
!     VERSION 3.0 2013-09-30
!
!     This subroutine counts the number of words in a line.
!
!     Development History:
!     2013-09-05  Original Development (ENVIRON International Corp.)

    implicit none

    character (len=*) :: line       ! input string
    integer           :: num_words  ! the number of words found

    integer           :: i,j,this_len
!
!-----Entry point
!
    i = 1
    this_len = len_trim(line)
    do while (i < this_len)
       if (any(line(i:i) == comment)) this_len = i - 1 ! ignore commented parts
       i = i + 1
    end do

    num_words = 0
    i = 1

    do while (i < this_len)
       if (any(line(i:i) == delim)) then ! advance to next non-space
          do while (any(line(i:i) == delim) .and. i < this_len)
             i = i + 1
          end do
          ! what if there's a delimiter at the end of the line?
          if (i == this_len .and. any(line(i:i) == delim) ) return
       end if                           ! line(i:i) now the beginnig of nextword

       j = i + 1                        ! next character
       if (line(i:i) == '"') then       ! advance to matching double quote
          do while (line(j:j) /= '"' .and. j < this_len)
             j = j + 1                  ! advance to end of this word (next ")
          end do
       else if (line(i:i) == "'") then  ! advance to matching single quote
          do while (line(j:j) /= '"' .and. j < this_len)
             j = j + 1                  ! advance to end of this word (next ')
          end do
       else                             ! word doesn't start with a quote
          do while ((.not. any(line(j:j) == delim)) .and. j < this_len)
             j = j + 1                  ! advance to end of this word
          end do
       end if
       num_words = num_words + 1
       i = j                            ! start at next character
    end do

  END SUBROUTINE count_words
!
!*************************************************************************
!
  SUBROUTINE usage

    write(*,*) "Usage: SciDosPost [-h | --help] [--sample] [-I:scpuff.ini] -i ctrl.inp"
    write(*,*) "Options:"
    write(*,*) "   -i file    Specify control file to use, SciDosPost.inp by default"
    write(*,*) "   -I:file    Specify the location of scipuff.ini to use"
    write(*,*) "   --sample   Print a sample control file to the screen"
    write(*,*) "   --help     Print this help message"
    write(*,*) "   -h         Print this help message"

    stop

  END SUBROUTINE usage
!
!*************************************************************************
!
  SUBROUTINE sample_control_file

1   format(a)

    write(*,1) "; This file can be space-delimited or comma-delimited, or a mixture."
    write(*,1) "; Comment characters are #, ;, and !.  Blank lines are ignored."
    write(*,1) "; Keywords are not case-sensitive.  Filenames are used verbatim, and can"
    write(*,1) "; include spaces if enclosed in quotes."
    write(*,1) 
    write(*,1) "# The keyword PROJECT gives the SCICHEM project (basename)"
    write(*,1) 
    write(*,1) "Project fc_egu1_07s3ab"
    write(*,1) 
    write(*,1) "# The keyword CALCulate specifies whether to include background concentrations"
    write(*,1) 
    write(*,1) "calculate plume  ! plume (default) or total = ambient + plume"
    write(*,1) 
    write(*,1) "# Time to process"
    write(*,1) 
    write(*,1) "Start 2010-01-01:00   ! any of these forms work, also 2010/01/01 00"
    write(*,1) "stop  2010123123      ! Omit Start and Stop to process ALL data found."
    write(*,1) 
    write(*,1) "# The repeatable keyword RECeptors specifies the Receptor Sets to use."
    write(*,1) "# Files can be the Class I receptor files downloaded from"
    write(*,1) "#   http://www2.nature.nps.gov/air/maps/Receptors/index.cfm"
    write(*,1) "# or contain user-specified receptors, one UTMx,UTMy, or LON,LAT pair per line."
    write(*,1) "# 2nd word is 1 if the receptor set is Class I, 2 for Class II."
    write(*,1) "# 3rd word is a user-specified NAME.  The NAME must match the first 4 characters"
    write(*,1) "# in the FLAG (2010) look-up tables for it to be treated as a Class I area, and"
    write(*,1) "# visilbity obscuration calculated."
    write(*,1) "# 4th word is either LATLON (syn: LL) or anything else for Cartesian"
    write(*,1) "# (including UTM). Must match coordinate system of the SCICHEM run."
    write(*,1) 
    write(*,1) "REC 1  meve   LatLon /home/data/ClassI/meve-recep.dat   ! Mesa Verde National Park"
    write(*,1) "REC 1  wemi   LL     /home/data/ClassI/wemi2-recep.dat  ! Weminuche Wilderness"
    write(*,1) "REC 2  fence  UTM    prop_boundary.rec"
    write(*,1) "rec 2  50m    utm    50m.rec"
    write(*,1) 
    write(*,1) "# OUTPUT section: "
    write(*,1) "# 1st word is CONcentration, DEPosition (WET or DRY) or VISibility"
    write(*,1) "# 2nd word is the chemical species to process"
    write(*,1) "# 3rd word is a selection criteria: "
    write(*,1) "#    ALL: output the max value (of rank) at ALL points in each receptor set"
    write(*,1) "#         Note: ALL means 'all values in space' here, not 'in time'"
    write(*,1) "#    MAX: output the single max value (of rank) within each receptor set"
    write(*,1) "#    >=N: output all values (of rank) that exceeds the specified value"
    write(*,1) "#         To get a time series, specify just one REC and use '>=0'"
    write(*,1) "# 4th word is the rank, e.g. the 8th highest high (98th percentile)"
    write(*,1) "# 5th and 6th words specify the period over which to average or take the maximum"
    write(*,1) "# 7th word is either MAX or AVG, the statistic to take over each FREQUENCY"
    write(*,1) "# 8th and 9th words specify the frequency, how often to take the MAX or AVG"
    write(*,1) "# 10th word is the output file type, CSV, XZY (data), or AERMOD-style POSTFILE"
    write(*,1) "# 11th word is the filename, can contain spaces if enclosed in quotes"
    write(*,1) "# "
    write(*,1) "# All RECeptor sets are written to each output, so XYZ and PLOT/POST files"
    write(*,1) "# should be run with only one REC line above, or you'll get a mashup."
    write(*,1) "# "
    write(*,1) "# Visibility obscuration results can only be calculated for Class I"
    write(*,1) "# receptor sets.  They will be silently skipped for other REC sets."
    write(*,1) "# The FLAG(2010) method probably only makes sense when using CALC PLUME above."
    write(*,1) "# "
    write(*,1) "#    SPEC  SEL RANK PERIOD STAT   FREQ  TYPE  FILENAME"
    write(*,1) "con  no2   all  8th   1 hr max_in 24 hr XYZ   no2.DV.max    ! x y value"
    write(*,1) "con  no2   max  8th   1 hr max_in 24 hr PLOT  no2.DV.plt    ! AERMOD PLOTFILE"
    write(*,1) "con  no2   >-1  1st   1 hr avg_in  1 hr POST  no2.1hr.pst   ! AERMOD POSTFILE"
    write(*,1) "con  no2   max  8th   1 hr max_in 24 hr CSV   max_concentrations.csv"
    write(*,1) "con  no2   max  1st   1 yr avg_in  1 yr CSV   max_concentrations.csv"
    write(*,1) "con  pm10  max  1st  24 hr avg_in 24 hr CSV   max_concentrations.csv"
    write(*,1) "con  pm10  max  1st   1 yr avg_in  1 yr CSV   max_concentrations.csv"
    write(*,1) "con  pm25  all  1st  24 hr avg_in 24 hr XYZ   pm25.DV.xyz.dat"
    write(*,1) "con  pm25  max  1st  24 hr avg_in 24 hr CSV   max_concentrations.csv"
    write(*,1) "con  pm25  max  1st   1 yr avg_in  1 yr CSV   max_concentrations.csv"
    write(*,1) "con  so2   max  4th   1 hr max_in 24 hr CSV   max_concentrations.csv"
    write(*,1) "con  so2   max  2nd   3 hr avg_in  3 hr CSV   max_concentrations.csv"
    write(*,1) "con  so2   max  1st  24 hr avg_in 24 hr CSV   max_concentrations.csv"
    write(*,1) "con  so2   max  1st   1 yr avg_in  1 yr CSV   max_concentrations.csv"
    write(*,1) "dep  N     max  1st   1 yr avg_in  1 yr CSV   max_deposition.csv    "
    write(*,1) "dry  N     max  1st   1 yr avg_in  1 yr CSV   max_deposition.csv    "
    write(*,1) "wet  N     max  1st   1 yr avg_in  1 yr CSV   max_deposition.csv    "
    write(*,1) "dep  S     max  1st   1 yr avg_in  1 yr CSV   max_deposition.csv    "
    write(*,1) "vis  dBext max  8th  24 hr avg_in 24 hr CSV   visibility_results.csv"
    write(*,1) "vis  Cont  max  8th  24 hr avg_in 24 hr CSV   visibility_contrib.csv"
    write(*,1) "vis  dBext max  1st  24 hr avg_in 24 hr CSV   visibility_daily.csv  "
    write(*,1) "vis  dBext >5   1st  24 hr avg_in 24 hr CSV   visibility_exceedences.csv"
    write(*,1) "vis  dBext >10  1st  24 hr avg_in 24 hr CSV   visibility_exceedences.csv"

    stop


  END SUBROUTINE sample_control_file
!
!*************************************************************************
!
  SUBROUTINE calc_con( iRset, iKind, iOut, iymdh)

    IMPLICIT NONE
    integer                 :: iRset  ! which receptor set to process
    integer                 :: iKind  ! which chemical species in con() to use
    integer                 :: iOut   ! which output we're working on
    integer                 :: ir     ! rank of this average (H1H, H8H, etc.)
    integer                 :: iRec   ! receptor index within this receptor set
    integer                 :: iymdh  ! current time in YYYYMMDDHH format


!--- add this hour to the running average

    do iRec = 1, nRec(iRset)
       if ( iymdh_max(iRset,iRec,iOut) == 0) iymdh_max(iRset,iRec,iOut) = iymdh

       call max_or_avg(sum_val(iRset,iRec,iOut), con(iRset,iRec,iKind), & 
            output(iOut)%stat, iymdh, iymdh_max(iRset,iRec,iOut) )
    end do

    iHours(iRset,iOut) = iHours(iRset,iOut) + 1
    if (debug) write(24,*)'iHours for iRset,iOut ',iRset,iOut,' = ',iHours(iRset,iOut)

!--- Find the n-period average value, then find the rank of this value,
!    and insert the value at the right rank

    if (debug) print*,"iRset,iOut,iHours,sum_val= ",iRset,iOut,iHours(iRset,iOut),&
       sum_val(iRset,1,iOut)

    if (iHours(iRset,iOut) == output(iOut)%frequency) then ! end frequency period
       if (debug) then
         write(24,*)'Output for iRset,iOut ',iRset,iOut,' at ',iHours(iRset,iOut)
         write(24,*)"Averging for output(iOut) ",iRset,iOut,iHours(iRset,iOut),output(iOut)%frequency,iymdh
       end if
       if (debug) then
          print*
          print*,"averging for output(iOut) ",iRset,iOut,iHours(iRset,iOut)
          print*,"sum_val = ",sum_val(iRset,1,iOut)
          print*,"avg_val = ",sum_val(iRset,1,iOut)/iHours(iRset,iOut)
          print*
       end if

       do iRec = 1, nRec(iRset)

          if ( output(iOut)%stat == "A" ) then ! average by div sum/hours
             sum_val(iRset,iRec,iOut) = &
                  sum_val(iRset,iRec,iOut)/iHours(iRset,iOut)
          end if ! if output(iOut)%stat == "M", we already have the maximum

          ir = find_rank( sum_val(iRset,iRec,iOut), iRset, iRec, iOut, &
               output(iOut)%rank)

          ! insert into max_val at right rank:

          call insert( sum_val(iRset,iRec,iOut), iRset, iRec, iOut,   &
               output(iOut)%rank, ir, iymdh_max(iRset,iRec,iOut) ) 

          if ( sum_val(iRset,iRec,iOut) >= output(iOut)%exceeds ) then

             if (debug) print*,"Output: ",iOut,iRset,iRec,iymdh, &
                  sum_val(iRset,iRec,iOut)
             write(string1,*) ">=",output(iOut)%exceeds
             call write_output(iOut, iRset, iRec, iymdh_max(iRset,iRec,iOut),  &
                  string1, sum_val(iRset,iRec,iOut))

          end if

       end do

       iHours(iRset,iOut) = 0
       sum_val(iRset,:,iOut) = 0.

    end if ! if (iHours(iRset,iOut) == output(iOut)%avgPeriods) then

    RETURN

  END SUBROUTINE calc_con
!
!*************************************************************************
!
  SUBROUTINE calc_dep( iRset, iKind, iOut, iymdh )

    IMPLICIT NONE
    integer                 :: iRset  ! which Class I area to process
    integer                 :: iKind  ! which chemical species in dep() to use
    integer                 :: iOut   ! which output we're working on
    integer                 :: ir     ! rank of this average (H1H, H8H, etc.)
    integer                 :: iRec   ! Class I area receptor index
    integer                 :: iymdh  ! current time in YYYYMMDDHH format
    real                    :: tmpDep ! Deposition rate for this hour

!--- add this hour to the running total deposition of N and S

    do iRec = 1, nRec(iRset)

       tmpDep = 0.

       if (output(iOut)%chemSpecies == "N") then ! Total Nitrogen Deposition

          if (iNO   > 0) tmpDep = tmpDep &
               + 14./30.  * dep(iRset,iRec,iNO   ) ! NO
          if (iNO2  > 0) tmpDep = tmpDep &
               + 14./46.  * dep(iRset,iRec,iNO2  ) ! NO2
          if (iNO3   > 0) tmpDep = tmpDep &
               + 14./62.  * dep(iRset,iRec,iNO3  ) ! NO3
          if (iN2O5  > 0) tmpDep = tmpDep &
               + 28./102. * dep(iRset,iRec,iN2O5 ) ! N2O5
          if (iHNO3  > 0) tmpDep = tmpDep &
               + 14./63.  * dep(iRset,iRec,iHNO3 ) ! HNO3
          if (iHONO  > 0) tmpDep = tmpDep &
               + 14./47.  * dep(iRset,iRec,iHONO ) ! HONO
          if (iPNA   > 0) tmpDep = tmpDep &
               + 14./79.  * dep(iRset,iRec,iPNA  ) ! HNO4
          if (iPAN   > 0) tmpDep = tmpDep &
               + 14./121. * dep(iRset,iRec,iPAN  ) ! CH3COOONO2
          if (iPANX  > 0) tmpDep = tmpDep &
               + 14./121. * dep(iRset,iRec,iPANX ) ! Higher PAN
          if (iNTR   > 0) tmpDep = tmpDep &
               + 14./130. * dep(iRset,iRec,iNTR  ) ! Org. Nitr
          if (iPNO3  > 0) tmpDep = tmpDep &
               + 28./62.  * dep(iRset,iRec,iPNO3 ) ! NH4NO3
          if (iANO3K > 0) tmpDep = tmpDep &
               + 28./62.  * dep(iRset,iRec,iANO3K) ! NH4NO3
          if (iNH3   > 0) tmpDep = tmpDep &
               + 14./17.  * dep(iRset,iRec,iNH3  ) ! ammonia
          if (iPNH4  > 0) tmpDep = tmpDep &
               + 14./18.  * dep(iRset,iRec,iPNH4 ) ! PM Ammonia
          if (iANH4K > 0) tmpDep = tmpDep &
               + 14./18.  * dep(iRset,iRec,iANH4K) ! PM Ammonia

       elseif (output(iOut)%chemSpecies == "S") then ! Total Sulfur Dep

          if (iSO2   > 0) tmpDep = tmpDep &
               + 32./64.  * dep(iRset,iRec,iSO2  )   ! SO2
          if (iSULF  > 0) tmpDep = tmpDep &
               + 32./98.  * dep(iRset,iRec,iSULF )   ! H2SO4
          if (iPSO4  > 0) tmpDep = tmpDep &
               + 32./96.  * dep(iRset,iRec,iPSO4 )   ! (NH4)2SO4
          if (iASO4K > 0) tmpDep = tmpDep &
               + 32./96.  * dep(iRset,iRec,iASO4K)   ! (NH4)2SO4

       else ! use iKind
          
          tmpDep = tmpDep + dep(iRset,iRec,iKind)

       end if

       call max_or_avg(sum_val(iRset,iRec,iOut), tmpDep, output(iOut)%stat, &
            iymdh, iymdh_max(iRset,iRec,iOut) )

    end do  ! do iRec = 1, nRec(iRset)

    iHours(iRset,iOut) = iHours(iRset,iOut) + 1
    if (debug) WRITE(24,*)'Dep iHours(iRset,iOut) ',iHours(iRset,iOut)

!--- Find the n-period average value, then find the rank of this value,
!    and insert the value at the right rank

    if (iHours(iRset,iOut) == output(iOut)%avgPeriods) then ! end averaging period

       do iRec = 1, nRec(iRset)

          if ( output(iOut)%stat == "A" ) then ! average by div sum/hours
             sum_val(iRset,iRec,iOut) = &
                  sum_val(iRset,iRec,iOut)/iHours(iRset,iOut)
          end if ! if output(iOut)%stat == "M", we already have the maximum

          ir = find_rank( sum_val(iRset,iRec,iOut), iRset, iRec, iOut, &
               output(iOut)%rank)

          ! inserts into max_val at right rank, with the right time
          call insert( sum_val(iRset,iRec,iOut), iRset, iRec, iOut,   &
               output(iOut)%rank, ir, iymdh_max(iRset,iRec,iOut) ) 

          if ( sum_val(iRset,iRec,iOut) >= output(iOut)%exceeds ) then

             write(string1,*) ">=",output(iOut)%exceeds
             call write_output(iOut, iRset, iRec, iymdh_max(iRset,iRec,iOut),  &
                  string1, sum_val(iRset,iRec,iOut))

          end if

       end do

       iHours(iRset,iOut) = 0
       sum_val(iRset,:,iOut) = 0.

    end if ! if (iHours(iRset,iOut) == output(iOut)%avgPeriods) then

   RETURN

  END SUBROUTINE calc_dep
!
!*************************************************************************
!
  SUBROUTINE calc_dry_dep( iRset, iKind, iOut, iymdh )

    IMPLICIT NONE
    integer                 :: iRset  ! which Class I area to process
    integer                 :: iKind  ! which chemical species in dep() to use
    integer                 :: iOut   ! which output we're working on
    integer                 :: ir     ! rank of this average (H1H, H8H, etc.)
    integer                 :: iRec   ! Class I area receptor index
    integer                 :: iymdh  ! current time in YYYYMMDDHH format
    real                    :: tmpDep ! Deposition rate for this hour

!--- add this hour to the running total deposition of N and S

    do iRec = 1, nRec(iRset)

       tmpDep = 0.

       if (output(iOut)%chemSpecies == "N") then ! Total Nitrogen Deposition

          if (iNO   > 0) tmpDep = tmpDep &
               + 14./30.  * drydep(iRset,iRec,iNO   ) ! NO
          if (iNO2  > 0) tmpDep = tmpDep &
               + 14./46.  * drydep(iRset,iRec,iNO2  ) ! NO2
          if (iNO3   > 0) tmpDep = tmpDep &
               + 14./62.  * drydep(iRset,iRec,iNO3  ) ! NO3
          if (iN2O5  > 0) tmpDep = tmpDep &
               + 28./102. * drydep(iRset,iRec,iN2O5 ) ! N2O5
          if (iHNO3  > 0) tmpDep = tmpDep &
               + 14./63.  * drydep(iRset,iRec,iHNO3 ) ! HNO3
          if (iHONO  > 0) tmpDep = tmpDep &
               + 14./47.  * drydep(iRset,iRec,iHONO ) ! HONO
          if (iPNA   > 0) tmpDep = tmpDep &
               + 14./79.  * drydep(iRset,iRec,iPNA  ) ! HNO4
          if (iPAN   > 0) tmpDep = tmpDep &
               + 14./121. * drydep(iRset,iRec,iPAN  ) ! CH3COOONO2
          if (iPANX  > 0) tmpDep = tmpDep &
               + 14./121. * drydep(iRset,iRec,iPANX ) ! Higher PAN
          if (iNTR   > 0) tmpDep = tmpDep &
               + 14./130. * drydep(iRset,iRec,iNTR  ) ! Org. Nitr
          if (iPNO3  > 0) tmpDep = tmpDep &
               + 28./62.  * drydep(iRset,iRec,iPNO3 ) ! NH4NO3
          if (iANO3K > 0) tmpDep = tmpDep &
               + 28./62.  * drydep(iRset,iRec,iANO3K) ! NH4NO3
          if (iNH3   > 0) tmpDep = tmpDep &
               + 14./17.  * drydep(iRset,iRec,iNH3  ) ! ammonia
          if (iPNH4  > 0) tmpDep = tmpDep &
               + 14./18.  * drydep(iRset,iRec,iPNH4 ) ! PM Ammonia
          if (iANH4K > 0) tmpDep = tmpDep &
               + 14./18.  * drydep(iRset,iRec,iANH4K) ! PM Ammonia

       elseif (output(iOut)%chemSpecies == "S") then ! Total Sulfur Dep

          if (iSO2   > 0) tmpDep = tmpDep &
               + 32./64.  * drydep(iRset,iRec,iSO2  )   ! SO2
          if (iSULF  > 0) tmpDep = tmpDep &
               + 32./98.  * drydep(iRset,iRec,iSULF )   ! H2SO4
          if (iPSO4  > 0) tmpDep = tmpDep &
               + 32./96.  * drydep(iRset,iRec,iPSO4 )   ! (NH4)2SO4
          if (iASO4K > 0) tmpDep = tmpDep &
               + 32./96.  * drydep(iRset,iRec,iASO4K)   ! (NH4)2SO4

       else ! use iKind
          
          tmpDep = tmpDep + drydep(iRset,iRec,iKind)

       end if

       call max_or_avg(sum_val(iRset,iRec,iOut), tmpDep, output(iOut)%stat, &
            iymdh, iymdh_max(iRset,iRec,iOut) )

    end do  ! do iRec = 1, nRec(iRset)

    iHours(iRset,iOut) = iHours(iRset,iOut) + 1

!--- Find the n-period average value, then find the rank of this value,
!    and insert the value at the right rank

    if (iHours(iRset,iOut) == output(iOut)%avgPeriods) then ! end averaging period

       do iRec = 1, nRec(iRset)

          if ( output(iOut)%stat == "A" ) then ! average by div sum/hours
             sum_val(iRset,iRec,iOut) = &
                  sum_val(iRset,iRec,iOut)/iHours(iRset,iOut)
          end if ! if output(iOut)%stat == "M", we already have the maximum

          ir = find_rank( sum_val(iRset,iRec,iOut), iRset, iRec, iOut, &
               output(iOut)%rank)

          ! inserts into max_val at right rank, with the right time
          call insert( sum_val(iRset,iRec,iOut), iRset, iRec, iOut,   &
               output(iOut)%rank, ir, iymdh_max(iRset,iRec,iOut) ) 

          if ( sum_val(iRset,iRec,iOut) >= output(iOut)%exceeds ) then

             write(string1,*) ">=",output(iOut)%exceeds
             call write_output(iOut, iRset, iRec, iymdh_max(iRset,iRec,iOut),  &
                  string1, sum_val(iRset,iRec,iOut))

          end if

       end do

       iHours(iRset,iOut) = 0
       sum_val(iRset,:,iOut) = 0.

    end if ! if (iHours(iRset,iOut) == output(iOut)%avgPeriods) then

   RETURN

  END SUBROUTINE calc_dry_dep
!
!*************************************************************************
!
  SUBROUTINE calc_wet_dep( iRset, iKind, iOut, iymdh )

    IMPLICIT NONE
    integer                 :: iRset  ! which Class I area to process
    integer                 :: iKind  ! which chemical species in dep() to use
    integer                 :: iOut   ! which output we're working on
    integer                 :: ir     ! rank of this average (H1H, H8H, etc.)
    integer                 :: iRec   ! Class I area receptor index
    integer                 :: iymdh  ! current time in YYYYMMDDHH format
    real                    :: tmpDep ! Deposition rate for this hour

!--- add this hour to the running total deposition of N and S

    do iRec = 1, nRec(iRset)

       tmpDep = 0.

       if (output(iOut)%chemSpecies == "N") then ! Total Nitrogen Deposition

          if (iNO   > 0) tmpDep = tmpDep &
               + 14./30.  * wetdep(iRset,iRec,iNO   ) ! NO
          if (iNO2  > 0) tmpDep = tmpDep &
               + 14./46.  * wetdep(iRset,iRec,iNO2  ) ! NO2
          if (iNO3   > 0) tmpDep = tmpDep &
               + 14./62.  * wetdep(iRset,iRec,iNO3  ) ! NO3
          if (iN2O5  > 0) tmpDep = tmpDep &
               + 28./102. * wetdep(iRset,iRec,iN2O5 ) ! N2O5
          if (iHNO3  > 0) tmpDep = tmpDep &
               + 14./63.  * wetdep(iRset,iRec,iHNO3 ) ! HNO3
          if (iHONO  > 0) tmpDep = tmpDep &
               + 14./47.  * wetdep(iRset,iRec,iHONO ) ! HONO
          if (iPNA   > 0) tmpDep = tmpDep &
               + 14./79.  * wetdep(iRset,iRec,iPNA  ) ! HNO4
          if (iPAN   > 0) tmpDep = tmpDep &
               + 14./121. * wetdep(iRset,iRec,iPAN  ) ! CH3COOONO2
          if (iPANX  > 0) tmpDep = tmpDep &
               + 14./121. * wetdep(iRset,iRec,iPANX ) ! Higher PAN
          if (iNTR   > 0) tmpDep = tmpDep &
               + 14./130. * wetdep(iRset,iRec,iNTR  ) ! Org. Nitr
          if (iPNO3  > 0) tmpDep = tmpDep &
               + 28./62.  * wetdep(iRset,iRec,iPNO3 ) ! NH4NO3
          if (iANO3K > 0) tmpDep = tmpDep &
               + 28./62.  * wetdep(iRset,iRec,iANO3K) ! NH4NO3
          if (iNH3   > 0) tmpDep = tmpDep &
               + 14./17.  * wetdep(iRset,iRec,iNH3  ) ! ammonia
          if (iPNH4  > 0) tmpDep = tmpDep &
               + 14./18.  * wetdep(iRset,iRec,iPNH4 ) ! PM Ammonia
          if (iANH4K > 0) tmpDep = tmpDep &
               + 14./18.  * wetdep(iRset,iRec,iANH4K) ! PM Ammonia

       elseif (output(iOut)%chemSpecies == "S") then ! Total Sulfur Dep

          if (iSO2   > 0) tmpDep = tmpDep &
               + 32./64.  * wetdep(iRset,iRec,iSO2  )   ! SO2
          if (iSULF  > 0) tmpDep = tmpDep &
               + 32./98.  * wetdep(iRset,iRec,iSULF )   ! H2SO4
          if (iPSO4  > 0) tmpDep = tmpDep &
               + 32./96.  * wetdep(iRset,iRec,iPSO4 )   ! (NH4)2SO4
          if (iASO4K > 0) tmpDep = tmpDep &
               + 32./96.  * wetdep(iRset,iRec,iASO4K)   ! (NH4)2SO4

       else ! use iKind
          
          tmpDep = tmpDep + wetdep(iRset,iRec,iKind)

       end if

       call max_or_avg(sum_val(iRset,iRec,iOut), tmpDep, output(iOut)%stat, &
            iymdh, iymdh_max(iRset,iRec,iOut) )

    end do  ! do iRec = 1, nRec(iRset)

    iHours(iRset,iOut) = iHours(iRset,iOut) + 1

!--- Find the n-period average value, then find the rank of this value,
!    and insert the value at the right rank

    if (iHours(iRset,iOut) == output(iOut)%avgPeriods) then ! end averaging period

       do iRec = 1, nRec(iRset)

          if ( output(iOut)%stat == "A" ) then ! average by div sum/hours
             sum_val(iRset,iRec,iOut) = &
                  sum_val(iRset,iRec,iOut)/iHours(iRset,iOut)
          end if ! if output(iOut)%stat == "M", we already have the maximum

          ir = find_rank( sum_val(iRset,iRec,iOut), iRset, iRec, iOut, &
               output(iOut)%rank)

          ! inserts into max_val at right rank, with the right time
          call insert( sum_val(iRset,iRec,iOut), iRset, iRec, iOut,   &
               output(iOut)%rank, ir, iymdh_max(iRset,iRec,iOut) ) 

          if ( sum_val(iRset,iRec,iOut) >= output(iOut)%exceeds ) then

             write(string1,*) ">=",output(iOut)%exceeds
             call write_output(iOut, iRset, iRec, iymdh_max(iRset,iRec,iOut),  &
                  string1, sum_val(iRset,iRec,iOut))

          end if

       end do

       iHours(iRset,iOut) = 0
       sum_val(iRset,:,iOut) = 0.

    end if ! if (iHours(iRset,iOut) == output(iOut)%avgPeriods) then

   RETURN

  END SUBROUTINE calc_wet_dep
!
!*************************************************************************
!
  SUBROUTINE calc_vis( iRset, what, iOut, mo, iymdh )

    IMPLICIT NONE

    character (len=*)       :: what   ! what to write, dBext or contributions
    
    integer                 :: iRset  ! which Class I area to process
    integer                 :: iOut   ! which output we're working on
    integer                 :: mo     ! month of current time-step
    integer                 :: ir     ! rank of this average (H1H, H8H, etc.)
    integer                 :: iRec   ! Class I area receptor index
    integer                 :: iymdh  ! current time in YYYYMMDDHH format
    integer                 :: i

    real                    :: TotSulfate, LgSulfate, SmSulfate
    real                    :: TotNitrate, LgNitrate, SmNitrate
    real                    :: TotOrganic, LgOrganic, SmOrganic
    real                    :: bNO3, bSO4, bOCM, bSOIL, bPEC, bNO2, bCORS
    real                    :: Bext, dBext
    real                    :: con_PEC, con_ASOIL, con_ACORS, con_NO2

!--- Calculate this hour's visibility obscuration

    do iRec = 1, nRec(iRset)

!--- Calculate Total Sulfate, and Large Sulfate fraction, according to
!    Figure 5 from FLAG (2010).  Ditto for Nitate and Organic Mass.
!    If some component of these are not found in the DOS file, then set
!    the concentrations to zero, but continue with the calculation.

       TotSulfate = NatCond(iRset,1)
       if (iPSO4  > 0) TotSulfate = TotSulfate + con(iRset,iRec,iPSO4)
       if (iASO4K > 0) TotSulfate = TotSulfate + con(iRset,iRec,iASO4K)
       if (TotSulfate < 20.) then              ! 20 ug/m3
          LgSulfate = TotSulfate / 20. * TotSulfate
       else
          LgSulfate = TotSulfate
       end if
       SmSulfate = TotSulfate - LgSulfate

       TotNitrate = NatCond(iRset,2)
       if (iPNO3  > 0) TotNitrate = TotNitrate + con(iRset,iRec,iPNO3)
       if (iANO3K > 0) TotNitrate = TotNitrate + con(iRset,iRec,iANO3K)
       if (TotNitrate < 20.) then              ! 20 ug/m3
          LgNitrate = TotNitrate / 20. * TotNitrate
       else
          LgNitrate = TotNitrate
       end if
       SmNitrate = TotNitrate - LgNitrate

       TotOrganic = NatCond(iRset,3)
       if (iPSOA  > 0) TotOrganic = TotOrganic + con(iRset,iRec,iPSOA)
       if (iPOC   > 0) TotOrganic = TotOrganic + con(iRset,iRec,iPOC)
       if (TotOrganic < 20.) then              ! 20 ug/m3
          LgOrganic = TotOrganic / 20. * TotOrganic
       else
          LgOrganic = TotOrganic
       end if
       SmOrganic = TotOrganic - LgOrganic

       con_PEC   = 0.  ! If these values not found in this run, then set
       con_ASOIL = 0.  ! them to zero, but continue with the calculation.
       con_ACORS = 0.
       con_NO2   = 0.
       if (iPEC > 0)   con_PEC   = con(iRset,iRec,iPEC)
       if (iASOIL > 0) con_ASOIL = con(iRset,iRec,iASOIL)
       if (iACORS > 0) con_ACORS = con(iRset,iRec,iACORS)
       if (iNO2   > 0) con_NO2   = con(iRset,iRec,iNO2)

!--- Now calculate the total (project + natural background) extinction, and
!    the "delta Bext" percent change to natural background.  Note that
!    Sm/Lg Sulfate/Nitrate/Organic already have the Project Component added
!    above.

       Bext =                                      &
            2.2 * SmallRH(iRset,mo) * SmSulfate  + &       ! small sulfate
            4.8 * LargeRH(iRset,mo) * LgSulfate  + &       ! large sulfate
            2.4 * SmallRH(iRset,mo) * SmNitrate  + &       ! small nitrate
            5.1 * LargeRH(iRset,mo) * LgNitrate  + &       ! large nitrate
            2.8 *                     SmOrganic  + &       ! small organic mass
            6.1 *                     LgOrganic  + &       ! large organic mass
            10. * (NatCond(iRset,4) + con_PEC)   + &       ! elemental carbon
            1.0 * (NatCond(iRset,5) + con_ASOIL) + &       ! fine soil
            0.6 * (NatCond(iRset,6) + con_ACORS) + &       ! coarse mass
            1.7 * SeaSalt(iRset,mo) * NatCond(iRset,7) + & ! sea salt
            NatCond(iRset,8)                     + &       ! Rayleigh
            0.1755 * con_NO2                               ! NO2

       dBext = (Bext - Bext_nat(iRset,mo)) / Bext_nat(iRset,mo) * 100. ! in %

!--- Calculate source-only component contributions
!    Adapted from a code snippet contributed by Bret Anderson.

       TotSulfate = 0.
       if (iPSO4  > 0) TotSulfate = TotSulfate + con(iRset,iRec,iPSO4)
       if (iASO4K > 0) TotSulfate = TotSulfate + con(iRset,iRec,iASO4K)
       if (TotSulfate < 20.) then              ! 20 ug/m3
          LgSulfate = TotSulfate / 20. * TotSulfate
       else
          LgSulfate = TotSulfate
       end if
       SmSulfate = TotSulfate - LgSulfate

       TotNitrate = 0.
       if (iPNO3  > 0) TotNitrate = TotNitrate + con(iRset,iRec,iPNO3)
       if (iANO3K > 0) TotNitrate = TotNitrate + con(iRset,iRec,iANO3K)
       if (TotNitrate < 20.) then              ! 20 ug/m3
          LgNitrate = TotNitrate / 20. * TotNitrate
       else
          LgNitrate = TotNitrate
       end if
       SmNitrate = TotNitrate - LgNitrate

       TotOrganic = 0.
       if (iPSOA  > 0) TotOrganic = TotOrganic + con(iRset,iRec,iPSOA)
       if (iPOC   > 0) TotOrganic = TotOrganic + con(iRset,iRec,iPOC)
       if (TotOrganic < 20.) then              ! 20 ug/m3
          LgOrganic = TotOrganic / 20. * TotOrganic
       else
          LgOrganic = TotOrganic
       end if
       SmOrganic = TotOrganic - LgOrganic

       bSO4 = (SmallRH(iRset,mo) * SmSulfate * 2.2) + &
            (LargeRH(iRset,mo) * LgSulfate * 4.8)
       bNO3 = (SmallRH(iRset,mo) * SmNitrate * 2.4) + &
            (LargeRH(iRset,mo) * LgNitrate * 5.1)
       bOCM = 2.8 * SmOrganic + 6.1 * LgOrganic
       bSOIL = con_ASOIL * 1.0
       bPEC = 10.0 * con_PEC
       bNO2 = 0.1755 * con_NO2
       bCORS = 0.6 * con_ACORS

!--- Finally, save the change (delta) in extinction for this hour, as a
!    running sum.

       call max_or_avg(sum_val(iRset,iRec,iOut), dBext, output(iOut)%stat, &
            iymdh, iymdh_max(iRset,iRec,iOut) )

       call max_or_avg(dBc_sum(iRset,iRec,iOut,1), Bext-Bext_nat(iRset,mo), output(iOut)%stat, &
            iymdh, iymdh_max(iRset,iRec,iOut) )
       call max_or_avg(dBc_sum(iRset,iRec,iOut,2), Bext_nat(iRset,mo), output(iOut)%stat, &
            iymdh, iymdh_max(iRset,iRec,iOut) )
       call max_or_avg(dBc_sum(iRset,iRec,iOut,3), bNO3, output(iOut)%stat, &
            iymdh, iymdh_max(iRset,iRec,iOut) )
       call max_or_avg(dBc_sum(iRset,iRec,iOut,4), bSO4, output(iOut)%stat, &
            iymdh, iymdh_max(iRset,iRec,iOut) )
       call max_or_avg(dBc_sum(iRset,iRec,iOut,5), bOCM, output(iOut)%stat, &
            iymdh, iymdh_max(iRset,iRec,iOut) )
       call max_or_avg(dBc_sum(iRset,iRec,iOut,6), bSOIL,output(iOut)%stat, &
            iymdh, iymdh_max(iRset,iRec,iOut) )
       call max_or_avg(dBc_sum(iRset,iRec,iOut,7), bPEC, output(iOut)%stat, &
            iymdh, iymdh_max(iRset,iRec,iOut) )
       call max_or_avg(dBc_sum(iRset,iRec,iOut,8), bNO2, output(iOut)%stat, &
            iymdh, iymdh_max(iRset,iRec,iOut) )
       call max_or_avg(dBc_sum(iRset,iRec,iOut,9), bCORS,output(iOut)%stat, &
            iymdh, iymdh_max(iRset,iRec,iOut) )

    end do   ! do iRec = 1,nRec

    iHours(iRset,iOut) = iHours(iRset,iOut) + 1

!--- Find the n-period average value, then find the rank of this value,
!    and insert the value at the right rank

    if (iHours(iRset,iOut) == output(iOut)%avgPeriods) then ! end averaging period

       do iRec = 1, nRec(iRset) ! sum_val is dBext here

          if ( output(iOut)%stat == "A" ) then ! average by div sum/hours
             sum_val(iRset,iRec,iOut) = &
                  sum_val(iRset,iRec,iOut)/iHours(iRset,iOut)
             do i = 1,nCont
                dBc_sum(iRset,iRec,iOut,i) = &
                     dBc_sum(iRset,iRec,iOut,i)/iHours(iRset,iOut)
             end do
          end if ! if output(iOut)%stat == "M", we already have the maximum

          ir = find_rank( sum_val(iRset,iRec,iOut), iRset, iRec, iOut, &
               output(iOut)%rank)
          call insert( sum_val(iRset,iRec,iOut), iRset, iRec, iOut,   &
               output(iOut)%rank, ir, iymdh_max(iRset,iRec,iOut) ) 

          do i = 1, nCont ! dBc is the dBext contributions
             call insert_dBc( dBc_sum(iRset,iRec,iOut,i), iRset, iRec, iOut, i, &
                  output(iOut)%rank, ir, iymdh_max(iRset,iRec,iOut) )
          end do

!---- Write out dBext if it exceeds the requested criteria.

          if (sum_val(iRset,iRec,iOut) >= output(iOut)%exceeds ) then

             write(string1,*) ">=",output(iOut)%exceeds
             if (output(iOut)%chemSpecies == "CONT") then
                tmp7(:) = dBc_sum(iRset,iRec,iOut,:)
                call write_vis( iOut, iRset, iRec, iymdh_max(iRset,iRec,iOut), &
                     string1, sum_val(iRset,iRec,iOut), tmp7, nCont )
             else
                call write_output( iOut, iRset, iRec, iymdh_max(iRset,iRec,iOut),&
                      string1, sum_val(iRset,iRec,iOut) )
             end if

          end if

       end do

       iHours(iRset,iOut) = 0
       sum_val(iRset,:,iOut) = 0.
       dBc_sum(iRset,:,iOut,:) = 0.

    end if ! if (iHours(iRset,iOut) == output(iOut)%avgPeriods) then

    RETURN

  END SUBROUTINE calc_vis
!
!*************************************************************************
!
  SUBROUTINE read_rec_set(nRsets, MaxRec)

    IMPLICIT NONE

    integer                 :: nRsets ! the number of Class I areas
    integer                 :: iRset  ! which Class I area to process
    integer                 :: MaxRec ! max( nRec(:) )
    integer                 :: ios, i, astat
    integer, parameter      :: lun = 100
    real                    :: xlon, xlat
    character (len=1)       :: slash, backslash
    character (len=99)      :: line
    logical                 :: lexist
    logical, dimension(:), allocatable :: header

    slash     = char(47) ! AKA "forward slash"
    backslash = char(92)

!
!--- Allocate the arrays to nrec
!
    allocate( nRec(nRsets), STAT=astat )
    if ( astat > 0 ) then
       nError = UK_ERROR
       WRITE(eMessage,*) 'Error allocating nRec with size ',nRsets*4
       return
    end if
    allocate( header(nRsets), STAT=astat )
    if ( astat > 0 ) then
       nError = UK_ERROR
       WRITE(eMessage,*) 'Error allocating header with size ',nRsets*4
       return
    end if
!
!--- Count the number of receptors, and allocate the global vars lat,lon
!
    do iRset = 1,nRsets
       inquire(file=TRIM(Rset(iRset)%Filename),exist=lexist)
       if (.not. lexist) goto 99
       write(*,*)'Reading receptor locations from ',TRIM(Rset(iRset)%Filename)
       open(lun+iRset,file=Rset(iRset)%Filename, status='old',err=99,iostat=ios)
       read(lun+iRset,'(a)') line
       read(line,*,iostat=ios) xlon,xlat
       if (ios == 0) then 
          header(iRset) = .false.
          rewind(lun+iRset)
       else
          header(iRset) = .true.
       endif
       if (debug) print*,"RecSet ",iRset," header = ",header(iRset)

       nRec(iRset) = 0
       read(lun+iRset,*,iostat=ios) xlon, xlat ! just make sure they're readable
       do while (ios == 0)
          nRec(iRset) = nRec(iRset) + 1
          read(lun+iRset,*,iostat=ios) xlon, xlat
       end do

    end do ! do iRset = 1,nRsets
    write(*,*)

    MaxRec = maxval(nRec(:))
!
!--- Allocate the arrays to hold the lon-lat and x,y for ALL receptor sets
!
    allocate( lat(iRset,MaxRec), lon(iRset,MaxRec), STAT=astat )
    if ( astat > 0 ) then
       nError = UK_ERROR
       WRITE(eMessage,*) 'Error allocating coordinates (lat-lon)'
       return
    end if
    lat = -999. ! fill with a "missing value" flag, since these will be sparse
    lon = -999.

    allocate( xRec(iRset,MaxRec), yRec(iRset,MaxRec), STAT=astat )
    if ( astat > 0 ) then
       nError = UK_ERROR
       WRITE(eMessage,*) 'Error allocating coordinates (xRec-yRec)'
       return
    end if
    xRec = -999. ! fill with a "missing value" flag, since these will be sparse
    yRec = -999.
!
!--- Now read the receptors, but don't read the elev (not used). This Rset may
!    be in lon-lat or UTM, but we'll store it in lon() and lat() for now.
!
    do iRset = 1,nRsets

       rewind(lun+iRset)
       if (header(iRset)) read(lun+iRset,*) ! skip header line
       do i = 1,nRec(iRset)
          read(lun+iRset,*,iostat=ios) lon(iRset,i), lat(iRset,i) ! could be UTM
!          print*,"iRset,iRec, lon, lat = ",iRset,i,lon(iRset,i), lat(iRset,i)
          if (ios /= 0) goto 999
       end do
       close(lun+iRset)

    end do ! do iRset = 1,nRsets

    if (allocated( header )) deallocate( header )

    RETURN ! no errors

99  CONTINUE
    nError = UK_ERROR
    WRITE(eMessage,*) 'Error opening file ',trim(Rset(iRset)%Filename)
    RETURN

999 CONTINUE
    nError = UK_ERROR
    WRITE(eMessage,*) 'Error ',ios,'reading file ',trim(Rset(iRset)%Filename)
    RETURN

  END SUBROUTINE read_rec_set
!
!*************************************************************************
!
  SUBROUTINE init_visibility(nRsets)

    IMPLICIT NONE

    integer                 :: nRsets ! number of Class I areas to consider
    integer                 :: iRset  ! loop over Class I Rset
    integer                 :: mo     ! month
    integer                 :: ios, astat
    integer, parameter      :: lun = 100

    real                    :: TotSulfate, LgSulfate, SmSulfate
    real                    :: TotNitrate, LgNitrate, SmNitrate
    real                    :: TotOrganic, LgOrganic, SmOrganic

    character (len=256)     :: line   ! one line from a FLAG(2010) CSV table
    character (len=99)      :: word   ! one word from a FLAG(2010) CSV table

    logical                 :: lexist

!--- Allocate the FLAG(2010) table containers. We'll allocate for ALL receptor
!    sets, even though not all may be Class I receptor sets.  Simpler, but uses
!    more memory.

    allocate( NatCond(nRsets,8), STAT=astat )
    IF( astat > 0 )THEN
       nError = UK_ERROR
       WRITE(eMessage,*) 'Error allocating NatCond'
       GO TO 9999
    END IF
    NatCond = 0.
    allocate( LargeRH(nRsets,12), STAT=astat )
    IF( astat > 0 )THEN
       nError = UK_ERROR
       WRITE(eMessage,*) 'Error allocating LargeRH'
       GO TO 9999
    END IF
    LargeRH = 0.
    allocate( SmallRH(nRsets,12), STAT=astat )
    IF( astat > 0 )THEN
       nError = UK_ERROR
       WRITE(eMessage,*) 'Error allocating SmallRH'
       GO TO 9999
    END IF
    SmallRH = 0.
    allocate( SeaSalt(nRsets,12), STAT=astat )
    IF( astat > 0 )THEN
       nError = UK_ERROR
       WRITE(eMessage,*) 'Error allocating SeaSalt'
       GO TO 9999
    END IF
    SeaSalt = 0.

    allocate( Bext_nat(nRsets,12), STAT=astat )
    IF( astat > 0 )THEN
       nError = UK_ERROR
       WRITE(eMessage,*) 'Error allocating Bext_nat'
       GO TO 9999
    END IF
    Bext_nat = 0.

!--- Read the FLAG look-up tables and extract the values for each Class I area

    NatCondFile = "flag.2010.table.6.annual.csv"
    CALL ADDPATH(NatCondFile,trim(flagDir))
    INQUIRE( file=NatCondFile,EXIST=lexist )
    if (.not. lexist) then
       nError = UK_ERROR
       WRITE(eMessage,*) 'Error opening ',trim(NatCondFile)
       GO TO 9999
    end if
    write(*,*) "Reading ",trim(NatCondFile)          ! table 5 (best 20%)
    open(lun, file=NatCondFile, status='old',err=91) ! or table 6 (annual)
    read(lun,*,iostat=ios) ! header line        
    do while (ios == 0)
       read(lun,'(A256)',iostat=ios) line
       call get_csv(line,1,word)
       do iRset = 1, nRsets
          if (Rset(iRset)%class == 1 .and. &
               trim(word) == trim(Rset(iRset)%Name)) then
             call get_csv(line, 3,word)
             read(word,*) NatCond(iRset,1) ! (NH4)2SO4
             call get_csv(line, 4,word)
             read(word,*) NatCond(iRset,2) ! NH4NO3
             call get_csv(line, 5,word)
             read(word,*) NatCond(iRset,3) ! Organic matter
             call get_csv(line, 6,word)
             read(word,*) NatCond(iRset,4) ! Elem carbon
             call get_csv(line, 7,word)
             read(word,*) NatCond(iRset,5) ! Soil
             call get_csv(line, 8,word)
             read(word,*) NatCond(iRset,6) ! Coarse Mass
             call get_csv(line, 9,word)
             read(word,*) NatCond(iRset,7) ! Sea Salt
             call get_csv(line,10,word)
             read(word,*) NatCond(iRset,8) ! Rayleigh
          end if
       end do
    end do
    close(lun)

    do iRset = 1, nRsets
       if ( Rset(iRset)%class == 1 .and. NatCond(iRset,1) == 0.) then
         nError = UK_ERROR
         WRITE(eMessage,*)"Error in init_visibility(): can't find Class I area '",  &
               trim(Rset(iRset)%Name),"' in NatCond file ",trim(NatCondFile)
         GO TO 9999
       endif
    end do

    LargeRHFile = "flag.2010.table.7.FlargeRH.csv"
    CALL ADDPATH(LargeRHFile,trim(flagDir))
    INQUIRE( file=LargeRHFile,EXIST=lexist )
    if (.not. lexist) then
       nError = UK_ERROR
       WRITE(eMessage,*) 'Error opening ',trim(LargeRHFile)
       GO TO 9999
    end if
    write(*,*) "Reading ",trim(LargeRHFile)
    open(lun, file=LargeRHFile, status='old',err=92) ! table 7 (large RH factors)
    read(lun,*,iostat=ios) ! header line
    do while (ios == 0)
       read(lun,'(A256)',iostat=ios) line
       call get_csv(line,1,word)
       do iRset = 1, nRsets
          if (Rset(iRset)%class == 1 .and. word == Rset(iRset)%Name) then
             do mo = 1, 12 ! read 12 months of LargeRH values
                call get_csv(line, 2+mo,word)
                read(word,*) LargeRH(iRset,mo)
             end do
          end if
       end do
    end do
    close(lun)

    SmallRHFIle = "flag.2010.table.8.FsmallRH.csv"
    CALL ADDPATH(SmallRHFIle,trim(flagDir))
    INQUIRE( file=SmallRHFile,EXIST=lexist )
    if (.not. lexist) then
       nError = UK_ERROR
       WRITE(eMessage,*) 'Error opening ',trim(SmallRHFile)
       GO TO 9999
    end if
    write(*,*) "Reading ",trim(SmallRHFile)
    open(lun, file=SmallRHFile, status='old',err=93) ! table 8 (small RH factors)
    read(lun,*,iostat=ios) ! header line
    do while (ios == 0)
       read(lun,'(A256)',iostat=ios) line
       call get_csv(line,1,word)
       do iRset = 1, nRsets
          if (Rset(iRset)%class == 1 .and. word == Rset(iRset)%Name) then
             do mo = 1, 12 ! read 12 months of SmallRH values
                call get_csv(line, 2+mo,word)
                read(word,*) SmallRH(iRset,mo)
             end do
          end if
       end do
    end do
    close(lun)

    SeaSaltFile = "flag.2010.table.9.FseasaltRH.csv"
    CALL ADDPATH(SeaSaltFile,trim(flagDir))
    INQUIRE( file=SeaSaltFile,EXIST=lexist )
    if (.not. lexist) then
       nError = UK_ERROR
       WRITE(eMessage,*) 'Error opening ',trim(SeaSaltFile)
       GO TO 9999
    end if
    write(*,*) "Reading ",trim(SeaSaltFile)
    open(lun, file=SeaSaltFile, status='old',err=94) ! table 9 (sea salt factors)
    read(lun,*,iostat=ios) ! header line
    do while (ios == 0)
       read(lun,'(A256)',iostat=ios) line
       call get_csv(line,1,word)
       do iRset = 1, nRsets
          if (Rset(iRset)%class == 1 .and. word == Rset(iRset)%Name) then
             do mo = 1, 12 ! read 12 months of SeaSalt values
                call get_csv(line, 2+mo,word)
                read(word,*) SeaSalt(iRset,mo)
             end do
          end if
       end do
    end do
    close(lun)
!
!--- Calculate the monthly natural background extinction, bext_natcond
!
    do iRset = 1, nRsets  ! loop over Class I Rsets
       if ( Rset(iRset)%class == 1 ) then
          do mo = 1, 12      ! 12 months
!
!--- Calculate Total Sulfate, and large sulfate fraction, according to
!    Figure 5 from FLAG (2010).  Ditto for Nitate and Organic Mass
!
             TotSulfate = NatCond(iRset,1)
             if (TotSulfate < 20.) then ! 20 ug/m3
                LgSulfate = TotSulfate/20.*TotSulfate
             else
                LgSulfate = TotSulfate
             end if
             SmSulfate = TotSulfate - LgSulfate

             TotNitrate = NatCond(iRset,2)
             if (TotNitrate < 20.) then ! 20 ug/m3
                LgNitrate = TotNitrate/20.*TotNitrate
             else
                LgNitrate = TotNitrate
             end if
             SmNitrate = TotNitrate - LgNitrate

             TotOrganic = NatCond(iRset,3)
             if (TotOrganic < 20.) then ! 20 ug/m3
                LgOrganic = TotOrganic/20.*TotOrganic
             else
                LgOrganic = TotOrganic
             end if
             SmOrganic = TotOrganic - LgOrganic

!--- Calculate natural background visibility obscuration, by Rset and Month

             Bext_nat(iRset,mo) =                           &
               2.2 * SmallRH(iRset,mo) * SmSulfate        + & ! small sulfate
               4.8 * LargeRH(iRset,mo) * LgSulfate        + & ! large sulfate
               2.4 * SmallRH(iRset,mo) * SmNitrate        + & ! small nitrate
               5.1 * LargeRH(iRset,mo) * LgNitrate        + & ! large nitrate
               2.8 *                     SmOrganic        + & ! sm org mass
               6.1 *                     LgOrganic        + & ! lg org mass
               10. *                     NatCond(iRset,4) + & ! elem. carbon
               1.0 *                     NatCond(iRset,5) + & ! fine soil
               0.6 *                     NatCond(iRset,6) + & ! coarse mass
               1.7 * SeaSalt(iRset,mo) * NatCond(iRset,7) + & ! sea salt
               NatCond(iRset,8)                           + & ! Rayleigh scat
               0.1755 * 0.                                    ! NO2

          end do  ! do mo = 1, 12 ! 12 months
       end if     ! if ( Rset(iRset)%class == 1 ) then
    end do        ! do iRset = 1, nRsets  ! loop over Class I Rsets

    write(*,*)    ! blank line to make it look nice

9999 RETURN

91 WRITE(eMessage,*)"SciDosPost: error opening file ",trim(NatCondFile)
   GO TO 9998

92 WRITE(eMessage,*)"SciDosPost: error opening file ",trim(LargeRHFile)
   GO TO 9998
93 WRITE(eMessage,*)"SciDosPost: error opening file ",trim(SmallRHFile)
   GO TO 9998

94 WRITE(eMessage,*)"SciDosPost: error opening file ",trim(SeaSaltFile)
   GO TO 9998

9998 nError = UK_ERROR

     RETURN

  END SUBROUTINE init_visibility
!
!*************************************************************************
!
  subroutine write_vis(iOut, iRset, iRec, iymdh, Cond, val, cont, nCont)

    real                    :: val     ! the dBext to write
    integer                 :: nCont   ! the number of Contributions
    real, dimension(nCont)  :: cont    ! the Contributions to write
    integer                 :: iOut    ! which output we're working on
    integer                 :: iRset   ! which receptor
    integer                 :: iRec    ! receptor index within this receptor set
    integer                 :: iymdh   ! current time in YYYYMMDDHH format
    character (len=*)       :: Cond    ! a description of what we're writing
    integer                 :: i

1   format(2(a,", "),2(f12.6,", "),i11.10,2(", ",i6),", ",a,99(", ",g16.6))
    

    if (iRec > 0) then
       write(output(iOut)%lun,1) trim(Rset(iRset)%Name),     &
            trim(output(iOut)%chemSpecies),                  &
            xRec(iRset,iRec), yRec(iRset,iRec),              &
            iymdh, output(iOut)%avgPeriods,                  &
            output(iOut)%frequency,trim(Cond),               &
            val, (cont(i), i=1,nCont)
    else
       write(output(iOut)%lun,1) trim(Rset(iRset)%Name),     &
            trim(output(iOut)%chemSpecies),                  &
            -999., -999.,                                    &
            -999, output(iOut)%avgPeriods,                   &
            output(iOut)%frequency, trim(Cond), -999.,       &
            (-999., i=1,nCont)
    end if

  end subroutine write_vis
    
!
!*************************************************************************
!
  subroutine write_output(iOut, iRset, iRec, iymdh, Cond, val)

    integer                 :: iOut    ! which output we're working on
    integer                 :: iRset   ! which receptor
    integer                 :: iRec    ! receptor index within this receptor set
    integer                 :: iymdh   ! current time in YYYYMMDDHH format
    integer                 :: iy,im,id,ih ! current time
    character (len=*)       :: Cond    ! a description of what we're writing
    real                    :: val     ! the value to write
    
    integer                 :: itmp    ! just a temporary integer
    character (len=8)       :: GroupID = "ALL     "  
    character (len=8)       :: AVEstr  = "  1-HR"  ! FIXME
    real                    :: elev = 0.           ! FIXME
    real                    :: Z = 0.              ! FIXME
    real                    :: X, Y    ! used to convert UTM from KM to meters

    if (output(iOut)%OutType == "CSV") then

1      format(2(a,", "),2(f12.6,", "),i11.10,2(", ",i6),", ",a,", ",g16.6)
2      format(2(f12.6," "),g16.6)

       if (iRec > 0) then
          write(output(iOut)%lun,1) trim(Rset(iRset)%Name),     &
               trim(output(iOut)%chemSpecies),                  &
               xRec(iRset,iRec), yRec(iRset,iRec),              &
               iymdh, output(iOut)%avgPeriods,                  &
               output(iOut)%frequency, trim(Cond), val
       else
          write(output(iOut)%lun,1) trim(Rset(iRset)%Name),     &
               trim(output(iOut)%chemSpecies),                  &
               -999., -999.,                                    &
               -999, output(iOut)%avgPeriods,                   &
               output(iOut)%frequency, trim(Cond), -999.
       end if

    else if (output(iOut)%OutType == "XYZ") then


       if (iRec > 0) then
          write(output(iOut)%lun,2) &
               xRec(iRset,iRec), yRec(iRset,iRec),              &
               val
       else
          write(output(iOut)%lun,2) &
               -999., -999.,                                    &
               -999
       end if
       
    else if (output(iOut)%OutType == "POST" .or. &
         output(iOut)%OutType == "PLOT") then
!                   1993020100
       if (iymdh >= 2000000000) then
          itmp = iymdh - 2000000000
       else if (iymdh > 0) then
          itmp = iymdh - 1900000000 ! cheap way to get YYMMDDHH
       else
          itmp = 0 ! goes with val == -HUGE(0.)
       endif

       call idate2ymdh(itmp,iy,im,id,ih)    ! POSTFILEs are always hr 1 to 24
       call legal_timestamp(iy,im,id,ih,24)
       itmp = iymdh2idate(iy,im,id,ih)
       
       if (ProjectCoordinate%mode == HD_UTM) then
          x = xRec(iRset,iRec) * 1000. ! km to m
          y = yRec(iRset,iRec) * 1000. ! km to m
       else
          x = xRec(iRset,iRec)
          y = yRec(iRset,iRec)
       end if

       if (iRec <= 0 .or. val == -HUGE(0.) ) then
!                                    X                Y    
          write(output(iOut)%lun,3)    -999.,           -999., &
!              AVERAGE CONC
               -999.,                                    &
!              ZELEV   ZHILL ZFLAG AVE     GRP        
               elev,   Z,    Z,    AVEstr, GroupID,    &
!              DATE                                NET ID
               itmp

       else if (val < 1.e-5 .and. val /= 0.) then
!                                    X  Y  AVERAGE CONC
          write(output(iOut)%lun,4)  x, y, val,        &
!              ZELEV   ZHILL ZFLAG AVE     GRP        
               elev,   Z,    Z,    AVEstr, GroupID,    &
!              DATE                                NET ID
               itmp
       else

!                                    X  Y  AVERAGE CONC
          write(output(iOut)%lun,3)  x, y, val,        &
!              ZELEV   ZHILL ZFLAG AVE     GRP        
               elev,   Z,    Z,    AVEstr, GroupID,    &
!              DATE                                NET ID
               itmp
       end if
       
!
!---- AERMOD POSTFILE output format:
!
3      format(3(1X,F13.5),3(1X,F8.2),2X,A6,2X,A8,2X,I8.8,2X,A8) ! version 12345
4      format(2(1X,F13.5),1X,G13.5,3(1X,F8.2),2X,A6,2X,A8,2X,I8.8,2X,A8) 

    endif
  
    return

  end subroutine write_output
!
!*************************************************************************
!
  subroutine max_or_avg(sumVal,newVal,stat,iTime_cur,iTime_max)
    
    real              :: SumVal      ! running total or maximum values
    real              :: newVal      ! new values to add or take max of
    character (len=1) :: stat        ! "A" = average (sum here) "M" = max
    integer           :: iTime_cur   ! current time stamp
    integer           :: iTime_max   ! if this is a new max, set to iTime_cur

    if ( stat == "A" ) then      ! average by taking the sum

       !print*,"adding ",newVal," to ",sumVal
       sumVal = sumVal + newVal
       iTime_max = iTime_cur     ! will only use the last one

    else if ( stat == "M" ) then ! take the maximum

       !print*,"maxing ",newVal," and ",sumVal
       if (newVal > sumVal .or. sumVal == 0) then ! sumVal == 0 takes the last 0
          sumVal = newVal
          iTime_max = iTime_cur  ! will use the correct one
       endif

    end if
    
    return

  end subroutine max_or_avg
!
!*************************************************************************
!
  integer function find_rank(val,iRset,iRec,iOut,rank)
    
    integer iRset, iRec, iOut, rank, r
    real    val
    
    r = 1
    if (debug) then
       print 10,"iRec,r,rank,val,max_val:",iRec,r,rank,val,  &
            max_val(iRset,iRec,iOut,r)
10     format(a,3i4,2g18.8)
       call flush(6)
    endif
    do while (val < max_val(iRset,iRec,iOut,r) .and. r < rank)
       if (debug) then
          print*,"r,val,max_val:",r,val,max_val(iRset,iRec,iOut,r)
          call flush(6)
       endif
       r = r + 1
    end do
    if (val < max_val(iRset,iRec,iOut,r)) r = r + 1 ! correct for r == rank
    if (debug) print*,"found rank: ",r
    find_rank = r

    return
  end function find_rank
!
!*************************************************************************
!
  SUBROUTINE insert(val, iRset, iRec, iOut, rank, ind, iymdh) ! real

! insert the value val in max_val(,) as the ind-th value, for a given iRec

    integer iRset, iRec, iOut, rank, r, ind, i
    integer iymdh  ! current time in YYYYMMDDHH format
    real    val

    if (ind > rank) return    ! nothing to be done

    if (ind < rank) then
       do r = rank, ind+1, -1 ! move those lower than ind down one notch
          max_val(iRset,iRec,iOut,r)  = max_val(iRset,iRec,iOut,r-1)
          max_time(iRset,iRec,iOut,r) = max_time(iRset,iRec,iOut,r-1)
       end do
    end if

    max_val(iRset,iRec,iOut,ind)  = val
    max_time(iRset,iRec,iOut,ind) = iymdh

    if (debug) then
       print*,"inserted ",val," at index ",ind," Time ",iymdh
       print*,"max_val now ",(max_val(iRset,iRec,iOut,i), i=1,rank)
    end if

    return
  END SUBROUTINE insert
!
!*************************************************************************
!
  SUBROUTINE insert_dBc(val, iRset, iRec, iOut, i, rank, ind, iymdh) ! real

! insert the value val in max_val(,) as the ind-th value, for a given iRec

    integer iRset, iRec, iOut, i,j, rank, r, ind
    integer iymdh  ! current time in YYYYMMDDHH format
    real    val

    if (ind > rank) return    ! nothing to be done

    if (ind < rank) then
       do r = rank, ind+1, -1 ! move those lower than ind down one notch
          dBc_max(iRset,iRec,iOut,r,i)  = dBc_max(iRset,iRec,iOut,r-1,i)
       end do
    end if

    dBc_max(iRset,iRec,iOut,ind,i)  = val

    if (debug) then
       print*,"inserted dBc ",val," at index ",ind
       print*,"dBc_max(i=1) now ",(dBc_max(iRset,iRec,iOut,j,1), j=1,rank)
    end if

    return
  END SUBROUTINE insert_dBc
!
!*************************************************************************
!
  REAL FUNCTION ppm2ugm3(Species)
!
!---- Convert from PPM (gasses) to ug/m3 (micro-grams per cubic meter).
!     Here are some verification values, taken from EPA definitions, which
!     are not necessarily the most accurate scientifically...
!          NO :   0.1 PPM = 123 ug/m3
!          NO2: 0.053 PPM = 100 ug/m3
!          NO2:   0.1 PPM = 188 ug/m3
!          SO2: 0.075 PPM = 196 ug/m3
!          CO :     9 PPM =  40 mg/m3 (millegrams, not micrograms)
!          O3 : 0.075 PPM = 147 ug/m3
!          Pb :     1 PPM = 8.5 mg/m3 (millegrams, not micrograms)
!     See http://www.skcinc.com/converter/converter.asp, for example.
!
    character (len=*) :: Species ! input:  "NO" or "NO2" or "O3", etc.
    real              :: factor  ! output: conversion factor
    real              :: MW      ! molecular weight of species

    if (Species == "") then
       MW = -999. ! flag for not found
    elseif (Species == "N")   then
       MW = 14.01
    elseif (Species == "N2")   then
       MW = 28.01
    elseif (Species == "NO") then
       MW = 30.01
    elseif (Species == "NO2") then
       MW = 46.01
    elseif (Species == "N2O") then
       MW = 44.01
    elseif (Species == "N2O5") then
       MW = 95.01
    elseif (Species == "HNO3") then
       MW = 63.01
    elseif (Species == "HONO") then
       MW = 47.01
    elseif (Species == "PNA") then ! HNO4
       MW = 79.01
    elseif (Species == "PAN") then ! CH3COOONO2
       MW = 121.05
    elseif (Species == "NH3")  then
       MW = 17.03
    elseif (Species == "S") then
       MW = 32.06
    elseif (Species == "SO2") then
       MW = 64.06
    elseif (Species == "CO")  then
       MW = 28.01
    elseif (Species == "CO2")  then
       MW = 44.01
    elseif (Species == "O3")  then
       MW = 48.00
    elseif (Species == "O2")  then
       MW = 32.00
    elseif (Species == "O")   then
       MW = 16.00
    elseif (Species == "H2O")  then
       MW = 18.02
    elseif (Species == "Pb") then
       MW = 207.20
    else
       MW = -999. ! flag for not found
    end if

    if (MW == -999.) then
       PPM2ugm3 = 1.                 ! default to no conversion
    else
       PPM2ugm3 = 1000. * MW / 24.45 ! 1000 is millegrams to micrograms
    endif

    RETURN

  END FUNCTION ppm2ugm3
!
!*************************************************************************
!
  SUBROUTINE find_units_conv(units_in, units_out, avg_time, units_conv)

    implicit none
! inputs:
    character (len=*) :: units_in, units_out
    integer           :: avg_time   ! in seconds

! output:
    real              :: units_conv ! output conversion factor

! local
    integer           :: i,j,k,l,m
    character (len=9) :: numer_in, numer_out, denom_in, denom_out, time_in

    i = index(units_in, "-")
    j = index(units_in, "/")
    k = len_trim(units_in)

    l = index(units_out,"/")
    m = len_trim(units_out)

    units_conv = 1.   ! default

    numer_out = units_out(1:l-1)
    denom_out = units_out(l+1:m)

    if ( i > 0 ) then ! either kg-s/m3 or g-s/m3, Tracer (not mult-component)
       time_in  = units_in(i+1:j-1)
       if ( TRIM(time_in) == "s" ) then
          units_conv = units_conv / avg_time ! s/s
       else if ( TRIM(time_in) == "hr" ) then
          units_conv = units_conv / avg_time * 3600 ! hr/s * s/hr
       else
          write(*,*) "Error in find_units_conv(): Unknown time units:", &
               trim(time_in)
          write(*,*) "  units_in  = ",trim(units_in)
          write(*,*) "  units_out = ",trim(units_out)
          stop
       endif
       numer_in = units_in(1:i-1)
       denom_in = units_in(j+1:k)
    else              ! any of kg/m2, g/m2, ug/m3 or ug/m2
       numer_in = units_in(1:j-1)
       denom_in = units_in(j+1:k)
    end if

    if ( numer_in == numer_out) then
       ! nothing to do here
    else if ( numer_in == "kg" .and. numer_out == "ug") then
       units_conv = units_conv * 1.e9  ! ug/kg
    else if ( numer_in == "g"  .and. numer_out == "ug") then
       units_conv = units_conv * 1.e6  ! ug/g
    else if ( numer_in == "ug" .and. numer_out == "kg") then
       units_conv = units_conv * 1.e-9 ! kg/ug
    end if

    if ( denom_in == denom_out ) then
       ! nothing to do here
    else if ( denom_in == "m2" .and. denom_out == "ha") then
       units_conv = units_conv * 1.e4 ! m^2/hectare
    else if ( denom_in == "ha" .and. denom_out == "m2") then
       units_conv = units_conv / 1.e4 ! hectare/m^2
    else if ( denom_in == "m2" .and. denom_out == "m3") then
       write(*,*) "Error in find_units_conv(): tried to conver /m2 to /m3"
       write(*,*) "  units_in  = ",trim(units_in)
       write(*,*) "  units_out = ",trim(units_out)
       stop
    end if

    RETURN

  END SUBROUTINE find_units_conv
!
!*************************************************************************
!
  LOGICAL FUNCTION leap_yr(Yr) ! Y2K correct.

    IMPLICIT NONE

    integer yr

! If integer divide and float divide give the same result, then it's
! evenly divisible.  Could have used mod() here, but I think this is
! more portable.  Might give problems on some old Pentium systems
! with that pesky 4./2. = 1.9999998 error.

! The year is 365.2422 days long.
! Rules for leap years: if year is evenly divisible by 4, then it's
! a leap year, except those evenly divisible by 100, but there is a
! leap year in those evenly divisible by 400.  This will give a mean
! year 365.2425 days long.  Error is .0003 days (25.92 seconds) so
! it will take 3333 years for the calendar to get off by one day.

    if ((float(Yr/4) .eq. Yr/4.) .and. ((float(Yr/100) .ne. Yr/100.) &
         .or. (float(Yr/400) .eq. Yr/400.))) then
       leap_yr = .true.
    else
       leap_yr = .false.
    end if

    RETURN

  END FUNCTION leap_yr
!
!------------------------------------------------------------------------------
!
  subroutine legal_timestamp(iy,im,id,ih,iMaxHr)
!
!-----Makes sure the time is a legal time, either 0-23 (if iMaxHr = 23) 
!     or 1-24 (if iMaxHr = 24).  
!
    IMPLICIT NONE

    integer :: iy,im,id,ih,iMaxHr

    do while (ih < iMaxHr-23)
       ih = ih + 24
       id = id - 1
       if (id < 1) then
          im = im - 1
          if (im == 0) then
             im = 12
             iy = iy - 1
             if (iy < 0) iy = iy + 100    ! May be 2-digit year
          end if
          id = id + iDaysInMth(im,iy)
       end if
    end do

    do while (ih > iMaxHr)
       ih = ih - 24
       id = id + 1
       if (id > iDaysInMth(im,iy)) then
          im = im + 1
          if (im > 12) then
             im = 1
             iy = iy + 1
             if (iy == 100) iy = 0        ! May be 2-digit year
          end if
          id = 1
       end if
    end do
    return

  end subroutine legal_timestamp
!
!------------------------------------------------------------------------------
!
  integer function iymdh2idate(iy,im,id,ih)
!
!-----Returns YYMMDDHH stamp from year, month, day, hour input
!
    integer iy,im,id,ih
    
    iymdh2idate = iy*1000000 + im*10000 + id*100 + ih
    return

  end function iymdh2idate
!
!------------------------------------------------------------------------------
!
  subroutine idate2ymdh(idate,iy,im,id,ih)
!
!-----Returns year, month, day, hour from YYMMDDHH or YYYYMMDDHH input
!
    integer iy,im,id,ih, idate

    iy = idate/1000000
    im = (idate - iy*1000000)/10000
    id = (idate - iy*1000000 - im*10000)/100
    ih = (idate - iy*1000000 - im*10000 - id*100) 
    return

  end subroutine idate2ymdh
!
!------------------------------------------------------------------------------
!
  integer function iDaysInMth(im,iy)
!
!-----Sets the number of days in the month
!
    integer           :: mon(12),iy,im
    data mon/31,28,31,30,31,30,31,31,30,31,30,31/

    if (Leap_yr(iy) .and. im == 2) then
       iDaysInMth = 29
    else
       iDaysInMth = mon(im)
    end if
    return

  end function iDaysInMth
!
!------------------------------------------------------------------------------
!
subroutine TimeDiff(iy1,im1,id1,ih1, iy2,im2,id2,ih2, hrs)
!
!------Returns the number of hours between two time-stamps
!
  integer, intent(in)  :: iy1,im1,id1,ih1, iy2,im2,id2,ih2
  integer, intent(out) :: hrs
  integer              :: i,minyr, hrs1,hrs2 ! local variales
!
!-----Calculate the number of hours since the beginning of the earliest year:
!
  minyr = min(iy1,iy2) - 1

  hrs1 = 24*(id1-1) + ih1 ! days of this month, plus hours for today
  do i = minyr, iy1-1     ! possibly zero times through the loop
     hrs1 = hrs1 + 8760   ! add the hours for all the years until this year
     if (Leap_yr(i)) hrs1 = hrs1 + 24
  end do
  if (im1 > 1) then       ! add the hours for all the months until last month
     do i = 1, im1-1
        hrs1 = hrs1 + iDaysInMth(i,iy1) * 24
     end do
  end if

  hrs2 = 24*(id2-1) + ih2 ! days of this month, plus hours for today
  do i = minyr, iy2-1     ! possibly zero times through the loop
     hrs2 = hrs2 + 8760   ! add the hours for all the years until this year
     if (Leap_yr(i)) hrs2 = hrs2 + 24
  end do
  if (im2 > 1) then       ! add the hours for all the months until last month
     do i = 1, im2-1
        hrs2 = hrs2 + iDaysInMth(i,iy2) * 24
     end do
  end if

  hrs = hrs2 - hrs1       ! answer is the difference

end subroutine TimeDiff

!======================================================================
! DataInitialization
!======================================================================

  SUBROUTINE InitSciDosPost()

! Intializes all common variables

    USE tooluser_fd
    USE Extract_fi
    USE GetTimes_fi
    USE contour_fd

    IMPLICIT NONE

!--- Initialize

    callerID = USER_ID
    maxTry   = 5
    nFields  = 0

!Error handling

    nError      = NO_ERROR
    eMessage    = ' '
    eInform     = ' '
    eAction     = ' '
    eRoutine    = ' '

!Flags

    extractMethod = GRID

    Project%name = ' '
    Project%path = ' '

!Plot type

    plotType%type     = HP_MEAN
    plotType%data     = 0.0

!Contour

    contourHead%number    = 1
    contourHead%DrawMode  = PLOT_OFF
    contourHead%LabelMode = PLOT_OFF
    contourHead%Scale     = 1.0
    contourHead%Unit      = 'default'
    contourMode           = CLOSE_CONTOUR

!Plot category data

    sliceHeight    = 0.0             !Horizontal slice height
    sliceXmin      = DEF_VAL_R       !Minimum X slice value - vertical slice
    sliceXmax      = DEF_VAL_R       !Maximum X slice value - vertical slice
    sliceYmin      = DEF_VAL_R       !Minimum Y slice value - vertical slice
    sliceYmax      = DEF_VAL_R       !Maximum Y slice value - vertical slice
    sliceZmin      = 0.0             !Minimum Z slice value - vertical slice
    sliceZmax      = 2500.           !Maximum Z slice value - vertical slice
    sliceNz        = 21              !Slice Z points - Vertical slice

    iround         = -999
    riskLevel      = 10.0

!Plot data sizes
    nClass    = 0
    nChoice   = 0
    nKind     = 0
    nTimePuff = 0
    nTimeSrf  = 0
    nTimeMet  = 0
    nTimeOut  = 0
    nTable    = 0
    nCol      = 0
    nRow      = 0
    nTriangle = 0
    nNode     = 0
    nPoint    = 0
    nLine     = 0

!Field max values

    FldMax = -HUGE(1.0)
    FldMin = HUGE(1.0)

!Command line strings

    ini_file    = ''
    PrjName     = ''

!Grid

    maxGrd         = 0                      !Maximum grid cells
    GridType       = NATIVE
    ContourType    = AUTO
    extractMethod  = GRID

    xMin =  HUGE(1.0)
    xMax = -HUGE(1.0)
    yMin = xMin
    yMax = xMax
    zMin = sliceZmin
    zMax = SliceZmax
    dx   = 0.0
    dy   = 0.0
    dz   = 0.0

    nx = 0
    ny = 0
    nz = 0

!Unit numbers
    lun_in  = 5
    lun_out = 6
    lun_grd = 0
    lun_ter = 0

    Field%category   = HP_SSLICE              !Field category index
    Field%class      = -1
    Field%choice     = 0
    Field%kind       = 0
    Field%timeID     = 0
    Field%userTime   = 0.0
    Field%iNotUsed   = SCIPoff
    Field%maxCells   = 25000
    Field%maxLev     = 99
    Field%fldLev     = 99
    Field%resolution = 1.0
    Field%interpType = 0.0
    Field%units      = 'XX'
    Field%project    = ' '
    Field%path       = ' '

! Fill up SCIPFieldCoordinateT structure
    Field%coordinate%mode                 = HD_LATLON
    Field%coordinate%UTMZone              = NOT_SET_I
    Field%coordinate%reference%x          = NOT_SET_R
    Field%coordinate%reference%y          = NOT_SET_R
    Field%coordinate%reference%lat        = NOT_SET_R
    Field%coordinate%reference%lon        = NOT_SET_R
    Field%coordinate%vertSlice%resolution = NOT_SET_I
    Field%coordinate%vertSlice%startPt%x  = NOT_SET_R
    Field%coordinate%vertSlice%startPt%y  = NOT_SET_R
    Field%coordinate%vertSlice%endPt      = Field%coordinate%vertSlice%startPt
    Field%coordinate%horzSlice%height     = NOT_SET_R
    Field%coordinate%horzSlice%mode       = NOT_SET_I

    NULLIFY( TimePuff )
    NULLIFY( TimeSrf )
    NULLIFY( TimeMet )
    NULLIFY( TimeOut )

    contourExt = '.txt'

    RETURN
  END SUBROUTINE InitSciDosPost

!======================================================================
! ToolInitialization
!======================================================================

  SUBROUTINE InitToolSciDosPost()

    USE basic_fd
    USE tooluser_fd
    USE SCIPtool
    USE Extract_fi
    USE GetTimes_fi

    IMPLICIT NONE

    INTEGER irv
    INTEGER request
    INTEGER(LEN_ADDRESS) CallBackAddress
    INTEGER idefault
    LOGICAL lExist

    CHARACTER(32)  mode
    CHARACTER(128) string1,string2,string3,string4

    TYPE( fileNameT ) :: file

    CHARACTER(PATH_MAXLENGTH), EXTERNAL :: StripNull, AddNull

    INTEGER,              EXTERNAL :: sysGetProfileString
    INTEGER,              EXTERNAL :: sysGetProfileInt
    INTEGER(LEN_ADDRESS), EXTERNAL :: ADDRESSOF
    INTEGER,              EXTERNAL :: sppCallback

    CallBackAddress = ADDRESSOF( sppCallback )

    request =  0

!--- Start up : Initialize

    IF( LEN_TRIM(ini_file) <= 0 )THEN
       WRITE(6,*)
       WRITE(6,'(''PATH for scipuff.ini file(press Enter for default): '',$)')
       READ(lun_in,'(A)')string2
       IF ( LEN_TRIM(string2) == 0 ) THEN
          ini_file = 'scipuff.ini'
          INQUIRE( file=ini_file,EXIST=lexist )
          IF( .NOT.lexist )THEN
             ini_file = 'DEFAULT'
          END IF
       ELSE
          ini_file = TRIM(ADJUSTL(string2))//'/scipuff.ini'
          INQUIRE( file=ini_file,EXIST=lexist )
          IF( .NOT.lexist )THEN
             WRITE(6,*)'Cannot find ',TRIM(ini_file)
             ini_file = 'DEFAULT'
          END IF
       END IF
    ELSE
       ini_file = ADJUSTL(ini_file)
    END IF

    IF( TRIM(ini_file) == 'DEFAULT' )THEN
       WRITE(6,*)'Using default scipuff.ini'
       ini_file = 'scipuff.ini'
    END IF

    INQUIRE(file=ini_file,EXIST=lexist)
    IF( .NOT.lexist )THEN
       nError      = UK_ERROR
       eMessage    = 'Unable to initialize the SCIPtool library'
       eInform     = 'Specified initialization file not found'
       eAction     = 'INI file='//TRIM(ini_file)
       eRoutine    = 'ToolInitialzaion'
       GOTO 9999
    END IF
    WRITE(6,*)
    WRITE(6,*)'Initializing SCIPtool from '//TRIM(ini_file)
    WRITE(6,*)

    string1 = AddNull( 'Paths' )
    string2 = AddNull( 'SciDataDir' )
    string3 = '.'
    string4 = AddNull( TRIM(ini_file) )
    irv = sysGetProfileString( string1,string2,string3,flagDir,string4 )
    IF( TRIM(flagDir) /= '.' )THEN
      string3 = flagDir
      flagDir = 'Flag2010'
      CALL ADDPATH(flagDir,string3)
    END IF
    WRITE(6,*)'flagDir = ',TRIM(flagDir)
    WRITE(6,*)

    string4 = AddNull( TRIM(ini_file) )
    string1 = AddNull( 'SCIPMode' )
    string2 = AddNull( 'GUIMode' )
    string3 = 'Standard'
    irv = sysGetProfileString( string1,string2,string3,mode,string4 )

    CALL cupper( mode )

    SELECT CASE( TRIM(mode) )
    CASE( 'STANDARD' )
       limit%met1D       = HUGE(1)
       limit%puffs       = 20000
       limit%surfaceGrid = 25000

    CASE( 'OPERATIONAL' )
       limit%met1D       = HUGE(1)
       limit%puffs       = 20000
       limit%surfaceGrid = 25000

    CASE( 'EXTENDED' )
       limit%met1D       = HUGE(1)
       limit%puffs       = 40000
       limit%surfaceGrid = 85000

    CASE( 'ULTIMATE' )
       limit%met1D       = HUGE(1)
       limit%puffs       = 60000
       limit%surfaceGrid = 100000

    CASE DEFAULT
       limit%met1D       = HUGE(1)
       limit%puffs       = 20000
       limit%surfaceGrid = 25000
       string2 = AddNull( 'MaxMet1D' )
       idefault = limit%met1D
       irv = sysGetProfileInt( string1,string2,idefault,limit%met1D,string4 )
       string2 = AddNull( 'MaxPuff' )
       idefault = limit%puffs
       irv = sysGetProfileInt( string1,string2,idefault,limit%puffs,string4 )
       string2 = AddNull( 'MaxGrid' )
       idefault = limit%surfaceGrid
       irv = sysGetProfileInt( string1,string2,idefault,limit%surfaceGrid,string4 )
    END SELECT

!==== Initialize PlotStub

    WRITE(6,'("Running SciDosPost in ",A," mode")')TRIM(mode)
    WRITE(6,'("  MaxPuff = ",I15)')limit%puffs
    WRITE(6,'("  MaxGrid = ",I15)')limit%surfaceGrid

    file%string = TRIM(ini_file)

    irv = SCIPInitTool( callerID,CallBackAddress,request,limit,file )
    IF( irv == SCIPFailure )THEN
       CALL toolError ('Failed to initialize the SCIPtool library' )
       GOTO 9999
    END IF

    lInit = .TRUE.

9999 CONTINUE

    RETURN
  END SUBROUTINE InitToolSciDosPost
!
!*************************************************************************
!
END MODULE SciDosPostTools
