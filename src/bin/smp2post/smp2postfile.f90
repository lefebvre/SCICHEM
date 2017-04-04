program smp2postfile
!
!------------------------------------------------------------------------------
! SPM2POSTFILE Program
! VERSION 0.2 2013-06-19
!
! Development History:
! 2013-05-17  Original Development: ENVIRON International Corp, 
!             Bart Brashers, bbrashers@environcorp.com.
! 2013-06-07  Version 0.1 delivered to SAGE, on to EPRI for testing.
! 2013-06-19  Version 0.2 delivered to SAGE.
!
!------------------------------------------------------------------------------
! This program is free software; you can redistribute it and/or 
! modify it under the terms of the GNU General Public License 
! as published by the Free Software Foundation; either version 2 
! of the License, or (at your option) any later version. 
! 
! This program is distributed in the hope that it will be useful, 
! but WITHOUT ANY WARRANTY; without even the implied warranty of 
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
! GNU General Public License for more details. 
!------------------------------------------------------------------------------

  implicit none

  character (len=256)  :: smp_filename        ! SAMPLER HISTORY   *.SMP filename
  character (len=6)    :: smp_filetype        ! Multi- or single-compoenent
  character (len=256)  :: postfilename        ! output filename (or basename)
  character (len=256)  :: all_species         ! list of all species to ouput
  character (len=256)  :: all_conv            ! list of all conversion rates
  character (len=20), allocatable :: smp_species(:) ! species found in *.SMP file
  character (len=20), allocatable :: usr_species(:) ! species requested by user
  character (len=99999):: line                ! one line of the SMP file
  character (len=100)  :: word                ! one word of the above line
  character (len=8)    :: datestr             ! current date
  character (len=10)   :: timestr             ! current time
  character (len=6)    :: AVEstr              ! AERMOD averaging time eg "  1-HR"
  character (len=50)   :: ext                 ! user-specified POSTFILE's .ext
  character (len=8)    :: GroupID             ! AERMOD-style GROUP ID

  real, allocatable    :: X(:),Y(:),Z(:)      ! location and height of receptor
  real, allocatable    :: elev(:)             ! terrain elevation of receptor
  real, allocatable    :: tmp(:)              ! the values in the SMP file
  real, allocatable    :: V(:)                ! the summed values in the SMP file
  real, allocatable    :: smp_conv(:)         ! (ug/m3)/PPM conversion factors
  real, allocatable    :: usr_conv(:)         ! user-specified conversion factor
  real                 :: smp_version         ! version number found in SMP file
  real                 :: avg_time            ! averaging time for concentrations
  real                 :: avg_time_units      ! avg_time/avg_time_units = hours
  real                 :: out_time            ! time diff between SMP records
  real                 :: out_time_units      ! out_time/out_time_units = hours
  real                 :: last_time           ! the previous output timestep (hr)

  integer   :: num_out_per_hr                 ! number of SMP records per hour
  integer   :: ierr,ierr2                     ! flag for some error or other
  integer   :: num_rec                        ! number of receptors in SAM file
  integer   :: num_smp_species                ! number of species   in SMP file
  integer   :: num_usr_species                ! number of user-requested species
  integer   :: num_usr_conv                   ! number of usr_conv values
  integer   :: num_skip                       ! used to parse single vs. multi
  integer   :: i,j,k,n                        ! index variables
  integer   :: byr,bmo,bdy,bhr,bmin           ! beginning time-stamp in SAM file
  integer   :: timezoneHr, timezoneMin        ! timezone of this run
  integer   :: yr,mo,dy,hr                    ! current time-stamp
  integer, parameter   :: smp_file=10         ! FORTRAN logical unit for SMP file
  integer, allocatable :: postfile(:)         ! FORTRAN logical units for output
  integer   :: yr2                            ! external function (below)
  integer   :: iargc                          ! external function

  logical, allocatable :: out_species(:)      ! Whether to write each species
  logical              :: verbose=.true.      ! Verbose or quiet screen output
  logical              :: debug=.false.       ! print debugging messages
!
!---- ENTRY POINT - START OF THE PROGRAM
!
  if (iargc() == 0) call usage ! print help message and exit
!
!---- Set some defaults
!
  postfilename = ""                       ! flag for change extension of SMP file
  GroupID      = "ALL"                    ! printed in POSTFILEs
  all_conv     = "internal_lookup_table"  ! for PPM-to-ug/m3 conversions
  all_species  = "all"                    ! convert all species found
!
!---- Parse the command line options
!
  i = 0
  do while (i < iargc())
     i = i + 1
     call getarg(i,line)
     if (debug) print*,"Processing option ",i,trim(line)
     if (line == "-d" .or. line == "--debug") then
        debug = .true.
     elseif (line == "-q" .or. line == "--quiet") then
        verbose = .false.
     elseif (line == "-i" .or. line == "--smp-file") then
        i = i + 1
        call getarg(i,smp_filename)
        if (debug) print*, "smp_filename = ",trim(smp_filename)
     elseif (line == "-o" .or. line == "--postfile") then
        i = i + 1
        call getarg(i,postfilename)
        if (debug) print*, "postfilename = ",trim(postfilename)
     elseif (line == "-s" .or. line == "--species") then
        i = i + 1
        call getarg(i,all_species)
        if (debug) print*, "all_species = ",trim(all_species)
     elseif (line == "-c" .or. line == "--conversion") then
        i = i + 1
        call getarg(i,all_conv)
        if (debug) print*, "all_conv = ",trim(all_conv)
     elseif (line == "-g" .or. line == "--group") then
        i = i + 1
        call getarg(i,GroupID)
        if (debug) print*, "GroupID = ",trim(GroupID)
     elseif (line == "-v" .or. line == "--version") then
        write(*,*) "SMP2POSTFILE VERSION 0.2 2013-06-19"
        stop
     elseif (line == "-h" .or. line == "--help") then
        call usage ! print message and exit
     else
        write(*,*)
        write(*,*) "Error: unknown command-line switch: ",trim(line)
        write(*,*)
        call usage
     end if
  end do
  
  if (debug) print*,"Done parsing command line."
!
!---- A few sanity checks:
!
  if (smp_filename == "") then
     write(*,*)
     write(*,*) " Error: -i *.SMP (Sampler History file) is required."
     write(*,*)
     call usage
  end if
!
!---- if the user didn't specify an output POSTFILE filename, use the input
!     filename, but change the extension
!
  if (postfilename == "") then
     j = index(smp_filename,".",.true.) ! find last ".", for .ext
     if (j == 0) then ! no extension, just tack one on
        postfilename = trim(smp_filename) // ".pst"
     else
        postfilename = smp_filename(1:j) // "pst"
     end if
     if (debug) print*,"postfilename set to ",trim(postfilename)
  endif
!
!---- Often-called FORMAT statement
!
  99999 format(a65000)   ! used for reading a really long line
!
!---- Count the number of user-requested species, and extract
!     them from the command-line string.  Do the same for the
!     conversion rates.
!
  if (all_species /= "all") then
     num_usr_species = 1        ! user must have given at least 1 species
     i = len_trim(all_species)  ! end of the line
     if (all_species(i:i) == ",") all_species = all_species(1:i-1) ! trailing ","
     do i = 1, len_trim(all_species) ! count the commas
        if (all_species(i:i) == ",") num_usr_species = num_usr_species + 1
     end do
     allocate( usr_species(num_usr_species) )
     read(all_species,*) (usr_species(i), i=1,num_usr_species)
     if (debug) print*,"User's number of species, species = ", &
          num_usr_species,usr_species
  else
     num_usr_species = -1 ! flag meaning "all species", used below
  endif

  if (all_conv /= "internal_lookup_table") then
     num_usr_conv = 1        ! user must have given at least 1 conv ratio
     i = len_trim(all_conv)  ! end of the line
     if (all_conv(i:i) == ",") all_conv = all_conv(1:i-1) ! trailing ","
     do i = 1, len_trim(all_conv) ! count the commas
        if (all_conv(i:i) == ",") num_usr_conv = num_usr_conv + 1
     end do
     if (num_usr_conv /= num_usr_species) then
        write(*,*)
        write(*,*) "Error: number of user-supplied species, and number of ",&
             "user-supplied"
        write(*,*) "       PPM-to-ug/m3 conversion rates do not match!"
        write(*,*) "       num_usr_species = ",num_usr_species
        write(*,*) "       num_usr_conv    = ",num_usr_conv
        stop
     endif
     allocate( usr_conv(num_usr_conv) )
     read(all_conv,*) (usr_conv(i), i=1,num_usr_conv)
     if (debug) print*,"User's number of conv, usr_conv = ", &
          num_usr_conv,usr_conv
  else
     num_usr_conv = -1
  end if
!
!---- Open the SAMPLER HISTORY (*.SMP) file, and parse the first two header
!     lines.  They look something like this:
!
!# Version:    0.1
!#    120 1995-01-01 08:00:00Z-08:00 3600.0 CARTESIAN (510.057,5099.611)=(-122.8700,46.0500)
!    Nrec Date       Time      zone  avg_time  Grid    Grid_origin        lon,lat
!
  if (debug) print*,"Opening smp_file = ",trim(smp_filename)
  open(smp_file, file=smp_filename, status='old',err=90)
  read(smp_file, 99999, iostat=ierr) line ! header line
  if (debug) print*,"1st line of smp_file = ",trim(line)
  if (ierr /= 0) goto 95
!
!---- Get the SMP file format version number, and branch accordingly to 
!     read and parse the header lines.
!
  smp_version = 0.0
  if (line(3:10) == "Version:") read(line(11:),*) smp_version
  if (smp_version == 0.1) then

     read(smp_file, 99999, iostat=ierr) line ! 2nd line
     if (ierr /= 0) goto 95

     call get_word(line," ",2,word) ! get the first word after the "#"
     if (debug) print*,"2nd word of header is ",trim(word)
     read(word,*) num_rec ! number of receptors to follow, 1 per line
     allocate( X(num_rec), Y(num_rec), Z(num_rec), elev(num_rec) )

     call get_word(line," ",3,word) ! get the next word, YYYY-MM-DD
     if (debug) print*,"3rd word of header is ",trim(word)
     read(word,'(i4,1x,i2,1x,i2)') byr, bmo, bdy
     
     call get_word(line," ",4,word) ! get the next word, HH:MM:SSZ-HH:MM
     if (debug) print*,"4th word of header is ",trim(word)
     read(word,'(i2,1x,i2,4x,i3,1x,i2)') bhr,bmin,timezoneHr,timezoneMin
     bhr  = bhr  + timezoneHr     ! this sets the timezone shift
     bmin = bmin + sign(timezoneMin,timezoneHr)
     if (bmin >= 30) bhr = bhr + 1 ! round up??? to nearest hour
     if (bmin < -30) bhr = bhr - 1 ! round down  to nearest hour
     call legal_timestamp(byr,bmo,bdy,bhr,24) ! normalize

     call get_word(line," ",5,word) ! get the next word, averaging time
     if (debug) print*,"5th word of header is ",trim(word)
     read(word,*) avg_time          ! in version 0.1, this is in seconds
     avg_time_units = 3600.         ! for future compatibility
     
     call get_word(line," ",6,word) ! get the next word, output interval
     if (debug) print*,"6th word of header is ",trim(word)
     read(word,*) out_time          ! in version 0.1, this is in seconds
     out_time_units = 3600.         ! for future compatibility
     num_out_per_hr = nint(out_time_units / out_time)
     if (debug) print*,"out_time, num_out_per_hr = ",out_time, num_out_per_hr
     if (num_out_per_hr * out_time /= out_time_units) then ! not a round number
        write(*,*) "Error: output time interval is not an even divisor of",&
             " 3600s (1 hour)."
        write(*,*) "       More programming would be needed to handle e.g. a",&
             " 7s output interval."
        write(*,*) "       Output interval in SMP file header: ",out_time
        stop
     end if
     
     ! FIXME: This is where one could read the lat-lon of the origin,
     !        which is not required in POSTFILEs.  We could add it to 
     !        the header, later, if it would be helpful.
!
!---- Done parsing 2nd line.  Next follows num_rec lines with the receptors
!
     do n = 1, num_rec

        read(smp_file, 99999, iostat=ierr) line ! 2nd line
        if (ierr /= 0) goto 95
        
        call get_word(line," ",2,word)
        read(word,*) X(n) ! in km
        call get_word(line," ",3,word)
        read(word,*) Y(n)
        call get_word(line," ",4,word)
        read(word,*) Z(n)
        call get_word(line," ",5,word)
        read(word,*) elev(n)

        if (.not. allocated( smp_species )) then ! only first time through

           call get_word(line," ",6,word)
           if (word == "CONC") then          ! single-compoment file

              smp_filetype = "single"
              num_smp_species = 1 
              if (debug) print*,"Allocating for SMP, ",num_smp_species," species"
              allocate( smp_species(num_smp_species) )
              allocate( smp_conv(   num_smp_species) )

              call get_word(line," ",7,smp_species(1)) ! chemical species
              if (debug) print*,"7th word of header is ",smp_species(1)

              ! single-component output is always in kg/m3 or g/m3, not PPM
              
              call get_word(line," ",8,word) ! units, either kg/m3 or g/m3
              call clower(word)
              if (debug) print*,"8th word of header is ",trim(word)
              if ( trim(word) == "kg/m3") then
                 smp_conv(1) = 1.e+9         ! convert to ug/m3
              else if ( trim(word) == "g/m3") then
                 smp_conv(1) = 1.e+6         ! convert to ug/m3
              else
                 write(*,*) "Error: unknown units in smp file"
                 write(*,*) trim(word)
                 stop
              endif
              if (debug) print*,"smp_conv(1) = ",smp_conv(1) 

           else if (word == "MC") then       ! multi-component file

              smp_filetype = "multi "
              call get_word(line," ",7,word) ! Should be TRAC:(species-list)
              i = index(line,"(")
              j = index(line,")")
              num_smp_species = 1  ! at least one, plus as many commas as found
              do k = i+1, j-1
                 if (line(k:k) == ",") num_smp_species = num_smp_species + 1
              end do

              if (debug) print*,"Allocating for SMP, ",num_smp_species," species"
              allocate( smp_species(num_smp_species) )
              allocate( smp_conv(   num_smp_species) )

              read(line(i+1:j-1),*) (smp_species(k), k=1,num_smp_species)

              ! 8th word is the tracer units (kg/m3), 9th word is for components

              call get_word(line," ",9,word)
              call clower(word)
              if ( trim(word) == "ppm") then
                 do k = 1, num_smp_species
                    call PPM2ugm3(smp_species(k),smp_conv(k))
                    if (debug) print*, "Species ",trim(smp_species(k)), &
                         " PPM-to-ug/m3 default conversion = ",smp_conv(k)
                 end do
              elseif ( trim(word) == "kg/m3") then
                 smp_conv = 1.e+9            ! convert to ug/m3
              else if ( trim(word) == "g/m3") then
                 smp_conv(1) = 1.e+6         ! convert to ug/m3
              else if ( trim(word) == "ug/m3") then
                 smp_conv = 1.               ! no conversion
              else
                 write(*,*) "Error: unknown units in smp file"
                 write(*,*) trim(word)
                 stop
              endif

           else
              write(*,*) "Error: This program can only handle single-component ", &
                   "(CONC) or multi-component (MC)" 
              write(*,*) "       SMP files.  First receptor line of header is:"
              write(*,*) trim(line)
              stop
           end if

           if (debug) print*,"SMP species list = ",smp_species
        
        end if ! if (.not. allocated( smp_species )) then
     end do ! do i = 1, num_rec : reading receptors from SMP header
!
!---- X,Y values in this version of the header are in km, POSTFILEs are in m
!
     X = X * 1000.  ! was in km, now in meters
     Y = Y * 1000.
!
!---- Finally, read the "real" header, which is not commented with "#", looks
!     something like this for a single-component file:
!              T        C001        V001        C002        V002
!     where the concentrations we want are C001, C002, etc.
!
!     And something like this for a multi-component file:
!              T        C001        V001    C001_001    C002_001
!     where the concentrations we want are C001_001, C002_001, etc.  That is,
!     skip C001 for multi-component runs.
!
        read(smp_file, 99999, iostat=ierr) line ! header line
        if (ierr /= 0) goto 95
!
!---- Here's where to add blocks for future versions of the header format
!
! else if smp_version == 0.2) then

  else ! if smp_version not found above, then we don't know what to do with it.

     write(*,*) "Error: Unknown version number of SMP file format.  First line:"
     write(*,*) trim(line)
     stop

  end if
!
!---- If no user-specified species were given, set identical to SMP species
!
  if (num_usr_species == -1) then ! flag meaning "output all species": fix that
     if (debug) print*,"Filling usr_species with smp_species: output all species"
     num_usr_species = num_smp_species
     allocate( usr_species(num_usr_species) )
     usr_species = smp_species    ! identical
  endif
!
!---- If no user-specified conversion factors were given, set defaults
!
  if (num_usr_conv == -1) then      ! flag meaning "use internal lookups"
     if (debug) print*,"Filling usr_conv with default for all species"
     num_usr_conv = num_usr_species ! we needed to know num_usr_species first,
     allocate( usr_conv(num_usr_conv) )  ! before we could allocate this
     usr_conv = 0.                  ! flag meaning no user-supplied conv ratios
  endif
!
!---- Figure out which columns of the SMP file (which species) to output.
!
  allocate( out_species(num_smp_species) )
  out_species = .false.           ! fill array with default, write none
  do i = 1, num_smp_species
     do j = 1, num_usr_species
        if (usr_species(j) == smp_species(i)) then
           out_species(i) = .true.
           if (usr_conv(j) /= 0.) then
              smp_conv(i) = usr_conv(j) ! use user-specified
              if (debug) print*,"Species ",trim(usr_species(j)),&
                   " PPM-to-ug/m3 over-ride conversion = ",usr_conv(j)
           end if
           if (debug) print*,"  Matched user-to-smp species ", &
                trim(usr_species(j))," to ",smp_species(i)
        endif
     end do
  end do
  if (count(out_species) == 0 .or. count(out_species) /= num_usr_species) then
     write(*,*) " Error: User-requested species not found in this file."
     write(*,*)   "   User species = ",usr_species
     write(*,*)   "   SMP species  = ",smp_species
     write(*,*)   "   SMP filename = ",trim(smp_filename)
     stop
  endif
!
!---- Open the output POSTFILE(s)
!
  allocate( postfile(num_smp_species) ) ! these are the LUNs for each POSTFILE
  do i = 1, num_smp_species
     if (out_species(i)) then
        postfile(i) = 100 + i           ! the LUN for this file
        if (num_usr_species > 1) then
           j = index(postfilename,".",.true.) ! find last ".", for .ext
           if (j == 0) then
              j = len_trim(postfilename)+1 ! use end if no .ext
              ext = ".pst"
           else
              ext = postfilename(j:len_trim(postfilename))
           end if
           open(postfile(i), file=postfilename(1:j-1) // "_" // &
                trim(usr_species(i)) // trim(ext), status='unknown', err=92)
           if (verbose) write(*,*) "Creating ",postfilename(1:j-1) &
                // "_" // trim(usr_species(i)) // trim(ext)
        else ! if there's only one output file, use the given filename verbatim
           open(postfile(i), file=postfilename, status='unknown',err=92)
           if (verbose) write(*,*) "Creating ",trim(postfilename)
        endif
        call flush(6) ! flush the standard output
     end if
  end do
!
!---- write a header to each POSTFILE
!
10 format(8a)         ! the formats used in the POSTFILE header
11 format(a,67x,6a)
12 format(a,i6,a)

  call date_and_time(datestr,timestr) ! current time, for the POSTFILE header

  do i = 1, num_smp_species
     if (out_species(i)) then
        write(postfile(i), 10) "* AERMOD ( 12345): ", smp_filename(1:73),  &
             datestr(3:4),"/",datestr(5:6),"/",datestr(7:8)
        write(postfile(i), 11) "* MODELING OPTIONS USED: ",                &
             timestr(1:2),":",timestr(3:4),":",timestr(5:6)
        ! I believe this next line is enough to keep AERMOD happy:
        write(postfile(i), 10) "* NonDFAULT CONC                                              ELEV"
        write(AVEstr,'(i3,"-HR")') nint(out_time*num_out_per_hr/out_time_units)
        write(postfile(i), 10) "*         POST/PLOT FILE OF CONCURRENT ",  &
             AVEstr, "-HR VALUES FOR SOURCE GROUP: ",GroupID
        write(postfile(i), 12) "*         FOR A TOTAL OF",num_rec," RECEPTORS."
        write(postfile(i), 10) "*         FORMAT: (3(1X,F13.5),3(1X,F8.2),2X,",&
             "A6,2X,A8,2X,I8.8,2X,A8)"
        write(postfile(i), 10) "*        X             Y      AVERAGE CONC", &
             "    ZELEV    ZHILL    ZFLAG    AVE     GRP       DATE     NET ID"
        write(postfile(i), 10) "* ____________  ____________  ____________", &
             "   ______   ______   ______  ______  ________  ________  ________"
     end if
  end do
!
!---- BEGIN MAIN LOOP THROUGH SMP FILE data
!
! Single-compoment:
! Allocation size is 1 (Time) plus [C + V + T] for each receptor
!
! Multi-component:
! Allocation size is 1 (Time) plus [C + V + T + number of species] for each rec
!
  if (smp_filetype == "single") then ! which means num_smp_species = 1
     num_skip = 2 ! use Cxxx, skip Vxxx and Txxx
  else if (smp_filetype == "multi") then
     num_skip = 3 ! skip the Cxxx, Vxxx, and Txxx, use Cnnn_xxx 
  else
     write(*,*) "Error: smp_filetype not as expected."
     stop
  end if

! V means "values"

  if (debug) then
     print*,"Number of receptors = ",num_rec
     print*,"Allocating for reading:", 1+num_rec*(num_skip + num_smp_species)
  end if
  allocate( V(1 + num_rec*(num_skip + num_smp_species)) )
  allocate( tmp(1 + num_rec*(num_skip + num_smp_species)) )
!
!---- AERMOD POSTFILE output format:
!
20 format(3(1X,F13.5),3(1X,F8.2),2X,A6,2X,A8,2X,I8.8,2X,A8) ! as of version 12345
!
!---- Alternate for numbers too small to write in f13.5
!
21 format(2(1X,F13.5),1X,G13.5,3(1X,F8.2),2X,A6,2X,A8,2X,I8.8,2X,A8) 

  read(smp_file, 99999, iostat=ierr) line
  if (ierr /= 0) goto 94
  read(line,*) V(1)
  last_time = V(1) - out_time * (num_out_per_hr+1)
  last_time = 0
  if (debug) print*,"BEGIN loop through smp_file."

  do while (ierr == 0)

     V = 0. ! initialize to zero, so we can sum
     if (debug) write(*,'(a,$)') "  Reading timestamp "
     do j = 1, num_out_per_hr
        read(line,*,iostat=ierr2) &
             (tmp(i), i = 1,1+num_rec*(num_skip+num_smp_species))
        if (ierr2 /= 0) goto 95
        V(1) = tmp(1)           ! keep last time-stamp
        if (debug) write(*,'(f12.0,$)') V(1)
        do i = 2,1+num_rec*(num_skip+num_smp_species)
           V(i) = V(i) + tmp(i) ! sum of all values found for this hour
        end do
        read(smp_file, 99999, iostat=ierr) line ! read next line for this hour
        if (ierr == -1) goto 80 ! end of file
        if (ierr /= 0) goto 94
     end do ! do j = 1, num_out_per_hr
     if (debug) write(*,*) ! CR on the timestamps found
     do i = 2,1+num_rec*(num_skip+num_smp_species)
        V(i) = V(i) / num_out_per_hr
     end do
!
!---- Detect missing output timesteps
!
     if (V(1) /= last_time + out_time * num_out_per_hr) goto 99
!
!---- Calculate every time step based on the start time, and V(1) aka T
!
     yr = byr; mo = bmo; dy = bdy; hr = bhr  ! byr etc already convert to LST
     hr = hr + nint(V(1)/(out_time * num_out_per_hr))
     call legal_timestamp(yr,mo,dy,hr,24)
     if (debug) write(*,'(a,i4,"-",i2.2,"-",i2.2,"_",i2.2," = ",f16.0," sec.")') &
          "  Output at ", yr,mo,dy,hr, V(1)
!
!---- Write this timestep's data to each POSTFILE
!
     do i = 1, num_smp_species
        if (out_species(i)) then
           do n = 1, num_rec
!
!---- k essentially maps species number i to column number of V(). 
!     Think of it as V(k(i)).
!
              if (smp_filetype == "single") then ! num_smp_species = i = 1
                 k = 1 + (n-1)*(num_skip+num_smp_species) + i
              else if (smp_filetype == "multi") then
                 k = 1 + (n-1)*(num_skip+num_smp_species) + num_skip + i
              else
                 k = 0 ! just so bounds checking will catch errors
              end if
!
!---- Finally, write the actual data to the POSTFILE

              if (smp_conv(i) * V(k) > 1.e-5) then
!                                             X    Y    
                 write(postfile(i),20,err=97) X(n),Y(n),   &
!                  AVERAGE CONC
                   smp_conv(i) * V(k),                  &
!                  ZELEV   ZHILL ZFLAG AVE     GRP        
                   elev(n),Z(n), Z(n), AVEstr, GroupID, &
!                  DATE                                NET ID
                   yr2(yr)*1000000+mo*10000+dy*100+hr
              else
!                                             X    Y    
                 write(postfile(i),21,err=97) X(n),Y(n),   &
!                  AVERAGE CONC
                   smp_conv(i) * V(k),                  &
!                  ZELEV   ZHILL ZFLAG AVE     GRP        
                   elev(n),Z(n), Z(n), AVEstr, GroupID, &
!                  DATE                                NET ID
                   yr2(yr)*1000000+mo*10000+dy*100+hr
              end if
           end do ! do n = 1, num_rec
        end if    ! if (out_species(i)) then
     end do       ! do i = 1, num_smp_species
    
     last_time = V(1)

  end do          ! do while (ierr == 0)
80 if (debug) then
     print*
     print*,"END   loop through smp_file."
  end if
!
!----Done, close and deallocate everything
!
  close(smp_file)             ! loc file already closed, above
  do i = 1, num_usr_species
     close(postfile(i))
  end do

  if (allocated( smp_species )) deallocate( smp_species )
  if (allocated( usr_species )) deallocate( usr_species )
  if (allocated( out_species )) deallocate( out_species )
  if (allocated( smp_conv    )) deallocate( smp_conv    )
  if (allocated( usr_conv    )) deallocate( usr_conv    )
  if (allocated( postfile    )) deallocate( postfile    )
  if (allocated( V           )) deallocate( V           )
  if (allocated( X           )) deallocate( X           )
  if (allocated( Y           )) deallocate( Y           )
  if (allocated( Z           )) deallocate( Z           )
  if (allocated( elev        )) deallocate( elev        )

  if (verbose) write(*,*) "smp2postfile finished normally."
  stop

90 write(*,*) "Error opening SAMPLER HISTORY (*.SMP) file ",trim(smp_filename)
  stop

92 write(*,*) "Error opening Output POSTIFLE" ,trim(postfilename)
  stop

94 write(*,*) "Error reading SAMPLER HISTORY (*.SMP) file ",trim(smp_filename)
  stop

95 write(*,*) "Error parsing SAMPLER HISTORY (*.SMP) file ",trim(smp_filename)
   write(*,*) "  Offending line is:"
   write(*,*) trim(line)
  stop

97 write(*,*) "Error writing to Output POSTIFLE ",trim(postfilename)
  stop

99 write(*,*) "Error reading the SAMPLER HISTORY (*.SMP) file ",trim(smp_filename)
   write(*,*) "   Expected time:",last_time + out_time * num_out_per_hr
   write(*,*) "   Found    time:",V(1)
  stop

end program smp2postfile
!
!*************************************************************************
!
subroutine usage
  write(*,*)
  write(*,*) "This program reads SCICHEM Sampler History files (concentrations)"
  write(*,*) "  and writes AERMOD-like POSTFILEs." 
  write(*,*)
  write(*,*) "Usage: smp2postfile [options]"
  write(*,*)
  write(*,*) "Options:"
  write(*,*) " -i, --smp-file filename    Input SAMPLER HISTORY (*.SMP) file name, required."
  write(*,*) " -o, --postfile filename    Output POSTFILE filename or basename, optional."
  write(*,*) "                            For multiple species, the name will be appended"
  write(*,*) "                            before the extension, e.g. filename_NO2.pst."
  write(*,*) "                            Default: scichem.pst"
  write(*,*) " -s, --species NO,NO2       Comma-separated list of chemical SPECIES to process,"
  write(*,*) "                            (no spaces allowed in list)."
  write(*,*) "                            Default: process ALL species found in SMP file."
  write(*,*) " -c, --conversion X,Y       Comma-separated list of conversion factors to convert"
  write(*,*) "                            from PPM (SCICHEM standard for multi-compoentent runs)"
  write(*,*) "                            of kg/m3 (SCICHEM standard for single-compoentent runs)"
  write(*,*) "                            to ug/m3 (micrograms per cubic meter, AERMOD standard)."
  write(*,*) "                            Must be in the same order as -s species, or as the "
  write(*,*) "                            species found in SMP file if no -s given.  "
  write(*,*) "                            Default: use internal look-up tables."
  write(*,*) " -g, --group                Group ID to write inside POSTFILE(s)."
  write(*,*) " -d, --debug                Print verbose debugging info to screen."
  write(*,*) " -q, --quiet                No output to the screen, please."
  write(*,*) " -v, --version              Print version info and quit."
  write(*,*) " -h, --help                 Print this help message and quit."
  write(*,*) 
  write(*,*) "Examples:"
  write(*,*) 
  write(*,*) "smp2post -i abc.smp -s NO,NO2 -g ABC"
  write(*,*) "  Will create abc_NO.pst and abc_NO2.pst, with the SRCGROUP name 'ABC'."
  write(*,*) 
  write(*,*) "smp2post -i abc.smp -o basename.pst"
  write(*,*) "  Will create basename_NO.pst and basename_NO2.pst, etc."
  write(*,*) 
  write(*,*) "smp2post -i abc.smp -s NO,NO2 -c 1,1"
  write(*,*) "  Will create abc_NO.pst and abc_NO2.pst, in PPM."

  stop
end subroutine usage
!
!*************************************************************************
!
subroutine legal_timestamp(iy,im,id,ih,iMaxHr)
!
!-----Makes sure the time is a legal time, either 0-23 (if iMaxHr = 23) 
!     or 1-24 (if iMaxHr = 24).  
!
  do while (ih.lt.0)
     ih = ih + 24
     id = id - 1
     if (id.lt.1) then
        im = im - 1
        if (im.eq.0) then
           im = 12
           iy = iy - 1
           if (iy.lt.0) iy = iy + 100                 ! May be 2-digit year
        end if
        id = id + iDaysInMth(im,iy)
     end if
  end do

  do while (ih.gt.iMaxHr)
     ih = ih - 24
     id = id + 1
     if (id.gt.iDaysInMth(im,iy)) then
        im = im + 1
        if (im.gt.12) then
           im = 1
           iy = iy + 1
           if (iy.eq.100) iy = 0                      ! May be 2-digit year
        end if
        id = 1
     end if
  end do
  return

end subroutine legal_timestamp
!
!*************************************************************************
!
integer function yr2(yr4)
  integer :: yr4
 
  if (yr4 < 2000) then
     yr2 = yr4 - 1900
  else
     yr2 = yr4 - 2000
  endif

  return
end function yr2
!
!*************************************************************************
!
integer function iDaysInMth(im,iy)

  INTEGER mon(12), iy,im
  data mon/31,28,31,30,31,30,31,31,30,31,30,31/
  
  if((mod(iy,4).eq.0 .or. iy.eq.0) .and. im.eq.2) then ! leap year
     iDaysInMth = 29
  else
     iDaysInMth = mon(im)
  end if
  
  return
end function iDaysInMth
!
!*************************************************************************
!
subroutine get_word(line,delimiter,which_word,word)

  character (len=*) :: line       ! input string
  character (len=*) :: delimiter  ! input delimiter (probably space)
  integer           :: which_word ! which word to return, 1 for 1st, etc.
  character (len=*) :: word       ! output string

  integer           :: i,j, this_word

  this_word = 0
  i = 1

  do while (this_word < which_word)
     if (line(i:i) == delimiter) then
        do while (line(i:i) == delimiter .and. i < len_trim(line))
           i = i + 1               ! advance to next non-space
        end do
     end if
     ! line(i:i) must now be the beginnig of the 1st word
     j = i + 1 ! next character
     do while (line(j:j) /= delimiter .and. j < len_trim(line))
        j = j + 1               ! advance to end of this word
     end do
     this_word = this_word + 1
     if (this_word == which_word) then
        word = line(i:j)
        return
     end if
     i = j + 1 ! advance to next 
  end do
  
end subroutine get_word
!
!*************************************************************************
!
subroutine PPM2ugm3(Species,factor)
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
     factor = 1.                 ! default to no conversion
  else
     factor = 1000. * MW / 24.45 ! 1000 is millegrams to micrograms
  endif

  return

end subroutine PPM2ugm3

!*******************************************************************************

SUBROUTINE clower(line)

IMPLICIT NONE

CHARACTER(*), INTENT( INOUT ) :: line

INTEGER i

DO i = 1,LEN(line)
  IF( line(i:i) < 'A' )CYCLE
  IF( line(i:i) > 'Z' )CYCLE
  line(i:i) = CHAR(ICHAR(line(i:i)) + 32)
END DO

RETURN
END
!
!*************************************************************************
!
