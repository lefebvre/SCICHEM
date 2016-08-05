SUBROUTINE InitMPI( id, nprocs, lMPI )
!DEC$ ATTRIBUTES DLLEXPORT :: InitMPI

USE localmpi
USE mpi_fi

IMPLICIT NONE

INTEGER id, nprocs
LOGICAL lMPI

CALL MPI_INIT( ierr )
CALL MPI_COMM_RANK( MPI_COMM_WORLD, myid, ierr )
CALL MPI_COMM_SIZE( MPI_COMM_WORLD, numprocs, ierr )

IF( numprocs < 0 .OR. isSerial )THEN
  useMPI   = .FALSE.
  numprocs = 1
ELSE IF( numprocs > 0 )THEN
  useMPI   = .TRUE.
  WRITE(*,*)'Process ', myid, ' of ', numprocs, ' is alive'
  CALL FLUSH(6)
END IF

id     = myid
nprocs = numprocs
lMPI   = useMPI

RETURN
END

!=======================================================================

SUBROUTINE CheckStatMPI( )

USE localmpi
USE mpi_fi


IMPLICIT NONE

INTEGER alloc_stat
INTEGER i, nsize

!CALL MPI_ALLGATHER (stat, 1, stype, rbuf, rcount, rtype, comm, ierr)

RETURN
END

!=======================================================================

SUBROUTINE SetUpPrjMPI( error )

!DEC$ ATTRIBUTES DLLEXPORT :: SetUpPrjMPI

USE localmpi
USE mpi_fi
USE message_fd

IMPLICIT NONE

TYPE ( messageT  ) error

IF( myid /= 0 )useMPI = .FALSE.

CALL MPI_BCAST( useMPI, 1, MPI_LOGICAL, 0, MPI_COMM_WORLD, ierr )
IF( ierr /= MPI_SUCCESS )THEN
  error%iParm = 999
  error%routine = 'SetUpPrjMPI'
  WRITE(error%aString,*)'Error in useMPI BCAST from process id = ',myid
  error%bString = 'Setting useMPI to false'
  useMPI = .FALSE.
END IF

RETURN
END
!=======================================================================

SUBROUTINE InitToolMPI( useMPIIn,myidIn,nProcs,ProjectName,error )

!DEC$ ATTRIBUTES DLLEXPORT :: InitToolMPI

USE localmpi
USE mpi_fi
USE charT_fd
USE files_fi
USE message_fd

IMPLICIT NONE

LOGICAL          :: useMPIIn
INTEGER          :: myidIn
INTEGER          :: nProcs
TYPE( char128T ) :: projectName
TYPE( messageT ) :: error


INTEGER alloc_stat

useMPI   = useMPIIn
myid     = myidIn
numprocs = nProcs

IF( isSerial )RETURN

ALLOCATE( nchunk(numprocs), ipuf_start(numprocs), stat = alloc_stat )
IF( alloc_stat /= 0 )THEN
  error%iParm = 999
  error%routine = 'InitToolMPI'
  error%aString = 'Error in allocating nchunk'
  WRITE(error%bString,*)'alloc_stat = ',alloc_stat
  GO TO 9999
END IF

IF( useMPI )THEN

  CALL MPI_BCAST( ProjectName%string, PATH_MAXLENGTH, MPI_CHARACTER, 0, MPI_COMM_WORLD, ierr )
  IF( ierr /= MPI_SUCCESS )THEN
    error%iParm   = 999
    error%routine = 'InitToolMPI'
    error%aString = 'Error in MPI_BCAST of '//TRIM(ProjectName%string)
    WRITE(error%bString,*)'ierr = ',ierr
    GOTO 9999
  END IF

  !WRITE(*,*)myid,': Call BuildTypesMPI from InitToolMPI'

  CALL BuildTypesMPI( )
  IF( Status%nStat /= MPI_SUCCESS )THEN
    error%iParm   = Status%nStat
    error%routine = Status%sRoutine
    error%aString = Status%sMessage
    error%bString = Status%sInform
    GO TO 9999
  END IF

END IF

9999 CONTINUE
RETURN
END

!=======================================================================

INTEGER FUNCTION AllocTypeMPI( newType,nsize,i )

USE localmpi
USE mpi_fi

IMPLICIT NONE

INTEGER :: newType, nsize, i

INTEGER alloc_stat

ALLOCATE( block_lengths(nsize),disp(nsize),typelist(nsize),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  Status%nStat = eALLOC
  Status%sMessage = 'Error allocating block_lengths'
  WRITE(Status%sInform,*)'Type = ',newType
  GO TO 9999
END IF

i = 0

AllocTypeMPI = Status%nStat

9999 CONTINUE

RETURN
END

!=======================================================================

INTEGER FUNCTION CreateTypeMPI( newType,nsize )

USE localmpi
USE mpi_fi

IMPLICIT NONE

INTEGER :: newType, nsize

INTEGER alloc_stat

base = disp(1)
disp = disp - base

CALL MPI_TYPE_CREATE_STRUCT(nsize, block_lengths, disp, typelist, newType, ierr)
IF( ierr /= MPI_SUCCESS )THEN
  Status%nStat    = eMPI
  Status%sMessage = 'Error from MPI_TYPE_CREATE_STRUCT'
  WRITE(Status%sInform,*)'Type = ',newType
  GO TO 9999
END IF

CALL MPI_TYPE_COMMIT(newType, ierr)
IF( ierr /= MPI_SUCCESS )THEN
  Status%nStat    = eMPI
  Status%sMessage = 'Error from MPI_TYPE_COMMIT'
  WRITE(Status%sInform,*)'Type = ',newType
  GO TO 9999
END IF

DEALLOCATE(block_lengths, disp, typelist, STAT=alloc_stat)
IF( alloc_stat /= 0 )THEN
  Status%nStat    = eALLOC
  Status%sMessage = 'Error deallocating block_lengths '
  WRITE(Status%sInform,*)'Type = ',newType
  GO TO 9999
END IF

CreateTypeMPI = Status%nStat

9999 CONTINUE

RETURN
END

!=======================================================================

INTEGER FUNCTION GetAddressMPI_I( n,i )

USE localmpi
USE mpi_fi

IMPLICIT NONE

INTEGER :: n, i

i = i + 1

CALL MPI_GET_ADDRESS( n,disp(i),ierr )
IF( ierr /= MPI_SUCCESS )THEN
  Status%nStat = eMPI
  Status%sMessage = 'Error from MPI_Get_address(I)'
END IF

block_lengths(i) = 1
typelist(i)      = MPI_INTEGER
GetAddressMPI_I  = Status%nStat

RETURN
END

!=======================================================================

INTEGER FUNCTION GetAddressMPI_R( r,i )

USE localmpi
USE mpi_fi

IMPLICIT NONE

REAL    :: r
INTEGER :: i

i = i + 1

CALL MPI_GET_ADDRESS( r,disp(i),ierr )
IF( ierr /= MPI_SUCCESS )THEN
  Status%nStat = eMPI
  Status%sMessage = 'Error from MPI_Get_address(R)'
END IF

block_lengths(i) = 1
typelist(i)      = MPI_REAL
GetAddressMPI_R  = Status%nStat

RETURN
END

!=======================================================================

INTEGER FUNCTION GetAddressMPI_L( lgl,i )

USE localmpi
USE mpi_fi

IMPLICIT NONE

LOGICAL :: lgl
INTEGER :: i

i = i + 1

CALL MPI_GET_ADDRESS( lgl,disp(i),ierr )
IF( ierr /= MPI_SUCCESS )THEN
  Status%nStat = eMPI
  Status%sMessage = 'Error from MPI_Get_address(L)'
END IF

block_lengths(i) = 1
typelist(i)      = MPI_LOGICAL
GetAddressMPI_L  = Status%nStat

RETURN
END

!=======================================================================

INTEGER FUNCTION GetAddressMPI_S( String,i )

USE localmpi
USE mpi_fi

IMPLICIT NONE

CHARACTER(*) :: String
INTEGER      :: i

i = i + 1

CALL MPI_GET_ADDRESS( String,disp(i),ierr )
IF( ierr /= MPI_SUCCESS )THEN
  Status%nStat = eMPI
  Status%sMessage = 'Error from MPI_Get_address(S)'
END IF

block_lengths(i) = LEN(String)
typelist(i)      = MPI_CHARACTER
GetAddressMPI_S  = Status%nStat

RETURN
END

!=======================================================================

SUBROUTINE BuildTypesMPI()

USE localmpi
USE mpi_fi
USE chem_fi
USE StepMC_mpi_fi

IMPLICIT NONE

TYPE( ChemSpecies_str ) :: ChemSpecies

INTEGER nsize, i

INTEGER, EXTERNAL :: AllocTypeMPI
INTEGER, EXTERNAL :: CreateTypeMPI
INTEGER, EXTERNAL :: GetAddressMPI_I
INTEGER, EXTERNAL :: GetAddressMPI_R
INTEGER, EXTERNAL :: GetAddressMPI_L
INTEGER, EXTERNAL :: GetAddressMPI_S

Status%nStat    = 0
Status%sRoutine = 'BuildTypesMPI'
Status%sInform  = ''
Status%sAction  = ''
Status%sMessage = ''

!--- Build a derived datatype for MPI status

n_int  = 1
n_char = 4
nsize  = n_int + n_char

IF( AllocTypeMPI(Status_MPI,nsize,i  ) /= 0 )GO TO 9999
IF( GetAddressMPI_I(Status%nStat,i   ) /= 0 )GO TO 9999
IF( GetAddressMPI_S(Status%sRoutine,i) /= 0 )GO TO 9999
IF( GetAddressMPI_S(Status%sMessage,i) /= 0 )GO TO 9999
IF( GetAddressMPI_S(Status%sInform,i ) /= 0 )GO TO 9999
IF( GetAddressMPI_S(Status%sAction,i ) /= 0 )GO TO 9999
IF( CreateTypeMPI(Status_MPI,nsize   ) /= 0 )GO TO 9999

!--- Build a derived datatype for ChemMCHead_mpi_str
!LOGICAL  ldyn         !Dynamics
!LOGICAL  lwash        !Wash
!INTEGER  flag         !Control flag
!INTEGER  cUnits       !Concentration units type
!INTEGER  eUnits       !Emission units type
!INTEGER  kUnits       !Reaction rate concentration units type
!INTEGER  kConv        !Reaction rate conversion factor type
!INTEGER  eConvType    !Emissions conversion factor type
!REAL     eConv        !Emissions units conversion factor
!REAL     tConv        !Reaction time units conversion factor
!REAL     rtol         !Relative tolerance
!LOGICAL  thermal      !Flag for exo/endothermic reactions
!---- Multicomponent species
!INTEGER nSpecies     !Total No. of component species
!---- Multicomponent reactions
!INTEGER nReactions   !No. of reactions
!INTEGER nZenith      !No. of zenith angles for photochemical rate data
!---- Pointers for "star" species
!INTEGER  nStar        !No. of non-linear overlap species
!---- Pointers for fast/slow species
!INTEGER nFast         !No. of fast species
!INTEGER nSlow         !No. of slow species
!INTEGER nParticle     !No. of particle species
!INTEGER nEquilibrium  !No. of equilibrium species
!---- Pointers for fast/slow reactions
!INTEGER nFastReact   !No. of fast reactions
!INTEGER nSlowReact   !No. of slow reactions
!INTEGER nEquilReact  !No. of equilibrium reactions
!INTEGER nPhotoReact  !No. of photochemical reactions
!---- Equilibrium solver arrays
!INTEGER  nSolveEq     !No. of species for nonlinear equilibrium solver
!INTEGER  nLinearEq    !No. of species that can be eliminated
!INTEGER  nDirectEq    !No. of species that can be solved directly
!INTEGER  nSubstEq     !No. of species that can be substituted
!INTEGER  imax   !Offset pointer for puff list
!LOGICAL  lBalance
!INTEGER  nRxnData
!INTEGER  nRxnNp
!INTEGER  nQLine

n_lgl  = 4
n_real = 3
n_int  = 26
nsize  = n_lgl + n_real + n_int

IF( AllocTypeMPI(ChemMCHead_MPI,nsize      ,i ) /= 0 )GO TO 9999
IF( GetAddressMPI_L(ChemMCHead%ldyn        ,i ) /= 0 )GO TO 9999
IF( GetAddressMPI_L(ChemMCHead%lwash       ,i ) /= 0 )GO TO 9999
IF( GetAddressMPI_I(ChemMCHead%flag        ,i ) /= 0 )GO TO 9999
IF( GetAddressMPI_I(ChemMCHead%cUnits      ,i ) /= 0 )GO TO 9999
IF( GetAddressMPI_I(ChemMCHead%eUnits      ,i ) /= 0 )GO TO 9999
IF( GetAddressMPI_I(ChemMCHead%kUnits      ,i ) /= 0 )GO TO 9999
IF( GetAddressMPI_I(ChemMCHead%kConv       ,i ) /= 0 )GO TO 9999
IF( GetAddressMPI_I(ChemMCHead%eConvType   ,i ) /= 0 )GO TO 9999
IF( GetAddressMPI_R(ChemMCHead%eConv       ,i ) /= 0 )GO TO 9999
IF( GetAddressMPI_R(ChemMCHead%tConv       ,i ) /= 0 )GO TO 9999
IF( GetAddressMPI_R(ChemMCHead%rtol        ,i ) /= 0 )GO TO 9999
IF( GetAddressMPI_L(ChemMCHead%thermal     ,i ) /= 0 )GO TO 9999
IF( GetAddressMPI_I(ChemMCHead%nSpecies    ,i ) /= 0 )GO TO 9999
IF( GetAddressMPI_I(ChemMCHead%nReactions  ,i ) /= 0 )GO TO 9999
IF( GetAddressMPI_I(ChemMCHead%nZenith     ,i ) /= 0 )GO TO 9999
IF( GetAddressMPI_I(ChemMCHead%nStar       ,i ) /= 0 )GO TO 9999
IF( GetAddressMPI_I(ChemMCHead%nFast       ,i ) /= 0 )GO TO 9999
IF( GetAddressMPI_I(ChemMCHead%nSlow       ,i ) /= 0 )GO TO 9999
IF( GetAddressMPI_I(ChemMCHead%nParticle   ,i ) /= 0 )GO TO 9999
IF( GetAddressMPI_I(ChemMCHead%nEquilibrium,i ) /= 0 )GO TO 9999
IF( GetAddressMPI_I(ChemMCHead%nFastReact  ,i ) /= 0 )GO TO 9999
IF( GetAddressMPI_I(ChemMCHead%nSlowReact  ,i ) /= 0 )GO TO 9999
IF( GetAddressMPI_I(ChemMCHead%nEquilReact ,i ) /= 0 )GO TO 9999
IF( GetAddressMPI_I(ChemMCHead%nPhotoReact ,i ) /= 0 )GO TO 9999
IF( GetAddressMPI_I(ChemMCHead%nSolveEq    ,i ) /= 0 )GO TO 9999
IF( GetAddressMPI_I(ChemMCHead%nLinearEq   ,i ) /= 0 )GO TO 9999
IF( GetAddressMPI_I(ChemMCHead%nDirectEq   ,i ) /= 0 )GO TO 9999
IF( GetAddressMPI_I(ChemMCHead%nSubstEq    ,i ) /= 0 )GO TO 9999
IF( GetAddressMPI_I(ChemMCHead%imax        ,i ) /= 0 )GO TO 9999
IF( GetAddressMPI_L(ChemMCHead%lBalance    ,i ) /= 0 )GO TO 9999
IF( GetAddressMPI_I(ChemMCHead%nRxnData    ,i ) /= 0 )GO TO 9999
IF( GetAddressMPI_I(ChemMCHead%nRxnNp      ,i ) /= 0 )GO TO 9999
IF( GetAddressMPI_I(ChemMCHead%nQLine      ,i ) /= 0 )GO TO 9999
IF( CreateTypeMPI(ChemMCHead_MPI,nsize        ) /= 0 )GO TO 9999

!--- Build a derived datatype for ChemSpecies_mpi_str
  !CHARACTER(16) :: name
  !CHARACTER(16) :: classAux        !Character string for class aux type and units
  !INTEGER       :: class
  !INTEGER       :: ID            !Species number
  !INTEGER       :: eqID          !ID for equilibrium species solver
  !LOGICAL       :: lstar         !Flag for overlap concentration
  !REAL          :: ambient       !Ambient concentration
  !REAL          :: tol           !Absolute tolerance
  !LOGICAL       :: ldos          !Dosage flag
  !LOGICAL       :: ldep          !Deposition flag
  !REAL          :: vdep          !Deposition velocity
  !REAL          :: scav          !Scavenging coefficient
  !REAL          :: mw            !Molecular weight
  !REAL          :: taudry         !
  !REAL          :: tauwet         !
  !REAL          :: nit            ! Number of nitrogen molecules for balance
  !REAL          :: lim            ! Volume limit for species
!---- Working storage
  !REAL    mass
  !REAL    conc
  !REAL    amb
  !REAL    msav
  !REAL    csav
  !REAL    mass2     !Used for overlap (jpuf)
  !REAL    conc2

n_char = 2
n_lgl  = 3
n_real = 16
n_int  = 3
nsize  = n_char + n_lgl + n_real + n_int

!WRITE(*,*)'Build ChemSpecies_MPI'

IF( AllocTypeMPI(ChemSpecies_MPI,nsize       ,i ) /= 0 )GO TO 9999
IF( GetAddressMPI_S(ChemSpecies%name         ,i ) /= 0 )GO TO 9999
IF( GetAddressMPI_S(ChemSpecies%classAux     ,i ) /= 0 )GO TO 9999
IF( GetAddressMPI_I(ChemSpecies%class        ,i ) /= 0 )GO TO 9999
IF( GetAddressMPI_I(ChemSpecies%ID           ,i ) /= 0 )GO TO 9999
IF( GetAddressMPI_I(ChemSpecies%eqID         ,i ) /= 0 )GO TO 9999
IF( GetAddressMPI_L(ChemSpecies%lstar        ,i ) /= 0 )GO TO 9999
IF( GetAddressMPI_R(ChemSpecies%ambient      ,i ) /= 0 )GO TO 9999
IF( GetAddressMPI_R(ChemSpecies%tol          ,i ) /= 0 )GO TO 9999
IF( GetAddressMPI_L(ChemSpecies%ldos         ,i ) /= 0 )GO TO 9999
IF( GetAddressMPI_L(ChemSpecies%ldep         ,i ) /= 0 )GO TO 9999
IF( GetAddressMPI_R(ChemSpecies%vdep         ,i ) /= 0 )GO TO 9999
IF( GetAddressMPI_R(ChemSpecies%scav         ,i ) /= 0 )GO TO 9999
IF( GetAddressMPI_R(ChemSpecies%mw           ,i ) /= 0 )GO TO 9999
IF( GetAddressMPI_R(ChemSpecies%taudry       ,i ) /= 0 )GO TO 9999
IF( GetAddressMPI_R(ChemSpecies%tauwet       ,i ) /= 0 )GO TO 9999
IF( GetAddressMPI_R(ChemSpecies%nit          ,i ) /= 0 )GO TO 9999
IF( GetAddressMPI_R(ChemSpecies%lim          ,i ) /= 0 )GO TO 9999
IF( GetAddressMPI_R(ChemSpecies%mass         ,i ) /= 0 )GO TO 9999
IF( GetAddressMPI_R(ChemSpecies%conc         ,i ) /= 0 )GO TO 9999
IF( GetAddressMPI_R(ChemSpecies%amb          ,i ) /= 0 )GO TO 9999
IF( GetAddressMPI_R(ChemSpecies%msav         ,i ) /= 0 )GO TO 9999
IF( GetAddressMPI_R(ChemSpecies%csav         ,i ) /= 0 )GO TO 9999
IF( GetAddressMPI_R(ChemSpecies%mass2        ,i ) /= 0 )GO TO 9999
IF( GetAddressMPI_R(ChemSpecies%conc2        ,i ) /= 0 )GO TO 9999
IF( CreateTypeMPI(ChemSpecies_MPI,nsize         ) /= 0 )GO TO 9999

!--- Build a derived datatype for ChemAqAer_mpi_str
!LOGICAL        :: laero
!INTEGER        :: naerp
!INTEGER        :: nsec_aer
!CHARACTER(128) :: file_imc
n_lgl  = 1
n_int  = 2
n_char = 1
nsize  = n_lgl + n_int + n_char

IF( AllocTypeMPI(ChemAqArHd_MPI,nsize  ,i ) /= 0 )GO TO 9999
IF( GetAddressMPI_L(ChemAqArHd%laero   ,i ) /= 0 )GO TO 9999
IF( GetAddressMPI_I(ChemAqArHd%naerp   ,i ) /= 0 )GO TO 9999
IF( GetAddressMPI_I(ChemAqArHd%nsec_aer,i ) /= 0 )GO TO 9999
IF( GetAddressMPI_S(ChemAqArHd%file_imc,i ) /= 0 )GO TO 9999
IF( CreateTypeMPI(ChemAqArHd_MPI ,nsize   ) /= 0 )GO TO 9999

Status%sRoutine = ""

9999 CONTINUE

RETURN
END

!=======================================================================

SUBROUTINE BuildReactTypeMPI( )

USE localmpi
USE mpi_fi
USE scipuff_fi
USE chem_fi
USE chem_mpi_fi, ONLY: ID_K_RAD
USE StepMC_mpi_fi

IMPLICIT NONE

INTEGER nsize,i
INTEGER alloc_stat
INTEGER nReact

TYPE( StepMCdataT ) :: mc

INTEGER, EXTERNAL :: AllocTypeMPI
INTEGER, EXTERNAL :: CreateTypeMPI
INTEGER, EXTERNAL :: GetAddressMPI_I
INTEGER, EXTERNAL :: GetAddressMPI_R
INTEGER, EXTERNAL :: GetAddressMPI_L
INTEGER, EXTERNAL :: GetAddressMPI_S

Status%nStat    = 0
Status%sRoutine = 'BuildReactTypeMPI'
Status%sInform  = ''
Status%sAction  = ''
Status%sMessage = ''

!--- Build a derived datatype for ChemReact_mpi_str

!INTEGER       :: class                !Reaction class
!INTEGER       :: type  !Reaction type
!REAL, DIMENSION(:), ALLOCATABLE :: data  !Rate coefficient data
!REAL          :: k     !current rate coefficient (set and used during calculation)
!REAL          :: H     !heat of reaction (J/kg) * conc units conversion factor
!REAL          :: Hr    !current H/(rho*cp) => temp rate = Hr * reaction rate
!INTEGER       :: A                    !ID of species A
!INTEGER       :: B                    !ID of species B (0 for linear reaction)
!REAL          :: fB                   !Reactant stoichiometric factor
!INTEGER       :: nP                   !No. of product species
!INTEGER, DIMENSION(:), ALLOCATABLE :: P   !List of product ID's
!REAL, DIMENSION(:), ALLOCATABLE :: fP  !List of product stoichiometric factors

nreact = chem%nReactions

ALLOCATE( ChemRxn(nreact),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  Status%nStat    = eAlloc
  Status%sInform  = ''
  Status%sAction  = ''
  Status%sMessage = ''
  GO TO 9999
END IF

n_int  = 6
n_real = 2
nsize  = n_real + n_int

IF( AllocTypeMPI(ChemReact_MPI,nsize,i ) /= 0 )GO TO 9999
IF( GetAddressMPI_I(ChemRxn(1)%class,i ) /= 0 )GO TO 9999
IF( GetAddressMPI_I(ChemRxn(1)%type ,i ) /= 0 )GO TO 9999
IF( GetAddressMPI_R(ChemRxn(1)%k    ,i ) /= 0 )GO TO 9999
IF( GetAddressMPI_I(ChemRxn(1)%A    ,i ) /= 0 )GO TO 9999
IF( GetAddressMPI_I(ChemRxn(1)%B    ,i ) /= 0 )GO TO 9999
IF( GetAddressMPI_R(ChemRxn(1)%fB   ,i ) /= 0 )GO TO 9999
IF( GetAddressMPI_I(ChemRxn(1)%nP   ,i ) /= 0 )GO TO 9999
IF( GetAddressMPI_I(ChemRxn(1)%nData,i ) /= 0 )GO TO 9999
IF( CreateTypeMPI(ChemReact_MPI ,nsize ) /= 0 )GO TO 9999


!--- Build a derived datatype for StepMCdat_mpi_str

!--- Add step variables
!INTEGER  ngd, nbd
!REAL t, dt

n_int  = 2
n_real = 2

!--- Add puff_mpi_str
!real xbar,ybar,zbar,det
!real c,cc,ccb,csav
!real sr,szz
!real vol,pres,temp,tdyn,vself
!integer pno,ityp,idtl,naux

n_real = n_real + 15
n_int  = n_int + 4

!--- Add met_mpi_str
!REAL tb,pb,hb,cw,cc,sfac
!REAL prate,prbl,zinv,hp
!REAL us2,ws2,xml,zb1,zruf

n_real = n_real + 15
nsize = n_int + n_real

IF( AllocTypeMPI(StepMCdat_MPI,nsize,i) /= 0 )GO TO 9999

IF( GetAddressMPI_I(mc%ngd     , i    ) /= 0 )GO TO 9999
IF( GetAddressMPI_I(mc%nbd     , i    ) /= 0 )GO TO 9999
IF( GetAddressMPI_R(mc%t       , i    ) /= 0 )GO TO 9999
IF( GetAddressMPI_R(mc%dt      , i    ) /= 0 )GO TO 9999

IF( GetAddressMPI_R(mc%pf%xbar , i    ) /= 0 )GO TO 9999
IF( GetAddressMPI_R(mc%pf%ybar , i    ) /= 0 )GO TO 9999
IF( GetAddressMPI_R(mc%pf%zbar , i    ) /= 0 )GO TO 9999
IF( GetAddressMPI_R(mc%pf%det  , i    ) /= 0 )GO TO 9999
IF( GetAddressMPI_R(mc%pf%c    , i    ) /= 0 )GO TO 9999
IF( GetAddressMPI_R(mc%pf%cc   , i    ) /= 0 )GO TO 9999
IF( GetAddressMPI_R(mc%pf%ccb  , i    ) /= 0 )GO TO 9999
IF( GetAddressMPI_R(mc%pf%csav , i    ) /= 0 )GO TO 9999
IF( GetAddressMPI_R(mc%pf%sr   , i    ) /= 0 )GO TO 9999
IF( GetAddressMPI_R(mc%pf%szz  , i    ) /= 0 )GO TO 9999
IF( GetAddressMPI_R(mc%pf%vol  , i    ) /= 0 )GO TO 9999
IF( GetAddressMPI_R(mc%pf%pres , i    ) /= 0 )GO TO 9999
IF( GetAddressMPI_R(mc%pf%temp , i    ) /= 0 )GO TO 9999
IF( GetAddressMPI_R(mc%pf%tdyn , i    ) /= 0 )GO TO 9999
IF( GetAddressMPI_R(mc%pf%vself, i    ) /= 0 )GO TO 9999
IF( GetAddressMPI_I(mc%pf%pno  , i    ) /= 0 )GO TO 9999
IF( GetAddressMPI_I(mc%pf%ityp , i    ) /= 0 )GO TO 9999
IF( GetAddressMPI_I(mc%pf%idtl , i    ) /= 0 )GO TO 9999
IF( GetAddressMPI_I(mc%pf%naux , i    ) /= 0 )GO TO 9999

IF( GetAddressMPI_R(mc%pm%tb   , i   ) /= 0 )GO TO 9999
IF( GetAddressMPI_R(mc%pm%pb   , i   ) /= 0 )GO TO 9999
IF( GetAddressMPI_R(mc%pm%hb   , i   ) /= 0 )GO TO 9999
IF( GetAddressMPI_R(mc%pm%cw   , i   ) /= 0 )GO TO 9999
IF( GetAddressMPI_R(mc%pm%cc   , i   ) /= 0 )GO TO 9999
IF( GetAddressMPI_R(mc%pm%sfac , i   ) /= 0 )GO TO 9999
IF( GetAddressMPI_R(mc%pm%prate, i   ) /= 0 )GO TO 9999
IF( GetAddressMPI_R(mc%pm%prbl , i   ) /= 0 )GO TO 9999
IF( GetAddressMPI_R(mc%pm%zinv , i   ) /= 0 )GO TO 9999
IF( GetAddressMPI_R(mc%pm%hp   , i   ) /= 0 )GO TO 9999
IF( GetAddressMPI_R(mc%pm%us2  , i   ) /= 0 )GO TO 9999
IF( GetAddressMPI_R(mc%pm%ws2  , i   ) /= 0 )GO TO 9999
IF( GetAddressMPI_R(mc%pm%xml  , i   ) /= 0 )GO TO 9999
IF( GetAddressMPI_R(mc%pm%zb1  , i   ) /= 0 )GO TO 9999
IF( GetAddressMPI_R(mc%pm%zruf , i   ) /= 0 )GO TO 9999

IF( CreateTypeMPI(StepMCdat_MPI,nsize  ) /= 0 )GO TO 9999

Status%sRoutine = ""

9999 CONTINUE

RETURN
END
