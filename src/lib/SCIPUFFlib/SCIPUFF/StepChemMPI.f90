!=======================================================================

SUBROUTINE RunChemSubMPI( projectName,error )
!DEC# ATTRIBUTES DLLEXPORT :: RunChemSubMPI

USE localmpi
USE mpi_fi
USE files_fi
USE charT_fd
USE message_fd

IMPLICIT NONE

TYPE ( messageT  ) error

TYPE( char128T ):: projectName

!WRITE(*,*)'***',myid,'***','In RunChemSubMPI'
CALL InitChemSubMPI( ProjectName )
IF( Status%nStat < 0 )GOTO 9999

DO

  CALL StepChemPuffMPI( 0 )
  IF( Status%nStat < 0 )THEN
    GOTO 9999
  ELSE IF( Status%nStat == sDone )THEN
    EXIT
  END IF

END DO

9998 CONTINUE
RETURN

9999 CONTINUE
error%iParm   = Status%nStat
error%routine = Status%sRoutine
error%aString = Status%sMessage
error%bString = Status%sInform

GOTO 9998

END

!=======================================================================

SUBROUTINE InitChemMainMPI( )

USE localmpi
USE mpi_fi
USE charT_fd
USE message_fd
USE scipuff_fi, ONLY:dynamic,lwash
USE chem_fi
USE AqAer_fi, ONLY: MAX_SP,file_imc

IMPLICIT NONE

INTEGER i,alloc_stat,nReactions
INTEGER nData, nP, nZen, nQLine
INTEGER rNO

REAL   ,          DIMENSION(:)  , ALLOCATABLE :: zenith
DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: rData
REAL   ,          DIMENSION(:,:), ALLOCATABLE :: rfP
INTEGER,          DIMENSION(:,:), ALLOCATABLE :: rnumP
CHARACTER(LEN=1), DIMENSION(:)  , POINTER     :: nmlLines
INTEGER(1),       DIMENSION(:)  , POINTER     :: nmlInts

INTERFACE
  SUBROUTINE RdAqAerMain( file_imc,maxLen,nmlLines )
    CHARACTER(*), INTENT( IN  )             :: file_imc
    INTEGER,      INTENT( OUT )             :: maxLen
    CHARACTER(LEN=1), DIMENSION(:),POINTER  :: nmlLines
  END SUBROUTINE
END INTERFACE

Status%sRoutine = 'InitChemMainMPI'

! Set ChemMCHead values
nReactions = chem%nReactions
nData      = 0
nP         = 0
DO i = 1,nReactions
  nData = MAX( nData,SIZE(chem%reaction(i)%Data) )
  nP    = MAX( nP   ,chem%reaction(i)%nP )
END DO
CALL RdAqAerMain( file_imc,nQLine,nmlLines )

! Copy the chemMC header variables for BCAST
ChemMCHead%ldyn         = dynamic
ChemMCHead%lwash        = lwash
ChemMCHead%flag         = chem%flag
ChemMCHead%cUnits       = chem%cUnits
ChemMCHead%eUnits       = chem%eUnits
ChemMCHead%kUnits       = chem%kUnits
ChemMCHead%kConv        = chem%kConv
ChemMCHead%eConvType    = chem%eConvType
ChemMCHead%eConv        = chem%eConv
ChemMCHead%tConv        = chem%tConv
ChemMCHead%rtol         = chem%rtol
ChemMCHead%thermal      = chem%thermal
ChemMCHead%nSpecies     = chem%nSpecies
ChemMCHead%nReactions   = chem%nReactions
ChemMCHead%nZenith      = chem%nZenith
ChemMCHead%nStar        = chem%nStar
ChemMCHead%nFast        = chem%nFast
ChemMCHead%nSlow        = chem%nSlow
ChemMCHead%nParticle    = chem%nParticle
ChemMCHead%nEquilibrium = chem%nEquilibrium
ChemMCHead%nFastReact   = chem%nFastReact
ChemMCHead%nSlowReact   = chem%nSlowReact
ChemMCHead%nEquilReact  = chem%nEquilReact
ChemMCHead%nPhotoReact  = chem%nPhotoReact
ChemMCHead%nSolveEq     = chem%nSolveEq
ChemMCHead%nLinearEq    = chem%nLinearEq
ChemMCHead%nDirectEq    = chem%nDirectEq
ChemMCHead%nSubstEq     = chem%nSubstEq
ChemMCHead%imax         = chem%imax
ChemMCHead%lBalance     = chem%lBalance
ChemMCHead%nRxnData     = nData
ChemMCHead%nRxnNp       = nP
ChemMCHead%nQLine       = nQLine

IF( useMPI )CALL MPI_BCAST( ChemMCHead,1,ChemMCHead_MPI,0,MPI_COMM_WORLD,ierr )

!WRITE(*,*)'***',myid,'***','In InitChemMainMPI nSpecies   = ',chem%nSpecies

nZen = chem%nZenith
ALLOCATE(zenith(nZen),STAT=alloc_stat)
DO i = 1,nZen
  zenith(i) = chem%zenith(i)
END DO

IF( useMPI )CALL MPI_BCAST( zenith,nZen,MPI_REAL,0,MPI_COMM_WORLD,ierr )

species => chem%species

IF( useMPI )CALL MPI_BCAST( chem%species,chem%nSpecies,ChemSpecies_MPI,0,MPI_COMM_WORLD,ierr )

!WRITE(*,*)'***',myid,'***','In InitChemMainMPI nReactions = ',chem%nReactions

CALL BuildReactTypeMPI( )

DO rNo = 1,nReactions
 ChemRxn(rNo)%class = chem%reaction(rNo)%class
 ChemRxn(rNo)%type  = chem%reaction(rNo)%type
 ChemRxn(rNo)%k     = chem%reaction(rNo)%k
 ChemRxn(rNo)%A     = chem%reaction(rNo)%A
 ChemRxn(rNo)%B     = chem%reaction(rNo)%B
 ChemRxn(rNo)%fB    = chem%reaction(rNo)%fB
 ChemRxn(rNo)%nP    = chem%reaction(rNo)%nP
 ChemRxn(rNo)%nData = SIZE(chem%reaction(rNo)%Data)
END DO

CALL MPI_BCAST( ChemRxn,nReactions,ChemReact_MPI,0,MPI_COMM_WORLD,ierr )

ALLOCATE(rData(nReactions,nData),rnumP(nReactions,nP),rfP(nReactions,nP),STAT=alloc_stat)

rData = 0.0D0
rnumP = 0
rfP   = 0.
DO rNo = 1,nReactions
  nData = SIZE(chem%reaction(rNo)%Data)
  nP    = chem%reaction(rNo)%nP
  DO i = 1,nData
    rData(rNo,i) = chem%reaction(rNo)%Data(i)
  END DO
  DO i = 1,nP
     rnumP(rNo,i) = chem%reaction(rNo)%P(i)
     rfP(rNo,i)   = chem%reaction(rNo)%fP(i)
  END DO
END DO

nData = ChemMCHead%nRxnData
nP    = ChemMCHead%nRxnNp
CALL MPI_BCAST( rData,nReactions*nData,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr )
CALL MPI_BCAST( rnumP,nReactions*nP,MPI_INTEGER,0,MPI_COMM_WORLD,ierr )
CALL MPI_BCAST( rfP,nReactions*nP,MPI_REAL,0,MPI_COMM_WORLD,ierr )

ALLOCATE(nmlInts(nQLine))
nmlInts = TRANSFER(nmlLines,nmlInts)
IF( nQLine > 0 )CALL MPI_BCAST( nmlInts,nQLine,MPI_INTEGER1,0,MPI_COMM_WORLD,ierr )

!WRITE(*,*)'***',myid,'***','In InitChemMainMPI call BuildReactTypeMPI'

9999 CONTINUE
IF( ALLOCATED(zenith) )DEALLOCATE(zenith,STAT=alloc_stat)
IF( ALLOCATED(rData)  )DEALLOCATE(rData,rnumP,rfP,STAT=alloc_stat)
IF( ASSOCIATED(nmlInts)  )DEALLOCATE(nmlInts)

RETURN
END

!=======================================================================

SUBROUTINE InitChemSubMPI( projectName )

USE localmpi
USE mpi_fi
USE files_fi
USE charT_fd
USE message_fd
USE scipuff_fi, ONLY:dynamic,lwash
USE chem_fi
USE error_fi
USE AqAer_fi, ONLY: MAX_SP,naerp,nsec_aer,laerosol,file_imc

IMPLICIT NONE

TYPE( char128T ) :: projectName
CHARACTER(128)   :: prjName

INTEGER, PARAMETER :: NOT_SET_I  = -(2**16-1)

INTEGER i, j, rNo, ios, alloc_stat
INTEGER mcID,nZen,nReactions
INTEGER nData,nP, nQLine

DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: rData
REAL   ,          DIMENSION(:,:), ALLOCATABLE :: rfP
INTEGER,          DIMENSION(:,:), ALLOCATABLE :: rnumP
CHARACTER(LEN=1), DIMENSION(:),   POINTER     :: nmlLines
INTEGER(1),       DIMENSION(:)  , POINTER     :: nmlInts


INTERFACE
  SUBROUTINE RdAqAerSub( prjName,maxLen,nmlLines )
    CHARACTER(*), INTENT( IN )             :: prjName
    INTEGER,      INTENT( IN )             :: maxLen
    CHARACTER(LEN=1), DIMENSION(:),POINTER :: nmlLines
  END SUBROUTINE
END INTERFACE

Status%sRoutine = 'InitChemSubMPI'

lun_err = 12
lun_tmp = 21
lun_log = 24

prjName = TRIM(ProjectName%string)

WRITE(file_log,'(A,"_p",I3.3,".log")')TRIM(prjName(1:118)),myid

OPEN( unit=lun_log,file=file_log,status='UNKNOWN',iostat=ios )
IF (ios /= 0) THEN
  Status%nStat    = eOPEN
  Status%sRoutine = 'SetUpPrjMPI'
  Status%sMessage = 'Error opening SCIPUFF log output file'
  WRITE(Status%sInform,*)'File=',TRIM(file_log),', ios = ',ios
  GO TO 9999
END IF

WRITE(lun_log,*)'Log for process id = ',myid
WRITE(lun_log,*)

ALLOCATE( ChemMC(1),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  Status%nStat    = eALLOC
  Status%sRoutine = 'InitChemSubMPI'
  Status%sMessage = 'Error allocating chemMC array'
  WRITE(Status%sInform,*)'alloc_stat = ',alloc_stat
  GO TO 9999
END IF

mcID = 1

chem => chemMC(mcID)

CALL ClearChemMC()

CALL MPI_BCAST( ChemMCHead,1,ChemMCHead_MPI,0,MPI_COMM_WORLD,ierr )

dynamic           = ChemMCHead%ldyn
lwash             = ChemMCHead%lwash
chem%flag         = ChemMCHead%flag
chem%cUnits       = ChemMCHead%cUnits
chem%eUnits       = ChemMCHead%eUnits
chem%kUnits       = ChemMCHead%kUnits
chem%kConv        = ChemMCHead%kConv
chem%thermal      = ChemMCHead%thermal
chem%eConv        = ChemMCHead%eConv
chem%tConv        = ChemMCHead%tConv
chem%rtol         = ChemMCHead%rtol
chem%eConvType    = ChemMCHead%eConvType
chem%nSpecies     = ChemMCHead%nSpecies
chem%nReactions   = ChemMCHead%nReactions
chem%nZenith      = ChemMCHead%nZenith
chem%nStar        = ChemMCHead%nStar
chem%nFast        = ChemMCHead%nFast
chem%nSlow        = ChemMCHead%nSlow
chem%nParticle    = ChemMCHead%nParticle
chem%nEquilibrium = ChemMCHead%nEquilibrium
chem%nFastReact   = ChemMCHead%nFastReact
chem%nSlowReact   = ChemMCHead%nSlowReact
chem%nEquilReact  = ChemMCHead%nEquilReact
chem%nPhotoReact  = ChemMCHead%nPhotoReact
chem%nSolveEq     = ChemMCHead%nSolveEq
chem%nLinearEq    = ChemMCHead%nLinearEq
chem%nDirectEq    = ChemMCHead%nDirectEq
chem%nSubstEq     = ChemMCHead%nSubstEq
chem%imax         = ChemMCHead%imax
chem%lBalance     = ChemMCHead%lBalance

!WRITE(*,*)'***',myid,'***',': In InitChemSubMPI nSpecies   = ',chem%nSpecies

nZen = chem%nZenith
ALLOCATE( chem%zenith(nZen),STAT=alloc_stat )
IF( alloc_stat /= 0 )GOTO 9999

CALL MPI_BCAST( chem%zenith,nZen,MPI_REAL,0,MPI_COMM_WORLD,ierr)

!---- Initialize material multi-component

species => chem%species
nSpecies = chem%nSpecies

ALLOCATE( chem%species(nSpecies),STAT=alloc_stat )
IF( alloc_stat /= 0 )GOTO 9999

CALL MPI_BCAST( chem%species,nSpecies,ChemSpecies_MPI,0,MPI_COMM_WORLD,ierr)

!-Initialize chem%reaction

nReactions = chem%nReactions

ALLOCATE( chem%reaction(nReactions),STAT=alloc_stat )
IF( alloc_stat /= 0 )GOTO 9999

CALL BuildReactTypeMPI( )

CALL MPI_BCAST( ChemRxn,nReactions,ChemReact_MPI,0,MPI_COMM_WORLD,ierr )

DO rNo = 1,nReactions
  chem%reaction(rNo)%class = ChemRxn(rNo)%class
  chem%reaction(rNo)%type  = ChemRxn(rNo)%type
  chem%reaction(rNo)%k     = ChemRxn(rNo)%k
  chem%reaction(rNo)%A     = ChemRxn(rNo)%A
  chem%reaction(rNo)%B     = ChemRxn(rNo)%B
  chem%reaction(rNo)%fB    = ChemRxn(rNo)%fB
  chem%reaction(rNo)%nP    = ChemRxn(rNo)%nP
  ALLOCATE(chem%reaction(rNo)%Data(ChemRxn(rNo)%nData),STAT=alloc_stat)
  ALLOCATE(chem%reaction(rNo)%fP(ChemRxn(rNo)%nP)     ,STAT=alloc_stat)
  ALLOCATE(chem%reaction(rNo)%P(ChemRxn(rNo)%nP)      ,STAT=alloc_stat)
END DO

nData = ChemMCHead%nRxnData
nP    = ChemMCHead%nRxnNp

ALLOCATE(rData(nReactions,nData),rnumP(nReactions,nP),rfP(nReactions,nP),STAT=alloc_stat)

CALL MPI_BCAST( rData,nReactions*nData,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr )
CALL MPI_BCAST( rnumP,nReactions*nP,MPI_INTEGER,0,MPI_COMM_WORLD,ierr )
CALL MPI_BCAST( rfP,nReactions*nP,MPI_REAL,0,MPI_COMM_WORLD,ierr )

DO rNo = 1,nReactions
  nData = SIZE(chem%reaction(rNo)%Data)
  DO i = 1,nData
    chem%reaction(rNo)%Data(i) = rData(rNo,i)
  END DO
  DO i = 1,chem%reaction(rNo)%nP
    chem%reaction(rNo)%P(i)  = rnumP(rNo,i)
    chem%reaction(rNo)%fP(i) = rfP(rNo,i)
  END DO
END DO

CALL SetChemPointers()

H2O = NOT_SET_I
DO i = 1,chem%nSpecies
  chem%species(i)%lstar = .FALSE.
  IF( TRIM(chem%species(i)%name) == 'H2O' .OR. &
      TRIM(chem%species(i)%name) == 'WATER' )THEN
    IF( chem%species(i)%ambient >= 0.0 ) H2O = i
  END IF
END DO

!-Initialize AqAer variables
nQLine = ChemMCHead%nQLine
IF( nQLine > 0 )THEN
  ALLOCATE( nmlLines(nQLine),nmlInts(nQLine),STAT=alloc_stat )
  CALL MPI_BCAST( nmlInts,nQLine,MPI_INTEGER1,0,MPI_COMM_WORLD,ierr )
  nmlLines = TRANSFER(nmlInts,nmlLines)
  CALL RdAqAerSub( prjName,nQLine,nmlLines )
END IF

CALL SetStarSpecies( mcID )
IF( nError /= NO_ERROR )GOTO 9999

CALL SetChemAqaer( )
IF( nError /= NO_ERROR )GOTO 9999

chem_aqaer%prjName = prjName
CALL InitAerAqEx( mcID,chem_aqaer,ID_SPECIES_PARTICLE,nError )
IF( nError /= NO_ERROR )THEN
  eRoutine = 'InitAerAq'
  eMessage = 'Error in InitAerAq. Check AqAer log for details'
  GOTO 9999
END IF

CALL GetChemAqaer( )

!====   Set pointers for "star" species

IF( chem%nStar > 0 )THEN

  IF( ASSOCIATED(chem%star) )DEALLOCATE( chem%star,STAT=alloc_stat )
  ALLOCATE( chem%star(chem%nStar),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    nError   = RD_ERROR
    eRoutine = 'SetStarSpecies'
    eMessage = 'Error allocating multicomponent arrays'
    GOTO 9999
  END IF

  j = 0
  DO i = 1,chem%nSpecies
    IF( chem%species(i)%lstar )THEN
      j = j + 1
      chem%star(j)%s => chem%species(i)
    END IF
  END DO

END IF


9999 CONTINUE

IF( ALLOCATED(ChemRxn) )DEALLOCATE(ChemRxn)
IF( ASSOCIATED(nmlLines) )DEALLOCATE(nmlLines,nmlInts)

RETURN
END

!=======================================================================

SUBROUTINE AllocStepMCDat( nspuf )
! Called from step for main processor

USE localmpi
USE mpi_fi
USE chem_fi
USE scipuff_fi
USE StepMC_mpi_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: nspuf

INTEGER ii,alloc_stat
INTEGER n,nsp

ALLOCATE( puffNos(nspuf),puffSav(nspuf),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'InitStepMCDat'
  eMessage = 'Error allocating puffNos'
  WRITE(eInform,*)'nspuf = ',nspuf
END IF

IF( multicomp )THEN

  DO ii = 1,mat_mc%nMCtype
    IF( mat_mc%type(ii) == MC_CHEM )THEN
      n = mat_mc%ID(ii)
      chem => chemMC(n)
      EXIT
    END IF
  END DO

  nSp = chem%nSpecies

  IF( ALLOCATED(StepMCdat) )DEALLOCATE(StepMCdat,StepMcSp,STAT=alloc_stat)
  ALLOCATE( StepMCdat(nspuf),StepMcSp(nspuf,nSp,nVarSp),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'InitStepMCDat'
    eMessage = 'Error allocating StepMCdat'
    WRITE(eInform,*)'nspuf = ',nspuf
  END IF

  iStepMC       = 0
  ngd_chem_sav  = ngd_chem
  nbad_chem_sav = nbad_chem

  max_chunk = MAX(1,INT(MP_BUFFER_MEM/(REAL(nSp)*REAL(nVarSp)*KIND(nSp))) - 1)

END IF

RETURN
END

!=======================================================================

SUBROUTINE SetStepMCMPI( ID,p,dt )
! Called from StepMC inside nspuf loop from Main

USE localmpi
USE mpi_fi
USE met_fi
USE step_p_fi, ONLY: tdyn,hp,csav,cnew
USE chem_fi
USE scipuff_fi
USE StepMC_mpi_fi
USE diagnostics_fi

IMPLICIT NONE

INTEGER,          INTENT( IN    ) :: ID
TYPE( puff_str ), INTENT( INOUT ) :: p    ! -- puff structure
REAL,             INTENT( IN    ) :: dt

TYPE( puff_dynamics ) pd

INTEGER i,isp

INTERFACE
  SUBROUTINE SetChemAmbient( xIn,yIn,zIn,t,leq,lmet,lplot )
    REAL,    INTENT( IN ) :: xIn, yIn, zIn, t
    LOGICAL, INTENT( IN ) :: leq
    LOGICAL, INTENT( IN ) :: lmet
    LOGICAL, OPTIONAL, INTENT( IN ) :: lplot
  END SUBROUTINE
END INTERFACE

Status%sRoutine = 'SetStepChemMPI'

chem => chemMC(ID)

!-----  get multi-components

CALL GetChemAux( ID,p )

nfast     = chem%nFast
nslow     = chem%nSlow
nequil    = chem%nEquilibrium
nparticle = chem%nParticle
nspecies  = nfast + nslow + nparticle
nspectot  = nspecies + nequil
nambient  = chem%nSpecies - nspectot

species => chem%species

IF( chem%nReactions == 0 )RETURN

fast  => chem%fast
slow  => chem%slow
equil => chem%equil

nsolve_eq = chem%nSolveEq
nlin_eq   = chem%nLinearEq
ndir_eq   = chem%nDirectEq
nsubs_eq  = chem%nSubstEq

indx_eq  => chem%IndexEq
itype_eq => chem%TypeEq
irow_eq  => chem%RowEq

rtol = chem%rtol

ResetEq = .FALSE.

!====   set ambient concentrations

CALL SetChemAmbient( p%xbar,p%ybar,p%zbar,t,.TRUE.,.TRUE. )
IF( nError /= NO_ERROR )GOTO 9999

iStepMC = iStepMC + 1

pStepMCdat => StepMCdat(iStepMC)

pStepMCdat%ngd = 0
pStepMCdat%nbd = 0
pStepMCdat%t   = t
pStepMCdat%dt  = dt

pStepMCdat%pf%xbar  = p%xbar
pStepMCdat%pf%ybar  = p%ybar
pStepMCdat%pf%zbar  = p%zbar
pStepMCdat%pf%det   = p%det
pStepMCdat%pf%c     = cnew
pStepMCdat%pf%cc    = p%cc
pStepMCdat%pf%ccb   = p%ccb
pStepMCdat%pf%csav  = csav
pStepMCdat%pf%sr    = p%sr
pStepMCdat%pf%szz   = p%szz
pStepMCdat%pf%pno   = 0
pStepMCdat%pf%ityp  = p%ityp
pStepMCdat%pf%idtl  = p%idtl
pStepMCdat%pf%naux  = p%naux
pStepMCdat%pf%vol   = vol
pStepMCdat%pf%pres  = pres
pStepMCdat%pf%temp  = temp
IF( dynamic )THEN
  CALL get_dynamics( p,pd )
  tdyn = MAX(100. - tb,pd%ctp/p%c)
ELSE
  tdyn = 0.
END IF
pStepMCdat%pf%tdyn  = tdyn
pStepMCdat%pf%vself = vself

pStepMCdat%pm%tb    = tb
pStepMCdat%pm%pb    = pb
pStepMCdat%pm%hb    = hb
pStepMCdat%pm%cw    = cw
pStepMCdat%pm%cc    = cc
pStepMCdat%pm%sfac  = sun_fac
pStepMCdat%pm%prate = prate
pStepMCdat%pm%prbl  = prbl
pStepMCdat%pm%zinv  = zinv
pStepMCdat%pm%hp    = hp
pStepMCdat%pm%us2   = us2
pStepMCdat%pm%ws2   = ws2
pStepMCdat%pm%xml   = xml
pStepMCdat%pm%zb1   = zb1
pStepMCdat%pm%zruf  = zruf

DO isp = 1,chem%nSpecies
  StepMcSp(iStepMC,isp,iDDep) = chem%species(isp)%mddp
  StepMcSp(iStepMC,isp,iWDep) = chem%species(isp)%mwdp
  StepMcSp(iStepMC,isp,iMass) = chem%species(isp)%mass
  StepMcSp(iStepMC,isp,iConc) = chem%species(isp)%conc
  StepMcSp(iStepMC,isp,iAmb)  = chem%species(isp)%amb
END DO

IF( lDiagno )THEN
  DO i = 1,nspectot
    chemistry(i) = chemistry(i) - chem%species(i)%msav
  END DO
END IF

9999 CONTINUE

RETURN
END

!=======================================================================

SUBROUTINE GetChemMPI( ID,ipuf,p,dt )

USE met_fi
USE chem_fi
USE scipuff_fi
USE step_p_fi, ONLY: tdyn,hp,csav
USE mpi_fi
USE StepMC_mpi_fi

IMPLICIT NONE

INTEGER         , INTENT( IN  ) :: ID   ! -- Chemistry multi-component ID
INTEGER         , INTENT( IN  ) :: ipuf
TYPE( puff_str ), INTENT( OUT ) :: p    ! -- puff structure
REAL            , INTENT( OUT ) :: dt   ! -- timestep (sec)

INTEGER i

chem => chemMC(ID)
pStepMCdat => StepMCdat(ipuf)

ngd_chem  = 0
nbad_chem = 0

t         = pStepMCdat%t
dt        = pStepMCdat%dt

p%xbar    = pStepMCdat%pf%xbar
p%ybar    = pStepMCdat%pf%ybar
p%zbar    = pStepMCdat%pf%zbar
p%det     = pStepMCdat%pf%det
p%c       = pStepMCdat%pf%c
p%cc      = pStepMCdat%pf%cc
p%ccb     = pStepMCdat%pf%ccb
csav      = pStepMCdat%pf%csav
p%sr      = pStepMCdat%pf%sr
p%szz     = pStepMCdat%pf%szz
pufNo     = pStepMCdat%pf%pno
p%ityp    = pStepMCdat%pf%ityp
p%idtl    = pStepMCdat%pf%idtl
p%naux    = pStepMCdat%pf%naux
vol       = pStepMCdat%pf%vol
pres      = pStepMCdat%pf%pres
temp      = pStepMCdat%pf%temp
tdyn      = pStepMCdat%pf%tdyn
vself     = pStepMCdat%pf%vself
tb        = pStepMCdat%pm%tb
pb        = pStepMCdat%pm%pb
hb        = pStepMCdat%pm%hb
cw        = pStepMCdat%pm%cw
cc        = pStepMCdat%pm%cc
sun_fac   = pStepMCdat%pm%sfac
prate     = pStepMCdat%pm%prate
prbl      = pStepMCdat%pm%prbl
zinv      = pStepMCdat%pm%zinv
hp        = pStepMCdat%pm%hp
us2       = pStepMCdat%pm%us2
ws2       = pStepMCdat%pm%ws2
xml       = pStepMCdat%pm%xml
zb1       = pStepMCdat%pm%zb1
zruf      = pStepMCdat%pm%zruf

DO i = 1,chem%nSpecies
  chem%species(i)%mddp   = StepMcSp(ipuf,i,iDDep)
  chem%species(i)%mwdp   = StepMcSp(ipuf,i,iWDep)
  chem%species(i)%mass   = StepMcSp(ipuf,i,iMass)
  chem%species(i)%conc   = StepMcSp(ipuf,i,iConc)
  chem%species(i)%amb    = StepMcSp(ipuf,i,iAmb)
END DO

RETURN
END

!=======================================================================

SUBROUTINE PutChemMPI( ID,ipuf )

USE met_fi
USE chem_fi
USE scipuff_fi
USE step_p_fi, ONLY: tdyn,hp,csav
USE mpi_fi
USE StepMC_mpi_fi

IMPLICIT NONE

INTEGER         , INTENT( IN    ) :: ID   ! -- Chemistry multi-component ID
INTEGER         , INTENT( IN    ) :: ipuf

INTEGER i

chem => chemMC(ID)
pStepMCdat => StepMCdat(ipuf)

pStepMCdat%pf%vol   = vol
pStepMCdat%pf%pres  = pres
pStepMCdat%pf%temp  = temp
pStepMCdat%pf%vself = vself

DO i = 1,chem%nSpecies
  StepMcSp(ipuf,i,iDDep) = chem%species(i)%mddp
  StepMcSp(ipuf,i,iWDep) = chem%species(i)%mwdp
  StepMcSp(ipuf,i,iMass) = chem%species(i)%mass
  StepMcSp(ipuf,i,iConc) = chem%species(i)%conc
  StepMcSp(ipuf,i,iAmb)  = chem%species(i)%amb
END DO

pStepMCdat%ngd = ngd_chem
pStepMCdat%nbd = nbad_chem

RETURN
END

!=======================================================================

SUBROUTINE SetChemMPI( ID,ipuf )

USE met_fi
USE chem_fi
USE scipuff_fi
USE step_p_fi, ONLY: tdyn,hp,csav
USE mpi_fi
USE StepMC_mpi_fi
USE diagnostics_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: ID, ipuf

INTEGER i

chem => chemMC(ID)
pStepMCdat => StepMCdat(ipuf)

vol   = pStepMCdat%pf%vol
pres  = pStepMCdat%pf%pres
temp  = pStepMCdat%pf%temp
vself = pStepMCdat%pf%vself

DO i = 1,chem%nSpecies
  chem%species(i)%mddp = StepMcSp(ipuf,i,iDDep)
  chem%species(i)%mwdp = StepMcSp(ipuf,i,iWDep)
  chem%species(i)%mass = StepMcSp(ipuf,i,iMass)
  chem%species(i)%conc = StepMcSp(ipuf,i,iConc)
  chem%species(i)%amb  = StepMcSp(ipuf,i,iAmb)
END DO

nspectot = chem%nFast + chem%nSlow + chem%nParticle + chem%nEquilibrium

IF( lDiagno )THEN
  !====   Diagnostics
  DO i = 1,nspectot
    chemistry(i)   = chemistry(i)   + chem%species(i)%mass
    ddeposition(i) = ddeposition(i) + chem%species(i)%mddp   !dry deposition diagnostic
    wdeposition(i) = wdeposition(i) + chem%species(i)%mwdp   !wet deposition diagnostic
  END DO
END IF

ngd_chem  = ngd_chem  + pStepMCdat%ngd
nbad_chem = nbad_chem + pStepMCdat%nbd

RETURN
END

!=======================================================================

SUBROUTINE StepChemPuffMPI( nspuf )

USE localmpi
USE mpi_fi
USE chem_fi
USE scipuff_fi
USE surface_fi
USE StepMC_mpi_fi

IMPLICIT NONE

INTEGER :: nspuf

INTEGER ID          ! -- Chemistry multi-component ID
INTEGER alloc_stat
INTEGER ipuf, k, mType, isplt

REAL    fac_srf, spltf
REAL    dt             ! -- timestep (sec)

TYPE( puff_str ) p  ! -- puff structure

!INTEGER, SAVE :: nchunk_count = 0

Status%sRoutine = 'StepChemPuffMPI'
Status%nStat = 0

nchunk     = 0
ipuf_start = 0

IF( myid == 0 )THEN
  mType = typeID(puff(puffNos(1)%ipuf)%ityp)%imat
  ID    = mat_mc%ID(material(mType)%mcID)
  DO ipuf = 1,nspuf
    k = puffNos(ipuf)%ipuf
    StepMCdat(ipuf)%pf%pno = k
  END DO
ELSE
  ID = 1
END IF

CALL ScatterPuffsMPI( ID,nspuf )
IF( Status%nStat < 0 )THEN
 GO TO 9999
ELSE IF( Status%nStat == sDone )THEN
 GO TO 9999
END IF

IF( myid == 0 )npuf_chunk = nchunk(1)

!<DEBUG>
!IF( myid == 0 )THEN
!  WRITE(700,*)'< Start Time,nspuf = ',t,nspuf
!  DO ipuf = 1,nspuf
!    WRITE(700,*)ipuf,stepMCDat(ipuf)%t,stepMCDat(ipuf)%dt,puffNos(ipuf)%ipuf
!  END DO
!  WRITE(700,*)'  End Time,nspuf = ',t,nspuf,' >'
!END IF
!nchunk_count = nchunk_count + 1
!WRITE(700+myid,*)nchunk_count,npuf_chunk
!<\DEBUG>

!--- Call step mc
DO ipuf = 1,npuf_chunk

  fac_srf = NOT_SET_R  ! For non static puffs

  CALL GetChemMPI( ID,ipuf,p,dt )

  CALL StepChem( ID,p,dt )
  IF( nError /= NO_ERROR )GOTO 9999

  CALL StepChemDep( p,ID,dt,fac_srf )
  IF( nError /= NO_ERROR )GOTO 9999

  CALL PutChemMPI( ID,ipuf )

END DO

IF( useMPI )CALL GatherPuffsMPI( ID )

IF( myid == 0 )THEN

  !-- Update deposition and pm

  ngd_chem  = ngd_chem_sav
  nbad_chem = nbad_chem_sav

  DO k = 1,nspuf

    ipuf  = puffNos(k)%ipuf
    isplt = puffNos(k)%npuf

    CALL SetChemMPI( ID,k )

    CALL StepDepMPI( ipuf,puffSav(k),dt )

    IF( isplt > 0 )THEN
      spltf = puffNos(k)%frac
      CALL PutChemAuxFrac( ID,puff(ipuf),1-spltf )
      CALL PutChemAuxFrac( ID,puff(isplt),spltf )
    ELSE
      CALL PutChemAux( ID,puff(ipuf) )
    ENDIF

  END DO

END IF

9999 CONTINUE

IF( ALLOCATED(StepMCdat) ) THEN

  DEALLOCATE(StepMCdat,StepMcSp,STAT=alloc_stat)
  IF( alloc_stat /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'StepChemPuffMPI'
    eMessage = 'Error deallocating StepMCdat'
    WRITE(eInform,*)'nspuf = ',nspuf
  END IF

END IF

RETURN
END

!=======================================================================

SUBROUTINE ScatterPuffsMPI( ID, nspuf )
! MainProcess : Send chunk of StepMCdat
! SubProcess  : Receive chunk of StepMCdat from main

USE localmpi
USE mpi_fi
USE scipuff_fi
USE chem_fi
USE StepMC_mpi_fi

IMPLICIT NONE

INTEGER, INTENT( IN ):: ID, nspuf

INTEGER alloc_stat
INTEGER i, j, nsp, iStart, iEnd
INTEGER iChe,nCh

CHARACTER, DIMENSION(MPI_MAX_ERROR_STRING)  :: eString

IF( myid == 0 )THEN

  !-- Scatter StepMCdat

  nchunk(1:numprocs) = nspuf/numprocs

  ipuf_start(1) = 1

  source = 0

  chem => chemMC(ID)

  nsp = chem%nSpecies

  DO j = 1,numprocs

    IF( j <= (nspuf - numprocs*nchunk(numprocs)) )nchunk(j) = nchunk(j) + 1
    IF( j > 1) THEN
      dest = j-1
      ipuf_start(j) = ipuf_start(dest) + nchunk(dest)
      !WRITE(*,'("***0:From Scatter sending at time t = ",F10.3,", to processor ",I3.3,", nchunk = ",I5.5)')t,j,nchunk(j)

      IF( useMPI )CALL MPI_Send ( nchunk(j),1,MPI_INTEGER,dest,tag_chunk,MPI_COMM_WORLD,ierr )
      !WRITE(*,'("===0:From Scatter sent at time t = ",F10.3,", to processor ",I3.3,", nchunk = ",I5.5)')t,j,nchunk(j)

      IF ( nchunk(j) > 0 ) THEN
        IF( useMPI )CALL MPI_Send ( StepMCdat(ipuf_start(j)), nchunk(j), StepMCdat_MPI, dest, &
                                    tag_putmc, MPI_COMM_WORLD, ierr )
        iStart = ipuf_start(j)
        iEnd   = ipuf_start(j) + nchunk(j) - 1
        DO i = iStart,iEnd,max_chunk
          iChe = MIN(iEnd,i+max_chunk-1)
          nCh  = iChe - i + 1
          !WRITE(*,'("===0:From Scatter sent at time t = ",F10.3,", to processor ",I3.3,", nchunk = ",3(I5.5,1x))')&
          !      t,j,i,iChe,nCh
          IF( useMPI )CALL MPI_Send ( StepMcSp(i:iChe,1:nSp,1:nVarSp), nCh*nSp*nVarSp, MPI_REAL, dest, &
                                      tag_putsp, MPI_COMM_WORLD, ierr )
        END DO
      END IF
    END IF

  END DO

ELSE

  !WRITE(*,'("***1:From Scatter receiving in processor ",I3.3)')myid+1

  CALL MPI_Recv ( npuf_chunk, 1, MPI_INTEGER, 0, &
                   tag_chunk, MPI_COMM_WORLD, stat, ierr )

  !WRITE(*,'("===1:From Scatter received in processor ",I3.3,", nchunk = ",I5.5)')myid+1,npuf_chunk

  IF ( npuf_chunk > 0 ) THEN

    chem => chemMC(1)

    nsp = chem%nSpecies

    ALLOCATE(StepMCdat(npuf_chunk),StepMcSp(npuf_chunk,nsp,nVarSp),STAT=alloc_stat)
    IF (alloc_stat /= 0) THEN
      nError   = UK_ERROR
      eRoutine = 'ScatterPuffsMPI'
      WRITE(eMessage,*)'Error allocating StepMCdat for process id',myid
      WRITE(eInform,*)'npuf_chunk = ',npuf_chunk
    END IF

    max_chunk = MAX(1,INT(MP_BUFFER_MEM/(REAL(nSp)*REAL(nVarSp)*KIND(nSp))) - 1)

    CALL MPI_Recv ( StepMCdat, npuf_chunk, StepMCdat_MPI, 0, &
                    tag_putmc, MPI_COMM_WORLD, stat, ierr )

    DO i = 1,npuf_chunk,max_chunk
      iChe = MIN(npuf_chunk,i+max_chunk-1)
      nCh  = iChe - i + 1
      !WRITE(*,'("===1:From Scatter receiving in processor ",I3.3,", nchunk = ",3(I5.5,1x))')myid+1,i,iChe,nCh
      CALL MPI_Recv ( StepMcSp(i:iChe,1:nSp,1:nVarSp), nCh*nsp*nVarSp, MPI_REAL, 0, &
                      tag_putsp, MPI_COMM_WORLD, stat, ierr )
    END DO

  ELSE IF( npuf_chunk == -1 )THEN

    Status%nStat = sDone
    GO TO 9999

  END IF

END IF

9999 CONTINUE
RETURN
END

!=======================================================================

SUBROUTINE GatherPuffsMPI( ID )
! SubProcess  : Send chunk of StepMCdat to main
! MainProcess : Receive chunk of StepMCdat from subs

USE localmpi
USE mpi_fi
USE chem_fi
USE files_fi
USE scipuff_fi
USE StepMC_mpi_fi

IMPLICIT NONE

INTEGER, INTENT( IN ):: ID

INTEGER i, j, nsp, alloc_stat
INTEGER iStart, iEnd
INTEGER iChe, nCh

chem => chemMC(ID)

nsp = chem%nSpecies

IF( myid == 0 )THEN

  !-- Gather StepMCdat
  DO j = 2,numprocs

    source = j-1

    IF ( nchunk(j) < 1 ) CYCLE

    iStart = ipuf_start(j)
    iEnd   = ipuf_start(j) + nchunk(j)-1

    !WRITE(*,'("***0:From Gather receiving at time t = ",F10.3,", from processor ",I1,", nchunk = ",I4.4)')t,j,nchunk(j)


    CALL MPI_Recv ( StepMCdat(iStart), nchunk(j), StepMCdat_MPI, source, &
                    tag_getmc, MPI_COMM_WORLD, stat, ierr )

    DO i = iStart,iEnd,max_chunk
      iChe = MIN(iEnd,i+max_chunk-1)
      nCh  = iChe - i + 1
      !WRITE(*,'("***0:From Gather receiving at time t = ",F10.3,", from processor ",I1,", nchunk = ",3(I5.5,1x))')&
      !           t,j,i,iChe,nCh
      CALL MPI_Recv (StepMcSp(i:iChe,1:nSp,1:nVarSp), nCh*nSp*nVarSp, MPI_REAL,&
                     source, tag_getsp, MPI_COMM_WORLD, stat, ierr )
    END DO

    !pStepMCdat => StepMCdat(ipuf_start(j))

    !IF (pStepMCdat%ipuf < 0) THEN
    !  nError = UK_ERROR
    !  k = ABS(pStepMCdat%ipuf)
    !  WRITE(lun_err,*)'Error from process id:',j
    !  WRITE(lun_log,*)'Error from process id:',j
    !  CALL dump_puff(k,puff(k))
    !END IF

  END DO

ELSE

  dest = 0

  IF( npuf_chunk > 0 )THEN

    !WRITE(*,'("***1:From Gather sending at time t = ",F10.3,", from processor ",I1,", nchunk = ",I4.4)')StepMCdat(1)%t,myid+1,npuf_chunk

    CALL MPI_Send ( StepMCdat, npuf_chunk, StepMCdat_MPI, dest, &
                    tag_getmc, MPI_COMM_WORLD, ierr )

    DO i = 1,npuf_chunk,max_chunk
      iChe = MIN(npuf_chunk,i+max_chunk-1)
      nCh  = iChe - i + 1
      !WRITE(*,'("***1:From Gather sending at time t = ",F10.3,", from processor ",I1,", nchunk = ",3(I5.5,1x))')&
      !StepMCdat(1)%t,myid+1,i,iChe,nCh
      CALL MPI_Send ( StepMcSp(i:iChe,1:nSp,1:nVarSp), nCh*nSp*nVarSp, MPI_REAL, dest, &
                      tag_getsp, MPI_COMM_WORLD, ierr )
    END DO

    IF( ALLOCATED(StepMCdat) )DEALLOCATE( StepMCdat,StepMCSp,STAT=alloc_stat )
    IF( alloc_stat /= 0 )THEN
      nError   = UK_ERROR
      eRoutine = 'GatherPuffsMPI'
      eMessage = 'Error deallocating StepMCdat'
      WRITE(eInform,*)'npuf_chunk = ',npuf_chunk
    END IF

  END IF

END IF

RETURN
END

!=======================================================================

SUBROUTINE StepDepMPI( ipuf,p,dt )

USE error_fi
USE chem_fi
USE scipuff_fi
USE surface_fi
USE step_p_fi
USE met_fi

IMPLICIT NONE

INTEGER :: ipuf
REAL    :: dt

INTEGER icls,jtyp
REAL    fac_srf

TYPE ( puff_str     ) p        !Puff structure
TYPE( puff_material ) pmatl
TYPE( puff_dynamics ) pd
TYPE( puff_totalcc  ) pt
TYPE( puff_liquid   ) pq

INTEGER, EXTERNAL :: getPuffiskew
LOGICAL, EXTERNAL :: IsLiquid, IsWetParticle
LOGICAL, EXTERNAL :: IsAerosol

jtyp   = p%ityp
CALL get_puff_material( jtyp,pmatl )

!-----  get dynamic puff parameters

IF( dynamic )THEN
  CALL get_dynamics( p,pd )
  tdyn = MAX(100. - tb,pd%ctp/p%c)
END IF

!-----  get liquid puff parameters

IF( IsLiquid(icls) .OR. IsWetParticle(icls) )CALL get_liquid( p,pq )

fac_srf = NOT_SET_R
ltot    = typeID(jtyp)%ltot
lsrf    = srf_puff(jtyp,1)%nblocks > 0
ldos    = srf_puff(jtyp,2)%nblocks > 0
lscale  = lsrf .OR. ldos .OR. lsmp

!-----  get total cc
IF( ltot )CALL get_totalcc( p,pt )

!-----  Initialize met fields, terrain, and puff variables

CALL step_init( p )
IF( nError /= NO_ERROR )GOTO 9999

IF( lsv_oper )CALL reset_lsv( si )

!------ scale turbulence for time-averaging

IF( t_avg /= DEF_VAL_R )THEN
  CALL turb_rescale( si,sv,vel2 )
END IF

!---- Check for skew turbulence in convective layer

iSkew = getPuffiskew( p )

CALL SetSkewTurb()

CALL get_dep( p,pmatl,pq,fac_srf,dt )

IF( lsrf .OR. ldos )THEN
  CALL step_dep( p,pt,ipuf,dt,fac_srf )
  IF( nError /= NO_ERROR )GOTO 9999
END IF

9999 CONTINUE

RETURN

END
!=======================================================================

SUBROUTINE RdAqAerMain( file_imc,maxLen,nmlLines )

USE error_fi

IMPLICIT NONE

CHARACTER(*), INTENT( IN  )             :: file_imc
INTEGER,      INTENT( OUT )             :: maxLen
CHARACTER(LEN=1), DIMENSION(:),POINTER :: nmlLines

INTEGER, PARAMETER         :: MAXLCH  = 1024 ! Maximum characters in line

INTEGER                    :: ios, alloc_stat, lun
INTEGER                    :: i, nCh, iPass
LOGICAL                    :: AqAerSec, isOpen
CHARACTER(MAXLCH)          :: line

maxLen = 0

INQUIRE( FILE=file_imc,OPENED=isOPEN,NUMBER=lun )

IF( isOpen )THEN
  REWIND( UNIT=lun,IOSTAT=ios )
ELSE
  lun = 20
  OPEN( UNIT=lun,FILE=file_imc,IOSTAT=ios )
  IF( ios /= 0 )THEN
    nError   = OP_ERROR
    eRoutine = 'ReadAqAer'
    eMessage = 'Error reading Multi-component input file'
    CALL ReportFileName( eInform,'File=',file_imc )
    GOTO 9999
  END IF
END IF

DO iPass = 1,2

  maxLen   = 0
  AqAerSec = .FALSE.

  DO WHILE( ios == 0 )

    READ(lun,'(A)',IOSTAT=ios)line
    IF( ios < 0 )THEN
      EXIT
    ELSE IF( ios > 0 )THEN
      WRITE(*,*)'Error reading line. ios = ',ios
      GO TO 9999
    END IF

    IF( AqAerSec .AND. line(1:1) == '#' )THEN
      AqAerSec = .FALSE.
      EXIT
    END IF

    IF( line(1:2) == '#Q' )THEN
      AqAerSec = .TRUE.
      CYCLE
    END IF

    IF( AqAerSec )THEN
      nCh = LEN_TRIM(line)
      IF( iPass == 1 )THEN
        maxLen = maxLen + nCh + 1
      ELSE
        DO i = 1,nCh
          maxLen = maxLen + 1
          nmlLines(maxLen) = line(i:i)
        END DO
        maxLen = maxLen + 1
        nmlLines(maxLen) = CHAR(10)
      END IF
    END IF

  END DO

  IF( iPass == 1 )THEN
    ALLOCATE( nmlLines(maxLen),STAT=alloc_stat )
    ios = 0
    REWIND(lun,IOSTAT=ios)
    IF( ios /= 0 )THEN
      WRITE(*,*)'Error rewinding imc file',ios
      GO TO 9999
    END IF
  END IF

END DO

CLOSE(lun)

9999 CONTINUE

RETURN
END

!=======================================================================

SUBROUTINE RdAqAerSub( prjName,maxLen,nmlLines )

USE mpi_fi
USE error_fi

IMPLICIT NONE

CHARACTER(*),                  INTENT( IN ) :: prjName
INTEGER,                       INTENT( IN ) :: maxLen
CHARACTER(LEN=1), DIMENSION(:),POINTER      :: nmlLines

INTEGER i, ios, lun
CHARACTER(LEN=10) mcFile

lun = 401
WRITE(mcFile,'("scratch",I3.3)')myid
OPEN(lun,FILE=mcFile,IOSTAT=ios)
IF( ios /= 0 )THEN
  WRITE(*,*)'Error opening ',mcFile,' with iostat = ',ios
  GOTO 9999
END IF

WRITE(lun,'("#Q")',ADVANCE='YES',IOSTAT=ios)
DO i = 1,maxLen
  WRITE(lun,'(A)',ADVANCE='NO',IOSTAT=ios)nmlLines(i)
  IF( ios /= 0 )THEN
    WRITE(*,*)'Error writing to file ',TRIM(mcFile),' with IOSTAT = ',ios
    GOTO 9999
  END IF
END DO

CLOSE(lun)

AqAerInp%nError  = nError
AqAerInp%prjName = prjName
AqAerInp%mcFile  = TRIM(mcFile)
CALL ReadAqAerEx( AqAerInp )
IF( nError /= NO_ERROR )THEN
  nError   = IV_ERROR
  eRoutine = 'ReadAqAer'
  eMessage = 'Error from AqAer module. See project qlog for details'
  GO TO 9999
END IF

9999 CONTINUE

OPEN( lun,FILE=mcFile,IOSTAT=ios )
IF( ios == 0 )CLOSE(lun,STATUS='DELETE')

RETURN
END

!=======================================================================

SUBROUTINE DeAllocMPI( irv )
!DEC# ATTRIBUTES DLLEXPORT :: DeAllocMPI

IMPLICIT NONE

INTEGER, INTENT( INOUT ) :: irv

INTEGER alloc_stat

!IF( ALLOCATED(chemSpcStep)   )DEALLOCATE( chemSpcStep, stat=alloc_stat )

irv = 0

RETURN
END

!=======================================================================

SUBROUTINE ScipuffEndSendMPI( )
!DEC# ATTRIBUTES DLLEXPORT :: ScipuffEndSendMPI

USE localmpi
USE mpi_fi

IMPLICIT NONE

INTEGER j, nch

DO j = 2,numprocs
  nch = -1
  CALL MPI_Send( nch, 1, MPI_INTEGER,j-1,tag_chunk,&
                 MPI_COMM_WORLD, ierr )
END DO

RETURN
END

!===============================================================================

SUBROUTINE PutChemAuxFrac( ID,p,frac )

! Puts back the multicomponent auxiliary data with the splitting fraction frac

USE scipuff_fi
USE chem_fi

IMPLICIT NONE

INTEGER,          INTENT( IN    ) :: ID
TYPE( puff_str ), INTENT( INOUT ) :: p
REAL,             INTENT( IN    ) :: frac

INTEGER i, nskp, nmc

nskp = typeID(p%ityp)%ipmc - 1

nmc = chemMC(ID)%nFast + chemMC(ID)%nSlow + chemMC(ID)%nParticle + chemMC(ID)%nEquilibrium

DO i = 1,nmc
  nskp = nskp + 1
  IF( chemMC(ID)%species(i)%mass /= NOT_SET_R )THEN
    p%aux(nskp) = chemMC(ID)%species(i)%mass * frac
  ELSE
    p%aux(nskp) = NOT_SET_R
  END IF
END DO

DO i = 1,chemMC(ID)%nStar
  nskp = nskp + 1
  IF( chemMC(ID)%species(i)%mass /= NOT_SET_R )THEN
    p%aux(nskp) = chemMC(ID)%star(i)%s%conc * frac *p%c
  ELSE
    p%aux(nskp) = NOT_SET_R
  END IF
END DO

IF( vol /= NOT_SET_R )THEN
  p%aux(nskp+1) = vol*frac*p%c
ELSE
  p%aux(nskp+1) = NOT_SET_R
END IF

IF( pres /= NOT_SET_R )THEN
  p%aux(nskp+2) = pres*frac*p%c
ELSE
  p%aux(nskp+2) = NOT_SET_R
END IF

IF( temp /= NOT_SET_R )THEN
  p%aux(nskp+3) = temp*frac*p%c
ELSE
  p%aux(nskp+3) = NOT_SET_R
END IF

IF( vself /= NOT_SET_R )THEN
  p%aux(nskp+4) = vself*frac*p%c
ELSE
  p%aux(nskp+4) = NOT_SET_R
END IF

RETURN
END

