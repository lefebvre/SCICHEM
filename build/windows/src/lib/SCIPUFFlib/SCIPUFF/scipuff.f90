!*******************************************************************************
!$RCSfile$
!$Revision$
!$Date$
!*******************************************************************************
MODULE StepScipuff_fi

! Stepping information

  SAVE

  LOGICAL lcontinue
  INTEGER istep, nstep, nend

END MODULE StepScipuff_fi

!===============================================================================

SUBROUTINE scipuff()

!-----  3d-puff dynamic model with second-order closure integrals

USE scipuff_fi
USE StepScipuff_fi
USE adjoint_fi
USE files_fi

IMPLICIT NONE

CHARACTER(80) cmsg,cmsg2,cmsg3

!------ Initialize the project for calculation

CALL InitScipuff()
IF( nError /= NO_ERROR )GOTO 9999

!------ main integration loop  -------

MainLoop: DO WHILE( lcontinue )

!------ increment output time

  IF( restart )THEN
    restart = .FALSE.
    IF( t >= t_save )THEN
      t_save = t_save + dt_save
    END IF
  ELSE
    t_save = t_save + dt_save
  END IF

!------- compute no. of steps to next output time or end of run

  nstep = INT((t_save-t)/delt+0.5)
  nend  = INT((tend_r-t)/delt+0.5)
  nstep = MIN(nstep,nend)
  nstep = MAX(nstep,1)

  istep     = 1
  lcontinue = .TRUE.

!-------- calculation loop

  DO WHILE( istep <= nstep .AND. lcontinue )

    CALL run( lcontinue,istep )
    IF( nError /= NO_ERROR )GOTO 9999

  END DO

!-------  output

  lcontinue = lcontinue .AND. (t+0.5*delt <= tend_r) .AND. (istop /= 2)

  CALL write_progress_bar( 0 )

  IF( .NOT.lcontinue .AND. npuf==0 )THEN

    eRoutine = 'scipuff'
    eMessage = 'Stopping run'
    WRITE(eInform,'(A,1PG11.4,A)')'No more puffs at Time = ',t/3600.,' hrs'
    CALL InfoMessage( )

!  ELSE IF( BTEST(run_mode,REVERSE_MODE) )THEN

!    IF( t <= tFirstTrigger )CYCLE MainLoop

    cmsg  = TRIM(name)//' : Writing output'
    cmsg2 = CHAR(0)
    cmsg3 = CHAR(0)
    CALL write_progress( cmsg,cmsg2,cmsg3 )
    IF( nError /= NO_ERROR )GOTO 9999

  END IF

  CALL output_all()
  IF( nError /= NO_ERROR )GOTO 9999

END DO MainLoop

9999 CONTINUE

!------ Exit SCIPUFF, even on error

CALL ExitScipuff( .FALSE. )

RETURN
END

!===============================================================================

SUBROUTINE InitScipuff()

!-----  Initialize SCIPUFF calculation

USE scipuff_fi
USE files_fi
USE StepScipuff_fi

IMPLICIT NONE

INTEGER ios

CHARACTER(12)           :: sysTime
CHARACTER(12), EXTERNAL :: sysGetTime

!------ set code version number

CALL set_version( iversion_code )
SCIPUFFinProgress = .TRUE.

!------ initialization

CALL initial()
IF( nError /= NO_ERROR )GOTO 9999

lsplit_report = .TRUE.
lcontinue     = .NOT.create
istop         = 0

nstep = INT((t_save-t)/delt+0.5)
nend  = INT((tend_r-t)/delt+0.5)
nstep = MIN(nstep,nend)
nstep = MAX(nstep,1)

istep = 0

!------ exit if only doing CREATE

IF( create )GOTO 9999

!------ setup for integration

sysTime = SysGetTime()

WRITE(lun_log,111,IOSTAT=ios)'Starting run at t =',t/3600., &
                             'hrs. with NPUFF = ',npuf,' at ',sysTime
111 FORMAT(A,1PG11.4,A,I5,2A)
IF( ios /= 0 )THEN
  nError   = WR_ERROR
  eRoutine = 'scipuff'
  eMessage = 'Error writing SCIPUFF log file'
  CALL ReportFileName( eInform,'File=',file_log )
  GOTO 9999
END IF

CALL enableSCIPUFFhalt( istop )

9999 CONTINUE

RETURN
END

!===============================================================================

LOGICAL FUNCTION StepScipuff( tStep )

USE scipuff_fi
USE StepScipuff_fi

IMPLICIT NONE

REAL, INTENT( OUT ) :: tStep    !SCIPUFF time at end of step

!-----  Take single SCIPUFF step

CHARACTER(80) cmsg,cmsg2,cmsg3

StepScipuff = .FALSE.

IF( .NOT.lcontinue )GOTO 9999

IF( istep == 0 )THEN

!------ increment output time

  IF( restart )THEN
    restart = .FALSE.
    IF( t >= t_save )THEN
      t_save = t_save + dt_save
    END IF
  ELSE
    t_save = t_save + dt_save
  END IF

!------- compute no. of steps to next output time or end of run

  nstep = INT((t_save-t)/delt+0.5)
  nend  = INT((tend_r-t)/delt+0.5)
  nstep = MIN(nstep,nend)
  nstep = MAX(nstep,1)

END IF

!-------- calculation step

CALL run( lcontinue,istep )
IF( nError /= NO_ERROR )GOTO 9999

lcontinue = lcontinue .AND. (t+0.5*delt <= tend_r) .AND. (istop /= 2)

!------- Check for output

IF( istep == nstep )THEN

  CALL write_progress_bar( 0 )

  IF( .NOT.lcontinue .AND. npuf==0 )THEN

    eRoutine = 'scipuff'
    eMessage = 'Stopping run'
    WRITE(eInform,'(A,1PG11.4,A)')'No more puffs at Time = ',t/3600.,' hrs'
    CALL InfoMessage( )

  ELSE

    cmsg  = TRIM(name)//' : Writing output'
    cmsg2 = CHAR(0)
    cmsg3 = CHAR(0)
    CALL write_progress( cmsg,cmsg2,cmsg3 )
    IF( nError /= NO_ERROR )GOTO 9999

  END IF

  CALL output_all()
  IF( nError /= NO_ERROR )GOTO 9999

!----- Reset step counter

  istep = 0

END IF

StepScipuff = lcontinue

9999 CONTINUE

tStep = t

RETURN
END

!===============================================================================

SUBROUTINE ExitScipuff( ssFlag )

USE scipuff_fi
USE StepScipuff_fi
USE met_fi
USE SWIMparam_fd
USE sampler_fi

IMPLICIT NONE

!-----  Exit SCIPUFF calculation

LOGICAL, INTENT( IN ) :: ssFlag  !Flag for Single Step mode

INTEGER irv

CHARACTER(80) cmsg,cmsg2,cmsg3

INTEGER, EXTERNAL :: SWIMupdateMet
INTEGER, EXTERNAL :: SWIMoutput

IF( nError /= NO_ERROR )GOTO 9999

!------ End of integration loop - show just abort button

CALL enableSCIPUFFhalt( SCIPUFF_HALT )

!------ Check output at end of run for Single Step mode

IF( istep /= nstep .AND. ssFlag )THEN
  CALL output_all()
  IF( nError /= NO_ERROR )GOTO 9999
END IF

!------ output met at end of run

IF( .NOT.create .AND. lout_met .AND. t > timeOutMet )THEN

!------ interpolate met to current time & set turbulence

  irv = SWIMupdateMet( t,SWIMstatus )
  CALL enableSCIPUFFhalt( SCIPUFF_HALT )  !Show just abort button
  IF( irv /= SWIMsuccess )THEN
    CALL setSWIMerror( 'SWIMupdateMet' )
    GOTO 9999
  END IF

  CALL WriteSWIMlog()

!------ output met fields

  cmsg  = 'Outputting wind field at end of run'
  cmsg2 = ' '
  cmsg3 = ' '
  CALL write_progress( cmsg,cmsg2,cmsg3 )
  IF( nError /= NO_ERROR )GOTO 9999
  irv = SWIMoutput()
  IF( irv /= SWIMsuccess )THEN
    CALL setSWIMerror( 'SWIMoutput' )
    GOTO 9999
  END IF
  CALL WriteSWIMlog()

END IF

!------ Output current (partial) sampler averages if not at output time

IF( .NOT.create )THEN
  IF( nsmp > 0 )THEN
    IF( (int_sensor .AND. lUpdateSum) .AND. (lSmpOut .OR. lavg) )THEN
      IF( t > tStartSamp )THEN
        IF( lavg )THEN
          IF( .NOT.lOutToSmp )CALL out_smp()
        ELSE
          IF( t < tSmpOut-tolSmpOut )CALL out_smp()
        END IF
      END IF
    END IF
  END IF
END IF

IF( nsmp > 0 )THEN
  CALL finalize_binary_sampler()
  IF( nError /= NO_ERROR )GOTO 9999
END IF

9999 CONTINUE

!------ close down

CALL end_scipuff()
CALL enableSCIPUFFhalt( SCIPUFF_DISABLE )
SCIPUFFinProgress = .FALSE.

RETURN
END

!===============================================================================

SUBROUTINE output_all()

USE scipuff_fi
USE surface_fi
USE files_fi
USE sagdef_fd
USE chem_fi
USE cont_rel_functions
USE StepScipuff_fi
USE met_fi
USE SWIMparam_fd
USE SCIPresults_fd

IMPLICIT NONE

CHARACTER(80) cmsg,cmsg2,cmsg3

INTEGER ios, irv
INTEGER i
INTEGER ndos_blocks
REAL    tdum

REAL tmp

INTEGER, EXTERNAL :: SAG_WriteBreakID
INTEGER, EXTERNAL :: SWIMupdateMet
INTEGER, EXTERNAL :: SetMetGrid
INTEGER, EXTERNAL :: SAG_ClearGridID
INTEGER, EXTERNAL :: SAG_OpenID, SAG_LastTime, SAG_CloseID

CHARACTER(12)           :: sysTime
CHARACTER(12), EXTERNAL :: sysGetTime

cmsg = TRIM(name)//' : Saving Output'
CALL time_message( cmsg2,t )
cmsg3 = ' '
CALL write_progress( cmsg,cmsg2,cmsg3 )

!------ Update met to current time
!       N.B. Mainly for saved BL depth and subsequent restarts

IF( .NOT.create )THEN
  irv = SWIMupdateMet( t,SWIMstatus )
  CALL enableSCIPUFFhalt( SCIPUFF_HALT )  !Show just abort button
  IF( irv /= SWIMsuccess )THEN
    CALL setSWIMerror( 'SWIMupdateMet' )
    GOTO 9999
  END IF

  IF( BTEST(SWIMstatus,SSB_EXPAND) )THEN
    irv = SetMetGrid()
    IF( irv /= SCIPsuccess )GOTO 9999
  END IF

  CALL WriteSWIMlog()
END IF

CALL compress_Definition()

CALL write_puff()
IF( nError /= NO_ERROR )GOTO 9999

cmsg3 = 'Writing project file'
CALL write_progress( cmsg,cmsg2,cmsg3 )

CALL write_prj()
IF( nError /= NO_ERROR )GOTO 9999

IF( surface .OR. dose )THEN
  cmsg3 = 'Writing surface file(s)'
  CALL write_progress( cmsg,cmsg2,cmsg3 )
END IF

IF( surface )THEN
  tmp = t/3600.
  irv = SAG_WriteBreakID( srfdep,tmp )
  IF( irv /= SAG_OK )THEN
    nError = WR_ERROR
    eRoutine = 'output_all'
    eMessage = 'Error writing surface deposition file'
    GOTO 9999
  END IF
  IF( BTEST(run_mode,DINCRMNT) )THEN
    IF( istep >= nstep )THEN
      irv = SAG_ClearGridID( srfdep )
      IF( irv /= SAG_OK )THEN
        nError = WR_ERROR
        eRoutine = 'output_all'
        eMessage = 'Error clearing surface dep grid'
        GOTO 9999
      END IF
      tLastDep = t
    END IF
  END IF
END IF

IF( dose )THEN
  tmp = t/3600.
  IF( BTEST(run_mode,REVERSE_MODE) )THEN
    ndos_blocks = ntypd/3
    DO i = 1,ndos_blocks
      irv = SAG_OpenID( srfdosAdj(i) )
      IF( irv /= SAG_OK )THEN
        nError = UK_ERROR
        eRoutine = 'output_all'
        eMessage = 'Error closing surface dose field'
        GOTO 9999
      END IF

      irv = SAG_LastTime( srfdosAdj(i),tdum,.TRUE. )
      IF( irv /= SAG_OK )THEN
        nError = WR_ERROR
        eRoutine = 'output_all'
        eMessage = 'Error positioning surface dose file'
        GOTO 9999
      END IF

      irv = SAG_WriteBreakID( srfdosAdj(i),tmp )
      IF( irv /= SAG_OK )THEN
        nError = WR_ERROR
        eRoutine = 'output_all'
        eMessage = 'Error writing surface dose file'
        GOTO 9999
      END IF

      irv = SAG_CloseID( srfdosAdj(i) )
      IF( irv /= SAG_OK )THEN
        nError = WR_ERROR
        eRoutine = 'output_all'
        eMessage = 'Error closing surface dose field'
        GOTO 9999
      END IF
    END DO
  ELSE

  IF( multicomp )THEN
    CALL StepMCDoseEq()
    IF( nError /= NO_ERROR )THEN
      nError = WR_ERROR
      eRoutine = 'output_all'
      eMessage = 'Error stepping MC equilibrium for surface dose file'
      GOTO 9999
    END IF
  END IF
  irv = SAG_WriteBreakID( srfdos,tmp )
  IF( irv /= SAG_OK )THEN
    nError = WR_ERROR
    eRoutine = 'output_all'
    eMessage = 'Error writing surface dose file'
    GOTO 9999
  END IF
  ! Ambient dose
  IF( multicomp )THEN
    irv = SAG_WriteBreakID( srfados,tmp )
    IF( irv /= SAG_OK )THEN
      nError = WR_ERROR
      eRoutine = 'output_all'
      eMessage = 'Error writing surface ambient dose file'
      GOTO 9999
    END IF
  END IF

  IF( BTEST(run_mode,DINCRMNT) )THEN
    IF( istep >= nstep )THEN
      irv = SAG_ClearGridID( srfdos )
      IF( irv /= SAG_OK )THEN
        nError = WR_ERROR
        eRoutine = 'output_all'
        eMessage = 'Error clearing surface dos grid'
        GOTO 9999
      END IF
     ! Ambient dose
      IF( multicomp )THEN
        irv = SAG_ClearGridID( srfados )
        IF( irv /= SAG_OK )THEN
          nError = WR_ERROR
          eRoutine = 'output_all'
          eMessage = 'Error clearing surface ambient dos grid'
          GOTO 9999
        END IF
      END IF
      tLastDos = t
    END IF
  END IF

  END IF
END IF

cmsg3 = ' '
CALL write_progress( cmsg,cmsg2,cmsg3 )

IF( multicomp )THEN
  WRITE(lun_log,112,IOSTAT=ios)'Number of succesful chemistry steps =',ngd_chem
  WRITE(lun_log,112,IOSTAT=ios)'Number of aborted chemistry steps   =',nbad_chem
  112 FORMAT(A,1PG11.4)
  ngd_chem  = 0
  nbad_chem = 0
  CALL CreateChemOutput()
END IF

sysTime = SysGetTime()
WRITE(lun_log,111,IOSTAT=ios)'Output completed at t =',t/3600., &
                    'hrs. with NCREL = ',count_nrel(),' and NPUFF = ', &
                     npuf,' at ',sysTime
111 FORMAT(A,1PG11.4,A,I4,A,I5,A,A)

IF( ios /= 0 )THEN
  nError   = WR_ERROR
  eRoutine = 'output_all'
  eMessage = 'Error writing SCIPUFF log file'
  CALL ReportFileName( eInform,'File=',file_log )
  GOTO 9999
END IF

9999 CONTINUE

RETURN
END

!===============================================================================

SUBROUTINE end_scipuff()

USE scipuff_fi
USE files_fi
USE mpi_fi, ONLY: useMPI

IMPLICIT NONE

INTEGER ios, nch

CHARACTER(80) cmsg,cmsg2,cmsg3

INTEGER, EXTERNAL :: SWIMexitRun

cmsg = TRIM(name)//' : Exiting SCIPUFF'
cmsg2 = ' '
cmsg3 = ' '
CALL write_progress( cmsg,cmsg2,cmsg3 )

IF( nError == NO_ERROR )THEN
  eRoutine = 'EndScipuff'
  WRITE(eInform,'(A,I5)')'No. Puffs = ',npuf
  IF( istop < 0 )THEN
    nError = UK_ERROR
    WRITE(eMessage,'(A,1PG11.4,A)')'Abnormal termination detected at Time = ',t/3600.,' hrs'
  ELSE IF( istop > 0 )THEN
    nError = S0_ERROR
    SELECT CASE( istop )
      CASE (1)
        WRITE(eMessage,'(A,1PG11.4,A)')'User requested Halt detected at Time = ',t/3600.,' hrs'
      CASE (2)
        WRITE(eMessage,'(A,1PG11.4,A)')'User StopOnOutput request detected at Time = ',t/3600.,' hrs'
      CASE DEFAULT
        WRITE(eMessage,'(A,1PG11.4,A)')'Unknown Halt detected at Time = ',t/3600.,' hrs'
    END SELECT
    WRITE(eInform,'(A,I5)')'No. Puffs = ',npuf
  ELSE
    WRITE(eMessage,'(A,1PG11.4,A)')'Normal termination detected at Time = ',t/3600.,' hrs'
  END IF
END IF

IF( nError == S0_ERROR )THEN
  WRITE(lun_log,'(A)',IOSTAT=ios)'************HALT***************'
ELSE IF( nError == WN_ERROR )THEN
  WRITE(lun_log,'(A)',IOSTAT=ios)'**********WARNING**************'
ELSE IF( nError /= NO_ERROR )THEN
  WRITE(lun_log,'(A)',IOSTAT=ios)'***********ERROR***************'
ELSE
  WRITE(lun_log,'(A)',IOSTAT=ios)'************DONE***************'
END IF

nch = LEN_TRIM(eRoutine)
IF( nch > 1 )THEN
  WRITE(lun_log,'(A)',IOSTAT=ios)TRIM(eRoutine)
END IF
WRITE(lun_log,'(A)',IOSTAT=ios)TRIM(eMessage)
WRITE(lun_log,'(A)',IOSTAT=ios)TRIM(eInform)
nch = LEN_TRIM(eAction)
IF( nch > 1 )THEN
  WRITE(lun_log,'(A)',IOSTAT=ios)TRIM(eAction)
END IF
WRITE(lun_log,'(A)',IOSTAT=ios)'*********************************'

IF( ios /= 0 )GOTO 9998

9997 CONTINUE

OPEN(UNIT=lun_err,FILE=file_err,STATUS='UNKNOWN',IOSTAT=ios)
IF( ios /= 0 )THEN
  nError   = OP_ERROR
  eRoutine = 'end_scipuff'
  eMessage = 'Error opening SCIPUFF error file'
  CALL ReportFileName( eInform,'File=',file_err )
ELSE
  WRITE(lun_err,'(I6,A)',IOSTAT=ios)nError,' :Scipuff Exit Status'
  WRITE(lun_err,'(A)',IOSTAT=ios)TRIM(eRoutine)
  WRITE(lun_err,'(A)',IOSTAT=ios)TRIM(eMessage)
  WRITE(lun_err,'(A)',IOSTAT=ios)TRIM(eInform)
  WRITE(lun_err,'(A)',IOSTAT=ios)TRIM(eAction)
  CLOSE(UNIT=lun_err,IOSTAT=ios)
END IF

CLOSE(UNIT=lun_puf,IOSTAT=ios)
CLOSE(UNIT=lun_prj,IOSTAT=ios)
CLOSE(UNIT=lun_tmp,IOSTAT=ios)
CLOSE(UNIT=lun_inp,IOSTAT=ios)
CLOSE(UNIT=lun_scn,IOSTAT=ios)
CLOSE(UNIT=lun_mcw,IOSTAT=ios)
CLOSE(UNIT=lun_msc,IOSTAT=ios)
CLOSE(UNIT=lun_met,IOSTAT=ios)
CLOSE(UNIT=lun_sfc,IOSTAT=ios)
CLOSE(UNIT=lun_ter,IOSTAT=ios)
CLOSE(UNIT=lun_smp,IOSTAT=ios)
CLOSE(UNIT=lun_sps,IOSTAT=ios)
IF( useMPI .AND. .NOT. create )THEN
  CALL ScipuffEndSendMPI( )
END IF
CLOSE(UNIT=lun_dgn,IOSTAT=ios)
CLOSE(UNIT=lun_amr,IOSTAT=ios)
IF( multicomp )CLOSE(UNIT=lun_asmp,IOSTAT=ios)

CALL scipuff_deallocate()

!------ Get last SWIM log messages & deallocate arrays

ios = SWIMexitRun()

CALL WriteSWIMlog()

RETURN

!------ set log write error and goto return

9998 CONTINUE

nError   = WR_ERROR
eRoutine = 'end_scipuff'
eMessage = 'Error writing SCIPUFF log file'
CALL ReportFileName( eInform,'File=',file_log )
GOTO 9997

END
