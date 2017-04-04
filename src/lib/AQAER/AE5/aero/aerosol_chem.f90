!*********************************************************************** 
! SCICHEM driver routine for the CMAQ-AE5 aerosol module               *
! This code has been developed for EPRI, Palo Alto, CA 94304           *
! Contract EP-P41335/C18216                                            *
!                                                                      * 
! Developed by Prakash Karamchandani                                   * 
! ENVIRON, Novato, CA 94998                                            * 
!*********************************************************************** 
subroutine step_aerosol_chem(dt,concamb,conc)
!******************************************************************************
!
! FUNCTION:  Driver for aerosol routines
!
! PRECONDITIONS REQUIRED:
!
! SUBROUTINES AND FUNCTIONS CALLED:
!               sat_humid            aeroproc              getpar
!
! REVISION HISTORY:
!      Version 2.0, by Prakash Karamchandani, November 2013
!      Based on CMAQ-AE5 aerosol module in CMAQ 4.7.1
!      ENVIRON
!      773 San Marin Drive, Suite 2115, Novato, CA 94998
!
! REVISION HISTORY:
! Additional updates for SCICHEM Beta-2 release, Nov 2013, PK, ENVIRON *
!******************************************************************************

! --- MODULES

USE AqAer_fi
USE AERO_INFO      ! from CMAQ
USE mpi_fi, ONLY: myid

implicit none

! --- ARGUMENTS

real dt              ! time step (seconds)
real conc(*)         ! array of species puff concentrations
real concamb(*)      ! array of species ambient concentrations

! --- LOCALS

! --- Total concs (plume + ambient)
REAL, DIMENSION(MAX_SP) :: conctot     ! array of species total concentrations

REAL GAMMA_N2O5       ! N2O5 heterogeneous reaction probability [ ]
! factors for converting from moles/m3 to ug/m3
REAL, PARAMETER :: H2SO4CONV = 1.0E6 * MWH2SO4
REAL, PARAMETER :: HNO3CONV  = 1.0E6 * MWHNO3
REAL, PARAMETER :: NH3CONV   = 1.0E6 * MWNH3
REAL, PARAMETER :: N2O5CONV  = 1.0E6 * MWN2O5
REAL, PARAMETER :: HCLCONV   = 1.0E6 * MWHCL
REAL, PARAMETER :: NO2CONV   = 1.0E6 * MWNO2
REAL, PARAMETER :: HONOCONV  = 1.0E6 * MWHONO
REAL, PARAMETER :: VALKCONV  = 1.0E6 * MWVALK
REAL, PARAMETER :: VXYLCONV  = 1.0E6 * MWVXYL
REAL, PARAMETER :: VTOLCONV  = 1.0E6 * MWVTOL
REAL, PARAMETER :: VBNZCONV  = 1.0E6 * MWVBNZ
REAL, PARAMETER :: VTRPCONV  = 1.0E6 * MWVTRP
REAL, PARAMETER :: VISOCONV  = 1.0E6 * MWVISO
REAL, PARAMETER :: VSQTCONV  = 1.0E6 * MWVSQT

! factors for converting from ug/m3 to moles/m3
REAL, PARAMETER :: H2SO4CONV1 = 1.0 / H2SO4CONV
REAL, PARAMETER :: HNO3CONV1  = 1.0 / HNO3CONV
REAL, PARAMETER :: NH3CONV1   = 1.0 / NH3CONV
REAL, PARAMETER :: N2O5CONV1  = 1.0 / N2O5CONV
REAL, PARAMETER :: HCLCONV1   = 1.0 / HCLCONV
REAL, PARAMETER :: NO2CONV1   = 1.0 / NO2CONV
REAL, PARAMETER :: HONOCONV1  = 1.0 / HONOCONV
REAL, PARAMETER :: VALKCONV1  = 1.0 / VALKCONV
REAL, PARAMETER :: VXYLCONV1  = 1.0 / VXYLCONV
REAL, PARAMETER :: VTOLCONV1  = 1.0 / VTOLCONV
REAL, PARAMETER :: VBNZCONV1  = 1.0 / VBNZCONV
REAL, PARAMETER :: VTRPCONV1  = 1.0 / VTRPCONV
REAL, PARAMETER :: VISOCONV1  = 1.0 / VISOCONV
REAL, PARAMETER :: VSQTCONV1  = 1.0 / VSQTCONV

! Minimum time step (for equilibrium only calculations)
REAL, PARAMETER :: DTMIN = 1.E-20

! Molecular weight ratios
REAL, PARAMETER :: NH3RAT  = MWNH3  / MWNH4, &
                   NO3RAT = MWHNO3 / MWNO3, &
                   HCLRAT  = MWHCL  / MWCL, &
                   H2SO4RAT = MWH2SO4 / MWSO4, &
                   NO2RAT = MWHNO3 / MWNO2, &
                   HONORAT = MWHNO3 / MWHONO, &
                   N2O5RAT = MWHNO3 / MWN2O5

! *** factors to set minimum value for number concentrations
REAL :: NUMMIN_AT   ! Aitken mode
REAL :: NUMMIN_AC   ! accumulation mode
REAL :: NUMMIN_CO   ! coarse mode

! *** factors to set minimum value for second moment
REAL :: M2MIN_AT    ! Aitken  mode
REAL :: M2MIN_AC    ! accumulation mode
REAL :: M2MIN_CO    ! coarse mode

! *** chemical production rates: [ ug / m**3 s ]
real so4rate      ! sulfate gas-phase production rate 

! *** modal diameters [ m ]

real dgatk           ! Aitken mode geometric mean diameter  [ m ]
real dgacc           ! accumulation geometric mean diameter [ m ]
real dgcor           ! coarse mode geometric mean diameter  [ m ] 

! *** log of modal geometric standard deviation

real xxlsgat         ! Aitken mode
real xxlsgac         ! accumulation mode
real xxlsgco         ! coarse mode

REAL      ESC36      ! exp( 4.5 * log^2 (sigmag) )      

! *** aerosol properties: 

! *** modal mass concentrations [ ug m**3 ]

real pmassat         ! mass concentration in Aitken mode 
real pmassac         ! mass concentration in accumulation mode
real pmassco         ! mass concentration in coarse mode 

! *** average modal particle densities  [ kg/m**3 ]

real pdensat         ! average particle density in Aitken mode 
real pdensac         ! average particle density in accumulation mode 
real pdensco         ! average particle density in coarse mode  

! *** variables to set up for "dry transport "
real m3_wet, m3_dry   ! third moment with and without water
real m2_wet, m2_dry   ! second moment with and without water
real m3subt           ! temp variable for dry 3rd moment calcs

! flag to include water in the 3rd moment calculation
LOGICAL, PARAMETER :: M3_WET_FLAG = .FALSE.

! *** if LIMIT_Sg = T, atkn & accum std. dev. are not changed by GETPAR

LOGICAL, PARAMETER :: LIMIT_Sg = .FALSE.

integer spc, vv   ! loop variables
integer i, j, ii
real hsx, pmb, rh, factor, factor1, pres_pa

! dummy variables for consistency with CMAQ routines
integer col, row, layer
real airdens, xlm, amu

! plume to total concentration ratios
!REAL RNO2, RHONO, RN2O5, RNO3, RNH4, RSO4, RNA, RCL, RNOY, OMRNOY
REAL RNOY, OMRNOY, RNH4, OMRNH4, RSO4, OMRSO4
REAL RNA, OMRNA, RCL, OMRCL
!REAL RNUMAT, RSURFAT, RNUMAC, RSURFAC, RNUMCO, RSURFCO
REAL PLUMECONC, TOTALCONC

! ratios for organic species (SOAs and semi-volatiles)
REAL RALK, RXYL, RTOL, RBNZ, RTRP, RISO, RSQT, &
     OMRALK, OMRXYL, OMRTOL, OMRBNZ, OMRTRP, OMRISO, OMRSQT
REAL RXYL3, RTOL3, RBNZ3, OMRXYL3, OMRTOL3, OMRBNZ3

! SOA formed from oligomerization
REAL ROLGA, ROLGB, OMROLGA, OMROLGB

! initial concentrations of HNO3, ANO3, NH3, ANH4, H2SO4, SO4, HCL, CL
! and changes in concentrations

!REAL chno30, cno3j0, cno3i0, dhno3, dno3j, dno3i

!REAL cno20, dno2, chono0, dhono, cn2o50, dn2o5

!REAL cnh30, cnh4j0, cnh4i0, dnh3, dnh4j, dnh4i

!REAL ch2so40, cso4j0, cso4i0, dh2so4, dso4j, dso4i

!REAL chcl0, cclj0, ccli0, dhcl, dclj, dcli

!REAL cnaj0, cnai0, dnaj, dnai

! Initial concs of organic species
!REAL CVALK0, CALKJ0, CVXYL10, CXYL1J0, CVXYL20, CXYL2J0, &
!     CVTOL10, CTOL1J0, CVTOL20, CTOL2J0, &
!     CVBNZ10, CBNZ1J0, CVBNZ20, CBNZ2J0, &
!     CVTRP10, CTRP1J0, CVTRP20, CTRP2J0, &
!     CVISO10, CISO1J0, CVISO20, CISO2J0, &
!     CVSQT0, CSQTJ0 

LOGICAL, SAVE :: FIRSTTIME = .TRUE.

if (FIRSTTIME) then

   FIRSTTIME = .FALSE.

! Species mapping for particle species
   call init_aero_map(aero_names,naerp,cblk_map,depv_sur)
   IF( error%nError /= NO_ERROR )return
! set internal species pointers for non-AE species
! inorganic gases:
   do spc = naerp+1, naero
      if (TRIM(aero_names(spc)) == 'SULF') then
         LSULF = spc
         CYCLE
      end if
      if (TRIM(aero_names(spc)) == 'HNO3') then
         LHNO3 = spc
         CYCLE
      end if
      if (TRIM(aero_names(spc)) == 'N2O5') then
         LN2O5 = spc
         CYCLE
      end if
      if (TRIM(aero_names(spc)) == 'NO2') then
         LNO2 = spc
         CYCLE
      end if
      if (TRIM(aero_names(spc)) == 'HONO') then
         LHONO = spc
         CYCLE
      end if
      if (TRIM(aero_names(spc)) == 'HCL') then
         LHCL = spc
         CYCLE
      end if
      if (TRIM(aero_names(spc)) == 'NH3') then
         LNH3 = spc
         CYCLE
      end if

! reaction products
      if (TRIM(aero_names(spc)) == 'ALKRXN') then
         LALKRXN = spc
         CYCLE
      end if
      if (TRIM(aero_names(spc)) == 'XYLNRXN') then
         LXYLNRXN = spc
         CYCLE
      end if
      if (TRIM(aero_names(spc)) == 'XYLHRXN') then
         LXYLHRXN = spc
         CYCLE
      end if
      if (TRIM(aero_names(spc)) == 'TOLNRXN') then
         LTOLNRXN = spc
         CYCLE
      end if
      if (TRIM(aero_names(spc)) == 'TOLHRXN') then
         LTOLHRXN = spc
         CYCLE
      end if
      if (TRIM(aero_names(spc)) == 'BNZNRXN') then
         LBNZNRXN = spc
         CYCLE
      end if
      if (TRIM(aero_names(spc)) == 'BNZHRXN') then
         LBNZHRXN = spc
         CYCLE
      end if
      if (TRIM(aero_names(spc)) == 'TRPRXN') then
         LTRPRXN = spc
         CYCLE
      end if
      if (TRIM(aero_names(spc)) == 'ISOPRXN') then
         LISOPRXN = spc
         CYCLE
      end if
      if (TRIM(aero_names(spc)) == 'SESQRXN') then
         LSESQRXN = spc
         CYCLE
      end if
      if (TRIM(aero_names(spc)) == 'SULRXN') then
         LSULFP = spc
         CYCLE
      end if

! *** Set pointers for vapor-phase semi-volatile organic compounds
      if (TRIM(aero_names(spc)) == 'SV_ALK') then
         LVALK = spc
         CYCLE
      end if
      if (TRIM(aero_names(spc)) == 'SV_XYL1') then
         LVXYL1 = spc
         CYCLE
      end if
      if (TRIM(aero_names(spc)) == 'SV_XYL2') then
         LVXYL2 = spc
         CYCLE
      end if
      if (TRIM(aero_names(spc)) == 'SV_TOL1') then
         LVTOL1 = spc
         CYCLE
      end if
      if (TRIM(aero_names(spc)) == 'SV_TOL2') then
         LVTOL2 = spc
         CYCLE
      end if
      if (TRIM(aero_names(spc)) == 'SV_BNZ1') then
         LVBNZ1 = spc
         CYCLE
      end if
      if (TRIM(aero_names(spc)) == 'SV_BNZ2') then
         LVBNZ2 = spc
         CYCLE
      end if
      if (TRIM(aero_names(spc)) == 'SV_TRP1') then
         LVTRP1 = spc
         CYCLE
      end if
      if (TRIM(aero_names(spc)) == 'SV_TRP2') then
         LVTRP2 = spc
         CYCLE
      end if
      if (TRIM(aero_names(spc)) == 'SV_ISO1') then
         LVISO1 = spc
         CYCLE
      end if
      if (TRIM(aero_names(spc)) == 'SV_ISO2') then
         LVISO2 = spc
         CYCLE
      end if
      if (TRIM(aero_names(spc)) == 'SV_SQT') then
         LVSQT = spc
         CYCLE
      end if
   end do
!debug
   !WRITE(99+myid,*)'lsulf,lhno3,lnh3,ln2o5,lhcl,lno2,lhono: ', &
              !lsulf,lhno3,lnh3,ln2o5,lhcl,lno2,lhono
   !WRITE(99+myid,*)'lsulfp,lalkrxn,lisoprxn,ltrprxn,ltolnrxn,ltolhrxn: ', &
              !lsulfp,lalkrxn,lisoprxn,ltrprxn,ltolnrxn,ltolhrxn
   !WRITE(99+myid,*)'lxylnrxn,lxylhrxn,lbnznrxn,lbnzhrxn,lsesqrxn: ', &
              !lxylnrxn,lxylhrxn,lbnznrxn,lbnzhrxn,lsesqrxn
   !WRITE(99+myid,*)'lvalk,lvxyl1,lvxyl2,lvtol1,lvtol2,lvbnz1: ', &
              !lvalk,lvxyl1,lvxyl2,lvtol1,lvtol2,lvbnz1
   !WRITE(99+myid,*)'lvbnz2,lvtrp1,lvtrp2,lviso1,lviso2,lvsqt: ', &
              !lvbnz2,lvtrp1,lvtrp2,lviso1,lviso2,lvsqt
!debug

! Check for required species
   if (lsulf == 0 .or. lhno3 == 0 .or. ln2o5 == 0 .or. lno2 == 0 .or. &
       lhono == 0 .or. lhcl  == 0 .or. lnh3 ==  0 .or. lsulfp == 0 ) then
       error%nError   = IV_ERROR
       error%eRoutine = 'step_aerosol_chem'
       error%eMessage = 'Necessary inorganic gaseous species for aerosol module' &
                   // '  not found in species list'
       return
   end if

   if (lvalk  == 0 .or. lvxyl1 == 0 .or. lvxyl2 == 0 .or. lvtol1 == 0 .or. &
       lvtol2 == 0 .or. lvbnz1 == 0 .or. lvbnz2 == 0 .or. lvtrp1 == 0 .or. &
       lvtrp2 == 0 .or. lviso1 == 0 .or. lviso2 == 0 .or. lvsqt  == 0) then
       error%nError   = IV_ERROR
       error%eRoutine = 'step_aerosol_chem'
       error%eMessage = 'Necessary semi-volatile organic species for ' &
                   // ' aerosol module not found in species list'
       return
   end if

! *** calculate minimum values for number and 2nd moment.

  xxlsgat = LOG( SGINIAT ); xxlsgac = LOG( SGINIAC ); xxlsgco = LOG( SGINICO )
  ESC36   = EXP( 4.5 * xxlsgco * xxlsgco )

  NUMMIN_AT = SO4FAC * AEROCONCMIN_AT &
              / ( DGINIAT ** 3 * EXP( 4.5 * xxlsgat * xxlsgat ) )
  NUMMIN_AC = SO4FAC * AEROCONCMIN_AC &
              / ( DGINIAC ** 3 * EXP( 4.5 * xxlsgac * xxlsgac ) )
  NUMMIN_CO = ANTHFAC * AEROCONCMIN_CO / ( DGINICO**3 * ESC36 )

  M2MIN_AT = NUMMIN_AT * DGINIAT ** 2 * EXP( 2.0 * xxlsgat * xxlsgat )
  M2MIN_AC = NUMMIN_AC * DGINIAC ** 2 * EXP( 2.0 * xxlsgac * xxlsgac )
  M2MIN_CO = NUMMIN_CO * DGINICO ** 2 * EXP( 2.0 * xxlsgco * xxlsgco )

  AECONMIN = CONMIN

  AECONMIN( VAT0 ) = NUMMIN_AT
  AECONMIN( VAC0 ) = NUMMIN_AC
  AECONMIN( VCO0 ) = NUMMIN_CO

  AECONMIN( VSURFAT ) = M2MIN_AT
  AECONMIN( VSURFAC ) = M2MIN_AC
  AECONMIN( VSURFCO ) = M2MIN_CO

  AECONMIN( VSO4AI ) = AEROCONCMIN_AT
  AECONMIN( VSO4AJ ) = AEROCONCMIN_AC
  AECONMIN( VANTHA ) = AEROCONCMIN_CO

!debug
  !WRITE(99+myid,*)'AECONMIN: ',AECONMIN
!debug

end if  ! If first time

! --- calculate humidity mixing ratio at saturation
pmb = patm*1013.25
call sat_humid(tk,pmb,hsx)
rh = hb/hsx
rh = MIN(rh,1.0)
rh = MAX(rh,0.01)

! --- pressure in pascals
pres_pa = patm * STDATMPA

! --- Set conversion factors for concentrations
! --- ppm or molecules/cc to moles/m3
if (ic_units == UNIT_PPM) then
 factor = 1.E-3/(298.*RGAS)
!   factor = patm*1.E-3/(tk*RGAS)
else if (ic_units == UNIT_MOLECULE) then
 factor = 1.3634E-19/(RGAS)
end if

! *** initialize CBLK

cblka = 0.0  ! Ambient concs
cblkp = 0.0  ! Plume concs
cblk  = 0.0  ! Total concs

! --- Fill CBLK array
! --- First fill aerosol species
do spc = 1, naerp
   cblka(spc) = concamb(cblk_map(spc))
   cblkp(spc) = conc(cblk_map(spc))
   cblk(spc)  = MAX( AECONMIN(spc), cblka(spc) + cblkp(spc) )
end do

! *** Add gas and vapor phase species to CBLK
cblka( VSULF ) = concamb( LSULF ) * factor * H2SO4CONV
cblkp( VSULF ) = conc( LSULF ) * factor * H2SO4CONV
cblk ( VSULF ) = MAX( CONMIN, cblka(VSULF) + cblkp(VSULF) )

cblka( VHNO3 ) = concamb( LHNO3 ) * factor * HNO3CONV
cblkp( VHNO3 ) = conc( LHNO3 ) * factor * HNO3CONV
cblk ( VHNO3 ) = MAX( CONMIN, cblka(VHNO3) + cblkp(VHNO3) )

cblka( VNH3  ) = concamb( LNH3  ) * factor * NH3CONV
cblkp( VNH3  ) = conc( LNH3  ) * factor * NH3CONV
cblk ( VNH3  ) = MAX( CONMIN, cblka(VNH3) + cblkp(VNH3) )

cblka( VHCL  ) = concamb( LHCL  ) * factor * HCLCONV
cblkp( VHCL  ) = conc( LHCL  ) * factor * HCLCONV
cblk ( VHCL  ) = MAX( CONMIN, cblka(VHCL) + cblkp(VHCL) )

cblka( VN2O5 ) = concamb( LN2O5 ) * factor * N2O5CONV
cblkp( VN2O5 ) = conc( LN2O5 ) * factor * N2O5CONV
cblk ( VN2O5 ) = MAX( CONMIN, cblka(VN2O5) + cblkp(VN2O5) )

cblka( VNO2  ) = concamb( LNO2  ) * factor * NO2CONV
cblkp( VNO2  ) = conc( LNO2  ) * factor * NO2CONV
cblk ( VNO2  ) = MAX( CONMIN, cblka(VNO2) + cblkp(VNO2) )

cblka( VHONO ) = concamb( LHONO ) * factor * HONOCONV
cblkp( VHONO ) = conc( LHONO ) * factor * HONOCONV
cblk ( VHONO ) = MAX( CONMIN, cblka(VHONO) + cblkp(VHONO) )

! SOA
cblka( VVALK  ) = concamb( LVALK  ) * factor * VALKCONV
cblkp( VVALK  ) = conc( LVALK  ) * factor * VALKCONV
cblk ( VVALK  ) = MAX( CONMIN, cblka(VVALK) + cblkp(VVALK) )
cblka( VVXYL1 ) = concamb( LVXYL1  ) * factor * VXYLCONV
cblkp( VVXYL1 ) = conc( LVXYL1  ) * factor * VXYLCONV
cblk ( VVXYL1 ) = MAX( CONMIN, cblka(VVXYL1) + cblkp(VVXYL1) )
cblka( VVXYL2 ) = concamb( LVXYL2  ) * factor * VXYLCONV
cblkp( VVXYL2 ) = conc( LVXYL2  ) * factor * VXYLCONV
cblk ( VVXYL2 ) = MAX( CONMIN, cblka(VVXYL2) + cblkp(VVXYL2) )
cblka( VVTOL1 ) = concamb( LVTOL1  ) * factor * VTOLCONV
cblkp( VVTOL1 ) = conc( LVTOL1  ) * factor * VTOLCONV
cblk ( VVTOL1 ) = MAX( CONMIN, cblka(VVTOL1) + cblkp(VVTOL1) )
cblka( VVTOL2 ) = concamb( LVTOL2  ) * factor * VTOLCONV
cblkp( VVTOL2 ) = conc( LVTOL2  ) * factor * VTOLCONV
cblk ( VVTOL2 ) = MAX( CONMIN, cblka(VVTOL2) + cblkp(VVTOL2) )
cblka( VVBNZ1 ) = concamb( LVBNZ1  ) * factor * VBNZCONV
cblkp( VVBNZ1 ) = conc( LVBNZ1  ) * factor * VBNZCONV
cblk ( VVBNZ1 ) = MAX( CONMIN, cblka(VVBNZ1) + cblkp(VVBNZ1) )
cblka( VVBNZ2 ) = concamb( LVBNZ2  ) * factor * VBNZCONV
cblkp( VVBNZ2 ) = conc( LVBNZ2  ) * factor * VBNZCONV
cblk ( VVBNZ2 ) = MAX( CONMIN, cblka(VVBNZ2) + cblkp(VVBNZ2) )
cblka( VVTRP1 ) = concamb( LVTRP1  ) * factor * VTRPCONV
cblkp( VVTRP1 ) = conc( LVTRP1  ) * factor * VTRPCONV
cblk ( VVTRP1 ) = MAX( CONMIN, cblka(VVTRP1) + cblkp(VVTRP1) )
cblka( VVTRP2 ) = concamb( LVTRP2  ) * factor * VTRPCONV
cblkp( VVTRP2 ) = conc( LVTRP2  ) * factor * VTRPCONV
cblk ( VVTRP2 ) = MAX( CONMIN, cblka(VVTRP2) + cblkp(VVTRP2) )
cblka( VVISO1 ) = concamb( LVISO1  ) * factor * VISOCONV
cblkp( VVISO1 ) = conc( LVISO1  ) * factor * VISOCONV
cblk ( VVISO1 ) = MAX( CONMIN, cblka(VVISO1) + cblkp(VVISO1) )
cblka( VVISO2 ) = concamb( LVISO2  ) * factor * VISOCONV
cblkp( VVISO2 ) = conc( LVISO2  ) * factor * VISOCONV
cblk ( VVISO2 ) = MAX( CONMIN, cblka(VVISO2) + cblkp(VVISO2) )
cblka( VVSQT  ) = concamb( LVSQT  ) * factor * VSQTCONV
cblkp( VVSQT  ) = conc( LVSQT  ) * factor * VSQTCONV
cblk ( VVSQT  ) = MAX( CONMIN, cblka(VVSQT) + cblkp(VVSQT) )

! *** Fetch gas-phase production rates.

! *** Initialize secondary organics products
orgprod = 0.0

if ( dt > DTMIN ) then
! *** sulfate
! *** sulfate (ug/m3/s)
  so4rate = H2SO4CONV * factor * conc( LSULFP ) / dt

! *** Secondary organic products
  if ( LALKRXN  > 0 ) orgprod( 1  ) = conc( LALKRXN  )
  if ( LXYLNRXN > 0 ) orgprod( 2  ) = conc( LXYLNRXN )
  if ( LXYLHRXN > 0 ) orgprod( 3  ) = conc( LXYLHRXN )
  if ( LTOLNRXN > 0 ) orgprod( 4  ) = conc( LTOLNRXN )
  if ( LTOLHRXN > 0 ) orgprod( 5  ) = conc( LTOLHRXN )
  if ( LBNZNRXN > 0 ) orgprod( 6  ) = conc( LBNZNRXN )
  if ( LBNZHRXN > 0 ) orgprod( 7  ) = conc( LBNZHRXN )
  if ( LTRPRXN  > 0 ) orgprod( 8  ) = conc( LTRPRXN  )
  if ( LISOPRXN > 0 ) orgprod( 9  ) = conc( LISOPRXN )
  if ( LSESQRXN > 0 ) orgprod( 10 ) = conc( LSESQRXN )

else

  so4rate = 0.

end if

dt = MAX( DTMIN, dt )

! Calculate plume/total ratios before aerosol module. Account for
! molecular weights since concs in cblk are in mass units.

! inorganic species
! Total NOy
! All NOy species expressed as HNO3
plumeconc = cblkp( VNO2 ) * NO2RAT + cblkp( VHONO ) * HONORAT + &
            2.*cblkp( VN2O5 ) * N2O5RAT + cblkp( VHNO3 ) + &
            ( cblkp( VNO3AJ ) + cblkp( VNO3AI ) ) * NO3RAT
totalconc = cblk( VNO2 ) * NO2RAT + cblk( VHONO ) * HONORAT + &
            2.*cblk( VN2O5 ) * N2O5RAT + cblk( VHNO3 ) + &
            ( cblk( VNO3AJ ) + cblk( VNO3AI ) )* NO3RAT
plumeconc = MAX( 0., plumeconc )
totalconc = MAX( CONMIN, totalconc )
rnoy = plumeconc/totalconc
omrnoy = 1. - rnoy

! NOy species ratios
!rno2  = cblkp( VNO2  ) / cblk( VNO2  )
!rhono = cblkp( VHONO ) / cblk( VHONO )
!rn2o5 = cblkp( VN2O5 ) / cblk( VN2O5 )

! Initial total concs of NOy species
!chno30 = cblk( VHNO3 )
!cno3j0 = cblk( VNO3AJ ) !* NO3RAT
!cno3i0 = cblk( VNO3AI ) !* NO3RAT
!rno3 = (cblkp( VHNO3 ) + (cblkp( VNO3AJ ) + cblkp( VNO3AI )) * NO3RAT) / &
!       (chno30 + cno3j0 + cno3i0)

!cno20  = cblk( VNO2  ) !* NO2RAT
!chono0 = cblk( VHONO ) !* HONORAT
!cn2o50 = cblk( VN2O5 ) !* N2O5RAT

rna = (cblkp(VNAJ) + cblkp(VNAI)) / (cblk(VNAJ) + cblk(VNAI))
rna = MAX( 0., rna )
omrna = 1. - rna
!cnaj0 = cblk( VNAJ )
!cnai0 = cblk( VNAI )

rnh4   = (cblkp( VNH3 ) + (cblkp( VNH4AJ ) + cblkp( VNH4AI ))*NH3RAT) / &
         (cblk( VNH3 ) + (cblk( VNH4AJ ) + cblk( VNH4AI ))*NH3RAT)
rnh4 = MAX( 0., rnh4 )
omrnh4 = 1. - rnh4
!cnh30  = cblk( VNH3 )
!cnh4j0 = cblk( VNH4AJ ) !* NH3RAT
!cnh4i0 = cblk( VNH4AI ) !* NH3RAT

rcl   = (cblkp( VHCL ) + (cblkp( VCLJ ) + cblkp( VCLI ))*HCLRAT) / &
        (cblk( VHCL ) + (cblk( VCLJ ) + cblk( VCLI ))*HCLRAT)
rcl = MAX( 0., rcl )
omrcl = 1. - rcl
!chcl0 = cblk( VHCL )
!cclj0 = cblk( VCLJ ) !* HCLRAT
!ccli0 = cblk( VCLI ) !* HCLRAT

! For sulfate, also account for H2SO4 production in plume
plumeconc = cblkp( VSULF ) + so4rate*dt + &
           (cblkp( VSO4AJ ) + cblkp( VSO4AI ))* H2SO4RAT
totalconc = cblk( VSULF ) + so4rate*dt + &
           (cblk( VSO4AJ ) + cblk( VSO4AI ))* H2SO4RAT
plumeconc = MAX( 0., plumeconc )
totalconc = MAX( CONMIN, totalconc )
rso4 = plumeconc/totalconc
omrso4 = 1. - rso4
!ch2so40 = cblk( VSULF )
!cso4j0  = cblk( VSO4AJ ) !* H2SO4RAT
!cso4i0  = cblk( VSO4AI ) !* H2SO4RAT

! organic species
! Account for production terms also
! SOAs from higher alkanes
plumeconc = cblkp( VVALK ) + cblkp( VALKJ ) + &
            orgprod(1) * factor * MWALKRXN * 1.E6 * SCALK
totalconc = cblk( VVALK ) + cblk( VALKJ ) + &
            orgprod(1) * factor * MWALKRXN * 1.E6 * SCALK
plumeconc = MAX( 0., plumeconc )
totalconc = MAX( CONMIN, totalconc )
ralk = plumeconc/totalconc
omralk = 1. - ralk
!cvalk0 = cblk( VVALK )
!calkj0 = cblk( VALKJ )

! SOAs from xylene
plumeconc = cblkp( VVXYL1 ) + cblkp( VVXYL2 ) + &
            cblkp( VXYL1J ) + cblkp( VXYL2J ) + &
            orgprod(2) * factor * MWXYLRXN * 1.E6 * (SCXYL1 + SCXYL2)
totalconc = cblk( VVXYL1 ) + cblk( VVXYL2 ) + &
            cblk( VXYL1J ) + cblk( VXYL2J ) + &
            orgprod(2) * factor * MWXYLRXN * 1.E6 * (SCXYL1 + SCXYL2)
plumeconc = MAX( 0., plumeconc )
totalconc = MAX( CONMIN, totalconc )
rxyl = plumeconc/totalconc
omrxyl = 1. - rxyl
!CVXYL10 = cblk( VVXYL1 )
!CVXYL20 = cblk( VVXYL2 )
!CXYL1J0 = cblk( VXYL1J )
!CXYL2J0 = cblk( VXYL2J )
plumeconc = cblkp( VXYL3J ) + orgprod(3) * factor * MWXYLRXN * 1.E6 * SCXYL3
totalconc = cblk( VXYL3J ) + orgprod(3) * factor * MWXYLRXN * 1.E6 * SCXYL3
plumeconc = MAX( 0., plumeconc )
totalconc = MAX( CONMIN, totalconc )
rxyl3 = plumeconc/totalconc
omrxyl3 = 1. - rxyl3

! SOAs from toluene
plumeconc = cblkp( VVTOL1 ) + cblkp( VVTOL2 ) + &
            cblkp( VTOL1J ) + cblkp( VTOL2J ) + &
            orgprod(4) * factor * MWTOLRXN * 1.E6 * (SCTOL1 + SCTOL2)
totalconc = cblk( VVTOL1 ) + cblk( VVTOL2 ) + &
            cblk( VTOL1J ) + cblk( VTOL2J ) + &
            orgprod(4) * factor * MWTOLRXN * 1.E6 * (SCTOL1 + SCTOL2)
plumeconc = MAX( 0., plumeconc )
totalconc = MAX( CONMIN, totalconc )
rtol = plumeconc/totalconc
omrtol = 1. - rtol
!CVTOL10 = cblk( VVTOL1 )
!CVTOL20 = cblk( VVTOL2 )
!CTOL1J0 = cblk( VTOL1J )
!CTOL2J0 = cblk( VTOL2J )
plumeconc = cblkp( VTOL3J ) + orgprod(5) * factor * MWTOLRXN * 1.E6 * SCTOL3
totalconc = cblk( VTOL3J ) + orgprod(5) * factor * MWTOLRXN * 1.E6 * SCTOL3
plumeconc = MAX( 0., plumeconc )
totalconc = MAX( CONMIN, totalconc )
rtol3 = plumeconc/totalconc
omrtol3 = 1. - rtol3

! SOAs from benzene
plumeconc = cblkp( VVBNZ1 ) + cblkp( VVBNZ2 ) + &
            cblkp( VBNZ1J ) + cblkp( VBNZ2J ) + &
            orgprod(6) * factor * MWBNZRXN * 1.E6 * (SCBNZ1 + SCBNZ2)
totalconc = cblk( VVBNZ1 ) + cblk( VVBNZ2 ) + &
            cblk( VBNZ1J ) + cblk( VBNZ2J ) + &
            orgprod(6) * factor * MWBNZRXN * 1.E6 * (SCBNZ1 + SCBNZ2)
plumeconc = MAX( 0., plumeconc )
totalconc = MAX( CONMIN, totalconc )
rbnz = plumeconc/totalconc
omrbnz = 1. - rbnz
!CVBNZ10 = cblk( VVBNZ1 )
!CVBNZ20 = cblk( VVBNZ2 )
!CBNZ1J0 = cblk( VBNZ1J )
!CBNZ2J0 = cblk( VBNZ2J )
plumeconc = cblkp( VBNZ3J ) + orgprod(7) * factor * MWBNZRXN * 1.E6 * SCBNZ3
totalconc = cblk( VBNZ3J ) + orgprod(7) * factor * MWBNZRXN * 1.E6 * SCBNZ3
plumeconc = MAX( 0., plumeconc )
totalconc = MAX( CONMIN, totalconc )
rbnz3 = plumeconc/totalconc
omrbnz3 = 1. - rbnz3

! SOAs from monoterpenes
plumeconc = cblkp( VVTRP1 ) + cblkp( VVTRP2 ) + &
            cblkp( VTRP1J ) + cblkp( VTRP2J ) + &
            orgprod(8) * factor * MWTRPRXN * 1.E6 * (SCTRP1 + SCTRP2)
totalconc = cblk( VVTRP1 ) + cblk( VVTRP2 ) + &
            cblk( VTRP1J ) + cblk( VTRP2J ) + &
            orgprod(8) * factor * MWTRPRXN * 1.E6 * (SCTRP1 + SCTRP2)
plumeconc = MAX( 0., plumeconc )
totalconc = MAX( CONMIN, totalconc )
rtrp = plumeconc/totalconc
omrtrp = 1. - rtrp
!CVTRP10 = cblk( VVTRP1 )
!CVTRP20 = cblk( VVTRP2 )
!CTRP1J0 = cblk( VTRP1J )
!CTRP2J0 = cblk( VTRP2J )

! SOAs from isoprene
plumeconc = cblkp( VVISO1 ) + cblkp( VVISO2 ) + &
            cblkp( VISO1J ) + cblkp( VISO2J ) + cblkp( VISO3J ) * 1.6/2.7 + &
            orgprod(9) * factor * MWISORXN * 1.E6 * (SCISO1 + SCISO2)
totalconc = cblk( VVISO1 ) + cblk( VVISO2 ) + &
            cblk( VISO1J ) + cblk( VISO2J ) + cblk( VISO3J ) * 1.6/2.7 + &
            orgprod(9) * factor * MWISORXN * 1.E6 * (SCISO1 + SCISO2)
plumeconc = MAX( 0., plumeconc )
totalconc = MAX( CONMIN, totalconc )
riso = plumeconc/totalconc
omriso = 1. - riso
!CVISO10 = cblk( VVISO1 )
!CVISO20 = cblk( VVISO2 )
!CISO1J0 = cblk( VISO1J )
!CISO2J0 = cblk( VISO2J )

! SOAs from sesquiterpene
plumeconc = cblkp( VVSQT ) + cblkp( VSQTJ ) + &
            orgprod(10) * factor * MWSQTRXN * 1.E6 * SCSQT
totalconc = cblk( VVSQT ) + cblk( VSQTJ ) + &
            orgprod(10) * factor * MWSQTRXN * 1.E6 * SCSQT
plumeconc = MAX( 0., plumeconc )
totalconc = MAX( CONMIN, totalconc )
rsqt = plumeconc/totalconc
omrsqt = 1. - rsqt
!CVSQT0 = cblk( VVSQT )
!CSQTJ0 = cblk( VSQTJ )

! SOAs from oligomerization of anthropogenic SOAs
plumeconc = (cblkp(VALKJ)*NCALK/MWVALK + &
            (cblkp(VXYL1J) + cblkp(VXYL2J))*NCXYL/MWVXYL + &
            (cblkp(VTOL1J) + cblkp(VTOL2J))*NCTOL/MWVTOL + &
            (cblkp(VBNZ1J) + cblkp(VBNZ2J))*NCBNZ/MWVBNZ)*12.0*OLGRAT + &
             cblkp(VOLGAJ)
totalconc = (cblk(VALKJ)*NCALK/MWVALK + &
            (cblk(VXYL1J) + cblk(VXYL2J))*NCXYL/MWVXYL + &
            (cblk(VTOL1J) + cblk(VTOL2J))*NCTOL/MWVTOL + &
            (cblk(VBNZ1J) + cblk(VBNZ2J))*NCBNZ/MWVBNZ)*12.0*OLGRAT + &
             cblk(VOLGAJ)
plumeconc = MAX( 0., plumeconc )
totalconc = MAX( CONMIN, totalconc )
rolga = plumeconc/totalconc
omrolga = 1. - rolga

! SOAs from oligomerization of biogenic SOAs
plumeconc = ((cblkp(VTRP1J) + cblkp(VTRP2J))*NCTRP/MWVTRP + &
             (cblkp(VISO1J) + cblkp(VISO2J))*NCISO/MWVISO + &
              cblkp(VSQTJ)*NCSQT/MWVSQT)*12.0*OLGRAT + &
            cblkp(VOLGBJ)
totalconc = ((cblk(VTRP1J) + cblk(VTRP2J))*NCTRP/MWVTRP + &
             (cblk(VISO1J) + cblk(VISO2J))*NCISO/MWVISO + &
              cblk(VSQTJ)*NCSQT/MWVSQT)*12.0*OLGRAT + &
            cblk(VOLGBJ)
plumeconc = MAX( 0., plumeconc )
totalconc = MAX( CONMIN, totalconc )
rolgb = plumeconc/totalconc
omrolgb = 1. - rolgb

! number concentrations and surface area
! better to do this in post-processor if number concs required
!rnumat = cblkp(VAT0)/cblk(VAT0)
!rnumac = cblkp(VAC0)/cblk(VAC0)
!rnumco = cblkp(VCO0)/cblk(VCO0)
!rsurfat = cblkp(VSURFAT)/cblk(VSURFAT)
!rsurfac = cblkp(VSURFAC)/cblk(VSURFAC)
!rsurfco = cblkp(VSURFCO)/cblk(VSURFCO)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!     call aerosol process routines

! Assign dummy variables (not used) for consistency with CMAQ aeroproc
col = 1
row = 1
layer = 1
airdens = ( patm * MWAIR ) / ( tk * RGAS )
xlm = 6.6328E-8 * tk  / ( T0 * patm )
amu = 1.458e-6 * tk * SQRT( tk ) / ( tk + 110.4 )

call aeroproc( NSPCSDA, cblk, dt, col, row, layer, &
               tk, pres_pa, airdens, rh, &
               so4rate, orgprod, NPSPCS, &
               xlm, amu, &
               dgatk, dgacc, dgcor, &
               xxlsgat, xxlsgac, xxlsgco, &
               pmassat, pmassac, pmassco, &
               pdensat, pdensac, pdensco, GAMMA_N2O5, lun_log )

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! *** Calculate 2nd and 3rd moments of the "dry" aerosol distribution
!     NOTE! "dry" aerosol excludes both H2O and SOA  (January 2004 --SJR)
!     EXCEPT!  nonvolatile SOA is part of dry aerosol (Oct 2007 --PVB)

! Aitken mode.
m3_wet = MAX( CONMIN, cblk( VAT3 ) )
m3subt = H2OFAC * cblk( VH2OAI )
m3_dry = MAX( CONMIN, m3_wet - m3subt )

m2_wet = cblk( VAT2 )
m2_dry = m2_wet * ( m3_dry / m3_wet ) ** TWO3

cblk( VAT3 ) = MAX( CONMIN, m3_dry )
cblk( VAT2 ) = MAX( CONMIN, m2_dry )

! accumulation mode.
m3_wet = MAX( CONMIN, cblk( VAC3 ) )
m3subt = H2OFAC * cblk( VH2OAJ ) &
       + ORGFAC * cblk( VALKJ  ) &
       + ORGFAC * cblk( VXYL1J ) &
       + ORGFAC * cblk( VXYL2J ) &
       + ORGFAC * cblk( VTOL1J ) &
       + ORGFAC * cblk( VTOL2J ) &
       + ORGFAC * cblk( VBNZ1J ) &
       + ORGFAC * cblk( VBNZ2J ) &
       + ORGFAC * cblk( VTRP1J ) &
       + ORGFAC * cblk( VTRP2J ) &
       + ORGFAC * cblk( VISO1J ) &
       + ORGFAC * cblk( VISO2J ) &
       + ORGFAC * cblk( VSQTJ  )
m3_dry = MAX( CONMIN, m3_wet - m3subt )

m2_wet = cblk( VAC2 )
m2_dry = m2_wet * ( m3_dry / m3_wet ) ** TWO3

cblk( VAC3 ) = MAX( CONMIN, m3_dry )
cblk( VAC2 ) = MAX( CONMIN, m2_dry )

!     coarse mode
m3_wet = MAX( CONMIN, cblk( VCO3 ) )
m3subt = h2ofac * cblk( VH2OK )
m3_dry = MAX( CONMIN, m3_wet - m3subt )
m2_wet = cblk( VCO2 )

m2_dry = m2_wet * ( m3_dry / m3_wet ) ** TWO3

cblk( VCO3 ) = MAX( CONMIN, m3_dry )
cblk( VCO2 ) = MAX( CONMIN, m2_dry )

! *** Calculate geometric mean diameters and standard deviations of the
!     "dry" size distribution

call getpar( NSPCSDA, cblk, pmassat, pmassac, pmassco, &
             pdensat, pdensac, pdensco, &
             dgatk, dgacc, dgcor, &
             xxlsgat, xxlsgac, xxlsgco, &
             M3_WET_FLAG, LIMIT_Sg )

! *** Calculate aerosol surface area from the dry 2nd moment.  Dry value is
!     used in transport routines.
cblk( VSURFAT ) = PI * cblk( VAT2 )
cblk( VSURFAC ) = PI * cblk( VAC2 )
cblk( VSURFCO ) = PI * cblk( VCO2 )

! *** Transfer new gas and aerosol information from CBLK to CONC
! Inorganic Species
!dno2 = cblk( VNO2 ) - cno20
!cblkp( VNO2 ) = cblkp( VNO2 ) + rnoy * dno2
!cblka( VNO2 ) = cblka( VNO2 ) + omrnoy * dno2
cblkp( VNO2 ) = rnoy * cblk( VNO2 )
cblka( VNO2 ) = omrnoy * cblk( VNO2 )

!dn2o5 = cblk( VN2O5 ) - cn2o50
!cblkp( VN2O5 ) = cblkp( VN2O5 ) + rnoy * dn2o5
!cblka( VN2O5 ) = cblka( VN2O5 ) + omrnoy * dn2o5
cblkp( VN2O5 ) = rnoy * cblk( VN2O5 )
cblka( VN2O5 ) = omrnoy * cblk( VN2O5 )

!dhono = cblk( VHONO ) - chono0
!cblkp( VHONO ) = cblkp( VHONO ) + rnoy * dhono
!cblka( VHONO ) = cblka( VHONO ) + omrnoy * dhono
cblkp( VHONO ) = rnoy * cblk( VHONO )
cblka( VHONO ) = omrnoy * cblk( VHONO )

!dhno3 = cblk( VHNO3 ) - chno30
!cblkp( VHNO3 ) = cblkp( VHNO3 ) + rnoy * dhno3
!cblka( VHNO3 ) = cblka( VHNO3 ) + omrnoy * dhno3
cblkp( VHNO3 ) = rnoy * cblk( VHNO3 )
cblka( VHNO3 ) = omrnoy * cblk( VHNO3 )

!dno3j = cblk( VNO3AJ ) - cno3j0
!dno3i = cblk( VNO3AI ) - cno3i0
!cblkp( VNO3AJ ) = cblkp( VNO3AJ ) + rnoy * dno3j
!cblka( VNO3AJ ) = cblka( VNO3AJ ) + omrnoy * dno3j
!cblkp( VNO3AI ) = cblkp( VNO3AI ) + rnoy * dno3i
!cblka( VNO3AI ) = cblka( VNO3AI ) + omrnoy * dno3i
cblkp( VNO3AJ ) = rnoy * cblk( VNO3AJ )
cblka( VNO3AJ ) = omrnoy * cblk( VNO3AJ )
cblkp( VNO3AI ) = rnoy * cblk( VNO3AI )
cblka( VNO3AI ) = omrnoy * cblk( VNO3AI )

!dnh3 = cblk( VNH3 ) - cnh30
!cblkp( VNH3 ) = cblkp( VNH3 ) + rnh4 * dnh3
!cblka( VNH3 ) = cblka( VNH3 ) + omrnh4 * dnh3
cblkp( VNH3 ) = rnh4 * cblk( VNH3 )
cblka( VNH3 ) = omrnh4 * cblk( VNH3 )

!dnh4j = cblk( VNH4AJ ) - cnh4j0
!dnh4i = cblk( VNH4AI ) - cnh4i0
!cblkp( VNH4AJ ) = cblkp( VNH4AJ ) + rnh4 * dnh4j
!cblka( VNH4AJ ) = cblka( VNH4AJ ) + omrnh4 * dnh4j
!cblkp( VNH4AI ) = cblkp( VNH4AI ) + rnh4 * dnh4i
!cblka( VNH4AI ) = cblka( VNH4AI ) + omrnh4 * dnh4i
cblkp( VNH4AJ ) = rnh4 * cblk( VNH4AJ )
cblka( VNH4AJ ) = omrnh4 * cblk( VNH4AJ )
cblkp( VNH4AI ) = rnh4 * cblk( VNH4AI )
cblka( VNH4AI ) = omrnh4 * cblk( VNH4AI )

!dh2so4 = cblk( VSULF ) - ch2so40
!cblkp( VSULF ) = cblkp( VSULF ) + rso4 * dh2so4
!cblka( VSULF ) = cblka( VSULF ) + omrso4 * dh2so4
cblkp( VSULF ) = rso4 * cblk( VSULF )
cblka( VSULF ) = omrso4 * cblk( VSULF )

!dso4j = cblk( VSO4AJ ) - cso4j0
!dso4i = cblk( VSO4AI ) - cso4i0
!cblkp( VSO4AJ ) = cblkp( VSO4AJ ) + rso4 * dso4j
!cblka( VSO4AJ ) = cblka( VSO4AJ ) + omrso4 * dso4j
!cblkp( VSO4AI ) = cblkp( VSO4AI ) + rso4 * dso4i
!cblka( VSO4AI ) = cblka( VSO4AI ) + omrso4 * dso4i
cblkp( VSO4AJ ) = rso4 * cblk( VSO4AJ )
cblka( VSO4AJ ) = omrso4 * cblk( VSO4AJ )
cblkp( VSO4AI ) = rso4 * cblk( VSO4AI )
cblka( VSO4AI ) = omrso4 * cblk( VSO4AI )

!dnaj = cblk( VNAJ ) - cnaj0
!dnai = cblk( VNAI ) - cnai0
!cblkp( VNAJ ) = cblkp( VNAJ ) + rna * dnaj
!cblka( VNAJ ) = cblka( VNAJ ) + omrna * dnaj
!cblkp( VNAI ) = cblkp( VNAI ) + rna * dnai
!cblka( VNAI ) = cblka( VNAI ) + omrna * dnai
cblkp( VNAJ ) = rna * cblk( VNAJ )
cblka( VNAJ ) = omrna * cblk( VNAJ )
cblkp( VNAI ) = rna * cblk( VNAI )
cblka( VNAI ) = omrna * cblk( VNAI )

!dhcl = cblk( VHCL ) - chcl0
!cblkp( VHCL ) = cblkp( VHCL ) + rcl * dhcl
!cblka( VHCL ) = cblka( VHCL ) + omrcl * dhcl
cblkp( VHCL ) = rcl * cblk( VHCL )
cblka( VHCL ) = omrcl * cblk( VHCL )

!dclj = cblk( VCLJ ) - cclj0
!dcli = cblk( VCLI ) - ccli0
!cblkp( VCLJ ) = cblkp( VCLJ ) + rcl * dclj
!cblka( VCLJ ) = cblka( VCLJ ) + omrcl * dclj
!cblkp( VCLI ) = cblkp( VCLI ) + rcl * dcli
!cblka( VCLI ) = cblka( VCLI ) + omrcl * dcli
cblkp( VCLJ ) = rcl * cblk( VCLJ )
cblka( VCLJ ) = omrcl * cblk( VCLJ )
cblkp( VCLI ) = rcl * cblk( VCLI )
cblka( VCLI ) = omrcl * cblk( VCLI )

! Organic Species
cblkp( VVALK ) = ralk * cblk( VVALK )
cblka( VVALK ) = omralk * cblk( VVALK )
cblkp( VALKJ ) = ralk * cblk( VALKJ )
cblka( VALKJ ) = omralk * cblk( VALKJ )

cblkp( VVXYL1 ) = rxyl * cblk( VVXYL1 )
cblka( VVXYL1 ) = omrxyl * cblk( VVXYL1 )
cblkp( VXYL1J ) = rxyl * cblk( VXYL1J )
cblka( VXYL1J ) = omrxyl * cblk( VXYL1J )

cblkp( VVXYL2 ) = rxyl * cblk( VVXYL2 )
cblka( VVXYL2 ) = omrxyl * cblk( VVXYL2 )
cblkp( VXYL2J ) = rxyl * cblk( VXYL2J )
cblka( VXYL2J ) = omrxyl * cblk( VXYL2J )

cblkp( VXYL3J ) = rxyl3 * cblk( VXYL3J )
cblka( VXYL3J ) = omrxyl3 * cblk( VXYL3J )

cblkp( VVTOL1 ) = rtol * cblk( VVTOL1 )
cblka( VVTOL1 ) = omrtol * cblk( VVTOL1 )
cblkp( VTOL1J ) = rtol * cblk( VTOL1J )
cblka( VTOL1J ) = omrtol * cblk( VTOL1J )

cblkp( VVTOL2 ) = rtol * cblk( VVTOL2 )
cblka( VVTOL2 ) = omrtol * cblk( VVTOL2 )
cblkp( VTOL2J ) = rtol * cblk( VTOL2J )
cblka( VTOL2J ) = omrtol * cblk( VTOL2J )

cblkp( VTOL3J ) = rtol3 * cblk( VTOL3J )
cblka( VTOL3J ) = omrtol3 * cblk( VTOL3J )

cblkp( VVBNZ1 ) = rbnz * cblk( VVBNZ1 )
cblka( VVBNZ1 ) = omrbnz * cblk( VVBNZ1 )
cblkp( VBNZ1J ) = rbnz * cblk( VBNZ1J )
cblka( VBNZ1J ) = omrbnz * cblk( VBNZ1J )

cblkp( VVBNZ2 ) = rbnz * cblk( VVBNZ2 )
cblka( VVBNZ2 ) = omrbnz * cblk( VVBNZ2 )
cblkp( VBNZ2J ) = rbnz * cblk( VBNZ2J )
cblka( VBNZ2J ) = omrbnz * cblk( VBNZ2J )

cblkp( VBNZ3J ) = rbnz3 * cblk( VBNZ3J )
cblka( VBNZ3J ) = omrbnz3 * cblk( VBNZ3J )

cblkp( VVISO1 ) = riso * cblk( VVISO1 )
cblka( VVISO1 ) = omriso * cblk( VVISO1 )
cblkp( VISO1J ) = riso * cblk( VISO1J )
cblka( VISO1J ) = omriso * cblk( VISO1J )

cblkp( VVISO2 ) = riso * cblk( VVISO2 )
cblka( VVISO2 ) = omriso * cblk( VVISO2 )
cblkp( VISO2J ) = riso * cblk( VISO2J )
cblka( VISO2J ) = omriso * cblk( VISO2J )

cblkp( VISO3J ) = riso * cblk( VISO3J )
cblka( VISO3J ) = omriso * cblk( VISO3J )

cblkp( VVSQT ) = rsqt * cblk( VVSQT )
cblka( VVSQT ) = omrsqt * cblk( VVSQT )
cblkp( VSQTJ ) = rsqt * cblk( VSQTJ )
cblka( VSQTJ ) = omrsqt * cblk( VSQTJ )

cblkp( VOLGAJ ) = rolga * cblk( VOLGAJ )
cblka( VOLGAJ ) = omrolga * cblk( VOLGAJ )

cblkp( VOLGBJ ) = rolgb * cblk( VOLGBJ )
cblka( VOLGBJ ) = omrolgb * cblk( VOLGBJ )

do spc = 1, naerp
   concamb(cblk_map(spc)) = MAX( cblka(spc), AECONMIN(spc) )
   conc(cblk_map(spc)) = MAX( cblkp(spc), -concamb(spc) )
end do

! *** Transfer new gas/vapor concentrations from CBLK to CONC
factor1 = 1. / factor

concamb( LSULF ) = MAX( CONMIN, cblka( VSULF ) * factor1 * H2SO4CONV1 )
conc( LSULF ) = MAX( cblkp( VSULF ) * factor1 * H2SO4CONV1, -concamb(LSULF) )

concamb( LHNO3 ) = MAX( CONMIN, cblka( VHNO3 ) * factor1 * HNO3CONV1  )
conc( LHNO3 ) = MAX( cblkp( VHNO3 ) * factor1 * HNO3CONV1, -concamb(LHNO3) )

concamb( LNH3 ) = MAX( CONMIN, cblka( VNH3 ) * factor1 * NH3CONV1  )
conc( LNH3 ) = MAX( cblkp( VNH3 ) * factor1 * NH3CONV1, -concamb(LNH3) )

concamb( LHCL ) = MAX( CONMIN, cblka( VHCL ) * factor1 * HCLCONV1  )
conc( LHCL ) = MAX( cblkp( VHCL ) * factor1 * HCLCONV1, -concamb(LHCL) )

concamb( LN2O5 ) = MAX( CONMIN, cblka( VN2O5 ) * factor1 * N2O5CONV1  )
conc( LN2O5 ) = MAX( cblkp( VN2O5 ) * factor1 * N2O5CONV1, -concamb(LN2O5) )

concamb( LNO2 ) = MAX( CONMIN, cblka( VNO2 ) * factor1 * NO2CONV1  )
conc( LNO2 ) = MAX( cblkp( VNO2 ) * factor1 * NO2CONV1, -concamb(LNO2) )

concamb( LHONO ) = MAX( CONMIN, cblka( VHONO ) * factor1 * HONOCONV1  )
conc( LHONO ) = MAX( cblkp( VHONO ) * factor1 * HONOCONV1, -concamb(LHONO) )

concamb( LVALK ) = MAX( CONMIN, cblka( VVALK  ) * factor1 * VALKCONV1 )
conc( LVALK ) = MAX( cblkp( VVALK ) * factor1 * VALKCONV1, -concamb(LVALK) )
concamb( LVXYL1 ) = MAX( CONMIN, cblka( VVXYL1 ) * factor1 * VXYLCONV1 )
conc( LVXYL1 ) = MAX( cblkp( VVXYL1 ) * factor1 * VXYLCONV1, -concamb(LVXYL1) )
concamb( LVXYL2 ) = MAX( CONMIN, cblka( VVXYL2 ) * factor1 * VXYLCONV1 )
conc( LVXYL2 ) = MAX( cblkp( VVXYL2 ) * factor1 * VXYLCONV1, -concamb(LVXYL2) )
concamb( LVTOL1 ) = MAX( CONMIN, cblka( VVTOL1 ) * factor1 * VTOLCONV1 )
conc( LVTOL1 ) = MAX( cblkp( VVTOL1 ) * factor1 * VTOLCONV1, -concamb(LVTOL1) )
concamb( LVTOL2 ) = MAX( CONMIN, cblka( VVTOL2 ) * factor1 * VTOLCONV1 )
conc( LVTOL2 ) = MAX( cblkp( VVTOL2 ) * factor1 * VTOLCONV1, -concamb(LVTOL2) )
concamb( LVBNZ1 ) = MAX( CONMIN, cblka( VVBNZ1 ) * factor1 * VBNZCONV1 )
conc( LVBNZ1 ) = MAX( cblkp( VVBNZ1 ) * factor1 * VBNZCONV1, -concamb(LVBNZ1) )
concamb( LVBNZ2 ) = MAX( CONMIN, cblka( VVBNZ2 ) * factor1 * VBNZCONV1 )
conc( LVBNZ2 ) = MAX( cblkp( VVBNZ2 ) * factor1 * VBNZCONV1, -concamb(LVBNZ2) )
concamb( LVTRP1 ) = MAX( CONMIN, cblka( VVTRP1 ) * factor1 * VTRPCONV1 )
conc( LVTRP1 ) = MAX( cblkp( VVTRP1 ) * factor1 * VTRPCONV1, -concamb(LVTRP1) )
concamb( LVTRP2 ) = MAX( CONMIN, cblka( VVTRP2 ) * factor1 * VTRPCONV1 )
conc( LVTRP2 ) = MAX( cblkp( VVTRP2 ) * factor1 * VTRPCONV1, -concamb(LVTRP2) )
concamb( LVISO1 ) = MAX( CONMIN, cblka( VVISO1 ) * factor1 * VISOCONV1 )
conc( LVISO1 ) = MAX( cblkp( VVISO1 ) * factor1 * VISOCONV1, -concamb(LVISO1) )
concamb( LVISO2 ) = MAX( CONMIN, cblka( VVISO2 ) * factor1 * VISOCONV1 )
conc( LVISO2 ) = MAX( cblkp( VVISO2 ) * factor1 * VISOCONV1, -concamb(LVISO2) )
concamb( LVSQT ) = MAX( CONMIN, cblka( VVSQT  ) * factor1 * VSQTCONV1 )
conc( LVSQT ) = MAX( cblkp( VVSQT ) * factor1 * VSQTCONV1, -concamb(LVSQT) )

! *** Zero out species representing the contributions to production 
!     of aerosols.
concamb( LSULFP )  = 0.0
conc( LSULFP )  = 0.0

if( LALKRXN  > 0 ) conc( LALKRXN  ) = 0.
if( LXYLNRXN > 0 ) conc( LXYLNRXN ) = 0.
if( LXYLHRXN > 0 ) conc( LXYLHRXN ) = 0.
if( LTOLNRXN > 0 ) conc( LTOLNRXN ) = 0.
if( LTOLHRXN > 0 ) conc( LTOLHRXN ) = 0.
if( LBNZNRXN > 0 ) conc( LBNZNRXN ) = 0.
if( LBNZHRXN > 0 ) conc( LTRPRXN  ) = 0.
if( LISOPRXN > 0 ) conc( LISOPRXN ) = 0.
if( LSESQRXN > 0 ) conc( LSESQRXN ) = 0.

return
end
