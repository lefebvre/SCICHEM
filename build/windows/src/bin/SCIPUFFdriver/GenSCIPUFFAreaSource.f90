!*******************************************************************************
!$RCSfile$
!$Revision$
!$Date$
!*******************************************************************************
INTEGER FUNCTION GenSCIPUFFAreaSource()

!------ Generate SCIPUFF area sources
!       Add multicomponent releases for all sources

USE SCIPUFFdriver_fi
USE constants_fd

IMPLICIT NONE

INTEGER nrel, irel, nx, ny, ns
INTEGER i, j, jrel, is, alloc_stat
REAL    xlen, ylen, ang, sigz
REAL    dx, dy, area, xmap, ymap
REAL    rho, vel, c, s

REAL, DIMENSION(:),   ALLOCATABLE :: x, y
REAL, DIMENSION(:,:), ALLOCATABLE :: xs, ys, xr, yr

INTEGER,       EXTERNAL :: SetMCrelease
INTEGER,       EXTERNAL :: ReallocMCRelList
INTEGER,       EXTERNAL :: ReallocRelList
REAL,          EXTERNAL :: GetMatlDensity
REAL,          EXTERNAL :: sind, cosd
CHARACTER(16), EXTERNAL :: int2str

GenSCIPUFFAreaSource = FAILURE

!------ Initialize for zero area sources

nrel  = new%scnHead%number
nrel0 = nrel

ALLOCATE( iAreaSrc(nrel,2),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  WRITE(*,'(A)') 'Error allocating area source index array'
  GOTO 9999
END IF

iAreaSrc = 0

!------ Check for area sources

DO irel = 1,nrel0

  SELECT CASE( relList(irel)%type )
    CASE( -HR_STACK )

      relStackData = TRANSFER(relList(irel)%relData,relStackData)

!------ Extract saved parameters

      xlen = relStackData%exitTemp  !X-length
      ylen = relStackData%exitVel   !Y-length
      ang  = relStackData%diameter  !angle
      sigz = relStackData%sigma     !Sigma-z (not used)

!------ Set number of sources

      CALL NumAreaSources( xlen,ylen,nx,ny ); ns = nx*ny

      area = xlen*ylen/FLOAT(ns)

!------ Compute velocity to insure implied density is less than material density
!       but set to at least 1 cm/s. N.B. Assumes material density is in kg/m^3

      rho = GetMatlDensity( relList(irel)%material )
      IF( rho > 0. )THEN
        vel = MAX(relStackData%rate/rho,0.01)
      ELSE
        vel = 0.01
      END IF

!------ Set physical stack parameters

      relStackData%rate     = relStackData%rate * area !Emissions is mass/s/area for area sources (only)
      relStackData%exitTemp = DEF_VAL_R                !Neutrally buoyant
      relStackData%exitVel  = vel                      !Small velocity
      relStackData%diameter = SQRT(4.*area/PI)         !Equivalent diameter

!------ multicomponent rates are per area

      IF( nMC > 0 )THEN
        DO i = 1,nMC
          j = (irel-1)*nMC + i
          relMCList(j)%MCmass = relMCList(j)%MCmass * area
        END DO
      END IF

!------ Set index for multiple area sources (also used for updating emissions via external file)
!       Copy release parameters into subdivided releases

      iAreaSrc(irel,1) = ns
      IF( ns > 1 )THEN
        iAreaSrc(irel,2) = nrel !Offset for multiple area sources
        nrel = nrel + ns -1
        IF( nrel > new%scnHead%max )THEN
          i = ReallocRelList( nrel-new%scnHead%max )  !Increment rel list as needed
          IF( i /= SUCCESS )GOTO 9999
        END IF
        DO i = 1,ns-1
          jrel = iAreaSrc(irel,2) + i
          relList(jrel) = relList(irel)
          relList(jrel)%relName = TRIM(relList(jrel)%relName)//':'//TRIM(int2str(i+1))
        END DO
        IF( nMC > 0 )THEN
          i = ReallocMCRelList( ns-1 )  !Increment MC rel list as needed
          IF( i /= SUCCESS )GOTO 9999
          is = (irel-1)*nMC
          DO i = 1,ns-1
            jrel = iAreaSrc(irel,2) + i
            DO j = 1,nMC
              nMCrel = nMCrel + 1
              relMCList(nMCrel)%relID = jrel
              relMCList(nMCrel)%MCname = relMCList(is+j)%MCname
              relMCList(nMCrel)%MCmass = relMCList(is+j)%MCmass
            END DO
          END DO
        END IF
      END IF

!------ Set source locations

      ALLOCATE( x(nx),y(ny),xs(nx,ny),ys(nx,ny),STAT=alloc_stat )
      IF( alloc_stat /= 0 )THEN
        WRITE(*,'(A)') 'Error allocating area source location arrays'
        GOTO 9999
      END IF

      dx = xlen/FLOAT(nx); dy = ylen/FLOAT(ny)

      x = (/ ((FLOAT(i)-0.5)*dx,i=1,nx) /)
      y = (/ ((FLOAT(i)-0.5)*dy,i=1,ny) /)

      DO j = 1,ny
       xs(:,j) = x
      END DO

      DO i = 1,nx
        ys(i,:) = y
      END DO

!------ Rotate

      IF( ang /= 0. )THEN

        ALLOCATE( xr(nx,ny),yr(nx,ny),STAT=alloc_stat )
        IF( alloc_stat /= 0 )THEN
          WRITE(*,'(A)') 'Error allocating area source location arrays for rotation'
          GOTO 9999
        END IF
        xr = xs; yr = ys

        c = cosd(ang); s = sind(ang)

        xs =  c*xr + s*yr             !ang is positive clockwise
        ys = -s*xr + c*yr

      END IF

!------ Set factors to convert from meters to project coordinates

      IF( new%input%domain%domain%coord == I_LATLON )THEN

        xmap = SPHFACR/cosd(SNGL(relList(irel)%yRel))
        ymap = SPHFACR

      ELSE

        xmap = xfac*1.E-3; ymap = xmap  !Cartesian or UTM always kilometers

      END IF

!------ Add multiple source locations to "corner"; results in project coordinates

      xs = xs * xmap
      ys = ys * ymap

      is = 0
      DO j = 1,ny
        DO i = 1,nx
          is = is + 1
          IF( is == 1 )THEN
            jrel = irel
          ELSE
            jrel = iAreaSrc(irel,2) + is - 1
          END IF
          relList(jrel)%relData = TRANSFER(relStackData,relList(jrel)%relData)
          relList(jrel)%xRel = relList(jrel)%xRel + xs(i,j)
          relList(jrel)%yRel = relList(jrel)%yRel + ys(i,j)
          relList(jrel)%type = HR_STACK   !Set to SCIPUFF stack type
        END DO

      END DO

  END SELECT

  IF( ALLOCATED(x)  )DEALLOCATE(x,STAT=alloc_stat)
  IF( ALLOCATED(y)  )DEALLOCATE(y,STAT=alloc_stat)
  IF( ALLOCATED(xs) )DEALLOCATE(xs,STAT=alloc_stat)
  IF( ALLOCATED(ys) )DEALLOCATE(ys,STAT=alloc_stat)
  IF( ALLOCATED(xr) )DEALLOCATE(xr,STAT=alloc_stat)
  IF( ALLOCATED(yr) )DEALLOCATE(yr,STAT=alloc_stat)

END DO

new%scnHead%number = nrel

GenSCIPUFFAreaSource = SUCCESS

9999 CONTINUE

IF( ALLOCATED(x)  )DEALLOCATE(x,STAT=alloc_stat)
IF( ALLOCATED(y)  )DEALLOCATE(y,STAT=alloc_stat)
IF( ALLOCATED(xs) )DEALLOCATE(xs,STAT=alloc_stat)
IF( ALLOCATED(ys) )DEALLOCATE(ys,STAT=alloc_stat)
IF( ALLOCATED(xr) )DEALLOCATE(xr,STAT=alloc_stat)
IF( ALLOCATED(yr) )DEALLOCATE(yr,STAT=alloc_stat)

RETURN
END

!==============================================================================

SUBROUTINE NumAreaSources( xlen,ylen,nx,ny )

USE SCIPUFFdriver_fi

IMPLICIT NONE

REAL,    INTENT( IN  ) :: xlen, ylen !Length and width of source rectangle
INTEGER, INTENT( OUT ) :: nx,   ny   !Number of sources dividing rectangle in x,y

REAL, PARAMETER :: FAC  = 0.25       !Tolerance for dividing length, e.g.,
REAL, PARAMETER :: FACM = 1 - FAC    !if xlen/ylen > n+fac, divide into n+1 sources

REAL r

IF( ylen < xlen )THEN
  IF( ylen <= 0 )THEN
    r = 1.
  ELSE
    r  = xlen/ylen
  END IF
  ny = nAreaSrc
  nx = INT(r+FACM) * ny
ELSE
  IF( xlen <= 0 )THEN
    r = 1.
  ELSE
    r  = ylen/xlen
  END IF
  nx = nAreaSrc
  ny = INT(r+FACM) * nx
END IF

RETURN
END

!==============================================================================

CHARACTER(*) FUNCTION int2str( num ) RESULT( str )

IMPLICIT NONE

INTEGER, INTENT( IN ) :: num

INTEGER ios

IF( num > 99999999 )THEN
  str = '********'
ELSE
  WRITE(str,FMT='(I8)',IOSTAT=ios) num
  IF( ios /= 0 )str = '********'
END IF

str = TRIM(ADJUSTL(str))

RETURN
END

!==============================================================================

INTEGER FUNCTION SetMCrelease()

!------ Setup multicomponent releases

USE SCIPUFFdriver_fi

IMPLICIT NONE

INTEGER alloc_stat, i, j, k

TYPE( MCrelData ), POINTER :: MCrel

SetMCrelease = FAILURE

nMCrel = nMC * new%scnHead%number

IF( nMCrel > 0 )THEN

  ALLOCATE( relMCList(nMCrel),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    WRITE(*,*) 'Error allocating multicomponent list array'
    WRITE(*,*) 'No. of releases: ',new%scnHead%number
    GOTO 9999
  END IF

  k = 0
  DO i = 1,new%scnHead%number
    DO j = 1,nMC
      k = k + 1
      relMCList(k)%relID = i
      relMCList(k)%MCname = TRIM(MCname(j))
      relMCList(k)%MCmass = MCrate(j,i)
    END DO
  END DO

END IF

SetMCrelease = SUCCESS

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION ReallocMCRelList( inc )

USE SCIPUFFdriver_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: inc  !Number of new releases

INTEGER alloc_stat, i

TYPE( releaseMCT ), DIMENSION(:), ALLOCATABLE :: tmpList

ReallocMCRelList = FAILURE

!------ Allocate temporary list

ALLOCATE( tmpList(nMCrel),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  WRITE(*,'(A)') 'Error allocating temporary MC release array'
  GOTO 9999
END IF

!------ Fill it with current releases

DO i = 1,nMCrel
  tmpList(i) = relMCList(i)
END DO

!------ Deallocate old list; reallocate with larger size

DEALLOCATE( relMCList,STAT=alloc_stat )

ALLOCATE( relMCList(nMCrel+inc*nMC),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  WRITE(*,'(A)') 'Error re-allocating new MC release array'
  GOTO 9999
END IF

!------ Copy current releases back; increment new%scnHead%max

DO i = 1,nMCrel
  relMCList(i)   = tmpList(i)
END DO

!------ Deallocate temporary list

DEALLOCATE( tmpList,STAT=alloc_stat )

ReallocMCRelList = SUCCESS

9999 CONTINUE

RETURN
END

