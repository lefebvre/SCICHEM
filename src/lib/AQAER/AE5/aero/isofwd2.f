
C=======================================================================
C
C *** ISORROPIA CODE
C *** SUBROUTINE CALCH1A
C *** CASE H1 ; SUBCASE 1
C
C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
C     1. SULFATE POOR (SULRAT > 2.0) ; SODIUM RICH (SODRAT >= 2.0)
C     2. SOLID AEROSOL ONLY
C     3. SOLIDS POSSIBLE : NH4NO3, NH4CL, NANO3, NA2SO4
C
C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
C *** GEORGIA INSTITUTE OF TECHNOLOGY
C *** WRITTEN BY ATHANASIOS NENES
C *** UPDATED BY CHRISTOS FOUNTOUKIS
C
C=======================================================================
C
      SUBROUTINE CALCH1A
      INCLUDE 'isrpia.inc'
      DOUBLE PRECISION LAMDA, LAMDA1, LAMDA2, KAPA, KAPA1, KAPA2, NAFR,
     &                 NO3FR
C
C *** CALCULATE NON VOLATILE SOLIDS ***********************************
C
      CNA2SO4 = W(2)
      CNH42S4 = ZERO
      NAFR    = MAX (W(1)-2*CNA2SO4, ZERO)
      CNANO3  = MIN (NAFR, W(4))
      NO3FR   = MAX (W(4)-CNANO3, ZERO)
      CNACL   = MIN (MAX(NAFR-CNANO3, ZERO), W(5))
      CLFR    = MAX (W(5)-CNACL, ZERO)
C
C *** CALCULATE VOLATILE SPECIES **************************************
C
      ALF     = W(3)                     ! FREE NH3
      BET     = CLFR                     ! FREE CL
      GAM     = NO3FR                    ! FREE NO3
C
      RTSQ    = R*TEMP*R*TEMP
      A1      = XK6/RTSQ
      A2      = XK10/RTSQ
C
      THETA1  = GAM - BET*(A2/A1)
      THETA2  = A2/A1
C
C QUADRATIC EQUATION SOLUTION
C
      BB      = (THETA1-ALF-BET*(ONE+THETA2))/(ONE+THETA2)
      CC      = (ALF*BET-A1-BET*THETA1)/(ONE+THETA2)
      DD      = BB*BB - 4.0D0*CC
      IF (DD.LT.ZERO) GOTO 100   ! Solve each reaction seperately
C
C TWO ROOTS FOR KAPA, CHECK AND SEE IF ANY VALID
C
      SQDD    = SQRT(DD)
      KAPA1   = 0.5D0*(-BB+SQDD)
      KAPA2   = 0.5D0*(-BB-SQDD)
      LAMDA1  = THETA1 + THETA2*KAPA1
      LAMDA2  = THETA1 + THETA2*KAPA2
C
      IF (KAPA1.GE.ZERO .AND. LAMDA1.GE.ZERO) THEN
         IF (ALF-KAPA1-LAMDA1.GE.ZERO .AND.
     &       BET-KAPA1.GE.ZERO .AND. GAM-LAMDA1.GE.ZERO) THEN
             KAPA = KAPA1
             LAMDA= LAMDA1
             GOTO 200
         ENDIF
      ENDIF
C
      IF (KAPA2.GE.ZERO .AND. LAMDA2.GE.ZERO) THEN
         IF (ALF-KAPA2-LAMDA2.GE.ZERO .AND. 
     &       BET-KAPA2.GE.ZERO .AND. GAM-LAMDA2.GE.ZERO) THEN
             KAPA = KAPA2
             LAMDA= LAMDA2
             GOTO 200
         ENDIF
      ENDIF
C
C SEPERATE SOLUTION OF NH4CL & NH4NO3 EQUILIBRIA 
C 
100   KAPA  = ZERO
      LAMDA = ZERO
      DD1   = (ALF+BET)*(ALF+BET) - 4.0D0*(ALF*BET-A1)
      DD2   = (ALF+GAM)*(ALF+GAM) - 4.0D0*(ALF*GAM-A2)
C
C NH4CL EQUILIBRIUM
C
      IF (DD1.GE.ZERO) THEN
         SQDD1 = SQRT(DD1)
         KAPA1 = 0.5D0*(ALF+BET + SQDD1)
         KAPA2 = 0.5D0*(ALF+BET - SQDD1)
C
         IF (KAPA1.GE.ZERO .AND. KAPA1.LE.MIN(ALF,BET)) THEN
            KAPA = KAPA1 
         ELSE IF (KAPA2.GE.ZERO .AND. KAPA2.LE.MIN(ALF,BET)) THEN
            KAPA = KAPA2
         ELSE
            KAPA = ZERO
         ENDIF
      ENDIF
C
C NH4NO3 EQUILIBRIUM
C
      IF (DD2.GE.ZERO) THEN
         SQDD2 = SQRT(DD2)
         LAMDA1= 0.5D0*(ALF+GAM + SQDD2)
         LAMDA2= 0.5D0*(ALF+GAM - SQDD2)
C
         IF (LAMDA1.GE.ZERO .AND. LAMDA1.LE.MIN(ALF,GAM)) THEN
            LAMDA = LAMDA1 
         ELSE IF (LAMDA2.GE.ZERO .AND. LAMDA2.LE.MIN(ALF,GAM)) THEN
            LAMDA = LAMDA2
         ELSE
            LAMDA = ZERO
         ENDIF
      ENDIF
C
C IF BOTH KAPA, LAMDA ARE > 0, THEN APPLY EXISTANCE CRITERION
C
      IF (KAPA.GT.ZERO .AND. LAMDA.GT.ZERO) THEN
         IF (BET .LT. LAMDA/THETA1) THEN
            KAPA = ZERO
         ELSE
            LAMDA= ZERO
         ENDIF
      ENDIF
C
C *** CALCULATE COMPOSITION OF VOLATILE SPECIES ***********************
C
200   CONTINUE
      CNH4NO3 = LAMDA
      CNH4CL  = KAPA
C
      GNH3    = ALF - KAPA - LAMDA
      GHNO3   = GAM - LAMDA
      GHCL    = BET - KAPA
C
      RETURN
C
C *** END OF SUBROUTINE CALCH1A *****************************************
C
      END
C=======================================================================
C
C *** ISORROPIA CODE
C *** SUBROUTINE CALCI6
C *** CASE I6
C
C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
C     1. SULFATE RICH, NO FREE ACID (1.0 <= SULRAT < 2.0)
C     2. SOLID & LIQUID AEROSOL POSSIBLE
C     3. SOLIDS POSSIBLE : (NH4)2SO4, NA2SO4
C
C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
C *** GEORGIA INSTITUTE OF TECHNOLOGY
C *** WRITTEN BY ATHANASIOS NENES
C *** UPDATED BY CHRISTOS FOUNTOUKIS
C
C=======================================================================
C
      SUBROUTINE CALCI6
      INCLUDE 'isrpia.inc'
      COMMON /SOLUT/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,
     &               PSI1, PSI2, PSI3, PSI4, PSI5, PSI6, PSI7, PSI8,
     &               A1,   A2,   A3,   A4,   A5,   A6,   A7,   A8
C
C *** FIND DRY COMPOSITION **********************************************
C
      CALL CALCI1A
C
C *** SETUP PARAMETERS ************************************************
C
      CHI1 = CNH4HS4               ! Save from CALCI1 run
      CHI2 = CLC    
      CHI3 = CNAHSO4
      CHI4 = CNA2SO4
      CHI5 = CNH42S4
C
      PSI1 = CNH4HS4               ! ASSIGN INITIAL PSI's
      PSI2 = CLC   
      PSI3 = CNAHSO4
      PSI4 = CNA2SO4
      PSI5 = CNH42S4
C
      CALAOU = .TRUE.              ! Outer loop activity calculation flag
      FRST   = .TRUE.
      CALAIN = .TRUE.
C
C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
C
      DO 10 I=1,NSWEEP
C
      A6 = XK1 *WATER/GAMA(7)*(GAMA(8)/GAMA(7))**2.
C
C  CALCULATE DISSOCIATION QUANTITIES
C
      BB   = PSI2 + PSI4 + PSI5 + A6                    ! PSI6
      CC   =-A6*(PSI2 + PSI3 + PSI1)
      DD   = BB*BB - 4.D0*CC
      PSI6 = 0.5D0*(-BB + SQRT(DD))
C
C *** CALCULATE SPECIATION ********************************************
C
      MOLAL (1) = PSI6                                    ! HI
      MOLAL (2) = 2.D0*PSI4 + PSI3                        ! NAI
      MOLAL (3) = 3.D0*PSI2 + 2.D0*PSI5 + PSI1            ! NH4I
      MOLAL (5) = PSI2 + PSI4 + PSI5 + PSI6               ! SO4I
      MOLAL (6) = PSI2 + PSI3 + PSI1 - PSI6               ! HSO4I
      CLC       = ZERO
      CNAHSO4   = ZERO
      CNA2SO4   = CHI4 - PSI4
      CNH42S4   = ZERO
      CNH4HS4   = ZERO
      CALL CALCMR                                         ! Water content
C
C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT     
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
C 
20    RETURN
C
C *** END OF SUBROUTINE CALCI6 *****************************************
C
      END

C=======================================================================
C
C *** ISORROPIA CODE
C *** SUBROUTINE CALCI5
C *** CASE I5
C
C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
C     1. SULFATE RICH, NO FREE ACID (1.0 <= SULRAT < 2.0)
C     2. SOLID & LIQUID AEROSOL POSSIBLE
C     3. SOLIDS POSSIBLE : (NH4)2SO4, NA2SO4
C
C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
C *** GEORGIA INSTITUTE OF TECHNOLOGY
C *** WRITTEN BY ATHANASIOS NENES
C *** UPDATED BY CHRISTOS FOUNTOUKIS
C
C=======================================================================
C
      SUBROUTINE CALCI5
      INCLUDE 'isrpia.inc'
      COMMON /SOLUT/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,
     &               PSI1, PSI2, PSI3, PSI4, PSI5, PSI6, PSI7, PSI8,
     &               A1,   A2,   A3,   A4,   A5,   A6,   A7,   A8
C
C *** FIND DRY COMPOSITION **********************************************
C
      CALL CALCI1A
C
C *** SETUP PARAMETERS ************************************************
C
      CHI1 = CNH4HS4               ! Save from CALCI1 run
      CHI2 = CLC    
      CHI3 = CNAHSO4
      CHI4 = CNA2SO4
      CHI5 = CNH42S4
C
      PSI1 = CNH4HS4               ! ASSIGN INITIAL PSI's
      PSI2 = CLC   
      PSI3 = CNAHSO4
      PSI4 = ZERO
      PSI5 = CNH42S4
C
      CALAOU =.TRUE.               ! Outer loop activity calculation flag
      PSI4LO = ZERO                ! Low  limit
      PSI4HI = CHI4                ! High limit
C    
C *** IF NA2SO4(S) =0, CALL FUNCI5B FOR Y4=0 ***************************
C
      IF (CHI4.LE.TINY) THEN
         Y1 = FUNCI5A (ZERO)
         GOTO 50
      ENDIF
C
C *** INITIAL VALUES FOR BISECTION ************************************
C
      X1 = PSI4HI
      Y1 = FUNCI5A (X1)
      YHI= Y1                      ! Save Y-value at HI position
C
C *** YHI < 0.0 THE SOLUTION IS ALWAYS UNDERSATURATED WITH NA2SO4 **
C
      IF (ABS(Y1).LE.EPS .OR. YHI.LT.ZERO) GOTO 50
C
C *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
C
      DX = (PSI4HI-PSI4LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = X1-DX
         Y2 = FUNCI5A (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
C
C *** { YLO, YHI } > 0.0 THE SOLUTION IS ALWAYS SUPERSATURATED WITH NH4CL  
C
      YLO= Y1                      ! Save Y-value at Hi position
      IF (YLO.GT.ZERO .AND. YHI.GT.ZERO) THEN
         Y3 = FUNCI5A (ZERO)
         GOTO 50
      ELSE IF (ABS(Y2) .LT. EPS) THEN   ! X2 IS A SOLUTION 
         GOTO 50
      ELSE
         CALL PUSHERR (0001, 'CALCI5')    ! WARNING ERROR: NO SOLUTION
         GOTO 50
      ENDIF
C
C *** PERFORM BISECTION ***********************************************
C
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCI5A (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR (0002, 'CALCI5')    ! WARNING ERROR: NO CONVERGENCE
C
C *** CONVERGED ; RETURN **********************************************
C
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCI5A (X3)
C 
50    RETURN

C *** END OF SUBROUTINE CALCI5 *****************************************
C
      END




C=======================================================================
C
C *** ISORROPIA CODE
C *** SUBROUTINE FUNCI5A
C *** CASE I5
C
C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
C     1. SULFATE RICH, NO FREE ACID (1.0 <= SULRAT < 2.0)
C     2. SOLID & LIQUID AEROSOL POSSIBLE
C     3. SOLIDS POSSIBLE : NA2SO4
C
C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
C *** GEORGIA INSTITUTE OF TECHNOLOGY
C *** WRITTEN BY ATHANASIOS NENES
C *** UPDATED BY CHRISTOS FOUNTOUKIS
C
C=======================================================================
C
      DOUBLE PRECISION FUNCTION FUNCI5A (P4)
      INCLUDE 'isrpia.inc'
      COMMON /SOLUT/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,
     &               PSI1, PSI2, PSI3, PSI4, PSI5, PSI6, PSI7, PSI8,
     &               A1,   A2,   A3,   A4,   A5,   A6,   A7,   A8
C
C *** SETUP PARAMETERS ************************************************
C
      PSI4   = P4     ! PSI3 already assigned in FUNCI5A
      FRST   = .TRUE.
      CALAIN = .TRUE.
C
C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
C
      DO 10 I=1,NSWEEP
C
      A4 = XK5 *(WATER/GAMA(2))**3.0
      A5 = XK7 *(WATER/GAMA(4))**3.0
      A6 = XK1 *WATER/GAMA(7)*(GAMA(8)/GAMA(7))**2.
C
C  CALCULATE DISSOCIATION QUANTITIES
C
      BB   = PSI2 + PSI4 + PSI5 + A6                    ! PSI6
      CC   =-A6*(PSI2 + PSI3 + PSI1)
      DD   = BB*BB - 4.D0*CC
      PSI6 = 0.5D0*(-BB + SQRT(DD))
C
C *** CALCULATE SPECIATION ********************************************
C
      MOLAL (1) = PSI6                            ! HI
      MOLAL (2) = 2.D0*PSI4 + PSI3                ! NAI
      MOLAL (3) = 3.D0*PSI2 + 2.D0*PSI5 + PSI1    ! NH4I
      MOLAL (5) = PSI2 + PSI4 + PSI5 + PSI6       ! SO4I
      MOLAL (6) = PSI2 + PSI3 + PSI1 - PSI6       ! HSO4I
      CLC       = ZERO
      CNAHSO4   = ZERO
      CNA2SO4   = CHI4 - PSI4
      CNH42S4   = ZERO
      CNH4HS4   = ZERO
      CALL CALCMR                                 ! Water content
C
C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT     
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
C
C *** CALCULATE OBJECTIVE FUNCTION ************************************
C
20    A4     = XK5 *(WATER/GAMA(2))**3.0    
      FUNCI5A= MOLAL(5)*MOLAL(2)*MOLAL(2)/A4 - ONE
      RETURN
C
C *** END OF FUNCTION FUNCI5A ********************************************
C
      END
C=======================================================================
C
C *** ISORROPIA CODE
C *** SUBROUTINE CALCI4
C *** CASE I4
C
C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
C     1. SULFATE RICH, NO FREE ACID (1.0 <= SULRAT < 2.0)
C     2. SOLID & LIQUID AEROSOL POSSIBLE
C     3. SOLIDS POSSIBLE : (NH4)2SO4, NA2SO4
C
C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
C *** GEORGIA INSTITUTE OF TECHNOLOGY
C *** WRITTEN BY ATHANASIOS NENES
C *** UPDATED BY CHRISTOS FOUNTOUKIS
C
C=======================================================================
C
      SUBROUTINE CALCI4
      INCLUDE 'isrpia.inc'
      COMMON /SOLUT/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,
     &               PSI1, PSI2, PSI3, PSI4, PSI5, PSI6, PSI7, PSI8,
     &               A1,   A2,   A3,   A4,   A5,   A6,   A7,   A8
C
C *** FIND DRY COMPOSITION **********************************************
C
      CALL CALCI1A
C
C *** SETUP PARAMETERS ************************************************
C
      CHI1 = CNH4HS4               ! Save from CALCI1 run
      CHI2 = CLC    
      CHI3 = CNAHSO4
      CHI4 = CNA2SO4
      CHI5 = CNH42S4
C
      PSI1 = CNH4HS4               ! ASSIGN INITIAL PSI's
      PSI2 = CLC   
      PSI3 = CNAHSO4
      PSI4 = ZERO  
      PSI5 = ZERO
C
      CALAOU = .TRUE.              ! Outer loop activity calculation flag
      PSI4LO = ZERO                ! Low  limit
      PSI4HI = CHI4                ! High limit
C    
C *** IF NA2SO4(S) =0, CALL FUNCI4B FOR Y4=0 ***************************
C
      IF (CHI4.LE.TINY) THEN
         Y1 = FUNCI4A (ZERO)
         GOTO 50
      ENDIF
C
C *** INITIAL VALUES FOR BISECTION ************************************
C
      X1 = PSI4HI
      Y1 = FUNCI4A (X1)
      YHI= Y1                      ! Save Y-value at HI position
C
C *** YHI < 0.0 THE SOLUTION IS ALWAYS UNDERSATURATED WITH NA2SO4 **
C
      IF (ABS(Y1).LE.EPS .OR. YHI.LT.ZERO) GOTO 50
C
C *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
C
      DX = (PSI4HI-PSI4LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = X1-DX
         Y2 = FUNCI4A (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
C
C *** { YLO, YHI } > 0.0 THE SOLUTION IS ALWAYS SUPERSATURATED WITH NH4CL  
C
      YLO= Y1                      ! Save Y-value at Hi position
      IF (YLO.GT.ZERO .AND. YHI.GT.ZERO) THEN
         Y3 = FUNCI4A (ZERO)
         GOTO 50
      ELSE IF (ABS(Y2) .LT. EPS) THEN   ! X2 IS A SOLUTION 
         GOTO 50
      ELSE
         CALL PUSHERR (0001, 'CALCI4')    ! WARNING ERROR: NO SOLUTION
         GOTO 50
      ENDIF
C
C *** PERFORM BISECTION ***********************************************
C
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCI4A (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR (0002, 'CALCI4')    ! WARNING ERROR: NO CONVERGENCE
C
C *** CONVERGED ; RETURN **********************************************
C
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCI4A (X3)
C
50    RETURN

C *** END OF SUBROUTINE CALCI4 *****************************************
C
      END




C=======================================================================
C
C *** ISORROPIA CODE
C *** SUBROUTINE FUNCI4A
C *** CASE I4
C
C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
C     1. SULFATE RICH, NO FREE ACID (1.0 <= SULRAT < 2.0)
C     2. SOLID & LIQUID AEROSOL POSSIBLE
C     3. SOLIDS POSSIBLE : (NH4)2SO4, NA2SO4
C
C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
C *** GEORGIA INSTITUTE OF TECHNOLOGY
C *** WRITTEN BY ATHANASIOS NENES
C *** UPDATED BY CHRISTOS FOUNTOUKIS
C
C=======================================================================
C
      DOUBLE PRECISION FUNCTION FUNCI4A (P4)
      INCLUDE 'isrpia.inc'
      COMMON /SOLUT/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,
     &               PSI1, PSI2, PSI3, PSI4, PSI5, PSI6, PSI7, PSI8,
     &               A1,   A2,   A3,   A4,   A5,   A6,   A7,   A8
C
C *** SETUP PARAMETERS ************************************************
C
      PSI4   = P4     ! PSI3 already assigned in FUNCI4A
      FRST   = .TRUE.
      CALAIN = .TRUE.
C
C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
C
      DO 10 I=1,NSWEEP
C
      A4 = XK5 *(WATER/GAMA(2))**3.0
      A5 = XK7 *(WATER/GAMA(4))**3.0
      A6 = XK1 *WATER/GAMA(7)*(GAMA(8)/GAMA(7))**2.
      A7 = SQRT(A4/A5)
C
C  CALCULATE DISSOCIATION QUANTITIES
C
      BB   = PSI2 + PSI4 + PSI5 + A6                    ! PSI6
      CC   =-A6*(PSI2 + PSI3 + PSI1)
      DD   = BB*BB - 4.D0*CC
      PSI6 = 0.5D0*(-BB + SQRT(DD))
C
      PSI5 = (PSI3 + 2.D0*PSI4 - A7*(3.D0*PSI2 + PSI1))/2.D0/A7 
      PSI5 = MIN (PSI5, CHI5)
C
C *** CALCULATE SPECIATION ********************************************
C
      MOLAL (1) = PSI6                            ! HI
      MOLAL (2) = 2.D0*PSI4 + PSI3                ! NAI
      MOLAL (3) = 3.D0*PSI2 + 2.D0*PSI5 + PSI1    ! NH4I
      MOLAL (5) = PSI2 + PSI4 + PSI5 + PSI6       ! SO4I
      MOLAL (6) = PSI2 + PSI3 + PSI1 - PSI6       ! HSO4I
      CLC       = ZERO
      CNAHSO4   = ZERO
      CNA2SO4   = CHI4 - PSI4
      CNH42S4   = CHI5 - PSI5
      CNH4HS4   = ZERO
      CALL CALCMR                                 ! Water content
C
C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT     
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
C
C *** CALCULATE OBJECTIVE FUNCTION ************************************
C
20    A4     = XK5 *(WATER/GAMA(2))**3.0    
      FUNCI4A= MOLAL(5)*MOLAL(2)*MOLAL(2)/A4 - ONE
      RETURN
C
C *** END OF FUNCTION FUNCI4A ********************************************
C
      END
C=======================================================================
C
C *** ISORROPIA CODE
C *** SUBROUTINE CALCI3
C *** CASE I3
C
C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
C     1. SULFATE RICH, NO FREE ACID (1.0 <= SULRAT < 2.0)
C     2. SOLID & LIQUID AEROSOL POSSIBLE
C     3. SOLIDS POSSIBLE : (NH4)2SO4, NA2SO4, NAHSO4, LC
C
C     THERE ARE THREE REGIMES IN THIS CASE:
C     1.(NA,NH4)HSO4(s) POSSIBLE. LIQUID & SOLID AEROSOL (SUBROUTINE CALCI3A)
C     2.(NA,NH4)HSO4(s) NOT POSSIBLE, AND RH < MDRH. SOLID AEROSOL ONLY 
C     3.(NA,NH4)HSO4(s) NOT POSSIBLE, AND RH >= MDRH. SOLID & LIQUID AEROSOL 
C
C     REGIMES 2. AND 3. ARE CONSIDERED TO BE THE SAME AS CASES I1A, I2B
C     RESPECTIVELY
C
C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
C *** GEORGIA INSTITUTE OF TECHNOLOGY
C *** WRITTEN BY ATHANASIOS NENES
C *** UPDATED BY CHRISTOS FOUNTOUKIS
C
C=======================================================================
C
      SUBROUTINE CALCI3
      INCLUDE 'isrpia.inc'
      EXTERNAL CALCI1A, CALCI4
C
C *** FIND DRY COMPOSITION **********************************************
C
      CALL CALCI1A
C
C *** REGIME DEPENDS UPON THE POSSIBLE SOLIDS & RH **********************
C
      IF (CNH4HS4.GT.TINY .OR. CNAHSO4.GT.TINY) THEN
         SCASE = 'I3 ; SUBCASE 1'  
         CALL CALCI3A                     ! FULL SOLUTION
         SCASE = 'I3 ; SUBCASE 1'  
      ENDIF
C
      IF (WATER.LE.TINY) THEN
         IF (RH.LT.DRMI3) THEN         ! SOLID SOLUTION
            WATER = TINY
            DO 10 I=1,NIONS
               MOLAL(I) = ZERO
10          CONTINUE
            CALL CALCI1A
            SCASE = 'I3 ; SUBCASE 2'  
C
         ELSEIF (RH.GE.DRMI3) THEN     ! MDRH OF I3
            SCASE = 'I3 ; SUBCASE 3'
            CALL CALCMDRH (RH, DRMI3, DRLC, CALCI1A, CALCI4)
            SCASE = 'I3 ; SUBCASE 3'
         ENDIF
      ENDIF
C 
      RETURN
C
C *** END OF SUBROUTINE CALCI3 ******************************************
C
      END



C=======================================================================
C
C *** ISORROPIA CODE
C *** SUBROUTINE CALCI3A
C *** CASE I3 ; SUBCASE 1
C
C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
C     1. SULFATE RICH, NO FREE ACID (1.0 <= SULRAT < 2.0)
C     2. SOLID & LIQUID AEROSOL POSSIBLE
C     3. SOLIDS POSSIBLE : (NH4)2SO4, NA2SO4, LC
C
C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
C *** GEORGIA INSTITUTE OF TECHNOLOGY
C *** WRITTEN BY ATHANASIOS NENES
C *** UPDATED BY CHRISTOS FOUNTOUKIS
C
C=======================================================================
C
      SUBROUTINE CALCI3A
      INCLUDE 'isrpia.inc'
      COMMON /SOLUT/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,
     &               PSI1, PSI2, PSI3, PSI4, PSI5, PSI6, PSI7, PSI8,
     &               A1,   A2,   A3,   A4,   A5,   A6,   A7,   A8
C
C *** FIND DRY COMPOSITION **********************************************
C
      CALL CALCI1A         ! Needed when called from CALCMDRH
C
C *** SETUP PARAMETERS ************************************************
C
      CHI1 = CNH4HS4               ! Save from CALCI1 run
      CHI2 = CLC    
      CHI3 = CNAHSO4
      CHI4 = CNA2SO4
      CHI5 = CNH42S4
C
      PSI1 = CNH4HS4               ! ASSIGN INITIAL PSI's
      PSI2 = ZERO   
      PSI3 = CNAHSO4
      PSI4 = ZERO  
      PSI5 = ZERO
C
      CALAOU = .TRUE.              ! Outer loop activity calculation flag
      PSI2LO = ZERO                ! Low  limit
      PSI2HI = CHI2                ! High limit
C
C *** INITIAL VALUES FOR BISECTION ************************************
C
      X1 = PSI2HI
      Y1 = FUNCI3A (X1)
      YHI= Y1                      ! Save Y-value at HI position
C
C *** YHI < 0.0 THE SOLUTION IS ALWAYS UNDERSATURATED WITH LC *********
C
      IF (YHI.LT.EPS) GOTO 50
C
C *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
C
      DX = (PSI2HI-PSI2LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = MAX(X1-DX, PSI2LO)
         Y2 = FUNCI3A (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
C
C *** { YLO, YHI } > 0.0 THE SOLUTION IS ALWAYS SUPERSATURATED WITH LC  
C
      IF (Y2.GT.EPS) Y2 = FUNCI3A (ZERO)
      GOTO 50
C
C *** PERFORM BISECTION ***********************************************
C
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCI3A (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR (0002, 'CALCI3A')    ! WARNING ERROR: NO CONVERGENCE
C
C *** CONVERGED ; RETURN **********************************************
C
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCI3A (X3)
C 
50    RETURN

C *** END OF SUBROUTINE CALCI3A *****************************************
C
      END




C=======================================================================
C
C *** ISORROPIA CODE
C *** SUBROUTINE FUNCI3A
C *** CASE I3 ; SUBCASE 1
C
C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
C     1. SULFATE RICH, NO FREE ACID (1.0 <= SULRAT < 2.0)
C     2. SOLID & LIQUID AEROSOL POSSIBLE
C     3. SOLIDS POSSIBLE : (NH4)2SO4, NA2SO4, LC
C
C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
C *** GEORGIA INSTITUTE OF TECHNOLOGY
C *** WRITTEN BY ATHANASIOS NENES
C *** UPDATED BY CHRISTOS FOUNTOUKIS
C
C=======================================================================
C
      DOUBLE PRECISION FUNCTION FUNCI3A (P2)
      INCLUDE 'isrpia.inc'
      COMMON /SOLUT/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,
     &               PSI1, PSI2, PSI3, PSI4, PSI5, PSI6, PSI7, PSI8,
     &               A1,   A2,   A3,   A4,   A5,   A6,   A7,   A8
C
C *** SETUP PARAMETERS ************************************************
C
      PSI2   = P2                  ! Save PSI2 in COMMON BLOCK
      PSI4LO = ZERO                ! Low  limit for PSI4
      PSI4HI = CHI4                ! High limit for PSI4
C    
C *** IF NH3 =0, CALL FUNCI3B FOR Y4=0 ********************************
C
      IF (CHI4.LE.TINY) THEN
         FUNCI3A = FUNCI3B (ZERO)
         GOTO 50
      ENDIF
C
C *** INITIAL VALUES FOR BISECTION ************************************
C
      X1 = PSI4HI
      Y1 = FUNCI3B (X1)
      IF (ABS(Y1).LE.EPS) GOTO 50
      YHI= Y1                      ! Save Y-value at HI position
C
C *** YHI < 0.0 THE SOLUTION IS ALWAYS UNDERSATURATED WITH NA2SO4 *****
C
      IF (YHI.LT.ZERO) GOTO 50
C
C *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
C
      DX = (PSI4HI-PSI4LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = MAX(X1-DX, PSI4LO)
         Y2 = FUNCI3B (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
C
C *** { YLO, YHI } > 0.0 THE SOLUTION IS ALWAYS SUPERSATURATED WITH NA2SO4
C
      IF (Y2.GT.EPS) Y2 = FUNCI3B (PSI4LO)
      GOTO 50
C
C *** PERFORM BISECTION ***********************************************
C
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCI3B (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR (0004, 'FUNCI3A')    ! WARNING ERROR: NO CONVERGENCE
C
C *** INNER LOOP CONVERGED **********************************************
C
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCI3B (X3)
C
C *** CALCULATE FUNCTION VALUE FOR OUTER LOOP ***************************
C
50    A2      = XK13*(WATER/GAMA(13))**5.0
      FUNCI3A = MOLAL(5)*MOLAL(6)*MOLAL(3)**3.D0/A2 - ONE
      RETURN
C
C *** END OF FUNCTION FUNCI3A *******************************************
C
      END



C=======================================================================
C
C *** ISORROPIA CODE
C *** FUNCTION FUNCI3B
C *** CASE I3 ; SUBCASE 2
C
C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
C     1. SULFATE RICH, NO FREE ACID (1.0 <= SULRAT < 2.0)
C     2. SOLID & LIQUID AEROSOL POSSIBLE
C     3. SOLIDS POSSIBLE : (NH4)2SO4, NA2SO4, LC
C
C     SOLUTION IS SAVED IN COMMON BLOCK /CASE/
C
C=======================================================================
C
      DOUBLE PRECISION FUNCTION FUNCI3B (P4)
      INCLUDE 'isrpia.inc'
C
      COMMON /SOLUT/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,
     &               PSI1, PSI2, PSI3, PSI4, PSI5, PSI6, PSI7, PSI8,
     &               A1,   A2,   A3,   A4,   A5,   A6,   A7,   A8
C
C *** SETUP PARAMETERS ************************************************
C
      PSI4   = P4   
C
C *** SETUP PARAMETERS ************************************************
C
      FRST   = .TRUE.
      CALAIN = .TRUE.
C
C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
C
      DO 10 I=1,NSWEEP
C
      A4 = XK5*(WATER/GAMA(2))**3.0
      A5 = XK7*(WATER/GAMA(4))**3.0
      A6 = XK1*WATER/GAMA(7)*(GAMA(8)/GAMA(7))**2.
      A7 = SQRT(A4/A5)
C
C  CALCULATE DISSOCIATION QUANTITIES
C
      BB   = PSI2 + PSI4 + PSI5 + A6                    ! PSI6
      CC   =-A6*(PSI2 + PSI3 + PSI1)
      DD   = BB*BB - 4.D0*CC
      PSI6 = 0.5D0*(-BB + SQRT(DD))
C
      PSI5 = (PSI3 + 2.D0*PSI4 - A7*(3.D0*PSI2 + PSI1))/2.D0/A7 
      PSI5 = MIN (PSI5, CHI5)
C
C *** CALCULATE SPECIATION ********************************************
C
      MOLAL(1) = PSI6                                  ! HI
      MOLAL(2) = 2.D0*PSI4 + PSI3                      ! NAI
      MOLAL(3) = 3.D0*PSI2 + 2.D0*PSI5 + PSI1          ! NH4I
      MOLAL(5) = PSI2 + PSI4 + PSI5 + PSI6             ! SO4I
      MOLAL(6) = MAX(PSI2 + PSI3 + PSI1 - PSI6, TINY)  ! HSO4I
      CLC      = MAX(CHI2 - PSI2, ZERO)
      CNAHSO4  = ZERO
      CNA2SO4  = MAX(CHI4 - PSI4, ZERO)
      CNH42S4  = MAX(CHI5 - PSI5, ZERO)
      CNH4HS4  = ZERO
      CALL CALCMR                                       ! Water content
C
C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT     
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
C
C *** CALCULATE OBJECTIVE FUNCTION ************************************
C
20    A4     = XK5 *(WATER/GAMA(2))**3.0    
      FUNCI3B= MOLAL(5)*MOLAL(2)*MOLAL(2)/A4 - ONE
      RETURN
C
C *** END OF FUNCTION FUNCI3B ********************************************
C
      END
C=======================================================================
C
C *** ISORROPIA CODE
C *** SUBROUTINE CALCI2
C *** CASE I2
C
C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
C     1. SULFATE RICH, NO FREE ACID (1.0 <= SULRAT < 2.0)
C     2. SOLID & LIQUID AEROSOL POSSIBLE
C     3. SOLIDS POSSIBLE : (NH4)2SO4, NA2SO4, NAHSO4, LC
C
C     THERE ARE THREE REGIMES IN THIS CASE:
C     1. NH4HSO4(s) POSSIBLE. LIQUID & SOLID AEROSOL (SUBROUTINE CALCI2A)
C     2. NH4HSO4(s) NOT POSSIBLE, AND RH < MDRH. SOLID AEROSOL ONLY 
C     3. NH4HSO4(s) NOT POSSIBLE, AND RH >= MDRH. SOLID & LIQUID AEROSOL 
C
C     REGIMES 2. AND 3. ARE CONSIDERED TO BE THE SAME AS CASES I1A, I2B
C     RESPECTIVELY
C
C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
C *** GEORGIA INSTITUTE OF TECHNOLOGY
C *** WRITTEN BY ATHANASIOS NENES
C *** UPDATED BY CHRISTOS FOUNTOUKIS
C
C=======================================================================
C
      SUBROUTINE CALCI2
      INCLUDE 'isrpia.inc'
      EXTERNAL CALCI1A, CALCI3A
C
C *** FIND DRY COMPOSITION **********************************************
C
      CALL CALCI1A
C
C *** REGIME DEPENDS UPON THE POSSIBLE SOLIDS & RH **********************
C
      IF (CNH4HS4.GT.TINY) THEN
         SCASE = 'I2 ; SUBCASE 1'  
         CALL CALCI2A                       
         SCASE = 'I2 ; SUBCASE 1'  
      ENDIF
C
      IF (WATER.LE.TINY) THEN
         IF (RH.LT.DRMI2) THEN         ! SOLID SOLUTION ONLY
            WATER = TINY
            DO 10 I=1,NIONS
               MOLAL(I) = ZERO
10          CONTINUE
            CALL CALCI1A
            SCASE = 'I2 ; SUBCASE 2'  
C
         ELSEIF (RH.GE.DRMI2) THEN     ! MDRH OF I2
            SCASE = 'I2 ; SUBCASE 3'
            CALL CALCMDRH (RH, DRMI2, DRNAHSO4, CALCI1A, CALCI3A)
            SCASE = 'I2 ; SUBCASE 3'
         ENDIF
      ENDIF
C 
      RETURN
C
C *** END OF SUBROUTINE CALCI2 ******************************************
C
      END


C=======================================================================
C
C *** ISORROPIA CODE
C *** SUBROUTINE CALCI2A
C *** CASE I2 ; SUBCASE A
C
C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
C     1. SULFATE RICH, NO FREE ACID (1.0 <= SULRAT < 2.0)
C     2. SOLID & LIQUID AEROSOL POSSIBLE
C     3. SOLIDS POSSIBLE : (NH4)2SO4, NA2SO4, NAHSO4, LC
C
C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
C *** GEORGIA INSTITUTE OF TECHNOLOGY
C *** WRITTEN BY ATHANASIOS NENES
C *** UPDATED BY CHRISTOS FOUNTOUKIS
C
C=======================================================================
C
      SUBROUTINE CALCI2A
      INCLUDE 'isrpia.inc'
      COMMON /SOLUT/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,
     &               PSI1, PSI2, PSI3, PSI4, PSI5, PSI6, PSI7, PSI8,
     &               A1,   A2,   A3,   A4,   A5,   A6,   A7,   A8
C
C *** FIND DRY COMPOSITION **********************************************
C
      CALL CALCI1A    ! Needed when called from CALCMDRH
C
C *** SETUP PARAMETERS ************************************************
C
      CHI1 = CNH4HS4               ! Save from CALCI1 run
      CHI2 = CLC    
      CHI3 = CNAHSO4
      CHI4 = CNA2SO4
      CHI5 = CNH42S4
C
      PSI1 = CNH4HS4               ! ASSIGN INITIAL PSI's
      PSI2 = ZERO   
      PSI3 = ZERO   
      PSI4 = ZERO  
      PSI5 = ZERO
C
      CALAOU = .TRUE.              ! Outer loop activity calculation flag
      PSI2LO = ZERO                ! Low  limit
      PSI2HI = CHI2                ! High limit
C
C *** INITIAL VALUES FOR BISECTION ************************************
C
      X1 = PSI2HI
      Y1 = FUNCI2A (X1)
      YHI= Y1                      ! Save Y-value at HI position
C
C *** YHI < 0.0 THE SOLUTION IS ALWAYS UNDERSATURATED WITH LC *********
C
      IF (YHI.LT.EPS) GOTO 50
C
C *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
C
      DX = (PSI2HI-PSI2LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = MAX(X1-DX, PSI2LO)
         Y2 = FUNCI2A (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
C
C *** { YLO, YHI } > 0.0 THE SOLUTION IS ALWAYS SUPERSATURATED WITH LC  
C
      IF (Y2.GT.EPS) Y2 = FUNCI3A (ZERO)
      GOTO 50
C
C *** PERFORM BISECTION ***********************************************
C
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCI2A (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR (0002, 'CALCI2A')    ! WARNING ERROR: NO CONVERGENCE
C
C *** CONVERGED ; RETURN **********************************************
C
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCI2A (X3)
C
50    RETURN

C *** END OF SUBROUTINE CALCI2A *****************************************
C
      END




C=======================================================================
C
C *** ISORROPIA CODE
C *** SUBROUTINE FUNCI2A
C *** CASE I2 ; SUBCASE 1
C
C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
C     1. SULFATE RICH, NO FREE ACID (1.0 <= SULRAT < 2.0)
C     2. SOLID & LIQUID AEROSOL POSSIBLE
C     3. SOLIDS POSSIBLE : (NH4)2SO4, NA2SO4, NAHSO4, LC
C
C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
C *** GEORGIA INSTITUTE OF TECHNOLOGY
C *** WRITTEN BY ATHANASIOS NENES
C *** UPDATED BY CHRISTOS FOUNTOUKIS
C
C=======================================================================
C
      DOUBLE PRECISION FUNCTION FUNCI2A (P2)
      INCLUDE 'isrpia.inc'
      COMMON /SOLUT/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,
     &               PSI1, PSI2, PSI3, PSI4, PSI5, PSI6, PSI7, PSI8,
     &               A1,   A2,   A3,   A4,   A5,   A6,   A7,   A8
C
C *** SETUP PARAMETERS ************************************************
C
      FRST   = .TRUE.
      CALAIN = .TRUE.
      PSI2   = P2                  ! Save PSI2 in COMMON BLOCK
      PSI3   = CHI3
      PSI4   = CHI4
      PSI5   = CHI5
      PSI6   = ZERO
C
C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
C
      DO 10 I=1,NSWEEP
C
      A3 = XK11*(WATER/GAMA(12))**2.0
      A4 = XK5 *(WATER/GAMA(2))**3.0
      A5 = XK7 *(WATER/GAMA(4))**3.0
      A6 = XK1 *WATER/GAMA(7)*(GAMA(8)/GAMA(7))**2.
      A7 = SQRT(A4/A5)
C
C  CALCULATE DISSOCIATION QUANTITIES
C
      IF (CHI5.GT.TINY .AND. WATER.GT.TINY) THEN     
         PSI5 = (PSI3 + 2.D0*PSI4 - A7*(3.D0*PSI2 + PSI1))/2.D0/A7 
         PSI5 = MAX(MIN (PSI5, CHI5), TINY)
      ENDIF
C
      IF (CHI4.GT.TINY .AND. WATER.GT.TINY) THEN     
         AA   = PSI2+PSI5+PSI6+PSI3
         BB   = PSI3*AA
         CC   = 0.25D0*(PSI3*PSI3*(PSI2+PSI5+PSI6)-A4)
         CALL POLY3 (AA, BB, CC, PSI4, ISLV)
         IF (ISLV.EQ.0) THEN
            PSI4 = MIN (PSI4, CHI4)
         ELSE
            PSI4 = ZERO
         ENDIF
      ENDIF
C
      IF (CHI3.GT.TINY .AND. WATER.GT.TINY) THEN     
         AA   = 2.D0*PSI4 + PSI2 + PSI1 - PSI6
         BB   = 2.D0*PSI4*(PSI2 + PSI1 - PSI6) - A3
         CC   = ZERO
         CALL POLY3 (AA, BB, CC, PSI3, ISLV)
         IF (ISLV.EQ.0) THEN
            PSI3 = MIN (PSI3, CHI3)
         ELSE
            PSI3 = ZERO
         ENDIF
      ENDIF
C
      BB   = PSI2 + PSI4 + PSI5 + A6                    ! PSI6
      CC   =-A6*(PSI2 + PSI3 + PSI1)
      DD   = BB*BB - 4.D0*CC
      PSI6 = 0.5D0*(-BB + SQRT(DD))
C
C *** CALCULATE SPECIATION ********************************************
C
      MOLAL (1) = PSI6                           ! HI
      MOLAL (2) = 2.D0*PSI4 + PSI3               ! NAI
      MOLAL (3) = 3.D0*PSI2 + 2.D0*PSI5 + PSI1   ! NH4I
      MOLAL (5) = PSI2 + PSI4 + PSI5 + PSI6      ! SO4I
      MOLAL (6) = PSI2 + PSI3 + PSI1 - PSI6      ! HSO4I
      CLC       = CHI2 - PSI2
      CNAHSO4   = CHI3 - PSI3
      CNA2SO4   = CHI4 - PSI4
      CNH42S4   = CHI5 - PSI5
      CNH4HS4   = ZERO
      CALL CALCMR                                ! Water content
C
C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT     
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
C
C *** CALCULATE FUNCTION VALUE FOR OUTER LOOP ***************************
C
20    A2      = XK13*(WATER/GAMA(13))**5.0
      FUNCI2A = MOLAL(5)*MOLAL(6)*MOLAL(3)**3.D0/A2 - ONE
      RETURN
C
C *** END OF FUNCTION FUNCI2A *******************************************
C
      END

C=======================================================================
C
C *** ISORROPIA CODE
C *** SUBROUTINE CALCI1
C *** CASE I1
C
C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
C     1. SULFATE RICH, NO FREE ACID (1.0 <= SULRAT < 2.0)
C     2. SOLID AEROSOL ONLY
C     3. SOLIDS POSSIBLE : NH4NO3, NH4CL, NA2SO4
C
C     THERE ARE TWO POSSIBLE REGIMES HERE, DEPENDING ON RELATIVE HUMIDITY:
C     1. WHEN RH >= MDRH ; LIQUID PHASE POSSIBLE (MDRH REGION)
C     2. WHEN RH < MDRH  ; ONLY SOLID PHASE POSSIBLE (SUBROUTINE CALCI1A)
C
C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
C *** GEORGIA INSTITUTE OF TECHNOLOGY
C *** WRITTEN BY ATHANASIOS NENES
C *** UPDATED BY CHRISTOS FOUNTOUKIS
C
C=======================================================================
C
      SUBROUTINE CALCI1
      INCLUDE 'isrpia.inc'
      EXTERNAL CALCI1A, CALCI2A
C
C *** REGIME DEPENDS UPON THE AMBIENT RELATIVE HUMIDITY *****************
C
      IF (RH.LT.DRMI1) THEN    
         SCASE = 'I1 ; SUBCASE 1'  
         CALL CALCI1A              ! SOLID PHASE ONLY POSSIBLE
         SCASE = 'I1 ; SUBCASE 1'
      ELSE
         SCASE = 'I1 ; SUBCASE 2'  ! LIQUID & SOLID PHASE POSSIBLE
         CALL CALCMDRH (RH, DRMI1, DRNH4HS4, CALCI1A, CALCI2A)
         SCASE = 'I1 ; SUBCASE 2'
      ENDIF
C 
C *** AMMONIA IN GAS PHASE **********************************************
C
C      CALL CALCNH3
C 
      RETURN
C
C *** END OF SUBROUTINE CALCI1 ******************************************
C
      END


C=======================================================================
C
C *** ISORROPIA CODE
C *** SUBROUTINE CALCI1A
C *** CASE I1 ; SUBCASE 1
C
C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
C     1. SULFATE RICH, NO FREE ACID (1.0 <= SULRAT < 2.0)
C     2. SOLID AEROSOL ONLY
C     3. SOLIDS POSSIBLE : NH4HSO4, NAHSO4, (NH4)2SO4, NA2SO4, LC
C
C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
C *** GEORGIA INSTITUTE OF TECHNOLOGY
C *** WRITTEN BY ATHANASIOS NENES
C *** UPDATED BY CHRISTOS FOUNTOUKIS
C
C=======================================================================
C
      SUBROUTINE CALCI1A
      INCLUDE 'isrpia.inc'
C
C *** CALCULATE NON VOLATILE SOLIDS ***********************************
C
      CNA2SO4 = 0.5D0*W(1)
      CNH4HS4 = ZERO
      CNAHSO4 = ZERO
      CNH42S4 = ZERO
      FRSO4   = MAX(W(2)-CNA2SO4, ZERO)
C
      CLC     = MIN(W(3)/3.D0, FRSO4/2.D0)
      FRSO4   = MAX(FRSO4-2.D0*CLC, ZERO)
      FRNH4   = MAX(W(3)-3.D0*CLC,  ZERO)
C
      IF (FRSO4.LE.TINY) THEN
         CLC     = MAX(CLC - FRNH4, ZERO)
         CNH42S4 = 2.D0*FRNH4

      ELSEIF (FRNH4.LE.TINY) THEN
         CNH4HS4 = 3.D0*MIN(FRSO4, CLC)
         CLC     = MAX(CLC-FRSO4, ZERO)
         IF (CNA2SO4.GT.TINY) THEN
            FRSO4   = MAX(FRSO4-CNH4HS4/3.D0, ZERO)
            CNAHSO4 = 2.D0*FRSO4
            CNA2SO4 = MAX(CNA2SO4-FRSO4, ZERO)
         ENDIF
      ENDIF
C
C *** CALCULATE GAS SPECIES *********************************************
C
      GHNO3 = W(4)
      GHCL  = W(5)
      GNH3  = ZERO
C
      RETURN
C
C *** END OF SUBROUTINE CALCI1A *****************************************
C
      END
C=======================================================================
C
C *** ISORROPIA CODE
C *** SUBROUTINE CALCJ3
C *** CASE J3
C
C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
C     1. SULFATE RICH, FREE ACID (SULRAT < 1.0)
C     2. THERE IS ONLY A LIQUID PHASE
C
C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
C *** GEORGIA INSTITUTE OF TECHNOLOGY
C *** WRITTEN BY ATHANASIOS NENES
C *** UPDATED BY CHRISTOS FOUNTOUKIS
C
C=======================================================================
C
      SUBROUTINE CALCJ3
      INCLUDE 'isrpia.inc'
C
      DOUBLE PRECISION LAMDA, KAPA
C
C *** SETUP PARAMETERS ************************************************
C
      CALAOU = .TRUE.              ! Outer loop activity calculation flag
      FRST   = .TRUE.
      CALAIN = .TRUE.
C
      LAMDA  = MAX(W(2) - W(3) - W(1), TINY)  ! FREE H2SO4
      CHI1   = W(1)                           ! NA TOTAL as NaHSO4
      CHI2   = W(3)                           ! NH4 TOTAL as NH4HSO4
      PSI1   = CHI1
      PSI2   = CHI2                           ! ALL NH4HSO4 DELIQUESCED
C
C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
C
      DO 10 I=1,NSWEEP
C
      A3 = XK1  *WATER/GAMA(7)*(GAMA(8)/GAMA(7))**2.0
C
C  CALCULATE DISSOCIATION QUANTITIES
C
      BB   = A3+LAMDA                        ! KAPA
      CC   =-A3*(LAMDA + PSI1 + PSI2)
      DD   = BB*BB-4.D0*CC
      KAPA = 0.5D0*(-BB+SQRT(DD))
C
C *** CALCULATE SPECIATION ********************************************
C
      MOLAL (1) = LAMDA + KAPA                 ! HI
      MOLAL (2) = PSI1                         ! NAI
      MOLAL (3) = PSI2                         ! NH4I
      MOLAL (4) = ZERO                         ! CLI
      MOLAL (5) = KAPA                         ! SO4I
      MOLAL (6) = LAMDA + PSI1 + PSI2 - KAPA   ! HSO4I
      MOLAL (7) = ZERO                         ! NO3I
C
      CNAHSO4   = ZERO
      CNH4HS4   = ZERO
C
      CALL CALCMR                              ! Water content
C
C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT     
      ELSE
         GOTO 50
      ENDIF
10    CONTINUE
C 
50    RETURN
C
C *** END OF SUBROUTINE CALCJ3 ******************************************
C
      END
C=======================================================================
C
C *** ISORROPIA CODE
C *** SUBROUTINE CALCJ2
C *** CASE J2
C
C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
C     1. SULFATE RICH, FREE ACID (SULRAT < 1.0)
C     2. THERE IS BOTH A LIQUID & SOLID PHASE
C     3. SOLIDS POSSIBLE : NAHSO4
C
C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
C *** GEORGIA INSTITUTE OF TECHNOLOGY
C *** WRITTEN BY ATHANASIOS NENES
C *** UPDATED BY CHRISTOS FOUNTOUKIS
C
C=======================================================================
C
      SUBROUTINE CALCJ2
      INCLUDE 'isrpia.inc'
C
      DOUBLE PRECISION LAMDA, KAPA
      COMMON /CASEJ/ CHI1, CHI2, CHI3, LAMDA, KAPA, PSI1, PSI2, PSI3, 
     &               A1,   A2,   A3
C
C *** SETUP PARAMETERS ************************************************
C
      CALAOU = .TRUE.              ! Outer loop activity calculation flag
      CHI1   = W(1)                ! NA TOTAL
      CHI2   = W(3)                ! NH4 TOTAL
      PSI1LO = TINY                ! Low  limit
      PSI1HI = CHI1                ! High limit
C
C *** INITIAL VALUES FOR BISECTION ************************************
C
      X1 = PSI1HI
      Y1 = FUNCJ2 (X1)
      YHI= Y1                      ! Save Y-value at HI position
C
C *** YHI < 0.0 THE SOLUTION IS ALWAYS UNDERSATURATED WITH NH42SO4 ****
C
      IF (ABS(Y1).LE.EPS .OR. YHI.LT.ZERO) GOTO 50
C
C *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
C
      DX = (PSI1HI-PSI1LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = X1-DX
         Y2 = FUNCJ2 (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
C
C *** { YLO, YHI } > 0.0 THE SOLUTION IS ALWAYS SUPERSATURATED WITH NH42SO4
C
      YLO= Y1                      ! Save Y-value at Hi position
      IF (YLO.GT.ZERO .AND. YHI.GT.ZERO) THEN
         Y3 = FUNCJ2 (ZERO)
         GOTO 50
      ELSE IF (ABS(Y2) .LT. EPS) THEN   ! X2 IS A SOLUTION 
         GOTO 50
      ELSE
         CALL PUSHERR (0001, 'CALCJ2')    ! WARNING ERROR: NO SOLUTION
         GOTO 50
      ENDIF
C
C *** PERFORM BISECTION ***********************************************
C
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCJ2 (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR (0002, 'CALCJ2')    ! WARNING ERROR: NO CONVERGENCE
C
C *** CONVERGED ; RETURN **********************************************
C
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCJ2 (X3)
C 
50    RETURN
C
C *** END OF SUBROUTINE CALCJ2 ******************************************
C
      END




C=======================================================================
C
C *** ISORROPIA CODE
C *** SUBROUTINE FUNCJ2
C *** CASE J2
C
C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
C     1. SULFATE RICH, FREE ACID (SULRAT < 1.0)
C     2. THERE IS BOTH A LIQUID & SOLID PHASE
C     3. SOLIDS POSSIBLE : (NH4)2SO4, NH4CL, NA2SO4
C
C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
C *** GEORGIA INSTITUTE OF TECHNOLOGY
C *** WRITTEN BY ATHANASIOS NENES
C *** UPDATED BY CHRISTOS FOUNTOUKIS
C
C=======================================================================
C
      DOUBLE PRECISION FUNCTION FUNCJ2 (P1)
      INCLUDE 'isrpia.inc'
C
      DOUBLE PRECISION LAMDA, KAPA
      COMMON /CASEJ/ CHI1, CHI2, CHI3, LAMDA, KAPA, PSI1, PSI2, PSI3, 
     &               A1,   A2,   A3
C
C *** SETUP PARAMETERS ************************************************
C
      FRST   = .TRUE.
      CALAIN = .TRUE.
C
      LAMDA  = MAX(W(2) - W(3) - W(1), TINY)  ! FREE H2SO4
      PSI1   = P1
      PSI2   = CHI2                           ! ALL NH4HSO4 DELIQUESCED
C
C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
C
      DO 10 I=1,NSWEEP
C
      A1 = XK11 *(WATER/GAMA(12))**2.0
      A3 = XK1  *WATER/GAMA(7)*(GAMA(8)/GAMA(7))**2.0
C
C  CALCULATE DISSOCIATION QUANTITIES
C
      BB   = A3+LAMDA                        ! KAPA
      CC   =-A3*(LAMDA + PSI1 + PSI2)
      DD   = BB*BB-4.D0*CC
      KAPA = 0.5D0*(-BB+SQRT(DD))
C
C *** CALCULATE SPECIATION ********************************************
C
      MOLAL (1) = LAMDA + KAPA                  ! HI
      MOLAL (2) = PSI1                          ! NAI
      MOLAL (3) = PSI2                          ! NH4I
      MOLAL (4) = ZERO                          ! CLI
      MOLAL (5) = KAPA                          ! SO4I
      MOLAL (6) = LAMDA + PSI1 + PSI2 - KAPA    ! HSO4I
      MOLAL (7) = ZERO                          ! NO3I
C
      CNAHSO4   = MAX(CHI1-PSI1,ZERO)
      CNH4HS4   = ZERO
C
      CALL CALCMR                               ! Water content
C
C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT     
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
C
C *** CALCULATE OBJECTIVE FUNCTION ************************************
C
20    FUNCJ2 = MOLAL(2)*MOLAL(6)/A1 - ONE
C
C *** END OF FUNCTION FUNCJ2 *******************************************
C
      END

C=======================================================================
C
C *** ISORROPIA CODE
C *** SUBROUTINE CALCJ1
C *** CASE J1
C
C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
C     1. SULFATE RICH, FREE ACID (SULRAT < 1.0)
C     2. THERE IS BOTH A LIQUID & SOLID PHASE
C     3. SOLIDS POSSIBLE : NH4HSO4, NAHSO4
C
C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
C *** GEORGIA INSTITUTE OF TECHNOLOGY
C *** WRITTEN BY ATHANASIOS NENES
C *** UPDATED BY CHRISTOS FOUNTOUKIS
C
C=======================================================================
C
      SUBROUTINE CALCJ1
      INCLUDE 'isrpia.inc'
C
      DOUBLE PRECISION LAMDA, KAPA
      COMMON /CASEJ/ CHI1, CHI2, CHI3, LAMDA, KAPA, PSI1, PSI2, PSI3, 
     &               A1,   A2,   A3
C
C *** SETUP PARAMETERS ************************************************
C
      CALAOU =.TRUE.               ! Outer loop activity calculation flag
      CHI1   = W(1)                ! Total NA initially as NaHSO4
      CHI2   = W(3)                ! Total NH4 initially as NH4HSO4
C
      PSI1LO = TINY                ! Low  limit
      PSI1HI = CHI1                ! High limit
C
C *** INITIAL VALUES FOR BISECTION ************************************
C
      X1 = PSI1HI
      Y1 = FUNCJ1 (X1)
      YHI= Y1                      ! Save Y-value at HI position
C
C *** YHI < 0.0 THE SOLUTION IS ALWAYS UNDERSATURATED WITH NH42SO4 ****
C
      IF (ABS(Y1).LE.EPS .OR. YHI.LT.ZERO) GOTO 50
C
C *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
C
      DX = (PSI1HI-PSI1LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = X1-DX
         Y2 = FUNCJ1 (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
C
C *** { YLO, YHI } > 0.0 THE SOLUTION IS ALWAYS SUPERSATURATED WITH NH42SO4
C
      YLO= Y1                      ! Save Y-value at Hi position
      IF (YLO.GT.ZERO .AND. YHI.GT.ZERO) THEN
         Y3 = FUNCJ1 (ZERO)
         GOTO 50
      ELSE IF (ABS(Y2) .LT. EPS) THEN   ! X2 IS A SOLUTION 
         GOTO 50
      ELSE
         CALL PUSHERR (0001, 'CALCJ1')    ! WARNING ERROR: NO SOLUTION
         GOTO 50
      ENDIF
C
C *** PERFORM BISECTION ***********************************************
C
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCJ1 (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR (0002, 'CALCJ1')    ! WARNING ERROR: NO CONVERGENCE
C
C *** CONVERGED ; RETURN **********************************************
C
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCJ1 (X3)
C 
50    RETURN
C
C *** END OF SUBROUTINE CALCJ1 ******************************************
C
      END




C=======================================================================
C
C *** ISORROPIA CODE
C *** SUBROUTINE FUNCJ1
C *** CASE J1
C
C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
C     1. SULFATE RICH, FREE ACID (SULRAT < 1.0)
C     2. THERE IS BOTH A LIQUID & SOLID PHASE
C     3. SOLIDS POSSIBLE : (NH4)2SO4, NH4CL, NA2SO4
C
C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
C *** GEORGIA INSTITUTE OF TECHNOLOGY
C *** WRITTEN BY ATHANASIOS NENES
C *** UPDATED BY CHRISTOS FOUNTOUKIS
C
C=======================================================================
C
      DOUBLE PRECISION FUNCTION FUNCJ1 (P1)
      INCLUDE 'isrpia.inc'
      DOUBLE PRECISION LAMDA, KAPA
      COMMON /CASEJ/ CHI1, CHI2, CHI3, LAMDA, KAPA, PSI1, PSI2, PSI3, 
     &               A1,   A2,   A3
C
C *** SETUP PARAMETERS ************************************************
C
      FRST   = .TRUE.
      CALAIN = .TRUE.
C
      LAMDA  = MAX(W(2) - W(3) - W(1), TINY)  ! FREE H2SO4
      PSI1   = P1
C
C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
C
      DO 10 I=1,NSWEEP
C
      A1 = XK11 *(WATER/GAMA(12))**2.0
      A2 = XK12 *(WATER/GAMA(09))**2.0
      A3 = XK1  *WATER/GAMA(7)*(GAMA(8)/GAMA(7))**2.0
C
      PSI2 = 0.5*(-(LAMDA+PSI1) + SQRT((LAMDA+PSI1)**2.D0+4.D0*A2))  ! PSI2
      PSI2 = MIN (PSI2, CHI2)
C
      BB   = A3+LAMDA                        ! KAPA
      CC   =-A3*(LAMDA + PSI2 + PSI1)
      DD   = BB*BB-4.D0*CC
      KAPA = 0.5D0*(-BB+SQRT(DD))    
C
C *** SAVE CONCENTRATIONS IN MOLAL ARRAY ******************************
C
      MOLAL (1) = LAMDA + KAPA                  ! HI
      MOLAL (2) = PSI1                          ! NAI
      MOLAL (3) = PSI2                          ! NH4I
      MOLAL (4) = ZERO
      MOLAL (5) = KAPA                          ! SO4I
      MOLAL (6) = LAMDA + PSI1 + PSI2 - KAPA    ! HSO4I
      MOLAL (7) = ZERO
C
      CNAHSO4   = MAX(CHI1-PSI1,ZERO)
      CNH4HS4   = MAX(CHI2-PSI2,ZERO)
C
      CALL CALCMR                               ! Water content
C
C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT     
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
C
C *** CALCULATE OBJECTIVE FUNCTION ************************************
C
20    FUNCJ1 = MOLAL(2)*MOLAL(6)/A1 - ONE
C
C *** END OF FUNCTION FUNCJ1 *******************************************
C
      END

