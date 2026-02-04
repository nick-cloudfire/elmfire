! *****************************************************************************
MODULE ELMFIRE_SPOTTING
! *****************************************************************************

USE ELMFIRE_VARS
USE ELMFIRE_SUBS

IMPLICIT NONE

CONTAINS

! *****************************************************************************
SUBROUTINE SET_SPOTTING_PARAMETERS(R1)
! *****************************************************************************

REAL, DIMENSION(:) :: R1
INTEGER :: I
I = NUM_PARAMETERS_RASTERS

COEFFS_UNSCALED(I+1) = MEAN_SPOTTING_DIST_MIN                   + R1(I+1)*(MEAN_SPOTTING_DIST_MAX                   - MEAN_SPOTTING_DIST_MIN                  )
COEFFS_UNSCALED(I+2) = NORMALIZED_SPOTTING_DIST_VARIANCE_MIN    + R1(I+2)*(NORMALIZED_SPOTTING_DIST_VARIANCE_MAX    - NORMALIZED_SPOTTING_DIST_VARIANCE_MIN   )
COEFFS_UNSCALED(I+3) = SPOT_WS_EXP_LO                           + R1(I+3)*(SPOT_WS_EXP_HI                           - SPOT_WS_EXP_LO                          )
COEFFS_UNSCALED(I+4) = SPOT_FLIN_EXP_LO                         + R1(I+4)*(SPOT_FLIN_EXP_HI                         - SPOT_FLIN_EXP_LO                        )
COEFFS_UNSCALED(I+5) = NINT(NEMBERS_MAX_LO                      + R1(I+5)*(NEMBERS_MAX_HI                           - NEMBERS_MAX_LO                          ))
COEFFS_UNSCALED(I+6) = GLOBAL_SURFACE_FIRE_SPOTTING_PERCENT_MIN + R1(I+6)*(GLOBAL_SURFACE_FIRE_SPOTTING_PERCENT_MAX - GLOBAL_SURFACE_FIRE_SPOTTING_PERCENT_MIN)
COEFFS_UNSCALED(I+7) = CROWN_FIRE_SPOTTING_PERCENT_MIN          + R1(I+7)*(CROWN_FIRE_SPOTTING_PERCENT_MAX          - CROWN_FIRE_SPOTTING_PERCENT_MIN         )
COEFFS_UNSCALED(I+8) = PIGN_MIN                                 + R1(I+8)*(PIGN_MAX                                 - PIGN_MIN                                )
 
MEAN_SPOTTING_DIST                   = COEFFS_UNSCALED(I+1)
NORMALIZED_SPOTTING_DIST_VARIANCE    = COEFFS_UNSCALED(I+2)
SPOT_WS_EXP                          = COEFFS_UNSCALED(I+3)
SPOT_FLIN_EXP                        = COEFFS_UNSCALED(I+4)
NEMBERS_MAX                          = INT(COEFFS_UNSCALED(I+5), KIND=KIND(NEMBERS_MAX))
GLOBAL_SURFACE_FIRE_SPOTTING_PERCENT = COEFFS_UNSCALED(I+6)
CROWN_FIRE_SPOTTING_PERCENT          = COEFFS_UNSCALED(I+7)
PIGN                                 = COEFFS_UNSCALED(I+8)

SURFACE_FIRE_SPOTTING_PERCENT(:) = MIN(MAX(GLOBAL_SURFACE_FIRE_SPOTTING_PERCENT * SURFACE_FIRE_SPOTTING_PERCENT_MULT(:),0.),100.)

! *****************************************************************************
END SUBROUTINE SET_SPOTTING_PARAMETERS
! *****************************************************************************

! *****************************************************************************
SUBROUTINE SPOTTING(IX0,IY0,WS20_NOW,FLIN, N_SPOT_FIRES, IX_SPOT_FIRE, IY_SPOT_FIRE, &
                    ICASE, DT_ELMFIRE, TIME_NOW, IGNMULT,  IFBFM, LIST_EMBER_TRACKER)
                    ! BLDG_FOOTPRINT_FRAC_LOCAL,FMC, WN_FUEL, 
! *****************************************************************************

IMPLICIT NONE

INTEGER, INTENT(IN) :: IX0, IY0, ICASE
INTEGER*2, INTENT(IN) :: IFBFM
INTEGER :: N_SPOT_FIRES, IX_SPOT_FIRE(:), IY_SPOT_FIRE(:)
REAL, INTENT(IN) :: WS20_NOW, FLIN, DT_ELMFIRE, TIME_NOW, IGNMULT!&
                    ! BLDG_FOOTPRINT_FRAC_LOCAL, FMC, WN_FUEL, 

TYPE (DLL), INTENT(INOUT) :: LIST_EMBER_TRACKER

REAL :: R0, X0(1:3), MSD, SIGMA_DIST, MU_DIST, MU_CROSSWIND_LOCAL, SIGMA_CROSSWIND_LOCAL, &
         NEMBERS_REAL, P, SARDOY_PARAMETERS(1:4)
REAL, PARAMETER :: TSTOP_SPOT= 1200.

X0(1) = (REAL(IX0)-0.5) * CC%CELLSIZE 
X0(2) = (REAL(IY0)-0.5) * CC%CELLSIZE 
X0(3) = DEM%R4(IX0,IY0,1) + MAX(CH%R4(IX0,IY0,1),2.0)

MSD        = MAX( MEAN_SPOTTING_DIST*(FLIN**SPOT_FLIN_EXP)*(WS20_NOW**SPOT_WS_EXP), 1.0)
MU_DIST    = LOG(MSD*MSD / SQRT(MSD * NORMALIZED_SPOTTING_DIST_VARIANCE + MSD*MSD))
SIGMA_DIST = SQRT(LOG(1. + MSD * NORMALIZED_SPOTTING_DIST_VARIANCE / (MSD*MSD)))

IF (USE_UMD_SPOTTING_MODEL) THEN

   ! CALCULATE DISTRIBUTION PARAMETERS FROM LOCAL WIND SPEED & FIRELINE INTENSITY FROM SARDOY'S MODEL
   IF (.NOT. USE_CUSTOMIZED_PDF) THEN
      SARDOY_PARAMETERS= SARDOY_PDF_PARAMETERS(WS20_NOW, FLIN, IFBFM)
      MU_DIST          = SARDOY_PARAMETERS(1)
      SIGMA_DIST       = SARDOY_PARAMETERS(2)
      MU_CROSSWIND_LOCAL      = SARDOY_PARAMETERS(3)
      SIGMA_CROSSWIND_LOCAL   = SARDOY_PARAMETERS(4)
   ELSE
      MU_DIST          = MU_DOWNWIND
      SIGMA_DIST       = SIGMA_DOWNWIND
      MU_CROSSWIND_LOCAL      = MU_CROSSWIND
      SIGMA_CROSSWIND_LOCAL   = SIGMA_CROSSWIND
   ENDIF

   ! Calculate number of embers to emit
   IF (USE_PHYSICAL_EMBER_NUMBER) THEN
      ! NEMBERS_REAL = EMBER_TO_EMIT_PER_CELL(WS20_NOW, EMBER_SAMPLING_FACTOR, CC%CELLSIZE, &
      !                   BLDG_FOOTPRINT_FRAC_LOCAL, FMC, WN_FUEL, IFBFM, TAU_EMBERGEN, FLIN)
      NEMBERS_REAL = EMBER_TO_EMIT_PER_CELL(CC%CELLSIZE, IFBFM, FLIN, EMBER_GR_PER_MW_BLDG, EMBER_GR_PER_MW_VEGE)
      NEMBERS_REAL = NEMBERS_REAL * DT_ELMFIRE
   ELSE
      NEMBERS_REAL = EMBER_GR * CC%CELLSIZE * CC%CELLSIZE * DT_ELMFIRE
   ENDIF
   NEMBERS = FLOOR(NEMBERS_REAL/EMBER_SAMPLING_FACTOR)
   P = MOD(NEMBERS_REAL,1.0)
   CALL RANDOM_NUMBER(R0)
   IF (R0 .LE. P) NEMBERS = NEMBERS + 1
   CONTINUE
ELSE
   CALL RANDOM_NUMBER(R0)
   NEMBERS = NEMBERS_MIN + NINT (R0 * REAL(NEMBERS_MAX - NEMBERS_MIN) )
ENDIF

IF (USE_EULERIAN_SPOTTING) THEN
   IF (NEMBERS_REAL .LE. 1E-5) RETURN
   CALL APPEND(LIST_EMBER_TRACKER, IX0, IY0, TIME_NOW)
   LIST_EMBER_TRACKER%TAIL%NUM_EMBERS     = NEMBERS_REAL
   LIST_EMBER_TRACKER%TAIL%X0_ELM(:)      = X0(:)
   LIST_EMBER_TRACKER%TAIL%SIGMA_DIST     = MAX(SIGMA_DIST, 1E-5)
   LIST_EMBER_TRACKER%TAIL%MU_DIST        = MAX(MU_DIST,    1E-5)
   LIST_EMBER_TRACKER%TAIL%SIGMA_CROSSWIND_LOCAL = MAX(SIGMA_CROSSWIND_LOCAL, 1E-5)
   LIST_EMBER_TRACKER%TAIL%MU_CROSSWIND_LOCAL    = MAX(MU_CROSSWIND_LOCAL,    1E-5)
   LIST_EMBER_TRACKER%TAIL%TARGET_ARRIVED = .FALSE.
ELSE
   IF (NEMBERS .EQ. 0) RETURN
   CALL EMBER_TRAJECTORY ( &
      CC%NCOLS                  , &
      CC%NROWS                  , &
      CC%CELLSIZE               , &
      NEMBERS                   , &
      X0                        , & 
      TSTOP_SPOT                , & 
      PIGN                      , &
      PHIP                      , &
      IRANK_WORLD               , &
      MIN_SPOTTING_DISTANCE     , &
      MAX_SPOTTING_DISTANCE     , &
      SIGMA_DIST                , &
      MU_DIST                   , &
      SIGMA_CROSSWIND_LOCAL            , &
      MU_CROSSWIND_LOCAL               , &
      SPOTTING_DISTRIBUTION_TYPE, &
      N_SPOT_FIRES              , &
      IX_SPOT_FIRE              , &
      IY_SPOT_FIRE              , &
      ICASE                     , &
      TIME_NOW                  , &
      IGNMULT )
ENDIF

CONTAINS

! *****************************************************************************
FUNCTION SARDOY_PDF_PARAMETERS(WS, FI, IFBFM)
! *****************************************************************************
! FUNCTION CALCULATES THE SPOTTING DISTANCE DISTRIBUTION (HAS TO BE LOGNORMAL)
! TAKE THE INPUTS LOCAL WIND SPEED AND FIRELINE INTENSITY, RETURE MU AND SIGMA
REAL, INTENT(IN) :: WS, FI
INTEGER*2, INTENT(IN) :: IFBFM
REAL, PARAMETER :: RHO_INF = 1.1 ! Air density, kg/m^2
REAL, PARAMETER :: C_PG    = 1.0 ! Air heat capacity, kJ/kg-K
REAL, PARAMETER :: T_INF   = 300.0 ! Ambient temperature, K
REAL, PARAMETER :: G       = 9.81! Gravitional acceleration, m^2/s
REAL :: I, U_WIND, LC, FR, MU_DIST, SIGMA_DIST, MU_X, SIGMA_X, MU_CROSSWIND_LOCAL, SIGMA_CROSSWIND_LOCAL, RHO_P, D_P, Q, B_STAR
REAL, DIMENSION(4) :: SARDOY_PDF_PARAMETERS
U_WIND = 0.447 * MAX(1E-3,ABS(WS)) / 0.87 ! Wind speed in m/s, Use 10-m wind speed
I  = MAX(FI,1E-6) / 1000.0 ! Fireline intensity in MW/m

IF (IFBFM .EQ. 91) THEN
   ! Himoto's model for firebrand deposition distribution
   LC     = 10.0  ! Characteristic length scale, use 10 m for now as typical dimension of buildings
   RHO_P  = 100.0       ! Particle density, kg/m^2
   D_P    = 5E-3      ! Thickness of disk ember, m
   Q      = I*LC*1000.0 ! Heat release rate, kW

   B_STAR = U_WIND/SQRT(G*LC)*(RHO_P/RHO_INF)**(-3.0/4.0)* &
            (D_P/LC)**(-3.0/4.0)*(Q/(RHO_INF*C_PG*T_INF*G**0.5*LC**2.5))**0.5

   MU_X    = 0.47 * B_STAR**(2.0/3.0) * LC
   SIGMA_X = 0.88 * B_STAR**(1.0/3.0) * LC

   MU_X = MAX(MU_X, 1E-5)
   SIGMA_X = MAX(SIGMA_X, 1E-5)
   MU_DIST    = LOG(MU_X / SQRT((SIGMA_X/MU_X)**2.0 + 1))
   SIGMA_DIST = SQRT(LOG(1. + (SIGMA_X/MU_X)**2.0))
ELSE
   ! Sardoy's model for firebrand deposition distribution
   LC = (I*1000.0 / (RHO_INF * C_PG * T_INF * SQRT(G))) ** 0.67  ! Characteristic length scale
   FR = U_WIND / SQRT(G * LC) ! Froude number
   IF (FR .LE. 1.0) THEN
      MU_DIST    = (I ** 0.54) / MAX(U_WIND ** 0.55,1.0E-5)
      MU_DIST    = 1.47 * MU_DIST + 1.14
      SIGMA_DIST = (U_WIND ** 0.44) / MAX(I ** 0.21,1.0E-5) 
      SIGMA_DIST = 0.86 * SIGMA_DIST + 0.19
   ELSE
      MU_DIST    = I ** 0.26 * U_WIND ** 0.11
      MU_DIST    = 1.32 * MU_DIST - 0.02
      SIGMA_DIST = 1.0 / MAX(I ** 0.01,1.0E-5) / MAX(U_WIND ** 0.02,1.0E-5)
      SIGMA_DIST = 4.95 * SIGMA_DIST - 3.48
   ENDIF
ENDIF

MU_CROSSWIND_LOCAL = 0.0
SIGMA_CROSSWIND_LOCAL = 0.92 * LC
SARDOY_PDF_PARAMETERS(1) = MIN(MU_DIST,5.0)
SARDOY_PDF_PARAMETERS(2) = SIGMA_DIST
SARDOY_PDF_PARAMETERS(3) = MU_CROSSWIND_LOCAL
SARDOY_PDF_PARAMETERS(4) = SIGMA_CROSSWIND_LOCAL

! *****************************************************************************
END FUNCTION SARDOY_PDF_PARAMETERS
! *****************************************************************************

! ! *****************************************************************************
! FUNCTION EMBER_TO_EMIT_PER_CELL(WS, N0, CELLSIZE_ELM, AF, FMC, WN_FUEL, IFBFM, TAU_EMBERGEN, FLIN)
! ! *****************************************************************************
! ! Calculates spotting distance distribution based on Sardoy's model.
! ! Takes as input local in speed and fireline intensity, reutrns MU and SIGMA
! REAL, INTENT(IN) :: WS, N0, CELLSIZE_ELM, AF, FMC, WN_FUEL, TAU_EMBERGEN, FLIN
! INTEGER*2, INTENT(IN) :: IFBFM
! REAL, PARAMETER :: D_TRUNK        = 0.2 ! Trunk diameter, m
! REAL, PARAMETER :: M_FIREBRAND    = 2.0e-4 ! firebrand mass, kg
! REAL, PARAMETER :: G       = 9.81! Gravitional acceleration, m^2/s
! REAL :: M_FUEL, U_WIND, N_EMBER, Y_FIREBRAND
! REAL :: EMBER_TO_EMIT_PER_CELL

! M_FUEL = CELLSIZE_ELM*CELLSIZE_ELM*WN_FUEL ! Available vegetation fuel mass in a cell, kg
! U_WIND = WS*0.447 ! wind speed, m/s (This is 20-ft wind, to be verified)

! IF (IFBFM .EQ. 91) THEN
!     ! Lee and Davidson, 2010, ember from structure
!     ! N_EMBER = 206.66*EXP(0.1876*U_WIND)*(CELLSIZE_ELM*CELLSIZE_ELM*AF)
!     N_EMBER = FLIN * CELLSIZE_ELM * EMBER_GR_PER_MW_VEGE
! ELSE
!     ! Ju et al, 2023, ember from vegetation
!     ! Y_FIREBRAND = 1.70*MAX(FMC,1E-6)**(-0.14)*(U_WIND/SQRT(G*D_TRUNK))**0.63+0.15
!     ! N_EMBER = Y_FIREBRAND*M_FUEL/M_FIREBRAND
!     N_EMBER = FLIN * CELLSIZE_ELM * EMBER_GR_PER_MW_BLDG
! ENDIF

! ! EMBER_TO_EMIT_PER_CELL = N_EMBER/MAX(N0*TAU_EMBERGEN,1E-6)
! EMBER_TO_EMIT_PER_CELL = N_EMBER

! ! *****************************************************************************
! END FUNCTION EMBER_TO_EMIT_PER_CELL
! ! *****************************************************************************

! *****************************************************************************
FUNCTION EMBER_TO_EMIT_PER_CELL(CELLSIZE_ELM, IFBFM, FLIN, EMBER_GR_PER_MW_BLDG, EMBER_GR_PER_MW_VEGE)
! *****************************************************************************
! Calculates spotting distance distribution based on Sardoy's model.
! Takes as input local in speed and fireline intensity, reutrns MU and SIGMA
REAL, INTENT(IN) :: CELLSIZE_ELM, FLIN, EMBER_GR_PER_MW_BLDG, EMBER_GR_PER_MW_VEGE
INTEGER*2, INTENT(IN) :: IFBFM
REAL :: EMBER_TO_EMIT_PER_CELL, N_EMBER

IF (IFBFM .EQ. 91) THEN
    N_EMBER = FLIN * CELLSIZE_ELM * EMBER_GR_PER_MW_BLDG / 1000
ELSE
    ! Ju et al, 2023, ember from vegetation
    N_EMBER = FLIN * CELLSIZE_ELM * EMBER_GR_PER_MW_VEGE / 1000
ENDIF

EMBER_TO_EMIT_PER_CELL = N_EMBER

! *****************************************************************************
END FUNCTION EMBER_TO_EMIT_PER_CELL
! *****************************************************************************

! *****************************************************************************
END SUBROUTINE SPOTTING
! *****************************************************************************

! *****************************************************************************
SUBROUTINE EMBER_TRAJECTORY( &
NX_ELM                     , &
NY_ELM                     , &
CELLSIZE_ELM               , &
NUM_EMBERS                 , &
X0_ELM                     , & 
TSTOP_ELM                  , &
PIGN_ELM                   , & 
PHIP                       , &
IRANK_WORLD                , &
MIN_SPOTTING_DISTANCE      , &
MAX_SPOTTING_DISTANCE      , &
SIGMA_DIST                 , &
MU_DIST                    , &
SIGMA_CROSSWIND_LOCAL             , &
MU_CROSSWIND_LOCAL                , &
SPOTTING_DISTRIBUTION_TYPE , &
N_SPOT_FIRES               , &
IX_SPOT_FIRE               , &
IY_SPOT_FIRE               , &
ICASE                      , &
TIME_NOW                   , &
IGNMULT)
! *****************************************************************************

INTEGER, INTENT(IN) :: NX_ELM, NY_ELM, NUM_EMBERS, IRANK_WORLD, ICASE
INTEGER :: N_SPOT_FIRES, IX_SPOT_FIRE(:), IY_SPOT_FIRE(:)
REAL, INTENT(IN) :: CELLSIZE_ELM, PIGN_ELM, MIN_SPOTTING_DISTANCE, MAX_SPOTTING_DISTANCE, &
                    SIGMA_DIST, MU_DIST, SIGMA_CROSSWIND_LOCAL, MU_CROSSWIND_LOCAL, TIME_NOW, IGNMULT
REAL, INTENT(IN) :: PHIP(:,:)

CHARACTER(60), INTENT(IN) :: SPOTTING_DISTRIBUTION_TYPE
REAL :: F_WIND, SPOTTING_DISTANCE, DIST, EPS, CROSSWIND_DEVIATION, UWIND_ABS, INV_UWIND_TIMES_CROSSWIND_DEVIATION

!These also come from elmfire but have local analogs:
REAL, INTENT(IN) :: X0_ELM(:), TSTOP_ELM
CHARACTER(3) :: THREE
CHARACTER(400) :: FNOUT
REAL :: R0, IGNPROB, WD1TO, WD2TO, WDTO, WS20, T, TSTOP, DT, HIGH, LOW, X_MAX, P_IGNITION, LNORM_QUANTILE, QUANTILE
REAL, DIMENSION(3) :: X, X0, UWIND, OFFSET
REAL, POINTER, DIMENSION(:,:), SAVE :: WS20_LO, WS20_HI, WD20_LO, WD20_HI
INTEGER :: IEMBER, I, J, I1, I2, J1, J2, IX, IY, IXLAST, IYLAST, ICOL, IROW, ICOUNT, K_MAX, ITLO_METEOROLOGY, ITHI_METEOROLOGY, IFBFM, IBLDGFM
LOGICAL :: GO
REAL, PARAMETER :: SQRT_2 = 1.4142135623731

X0   (:) = X0_ELM(:)               ! Initial position vector
TSTOP    = TSTOP_ELM               ! Stop time 

WRITE(THREE, '(I3.3)') IRANK_WORLD
FNOUT = 'ignitions_' // THREE // '.csv'

UWIND (3) = 0. 
OFFSET(3) = 0.

! Find the maximum spotting distance accoring to criterion P_EPS (P_EPS=0.001 by default)
IF (USE_UMD_SPOTTING_MODEL) THEN
   QUANTILE=SQRT_2*ERFINV_LOCAL(2.0*P_EPS-1.0)
   LNORM_QUANTILE = EXP(MU_DIST + SIGMA_DIST * QUANTILE)
   K_MAX = NINT(LNORM_QUANTILE/CELLSIZE_ELM)
   X_MAX = K_MAX*CELLSIZE_ELM
ENDIF

DO IEMBER = 1, NUM_EMBERS

   CALL RANDOM_NUMBER(R0); EPS = 8.*(R0 - 0.5)

! Get spotting distance
   CALL RANDOM_NUMBER(R0)
   IF (TRIM(SPOTTING_DISTRIBUTION_TYPE) .EQ. 'UNIFORM') THEN   
      SPOTTING_DISTANCE = MIN_SPOTTING_DISTANCE + R0 * (MAX_SPOTTING_DISTANCE - MIN_SPOTTING_DISTANCE)
   ELSE IF (USE_UMD_SPOTTING_MODEL) THEN
      LOW  = SARDOY_PDFINV(0.0, MU_DIST, SIGMA_DIST)
      HIGH = SARDOY_PDFINV(X_MAX+CELLSIZE_ELM*0.5, MU_DIST, SIGMA_DIST)
      R0   = R0 * (HIGH - LOW) + LOW
      SPOTTING_DISTANCE = EXP(SQRT(2.) * SIGMA_DIST * ERFINV_LOCAL(2.*R0-1.) + MU_DIST)
      SPOTTING_DISTANCE = NINT(SPOTTING_DISTANCE/CELLSIZE_ELM)*CELLSIZE_ELM
   ELSE !Lognormal
      IF (R0 .GT. 0.5) THEN
         SPOTTING_DISTANCE = EXP(SQRT(2.) * SIGMA_DIST * ERFINV(2.*R0-1.) + MU_DIST)
      ELSE
         SPOTTING_DISTANCE = EXP(MU_DIST - SQRT(2.) * SIGMA_DIST * ERFINV_LOCAL(1.-2.*R0))
      ENDIF      
   ENDIF

   DIST = 0.

   OFFSET(1:2) = X0(1:2)
   X   (:)   = X0(:) - OFFSET(:)
   T         = 0.
  
   IXLAST = 0
   IYLAST = 0

   IX = CEILING ((X(1) + OFFSET(1)) / CELLSIZE_ELM)
   IX = MAX(IX,1) ; IX = MIN (IX,NX_ELM)
   ICOL = ICOL_ANALYSIS_F2C(IX)

   IY = CEILING ((X(2) + OFFSET(2)) / CELLSIZE_ELM)
   IY = MAX(IY,1) ; IY = MIN (IY,NY_ELM)
   IROW = IROW_ANALYSIS_F2C(IY)

   ITLO_METEOROLOGY = MAX(1 + FLOOR((T+TIME_NOW) / DT_METEOROLOGY),1)
   ITLO_METEOROLOGY = MIN(ITLO_METEOROLOGY, NUM_METEOROLOGY_TIMES)
   ITHI_METEOROLOGY = MIN(ITLO_METEOROLOGY + 1, NUM_METEOROLOGY_TIMES)
   F_WIND = (T + TIME_NOW - REAL(ITLO_METEOROLOGY-1) * DT_METEOROLOGY) / DT_METEOROLOGY
   IF (ITLO_METEOROLOGY .EQ. ITHI_METEOROLOGY) F_WIND = 1.

   WS20_LO => WSP   (:,:,ITLO_METEOROLOGY)
   WS20_HI => WSP   (:,:,ITHI_METEOROLOGY)
   WD20_LO => WDP   (:,:,ITLO_METEOROLOGY)
   WD20_HI => WDP   (:,:,ITHI_METEOROLOGY)
   
   WS20 = WS20_LO(ICOL,IROW) * (1. - F_WIND) + F_WIND * WS20_HI(ICOL,IROW) 
   WS20 = 0.447 * WS20

   DT =  CELLSIZE_ELM / MAX(WS20, 0.01) 

   IF (USE_UMD_SPOTTING_MODEL) THEN
      NUM_TRACKED_EMBERS = MIN (NUM_TRACKED_EMBERS + 1, EMBER_TRACKER_SIZE)
      SPOTTING_STATS(NUM_TRACKED_EMBERS)%X_FROM            = X(1) + OFFSET(1)
      SPOTTING_STATS(NUM_TRACKED_EMBERS)%Y_FROM            = X(2) + OFFSET(2)
      SPOTTING_STATS(NUM_TRACKED_EMBERS)%IX_FROM           = IX
      SPOTTING_STATS(NUM_TRACKED_EMBERS)%IY_FROM           = IY
      SPOTTING_STATS(NUM_TRACKED_EMBERS)%X_TO              = -9E9
      SPOTTING_STATS(NUM_TRACKED_EMBERS)%Y_TO              = -9E9
      SPOTTING_STATS(NUM_TRACKED_EMBERS)%IX_TO             = -9999
      SPOTTING_STATS(NUM_TRACKED_EMBERS)%IY_TO             = -9999
      SPOTTING_STATS(NUM_TRACKED_EMBERS)%DIST              = -9999
      SPOTTING_STATS(NUM_TRACKED_EMBERS)%TTRAVEL           = -9E9
      SPOTTING_STATS(NUM_TRACKED_EMBERS)%TLAUNCH           = TIME_NOW
      SPOTTING_STATS(NUM_TRACKED_EMBERS)%TIGN              = -9E9
      SPOTTING_STATS(NUM_TRACKED_EMBERS)%POSITIVE_IGNITION = .FALSE.
      SPOTTING_STATS(NUM_TRACKED_EMBERS)%ALREADY_IGNITED   = .FALSE.
   ENDIF

   ICOUNT = 0

   DO WHILE (T .LT. TSTOP .AND. DIST .LT. SPOTTING_DISTANCE )
      ICOUNT = ICOUNT + 1
      T = T + DT
      IX = CEILING ((X(1) + OFFSET(1)) / CELLSIZE_ELM)
      IX = MAX(IX,1) ; IX = MIN (IX,NX_ELM)
      ICOL = ICOL_ANALYSIS_F2C(IX)

      IY = CEILING ((X(2) + OFFSET(2)) / CELLSIZE_ELM)
      IY = MAX(IY,1) ; IY = MIN (IY,NY_ELM)
      IROW = IROW_ANALYSIS_F2C(IY)

      IF (IX .NE. IXLAST .OR. IY .NE. IYLAST) THEN
         IF (IX .GE. NX_ELM .OR. IX .LE. 1) THEN
            T = 9E9; CYCLE
         ENDIF
         IF (IY .GE. NY_ELM .OR. IY .LE. 1) THEN
            T = 9E9; CYCLE
         ENDIF

         ITLO_METEOROLOGY = MAX(1 + FLOOR((T+TIME_NOW) / DT_METEOROLOGY),1)
         ITLO_METEOROLOGY = MIN(ITLO_METEOROLOGY, NUM_METEOROLOGY_TIMES)
         ITHI_METEOROLOGY = MIN(ITLO_METEOROLOGY + 1, NUM_METEOROLOGY_TIMES)
         F_WIND = (T + TIME_NOW - REAL(ITLO_METEOROLOGY-1) * DT_METEOROLOGY) / DT_METEOROLOGY
         IF (ITLO_METEOROLOGY .EQ. ITHI_METEOROLOGY) F_WIND = 1.

         WS20_LO => WSP   (:,:,ITLO_METEOROLOGY)
         WS20_HI => WSP   (:,:,ITHI_METEOROLOGY)
         WD20_LO => WDP   (:,:,ITLO_METEOROLOGY)
         WD20_HI => WDP   (:,:,ITHI_METEOROLOGY)

         WS20 = WS20_LO(ICOL,IROW) * (1. - F_WIND) + F_WIND * WS20_HI(ICOL,IROW) 
         WS20 = 0.447 * WS20

         DT = MIN ( 0.5 * CELLSIZE_ELM / MAX (WS20, 0.01), 5.0)

         WD1TO = WD20_LO(ICOL,IROW) + 180. ; IF (WD1TO .GT. 360) WD1TO = WD1TO - 360.
         WD2TO = WD20_HI(ICOL,IROW) + 180. ; IF (WD2TO .GT. 360) WD2TO = WD2TO - 360.

         WDTO  = WD1TO + F_WIND * (WD2TO - WD1TO)
         WDTO  = WDTO + EPS
         IF (WDTO .GT. 360.) WDTO = WDTO - 360.
         IF (WDTO .LT.   0.) WDTO = WDTO + 360.

         UWIND(1) = WS20 * SIN(WDTO*PI/180.)
         UWIND(2) = WS20 * COS(WDTO*PI/180.)
      ENDIF

      IF (ABS(UWIND(1)) .LT. 1E-6 .AND. ABS(UWIND(2)) .LT. 1E-6) T=9E9
      IF (ICOUNT .GT. 100000) T=9E9

      X(1:2)   = X(1:2) + UWIND(1:2) * DT
      DIST     = DIST + WS20 * DT
      
      IXLAST = IX
      IYLAST = IY
   ENDDO

   IF (T .LT. 1E9) THEN
      ! Add crosswind direction distribution
      IF (USE_UMD_SPOTTING_MODEL) THEN
         CALL RANDOM_NUMBER(R0)
         CROSSWIND_DEVIATION = SQRT_2 * ERFINV_LOCAL(2.0*R0-1.0) * SIGMA_CROSSWIND_LOCAL + MU_CROSSWIND_LOCAL

         UWIND_ABS = NORM2(UWIND(1:2))
         INV_UWIND_TIMES_CROSSWIND_DEVIATION = CROSSWIND_DEVIATION/MAX(1E-6,UWIND_ABS)
         X(1)=X(1)+(COS(90*PI/180.)*UWIND(1)-SIN(90*PI/180.)*UWIND(2))*INV_UWIND_TIMES_CROSSWIND_DEVIATION
         X(2)=X(2)+(SIN(90*PI/180.)*UWIND(1)+COS(90*PI/180.)*UWIND(2))*INV_UWIND_TIMES_CROSSWIND_DEVIATION      
      ENDIF

      IX = CEILING ((X(1) + OFFSET(1)) / CELLSIZE_ELM) ; IX = MAX(IX,1) ; IX = MIN (IX,NX_ELM)
      IY = CEILING ((X(2) + OFFSET(2)) / CELLSIZE_ELM) ; IY = MAX(IY,1) ; IY = MIN (IY,NY_ELM)
      IF (DUMP_EMBER_FLUX) EMBER_FLUX%R4(IX,IY,1) = EMBER_FLUX%R4(IX,IY,1) + EMBER_SAMPLING_FACTOR
      STATS_NEMBERS(ICASE) = STATS_NEMBERS(ICASE) + 1.
      IF (USE_EMBER_COUNT_BINS) EMBER_COUNT(IX,IY) = EMBER_COUNT(IX,IY) + INT(IX, KIND=KIND(EMBER_COUNT(1,1))) ! To avoid implicit conversion warning

      IF (USE_UMD_SPOTTING_MODEL) THEN
         SPOTTING_STATS(NUM_TRACKED_EMBERS)%X_TO    = X(1) + OFFSET(1)
         SPOTTING_STATS(NUM_TRACKED_EMBERS)%Y_TO    = X(2) + OFFSET(2)
         SPOTTING_STATS(NUM_TRACKED_EMBERS)%IX_TO   = IX
         SPOTTING_STATS(NUM_TRACKED_EMBERS)%IY_TO   = IY
         SPOTTING_STATS(NUM_TRACKED_EMBERS)%DIST    = DIST
         SPOTTING_STATS(NUM_TRACKED_EMBERS)%TTRAVEL = T
         SPOTTING_STATS(NUM_TRACKED_EMBERS)%TIGN    = T+TIME_NOW
      ENDIF

      ! Following modification per UCB's request to apply building model-sensitive ignition probability by Yiren
      IFBFM = FBFM%I2(IX,IY,1)
      IF(IFBFM .NE. 91) THEN
         P_IGNITION = PIGN_ELM
      ELSE
         IF(USE_BLDG_SPREAD_MODEL .AND. BLDG_SPREAD_MODEL_TYPE .EQ. 2) THEN
            IBLDGFM = BLDG_FUEL_MODEL%I2(IX,IY,1)
            P_IGNITION = BUILDING_FUEL_MODEL_TABLE(IBLDGFM)%P_IGNITION
         ELSE
            P_IGNITION = PIGN_ELM
         ENDIF
      ENDIF
      IGNPROB=0.01*P_IGNITION*IGNMULT

      CALL RANDOM_NUMBER(R0)

      IF (IGNPROB .GT. R0 .AND. DIST .GT. 0.5*CELLSIZE_ELM) THEN
         
         GO = .TRUE. 
         IF (.NOT. USE_UMD_SPOTTING_MODEL) THEN
            I1 = MAX(IX-3,1     )
            I2 = MIN(IX+3,NX_ELM)
            J1 = MAX(IY-3,1     )
            J2 = MIN(IY+3,NY_ELM)
            
            DO J = J1, J2
            DO I = I1, I2
               IF (PHIP(I,J) .LT. 0.) GO = .FALSE.
            ENDDO
            ENDDO
         ENDIF

         IF (GO) THEN
            IF (USE_UMD_SPOTTING_MODEL) THEN
               SPOTTING_STATS(NUM_TRACKED_EMBERS)%POSITIVE_IGNITION = .TRUE.
            ELSE
               N_SPOT_FIRES = N_SPOT_FIRES + 1
               IX_SPOT_FIRE(N_SPOT_FIRES) = IX
               IY_SPOT_FIRE(N_SPOT_FIRES) = IY
            ENDIF
         ENDIF
      ENDIF

   ENDIF

ENDDO

CONTAINS

! *****************************************************************************
REAL FUNCTION SARDOY_PDFINV(X, MU_DIST, SIGMA_DIST)
! *****************************************************************************

REAL, INTENT(IN) :: X, MU_DIST, SIGMA_DIST
REAL, PARAMETER :: SQRT_2 = 1.4142135623731

SARDOY_PDFINV = 0.5*(1+ERF((LOG(MAX(X,1E-6))-MU_DIST)/SQRT_2/SIGMA_DIST))

! *****************************************************************************
END FUNCTION SARDOY_PDFINV
! *****************************************************************************

! *****************************************************************************
END SUBROUTINE EMBER_TRAJECTORY
! *****************************************************************************

! *****************************************************************************
SUBROUTINE EMBER_TRAJECTORY_EULERIAN( &
NX_ELM                     , &
NY_ELM                     , &
CELLSIZE_ELM               , &
C                          , &
T_ELMFIRE                  , &
DT_ELMFIRE)
! *****************************************************************************

INTEGER, INTENT(IN) :: NX_ELM, NY_ELM
REAL, INTENT(IN) :: CELLSIZE_ELM, T_ELMFIRE, DT_ELMFIRE
TYPE (NODE), POINTER, INTENT(INOUT) :: C

! REAL :: EPS

!These also come from elmfire but have local analogs:

REAL :: WD1TO, WD2TO, WDTO, WS20, T, DT, DIST_LAST, F_WIND, X_MAX, Y_MAX, P_LAND, P_LAND_CROSSWIND, LNORM_QUANTILE, &
        NORM_QUANTILE, QUANTILE, NORM_FACTOR_CROSSWIND, NORM_FACTOR, X_CROSSWIND, Y_CROSSWIND, INV_UWIND_TIMES_CROSSWIND_DEVIATION, &
        UWIND_ABS, NUM_EMBERS, SIGMA_DIST, MU_DIST, SIGMA_CROSSWIND_LOCAL, MU_CROSSWIND_LOCAL
REAL, DIMENSION(3) :: X, X0, UWIND, OFFSET
INTEGER :: IX, IY, IXLAST, IYLAST,ICOL, IROW, ICOUNT, K_MAX, K_MAX_CROSSWIND, ITLO_METEOROLOGY, ITHI_METEOROLOGY, I, &
           IX_CROSSWIND, IY_CROSSWIND
REAL, POINTER, DIMENSION(:,:), SAVE :: WS20_LO_SPOTTING, WS20_HI_SPOTTING, WD20_LO_SPOTTING, WD20_HI_SPOTTING
REAL, PARAMETER :: SQRT_2 = 1.4142135623731

NUM_EMBERS = C%NUM_EMBERS
X0(:)      = C%X0_ELM(:)
SIGMA_DIST = C%SIGMA_DIST
MU_DIST    = C%MU_DIST
SIGMA_CROSSWIND_LOCAL = C%SIGMA_CROSSWIND_LOCAL
MU_CROSSWIND_LOCAL    = C%MU_CROSSWIND_LOCAL

UWIND (3) = 0. 
OFFSET(3) = 0.

! Find the maximum spotting distance accoring to criterion P_EPS (P_EPS=0.001 by default)
QUANTILE=SQRT_2*ERFINV_LOCAL(2.0*(1.0-P_EPS)-1.0)
LNORM_QUANTILE = EXP(MU_DIST + SIGMA_DIST * QUANTILE)
K_MAX = NINT(LNORM_QUANTILE/CELLSIZE_ELM)
X_MAX = K_MAX*CELLSIZE_ELM

IF(USE_CROSSWIND_DISTRIBUTION) THEN
   NORM_QUANTILE  = MU_CROSSWIND_LOCAL + SIGMA_CROSSWIND_LOCAL * QUANTILE
   K_MAX_CROSSWIND = CEILING(NORM_QUANTILE/CELLSIZE_ELM)
   K_MAX_CROSSWIND = MAX(K_MAX_CROSSWIND, 1)
ELSE
   K_MAX_CROSSWIND = 0
ENDIF
Y_MAX = K_MAX_CROSSWIND*CELLSIZE_ELM

IF (C%DIST .GE. X_MAX) THEN
   C%TARGET_ARRIVED = .TRUE.
   RETURN
ENDIF

NORM_FACTOR = SARDOY_CDF(1E-6,REAL(K_MAX)*CELLSIZE_ELM, MU_DIST, SIGMA_DIST)

IF (K_MAX_CROSSWIND .GT. 1) THEN
   NORM_FACTOR_CROSSWIND = 0.5*(ERF((Y_MAX+CELLSIZE_ELM*0.5-MU_CROSSWIND_LOCAL)/SQRT_2/SIGMA_CROSSWIND_LOCAL)- &
                               ERF((-Y_MAX-CELLSIZE_ELM*0.5-MU_CROSSWIND_LOCAL)/SQRT_2/SIGMA_CROSSWIND_LOCAL))
ELSE
   NORM_FACTOR_CROSSWIND = 1.0
ENDIF

DIST_LAST = C%DIST

OFFSET(1:2) = X0(1:2)
X   (:)   = X0(:) - OFFSET(:)
T         = 0.
IF(C%TIME_ACTUAL .LT. 0) C%TIME_ACTUAL=T_ELMFIRE
IF(C%TIME_ACTUAL .GE. T_ELMFIRE+DT_ELMFIRE) RETURN

IXLAST = 0
IYLAST = 0

IX = CEILING ((X(1) + OFFSET(1)) / CELLSIZE_ELM)
IX = MAX(IX,1) ; IX = MIN (IX,NX_ELM)
ICOL = ICOL_ANALYSIS_F2C(IX)

IY = CEILING ((X(2) + OFFSET(2)) / CELLSIZE_ELM)
IY = MAX(IY,1) ; IY = MIN (IY,NY_ELM)
IROW = IROW_ANALYSIS_F2C(IY)

ITLO_METEOROLOGY = MAX(1 + FLOOR((C%TIME_ACTUAL) / DT_METEOROLOGY),1)
ITLO_METEOROLOGY = MIN(ITLO_METEOROLOGY, NUM_METEOROLOGY_TIMES)
ITHI_METEOROLOGY = MIN(ITLO_METEOROLOGY + 1, NUM_METEOROLOGY_TIMES)
F_WIND = (C%TIME_ACTUAL  - REAL(ITLO_METEOROLOGY-1) * DT_METEOROLOGY) / DT_METEOROLOGY
IF (ITLO_METEOROLOGY .EQ. ITHI_METEOROLOGY) F_WIND = 1.

WS20_LO_SPOTTING => WSP   (:,:,ITLO_METEOROLOGY)
WS20_HI_SPOTTING => WSP   (:,:,ITHI_METEOROLOGY)

WS20 = WS20_LO_SPOTTING(ICOL,IROW) * (1. - F_WIND) + F_WIND * WS20_HI_SPOTTING(ICOL,IROW) 
WS20 = 0.447 * WS20

DT =  CELLSIZE_ELM / MAX(WS20, 0.01) 
! DT = MIN(DT, DT_ELMFIRE)

ICOUNT = 0

! CALL RANDOM_NUMBER(R0); EPS = 8.*(R0 - 0.5)

DO WHILE (T .LT. DT_ELMFIRE)

   IF (C%DIST .GE. X_MAX) THEN
      C%TARGET_ARRIVED = .TRUE.
      EXIT
   ENDIF 

   ICOUNT = ICOUNT + 1
   
   T = T + DT

   IX = CEILING ((X(1) + OFFSET(1)) / CELLSIZE_ELM)
   IX = MAX(IX,1) ; IX = MIN (IX,NX_ELM)
   ICOL = ICOL_ANALYSIS_F2C(IX)

   IY = CEILING ((X(2) + OFFSET(2)) / CELLSIZE_ELM)
   IY = MAX(IY,1) ; IY = MIN (IY,NY_ELM)
   IROW = IROW_ANALYSIS_F2C(IY)

   IF (IX .NE. IXLAST .OR. IY .NE. IYLAST) THEN
      IF (IX .GE. NX_ELM .OR. IX .LE. 1) THEN
         T = 9E9; CYCLE
      ENDIF
      IF (IY .GE. NY_ELM .OR. IY .LE. 1) THEN
         T = 9E9; CYCLE
      ENDIF

      ITLO_METEOROLOGY = MAX(1 + FLOOR((C%TIME_ACTUAL) / DT_METEOROLOGY),1)
      ITLO_METEOROLOGY = MIN(ITLO_METEOROLOGY, NUM_METEOROLOGY_TIMES)
      ITHI_METEOROLOGY = MIN(ITLO_METEOROLOGY + 1, NUM_METEOROLOGY_TIMES)
      F_WIND = (C%TIME_ACTUAL - REAL(ITLO_METEOROLOGY-1) * DT_METEOROLOGY) / DT_METEOROLOGY
      IF (ITLO_METEOROLOGY .EQ. ITHI_METEOROLOGY) F_WIND = 1.

      WS20_LO_SPOTTING => WSP   (:,:,ITLO_METEOROLOGY)
      WS20_HI_SPOTTING => WSP   (:,:,ITHI_METEOROLOGY)
      WD20_LO_SPOTTING => WDP   (:,:,ITLO_METEOROLOGY)
      WD20_HI_SPOTTING => WDP   (:,:,ITHI_METEOROLOGY)

      WS20 = WS20_LO_SPOTTING(ICOL,IROW) * (1. - F_WIND) + F_WIND * WS20_HI_SPOTTING(ICOL,IROW) 
      WS20 = 0.447 * WS20

      DT =  CELLSIZE_ELM / MAX(WS20, 0.01) 
      ! DT = MIN(DT, DT_ELMFIRE)

      WD1TO = WD20_LO_SPOTTING(ICOL,IROW) + 180. ; IF (WD1TO .GT. 360) WD1TO = WD1TO - 360.
      WD2TO = WD20_HI_SPOTTING(ICOL,IROW) + 180. ; IF (WD2TO .GT. 360) WD2TO = WD2TO - 360.

      WDTO  = WD1TO + F_WIND * (WD2TO - WD1TO)
      WDTO  = WDTO ! Removed artificial fluctuation
      IF (WDTO .GT. 360.) WDTO = WDTO - 360.
      IF (WDTO .LT.   0.) WDTO = WDTO + 360.

      UWIND(1) = WS20 * SIN(WDTO*PI/180.)
      UWIND(2) = WS20 * COS(WDTO*PI/180.)
   ENDIF

   IF (ABS(UWIND(1)) .LT. 1E-6 .AND. ABS(UWIND(2)) .LT. 1E-6) T=9E9
   IF (ICOUNT .GT. 100000) T=9E9
   
   X(1:2)   = X(1:2) + UWIND(1:2) * DT
   C%DIST   = C%DIST + WS20 * DT
   C%X0_ELM(:) = X(:)+OFFSET(:)
   C%TIME_ACTUAL = C%TIME_ACTUAL+DT

   IXLAST = IX
   IYLAST = IY

   ! Filling entries for ember flux table, which will be used as input for the ignition model
   IX = CEILING ((X(1) + OFFSET(1)) / CELLSIZE_ELM)
   IX = MAX(IX,1) ; IX = MIN (IX,NX_ELM)

   IY = CEILING ((X(2) + OFFSET(2)) / CELLSIZE_ELM)
   IY = MAX(IY,1) ; IY = MIN (IY,NY_ELM)

   P_LAND = SARDOY_CDF(DIST_LAST,C%DIST, MU_DIST, SIGMA_DIST)/MAX(NORM_FACTOR,1E-6)

   UWIND_ABS = NORM2(UWIND(1:2))

   IF (K_MAX_CROSSWIND .GT. 1 .AND. USE_CROSSWIND_DISTRIBUTION) THEN
      P_LAND_CROSSWIND = 0.5*(ERF((CELLSIZE_ELM*0.5-MU_CROSSWIND_LOCAL)/SQRT_2/SIGMA_CROSSWIND_LOCAL)- &
                             ERF((-CELLSIZE_ELM*0.5-MU_CROSSWIND_LOCAL)/SQRT_2/SIGMA_CROSSWIND_LOCAL))/NORM_FACTOR_CROSSWIND
      IF (DUMP_EMBER_FLUX_TRANSIENT) EMBER_FLUX_TRANSIENT%R4(IX,IY, 1) = EMBER_FLUX_TRANSIENT%R4(IX,IY, 1) + NUM_EMBERS*P_LAND*P_LAND_CROSSWIND
      EMBER_FLUX%R4(IX,IY, 1) = EMBER_FLUX%R4(IX,IY, 1) + NUM_EMBERS*P_LAND*P_LAND_CROSSWIND

      IF(EMBER_TIGN(IX,IY) .LT. 0.0) THEN
         EMBER_TIGN(IX,IY) = C%TIME_ACTUAL
         CALL APPEND(LIST_EMBER_DEPOSITED, IX, IY, EMBER_TIGN(IX,IY))
      ELSE
         IF(EMBER_TIGN(IX,IY) .GT. C%TIME_ACTUAL) EMBER_TIGN(IX,IY) = C%TIME_ACTUAL
      ENDIF
      DO I=2, (K_MAX_CROSSWIND+1)
         P_LAND_CROSSWIND = 0.5*(ERF((CELLSIZE_ELM*(I-1+0.5)-MU_CROSSWIND_LOCAL)/SQRT_2/SIGMA_CROSSWIND_LOCAL)- &
                                ERF((CELLSIZE_ELM*(I-2+0.5)-MU_CROSSWIND_LOCAL)/SQRT_2/SIGMA_CROSSWIND_LOCAL))/NORM_FACTOR_CROSSWIND
         ! Side-1
         INV_UWIND_TIMES_CROSSWIND_DEVIATION = CELLSIZE_ELM*(I-1)/MAX(1E-6,UWIND_ABS)
         X_CROSSWIND=X(1)+(COS(90*PI/180.)*UWIND(1)-SIN(90*PI/180.)*UWIND(2))*INV_UWIND_TIMES_CROSSWIND_DEVIATION
         Y_CROSSWIND=X(2)+(SIN(90*PI/180.)*UWIND(1)+COS(90*PI/180.)*UWIND(2))*INV_UWIND_TIMES_CROSSWIND_DEVIATION

         ! These lines ensures a symetric distribution on the map in ideal conditions
         IX_CROSSWIND = CEILING ((X_CROSSWIND + OFFSET(1)) / CELLSIZE_ELM)
         IX_CROSSWIND = MAX(IX_CROSSWIND,1) ; IX_CROSSWIND = MIN (IX_CROSSWIND,NX_ELM)

         IY_CROSSWIND = CEILING ((Y_CROSSWIND + OFFSET(2)) / CELLSIZE_ELM)
         IY_CROSSWIND = MAX(IY_CROSSWIND,1) ; IY_CROSSWIND = MIN (IY_CROSSWIND,NY_ELM)

         IF (DUMP_EMBER_FLUX_TRANSIENT) EMBER_FLUX_TRANSIENT%R4(IX_CROSSWIND,IY_CROSSWIND, 1) = EMBER_FLUX_TRANSIENT%R4(IX_CROSSWIND,IY_CROSSWIND, 1) + NUM_EMBERS*P_LAND*P_LAND_CROSSWIND
         EMBER_FLUX%R4(IX_CROSSWIND,IY_CROSSWIND, 1) = EMBER_FLUX%R4(IX_CROSSWIND,IY_CROSSWIND, 1) + NUM_EMBERS*P_LAND*P_LAND_CROSSWIND

         IF(EMBER_TIGN(IX_CROSSWIND,IY_CROSSWIND) .LT. 0.0) THEN
            EMBER_TIGN(IX_CROSSWIND,IY_CROSSWIND) = C%TIME_ACTUAL
            CALL APPEND(LIST_EMBER_DEPOSITED, IX_CROSSWIND,IY_CROSSWIND, EMBER_TIGN(IX_CROSSWIND,IY_CROSSWIND))
         ELSE
            IF(EMBER_TIGN(IX_CROSSWIND,IY_CROSSWIND) .GT. C%TIME_ACTUAL) EMBER_TIGN(IX_CROSSWIND,IY_CROSSWIND) = C%TIME_ACTUAL
         ENDIF
         ! Side-2
         INV_UWIND_TIMES_CROSSWIND_DEVIATION = -CELLSIZE_ELM*(I-1)/MAX(1E-6,UWIND_ABS)
         X_CROSSWIND=X(1)+(COS(90*PI/180.)*UWIND(1)-SIN(90*PI/180.)*UWIND(2))*INV_UWIND_TIMES_CROSSWIND_DEVIATION
         Y_CROSSWIND=X(2)+(SIN(90*PI/180.)*UWIND(1)+COS(90*PI/180.)*UWIND(2))*INV_UWIND_TIMES_CROSSWIND_DEVIATION

         IX_CROSSWIND = CEILING ((X_CROSSWIND + OFFSET(1)) / CELLSIZE_ELM)
         IX_CROSSWIND = MAX(IX_CROSSWIND,1) ; IX_CROSSWIND = MIN (IX_CROSSWIND,NX_ELM)
         
         IY_CROSSWIND = CEILING ((Y_CROSSWIND + OFFSET(2)) / CELLSIZE_ELM)
         IY_CROSSWIND = MAX(IY_CROSSWIND,1) ; IY_CROSSWIND = MIN (IY_CROSSWIND,NY_ELM)

         IF (DUMP_EMBER_FLUX_TRANSIENT) EMBER_FLUX_TRANSIENT%R4(IX_CROSSWIND,IY_CROSSWIND, 1) = EMBER_FLUX_TRANSIENT%R4(IX_CROSSWIND,IY_CROSSWIND, 1) + NUM_EMBERS*P_LAND*P_LAND_CROSSWIND
         EMBER_FLUX%R4(IX_CROSSWIND,IY_CROSSWIND, 1) = EMBER_FLUX%R4(IX_CROSSWIND,IY_CROSSWIND, 1) + NUM_EMBERS*P_LAND*P_LAND_CROSSWIND
         
         IF(EMBER_TIGN(IX_CROSSWIND,IY_CROSSWIND) .LT. 0.0) THEN
            EMBER_TIGN(IX_CROSSWIND,IY_CROSSWIND) = C%TIME_ACTUAL
            CALL APPEND(LIST_EMBER_DEPOSITED, IX_CROSSWIND,IY_CROSSWIND, EMBER_TIGN(IX_CROSSWIND,IY_CROSSWIND))
         ELSE
            IF(EMBER_TIGN(IX_CROSSWIND,IY_CROSSWIND) .GT. C%TIME_ACTUAL) EMBER_TIGN(IX_CROSSWIND,IY_CROSSWIND) = C%TIME_ACTUAL
         ENDIF
      ENDDO
   ELSE
      IF (DUMP_EMBER_FLUX_TRANSIENT) EMBER_FLUX_TRANSIENT%R4(IX,IY, 1) = EMBER_FLUX_TRANSIENT%R4(IX,IY, 1) + NUM_EMBERS*P_LAND
      EMBER_FLUX%R4(IX,IY, 1) = EMBER_FLUX%R4(IX,IY, 1) + NUM_EMBERS*P_LAND

      IF(EMBER_TIGN(IX,IY) .LT. 0.0) THEN
         EMBER_TIGN(IX,IY) = C%TIME_ACTUAL
         CALL APPEND(LIST_EMBER_DEPOSITED, IX, IY, EMBER_TIGN(IX,IY))
      ELSE
         IF(EMBER_TIGN(IX,IY) .GT. C%TIME_ACTUAL) EMBER_TIGN(IX,IY) = C%TIME_ACTUAL
      ENDIF
   ENDIF

   DIST_LAST = C%DIST
   IF (C%TIME_ACTUAL .GT. T_ELMFIRE+DT_ELMFIRE) EXIT
ENDDO

CONTAINS

! *****************************************************************************
REAL FUNCTION SARDOY_CDF(X_START, X_END, MU_DIST, SIGMA_DIST)
! *****************************************************************************

REAL, INTENT(IN) :: X_START, X_END,  MU_DIST, SIGMA_DIST
REAL, PARAMETER :: SQRT_2 = 1.4142135623731

SARDOY_CDF = 0.5*(1+ERF((LOG(MAX(X_END,1E-6))-MU_DIST)/SQRT_2/SIGMA_DIST)) - &
             0.5*(1+ERF((LOG(MAX(X_START,1E-6))-MU_DIST)/SQRT_2/SIGMA_DIST))

! *****************************************************************************
END FUNCTION SARDOY_CDF
! *****************************************************************************

! *****************************************************************************
END SUBROUTINE EMBER_TRAJECTORY_EULERIAN
! *****************************************************************************

! *****************************************************************************
SUBROUTINE CLEAR_USED_EMBER(T_ELMFIRE)
! *****************************************************************************
! Subroutine to delete used particles, save space for Lagrangian scheme
USE ELMFIRE_VARS

REAL, INTENT(IN) :: T_ELMFIRE

INTEGER :: I, NUM_UNUSED_EMBERS
TYPE(SPOTTING_TRACKER), ALLOCATABLE, DIMENSION(:) :: SPOTTING_STATS_TEMP

ALLOCATE(SPOTTING_STATS_TEMP(SIZE(SPOTTING_STATS)))
SPOTTING_STATS_TEMP = SPOTTING_STATS

NUM_UNUSED_EMBERS=0

DO I=1,NUM_TRACKED_EMBERS
   IF (SPOTTING_STATS(I)%TIGN .GE. T_ELMFIRE) THEN
      NUM_UNUSED_EMBERS=NUM_UNUSED_EMBERS+1
      SPOTTING_STATS_TEMP(NUM_UNUSED_EMBERS) = SPOTTING_STATS(I)
   ENDIF
ENDDO
NUM_TRACKED_EMBERS =  NUM_UNUSED_EMBERS
SPOTTING_STATS = SPOTTING_STATS_TEMP
! *****************************************************************************
END SUBROUTINE CLEAR_USED_EMBER
! *****************************************************************************

! *****************************************************************************
SUBROUTINE EMBER_IGNITION(C, T_ELMFIRE, DT_ELMFIRE, UWIND)
! *****************************************************************************
! Firebrand ignition model, based on the ember accumulation history
USE ELMFIRE_VARS

TYPE (NODE), POINTER, INTENT(INOUT) :: C

REAL, INTENT(IN) :: T_ELMFIRE, DT_ELMFIRE, UWIND

REAL :: NUM_ACCUMULATED_EMBERS_PUA, V_AIR, P_IGN, TAU_IGN, T_DEVELOP, &
        COEF_WIND, PSI, M_EMBER, IGNITION_CRITERION, P_N, R0, HFT, F, HARDENING_FACTOR
INTEGER :: IFBFM, IBLDGFM, IX, IY
INTEGER, PARAMETER :: NO_DATA = -9999

! Lines added to utilize the tabulated ignition probability proposed by UCB
IX = C%IX
IY = C%IY
IFBFM = FBFM%I2(IX,IY,1)
IF(IFBFM .NE. 91) THEN
   P_IGN = PIGN*0.01
   HARDENING_FACTOR = 1.0
ELSE
   IF(USE_BLDG_SPREAD_MODEL) THEN
      IBLDGFM = C%IBLDGFM
      P_IGN = BUILDING_FUEL_MODEL_TABLE(IBLDGFM)%P_IGNITION*0.01
      HARDENING_FACTOR = BUILDING_FUEL_MODEL_TABLE(IBLDGFM)%HARDENING_FACTOR
   ELSE
      P_IGN = PIGN*0.01
      HARDENING_FACTOR = GLOBAL_HARDENING_FACTOR
   ENDIF
ENDIF


IF (IFBFM .EQ. 91) THEN
   IF(USE_BLDG_SPREAD_MODEL) THEN
      TAU_IGN = BUILDING_FUEL_MODEL_TABLE(IBLDGFM)%TAU_IGN ! Value derived from ThermaKin simulation for WRC.
      T_DEVELOP = BUILDING_FUEL_MODEL_TABLE(IBLDGFM)%T_1MW !
   ELSE
      TAU_IGN = 42.1 ! Value derived from ThermaKin simulation for PTW.
      T_DEVELOP = 300.0 ! Assume a medium fire growth rate for not defined structural fuels
   ENDIF
ELSE
   TAU_IGN = LOCAL_IGNITION_TIME
   T_DEVELOP = CELL_IGNITION_DELAY
ENDIF

IF (.NOT. C%LOCAL_IGNITION) THEN

   NUM_ACCUMULATED_EMBERS_PUA = EMBER_FLUX%R4(IX,IY,1)/ANALYSIS_CELLSIZE/ANALYSIS_CELLSIZE
   
   IF(.NOT. USE_SIMPLE_IGNITION_MODEL)THEN        
      ! Ignition critical ember mass density from De Beer' Thesis, 2023
      M_EMBER = 0.2 ! Estimated ember mass, 0.2 g

      IF(IFBFM .NE. 91) THEN
         IF (CC%R4(IX,IY, 1) .GT. 1E-4 .AND. CH%R4(IX,IY, 1) .GT. 1E-4) THEN !Canopy is present
            HFT = CH%R4(IX,IY, 1) / 0.3048
            F = 0.3333 * CC%R4(IX,IY, 1) * CROWN_RATIO !Same as BEHAVE
         ELSE !Canopy is not present
            HFT = FUEL_MODEL_TABLE_2D(IFBFM,30)%DELTA
            F = 0.05
         ENDIF
         HFT = MAX(1E-5, HFT)
         F = MAX(1E-5, F)
         COEF_WIND = 0.555/SQRT(F*HFT)/LOG((HFT+20-0.64*HFT)/(0.13*HFT))
         COEF_WIND = MIN(COEF_WIND,1.0)
      ELSE
         ! Hardcoded values consistant with IAFSS2026 publication
         F = 1.0 
         HFT = 8.0
         COEF_WIND = 0.555/SQRT(F*HFT)/LOG((HFT+20-0.64*HFT)/(0.13*HFT))
      ENDIF

      PSI = NUM_ACCUMULATED_EMBERS_PUA * M_EMBER/1E4 ! Firebrand coverage density (mass load, g/cm2) 
      V_AIR = UWIND*COEF_WIND*0.447
      IGNITION_CRITERION = HARDENING_FACTOR*PSI*(V_AIR+0.073)*(V_AIR-4.111)+0.211 ! UMD Fitted curve at P_IGN = 0.5 for PTW

      ! This change corresponds to the ignition model publication, per comments of editors
      IF(DIFF_WILDLAND_IGNITION) THEN 
         IF(IFBFM .NE. 91) IGNITION_CRITERION = -1.0 ! Assume for vegetative fuels, firebrands can always ignite the fuels (smoldering-to-flaming transition).
      ENDIF

      IF(IGNITION_CRITERION .LT. 0)THEN
         P_IGN = 0.90
      ELSE
         P_IGN = 0.0
      ENDIF
      ! End of De Beer's model

   ENDIF ! IF(.NOT. USE_SIMPLE_IGNITION_MODEL)THEN

   IF(NUM_ACCUMULATED_EMBERS_PUA .GT. 0) THEN
      P_N = 1.0-(1.0-P_IGN)**(DT_ELMFIRE/MAX(TAU_IGN, 1E-6)) ! Equivalent ignition probability at current time step
   ELSE
      P_N = 0.0
   ENDIF

   CALL RANDOM_NUMBER(R0)
   IF(R0 .LT. P_N) THEN
      C%T_LOCAL_IGNITION = T_ELMFIRE+DT_ELMFIRE
      C%LOCAL_IGNITION   = .TRUE.
   ENDIF
ELSE
   IF(T_ELMFIRE+DT_ELMFIRE-C%T_LOCAL_IGNITION .GE. T_DEVELOP) C%FULL_DEV_IGNITION = .TRUE.
ENDIF

! *****************************************************************************
END SUBROUTINE EMBER_IGNITION
! *****************************************************************************

! *****************************************************************************
SUBROUTINE EMBER_CONSUMPTION(IX,IY,T_ELMFIRE, DT_ELMFIRE)
! *****************************************************************************
! Firebrand ignition model, based on the ember accumulation history
USE ELMFIRE_VARS

REAL, INTENT(IN) :: T_ELMFIRE, DT_ELMFIRE
INTEGER, INTENT(IN) :: IX,IY

REAL :: NUM_ACCUMULATED_EMBERS_PUA, V_AIR, COEF_WIND, PSI, M_EMBER, HFT, F, &
        MIN_LIFETIME, T_RISE, HF_RISE, HF_DECAY, HF_T0, HF_MAX, HF_CRIT, &
        T_HALFMAX_1, T_HALFMAX_2, T_LIFETIME, DNPP_FIREBRAND_DT, F_WIND, WS20
INTEGER :: IFBFM, ITLO_METEOROLOGY, ITHI_METEOROLOGY, ICOL, IROW
REAL, POINTER, DIMENSION(:,:), SAVE :: WS20_LO_SPOTTING, WS20_HI_SPOTTING

M_EMBER      = 0.2  ! g, firebrand particle mass
MIN_LIFETIME = 1E9  ! s, minimum firebrand life time

NUM_ACCUMULATED_EMBERS_PUA = EMBER_FLUX%R4(IX,IY,1)/ANALYSIS_CELLSIZE/ANALYSIS_CELLSIZE

PSI = NUM_ACCUMULATED_EMBERS_PUA * M_EMBER / 1E4    ! g/cm2

ITLO_METEOROLOGY = MAX(1 + FLOOR((T_ELMFIRE / DT_METEOROLOGY)),1)
ITLO_METEOROLOGY = MIN(ITLO_METEOROLOGY, NUM_METEOROLOGY_TIMES)
ITHI_METEOROLOGY = MIN(ITLO_METEOROLOGY + 1, NUM_METEOROLOGY_TIMES)
F_WIND = (T_ELMFIRE - REAL(ITLO_METEOROLOGY-1) * DT_METEOROLOGY) / DT_METEOROLOGY
IF (ITLO_METEOROLOGY .EQ. ITHI_METEOROLOGY) F_WIND = 1.

WS20_LO_SPOTTING => WSP   (:,:,ITLO_METEOROLOGY)
WS20_HI_SPOTTING => WSP   (:,:,ITHI_METEOROLOGY)

ICOL = ICOL_ANALYSIS_F2C(IX)
IROW = IROW_ANALYSIS_F2C(IY)

WS20 = WS20_LO_SPOTTING(ICOL,IROW) * (1. - F_WIND) + F_WIND * WS20_HI_SPOTTING(ICOL,IROW) 
WS20 = 0.447 * WS20

IFBFM = FBFM%I2(IX,IY,1)
IF(IFBFM .NE. 91) THEN
   IF (CC%R4(IX,IY, 1) .GT. 1E-4 .AND. CH%R4(IX,IY, 1) .GT. 1E-4) THEN !Canopy is present
      HFT = CH%R4(IX,IY, 1) / 0.3048
      F = 0.3333 * CC%R4(IX,IY, 1) * CROWN_RATIO !Same as BEHAVE
   ELSE !Canopy is not present
      HFT = FUEL_MODEL_TABLE_2D(IFBFM,30)%DELTA
      F = 0.05
   ENDIF
   HFT = MAX(1E-5, HFT)
   F = MAX(1E-5, F)
   COEF_WIND = 0.555/SQRT(F*HFT)/LOG((HFT+20-0.64*HFT)/(0.13*HFT))
   COEF_WIND = MIN(COEF_WIND,1.0)
ELSE
   ! Hardcoded values consistant with IAFSS2026 publication
   F = 1.0 
   HFT = 8.0
   COEF_WIND = 0.555/SQRT(F*HFT)/LOG((HFT+20-0.64*HFT)/(0.13*HFT))
ENDIF
V_AIR = WS20*COEF_WIND

! Firebrand pile heat flux profile derived form Jacque Debeer's model
T_RISE   = MAX(-1.02*V_AIR**2+9.08*V_AIR+35    ,0.0)    !s
HF_RISE  = MAX(-0.32*V_AIR**2+1.59*V_AIR+0.1   ,0.0)*TANH(12.0*PSI)
HF_RISE  = MAX(HF_RISE, 1E-6)
HF_DECAY = MAX(1E-6,-0.05*V_AIR**2 + 0.02*V_AIR-0.01)*TANH(12.0*PSI)
HF_DECAY = MAX(HF_DECAY, 1E-6)

HF_RISE = (0.13*2.4+0.65)*HF_RISE
HF_T0   = HF_RISE*12.0    !kW/m2
HF_MAX  = HF_T0+HF_RISE*T_RISE
! Assuming minimum HF value for a effectively burning pile
HF_CRIT = 10.0

T_HALFMAX_1 = MAX((HF_CRIT-HF_T0)/HF_RISE,0.0)
T_HALFMAX_2 = (T_RISE+(HF_CRIT-HF_MAX)/HF_DECAY)
T_LIFETIME  = MAX(T_HALFMAX_2-T_HALFMAX_1,0.0)
! t_lifetime=min_lifetime;

! Assuming linear firebrand pile mass consumption rate dN''/dt = - N''/t_lifetime
DNPP_FIREBRAND_DT = NUM_ACCUMULATED_EMBERS_PUA/MAX(T_LIFETIME,MIN_LIFETIME)    ! pcs/s

EMBER_FLUX%R4(IX,IY,1) = EMBER_FLUX%R4(IX,IY,1) - DT_ELMFIRE * DNPP_FIREBRAND_DT * ANALYSIS_CELLSIZE * ANALYSIS_CELLSIZE

! *****************************************************************************
END SUBROUTINE EMBER_CONSUMPTION
! *****************************************************************************

! *****************************************************************************
SUBROUTINE CALC_SPOTTING_DURATION(C)
! *****************************************************************************
! Determine the spotting duration when using umd spotting model
USE ELMFIRE_VARS

TYPE (NODE), POINTER, INTENT(INOUT) :: C

REAL :: HRRPUA_CRIT_LOCAL, HRRPUA_PEAK_LOCAL, T_EARLY_LOCAL, T_DECAY_LOCAL, T_FULLDEV_LOCAL

IF (USE_PHYSICAL_SPOTTING_DURATION) THEN
   IF (C%T_START_SPOTTING .LT. 0.0) THEN
      IF (C%IFBFM .EQ. 91 ) THEN
         IF(USE_BLDG_SPREAD_MODEL) THEN
            HRRPUA_CRIT_LOCAL = CRITICAL_SPOTTING_FIRELINE_INTENSITY(FBFM%I2(C%IX,C%IY,1))/ANALYSIS_CELLSIZE
            IF (C%HRR_TRANSIENT .GE. HRRPUA_CRIT_LOCAL) THEN
               ! Assumed fire curve with linear growth and decay phases
               HRRPUA_PEAK_LOCAL = BUILDING_FUEL_MODEL_TABLE(C%IBLDGFM)%HRRPUA_PEAK
               T_EARLY_LOCAL = BUILDING_FUEL_MODEL_TABLE(C%IBLDGFM)%T_EARLY
               T_DECAY_LOCAL = BUILDING_FUEL_MODEL_TABLE(C%IBLDGFM)%T_DECAY
               T_FULLDEV_LOCAL = BUILDING_FUEL_MODEL_TABLE(C%IBLDGFM)%T_FULLDEV
               C%T_START_SPOTTING = C%TIME_OF_ARRIVAL
               C%T_END_SPOTTING = C%T_START_SPOTTING + (HRRPUA_PEAK_LOCAL-HRRPUA_CRIT_LOCAL) * &
                     (T_EARLY_LOCAL/HRRPUA_PEAK_LOCAL  + (T_DECAY_LOCAL - T_FULLDEV_LOCAL)/HRRPUA_PEAK_LOCAL) + &
                     T_FULLDEV_LOCAL - T_EARLY_LOCAL
            ENDIF
         ELSE
            C%T_START_SPOTTING = C%TIME_OF_ARRIVAL
            C%T_END_SPOTTING = C%T_START_SPOTTING+TAU_EMBERGEN
         ENDIF ! IF(USE_BLDG_SPREAD_MODEL)
      ELSE
         C%T_START_SPOTTING = C%TIME_OF_ARRIVAL
         C%T_END_SPOTTING = C%T_START_SPOTTING+C%LOCAL_EMBERGEN_DURATION
      ENDIF ! C%T_START_SPOTTING .LT. 1E-3
   ENDIF ! USE_PHYSICAL_SPOTTING_DURATION
ELSE
   IF (C%T_START_SPOTTING .LT. 0.0) THEN
      C%T_START_SPOTTING = C%TIME_OF_ARRIVAL
      C%T_END_SPOTTING = C%T_START_SPOTTING+TAU_EMBERGEN
   ENDIF
ENDIF ! USE_PHYSICAL_SPOTTING_DURATION

C%SPOTTING_DURATION_CALCULATED = .TRUE.
! *****************************************************************************
END SUBROUTINE CALC_SPOTTING_DURATION
! *****************************************************************************

! *****************************************************************************
FUNCTION ERFINV_LOCAL(X) RESULT(Y)
! *****************************************************************************
! This function serves in substitution for the ERFINV function in elmfire_subs.f90 for the superceded spotting model.
IMPLICIT NONE
REAL(4), INTENT(IN) :: X
REAL(4) :: Y, A, LN_EXPR, PI_VAL, TERM1, TERM2

IF (ABS(X) >= 1.0E0) THEN
  PRINT *, 'ERROR: ERFINV_LOCAL(X) domain is |X| < 1. Received:', X
  STOP
END IF

A = 0.147E0
PI_VAL = 3.1415927E0
LN_EXPR = LOG(1.0E0 - X*X)

TERM1 = 2.0E0 / (PI_VAL * A) + LN_EXPR / 2.0E0
TERM2 = SQRT(TERM1**2 - LN_EXPR / A)

Y = SIGN(1.0E0, X) * SQRT(TERM2 - TERM1)
! *****************************************************************************
END FUNCTION ERFINV_LOCAL
! *****************************************************************************

! *****************************************************************************
END MODULE ELMFIRE_SPOTTING
! *****************************************************************************