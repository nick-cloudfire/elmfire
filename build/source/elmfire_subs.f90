! *****************************************************************************
MODULE ELMFIRE_SUBS
! *****************************************************************************

USE ELMFIRE_VARS
USE MPI_F08

IMPLICIT NONE

CONTAINS

! *****************************************************************************
SUBROUTINE WRITE_TIMINGS_TO_DISK
! *****************************************************************************

INTEGER :: I,IOS,IR,LU
CHARACTER(400) :: FN

LU=3939

FN = TRIM(OUTPUTS_DIRECTORY) // 'timings_' // TRIM(PROCNAME) // '.csv'  
OPEN(LU,FILE=TRIM(FN),FORM='FORMATTED',STATUS='REPLACE',IOSTAT=IOS)
WRITE(LU,100) '#,', (IR, IR=0, NPROC_HOST-1)
DO I = 1, 62
   WRITE(LU,300) I, (TIMINGS(IR+1,I), IR=0, NPROC_HOST-1)
ENDDO
CLOSE(LU)

100 FORMAT (A,128(I3,','))
300 FORMAT (I3,',',128(F11.5,','))

! *****************************************************************************
END SUBROUTINE WRITE_TIMINGS_TO_DISK
! *****************************************************************************

! *****************************************************************************
SUBROUTINE ACCUMULATE_CPU_USAGE(IBLOCK,IT1,IT2)
! *****************************************************************************

INTEGER, INTENT(IN) :: IBLOCK
INTEGER, INTENT(INOUT) :: IT1
INTEGER :: IT2

CALL SYSTEM_CLOCK(IT2)
TIMINGS(IRANK_HOST+1,IBLOCK) = TIMINGS(IRANK_HOST+1,IBLOCK) + REAL(IT2 - IT1) / REAL(CLOCK_COUNT_RATE)
CALL SYSTEM_CLOCK(IT1)

! *****************************************************************************
END SUBROUTINE ACCUMULATE_CPU_USAGE
! *****************************************************************************

! *****************************************************************************
SUBROUTINE MPI_BCAST_RASTER_HEADER(R, IROOT, JUST_SEND_SIZE)
! *****************************************************************************

TYPE (RASTER_TYPE) :: R
INTEGER, INTENT(IN) :: IROOT
LOGICAL, INTENT(IN) :: JUST_SEND_SIZE
INTEGER :: IERR

IF (JUST_SEND_SIZE) THEN
   CALL MPI_BCAST(R%NROWS         ,  1, MPI_INTEGER    , IROOT, MPI_COMM_WORLD, IERR)
   CALL MPI_BCAST(R%NCOLS         ,  1, MPI_INTEGER    , IROOT, MPI_COMM_WORLD, IERR)
   CALL MPI_BCAST(R%NBANDS        ,  1, MPI_INTEGER    , IROOT, MPI_COMM_WORLD, IERR)
ELSE
   CALL MPI_BCAST(R%BYTEORDER     ,  1, MPI_CHARACTER  , IROOT, MPI_COMM_WORLD, IERR)
   CALL MPI_BCAST(R%LAYOUT        ,  3, MPI_CHARACTER  , IROOT, MPI_COMM_WORLD, IERR)
   CALL MPI_BCAST(R%NROWS         ,  1, MPI_INTEGER    , IROOT, MPI_COMM_WORLD, IERR)
   CALL MPI_BCAST(R%NCOLS         ,  1, MPI_INTEGER    , IROOT, MPI_COMM_WORLD, IERR)
   CALL MPI_BCAST(R%NBANDS        ,  1, MPI_INTEGER    , IROOT, MPI_COMM_WORLD, IERR)
   CALL MPI_BCAST(R%NBITS         ,  1, MPI_INTEGER    , IROOT, MPI_COMM_WORLD, IERR)
   CALL MPI_BCAST(R%BANDROWBYTES  ,  1, MPI_INTEGER    , IROOT, MPI_COMM_WORLD, IERR)
   CALL MPI_BCAST(R%TOTALROWBYTES ,  1, MPI_INTEGER    , IROOT, MPI_COMM_WORLD, IERR)
   CALL MPI_BCAST(R%PIXELTYPE     , 10, MPI_CHARACTER  , IROOT, MPI_COMM_WORLD, IERR)
   CALL MPI_BCAST(R%ULXMAP        ,  1, MPI_REAL       , IROOT, MPI_COMM_WORLD, IERR)
   CALL MPI_BCAST(R%ULYMAP        ,  1, MPI_REAL       , IROOT, MPI_COMM_WORLD, IERR)
   CALL MPI_BCAST(R%XDIM          ,  1, MPI_REAL       , IROOT, MPI_COMM_WORLD, IERR)
   CALL MPI_BCAST(R%YDIM          ,  1, MPI_REAL       , IROOT, MPI_COMM_WORLD, IERR)
   CALL MPI_BCAST(R%NODATA_VALUE  ,  1, MPI_REAL       , IROOT, MPI_COMM_WORLD, IERR)
   CALL MPI_BCAST(R%CELLSIZE      ,  1, MPI_REAL       , IROOT, MPI_COMM_WORLD, IERR)
   CALL MPI_BCAST(R%XLLCORNER     ,  1, MPI_REAL       , IROOT, MPI_COMM_WORLD, IERR)
   CALL MPI_BCAST(R%YLLCORNER     ,  1, MPI_REAL       , IROOT, MPI_COMM_WORLD, IERR)
ENDIF

! *****************************************************************************
END SUBROUTINE MPI_BCAST_RASTER_HEADER
! *****************************************************************************

SUBROUTINE BCAST_WEATHER 

INTEGER :: IERR, FUEL_TOPO_COUNT, WEATHER_COUNT

FUEL_TOPO_COUNT = ASP%NCOLS * ASP%NROWS
WEATHER_COUNT= WS%NCOLS * WS%NROWS * SIZE(WS%R4, 3)

CALL MPI_BCAST(WS%R4  , WEATHER_COUNT, MPI_REAL, 0, MPI_COMM_HOST_IRANK0, IERR)
CALL MPI_BCAST(WD%R4  , WEATHER_COUNT, MPI_REAL, 0, MPI_COMM_HOST_IRANK0, IERR)
CALL MPI_BCAST(M1%R4  , WEATHER_COUNT, MPI_REAL, 0, MPI_COMM_HOST_IRANK0, IERR)
CALL MPI_BCAST(M10%R4 , WEATHER_COUNT, MPI_REAL, 0, MPI_COMM_HOST_IRANK0, IERR)
CALL MPI_BCAST(M100%R4, WEATHER_COUNT, MPI_REAL, 0, MPI_COMM_HOST_IRANK0, IERR)
CALL MPI_BCAST(MLH%R4 , WEATHER_COUNT, MPI_REAL, 0, MPI_COMM_HOST_IRANK0, IERR)
CALL MPI_BCAST(MLW%R4 , WEATHER_COUNT, MPI_REAL, 0, MPI_COMM_HOST_IRANK0, IERR)
CALL MPI_BCAST(MFOL%R4, WEATHER_COUNT, MPI_REAL, 0, MPI_COMM_HOST_IRANK0, IERR)

IF (USE_ERC) THEN
   CALL MPI_BCAST(ERC%R4, WEATHER_COUNT, MPI_REAL, 0, MPI_COMM_HOST_IRANK0, IERR)
   CALL MPI_BCAST(IGNFAC%R4, WEATHER_COUNT, MPI_REAL, 0, MPI_COMM_HOST_IRANK0, IERR)
ENDIF

END SUBROUTINE BCAST_WEATHER

! *****************************************************************************
SUBROUTINE BCAST_FUEL_TOPOGRAPHY
! *****************************************************************************

INTEGER :: IERR, FUEL_TOPO_COUNT

FUEL_TOPO_COUNT = ASP%NCOLS * ASP%NROWS

IF (USE_IGNITION_MASK) CALL MPI_BCAST(IGN_MASK%R4, FUEL_TOPO_COUNT, MPI_REAL, 0, MPI_COMM_HOST_IRANK0, IERR)

CALL MPI_BCAST(ASP%R4, FUEL_TOPO_COUNT, MPI_REAL, 0, MPI_COMM_HOST_IRANK0, IERR)
CALL MPI_BCAST(CBH%R4, FUEL_TOPO_COUNT, MPI_REAL, 0, MPI_COMM_HOST_IRANK0, IERR)
CALL MPI_BCAST(CBD%R4, FUEL_TOPO_COUNT, MPI_REAL, 0, MPI_COMM_HOST_IRANK0, IERR)
CALL MPI_BCAST(CC%R4, FUEL_TOPO_COUNT, MPI_REAL, 0, MPI_COMM_HOST_IRANK0, IERR)
CALL MPI_BCAST(CH%R4, FUEL_TOPO_COUNT, MPI_REAL, 0, MPI_COMM_HOST_IRANK0, IERR)

IF (MODE .NE. 2) CALL MPI_BCAST(DEM%R4, FUEL_TOPO_COUNT, MPI_REAL, 0, MPI_COMM_HOST_IRANK0, IERR)

CALL MPI_BCAST(FBFM%I2, FUEL_TOPO_COUNT, MPI_SHORT, 0, MPI_COMM_HOST_IRANK0, IERR)

CALL MPI_BCAST(SLP%R4, FUEL_TOPO_COUNT, MPI_REAL, 0, MPI_COMM_HOST_IRANK0, IERR)
CALL MPI_BCAST(ADJ%R4, FUEL_TOPO_COUNT, MPI_REAL, 0, MPI_COMM_HOST_IRANK0, IERR)

IF (MODE .NE. 2) CALL MPI_BCAST(PHI0%R4, FUEL_TOPO_COUNT, MPI_REAL, 0, MPI_COMM_HOST_IRANK0, IERR)

CALL MPI_BCAST(WAF%R4, FUEL_TOPO_COUNT, MPI_REAL, 0, MPI_COMM_HOST_IRANK0, IERR)
CALL MPI_BCAST(OMCOSSLPRAD%R4, FUEL_TOPO_COUNT, MPI_REAL, 0, MPI_COMM_HOST_IRANK0, IERR)

CALL MPI_BCAST(ISNONBURNABLE, FUEL_TOPO_COUNT, MPI_BYTE, 0, MPI_COMM_HOST_IRANK0, IERR)

IF (USE_POPULATION_DENSITY) CALL MPI_BCAST(POPULATION_DENSITY%R4, FUEL_TOPO_COUNT, MPI_REAL, 0, MPI_COMM_HOST_IRANK0, IERR)
IF (USE_REAL_ESTATE_VALUE ) CALL MPI_BCAST(REAL_ESTATE_VALUE%R4, FUEL_TOPO_COUNT, MPI_REAL, 0, MPI_COMM_HOST_IRANK0, IERR)
IF (USE_LAND_VALUE        ) CALL MPI_BCAST(LAND_VALUE%R4, FUEL_TOPO_COUNT, MPI_REAL, 0, MPI_COMM_HOST_IRANK0, IERR)
IF (USE_SDI               ) CALL MPI_BCAST(SDI%R4, FUEL_TOPO_COUNT, MPI_REAL, 0, MPI_COMM_HOST_IRANK0, IERR)
IF (USE_BLDG_SPREAD_MODEL) THEN
   CALL MPI_BCAST(BLDG_AREA%R4            , FUEL_TOPO_COUNT, MPI_REAL , 0, MPI_COMM_HOST_IRANK0, IERR)
   CALL MPI_BCAST(BLDG_SEPARATION_DIST%R4 , FUEL_TOPO_COUNT, MPI_REAL , 0, MPI_COMM_HOST_IRANK0, IERR)
   CALL MPI_BCAST(BLDG_NONBURNABLE_FRAC%R4, FUEL_TOPO_COUNT, MPI_REAL , 0, MPI_COMM_HOST_IRANK0, IERR)
   CALL MPI_BCAST(BLDG_FOOTPRINT_FRAC%R4  , FUEL_TOPO_COUNT, MPI_REAL , 0, MPI_COMM_HOST_IRANK0, IERR)
   CALL MPI_BCAST(BLDG_FUEL_MODEL%I2      , FUEL_TOPO_COUNT, MPI_SHORT, 0, MPI_COMM_HOST_IRANK0, IERR)
ENDIF

IF (USE_PYROMES) CALL MPI_BCAST(PYROMES%I2, FUEL_TOPO_COUNT, MPI_SHORT, 0, MPI_COMM_HOST_IRANK0, IERR)

! *****************************************************************************
END SUBROUTINE BCAST_FUEL_TOPOGRAPHY
! *****************************************************************************

! *****************************************************************************
SUBROUTINE PERTURB_RASTERS(R1)
! *****************************************************************************

REAL, INTENT(IN), DIMENSION(:) :: R1

INTEGER :: I

DO I = 1, NUM_RASTERS_TO_PERTURB
   COEFFS_UNSCALED(I) = PDF_LOWER_LIMIT(I) + R1(I) * (PDF_UPPER_LIMIT(I) - PDF_LOWER_LIMIT(I))
   SELECT CASE (TRIM(RASTER_TO_PERTURB(I)))
      CASE('ADJ')
         PERTURB_ADJ  = COEFFS_UNSCALED(I)
      CASE('CBD')
         PERTURB_CBD  = COEFFS_UNSCALED(I)
      CASE('CBH')
         PERTURB_CBH  = COEFFS_UNSCALED(I)
      CASE('FMC')
         PERTURB_FMC  = COEFFS_UNSCALED(I)
      CASE('M1')
         PERTURB_M1   = COEFFS_UNSCALED(I)
      CASE('M10')
         PERTURB_M10  = COEFFS_UNSCALED(I)
      CASE('M100')
         PERTURB_M100 = COEFFS_UNSCALED(I)
      CASE('MLH')
         PERTURB_MLH  = COEFFS_UNSCALED(I)
      CASE('MLW')
         PERTURB_MLW  = COEFFS_UNSCALED(I)
      CASE('WAF')
         PERTURB_WAF  = COEFFS_UNSCALED(I)
      CASE('WD')
         PERTURB_WD   = COEFFS_UNSCALED(I)
      CASE('WS')
         PERTURB_WS   = COEFFS_UNSCALED(I)
   END SELECT

ENDDO

! *****************************************************************************
END SUBROUTINE PERTURB_RASTERS
! *****************************************************************************

! *****************************************************************************
SUBROUTINE GET_OPERATING_SYSTEM
! *****************************************************************************

CHARACTER(2000) :: PATH

CALL GET_ENVIRONMENT_VARIABLE('PATH',PATH)

IF (PATH(1:1) .EQ. '/') THEN 
   OPERATING_SYSTEM = 'linux  '
   PATH_SEPARATOR   = '/'
   DELETECOMMAND    = '/bin/rm -f '
ELSE
   OPERATING_SYSTEM = 'windows'
   PATH_SEPARATOR   = '\'
   DELETECOMMAND    = 'del   '
ENDIF

! *****************************************************************************
END SUBROUTINE GET_OPERATING_SYSTEM
! *****************************************************************************

! *****************************************************************************
SUBROUTINE ALLOCATE_EMPTY_RASTER(RASTER,NCOLS,NROWS,NBANDS,XLLCORNER,YLLCORNER,CELLSIZE,NODATA_VALUE,PIXELTYPE)
! *****************************************************************************

TYPE(RASTER_TYPE), INTENT(INOUT) :: RASTER
INTEGER, INTENT(IN) :: NCOLS, NROWS, NBANDS
REAL, INTENT(IN) :: XLLCORNER, YLLCORNER, CELLSIZE, NODATA_VALUE
CHARACTER(10), INTENT(IN) :: PIXELTYPE
INTEGER :: NBITS

IF (TRIM(PIXELTYPE) .NE. 'FLOAT' .AND. TRIM(PIXELTYPE) .NE. 'SIGNEDINT' ) THEN
   WRITE(*,*) "Error in ALLOCATE_EMPTY_RASTER. For now, only available PIXELTYPE are 'FLOAT' and 'SIGNEDINT'."
   STOP
ENDIF

SELECT CASE(TRIM(PIXELTYPE))
   CASE('FLOAT')
      NBITS=32
   CASE('SIGNEDINT')
      NBITS=16
END SELECT

! Thse three variables are used by ELMFIRE but are not in the BIL header:
RASTER%XLLCORNER    = XLLCORNER
RASTER%YLLCORNER    = YLLCORNER
RASTER%CELLSIZE     = CELLSIZE

! These variables, in order, get written to the BIL header
RASTER%BYTEORDER     = 'I'
RASTER%LAYOUT        = 'BIL'
RASTER%NROWS         = NROWS
RASTER%NCOLS         = NCOLS
RASTER%NBANDS        = NBANDS
RASTER%NBITS         = NBITS
RASTER%BANDROWBYTES  = (RASTER%NBITS/8) * RASTER%NCOLS
RASTER%TOTALROWBYTES = RASTER%BANDROWBYTES * RASTER%NBANDS
RASTER%PIXELTYPE     = PIXELTYPE
RASTER%ULXMAP        = RASTER%XLLCORNER + 0.5*RASTER%CELLSIZE
RASTER%ULYMAP        = RASTER%YLLCORNER - 0.5*RASTER%CELLSIZE + REAL(RASTER%NROWS)*RASTER%CELLSIZE
RASTER%XDIM          = RASTER%CELLSIZE
RASTER%YDIM          = RASTER%CELLSIZE
RASTER%NODATA_VALUE  = NODATA_VALUE
 
SELECT CASE(TRIM(PIXELTYPE))
   CASE('FLOAT')
      ALLOCATE(RASTER%R4(1:NCOLS,1:NROWS,1:NBANDS))
      RASTER%R4(:,:,:) = NODATA_VALUE
   CASE('SIGNEDINT')
      ALLOCATE(RASTER%I2(1:NCOLS,1:NROWS,1:NBANDS))
      RASTER%I2(:,:,:) = NINT(NODATA_VALUE,2)
   CASE DEFAULT
      CONTINUE
END SELECT

! *****************************************************************************
END SUBROUTINE ALLOCATE_EMPTY_RASTER
! *****************************************************************************

! *****************************************************************************
SUBROUTINE MAP_FINE_TO_COARSE(COARSE,FINE,ICOL_COARSE,IROW_COARSE)
! *****************************************************************************

TYPE(RASTER_TYPE), INTENT(IN) :: COARSE, FINE
INTEGER, DIMENSION(:), INTENT(OUT) :: ICOL_COARSE, IROW_COARSE !fine to coarse

INTEGER :: ICOL, IROW
REAL, ALLOCATABLE, DIMENSION (:) :: X, Y

ALLOCATE(X(1:FINE%NCOLS))
ALLOCATE(Y(1:FINE%NROWS))

DO ICOL = 1, FINE%NCOLS
   X(ICOL) = FINE%XLLCORNER + (REAL(ICOL) - 0.5) * FINE%CELLSIZE
ENDDO

DO IROW = 1, FINE%NROWS
   Y(IROW) = FINE%YLLCORNER + (REAL(IROW) - 0.5) * FINE%CELLSIZE
ENDDO

ICOL_COARSE(:) = MIN(MAX(CEILING( (X(:) - COARSE%XLLCORNER) / COARSE%CELLSIZE),1),COARSE%NCOLS)
IROW_COARSE(:) = MIN(MAX(CEILING( (Y(:) - COARSE%YLLCORNER) / COARSE%CELLSIZE),1),COARSE%NROWS)

DEALLOCATE(X)
DEALLOCATE(Y)

! *****************************************************************************
END SUBROUTINE MAP_FINE_TO_COARSE
! *****************************************************************************

! *****************************************************************************
INTEGER FUNCTION ICOL_FROM_X(X,XLL,CELLSIZE)
! *****************************************************************************

REAL, INTENT(IN) :: X,XLL,CELLSIZE
REAL :: DIST

DIST = X - XLL 

ICOL_FROM_X = CEILING(DIST / CELLSIZE)

! *****************************************************************************
END FUNCTION ICOL_FROM_X
! *****************************************************************************

! *****************************************************************************
INTEGER FUNCTION IROW_FROM_Y(Y,YLL,CELLSIZE)
! *****************************************************************************

REAL, INTENT(IN) :: Y,YLL,CELLSIZE
REAL :: DIST

DIST = Y - YLL 

IROW_FROM_Y = CEILING(DIST / CELLSIZE)

! *****************************************************************************
END FUNCTION IROW_FROM_Y
! *****************************************************************************

! *****************************************************************************
REAL FUNCTION X_FROM_ICOL(ICOL,XLL,CELLSIZE)
! *****************************************************************************

INTEGER, INTENT(IN) :: ICOL
REAL, INTENT(IN) :: XLL, CELLSIZE

X_FROM_ICOL = XLL + (REAL(ICOL)-0.5) * CELLSIZE 

! *****************************************************************************
END FUNCTION X_FROM_ICOL
! *****************************************************************************

! *****************************************************************************
REAL FUNCTION Y_FROM_IROW(IROW,YLL,CELLSIZE)
! *****************************************************************************

INTEGER, INTENT(IN) :: IROW
REAL, INTENT(IN) :: YLL, CELLSIZE

Y_FROM_IROW = YLL + (REAL(IROW)-0.5) * CELLSIZE 

! *****************************************************************************
END FUNCTION Y_FROM_IROW
! *****************************************************************************

! *****************************************************************************
SUBROUTINE INTERP_WD_RASTER(L,WD_LO,WD_HI,F)
! *****************************************************************************

TYPE (DLL), INTENT(INOUT) :: L
REAL, DIMENSION(:,:), INTENT(IN) :: WD_LO, WD_HI
REAL, INTENT(IN) :: F
INTEGER :: I, ICOL, IROW
REAL :: WD1TO, WD2TO, WDTO
TYPE(NODE), POINTER :: C

C => L%HEAD
DO I = 1, L%NUM_NODES
   ICOL = ICOL_ANALYSIS_F2C(C%IX)
   IROW = IROW_ANALYSIS_F2C(C%IY)
   WD1TO = WD_LO(ICOL,IROW) + 180. ; IF (WD1TO .GT. 360) WD1TO = WD1TO - 360.
   WD2TO = WD_HI(ICOL,IROW) + 180. ; IF (WD2TO .GT. 360) WD2TO = WD2TO - 360.
   WDTO = WD1TO + F * (WD2TO - WD1TO)
   C%WD20_INTERP = WDTO + 180. + PERTURB_WD
   IF (C%WD20_INTERP .GT. 360.) C%WD20_INTERP = C%WD20_INTERP - 360.
   IF (C%WD20_INTERP .LT.   0.) C%WD20_INTERP = C%WD20_INTERP + 360.
   C%WD20_NOW = C%WD20_INTERP
   C => C%NEXT
ENDDO
  
! *****************************************************************************
END SUBROUTINE INTERP_WD_RASTER
! *****************************************************************************

! *****************************************************************************
SUBROUTINE INTERP_WD_RASTER_SINGLE(NODEIN,WD_LO,WD_HI,F)
! *****************************************************************************

TYPE(NODE), POINTER, INTENT(OUT) :: NODEIN
REAL, DIMENSION(:,:), INTENT(IN) :: WD_LO, WD_HI
REAL, INTENT(IN) :: F
INTEGER :: ICOL, IROW
REAL :: WD1TO, WD2TO, WDTO
TYPE(NODE), POINTER :: C

C => NODEIN

ICOL = ICOL_ANALYSIS_F2C(C%IX)
IROW = IROW_ANALYSIS_F2C(C%IY)
WD1TO = WD_LO(ICOL,IROW) + 180. ; IF (WD1TO .GT. 360) WD1TO = WD1TO - 360.
WD2TO = WD_HI(ICOL,IROW) + 180. ; IF (WD2TO .GT. 360) WD2TO = WD2TO - 360.
WDTO = WD1TO + F * (WD2TO - WD1TO)
C%WD20_INTERP = WDTO + 180. + PERTURB_WD
IF (C%WD20_INTERP .GT. 360.) C%WD20_INTERP = C%WD20_INTERP - 360.
IF (C%WD20_INTERP .LT.   0.) C%WD20_INTERP = C%WD20_INTERP + 360.

C%WD20_NOW = C%WD20_INTERP
  
! *****************************************************************************
END SUBROUTINE INTERP_WD_RASTER_SINGLE
! *****************************************************************************

! *****************************************************************************
SUBROUTINE INTERP_RASTER_LINKEDLIST(L,LO,HI,F,IQUANTITY)
! *****************************************************************************

TYPE (DLL), INTENT(INOUT) :: L
REAL, DIMENSION(:,:), INTENT(IN ) :: LO,HI
INTEGER, INTENT(IN) :: IQUANTITY
REAL, INTENT(IN) :: F
INTEGER :: I,ICOL,IROW
TYPE(NODE), POINTER :: C
REAL, PARAMETER :: CONVERSION_FACTOR = 5280./60.

C => L%HEAD

SELECT CASE (IQUANTITY)

   CASE (1)
      DO I = 1, L%NUM_NODES
         ICOL = WX_ICOL_FROM_ANALYSIS_IX(C%IX)
         IROW = WX_IROW_FROM_ANALYSIS_IY(C%IY)
         C%M1 = LO(ICOL,IROW) + F * (HI(ICOL,IROW) - LO(ICOL,IROW) )
         C%M1 = MAX (C%M1 + PERTURB_M1, 0.01)
         C => C%NEXT
      ENDDO

   CASE (2)
      DO I = 1, L%NUM_NODES
         ICOL = WX_ICOL_FROM_ANALYSIS_IX(C%IX)
         IROW = WX_IROW_FROM_ANALYSIS_IY(C%IY)
         C%M10 = LO(ICOL,IROW) + F * (HI(ICOL,IROW) - LO(ICOL,IROW) )
         C%M10 = MAX (C%M10 + PERTURB_M10, 0.01)
         C => C%NEXT
      ENDDO

   CASE (3)
      DO I = 1, L%NUM_NODES
         ICOL = WX_ICOL_FROM_ANALYSIS_IX(C%IX)
         IROW = WX_IROW_FROM_ANALYSIS_IY(C%IY)
         C%M100 = LO(ICOL,IROW) + F * (HI(ICOL,IROW) - LO(ICOL,IROW) )
         C%M100 = MAX (C%M100 + PERTURB_M100, 0.01)
         C => C%NEXT
      ENDDO

   CASE (4)
      DO I = 1, L%NUM_NODES
         ICOL = WX_ICOL_FROM_ANALYSIS_IX(C%IX)
         IROW = WX_IROW_FROM_ANALYSIS_IY(C%IY)
         C%MLH = LO(ICOL,IROW) + F * (HI(ICOL,IROW) - LO(ICOL,IROW) )
         C%MLH = MAX(C%MLH + PERTURB_MLH, 0.3)
         C => C%NEXT
      ENDDO

   CASE (5)
      DO I = 1, L%NUM_NODES
         ICOL = WX_ICOL_FROM_ANALYSIS_IX(C%IX)
         IROW = WX_IROW_FROM_ANALYSIS_IY(C%IY)
         C%MLW = LO(ICOL,IROW) + F * (HI(ICOL,IROW) - LO(ICOL,IROW) )
         C%MLW = MAX(C%MLW + PERTURB_MLW, 0.6)
         C => C%NEXT
      ENDDO

   CASE (6)
      DO I = 1, L%NUM_NODES
         ICOL = WX_ICOL_FROM_ANALYSIS_IX(C%IX)
         IROW = WX_IROW_FROM_ANALYSIS_IY(C%IY)
         C%FMC = LO(ICOL,IROW) + F * (HI(ICOL,IROW) - LO(ICOL,IROW) )
         C%FMC = C%FMC + PERTURB_FMC
         C => C%NEXT
      ENDDO

   CASE (7)
      DO I = 1, L%NUM_NODES
         ICOL = WX_ICOL_FROM_ANALYSIS_IX(C%IX)
         IROW = WX_IROW_FROM_ANALYSIS_IY(C%IY)
         C%WS20_INTERP = LO(ICOL,IROW) + F * (HI(ICOL,IROW) - LO(ICOL,IROW) )
         C%WS20_INTERP = MAX(C%WS20_INTERP + PERTURB_WS, 0.0)
         C%WS20_NOW = C%WS20_INTERP
         C%WSMF = C%WS20_NOW * MAX((WAF%R4(C%IX,C%IY,1) + PERTURB_WAF),0.) * CONVERSION_FACTOR
         C => C%NEXT
      ENDDO

END SELECT

! *****************************************************************************
END SUBROUTINE INTERP_RASTER_LINKEDLIST
! *****************************************************************************

! *****************************************************************************
SUBROUTINE INTERP_RASTER_LINKEDLIST_BILINEAR(L,LO,HI,F,IQUANTITY)
! *****************************************************************************

TYPE (DLL), INTENT(INOUT) :: L
REAL, DIMENSION(:,:), INTENT(IN) :: LO,HI
INTEGER, INTENT(IN) :: IQUANTITY
REAL, INTENT(IN) :: F
TYPE(NODE), POINTER :: C
REAL, PARAMETER :: CONVERSION_FACTOR = 5280./60.
REAL :: X1, X2, Y1, Y2, CX, CY, PL, PH, PNOW
REAL :: Q11L, Q21L, Q12L, Q22L, Q11H, Q21H, Q12H, Q22H
INTEGER :: I, I1, I2, J1, J2

C => L%HEAD

DO I = 1, L%NUM_NODES
    
   CALL GET_BILINEAR_INTERPOLATE_COEFFS(C%IX, C%IY, X1, Y1, X2, Y2, I1, J1, I2, J2, CX, CY)

   Q12L = LO(I1, J2) ; Q22L = LO(I2, J2)
   Q11L = LO(I1, J1) ; Q21L = LO(I2, J1)
   PL   = BILINEAR_INTERPOLATE(CX, CY, X1, Y1, X2, Y2, Q11L, Q21L, Q12L, Q22L)
         
   Q12H = HI(I1, J2) ; Q22H = HI(I2, J2)
   Q11H = HI(I1, J1) ; Q21H = HI(I2, J1)
   PH   = BILINEAR_INTERPOLATE(CX, CY, X1, Y1, X2, Y2, Q11H, Q21H, Q12H, Q22H)
   
   PNOW = PL + F * (PH - PL)

   SELECT CASE (IQUANTITY)
      CASE (1)
         C%M1   = MAX (PNOW + PERTURB_M1  , 0.01)
      CASE (2)
         C%M10  = MAX (PNOW + PERTURB_M10 , 0.01)
      CASE (3)
         C%M100 = MAX (PNOW + PERTURB_M100, 0.01)
      CASE (4)
         C%MLH  = MAX (PNOW + PERTURB_MLH , 0.30)
      CASE (5)
         C%MLW  = MAX (PNOW + PERTURB_MLW , 0.60)
      CASE (6)
         C%FMC = PNOW + PERTURB_FMC
      CASE (7)
         C%WS20_INTERP = MAX(PNOW + PERTURB_WS, 0.0)
         C%WS20_NOW = C%WS20_INTERP
         C%WSMF = C%WS20_NOW * MAX((WAF%R4(C%IX,C%IY,1) + PERTURB_WAF),0.) * CONVERSION_FACTOR
      END SELECT
   
   C => C%NEXT

ENDDO

! *****************************************************************************
END SUBROUTINE INTERP_RASTER_LINKEDLIST_BILINEAR
! *****************************************************************************

! *****************************************************************************
SUBROUTINE INTERP_WIND_LINKEDLIST_BILINEAR(L,WSLO,WSHI,WDLO,WDHI,F)
! *****************************************************************************

TYPE (DLL), INTENT(INOUT) :: L
REAL, DIMENSION(:,:), INTENT(IN) :: WSLO,WSHI,WDLO,WDHI
REAL, INTENT(IN) :: F
TYPE(NODE), POINTER :: C
REAL, PARAMETER :: CONVERSION_FACTOR = 5280./60.
REAL :: X1, X2, Y1, Y2, CX, CY
REAL :: WS11L, WS21L, WS12L, WS22L, WS11H, WS21H, WS12H, WS22H, &
        WD11L, WD21L, WD12L, WD22L, WD11H, WD21H, WD12H, WD22H, &
        UX11L, UX21L, UX12L, UX22L, UX11H, UX21H, UX12H, UX22H, &
        UY11L, UY21L, UY12L, UY22L, UY11H, UY21H, UY12H, UY22H
REAL :: UXL, UXH, UYL, UYH, UXNOW, UYNOW, WSNOW, WDNOW

INTEGER :: I, I1, I2, J1, J2

C => L%HEAD

DO I = 1, L%NUM_NODES
    
   CALL GET_BILINEAR_INTERPOLATE_COEFFS(C%IX, C%IY, X1, Y1, X2, Y2, I1, J1, I2, J2, CX, CY)

! Wind speed and wind direction (lo)
   WS12L = WSLO(I1, J2) ; WS22L = WSLO(I2, J2)
   WS11L = WSLO(I1, J1) ; WS21L = WSLO(I2, J1)
   
   WD12L = WDLO(I1, J2) ; WD22L = WDLO(I2, J2)
   WD11L = WDLO(I1, J1) ; WD21L = WDLO(I2, J1)

! Convert to x and y components
   UX12L = UX_FROM_WSWD(WS12L, WD12L) ; UX22L = UX_FROM_WSWD(WS22L, WD22L)
   UX11L = UX_FROM_WSWD(WS11L, WD11L) ; UX21L = UX_FROM_WSWD(WS21L, WD21L)

   UY12L = UY_FROM_WSWD(WS12L, WD12L) ; UY22L = UY_FROM_WSWD(WS22L, WD22L)
   UY11L = UY_FROM_WSWD(WS11L, WD11L) ; UY21L = UY_FROM_WSWD(WS21L, WD21L)
   
! Interpolate spatially
   UXL   = BILINEAR_INTERPOLATE(CX, CY, X1, Y1, X2, Y2, UX11L, UX21L, UX12L, UX22L)
   UYL   = BILINEAR_INTERPOLATE(CX, CY, X1, Y1, X2, Y2, UY11L, UY21L, UY12L, UY22L)

! Wind speed and wind direction (hi)
   WS12H = WSHI(I1, J2) ; WS22H = WSHI(I2, J2)
   WS11H = WSHI(I1, J1) ; WS21H = WSHI(I2, J1)
   
   WD12H = WDHI(I1, J2) ; WD22H = WDHI(I2, J2)
   WD11H = WDHI(I1, J1) ; WD21H = WDHI(I2, J1)

! Convert to x and y components
   UX12H = UX_FROM_WSWD(WS12H, WD12H) ; UX22H = UX_FROM_WSWD(WS22H, WD22H)
   UX11H = UX_FROM_WSWD(WS11H, WD11H) ; UX21H = UX_FROM_WSWD(WS21H, WD21H)

   UY12H = UY_FROM_WSWD(WS12H, WD12H) ; UY22H = UY_FROM_WSWD(WS22H, WD22H)
   UY11H = UY_FROM_WSWD(WS11H, WD11H) ; UY21H = UY_FROM_WSWD(WS21H, WD21H)
   
! Interpolate spatially
   UXH   = BILINEAR_INTERPOLATE(CX, CY, X1, Y1, X2, Y2, UX11H, UX21H, UX12H, UX22H)
   UYH   = BILINEAR_INTERPOLATE(CX, CY, X1, Y1, X2, Y2, UY11H, UY21H, UY12H, UY22H)

! Interpolate temporally
   UXNOW = UXL + F * (UXH - UXL)
   UYNOW = UYL + F * (UYH - UYL)
   
! Convert to wind speed and direction
   IF      (UXNOW .EQ. 0. .AND. UYNOW .EQ. 0.) THEN 
      WDNOW = 0.
   ELSE IF (UXNOW .GT. 0. .AND. UYNOW .EQ. 0.) THEN
      WDNOW = 0.5*PI
   ELSE IF (UXNOW .LT. 0. .AND. UYNOW .EQ. 0.) THEN          
      WDNOW = 1.5*PI          
   ELSE IF (UXNOW .EQ. 0. .AND. UYNOW .GT. 0.) THEN
      WDNOW = 0.0*PI
   ELSE IF (UXNOW .EQ. 0. .AND. UYNOW .LT. 0.) THEN          
      WDNOW = 1.0*PI
   ELSE
      IF (UXNOW .GT. 0. .AND. UYNOW .GT. 0.) WDNOW = 0.     + ATAN(  UXNOW /  UYNOW ) 
      IF (UXNOW .GT. 0. .AND. UYNOW .LT. 0.) WDNOW = 0.5*PI + ATAN( -UYNOW /  UXNOW )
      IF (UXNOW .LT. 0. .AND. UYNOW .LT. 0.) WDNOW = PI     + ATAN(  UXNOW /  UYNOW )
      IF (UXNOW .LT. 0. .AND. UYNOW .GT. 0.) WDNOW = 1.5*PI + ATAN(  UYNOW / ABS(UXNOW) )
   ENDIF

   WSNOW = SQRT(UXNOW*UXNOW + UYNOW*UYNOW)
   
   WDNOW = WDNOW * 180. / PI
   WDNOW = WDNOW + 180. 
   IF (WDNOW .GT. 360) WDNOW = WDNOW - 360.

   C%WD20_INTERP = WDNOW + PERTURB_WD
   IF (C%WD20_INTERP .GT. 360.) C%WD20_INTERP = C%WD20_INTERP - 360.
   IF (C%WD20_INTERP .LT.   0.) C%WD20_INTERP = C%WD20_INTERP + 360.
   C%WD20_NOW = C%WD20_INTERP

   C%WS20_INTERP = MAX(WSNOW + PERTURB_WS, 0.0)
   C%WS20_NOW = C%WS20_INTERP
   C%WSMF = C%WS20_NOW * MAX((WAF%R4(C%IX,C%IY,1) + PERTURB_WAF),0.) * CONVERSION_FACTOR
   
   C => C%NEXT

ENDDO

! *****************************************************************************
END SUBROUTINE INTERP_WIND_LINKEDLIST_BILINEAR
! *****************************************************************************

! *****************************************************************************
SUBROUTINE INTERP_WIND_SINGLE_BILINEAR(NODEIN,WSLO,WSHI,WDLO,WDHI,F)
! *****************************************************************************

TYPE(NODE), POINTER, INTENT(OUT) :: NODEIN
REAL, DIMENSION(:,:), INTENT(IN) :: WSLO,WSHI,WDLO,WDHI
REAL, INTENT(IN) :: F
TYPE(NODE), POINTER :: C
REAL, PARAMETER :: CONVERSION_FACTOR = 5280./60.
REAL :: X1, X2, Y1, Y2, CX, CY
REAL :: WS11L, WS21L, WS12L, WS22L, WS11H, WS21H, WS12H, WS22H, &
        WD11L, WD21L, WD12L, WD22L, WD11H, WD21H, WD12H, WD22H, &
        UX11L, UX21L, UX12L, UX22L, UX11H, UX21H, UX12H, UX22H, &
        UY11L, UY21L, UY12L, UY22L, UY11H, UY21H, UY12H, UY22H
REAL :: UXL, UXH, UYL, UYH, UXNOW, UYNOW, WSNOW, WDNOW

INTEGER :: I1, I2, J1, J2

C => NODEIN
    
CALL GET_BILINEAR_INTERPOLATE_COEFFS(C%IX, C%IY, X1, Y1, X2, Y2, I1, J1, I2, J2, CX, CY)

! Wind speed and wind direction (lo)
WS12L = WSLO(I1, J2) ; WS22L = WSLO(I2, J2)
WS11L = WSLO(I1, J1) ; WS21L = WSLO(I2, J1)
   
WD12L = WDLO(I1, J2) ; WD22L = WDLO(I2, J2)
WD11L = WDLO(I1, J1) ; WD21L = WDLO(I2, J1)

! Convert to x and y components
UX12L = UX_FROM_WSWD(WS12L, WD12L) ; UX22L = UX_FROM_WSWD(WS22L, WD22L)
UX11L = UX_FROM_WSWD(WS11L, WD11L) ; UX21L = UX_FROM_WSWD(WS21L, WD21L)

UY12L = UY_FROM_WSWD(WS12L, WD12L) ; UY22L = UY_FROM_WSWD(WS22L, WD22L)
UY11L = UY_FROM_WSWD(WS11L, WD11L) ; UY21L = UY_FROM_WSWD(WS21L, WD21L)
   
! Interpolate spatially
UXL   = BILINEAR_INTERPOLATE(CX, CY, X1, Y1, X2, Y2, UX11L, UX21L, UX12L, UX22L)
UYL   = BILINEAR_INTERPOLATE(CX, CY, X1, Y1, X2, Y2, UY11L, UY21L, UY12L, UY22L)

! Wind speed and wind direction (hi)
WS12H = WSHI(I1, J2) ; WS22H = WSHI(I2, J2)
WS11H = WSHI(I1, J1) ; WS21H = WSHI(I2, J1)
WD12H = WDHI(I1, J2) ; WD22H = WDHI(I2, J2)
WD11H = WDHI(I1, J1) ; WD21H = WDHI(I2, J1)

! Convert to x and y components
UX12H = UX_FROM_WSWD(WS12H, WD12H) ; UX22H = UX_FROM_WSWD(WS22H, WD22H)
UX11H = UX_FROM_WSWD(WS11H, WD11H) ; UX21H = UX_FROM_WSWD(WS21H, WD21H)

UY12H = UY_FROM_WSWD(WS12H, WD12H) ; UY22H = UY_FROM_WSWD(WS22H, WD22H)
UY11H = UY_FROM_WSWD(WS11H, WD11H) ; UY21H = UY_FROM_WSWD(WS21H, WD21H)
   
! Interpolate spatially
UXH   = BILINEAR_INTERPOLATE(CX, CY, X1, Y1, X2, Y2, UX11H, UX21H, UX12H, UX22H)
UYH   = BILINEAR_INTERPOLATE(CX, CY, X1, Y1, X2, Y2, UY11H, UY21H, UY12H, UY22H)

! Interpolate temporally
UXNOW = UXL + F * (UXH - UXL)
UYNOW = UYL + F * (UYH - UYL)
   
! Convert to wind speed and direction
IF      (UXNOW .EQ. 0. .AND. UYNOW .EQ. 0.) THEN 
   WDNOW = 0.
ELSE IF (UXNOW .GT. 0. .AND. UYNOW .EQ. 0.) THEN
   WDNOW = 0.5*PI
ELSE IF (UXNOW .LT. 0. .AND. UYNOW .EQ. 0.) THEN          
   WDNOW = 1.5*PI          
ELSE IF (UXNOW .EQ. 0. .AND. UYNOW .GT. 0.) THEN
   WDNOW = 0.0*PI
ELSE IF (UXNOW .EQ. 0. .AND. UYNOW .LT. 0.) THEN          
   WDNOW = 1.0*PI
ELSE
   IF (UXNOW .GT. 0. .AND. UYNOW .GT. 0.) WDNOW = 0.     + ATAN(  UXNOW /  UYNOW ) 
   IF (UXNOW .GT. 0. .AND. UYNOW .LT. 0.) WDNOW = 0.5*PI + ATAN( -UYNOW /  UXNOW )
   IF (UXNOW .LT. 0. .AND. UYNOW .LT. 0.) WDNOW = PI     + ATAN(  UXNOW /  UYNOW )
   IF (UXNOW .LT. 0. .AND. UYNOW .GT. 0.) WDNOW = 1.5*PI + ATAN(  UYNOW / ABS(UXNOW) )
ENDIF

WSNOW = SQRT(UXNOW*UXNOW + UYNOW*UYNOW)
   
WDNOW = WDNOW * 180. / PI
WDNOW = WDNOW + 180. 
IF (WDNOW .GT. 360) WDNOW = WDNOW - 360.

C%WD20_INTERP = WDNOW + PERTURB_WD
IF (C%WD20_INTERP .GT. 360.) C%WD20_INTERP = C%WD20_INTERP - 360.
IF (C%WD20_INTERP .LT.   0.) C%WD20_INTERP = C%WD20_INTERP + 360.
C%WD20_NOW = C%WD20_INTERP

C%WS20_INTERP = MAX(WSNOW + PERTURB_WS, 0.0)
C%WS20_NOW = C%WS20_INTERP
C%WSMF = C%WS20_NOW * MAX((WAF%R4(C%IX,C%IY,1) + PERTURB_WAF),0.) * CONVERSION_FACTOR
   
! *****************************************************************************
END SUBROUTINE INTERP_WIND_SINGLE_BILINEAR
! *****************************************************************************

! *****************************************************************************
REAL FUNCTION UX_FROM_WSWD(WS,WD)
! *****************************************************************************
   REAL, INTENT(IN) :: WS, WD
   UX_FROM_WSWD = WS * COS( (WD + 90.) * PI / 180.) 
! *****************************************************************************
END FUNCTION UX_FROM_WSWD
! *****************************************************************************

! *****************************************************************************
REAL FUNCTION UY_FROM_WSWD(WS,WD)
! *****************************************************************************
   REAL, INTENT(IN) :: WS, WD
   UY_FROM_WSWD = WS * SIN( (WD - 90.) * PI / 180.) 
! *****************************************************************************
END FUNCTION UY_FROM_WSWD
! *****************************************************************************

! *****************************************************************************
SUBROUTINE GET_BILINEAR_INTERPOLATE_COEFFS(IX, IY, X1, Y1, X2, Y2, I1, J1, I2, J2, CX, CY)
! *****************************************************************************

INTEGER, INTENT(IN) :: IX, IY
REAL, INTENT(OUT) :: X1, Y1, X2, Y2, CX, CY
INTEGER, INTENT(OUT) :: I1, J1, I2, J2

REAL :: XLL_WX, YLL_WX, CELLSIZE_WX, XLL_FUEL, YLL_FUEL, CELLSIZE_FUEL
INTEGER :: NCOL_WX, NROW_WX

XLL_WX        = WS%XLLCORNER
YLL_WX        = WS%YLLCORNER
CELLSIZE_WX   = WS%CELLSIZE
NCOL_WX       = WS%NCOLS
NROW_WX       = WS%NROWS

XLL_FUEL      = ADJ%XLLCORNER
YLL_FUEL      = ADJ%YLLCORNER
CELLSIZE_FUEL = ADJ%CELLSIZE

CX = XLL_FUEL + (REAL(IX) - 0.5) * CELLSIZE_FUEL
CY = YLL_FUEL + (REAL(IY) - 0.5) * CELLSIZE_FUEL

I1 = 1 + NINT( (CX - XLL_WX) / CELLSIZE_WX )
I1 = MAX ( MIN(I1    , NCOL_WX), 1 )
I2 = MAX ( MIN(I1 + 1, NCOL_WX), 1 )

J1 = 1 + NINT( (CY - YLL_WX) / CELLSIZE_WX )
J1 = MAX ( MIN(J1,     NROW_WX), 1 )
J2 = MAX ( MIN(J2 + 1, NROW_WX), 1 )

X1 = XLL_WX + (REAL(I1) - 0.5) * CELLSIZE_WX
X2 = XLL_WX + (REAL(I2) - 0.5) * CELLSIZE_WX
Y1 = YLL_WX + (REAL(J1) - 0.5) * CELLSIZE_WX
Y2 = YLL_WX + (REAL(J2) - 0.5) * CELLSIZE_WX

! *****************************************************************************
END SUBROUTINE GET_BILINEAR_INTERPOLATE_COEFFS
! *****************************************************************************

! *****************************************************************************
REAL FUNCTION BILINEAR_INTERPOLATE(X, Y, X1, Y1, X2, Y2, Q11, Q21, Q12, Q22)
! *****************************************************************************

REAL, INTENT(IN) :: X, Y, X1, Y1, X2, Y2, Q11, Q21, Q12, Q22
REAL :: X2MX1, Y2MY1, DENOM, X2MX, Y2MY, XMX1, YMY1, NUMER1, NUMER2, NUMER3, &
        NUMER4, P

X2MX1 = X2 - X1
Y2MY1 = Y2 - Y1
DENOM = X2MX1 * Y2MY1 

X2MX  = X2 - X
Y2MY  = Y2 - Y
XMX1  = X - X1
YMY1  = Y - Y1

IF (DENOM .GT. 1E-3) THEN
   NUMER1 = X2MX * Y2MY * Q11
   NUMER2 = XMX1 * Y2MY * Q21
   NUMER3 = X2MX * YMY1 * Q12
   NUMER4 = XMX1 * YMY1 * Q22
   P = ( NUMER1 + NUMER2 + NUMER3 + NUMER4 ) / DENOM
ELSE IF (X2MX1 .LT. 1E-3) THEN ! Only interpolate in y direction
   P = ( Q11 * Y2MY + Q12 * YMY1 ) / Y2MY1
ELSE ! Only interpolate in x direction
   P = ( Q11 * X2MX + Q21 * XMX1 ) / X2MX1
ENDIF

BILINEAR_INTERPOLATE = P 

! *****************************************************************************
END FUNCTION BILINEAR_INTERPOLATE
! *****************************************************************************

! *****************************************************************************
INTEGER FUNCTION WX_ICOL_FROM_ANALYSIS_IX(IX_IN)
! *****************************************************************************

INTEGER, INTENT(IN) :: IX_IN
INTEGER :: IX

IX                       = MAX ( MIN (IX_IN,                 FBFM%NCOLS ), 1 )
WX_ICOL_FROM_ANALYSIS_IX = MAX ( MIN (ICOL_ANALYSIS_F2C(IX),   WS%NCOLS ), 1 )

! *****************************************************************************
END FUNCTION WX_ICOL_FROM_ANALYSIS_IX
! *****************************************************************************

! *****************************************************************************
INTEGER FUNCTION WX_IROW_FROM_ANALYSIS_IY(IY_IN)
! *****************************************************************************

INTEGER, INTENT(IN) :: IY_IN
INTEGER :: IY

IY                       = MAX ( MIN (IY_IN,                 FBFM%NROWS ), 1 )
WX_IROW_FROM_ANALYSIS_IY = MAX ( MIN (IROW_ANALYSIS_F2C(IY),   WS%NROWS ), 1 )

! *****************************************************************************
END FUNCTION WX_IROW_FROM_ANALYSIS_IY
! *****************************************************************************

! *****************************************************************************
INTEGER FUNCTION ICOL_FINE_TO_COARSE(IX_IN)
! *****************************************************************************

INTEGER, INTENT(IN) :: IX_IN
INTEGER :: IX

IX = MAX(MIN(IX_IN, FBFM%NCOLS),1)
ICOL_FINE_TO_COARSE = MAX(MIN(ICOL_ANALYSIS_F2C(IX),WS%NCOLS),1)

! *****************************************************************************
END FUNCTION ICOL_FINE_TO_COARSE
! *****************************************************************************

! *****************************************************************************
SUBROUTINE INTERP_RASTER_LINKEDLIST_SINGLE(NODEIN,LO,HI,F,IQUANTITY)
! *****************************************************************************

TYPE (NODE), POINTER, INTENT(OUT) :: NODEIN
REAL, DIMENSION(:,:), INTENT(IN) :: LO,HI
INTEGER, INTENT(IN) :: IQUANTITY
REAL, INTENT(IN) :: F
INTEGER :: ICOL,IROW
TYPE(NODE), POINTER :: C
REAL, PARAMETER :: CONVERSION_FACTOR = 5280./60.

C => NODEIN
ICOL = ICOL_ANALYSIS_F2C(C%IX)
IROW = IROW_ANALYSIS_F2C(C%IY)

SELECT CASE (IQUANTITY)

   CASE (1)
      C%M1 = LO(ICOL,IROW) + F * (HI(ICOL,IROW) - LO(ICOL,IROW) )
      C%M1 = MAX(C%M1 + PERTURB_M1, 0.01)

   CASE (2)
      C%M10 = LO(ICOL,IROW) + F * (HI(ICOL,IROW) - LO(ICOL,IROW) )
      C%M10 = MAX(C%M10 + PERTURB_M10, 0.01)

   CASE (3)
      C%M100 = LO(ICOL,IROW) + F * (HI(ICOL,IROW) - LO(ICOL,IROW) )
      C%M100 = MAX(C%M100 + PERTURB_M100, 0.01)

   CASE (4)
      C%MLH = LO(ICOL,IROW) + F * (HI(ICOL,IROW) - LO(ICOL,IROW) )
      C%MLH = MAX(C%MLH + PERTURB_MLH, 0.3)

   CASE (5)
      C%MLW = LO(ICOL,IROW) + F * (HI(ICOL,IROW) - LO(ICOL,IROW) )
      C%MLW = MAX(C%MLW + PERTURB_MLW, 0.6)

   CASE (6)
      C%FMC = LO(ICOL,IROW) + F * (HI(ICOL,IROW) - LO(ICOL,IROW) )
      C%FMC = C%FMC + PERTURB_FMC

   CASE (7)
      C%WS20_INTERP = LO(ICOL,IROW) + F * (HI(ICOL,IROW) - LO(ICOL,IROW) )
      C%WS20_INTERP = MAX(C%WS20_INTERP + PERTURB_WS, 0.0)
      C%WS20_NOW = C%WS20_INTERP
      C%WSMF = C%WS20_NOW * MAX((WAF%R4(C%IX,C%IY,1) + PERTURB_WAF),0.) * CONVERSION_FACTOR

END SELECT

! *****************************************************************************
END SUBROUTINE INTERP_RASTER_LINKEDLIST_SINGLE
! *****************************************************************************

! *****************************************************************************
SUBROUTINE INTERP_RASTER_LINKEDLIST_SINGLE_BILINEAR(NODEIN,LO,HI,F,IQUANTITY)
! *****************************************************************************

TYPE (NODE), POINTER, INTENT(OUT) :: NODEIN
REAL, DIMENSION(:,:), INTENT(IN) :: LO,HI
INTEGER, INTENT(IN) :: IQUANTITY
REAL, INTENT(IN) :: F
TYPE(NODE), POINTER :: C
REAL, PARAMETER :: CONVERSION_FACTOR = 5280./60.
REAL :: X1, X2, Y1, Y2, CX, CY, PL, PH, PNOW
REAL :: Q11L, Q21L, Q12L, Q22L, Q11H, Q21H, Q12H, Q22H
INTEGER :: I1, I2, J1, J2

C => NODEIN

CALL GET_BILINEAR_INTERPOLATE_COEFFS(C%IX, C%IY, X1, Y1, X2, Y2, I1, J1, I2, J2, CX, CY)

Q12L = LO(I1, J2) ; Q22L = LO(I2, J2)
Q11L = LO(I1, J1) ; Q21L = LO(I2, J1)
PL   = BILINEAR_INTERPOLATE(CX, CY, X1, Y1, X2, Y2, Q11L, Q21L, Q12L, Q22L)
         
Q12H = HI(I1, J2) ; Q22H = HI(I2, J2)
Q11H = HI(I1, J1) ; Q21H = HI(I2, J1)
PH   = BILINEAR_INTERPOLATE(CX, CY, X1, Y1, X2, Y2, Q11H, Q21H, Q12H, Q22H)
   
PNOW = PL + F * (PH - PL)

SELECT CASE (IQUANTITY)
   CASE (1)
      C%M1   = MAX(PNOW + PERTURB_M1  , 0.01)
   CASE (2)
      C%M10  = MAX(PNOW + PERTURB_M10 , 0.01)
   CASE (3)
      C%M100 = MAX(PNOW + PERTURB_M100, 0.01)
   CASE (4)
      C%MLH  = MAX(PNOW + PERTURB_MLH , 0.30)
   CASE (5)
      C%MLW  = MAX(PNOW + PERTURB_MLW , 0.60)
   CASE (6)
      C%FMC  = PNOW + PERTURB_FMC
   CASE (7)
      C%WS20_INTERP = MAX(PNOW + PERTURB_WS, 0.0)
      C%WS20_NOW = C%WS20_INTERP
      C%WSMF = C%WS20_NOW * MAX((WAF%R4(C%IX,C%IY,1) + PERTURB_WAF),0.) * CONVERSION_FACTOR
END SELECT

! *****************************************************************************
END SUBROUTINE INTERP_RASTER_LINKEDLIST_SINGLE_BILINEAR
! *****************************************************************************

! *****************************************************************************
SUBROUTINE APPLY_WIND_FLUCTUATIONS(L)
! *****************************************************************************

TYPE (DLL), INTENT(INOUT) :: L
TYPE(NODE), POINTER :: C
INTEGER :: I
REAL :: FAC1, FAC2, R2(1:2)
REAL, PARAMETER :: CONVERSION_FACTOR = 5280./60.

CALL RANDOM_NUMBER(R2)
R2(1) = WIND_SPEED_FLUCTUATION_INTENSITY     * (R2(1) - 0.5)
R2(2) = WIND_DIRECTION_FLUCTUATION_INTENSITY * (R2(2) - 0.5)

FAC1 = 1. + R2(1)
FAC2 = R2(2) * 360.

C => L%HEAD
DO I = 1, L%NUM_NODES
   C%WS20_NOW = FAC1 * C%WS20_INTERP
   C%WSMF = C%WS20_NOW * MAX((WAF%R4(C%IX,C%IY,1) + PERTURB_WAF),0.) * CONVERSION_FACTOR
   C%WD20_NOW = C%WD20_INTERP + FAC2
   IF (C%WD20_NOW .GT. 360.) C%WD20_NOW = C%WD20_NOW - 360.
   IF (C%WD20_NOW .LT.   0.) C%WD20_NOW = C%WD20_NOW + 360.
   C => C%NEXT
ENDDO

! *****************************************************************************
END SUBROUTINE APPLY_WIND_FLUCTUATIONS
! *****************************************************************************

! *****************************************************************************
ELEMENTAL TYPE(DLL) FUNCTION NEW_DLL()
! *****************************************************************************

NEW_DLL = DLL(NULL(),NULL(),0)

! *****************************************************************************
END FUNCTION NEW_DLL
! *****************************************************************************
 
! *****************************************************************************
LOGICAL FUNCTION CHECK_BARRIER_BREACH(C)
! *****************************************************************************
   TYPE(NODE), POINTER :: C

   CHECK_BARRIER_BREACH = .TRUE.
   IF (0.3048 * 1.5 * C%FLAME_LENGTH .LE. BARRIER_WIDTH%R4(C%IX,C%IY,1)) THEN
      CHECK_BARRIER_BREACH = .FALSE.
   ENDIF 

! *****************************************************************************
END FUNCTION CHECK_BARRIER_BREACH
! *****************************************************************************

! *****************************************************************************
SUBROUTINE APPEND(DL2, IX, IY, T)
! *****************************************************************************
   TYPE(DLL), INTENT(INOUT) :: DL2
   INTEGER,  INTENT(IN)     :: IX, IY
   REAL,     INTENT(IN)     :: T

   TYPE(NODE), POINTER :: NP
   TYPE(NODE_WRAPPER), ALLOCATABLE :: TEMP(:)  ! Temporary array for resizing DWI_SU
   INTEGER :: N  ! Store the new count DWI_SU
   
! If the list is empty
IF (DL2%NUM_NODES == 0) THEN
   CALL INIT(DL2, IX, IY, T)

! Debug array allocation
   IF (ALLOCATED(DL2%NODE_POINTERS)) THEN
      DEALLOCATE(DL2%NODE_POINTERS)
   END IF  
   
   ALLOCATE(DL2%NODE_POINTERS(1))      ! DWI_SU
   DL2%NODE_POINTERS(1)%PTR => DL2%HEAD   ! DWI_SU
   RETURN
END IF
 
! Add new element ot the end
DL2%NUM_NODES = DL2%NUM_NODES + 1
N = SIZE(DL2%NODE_POINTERS)+1  ! Store the new count DWI_SU + YIREN DEBUG

NP => DL2%TAIL
ALLOCATE(DL2%TAIL)
DL2%TAIL%IX         =  IX
DL2%TAIL%IY         =  IY
DL2%TAIL%TIME_ADDED =  T

DL2%TAIL%IFBFM   =  FBFM%I2(IX,IY,1)

#ifdef _WUI
IF (USE_BLDG_SPREAD_MODEL) DL2%TAIL%IBLDGFM =  BLDG_FUEL_MODEL%I2(IX,IY,1)
#endif

DL2%TAIL%ADJ     =  ADJ%R4(IX,IY,1)
DL2%TAIL%TANSLP2 =  TANSLP2(MAX(MIN(NINT(SLP%R4(IX,IY,1)),90),0))

DL2%TAIL%PREV       => NP
DL2%TAIL%PREV%NEXT  => DL2%TAIL

#ifdef _WUI
! Resize NODE_POINTERS array dynamically DWI_SU
ALLOCATE(TEMP(N-1))
TEMP = DL2%NODE_POINTERS
DEALLOCATE(DL2%NODE_POINTERS)
ALLOCATE(DL2%NODE_POINTERS(N))
DL2%NODE_POINTERS(1:N-1) = TEMP
DEALLOCATE(TEMP) 

! Store the new node in the array DWI_SU
NULLIFY(DL2%NODE_POINTERS(N)%PTR)
DL2%NODE_POINTERS(N)%PTR => DL2%TAIL
#endif
! *****************************************************************************   
END SUBROUTINE APPEND
! *****************************************************************************

! *****************************************************************************
SUBROUTINE APPEND_TO_DYNAMIC_ARRAY(IX, IY, N_ROWS, DYNAMIC_ARRAY)
! *****************************************************************************

    INTEGER, INTENT(IN) :: IX, IY                         ! NEW_VALUES_TO_APPEND
    INTEGER, INTENT(INOUT) :: N_ROWS   
    REAL, ALLOCATABLE, INTENT(INOUT), DIMENSION(:,:) :: DYNAMIC_ARRAY  ! Dynamic 2D array to store IX and IY


    ! Local temporary array for resizing
    INTEGER, ALLOCATABLE, DIMENSION(:,:) :: TEMP_ARRAY

    ! Handle the case where the array is unallocated
    IF (N_ROWS .LE. 1) THEN
        ALLOCATE(DYNAMIC_ARRAY(1, 2))           ! Allocate first column
        DYNAMIC_ARRAY(1, 1) = IX
        DYNAMIC_ARRAY(1, 2) = IY
        N_ROWS = 1                              ! Set number of rows to 1
    ELSE
        ! Allocate a temporary array with one additional column
        ALLOCATE(TEMP_ARRAY(N_ROWS, 2))
        TEMP_ARRAY(1:N_ROWS-1, :) = DYNAMIC_ARRAY  ! Copy existing data
        TEMP_ARRAY(N_ROWS, 1) = IX           ! Add new IX value
        TEMP_ARRAY(N_ROWS, 2) = IY           ! Add new IY value

        ! Replace the old array with the resized one
        DEALLOCATE(DYNAMIC_ARRAY)
        ALLOCATE(DYNAMIC_ARRAY(N_ROWS, 2))
        DYNAMIC_ARRAY = TEMP_ARRAY
    END IF


! *****************************************************************************
END SUBROUTINE APPEND_TO_DYNAMIC_ARRAY
! *****************************************************************************

! *****************************************************************************
ELEMENTAL SUBROUTINE INIT(DL2, IX, IY, T)
! *****************************************************************************

TYPE(DLL), INTENT(INOUT) :: DL2
INTEGER, INTENT(IN)      :: IX, IY
REAL, INTENT(IN) :: T

ALLOCATE(DL2%HEAD)
DL2%TAIL => DL2%HEAD
DL2%TAIL%IX = IX
DL2%TAIL%IY = IY
DL2%TAIL%IFBFM      =  FBFM%I2(IX,IY,1)
DL2%TAIL%ADJ        =  ADJ%R4(IX,IY,1)
DL2%TAIL%TANSLP2    =  TANSLP2(MAX(MIN(NINT(SLP%R4(IX,IY,1)),90),0))
DL2%TAIL%TIME_ADDED =  T
DL2%NUM_NODES = 1

! *****************************************************************************
END SUBROUTINE INIT
! *****************************************************************************

! *****************************************************************************
SUBROUTINE DELETE_NODE(DL2, CURRENT)
! *****************************************************************************

TYPE(DLL), INTENT(INOUT) :: DL2
TYPE(NODE), POINTER, INTENT(INOUT) :: CURRENT
TYPE(NODE), POINTER :: NP

! IF (.NOT. ASSOCIATED(CURRENT)) THEN
!    WRITE(*,*) 'EXITING BECAUSE CURRENT NOT ASSOCIATED'
!    RETURN
! ENDIF

NP => CURRENT
CONTINUE

IF (ASSOCIATED(CURRENT%PREV) .AND. ASSOCIATED(CURRENT%NEXT)) THEN !Deleting intermediate node
   CURRENT%PREV%NEXT => CURRENT%NEXT
   CURRENT%NEXT%PREV => CURRENT%PREV
   CURRENT => CURRENT%PREV
CONTINUE

ELSE IF (ASSOCIATED(CURRENT%PREV) .AND. (.NOT. ASSOCIATED(CURRENT%NEXT))) THEN ! Deleting tail node
   CURRENT%PREV%NEXT => NULL()
   CURRENT => CURRENT%PREV
   DL2%TAIL => CURRENT
CONTINUE
ELSE IF (.NOT. ASSOCIATED(CURRENT%PREV) .AND. ASSOCIATED(CURRENT%NEXT)) THEN ! Deleting head node
   DL2%HEAD => CURRENT%NEXT
   CURRENT => DL2%HEAD
   CURRENT%PREV => NULL()
CONTINUE
!ELSE
!   CONTINUE
ENDIF

DEALLOCATE(NP)

DL2%NUM_NODES = DL2%NUM_NODES - 1

! *****************************************************************************
END SUBROUTINE DELETE_NODE
! ***************************************************************************** 

! *****************************************************************************
SUBROUTINE TIDY(DL2)
! *****************************************************************************

TYPE(DLL), INTENT(INOUT) :: DL2
TYPE(NODE), POINTER :: CURRENT, LAST

! INTEGER :: COUNT

! COUNT = 0 

IF (DL2%NUM_NODES .EQ. 1) THEN
   DEALLOCATE(DL2%HEAD)
ELSE
   CURRENT => DL2%HEAD
   DO
      LAST => CURRENT
      CURRENT => CURRENT%NEXT
      IF (ASSOCIATED(LAST)) THEN
         ! COUNT = COUNT + 1
         DEALLOCATE(LAST)
      END IF
      IF (ASSOCIATED(CURRENT, DL2%TAIL)) THEN
!         COUNT = COUNT + 1
         DEALLOCATE(CURRENT)
         EXIT
      ENDIF
   ENDDO
ENDIF
! *****************************************************************************
END SUBROUTINE TIDY
! *****************************************************************************

! *****************************************************************************
SUBROUTINE locate(xx,n,x,j)
! *****************************************************************************
! Given an array xx(1:n), and given a value x, returns a value j such that x is between
! xx(j) and xx(j+1). xx(1:n) must be monotonic, either increasing or decreasing. j=0
! or j=n is returned to indicate that x is out of range.

INTEGER j,n
REAL :: x,xx(n)
INTEGER jl,jm,ju
jl=0 !Initialize lower
ju=n+1 !and upper limits.
 10 if(ju-jl.gt.1)then !If we are not yet done,
       jm=(ju+jl)/2 !compute a midpoint,
       if((xx(n).ge.xx(1)).eqv.(x.ge.xx(jm)))then
          jl=jm !and replace either the lower limit
       else
          ju=jm !or the upper limit, as appropriate.
       endif
       goto 10 !Repeat until
    endif !the test condition 10 is satisfied.
      
if(x.eq.xx(1))then !Then set the output
   j=1
else if(x.eq.xx(n))then
   j=n-1
else
   j=jl
endif

! *****************************************************************************
END SUBROUTINE LOCATE
! *****************************************************************************

!******************************************************************************
SUBROUTINE SUNRISE_SUNSET_CALCS
!******************************************************************************
LOGICAL :: LEAPYEAR
INTEGER :: DAY_OF_YEAR, HOUR_OF_DAY
REAL :: DAYS_PER_YEAR, GAMMA, EQTIME, COSPIMTHETA, DECL, HA, HA_SUNRISE, LAT_RAD, LON_RAD, PHI, THETA, TST, &
        SUNRISE_MIN_UTC, SUNRISE_H_UTC, SUNSET_MIN_UTC, HA_SUNSET, SUNSET_H_UTC, LON_DEG, LAT_DEG

CALL XY_TO_LATLON(ASP%XLLCORNER, ASP%YLLCORNER, LAT_DEG, LON_DEG)

LON_RAD = LON_DEG * PI / 180.
LAT_RAD = LAT_DEG * PI / 180.

LEAPYEAR = .FALSE.
IF (MOD(CURRENT_YEAR,4) .EQ. 0) LEAPYEAR = .TRUE.
DAYS_PER_YEAR = 365.
IF (LEAPYEAR) DAYS_PER_YEAR = 366.

DAY_OF_YEAR = 1 + FLOOR(REAL(HOUR_OF_YEAR) / 24.)
HOUR_OF_DAY = HOUR_OF_YEAR - (DAY_OF_YEAR - 1) * 24
GAMMA = 2.0 * (PI/DAYS_PER_YEAR) * (DAY_OF_YEAR - 1)

EQTIME = 229.18 * ( 0.000075 + 0.001868*COS(GAMMA) - 0.032077*SIN(GAMMA) - 0.014615*COS(2.*GAMMA) - 0.040849*SIN(2.*GAMMA) )
DECL   = 0.006918 - 0.399912*COS(GAMMA) + 0.070257*SIN(GAMMA) - 0.006758*COS(2.*GAMMA) + 0.000907*SIN(2.*GAMMA) - 0.002697*COS(3.*GAMMA) + 0.00148*SIN(3.*GAMMA)

! Begin part not needed for sunrise / sunset calcs:
TST = REAL(HOUR_OF_DAY) * 60. + EQTIME + 4. * LON_DEG
HA = 0.25 * TST - 180.
PHI = ACOS(SIN(LAT_RAD)*SIN(DECL)+COS(LAT_RAD)*COS(DECL)*COS(HA*PI/180))
COSPIMTHETA = (SIN(LAT_RAD)*COS(PHI) - SIN(DECL)) / (COS(LAT_RAD)*SIN(PHI))
THETA = PI - ACOS(COSPIMTHETA)
! End part not needed for sunrise / sunset calcs

HA_SUNRISE = ACOS( COS(90.833*PI/180) / (COS(LAT_RAD)*COS(DECL)) -TAN(LAT_RAD)*TAN(DECL) )
SUNRISE_MIN_UTC = 720. - 4.*(LON_DEG + HA_SUNRISE*180./PI) - EQTIME
SUNRISE_H_UTC = SUNRISE_MIN_UTC / 60.

HA_SUNSET = -HA_SUNRISE
SUNSET_MIN_UTC = 720. - 4.*(LON_DEG + HA_SUNSET*180./PI) - EQTIME
SUNSET_H_UTC = SUNSET_MIN_UTC / 60.

SUNRISE_HOUR = SUNRISE_H_UTC ! Scope is global
SUNSET_HOUR  = SUNSET_H_UTC ! Scope is global

! *****************************************************************************
END SUBROUTINE SUNRISE_SUNSET_CALCS
!******************************************************************************

! *****************************************************************************
SUBROUTINE SHUTDOWN
! *****************************************************************************

INTEGER :: I, IERR
CHARACTER(4) :: FOUR
LOGICAL :: LOPEN
CHARACTER(400) :: FN

IF (NUM_TIME_AT_BURNED_ACRES .GT. 0) THEN
   DO I = 0, NPROC - 1
      WRITE(FOUR, '(I4.4)') I
      FN = TRIM(OUTPUTS_DIRECTORY) // 'burned-acres-timings_' // FOUR // '.csv'
      INQUIRE(UNIT=LUBAT+I,OPENED=LOPEN)
      IF (LOPEN) CLOSE(LUBAT+I) 
   ENDDO
ENDIF

IF (PROCESS_TIMED_LOCATIONS) THEN
   DO I = 0, NPROC - 1
      WRITE(FOUR, '(I4.4)') I
      FN = TRIM(OUTPUTS_DIRECTORY) // 'timed-locations-events_' // FOUR // '.csv'
      INQUIRE(UNIT=LUTASL+I,OPENED=LOPEN)
      IF (LOPEN) CLOSE(LUTASL+I)
   ENDDO
ENDIF

IF (NPROC .GT. 1) THEN
   CALL MPI_WIN_FREE(WIN_WS           )
   CALL MPI_WIN_FREE(WIN_WD           )
   CALL MPI_WIN_FREE(WIN_M1           )
   CALL MPI_WIN_FREE(WIN_M10          )
   CALL MPI_WIN_FREE(WIN_M100         )
   IF (USE_ERC) THEN
      CALL MPI_WIN_FREE(WIN_ERC       )
      CALL MPI_WIN_FREE(WIN_IGNFAC    )
   ENDIF
   CALL MPI_WIN_FREE(WIN_MLH           )
   CALL MPI_WIN_FREE(WIN_MLW           )
   CALL MPI_WIN_FREE(WIN_MFOL          )
   CALL MPI_WIN_FREE(WIN_ASP           )
   CALL MPI_WIN_FREE(WIN_CBH           )
   CALL MPI_WIN_FREE(WIN_CBD           )
   CALL MPI_WIN_FREE(WIN_CC            )
   CALL MPI_WIN_FREE(WIN_CH            )
   IF (MODE .NE. 2) CALL MPI_WIN_FREE(WIN_DEM)
   CALL MPI_WIN_FREE(WIN_FBFM          )
   CALL MPI_WIN_FREE(WIN_SLP           )
   CALL MPI_WIN_FREE(WIN_ADJ           )
   IF (MODE .NE. 2) CALL MPI_WIN_FREE(WIN_PHI0)
   CALL MPI_WIN_FREE(WIN_WAF           )
   CALL MPI_WIN_FREE(WIN_OMCOSSLPRAD   )
   CALL MPI_WIN_FREE(WIN_ISNONBURNABLE )

   IF (USE_POPULATION_DENSITY) CALL MPI_WIN_FREE(WIN_POPULATION_DENSITY)
   IF (USE_REAL_ESTATE_VALUE ) CALL MPI_WIN_FREE(WIN_REAL_ESTATE_VALUE )
   IF (USE_LAND_VALUE        ) CALL MPI_WIN_FREE(WIN_LAND_VALUE        )

   CALL MPI_WIN_FREE(WIN_STATS_X                         )
   CALL MPI_WIN_FREE(WIN_STATS_Y                         )
   CALL MPI_WIN_FREE(WIN_STATS_ASTOP                     )
   CALL MPI_WIN_FREE(WIN_STATS_TSTOP                     )   
   CALL MPI_WIN_FREE(WIN_STATS_SURFACE_FIRE_AREA         )
   CALL MPI_WIN_FREE(WIN_STATS_CROWN_FIRE_AREA           )
   CALL MPI_WIN_FREE(WIN_STATS_FIRE_VOLUME               )
   CALL MPI_WIN_FREE(WIN_STATS_AFFECTED_POPULATION       )
   CALL MPI_WIN_FREE(WIN_STATS_AFFECTED_REAL_ESTATE_VALUE)
   CALL MPI_WIN_FREE(WIN_STATS_AFFECTED_LAND_VALUE       )
   CALL MPI_WIN_FREE(WIN_STATS_FINAL_CONTAINMENT_FRAC    )
   CALL MPI_WIN_FREE(WIN_STATS_NEMBERS                   )
   CALL MPI_WIN_FREE(WIN_STATS_ICASE                     )
   CALL MPI_WIN_FREE(WIN_STATS_IWX_BAND_START            )
   CALL MPI_WIN_FREE(WIN_STATS_IWX_SERIAL_BAND           )
   CALL MPI_WIN_FREE(WIN_STATS_SIMULATION_TSTOP_HOURS    )
   CALL MPI_WIN_FREE(WIN_STATS_WALL_CLOCK_TIME           )
ENDIF

CALL MPI_BARRIER(MPI_COMM_WORLD, IERR)

! Dump timings:
CALL SYSTEM_CLOCK(IT_STOP)
TIMINGS(IRANK_HOST+1,1) = REAL(IT_STOP - IT_START) / REAL(CLOCK_COUNT_RATE)
CALL MPI_BARRIER(MPI_COMM_WORLD, IERR)
IF (DUMP_TIMINGS .AND. IRANK_HOST .EQ. 0) CALL WRITE_TIMINGS_TO_DISK

CALL MPI_BARRIER(MPI_COMM_WORLD, IERR)
IF (NPROC .GT. 1) CALL MPI_WIN_FREE(WIN_TIMINGS)

CALL MPI_BARRIER(MPI_COMM_WORLD, IERR)

CALL MPI_FINALIZE(IERR)

if (IRANK_WORLD .eq. 0 .and. CLEAN_SCRATCH) call execute_command_line("rm -f " // trim(SCRATCH) // "/*")

IF (IRANK_WORLD .EQ. 0) WRITE(*,*) 'End of simulation reached successfully. Shutting down.'

! *****************************************************************************
END SUBROUTINE SHUTDOWN
! *****************************************************************************

! *****************************************************************************
SUBROUTINE ERC_IGNITION_FACTOR (ERC, IGNFAC, IB1, IB2)
! *****************************************************************************

TYPE (RASTER_TYPE), INTENT(IN) :: ERC
TYPE (RASTER_TYPE) :: IGNFAC
INTEGER, INTENT(IN) :: IB1, IB2
INTEGER :: IBAND, IROW, ICOL
REAL :: ERCVAL, EXPONENT

IGNFAC%BYTEORDER     = ERC%BYTEORDER
IGNFAC%LAYOUT        = ERC%LAYOUT
IGNFAC%NROWS         = ERC%NROWS
IGNFAC%NCOLS         = ERC%NCOLS
IGNFAC%NBANDS        = ERC%NBANDS
IGNFAC%NBITS         = ERC%NBITS
IGNFAC%BANDROWBYTES  = ERC%BANDROWBYTES
IGNFAC%TOTALROWBYTES = ERC%TOTALROWBYTES
IGNFAC%PIXELTYPE     = ERC%PIXELTYPE
IGNFAC%ULXMAP        = ERC%ULXMAP
IGNFAC%ULYMAP        = ERC%ULYMAP
IGNFAC%XDIM          = ERC%XDIM
IGNFAC%YDIM          = ERC%YDIM
IGNFAC%NODATA_VALUE  = ERC%NODATA_VALUE
IGNFAC%CELLSIZE      = ERC%CELLSIZE
IGNFAC%XLLCORNER     = ERC%XLLCORNER
IGNFAC%YLLCORNER     = ERC%YLLCORNER

IF (ERC_IS_PLIGNRATE) THEN
   DO IBAND = IB1, IB2
      DO IROW = 1, ERC%NROWS
      DO ICOL = 1, ERC%NCOLS
         IGNFAC%R4(ICOL,IROW,IBAND) = MAX(PLIGNRATE_MIN, ERC%R4(ICOL,IROW,IBAND))
      ENDDO
      ENDDO
   ENDDO
ELSE
   DO IBAND = IB1, IB2
      DO IROW = 1, ERC%NROWS
      DO ICOL = 1, ERC%NCOLS
         ERCVAL = MIN( MAX(ERC%R4(ICOL,IROW,IBAND),0.),100.0)
         EXPONENT = 0.02768 * ERCVAL - 0.2333
         IGNFAC%R4(ICOL,IROW,IBAND) = MIN( MAX ( 2.92E-3 * 10**EXPONENT, 1E-3 ), 1E0)
      ENDDO
      ENDDO
   ENDDO
ENDIF

! *****************************************************************************
END SUBROUTINE ERC_IGNITION_FACTOR
! *****************************************************************************

! *****************************************************************************
REAL FUNCTION ERFINV(X)
! *****************************************************************************

REAL, INTENT(IN) :: X
REAL, PARAMETER :: HALFSQRTPI = 0.88622692545
REAL :: X2, X4, X6, X8
REAL, PARAMETER :: C1 = 1.000000000
REAL, PARAMETER :: C2 = 0.261799388
REAL, PARAMETER :: C3 = 0.143931731
REAL, PARAMETER :: C4 = 0.097663620
REAL, PARAMETER :: C5 = 0.073299079
REAL, PARAMETER :: C6 = 0.058372501

!ERFINV = 0.5*SQRT(PI)*(C1*X + C2*X**3 + C3*X**5 + C4*X**7 + C5*X**9 + C6*X**11) 

X2  = X  * X
X4  = X2 * X2
X6  = X4 * X2
X8  = X6 * X2
!X10 = X8 * X2

!ERFINV = HALFSQRTPI * X * (C1 + C2*X2 + C3*X4 + C4*X6 + C5*X8 + C6*X10) 

ERFINV = HALFSQRTPI * X * (C1 + C2*X2 + C3*X4 + C4*X6 + C5*X8) 

! *****************************************************************************
END FUNCTION ERFINV
! *****************************************************************************

! *****************************************************************************
REAL FUNCTION ISF(FUEL, RSF, CF)
! *****************************************************************************

REAL,INTENT(IN) :: RSF, CF
INTEGER*2, intent(in) :: FUEL
ISF = log(max(0.01,1-(RSF/(FUEL_MODEL_TABLE_FBP(FUEL)%a*CF))**(1/FUEL_MODEL_TABLE_FBP(FUEL)%c)))/(-FUEL_MODEL_TABLE_FBP(FUEL)%b)

! *****************************************************************************
END FUNCTION ISF
! *****************************************************************************

! *****************************************************************************
RECURSIVE REAL FUNCTION RSI(FUEL, ISI, CF) result(rsi_val)
! *****************************************************************************

REAL,INTENT(IN) :: ISI, CF
INTEGER*2, intent(in) :: FUEL
REAL RSI_100, PDF

PDF = mod(FUEL,100)/100.0
IF (FUEL .eq. 40 .or. FUEL .eq. 60 .or. (FUEL .ge. 400 .and. FUEL .le. 499) .or. (FUEL .ge. 600 .and. FUEL .le. 699)) then ! M1
   rsi_val = PDF * RSI(2_2, ISI, CF) + (1-PDF)*RSI(11_2, ISI, CF)
ELSE IF (FUEL .eq. 50 .or. (FUEL .ge. 500 .and. FUEL .le. 599)) then ! M2
   rsi_val = PDF * RSI(2_2, ISI, CF) + 0.2*(1-PDF)*RSI(11_2, ISI, CF)
ELSE IF (FUEL .eq. 70 .or. FUEL .eq. 90 .or. (FUEL .ge. 700 .and. FUEL .le. 799) .or. (FUEL .ge. 900 .and. FUEL .le. 999)) THEN ! M3
   RSI_100 = 120*(1-exp(-0.0572*ISI))**1.4
   rsi_val = PDF*RSI_100 + (1-PDF)*RSI(11_2, ISI, CF)
ELSE IF (FUEL .eq. 80 .or. (FUEL .ge. 800 .and. FUEL .le. 899)) THEN ! M4
   RSI_100 = 100*(1-exp(-0.0404*ISI))**1.48
   rsi_val = PDF*RSI_100 + 0.2*(1-PDF)*RSI(11_2, ISI, CF)
ELSE
   rsi_val = FUEL_MODEL_TABLE_FBP(FUEL)%a*(1-exp(-FUEL_MODEL_TABLE_FBP(FUEL)%b*ISI))**FUEL_MODEL_TABLE_FBP(FUEL)%c*CF
ENDIF


! *****************************************************************************
END FUNCTION RSI
! *****************************************************************************

! *****************************************************************************
RECURSIVE REAL FUNCTION SFC(FUEL, FFMC, BUI) result(out)
! *****************************************************************************
INTEGER*2, INTENT(IN) :: FUEL 
REAL, INTENT(IN) :: FFMC, BUI

REAL :: PC
PC = 0

select case (FUEL)
   case (1)
      if (FFMC .gt. 84) THEN
         out = 0.75 + 0.75*sqrt((1-exp(-0.23*(FFMC-84))))
      else
         out = 0.75 - 0.75*sqrt((1-exp(-0.23*(84-FFMC))))
      endif
   case (2, 70, 80, 90, 700:1000)
      out = 5.0*(1-exp(-0.0115*BUI))
   case (3, 4)
      out = 5.0*(1-exp(-0.0164*BUI))**2.24
   case (5, 6)
      out = 5.0*(1-exp(-0.0149*BUI))**2.48
   case (7)
      out = max(0.0,2*(1-exp(-0.104*(FFMC-70))))+1.5*(1-exp(-0.0201*BUI))
   case (11, 12, 13)
      out = 1.5*(1-exp(-0.0183*BUI))
   case (40, 50, 60, 400:699)
      PC = mod(FUEL, 100)/100.0
      out = PC * SFC(2_2,FFMC,BUI) + (1 - PC) * SFC(11_2,FFMC,BUI)
   case(31,32,33)
      out = 0.35
   case (21)
      out = max(0.0,4*(1-exp(-0.034*BUI)))+4.0*(1-exp(-0.025*BUI))
   case (22)
      out = max(0.0,10*(1-exp(-0.013*BUI)))+6.0*(1-exp(-0.060*BUI))
   case (23)
      out = max(0.0,12*(1-exp(-0.0166*BUI)))+20.0*(1-exp(-0.021*BUI))
   case Default
      out = 0
end select

! *****************************************************************************
END FUNCTION SFC
! *****************************************************************************

! *****************************************************************************
REAL FUNCTION BUI(day_of_weather, month_of_weather)
! *****************************************************************************
REAL :: V, DC, Q_prev, Q_RT, DC_RT, K, DMC, b, Pe, M_prev, M_RT, DMC_RT
INTEGER, intent(in) :: day_of_weather, month_of_weather
! ------------- DROUGHT CODE -------------------------------

V = max(0.0,0.36*(max(0.0,T_midday(day_of_weather) + 2.8)) + Lf(month_of_weather))
if (precip(day_of_weather) .le. 2.8) THEN
   DC = DC_prev + 0.5 * V
ELSE
   Q_prev = 800 * exp(-DC_prev/400)
   Q_RT = max(0.0,Q_prev + 3.937*(0.83*precip(day_of_weather) -1.27))
   DC_RT = 400*log(800/Q_RT)
   DC=DC_RT + 0.5 * V
ENDIF

! -------------- DROUGHT MOISTURE CODE ---------------------

K = 1.894*(max(0.0,T_midday(day_of_weather) + 1.1))*(100 - H_midday(day_of_weather))*Le(month_of_weather)*10.0**(-6.0)
if (precip(day_of_weather) .le. 1.5) THEN
   DMC = DMC_prev + 100 * K
ELSE
   if (DMC_prev .le. 33) THEN
      b=100/(0.5+0.3*DMC_prev)
   ELSEIF (DMC_prev .le. 65) THEN
      b=14-1.3*log(DMC_prev)
   ELSE
      b=6.2*log(DMC_prev)-17.2
   ENDIF
   Pe = 0.92* precip(day_of_weather) - 1.27
   M_prev = 20 + exp(5.6348-DMC_prev/43.43)
   M_RT = M_prev + (1000*Pe)/(48.77+b*Pe)
   DMC_RT = max(0.0,244.72-43.43*log(M_RT-20))
   DMC=DMC_RT + 100 * K
ENDIF

DMC_prev = DMC
DC_prev = DC

! -------------- BUILD-UP INDEX ---------------------

BUI = 0.8*DMC*DC/(DMC+0.4*DC)
! *****************************************************************************
END FUNCTION BUI
! *****************************************************************************

! *****************************************************************************
CHARACTER(16) FUNCTION HOUR_OF_YEAR_TO_TIMESTAMP(YEAR, HOUR_OF_YEAR)
! *****************************************************************************

INTEGER, INTENT(IN) :: YEAR
INTEGER, INTENT(IN) :: HOUR_OF_YEAR
INTEGER :: I, HOUR_OF_MONTH, DAY_OF_MONTH, HOUR_OF_DAY, MONTH
INTEGER, DIMENSION(12) :: DAYS_PER_MONTH=(/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /), &
                          MONTH_BEGIN_HOUR, MONTH_END_HOUR
LOGICAL :: FOUND
CHARACTER(16) :: TIMESTAMP

IF ( (MODULO(YEAR,4) .EQ. 0 .AND. MODULO(YEAR,100) .NE. 0) .OR. MODULO(YEAR,400) .EQ. 0 ) DAYS_PER_MONTH(2)=29

MONTH_BEGIN_HOUR(1) = 0
MONTH_END_HOUR  (1) = DAYS_PER_MONTH(1) * 24
DO I = 2, 12
   MONTH_BEGIN_HOUR(I) = MONTH_END_HOUR  (I-1)
   MONTH_END_HOUR  (I) = MONTH_BEGIN_HOUR(I)   + DAYS_PER_MONTH(I) * 24
ENDDO

FOUND = .FALSE.
MONTH = 1
DO I = 1, 12
   IF (FOUND) CYCLE
   IF (HOUR_OF_YEAR .GE. MONTH_BEGIN_HOUR(I) .AND. HOUR_OF_YEAR .LT. MONTH_END_HOUR(I) ) THEN
      FOUND = .TRUE.
      MONTH = I
   ENDIF
ENDDO

HOUR_OF_MONTH = HOUR_OF_YEAR - MONTH_BEGIN_HOUR(MONTH)
DAY_OF_MONTH  = 1 + HOUR_OF_MONTH / 24
HOUR_OF_DAY   = MODULO( HOUR_OF_MONTH,24)

WRITE( TIMESTAMP,  '(I4.4,"-",I2.2,"-",I2.2," ",I2.2,":00")') YEAR, MONTH, DAY_OF_MONTH, HOUR_OF_DAY

HOUR_OF_YEAR_TO_TIMESTAMP = TIMESTAMP

! *****************************************************************************
END FUNCTION HOUR_OF_YEAR_TO_TIMESTAMP
! *****************************************************************************

! *****************************************************************************
SUBROUTINE XY_TO_LATLON(X, Y, LAT, LON)
! *****************************************************************************
REAL, INTENT(IN)  :: X, Y              ! projected coordinates (meters)
REAL, INTENT(OUT) :: LAT, LON          ! output lat, lon in degrees

INTEGER :: Z
CHARACTER(256) :: SHELLSTR, TMPIN, TMPOUT
INTEGER :: LUIN, LUOUT, IOS
character(len=32) :: istr

! Create simple temp file names (you can do something fancier if needed)
write(istr,'(I0)') IRANK_WORLD
TMPIN  = TRIM(SCRATCH) // 'gdal_xy_to_ll_in_'//trim(istr)//'.txt'
TMPOUT = TRIM(SCRATCH) // 'gdal_xy_to_ll_out_'//trim(istr)//'.txt'

! 1. Write (x, y) to input file for gdaltransform
OPEN(NEWUNIT=LUIN, FILE=TMPIN, STATUS='REPLACE', ACTION='WRITE', IOSTAT=IOS)
IF (IOS /= 0) THEN
   WRITE(*,*) 'Error opening temp input file for gdaltransform, IOSTAT=', IOS
   RETURN
END IF
WRITE(LUIN,'(F24.8,1X,F24.8)') X, Y
CLOSE(LUIN)

! 2. Build gdaltransform command:
!    gdaltransform -s_srs SRC_SRS -t_srs EPSG:4326 < TMPIN > TMPOUT
SHELLSTR = TRIM(PATH_TO_GDAL) // 'gdaltransform -s_srs "' // TRIM(A_SRS) // '"' // &
            ' -t_srs EPSG:4326 < ' // TRIM(TMPIN) // ' > ' // TRIM(TMPOUT)

! WRITE(*,*) 'Running: ', TRIM(SHELLSTR)
CALL EXECUTE_COMMAND_LINE(TRIM(SHELLSTR), EXITSTAT=IOS)

IF (IOS /= 0) THEN
   WRITE(*,*) 'gdaltransform failed, EXITSTAT=', IOS
   RETURN
END IF

! 3. Read lon, lat, z from output file
OPEN(NEWUNIT=LUOUT, FILE=TMPOUT, STATUS='OLD', ACTION='READ', IOSTAT=IOS)
IF (IOS /= 0) THEN
   WRITE(*,*) 'Error opening temp output file from gdaltransform, IOSTAT=', IOS
   RETURN
END IF

! gdaltransform outputs: lon lat z
READ(LUOUT,*,IOSTAT=IOS) LON, LAT, Z
CLOSE(LUOUT)

IF (IOS .NE. 0) THEN
   WRITE(*,*) 'Error reading gdaltransform output, IOSTAT=', IOS
   RETURN
END IF

! 4. (Optional) clean up temp files
CALL EXECUTE_COMMAND_LINE('rm -f ' // TRIM(TMPIN)  // ' ' // TRIM(TMPOUT))
! *****************************************************************************
END SUBROUTINE XY_TO_LATLON
! *****************************************************************************

! *****************************************************************************
subroutine read_geotiff_meta_gdalinfo()
   character(len=1024) :: cmd, line
   character(len=256)  :: tmpfile, tmpfile_epsg, tempFilename, istr
   integer :: iu, ios
   integer :: ncols, nrows
   real(8) :: x0, y0, dx, dy
   integer :: epsg
   logical :: is_metre

   ! Defaults
   ncols = -1
   nrows = -1
   x0 = 0d0
   y0 = 0d0
   dx = 0d0
   dy = 0d0
   epsg = -1
   is_metre = .false.

   write(istr,'(I0)') IRANK_WORLD
   tmpfile      = trim(SCRATCH) // '/' // '._gdalinfo_tmp_'//trim(istr)//'.txt'
   tmpfile_epsg = trim(SCRATCH) // '/' // '._gdalsrsinfo_tmp_'//trim(istr)//'.txt'

   tempFilename = trim(ASP_FILENAME)
   if (USE_TILED_IO) then
      tempFilename = trim(tempFilename) // '_1_1.bsq'
   else
      tempFilename = trim(tempFilename) // '.tif'
   endif

   call read_basic_raster_meta()
   call read_epsg_with_gdalsrsinfo()

   if (.not. is_metre) then
      error stop 'DEM CRS does not appear to use metre linear units.'
   endif

   if (ncols <= 0 .or. nrows <= 0) error stop 'Could not parse raster Size is ...'
   if (dx == 0d0 .or. dy == 0d0)   error stop 'Could not parse Pixel Size ...'

   ANALYSIS_CELLSIZE  = abs(dx)
   ANALYSIS_XLLCORNER = x0
   ANALYSIS_YLLCORNER = y0 + dy * dble(nrows)

   if (epsg > 0) then
      A_SRS = 'EPSG:' // trim(int_to_str(epsg))
   else
      A_SRS = 'UNKNOWN'
   end if

contains

   subroutine read_basic_raster_meta()
      write(cmd,'(a)') 'gdalinfo "' // trim(FUELS_AND_TOPOGRAPHY_DIRECTORY) // '/' // &
                       trim(tempFilename) // '" > "' // trim(tmpfile) // '"'
      call execute_command_line(trim(cmd))

      open(newunit=iu, file=trim(tmpfile), status='old', action='read', iostat=ios)
      if (ios /= 0) error stop 'Failed to open gdalinfo output file.'

      do
         read(iu, '(A)', iostat=ios) line
         if (ios /= 0) exit

         call parse_size
         call parse_origin
         call parse_pixel_size
         call parse_is_metre_units
      end do

      close(iu)
   end subroutine read_basic_raster_meta

   subroutine read_epsg_with_gdalsrsinfo()
      integer :: p
      character(len=1024) :: text

      write(cmd,'(a)') 'gdalsrsinfo -o epsg "' // trim(FUELS_AND_TOPOGRAPHY_DIRECTORY) // '/' // &
                     trim(tempFilename) // '" > "' // trim(tmpfile_epsg) // '"'
      call execute_command_line(trim(cmd))

      open(newunit=iu, file=trim(tmpfile_epsg), status='old', action='read', iostat=ios)
      if (ios /= 0) return

      do
         read(iu, '(A)', iostat=ios) line
         if (ios /= 0) exit

         line = adjustl(line)

         p = index(line, 'EPSG:')
         if (p > 0) then
            text = adjustl(line(p+5:))
            read(text, *, iostat=ios) epsg
            if (ios /= 0) epsg = -1
            exit
         end if
      end do

      close(iu)
   end subroutine read_epsg_with_gdalsrsinfo

   pure logical function contains_ci(s, pat)
      implicit none
      character(len=*), intent(in) :: s, pat
      character(len=len(s))   :: sl
      character(len=len(pat)) :: pl
      integer :: i

      sl = s
      pl = pat

      do i = 1, len(sl)
         if (iachar(sl(i:i)) >= iachar('A') .and. iachar(sl(i:i)) <= iachar('Z')) then
            sl(i:i) = achar(iachar(sl(i:i)) + 32)
         end if
      end do

      do i = 1, len(pl)
         if (iachar(pl(i:i)) >= iachar('A') .and. iachar(pl(i:i)) <= iachar('Z')) then
            pl(i:i) = achar(iachar(pl(i:i)) + 32)
         end if
      end do

      contains_ci = index(sl, pl) > 0
   end function contains_ci

   subroutine parse_is_metre_units
      if (contains_ci(line, 'linear units:') .and. contains_ci(line, 'metre')) then
         is_metre = .true.
      else if (contains_ci(line, 'lengthunit["metre"')) then
         is_metre = .true.
      else if (contains_ci(line, 'unit["metre"')) then
         is_metre = .true.
      end if
   end subroutine parse_is_metre_units

   subroutine parse_size
      integer :: p
      character(len=256) :: rest

      p = index(line, 'Size is')
      if (p > 0) then
         rest = adjustl(line(p+len('Size is'):))
         read(rest, *, iostat=ios) ncols
         if (ios == 0) then
            p = index(rest, ',')
            if (p > 0) read(rest(p+1:), *, iostat=ios) nrows
         end if
      end if
   end subroutine parse_size

   subroutine parse_origin
      integer :: p1, p2
      character(len=256) :: inside

      p1 = index(line, 'Origin = (')
      if (p1 > 0) then
         p1 = p1 + len('Origin = (')
         p2 = index(line(p1:), ')')
         if (p2 > 0) then
            inside = line(p1:p1+p2-2)
            read(inside, *, iostat=ios) x0, y0
         end if
      end if
   end subroutine parse_origin

   subroutine parse_pixel_size
      integer :: p1, p2
      character(len=256) :: inside

      p1 = index(line, 'Pixel Size = (')
      if (p1 > 0) then
         p1 = p1 + len('Pixel Size = (')
         p2 = index(line(p1:), ')')
         if (p2 > 0) then
            inside = line(p1:p1+p2-2)
            read(inside, *, iostat=ios) dx, dy
         end if
      end if
   end subroutine parse_pixel_size

   pure function int_to_str(i) result(s)
      integer, intent(in) :: i
      character(len=:), allocatable :: s
      character(len=32) :: buf
      write(buf, '(I0)') i
      s = trim(buf)
   end function int_to_str

end subroutine read_geotiff_meta_gdalinfo
! *****************************************************************************

! *****************************************************************************
END MODULE ELMFIRE_SUBS
! *****************************************************************************