      MODULE GLOBALS
      SAVE

      CHARACTER*120 dfile ! base name of project (includes .cmn)
      INTEGER IQUOTE
      CHARACTER*1 QUOTE
      INTEGER S_OUT ! Summary flag: 0 - basic summary, 1 - detailed summary

      INTEGER spflag ! 0 == SP extension not used, 1 == used.
      INTEGER isuply ! water supply flag: 0 == don't use water supply, 
                                ! 1 == use water supply
      INTEGER ETFLAG(1:12) ! ET Method:
!                    1 - Blaney-Criddle (SCS BC Original and Enhanced) 
!                    2 - Reference Equation  (Grass and Alfalfa Based)
!                    3 - Other Uses
!                    4 - Kimberly Penman Data for the South Platte
!                    5 - Calibrated Blaney-Criddle
!                    6 - ASCE
!                    7 - Hargreaves
!                    8 - Pochop
!                    9 - User-Supplied ET
!                   10 - Penman 1948 version
!          FLAG = 0 = do not compute
!          FLAG = 1 = compute

      LOGICAL hasDailyMethod ! If there is a daily method, this will be true.  This is used when reading surface water data since only daily or monthly data is stored.
      LOGICAL isDailyMethod ! If the current ET method is daily, this will be true.

      INTEGER RN_XCO ! Effective rainfall Method: 1 - SCS method, 2 - usbr mehod
      INTEGER RN_REF ! 1 - a max effective rainfall is specified 
!                      2 - factor x total rainfall
!                      3 - curve number method
      INTEGER RCHGFLG  ! Recharge Flag
!            1 - Calculate recharge from DP & RO
!            0 - Do NOT calculate recharge from DP & RO

      INTEGER ERainSM ! Use Excess Effective Rainfall in the Soil Moisture
                                ! 1 - Yes, 0 - No
      REAL APD ! Application depth (only for SCS)

      REAL Sr_Inc, Sr_tot ! Sr Annual Increase
      LOGICAL useCropMode ! Elgaali's ET mode

      CHARACTER*120 TITLE(6) ! Title of project
      INTEGER FLEN ! Length of file name
      INTEGER nbasin ! Number of basins in project
      INTEGER N_STA ! Number of weather stations
      INTEGER NYR1 ! Simulation start year
      INTEGER NYR2 ! Simulation end year
      INTEGER GNYR1 ! Project dataet start year
      INTEGER GNYR2 ! Project dataset end year
      INTEGER NYRS ! Number of simulation years
      INTEGER GNYRS ! Number of dataset years
      INTEGER PROJTYPE ! The method used to project synthesized data (see summary.f)
      INTEGER HISTYR2 ! The last year of historical data; the rest of the data
      INTEGER INCOMPLETE_MONTH ! When using an incomplete last year, this is the month of the year.
      INTEGER INCOMPLETE_DAY ! When using an incomplete last year, this is the day of the year.
      INTEGER N_ET_M  ! Number of ET Methods
      PARAMETER (N_ET_M = 10)
   
!       is projected
      CHARACTER*200 PROJ_DETAILS ! How the projected data was calculated.

      LOGICAL BC_USE_ELEV ! Use BC elevation offsets?
      LOGICAL CBC_USE_REF_KC ! Use reference Kc instead of BC coefficients?
      LOGICAL BC_KC_OVERRIDES_KT ! Does the BC coefficient represent Kt * Kc?
      REAL BC_ELEV_PERC(12) ! Monthly elevation offsets
      REAL BC_ELEV_BASE ! Elevation at which offsets were calibrated.
      REAL RPARA(3) ! Rainfall Parameters
      REAL DP_ROPerc(12) ! Monthly runoff/deep perc split.
      REAL, ALLOCATABLE :: SMEF(:) ! Size is nyrs; carry over soil moisture coefficient (fraction)
      REAL, ALLOCATABLE :: WBU(:,:) ! Size is maxparcel, nyrs; I don't know what this does.
      REAL, ALLOCATABLE :: CUHG(:,:) ! Size is maxparcel, nyrs; I don't know what this does.

!     Basin data.
      INTEGER, ALLOCATABLE :: TYPOUT(:) ! Size NBASIN; flag indicating 
                                ! output mode for each basin.
      INTEGER INCH              ! CU is in inches if set to 1
      CHARACTER*60, ALLOCATABLE :: BAS_ID(:) ! Size NBASIN; name of basin.
      REAL, ALLOCATABLE :: BELEV(:) ! Size NBASIN, elevation of basin
      REAL, ALLOCATABLE :: BLAT(:) ! Size NBASIN, latitude of basin
      REAL, ALLOCATABLE :: BLONG(:) ! Size NBASIN, longitude of basin
      REAL, ALLOCATABLE :: WWS(:,:), WRS(:,:) ! Size N_STA; weather stations
                          !  weight matrix for temp and precip, respectively.
      INTEGER MAXPARCEL ! The largest number of fields in all the basins.
      INTEGER MAXWELL ! The largest number of wells in all the basins.
      INTEGER MAXDITCH ! The largest number of ditches in all the basins.
      INTEGER, ALLOCATABLE :: NPARCE(:)    ! Size NBASIN, this has the augmented number of parcels (winter wheat will add an additional spring wheat field).
      INTEGER n_wheat_parcel
      INTEGER, ALLOCATABLE :: NPARCE_ORI(:) ! Size NBASIN, number of "actual" fields before adding winter wheat spring period parcels.
      INTEGER, ALLOCATABLE :: CROP_KEY(:,:) ! Size NBASIN, MAXPARCEL; crop ID number for each field.
      INTEGER, ALLOCATABLE :: SOIL_KEY(:,:) ! Size NBASIN, MAXPARCEL; soil ID number for each field.
      REAL, ALLOCATABLE :: AEFF(:,:,:) ! Size NBASIN, MAXPARCEL, 12 months; field application efficiency.
      REAL, ALLOCATABLE :: AREA(:,:,:) ! Size NBASIN, MAXPARCEL; area of the field in acres.
      REAL, ALLOCATABLE :: PJAREA(:) ! Size is nyears; the total area of the project for that year.
      REAL, ALLOCATABLE :: TMEAN3(:,:,:), RNTOT3(:,:,:) ! Size nbasin, nyrs, 12;
      REAL, ALLOCATABLE :: TMIN3(:,:,:), TMAX3(:,:,:) ! Size nbasin, nyrs, 12;
           ! min and max temp for a basin in a given year and month.
      REAL, ALLOCATABLE :: TRNG_C(:,:,:) ! Range in temperature - Size nbasin, nyrs, 12;

      REAL, ALLOCATABLE :: T28(:,:,:),T32(:,:,:) ! Size nbasin, nyrs, 2; spring (1) and fall (2) frost dates for each basin each year.
      REAL, ALLOCATABLE :: ETO(:,:,:), ETR(:,:,:) ! Size nbasin, nyrs, 366; Penman ETs for the basin in the given year and day.
      REAL, ALLOCATABLE :: ETC(:,:,:), ETW(:,:,:) ! Size nbasin, nyrs, 366; Penman ETs for the basin in the given year and day.
      REAL ETC_Y(366), ETW_Y(366) ! Size nbasin, nyrs, 366; crop mode ETs for the basin in the given year and day.

      INTEGER, ALLOCATABLE :: fbegmo(:,:), fendmo(:,:) ! Size is nbasins, nyrs; the beginning and ending month of the harvest for the farm.  If ET method daily, then only the day is used.
      INTEGER, ALLOCATABLE :: fbegda(:,:), fendda(:,:) ! Size is nbasins, nyrs; the beginning and ending day of the harvest for the farm.
      REAL PROFSSIM ! Portion of profile full at start of simulation
      INTEGER, ALLOCATABLE :: PROFLAG(:) ! Size is nyears; Soil moisture mode =
!              0 = compute portion of prof. full based on winter carry over.
!              1 = user set the soil moisture at the beginning of each season to be a specified number (profssea).
!              2 = use user defined portion of prof. full @ end of season
!              3 = use both.
      REAL, ALLOCATABLE :: PROFSSEA(:) ! Size is nyrs; portion of profile full at start of season
      REAL, ALLOCATABLE :: PROFEND(:) ! Size is nyrs; portion of profile full at end of season
      INTEGER, ALLOCATABLE :: maxcov(:) ! Size is maxparcels; date of max coverage for the parcel.
      INTEGER, ALLOCATABLE :: npart(:) ! Size is maxparcels; number of days in beginning month in spring
      REAL, ALLOCATABLE :: incrz(:) ! Size is maxparcel; increase in root zone?
      REAL, ALLOCATABLE :: incst(:) ! Size is maxparcel; increase in soil storage?
      
      REAL, ALLOCATABLE :: SPRINKLER(:,:) ! Size is nbasins, maxparcels; 1 = field uses a sprinkler (or otherwise uses sprinkler spray loss), otherwise 0.
      REAL, ALLOCATABLE :: INC_IWR(:) ! Size is nbasins; increase in CU due to sprinkler spray loss
      REAL, ALLOCATABLE :: CUSHORT(:) ! Size is nbasins; shortage for alfalfa/pasture
      INTEGER, ALLOCATABLE :: FARMSDF(:)  ! Size is nbasins; SDF associated with the farm.

!     Crop data
      INTEGER N_CROPS ! Total number of crops
      INTEGER N_SOILS ! Total number of soils

!     The following all have size N_CROPS:
      INTEGER, ALLOCATABLE :: CROP_TYPE(:) ! Crop classification, either 0 (Alfalfa), 1 (perenial), 2 (annual), or 3 (UnknownCropType)
      INTEGER, ALLOCATABLE :: SUB_CROP_TYPE(:) ! Crop classification, either 0 (WinterWheat), 1 (SpringPeriodOfWinterWheat), 2 (KentuckyBluegrass), 3 (PastureGrass), or 4 (UnknownSubCropType)
      INTEGER, ALLOCATABLE :: GDATE1(:), GDATE2(:) ! Planting date of crop
      INTEGER, ALLOCATABLE :: GDATE3(:), GDATE4(:) ! Harvest date of crop.
      INTEGER, ALLOCATABLE :: GDATE5(:) ! Days to full cover
      INTEGER, ALLOCATABLE :: GDATES(:) ! Length of season
      REAL, ALLOCATABLE :: TMOIS1(:),TMOIS2(:) ! Planting temp and frost temp
      REAL, ALLOCATABLE :: MAD(:) ! Maximum allowable soil depletion
      REAL, ALLOCATABLE :: IRZ(:) ! Initial root zone
      REAL, ALLOCATABLE :: FRZ(:) ! Max root
      REAL, ALLOCATABLE :: RZ(:) ! Root depth
      INTEGER, ALLOCATABLE :: TFLG1(:),TFLG2(:) ! Spring and fall frost method to use
      INTEGER, ALLOCATABLE :: CUT2(:),CUT3(:) ! Alfalfa second and third cutting days
!     The following all have size N_SOILS:
      REAL, ALLOCATABLE :: AWC(:) ! Average water holding capacity , size is N_SOILS


      REAL, ALLOCATABLE :: C_AREA(:,:) ! Size is nyrs, DIM_CI; total area of each crop type.
      REAL, ALLOCATABLE :: T_AREA(:,:) ! Size is nbasins, nyrs; total area of each basin.
      INTEGER naccum(12) ! accumulative days to midpoint of month
      INTEGER nperct(12) ! naccum/growing season length

!     BC calibration coefficients
      REAL, ALLOCATABLE :: CBC(:,:) ! Calibrated Blaney-Criddle Monthly Coefficients.  Size is ncrops, months

!     Used in Penman-Monteith
      REAL KCDAY(40,33), KCB(40,33) ! Crop period, crop coefficient
      INTEGER ETMETH(40) ! alfalfa (=1) or grass

!     Used for Growing degree days
      LOGICAL HAS_GDD
      INTEGER USE_GDD(40) ! If true, crop will use growing degree days.
      INTEGER GDD_TEMP(40, 3) ! Crop ID, base, min, max temps
      REAL GDD_COEFFS(40, 7) ! Crop ID, coeffiecients for GDD 6th-order function, g + g1 + g2^2 + g3^3 etc

!     Weather data
!     All sizes are N_STA, one for each weather station.
      CHARACTER*80, ALLOCATABLE :: STNAME(:) ! name of the weather station
      REAL, ALLOCATABLE :: SLONG(:),SLAT(:),ELEV(:) ! longitude, latitude, elevation
      REAL, ALLOCATABLE :: ZH(:),ZM(:) ! height of temp and wind measurements respectively 
      REAL PCLITE(12) ! monthly percent daylight hours, see dayhrs.f

!     Blaney-Criddle crop data
      INTEGER AKCLEN    ! annual crops
      PARAMETER (AKCLEN = 21)
      INTEGER PKCLEN    ! perrenial crops
      PARAMETER (PKCLEN = 25)
      INTEGER, ALLOCATABLE :: nckca(:,:), nckcp(:,:) ! Crop period, n_crops by [ap]kclen
      REAL, ALLOCATABLE :: ckcp(:,:), ckca(:,:) ! Crop coefficient, n_crops by [ap]kclen
      LOGICAL IGNORE_FROST_DATES ! If true, then don't consider frost dates when calculating planting temperature.

      REAL PI
      PARAMETER (PI = 3.14159)

!     Water supply variables
      REAL, ALLOCATABLE :: CEFF(:,:) ! Size is nbasins, nyrs; conveyance efficiciency?  Doesn't seem to be used (for old version of CU Model?)
      CHARACTER*40, ALLOCATABLE :: DITCHNAM(:,:) ! Size is nbasins, maxditches; the name of the ditch.
      INTEGER, ALLOCATABLE :: DITCHSDF(:,:)  ! Size is nbasins, maxditches; sdf value for the ditch.
      INTEGER, ALLOCATABLE :: EXIST(:)                  ! DIM_NY

!     Well variables
      CHARACTER*60, ALLOCATABLE :: WBNAME(:) ! Size is nbasins; the name of the farm given in the well file (this is unused since the basins are read in the same order as the .cmn file)
      INTEGER, ALLOCATABLE :: NWELL(:)  ! Size is nbasins; number of wells per basin.
      INTEGER, ALLOCATABLE :: WPERNUM(:,:) ! Size is nbasins, maxwells; well ID number.
      REAL, ALLOCATABLE :: WELLPOR(:,:,:) ! Size is nbasins, maxwells, nyears; the contribution of the particular well to the farm.
      REAL, ALLOCATABLE :: WELSUP(:,:,:,:) ! Size is nbasins, maxwells,nyrs,12; the monthly well supply for a particular well if discharge measurements are provided.
      REAL, ALLOCATABLE :: WellPWS(:,:,:) ! Size is nbasins, nyrs, 12; well water available for CU?
      REAL, ALLOCATABLE :: WellP(:,:,:) ! Size is nbasins, nyrs, 12; well pumping for the basin?
      REAL, ALLOCATABLE :: WEFF(:,:,:,:) ! Size is nbasins, maxwells, nyrs, 12; the well efficiency if discharge measurements are being used.
      LOGICAL LIMIT_WELL_DISCH ! if true, then limit well depletion by max capacity
      REAL, ALLOCATABLE :: WFLOW(:,:) ! nbasins, maxwells.  Max capacity of the well.
      REAL, ALLOCATABLE :: SDFM(:,:) ! Size is nbasins, maxwells; the SDF mode of the well in the farm:
                                      !  0 - Augmentation Plan Value
                                      !  1 - Based on Decreed Location
                                      !  2 - Based on GPS Location
                                      !  3 - User Defined
      REAL, ALLOCATABLE :: BWELSUP(:,:,:) ! Size is nbasins, nyrs,12; sum of discharge measurements in acre-feet for all wells on a farm.
      REAL, ALLOCATABLE :: BWELACU(:,:,:) ! Size is nbasins, nyrs,12; Well water available for CU each month.
      REAL, ALLOCATABLE :: FARM_RO(:,:,:,:) ! Size is nbasins, nyrs, 13, 8; The 8 is for all the possible ET types (original & enhanced).  This is the calculated runoff for the given farm in the given year and month.
      REAL, ALLOCATABLE :: FARM_DP(:,:,:,:) ! Size is nbasins, nyrs, 13, 8; The 8 is for all the possible ET types (original & enhanced).  This is the calculated deep percolation for the given farm in the given year and month.
      REAL, ALLOCATABLE :: DITCH_DP(:,:,:,:) ! Size is nbasins, nyrs, 13, 8; The 8 is for all the possible ET types (original & enhanced).  This is the conveyance loss from the surface water supplies for the given farm in the given year and month.
      INTEGER, ALLOCATABLE :: WMODE(:) ! NBASIN
 ! The mode that the wells are using: 0 - No water shortages allowed (wells will pump enough to meet demand)  1 - shortages allowed (discharge measurements must be provided).
      INTEGER, ALLOCATABLE :: WELL_EFF_IS_FIELD_EFF(:) ! NBASIN
      ! This indicates whether well field efficiency should be used or the basin's composite application efficiency.

      INTEGER, ALLOCATABLE :: SUPDIST(:)  ! NBASIN
! Water Supply Distribution
!              1 - Water Supply is Evenly Distributed
!              0 - Surface Water Supply is used first
!              2 - Well Water is used first


 
!     Start and end of growing season
      INTEGER, ALLOCATABLE :: JBEG(:,:), JEND(:,:) ! Size is NPARCE, nyrs; the beginning and ending growing season for a parcel in a given year.

!     When getting the month and day from the julian day, we need a year.  In
!       order to minimize parameter passing, use m_year as the current year.
      INTEGER m_year

      CHARACTER*15, ALLOCATABLE :: SNAME(:)  ! N_SOILS
      CHARACTER*20, ALLOCATABLE :: CNAME(:)  ! N_CROPS

      LOGICAL USE_WATER_STRESS ! True if the user wants to reduce the CU of the plant when soil moisture storage goes below RAM.

      INTEGER USER_ET_MODE ! 0 == NWR, 1 == RefET
      LOGICAL hasUserSuppliedCU ! Set to false if potential crop consumptive use needs to be calculated.  Otherwise CU will
      ! represent actual consumptive use.

      INTEGER MONTH(12)
      DATA MONTH/31,28,31,30,31,30,31,31,30,31,30,31/

      INTEGER MIDDLE(12)
      DATA MIDDLE/16,45,75,105,136,166,197,228,258,289,319,350/

      CHARACTER*90 DLINE, SLINE
      CHARACTER*272 DLLINE,SLLINE

      DATA DLINE/'======================================================&
	   &===================================='/
      DATA SLINE/'------------------------------------------------------&
	   &------------------------------------'/
      DATA DLLINE/'=====================================================&
	   &==================================================================&
	   &==================================================================&
	   &==================================================================&
	   &===================='/
      DATA SLLINE/'-----------------------------------------------------&
	&------------------------------------------------------------------&
	&------------------------------------------------------------------&
	&------------------------------------------------------------------&
	&--------------------'/

      CHARACTER*3 AMN(12)
      DATA AMN/'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug', 'Sep','Oct','Nov','Dec'/

      END MODULE
