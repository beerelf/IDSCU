!
!    Copyright (C) 1995  Integrated Decision Support Group
!                        Colorado State University
!
!    This program is free software; you can redistribute it and/or modify
!    it under the terms of the GNU General Public License as published by
!    the Free Software Foundation; either version 1, or (at your option)
!    any later version.
!
!    This program is distributed in the hope that it will be useful,
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!    GNU General Public License for more details.
!
!    You should have received a copy of the GNU General Public License
!    along with this program; if not, write to the Free Software
!    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
!
!
!   File           :  run_cu.f
!   Author         :  HB Manguerra and Luis Garcia
!   Date           :  December 1994
!   Purpose        :  This is the main calling program of the cu model.  The
!                     executable requires an argument that corresponds to the
!                     base name of the input files. To run the cu model, type 
!                     run_cu <base_name> from the unix prompt.
!   Assumptions    :  The crop consumptive use in irrigated areas is 
!                     calculated using Blaney-Criddle and Penman-Monteith 
!                     equations.  The consumptive use due
!                     to other uses include reservoir evaporation, stockpond
!                     evaporation, municipal/commercial use, industrial use,
!                     livestock use and export.  The industrial use is mainly
!                     compose of uses from thermal electric power and mineral
!                     resources.
!   Limitations    :  This version has been compiled and ran successfully in
!                     an SGI platform.  Versions are being prepared for 
!                     the Suns workstation and the PC. Other program limita-
!                     tions are documented in the appropriate subroutines.
!
!  History         :  (Date, Author, Description)
!
!  10/05/95   HBM  :  Version 2.1
!
!  11/08/95   HBM  :  Version 2.2
!
!  11/15/95   HBM  :  Version 2.2.1
!
!  12/13/95   HBM  :  Version 3.0
!
!   4/22/95   LAG  :  Version 3.1 This version allows the frost dates to
!                                 be -999 and return the julian days for
!                                 them as -999.  This assumes that these
!                                 stations are not being used to compute
!                                 begining and ending of the season but as
!                                 rainfall stations.
!
!   7/01/96   LAG  :  Version 3.2 This version changed the Enhanced BC
!                                 soil moisture budget.  It changed the
!                                 file wbudget to not sum the carryover
!                                 soil moisture.  It also removed the
!                                 initialization of the storage in distr
!                                 which was creating a problem.
!
!   9/06/96   LAG  :  Version 3.3 This version prints a small message in
!                                 *.obc and *.owb when no structures have
!                                 been specified as requiring this type
!                                 of output.
!  
!   9/09/96   LAG  :  Version 3.4 This version print an error message if
!                                 latitude is larger than 65 or smaller than
!                                 zero.
!
!  10/07/96   LAG  :  Version 3.5 This version prints several new error 
!                                 messages, it has a correction in the water
!                                 budget and the common block does not 
!                                 declare the size of the arrays.
!
!  11/18/96   LAG  :  Version 3.6 This version prints the water budget in 
!                                 integer format to be compatible with the
!                                 water supply that is provided in whole 
!                                 numbers.  It also contains a small change
!                                 stockp subroutine which changes the percent
!                                 of daylight hours each month.
!
!  11/24/96   LAG  :  Version 3.61 This version prints the water budget and
!                                 IWR or crop CU depending if water supply
!                                 is available or not.
!
!   1/15/97   LAG  :  Version 3.7 This version allows the user to put a name
!                                 for each export site.  It also allows the
!                                 user to report reservoir evaporation or 
!                                 calculate it.
!
!   5/29/98   LAG   : Version 4.0 This is the first version for the South Platte
!                                 This version support the Kimberly Penman and 
!                                 can distribute the water use between wells.
!                                 The model uses new formats for the common file
!                                 and the water supply file.
!
!   8/16/98   LAG   : Version 4.1 This version in the South Platte version uses
!                                 AWC changed from FT/FT to INCHES/FT.
!
!   6/10/01   LAG   : Version 5.0 Adds recharge for DP & RO, DP/RO percentages
!                                 SDF values for farms & ditches.
!
!***************************************************************************
!
!     PROGRAM RUN_CU
!  
!***************************************************************************

PROGRAM RUN_CU

  USE PORTLIB
  USE MSFLIB
  USE Globals

  INTERFACE
     INTEGER FUNCTION GET_MAXWELL()
     END FUNCTION GET_MAXWELL

     INTEGER FUNCTION GET_MAXDITCH()
     END FUNCTION GET_MAXDITCH

     INTEGER FUNCTION GetSpringWheatKey()
     END FUNCTION GetSpringWheatKey

     SUBROUTINE CALC_CU(et_method, nperiods)
       integer et_method
       integer nperiods ! 12 for monthly, 366 for daily
     END SUBROUTINE CALC_CU

     SUBROUTINE INDECE()
     END SUBROUTINE INDECE

     SUBROUTINE SUMMARY()
     END SUBROUTINE SUMMARY

     SUBROUTINE BUDGET(et_method)
       INTEGER et_method
     END SUBROUTINE BUDGET

     SUBROUTINE WBUDGET(et_method)
       INTEGER et_method
     END SUBROUTINE WBUDGET

     SUBROUTINE SPWELLW(et_method, nperiods)
       INTEGER et_method, nperiods
     END SUBROUTINE SPWELLW

     SUBROUTINE PROJ()
     END SUBROUTINE PROJ

     REAL FUNCTION parse_num(instr)
       CHARACTER*120 instr
     END FUNCTION parse_num
     
     CHARACTER*20 FUNCTION GetETString(ET)
       INTEGER ET
     END FUNCTION GetETString

  END INTERFACE

  !Local Variable Declarations
  REAL, ALLOCATABLE :: GAREA(:) ! DIM_NY


  !PROFSSIM - PORTION OF PROFILE FULL AT START OF SIMULATION
  !PROFSSEA - PORTION OF PROFILE FULL AT START OF SEASON
  !PROFEND  - PORTION OF PROFILE FULL AT END OF SEASON
  !PROFLAG  - 1 = USE USER DEFINED PORTION OF PROF. FULL @ START SEASON
  !PROFLAG  - 2 = USE USER DEFINED PORTION OF PROF. FULL @ END OF SEASON
  !PROFLAG  - 3 = USE BOTH
  !PROFLAG  - 0 = COMPUTE PORTION OF PROF. FULL BASED ON WINTER CARRY OVER

  INTEGER IERR
  INTEGER I, J, K, IYR, M
  INTEGER(2) N
  CHARACTER*120 dfile1, argmet, line1
  CHARACTER*4 SPext
  INTEGER KK, curpos, istat
  REAL VERS, GUIVERSION
  INTEGER CROP_ID, NP, skip_int
  INTEGER, ALLOCATABLE :: crop_order(:)
  INTEGER ET_PERIODS(N_ET_M), ET
  DATA ET_PERIODS/13,366,13,366,13,366,13,13,366,366/

  !Specify program version
  VERS = 6.0

  !Handle command line argument, prompt the user if argument is
  !not provided
  IF (IARGC().EQ.0) THEN
     WRITE (*,100)
100  FORMAT(1x,'Enter base name?') 
     READ(*,101) dfile
101  FORMAT(A120)
     WRITE(*,105)
105  FORMAT(1x,'Using South Platte Extension (Yes or No)?')
     READ(*,904) SPext
904  FORMAT(A3)
     IF( SPext .EQ. 'yes' .OR. SPext .EQ. 'YES' .OR. SPext .EQ. 'Y' &
	  .OR. SPext .EQ. 'y' ) THEN 
	SPFLAG = 1
     ELSE
	SPFLAG = 0
     ENDIF
  ELSE
     N=1
     CALL GETARG(N,argmet)
     IF (argmet.EQ.'-h'.OR.argmet.EQ.'-H') THEN
	WRITE(*,103)
	STOP
     ELSEIF (argmet.EQ.'-v'.OR.argmet.EQ.'-V') THEN
	WRITE(*,102) vers
	STOP
     ELSE
	IF(argmet .EQ. '-SP' .OR. argmet .EQ. '-sp') THEN
	   SPFLAG = 1
	   N=2
	   CALL GETARG(N,argmet)
	ELSE
	   SPFLAG = 0
	   N=1
	   CALL GETARG(N,argmet)
	ENDIF
	dfile = argmet
     ENDIF
  ENDIF

  !----------------------------------------------------------------------------
  !     Read main file *.cmn
  !----------------------------------------------------------------------------

  !Fix to allow blanks in the name of the file

  flen = len_trim(dfile) + 1

  dfile1 = dfile
  dfile1(flen:flen+4) = '.cmn'

  OPEN (UNIT=1,FILE=dfile1,STATUS='OLD',IOSTAT=IERR)
  IF (IERR.NE.0) CALL MYEXIT(1) 

  !Read 3 Rows of Title strings
  READ(1,900,ERR=104) (TITLE(I), I=1,3) 

  !Read Version Number Row and Disregard for now
  READ(1,199,ERR=125) GUIVersion
199 FORMAT(8X,F4.1)

  !Read Begin and End Year
  READ(1,*,ERR=106) GNYR1,GNYR2,PROJTYPE,HISTYR2
  READ(1,fmt='(a)') PROJ_DETAILS ! Post proj
  READ(1,fmt='(a)') PROJ_DETAILS ! Pre proj

  !Read simulation years.
  READ(1,200,ERR=107) LINE1
200 FORMAT(A80)
  IF(LINE1(1:3) .EQ. 'Ave' .OR. LINE1(1:3) .EQ. 'AVE' ) THEN
     NYR1 = GNYR2 + 1
     NYR2 = GNYR2 + 1
     NYRS = 1
  ELSE   
     READ(LINE1,*,ERR=107) NYR1,NYR2 ! subset
     GNYRS = GNYR2 - GNYR1 + 1
     NYRS = NYR2 - NYR1 + 1

     IF ((GNYR1.GT.NYR1).OR.(GNYR2.LT.NYR2)) CALL MYEXIT(133)
  ENDIF

  ! Read incomplete end year date.
  READ(1,*,ERR=107) INCOMPLETE_MONTH, INCOMPLETE_DAY

  !Read Number of SubAreas
  READ(1,*,ERR=108) NBASIN

  !Patterson: allocate space to dynamic arrays
  ALLOCATE(BAS_ID(NBASIN))

  !Read maxwells
  MAXWELL = GET_MAXWELL()

  if (MAXWELL .GT. 0) then
     ALLOCATE(WEFF(NBASIN, MAXWELL, NYRS, 12))
     ALLOCATE(WFLOW(NBASIN, MAXWELL))
     ALLOCATE(WPERNUM(NBASIN, MAXWELL))
     ALLOCATE(WELLPOR(NBASIN, MAXWELL, nyrs))
     ALLOCATE(WELSUP(NBASIN, MAXWELL, nyrs, 12))
     ALLOCATE(SDFM(NBASIN, MAXWELL))
  endif

  ALLOCATE(NWELL(NBASIN))
  ALLOCATE(WBNAME(NBASIN))
  ALLOCATE(SUPDIST(NBASIN))
  ALLOCATE(WMODE(NBASIN))
  ALLOCATE(WELL_EFF_IS_FIELD_EFF(NBASIN))

  !Check for daily or monthly surface data.  If any method is daily, then suface water is all daily.
  dfile1 = dfile
  dfile1(flen:flen+4) = '.wd'
  OPEN (UNIT=7,FILE=dfile1,STATUS='OLD',IOSTAT=IERR)
  !If the file exists, then we have daily methods.
  hasDailyMethod = (IERR .eq. 0)
  close(7)

  MAXDITCH = GET_MAXDITCH()

  ALLOCATE(DITCHNAM(NBASIN, MAXDITCH))
  ALLOCATE(DITCHSDF(NBASIN, MAXDITCH))

  !----------------------------------------------------------------------------
  !     Read flags for ET methods 
  !     ET Method: ETFLAG(1) - Blaney-Criddle (SCS BC Original and Enhanced) 
  !                    2 - Reference Equation  (Grass and Alfalfa Based)
  !                    3 - Other Uses
  !                    4 - Kimberly Penman Data for the South Platte
  !                    5 - Calibrated Blaney-Criddle
  !                    6 - ASCE
  !                    7 - 85 Hargreaves
  !                    8 - Pochop
  !          FLAG = 0 = do not compute
  !          FLAG = 1 = compute
  !----------------------------------------------------------------------------

  READ(1,*,ERR=109) ETFLAG(1), ETFLAG(2), ETFLAG(3), ETFLAG(4), &
       ETFLAG(5), ETFLAG(6), ETFLAG(7), ETFLAG(8), ETFLAG(9), ETFLAG(10)

  READ(1,*) CBC_USE_REF_KC

  !-------------------------------------------------------------
  !     Read Calibrated BC elevation data.
  READ(1,*,ERR=109) i, BC_ELEV_BASE, (BC_ELEV_PERC(J), J=1,12)
  BC_USE_ELEV = i .EQ. 1
  !-------------------------------------------------------------

  !----------------------------------------------------------------------------
  !     Effective rainfall Method: 
  !       monthly: RN_XCO  = 1 - SCS method
  !                        = 2 - usbr mehod
  !                APD     = Aplication Depth only for SCS
  !                ERainSM = Use Excess Effective Rainfall in the Soil Moisture
  !                        = 1 - Yes, 0 - No
  !       daily  : RN_REF  = 1 - a max effective rainfall is specified 
  !                        = 2 - factor x total rainfall
  !                        = 3 - curve number method
  !----------------------------------------------------------------------------

  READ(1,*,ERR=110) RN_XCO, APD, ERainSM

  !Read Rainfall Parameters
  READ(1,*,ERR=111) RN_REF, RPARA(1), RPARA(2), RPARA(3)

  !Read Flag for calculating water supply
  !     SUPDIST - Water Supply Distribution
  !             = 1 - Water Supply is Evenly Distributed
  !             = 0 - Surface Water Supply is used first
  !             = 2 - Well Water is used first
  !     RCHGFLG - Recharge Flag
  !             = 1 - Calculate recharge from DP & RO
  !             = 0 - Do NOT calculate recharge from DP & RO
  !     USER_ET_MODE - How to handle user-supplied ET data
  !             = 0 - Data is Net Water Requirement
  !             = 1 - Data is Reference ET
  !
  READ(1,*,ERR=112) ISUPLY, RCHGFLG, skip_int, USE_WATER_STRESS, USER_ET_MODE

  !Patterson: ignore recharge; currently it is calculated in the interface.
  RCHGFLG = 0

  IF( GUIVersion .GE. 2.3 ) THEN
     READ(1,*, ERR=1121) (DP_ROPERC(I),I=1,12) 
  ENDIF
  IF( GUIVersion .GE. 2.9 ) THEN
     !Ignore recharge options.
     CALL SKIPLN(1,1)
  ENDIF

  ! Read flag indicating whether frost dates should be ignored
  READ(1,*, ERR=126) IGNORE_FROST_DATES
  
  !Read General Output Options
  READ(1,*,ERR=113) S_OUT, IQUOTE 
  IF (IQUOTE.EQ.0) THEN
     QUOTE = ' '
  ELSE
     QUOTE = '"'
  ENDIF

  ! Read Blaney-Criddle Kt override option
  READ(1,*,ERR=127) BC_KC_OVERRIDES_KT

  !Read Snow Melt Efficiency
  CALL SKIPLN(1,NYR1-GNYR1)

  !     PROFSSIM IS THE PORTION OF PROFILE FULL AT BEGINNING
  !     OF SIMULATION.
  !     PROFEND IS THE PORTION OF THE SOIL PROFILE FULL AT THE
  !     END OF THE GROWING SEASON
  !
  !IDS  ADDED THE ABILITY TO RESET THE INITIAL SOIL MOISTURE TO A USER
  !     DEFINED PERCENT OF TOTAL AVAILABLE SOIL MOISTURE
  !     PROFLAG = 1 MEANS USER SET THE SOIL MOISTURE AT THE BEGINNING
  !               OF EACH SEASON TO BE A SPECIFIED NUMBER (PROFSSEA).
  !

  ALLOCATE(SMEF(NYRS))
  ALLOCATE(PROFSSEA(NYRS))
  ALLOCATE(PROFLAG(NYRS))
  ALLOCATE(PROFEND(NYRS))

  DO I = 1, NYRS
     READ(1,*,ERR=114) IYR,SMEF(I),PROFSSIM,PROFSSEA(I),PROFLAG(I), PROFEND(I)
  END DO

  CALL SKIPLN(1,GNYR2-NYR2)

  !Skip wellPumpingIsAnnual
  CALL SKIPLN(1,1)

  !---------------------------------------------------------------------------
  !     Read Crop Data
  !---------------------------------------------------------------------------

  READ(1,900,ERR=115) TITLE(4) 

  !Read number of crop scenarios
  READ(1,*,ERR=116) N_CROPS, N_SOILS

  !Allocate memory.
  ALLOCATE(SNAME(N_SOILS))
  ALLOCATE(CNAME(N_CROPS))

  ALLOCATE(CROP_TYPE(N_CROPS))
  ALLOCATE(SUB_CROP_TYPE(N_CROPS))
  ALLOCATE(GDATE1(N_CROPS))
  ALLOCATE(GDATE2(N_CROPS))
  ALLOCATE(GDATE3(N_CROPS))
  ALLOCATE(GDATE4(N_CROPS))
  ALLOCATE(GDATE5(N_CROPS))
  ALLOCATE(GDATES(N_CROPS))
  ALLOCATE(TMOIS1(N_CROPS))
  ALLOCATE(TMOIS2(N_CROPS))
  ALLOCATE(MAD(N_CROPS))
  ALLOCATE(IRZ(N_CROPS))
  ALLOCATE(FRZ(N_CROPS))
  ALLOCATE(RZ(N_CROPS))
  ALLOCATE(TFLG1(N_CROPS))
  ALLOCATE(TFLG2(N_CROPS))
  ALLOCATE(CUT2(N_CROPS))
  ALLOCATE(CUT3(N_CROPS))

  ALLOCATE(AWC(N_SOILS))

  ALLOCATE(nckca(N_CROPS, akclen))
  ALLOCATE(nckcp(N_CROPS, pkclen))
  ALLOCATE(ckca(N_CROPS, akclen))
  ALLOCATE(ckcp(N_CROPS, pkclen))
  ALLOCATE(CBC(N_CROPS, 12))

  !Read Crop Table

  ! Store crop order
  ALLOCATE(crop_order(N_CROPS))

  DO K=1,N_CROPS
!     READ(1,fmt='(*,*,A)',ERR=119) N, CROP_TYPE(K), CPNAME(K)
     READ(1,fmt='(A)') LINE1
     N = parse_num(line1)
     CROP_TYPE(N) = parse_num(line1)
     SUB_CROP_TYPE(N) = parse_num(line1)
     CNAME(N) = line1

     !Bail out if crop type not set.
     IF (CROP_TYPE(N) .eq. 4) THEN
	!UnknownCropType
	print *, CNAME(N), " has no crop type."
	CALL MYEXIT(-1)
     ENDIF

     crop_order(K) = N
  END DO

  DO K=1,N_SOILS
     !READ(1,fmt='(I,I,A)',ERR=119) N, AWC(K), SOILNAME(K)
     READ(1,fmt='(A)') LINE1
     N = parse_num(line1)
     SNAME(N) = line1
  END DO


  READ(1,900,ERR=118) TITLE(5)
  DO K=1,N_CROPS
     IF (CROP_TYPE(crop_order(K)) .EQ. 0) THEN
	! Alfalfa
	READ(1,*,ERR=119) N, GDATE1(N), GDATE2(N), &
	     GDATE3(N), GDATE4(N), GDATE5(N), &
	     GDATES(N), TMOIS1(N), TMOIS2(N), &
	     MAD(N), IRZ(N), FRZ(N), &
	     TFLG1(N), TFLG2(N), &
	     CUT2(N), CUT3(N)
     ELSE
	READ(1,*,ERR=119) N, GDATE1(N), GDATE2(N), &
	     GDATE3(N), GDATE4(N), GDATE5(N), &
	     GDATES(N), TMOIS1(N), TMOIS2(N), &
	     MAD(N), IRZ(N), FRZ(N), &
	     TFLG1(N), TFLG2(N)
     END IF

     IF( GDATE1(N) .EQ. 0 .OR. GDATE1(N) .GT. 12 ) THEN
	CALL MYEXIT(140)
     ENDIF
     IF( GDATE2(N) .EQ. 0 .OR. GDATE2(N) .GT. 31 ) THEN
	CALL MYEXIT(141)
     ENDIF
     IF( GDATE3(N) .EQ. 0 .OR. GDATE3(N) .GT. 12 ) THEN
	CALL MYEXIT(142)
     ENDIF
     IF( GDATE4(N) .EQ. 0 .OR. GDATE4(N) .GT. 31 ) THEN
	CALL MYEXIT(143)
     ENDIF
     IF( GDATE5(N) .EQ. 0 .OR. GDATE5(N) .GT. 365 ) THEN
	CALL MYEXIT(144)
     ENDIF
     IF( GDATES(N) .EQ. 0 .OR. GDATES(N) .GT. 366 ) THEN
	CALL MYEXIT(145)
     ENDIF

     !IRZ(N) = FRZ(N)        ! assumes no change in root depth
     RZ(N) = IRZ(N)

  END DO

  DO K=1,N_SOILS
     READ(1,*,ERR=119) N, AWC(N)
  END DO

  ALLOCATE(BELEV(NBASIN))
  ALLOCATE(BLAT(NBASIN))
  ALLOCATE(BLONG(NBASIN))
  ALLOCATE(NPARCE(NBASIN))
  ALLOCATE(NPARCE_ORI(NBASIN))
  ALLOCATE(TYPOUT(NBASIN))
  ALLOCATE(CUSHORT(NBASIN))
  ALLOCATE(INC_IWR(NBASIN))
  ALLOCATE(FARMSDF(NBASIN))

  ALLOCATE(EXIST(NYRS))
  ALLOCATE(CEFF(NBASIN,NYRS))

  !Read Acreage Data for Each Basin
  READ(1,900,ERR=121) TITLE(6)
  MAXPARCEL = 0
  curpos = FTELL(1)
  DO I=1,NBASIN
     ! Initialize parameters
     NWELL(I) = 0

     ! Read data
     READ(1,901,ERR=122) BAS_ID(I)
     IF( SPFLAG .EQ. 1) THEN
	READ(1,*,ERR=123)BELEV(I),BLAT(I),BLONG(I),NPARCE(I), &
	     TYPOUT(I), CUSHORT(I), INC_IWR(I), FARMSDF(I)
     ELSE
	READ(1,*,ERR=123)BELEV(I),BLAT(I),BLONG(I),NPARCE(I), TYPOUT(I)
     ENDIF

     np = NPARCE(I)

     ! Store original number of fields.
     NPARCE_ORI(I) = NPARCE(I)

     ! Skip monthly efficiency option line.
     CALL SKIPLN(1,1)

     ! Skip parcel information for now. 
     DO K=1,NPARCE(I)
	READ(1,*) CROP_ID
	IF (CROP_ID .GE. 1) THEN
	   IF (SUB_CROP_TYPE(CROP_ID) .eq. 0) then
	      ! Add space for an additional parcel for the spring portion
	      ! of the winter wheat.
	      np = np + 1
	   ENDIF
	ENDIF
     END DO

     IF( np .GT. MAXPARCEL) MAXPARCEL = np
  END DO

  ALLOCATE(npart(MAXPARCEL))

  ALLOCATE(CROP_KEY(NBASIN, MAXPARCEL))
  ALLOCATE(SOIL_KEY(NBASIN, MAXPARCEL))
  ALLOCATE(AEFF(NBASIN, MAXPARCEL, 12))
  ALLOCATE(AREA(NBASIN, MAXPARCEL, NYRS))
  ALLOCATE(C_AREA(NYRS, N_CROPS))
  ALLOCATE(SPRINKLER(NBASIN, MAXPARCEL))
  ALLOCATE(GAREA(GNYRS))
  ALLOCATE(T_AREA(NBASIN, NYRS))

  ALLOCATE(incrz(MAXPARCEL))
  ALLOCATE(incst(MAXPARCEL))
  ALLOCATE(maxcov(MAXPARCEL))

  ALLOCATE(fbegmo(NBASIN, NYRS))
  ALLOCATE(fendmo(NBASIN, NYRS))

  ! For daily methods
  ALLOCATE(fbegda(NBASIN, NYRS))
  ALLOCATE(fendda(NBASIN, NYRS))

  IF (MAXPARCEL .eq. 0) THEN
     CALL MYEXIT(168)
  END IF
  ALLOCATE(jbeg(MAXPARCEL, NYRS))
  ALLOCATE(jend(MAXPARCEL, NYRS))

  ! Reset position to beginning of basin data.
  istat = FSEEK(1, curpos, 0)

  DO I=1,NBASIN
     !Skip basin ID
     READ(1,fmt='(a)')
     !Skip basin data
     READ(1,fmt='(a)')

     n_wheat_parcel = 0

     ! Skip monthly efficiency option line.
     CALL SKIPLN(1,1)

     DO K=1,NPARCE(I)
	READ(1,*,ERR=124) CROP_KEY(I,K),SOIL_KEY(I,K),(AEFF(I,K,M),M=1,12), &
	     (GAREA(J),J=1,GNYRS), SPRINKLER(I,K)
	DO M=1,12
	   IF( AEFF(I,K,M) .LE. 0.01) THEN
	      WRITE(*,*) 'PARCEL ',K,' AT MONTH ',M,' OF BASIN ', BAS_ID(I)
	      CALL MYEXIT(124)
	   ENDIF
	   IF( AEFF(I,K,M) .GT. 1.00) THEN
	      WRITE(*,*) 'PARCEL ',K,' AT MONTH ',M,' OF BASIN ', BAS_ID(I)
	      CALL MYEXIT(125)
	   ENDIF
	END DO
	IF( CROP_KEY(I,K) .LE. 0) THEN
	   WRITE(*,*) 'PARCEL ',K,' OF BASIN ',BAS_ID(I)
	   CALL MYEXIT(94)
	ENDIF
	IF( SOIL_KEY(I,K) .LE. 0) THEN
	   WRITE(*,*) 'PARCEL ',K,' OF BASIN ',BAS_ID(I)
	   CALL MYEXIT(147)
	ENDIF
	
	IF (SUB_CROP_TYPE(CROP_KEY(I,K)) .EQ. 0)  THEN ! Winter wheat
	   !Patterson and garcia: add extra field for winter wheat, spring period.
	   n_wheat_parcel = n_wheat_parcel + 1
	   CROP_KEY(I,NPARCE(I) + n_wheat_parcel) = GetSpringWheatKey()
	   SOIL_KEY(I,NPARCE(I) + n_wheat_parcel) = SOIL_KEY(I,K)
	   SPRINKLER(I,NPARCE(I)+n_wheat_parcel) = SPRINKLER(I,K)
	   DO M=1,12
	      AEFF(I,NPARCE(I) + n_wheat_parcel, M) = AEFF(I,K,M)
	   END DO
	ELSEIF (SUB_CROP_TYPE(CROP_KEY(I,K)) .EQ. 1) THEN ! spring wheat
	   PRINT *, "Spring wheat field found.  Only winter wheat fields can be used."
	   call myexit(95)
	ENDIF
	DO J = 1,NYRS
	   AREA(I,K,J) = GAREA(J+NYR1-GNYR1)
	   if (SUB_CROP_TYPE(CROP_KEY(I,K)) .EQ. 0) THEN
	      ! Assign area to synthetic spring period winter wheat parcel.
	      AREA(I,NPARCE(I)+n_wheat_parcel,J) = GAREA(J+NYR1-GNYR1)
	   endif
	END DO
     END DO
     NPARCE(I) = NPARCE(I) + n_wheat_parcel
  END DO

  CLOSE(1)

  !Calculate Total Area per Basin per Year - handy if
  !project summary are desired to be in inches rather than acre-ft

  DO I=1,NBASIN
     DO J=1,NYRS
	T_AREA(I,J) = 0.0
     END DO
  END DO

  DO I=1,NBASIN
     DO J=1,NYRS
	DO K=1,NPARCE(I)
	   T_AREA(I,J) = T_AREA(I,J) + AREA(I,K,J)
	END DO
     END DO
  END DO

  !Calculate total area of project
  ALLOCATE(PJAREA(NYRS))

  DO J=1,NYRS
     PJAREA(J) = 0.0
  END DO

  DO J=1,NYRS
     DO I=1,NBASIN
	PJAREA(J) = PJAREA(J) + T_AREA(I,J)
     END DO
  END DO

  !Calculate total area by crops

  DO J=1,NYRS
     DO K=1,N_CROPS
	C_AREA(J,K) = 0.0
     END DO
  END DO

  DO I=1,NBASIN
     DO J=1,NYRS
	DO K=1,NPARCE(I)
	   KK = CROP_KEY(I,K)
	   C_AREA(J,KK) = C_AREA(J,KK) + AREA(I,K,J)
	END DO
     END DO
  END DO

  !Start Calculation

  !Write the summary of the input
  CALL SUMMARY()

  !Read the well file and assign wells to fields for calculating
  !     the water supply provided by wells.
  IF( SPFLAG .EQ. 1 .AND. ISUPLY .EQ. 1) CALL SPWELLR

  !Open *.owb file for writing
  dfile1 = dfile
  dfile1(flen:flen+4) = '.owb'
  OPEN (UNIT=7,FILE=dfile1,STATUS='UNKNOWN')

  !-----open detailed soil moisture file 
  dfile1(flen:flen+4) = '.owbd'
  OPEN(UNIT=23,FILE=dfile1,STATUS='UNKNOWN')

  !temporary files for project summary
  OPEN (UNIT=14,FILE='tmp4',STATUS='UNKNOWN')
  OPEN (UNIT=15,FILE='tmp5',STATUS='UNKNOWN')

  DO ET = 1,N_ET_M
     !Calculate the ET and write the results
     IF (ETFLAG(ET).EQ.1) THEN
	!Patterson: try to lump all ETs into one subroutine.
	print *, "Calculating CU for ", GetETString(ET)

	CALL CALC_CU(ET, ET_PERIODS(ET))
	if (useCropMode) then
	   print *, "Using Leaf Area Index mode."
	end if

	!Calculate the water budget with water supply or without it
	print *, "Calculating water budget for ", GetETString(ET)
	IF (ISUPLY.EQ.0) CALL BUDGET(ET)
	IF (ISUPLY.EQ.1) CALL WBUDGET(ET)

	!Distribute any shortages between wells (Only for South Platte Version)
	print *, "Distributing well water for ", GetETString(ET)
	IF( SPFLAG .EQ. 1) CALL SPWELLW(ET, ET_PERIODS(ET)+1)
     ENDIF
  END DO

  !Close the owl file
  CLOSE(9)

  !Close the owb file
  CLOSE(7)
  CALL FLUSH(7)

  CLOSE(23)

  !Close the temporary files for project summary
  CLOSE(14)
  CLOSE(15)

  !Calculate the CU for other uses
  IF (ETFLAG(3).EQ.1) CALL OTHER

  !Write the project summary      
  CALL PROJ

  !Include water supply in input summary file
  IF (ISUPLY.EQ.1) THEN
     DO I=1,NBASIN
	IF(SPFLAG .EQ. 1) THEN
	   CALL SPWSUPPLY(I)
	ELSE
	   CALL WSUPPLY(I)
	ENDIF
     END DO
  ENDIF

  CALL MYEXIT(0)

  !Read error exit calls
104 CALL MYEXIT(7)

106 CALL MYEXIT(14)
107 CALL MYEXIT(15)
108 CALL MYEXIT(16)
109 CALL MYEXIT(17)
110 CALL MYEXIT(18)
111 CALL MYEXIT(19)
112 CALL MYEXIT(20)
1121 CALL MYEXIT(201)
113 CALL MYEXIT(21)
114 CALL MYEXIT(22)
115 CALL MYEXIT(23)
116 CALL MYEXIT(24)
117 CALL MYEXIT(25)
118 CALL MYEXIT(26)
119 CALL MYEXIT(27)
120 CALL MYEXIT(28)
121 CALL MYEXIT(29)
122 CALL MYEXIT(30)
123 CALL MYEXIT(31)
124 CALL MYEXIT(32)
125 CALL MYEXIT(132)
126 CALL MYEXIT(169)
127 CALL MYEXIT(170)


900 FORMAT(A120)
901 FORMAT(A60)
902 FORMAT(I2,A20)
903 FORMAT(I3,A20,A15)
999 FORMAT(1x,'program is running...')

102 FORMAT(/'       run_cu version: ',F5.2)
103 FORMAT(/'Usage: run_cu [filename] &
       &The Consumptive Use (CU) Model is written in FORTRAN 77 using &
       &a Microsoft Fortran compiler.  Evapotranspiration (ET) from &
       &irrigated agricultural areas is calculated using Blaney- &
       &Criddle (monthly model) and/or Penman-Monteith (daily model) &
       &ET estimation methods.  Consumptive use and losses from non- &
       &evapotranspiration (other uses) categories namely; stock &
       &pond, reservoir, municipal, industrial, livestock and export &
       &are also calculated.'// &
       'See manpages for run_cu for more information.'//&
       'The following command line arguments are recognized:'//&
       '       -h    Print this help screen.'//&
       '       -v    Print the program version.')

  STOP 
END PROGRAM RUN_CU
