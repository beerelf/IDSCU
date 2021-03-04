SUBROUTINE PROTO(IB, et_method, nyr, actual_nper, use_smb, &
     !     Output
     cu, sprink_l, cuirr, er, exces_er, shorts_d, shorts_stress)

  !***************************************************************************
  !     
  !     Function        : proto.f 
  !     Author          : HB Manguerra
  !     Date            : May 1995 
  !     Purpose         : This is the main calling subroutine for calculating
  !     crop consumptive use by Penman-Monteith ET method.
  !     Calling program : run_cu.f
  !     Called programs : etref.f, avgmon.f, weight.f, frost.f,
  !     kbasal.f, wbuild.f, finput.f, growth.f, clndr.f, 
  !     rain.f, acount.f, foutput.f, myexit.f supply.f
  !     Input arguments : none
  !     Output arguments: none
  !     Assumptions     :
  !     Limitations     :
  !     Notes           : Also reads the published 28 and 32 degree F frost
  !     dates for all years in every weather station and
  !     weigh them to get the representative frost date
  !     for the given area.
  !     
  !     History         :(Date, Author, Description)
  !     
  !     11/02/95   HBM  : Units changed from 1000 acre-ft to acre-ft.
  !     
  !     11/15/95   HBM  : Basal kc is not supported anymore. 
  !     
  !     12/4/02    Patterson: Started conversion to daily water budget output.
  !     
  !***************************************************************************


  !-----Include Global Variables and Data Defaults
  USE Globals

  !     Parameters
  INTEGER IB, et_method, nyr, actual_nper
  LOGICAL use_smb ! If TRUE, then use a soil moisture budget
  !     Output
  REAL cu(:,:), cuirr(:,:), er(:,:), exces_er(:,:), shorts_d(:), shorts_stress(:)
  REAL sprink_l(:,:)

  INTERFACE
     INTEGER FUNCTION DaysInYear(yr)
       INTEGER yr
     END FUNCTION DaysInYear

     SUBROUTINE FINPUT(CKEY, SKEY, &
	  !Outputs
	  SMCAP0, RNFAC, CN, DEFIT)
       INTEGER CKEY, SKEY
       REAL SMCAP0, RNFAC, CN(3), DEFIT(12)
     END SUBROUTINE FINPUT

     SUBROUTINE FOUTPUT(IP, NYR, IB, DOY, IYEAR, &
          IMON, IDAY, JSTR, JSTP, SMSTG_D, SMSTG_PREV, &
          REFET_D, XKS, XKA, XKCB_D, ET_D, ER_D, DPERC_D, QIRR_D, NDYR)
       INTEGER IP, DOY, NDYR           ! Day of year
       INTEGER IB, NYR
       INTEGER IYEAR, IMON, IDAY ! Date of output
       INTEGER JSTR, JSTP     ! Crop start and end (planting and harvest)
       REAL SMSTG_D, SMSTG_PREV ! Soil moisture storage for today and yesterday
       !         REAL SMSTG0            ! Initial soil storage
       REAL REFET_D           ! Daily soil moisture storage, alfalfa or grass based depending on mode.
       REAL XKS, XKA, XKCB_D
       REAL ET_D, ER_D, DPERC_D, QIRR_D
     END SUBROUTINE FOUTPUT

     SUBROUTINE GROWTH(KEY, JSTR, JSTP, &
	  !     Output
          XKCB)
       INTEGER KEY, JSTR, JSTP
       REAL XKCB(366)
     END SUBROUTINE GROWTH

     SUBROUTINE GROWTH_GDD(IB, NYR, KEY, JSTR, JSTP, WRITE_REPORT, &
	  !     Output
          XKCB, GDD)
       INTEGER IB, NYR, KEY, JSTR, JSTP
       REAL XKCB(366), GDD(366)
       LOGICAL WRITE_REPORT
     END SUBROUTINE GROWTH_GDD

     REAL FUNCTION RAIN (ib, nyr, DOY, RNFAC, CN, QIRR)
       INTEGER ib, nyr, DOY
       REAL RNFAC, CN(3), QIRR(:)
     END FUNCTION RAIN

     SUBROUTINE ACOUNT (DOY, SMCAP, TRANS_D, WSEVAP_D, ER_D, &
          XKS, DEFIT, use_smb, &
	  !     Outputs
          QIRR_D, ET_D, DPERC_D, SMSTG_D)
       INTEGER DOY
       REAL SMCAP, TRANS_D, WSEVAP_D, ER_D, QIRR_D
       REAL XKS, DEFIT(12)
       REAL ET_D, DPERC_D, SMSTG_D
       LOGICAL use_smb
     END SUBROUTINE ACOUNT

     SUBROUTINE CLNDR (jday, year, &
	  !     Outputs
          m_mon, m_day)
       INTEGER jday, year, m_mon, m_day
     END SUBROUTINE CLNDR

     LOGICAL FUNCTION CheckCropCoeffs (et_method, ckey)
       INTEGER et_method, ckey
     END FUNCTION CheckCropCoeffs
  END INTERFACE

  !-----Local Variable Declaration
  CHARACTER*120 ofile1
  CHARACTER*120 ofile5
  INTEGER II, IP
  INTEGER JSTR, JSTP, NDYR
  INTEGER CKEY ! crop index
  INTEGER SKEY ! soil index
  REAL TAW, AWCMAD, Dr
  REAL pincrz, pincst
  !     NOTE: prunoff, pshorts, pcussup, preqt, pfsuply are global
  INTEGER IMON, IDAY, DOY
  REAL SMSTG(0:366)         ! Daily soil moisture storage
  REAL REFET(366)           ! Daily soil moisture storage, alfalfa or grass based depending on mode.

  REAL CN(3)       ! Curve number parameters when RN_REF = 3
  REAL WSEVAP(366) ! Not sure, unused
  REAL DPERC(366)  ! Daily deep percolation
  REAL QIRR(366)   ! Daily irrigation amount
  REAL DEFIT(12)   ! Minimum soil moisture deficit?
  REAL ET(366)     ! Daily evapo-transpiration
  REAL SMCAP       ! Soil moisture capacity
  REAL ISTOR       ! Initial Soil Storage
  REAL SMCAP0      ! Initial soil moisture capacity
  REAL SMSTGD      ! Soil moisture storage
  !      REAL SMSTG0      ! Initial soil moisture storage
  REAL SMSTG_PREV  ! Previous day's soil moisture
  REAL RNFAC       ! Rainfall factor
  !      REAL EVAPTD      ! Don't know
  REAL XKCB(366)   ! Interpolated daily crop coefficient
  REAL XKS, XKA    ! Unknown
  REAL GDD(366)    ! Accumulated growing degree days for the given day.

  INTEGER l
  REAL extra_dp

  SMSTGD = 0

  !     Needed for CLNDR function.
  m_year = nyr + nyr1 - 1

  !-----specify input file extension
  ofile1 = dfile
  ofile5 = dfile

  DO IP = 1, NPARCE(IB)
     CKEY = CROP_KEY(IB,IP)
     SKEY = SOIL_KEY(IB,IP)

     AWCMAD = MAD(CKEY) * AWC(SKEY) / 100.0

     !---------------------------------------------------------------
     !     Begin Yearly Loop 
     !---------------------------------------------------------------

     CALL FINPUT(CKEY, SKEY, &
	  !     Outputs
	  SMCAP0, RNFAC, CN, DEFIT)

     JSTR = JBEG(IP,1)      ! very start / crop

     !--------Throw error if no crop coefficients provided.
     if (.not. (CheckCropCoeffs(et_method, ckey) .or. hasCIR)) then
     !if (KCB(ckey,1) .eq. 0 .and. cu(1,1) .lt. 0) then
	write(*, fmt='(a,x,a10)') &
	     "No crop coefficients provided for ", CNAME(CKEY)
	call myexit(100)
     endif

     !         SMSTG0 = SMCAP0
     !         IF (JSTR.NE.1) SMSTG(JSTR-1) = SMCAP0
     NDYR = DaysInYear(nyr + NYR1 -1)

     JSTR = JBEG(IP,nyr)    ! julian day start
     JSTP = JEND(IP,nyr)    ! julian day end

     !-----Initialize Variables
     DO II = 1, NDYR 
	QIRR(II)   = 0.0
	DPERC(II)  = 0.0
	WSEVAP(II) = 0.0
	ET(II) = 0.0
	!CU(IP, II) = 0.0
	XKCB(II) = 0.0
	GDD(II) = 0.0
     END DO

     ! If CU has already been assigned by the user, then skip refet.
     IF (.not. hasCIR) THEN
	IF (useCropMode .AND. CNAME(CKEY) .EQ. "CORN_GRAIN") THEN
	   DO II = 1,NDYR
	      REFET(II) = ETC(ib, NYR, II)
	   END DO
	ELSEIF (useCropMode .AND. CNAME(CKEY) .EQ. "SPRING_WHEAT") THEN
	   DO II = 1,NDYR
	      REFET(II) = ETW(ib, NYR, II)
	   END DO
	ELSEIF (ETMETH(ckey).EQ.1) THEN
	   DO II = 1,NDYR
	      REFET(II) = ETR(ib, NYR, II)
	   END DO
	ELSE
	   DO  II = 1,NDYR
	      REFET(II) = ETO(ib, NYR, II)
	   END DO
	ENDIF
     ENDIF

     !-----Calculate Crop Coefficients  (skip if CU is user-supplied)
     IF (.not. hasCIR) THEN
	IF (USE_GDD(CKEY)) THEN
	   WRITE(22,*) "Crop type is ", CNAME(CROP_KEY(IB,IP))
	   CALL GROWTH_GDD(IB, NYR, CKEY, JSTR, JSTP, .TRUE., XKCB, GDD)
	ELSE
	   CALL GROWTH(CKEY,JSTR,JSTP,XKCB)
	END IF
     END IF

     !---------------------------------------------------------------
     !     Begin Daily Loop
     !---------------------------------------------------------------

     DO DOY = 1, actual_nper

	!-----Verify if maximum root is achieved at full cover
	IF ((DOY-JSTR).GE.GDATE5(CKEY)) THEN
	   pincst = 0.0
	   pincrz = 0.0
	ENDIF

	!-----Calculate Gregorian Day and Month from Julian Day
	CALL CLNDR(DOY,m_year,IMON,IDAY)

	!$$$            EVAPDT = IRZ(CKEY)
	!$$$            CAPVAP = SMCAP * EVAPDT/IRZ(CKEY)

	! If CU has already been assigned by the user, then skip er.
	IF (.not. hasCIR) THEN
	   !-----Calculate Effective Precipitation
	   l = max(1, doy-5)
	   er(ip, DOY) = RAIN(ib, nyr, DOY, RNFAC, CN, qirr(l:doy))
	ENDIF

	!-----Calculate Ka and transpiration during Growing Season
	IF (use_smb) THEN

	   IF (nyr .EQ. 1 .AND. DOY.eq.1) THEN     ! initial storage
	      IF (SPFLAG .EQ. 1) THEN
		 !IDS                  istor = irz(key)*awc(key)
		 ! THIS CHANGE ALLOWS THE INITIAL SOIL STORAGE TO BE
		 ! LESS THAN FULL AT THE BEGINING OF THE SIMULATION
		 ! PROFSSIM IS THE PORTION OF PROFILE FULL AT BEGINNING
		 ! OF SIMULATION.
		 !
		 ! ADDED THE ABILITY TO RESET THE INITIAL SOIL MOISTURE TO A USER
		 ! DEFINED PERCENT OF TOTAL AVAILABLE SOIL MOISTURE.
		 !   PROFLAG = 1 MEANS USER SET THE SOIL MOISTURE AT THE BEGINNING
		 !      OF EACH SEASON TO BE A SPECIFIED NUMBER (PROFSSEA).
		 !     
		 IF( PROFLAG(nyr).EQ.1 .OR. PROFLAG(nyr).EQ.3) THEN
		    ISTOR = irz(ckey)*AWCMAD*profssea(nyr)
		 ELSE  
		    ISTOR = irz(ckey)*AWCMAD*profssim
		 ENDIF
	      ELSE
		 ISTOR = 12.0*irz(ckey)*AWCMAD
	      ENDIF
	      SMSTG(DOY) = ISTOR ! storage at the start
	      ! of simulation based on
	      ! initial root depth
	   ELSE                   ! adjust rootzone and add wbu
	      !-----Update soil moisture capacity

	      IF (SPFLAG .EQ. 1) THEN
		 istor = irz(ckey)*AWCMAD
	      ELSE
		 istor = 12.0*irz(ckey)*AWCMAD
	      ENDIF
	      IF( frz(ckey) .EQ. 0.0 ) THEN
		 SMSTG(DOY) = 0.0
	      ELSE
		 SMSTG(DOY) = SMSTG(DOY-1)*irz(ckey)/frz(ckey)
	      ENDIF

	      SMSTG(DOY) = amin1(istor,SMSTG(DOY))

	   ENDIF

	   !----------------------------------------------
	   ! Calculate total initial storage of all parcels
	   !----------------------------------------------
	   IF (SPFLAG .EQ. 1) THEN
	      smcap = rz(ckey)*AWCMAD
	   ELSE
	      smcap = 12.0*rz(ckey)*AWCMAD
	   ENDIF

           IF (USE_WATER_STRESS) THEN
	      ! Taken from Design and Operation of Farm Irrigation Systems p. 251
	      TAW = rz(ckey)*awc(skey)
              ! Dr is the amount of depletion from root zone, which is the total RAM - current RAM.
              Dr = (TAW * MAD(CKEY)/100) - SMSTG(DOY)
	      XKA = (TAW - Dr) / ((1 - (MAD(CKEY) / 100)) * TAW)
              IF (XKA .gt. 1.0) THEN
		 XKA = 1.0
              END IF
           ELSE
	      XKA = 1.0
           END IF
	ELSE
	   SMSTG(DOY) = 0.0            ! storage at the start
	   XKA = 1.0
	ENDIF

        if (XKA .lt. 1 .and. SMSTG(DOY) .gt. 0) then
	   print *, "ERROR IN WATER STRESS"
        end if
	
	cuirr(ip,DOY) = 0.0
	exces_er(ip,DOY) = 0.0

	! CU will already be assigned if it user-supplied.
	IF (.not. hasCIR) THEN
	   IF ((CNAME(CKEY) .EQ. "CORN_GRAIN" .OR. CNAME(CKEY) .EQ. "SPRING_WHEAT").AND.(useCropMode)) THEN
	      CU(IP, DOY) = REFET(DOY)
	   ELSE
	      !IDS Commented out effect of reducing the amount of available soil moisture.  This is
	      !IDS   shown in the shorts_d variable.
	      CU(IP, DOY) = REFET(DOY) * XKCB(DOY) * XKA
	   END IF
	ELSE
	   cuirr(IP, DOY) = cu(IP, DOY)
	END IF

!          Calculate an initial shortage that represents the water that the plant is unable
!	      to take up.
	shorts_stress(DOY) = shorts_stress(DOY) + REFET(DOY) * XKCB(DOY) * (1-XKA)

	!
	!-----------Calculate amount of excess precipitation above effective
	!-----------If ERainSM = 1 Use excess rainfall otherwise set excess
	!-----------Rainfall to zero.

	extra_dp = 0
	IF (er(ip,DOY).GT.cu(ip,DOY)) THEN
	   extra_dp = er(ip,DOY) - cu(ip,DOY)
	   IF (ERainSM .EQ. 1) THEN
	      exces_er(ip,DOY) = extra_dp
	   ELSE
	      exces_er(ip,DOY) = 0.0
	   ENDIF
	   er(ip,DOY) = cu(ip,DOY)
	ENDIF

	!$$$            if (ISUPLY .EQ. 1) then
	!--------------Calculate Consumptive Use Requirement by Soil Moisture Budget
	CALL ACOUNT (DOY, SMCAP, CU(IP,DOY), WSEVAP(DOY), &
	     ER(IP,DOY), XKS, DEFIT, use_smb, &
	     ! Outputs
	     QIRR(DOY), ET(DOY), DPERC(DOY), SMSTG(DOY))
	!$$$            else
	!$$$C--------------Calculate Consumptive Use Requirement by original method.
	!$$$               
	!$$$            endif

	! Patterson: this will output effective rain greater than CU 
	!   to the detailed output.
	DPERC(DOY) = extra_dp

	!-----Assign temporary variable to storage at day 0
	IF (DOY.eq.1) THEN 
	   SMSTG_PREV = SMSTG(DOY)
	ELSE
	   SMSTG_PREV = SMSTG(DOY-1)
	ENDIF

	!
	!              Increase the CU to account of the additional losses if
	!              Sprinklers are being used.
	!
	IF(SPRINKLER(IB,IP) .EQ. 1) THEN
	   sprink_l(IP, DOY) = (cu(IP,DOY) - ER(IP,DOY)) *inc_iwr(IB)
	ENDIF

	!           Update cuirr
	cuirr(ip, doy) = (CU(IP,DOY) + SPRINK_L(IP, DOY)) - ER(IP,DOY)

	!-----End of daily Calculations
     END DO

     DO DOY = 1,NDYR
	!-----Calculate Gregorian Day and Month from Julian Day
	CALL CLNDR(DOY,m_year,IMON,IDAY)

	!-----Write results to Output Files
	if (.not. use_smb) then
	   ! Only write detailed output once, so test use_smb flag.
	   CALL FOUTPUT(IP, nyr, IB, DOY, NYR1 + nyr -1, &
		IMON, IDAY, JSTR, JSTP, SMSTG(DOY), SMSTG_PREV, &
		REFET(DOY), XKS, XKA, XKCB(DOY), &
		ET(DOY), ER(IP, DOY), DPERC(DOY), QIRR(DOY), NDYR)
	endif
     END DO

     !-----End of Crop calculations
  END DO

1000 RETURN 

101 CALL MYEXIT(10)

END SUBROUTINE PROTO

