SUBROUTINE PROTO(IB, et_method, nyr, actual_nper, DOY, storag, use_smb, LAI, Dwt, WCwp, soilClass, suploss, &
				!     Output
     cu, sprink_l, cuirr, er, exces_er, shorts_d, shorts_stress, XKCB, XKA, et, Qu)

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


  !-----Include Global Variables and Data Degaults
  USE Globals

  !     Parameters
  INTEGER IB, et_method, nyr, actual_nper
  REAL STORAG(:)
  LOGICAL use_smb ! If TRUE, then use a soil moisture budget
  INTEGER soilClass(:)
  REAL LAI(:, :, :), Dwt(:, :, :)
  REAL WCwp(:), suploss(:,:)

  !     Output
  REAL cu(:,:), cuirr(:,:), er(:,:), exces_er(:,:), shorts_d(:), shorts_stress(:)
  REAL sprink_l(:,:), XKCB(:,:), XKA(:,:), et(:,:), Qu(:,:)

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

     REAL FUNCTION RAIN (ib, nyr, DOY, RNFAC, CN, suploss)
       INTEGER ib, nyr, DOY
       REAL RNFAC, CN(3), suploss(:,:)
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
  INTEGER IP
  INTEGER JSTR, JSTP
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
  REAL SMCAP       ! Soil moisture capacity
  REAL ISTOR       ! Initial Soil Storage
  REAL SMCAP0      ! Initial soil moisture capacity
  REAL SMSTGD      ! Soil moisture storage
  !      REAL SMSTG0      ! Initial soil moisture storage
  REAL SMSTG_PREV  ! Previous day's soil moisture
  REAL RNFAC       ! Rainfall factor
  !      REAL EVAPTD      ! Don't know
  REAL XKS    ! Unknown
  REAL GDD(366)    ! Accumulated growing degree days for the given day.

  INTEGER l, IERR
  REAL extra_dp

  CHARACTER*120 dfile1

  ! Water uptake parameters
  REAL et_p ! potential ET, needed for when the user is using user-provided ET, which is the actual ET.
  REAL WU_a1, WU_a2, WU_a3, WU_a4, WU_b1, WU_b2, WU_b3, WU_b4
  REAL et_mm ! crop ET in mm
  REAL et_a_mm ! crop ET in mm
  REAL rz_mm ! root zone in feet to meters.
  REAL TAW_mm ! total available water in mm
  REAL STORAG_mm ! total RAM available in mm
  REAL Dr_mm ! soil moisture depletion in mm
  REAL WC
  REAL k ! ?
  REAL Dwtc !?
  REAL WCs !?
  REAL WCc
  REAL Qu_max
  REAL WCfc

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
     if (.not. (CheckCropCoeffs(et_method, ckey) .or. hasUserSuppliedCU)) then
	!if (KCB(ckey,1) .eq. 0 .and. cu(1,1) .lt. 0) then
	write(*, fmt='(a,x,a10)') &
	     "No crop coefficients provided for ", CNAME(CKEY)
	call myexit(100)
     endif

     !         SMSTG0 = SMCAP0
     !         IF (JSTR.NE.1) SMSTG(JSTR-1) = SMCAP0
     JSTR = JBEG(IP,nyr)    ! julian day start
     JSTP = JEND(IP,nyr)    ! julian day end

     !-----Initialize Variables
     QIRR(DOY)   = 0.0
     DPERC(DOY)  = 0.0
     WSEVAP(DOY) = 0.0
     !CU(IP, DOY) = 0.0
     GDD(DOY) = 0.0

     IF (useCropMode .AND. CNAME(CKEY) .EQ. "CORN_GRAIN") THEN
        REFET(DOY) = ETC(ib, NYR, DOY)
     ELSEIF (useCropMode .AND. CNAME(CKEY) .EQ. "SPRING_WHEAT") THEN
        REFET(DOY) = ETW(ib, NYR, DOY)
     ELSEIF (ETMETH(ckey).EQ.1) THEN
        REFET(DOY) = ETR(ib, NYR, DOY)
     ELSE
        REFET(DOY) = ETO(ib, NYR, DOY)
     ENDIF

     !-----Calculate Crop Coefficients ! Patterson: enabled for user-supplied Cu for upflux calculation
     IF (.true. .or. .not. hasUserSuppliedCU) THEN
	IF (USE_GDD(CKEY)) THEN
	   WRITE(22,*) "Crop type is ", CNAME(CROP_KEY(IB,IP))
	   CALL GROWTH_GDD(IB, NYR, CKEY, JSTR, JSTP, .TRUE., XKCB(IP,:), GDD)
	ELSE
	   CALL GROWTH(CKEY,JSTR,JSTP,XKCB(IP,:))
	END IF
     END IF

     !---------------------------------------------------------------
     !     Begin Daily Loop
     !---------------------------------------------------------------

     !-----Verify if maximum root is achieved at full cover
     IF ((DOY-JSTR).GE.GDATE5(CKEY)) THEN
	pincst = 0.0
	pincrz = 0.0
     ENDIF

     !-----Calculate Gregorian Day and Month from Julian Day
     CALL CLNDR(DOY,m_year,IMON,IDAY)

     !$$$            EVAPDT = IRZ(CKEY)
     !$$$            CAPVAP = SMCAP * EVAPDT/IRZ(CKEY)

     !-----Calculate Effective Precipitation
     er(ip, DOY) = RAIN(ib, nyr, DOY, RNFAC, CN, suploss)

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

	IF (USE_WATER_STRESS .AND. use_smb) THEN
    ! Taken from Design and Operation of Farm Irrigation Systems p. 251
	   TAW = rz(ckey)*awc(skey)
           ! Dr is the amount of depletion from root zone, which is the total RAM - current RAM.
           Dr = (TAW * MAD(CKEY)/100.0) - STORAG(IP)

           if (Dr .gt. TAW) then
              print *, "warning: root depletion exceeds total moisture."
              Dr = TAW
           end if
           IF (MAD(CKEY) .LT. 100) THEN
              XKA(IP,DOY) = (TAW - Dr) / ((1 - (MAD(CKEY) / 100.0)) * TAW)
              IF (XKA(IP,DOY) .gt. 1.0) THEN
                 XKA(IP,DOY) = 1.0
              END IF
           ELSE
              ! This is the case where there is no management allowed depletion, so the plant
              !   is able to use all the available water in the root zone.  Once this water
              !   is gone, then there can be no ET.
              IF (Dr .gt. TAW) THEN
                 XKA(IP,DOY) = 0.0
              ELSE
                 XKA(IP,DOY) = 1.0
              END IF
           
           END IF
        ELSE
           XKA(IP,DOY) = 1.0
        END IF
     ELSE
	SMSTG(DOY) = 0.0            ! storage at the start
	XKA(IP,DOY) = 1.0
     ENDIF

     if (XKA(IP,DOY) .lt. 1 .and. STORAG(IP) .gt. 0) then
	print *, "ERROR IN WATER STRESS"
     end if

     cuirr(ip,DOY) = 0.0
     exces_er(ip,DOY) = 0.0

     ! CU will already be assigned if it user-supplied.
     IF (.not. hasUserSuppliedCU) THEN
	IF ((CNAME(CKEY) .EQ. "CORN_GRAIN" .OR. CNAME(CKEY) .EQ. "SPRING_WHEAT").AND.(useCropMode)) THEN
	   CU(IP, DOY) = REFET(DOY)
	ELSE
	   CU(IP, DOY) = REFET(DOY) * XKCB(IP, DOY) * XKA(IP,DOY)
	END IF

        ! calculate et potential from reference
        et_p = REFET(DOY) * XKCB(IP, DOY)
     ELSE
        ! Patterson: calculate potential ET for groundwater upflux calculation
        et_p = CU(IP, DOY)
     END IF

     ! Calculate an initial shortage that represents the water that the plant is unable
     !   to take up.
     shorts_stress(DOY) = shorts_stress(DOY) + REFET(DOY) * XKCB(IP, DOY) * (1-XKA(IP,DOY))

     IF (soilClass(IP) .GE. 0 .AND. use_smb .AND. Dwt(IP, nyr, doy) .GT. 0) THEN
        ! Calculate upward flux from water table.
        !   Note all units are in mm
        ! Assign parametric variables using soilClass
        if (soilClass(IP) .eq. 0) then
           ! silt
           WU_a1 = 4.6
           WU_a2 = -1.3
           WU_a3 = 321.75
           WU_a4 = 375
           WU_b1 = -0.65
           WU_b2 = 6.6
           WU_b3 = -0.27
           WU_b4 = -0.17
        elseif (soilClass(IP) .eq. 1) then
           ! sandy loam
           WU_a1 = 7.55
           WU_a2 = -0.15
           WU_a3 = 321.75
           WU_a4 = 375
           WU_b1 = -2.03
           WU_b2 = 2.1
           WU_b3 = -0.54
           WU_b4 = -0.16
        elseif (soilClass(IP) .eq. 2) then
           ! clay loam
           WU_a1 = 1.11	
           WU_a2 = -1.4
           WU_a3 = 321.75
           WU_a4 = 375
           WU_b1 = -0.98
           WU_b2 = 6.8
           WU_b3 = -0.16
           WU_b4 = -0.32
        end if

        ! The groundwater upflux code is based on potential ET (et_p).
        et_mm = et_p * 25.4
        et_a_mm = CU(IP, DOY) * 25.4
        rz_mm = irz(ckey) * 0.3048 * 1000.0

        ! WC is total water content, or TAM + water stored below wilting point.
        TAW_mm = rz(ckey)*awc(skey) * 25.4
        STORAG_mm = STORAG(IP) * 25.4 ! STORAG is related to RAM
        Dr_mm = (TAW_mm * MAD(CKEY)/100.0) - STORAG_mm

        ! net water content is (total available soil moisture - depleted moisture)
        WC = (TAW_mm - Dr_mm)

        ! New method to calculate a_3
        WCfc = awc(skey) * 25.4
        WU_a3 = 1.1 * (WCfc + WCwp(IP)) / 2.0
        WU_a4 = WCfc

        ! k calculation
        IF (.not. hasUserSuppliedCU) THEN
           IF (et_mm .LE. 4) THEN
              k = (1 - EXP(-0.6 * LAI(IP, nyr, doy)))
           ELSE
              k = 3.8 / et_mm
           END IF
        ELSE
           k = 1
        END IF

        IF (et_mm .LE. 4) THEN
           Dwtc = (WU_a2 * et_mm + WU_b2)
        ELSE
           Dwtc = 1.4
        END IF

        IF (Dwt(IP, nyr, doy) .LE. 3) THEN
           WCs = (WU_a3 * Dwt(IP, nyr, doy) ** WU_b3)
        ELSE
           WCs = WCwp(IP)
        END IF

        ! Spread to root zone
        !WCs = WCs * rz_mm

        WCc = WU_a4 * Dwt(IP, nyr, doy) ** WU_b4

        IF (Dwt(IP, nyr, doy) .LE. Dwtc) THEN
           IF (.not. hasUserSuppliedCU) THEN           
              ! If we don't have actual ET, then we need k to provide an estimate of actual ET from potential.
              Qu_max = (k * et_mm)
           ELSE
              ! Don't need to introduce k term because we already are using actual ET. ****et_a
              Qu_max = et_a_mm
           END IF
        ELSE
           Qu_max = WU_a1 * Dwt(IP, nyr, doy) ** WU_b1
        END IF

        Qu(IP, doy) = 0
        IF (WC .LT. WCs) THEN
           Qu(IP,doy) = Qu_max
        ELSE
           IF (WCs .LE. WC .and. WC .LE. WCc) THEN
              Qu(IP, doy) = Qu_max * ((WCc-WC)/(WCc-WCs))
           END IF
        END IF

        ! Output debugger info
        if (doy .eq. 147) then !5/27
           dfile1 = dfile
           dfile1(flen:flen+4) = '.wupd'

           ! Read to the selected modeling area.
           OPEN (UNIT=2, FILE=dfile1, STATUS='REPLACE', IOSTAT=IERR)

           WRITE(2, *) "ETp,LAI,Dwt,WC,Root Zone (mm),k,Dwtc,WCs,WCc,Qu(max),Qu"
           WRITE(2, '(12(F12.2,","))') et_mm, LAI(IP, nyr, doy), Dwt(IP, nyr, doy), WC, &
                rz_mm, k, Dwtc, WCs, WCc, Qu_max, Qu(IP, doy)
           WRITE(2, *)
           WRITE(2, *) "a1, a2, a3, a4, b1, b2, b3, b4"
           WRITE(2, '(8(F12.2,","))') WU_a1, WU_a2, WU_a3, WU_a4, WU_b1, WU_b2, WU_b3, WU_b4
           WRITE(2, *)
           WRITE(2, *) "WCfc, WCwp, Soil Type"
           WRITE(2, '(2(F12.2,","), (I2))') WCfc, WCwp(IP), soilClass(IP)

           CLOSE(2)
        end if

        ! Convert Qu from mm to inches
        Qu(IP, doy) = Qu(IP, doy) / 25.4

     END IF


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
	  QIRR(DOY), ET(IP, DOY), DPERC(DOY), SMSTG(DOY))
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

     ! Update cuirr
     cuirr(ip, doy) = (CU(IP,DOY) + SPRINK_L(IP, DOY)) - ER(IP,DOY)

     !-----End of daily Calculations
     !-----End of Crop calculations
  END DO

1000 RETURN 

101 CALL MYEXIT(10)

END SUBROUTINE PROTO

