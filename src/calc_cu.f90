SUBROUTINE CALC_CU(et_method, nperiods)

  !***************************************************************************
  !
  !   Function        : calc_cu.f
  !   Author          : Dave Patterson
  !   Date            : Dec 2002
  !   Purpose         : This is the main calling subroutine for calculating
  !                     crop consumptive use by all ET estimation 
  !                     methods.  Adapted from Henry's MAINXC
  !   Calling program : run_cu.f 
  !   Called programs : dayhrs.f, readin.f, calpcrop.f, monthly.f, supply.f
  !                     frost.f, wbuild.f 
  !   Input arguments : et_method - which ET method to use in CU calculation.
  !   Output arguments: none
  !   Assumptions     :
  !   Limitations     :
  !   Version 1.1     : This version contains the OBCFLAG that checks if the
  !                   : detailed output for blaney-criddle was selected if
  !                   : it was not it puts a message in the *.obc file.
  !
  !***************************************************************************

  USE GLOBALS

  ! Parameters
  INTEGER et_method
  INTEGER nperiods ! 12 for monthly, 366 for daily

  INTERFACE
     SUBROUTINE READ_WEATHER(et_method)
       INTEGER et_method ! ET method to use
     END SUBROUTINE READ_WEATHER

     SUBROUTINE READ_CROP_BC()
     END SUBROUTINE READ_CROP_BC

     SUBROUTINE READ_CROP_KP()
     END SUBROUTINE READ_CROP_KP

     SUBROUTINE READ_CROP_POCHOP()
     END SUBROUTINE READ_CROP_POCHOP

     LOGICAL FUNCTION READ_CROP_GDD()
     END FUNCTION READ_CROP_GDD

     SUBROUTINE SUPPLY(IB, &
				! Outputs
	  suply)
       INTEGER IB
       REAL SUPLY(:,:)
     END SUBROUTINE SUPPLY

     SUBROUTINE SPSUPPLY(IB, et_method, &
				! Outputs
	  suply, suploss)
       INTEGER IB ! Index of basin to process
       INTEGER et_method ! ET method to use
       REAL SUPLY(:,:), SUPLOSS(:,:)
     END SUBROUTINE SPSUPPLY

     SUBROUTINE SPDSUPPLY(IB, et_method, &
				! Outputs
	  suply, suploss)
       INTEGER IB ! Index of basin to process
       INTEGER et_method ! ET method to use
       REAL SUPLY(:,:), SUPLOSS(:,:)
     END SUBROUTINE SPDSUPPLY

     SUBROUTINE FROST(ib, daily)
       INTEGER IB ! Index of basin to process
       LOGICAL daily ! True if using daily method
     END SUBROUTINE FROST

     SUBROUTINE CALPCROP(IB, et_method, nperiods, suply, suploss, &
	  ! Outputs
          tot_rain, cuirr, er, cu, sprink_l, &
          storag, exces_er, cussup, shorts, shorts_from_reduc, shorts_stress, &
          runoff, reqt, er_st, carry, &
          wpump, fsuply, sur_sup, wel_sup, avg_rz, maxstore, &
          nbegmo, nbegda, nendmo, nendda)
       INTEGER IB, et_method, nperiods
       REAL SUPLY(:,:), SUPLOSS(:,:)
       ! Outputs
       REAL tot_rain(:,:), cuirr(:,:), er(:,:), cu(:,:), sprink_l(:,:)
       REAL storag(:), exces_er(:,:), cussup(:), shorts(:), shorts_from_reduc(:)
       REAL shorts_stress(:)
       REAL runoff(:), reqt(:), er_st(:), carry(:)
       REAL wpump(:), fsuply(:), sur_sup(:), wel_sup(:), avg_rz(:), maxstore(:)
       INTEGER nbegmo(:), nbegda(:), nendmo(:), nendda(:)
     END SUBROUTINE CALPCROP

     SUBROUTINE WEIGHT(N,NSTA,IB,WW,WP, &
				! Outputs
	  ETR,ETO,RF)
       INTEGER N, NSTA, IB
       REAL WW(:,:), WP(:,:)
       REAL, DIMENSION(:,:) :: ETR, ETO, RF
     END SUBROUTINE WEIGHT

  END INTERFACE

  !-----Local variable declaration
  INTEGER OFLAG
  INTEGER IB, IP, L
  CHARACTER*120 ofile1
  CHARACTER*40 et_method_name

  !     These apply to the current basin.
  REAL SUPLY(nyrs,nperiods) ! Size is nyrs, 12; total river supply.
  REAL SUPLOSS(nyrs, nperiods) ! Size is nyrs, 12; surface water supply after conveyance loss calculated.
  REAL tot_rain(maxparcel,nperiods) ! Size is maxparcel, 12; total rainfall for a particular field.
  REAL cuirr(maxparcel,nperiods+1) ! Size is maxparcel, 12; IWR? 
  REAL er(maxparcel,nperiods+1) ! Size is maxparcel, 12; Effective rainfall to CU (plus total)
  REAL cu(maxparcel,nperiods+1) ! Size is maxparcel, 12; calculated CU of the field for the month (plus total)
  REAL sprink_l(maxparcel,nperiods+1) ! Size is maxparcel, 12; sprinkler spray loss of the field for the month (plus total)
  REAL storag(maxparcel) ! Size is maxparcel; soil moisture storage
  REAL exces_er(maxparcel,nperiods+1) ! Size is maxparcel, 12; Excess precip at field (plus total)
  REAL cussup(nperiods) ! total pumping required to satisfy CU?
  REAL shorts(nperiods) ! Shortage
  REAL shorts_stress(nperiods) ! Shortage due to crop stress
  REAL shorts_from_reduc(nperiods) ! Grass and alfalfa shortage
  REAL runoff(nperiods) ! farm surface water deep perc & runoff
  REAL reqt(nperiods) ! Irrigation Water requirement
  REAL er_st(nperiods) ! Effective rainfall to soil storage
  REAL carry(nperiods) ! Carry-over soil moisture (end of month)
  REAL wpump(nperiods) ! Well water available for CU
  REAL fsuply(nperiods) ! Monthly farm supply
  REAL sur_sup(nperiods) ! CU met by surface water.
  REAL wel_sup(nperiods) ! CU met by groundwater (calculated; don't confuse with well discharge measurements).
  REAL avg_rz(nperiods) ! The average root depth at the basin.
  REAL maxstore(nperiods) ! The max storage capacity at the basin.
  INTEGER nbegmo(maxparcel),nbegda(maxparcel) ! Size is maxparcel; gregorian version of JBEG(IP,nyr)
  INTEGER nendmo(maxparcel),nendda(maxparcel) ! Size is maxparcel; gregorian version of JEND(IP,nyr)
  INTEGER Y

  ! don't need to calculate CIR if the user supplies this.
  hasUserSuppliedCU = USER_ET_MODE .eq. 0 .and. et_method .eq. 9

  isDailyMethod = .false.
  IF(et_method.EQ.2 .OR. et_method.EQ.4 &
       .OR. et_method.EQ.6 .OR. et_method.EQ.10 &
       .OR. et_method.EQ.9) THEN
     isDailyMethod = .true.
  END IF

  IF (allocated(FARM_RO)) THEN
     deallocate(FARM_RO)
     deallocate(FARM_DP)
     deallocate(DITCH_DP)
     deallocate(BWELSUP)
     deallocate(BWELACU)
  ENDIF
  ALLOCATE(FARM_RO(NBASIN, NYRS, nperiods+1, N_ET_M*2))
  ALLOCATE(FARM_DP(NBASIN, NYRS, nperiods+1, N_ET_M*2))
  ALLOCATE(DITCH_DP(NBASIN, NYRS, nperiods+1, N_ET_M*2))

  ALLOCATE(BWELSUP(NBASIN, NYRS, nperiods))
  ALLOCATE(BWELACU(NBASIN, NYRS, nperiods))

  IF (allocated(WellP)) THEN
     DEALLOCATE(WellP)
     DEALLOCATE(WellPWS)
  ENDIF
  ALLOCATE(WellP(NBASIN, NYRS, nperiods))
  ALLOCATE(WellPWS(NBASIN, NYRS, nperiods))

  DO ib=1,nbasin
     DO y=1,nyrs
	DO l=1,nperiods
	   BWELSUP(ib, y, l) = 0
	   BWELACU(ib, y, l) = 0
	   WellP(ib, y, l) = 0
	   WellPWS(ib, y, l) = 0
	   if (ib .eq. 1) then
	      SUPLY(y, l) = 0.0
	      SUPLOSS(y, l) = 0.0
	   end if
	END DO
	jbeg(1,y) = 1
	jend(1,y) = 365
     END DO

     DO l=1,nperiods
	fsuply(l) = 0
     END DO
  END DO

  DO ip=1,MAXPARCEL
     storag(IP) = 0
  END DO

  !-----specify input file extension
  ofile1 = dfile

  ! Reset the growing degree data output file.
  ofile1(flen:flen+4) = '.ogdd'
  OPEN(UNIT=22,FILE=ofile1,STATUS='REPLACE')

  !-----temporary files for water budget
  OPEN(UNIT=11,FILE='tmp1',STATUS='UNKNOWN')
  OPEN(UNIT=12,FILE='tmp2',STATUS='UNKNOWN')

  IF( et_method .EQ. 4) THEN
     ofile1(flen:flen+4) = '.okp'
     ET_METHOD_NAME = "Kimberly-Penman"
  ELSEIF( et_method .EQ. 1) THEN
     ofile1(flen:flen+4) = '.obc'
     ET_METHOD_NAME = "Blaney-Criddle"
  ELSEIF( et_method .EQ. 5) THEN
     ofile1(flen:flen+5) = '.ocbc'
     ET_METHOD_NAME = "Calibrated Blaney-Criddle"
  ELSEIF (et_method .EQ. 2) THEN
     ofile1(flen:flen+4) = '.opm'
     ET_METHOD_NAME = "Penman-Monteith"
  ELSEIF (et_method .EQ. 10) THEN
     ofile1(flen:flen+4) = '.op48'
     ET_METHOD_NAME = "Penman 1948"
  ELSEIF (et_method .EQ. 6) THEN
     ofile1(flen:flen+4) = '.oas'
     ET_METHOD_NAME = "ASCE Standarized Ref. ET"
  ELSEIF( et_method .EQ. 7) THEN
     ofile1(flen:flen+4) = '.ohg'
     ET_METHOD_NAME = "Hargreaves"
  ELSEIF( et_method .EQ. 8) THEN
     ofile1(flen:flen+4) = '.opc'
     ET_METHOD_NAME = "BC - Pochop"
  ELSEIF( et_method .EQ. 9) THEN
     ofile1(flen:flen+4) = '.oet'
     ET_METHOD_NAME = "User-Supplied ET"
  ENDIF

  !-----set obcflag to zero
  oflag = 0

  !-----open detailed output file
  OPEN(UNIT=3,FILE=ofile1,STATUS='UNKNOWN')

  ! Read crop coefficient data

  !-----Write Headings and read crop data.
  IF (et_method .EQ. 4) THEN
     WRITE(3,910) QUOTE,QUOTE,QUOTE,QUOTE
     WRITE(23,973)
  ELSEIF (et_method .EQ. 1) THEN
     WRITE(3,900) QUOTE,QUOTE,QUOTE,QUOTE
     WRITE(23,974)
  ELSEIF (et_method .EQ. 5) THEN
     WRITE(3,911) QUOTE,QUOTE,QUOTE,QUOTE
     WRITE(23,978)
  ELSEIF (et_method .EQ. 2) THEN
     WRITE(3,907) QUOTE,QUOTE,QUOTE,QUOTE
     WRITE(23,970)
  ELSEIF (et_method .EQ. 10) THEN
     WRITE(3,917) QUOTE,QUOTE,QUOTE,QUOTE
     WRITE(23,971)
  ELSEIF (et_method .EQ. 6) THEN
     WRITE(3,912) QUOTE,QUOTE,QUOTE,QUOTE
     WRITE(23,975)
  ELSEIF (et_method .EQ. 7) THEN
     WRITE(3,913) QUOTE,QUOTE,QUOTE,QUOTE
     WRITE(23,976)
  ELSEIF (et_method .EQ. 8) THEN
     WRITE(3,909) QUOTE,QUOTE,QUOTE,QUOTE
     WRITE(23,972)
  ELSEIF (et_method .EQ. 9) THEN
     WRITE(3,814) QUOTE,QUOTE,QUOTE,QUOTE
     WRITE(23,977)
  ENDIF

  ! Check for user-supplied ET mode with NWR provided.
  IF (.true.) THEN
     IF (et_method .EQ. 4) THEN
	CALL READ_CROP_PM()
     ELSEIF (et_method .EQ. 1) THEN
	CALL READ_CROP_BC()
     ELSEIF (et_method .EQ. 5) THEN
	CALL READ_CROP_BC()
	IF (CBC_USE_REF_KC) THEN
	   ! Also read reference crop coefficients
	   CALL READ_CROP_PM()
	END IF
     ELSEIF (et_method .EQ. 2 .OR. et_method .EQ. 10) THEN
	CALL READ_CROP_PM()
     ELSEIF (et_method .EQ. 6) THEN
	CALL READ_CROP_PM()
     ELSEIF (et_method .EQ. 7) THEN
	CALL READ_CROP_PM()
     ELSEIF (et_method .EQ. 8) THEN
	CALL READ_CROP_POCHOP()
     ELSEIF (et_method .EQ. 9) THEN
	! User ET assumes daily
	CALL READ_CROP_PM()
     ENDIF

     IF (et_method .NE. 1 .AND. et_method .NE. 5) THEN
	HAS_GDD = READ_CROP_GDD()
	IF (HAS_GDD) THEN
	   WRITE(22, *) "day, min_temp, max_temp, accumulated gdd, Kc"
	END IF
     ELSE
	HAS_GDD = .FALSE.
     END IF

     CALL READ_WEATHER(et_method)
  END IF

  !-----Start Sub-Basin Calculations
  DO IB = 1, NBASIN
     IF (TYPOUT(IB).EQ.3) THEN
	WRITE(3,901) QUOTE,BAS_ID(IB),QUOTE
	OFLAG = 1
     ENDIF

     ! Find winter wheat parcels because there will be the same number
     !   of fake spring wheat parcels.
     n_wheat_parcel = 0
     DO IP=1,NPARCE(IB)
	IF (SUB_CROP_TYPE(CROP_KEY(IB,IP)) .EQ. 0) THEN
           n_wheat_parcel = n_wheat_parcel + 1
	END IF
     END DO

     !-----Read Water Supply
     IF (ISUPLY.EQ.1 .AND. SPFLAG.EQ.0) THEN
	CALL SUPPLY(IB, suply)
     ELSEIF (ISUPLY.EQ.1 .AND. SPFLAG.EQ.1) THEN
	if (.not. hasDailyMethod) then
	   CALL SPSUPPLY(IB, et_method, suply, suploss)
	else
	   CALL SPDSUPPLY(IB, et_method, suply, suploss)
	endif
     ENDIF

     !-----calculate daylight hours from basin latitude and year
     CALL DAYHRS(BLAT(IB),PCLITE)

     !-----determine growing season from frost dates
     !------- Daily Methods (Penman Monteith, ASCE, Kimberly Penman)
     !--- Patterson: I put this back in even when user supplies CIR values
     !--- because we still need to calculate the potential ET.
     if (.true. .or. .not. hasUserSuppliedCU) then
	if (et_method .NE. 2 .AND. et_method .NE. 6 &
	     .AND. et_method .NE. 4 .AND. et_method .NE. 10 &
	     .AND. et_method .NE. 9) then
	   CALL FROST(IB, .false.)
	   !--------Monthly Methods (BC, Calibrated BC, Hargreaves, Pochop)
	else
	   CALL FROST(IB, .true.)
	endif
     endif

     !-----calculate carry-over soil moisture
     !LAG         CALL WBUILD(2,IB)

     !-----main blaney-criddle, Hargreaves or Pochop calculations
     CALL CALPCROP(IB, et_method, nperiods, suply, suploss, &
	  ! Outputs
	  tot_rain, cuirr, er, cu, sprink_l, &
	  storag, exces_er, cussup, shorts, shorts_from_reduc, shorts_stress, &
	  runoff, reqt, er_st, carry, &
	  wpump, fsuply, sur_sup, wel_sup, avg_rz, maxstore, &
	  nbegmo, nbegda, nendmo, nendda)
  END DO

  IF( OFLAG .EQ. 0 .AND. et_method .EQ. 1) THEN	   
     WRITE(3, 921)
     WRITE(3, 922)
     WRITE(3, 923)
     WRITE(3, 924)
  ELSEIF( OFLAG .EQ. 0 .AND. et_method .EQ. 4) THEN
     WRITE(3, 925)
     WRITE(3, 926)
     WRITE(3, 927)
     WRITE(3, 928)
  ELSEIF( OFLAG .EQ. 0 .AND. et_method .EQ. 5) THEN
     WRITE(3, 935)
     WRITE(3, 936)
     WRITE(3, 937)
     WRITE(3, 938)
  ELSEIF( OFLAG .EQ. 0 .AND. et_method .EQ. 6) THEN
     WRITE(3, 941)
     WRITE(3, 942)
     WRITE(3, 943)
     WRITE(3, 944)
  ELSEIF( OFLAG .EQ. 0 .AND. et_method .EQ. 7) THEN
     WRITE(3, 945)
     WRITE(3, 946)
     WRITE(3, 947)
     WRITE(3, 948)
  ELSEIF( OFLAG .EQ. 0 .AND. et_method .EQ. 8) THEN	   
     WRITE(3, 955)
     WRITE(3, 956)
     WRITE(3, 957)
     WRITE(3, 958)
  ELSEIF( OFLAG .EQ. 0 .AND. et_method .EQ. 9) THEN	   
     WRITE(3, 955)
     WRITE(3, 956)
     WRITE(3, 957)
     WRITE(3, 958)
  ELSEIF( OFLAG .EQ. 0 .AND. et_method .EQ. 10) THEN	   
     WRITE(3, 955)
     WRITE(3, 956)
     WRITE(3, 957)
     WRITE(3, 958)
  ENDIF

  CLOSE(3)
  CLOSE(11)
  CLOSE(12)
  CLOSE(22)

  WRITE(23,*) "END DATA"

900 FORMAT(15x,A1,'Detailed Results of the Consumptive Use Calculation', &
       A1/A1,30x,'Blaney-Criddle Method',A1/)
901 FORMAT(A1,A60,A1/)
904 FORMAT(A1,'Mean Monthly Temperature (Farenheit)',A1)
905 FORMAT(A1,'Total Monthly Rainfall (Inches)',A1)
907 FORMAT(15x,A1,'Detailed Results of the Consumptive Use Calculation', &
       A1/A1,30x,'Penman-Monteith Method',A1/)
917 FORMAT(15x,A1,'Detailed Results of the Consumptive Use Calculation', &
       A1/A1,30x,'Penman 1948 Method',A1/)
909 FORMAT(15x,A1,'Detailed Results of the Consumptive Use Calculation', &
       A1/A1,21x,' Pochop Blaney Criddle Method',A1/)
910 FORMAT(15x,A1,'Detailed Results of the Consumptive Use Calculation', &
       A1/A1,30x,'Kimberly-Penman Method',A1/)
911 FORMAT(15x,A1,'Detailed Results of the Consumptive Use Calculation', &
       A1/A1,24x,' Blaney Criddle Method',A1/)
912 FORMAT(15x,A1,'Detailed Results of the Consumptive Use Calculation', &
       A1/A1,34x,'ASCE Method',A1/)
913 FORMAT(15x,A1,'Detailed Results of the Consumptive Use Calculation', &
       A1/A1,32x,'Hargreaves Method',A1/)
814 FORMAT(15x,A1,'Detailed Results of the User-Supplied ET Calculation', &
       A1/A1,32x,'User-Supplied ET Method',A1/)

914 FORMAT (1x,A1,'Subarea',A1,9x,A1,'Jan',A1,3x,A1,'Feb',A1,3x,A1, &
       'Mar',A1,3x,A1,'Apr',A1,3x,A1,'May',A1,3x,A1,'Jun',A1,3x,A1, &
       'Jul',A1,3x,A1,'Aug',A1,3x,A1,'Sep',A1,3x,A1,'Oct',A1,3x,A1, &
       'Nov',A1,3x,A1,'Dec',A1,2x,A1,'Mean',A1)
916 FORMAT (1x,A1,'Subarea',A1,9x,A1,'Jan',A1,3x,A1,'Feb',A1,3x,A1, &
       'Mar',A1,3x,A1,'Apr',A1,3x,A1,'May',A1,3x,A1,'Jun',A1,3x,A1, &
       'Jul',A1,3x,A1,'Aug',A1,3x,A1,'Sep',A1,3x,A1,'Oct',A1,3x,A1, &
       'Nov',A1,3x,A1,'Dec',A1,2x,A1,'Total',A1)
915 FORMAT(A120)
920 FORMAT(A1,A14,A1,13F8.2)
921 FORMAT(1X,'=====================================================')
922 FORMAT(1X,'THIS RUN DID NOT CONTAIN A DETAILED B-C OUTPUT OPTION')
923 FORMAT(1X,'       FOR ANY OF THE SUB-AREAS BEING MODELED')
924 FORMAT(1X,'=====================================================')

925 FORMAT(1X,'=====================================================')
926 FORMAT(1X,'THIS RUN DID NOT CONTAIN A DETAILED K-P OUTPUT OPTION')
927 FORMAT(1X,'       FOR ANY OF THE SUB-AREAS BEING MODELED')
928 FORMAT(1X,'=====================================================')

935 FORMAT(1X,'=====================================================')
936 FORMAT(1X,'  THIS RUN DID NOT CONTAIN A DETAILED CALIBRATED BC')
937 FORMAT(1X,'OUTPUT OPTION FOR ANY OF THE SUB-AREAS BEING MODELED')
938 FORMAT(1X,'=====================================================')

941 FORMAT(1X,'=====================================================')
942 FORMAT(1X,'   THIS RUN DID NOT CONTAIN DETAILED ASCE OUTPUT')
943 FORMAT(1X,'   OPTION FOR ANY OF THE SUB-AREAS BEING MODELED')
944 FORMAT(1X,'=====================================================')

945 FORMAT(1X,'=====================================================')
946 FORMAT(1X,'   THIS RUN DID NOT CONTAIN A DETAILED HARGREAVES')
947 FORMAT(1X,'OUTPUT OPTION FOR ANY OF THE SUB-AREAS BEING MODELED')
948 FORMAT(1X,'=====================================================')

955 FORMAT(1X,'=====================================================')
956 FORMAT(1X,'   THIS RUN DID NOT CONTAIN A DETAILED POCHOP BC')
957 FORMAT(1X,'OUTPUT OPTION FOR ANY OF THE SUB-AREAS BEING MODELED')
958 FORMAT(1X,'=====================================================')

959 FORMAT(1X,'=====================================================')
960 FORMAT(1X,'   THIS RUN DID NOT CONTAIN A DETAILED USER-ET')
961 FORMAT(1X,'OUTPUT OPTION FOR ANY OF THE SUB-AREAS BEING MODELED')
962 FORMAT(1X,'=====================================================')

!-- formats for the detailed soil moisture output
970 FORMAT('Detailed Soil Moisture for Penman-Monteith (Water Budget) Method')
971 FORMAT('Detailed Soil Moisture for Penman 1948 (Water Budget) Method')
972 FORMAT('Detailed Soil Moisture for Pochop (Water Budget) Method')
973 FORMAT('Detailed Soil Moisture for Kimberly-Penman (Water Budget) Method')
974 FORMAT('Detailed Soil Moisture for SCS Modified Blaney-Criddle (Enhanced) Method')
975 FORMAT('Detailed Soil Moisture for ASCE Standarized Ref. ET (Water Budget) Method')
976 FORMAT('Detailed Soil Moisture for Hargreaves (Water Budget) Method')
977 FORMAT('Detailed Soil Moisture for User-Supplied ET (Water Budget) Method')
978 FORMAT('Detailed Soil Moisture for Calibrated Blaney-Criddle (Water Budget) Method')


  RETURN 
END SUBROUTINE CALC_CU

