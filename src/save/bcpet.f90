SUBROUTINE BCPET(IB, et_method, nyr, actual_nper, ttemps, ddays, &
     ttempf, ddayf, xxf, xxkt, xxkc, Ret, &
     nbegmo, nbegda, nendmo, nendda, tot_rain, &
				!     Outputs
     cu, sprink_l, cuirr, er, exces_er)

  !***************************************************************************
  !
  !   Function        : bcpet.f
  !   Author          : Luis A. Garcia
  !   Date            : October 1999
  !   Purpose         : This calculates the potential ET by Blaney- 
  !                     Criddle ET method.  The original version is based
  !                     on the SCS Modified Blaney-Criddle method. 
  !   Calling program : calpcrop.f 
  !   Called programs : clndr.f, perencrp.f, annuacrp.f, xcrain.f 
  !   Input arguments : ib = current basin 
  !   Output arguments: none
  !   Assumptions     :
  !   Limitations     :
  !   Notes           : The routines for the original Blaney-Criddle method 
  !                     are based on USBR XCONS2 program which uses the SCS 
  !                     Modified Blaney-Criddle ET Estimation Method.
  !
  !   History         :(Date, Author, Description)
  !
  !
  !***************************************************************************

  USE Globals

  !     Parameters
  INTEGER IB, et_method, nyr, actual_nper
  REAL ttemps(:), ddays(:), ttempf(:)
  REAL ddayf(:)
  REAL xxf(:,:),Ret(:,:),xxkc(:,:)
  REAL xxkt(:,:), tot_rain(:,:)
  INTEGER nbegmo(:), nbegda(:), nendmo(:), nendda(:)
  !     Output
  REAL cu(:,:), sprink_l(:,:), cuirr(:,:), er(:,:), exces_er(:,:)

  INTERFACE
     INTEGER FUNCTION DaysInYear(yr)
       INTEGER yr
     END FUNCTION DaysInYear

     SUBROUTINE KPANNUAL(ib,ckey,nyr,begs,ends,begmo,begda,endmo,endda, &
	  !Outputs
	  t_npart, xkc, days, temps, dayf, tempf)
       INTEGER ib, nyr, ends, begmo, endmo, begs, ckey
       INTEGER begda, endda
       INTEGER t_npart
       REAL xkc(12)
       REAL days, temps, dayf, tempf
     END SUBROUTINE KPANNUAL

     SUBROUTINE KPPEREN(ib,ckey,nyr,begs,ends,begmo,begda,endmo,endda, &
	  !Outputs
	  t_npart, xkc, days, temps, dayf, tempf)
       INTEGER ib, begs, begmo, begda, endmo, endda, ends, ckey, nyr
       INTEGER t_npart
       REAL xkc(12)
       REAL days, temps, dayf, tempf
     END SUBROUTINE KPPEREN

     SUBROUTINE ANNUACRP(ib,ckey,nyr,begs,ends,begmo,begda,endmo,endda,  &
	  !Outputs
	  t_npart, days, temps, dayf, tempf, xf, xkt, xkc)
       INTEGER ib,ckey,nyr, ends, begs
       INTEGER begmo,begda,endmo,endda
       INTEGER t_npart
       REAL days, temps, dayf, tempf
       REAL xf(12), xkt(12), xkc(12)
     END SUBROUTINE ANNUACRP

     SUBROUTINE PERENCRP(ib,ckey,nyr,begs,ends,begmo,begda,endmo, &
          endda,et_method,xf,xkt,xkc, &
	  !Outputs
	  t_npart, days, temps, dayf, tempf)
       INTEGER ib, ckey, nyr, ends, begs, et_method
       INTEGER begmo,begda,endmo,endda
       REAL xf(12), xkt(12), xkc(12)
       INTEGER t_npart
       REAL days, temps, dayf, tempf
     END SUBROUTINE PERENCRP

     SUBROUTINE XCRAIN(ib,ip,nyr,begmo,begda,endmo,endda,apdep, &
	  !Outputs
          t_npart, cu, er, tot_rain)
       INTEGER ib,ip,nyr,begmo,begda,endmo,endda
       REAL apdep
       INTEGER t_npart
       REAL er(:,:), cu(:,:), tot_rain(:,:)
     END SUBROUTINE XCRAIN

     INTEGER FUNCTION CRPTYPE (ndx)
       INTEGER ndx
     END FUNCTION CRPTYPE

     LOGICAL FUNCTION CheckCropCoeffs (et_method, ckey)
       INTEGER et_method, ckey
     END FUNCTION CheckCropCoeffs
  END INTERFACE

  !-----Local Variable Declaration
  REAL xf(12), xkt(12), xkc(12), Lat_heat
  INTEGER IP, K, L, CKEY, DOY
  INTEGER nbeg, nend, ndays
  REAL days, temps, dayf, tempf
  REAL Dr, LAT_rad, SS_ang, SD, Ra, Tavg, PC_ELEV_ADJ

  ALLOCATE(CUHG(nparce(IB), 12))

  DO IP = 1, nparce(IB)
     ckey = crop_key(IB,IP)

     nbeg = jbeg(IP,nyr)
     nend = jend(IP,nyr)

     !-----------------------------------------------------------------------
     !  start calculating potential crop evapotranspiration by blaney-criddle
     !-----------------------------------------------------------------------
     !---------------------------------
     !Initialize monthly values to zero
     !---------------------------------
     DO K = 1,12
	xf(K) = 0.0
	xkt(K) = 0.0
	xkc(K) = 0.0
	xxf(IP, K) = 0.0
	xxkt(IP, K) = 0.0
	xxkc(IP, K) = 0.0
	nperct(K) = 0.0
	naccum(K) = 0
     END DO

     if (nbeg .gt. nend) cycle

     !----------------------------------------------------------
     !Using Kimberly Penman or Hargreaves read crop coefficients
     !----------------------------------------------------------
     ! Patterson: added calibrated BC here so that it uses grass/alfalfa reference crop coefficients.
     IF( et_method .EQ. 7) THEN
	IF (crptype(ckey).eq.1) &
	     CALL KPPEREN(ib, ckey, nyr, nbeg, nend, &
	     nbegmo(IP), nbegda(IP),nendmo(IP),nendda(IP), &
	     npart(IP), xkc,days, temps, dayf, tempf)

	IF (crptype(ckey).eq.2) &
	     CALL KPANNUAL(ib, ckey, nyr, nbeg, nend, &
	     nbegmo(IP), nbegda(IP),nendmo(IP),nendda(IP), &
	     npart(IP), xkc,days, temps, dayf, tempf)
	
     !--------------------
     !Using Blaney-Criddle
     !--------------------
     ELSEIF( et_method.EQ.1) THEN
	!-------------------
	!perrenial or annual
	!-------------------
	IF (crptype(ckey).eq.1) &
	     CALL PERENCRP(ib,ckey,nyr,nbeg,nend,nbegmo(IP), &
	     nbegda(IP),nendmo(IP),nendda(IP), et_method, &
	     xf, xkt, xkc, &
	     npart(IP),days, temps, dayf, tempf)
	IF (crptype(ckey).eq.2) &
	     CALL ANNUACRP(ib,ckey,nyr,nbeg,nend,nbegmo(IP), &
	     nbegda(IP),nendmo(IP),nendda(IP), &
	     npart(IP),days, temps, dayf, tempf, &
	     xf, xkt, xkc)

     !--------------------
     !Using Calibrated Blaney-Criddle
     !--------------------
     ELSEIF( et_method.EQ.5) THEN
	! This gives us F
	IF (crptype(ckey).eq.1) &
	     CALL PERENCRP(ib,ckey,nyr,nbeg,nend,nbegmo(IP), &
	     nbegda(IP),nendmo(IP),nendda(IP), et_method, &
	     xf, xkt, xkc, &
	     npart(IP),days, temps, dayf, tempf)
	IF (crptype(ckey).eq.2) &
	     CALL ANNUACRP(ib,ckey,nyr,nbeg,nend,nbegmo(IP), &
	     nbegda(IP),nendmo(IP),nendda(IP), &
	     npart(IP),days, temps, dayf, tempf, &
	     xf, xkt, xkc)

	! Calibrated BC uses the Kc calculated from hargreaves with F calculated from Blaney-Criddle.
	IF (CBC_USE_REF_KC) THEN
	   ! This gives us Kc using Hargreaves method.
	   IF (crptype(ckey).eq.1) &
		CALL KPPEREN(ib, ckey, nyr, nbeg, nend, &
		nbegmo(IP), nbegda(IP),nendmo(IP),nendda(IP), &
		npart(IP), xkc,days, temps, dayf, tempf)

	   IF (crptype(ckey).eq.2) &
		CALL KPANNUAL(ib, ckey, nyr, nbeg, nend, &
		nbegmo(IP), nbegda(IP),nendmo(IP),nendda(IP), &
		npart(IP), xkc,days, temps, dayf, tempf)
	END IF

     !---------------
     !Using Pochop BC
     !---------------
     ELSEIF( et_method.EQ.8) THEN
	!-------------------
	!perrenial
	!-------------------
	IF (crptype(ckey).eq.1) THEN
	   CALL PERENCRP(ib,ckey,nyr,nbeg,nend,nbegmo(IP), &
                nbegda(IP),nendmo(IP),nendda(IP), et_method, &
		xf, xkt, xkc, &
                npart(IP),days, temps, dayf, tempf)
	ENDIF

     ENDIF
     !-----------------------------------------------------------
     !Compute potential crop evapotranspiration by Blaney-Criddle
     !-----------------------------------------------------------
     IF (.not. CheckCropCoeffs(et_method, ckey)) then
	write(*, fmt='(a,x,a10)') &
	     "No crop coefficients provided for ", CNAME(CKEY)
	call myexit(100)
     endif

     DO L=1,actual_nper
	!Override Kt if the crop is a phreatophyte.  For now assume 1.
	IF (crop_type(ckey).eq.3 .or. BC_KC_OVERRIDES_KT) THEN
	   xkt(L) = 1.0
	ENDIF

	!--------------------
	!Using Blaney-Criddle
	!--------------------
	IF( et_method .EQ. 1) THEN
	   cu(IP,L) = xf(L)*xkt(L)*xkc(L)
	!-----------------------------------  
	!Calibrated Blaney-Criddle Equations
	!-----------------------------------
	ELSEIF( et_method .EQ. 5) THEN
	   IF (CBC_USE_REF_KC) THEN
	      cu(IP,L) = xf(L)*xkc(L)*cbc(ckey,L)
	   ELSE
	      cu(IP,L) = xf(L)*xkt(L)*xkc(L)*cbc(ckey,L)
	   END IF
	!--------------------
	!Using Pochop Blaney-Criddle
	!--------------------
	ELSEIF( et_method .EQ. 8) THEN
	   cu(IP,L) = xf(L)*xkt(L)*xkc(L)
	   !-------
	   !bluegrass
	   !-------
	   IF (sub_crop_type(ckey) .EQ. 2) THEN
	      IF (L .GT. 5 .AND. L .LT. 9) THEN
		 PC_ELEV_ADJ = 0.094
		 !LAG  PC_ELEV_ADJ = 0.076 - Based on original ASAE Paper
	      ELSE  
		 PC_ELEV_ADJ = 0.094
	      ENDIF
	      IF (belev(ib) .LT. 10.0) THEN
		 write(*,*) 'Error - Field ',ib,' Elev ',belev(ib)
		 CALL myexit(151)
	      ENDIF
	      cu(IP,L) = cu(IP,L) + (PC_ELEV_ADJ * cu(IP,L)) &
                   * ((belev(ib) - 4429) / (1000 * 3.281))
	   !-----
	   !Alfalfa
	   !-----
	   ELSEIF (crop_type(ckey) .EQ. 0) THEN 
	      IF(L .GT. 5 .AND. L .LT. 9) THEN
		 PC_ELEV_ADJ = 0.064
		 !LAG   PC_ELEV_ADJ = 0.091 - Based on original ASAE Paper
	      ELSE  
		 PC_ELEV_ADJ = 0.064
	      ENDIF
	      IF( belev(ib) .LT. 10.0) THEN
		 write(*,*) 'Field ',ib,' Elev ',belev(ib)
		 CALL myexit(151)
	      ENDIF
	      cu(IP,L) = cu(IP,L) + (PC_ELEV_ADJ * cu(IP,L)) &
                   * ((belev(ib) - 4429) / (1000 * 3.281))
	   ENDIF
	!----------------
	!Using Hargreaves
	!----------------
	ELSEIF( et_method .EQ. 7) THEN
	   if (Tmean3(IB,NYR,L) .gt. -999) then
	      DOY = middle(L)
	      !DOY = julian(L, 15, nyr1 + nyr - 1)
	      Dr = 1 + 0.033 * COS(((2.*PI)/365.)*DOY)	! eq. 23 p. 23 ASCE Reference ET Equation 2005
	      SD = 0.409 * SIN((((2.*PI)/365.)*DOY)-1.39) ! eq. 24 p. 24
	      LAT_rad = (PI/180.0) * BLAT(IB) ! eq. 22 p. 23   
	      SS_ang = ACOS(-TAN(LAT_rad)*TAN(SD)) 
	      Ra = (24.0/PI) * 4.92 * Dr * ((SS_ang * SIN(LAT_rad)  &
		   * SIN(SD)) +(COS(LAT_rad) * COS(SD) * SIN(SS_ang)))  ! eq. 21 p. 23
	      Tavg  = .5556 * (Tmean3(IB,NYR,L) - 32) ! farenheit to centigrade
	      Lat_heat = 2.501 - 0.002361 * Tavg ! eq. 7.1 p. 133 ASCE 70
	      CUHG(IP,L) = (0.0023 * Ra * (tavg + 17.8)  &
		   * (TRNG_C(IB,NYR,L)**0.5))/Lat_heat
	      !--------------------------------------
	      ! Convert potential ET from mm to inches
	      !--------------------------------------
	      ndays = month(L)
	      if (L .eq. 2) then
		 if (DaysInYear(NYR1+nyr-1) .eq. 366) ndays = 29
	      endif

	      CUHG(IP,L) = (CUHG(IP,L)/25.4) * ndays
	      IF (L.EQ.nbegmo(IP)) THEN
		 cu(IP,L)=xkc(L)*CUHG(IP,L)*((1.0*npart(IP))/ndays)
	      ELSEIF (L.EQ.nendmo(IP)) THEN
		 cu(IP,L)=xkc(L)*CUHG(IP,L)*((1.0*nendda(IP))/ndays)
	      ELSE
		 cu(IP,L)=xkc(L)*CUHG(IP,L)
	      ENDIF
	   endif
	ENDIF

	!Garcia & Patterson: added elevation adjustment to ET calc.

	IF (BC_USE_ELEV .and. et_method.EQ.5) THEN
	   cu(IP,L) = cu(IP,L) + (BC_ELEV_PERC(L)/100. * cu(IP,L)) &
		* ((belev(ib) - BC_ELEV_BASE) / (1000 * 3.281))
	ENDIF
     END DO

     !------------------------------------------------------
     !Assign variables used for generating *.obc output file
     !------------------------------------------------------
     ttemps(IP) = temps
     ddays(IP) = days
     ttempf(IP) = tempf
     ddayf(IP) = dayf
     DO L=1,12
	IF(SPFLAG .EQ. 1 .AND. et_method .EQ. 7) THEN
	   Ret(IP,L) = CUHG(IP,L)
	ENDIF

	xxf(IP,L) = xf(L)
	xxkt(IP,L) = xkt(L)
	xxkc(IP,L) = xkc(L)
     END DO

     !-----------------------------------------------------------------------
     !  start calculating effective rainfall and irrigation water reqts 
     !-----------------------------------------------------------------------

     !--------------------------
     !Compute Effective Rainfall
     !-------------------------- 
     CALL XCRAIN(ib,ip,nyr,nbegmo(IP),nbegda(IP), &
	  nendmo(IP),nendda(IP),APD, &
	  npart(IP), cu, er, tot_rain)
     !-------------------------
     !Initialize totals to zero
     !-------------------------
     cu(ip,13) = 0.0
     sprink_l(ip,13) = 0.0
     er(ip,13) = 0.0
     cuirr(ip,13) = 0.0
     exces_er(ip,13) = 0.0

     DO L=1,12

	cuirr(ip,L) = 0.0
	exces_er(ip,L) = 0.0

	IF(er(ip,L).GT.cu(ip,L)) THEN

	   !Calculate amount of excess precipitation above effective
	   !If ERainSM = 1 Use excess rainfall otherwise set excess
	   !  Rainfall to zero.

	   IF( ERainSM .EQ. 1) THEN
	      exces_er(ip,L) = er(ip,L) - (cu(ip,L))
	   ELSE
	      exces_er(ip,L) = 0.0
	   ENDIF
	   er(ip,L) = cu(ip,L)
	ENDIF

	!If the month is outside the season then set the
	!  excess rainfall to the total rainfall * snow melt
	!  efficiency.

	IF ((L.LT.fbegmo(IB,nyr)).OR.(L.GT.fendmo(IB,nyr))) THEN 

	   exces_er(ip,L) = tot_rain(IP,L)*SMEF(nyr)

	ENDIF

	!During the growing season set the effective precipitation equal to the CU

	!Increase the CU to account for the additional losses if
	!  Sprinklers are being used.

	IF(SPRINKLER(IB,IP) .EQ. 1) THEN
	   sprink_l(IP, L) = (cu(IP,L) - er(ip,L)) *inc_iwr(ib)
	ELSE
	   sprink_l(IP, L) = 0
	ENDIF

	cuirr(ip,L) = (cu(ip,L) + sprink_l(ip, L)) - er(ip,L)

	cu(ip,13) = cu(ip,13) + cu(ip,L)
	sprink_l(ip,13) = sprink_l(ip,13) + sprink_l(ip,L)
	er(ip,13) = er(ip,13) + er(ip,L)
	exces_er(ip,13) = exces_er(ip,13) + exces_er(ip,L)
	cuirr(ip,13) = cuirr(ip,13) + cuirr(ip,L)
661  END DO

800 END DO

  DEALLOCATE(CUHG)

  RETURN
END SUBROUTINE BCPET
