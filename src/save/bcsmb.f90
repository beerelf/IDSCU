SUBROUTINE BCSMB(nyr, IB, nperiods, actual_nper, farmlos, er, exces_er, &
     cuirr, nendmo, nendda, cappeff, &
     !Output
     storag, er_st, carry, &
     shorts, wpump, reqt, fsuply, runoff, cussup, &
     sur_sup, wel_sup, avg_rz, maxstore)

  !***************************************************************************
  !
  !   Function        : bcsmb.f
  !   Author          : Luis A. Garcia
  !   Date            : October 1999
  !   Purpose         : This calculates the soil moisture budget by Blaney-
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

  USE globals

  !Parameters
  INTEGER nyr, IB, nperiods, actual_nper
  INTEGER nendmo(:), nendda(:)
  REAL farmlos(:), er(:,:), exces_er(:,:), cuirr(:,:)
  REAL storag(:), cappeff(:)

  !Output
  REAL wpump(:), reqt(:), shorts(:), er_st(:), carry(:)
  REAL fsuply(:), runoff(:), cussup(:)
  REAL sur_sup(:), wel_sup(:), avg_rz(:), maxstore(:)

  INTERFACE
     INTEGER FUNCTION DaysInYear(yr)
       INTEGER yr
     END FUNCTION DaysInYear

     SUBROUTINE WSDIST(L,IB,nyr,tfsup,Re_Ro, cussup, fsuply, wsuply, &
	  ! Outputs
	  sur_sup, wel_sup)
       INTEGER L, IB, nyr
       REAL tfsup, Re_Ro, cussup(:), fsuply(:), wsuply(:)
       REAL sur_sup(:), wel_sup(:)
     END SUBROUTINE WSDIST

     SUBROUTINE SCHED(IB,IY,L,re2_s,cuirr, &
	  !Outputs
	  stor,erain,dep,stg,shrt)
       INTEGER IB, IY, L
       REAL re2_s(:,:), cuirr(:,:)
       REAL stor(*)
       REAL dep, erain, stg, shrt
     END SUBROUTINE SCHED

     SUBROUTINE WELLPUMP(BASIN,WY,M,SHORTW,PUMP,WSFLAG)
       INTEGER BASIN, M, WY, WSFLAG
       REAL PUMP, SHORTW
     END SUBROUTINE WELLPUMP

     SUBROUTINE WELLSUP(BASIN,WY,L,PUMP,FIELD_EFF,NPERIODS)
       INTEGER BASIN, L, WY, NPERIODS
       REAL PUMP, FIELD_EFF
     END SUBROUTINE WELLSUP

     SUBROUTINE DISTR(IB,IY,L,nperiods,exces_er,cuirr,Qs,Qr,pc_rz,nendmo, &
	  !Outputs
	  er_stor, er2_s, storag, ro, sh, acu, stg, re_ro, SMBSeason)
       INTEGER IB, IY, L, nendmo(:), nperiods
       REAL exces_er(:,:), cuirr(:,:), er2_s(:,:)
       REAL storag(:)
       REAL Qs, Qr, pc_rz(:,:)
       REAL er_stor, ro, sh, acu, stg, re_ro
       INTEGER SMBSeason
     END SUBROUTINE DISTR

     INTEGER FUNCTION JULIAN (m_mon, m_day, yr)
       INTEGER m_mon, m_day, yr
     END FUNCTION JULIAN
  END INTERFACE

  !Local Variable Declaration

  INTEGER L, IP, CKEY, SKEY
  INTEGER daymaxc, ndays
  ! 0 if the current day is within the growing season.
  INTEGER SMBSeason

  REAL wsup, totsup, awcmad
  REAL scap(MAXPARCEL), er2_s(MAXPARCEL,nperiods), istor
  REAL re_ro
  REAL bcdperc(MAXPARCEL,nperiods+1)
  REAL endstor(MAXPARCEL)
  REAL pc_rz(MAXPARCEL,nperiods) ! rootzone at each parcel for each time period.

  ! These adjust for daily or monthly mode.  If monthly, then these are
  !   the starting and ending months.  If daily, then these are the
  !   starting and ending julian days.
  INTEGER harvest_start, harvest_end

  if (nperiods .EQ. 12)  then
     harvest_start = fbegmo(IB,nyr)
     harvest_end = fendmo(IB,nyr)
  else
     ! TODO: handle leap years.
     harvest_start = fbegda(IB, nyr)
     harvest_end = fendda(IB,nyr)
  endif

  !-----------------------
  !Do soil moisture budget
  !-----------------------

  DO IP = 1, nparce(IB)
     DO L = 1, nperiods
	bcdperc(ip,L) = 0.0
	er2_s(ip,L) = 0.0
     END DO
     bcdperc(ip,nperiods+1) = 0.0
  END DO
     
  DO L = 1, nperiods
     !Set the shortages and well pump to zero

     shorts(L) = 0.0
     wpump(L) = 0.0
     cussup(L) = 0.0
     er_st(L) = 0.0
     runoff(L) = 0.0
     carry(L) = 0.0
     sur_sup(L) = 0
     wel_sup(L) = 0
     avg_rz(L) = 0
     maxstore(L) = 0
     !fsuply(L) = 0
  END DO

  DO L = 1, actual_nper
     !Do loop for the number of parcels that are part of each farm

     DO IP = 1, nparce(IB)

	ckey = crop_key(IB,IP)
	skey = soil_key(IB,IP)
	awcmad = mad(ckey) * awc(skey) / 100.0

	if (l .gt. 1) then
	   pc_rz(IP, l) = pc_rz(IP, l-1)
	else
	   pc_rz(IP, l) = rz(ckey)
	end if

	! Check if this is part of the growing season

	if ((L.GE.harvest_start).and.(L.LE.harvest_end)) then
	   IF (L.EQ.harvest_start) THEN

	      !IDS ADDED THE ABILITY TO RESET THE INITIAL SOIL MOISTURE TO A USER
	      !   DEFINED PERCENT OF TOTAL AVAILABLE SOIL MOISTURE.
	      ! PROFLAG = 1 MEANS USER SET THE SOIL MOISTURE AT THE BEGINNING
	      !   OF EACH SEASON TO BE A SPECIFIED NUMBER (PROFSSEA).
	      !
	      IF(PROFLAG(nyr).EQ.1 .OR. PROFLAG(nyr).EQ.3) THEN
		 ! Patterson: changed to use root growth
		 !storag(ip) = irz(ckey)*awcmad*profssea(nyr)
		 storag(ip) = pc_rz(IP, L)*awcmad*profssea(nyr)
	      ELSEIF( PROFLAG(nyr) .EQ. 2) THEN
		 !endstor(ip) = irz(ckey)*awcmad*profend(nyr)
		 endstor(ip) = pc_rz(IP, L)*awcmad*profend(nyr)
	      ENDIF
	      bcdperc(ip,nperiods+1) = 0.0
	   ENDIF
	   !----------------------------------------------------
	   !Calculate irrigation water reqt, rainfall carry-over
	   !----------------------------------------------------

	   er2_s(ip,L) = exces_er(ip,L)


	   ! IF DRY LAND WATER BUDGET ADD RAINFALL TO STORAGE
	   ! OTHERWISE THIS IS DONE IN THE DISTR SUBROUTINE.

	   IF(ISUPLY .EQ. 0) THEN
	      storag(ip) = storag(ip) + er2_s(ip,L)
	   ENDIF
	   !----------------------------------------------
	   !Check if max coverage has been already reached
	   !----------------------------------------------

	   if (nperiods .EQ. 12) then
	      !Monthly
	      ndays = month(L)
	      if (L .eq. 2) then
		 if (DaysInYear(NYR1+nyr-1) .eq. 366) ndays = 29
	      endif

	      daymaxc = maxcov(IP)-julian(L,ndays,nyr)
	      IF (daymaxc.GT.0) THEN
		 daymaxc = ndays
	      else

		 ! If the user enters the frost date as january then the
		 ! daymaxc need to be the days.

		 IF ((L .EQ. 1).AND.(maxcov(IP) .LT. ndays)) THEN
		    daymaxc = maxcov(IP)
		 ELSEIF (-daymaxc.LE.ndays) THEN
		    daymaxc = maxcov(IP) - julian(L-1,month(L-1),nyr)
		 ELSE
		    daymaxc = 0
		 ENDIF
	      ENDIF

	      IF (L.EQ.fbegmo(IB,nyr)) THEN
		 pc_rz(IP,L) = pc_rz(IP,L) + incrz(IP)* MIN(daymaxc,npart(IP))
		 storag(IP)=storag(IP)+incst(IP)* MIN(daymaxc,npart(IP))
	      ELSEIF (L.EQ.fendmo(IB,nyr)) THEN
		 pc_rz(IP,L) = pc_rz(IP,L) + incrz(IP)* MIN(daymaxc,nendda(IP))
		 storag(IP) = storag(IP) + incst(IP)* MIN(daymaxc,nendda(IP))
	      ELSE
		 pc_rz(IP,L) = pc_rz(IP,L) + incrz(IP)* MIN(daymaxc,ndays)
		 storag(IP) = storag(IP)+incst(IP)*MIN(daymaxc,ndays)
	      ENDIF
	   else
	      daymaxc = maxcov(IP)

	      ! Daily--just add the growth increment if not to cover.
	      if (L .LE. daymaxc) then
		 pc_rz(IP,L) = pc_rz(IP,L) + incrz(IP)
		 storag(IP) = storag(IP) + incst(IP)
	      endif
	   ENDIF

	   IF (SPFLAG .EQ. 1) THEN
	      scap(IP) = pc_rz(IP,L) * awcmad
	   ELSE
	      scap(IP) = 12.0 * pc_rz(IP,L) * awcmad
	   ENDIF
	   ! ----------------------------------------------
	   ! Compute deep percolation and update re2 stored
	   ! ----------------------------------------------
	   IF (storag(IP).GE.scap(IP)) THEN
	      bcdperc(IP,L) = bcdperc(IP,L) + (storag(IP) - scap(IP))
	      er2_s(IP,L) = er2_s(IP,L) - (storag(IP) - scap(IP))
	      ! ---------------------------------------------------------
	      ! RESET rainfall contribution to soil storage IF it is negative
	      ! LAG 6/16/96
	      ! ---------------------------------------------------------
	      IF ( er2_s(IP,L) .LT. 0.0) er2_s(IP,L) = 0.0

	      IF ( er2_s(ip,l) .LT. 0.0 ) THEN
		 WRITE(*,*) 'er2_s is less than zero'
		 WRITE(*,*) er2_s(ip,l), storag(ip), scap(ip),ip
	      ENDIF
	      storag(IP) = scap(IP)
	   ENDIF
	   bcdperc(IP,nperiods+1) = bcdperc(IP,nperiods+1) + bcdperc(IP,L)

	ELSE

	   ! PROFLAG = 1 MEANS USER SET THE SOIL MOISTURE AT THE BEGINNING
	   !  OF EACH SEASON TO BE A SPECIFIED NUMBER (PROFSSEA).
	   !  THEREFORE DO NOT SET ANY PRECIPITATION VALUE TO BE ADDED TO THE
	   !  WATER BUDGET

	   IF( PROFLAG(nyr).EQ.1 .OR.PROFLAG(nyr).EQ.3) THEN
	      ! Patterson: changed to use root growth
	      !istor = irz(ckey)*awcmad*profssea(nyr)
	      istor = pc_rz(IP, L)*awcmad*profssea(nyr)
	      storag(IP) = istor
	      er2_s(IP,L) = 0.0
	   ELSE
	      er2_s(IP,L)=er(IP,L)*SMEF(nyr)
	      !LUIS                     er2_s(IP,L) = exces_er(IP,L)
	   ENDIF
	ENDIF  ! end of if during the growing season

	! Accumulate root zone
	if (T_AREA(IB,NYR) .gt. 0) then
	   maxstore(l) = maxstore(l) + pc_rz(IP, L)/12.0 * awcmad * AREA(IB,IP,NYR)
	endif
     END DO

     !-----------------------------------------------------------
     !Perform and Write Water Balance for Blaney-Criddle Enhanced
     !-----------------------------------------------------------

     IF (ISUPLY.EQ.1) THEN

	CALL WELLSUP(IB,NYR,L,wsup, cappeff(L), NPERIODS)

	if (nperiods .EQ. 12) then
	   !Monthly

	   CALL DISTR(IB,nyr,L,nperiods,exces_er,cuirr, &
		fsuply(L)+BWELACU(IB,NYR,L),reqt(L),pc_rz, &
		nendmo, &
				!Outputs
		er_st(L), er2_s, storag, runoff(L),shorts(L), &
		cussup(L),carry(L),Re_Ro, SMBSeason)
	else ! Daily
	   CALL DISTR(IB,nyr,L,nperiods,exces_er,cuirr, &
		fsuply(L)+BWELACU(IB,NYR,L),reqt(L),pc_rz, &
		jend(:,nyr), &
		!Outputs
		er_st(L), er2_s, storag, runoff(L),shorts(L), &
		cussup(L),carry(L),Re_Ro, SMBSeason)
	endif

	!-------------------------------------------------------------
	!Check if the well mode is 0 - Percent of GW Supplied by Well
	!-------------------------------------------------------------
	IF(WMODE(IB) .EQ. 0 ) THEN
	   WellPWS(IB,NYR,L) = shorts(L)
	   CALL WELLPUMP(IB,NYR,L,SHORTS(L),wpump(L),1)
	   WellPWS(IB,NYR,L) = wpump(L)
	ENDIF
     ENDIF


     !-------------------------------------------------------------------
     !Code added to figure the amount of pumping that takes place
     !when using the enhanced Blaney Criddle - Modified for South Platte.
     !-------------------------------------------------------------------

     IF (ISUPLY.EQ.0) THEN
	CALL sched(IB,nyr,L,er2_s,cuirr, &
	     storag,er_st(L),cussup(L),carry(L),shorts(L))
     ENDIF

     totsup = fsuply(L)+BWELACU(IB,NYR,L)+Re_Ro-Runoff(L)

     CALL WSDIST(L,IB,nyr,totsup,Re_Ro, cussup, fsuply, BWELACU(IB,NYR,:), &
	  sur_sup, wel_sup)

     runoff(L) = runoff(L) +  farmlos(L)

     !---------------------------------------------------------------------
     ! Write soil moisture for the current timeperiod.
     !---------------------------------------------------------------------
     ! convert from inches to ac-ft
     IF (ISUPLY .EQ. 1) THEN
	WRITE(23,*) "soil_moisture", NYR1+nyr-1, L, (storag(IP)*AREA(IB,IP,NYR)/12.0, IP=1,nparce(IB) - n_wheat_parcel)
	WRITE(23,*) "cir", NYR1+nyr-1, L, (cuirr(IP, L)*AREA(IB,IP,NYR)/12.0, IP=1,nparce(IB) - n_wheat_parcel)
	WRITE(23,*) "eff_rain", NYR1+nyr-1, L, (er(IP, L)*AREA(IB,IP,NYR)/12.0, IP=1,nparce(IB) - n_wheat_parcel)
        WRITE(23,*) "rain_to_soil", NYR1+nyr-1, L, (er2_s(IP, L)*AREA(IB,IP,NYR)/12.0, IP=1,nparce(IB) - n_wheat_parcel)
     ELSE
     END IF
  END DO

  RETURN
END SUBROUTINE BCSMB
