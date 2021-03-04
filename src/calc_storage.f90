SUBROUTINE CALC_STORAGE(IB, nperiods, nyr, L, nendmo, nendda, pc_rz, storag)

  USE GLOBALS

  INTEGER IB, L, nperiods, nyr
  INTEGER nendmo(:), nendda(:)
  REAL pc_rz(:,:) ! rootzone at each parcel for each time period.
  REAL storag(:) ! storage at each parcel

  INTERFACE
     INTEGER FUNCTION DaysInYear(yr)
       INTEGER yr
     END FUNCTION DaysInYear

     INTEGER FUNCTION JULIAN (m_mon, m_day, yr)
       INTEGER m_mon, m_day, yr
     END FUNCTION JULIAN
  END INTERFACE

  INTEGER IP, CKEY, SKEY, daymaxc, ndays
  INTEGER harvest_start, harvest_end
  REAL awcmad

  if (nperiods .EQ. 12)  then
     harvest_start = fbegmo(IB,nyr)
     harvest_end = fendmo(IB,nyr)
  else
     ! TODO: handle leap years.
     harvest_start = fbegda(IB, nyr)
     harvest_end = fendda(IB,nyr)
  endif

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
	      storag(ip) = pc_rz(IP, L)*awcmad*profssea(nyr)
	   ELSEIF( PROFLAG(nyr) .EQ. 2) THEN
              ! unused
	      !endstor(ip) = pc_rz(IP, L)*awcmad*profend(nyr)
	   ENDIF
	ENDIF

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
           ! Daily--just add the growth increment if not to cover.
	   daymaxc = maxcov(IP)

	   if (L .LE. daymaxc) then
	      pc_rz(IP,L) = pc_rz(IP,L) + incrz(IP)
	      storag(IP) = storag(IP) + incst(IP)
	   endif
	ENDIF
     END IF
  END DO

END SUBROUTINE CALC_STORAGE
