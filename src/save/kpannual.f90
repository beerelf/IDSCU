SUBROUTINE KPANNUAL(ib,ckey,nyr,begs,ends,begmo,begda,endmo,endda, &
  !Outputs
     t_npart, xkc, days, temps, dayf, tempf)

  !***************************************************************************
  !
  !   Function        : kpannual.f
  !   Author          : HB Manguerra
  !   Date            : December 1994
  !   Purpose         : This calculates the monthly crop coefficients
  !                     mean temperature and percent daylight hours for
  !                     annual crops.  It includes the calculation required
  !                     for part months (i.e. the beginning and ending months)
  !                     of the growing season.
  !   Calling program : calpcrop.f 
  !   Called programs : myexit.f, intertd.f, interkc.f 
  !   Input arguments : nbeg   = start of growing season in julian day 
  !                     nend   = end of growing season in julian day
  !                     nbegmo = begin month of growing season
  !                     nbegda = begin day of growing season
  !                     nendmo = end month of growing season
  !                     nendda = end day of growing season
  !                     ckey   = crop index
  !   Output arguments: npart  = number of days in beginning month  in spring 
  !   Assumptions     :
  !   Limitations     : 
  !   Notes           : The main routines were taken from USBR XCONS2 program
  !                     which uses the SCS Modified Blaney-Criddle ET
  !                     Estimation Method.
  !
  !***************************************************************************

  USE Globals

  !Parameters
  INTEGER ib, nyr, ends, begmo, endmo, begs, ckey
  INTEGER begda, endda
  !Outputs
  INTEGER t_npart
  REAL xkc(12)
  REAL days, temps, dayf, tempf

  INTERFACE
     INTEGER FUNCTION DaysInYear(yr)
       INTEGER yr
     END FUNCTION DaysInYear

     INTEGER FUNCTION JULIAN (m_mon, m_day, yr)
       INTEGER m_mon, m_day, yr
     END FUNCTION JULIAN

     SUBROUTINE CLNDR (jday, year, &
	  !Outputs
          m_mon, m_day)
       INTEGER jday, year, m_mon, m_day
     END SUBROUTINE CLNDR

     SUBROUTINE INTERTD(ib,K,N,t_npart,selmon,midpt, tempc, dayC)
       INTEGER ib,t_npart, K, N, selmon, midpt
       !Outputs
       REAL dayC, tempc
     END SUBROUTINE INTERTD

  END INTERFACE

  !Locals
  INTEGER K, IMON, IDAY, ndays_begmo
  REAL fake3, SXKCB
  INTEGER NDAYS, DOY, OMON, midpts, midptf
  REAL XKPC(366)
  LOGICAL is_leap

  !annual crops
  !winter wheat fall beginning date   
  !must be before December 15        

  ndays_begmo = month(begmo)
  is_leap = .false.
  if (DaysInYear(NYR1+nyr-1) .eq. 366) is_leap = .true.

  if (begmo .eq. 2 .and. is_leap) ndays_begmo = ndays_begmo + 1

  !adjust end of growing season from length of growing season
  IF((ends-begs+1) .GT. GDATES(ckey)) ends=begs + GDATES(ckey) - 1

  !Calculate midpoint of beginning month in spring
  midpts=((ndays_begmo-begda+ 1)/2.0) + begda

  !Calculate midpoint of ending month in spring
  midptf= (endda + 1)/2.0

  !Calculate number of days in beginning month of spring
  t_npart= ndays_begmo-begda + 1

  !spring part month computations
  !naccum = accumulative days to midpoint of month
  !nperct = naccum/growing season length

  !  if beginning and ending month are the same,use spring numbers
  !  for computations

  IF (begmo.EQ.endmo) midpts = ((endda - begda+1)/2) + begda 
  naccum(begmo) = JULIAN(begmo,midpts,nyr) - begs
  fake3 = ends - begs
  nperct(begmo) =(naccum(begmo)/( fake3 ))* 100.0

  !interpolate temperature and dayhours for beginning month in spring
  IF (sub_crop_type(ckey) .NE. 0) THEN ! Not winter wheat
     DO K = 1, 8
	IF (tmean3(ib,nyr,k) .gt. -999) THEN
	   IF(JULIAN(begmo,midpts,nyr).LT.middle(K)) then
	      CALL INTERTD(ib,K,nyr,t_npart,begmo, midpts, temps,days)
	      goto 109
	   endif

	   IF(JULIAN(begmo,midpts,nyr).EQ.middle(K)) then
	      temps = tmean3(ib, nyr ,K)
	      goto 109
	   endif
	endif
     END DO
     !call MYEXIT(121)
  else
     DO K = 1,12
	IF (tmean3(ib,nyr,k) .gt. -999) THEN
	   IF (JULIAN(begmo, midpts,nyr) .LT. middle(K)) then
	      CALL INTERTD(ib,K,nyr,t_npart,begmo, midpts, temps,days)
	      goto 109
	   endif
	   
	   IF (JULIAN(begmo, midpts,nyr) .EQ. middle(K)) then 
	      temps = tmean3(ib, nyr ,K) 
	      goto 109
	   endif
	ENDIF
     END DO
     !call MYEXIT(123)
  endif

  !interpolate temperature and dayhours for ending month
109 IF (begmo.NE.endmo) THEN
     DO K = 1,12
	IF (tmean3(ib,nyr,k) .gt. -999) THEN
	   IF( JULIAN(endmo, midptf,nyr).LT. middle(K)) THEN
	      CALL INTERTD(ib,K,nyr,endda, endmo, midptf, tempf,dayf)
	      GO TO 118
	   ENDIF
	   IF( JULIAN(endmo, midptf,nyr) .EQ. middle(K)) THEN 
	      tempf = tmean3(ib, nyr, K)
	      GO TO 118
	   ENDIF
	ENDIF
     END DO
     !call MYEXIT(122)
     tempf = 0
  ENDIF

  !Calculate Crop Coefficients 
118 CALL KPGROWTH(CKEY, begs, ends, XKPC)

  ! Begin Daily Loop

  SXKCB = 0.0
  NDAYS = 0
  DO DOY = BEGS,ENDS 

     !Calculate Gregorian Day and Month from Julian Day
     CALL CLNDR(DOY,m_year,IMON,IDAY)
     IF(DOY .EQ. BEGS) THEN
	OMON = IMON
     ENDIF

     IF(OMON .EQ. IMON) THEN
	SXKCB = SXKCB + XKPC(DOY)
	NDAYS = NDAYS + 1
     ELSE
	XKC(OMON) = SXKCB/NDAYS
	OMON = IMON
	NDAYS = 0
	SXKCB = 0
     ENDIF

     !End of daily Calculations
  END DO
  if (ndays .gt. 0) then
     XKC(OMON) = SXKCB/NDAYS
  endif
  NDAYS = 0
  SXKCB = 0

  RETURN
END SUBROUTINE KPANNUAL

