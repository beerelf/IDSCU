SUBROUTINE KPPEREN(ib,ckey,nyr,begs,ends,begmo,begda, endmo, endda, &
     !Outputs
     t_npart, xkc, days, temps, dayf, tempf)

  !***************************************************************************
  !
  !   Function        : kpcrop.f
  !   Author          : LA Garcia
  !   Date            : October 1998
  !   Purpose         : This reads the Kimberly-Penman crop coefficient file
  !                     and decides which type of crop coefficient (mean Alfalfa). 
  !   Calling program : readin.f 
  !   Called programs : none
  !   Input arguments : nend  = end of growing season in julian day
  !   Input arguments : nbeg  = start of growing season in julian day
  !                     nend  = end of growing season in julian day
  !                     nbegmo = begin month of growing season
  !                     nbegda = begin day of growing season
  !                     nendmo = end month of growing season
  !                     nendda = end day of growing season
  !                     ckey    = crop index
  !   Output arguments: npart = number of days in beginning month  in spring
  !   Assumptions     :
  !   Limitations     :
  !   Notes           :  
  !
  !   History         : (Date, Author, Description)
  !
  !
  !***************************************************************************

  USE Globals

  !Parameters
  INTEGER ib, begs, begmo, begda, endmo, endda, ends, ckey, nyr

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

     SUBROUTINE INTERTD(ib,K,N,t_npart,selmon,midpt, tempc, dayC)
       INTEGER ib,t_npart, K, N, selmon, midpt
       !Outputs
       REAL dayC, tempc
     END SUBROUTINE INTERTD

     SUBROUTINE CLNDR (jday, year, &
				!Outputs
          m_mon, m_day)
       INTEGER jday, year, m_mon, m_day
     END SUBROUTINE CLNDR
  END INTERFACE

  !Local Variable Declaration
  INTEGER  OMON, NDAYS

  REAL SXKCB
  INTEGER K, ndays_begmo
  INTEGER midpts, midptf, DOY, IMON, IDAY
  REAL XKCB(366)
  LOGICAL is_leap

  ndays_begmo = month(begmo)
  is_leap = .false.
  if (DaysInYear(NYR1+nyr-1) .eq. 366) is_leap = .true.

  if (begmo .eq. 2 .and. is_leap) ndays_begmo = ndays_begmo + 1

  !Calculate midpoint of beginning month in spring
  midpts=((ndays_begmo-begda + 1 )/2) + begda

  !Calculate midpoint of ending month in spring
  midptf= (endda + 1)/2

  !Calculate number of days in beginning month of spring
  t_npart=(ndays_begmo-begda + 1 )

  !interpolate temperature and dayhours for beginning month in spring
  DO K = 1, 8
     IF (tmean3(ib,nyr,k) .gt. -999) THEN
	IF(JULIAN(begmo, midpts,nyr) .LT. middle(K)) THEN
	   CALL INTERTD(ib,K,nyr,t_npart,begmo, midpts, temps,days)
	   GO TO 20
	ENDIF
	IF(JULIAN(begmo, midpts,nyr) .EQ. middle(k)) THEN
	   temps = tmean3(ib,nyr,k)
	   !LAG            days = pclite(k)
	   GO TO 20
	ENDIF
     ENDIF
  END DO
  !CALL MYEXIT(117)

  !interpolate temperature and dayhours for ending month in spring
20 DO k = 1,12
     IF (tmean3(ib,nyr,k) .gt. -999) THEN
	IF( JULIAN(endmo, midptf,nyr).LT. middle(K)) THEN
	   CALL INTERTD(ib,K,nyr,endda, endmo, midptf, tempf,dayf)
	   GOTO 40
	ENDIF
	IF( JULIAN(endmo, midptf,nyr) .EQ. middle(K)) THEN
	   tempf = tmean3(ib,nyr, K)
	   !LAG            dayf  = pclite(K)
	   GOTO 40
	ENDIF
     ENDIF
  END DO
  !CALL MYEXIT(118)
  !! - this isn't fatal.  Assume partial year of weather data.
  tempf = 0
  dayf = 0

  !Calculate Crop Coefficients 
40  CALL KPGROWTH(CKEY, begs, ends, XKCB)

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
	SXKCB = SXKCB + XKCB(DOY)
	NDAYS = NDAYS + 1
     ELSE
	XKC(OMON) = SXKCB/NDAYS
	OMON = IMON
	NDAYS = 0
	SXKCB = 0
     ENDIF

     !End of daily Calculations
  END DO
  !LUIS        XKC(OMON) = SXKCB/NDAYS
  !LUIS         NDAYS = 0
  !LUIS         SXKCB = 0

  RETURN
END SUBROUTINE KPPEREN
