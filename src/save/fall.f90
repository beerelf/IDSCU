SUBROUTINE FALL (ib, yr, tfrost, daily, &
				!Outputs
     Jdays)

  !***************************************************************************
  !
  !   Function        : fall.f
  !   Author          : HB Manguerra
  !   Date            : December 1994
  !   Purpose         : This calculates the first day of frost in
  !                     fall based on monthly mean temperature.
  !   Calling program : frost.f 
  !   Called programs : julian.f 
  !   Input arguments : yr     = current year 
  !                     tfrost = frost temperature in fall below which crop
  !                              will die.
  !   Output arguments: jdays  = first day of frost in fall in julian day 
  !   Assumptions     : The calculation assumes that the fall frost
  !                     temperature occur only after July 15.  If the weather
  !                     data does not satisfy this assumption, the program
  !                     may fail. 
  !   Limitations     :
  !   Notes           : The routines are based on USBR XCONS2 program which
  !                     uses the SCS Modified Blaney-Criddle ET Estimation
  !                     Method.
  !
  !***************************************************************************


  USE Globals

  !Parameters
  INTEGER ib, yr
  REAL tfrost
  LOGICAL daily
  !Outputs
  INTEGER Jdays
  REAL tmean3_monthly(12)

  INTERFACE
     INTEGER FUNCTION JULIAN (m_mon, m_day, yr)
       INTEGER m_mon, m_day, yr
     END FUNCTION JULIAN

     INTEGER FUNCTION DaysInYear(yr)
       INTEGER yr
     END FUNCTION DaysInYear
  END INTERFACE

  !Local Variable Declaration
  INTEGER I, idiff, kdays
  INTEGER fmonth, m, d
  REAL NDAYS(11)

  DATA NDAYS /29.5,29.5,30.5,30.5,30.5,30.5,31.0,30.5,30.5,30.5,30.5/

  if (daily) then
     jdays = 0
     do m = 1,12
	tmean3_monthly(m) = 0
	kdays = month(m)    ! number of days in month
	if (m .eq. 2) then
	   if (DaysInYear(NYR1+yr-1) .eq. 366) kdays = 29
	endif

	n = 0
	do d = 1,kdays
	   jdays = jdays + 1
	   if (tmean3(ib,yr,jdays) .gt. -999 .and. tmean3(ib,yr,jdays) .ne. 0) then
	      tmean3_monthly(m) = tmean3_monthly(m) + tmean3(ib,yr,jdays)
	      n = n + 1
	   else
	      ! Error in weather data, so abort.
	      !jdays = 1
	      !return
	   endif
	end do
	if (jdays .gt. 0) then
	   tmean3_monthly(m) = tmean3_monthly(m) / n
	else
	   tmean3_monthly(m) = -999
	endif
     end do
  else
     ! Just copy weather data
     do m = 1,12
	if (tmean3(ib,yr,m) .gt. -999 .and. tmean3(ib,yr,m) .ne. 0) then
	   tmean3_monthly(m) = tmean3(ib,yr,m)
	else
	   ! Error in weather data, so abort.
	   jdays = 1
	   return
	end if
     end do
  endif

  DO I=7,11
     IF (tmean3_monthly(I+1).GT. 0 .AND. tmean3_monthly(I+1).LT. tfrost) GO TO 300
  END DO

  fmonth = 12
  kdays = 15
  GO TO 500
300 if ((tmean3_monthly(I)-tmean3_monthly(I+1)) .lt. 1) then
     idiff = 0
  else
     idiff = NDAYS(I)*(tmean3_monthly(I)-tfrost)/ &
	  (tmean3_monthly(I)-tmean3_monthly(I+1)) &
	  + 0.5            ! 0.5 is added for rounding
  endif
  if (idiff .le. 0) then
     fmonth = i
     kdays = 1
     !$$$          print *, "Warning: frost date in year ", NYR1 + yr, " occurred in July."
  else IF (idiff .LE. 15) THEN
     fmonth = I
     kdays = idiff + 15
  ELSE IF( idiff .GT. 31) THEN
     call myexit(70)
  ELSE
     fmonth = I+1
     kdays = idiff - 15
  ENDIF
500 Jdays = JULIAN(fmonth,kdays,yr)

  RETURN
END SUBROUTINE FALL

