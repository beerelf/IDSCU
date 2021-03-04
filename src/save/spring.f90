SUBROUTINE SPRING (ib, yr, tfrost, daily,&
     !Outputs
     Jdays)

  !***************************************************************************
  !
  !   Function        : spring.f
  !   Author          : HB Manguerra
  !   Date            : December 1994
  !   Purpose         : This calculates the last day of frost occurence in 
  !                     spring based on mean monthly temperature.   
  !   Calling program : frost.f
  !   Called programs : julian.f 
  !   Input arguments : yr     = current year
  !                     tfrost = frost temperature in spring below which crop
  !                              will not emerge. 
  !   Output arguments: jday   = last day of frost in spring in julian day
  !   Assumptions     : This assumes that the spring frost occurs no later
  !                     than July 15.  If this is not satisfied by the
  !                     given weather data, the program may fail.
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

  INTERFACE
     INTEGER FUNCTION JULIAN (m_mon, m_day, yr)
       INTEGER m_mon, m_day, yr
     END FUNCTION JULIAN

     INTEGER FUNCTION DaysInYear(yr)
       INTEGER yr
     END FUNCTION DaysInYear
  END INTERFACE

  !Local Variable Declaration
  INTEGER kdays, idiff, I
  INTEGER Smonth, m, d
  REAL NDAYS(11), n
  REAL tmean3_monthly(12)

  REAL avg_start, avg_end, avg

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
	      !jdays = 365
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
	   jdays = 365
	   return
	end if
     end do
  endif

  !Patterson: remove the .true. to use the moving average method
  !  for daily data.
  if (.true. .or. .not. daily) then
     Smonth = 7
     kdays = 15
     IF(tmean3_monthly(7) .gt. -999 .AND. tmean3_monthly(7).LT.tfrost) THEN
	! empty
     ELSE
	DO i=7,1,-1
	   IF(tmean3_monthly(i) .gt. -999 .AND. tmean3_monthly(i).LE. tfrost) GO TO 300
	END DO
	Smonth = 1
	kdays  = 1
	GOTO 400

300	IF ((MOD(YR+NYR1-1,4)).EQ. 0) THEN
	   NDAYS(1) =  30.0
	   NDAYS(2) = 30.0
	ENDIF
	if (tmean3_monthly(I+1) .gt. -999) then
	   idiff =30.5 * (tfrost-tmean3_monthly(I)) / &
		(tmean3_monthly(I+1)-tmean3_monthly(I)) &
		+ 0.5             ! 0.5 is added for rounding
	   IF (idiff .LE. 15) THEN
	      Smonth = I
	      kdays = idiff + 15
	   ELSE IF( idiff .GT. 31) THEN
	      call myexit(73)
	   ELSE
	      Smonth = I+1
	      kdays = idiff - 15
	   ENDIF
	else
	   ! Unable to compute planting date.
	   Smonth = 12
	   kdays = 31
	endif
     ENDIF
400  Jdays = JULIAN(Smonth,kdays,yr)
  else
     ! Use moving average.
     smonth = -1

     ! Keep track of the ending and starting days of the moving average.
     avg_start = 1
     avg_end = 1
     avg = 0
     ! Accumulate 30 days of weather.
     DO d = 1, 30
	avg = avg + tmean3(ib,yr,avg_end)
	avg_end = avg_end + 1
     end do

     ! Avg is now loaded with the first 30 days of Jan
     DO m=1,7
	kdays = month(m)   ! number of days in month
	if (m .eq. 2) then
	   if (DaysInYear(NYR1+yr-1) .eq. 366) kdays = 29
	endif

	do d = 1,kdays
	   if (avg / 30.0 .ge. tfrost) then
	      ! Found the planting date.
	      goto 500
	   end if

	   ! Fix the avg by removing the start and adding the end
	   avg = avg + tmean3(ib,yr,avg_end) - tmean3(ib,yr,avg_start)
	   avg_start = avg_start + 1
	   avg_end = avg_end + 1
	end do
     end do
500  jdays = avg_end - 1 ! Remove the day that is added after the avg calc
  end if

  RETURN
END SUBROUTINE SPRING

