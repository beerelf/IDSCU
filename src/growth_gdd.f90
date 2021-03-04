SUBROUTINE GROWTH_GDD(IB, YR, CKEY, JSTR, JSTP, WRITE_REPORT, &
				!Output
     XKCB, GDD)

  !***************************************************************************
  !
  !   Function        : growth_gdd.f 
  !   Author          : LA Garcia and Dave Patterson
  !   Date            : October 2005
  !   Purpose         : This calculates the daily crop coefficients using
  !                   : growing degree days from file .gdd.
  !   Calling program : proto.f
  !   Called programs : 
  !   Input arguments : ckey - crop index
  !                   : jstr - start day of ET (planting)
  !                   : jstp - end day of ET (harvest)
  !   Output arguments: none
  !   Assumptions     : 
  !   Limitations     : 
  !   Notes           : 
  !   History         : (Date, Author, Description)
  !
  !***************************************************************************


  USE Globals

  !Parameters
  INTEGER IB, YR, CKEY, JSTR, JSTP
  LOGICAL WRITE_REPORT
  !Outputs
  REAL XKCB(366), GDD(366)

  !Local Variable Declaration
  INTEGER day
  REAL min_temp, max_temp, n_gdd

  ! Start with planting date and calculate the crop coefficient using the accumulated growing degree day.
  n_gdd = 0
  DO day = JSTR, JSTP
     ! Cap min and max temperatures
     min_temp = tmin3(IB, YR, day)
     ! Assume temperatures are in celcius
     min_temp = .5556 * (min_temp - 32)
     if (min_temp .lt. gdd_temp(ckey, 2)) then
	min_temp = gdd_temp(ckey, 2)
     end if
     max_temp = tmax3(IB, YR, day)
     ! Assume temperatures are in celcius
     max_temp = .5556 * (max_temp - 32)
     if (max_temp .gt. gdd_temp(ckey, 3)) then
	max_temp = gdd_temp(ckey, 3)
     end if
     ! Use averaging method, max - min / 2 - base
     gdd(day) = (max_temp + min_temp) / 2.0 - gdd_temp(ckey, 1)
     if (gdd(day) .le. 0) then
	gdd(day) = 0.0
     end if
     n_gdd = n_gdd + gdd(day)
     XKCB(day) = GDD_COEFFS(ckey, 1) + GDD_COEFFS(ckey, 2) * n_gdd &
	  + GDD_COEFFS(ckey, 3) * n_gdd**2 + GDD_COEFFS(ckey, 4) * n_gdd**3 &
	  + GDD_COEFFS(ckey, 5) * n_gdd**4 + GDD_COEFFS(ckey, 6) * n_gdd**5 &
	  + GDD_COEFFS(ckey, 7) * n_gdd**6

     IF (XKCB(day) .LT. 0) THEN
	XKCB(day) = 0
     END IF

     IF (WRITE_REPORT) THEN
	WRITE(22, *) DAY, MIN_TEMP, MAX_TEMP, N_GDD, XKCB(day)
     END IF
  END DO

  RETURN
END SUBROUTINE GROWTH_GDD
