SUBROUTINE AVGMON(ndyr,mn,mx,rf,dewpt,solar,wind,tavg,rdata, &
     dpdata,srdata,wddata)

  !***************************************************************************
  !
  !   Function        : avgmon.f
  !   Author          : HB Manguerra
  !   Date            : December 1994
  !   Purpose         : This calculates the monthly mean temperature, dew
  !                     point temperature, solar radiation, wind speed and
  !                     total rainfall from daily weather data of a given 
  !                     weather station over one year.
  !   Calling program : proto.f summary.f
  !   Called programs : none
  !   Input arguments : ndyr   = number days in the year   
  !                     mn(366) = daily minimum temperature for the year
  !                     mx(366) = daily maximum temperature for the year
  !                     rf(366) = daily total rainfall for the year
  !   Output arguments: tavg(12)= monthly mean temperature for the year
  !                     rdata(12)=monthly total rainfall for the year
  !   Assumptions     : The mean daily temperature is estimated by an 
  !                     arithmetic average of the minimum and maximum
  !                     temperature.
  !   Limitations     :
  !   Notes           : The mean monthly temperature is required for calcu-
  !                     lating frost dates.  The monthly weather summaries
  !                     are used when creating the input summary file.
  !
  !***************************************************************************

  USE globals

  !Parameters
  INTEGER ndyr
  REAL mn(366),mx(366),rf(366)
  REAL dewpt(366),solar(366),wind(366)
  REAL tavg(12), rdata(12)
  REAL dpdata(12),srdata(12),wddata(12)

  INTERFACE
     SUBROUTINE CLNDR (jday, year, &
	  !Outputs
	  m_mon, m_day)
       INTEGER jday, year, m_mon, m_day
     END SUBROUTINE CLNDR
  END INTERFACE

  !Locals
  INTEGER ii,im,id,ilast
  REAL sum1,sum2,sum3,sum4,sum5,sum6

  INTEGER days_in_month

  ilast = 1
  sum1 = 0.0
  sum2 = 0.0
  sum3 = 0.0
  sum4 = 0.0
  sum5 = 0.0
  sum6 = 0.0

  do ii = 1, ndyr
     call clndr(ii,m_year,im,id)

     if (im.ne.ilast) then  ! next month
	days_in_month = month(ilast)
	if (ndyr.eq.366 .and. ilast .eq. 2) days_in_month = 29

	tavg(ilast) = 0.5*(sum1+sum2)/days_in_month
	rdata(ilast) = sum3
	dpdata(ilast) = sum4/days_in_month
	srdata(ilast) = sum5/days_in_month
	wddata(ilast) = sum6/days_in_month
	ilast = im
	sum1 = 0.0
	sum2 = 0.0
	sum3 = 0.0
	sum4 = 0.0
	sum5 = 0.0
	sum6 = 0.0
     endif
     ! Ignore nodata values.
     if (mx(ii) .ne. -999) sum1 = sum1 + mx(ii)
     if (mn(ii) .ne. -999) sum2 = sum2 + mn(ii)
     if (rf(ii) .ne. -999) sum3 = sum3 + rf(ii)
     if (dewpt(ii) .ne. -999) sum4 = sum4 + dewpt(ii)
     if (solar(ii) .ne. -999) sum5 = sum5 + solar(ii)
     if (wind(ii) .ne. -999) sum6 = sum6 + wind(ii)

  END DO

  ! The last month is missed, so do it here:
  tavg(12) = 0.5*(sum1+sum2)/31
  rdata(12) = sum3
  dpdata(12) = sum4/31
  srdata(12) = sum5/31
  wddata(12) = sum6/31

  return
end SUBROUTINE AVGMON
