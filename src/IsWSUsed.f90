!     If the given weather station is used by any farm, then return 
!     true.  This is useful for checking if we can safely ignore missing
!     weather data.
logical function IsWSUsed(istation)

  USE Globals

  integer istation
      
  INTEGER ib

  IsWSUsed = .false.

  do ib = 1, NBASIN
     if (WWS(ib, istation) .gt. 0) then
	IsWSUsed = .true.
	return
     endif
  end do

end function IsWSUsed
