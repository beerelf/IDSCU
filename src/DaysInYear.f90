INTEGER FUNCTION DaysInYear(yr)

  !***************************************************************************
  !
  !   Function        : DaysInYear.f
  !   Author          : Dave Patterson
  !   Date            : dec 2002
  !   Purpose         : This returns 365 unless the year is a leap year,
  !                     in which case 366 is returned.
  !   Calling program : 
  !   Called programs : none
  !   Input arguments : none   
  !   Output arguments: none 
  !   Assumptions     :
  !   Limitations     :
  !   Notes           :  
  !
  !   History         : (Date, Author, Description)
  !
  !
  !***************************************************************************

  USE Globals

  INTEGER yr
  INTEGER yr_p

  !If year is an offset, then get the actual year.
  if (yr .lt. 1500) then
     yr_p = NYR1 + yr -1
  else
     yr_p = yr
  endif

  !From http://world.std.com/~dpbsmith/leapyearfaq.txt
  IF ((MOD(yr_p,400)).EQ. 0) THEN
     DaysInYear  = 366
  ELSE IF ((MOD(yr_p,100)).EQ. 0) THEN
     DaysInYear  = 365
  ELSE IF ((MOD(yr_p,4)).EQ. 0) THEN
     DaysInYear  = 366
  ELSE
     DaysInYear = 365
  END IF

END FUNCTION DaysInYear
