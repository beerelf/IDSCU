REAL FUNCTION AVG_DAILY(jul, nperiods, val)

  !***************************************************************************
  !
  !   Function        : AVG_DAILY.f
  !   Author          : Dave Patterson
  !   Date            : April 2003
  !   Purpose         : Calculate the average daily value for the given julian
  !                     day and value.
  !   Calling program : -
  !   Called programs : none
  !   Input arguments : none
  !   Output arguments: none
  !   Assumptions     :
  !   Limitations     : 
  !   Notes           :
  !***************************************************************************

  USE Globals

  !Input args
  INTEGER jul, nperiods
  REAL val

  INTERFACE
     SUBROUTINE CLNDR (jday, year, &
	  !Outputs
          m_mon, m_day)
       INTEGER jday, year, m_mon, m_day
     END SUBROUTINE CLNDR
  END INTERFACE

  !Locals
  INTEGER M, D

  !Convert the Julian day to a month and day
  CALL CLNDR(jul, m_year, M, D) ! D is ignored

  !Check for leap year.
  D = MONTH(M)
  if (M .eq. 2 .and. nperiods .eq. 366) D = D + 1

  AVG_DAILY = val / D

  RETURN

END FUNCTION AVG_DAILY
