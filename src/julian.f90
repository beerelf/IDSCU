INTEGER FUNCTION JULIAN (m_mon, m_day, yr)

  !***************************************************************************
  !
  !   Function        : julian.f
  !   Author          : HB Manguerra
  !   Date            : December 1994
  !   Purpose         : This calculates the equivalent of calendar date to
  !                     julian day (day of the year).
  !   Calling programs: annuacrp.f, perencrp.f, fall.f, spring.f, frost.f,
  !                     intertd.f
  !   Called programs : myexit.f 
  !   Input arguments : m_mon = calendar month (1-12)
  !                     m_day = calendar day of the month (1-31)
  !   Output arguments: julian= julian day or day of the year (1-366)
  !   Assumptions     :
  !   Limitations     :
  !   Notes           : Leap years are handled.
  !
  !***************************************************************************

  USE globals

  ! Parameters
  INTEGER m_mon, m_day, yr

  INTERFACE
     FUNCTION DaysInYear(yr)
       INTEGER DaysInYear, yr
     END FUNCTION DaysInYear
  END INTERFACE

  !Local variable declaration
  INTEGER sum, ii 

  !error check for day
  IF( m_day .EQ. -999) THEN
     JULIAN = -999
     RETURN
  ELSEIF (m_day .LT. 0 .OR. m_day .GT. 31 ) THEN
     WRITE (*,1) m_day 
     m_day = 15
     call MYEXIT(71)
  ENDIF

  !error check for month
  IF( m_mon .EQ. -999) THEN
     JULIAN = -999
     RETURN
  ELSEIF (m_mon .LT. 1 .OR. m_mon .GT. 12) THEN
 !IDS         WRITE (*,2) m_mon 
 !IDS         m_mon = 4
 !IDS         call MYEXIT(72)
     JULIAN = 0
     RETURN
  ENDIF

  sum = 0
  DO ii = 1,m_mon-1
     sum = sum + MONTH(ii)
     !Check for leap year.
     IF ((DaysInYear(yr) .gt. 365) .and. (ii .eq. 2)) then
	sum = sum + 1
     endif
  END DO
  JULIAN = sum + m_day 

1 FORMAT (' ', 6X, 'THE DAY ', I8, ' IS NOT A VALID DATE')
2 FORMAT (' ', 6X, 'THE MONTH ' I8, ' IS NOT A VALID MONTH')

  RETURN
END FUNCTION JULIAN

