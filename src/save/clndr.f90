SUBROUTINE CLNDR (jday, year, m_mon, m_day)

  !***************************************************************************
  !
  !   Function        : clndr.f
  !   Author          : HB Manguerra
  !   Date            : December 1994
  !   Purpose         : This calculates the equivalent of julian day (or day
  !                     of the year) to calendar month(1-12) and day(1-31).
  !   Calling programs: annuacrp.f, perencrp.f, avgmon.f, calpcrop.f, etoref.f,
  !                     etrref.f, growth.f, proto.f, wbuild.f
  !   Called programs : none 
  !   Input arguments : jday  = julian day or day of the year (1-366) 
  !   Output arguments: m_mon = calendar month (1-12) 
  !                     m_day = calendar day of the month (1-31)
  !   Assumptions     :
  !   Limitations     :
  !   Notes           : Leap years are handled. 
  !
  !***************************************************************************

  USE Globals

  !Parameters
  INTEGER jday,year,m_mon,m_day

  !Outputs

  INTERFACE
     INTEGER FUNCTION DaysInYear(yr)
       INTEGER yr
     END FUNCTION DaysInYear
  END INTERFACE

  !Local Variable Declaration
  INTEGER sum, ndays
  LOGICAL is_leap

  sum = 0
  m_mon  = 0
  is_leap = .false.
  if (DaysInYear(year) .eq. 366) is_leap = .true.

10 m_mon = m_mon + 1
  ndays = MONTH(m_mon)
  if (is_leap .and. m_mon .eq. 2) ndays = 29
  sum = sum + ndays
  IF (sum.LT.jday) GOTO 10
  m_day = jday - sum + ndays

  RETURN
END SUBROUTINE CLNDR
