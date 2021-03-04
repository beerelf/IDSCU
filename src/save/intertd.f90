SUBROUTINE INTERTD(ib,K,nyr,t_npart,selmon,midpt, &
				!Outputs
     tempc, dayC)

  !***************************************************************************
  !
  !   Function        : intertd.f
  !   Author          : HB Manguerra
  !   Date            : December 1994
  !   Purpose         : This computes the monthly mean temperature and 
  !                     percent daylight hours for part months (beginning
  !                     month in the spring and ending month in the fall).
  !   Calling programs: perencrp.f, annuacrp.f 
  !   Called programs : none 
  !   Input arguments : K      = month index 
  !                     nyr      = current year
  !                     t_npart  = number of days within the growing season in
  !                              beginning month in spring and ending month
  !                              in fall.
  !                     selmon = beginning or ending month index
  !                     midpt  = midpoint day of spring part month or fall
  !                               part month.
  !   Output arguments: 
  !                     tempc  = interpolated mean temperature for the part
  !                              month
  !                     dayC   = interpolated daylight hours for the part 
  !                              month
  !   Assumptions     :
  !   Limitations     :
  !   Notes           : The routines are based on USBR XCONS2 program which
  !                     uses the SCS Modified Blaney-Criddle ET Estimation
  !                     Method.
  !
  !***************************************************************************

  USE Globals

  !Parameters
  INTEGER ib, t_npart, K, nyr, selmon, midpt
  !Outputs
  REAL dayC, tempc

  INTERFACE
     INTEGER FUNCTION JULIAN (m_mon, m_day, yr)
       !Parameters
       INTEGER m_mon, m_day, yr
     END FUNCTION JULIAN
  END INTERFACE

  !Local Variable Declaration
  REAL d1, d2

  d1 = JULIAN(selmon, midpt, nyr)
  d2 = t_npart

  !interpolating temperature data 


  tempc = tmean3(ib,nyr,k-1)+(( d1 - middle(k-1)) /(middle(k) -  &
       middle(k-1)))  *(tmean3(ib,nyr,k) - tmean3(ib,nyr,k-1))

  !interpolating daylight data 

  dayc = ( pclite(k-1)+((d1-middle(k-1)) /  &
       (middle(k) - middle(k-1))) *(pclite(k) -  &
       pclite(k-1))) *(d2/month(selmon))

  RETURN
END SUBROUTINE INTERTD
