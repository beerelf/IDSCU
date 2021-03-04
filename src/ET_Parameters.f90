!***************************************************************************
!C
!C   Include File    : etcommon.inc
!C   Author          : HB Manguerra
!C   Date            : December 1994
!C   Purpose         : This includes declaration of all variables and 
!C                     parameters used in the Penman-Monteith calculation
!C                     of reference evapotranspiration (soil moisture budget
!C                     not included.
!C   Calling program : etref.f, etrref.f, etoref.f 
!C   Assumptions     :
!C   Limitations     :
!C   Notes           :
!C
!C***************************************************************************

      MODULE ET_Parameters

      REAL HCALF, LHCALF, LAIALF,ZOMALF,ZOHALF,DALF,RCALF
      REAL HCGRS, LAIGRS, ZOMGRS, ZOHGRS, DGRS, RCGRS, B1

      PARAMETER(HCALF=0.5)  ! Units in meters
      PARAMETER(LHCALF=-0.69314718)                      ! ln(hcalf)
      !PARAMETER(LAIALF = 5.5 + 1.5*LHCALF)
      PARAMETER(ZOMALF = .123*HCALF)
      PARAMETER(ZOHALF = .0123*HCALF)
      PARAMETER(DALF = .67*HCALF)
      !PARAMETER(RCALF = 200/LAIALF)

      PARAMETER(HCGRS = .12)
      PARAMETER(LAIGRS = 24.0*HCGRS)
      PARAMETER(ZOMGRS = .123*HCGRS)
      PARAMETER(ZOHGRS = .0123*HCGRS)
      PARAMETER(DGRS = 0.67*HCGRS)
      PARAMETER(RCGRS = 200.0/LAIGRS)
      PARAMETER(B1 = -0.139)

      END MODULE
