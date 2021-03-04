REAL FUNCTION RAIN (ib, nyr, DOY, RNFAC, CN, QIRR)

  !***************************************************************************
  !
  !   Function        : rain.f 
  !   Author          : HB Manguerra
  !   Date            : December 1994
  !   Purpose         : This calculates the daily effective rainfall used with
  !                     the Penman-Monteith ET method.  
  !   Calling program : proto.f
  !   Called programs : myexit.f 
  !   Input arguments : doy      = day of the year
  !                     rn_ref   = method for calculating effective rainfall
  !                     rnfac    = factor to be multiplied to total rainfall
  !                                to get effective when irn = 2
  !                     cn(3)    = curve number parameters when irn = 3
  !                     qir(366) = daily irrigation amount when irn = 3. Only
  !                                irrigation of the past 5 days is used.
  !                     pcp(366) = daily total rainfall.  Only the total
  !                                rainfall of the current day pcp(doy) is
  !                                used.
  !   Output arguments: epcp     = effective daily rainfall of the current
  !                                day. 
  !   Assumptions     :
  !   Limitations     :
  !   Notes           : Daily effective rainfall is calculated using three
  !                     methods namely:
  !                     1 = all daily total rainfall below 1.0 in is effective
  !                         with maximum effective = 1.0 in.
  !                     2 = effective rainfall = factor x total rainfall
  !                     3 = curve number method from NEH SEC 4 METHOD
  !                     A choice of rn_ref = 0 causes the program to ignore 
  !                     rainfall in the soil moisture budget.
  !                     Routines for the Curve Number Method were taken from 
  !                     the rain.f subroutine of SMB program developed by
  !                     Wheeler and Associates.
  !
  !***************************************************************************

  USE Globals

  !Input Parameters
  INTEGER ib, nyr, DOY
  REAL RNFAC, CN(3), QIRR(:)

  !Local Variable Declaration
  INTEGER IAMC
  REAL EFFPCP, AM, S, X, RO

  !Do not consider rainfall
  IF (RN_REF.EQ.0) THEN
     EFFPCP = 0.0
     !Maximum Effective Precipitation = 1.0 inch
  ELSEIF (RN_REF.EQ.1) THEN
     EFFPCP = AMIN1(RNTOT3(ib, nyr, DOY), CN(1))
     !Effective Precipitation is specified from a factor
  ELSEIF (RN_REF.EQ.2) THEN
     EFFPCP = RNTOT3(ib, nyr, DOY) * RNFAC
     !Effective Precipitation is estimated using NEH Curve number method 
  ELSEIF (RN_REF.EQ.3) THEN
     IF (DOY.EQ.1) THEN
	AM = 0
     ELSEIF (DOY.EQ.2) THEN
	AM = RNTOT3(ib, nyr, 1)
     ELSEIF (DOY.EQ.3) THEN
	AM = RNTOT3(ib, nyr, 2)+ RNTOT3(ib, nyr, 1)
     ELSEIF (DOY.EQ.4) THEN
	AM = RNTOT3(ib, nyr, 3)+ RNTOT3(ib, nyr, 2) + RNTOT3(ib, nyr, 1)
     ELSEIF (DOY.EQ.5) THEN
	AM = RNTOT3(ib, nyr, 4)+ RNTOT3(ib, nyr, 3) + RNTOT3(ib, nyr, 2)+RNTOT3(ib, nyr, 1)
     ELSE
	AM = RNTOT3(ib, nyr, DOY-1) + RNTOT3(ib, nyr, DOY-2) &
	     + RNTOT3(ib, nyr, DOY-3) + RNTOT3(ib, nyr, DOY-4) &
	     + RNTOT3(ib, nyr, DOY-5)
     ENDIF
     IF (AM .LT. 1.4)  IAMC = 1
     IF (AM .GE. 1.4 .AND. AM .LT. 2.1)  IAMC = 2
     IF (AM .GE. 2.1)  IAMC = 3


     IF (DOY.GE.5) THEN
	IF (QIRR(DOY-1) .GT. 0 .OR. QIRR(DOY-2) .GT. 0 .OR. &
	     QIRR(DOY-3) .GT. 0 .OR. QIRR(DOY-4) .GT. 0 .OR. &
	     QIRR(DOY) .GT. 0)  IAMC = 3
     ELSEIF (DOY.EQ.4) THEN
	IF (QIRR(DOY-1) .GT. 0 .OR. QIRR(DOY-2) .GT. 0 .OR. &
	     QIRR(DOY-3) .GT. 0 .OR. QIRR(DOY) .GT. 0)  IAMC = 3
     ELSEIF (DOY.EQ.3) THEN
	IF (QIRR(DOY-1) .GT. 0 .OR. QIRR(DOY-2) .GT. 0 .OR. &
	     QIRR(DOY) .GT. 0)  IAMC = 3
     ELSEIF (DOY.EQ.2) THEN
	IF (QIRR(DOY-1) .GT. 0 .OR. QIRR(DOY) .GT. 0)  IAMC = 3
     ELSE
	IF (QIRR(DOY) .GT. 0) IAMC = 3
     ENDIF


     S = 1000/CN(IAMC) - 10
     X = RNTOT3(ib, nyr, DOY) - 0.2 * S
     IF (X .LE. 0)  X = 0
     RO = X**2 / (RNTOT3(ib, nyr, DOY) + 0.8 * S)
     EFFPCP = RNTOT3(ib, nyr, DOY) - RO
     !Error - no method is chosen - abort program
  ELSE
     CALL MYEXIT(99)
  ENDIF

  RAIN = EFFPCP

  RETURN
END FUNCTION RAIN
