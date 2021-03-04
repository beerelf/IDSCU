REAL FUNCTION RAIN (ib, nyr, DOY, RNFAC, CN, suploss)

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
  REAL RNFAC, CN(3), suploss(:,:)

  !Local Variable Declaration
  INTEGER IAMC, I
  REAL EFFPCP, AM, S, X, RO
  LOGICAL has_irrigation

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
     ! Accumulate precip from last 5 days
     AM = 0
     DO i = DOY-5, DOY-1
        if (i .GT. 0) THEN
           AM = AM + RNTOT3(ib, nyr, i)
        END IF
     END DO

     IF (AM .LT. 1.4)  IAMC = 1
     IF (AM .GE. 1.4 .AND. AM .LT. 2.1)  IAMC = 2
     IF (AM .GE. 2.1)  IAMC = 3

     ! Look for any irrigation over the previous five days
     has_irrigation = .false.
     DO i = DOY-5, DOY-1
        IF (i .GT. 0) THEN
           IF (suploss(nyr, i) .GT. 0) THEN
              IAMC = 3
              has_irrigation = .true.
           END IF
        END IF
     END DO

     ! Patterson: hardcode CN coefficeints for Nathan
     CN(1) = 66
     CN(2) = 82
     CN(3) = 95
     
     S = 1000/CN(IAMC) - 10
     RO = 0
     IF (RNTOT3(ib, nyr, DOY) .GT. 0.2 * S) THEN
        X = RNTOT3(ib, nyr, DOY) - 0.2 * S
        RO = X**2 / (RNTOT3(ib, nyr, DOY) + 0.8 * S)
     END IF
     EFFPCP = RNTOT3(ib, nyr, DOY) - RO

     if (DOY .eq. 285) then
        print *, "on day ", DOY, "AM =", AM, "S = ", S,  "X = ", X, "RO =", RO, "IAMC =", IAMC, "has_irrigation=", has_irrigation
     end if
  ELSE
     !Error - no method is chosen - abort program
     CALL MYEXIT(99)
  ENDIF

  RAIN = EFFPCP

  RETURN
END FUNCTION RAIN
