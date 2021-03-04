SUBROUTINE FINPUT(CKEY, SKEY, &
     !Outputs
     SMCAP0, RNFAC, CN, DEFIT)

  !***************************************************************************
  !
  !   Function        : finput.f
  !   Author          : HB Manguerra
  !   Date            : December 1994
  !   Purpose         : This assigns the global common variables to variables
  !                     specific to Penman-Monteith equation.  It also assigns
  !                     the soil index that is based on the usda 12 textural 
  !                     classes to another index that is based just on 6 main
  !                     textural classes.
  !   Calling program : proto.f
  !   Called programs : none
  !   Input arguments : ic = current crop and soil combination 
  !   Output arguments: none
  !   Assumptions     :
  !   Limitations     : The drying properties are only available for 6 main
  !                     textural classes which required the reclassification
  !                     of the soil type. 
  !   Notes           : 
  !
  !***************************************************************************

  USE Globals

  !Parameters
  INTEGER CKEY, SKEY ! crop and soil key
  !Output
  REAL SMCAP0, RNFAC, CN(3), DEFIT(12)

  !Local Variable Declaration
  INTEGER I 
  REAL IXIN

  !Assign global variables to Reference Equation
  !local variables

  IF (SPFLAG .EQ. 1) THEN
     SMCAP0 = AWC(SKEY)*IRZ(CKEY)      ! initial smcap
  ELSE
     SMCAP0 = 12.0*AWC(SKEY)*IRZ(CKEY)
  ENDIF
  RNFAC = RPARA(1)                   ! rainfall factor
  CN(1) = RPARA(1)                   ! curve number 1
  CN(2) = RPARA(2)                   ! curve number 2
  CN(3) = RPARA(3)                   ! curve number 3
  IXIN = MAD(CKEY)                       ! assumed constant/crop
  DO I=1,12
     DEFIT(I) = .01*REAL(IXIN)
  END DO

1000 RETURN
END SUBROUTINE FINPUT
