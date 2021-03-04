SUBROUTINE IRGATE(SMCAP, SMSTGD, DEFIT_M, TRANS_D, use_smb, &
				!Outputs
     QIRR_D)

  !***************************************************************************
  !
  !   Function        : irgate.f
  !   Author          : HB Manguerra
  !   Date            : December 1994
  !   Purpose         : This calculates the irrigation water requirement once
  !                   : the available soil moisture falls down below the
  !                     management allowed depletion.
  !   Calling program : acount.f 
  !   Called programs : none 
  !   Input arguments : cid - crop index
  !   Output arguments: none
  !   Assumptions     :
  !   Limitations     :
  !   Notes           : This is used by Penman-Monteith method only. 
  !
  !   History         :(Date, Author, Description)
  !
  !   11/13/95   HBM  : Irrigation is set at the end of every month to make
  !                     it consistent to the water resources planning model
  !                     which is monthly and to the monthly B-C method. 
  !
  !   11/15/95   HBM  : Basal kc type is not supported anymore.
  !
  !***************************************************************************


  USE Globals

  !Parameters
  REAL SMCAP, SMSTGD, DEFIT_M, TRANS_D
  LOGICAL use_smb
  !Outputs
  REAL QIRR_D

  !Calculate IWR at the end of the month or growing season or  when soil
  !     moisture deficit reaches a certain minimum

  ! Patterson: removed the test for end of month and growing season.
  !$$$      IF ((DOY.EQ.JSTP).OR.(NDAY.EQ.1).OR.
  !$$$     :   (SMSTGD .LT. ((1-DEFIT(IMON))*SMCAP))) THEN
  !

  IF (use_smb) THEN
     IF (SMSTGD .LT. ((1-DEFIT_M)*SMCAP)) THEN
	QIRR_D = SMCAP - SMSTGD + TRANS_D
     ENDIF
  ELSE
     ! Just use effective rainfall.
     QIRR_D = TRANS_D - SMSTGD ! SMSTGD = effective rain for the day.
     IF (QIRR_D .lt. 0) QIRR_D = 0
  ENDIF
  RETURN
END SUBROUTINE IRGATE
