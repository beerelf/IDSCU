SUBROUTINE RECREA(IB,CU_RC)

  !***************************************************************************
  !
  !   Function        : recrea.f
  !   Author          : HB Manguerra
  !   Date            : April 1995
  !   Purpose         : This summarizes in tabular form the consumptive use
  !                     from recreation category.
  !   Calling program : other.f 
  !   Called programs : otable.f 
  !   Input arguments : ib    = current sub-basin
  !   Output arguments: cu_mn = monthly consumptive use from recreation 
  !                             for all years (acre-ft)
  !   Assumptions     :
  !   Limitations     :
  !   Notes           : The monthly consumptive use is read from *.oth file.  
  !                     No calculation is done other than summarizing them to
  !                     annual values.
  !
  !***************************************************************************

  USE Globals

  !Local Variable Declaration
  INTEGER I,J,IYR,IB
  REAL CU_RC(nyrs,12)
  CHARACTER*80 OTITLE, REMARK

  READ(2,900) OTITLE
  READ(2,900) REMARK 
  !Read Fish and Wildlife Consumptive Data
  CALL SKIPLN(2,NYR1-GNYR1)
  DO I = 1, NYRS
     READ(2,*) IYR, (CU_RC(I,J), J=1,12)
  END DO
  CALL SKIPLN(2,GNYR2-NYR2)

  DO J = 1, 12
     CU_RC(I,J) = CU_RC(I,J)
  END DO

  WRITE(20,902) QUOTE,BAS_ID(IB),QUOTE
  WRITE(20,901) QUOTE,QUOTE
  CALL OTABLE(CU_RC)

900 FORMAT(A80)
901 FORMAT(A1,'Recreation Consumptive Use(acre-ft)',A1)
902 FORMAT(A1,A60,A1)

  RETURN
END SUBROUTINE RECREA
