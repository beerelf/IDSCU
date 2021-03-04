SUBROUTINE EXPORT(IB,CU_EX)

  !***************************************************************************
  !
  !   Function        : export.f
  !   Author          : HB Manguerra
  !   Date            : December 1994
  !   Purpose         : This summarizes in tabular form the consumptive use
  !                     from export (transmountain diversions) category.
  !   Calling program : other.f 
  !   Called programs : otable.f 
  !   Input arguments : ib    = current sub-basin
  !   Output arguments: cu_ex = monthly consumptive use from export sites
  !                             for all years (acre-ft)
  !   Assumptions     :
  !   Limitations     :
  !   Notes           : The consumptive use is read from the *.oth file. No
  !                     other computations are involved.
  !
  !***************************************************************************

  USE Globals

  !Local Variable Declaration
  INTEGER I,J,K,IYR,NEX,IB
  REAL CU_EX(nyrs,12),C(12)
  CHARACTER*80 OTITLE, REMARK

  !Initialize
  DO I = 1,NYRS
     DO J = 1,12
	CU_EX(I,J) = 0.0
     END DO
  END DO

  READ(2,900) OTITLE
  READ(2,900) REMARK
  !Read Number of Export Sites and Diversion Amount
  READ(2,*) NEX
  DO K = 1, NEX
     READ(2,900) REMARK
     READ(2,900) REMARK
     CALL SKIPLN(2,NYR1-GNYR1)
     DO I = 1, NYRS
	READ(2,*) IYR, (C(J),J=1,12)
	DO J = 1, 12
	   CU_EX(I,J) = CU_EX(I,J) + C(J)
	END DO
     END DO
     CALL SKIPLN(2,GNYR2-NYR2)
  END DO

  WRITE(20,902) QUOTE,BAS_ID(IB),QUOTE
  WRITE(20,901) QUOTE,QUOTE
  CALL OTABLE(CU_EX)

900 FORMAT(A80)
901 FORMAT(A1,'Export Consumptive Use(acre-ft)',A1)
902 FORMAT(A1,A60,A1)

  RETURN
END SUBROUTINE EXPORT
