SUBROUTINE STABLE(RAW,FLG)

  !***************************************************************************
  !
  !   Function        : stable.f
  !   Author          : HB Manguerra
  !   Date            : December 1994
  !   Purpose         : This prepares summary tables for water supply. 
  !   Calling program : wsupply.f
  !   Called programs :
  !   Input arguments : raw = monthly raw data 
  !                   : flg = flag (1 = annual average; 0 = annual total)
  !   Output arguments: none
  !   Assumptions     : 
  !   Limitations     : 
  !   Notes           : 
  !
  !***************************************************************************

  USE Globals

  !Local variable declaration
  INTEGER I,J,IDUM
  INTEGER FLG
  REAL RAW(nyrs,12),ANNUAL(nyrs),MONTLY(12),TOTAL

  !Initialize
  DO I = 1, NYRS 
     ANNUAL(I) = 0.0
  END DO
  DO J = 1, 12
     MONTLY(J) = 0.0
  END DO

  !Calculate Annual Total (or average), Monthly Total, and Grand Total 
  DO I = 1, NYRS
     DO J = 1, 12
	ANNUAL(I) = ANNUAL(I) + RAW(I,J)
	MONTLY(J) = MONTLY(J) + RAW(I,J)
     END DO
  END DO

  IF (FLG.EQ.1) THEN
     DO I = 1, NYRS
	ANNUAL(I) = ANNUAL(I)/12.0
     END DO
  ENDIF

  TOTAL = 0.0
  DO I = 1,NYRS
     TOTAL = TOTAL + ANNUAL(I)
  END DO

  !Write Results
  WRITE(8,900) DLLINE
  IF (FLG.EQ.1) THEN
     WRITE(8,903) (QUOTE, IDUM = 1,28)
  ELSE 
     WRITE(8,906) (QUOTE, IDUM = 1,28)
  ENDIF

  WRITE(8,900) SLLINE
  DO I = 1, NYRS
     WRITE(8,904) NYR1+I-1,(RAW(I,J),J=1,12),ANNUAL(I)
  END DO
  WRITE(8,900) SLLINE

  WRITE(8,905) QUOTE,QUOTE,(MONTLY(J)/NYRS, J=1,12), TOTAL/NYRS

900 FORMAT(A120)
903 FORMAT (1x,A1,'Year',A1,4x,A1,'Jan',A1,3x,A1,'Feb',A1,3x,A1,'Mar', &
       A1,3x,A1,'Apr',A1,3x,A1,'May',A1,3x,A1,'Jun',A1,3x,A1,'Jul',A1,3x, &
       A1,'Aug',A1,3x,A1,'Sep',A1,3x,A1,'Oct',A1,3x,A1,'Nov',A1,3x,A1, &
       'Dec', :A1,6x,A1,'Mean',A1)
904 FORMAT (2x,I4,2x,12F8.2,F12.2)
905 FORMAT (1x,A1,'Mean',A1,1x,12F8.2,F12.2//)
906 FORMAT (1x,A1,'Year',A1,4x,A1,'Jan',A1,3x,A1,'Feb',A1,3x,A1,'Mar', &
       A1,3x,A1,'Apr',A1,3x,A1,'May',A1,3x,A1,'Jun',A1,3x,A1,'Jul',A1,3x, &
       A1,'Aug',A1,3x,A1,'Sep',A1,3x,A1,'Oct',A1,3x,A1,'Nov',A1,3x,A1, &
       'Dec', :A1,6x,A1,'Total',A1)

  RETURN
END SUBROUTINE STABLE
