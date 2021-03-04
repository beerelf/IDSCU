SUBROUTINE OTABLE(CUO)

  !***************************************************************************
  !
  !   Function        : otable.f
  !   Author          : HB Manguerra
  !   Date            : December 1994
  !   Purpose         : This prepares the summary tables of the CU calculations 
  !                     for non-et categories. 
  !   Calling program : other.f, lstock.f, reserv.f, munic.f, fswild.f,
  !                     stockp.f, minera.f, therma.f, export.f, recrea.f
  !   Called programs : 
  !   Input arguments : cu = monthly consumptive use 
  !   Output arguments: none
  !   Assumptions     : 
  !   Limitations     : 
  !   Notes           : 
  !
  !***************************************************************************

  USE Globals

  !Local variable declaration
  INTEGER I,J,IDUM
  REAL CUO(nyrs,12),ANNUAL(nyrs),MONTLY(12),TOTAL

  !Initialize
  DO I = 1, NYRS 
     ANNUAL(I) = 0.0
  END DO
  DO J = 1, 12
     MONTLY(J) = 0.0
  END DO
  !Calculate Annual Total, Monthly Total, and Grand Total 
  DO I = 1, NYRS
     DO J = 1, 12
	ANNUAL(I) = ANNUAL(I) + CUO(I,J)
	MONTLY(J) = MONTLY(J) + CUO(I,J)
     END DO
  END DO

  TOTAL = 0.0
  DO I = 1,NYRS
     TOTAL = TOTAL + ANNUAL(I)
  END DO

  !Write Results
  WRITE(20,900) DLLINE
  WRITE(20,903) (QUOTE, IDUM=1,28)
  WRITE(20,900) SLLINE
  DO I = 1, NYRS
     WRITE(20,904) NYR1+I-1,(CUO(I,J),J=1,12),ANNUAL(I)
  END DO
  WRITE(20,900) SLLINE

  WRITE(20,905) QUOTE,QUOTE,(MONTLY(J)/NYRS, J=1,12), TOTAL/NYRS

900 FORMAT(A120)

903 FORMAT (1x,A1,'Year',A1,4x,A1,'Jan',A1,3x,A1,'Feb',A1,3x,A1,'Mar', &
	 A1,3x,A1,'Apr',A1,3x,A1,'May',A1,3x,A1,'Jun',A1,3x,A1,'Jul',A1,3x, &
	 A1,'Aug',A1,3x,A1,'Sep',A1,3x,A1,'Oct',A1,3x,A1,'Nov',A1,3x,A1, &
	 'Dec', :A1,5x,A1,'Annual',A1)
904 FORMAT (2x,I4,2x,12F8.1,F12.1)
905 FORMAT (1x,A1,'Mean',A1,1x,12F8.1,F12.1//)

  RETURN
END SUBROUTINE OTABLE
