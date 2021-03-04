SUBROUTINE SUPPLY(IB, &
     !Outputs
     suply)


  !***************************************************************************
  !
  !   Function        : supply.f
  !   Author          : HB Manguerra
  !   Date            : December 1994
  !   Purpose         : This reads the monthly water supply data from file
  !                     *.sup.   
  !   Calling program : mainxc.f, proto.f 
  !   Called programs : none
  !   Input arguments : ib = current sub-basin 
  !   Output arguments: none
  !   Assumptions     :
  !   Limitations     :
  !   Notes           : This is repeatedly called for each subarea.
  !                     Water supply should be in acre-ft.
  !
  !   History         :(Date, Author, Description)
  !
  !     11/02/95    HBM    Units changed from 1000 acre-ft to acre-ft.
  !
  !***************************************************************************

  USE Globals

  !Parameters
  INTEGER IB
  !Outputs
  REAL SUPLY(:,:) ! nyrs,12

  !Locals
  INTEGER I,J,IY,NBAS,YR1,YR2
  INTEGER IERR
  REAL GCEFF(NYRS)
  CHARACTER*80 REMARK
  CHARACTER*120 dfile1

  IF (IB.EQ.1) THEN
     dfile1 = dfile
     dfile1(flen:flen+4) = '.sup'
     OPEN (UNIT=19,FILE=dfile1,STATUS='OLD',IOSTAT=IERR)
     IF (IERR.NE.0) CALL MYEXIT(6)
     READ(19,*,ERR=101) NBAS, YR1, YR2
     IF ((YR1.NE.GNYR1).OR.(YR2.NE.GNYR2)) CALL MYEXIT(61)
     IF (NBAS.NE.NBASIN) CALL MYEXIT(60)
     READ(19,900,ERR=101) REMARK
     DO I=1,NBAS
	READ(19,*,ERR=101) (GCEFF(J), J=1,GNYRS)
	DO J = 1,NYRS
	   CEFF(I,J) = GCEFF(J+NYR1-GNYR1)
	END DO
     END DO
  ENDIF

  READ(19,900,ERR=101) REMARK
  CALL SKIPLN(19,NYR1-GNYR1)
  DO J=1,NYRS
     READ(19,*,ERR=101) IY, (SUPLY(J,I), I=1,12), EXIST(J)
  END DO
  !IF the values of suply is missing (less than -90) then set it to
  !  zero for modeling purposes.

  DO I=1,12
     IF ( SUPLY(J,I) .LT. -90.0) &
	  SUPLY(J,I) = 0.0
  END DO

  CALL SKIPLN(19,GNYR2-NYR2)

  IF (IB.EQ.NBASIN) CLOSE(19)


900 FORMAT(A80)

  RETURN

101 CALL MYEXIT(13)
END SUBROUTINE SUPPLY
