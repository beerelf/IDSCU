SUBROUTINE WSUPPLY(IB)

  !***************************************************************************
  !
  !   Function        : wsupply.f
  !   Author          : HB Manguerra
  !   Date            : February 1995 
  !   Purpose         : This reads the monthly water supply data from *.sup
  !                     input file and incorporate it to the input summary
  !                     file *.sum.
  !                     from file *.sup.   
  !   Calling program : run_cu.f 
  !   Called programs : stable.f, myexit.f
  !   Input arguments : ib = current sub-basin 
  !   Output arguments: none
  !   Assumptions     :
  !   Limitations     :
  !   Notes           :
  !   History         :(Date, Author, Description)
  !
  !     11/02/95    HBM    Units changed from 1000 acre-ft to acre-ft.
  !
  !***************************************************************************

  USE Globals

  ! Locals
  INTEGER I,J,IB,IY,NBAS,YR1,YR2,IDUM
  INTEGER IERR
  REAL WSUM(12), WSUMM
  REAL GCEFF(nyrs)
  CHARACTER*80 REMARK
  CHARACTER*120 dfile1
  REAL SUPLY(nyrs, 12)

  IF (IB.EQ.1) THEN
     dfile1 = dfile
     dfile1(flen:flen+4) = '.sup'
     OPEN (UNIT=19,FILE=dfile1,STATUS='OLD',IOSTAT=IERR)
     IF (IERR.NE.0) CALL MYEXIT(6)
     READ(19,*,ERR=101) NBAS, YR1, YR2
     IF ((YR1.NE.GNYR1).OR.(YR2.NE.GNYR2)) CALL MYEXIT(99)
     IF (NBAS.NE.NBASIN) CALL MYEXIT(99)
     READ(19,900,ERR=101) REMARK
     DO I=1,NBAS
	READ(19,*,ERR=101) (GCEFF(J), J=1,NYRS)
	DO J = 1,NYRS
	   CEFF(I,J) = GCEFF(J+NYR1-GNYR1)
	END DO
     END DO
  ENDIF

  READ(19,900,ERR=101) REMARK
  CALL SKIPLN(19,NYR1-GNYR1)
  DO J=1,NYRS
     READ(19,*,ERR=101) IY, (SUPLY(J,I), I=1,12)

     !-----------IF the values of suply is missing (less than -90) then set it to
     !           zero for modeling purposes.

     DO I=1,12
	IF (SUPLY(J,I) .LT. -90.0) SUPLY(J,I) = 0.0
     END DO
  END DO

  CALL SKIPLN(19,GNYR2-NYR2)

  IF (IB.EQ.NBASIN) CLOSE(19)


  !Write water supply information into the summary file
  IF (S_OUT.EQ.0) THEN
     IF (IB.EQ.1) THEN
	WRITE(8,*)
	WRITE(8,902) QUOTE,QUOTE
	WRITE(8,915) DLLINE
	WRITE(8,914) (QUOTE, IDUM = 1,28)
	WRITE(8,915) SLLINE
     ENDIF

     DO I = 1, 12
	WSUM(I) = 0.0
     END DO

     WSUMM = 0.0

     DO J = 1, NYRS
	DO I = 1, 12
	   WSUM(I) = WSUM(I) + SUPLY(J,I)
	END DO
     END DO
     DO I = 1, 12
	WSUMM = WSUMM + WSUM(I)
     END DO

     WRITE(8,920) QUOTE,BAS_ID(IB),QUOTE,(WSUM(I)/NYRS, I=1,12),WSUMM/NYRS

     IF (IB.EQ.NBASIN) WRITE(8,915) SLLINE

  ELSE
     WRITE(8,901) QUOTE,BAS_ID(IB),QUOTE
     CALL STABLE(SUPLY,0)   ! 0 = calculate annual total
  ENDIF


900 FORMAT(A80)
901 FORMAT(A1,'Water Supply Information for SubArea (acre-ft) = ', A40,A1)
902 FORMAT(A1,'Water Supply Information',A1)
914 FORMAT (1x,A1,'Subarea',A1,9x,A1,'Jan',A1,3x,A1,'Feb',A1,3x,A1, &
       'Mar',A1,3x,A1,'Apr',A1,3x,A1,'May',A1,3x,A1,'Jun',A1,3x,A1, &
       'Jul',A1,3x,A1,'Aug',A1,3x,A1,'Sep',A1,3x,A1,'Oct',A1,3x,A1, &
       'Nov',A1,3x,A1,'Dec',A1,2x,A1,'Mean',A1)
915 FORMAT(A120)
920 FORMAT(A1,A14,A1,13F8.2)


  RETURN

101 CALL MYEXIT(13)
END SUBROUTINE WSUPPLY
