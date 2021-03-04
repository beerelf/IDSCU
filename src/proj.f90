SUBROUTINE PROJ

  !***************************************************************************
  !
  !   Function        : proj.f
  !   Author          : HB Manguerra
  !   Date            : December 1994
  !   Purpose         : This generates the *.prj file which is a tabular CU
  !                     summary (month and year) as calculated by Penman-
  !                     Monteith and/or Blaney-Criddle ET estimation methods.
  !   Calling program : run_cu.f
  !   Called programs : table.f 
  !   Input arguments : none
  !   Output arguments: none
  !   Assumptions     :
  !   Limitations     :
  !   Notes           : To minimize the use of higher dimensional arrays, the
  !                     results needed to generate the tables were stored
  !                     in temporary files as soon as they were computed by
  !                     the estimation methods.  This subroutine reads these
  !                     temporary files and rewrite them to the desired
  !                     tabular output.
  !
  !
  !   History         : (Date, Author, Description)
  !
  !    5/16/95  HBM   : Change units to acre-ft from 1000 acre-ft
  !
  !   12/14/95  HBM   : Modify summary table to include area and depletion in 
  !                     unit depth (AF/acre)
  !
  !***************************************************************************

  USE Globals

  INTERFACE
     SUBROUTINE TABLE(IFILE,ET_METHOD, nperiods, IB, &
				! Output
	  BODY,RIGH,DOWN,PTOT)
       INTEGER IB, ET_METHOD, IFILE, nperiods(:)
       REAL RIGH(:)           ! nyrs
       REAL DOWN(:), BODY(:,:), PTOT
     END SUBROUTINE TABLE

     FUNCTION DaysInYear(yr)
       INTEGER DaysInYear, yr
     END FUNCTION DaysInYear
  END INTERFACE

  !-----Local variable declarations
  INTEGER I, IY, M, IB, IDUM, et_method
  CHARACTER*120 ofile1 
  REAL RIGH(nyrs)
  REAL DOWN(366)
  REAL PTOT, BODY(nyrs,366)
  REAL BODY_1(nyrs,366,N_ET_M), BODY_2(nyrs,366,N_ET_M)
  REAL RIGH_1(nyrs,N_ET_M), RIGH_2(nyrs,N_ET_M)
  REAL DOWN_1(366,N_ET_M), DOWN_2(366,N_ET_M)
  REAL PTOT_1(N_ET_M), PTOT_2(N_ET_M)
  REAL AR_AVG, UDEPTH 
  INTEGER nperiods(NYRS) ! Number of days in each year
  INTEGER nper ! Number of periods in the current year.

  !-----Open Project Summary Output File for Writing
  IF ((ETFLAG(1).EQ.1).OR.(ETFLAG(2).EQ.1).OR.(ETFLAG(4).EQ.1).OR. &
       (ETFLAG(5).EQ.1).OR.(ETFLAG(6).EQ.1).OR.(ETFLAG(7).EQ.1) &
       .OR.ETFLAG(8).EQ.1 .OR. (ETFLAG(9) .EQ. 1) .OR. (ETFLAG(10) .EQ. 1)) THEN
     ofile1 = dfile
     ofile1(flen:flen+4) = '.prj'
     OPEN(UNIT=10,FILE=ofile1,STATUS='UNKNOWN')

     !---------------------------------------------------------------------------
     !
     !     Original - Summary Table Per SubArea
     !
     !---------------------------------------------------------------------------

     !-----Read temporary file
     OPEN(UNIT=14,FILE='tmp4',STATUS='OLD')
     !-----Read temporary file
     OPEN(UNIT=15,FILE='tmp5',STATUS='OLD')

     DO I=1,N_ET_M
	! If this method is not used, then skip.
	if (ETFLAG(I) .EQ. 0) cycle
	! Print summary for this method.
	et_method = I

	DO IY = 1, NYRS
           IF (et_method .EQ. 2 .OR. et_method .EQ. 6 .OR. et_method .EQ. 10 &
		.OR. et_method .EQ. 4 .OR. et_method .EQ. 9) THEN
	      ! Daily method
	      nperiods(IY) = DaysInYear(IY + NYR1 -1)
	   else
	      ! Monthly
	      nperiods(IY) = 12
	   endif
	END DO

	! Initialize Project Variables
	DO IY = 1, NYRS
	   DO M = 1, nperiods(IY)
	      BODY_1(IY,M,et_method) = 0.0
	   END DO
	END DO

	DO IY =  1, NYRS
	   RIGH_1(IY,et_method) = 0.0
	END DO

	DO M = 1, 366
	   DOWN_1(M,et_method) = 0.0
	END DO

	PTOT_1(et_method) = 0.0

	DO IB = 1,NBASIN
	   CALL TABLE(14,et_method,nperiods,IB,BODY,RIGH,DOWN,PTOT)

	   !-----------Update Project Total
	   DO IY = 1, NYRS
	      RIGH_1(IY,et_method) = RIGH_1(IY,et_method) + RIGH(IY)
	      DO M = 1, nperiods(IY)
		 BODY_1(IY,M,et_method) = BODY_1(IY,M,et_method) &
		      + BODY(IY,M)
              END DO
           END DO

	   DO M = 1, 366
	      DOWN_1(M,et_method) = DOWN_1(M,et_method) + DOWN(M)
	   END DO
	   PTOT_1(et_method) = PTOT_1(et_method) + PTOT 
	END DO

	!---------------------------------------------------------------------------
	!
	!     Enhanced - Summary Table Per SubArea
	!
	!---------------------------------------------------------------------------


	!--------Initialize Project Variables
	DO IY = 1, NYRS
	   DO M = 1, nperiods(IY)
	      BODY_2(IY,M,et_method) = 0.0
	   END DO
	END DO

	DO IY =  1, NYRS
	   RIGH_2(IY,et_method) = 0.0
	END DO

	DO M = 1, 12
	   DOWN_2(M,et_method) = 0.0
	END DO

	PTOT_2(et_method) = 0.0

	DO IB = 1,NBASIN
	   CALL TABLE(15,et_method,nperiods,IB,BODY,RIGH,DOWN,PTOT)

	   !-----Update Project Total
	   DO IY = 1, NYRS
	      RIGH_2(IY,et_method) = RIGH_2(IY,et_method) + RIGH(IY)
	      DO M = 1, nperiods(IY)
		 BODY_2(IY,M,et_method) = BODY_2(IY,M,et_method) + BODY(IY,M)
	      END DO
	   END DO

	   DO M = 1, 12
	      DOWN_2(M,et_method) = DOWN_2(M,et_method) + DOWN(M)
	   END DO
	   PTOT_2(et_method) = PTOT_2(et_method) + PTOT 
	END DO

     END DO

     CLOSE(14,STATUS='delete')
     CLOSE(15,STATUS='delete')
  ENDIF


  !-----Calculate project average annual area
  AR_AVG = 0.0
  DO IY = 1, NYRS
     AR_AVG = AR_AVG + PJAREA(IY)
  END DO
  AR_AVG = AR_AVG/NYRS

  !---------------------------------------------------------------------------
  !
  !     Original - Project Summary Table 
  !
  !---------------------------------------------------------------------------

  DO et_method=1,N_ET_M
     IF (ETFLAG(et_method) .EQ. 0) cycle
     DO IY = 1, NYRS
	IF (et_method .EQ. 2 .OR. et_method .EQ. 6 .OR. et_method .EQ. 10 &
	     .OR. et_method .EQ. 4 .OR. et_method .EQ. 9) THEN
	   ! Daily method
	   nperiods(IY) = DaysInYear(IY + NYR1 -1)
	ELSE
	   ! Monthly
	   nperiods(IY) = 12
	ENDIF
     END DO

     DO IY = 1, NYRS
	DO M = 1, nperiods(IY)
	   BODY(IY,M) = BODY_1(IY,M,et_method)
	END DO
	RIGH(IY) = RIGH_1(IY,et_method)
	DOWN(IY) = DOWN_1(IY,et_method)
     END DO
     CALL TABLE(14,et_method,nperiods,-99,BODY,RIGH,DOWN,PTOT)
     DO IY = 1, NYRS
	DO M = 1, nperiods(IY)
	   BODY(IY,M) = BODY_2(IY,M,et_method)
	END DO
	RIGH(IY) = RIGH_2(IY,et_method)
	DOWN(IY) = DOWN_2(IY,et_method)
     END DO
     CALL TABLE(15,et_method,nperiods,-99,BODY,RIGH,DOWN,PTOT)
  END DO
  RETURN


  DO et_method=1,N_ET_M

     ! If this method is not used, then skip.
     IF (ETFLAG(et_method) .EQ. 0) cycle

     DO IY = 1, NYRS
	IF (et_method .EQ. 2 .OR. et_method .EQ. 6 .OR. et_method .EQ. 10 &
	     .OR. et_method .EQ. 4 .OR. et_method .EQ. 9) THEN
	   ! Daily method
	   nperiods(IY) = DaysInYear(IY + NYR1 -1)
	ELSE
	   ! Monthly
	   nperiods(IY) = 12
	ENDIF
     END DO

     WRITE(10,901) QUOTE, QUOTE
     IF (INCH.EQ.1) WRITE(10,905) QUOTE, QUOTE
     IF (INCH.NE.1 .AND. ISUPLY .EQ.1 ) WRITE(10,906) QUOTE,QUOTE
     IF (INCH.NE.1 .AND. ISUPLY .EQ.0 ) WRITE(10,916) QUOTE,QUOTE

     IF (et_method .EQ. 1) THEN
	WRITE (10,908) QUOTE, QUOTE				! BC
     ELSEIF (et_method .EQ. 2) THEN
	WRITE (10,910) QUOTE, QUOTE				! BC
     ELSEIF (et_method .EQ. 10) THEN
	WRITE (10,970) QUOTE, QUOTE				! BC
     ELSEIF (et_method .EQ. 4) THEN
	WRITE (10,918) QUOTE,QUOTE                 ! KP
     ELSEIF (et_method .EQ. 5) THEN      
	WRITE (10,928) QUOTE, QUOTE                ! Calibrated BC
     ELSEIF (et_method .EQ. 6) THEN      
	WRITE (10,938) QUOTE, QUOTE                ! ASCE
     ELSEIF (et_method .EQ. 7) THEN      
	WRITE (10,948) QUOTE, QUOTE                ! Hargreaves
     ELSEIF (et_method .EQ. 8) THEN      
	WRITE (10,958) QUOTE, QUOTE                ! Pochop
     ELSEIF (et_method .EQ. 9) THEN      
	WRITE (10,968) QUOTE, QUOTE                ! User ET
     ENDIF

     WRITE(10,900) DLLINE
     WRITE(10,902) (QUOTE,IDUM=1,32)
     WRITE(10,900) SLLINE

     UDEPTH = 0.0
     DO IY = 1, NYRS
	IF (PJAREA(IY).NE.0) THEN
	   UDEPTH = RIGH_1(IY,et_method)/PJAREA(IY)
	ENDIF
	IF (nperiods(IY) > 12) THEN
	   nper = nperiods(IY)
	   WRITE(10,9031) NYR1+IY-1,PJAREA(IY), &
		(BODY_1(IY,M,et_method), M=1,nper) &
		,RIGH_1(IY,et_method),UDEPTH
	ELSE
	   WRITE(10,903) NYR1+IY-1,PJAREA(IY), &
		(BODY_1(IY,M,et_method), M=1,nperiods(IY)) &
		,RIGH_1(IY,et_method),UDEPTH
	ENDIF
     END DO

     UDEPTH = 0.0
     IF (AR_AVG.NE.0) UDEPTH = PTOT_1(et_method)/NYRS/AR_AVG
     WRITE(10,900) SLLINE
     IF (nperiods(IY-1) > 12) THEN
	nper = 366 ! Average accounts for leap year.
	WRITE(10,9041) QUOTE,QUOTE, AR_AVG, &
	     (DOWN_1(M,et_method)/NYRS, M=1,nper), &
	     PTOT_1(et_method)/NYRS, UDEPTH
     ELSE
	WRITE(10,904) QUOTE,QUOTE, AR_AVG, &
	     (DOWN_1(M,et_method)/NYRS, M=1,12), &
	     PTOT_1(et_method)/NYRS, UDEPTH
     ENDIF

     !---------------------------------------------------------------------------
     !
     !     Original - Project Summary Table 
     !
     !---------------------------------------------------------------------------

     WRITE(10,901) QUOTE, QUOTE
     IF (INCH.EQ.1) WRITE(10,905) QUOTE, QUOTE
     IF (INCH.NE.1 .AND. ISUPLY.EQ.1 ) WRITE(10,906) QUOTE, QUOTE
     IF (INCH.NE.1 .AND. ISUPLY.EQ.0 ) WRITE(10,916) QUOTE, QUOTE
     IF (et_method .EQ. 1) THEN
	WRITE (10,909) QUOTE, QUOTE                ! BC
     ELSEIF (et_method .EQ. 2) THEN
	WRITE (10,911) QUOTE, QUOTE                ! PM
     ELSEIF (et_method .EQ. 10) THEN
	WRITE (10,971) QUOTE, QUOTE                ! Penman 1948
     ELSEIF (et_method .EQ. 4) THEN
	WRITE (10,919) QUOTE, QUOTE                ! KP
     ELSEIF (et_method .EQ. 5) THEN
	WRITE (10,929) QUOTE, QUOTE                ! Calibrated BC
     ELSEIF (et_method .EQ. 6) THEN
	WRITE (10,939) QUOTE, QUOTE                ! ASCE
     ELSEIF (et_method .EQ. 7) THEN
	WRITE (10,949) QUOTE, QUOTE                ! Hargreaves
     ELSEIF (et_method .EQ. 8) THEN
	WRITE (10,959) QUOTE, QUOTE                ! Pochop
     ELSEIF (et_method .EQ. 9) THEN
	WRITE (10,969) QUOTE, QUOTE                ! User ET
     ENDIF

     WRITE(10,900) DLLINE
     WRITE(10,902) (QUOTE,IDUM=1,32)
     WRITE(10,900) SLLINE

     UDEPTH = 0.0
     DO IY = 1, NYRS
	IF (PJAREA(IY).NE.0) THEN
	   UDEPTH = RIGH_2(IY,et_method)/PJAREA(IY)
	ENDIF
	IF (nperiods(IY) > 12) THEN
	   nper = nperiods(IY)
	   WRITE(10,9031) NYR1+IY-1,PJAREA(IY), &
		(BODY_2(IY,M,et_method), M=1,nper), &
		RIGH_2(IY,et_method),UDEPTH
	ELSE
	   WRITE(10,903) NYR1+IY-1,PJAREA(IY), &
		(BODY_2(IY,M,et_method), M=1,nperiods(IY)), &
		RIGH_2(IY,et_method),UDEPTH
	ENDIF
     END DO

     UDEPTH = 0.0
     IF (AR_AVG.NE.0) UDEPTH = PTOT_2(et_method)/NYRS/AR_AVG
     WRITE(10,900) SLLINE
     IF (nperiods(IY-1) .GT. 12) THEN
	nper = 366 ! Average accounts for leap year.
	WRITE(10,9041) QUOTE,QUOTE, AR_AVG, &
	     (DOWN_2(M,et_method)/NYRS, M=1,nper), &
	     PTOT_2(et_method)/NYRS, UDEPTH
     ELSE
	WRITE(10,904) QUOTE,QUOTE, AR_AVG, &
	     (DOWN_2(M,et_method)/NYRS, M=1,12), &
	     PTOT_2(et_method)/NYRS, UDEPTH
     ENDIF

  END DO

  CLOSE(10)
  CALL FLUSH(10)

900 FORMAT (A120)
902 FORMAT (A1,'Year',A1,2x,A1,'Area',A1,1x,A1,'Jan',A1,4x,A1,'Feb', &
       A1,4x,A1,'Mar', &
       A1,5x,A1,'Apr',A1,5x,A1,'May',A1,5x,A1,'Jun',A1,5x,A1,'Jul',A1,5x, &
       A1,'Aug',A1,5x,A1,'Sep',A1,5x,A1,'Oct',A1,4x,A1,'Nov',A1,3x,A1, &
       'Dec',A1,4x,A1,'Annual',A1,3x,A1,'AF/ac',A1)
903 FORMAT (1x,I4,2x,F9.2,F8.2,2F9.2,7F10.2,F9.2,F8.2,F12.2,F9.2)
9031 FORMAT (1x,I4,2x,F9.2,<nper>F9.3,F12.2,F9.3)
913 FORMAT (1x,I4,2x,F7.0)
904 FORMAT (A1,'Mean',A1,1x,F9.2,F8.2,2F9.2,7F10.2,F9.2,1x,F8.2,1x, &
       F12.2,1x,F9.2)
9041 FORMAT (A1,'Mean',A1,1x,F9.2,<nper>F9.3,1x,F8.2,1x,F12.2,1x,F9.2)
901 FORMAT (A1,'Project Total',A1)
905 FORMAT (37x,A1,'Total Depletion of Water Supplies (inches)',A1)
906 FORMAT (35x,A1,'Total Depletion of Water Supplies (acre-ft)',A1)
916 FORMAT (33x,A1,'POTENTIAL Crop Consumptive Use (acre-ft)',A1)

908 FORMAT (35x,A1,'SCS Modified Blaney-Criddle (USBR Original)',A1)
909 FORMAT (35x,A1,'  SCS Modified Blaney-Criddle (Enhanced)',A1)
910 FORMAT (41x,A1,'    Penman-Monteith',A1)
970 FORMAT (41x,A1,'    Penman 1948',A1)
911 FORMAT (35x,A1,'    Penman-Monteith (Water Budget)',A1)
971 FORMAT (35x,A1,'    Penman 1948 (Water Budget)',A1)
918 FORMAT (41x,A1,'    Kimberly-Penman',A1)
919 FORMAT (35x,A1,'Kimberly-Penman (Water Budget)',A1)
928 FORMAT (30x,A1,'SCS Modified Calibrated Blaney-Criddle (USBR Original)',A1)
929 FORMAT (30x,A1,'  SCS Modified Calibrated Blaney-Criddle (Enhanced)',A1)
938 FORMAT(35x,A1,'ASCE Standarized Ref. ET ',A1)
939 FORMAT(31x,A1,'ASCE Standarized Ref. ET (Water Budget)',A1)

948 FORMAT(43x,A1,'Hargreaves',A1)
949 FORMAT(38x,A1,'Hargreaves (Water Budget)',A1)

958 FORMAT (30x,A1,'Pochop',A1)
959 FORMAT (30x,A1,'Pochop (Water Budget)',A1)

968 FORMAT (30x,A1,'User-Supplied ET',A1)
969 FORMAT (30x,A1,'User-Supplied ET (Water Budget)',A1)

  RETURN
END SUBROUTINE PROJ
