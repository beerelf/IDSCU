SUBROUTINE TABLE(IFILE,ET_METHOD, nperiods, IB, &
     !Output
     BODY,RIGH,DOWN,PTOT)

  !***************************************************************************
  !
  !   Function        : table.f
  !   Author          : HB Manguerra
  !   Date            : December 1994
  !   Purpose         : This creates the project summary tables for
  !                     each subarea for all years.
  !   Calling program : proj.f
  !   Called programs : none
  !   Input arguments : ifile = file unit number
  !                   : ib    = current basin
  !   Output arguments: body  = monthly water depletion in a basin for all
  !                             years considered.
  !                     righ  = annual water depletion in a basin for all
  !                             years considered.
  !                     down  = average monthly water depletion in the basin
  !                     ptot  = avearge annual water depletion in the basin
  !   Assumptions     :
  !   Limitations     :
  !   Notes           : The values can be provided in acre-ft or in inches.
  !                     This capability is not provided to the user as an 
  !                     option in the input file.  The global variable INCH
  !                     should be set to 1 in this subroutine if the values
  !                     are to be provided in INCHES rather than in ACRE-FT. 
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

  !Parameters
  INTEGER IB, ET_METHOD, IFILE, nperiods(:)
  REAL RIGH(:) !nyrs
  REAL DOWN(:), BODY(:,:), PTOT

  INTERFACE
     INTEGER FUNCTION DaysInYear(yr)
       INTEGER yr
     END FUNCTION DaysInYear
  END INTERFACE

  !Local variable declaration
  INTEGER IY, M, IDUM, IPER, D, NLEAP
  REAL AR_AVG, UDEPTH
  INTEGER offset
  CHARACTER*10 DAILY(12,31), DAILYT(12,31)
  REAL DAILYAVG(12,31)
  LOGICAL is_leap

  !Initialize
  PTOT = 0.0
  DO M = 1,12
     DO  D = 1,31
	DAILYAVG(M,D) = 0.0
     END DO
  END DO

  IF( IB .NE. -99) THEN

     DO IY = 1, NYRS
	RIGH(IY) = 0.0
     END DO

     DO  M = 1,366
	DOWN(M) = 0.0
     END DO

     DO  IY = 1, NYRS
	READ(IFILE,*) (BODY(IY,M), M=1,nperiods(IY))

	!If Project Summary Output in inches
	INCH = 0
	IF (INCH.EQ.1) THEN 
	   DO M  = 1, nperiods(IY)
	      IF (T_AREA(IB,IY).NE.0) BODY(IY,M) = 12000*BODY(IY,M) &
		   / T_AREA(IB,IY)
           END DO
	ENDIF

	IF ( nperiods(IY) .EQ. 12) THEN
	   DO  M = 1, nperiods(IY)
	      !--   Get Annual totals
	      RIGH(IY) = RIGH(IY) + BODY(IY,M)
	      !-Get Monthly/daily Totals
	      DOWN(M) = DOWN(M) + BODY(IY,M)
           END DO
	ENDIF
     END DO
  ENDIF

  DO  IY = 1, NYRS

     IF (nperiods(IY) .eq. 366) THEN
	is_leap = .true.
     ELSE
	is_leap = .false.
     ENDIF

     IF (nperiods(IY) .GT. 13) THEN
	!If is daily and not a leap year, since average year is
	!  assumed to have 366 days, we need to skip the empty leap day cell.
	IF ((m .ge. 3) .and. (.not. is_leap)) then
	   offset = 1
	ELSE 
	   offset = 0
	ENDIF
	IPER = 0
	DO M = 1, 12
	   DO D = 1,MONTH(M)
	      IPER = IPER + 1
	      WRITE(DAILY(M,D),FMT="(F9.3)") BODY(IY,IPER + offset)
	      DAILYAVG(M,D) = DAILYAVG(M,D) + BODY(IY,IPER + offset)
           END DO
	   DO D = MONTH(M) + 1, 31
	      WRITE(DAILY(M,D),FMT="(A9)") "---------" 
           END DO
        END DO
     ENDIF
     PTOT = PTOT + RIGH(IY)
  END DO

  IF (IB .LT. 1 .or. TYPOUT(IB).NE.0) THEN
     IF( IB .EQ. -99 ) THEN
	WRITE(10,907) QUOTE,"Project Total                         ",QUOTE
     ELSE
	WRITE(10,907) QUOTE,BAS_ID(IB),QUOTE
     ENDIF
     IF (INCH.EQ.1) WRITE(10,905) QUOTE,QUOTE  
     IF (INCH.NE.1 .AND. ISUPLY .EQ.1 ) WRITE(10,906) QUOTE,QUOTE
     IF (INCH.NE.1 .AND. ISUPLY .EQ.0 ) WRITE(10,916) QUOTE,QUOTE

     !Check which ET Methods for the No Water Budget Option

     IF(IFILE .EQ. 14) THEN
	IF(et_method .EQ. 1) THEN
	   WRITE (10,908) QUOTE, QUOTE !BC
	ELSEIF(et_method .EQ. 2) THEN
	   WRITE (10,910) QUOTE, QUOTE !PM
	ELSEIF(et_method .EQ. 10) THEN
	   WRITE (10,970) QUOTE, QUOTE !P 48
	ELSEIF(et_method .EQ. 4) THEN
	   WRITE (10,918) QUOTE, QUOTE !KP
	ELSEIF(et_method .EQ. 5) THEN
	   WRITE(10,928) QUOTE, QUOTE !Calibrated BC
	ELSEIF(et_method .EQ. 6) THEN
	   WRITE(10,938) QUOTE, QUOTE !ASCE
	ELSEIF(et_method .EQ. 7) THEN
	   WRITE(10,948) QUOTE, QUOTE !Hargreaves
	ELSEIF(et_method .EQ. 8) THEN
	   WRITE(10,958) QUOTE, QUOTE !Pochop
	ELSEIF(et_method .EQ. 9) THEN
	   WRITE(10,960) QUOTE, QUOTE
	ENDIF
     ENDIF

     IF(IFILE .EQ. 15) THEN
	IF(et_method .EQ. 1) THEN
	   WRITE (10,909) QUOTE, QUOTE !BC
	ELSEIF(et_method .EQ. 2) THEN
	   WRITE (10,911) QUOTE, QUOTE !PM
	ELSEIF(et_method .EQ. 10) THEN
	   WRITE (10,971) QUOTE, QUOTE !P 48
	ELSEIF(et_method .EQ. 4) THEN
	   WRITE (10,919) QUOTE, QUOTE !KP
	ELSEIF(et_method .EQ. 5) THEN
	   WRITE(10,929) QUOTE, QUOTE !Calibrated BC
	ELSEIF(et_method .EQ. 6) THEN
	   WRITE(10,939) QUOTE, QUOTE !ASCE
	ELSEIF(et_method .EQ. 7) THEN
	   WRITE(10,949) QUOTE, QUOTE !Hargreaves
	ELSEIF(et_method .EQ. 8) THEN
	   WRITE(10,959) QUOTE, QUOTE !Pochop
	ELSEIF(et_method .EQ. 9) THEN
	   WRITE(10,961) QUOTE, QUOTE
	ENDIF
     ENDIF

     IF (nperiods(1) > 12) THEN
	WRITE(10,900) DLLINE
	WRITE(10,912) QUOTE, (IDUM, IDUM = 1,31)
	WRITE(10,900) SLLINE
     ELSE
	WRITE(10,900) DLLINE
	WRITE(10,902) (QUOTE, IDUM = 1,32)
	WRITE(10,900) SLLINE
     ENDIF

     NLEAP = 0
     AR_AVG = 0.0
     UDEPTH = 0.0
     DO  IY = 1, NYRS
	IF( IB .EQ. -99 ) THEN
	   AR_AVG = AR_AVG + PJAREA(IY)
	   IF (PJAREA(IY).NE.0) UDEPTH = RIGH(IY)/PJAREA(IY) 

	ELSE
	   AR_AVG = AR_AVG + T_AREA(IB,IY)
	   IF (T_AREA(IB,IY).NE.0) UDEPTH = RIGH(IY)/T_AREA(IB,IY) 
	ENDIF
	IF (nperiods(IY) > 12) THEN
	   !------If is daily and not a leap year, since average year is
	   !------assumed to have 366 days, we need to skip the empty leap day cell.
	   IPER = 0
	   DO M = 1, 12
	      IF (nperiods(IY) .eq. 366 .AND. M .EQ. 2) THEN
		 offset = 1
		 NLEAP = NLEAP + 1
	      ELSE
		 offset = 0
	      ENDIF

	      DO  D = 1,MONTH(M)+offset
		 IPER = IPER + 1
		 WRITE(DAILY(M,D),FMT="(F9.3)") BODY(IY,IPER)
		 DAILYAVG(M,D) = DAILYAVG(M,D) + BODY(IY,IPER)
              END DO
	      DO D = MONTH(M) + offset + 1, 31
		 WRITE(DAILY(M,D),FMT="(A9)") "---------" 
              END DO
	   END DO
	   DO M = 1,12
	      IF( IB .EQ. -99 ) THEN
		 WRITE(10,9031) AMN(M),NYR1+IY-1,PJAREA(IY), &
		      (DAILY(M,D),D=1,31)
	      ELSE
		 WRITE(10,9031) AMN(M),NYR1+IY-1,T_AREA(IB,IY), &
		      (DAILY(M,D),D=1,31)
	      ENDIF
	   END DO
	ELSE
	   IF( IB .EQ. -99 ) THEN
	      WRITE(10,903) NYR1+IY-1,PJAREA(IY),(BODY(IY,M), &
                   M=1,nperiods(IY)), RIGH(IY),UDEPTH

	   ELSE
	      WRITE(10,903) NYR1+IY-1,T_AREA(IB,IY),(BODY(IY,M), &
                   M=1,nperiods(IY)), RIGH(IY),UDEPTH
	   ENDIF
	ENDIF
     END DO

     DO M = 1,12
	IF( M .EQ. 2) THEN
	   offset = 1
	ELSE
	   offset = 0
	ENDIF
	DO D = 1, MONTH(M) + offset
	   IF( M .EQ. 2 .AND. D .EQ. 29) THEN
	      WRITE(DAILYT(M,D),FMT="(F9.3)") DAILYAVG(M,D)/NLEAP
	   ELSE
	      WRITE(DAILYT(M,D),FMT="(F9.3)") DAILYAVG(M,D)/NYRS
	   ENDIF
        END DO
	DO D = MONTH(M) + offset + 1, 31
	   WRITE(DAILYT(M,D),FMT="(A9)") "---------" 
        END DO

     END DO

     UDEPTH = 0.0
     AR_AVG = AR_AVG/NYRS
     IF (AR_AVG.NE.0) UDEPTH = PTOT/NYRS/AR_AVG
     WRITE(10,900) SLLINE
     !Daily ET Methods (Kimberly Penman, Penman Monteith and ASCE)
     IF (hasDailyMethod) THEN

	!Monthly Methods (BC, Calibrated BC, Hargreaves, Pochop)
     ELSE 
	WRITE(10,904) QUOTE,QUOTE, AR_AVG,(DOWN(M)/NYRS, M=1,12), &
	     PTOT/NYRS,UDEPTH
     ENDIF
     WRITE(10,*)
  ENDIF

900 FORMAT (A120)
  !901  FORMAT (56A1,<nper>A1)
9010 FORMAT (12(A6,1x))
912 FORMAT (A1,'Mon,Year ','  Acreage ',31('    ',I2,'    '))
902 FORMAT (A1,'Year',A1,1x,A1,'Acreage',A1,1x,A1,'Jan',A1,4x,A1, &
       'Feb',A1,4x,A1,'Mar',A1,5x,A1,'Apr',A1,5x,A1,'May', &
       A1,5x,A1,'Jun',A1,5x,A1,'Jul',A1,5x,A1,'Aug',A1,5x, &
       A1,'Sep',A1,5x,A1,'Oct',A1,4x,A1,'Nov',A1,3x,A1, &
       'Dec',A1,4x,A1,'Annual',A1,3x,A1,'AF/ac',A1)

903 FORMAT (1x,I4,2x,F9.2,F8.2,2F9.2,7F10.2,F9.2,F8.2,F12.2,F9.2)
9031 FORMAT (1x,A3,",",I4,2x,F9.2,2x,31(A9,1x))
904 FORMAT (A1,'Mean',A1,1x,F9.2,F8.2,2F9.2,7F10.2,F9.2,1x,F8.2,1x, &
       F12.2,1x,F9.2)
9041 FORMAT (1x,A3,7x,31(A9,1x))
905 FORMAT (37x,A1,'Total Depletion of Water Supplies (inches)',A1)
906 FORMAT (35x,A1,'Total Depletion of Water Supplies (acre-ft)',A1)
916 FORMAT (33x,A1,'POTENTIAL Crop Consumptive Use (acre-ft)',A1)

907 FORMAT (A1,A60,A1)
908 FORMAT (35x,A1,'SCS Modified Blaney-Criddle (USBR Original)',A1)
909 FORMAT (35x,A1,'  SCS Modified Blaney-Criddle (Enhanced)',A1)
910 FORMAT (41x,A1,'    Penman-Monteith',A1)
911 FORMAT (35x,A1,'    Penman-Monteith (Water Budget)',A1)
970 FORMAT (41x,A1,'    Penman 1948',A1)
971 FORMAT (35x,A1,'    Penman 1948 (Water Budget)',A1)
918 FORMAT (41x,A1,'    Kimberly-Penman',A1)
919 FORMAT (35x,A1,'Kimberly-Penman (Water Budget)',A1)
928 FORMAT (30x,A1,'SCS Modified Calibrated Blaney-Criddle (USBR Original)',A1)
929 FORMAT (30x,A1,'  SCS Modified Calibrated Blaney-Criddle (Enhanced)',A1)
938 FORMAT (35x,A1,'  ASCE Standarized Ref. ET',A1)
939 FORMAT (27x,A1,'ASCE Standarized Ref. ET (Water Budget)',A1)
948 FORMAT (45x,A1,'Hargreaves',A1)
949 FORMAT (40x,A1,'Hargreaves (Water Budget)',A1)
958 FORMAT (46x,A1,'Pochop',A1)
959 FORMAT (41x,A1,'Pochop (Water Budget)',A1)
960 FORMAT (46x,A1,'User-Supplied ET',A1)
961 FORMAT (41x,A1,'User-Supplied ET (Water Budget)',A1)

  RETURN
END SUBROUTINE TABLE
