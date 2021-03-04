SUBROUTINE SPWSUPPLY(IB)

  !***************************************************************************
  !
  !   Function        : spwsupply.f
  !   Author          : LA Garcia
  !   Date            : April 1998 
  !   Purpose         : This reads the monthly water supply data from *.wsp
  !                     input file and incorporate it to the input summary
  !                     file *.sum.	It also reads information from the .wel
  !                     file.   
  !   Calling program : run_cu.f 
  !   Called programs : stable.f, myexit.f
  !   Input arguments : ib = current sub-basin 
  !   Output arguments: none
  !   Assumptions     :
  !   Limitations     :
  !   Notes           :
  !   History         :(Date, Author, Description)
  !
  !
  !***************************************************************************

  USE Globals

  ! Parameters
  INTEGER IB

  INTERFACE
     INTEGER FUNCTION DaysInYear(yr)
       INTEGER yr
     END FUNCTION DaysInYear
  END INTERFACE

  ! Locals
  REAL SUPLY(nyrs, 366), SUPLOSS(nyrs, 366)
  LOGICAL aggToMonthly

  INTEGER D,Y,M,I,IY,J,NBAS,YR1,YR2,IDUM
  INTEGER IERR, NDITCH, UNITCODE, FHEADG

  REAL WSUM(366), WSUMM
  REAL SUPPLY(366), MSUPPLY(31), TSHARES, PERSHARE, CONEFF 

  CHARACTER*30 BNAME, MNAME
  CHARACTER*40 DNAME
  CHARACTER*120 dfile1

  INTEGER ndyr, dy, ndays, cnt, skip_mult

  ! These are used in the case where a ditch that is specified for a farm
  !   might also irrigate other acreage, so we want to prorate the ditch
  !   supply by the amount that actually goes to the farm.
  INTEGER prorate_ditch
  REAL DITCH_ACRES(GNYR2-GNYR1+1), TOTAL_DITCH_ACRES(GNYR2-GNYR1+1), ditch_pro

  ! Ditch shares vary annually.
  INTEGER Y_IDX
  REAL ANNUAL_SHARES(GNYR2-GNYR1+1)

  cnt = 12 ! Monthly
  skip_mult = 1 ! Number of lines to skip per year (1 month's worth)
  aggToMonthly = .true.
  if (hasDailyMethod) then
     cnt = 366 ! Daily
     skip_mult = 12        ! Skip 12 lines per year
     aggToMonthly = .false.
  endif

  DO Y=1, NYRS              
     DO M=1,cnt
	SUPPLY(M) = 0.0
	SUPLY(Y,M) = 0.0
	SUPLOSS(Y,M) = 0.0
     END DO
  END DO


  !-----Check if this is the first basin/farm/subarea.  If so then open the supply file.
  IF (IB.EQ.1) THEN
     dfile1 = dfile
     dfile1(flen:flen+4) = '.sup'
     OPEN (UNIT=19,FILE=dfile1,STATUS='OLD',IOSTAT=IERR)
     IF (IERR.NE.0) CALL MYEXIT(101)
     READ(19,*,ERR=107) NBAS, YR1, YR2
     IF ((YR1.NE.GNYR1).OR.(YR2.NE.GNYR2)) CALL MYEXIT(103)
     IF (NBAS.NE.NBASIN) CALL MYEXIT(104)

     CALL SKIPLN(19,1)      ! Skip vary annual flag.
  ENDIF

  READ(19,800,ERR=105) BNAME, NDITCH, FHEADG, prorate_ditch

  !------------------------------
  ! IF FHEADG = 0 Means use headgate diversion provide by user
  ! IF FHEADG = 1 Means use ditch diversions computed
  !-------------------------------
  IF( FHEADG .EQ. 0 ) THEN
     READ(19,803) DNAME
     READ(19, *) (ANNUAL_SHARES(Y), Y = 1,(GNYR2-GNYR1+1))
     READ(19, *) (DITCH_ACRES(Y), Y = 1,(GNYR2-GNYR1+1))
     READ(19, *) (TOTAL_DITCH_ACRES(Y), Y = 1,(GNYR2-GNYR1+1))
     CALL SKIPLN(19,(NYR1-GNYR1)*skip_mult)
     DO Y=1, NYRS
	if (.not. hasDailyMethod) then
	   READ(19,*,ERR=106) IY,(SUPPLY(M), M=1,12)

	   DO M=1,12
	      IF (SUPPLY(M) .LT. 0) SUPPLY(M) = 0
	      SUPLY(Y,M) = SUPLY(Y,M) + SUPPLY(M)
	      SUPLOSS(Y,M) = SUPLOSS(Y,M) + SUPPLY(M)
	   END DO
	else
	   ndyr = DaysInYear(NYR1 + Y -1)
	   cnt = 0
	   DO M=1,12
	      ndays = month(m)
	      if (ndyr.eq.366 .and. m .eq. 2) ndays = 29

	      READ(19,*,ERR=106) IY,MNAME,(MSUPPLY(DY), DY=1,ndays)

	      ! Put the daily data in the right place.
	      do dy = 1, ndays
		 IF (MSUPPLY(dy) .LT. 0) MSUPPLY(dy) = 0
		 SUPLY(Y, cnt+dy) = MSUPPLY(dy)
		 SUPLOSS(Y, cnt+dy) = MSUPPLY(dy)
	      end do
	      cnt = cnt + ndays
	   end do
	endif

     END DO
     CALL SKIPLN(19,(GNYR2-NYR2)*skip_mult)
     DO D=1,NDITCH
	CALL SKIPLN(19,1) ! Ditch name
	CALL SKIPLN(19,1) ! Ditch shares
	CALL SKIPLN(19,1) ! irrigated acres of ditch
	CALL SKIPLN(19,1) ! total irrigated acres of ditch
	CALL SKIPLN(19,(GNYR2-GNYR1+1)*skip_mult) ! Ditch supply
     END DO
  ELSE
     ! Patterson: Read the farm headgate as a ditch.  This will be
     !    treated as supplemental water.
     DO D=1,NDITCH+1
	READ(19,801) DNAME, TSHARES, CONEFF, UNITCODE
	READ(19, *) (ANNUAL_SHARES(Y), Y = 1,(GNYR2-GNYR1+1))
	READ(19, *) (DITCH_ACRES(Y), Y = 1,(GNYR2-GNYR1+1))
	READ(19, *) (TOTAL_DITCH_ACRES(Y), Y = 1,(GNYR2-GNYR1+1))

	CALL SKIPLN(19,(NYR1-GNYR1)*skip_mult)
	DO Y=1, NYRS              
	   !This compensates for when the simulation year is not the
	   !  same as the start of record.
	   Y_IDX = Y + (NYR1-GNYR1)

	   ditch_pro = 1
	   if (prorate_ditch .eq. 1 .and. TOTAL_DITCH_ACRES(Y_IDX) .gt. 0) then
	      ditch_pro = DITCH_ACRES(Y_IDX)/TOTAL_DITCH_ACRES(Y_IDX)
	   end if

	   if (tshares .GT. 0) then
	      PERSHARE = (ANNUAL_SHARES(Y_IDX)/TSHARES)
	   else
	      PERSHARE = 0
	   endif

	   !Add in ditch proration.
	   PERSHARE = PERSHARE * ditch_pro

	   if (.not. hasDailyMethod) then
	      READ(19,*,ERR=106) IY,(SUPPLY(M), M=1,12)

	      DO M=1,12
		 IF (SUPPLY(M) .LT. 0) SUPPLY(M) = 0
		 SUPLY(Y,M) = SUPLY(Y,M) + PERSHARE*SUPPLY(M)
		 SUPLOSS(Y,M) = SUPLOSS(Y,M) + CONEFF*SUPPLY(M)*PERSHARE
	      END DO
	   else
	      ndyr = DaysInYear(NYR1 + Y -1)
	      cnt = 0
	      DO M=1,12
		 ndays = month(m)
		 if (ndyr.eq.366 .and. m .eq. 2) ndays = 29

		 READ(19,*,ERR=106) IY, MNAME, (MSUPPLY(DY), DY=1,ndays)

		 ! Put the daily data in the right place.
		 do dy = 1, ndays
		    IF (MSUPPLY(dy) .LT. 0) MSUPPLY(dy) = 0
		    SUPPLY(cnt+dy) = MSUPPLY(dy)
		    SUPLY(Y,cnt+dy) = SUPLY(Y,cnt+dy) + &
			 PERSHARE*SUPPLY(cnt+dy)
		    SUPLOSS(Y,cnt+dy) = SUPLOSS(Y,cnt+dy) + &
			 CONEFF*SUPPLY(cnt+dy) * PERSHARE
		 end do
		 cnt = cnt + ndays
	      end do
	   endif
	END DO
	CALL SKIPLN(19,(GNYR2-NYR2)*skip_mult)
     END DO
  ENDIF
  IF (IB.EQ.NBASIN) THEN
     CLOSE(19)
  ENDIF

  !-----Write water supply information into the summary file
  cnt = 12 ! Monthly
  if (hasDailyMethod) then
     cnt = 366 ! Daily
  endif

  IF (S_OUT.EQ.0) THEN
     IF (IB.EQ.1) THEN
	WRITE(8,*)
	WRITE(8,902) QUOTE,QUOTE
	WRITE(8,915) DLLINE
	if (hasDailyMethod) then
	   WRITE(8,9141) (IDUM, IDUM = 1,31)
	else
	   WRITE(8,914)  (QUOTE, IDUM = 1,28)
	endif
	WRITE(8,915) SLLINE
     ENDIF

     DO I = 1, cnt
	WSUM(I) = 0.0
     END DO

     WSUMM = 0.0

     DO J = 1, NYRS
	if (hasDailyMethod) cnt = DaysInYear(NYR1 + J -1)
	DO I = 1, cnt
	   WSUM(I) = WSUM(I) + SUPLY(J,I)
        END DO
     END DO
     if (hasDailyMethod) cnt = 366
     DO  I = 1, cnt
	WSUMM = WSUMM + WSUM(I)
     END DO

     if (.not. hasDailyMethod) then
	WRITE(8,920) QUOTE,BAS_ID(IB),QUOTE,(WSUM(I)/NYRS, &
	     I=1,12),WSUMM/NYRS
     else
	WRITE(8,9201) QUOTE,BAS_ID(IB),QUOTE,(WSUM(I)/NYRS, &
	     I=1,cnt),WSUMM/NYRS
     endif

     IF (IB.EQ.NBASIN) WRITE(8,915) SLLINE

  ELSE
     WRITE(8,901) QUOTE,BAS_ID(IB),QUOTE
     CALL STABLE(SUPLY,0)   ! 0 = calculate annual total
  ENDIF

800 FORMAT(A27,I5,I5,I5)
801 FORMAT(A40,2(F10.1),I5)
802 FORMAT(A40,I10)
803 FORMAT(A40)
901 FORMAT(A1,'Water Supply Information for SubArea (acre-ft) = ', &
       A60,A1)
902 FORMAT(A1,'Water Supply Information',A1)
914 FORMAT (1x,A1,'Subarea',A1,9x,A1,'Jan',A1,3x,A1,'Feb',A1,3x,A1, &
       'Mar',A1,3x,A1,'Apr',A1,3x,A1,'May',A1,3x,A1,'Jun',A1,3x,A1, &
       'Jul',A1,3x,A1,'Aug',A1,3x,A1,'Sep',A1,3x,A1,'Oct',A1,3x,A1, &
       'Nov',A1,3x,A1,'Dec',A1,2x,A1,'Mean',A1)
9141 FORMAT (1x,A1,'Subarea',A1,9x,A1,31(I3,3x),'Mean',A1)
915 FORMAT(A120)
920 FORMAT(A1,A14,A1,13F8.2)
9201 FORMAT(A1,A14,A1,<cnt+1>F8.2)

  RETURN

105 CALL MYEXIT(105)
106 CALL MYEXIT(106)
107 CALL MYEXIT(107)
108 CALL MYEXIT(108)

END SUBROUTINE SPWSUPPLY
