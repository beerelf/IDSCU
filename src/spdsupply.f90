SUBROUTINE SPDSUPPLY(IB, et_method, &
				!Outputs
     suply, suploss)

  !***************************************************************************
  !
  !   Function        : spdsupply.f
  !   Author          : LA Garcia
  !   Date            : April 1998 
  !   Purpose         : This reads the daily water supply data from file
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
  !
  !***************************************************************************

  USE Globals

  ! Parameters
  INTEGER IB
  INTEGER et_method         ! ET method to use

  INTERFACE
     INTEGER FUNCTION DaysInYear(yr)
       INTEGER yr
     END FUNCTION DaysInYear
  END INTERFACE

  !Outputs
  REAL SUPLY(:,:), SUPLOSS(:,:)

  !Locals
  INTEGER D,DY,Y,M,IY,NBAS,YR1,YR2
  INTEGER IERR, NDITCH, UNITCODE, FHEADG
  LOGICAL aggToMonthly

  REAL MSUPPLY(31), TSHARES, PERSHARE, CONEFF 

  CHARACTER*30 BNAME, MNAME
  CHARACTER*120 dfile1

  !Ditch shares vary annually.
  INTEGER Y_IDX
  REAL ANNUAL_SHARES(GNYR2-GNYR1+1)

  !These are used in the case where a ditch that is specified for a farm
  !  might also irrigate other acreage, so we want to prorate the ditch
  !  supply by the amount that actually goes to the farm.
  INTEGER prorate_ditch
  REAL DITCH_ACRES(GNYR2-GNYR1+1), TOTAL_DITCH_ACRES(GNYR2-GNYR1+1), ditch_pro

  INTEGER ndyr, ndays, cnt, c

  cnt = 12;
  aggToMonthly = .true.
  if (et_method .eq. 2 .or. et_method .eq. 4 .or. et_method .eq. 6 &
       .or. et_method .eq. 9 .or. et_method .eq. 10) then
     cnt = 366
     aggToMonthly = .false.
  endif

  DO Y=1, NYRS
     DO c=1,cnt
	SUPLY(Y,c) = 0.0
	SUPLOSS(Y,c) = 0.0
     END DO
  END DO

  !Check if this is the first basin/farm/subarea.  If so then open the supply file.
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

  READ(19,900,ERR=105) BNAME, NDITCH, FHEADG, prorate_ditch

  !------------------------------
  ! IF FHEADG = 0 Means use all ditch supplies
  ! IF FHEADG = 1 Means use only supplemental ditch (farm headgate)
  ! IF FHEADG = 2 Means use only non-supplemental ditches
  !-------------------------------

  !Patterson: Read the farm headgate as a ditch.  This will be treated as
  !   supplemental water.
  DO D=1,NDITCH+1
     READ(19,901) DITCHNAM(IB,D), TSHARES, CONEFF, UNITCODE, DITCHSDF(IB,D)
     READ(19, *) (ANNUAL_SHARES(Y), Y = 1,(GNYR2-GNYR1+1))
     READ(19, *) (DITCH_ACRES(Y), Y = 1,(GNYR2-GNYR1+1))
     READ(19, *) (TOTAL_DITCH_ACRES(Y), Y = 1,(GNYR2-GNYR1+1))

     CALL SKIPLN(19,(NYR1-GNYR1)*12)

     DO Y=1, NYRS
	ndyr = DaysInYear(NYR1 + Y -1)
	cnt = 0

 !This compensates for when the simulation year is not the
 !  same as the start of record.
	Y_IDX = Y + (NYR1-GNYR1)

	ditch_pro = 1
	if (prorate_ditch .eq. 1 .and. TOTAL_DITCH_ACRES(Y_IDX) .gt. 0) then
	   ditch_pro = DITCH_ACRES(Y_IDX)/TOTAL_DITCH_ACRES(Y_IDX)
	end if

	DO M=1,12
	   ndays = month(m)
	   if (ndyr.eq.366 .and. m .eq. 2) ndays = 29

	   READ(19,*,ERR=106) IY,MNAME,(MSUPPLY(DY), DY=1,ndays)

	   if ((FHEADG .EQ. 0) .OR. (FHEADG .EQ. 1 .AND. d .EQ. 1) &
		.OR. (FHEADG .EQ. 2 .AND. d .GT. 1)) THEN
       !Check if the supply is at the river or the farm headgate
	      if (tshares .GT. 0) then
		 PERSHARE = (ANNUAL_SHARES(Y_IDX)/TSHARES)
	      else
		 PERSHARE = 0
	      endif

       !Add in ditch proration.
	      PERSHARE = PERSHARE * ditch_pro

       !Put the daily data in the right order.
	      do dy = 1, ndays
	  !Patterson: convert negative numbers to zero (assume these are nodata)
		 IF (MSUPPLY(dy) .LT. 0) MSUPPLY(dy) = 0

		 if (aggToMonthly) then
		    SUPLY(Y,m) = SUPLY(Y,m) + PERSHARE*MSUPPLY(dy)
		    SUPLOSS(Y,m) = SUPLOSS(Y,m) + CONEFF*MSUPPLY(dy) * PERSHARE
		 else
		    SUPLY(Y,cnt+dy) = SUPLY(Y,cnt+dy) + PERSHARE*MSUPPLY(dy)
		    SUPLOSS(Y,cnt+dy) = SUPLOSS(Y,cnt+dy) + &
			 CONEFF*MSUPPLY(dy) * PERSHARE
		 endif
	      end do
	      cnt = cnt + ndays
	   end if
	end do
     END DO
     CALL SKIPLN(19,(GNYR2-NYR2)*12)
  END DO

  IF (IB.EQ.NBASIN) THEN
     CLOSE(19)
  ENDIF

900 FORMAT(A27,I5,I5,I5)
901 FORMAT(A40,2(F10.1),2(I5), 2(F10.1))
902 FORMAT(A40,I10)
903 FORMAT(A40)

  RETURN

105 CALL MYEXIT(105)
106 CALL MYEXIT(106)
107 CALL MYEXIT(107)

END SUBROUTINE SPDSUPPLY
