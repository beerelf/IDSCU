SUBROUTINE SPWELLW(et_method, nperiods)

  !***************************************************************************
  !
  !   Function        : spwell.f
  !   Author          : LA Garcia
  !   Date            : May 1998 
  !   Purpose         : This writes the well output file *.owl.
  !   Calling program : run_cu.f 
  !   Called programs : stable.f, myexit.f
  !   Input arguments : et_method - Type of ET Method
  !   Output arguments: none
  !   Assumptions     :
  !   Limitations     :
  !   Notes           :
  !   History         :(Date, Author, Description)
  !
  !
  !***************************************************************************

  USE Globals

  INTEGER nperiods

  INTERFACE
     INTEGER FUNCTION DaysInYear(yr)
       INTEGER yr
     END FUNCTION DaysInYear

     SUBROUTINE CLNDR (jday, year, &
				! Outputs
	  m_mon, m_day)
       INTEGER jday, year, m_mon, m_day
     END SUBROUTINE CLNDR
  END INTERFACE

  INTEGER I,IB,IDUM,et_method, INDEX, GetDP_ETindex
  INTEGER WY, NW, Y, L, CREATED
  DATA CREATED /0/
  INTEGER m, d

  REAL WSUM(nyrs,nperiods), PSUM
  REAL WELPOR(maxwell,nyrs,nperiods)

  CHARACTER*120 dfile1

  INTEGER ndays(NYRS) ! Number of days in each year
  INTEGER s_i ! Index of the total column (either 13 or 366/367)

  !-----Write water supply information into the summary file
  IF(CREATED .EQ. 0) THEN
     dfile1 = dfile
     dfile1(flen:flen+4) = '.owl'

     OPEN (UNIT=9,FILE=dfile1,STATUS='UNKNOWN')
     CREATED = 1
  ENDIF

  IF( ISUPLY .NE. 1) THEN
     WRITE(9,921)
     WRITE(9,922)
     WRITE(9,923)
     WRITE(9,924)
  ELSE
     WRITE(9,*)				 
     WRITE(9,902) QUOTE,QUOTE

     ! Write the header telling the user if they are using the 
     ! Blaney-Criddle, Kimberly-Penman, or ASCE WITHOUT WATER BUDGET

     IF( et_method .EQ. 1) THEN
	WRITE(9,906) QUOTE, QUOTE
     ELSEIF( et_method .EQ. 2) THEN
	WRITE(9,912) QUOTE, QUOTE
     ELSEIF( et_method .EQ. 10) THEN
	WRITE(9,972) QUOTE, QUOTE
     ELSEIF( et_method .EQ. 4) THEN
	WRITE(9,908) QUOTE, QUOTE
     ELSEIF( et_method .EQ. 5) THEN
	WRITE(9,910) QUOTE, QUOTE
     ELSEIF( et_method .EQ. 6) THEN
	WRITE(9,928) QUOTE, QUOTE      ! ASCE
     ELSEIF( et_method .EQ. 7) THEN
	WRITE(9,938) QUOTE, QUOTE      ! Hargreaves
     ELSEIF( et_method .EQ. 8) THEN
	WRITE(9,941) QUOTE, QUOTE      ! Pochop
     ELSEIF( et_method .EQ. 9) THEN
	WRITE(9,943) QUOTE, QUOTE      ! User-supplied ET

     ENDIF

     DO IB= 1, NBASIN
	IF( TYPOUT(IB) .GE. 2) THEN

	   WRITE(9,*)
	   IF (RCHGFLG .EQ. 0) THEN
	      WRITE(9,901) NWELL(IB),QUOTE,WBNAME(IB),QUOTE,QUOTE,QUOTE  
	   ELSE
	      WRITE(9,905) NWELL(IB),QUOTE,WBNAME(IB),QUOTE,QUOTE,QUOTE
	   ENDIF
	   WRITE(9,915) DLLINE
	   WRITE(9,914) (QUOTE, IDUM = 1,33)
	   WRITE(9,915) SLLINE

	   DO Y = 1, NYRS
	      if (hasDailyMethod) then
		 ! Daily method
		 ndays(Y) = DaysInYear(Y + NYR1 -1)
	      else
		 ! Monthly
		 ndays(Y) = 12
	      endif

	      DO L = 1, ndays(Y)
		 ! If daily, then we need to get the month we're in.
		 if (hasDailyMethod) then
		    call CLNDR(L, Y + NYR1 -1, m, d)
		 else
		    m = L
		 endif

		 PSUM = 0.0
		 DO NW = 1, NWELL(IB)
		    ! Divide up the CU of groundwater between wells.
		    IF( WellP(IB,Y,L) .LT. 0.01) THEN
		       WELPOR(NW,Y,L) = 0.0
		    ELSE
		       IF (WMODE(IB) .NE. 2 ) THEN
			  WELPOR(NW,Y,L)=WELLPOR(IB,NW,Y)
		       ELSE
			  WELPOR(NW,Y,L)=WELSUP(IB,NW,Y,M)/WellP(IB,Y,L)
		       ENDIF
		       PSUM = PSUM + WELPOR(NW,Y,L)
		    ENDIF
		 END DO
		 DO NW = 1, NWELL(IB)
		    IF(WELPOR(NW,Y,L) .GT. 0.0) THEN
		       WELPOR(NW,Y,L) = WELPOR(NW,Y,L)/PSUM
		    ENDIF
		 END DO
	      END DO
	   END DO

	   DO NW = 1, NWELL(IB)
	      DO WY = 1, NYRS
		 DO I = 1, ndays(WY) +1
		    WSUM(WY,I) = 0.0
		 END DO
	      END DO

	      DO WY = 1, NYRS
		 !-----------------------------------------------------------
		 ! Check the well mode to use 
		 !            0 - Percent of GW Supplied by Well
		 !            2 - Monthly Flow Rates
		 ! Check if shortage should be pro-rated or
		 !   volume/flow pumped is provided
		 !----------------------------------------------------------- 
		 s_i = ndays(WY) + 1

		 IF (WMODE(IB) .NE. 2 ) THEN
		    DO L = 1, ndays(WY)
		       WSUM(WY,L)=WSUM(WY,L) + WellP(IB,WY,L)* &
			    WELPOR(NW,WY,L) ! *** use welpor
		       WSUM(WY,s_i) = WSUM(WY,L) + WSUM(WY,s_i)
		    END DO
		    !-------------------------------------------------------
		    ! If pumping is provided then print the amount actually
		    !   used
		    !-------------------------------------------------------
		 ELSE
		    DO I = 1, ndays(WY)
		       WSUM(WY,I) = WSUM(WY,I) + WellP(IB,WY,I)* &
			    WELPOR(NW,WY,I)
		       WSUM(WY,s_i) = WSUM(WY,I) + WSUM(WY,s_i)
		    END DO
		 ENDIF

		 WRITE(9,920) NYR1+WY-1,QUOTE,WPERNUM(IB,NW),QUOTE, &
		      SDFM(IB,NW),(WSUM(WY,I),I=1,s_i)

              END DO
           END DO

	   IF (RCHGFLG .EQ. 1) THEN  

	      INDEX = GetDP_ETindex(et_method, 0)

	      DO WY = 1, NYRS

		 WRITE(9,930) NYR1+WY-1,QUOTE,QUOTE, &
		      FARMSDF(IB),(FARM_DP(IB,WY,I,INDEX),I=1,s_i)

	      END DO

	      DO WY = 1, NYRS

		 WRITE(9,940) NYR1+WY-1,QUOTE,QUOTE, &
		      0,(FARM_RO(IB,WY,I,INDEX),I=1,s_i)

	      END DO

	   ENDIF
	ENDIF

     END DO

     WRITE(9,915) SLLINE
     !-------------------------------------------------------
     ! Write the header telling the user if they are using the 
     !    Blaney-Criddle, Kimberly-Penman or ASCE WITH WATER BUDGET.
     !-------------------------------------------------------
     WRITE(9,*)				 
     WRITE(9,902) QUOTE,QUOTE

     IF( et_method .EQ. 1) THEN
	WRITE(9,907) QUOTE, QUOTE
     ELSEIF( et_method .EQ. 2) THEN
	WRITE(9,913) QUOTE, QUOTE
     ELSEIF( et_method .EQ. 10) THEN
	WRITE(9,973) QUOTE, QUOTE
     ELSEIF( et_method .EQ. 4) THEN
	WRITE(9,909) QUOTE, QUOTE
     ELSEIF( et_method .EQ. 5) THEN
	WRITE(9,911) QUOTE, QUOTE
     ELSEIF(et_method .EQ. 6) THEN
	WRITE(9,929) QUOTE, QUOTE ! ASCE
     ELSEIF(et_method .EQ. 7) THEN
	WRITE(9,939) QUOTE, QUOTE ! Hargreaves
     ELSEIF(et_method .EQ. 8) THEN
	WRITE(9,942) QUOTE, QUOTE ! Pochop
     ELSEIF(et_method .EQ. 9) THEN
	WRITE(9,944) QUOTE, QUOTE ! User-supplied ET

     ENDIF

     DO IB= 1, NBASIN
	IF( TYPOUT(IB) .GE. 2) THEN

	   WRITE(9,*)
	   IF (RCHGFLG .EQ. 0) THEN
	      WRITE(9,901) NWELL(IB),QUOTE,WBNAME(IB),QUOTE,QUOTE,QUOTE  
	   ELSE
	      WRITE(9,905) NWELL(IB),QUOTE,WBNAME(IB),QUOTE,QUOTE,QUOTE
	   ENDIF
	   WRITE(9,915) DLLINE
	   WRITE(9,914) (QUOTE, IDUM = 1,33)
	   WRITE(9,915) SLLINE

	   DO Y = 1, NYRS
	      if (hasDailyMethod)  then
		 ! Daily method
		 ndays(Y) = DaysInYear(Y + NYR1 -1)
	      else
		 ! Monthly
		 ndays(Y) = 12
	      endif

	      DO L = 1, ndays(Y)
		 ! If daily, then we nede to get the month we're in.
		 if (hasDailyMethod)  then
		    call CLNDR(L, Y + NYR1 -1, m, d)
		 else
		    m = L
		 endif

		 PSUM = 0.0
		 DO NW = 1, NWELL(IB)
		    IF( WellPWS(IB,Y,L) .LT. 0.01) THEN
		       WELPOR(NW,Y,L) = 0.0
		    ELSE
		       IF (WMODE(IB) .NE. 2 ) THEN
			  WELPOR(NW,Y,L)=WELLPOR(IB,NW,Y)
		       ELSE
			  WELPOR(NW,Y,L)=WELSUP(IB,NW,Y,M)/WellPWS(IB,Y,L)
		       ENDIF
		       PSUM = PSUM + WELPOR(NW,Y,L)
		    ENDIF
                 END DO

		 DO NW = 1, NWELL(IB)
		    IF(WELPOR(NW,Y,L) .GT. 0.0) THEN
		       WELPOR(NW,Y,L) = WELPOR(NW,Y,L)/PSUM
		    ENDIF
		 END DO
	      END DO
	   END DO

	   DO NW = 1, NWELL(IB)
	      DO WY = 1, NYRS
		 DO I = 1, ndays(WY) +1
		    WSUM(WY,I) = 0.0
                 END DO
              END DO

	      DO WY = 1, NYRS
		 !--------------------------------------------------------
		 !  Check the well mode to use 
		 !         0 - Percent of GW Supplied by Well
		 !         2 - Monthly Flow Rates
		 !  Check if shortage should be pro-rated or volume/flow pumped is provided
		 !--------------------------------------------------------
		 s_i = ndays(WY) + 1

		 IF (WMODE(IB) .NE. 2 ) THEN
		    DO I = 1, ndays(WY)
		       WSUM(WY,I)=WSUM(WY,I) + WellPWS(IB,WY,I)* &
			    WELPOR(NW,WY,I) ! *** use welpor
		       WSUM(WY,s_i) = WSUM(WY,I) + WSUM(WY,s_i)
		    END DO
		    !----------------------------------------------------------
		    ! If pumping is provided then print the amount actually used
		    !----------------------------------------------------------
		 ELSE
		    DO I = 1, ndays(WY)
		       WSUM(WY,I) = WSUM(WY,I) + WellPWS(IB,WY,I)* &
			    WELPOR(NW,WY,I)
		       WSUM(WY,s_i) = WSUM(WY,I) + WSUM(WY,s_i)
		    END DO
		 ENDIF

		 WRITE(9,920) NYR1+WY-1,QUOTE,WPERNUM(IB,NW),QUOTE, &
                      SDFM(IB,NW),(WSUM(WY,I),I=1,s_i)

              END DO
           END DO

	   IF (RCHGFLG .EQ. 1) THEN  

	      INDEX = GetDP_ETindex(et_method, 0)

	      DO WY = 1, NYRS

		 WRITE(9,930) NYR1+WY-1,QUOTE,QUOTE, &
		      FARMSDF(IB),(FARM_DP(IB,WY,I,INDEX),I=1,s_i)

	      END DO

	      DO WY = 1, NYRS

		 WRITE(9,940) NYR1+WY-1,QUOTE,QUOTE, &
		      0,(FARM_RO(IB,WY,I,INDEX),I=1,s_i)

	      END DO
	   ENDIF
	ENDIF
     END DO

     WRITE(9,915) SLLINE
  ENDIF

901 FORMAT(1x,I3,A1,' Well(s) in SubArea ', &
       A60,A1,A1,' The pumping for each well is shown below',A1)
905 FORMAT(1x,I3,A1,' Well(s) in SubArea ', &
       A60,A1,A1,' The pumping for each well and recharge from runoff and&
       & deep perc is shown below',A1)
902 FORMAT(39x,A1,'   Well Pumping Information (acre-feet)',A1)

906 FORMAT(39x,A1,'SCS Modified Blaney-Criddle (USBR Original)',A1)
907 FORMAT(39x,A1,'  SCS Modified Blaney-Criddle (Enhanced)',A1)
908 FORMAT(53x,A1,'Kimberly-Penman',A1) 
909 FORMAT(46x,A1,'Kimberly-Penman (Water Budget)',A1) 
910 FORMAT (30x,A1,'SCS Modified Calibrated Blaney-Criddle (USBR Origi&
       &nal)',A1)
911 FORMAT (30x,A1,'  SCS Modified Calibrated Blaney-Criddle (Enhanced&
       &)',A1)
912 FORMAT(53x,A1,'Penman-Monteith',A1) 
913 FORMAT(46x,A1,'Penman-Monteith (Water Budget)',A1) 
972 FORMAT(53x,A1,'Penman 1948',A1) 
973 FORMAT(46x,A1,'Penman 1948 (Water Budget)',A1) 

914 FORMAT(A1,'YEAR',A1,A1,'  WDID # ',A1,1x,A1,'SDF ', &
       A1,2x,A1,'Jan',A1,3x, &
       A1,'Feb',A1,3x,A1,'Mar',A1,3x,A1,'Apr',A1,3x,A1,'May',A1,3x,A1, &
       'Jun',A1,3x,A1,'Jul',A1,3x,A1,'Aug',A1,3x,A1,'Sep',A1,3x,A1, &
       'Oct',A1,3x,A1,'Nov',A1,3x,A1,'Dec',A1,3x,A1,'Total',A1/ &
       16x,A1,'(days)',A1)
915 FORMAT(A128)
920 FORMAT(I4,1x,A1,I10,A1,F6.0,<s_i-1>F8.3,F9.3)
930 FORMAT(I4,1x,A1,'DEEP_PERC.',A1,I6,<s_i-1>F8.1,F9.1)
940 FORMAT(I4,1x,A1,' RUNOFF   ',A1,I6,<s_i-1>F8.1,F9.1)
921 FORMAT(1X,'==============================================')
922 FORMAT(1X,'THIS RUN DID NOT CONTAIN A WATER SUPPLY OPTION')
923 FORMAT(1X,'    THEREFORE NO WELL PUMPING IS REPORTED')
924 FORMAT(1X,'==============================================')
928 FORMAT(49x,A1,'ASCE Standarized Ref. ET',A1) 
929 FORMAT(41x,A1,'ASCE Standarized Ref. ET (Water Budget)',A1) 
938 FORMAT(54x,A1,'Hargreaves',A1) 
939 FORMAT(49x,A1,'Hargreaves (Water Budget)',A1) 
941 FORMAT(54x,A1,'Pochop',A1) 
942 FORMAT(49x,A1,'Pochop (Water Budget)',A1) 
943 FORMAT(54x,A1,'User-Supplied ET',A1) 
944 FORMAT(49x,A1,'User-Supplied ET (Water Budget)',A1) 

  RETURN
END SUBROUTINE SPWELLW
