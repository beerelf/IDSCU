SUBROUTINE SPWELLR

  !***************************************************************************
  !
  !   Function        : spwellr.f
  !   Author          : LA Garcia
  !   Date            : May 1998 
  !   Purpose         : This reads the well input file *.well and calculates the
  !                     amount of pumpage that needs to be distributed to each
  !                     well associated with a farm or basin.
  ! 
  !   Calling program : run_cu.f 
  !   Called programs : myexit.f
  !   Input arguments : none 
  !   Output arguments: none
  !   Assumptions     :
  !   Limitations     :
  !   Notes           :
  !   History         :(Date, Author, Description)
  !
  !   
  !
  !
  !***************************************************************************

  USE Globals

  INTEGER M, I, IB
  INTEGER WYR1, WYR2, WNBAS, WY, NW
  INTEGER WIY, SDF0(MAXWELL), SDF1(MAXWELL), SDF2(MAXWELL)
  INTEGER SDF3(MAXWELL)
  INTEGER SDFMODE(MAXWELL)
  INTEGER IERR, WELLUNIT(MAXWELL), PUMPDAY(12)

  REAL WSUM(nyrs,13), PERIRR(MAXWELL, GNYR2-GNYR1+1)
  INTEGER Y_IDX
  REAL WELLSHARE(MAXWELL,nyrs), TSHARE(nyrs)
  REAL PUMPCFS(12)
  REAL DEEP_PERC_OVERRIDE

  CHARACTER*60 WNAME(MAXWELL)
  CHARACTER*120 dfile1
  LOGICAL WFILEXST

  dfile1 = dfile
  dfile1(flen:flen+4) = '.wel'
  WFILEXST = .FALSE.
  INQUIRE(FILE=dfile1, EXIST=WFILEXST)
  IF(WFILEXST) THEN 
     OPEN (UNIT=21,FILE=dfile1,STATUS='OLD',IOSTAT=IERR)
     IF (IERR.NE.0) CALL MYEXIT(102)
  ENDIF

  IF(WFILEXST) THEN
     READ(21,*,ERR=108) WNBAS, WYR1, WYR2
     IF ((WYR1.NE.GNYR1).OR.(WYR2.NE.GNYR2)) CALL MYEXIT(109)
     IF (WNBAS.NE.NBASIN) CALL MYEXIT(110)

     !Patterson: Well limit flag.
     read(21,*) LIMIT_WELL_DISCH

     DO IB= 1, NBASIN
	READ(21,800,ERR=111) WBNAME(IB)
	READ(21, *) NWELL(IB), SUPDIST(IB), WELL_EFF_IS_FIELD_EFF(IB)
	DO NW = 1, NWELL(IB)
	   READ(21,802) WNAME(NW), WPERNUM(IB,NW)
	   READ(21,*)SDF0(NW),SDF1(NW),SDF2(NW),SDF3(NW), SDFMODE(NW), WFLOW(IB,NW), DEEP_PERC_OVERRIDE

	   !--------------------------------------------------------------
	   ! Check the well mode to use 
	   !        0 - Percent of GW Supplied by Well
	   !        2 - Monthly Flow Rates
	   !--------------------------------------------------------------

	   READ(21,*)WMODE(IB),WELLUNIT(NW)

	   ! Read well portion for each year.
	   READ(21,*,ERR=112) (PERIRR(NW, WY), WY=1,GNYR2-GNYR1+1)

	   ! Read well application efficiency for each year.
	   CALL SKIPLN(21,(NYR1-GNYR1))
	   DO WY=1,NYRS
	      READ(21,*,ERR=112) WIY,(WEFF(IB,NW,WY,M), M=1,12)
	   END DO

	   CALL SKIPLN(21,(GNYR2-NYR2))
	   CALL SKIPLN(21,(NYR1-GNYR1))

	   !---------------------------------------------------------------
	   ! IF the input is well pumping in CFS then I need to read a flow
	   !    and a number of days.
	   !---------------------------------------------------------------
	   IF (WMODE(IB) .EQ. 2 .AND. WELLUNIT(NW) .EQ. 0 ) THEN
	      DO WY=1,NYRS
		 READ(21,*,ERR=112)WIY,(PUMPDAY(M),PUMPCFS(M),M=1,12)
		 DO M = 1, 12
		    !-------------------------------------------
		    ! Conversion from CFS per day to AF is 1.9835
		    !-------------------------------------------
		    WELSUP(IB,NW,WY,M)=PUMPDAY(M)*PUMPCFS(M)*1.9835
                 END DO
	      END DO
	   ELSE
	      DO WY=1,NYRS      
		 READ(21,*,ERR=112) WIY,(WELSUP(IB,NW,WY,M), M=1,12)
	      END DO
	   ENDIF
	   CALL SKIPLN(21,(GNYR2-NYR2))
	END DO

	DO WY = 1, NYRS
	   TSHARE(WY) = 0.0
        END DO
	!---------------------------------------------------------------------
	! Check if Well Pumping is Given Otherwise Distribute Shortages to the wells
	!--------------------------------------------------------------------------
	DO NW = 1, NWELL(IB)
	   IF (WMODE(IB) .NE. 2) THEN
	      DO WY = 1, NYRS
		 !This compensates for when the simulation year is not the
		 !  same as the start of record.
		 Y_IDX = WY + (NYR1-GNYR1)

		 !--------------------------------------------------------
		 ! Check the well mode to use 
		 !       0 - Percent of GW Supplied by Well
		 !       2 - Monthly Flow Rates
		 !--------------------------------------------------------
		 IF (WMODE(IB) .EQ. 0) THEN
		    WELLSHARE(NW,WY) = PERIRR(NW, Y_IDX)
		 ENDIF
		 TSHARE(WY) = TSHARE(WY) + WELLSHARE(NW,WY) 
	      END DO
	   ENDIF
        END DO

	DO NW = 1, NWELL(IB)
	   !----------------------------------------------------------
	   ! Check the type of SDF to use
	   !       0 - Augmentation Plan Value
	   !       1 - Based on Decreed Location
	   !       2 - Based on GPS Location
	   !       3 - User Defined
	   !----------------------------------------------------------
	   IF(SDFMODE(NW) .EQ. 0) THEN
	      SDFM(IB,NW) = SDF0(NW)
	   ELSEIF(SDFMODE(NW) .EQ. 1) THEN
	      SDFM(IB,NW) = SDF1(NW)
	   ELSEIF(SDFMODE(NW) .EQ. 2) THEN
	      SDFM(IB,NW) = SDF2(NW) 
	   ELSEIF(SDFMODE(NW) .EQ. 3) THEN
	      SDFM(IB,NW) = SDF3(NW)
	   ENDIF

	   DO WY = 1, NYRS
	      DO I = 1, 13
		 WSUM(WY,I) = 0.0
	      END DO
	   END DO

	   DO WY = 1, NYRS
	      !--------------------------------------------------------
	      ! Check the well mode to use 
	      !    0 - Percent of GW Supplied by Well
	      !    2 - Monthly Flow Rates
	      ! Check if shortage should be pro-rated or
	      !    volume/flow pumped is provided
	      !-------------------------------------------------------- 
	      IF(WMODE(IB) .NE. 2 ) THEN
		 !-----------------------------------------------------
		 ! For each well compute the pro-ration of total pumping
		 ! If no wells are assigned to the farm
		 !-----------------------------------------------------  
		 IF(.false. .and. TSHARE(WY) .LT. 0.001 ) THEN ! disabled because wells should be allowed to be shut off.
		    WELLPOR(IB,NW,WY) = 1.0/NWELL(IB) 
		    !---------------------------------------------------
		    ! If well shares add to more than 1 then prorate the
		    !   amounts so that they add to 1.                     
		    !---------------------------------------------------  
		 ELSEIF( TSHARE(WY) .GT. 1) THEN
		    WELLPOR(IB,NW,WY) = WELLSHARE(NW,WY)/TSHARE(WY) 
		 ELSE
		    ! If well shares add to less than one then leave it alone
		    ! NOTE !!!! This means that some of the shortages are not assigned
		    !   to wells.
		    WELLPOR(IB,NW,WY) = WELLSHARE(NW,WY)      
		    !
		    ! NOTE !!!! This means that no shortages are allowed the shortages
		    !   are pro-rated.
		    !
		    ! Added Patterson 10/1/03  Wells are now allowed to irrigate only a portion of the farm.
		    !                        WELLPOR(IB,NW) = WELLSHARE(NW)/TSHARE(WY)

		 ENDIF
	      ENDIF
           END DO
        END DO

     END DO

     CLOSE(21)
  ENDIF

800 FORMAT(A60)
802 FORMAT(A60,I10)
803 FORMAT(A27)

  RETURN

108 CALL MYEXIT(108)
111 CALL MYEXIT(111)
112 CALL MYEXIT(112)

END SUBROUTINE SPWELLR
