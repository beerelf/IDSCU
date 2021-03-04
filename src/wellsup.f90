SUBROUTINE WELLSUP(BASIN,WY, L,PUMP, field_eff, NPERIODS)

  !***************************************************************************
  !
  !   Function        : wellsup.f
  !   Author          : LA Garcia
  !   Date            : October 1998 
  !   Purpose         : This finds what wells the user sets the pumping for and
  !                     computes the total pumping and uses it to add to the supply
  !                     that is available.
  !
  !   Calling program : run_cu.f 
  !   Called programs : myexit.f
  !   Input arguments : basin = basin number
  !                     wy = current year
  !                     m = current month 
  !   Output argument : wpump = amount pumped by wells for this basin
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

  !Parameters
  INTEGER BASIN, L, WY, NPERIODS
  REAL PUMP, field_eff

  INTERFACE
     SUBROUTINE CLNDR (jday, year, &
	  !Outputs
	  m_mon, m_day)
       INTEGER jday, year, m_mon, m_day
     END SUBROUTINE CLNDR

     REAL FUNCTION AVG_DAILY(jul, nperiods, val)
       !Input args
       INTEGER jul, nperiods
       REAL val
     END FUNCTION AVG_DAILY
  END INTERFACE

  !Locals
  INTEGER NW, M, D
  REAL well_eff  ! the well efficiency to apply, either the field efficiency
                 !  or the well efficiency.

  !If we are using daily mode, then we need to calculate a
  !month in order to distribute monthly pumping across the current
  !day.
  m_year = nyr1 + wy - 1
  if (nperiods .eq. 12) then
     !Monthly.
     M = L
     D = 0
  else
     !Daily -- convert julian day to month and day
     CALL CLNDR(L, m_year, M, D)
  endif

  PUMP = 0.0
  BWELSUP(BASIN,WY,L) = 0.0
  BWELACU(BASIN,WY,L) = 0.0

  DO NW= 1, NWELL(BASIN) 
     !-------------------------------------------------------------
     !Check the well mode to use 
     !          0 - Percent of GW Supplied by Well
     !          2 - Monthly Flow Rates
     !-------------------------------------------------------------
     IF (WMODE(BASIN) .EQ. 2 ) THEN
	 if (WELL_EFF_IS_FIELD_EFF(BASIN)) then
            well_eff = field_eff
	 else
            well_eff = WEFF(BASIN,NW,WY,M)
	 end if
         
	IF( WELSUP(BASIN,NW,WY,M) .GT. 0.0 .AND. well_eff .EQ. 0.0) THEN
	   CALL MYEXIT(129)
	ELSEIF( well_eff .GT. 0.0) THEN
	   IF (WELSUP(BASIN,NW,WY,M) .GT. 0 .AND. well_eff .GT. 0) THEN
	      PUMP = PUMP + WELSUP(BASIN,NW,WY,M)*well_eff

	      !Sum of discharge measurements in acre-feet for all wells on a
	      !farm (modeling area)
	      IF (nperiods .GT. 12) then
		 ! Convert to average daily.
		 BWELSUP(BASIN,WY,L) = BWELSUP(BASIN,WY,L) + AVG_DAILY(L, nperiods, WELSUP(BASIN,NW,WY,M))
	      ELSE
		 BWELSUP(BASIN,WY,L) = BWELSUP(BASIN,WY,L) + WELSUP(BASIN,NW,WY,M)
	      END IF
	   END IF
	ENDIF
     ENDIF
  END DO

  !Well water available for CU each month/day

  IF (WMODE(BASIN) .EQ. 2 ) THEN
     if (nperiods .gt. 12) then
	!Distribute the monthly pumping 
	BWELACU(BASIN,WY,L) = AVG_DAILY(L, nperiods, PUMP)
     else
	BWELACU(BASIN,WY,L) = PUMP
     endif
  ENDIF

  RETURN
END SUBROUTINE WELLSUP
