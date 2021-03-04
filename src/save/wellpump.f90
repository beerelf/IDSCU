SUBROUTINE WELLPUMP(BASIN,WY,M,SHORTW,PUMP,WSFLAG)

  !***************************************************************************
  !
  !   Function        : wellpump.f
  !   Author          : LA Garcia
  !   Date            : October 1998 
  !   Purpose         : This finds if wells are providing water to this particular
  !                     basin/farm and prorates any shortages so that wells provide
  !                     the water.
  ! 
  !   Calling program : run_cu.f 
  !   Called programs : myexit.f
  !   Input arguments : basin = basin number
  !                     wy = current year
  !                     m = current month 
  !                     cirreff = composite irrigation efficiency
  !   Output argument : pump = amount pumped by wells for this basin
  !   Assumptions     :
  !   Limitations     :
  !   Notes           :
  !   History         :(Date, Author, Description)
  !
  !   
  !***************************************************************************

  USE Globals

  !Parameters
  INTEGER BASIN, M, WY, WSFLAG
  REAL PUMP, SHORTW

  !ocals
  INTEGER NW
  REAL WP

  PUMP = 0.0
  WP = 0

  DO NW = 1, NWELL(BASIN)
     !-------------------------------------------------------------
     !Check the well mode to use 
     !        0 - Percent of GW Supplied by Well
     !        2 - Monthly Flow Rates
     !-------------------------------------------------------------
     IF (WMODE(BASIN) .NE. 2 ) THEN
	IF( WSFLAG .EQ. 1) THEN
	   WP = WellPWS(BASIN,WY,M)*WELLPOR(BASIN,NW,WY)
	ELSEIF( WSFLAG .EQ. 0) THEN
	   WP = WellP(BASIN,WY,M)*WELLPOR(BASIN,NW,WY)
	ENDIF
	PUMP = PUMP + WP
     ENDIF
  END DO

  IF(SHORTW .LT. PUMP) THEN
     PUMP = SHORTW
  ENDIF

  IF( PUMP .LT. SHORTW) THEN
     SHORTW = SHORTW - PUMP
  ELSE
     SHORTW = 0.0
  ENDIF


  RETURN
END SUBROUTINE WELLPUMP
