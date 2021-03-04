      SUBROUTINE WELLPUMP_DAILY(BASIN,WY,DOY,SHORTW,PUMP,WSFLAG)

C***************************************************************************
C
C   Function        : wellpump_daily.f
C   Author          : LA Garcia & Dave Patterson
C   Date            : December 2002
C   Purpose         : This finds if wells are providing water to this particular
C                     basin/farm and prorates any shortages so that wells provide
C                     the water.
C 
C   Calling program : run_cu.f 
C   Called programs : myexit.f
C   Input arguments : basin = basin number
C                     wy = current year
C                     doy = current julian day of year
C                     cirreff = composite irrigation efficiency
C   Output argument : pump = amount pumped by wells for this basin
C   Assumptions     :
C   Limitations     :
C   Notes           :
C   History         :(Date, Author, Description)
C
C   
C***************************************************************************

      USE Globals

      INTEGER BASIN, DOY, WY, NW,WSFLAG
      REAL PUMP, SHORTW, WP
      PUMP = 0.0
      WP = 0

      DO 20 NW = 1, NWELL(BASIN)
C        -------------------------------------------------------------
C        Check the well mode to use 
C                                   0 - Percent of GW Supplied by Well
C                                   2 - Monthly Flow Rates
C        -------------------------------------------------------------
	   IF (WELLMODE .NE. 2 ) THEN
            IF( WSFLAG .EQ. 1) THEN
               WP = WellPWS_D(BASIN,WY,DOY)*WELLPOR(BASIN,NW)
            ELSEIF( WSFLAG .EQ. 0) THEN
               WP = WellP_D(BASIN,WY,DOY)*WELLPOR(BASIN,NW)
            ENDIF
            PUMP = PUMP + WP
         ENDIF
20    CONTINUE

      IF(SHORTW .LT. PUMP) THEN
         PUMP = SHORTW
      ENDIF

      RETURN
      END
