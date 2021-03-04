      SUBROUTINE WELLWBP(BASIN,WY,M,SHORTW, PUMP)

C***************************************************************************
C
C   Function        : wellpump.f
C   Author          : LA Garcia
C   Date            : October 1998 
C   Purpose         : This finds if wells are providing water to this particular
C                     basin/farm and prorates any shortages so that wells provide
C                     the water.
C 
C   Calling program : run_cu.f 
C   Called programs : myexit.f
C   Input arguments : basin = basin number
C                     wy = current year
C                     m = current month 
C   Output argument : pump = amount pumped by wells for this basin
C   Assumptions     :
C   Limitations     :
C   Notes           :
C   History         :(Date, Author, Description)
C
C   
C
C
C***************************************************************************

      USE Globals

      INTEGER BASIN, M, WY, NW
      REAL PUMP, SHORTW, PUMPWell

      PUMP = 0.0
      PUMPWell = 0.0

      DO 10 NW= 1, NWELL(BASIN) 
C        -------------------------------------------------------------
C        Check the well mode to use 
C                                   0 - Percent of GW Supplied by Well
C                                   2 - Monthly Flow Rates
C        -------------------------------------------------------------
         IF(WELLMODE(BASIN,NW) .NE. 2 ) THEN
            PUMPWell = SHORTW*WELLPOR(BASIN,NW) 
            PUMP = PUMP + PUMPWell
            WellPWB(BASIN,WY,M) = WellPWB(BASIN,WY,M) + PUMPWell
         ELSE
            IF(SHORTW .GT. WELSUP(BASIN,NW,WY,M)) THEN
               PUMP = PUMP + WELSUP(BASIN,NW,WY,M)
            ELSE
               PUMP = PUMP + SHORTW
            ENDIF
         ENDIF
 10   CONTINUE
      
      IF( PUMP .LT. SHORTW) THEN
         SHORTW = SHORTW - PUMP
      ELSE
         SHORTW = 0.0
      ENDIF

      RETURN

      END
