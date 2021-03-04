      SUBROUTINE WELLSUP_DAILY(BASIN,WY,DOY,PUMP)

C***************************************************************************
C
C   Function        : wellsup_daily.f
C   Author          : LA Garcia & Dave Patterson
C   Date            : Dec 2002
C   Purpose         : This finds what wells the user sets the pumping for and
C                     computes the total pumping and uses it to add to the supply
C                     that is available.  Daily equivalent of wellsup.f
C
C   Calling program : run_cu.f 
C   Called programs : myexit.f
C   Input arguments : basin = basin number
C                     wy = current year
C                     doy = current julian day
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

      INTEGER BASIN, DOY, WY, NW
      REAL PUMP
      INTEGER D, M

C     Comment these out when ready to use
      REAL BWELSUP_D(nbasin, nyrs, 366)
      REAL BWELACU_D(nbasin, nyrs, 366)

      PUMP = 0.0
      BWELSUP_D(BASIN,WY,DOY) = 0.0
      BWELACU_D(BASIN,WY,DOY) = 0.0

      CALL CLNDR(DOY,M,D)

      DO 10 NW = 1, NWELL(BASIN) 
C        -------------------------------------------------------------
C        Check the well mode to use 
C                                   0 - Percent of GW Supplied by Well
C                                   2 - Monthly Flow Rates
C        -------------------------------------------------------------
         IF(WELLMODE .EQ. 2 ) THEN
            IF( WELSUP(BASIN,NW,WY,M) .GT. 0.0 .AND. 
     :           WEFF(BASIN,NW,M) .EQ. 0.0) THEN
               CALL MYEXIT(129)
            ELSEIF( WEFF(BASIN,NW,M) .GT. 0.0) THEN
               PUMP = PUMP + WELSUP(BASIN,NW,WY,M)*WEFF(BASIN,NW,M) 
     :              / MONTH(M) ! Average Daily pumping

C              Sum of discharge measurements in acre-feet for all wells on a
C              farm (modeling area)

               BWELSUP_D(BASIN,WY,DOY) = BWELSUP_D(BASIN,WY,DOY) + 
     :              WELSUP(BASIN,NW,WY,M) / MONTH(M)
            ENDIF
         ENDIF
 10   CONTINUE

C     Well water available for CU each month

      IF(WELLMODE .EQ. 2 ) THEN
         BWELACU_D(BASIN,WY,DOY) = PUMP
      ENDIF

      RETURN
      END
