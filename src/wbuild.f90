      SUBROUTINE WBUILD(NDX,IB)

C***************************************************************************
C
C   Function        : wbuild.f
C   Author          : HB Manguerra
C   Date            : December 1994
C   Purpose         : This calculates the winter carry over soil moisture 
C                     for the current growing season.  The total pptn 
C                     during the preceding non-growing period is calculated.
C                     A carry over soil moisture coefficient is multiplied
C                     to the total precipitation to get the carry over soil
C                     moisture. 
C   Calling program : proto.f, mainxc.f 
C   Called programs : none 
C   Input arguments : ndx = estimation method (1 = PM, 2 = BC)
C                     ib  = current sub-basin
C   Output arguments: none 
C   Assumptions     :
C   Limitations     :
C   Notes           : 
C
C***************************************************************************

      USE Globals

C-----Local Variable Declaration
      INTEGER II,IP,IY,ENDYR,NDX,M1,M2,D1,D2,IB
      REAL SUM1, SUM2

      INTERFACE
         INTEGER FUNCTION DaysInYear(yr)
         INTEGER yr
         END FUNCTION

         SUBROUTINE CLNDR (jday, year,
C     Outputs
     $     m_mon, m_day)
         INTEGER jday, year, m_mon, m_day
         END SUBROUTINE
      END INTERFACE

      INTEGER ndays_m1, ndays_m2

      IF (ALLOCATED(WBU)) THEN
         DEALLOCATE(WBU)
      ENDIF
      ALLOCATE(WBU(MAXPARCEL, NYRS))

      IF (NDX.EQ.1) THEN        ! using daily rainfall data 
         DO 5 IP = 1, NPARCE(IB)
            DO 5 IY = 2, NYRS
               ENDYR = 365
               IF (MOD(NYR1+IY-1,4).EQ.0) ENDYR = 366
               SUM1 = 0.0
               SUM2 = 0.0

C-----Sum precipitation of last year after last year growing season
               DO 10 II = JEND(IP,IY-1), ENDYR
                  SUM1 = SUM1 + RNFALL(IY-1,II)    
 10            CONTINUE
C-----Sum precipitation of current year before current growing season
               DO 20 II = 1, JBEG(IP,IY)
                  SUM2 = SUM2 + RNFALL(IY,II)
 20            CONTINUE
               WBU(IP,IY) = SMEF(IY-1)*SUM1 + SMEF(IY)*SUM2
 5       CONTINUE

      ELSE                     ! using monthly rainfall data
         DO 26 IP = 1, NPARCE(IB)
            DO 25 IY = 2, NYRS
               MONTH(2) = 28 
               IF (MOD(NYR1+IY-1,4).EQ.0) MONTH(2) = 29 
               SUM1 = 0.0
               SUM2 = 0.0 
               CALL CLNDR(JEND(IP,IY-1),m1,d1) ! start of nongrowing season
               CALL CLNDR(JBEG(IP,IY),m2,d2) ! end of nongrowing season

               ndays_m1 = month(m1)
               if (m1 .eq. 2) then
                  if (DaysInYear(NYR1+IY-1) .eq. 366) ndays_m1 = 29
               endif
               ndays_m2 = month(m2)
               if (m2 .eq. 2) then
                  if (DaysInYear(NYR1+IY-1) .eq. 366) ndays_m2 = 29
               endif

C--------Sum precipitation of last year after last year growing season
               SUM1 = RNTOT3(ib,IY-1,m1) * (ndays_m1-d1)/ndays_m1
               DO 30 II = m1+1,12 
                  SUM1 = SUM1 + RNTOT3(ib,IY-1,II) 
 30            CONTINUE

C--------Sum precipitation of current year before current growing season
               DO 40 II = 1, m2-1 
                  SUM2 = SUM2 + RNTOT3(ib,IY,II)
 40            CONTINUE

               SUM2 = SUM2 + RNTOT3(ib,IY,m2) * d2/ndays_m2 
               WBU(IP,IY) = SMEF(IY-1)*SUM1 + SMEF(IY)*SUM2
 25         CONTINUE
 26      CONTINUE
      ENDIF

      RETURN
      END
