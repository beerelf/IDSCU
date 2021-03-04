      SUBROUTINE WSDIST_DAILY(DOY,IB,tfsup,Re_Ro)

C***************************************************************************
C
C   Function        : wsdist.f
C   Author          : LA Garcia & Dave Patterson
C   Date            : Dec 2002
C   Purpose         : This calculates the water supply distribution for the
C                     surface and groundwater depending on the options the
C                     user has selected.
C   Calling programs: proto.f
C   Called programs :  
C   Input arguments : ib = current basin 
C                   : DOY  = current day
C                   : Re_Ro = Effective Rainfall Runoff
C   Output arguments: none
C   Assumptions     :
C   Limitations     :
C
C   History         :(Date, Author, Description)
C
C
C***************************************************************************

      USE Globals

C-----Local Variable Declaration

      INTEGER DOY, IB
      REAL tfsup, Re_Ro 

C     Comment these out when ready to use
      REAL sur_sup_d(366)
      REAL wel_sup_d(366)
      REAL pcussup(366), pfsuply(366)
      REAL BWELACU_D(nbasin, nyrs, 366)
      INTEGER nyr

      sur_sup_d(DOY) = 0.0
      wel_sup_d(DOY) = 0.0
C
C     SUPDIST - Water Supply Distribution
C             = 1 - Water Supply is Evenly Distributed
C             = 0 - Surface Water Supply is used first
C
C           If the total suply is greater than zero and the distribution
C           is equally distributed between surface and groundwater
C           If well mode is 0 then use gw pumping record

      IF(tfsup .GT. 0.0 .AND. SUPDIST .EQ. 1 .AND. 
     :   WELLMODE .EQ. 2 ) THEN
      
         IF(pcussup(DOY) .GT. tfsup) THEN
            sur_sup_d(DOY) = pfsuply(DOY)
            wel_sup_d(DOY) = tfsup - sur_sup_d(DOY)
         ELSEIF(pcussup(DOY) .LE. tfsup) THEN                                
C
C           Compute the percentage that the surface and GW are of the
C           total supply.
C                  
C           surper = fsuply(DOY)/tfsup
C           gwper = BWELACU(IB,NYR,DOY)/tfsup
C           sur_sup_d(DOY) = pcussup(DOY)*surper
C           wel_sup_d(DOY) = pcussup(DOY)*gwper

            sur_sup_d(DOY) = pcussup(DOY)/2.0
            wel_sup_d(DOY) = pcussup(DOY)/2.0
C
C           Set the total farm supply to the water required
C
            IF(sur_sup_d(DOY) .GT. pfsuply(DOY)) THEN
               sur_sup_d(DOY) = pfsuply(DOY)
               wel_sup_d(DOY) = pcussup(DOY) - sur_sup_d(DOY)
            ELSEIF(wel_sup_d(DOY) .GT. BWELACU_D(IB,NYR,DOY)) THEN
               wel_sup_d(DOY) = BWELACU_D(IB,NYR,DOY)
               sur_sup_d(DOY) = pcussup(DOY) - wel_sup_d(DOY)
            ENDIF 
   
         ENDIF

C              
C        If the total suply is greater than zero and the distribution
C        is first surface and any left over groundwater if amount of
C        water needed is less than what is available
C
      ELSEIF(tfsup .GT. 0.0 .AND. SUPDIST .EQ. 0 ) THEN
CLAG         IF(Re_Ro .GT. 0.0) THEN
CLAG            sur_sup_d(DOY) = 0.0
CLAG            wel_sup_d(DOY) = 0.0
         IF(pcussup(DOY).GT.pfsuply(DOY) .AND. 
     :        pcussup(DOY).LT.tfsup) THEN
            sur_sup_d(DOY) = pfsuply(DOY)
            wel_sup_d(DOY) = pcussup(DOY)-sur_sup_d(DOY)
C
C     If the total suply is greater than zero and the distribution
C     is first surface and any left over groundwater if amount of
C     water needed is more than what is available
C
         ELSEIF(pcussup(DOY).GT.pfsuply(DOY) .AND. 
     :          pcussup(DOY).GE.tfsup) THEN
            sur_sup_d(DOY) = pfsuply(DOY)
            wel_sup_d(DOY) = tfsup-sur_sup_d(DOY)
         ELSEIF(pcussup(DOY) .LE. pfsuply(DOY)) THEN
            sur_sup_d(DOY) = pcussup(DOY)
            wel_sup_d(DOY) = 0.0
         ENDIF    

      ENDIF
C
C     If the well mode is equal to the option where the well pumping
C     is used (2) and it is assigned based on the user selection of
C     surface first of surface and gw in equal amounts.
C
      IF( WELLMODE .EQ. 2 ) THEN
      
C
C        IF Re_Ro is greater or equal to 0 then this is the water budget
C        for the option with soil moisture.
C
         IF( Re_Ro .GE. 0.0 ) THEN
            WellPWS(IB,NYR,DOY) = wel_sup_d(DOY)
         ELSE
            WellP(IB,NYR,DOY) = wel_sup_d(DOY)
         ENDIF
      
      ENDIF
      
	RETURN
	END      
