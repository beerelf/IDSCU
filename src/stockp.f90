SUBROUTINE STOCKP(IB,DLITE,CU_SP)

  !***************************************************************************
  !
  !   Function        : stockp.f
  !   Author          : HB Manguerra
  !   Date            : December 1994
  !   Purpose         : This calculates and summarizes in tabular forms the 
  !                     consumptive use from stockponds.  The gross evapora-
  !                     tion is obtained from the product of the surface
  !                     area of the stockpond and the  free water surface
  !                     (FWS) evaporation.  The surface area is estimated
  !                     from the tank capacity using an empirical equation.
  !                     The net evaporation is obtained by accounting for the
  !                     rainfall.
  !   Calling program : other.f 
  !   Called programs : oweigh.f, otable.f 
  !   Input arguments : ib    = current basin
  !   Output arguments: cu_sp = monthly consumptive use of stockponds for all
  !                             years (acre-ft).
  !   Assumptions     : The following equation by Williams, J.R. and Arnold,
  !                     J.G (1985) is used to calculate surface area from 
  !                     tank capacity:
  !       
  !                     surface area = w1 x (tank capacity) ^ w2
  !
  !                         w1 = 0.00013 
  !                         w2 = 0.9
  !        
  !                     The above empirical equation which was derived from a 
  !                     large number of stockponds and small reservoirs in
  !                     Texas and Oklahoma is assumed to be valid for other
  !                     areas.
  !                     
  !   Limitations     :
  !   Notes           : The consumptive use is calculated as parameters are
  !                     read from the *.oth file.  The folowing parameters
  !                     are required:
  !                     
  !                     n_sp  = number of stockponds
  !                     nsta  = number of weather stations
  !                     tcap  = tank capacity (acre-ft)
  !                     ff    = fullness factor
  !                     w_sp  = station weights used for estimating evapora-
  !                             tion and rainfall from the different weather
  !                             stations.  Evaporation is calculated
  !                             based on the mean temperature.
  !
  !                     The weather data is read from either the *.wm or
  !                     *.wd file.
  !
  !  History         :  (Date, Author, Description)
  !
  !  10/05/95   HBM  :  
  !
  !  11/18/96   LAG  :  The DLITE array in Line 99 was changed from (12) to
  !                     (J) so it would change it month.
  !
  !***************************************************************************

  USE Globals

  INTEGER I,J,K,N_SP,IS,NSTA,IB 
  REAL SAREA, EVAP, FF, TCAP, W1, W2
  REAL TM(nyrs,12),RN(nyrs,12),CU_SP(nyrs,12),W_SP(13)
  REAL DLITE(12), KK(12)
  CHARACTER*80 OTITLE, REMARK

  !Monthly coeff from Blaney Formula
  DATA KK/0.70,0.80,0.80,1.04,0.94,1.05,1.02,0.96,1.11,0.94,0.80,0.70/

  !Set values of coefficients of equation used for calculatig 
  !  surface area from tank capacity
  W1 = 0.00013
  W2 = 0.9

  !Initialize
  DO I = 1,NYRS
     DO J = 1,12
	CU_SP(I,J) = 0.0
     END DO
  END DO

  READ(2,900) OTITLE
  READ(2,900) REMARK
  !Read Stockpond Data(tank capacity,fullness factor,weights)
  READ (2,*) N_SP,NSTA
  READ(2,900) REMARK
  DO K = 1, N_SP
     READ(2,*) TCAP, FF, (W_SP(IS),IS=1,NSTA)

     !Read Temperature and Rainfall Data and Perform Weighting
     CALL OWEIGH(W_SP,TM,RN)

     !Convert Tank Capacity and Temperature to Surface Area and 
     !  Evaporation, respectively; and calculate consumptive use
     SAREA = 2.471*W1*(1233.5*TCAP)**W2
     DO I = 1, NYRS
	DO J = 1, 12
	   EVAP = 0.01*DLITE(J)*TM(I,J)*0.7*KK(J) ! inches
	   ! from Blaney Formula
	   CU_SP(I,J) = CU_SP(I,J) + &
		FF*SAREA*AMAX1((EVAP-RN(I,J)),0.0) ! acre-in
	END DO
     END DO
  END DO

  !Convert acre-in/month to acre-ft/month
  DO I = 1, NYRS
     DO J = 1, 12
	CU_SP(I,J) = CU_SP(I,J) / 12.0
     END DO
  END DO

  WRITE(20,902) QUOTE,BAS_ID(IB),QUOTE
  WRITE(20,901) QUOTE,QUOTE
  CALL OTABLE(CU_SP)

900 FORMAT(A80)
901 FORMAT(A1,'Stockpond Consumptive Use(acre-ft)',A1)
902 FORMAT(A1,A60,A1)


  RETURN
END SUBROUTINE STOCKP
