SUBROUTINE RESERV(IB,DLITE,CU_RS)

  !***************************************************************************
  !
  !   Function        : reserv.f
  !   Author          : HB Manguerra
  !   Date            : December 1994
  !   Purpose         : This calculates and summarizes in tabular form the 
  !                     consumptive use from reservoirs.
  !                     The gross evaporation is obtained
  !                     from the product of the surface area of the 
  !                     reservoir and the free water surface (FWS) evaporation.
  !                     The FWS evaporation is computed based on the mean 
  !                     temperature using the Blaney formula.  The net 
  !                     evaporation is obtained by accounting for the rainfall.
  !   Calling program : other.f 
  !   Called programs : oweigh.f, otable.f 
  !   Input arguments : n     = current sub-basin 
  !   Output arguments: cu_rs = monthly consumptive use from reservoir for all
  !                             years (acre-ft).
  !   Assumptions     :
  !   Limitations     :
  !   Notes           : The consumptive use is calculated as parameters are
  !                     read from the *.oth file.  The folowing parameters
  !                     are required:
  !     
  !                     n_rs  = number of reservoirs 
  !                     nsta  = number of weather stations
  !                     sarea = surface area (acre-ft)
  !                     ff    = fullness factor
  !                     w_rs  = station weights used for estimating evapora-
  !                             tion and rainfall from the different weather
  !                             stations.  Evaporation is calculated
  !                             based on the mean temperature.
  !
  !                     The weather data is read from either the *.wm or
  !                     *.wm file.
  !
  !***************************************************************************

  USE Globals

  !Local variable declaration
  INTEGER I,J,K,N_RS,IS,NSTA,IB,RIYR
  REAL SAREA,FF, EVAP
  REAL TM(nyrs,12),RN(nyrs,12),CU_RS(nyrs,12),W_RS(13)
  REAL DLITE(12), KK(12)
  CHARACTER*80 OTITLE, REMARK

  !Monthly coeff from Blaney Formula
  DATA KK/0.70,0.80,0.80,1.04,0.94,1.05,1.02,0.96,1.11,0.94,0.80,0.70/

  !Initialize
  DO I = 1,NYRS
     DO J = 1,12
	CU_RS(I,J) = 0.0
     END DO
  END DO

  READ(2,900) OTITLE
  READ(2,900) REMARK 
  !Read reservoir Data(surface area,fullness factor,weights)
  READ (2,*) N_RS,NSTA
  READ(2,900) REMARK 
  IF( NSTA .NE. 0 .AND. N_RS .NE. 0 ) THEN
     DO K = 1, N_RS
	READ(2,*) SAREA, FF, (W_RS(IS),IS=1,NSTA)

	!Read Temperature and Rainfall Data and Perform Weighting
	CALL OWEIGH(W_RS,TM,RN)

	!Convert Temperature to Evaporation; calculate consumptive use
	DO I = 1, NYRS
	   DO J = 1, 12
	      EVAP = 0.01*DLITE(12)*TM(I,J)*0.7*KK(J) ! inches
	      ! from Blaney Formula
	      CU_RS(I,J) = CU_RS(I,J) + &
		   FF*SAREA*AMAX1((EVAP-RN(I,J)),0.0) ! acre-in
	   END DO
	END DO
     END DO

     !Convert acre-in/month to acre-ft/month
     DO I = 1, NYRS
	DO J = 1, 12
	   CU_RS(I,J) = CU_RS(I,J) / 12.0
	END DO
     END DO

  ELSEIF( N_RS .NE. 0 .AND. NSTA .EQ. 0 ) THEN 

     !Read the actual Evaporation from the reservoirs

     DO K = 1, NYRS
	READ(2,*) RIYR, (CU_RS(K,J),J=1,12)
     END DO
  ENDIF
  WRITE(20,902) QUOTE,BAS_ID(IB),QUOTE
  WRITE(20,901) QUOTE,QUOTE
  CALL OTABLE(CU_RS)

900 FORMAT(A80)
901 FORMAT(A1,'Reservoir Consumptive Use(acre-ft)',A1)
902 FORMAT(A1,A60,A1)

  RETURN
END SUBROUTINE RESERV
