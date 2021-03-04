REAL FUNCTION ETRREF(DOY,APRM,BPRM,TAVG,TMIN,TMAX,EMX,EMN, &
     EAV,EDPT,WD,RS,SR,ALAIC,s_ELEV,s_ZM,s_ZH, ET_METHOD)

  !***************************************************************************
  !
  !   Function        : etrref.f
  !   Author          : HB Manguerra
  !   Date            : October 1994
  !   Purpose         : This calculates the alfalfa-based reference 
  !                     evapotranspiration.
  !   Calling program : etref.f 
  !   Called programs : none
  !   Input arguments : DOY    = current day of year
  !                     APRM, BPRM - ?
  !                     TAVG - average temp over last four days
  !                     TMIN,TMAX - min and max temp for DOY
  !                     EMX,EMN,EAV,EDPT - ?
  !                     WD,RS - wind and solar radiation for DOY
  !                     ELEV - weather station elevation
  !                     ZM,ZH - weather station ZM (height of wind) and ZH (height of temp & humidity) for DOY
  !   Output arguments: etrout = reference evapotranspiration alfalfa-based 
  !                              for the current day of year
  !   Assumptions     :
  !   Limitations     :
  !   Notes           : The equation formulations are based on ASCE Manuals
  !                     and Reports No. 70 on Evapotranspiration and Irriga-
  !                     tion Water Requirements by Jensen et al., 1990.  Some
  !                     of the routines have been taken from SMB supporting
  !                     program ETOETR.FOR
  !
  !   Definition of Important Variables:
  !
  !     Etr    =  Reference crop evapotranspiration (Alfalfa)
  !     Eto    =  Referance crop evapotranspiration (Grass)
  !     DELTA  =  Slope of the Saturation Vapor Pressure-Temperature curve
  !     PC     =  Psychrometric Constant
  !     RN     =  Net Radiation
  !     G      =  Heat Flux Density to the ground
  !     VPD    =  Vapor Pressure Deficit
  !     Wf     =  Wind Function

  !***************************************************************************

  USE Globals
  USE ET_Parameters

  !Parameters
  INTEGER DOY, ET_METHOD
  REAL APRM,BPRM,TMIN,TMAX,EMN,EMX,EAV,EDPT,WD,RS
  REAL SR, ALAIC
  REAL TAVG(0:3) ! TAVG, TAVG_1, TAVG_2, TAVG_3
  REAL s_ELEV,s_ZM,s_ZH, U2

  INTERFACE
     SUBROUTINE CLNDR (jday, year, &
				!Outputs
          m_mon, m_day)
       INTEGER jday, year, m_mon, m_day
     END SUBROUTINE CLNDR
  END INTERFACE

  !Local Variable Declaration
  INTEGER MON, DAY, N, M
  REAL DELTA, XLTHT, PC, PX, ALBEDO
  REAL XA1, A1, RBO, RSO, RATIO, A, B, Aw, Bw, Wf
  REAL RB, G, UZ, VPD, XNUM, RAALF, XKK1
  REAL ETOALF1, ETOALF2, RN, FAC1, FAC2

  CALL CLNDR(DOY,m_year,MON,DAY)

  LAIALF = 5.5 + 1.5*LHCALF
  RCALF = 200/LAIALF

  ! Changed PC and DELTA computation to be the same as ASCE
  PX = 101.3 * ((293-0.0065 * s_ELEV)/293)**5.26     ! eq. 3 p. 9
  PC = 0.000665 * PX                                 ! eq. 4 p. 9
  DELTA = (2503.0*EXP((17.27*TAVG(0))/(TAVG(0)+237.3)))/ &
       ((TAVG(0) + 237.3)**2)                        ! eq. 5 p. 9

  !DELTA = 4098*EAV / (TAVG(0)+237.3)**2           ! eq. 7.13 p. 175
  ! Atmospheric pressure 
  !PX = 101.3 - .01055 * s_ELEV                      ! eq. 7.4  p. 169
  XLTHT = 2.501 - .002361 * TAVG(0)    ! latent heat  ! eq. 7.1  p. 169
  !PC = ( .001013 * PX) / (.622 * XLTHT)           ! eq. 7.15 p. 175

  !Calculate Solar Radiation
  M = MON
  N = DAY
  ALBEDO  =  0.29 + 0.06 * SIN(30*(M+0.0333*N+2.25)*.0174533) ! convert degrees to radians
  !ALBEDO  =  0.29 + 0.06 * SIN(30*(M+0.0333*N+2.25))
  !ALBEDO = .21 ! eq. 6.67 p. 137
  
  XA1 = (.0154 * (30 * M + N - 207)) **2          
  A1 = .26 + .1 * EXP(-XA1)                       ! eq. 6.68 p. 137

  RBO = (A1 + B1*SQRT(EDPT)) * 4.903E-09 * &       ! eq. 3.17 p. 35
       ((TMAX+273.15)**4 + (TMIN+273.15)**4)  * .50
  ! Dean Santistevan pointed out that RSO is incorrect
  !RSO = APRM + BPRM * COS(6.28318*(DOY-170)/365)  ! eq. 6.66 p. 135
  RSO = APRM + BPRM * COS((6.28318*DOY)/(365-170))

  RATIO = RS / RSO
  IF (RATIO - 0.70) 30,30,40
30 A = 1.017                                       ! p. 137
  B = -0.06                                       ! p. 137
  GO TO 50
40 A = 1.126                                       ! p. 137
  B = -0.07                                       ! p. 137

50 RB = RBO * (A * (RS/RSO) + B)                   ! eq. 3.16 p. 35
  RN = (1 - ALBEDO) * RS - RB                     ! eq. 3.5  p. 30

  ! Psychrometric constant
  !G = 0.377 * (TAVG(0) - ((TAVG(1)+TAVG(2)+TAVG(3))*.3333))
  ! replaced with ASCE version
  G = PC

  !Calculate Wind Function
  !Adjust Wind for height of Anemometer

  UZ = WD/86.4                                    ! uz in m/s   
  IF (UZ .LT. .01) UZ = .01
  VPD = .50 * (EMX+EMN) - EDPT                    ! meth 3, p. 138

  IF( ET_METHOD .EQ. 2 ) THEN
     !Penman-Monteith Alfalfa
     IF (.NOT. useCropMode) THEN
	RAALF = (ALOG((s_ZM-DALF)/ZOMALF)*ALOG((s_ZH-DALF)/ZOHALF)) &
	     / (.1681*UZ)                           ! eq. 6.18 p. 93
	XNUM = PC*(1+RCALF/RAALF)                    ! eq. 6.19 p. 93
	FAC1 = DELTA/(DELTA+XNUM)
	FAC2 = PC/(DELTA+XNUM)
	XKK1 = 1710-6.85*TAVG(0)                     ! eq. 6.24a p. 96
	ETOALF1 = FAC1*(RN-G)
	ETOALF2 = FAC2*XKK1*VPD/RAALF                ! eq. 6.17b p. 96
	ETRREF = (ETOALF1 + ETOALF2) / XLTHT / 25.4
     ELSE
	LAIALF = (5.5 + 1.5*LHCALF) * ALAIC
	RCALF = (SR*Sr_tot)/(0.5*LAIALF)

	RAALF = (ALOG((s_ZM-DALF)/ZOMALF)*ALOG((s_ZH-DALF)/ZOHALF)) &
	     / (.1681*UZ)                           ! eq. 6.18 p. 93
	LAIALF = (5.5 + 1.5*LHCALF) * ALAIC

	RCALF = (SR*Sr_tot)/(0.5*LAIALF)

	XNUM = PC*(1+RCALF/RAALF)                    ! eq. 6.19 p. 93
	FAC1 = DELTA/(DELTA+XNUM)
	FAC2 = PC/(DELTA+XNUM)
	XKK1 = 1710-6.85*TAVG(0)                     ! eq. 6.24a p. 96
	ETOALF1 = FAC1*(RN-G)
	ETOALF2 = FAC2*XKK1*VPD/RAALF                ! eq. 6.17b p. 96
	ETRREF = (ETOALF1 + ETOALF2) / XLTHT / 25.4
     ENDIF
  ELSEIF ( ET_METHOD .EQ. 4 ) THEN
     U2 = Uz * (ALOG((2.0-DALF)/ZOMALF)/ALOG((s_ZM-DALF)/ZOMALF)) ! eq. 7.24 p. 177
     Aw = 0.4 + 1.4*EXP(-((DOY - 173.)/58.0)**2.0) ! eq. 6.27 p. 98
     Bw = 0.605 + 0.345*EXP(-((DOY - 243.)/80.0)**2.0)
     Wf = Aw + Bw * U2
     
     
     
     FAC1 = DELTA/(DELTA+PC)
     FAC2 = PC/(DELTA+PC)
     ETOALF1 = FAC1*(RN-G)
     ETOALF2 = FAC2*6.43*Wf*VPD                   ! eq. 6.15b p. 982
     ETRREF = (ETOALF1 + ETOALF2) / XLTHT / 25.4
  ELSEIF ( ET_METHOD .EQ. 10 ) THEN
	 ! Not defined for alfalfa
	 ETRREF = 0
  ENDIF

  WRITE(29,*) 'Day,EAV,TAVG,DELTA,ELEV,PX,XLTHT,PC'
  WRITE(29,*) DOY,EAV,TAVG(0),DELTA,s_ELEV,PX,XLTHT,PC
  WRITE(29,*) 'ALBEDO,XA1,A1,RBO,RSO,APRM,BPRM,RS,RATIO'
  WRITE(29,*) ALBEDO,XA1,A1,RBO,RSO,APRM,BPRM,RS,RATIO
  WRITE(29,*) 'A,B,RB,RN,G,UZ,EMX,EMN,EDPT,VPD'
  WRITE(29,*) A,B,RB,RN,G,UZ,EMX,EMN,EDPT,VPD
  WRITE(29,*) 'ZM,DALF,ZOMALF,ZH,ZOHALF,RAALF'
  WRITE(29,*) s_ZM,DALF,ZOMALF,s_ZH,ZOHALF,RAALF
  WRITE(29,*) 'RCALF,XNUM,XKK1,ETOALF1,ETOALF2,ETROUT'
  WRITE(29,*) RCALF,XNUM,XKK1,ETOALF1,ETOALF2,ETRREF

  RETURN
END FUNCTION ETRREF
