REAL FUNCTION ETOREF(DOY,APRM,BPRM,TAVG,TMIN,TMAX,EMX,EMN, &
     EAV,EDPT,WD,RS,s_ELEV,s_ZM,s_ZH, ET_METHOD)

  !***************************************************************************
  !
  !   Function        : etoref.f
  !   Author          : HB Manguerra
  !   Date            : December 1994
  !   Purpose         : This calculates the grass-based reference evapotrans- 
  !                     piration during the given day. 
  !   Calling program : etref.f
  !   Called programs : none
  !   Input arguments : DOY    = current day of year
  !                     APRM, BPRM - ?
  !                     TAVG - average temp over last four days
  !                     TMIN,TMAX - min and max temp for DOY
  !                     EMX,EMN,EAV,EDPT - ?
  !                     WD,RS - wind and solar radiation for DOY
  !                     ELEV - weather station elevation
  !                     ZM,ZH - weather station ZM and ZH for DOY
  !   Output arguments: etoout = reference evapotranspiration grass-based for
  !                              the current day of year 
  !   Assumptions     :
  !   Limitations     :
  !   Notes           : The equation formulations are based on ASCE Manuals
  !                     and Reports No. 70 on Evapotranspiration and Irriga-
  !                     tion Water Requirements by Jensen et al., 1990. Some
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
  !
  !***************************************************************************

  USE Globals
  USE ET_Parameters

  !Parameters
  INTEGER DOY, ET_METHOD
  REAL APRM,BPRM,TMIN,TMAX,EMN,EMX,EAV,EDPT,WD,RS
  REAL TAVG(0:3) ! TAVG, TAVG_1, TAVG_2, TAVG_3
  REAL s_ELEV,s_ZM,s_ZH

  !Local Variable Declaration
  INTEGER MON, DAY, N, M
  REAL DELTA, XLTHT, PC, PX, ALBEDO
  REAL XA1, A1, RBO, RSO, RATIO, A, B, Aw, Bw, Wf 
  REAL RB, G, UZ, VPD, XNUM, RAGRS, XKK1
  REAL ETOGRS1, ETOGRS2, RN, FAC1, FAC2, FAC3, FAC4, U2


  CALL CLNDR(DOY,m_year,MON,DAY)

  LAIALF = 5.5 + 1.5*LHCALF
  RCALF = 200/LAIALF

190 DELTA = 4098*EAV / (TAVG(0)+237.3)**2            ! eq. 7.13 p. 175
  PX = 101.3 - .01055 * s_ELEV                     ! eq. 7.4  p. 169
  XLTHT = 2.501 - .002361 * TAVG(0)                ! eq. 7.1  p. 169
  PC = ( .001013 * PX) / (.622 * XLTHT)            ! eq. 7.15 p. 175

  !Calculate Solar Radiation
  M = MON
  N = DAY
  ALBEDO  =  0.29 + 0.06 * SIN(30*(M+0.0333*N+2.25)*.0175)
  ! eq. 6.67 p. 137
  XA1 = (.0154 * (30 * M + N - 207)) **2
  A1 = .26 + .1 * EXP(-XA1)                        ! eq. 6.68 p. 137

  RBO = (A1 + B1*SQRT(EDPT)) * 4.903E-09 * &        ! eq. 3.17 p. 35
       ((TMAX+273.15)**4 + (TMIN+273.15)**4)  * .50
  RSO = APRM + BPRM * COS((6.28318*DOY)/(365-172)) ! eq. 6.66 p. 135

  RATIO = RS / RSO
  IF (RATIO - 0.70) 30,30,40
30 A = 1.017                                        ! p. 137
  B = -0.06                                        ! p. 137
  GO TO 50
40 A = 1.126                                        ! p. 137
  B = -0.07                                        ! p. 137

50 RB = RBO * (A * (RS/RSO) + B)                    ! eq. 3.16 p. 35
  RN = (1 - ALBEDO) * RS - RB                      ! eq. 3.5  p. 30

  G = 0.377 * (TAVG(0) - ((TAVG(1)+TAVG(2)+TAVG(3))*.3333))

  !Calculate wind function
  !Adjust Wind for height of Anemometer

  UZ = WD/86.4                                     ! uz in m/s
  IF (UZ .LT. .01) UZ = .01

  VPD = .50 * (EMX+EMN) - EDPT                     ! meth 3, p. 138

  IF( ET_METHOD .EQ. 2 ) THEN
     ! Penman Monteith Grass  ***     
     RAGRS = (ALOG((s_ZM-DGRS)/ZOMGRS)*ALOG((s_ZH-DGRS)/ZOHGRS)) &
	  / (.1681*UZ)                            ! eq. 6.18 p. 93
     XNUM = PC*(1+RCGRS/RAGRS)                     ! eq. 6.19 p. 93
     FAC3 = DELTA/(DELTA+XNUM)
     FAC4 = PC/(DELTA+XNUM)
     XKK1 = 1710-6.85*TAVG(0)                      ! eq. 6.24a p. 96
     ETOGRS1 = FAC3*(RN-G)
     ETOGRS2 = FAC4*XKK1*VPD/RAGRS                 ! eq. 6.17b p. 93
     ETOREF = (ETOGRS1 + ETOGRS2) / XLTHT / 25.4
  ELSEIF ( ET_METHOD .EQ. 4 ) THEN
     U2 = Uz * (ALOG((2.0-DGRS)/ZOMGRS)/ALOG((s_ZH-DGRS)/ZOMGRS))
     ! eq. 7.24 p. 177
     Aw = 0.4 + 1.4*EXP(-((DOY - 173.)/58.0)**2.0) ! eq. 6.27 p. 98
     Bw = 0.605 + 0.345*EXP(-((DOY - 243.)/80.0)**2.0)
     Wf = Aw + Bw * U2
     FAC1 = DELTA/(DELTA+PC)
     FAC2 = PC/(DELTA+PC)
     ETOGRS1 = FAC1*(RN-G)
     ETOGRS2 = FAC2*6.43*Wf*VPD                    ! eq. 6.15b p. 98
     ETOREF = (ETOGRS1 + ETOGRS2) / XLTHT / 25.4
  ELSEIF ( ET_METHOD .EQ. 10 ) THEN
     ! Calculate saturation vapor pressure at mean air temp
     ! Patterson: using http://www.fao.org/docrep/X0490E/x0490e07.htm#air%20humidity

     ! Keeping our delta and vpdiff (VPD) calculation.

     XLTHT = 2.45 ! latent heat MJ kg-1

     ! Atmospheric pressure
     PX = 101.3 * (((293 - 0.0065 * s_ELEV) / 293) ** 5.26)       ! kPa

     PC = 1.013E-3 * PX / (.622 * XLTHT) ! kPa/C deg

     ! Convert wind from km/day to m/s
     U2 = WD/86400 * 4.87 / alog(67.8 * s_ZM)  ! m/s wind speed at 2 m

     ! For MRGCD:
     ! WD km/day
     !U2 = WD*((2/3.75)**.2)
     !U2 = U2 / 86.4 ! km/day -> m/s

     Wf = 15.36 * (1.0 + (0.0062*U2))  ! U2 converted from m/s -> km/day
     
     FAC1 = DELTA/(DELTA+PC)
     FAC2 = PC/(DELTA+PC)
     ETOGRS1 = FAC1*(RN-G)
     ETOGRS2 = FAC2*Wf*VPD                    ! eq. 6.15b p. 98
     ETOREF = (ETOGRS1 + ETOGRS2) / XLTHT / 25.4
  ENDIF

  !     IF ((M.EQ.6).AND.(N.EQ.1)) THEN
  !	  WRITE(*,*) 'EAV,TAVG,DELTA,ELEV,PX,XLTHT,PC'
  !       WRITE(*,*) EAV,TAVG(0),DELTA,s_ELEV,PX,XLTHT,PC
  !	  WRITE(*,*) TAVG(1),TAVG(2),TAVG(3)
  !	  WRITE(*,*)
  !	  WRITE(*,*) 'ALBEDO,XA1,A1,RBO,RSO,APRM,BPRM,RS,RATIO'
  !	  WRITE(*,*) ALBEDO,XA1,A1,RBO,RSO,APRM,BPRM,RS,RATIO
  !	  WRITE(*,*)
  !	  WRITE(*,*) 'A,B,RB,RN,G,UZ,EMX,EMN,EDPT,VPD'
  !	  WRITE(*,*) A,B,RB,RN,G,UZ,EMX,EMN,EDPT,VPD
  !	  WRITE(*,*)
  !	  WRITE(*,*) 'ZM,DGRS,ZOMGRS,ZH,ZOHGRS,RAGRS'
  !	  WRITE(*,*) s_ZM,DGRS,ZOMGRS,s_ZH,ZOHGRS,RAGRS
  !	  WRITE(*,*)
  !	  WRITE(*,*) 'RCGRS,XNUM,FAC3,FAC4,XKK1,ETOGRS1,ETOGRS2,ETOOUT'
  !	  WRITE(*,*) RCGRS,XNUM,FAC3,FAC4,XKK1,ETOGRS1,ETOGRS2,ETOREFC
  !     ENDIF

  RETURN
END FUNCTION ETOREF

