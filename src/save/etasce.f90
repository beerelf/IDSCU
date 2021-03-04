REAL FUNCTION ETASCE(DOY,APRM,BPRM,TAVG,TMIN,TMAX,dptT, &
     EMX,EMN,EAV,EDPT,WD,Rs,s_ELEV,s_Zw,s_Zp,s_LAT,ETof)

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
  !                     dptT - dew point temp for DOY
  !                     EMX,EMN,EAV,EDPT - ?
  !                     WD,RS - wind and solar radiation for DOY
  !                     s_ELEV - weather station elevation
  !                     s_Zw - height of wind speed measurement for DOY
  !                     s_Zp - height of humidity (psychrometer) and temp. measurements
  !                     s_lat - latitude of the station
  !                     ETof - flag to determine if grass (ETof = 1) or alfalfa (ETof = 0) 
  !                           reference crop values will be computed
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
  INTEGER DOY, ETof
  REAL APRM,BPRM,TMIN,TMAX,EMN,EMX,EAV,EDPT,WD,Rs
  REAL TAVG(0:3) ! TAVG, TAVG_1, TAVG_2, TAVG_3
  REAL s_ELEV,s_Zw,s_Zp,dptT,s_LAT

  !Local Variable Declaration
  INTEGER MON, DAY, N, M
  REAL DELTA, XLTHT, PC, PRESS, ALBEDO, SD
  REAL XA1, A1, RBO, Rso, RATIO, A, B 
  REAL RB, UZ, LAT_rad
  REAL RN, FAC1, FAC2, FAC3, CD, CN
  REAL ETOALF1, ETOALF2, Gday, rs_rso
  REAL(8) Rnl
  REAL Es_min, Es_max, Es, Ea, U_2, DR, SS_ang, Ra, Rns

  !Check for empty weather data.
  if (s_Zw .eq. 0) then
     ETASCE = 0
     return
  end if

  CALL CLNDR(DOY,m_year,MON,DAY)

  PRESS = 101.3 * ((293-0.0065 * s_ELEV)/293)**5.26     ! eq. 3 p. 9
  PC = 0.000665 * PRESS                                 ! eq. 4 p. 9
  DELTA = (2504.0*EXP((17.27*TAVG(0))/(TAVG(0)+237.3)))/ &
       ((TAVG(0) + 237.3)**2)                        ! eq. 5 p. 9

  Es_min = 0.6108* EXP((17.27*TMIN)/(TMIN+237.3))       ! eq. 7 p. 10
  Es_max = 0.6108* EXP((17.27*TMAX)/(TMAX+237.3))       ! eq. 7 p. 10

  Es = (Es_min + Es_max)/2.0                            ! eq. 6 p. 10

  Ea = 0.6108 * EXP((17.27 * dptT)/(dptT + 237.3))      ! eq. 8 p. 12

  Uz = WD/86.4

  U_2 = Uz * ((4.87)/(ALOG(67.8*s_Zw - 5.42)))          ! eq. 33 p. 25

  Dr = 1 + 0.033 * COS(((2.*PI)/365.)*DOY)              ! eq. 23 p. 22

  SD = 0.409 * SIN((((2.*PI)/365.)*DOY)-1.39)           ! eq. 24 p. 22

  LAT_rad = (PI/180.0) * s_LAT                          ! eq. 22 p. 22   

  SS_ang = ACOS(-TAN(LAT_rad)*TAN(SD))

  Ra = (24.0/PI) * 4.92 * Dr * ((SS_ang * SIN(LAT_rad) * SIN(SD)) + &
       (COS(LAT_rad) * COS(SD) * SIN(SS_ang)))          ! eq. 21 p. 22

  Rso = ((0.75 + (2.0E-05 * s_ELEV)) * Ra)              ! eq. 46 p. 34

  rs_rso = Rs/Rso
  if (rs_rso .gt. 1 ) then
	rs_rso = 1.0
  elseif (rs_rso .lt. .3) then
	rs_rso = .3
  end if


  Rnl = 4.901E-09 * (((TMAX+273.16)**4. + (TMIN+273.16)**4.)/2.) &
       * (0.34 - 0.14 * (Ea ** 0.5)) * (1.35 * rs_rso - 0.35)
  ! eq. 18 p. 18

  Rns = (1.0 - 0.23) * Rs                               ! eq. 16 p. 17

  Rn = Rns - Rnl                                        ! eq. 15 p. 16

  IF (Etof .EQ. 1) THEN
     Cn = 900.0
     Cd = 0.34
  ELSE
     Cn = 1600.0
     Cd = 0.38
  ENDIF

  Gday = 0.0

  FAC1 = PC * (Cn / (TAVG(0)+273.0) )
  FAC2 = U_2 * (Es - Ea)
  FAC3 = DELTA + PC * (1.0 + Cd * U_2)
  ETOALF1 = 0.408 * DELTA * (Rn-Gday)
  ETOALF2 = FAC1 * FAC2
  ETASCE = ((ETOALF1 + ETOALF2) / FAC3) / 25.4

  !      IF (DOY .EQ. 184) THEN
  !	  IF(DOY .EQ. 184 .AND. ETof .EQ. 0 ) THEN
  !	     OPEN (UNIT=91,FILE='ASCE_o.txt',STATUS='NEW',IOSTAT=IERR)
  !       ELSE
  !	     OPEN (UNIT=91,FILE='ASCE_f.txt',STATUS='NEW',IOSTAT=IERR)
  !        ENDIF
  !
  !	 WRITE(91,*) 'EAV,TAVG,DELTA,ELEV,PC,RSO,APRM,BPRM,RS,RN,UZ,EMX,EMN
  !     1,EDPT,ZM,DGRS,ZOMGRS,ZH,ZOHGRS,DOY,FAC1,FAC2,FAC3,ETASCE'
  !       WRITE(91,100) EAV,TAVG(0),DELTA,s_ELEV,PC,RSO,APRM,BPRM,RS,RN,UZ,
  !     1  EMX,EMN,EDPT,s_Zw,DGRS,ZOMGRS,s_Zp,ZOHGRS,DOY,FAC1,FAC2,FAC3,
  !     2  ETASCE*25.4
  ! 100   FORMAT(24(F8.2,1x))
  !      
  !      WRITE(*,*) TAVG(1),TAVG(2),TAVG(3)
  !	 WRITE(*,*)
  !	 WRITE(*,*) 'RSO,APRM,BPRM,RS'
  !	 WRITE(*,*) RSO,APRM,BPRM,RS
  !	 WRITE(*,*)
  !	 WRITE(*,*) 'RN,UZ,EMX,EMN,EDPT,'
  !	 WRITE(*,*) RN,UZ,EMX,EMN,EDPT
  !	 WRITE(*,*)
  !	 WRITE(*,*) 'ZM,DGRS,ZOMGRS,ZH,ZOHGRS'
  !	 WRITE(*,*) s_Zw,DGRS,ZOMGRS,s_Zp,ZOHGRS
  !	 WRITE(*,*)
  !	 WRITE(*,*) 'DOY   FAC1   FAC2   FAC3  ETASCE'
  !	 WRITE(*,*) DOY,FAC1,FAC2,FAC3,ETASCE*25.4
  !
  !	  CLOSE(91)
  !
  !       ENDIF

  RETURN
END FUNCTION ETASCE

