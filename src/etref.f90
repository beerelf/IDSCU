SUBROUTINE ETREF(is,ndyr,minT,maxT,dptT,wd,rs,rh_mean,Ea, &
     s_ZM,s_ZH, s_slat, s_elev, &
     et_method, lai_c, lai_w, alaic, sr, &
     !Outputs
     etr_y,eto_y)

  !***************************************************************************
  !
  !   Function        : etref.f
  !   Author          : HB Manguerra
  !   Date            : December 1994
  !   Purpose         : This calculates the reference evapotranspiration at
  !                     the current weather station for both grass and alfalfa
  !                     based formulations.  The weather data are read from
  !                     the daily weather input file *.wd. 
  !   Calling program : proto.f 
  !   Called programs : etoref.f, etrref.f 
  !   Input arguments : is         = weather station index
  !                     ndyr       = number of days in a year
  !   Output arguments: etr(366)   = reference evapotranspiration alfalfa-based 
  !                     eto(366)   = reference evapotranspiratin grass-based
  !                     minim(366) = minimum temperature
  !                     maxim(366) = maximum temperature
  !                     sr         = stomata resistance
  !                     lai_c      = leaf area index (corn)
  !                     lai_w      = leaf area index (wheat)
  !                     rh_mean    = mean relative humidity
  !   Assumptions     :
  !   Limitations     :
  !   Notes           : The data set has units in english.  The calculations
  !                     are however, done in metric.  The results are 
  !                     transformed back to english units.
  !
  !***************************************************************************

  USE Globals

  !Parameters
  INTEGER ndyr, is, et_method
  REAL etr_y(366), eto_y(366)
  REAL maxT(366), minT(366), dptT(366), lai_c(366), lai_w(366)
  REAL s_ZM, s_ZH, alaic, sr(3)
  REAL s_slat, s_elev ! station latitude and elevation.
  REAL rh_mean(366), Ea(366)
  !Outputs
  REAL wd(366),rs(366)

  INTERFACE
     logical function IsWSUsed(istation)
       integer istation
     end function IsWSUsed

     REAL FUNCTION ETRREF(DOY,APRM,BPRM,TAVG,TMIN,TMAX,EMX,EMN, &
	  EAV,EDPT,WD,RS,SR1,ALAIC,S_ELEV,s_ZM,s_ZH,ET_METHOD)
       !Parameters
       INTEGER DOY, ET_METHOD
       REAL APRM,BPRM,TMIN,TMAX,EMN,EMX,EAV,EDPT,WD,RS
       REAL SR1, ALAIC
       REAL TAVG(0:3)         ! TAVG, TAVG_1, TAVG_2, TAVG_3
       REAL S_ELEV,s_ZM,s_ZH
     END FUNCTION ETRREF

     REAL FUNCTION ETOREF(DOY,APRM,BPRM,TAVG,TMIN,TMAX,EMX,EMN, &
	  EAV,EDPT,WD,RS,S_ELEV,s_ZM,s_ZH,ET_METHOD)
       !Parameters
       INTEGER DOY, ET_METHOD
       REAL APRM,BPRM,TMIN,TMAX,EMN,EMX,EAV,EDPT,WD,RS
       REAL TAVG(0:3)         ! TAVG, TAVG_1, TAVG_2, TAVG_3
       REAL S_ELEV,s_ZM,s_ZH
     END FUNCTION ETOREF

     REAL FUNCTION ETASCE(DOY,APRM,BPRM,TAVG,TMIN,TMAX,dptTd, &
	  EMX,EMN,EAV,EDPT,WD,RS,S_ELEV,s_ZM,s_ZH,s_LAT,ETof)
       !Parameters
       INTEGER DOY, ETof
       REAL APRM,BPRM,TMIN,TMAX,EMN,EMX,EAV,EDPT,WD,RS
       REAL TAVG(0:3)         ! TAVG, TAVG_1, TAVG_2, TAVG_3
       REAL S_ELEV,s_ZM,s_ZH, s_LAT, dptTd
     END FUNCTION ETASCE

     REAL FUNCTION ETCHREF(DOY,APRM,BPRM,TAVG,TMIN,TMAX,EMX,EMN, &
	  EAV,EDPT,WD,RS,S_ELEV,s_ZM,s_ZH,ET_METHOD, LAI_C, SR1)
       !-----Parameters
       INTEGER DOY, ET_METHOD
       REAL APRM,BPRM,TMIN,TMAX,EMN,EMX,EAV,EDPT,WD,RS
       REAL TAVG(0:3)         ! TAVG, TAVG_1, TAVG_2, TAVG_3
       REAL S_ELEV,s_ZM,s_ZH, LAI_C, SR1
     END FUNCTION ETCHREF

  END INTERFACE

  !Local Variable Declaration
  INTEGER I, ETof
  REAL TMIN, TMAX, TDPT           ! min and max in centigrade
  REAL APRM, BPRM, Eamin, Eamax
  REAL EMX, EMN, EAV, EDPT  ! actual vapor perssure
  REAL TAVG(0:3)            ! Average temp over the last four days

  !Don't bother calculating if this weather station isn't used.
  if (.not. IsWSUSed(is)) then
     return
  endif

  APRM = 31.54 - .273 * s_slat + .00078 * s_elev   ! eq. 6.66 p. 135
  BPRM = -.30 + .268 * s_slat + .00041 * s_elev    ! eq. 6.66 p. 135
  Sr_tot = 1

  ! errata
  DO I = 1, NDYR
     ETO_Y(I) = 0
     ETR_Y(I) = 0
     ETC_Y(I) = 0
     ETW_Y(I) = 0

     if (maxT(I) .GT. -999 .AND. minT(I) .GT. -999) then
	TMAX  = .5556 * (maxT(I) - 32) ! farenheit to centigrade
	TMIN  = .5556 * (minT(I) - 32)

	! Elgaali
	Sr_tot = Sr_tot + Sr_Inc

	!Calculate Average Temperature and Saturation V. Pressure
	IF (I .EQ. 1) THEN
	   TAVG(0) = (TMAX+TMIN) * 0.50
	   TAVG(1) = TAVG(0)
	   TAVG(2) = TAVG(0)
	   TAVG(3) = TAVG(0)
	ELSE
	   TAVG(3) = TAVG(2)
	   TAVG(2) = TAVG(1)
	   TAVG(1) = TAVG(0)
	   TAVG(0) = (TMAX+TMIN) * 0.50
	ENDIF

	EMX = EXP((16.78*TMAX-116.9)/(TMAX+237.3))
	EMN = EXP((16.78*TMIN-116.9)/(TMIN+237.3))
        ! Check if actual vapor pressure is provided
        if (Ea(i) .GE. 0) THEN
           EAV = Ea(i)
        else
           ! Note that ASCE calculates its own value
           EAV = EXP((16.78*TAVG(0)-116.9)/(TAVG(0)+237.3))
        end if

	Eamin = 0.6108 * EXP((17.27 * TMIN)/(TMIN + 237.3))  ! kPa
	Eamax = 0.6108 * EXP((17.27 * TMAX)/(TMAX + 237.3))

	IF (dptT(I) .lt. -100) THEN
	   ! Use relative humidity to calculate vapor pressure	
	   TDPT  = rh_mean(i)
	   EDPT = (TDPT/100.0) * ((Eamin + Eamax)/2.0) ! eq 14 p. 15 ASCE New Equation
	ELSE
	   ! Use dew point to calculate vapor pressure
	   TDPT  = .5556 * (dptT(I) - 32)
	   ! Patterson: I don't know what the origin of this statement is:
	   !         EDPT = 0.1 * EDPT      ! mbars to kPas 
	   EDPT = EXP((16.78*TDPT-116.9)/(TDPT+237.3))   ! equation 7.11 manual 70
	END IF

	!Calculate Penman Reference ET OR 1982 Kimberly Penman Reference ET
	IF (et_method .EQ. 2 .OR. et_method .EQ. 4 .or. et_method .EQ. 10) THEN 
	   ETO_Y(I) = ETOREF(I,APRM,BPRM,TAVG,TMIN,TMAX,EMN,EMX,EAV, &
		EDPT, WD(I),RS(I),s_elev,s_ZM,s_ZH, ET_METHOD)

	   ETR_Y(I) = ETRREF(I,APRM,BPRM,TAVG,TMIN,TMAX,EMN,EMX,EAV, &
		EDPT, WD(I),RS(I),SR(1),ALAIC,s_elev,s_ZM,s_ZH, ET_METHOD)
	ENDIF
	IF( et_method .EQ. 6 .or. et_method .eq. 9) THEN
	   !Calculate GRASS reference ET (1 as last argument)
	   ETof=1
           if (Ea(i) .GE. 0) THEN
              EAV = Ea(i)
           else
              ! Note that ASCE does not use this value, so set to nodata
              !   to force ETASCE to calculate Ea.
              EAV = -999
           end if
	   ETO_Y(I) = ETASCE(I,APRM,BPRM,TAVG,TMIN,TMAX,TDPT,EMN, &
		EMX,EAV,EDPT,WD(I),RS(I),s_elev,s_ZM,s_ZH, s_slat,ETof)
	   !Calculate Alfalfa reference ET (0 as last argument)
	   ETof=0
	   ETR_Y(I) = ETASCE(I,APRM,BPRM,TAVG,TMIN,TMAX,TDPT,EMN, &
		EMX,EAV,EDPT,WD(I),RS(I),s_elev,s_ZM,s_ZH, s_slat,ETof)
	ENDIF

	IF (useCropMode) THEN
	   ETC_Y(I) = ETCHREF(I,APRM,BPRM,TAVG,TMIN,TMAX,EMN,EMX,EAV, &
		EDPT, WD(I),RS(I),s_elev,s_ZM,s_ZH, 1, lai_c(I), SR(2))

	   ETW_Y(I) = ETCHREF(I,APRM,BPRM,TAVG,TMIN,TMAX,EMN,EMX,EAV, &
		EDPT, WD(I),RS(I),s_elev,s_ZM,s_ZH, 2, lai_w(I), SR(3))
	END IF
     ENDIF

     !Patterson: remove negatives
     if (ETO_Y(I) .lt. 0) then
	ETO_Y(I) = 0
     end if
     if (ETR_Y(I) .lt. 0) then
	ETR_Y(I) = 0
     end if

  END DO

  RETURN
END SUBROUTINE ETREF
