SUBROUTINE READ_WEATHER(et_method)

  !***************************************************************************
  !
  !   Function        : read_weather.f
  !   Author          : HB Manguerra
  !   Date            : May 1995 
  !   Purpose         : This calculates the monthly mean temperature  and
  !                     rainfall representative of the given area by 
  !                     weighted average of sorrounding weather stations. 
  !                     This also reads the
  !                     published 28 and 32 degree F frost date for all years 
  !                     of every weather station and weigh them to get the
  !                     representative frost date of the given area. 
  !   Calling program : mainxc.f 
  !   Called programs : none 
  !   Input arguments : et_method 
  !                   : et_method = 1 - Means using Blaney Criddle
  !                   :        4 - Means using Kimberly Penman
  !                   :        5 - Means using Calibrated Blaney Criddle 
  !   Output arguments: none
  !   Assumptions     :
  !   Limitations     :
  !   Notes           : The weather parameters to be weighted are read from
  !                     *.wm input file.
  !
  !***************************************************************************

  USE Globals

  !-----Parameters
  INTEGER et_method

  INTERFACE
     logical function IsWSUsed(istation)
       integer istation
     end function IsWSUsed

     INTEGER FUNCTION JULIAN (m_mon, m_day, yr)
       INTEGER m_mon, m_day, yr
     END FUNCTION JULIAN

     INTEGER FUNCTION DaysInYear(yr)
       INTEGER yr
     END FUNCTION DaysInYear

     SUBROUTINE ETREF(is,ndyr,minT,maxT,dpt,wd,rs,rh_mean,Ea,s_ZM,s_ZH, &
	  s_slat,s_elev, et_method, lai_c,lai_w, alaic, sr, &
				!     Outputs
	  etr_y,eto_y)
       INTEGER ndyr, is, et_method
       REAL etr_y(366), eto_y(366)
       REAL maxT(366), minT(366), dpt(366), lai_c(366),lai_w(366)
       REAL s_ZM, s_ZH, RS(366), RH_MEAN(366), Ea(366)
       REAL s_slat, s_elev    ! station latitude and elevation.
       REAL wd(366),sr(3), ALAIC
     END SUBROUTINE ETREF

     REAL FUNCTION parse_num(instr)
       CHARACTER*120 instr
     END FUNCTION parse_num

     SUBROUTINE ReadET(et)
       REAL et(:,:,:)
     END SUBROUTINE ReadET
  END INTERFACE

  !Local variable declaration
  INTEGER i, j, iy, ib, iper, nperiods
  INTEGER yr1, yr2, IYEAR, NBAS
  INTEGER MON,DAY,T1,T2,T3,T4,T5,T6,T7,T8,T28_1,T28_2,T32_1,T32_2
  INTEGER IERR, IERR1
  REAL X1(366),X2(366),X3(366),X4(366),X5(366),X6(366),X7(366),X8(366)
  REAL dummy, SWWS
  CHARACTER*80 REMARK
  CHARACTER*320 LINE1
  CHARACTER*120 dfile1
  LOGICAL isMonthly ! Indicates how weather data should be parsed.
  INTEGER yr_offset
  LOGICAL needWarning

  !Local Penmans
  REAL maxT(366), minT(366), DPT(366), RS(366), WD(366)
  REAL RH_MEAN(366), Ea(366)
  REAL eto_y(366), etr_y(366) ! year's worth of ET
  REAL LAI_C(366), LAI_W(366), SR(3), ALAIC

  CHARACTER*10 substr
  INTEGER s
  REAL f

  REAL FT_M
  PARAMETER (FT_M = 0.3047999902464)

  !specify input file extension
  dfile1 = dfile

  isMonthly = .true.

  IF(et_method.EQ.1.OR.et_method.EQ.5) THEN
     dfile1(flen:flen+4) = '.wm' ! B-C: Use monthly weather
  ELSEIF(et_method.EQ.2 .OR. et_method.EQ.4 &
       .OR. et_method.EQ.6 .OR. et_method.EQ.10 &
       .OR. et_method.EQ.9) THEN
     isMonthly = .false.
     dfile1(flen:flen+4) = '.wd' ! ASCE and User ET: Use daily weather
  ELSEIF(et_method.EQ.7 .OR. et_method.EQ.8) THEN
     dfile1(flen:flen+4) = '.wm' ! HG or Pochop: Use monthly weather
  ENDIF

  !-----open and read input file

  OPEN(UNIT=18,FILE=dfile1(1:flen+3),STATUS='OLD',IOSTAT=IERR)
  OPEN(UNIT=58,FILE='debug1',STATUS='UNKNOWN',IOSTAT=IERR1)
  IF (IERR1 .NE. 0) CALL MYEXIT(91)
  IF (IERR.NE.0 .AND. et_method.EQ.1) CALL MYEXIT(2)
  IF (IERR.NE.0 .AND. et_method.EQ.5) CALL MYEXIT(2)
  IF (IERR.NE.0 .AND. et_method.EQ.4) CALL MYEXIT(90)

  CALL SKIPN(18)
  READ(18,*,ERR=101) nbas,N_STA,yr1,yr2

  IF ((YR1.NE.GNYR1).OR.(YR2.NE.GNYR2)) CALL MYEXIT(99)
  IF (NBAS.NE.NBASIN) CALL MYEXIT(99)

  IF (isMonthly) THEN
     nperiods = 12
  ELSE
     nperiods = 366
  ENDIF

  call AllocateWeatherVars(isMonthly, nperiods, et_method .eq. 7)

  DO IB = 1, NBAS
     DO I = 1, N_STA
	WWS(IB, I) = 0
	WRS(IB, I) = 0
     END DO
  END DO

  READ(18,900,ERR=102) REMARK
  !Read matrix of weights for non-precipitation parameters
  DO IB = 1, NBAS
     READ(18,41,ERR=103) LINE1
41   FORMAT(A320)               
     SWWS = 0.0
     ! Skip basin name
     LINE1 = LINE1(28:320)
     do while (LEN(LINE1) .gt. 0)
	! Weather station is first token
	I = index(LINE1, ":")
	if (I .eq. 0) exit
	substr = LINE1(1:I-1)
	s = parse_num(substr)
 ! Skip : and read weight
	LINE1 = LINE1(I+1:LEN(LINE1))
	f = parse_num(LINE1)
	WWS(IB, s) = f
	SWWS = SWWS + WWS(IB,s)
     END do
     IF(N_STA .GT. 0 .AND. (SWWS .LT. 0.999 .OR. SWWS .GT. 1.001)) &
	  CALL MYEXIT(128)
  END DO

  READ(18,900,ERR=105) REMARK
  !Read matrix of weights for precipitation parameters
  DO IB = 1, NBAS
     READ(18,41,ERR=106) LINE1   
     ! Skip basin name
     LINE1 = LINE1(28:320)
     do while (LEN(LINE1) .gt. 0)
	! Weather station is first token
	I = index(LINE1, ":")
	if (I .eq. 0) exit
	substr = LINE1(1:I-1)
	s = parse_num(substr)
 ! Skip : and read weight
	LINE1 = LINE1(I+1:LEN(LINE1))
	f = parse_num(LINE1)
	WRS(IB, s) = f
     END do
     IF(N_STA .GT. 0 .AND. (SWWS .LT. 0.999 .OR. SWWS .GT. 1.001)) &
	  CALL MYEXIT(128)
  END DO

  !Initialize variables to zero
  DO ib = 1,nbas
     DO i = 1,nyrs
	t28(ib,i,1) = 0
	t28(ib,i,2) = 0
	t32(ib,i,1) = 0
	t32(ib,i,2) = 0
	DO j = 1,nperiods
	   tmean3(ib,i,j) = 0.0
	   tmax3(ib,i,j) = 0.0
	   tmin3(ib,i,j) = 0.0
	   rntot3(ib,i,j) = 0.0
	   IF (et_method.EQ.7) THEN
	      trng_c(ib,i,j) = 0.0
	   ENDIF
	   IF (.not. isMonthly) THEN
	      etr(ib,i,j) = 0.0
	      eto(ib,i,j) = 0.0
	      etc(ib,i,j) = 0.0
	      etw(ib,i,j) = 0.0
	   ENDIF
	END DO
     END DO
  END DO

  ! Create output file for ET values.
  dfile1 = dfile
  dfile1(flen:flen+6) = '.refet'
  IF (et_method .NE. 6) THEN ! Don't do ASCE
     OPEN(UNIT=29,FILE=dfile1,STATUS='UNKNOWN',IOSTAT=IERR)
  END IF

  DO i = 1,N_STA
     READ(18,900,ERR=108) STNAME(i)
     READ(18,900,ERR=109) REMARK
     IF (.not. isMonthly) THEN
	! Read daily parameters
	READ(18,*) SLAT(i), SLONG(i), ELEV(i), ZH(i), ZM(i)
 !READ(18,*) SLAT(i), SLONG(i), ELEV(i), ZM(i), ZH(i)

 ! Convert feet to meters
	ELEV(i) = ELEV(i) * FT_M
	ZH(i) = ZH(i) * FT_M
	ZM(i) = ZM(i) * FT_M

	if (ZH(i) .le. 0)  then
	   if (IsWSUsed(i) .and. et_method .NE. 9) then ! 9 is user-supplied
	      print *, "Weather station ", STNAME(i), &
		   " is missing height of temperature measurements."
	      call myexit(100)
	   endif
	endif
	if (ZM(i) .le. 0)  then
	   if (IsWSUsed(i) .and. et_method .NE. 9) then ! 9 is user-supplied
	      print *, "Weather station ", STNAME(i), &
		   " is missing height of wind measurements."
	      call myexit(100)
	   endif
	endif
     ELSE
	READ(18,*,ERR=110) dummy,dummy
     ENDIF

     ! Skip weather station URL
     call skipln(18, 1)

     DO iy = 1,gnyrs
	m_year = iy + GNYR1 - 1
	READ(18,*,ERR=111) IYEAR,T1,T2,T3,T4,T5,T6,T7,T8

	T28_1 = JULIAN(T1,T2,m_year)
	T28_2 = JULIAN(T3,T4,m_year)
	T32_1 = JULIAN(T5,T6,m_year)
	T32_2 = JULIAN(T7,T8,m_year)

	IF (isMonthly) THEN
	   nperiods = 12
	ELSE
	   nperiods = DaysInYear(IYEAR)
	ENDIF

	READ(18,900,ERR=112) REMARK

 ! Read Elgaali's stuff
	useCropMode = .true.
	if (et_method .ne. 2) then
	   useCropMode = .false.
	else 
	   dfile1(flen:flen+5) = '.sr' ! Leaf Area Index for Corn & Wheat
	   OPEN(UNIT=28,FILE='crop.sr',STATUS='OLD',IOSTAT=IERR)
	   IF (IERR.NE.0) THEN
	      useCropMode = .false.
	   ELSE
       ! Sr(1) = Alfalfa, Sr(2) = Corn, Sr(3) = Wheat
	      READ(28,*,ERR=163) Sr(1), Sr(2), Sr(3), Sr_Inc
	      CLOSE(28)
	   END IF

	   dfile1(flen:flen+5) = '.alf' ! Leaf Area Index for Corn & Wheat
	   OPEN(UNIT=28,FILE='crop.alf',STATUS='OLD',IOSTAT=IERR)
	   IF (IERR.NE.0) THEN
	      useCropMode = .false.
	   ELSE
       ! Alfalfa Leaf Area Index Variable to change value
	      READ(28,*,ERR=165) ALAIC
	      CLOSE(28)
	   END IF

	   dfile1(flen:flen+5) = '.lai' ! Leaf Area Index for Corn & Wheat
	   OPEN(UNIT=28,FILE='crop.lai',STATUS='OLD',IOSTAT=IERR)
	   IF (IERR.NE.0) useCropMode = .false.
	end if

 !--------------Read the temperature and rainfall data
	DO iper=1,nperiods
    !-------------------------------------------------------------
    !Read all weather data regardless of whether it's needed or not.
    !-------------------------------------------------------------
	   IF (isMonthly) THEN
	      READ(18,*,ERR=113) MON,X1(iper),X2(iper),X3(iper),X4(iper)

	   ELSE
	      READ(18,*,ERR=114) DAY, X1(iper),X2(iper), &
		   X3(iper),X4(iper),X5(iper),X6(iper),X7(iper),X8(iper)

       ! Also read in Elgaali's stuff here.
	      IF (useCropMode) THEN
		 READ(28,*,ERR=160) dummy, LAI_C(iper), LAI_W(iper)
	      END IF
	   ENDIF

    !Check if this year is skipped.
	   if (iyear .lt. NYR1 .or. iyear .gt. NYR2) cycle

	   yr_offset = NYR1 - GNYR1

    !Check if value is less than -90 this means that the value was
    !  missing and we set it to zero for modeling purposes.

    !----------------------------
    !Penman-Monteith or ASCE Data
    !----------------------------
	   IF (et_method.EQ.2 .or. et_method.eq.4 .or.  &
		et_method .EQ. 6 .or. et_method.eq.10 .or. et_method .EQ. 9) THEN
	      maxT(IPER)  = X1(IPER) ! farenheit to centigrade
	      minT(IPER)  = X2(IPER)
	      DPT(IPER)  = X3(IPER) ! X3 = Dew Pt
	      RS(IPER) = X4(IPER) / 23.892 ! langley to MJ / m2
	      WD(IPER) = X5(IPER) * 1.609 ! mi/day to km/day
	      RH_MEAN(IPER) = X7(IPER)
	      Ea(IPER) = X8(IPER)
	   ENDIF

	END DO

	IF (useCropMode) CLOSE(28)

	if (iyear .lt. NYR1 .or. iyear .gt. NYR2) cycle

	yr_offset = NYR1 - GNYR1

 !----------------------------
 !Calculate Penman Monteith ET
 !----------------------------
	IF (et_method .EQ. 2) THEN
	   IF (IsWSUSed(i)) THEN
	      WRITE(29, 9000) STNAME(I), iyear
9000	      FORMAT("Computing PM ET reference for ",A10," year ",I4)
	      CALL ETREF(i,nperiods, &
		   minT,maxT,dpt,wd,rs,rh_mean,Ea,ZM(i),ZH(i),slat(i),elev(i), &
		   et_method, lai_c,lai_w, alaic, sr, etr_y,eto_y)
	   END IF
	END IF
 !----------------------------
 !Calculate Penman 1948 ET
 !----------------------------
	IF (et_method .EQ. 10) THEN
	   IF (IsWSUSed(i)) THEN
	      WRITE(29, 9000) STNAME(I), iyear
	      CALL ETREF(i,nperiods, &
		   minT,maxT,dpt,wd,rs,rh_mean,Ea,ZM(i),ZH(i),slat(i),elev(i), &
		   et_method, lai_c,lai_w, alaic, sr, etr_y,eto_y)
	   END IF
	END IF
 !----------------------------
 !Calculate Kimberly Penman ET
 !----------------------------
	IF (et_method .EQ. 4) THEN
	   IF (IsWSUSed(i)) THEN
	      WRITE(29, 9000) STNAME(I), iyear
	      CALL ETREF(i,nperiods, &
		   minT,maxT,dpt,wd,rs,rh_mean,Ea,ZM(i),ZH(i),slat(i),elev(i), &
		   et_method, lai_c,lai_w, alaic, sr, etr_y,eto_y)
	   END IF
	END IF
 !-----------------
 !Calculate ASCE ET
 !-----------------
	IF (et_method .EQ. 6 .or. et_method .EQ. 9) THEN ! Patterson: inserted so that ASCE is used to calculate potential ET
	   IF (IsWSUSed(i)) THEN
	      CALL ETREF(i,nperiods, &
		   minT,maxT,dpt,wd,rs,rh_mean,Ea,ZM(i),ZH(i),slat(i),elev(i), &
		   et_method, lai_c,lai_w, alaic, sr, etr_y,eto_y)
	   END IF
	END IF

	needWarning = .true.

	DO ib = 1,nbas
	   DO iper=1,nperiods
	      if (needWarning) then
	  !If the temperature is zero and this station is being
	  !  used then write an error message.
		 IF ((.not. IsMonthly) .and. WWS(IB,I).GT. 0.0) THEN
		    IF (X1(iper) .EQ. -999) THEN
		       WRITE(*,*) 'Weather station ', STNAME(i), &
			    ', Year ',IY+YR1-1, ', Julian day ', IPER, &
			    ' has uninitialized max temperature.'
		       needWarning = .false.
		    ENDIF
		    IF (X2(iper) .EQ. -999) THEN
		       WRITE(*,*) 'Weather station ', STNAME(i), &
			    ', Year ',IY+YR1-1, ', Julian day ', IPER, &
			    ' has uninitialized min temperature.'
		       needWarning = .false.
		    ENDIF
                    ! If weather data is user-supplied, then don't warn about missing dew temp, solar, or wind.
		    IF (X3(iper) .EQ. -999 .AND. X6(iper) .EQ. -999 .AND. et_method .NE. 9) THEN
		       WRITE(*,*) 'Weather station ', STNAME(i), &
			    ', Year ',IY+YR1-1, ', Julian day ', IPER, &
			    ' has uninitialized dew temperature.'
		       needWarning = .false.
		    ENDIF
		    IF (X4(iper) .EQ. -999 .AND. et_method .NE. 9) THEN
		       WRITE(*,*) 'Weather station ', STNAME(i), &
			    ', Year ',IY+YR1-1, ', Julian day ', IPER, &
			    ' has uninitialized solar radiation.'
		       needWarning = .false.
		    ENDIF
		    IF (X5(iper) .EQ. -999 .AND. et_method .NE. 9) THEN
		       WRITE(*,*) 'Weather station ', STNAME(i), &
			    ', Year ',IY+YR1-1, ', Julian day ', IPER, &
			    ' has uninitialized wind speed.'
		       needWarning = .false.
		    ENDIF
		 ENDIF

		 IF (isMonthly .and. WWS(IB,I).GT. 0.0) THEN

      !If the temperature is zero and this station is being
      !  used then write an error message.
		    IF (X1(iper) .LT. -90.0) THEN
		       WRITE(*,121) 'Weather station ', STNAME(i), &
			    ', Year ',IY+YR1-1, ', Month ', IPER, &
			    ' has uninitialized mean temperature.'
		       needWarning = .false.
120		       FORMAT(A9, A15, A14)
121		       FORMAT(A16,A15,A7,I4,A8,I4,A41)
		    ENDIF
		    IF (et_method.EQ.7) THEN !85 Hargreaves compute temperature range
		       IF (X2(iper) .LT. -90.0) THEN
			  WRITE(*,121) 'Weather station ', STNAME(i), &
			       ', Year ',IY+YR1-1, ', Month ', IPER, &
			       ' has uninitialized mean max. temperature.'
			  needWarning = .false.
		       ENDIF
		       IF (X3(iper) .LT. -90.0) THEN
			  WRITE(*,121) 'Weather station ', STNAME(i), &
			       ', Year ',IY+YR1-1, ', Month ', IPER, &
			       ' has uninitialized mean min. temperature.'
			  needWarning = .false.
		       ENDIF
		    ENDIF
		 ENDIF
	      ENDIF
	   END DO
	END DO

 ! Convert illegal values to zero.
	DO ib = 1,nbas
	   DO iper=1,nperiods
	      IF ((.not. IsMonthly) .and. WWS(IB,I).GT. 0.0) THEN
		 IF (X1(iper) .EQ. -999) THEN
		    X1(iper) = 0
		 ENDIF
		 IF (X2(iper) .EQ. -999) THEN
		    X2(iper) = 0
		 ENDIF
		 IF (X3(iper) .EQ. -999 .AND. X6(iper) .EQ. -999) THEN
		    X3(iper) = 0
		 ENDIF
		 IF (X4(iper) .EQ. -999) THEN
		    X4(iper) = 0
		 ENDIF
		 IF (X5(iper) .EQ. -999) THEN
		    X5(iper) = 0
		 ENDIF
	      ENDIF

	      IF (isMonthly) THEN
	  !---------------------
	  !monthly weather data.
	  !---------------------
		 tmean3(ib,iy-yr_offset,iper) = &
		      tmean3(ib,iy-yr_offset,iper) + wws(ib,i) &
		      * X1(iper)
		 rntot3(ib,iy-yr_offset,iper) = &
		      rntot3(ib,iy-yr_offset,iper) + wrs(ib,i) &
		      * X4(iper)
		 IF (et_method.EQ.7) THEN !85 Hargreaves compute temperature range
		    trng_c(ib,iy-yr_offset,iper) = &
			 trng_c(ib,iy-yr_offset,iper)+wws(ib,i)* &
			 ((.5556*(X2(iper)-32.))-(.5556*(X3(iper)-32.)))
		 ENDIF
	      ELSE
	  !----------------------- 
	  !Penman-Monteith or ASCE
	  !-----------------------
		 tmean3(ib,iy-yr_offset,iper) =  &
		      tmean3(ib,iy-yr_offset,iper) + wws(ib,i) &
		      * ((X1(iper) + X2(iper)) / 2.0)
		 tmax3(ib,iy-yr_offset,iper) =  &
		      tmax3(ib,iy-yr_offset,iper) + wws(ib,i) &
		      * X1(iper)
		 tmin3(ib,iy-yr_offset,iper) =  &
		      tmin3(ib,iy-yr_offset,iper) + wws(ib,i) &
		      * X2(iper)
		 rntot3(ib,iy-yr_offset,iper) = &
		      rntot3(ib,iy-yr_offset,iper) + wrs(ib,i) &
		      * X6(iper)
		 etr(ib,iy-yr_offset,iper) = &
		      etr(ib,iy-yr_offset,iper) + wws(ib,i) &
		      * etr_y(iper)
		 eto(ib,iy-yr_offset,iper) = &
		      eto(ib,iy-yr_offset,iper) + wws(ib,i) &
		      * eto_y(iper)
		 etc(ib,iy-yr_offset,iper) = &
		      etc(ib,iy-yr_offset,iper) + wws(ib,i) &
		      * etc_y(iper)
		 etw(ib,iy-yr_offset,iper) = &
		      etw(ib,iy-yr_offset,iper) + wws(ib,i) &
		      * etw_y(iper)
	      ENDIF

       !If the weighted value of rainfall is less than zero it is
       !  corrected to zero.  

	      IF(rntot3(ib,iy-yr_offset,iper) .LT. 0.0) &
		   rntot3(ib,iy-yr_offset,iper) = 0.0

	   END DO

	END DO

	DO ib = 1,nbas

    !If the user did not give any 28 or 32 degree frost dates
    !  then set them to the first day of the year and the last day
    !  of the year.

    !IF (T28_1 .EQ. 0 .AND. WWS(IB,I) .GT. 0) THEN
    !   CALL MYEXIT(74)
    !ENDIF
    !IF (T32_1 .EQ. 0 .AND. WWS(IB,I) .GT. 0) THEN
    !   CALL MYEXIT(74)
    !ENDIF
    ! Removed fall frost date check
    !IF (T28_2 .EQ. 0 .AND. WWS(IB,I) .GT. 0) THEN
    !   CALL MYEXIT(74)
    !ENDIF
    !IF (T32_2 .EQ. 0 .AND. WWS(IB,I) .GT. 0) THEN
    !   CALL MYEXIT(74)
    !ENDIF

	   T28(ib,IY-yr_offset,1) = T28(ib,IY-yr_offset,1) &
		+ WWS(IB,I) * T28_1
	   T28(ib,IY-yr_offset,2) = T28(ib,IY-yr_offset,2) &
		+ WWS(IB,I) * T28_2
	   T32(ib,IY-yr_offset,1) = T32(ib,IY-yr_offset,1) &
		+ WWS(IB,I) * T32_1
	   T32(ib,IY-yr_offset,2) = T32(ib,IY-yr_offset,2) &
		+ WWS(IB,I) * T32_2

	END DO

     END DO

  END DO

  IF (et_method .NE. 6) THEN
     CLOSE(29)
  END IF

  CLOSE(18)

  ! Check for user-supplied reference ET.  This will overwrite the ETr values.
  if (et_method .EQ. 9) then
     if (USER_ET_MODE .EQ. 1) then
	! Fill ETr and ETo values
        call ReadET(etr)
        call ReadET(eto)
     END IF
  END IF

900 FORMAT (A80)

  RETURN

101 CALL MYEXIT(9) 
102 CALL MYEXIT(9) 
103 CALL MYEXIT(9) 
104 CALL MYEXIT(9) 
105 CALL MYEXIT(9) 
106 CALL MYEXIT(9) 
107 CALL MYEXIT(9) 
108 CALL MYEXIT(9) 
109 CALL MYEXIT(9) 
110 CALL MYEXIT(9) 
111 CALL MYEXIT(9) 
112 CALL MYEXIT(9) 
113 CALL MYEXIT(9) 
114 CALL MYEXIT(9) 
160 CALL MYEXIT(160)
163 CALL MYEXIT(163)
165 CALL MYEXIT(165)

END SUBROUTINE READ_WEATHER

SUBROUTINE AllocateWeatherVars(monthly, nperiods, needHarg)
  USE GLOBALS

  !-----Parameters
  LOGICAL monthly, needHarg
  INTEGER nperiods

  if (ALLOCATED(TMEAN3)) DEALLOCATE(TMEAN3)
  if (ALLOCATED(TMAX3)) DEALLOCATE(TMAX3)
  if (ALLOCATED(TMIN3)) DEALLOCATE(TMIN3)
  if (ALLOCATED(RNTOT3)) DEALLOCATE(RNTOT3)
  if (ALLOCATED(ETR)) DEALLOCATE(ETR)
  if (ALLOCATED(ETO)) DEALLOCATE(ETO)
  if (ALLOCATED(ETC)) DEALLOCATE(ETC)
  if (ALLOCATED(ETW)) DEALLOCATE(ETW)
  if (ALLOCATED(TRNG_C)) DEALLOCATE(TRNG_C)

  ALLOCATE(TMEAN3(NBASIN, NYRS, nperiods))
  ALLOCATE(TMAX3(NBASIN, NYRS, nperiods))
  ALLOCATE(TMIN3(NBASIN, NYRS, nperiods))
  ALLOCATE(RNTOT3(NBASIN, NYRS, nperiods))
  !If daily method allocate (ET_METHOD = 2, 4, 6, 10)
  IF (.not. monthly) then
     ALLOCATE(ETR(NBASIN, NYRS, nperiods))
     ALLOCATE(ETO(NBASIN, NYRS, nperiods))
     ALLOCATE(ETC(NBASIN, NYRS, nperiods))
     ALLOCATE(ETW(NBASIN, NYRS, nperiods))
  ENDIF
  IF (needHarg) THEN
     ALLOCATE(TRNG_C(NBASIN, NYRS, nperiods))
  ENDIF

  IF (.not. ALLOCATED(ZH)) THEN
     ALLOCATE(ZH(N_STA))
     ALLOCATE(ZM(N_STA))

     ALLOCATE(WWS(NBASIN, N_STA))
     ALLOCATE(WRS(NBASIN, N_STA))
     ALLOCATE(STNAME(N_STA))

     ALLOCATE(t28(NBASIN, NYRS, 2))
     ALLOCATE(t32(NBASIN, NYRS, 2))
  ENDIF

  IF (.not. monthly) then
     !Allocate daily vars.
     IF (.not. ALLOCATED(SLONG)) THEN
	ALLOCATE(SLONG(N_STA))
	ALLOCATE(SLAT(N_STA))
	ALLOCATE(ELEV(N_STA)) ! longitude, latitude, elevation
     ENDIF
  ENDIF

end SUBROUTINE AllocateWeatherVars
