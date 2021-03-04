SUBROUTINE PERENCRP(ib,ckey,nyr,begs,ends,begmo,begda,endmo, &
     endda,et_method,xf,xkt,xkc, &
				!Outputs
     t_npart, days, temps, dayf, tempf)

  !***************************************************************************
  !
  !   Function        : perencrp.f
  !   Author          : HB Manguerra
  !   Date            : December 1994
  !   Purpose         : This computes the crop coefficient, mean temperature
  !                     percent daylight hours and parameter f of the month
  !                     for perennial crops for the Blaney-Criddle method.
  !                     It includes the computation required for part months
  !                     (i.e. beginning and ending months) of the growing
  !                     season.
  !   Calling program : calpcrop.f
  !   Called programs : clndr.f, intertd.f
  !   Input arguments : nend  = end of growing season in julian day
  !   Input arguments : ckey = crop index
  !                     nbeg  = start of growing season in julian day
  !                     nend  = end of growing season in julian day
  !                     nbegmo = begin month of growing season
  !                     nbegda = begin day of growing season
  !                     nendmo = end month of growing season
  !                     nendda = end day of growing season
  !   Output arguments: npart = number of days in beginning month  in spring
  !   Assumptions     :
  !   Limitations     :
  !   Notes           : The routines are based on USBR XCONS2 program which
  !                     uses the SCS Modified Blaney-Criddle ET Estimation
  !                     Method.
  !
  !***************************************************************************

  USE Globals

  !Parameters
  INTEGER ib, nyr, ends, ckey, begs
  INTEGER begmo,begda,endmo,endda,et_method
  REAL xf(12), xkt(12), xkc(12)
  !Outputs
  INTEGER t_npart
  REAL days, temps, dayf, tempf

  INTERFACE
     INTEGER FUNCTION DaysInYear(yr)
       INTEGER yr
     END FUNCTION DaysInYear

     INTEGER FUNCTION JULIAN (m_mon, m_day, yr)
       INTEGER m_mon, m_day, yr
     END FUNCTION JULIAN

     SUBROUTINE INTERTD(ib,K,N,t_npart,selmon,midpt, tempc, dayC)
       INTEGER ib,t_npart, K, N, selmon, midpt
       !Outputs
       REAL dayC, tempc
     END SUBROUTINE INTERTD

  END INTERFACE

  !Local variable declaration
  INTEGER K, ndays_begmo
  INTEGER J, midspr
  INTEGER begmon, endmon, midfal, mid
  INTEGER midpts, midptf
  LOGICAL is_leap

  ndays_begmo = month(begmo)
  is_leap = .false.
  if (DaysInYear(NYR1+nyr-1) .eq. 366) is_leap = .true.

  if (begmo .eq. 2 .and. is_leap) ndays_begmo = ndays_begmo + 1

  !Calculate midpoint of beginning month in spring
  midpts=((ndays_begmo-begda + 1 )/2) + begda

  !Calculate midpoint of ending month in spring
  midptf= (endda + 1)/2

  !Calculate number of days in beginning month of spring
  t_npart=(ndays_begmo-begda + 1 )

  !interpolate temperature and dayhours for beginning month in spring
  DO K = 1, 8
     IF (tmean3(ib,nyr,k) .gt. -999) THEN
	IF(JULIAN(begmo, midpts,1990) .LT. middle(K)) THEN
	   CALL INTERTD(ib,K,nyr,t_npart,begmo, midpts, temps,days)
	   GO TO 69
	ENDIF
	IF(JULIAN(begmo, midpts,1990) .EQ. middle(k)) THEN
	   temps = tmean3(ib,nyr,k)
	   days = pclite(k)
	   GO TO 69
	ENDIF
     ENDIF
  END DO
  !CALL MYEXIT(119)

  !interpolate temperature and dayhours for ending month in spring
69 DO k = 1,12
     IF (tmean3(ib,nyr,k) .gt. -999) THEN
	IF( JULIAN(endmo, midptf,1990).LT. middle(K)) THEN
	   CALL INTERTD(ib,K,nyr,endda, endmo, midptf, tempf,dayf)
	   GO TO 78
	ENDIF
	IF( JULIAN(endmo, midptf,1990) .EQ. middle(K)) THEN
	   tempf = tmean3(ib, nyr, K)
	   dayf  = pclite(K)
	   GO TO 78
	ENDIF
     ENDIF
  END DO
  !CALL MYEXIT(120)
  !! - this isn't fatal.  Assume partial year of weather data.
  tempf = 0
  dayf = 0

  !interpolate kc for beginning month in spring
78 midspr = begs - begda + midpts
  DO K=1,PKCLEN
     IF(nckcp(ckey,K).GT.midspr) GO TO 82
     IF(nckcp(ckey,K).EQ.midspr) THEN
	xkc(begmo) = ckcp(ckey,K)
	GO TO 83
     ENDIF
  END DO
  WRITE(*,*) "Error with Crop Coefficients"
  WRITE(*,*) 'Error Crop Name is ',CNAME(CKEY)
  CALL MYEXIT(131)
82 xkc(begmo) = ckcp(ckey,K-1) + (ckcp(ckey,K)  &
       -  ckcp(ckey,K-1))*((midspr- REAL(nckcp(ckey,k-1)))/ &
       (nckcp(ckey,K)-REAL(nckcp(ckey,k-1))))
83 xf(begmo)=(temps*days)/100.0
  if (temps .gt. 0) then
     IF (et_method .EQ. 8) THEN
	!-------
	!Alfalfa
	!-------
	IF (crop_type(ckey) .EQ. 0) THEN
	   xkt(begmo) = 0.01411*temps - 0.07556
	   !-----
	   !Bluegrass
	   !-----
	ELSEIF (sub_crop_type(ckey) .EQ. 2) THEN
	   xkt(begmo) = 0.00328*temps + 0.6511
	ENDIF
     ELSE
	IF (temps.LT.36.0) THEN
	   xkt(begmo)=0.3
	ELSE
	   xkt(begmo)=0.0173*temps-0.314
	ENDIF
     ENDIF
  endif

  !full months -- kc computation

  mid=15
  begmon = begmo + 1
  endmon= endmo - 1
  DO K = begmon,endmon
     DO J=1,PKCLEN
	IF(nckcp(ckey,J).EQ.JULIAN(K,mid,1990)) GO TO 88
     END DO
     CALL MYEXIT(92)

88   xkc(K) = ckcp(ckey, J)
     if (tmean3(ib,nyr,K) .gt. -999) then
	xf(K)=(tmean3(ib,nyr,K)*pclite(K))/100.0
	IF (et_method .EQ. 8) THEN
	   !-------
	   !Alfalfa
	   !-------
	   IF (crop_type(ckey) .EQ. 0) THEN
	      xkt(K) = 0.01411*tmean3(ib,nyr,K) - 0.07556
	      !-----
	      !Bluegrass
	      !-----
	   ELSEIF (sub_crop_type(ckey) .EQ. 2) THEN
	      xkt(K) = 0.00328*tmean3(ib,nyr,K) + 0.6511
	   ENDIF
	ELSE   
	   IF(tmean3(ib,nyr,K).LT.36.0) THEN
	      xkt(K)=0.3
	   ELSE
	      xkt(K)=0.0173*tmean3(ib,nyr,K)-0.314
	   ENDIF
	ENDIF
     end if
  END DO

  !interpolate kc for ending month in fall
  midfal = ends-midptf
  DO 95 K=1,PKCLEN
     IF(nckcp(ckey,K).GT.midfal) GO TO 96
     IF(nckcp(ckey,K).EQ.midfal) xkc(endmo) = ckcp(ckey,K)
     IF(nckcp(ckey,K).EQ.midfal) GO TO 97
95 END DO
96 xkc(endmo) = ckcp(ckey,K-1)+(ckcp(ckey,K)  &
       - ckcp(ckey,K-1))*((midfal- REAL(nckcp(ckey,k-1)))/ &
       (nckcp(ckey,K)-REAL(nckcp(ckey,k-1))))
97 xf(endmo)=(tempf*dayf)/100.0
  IF( et_method .EQ. 8) THEN
     !-------
     !Alfalfa
     !-------
     IF(CROP_TYPE(ckey) .EQ. 0) THEN
	xkt(endmo) = 0.01411*tempf - 0.07556
	!-----
	!Bluegrass
	!-----
     ELSEIF(SUB_CROP_TYPE(ckey) .EQ. 2) THEN
	xkt(endmo) = 0.00328*tempf + 0.6511
     ENDIF
  ELSE   
     IF(tempf.LT.36.0) THEN
	xkt(endmo) = 0.3
     ELSE
	xkt(endmo) = 0.0173*tempf-0.314
     ENDIF
  ENDIF

  RETURN
END SUBROUTINE PERENCRP
